/*  connect.c - perform network connections/disconnections
 *
 *  Modifications:
 *
 *	31-Jul-1986 mz	Use tools.h definitions
 *	03-Mar-1987 bw	Stub DOS5 version
 *	30-Oct-1987 bw	Change 'DOS5' to 'OS2'
 */

#include "..\h\tools.h"
#include <string.h>

#include <dos.h>

/*  Forward internal declarations
 */
static int FindFreeDrive (void);
static int MachineConnect (char *con);

/*  FindFreeDrive - find the lowest drive that is not in use
 *
 *  returns 0-based index of first drive not in use
 */
static int FindFreeDrive ()
{
    union REGS regs;

    for (regs.x.bx = 1; regs.x.bx <= 26; regs.x.bx++) {
	regs.x.ax = 0x4409;
	intdos (&regs, &regs);
	if (regs.x.cflag)
	    return regs.x.bx-1;
	}
    return -1;
}

/*  MachineConnect - attempt a connection to a particular drive
 *
 *  MachineConnect will attempt a drive connection to a connection without
 *  a password, reusing a drive if it exists.
 *
 *  con 	text of connection
 *
 *  returns	drive token if successful, -1 otherwise
 */
static int MachineConnect (char *con)
{
    char dev[MAXPATHLEN];
    char path[MAXPATHLEN];
    union REGS regs;
    struct SREGS sregs;
    int i;

    segread (&sregs);
    sregs.es = sregs.ds;

    /* try existing connections first */
    for (i = 0; ; i++) {
	regs.x.ax = 0x5F02;
	regs.x.bx = i;
	regs.x.si = (unsigned) dev;
	regs.x.di = (unsigned) path;

	intdosx (&regs, &regs, &sregs);
	if (regs.x.cflag)
	    break;
	if (!strcmpi (con, path))
	    return ((int) (dev[0] | 0x20) - (int) 'a') | REALDRIVE;
	}

    /* try creating a new drive */
    if ((i = FindFreeDrive ()) == -1)
	return -1;
    dev[0] = (char) (i + 'a');
    dev[1] = ':';
    dev[2] = 0;
    con[strlen(con)+1] = 0;

    regs.x.ax = 0x5F03;
    regs.x.bx = 0x0004;
    regs.x.cx = 0x4D5A;
#if ( defined(M_I86CM) || defined (M_I86LM) || defined (M_I86HM) )
    segread( &sregs);
    sregs.ds = FP_SEG(dev);
    regs.x.si = FP_OFF(dev);
	 sregs.es = FP_SEG(con);
	 regs.x.di = FP_OFF(con);
    intdosx(&regs, &regs, &sregs);
#else
    regs.x.si = (unsigned) dev;
    regs.x.di = (unsigned) con;
    intdos (&regs, &regs);
#endif

    if (regs.x.cflag)
	return -1;
    else
	return i;
}

/*  Connect - take an arbitrary pathname and connect to the appropriate
 *  server.
 *
 *  connect takes a name in the form \\mach\path and attempts to connect to
 *  the named machine.	Since DOS 3.x has problems with connections, we
 *  attach the connection to a drive.
 *
 *  path	network name of the form
 *		    \\machine\path  -or-
 *		    \\machine\shortname\path
 *  con 	buffer to place the successful connection
 *  sub 	buffer to place the remainder of the path
 *
 *  returns	-1 if connection was not successful, otherwise
 *		returns drive token
 */
int Connect (path, con, sub)
char *path;
char *con;
char *sub;
{
    int drive;
    char *p;

    /* make sure double backslash present */
    if (!fPathChr (path[0]) || !fPathChr (path[1]))
	return -1;

    /* find end of machine name */
    p = path+2;
    while (*p && !fPathChr (*p))
	p++;

    Move ((char far *) path, (char far *) con, p-path);
    con[p-path] = 0;
    strcpy (sub, p);
    if ((drive = MachineConnect (con)) != -1)
	return drive;

    /* try finding end of shortname */
    p++;
    while (*p && !fPathChr (*p))
	p++;

    Move ((char far *) path, (char far *) con, p-path);
    con[p-path] = 0;
    strcpy (sub, p);
    if ((drive = MachineConnect (con)) != -1)
	return drive;

    return -1;
}

/*  fDisconnect - remove a network connection
 *
 *  fDisconnect will delete a connection to a particular drive
 *
 *  drive	drive token of drive to be torched
 *
 *  returns	TRUE iff disconnection was successful
 */
flagType fDisconnect (drive)
int drive;
{
    char bufDrive[3];
    union REGS regs;

    if (ISTMPDRIVE(drive)) {
	bufDrive[0] = (char) (TOKTODRV(drive) + 'A');
	bufDrive[1] = ':';
	bufDrive[2] = 0;

	regs.x.ax = 0x5F04;
	regs.x.si = (unsigned) bufDrive;

	intdos (&regs, &regs);
	return (flagType) (regs.x.cflag == 0);
	}
    else
	return TRUE;
}
