/* return text of current directory
 *
 *  Modifications:
 *
 *	29-Oct-1986 mz	Lower case output
 *	09-Dec-1986 bw	Added DOS 5 support.
 *	30-Oct-1987 bw	Change 'DOS5' to 'OS2'
 *	20-Nov-1987 bw	Set errno to 19 for invalid drive
 *	03-Mar-1989 bw	Set C RTL _doserrno in OS/2.
 *	05-Jul-1989 bw	use MAXPATHLEN
 *
 */
#if defined(NT)
#define INCL_DOSERRORS
#include <windows.h>
#elif defined(OS2)
#define INCL_DOSERRORS
#include <os2.h>
#elif defined(DOS)
#include <dos.h>
#endif

#include <stdlib.h>
#include <string.h>

#include "..\h\tools.h"

int curdir (
char *buf,
char drive
) {
#if defined(DOS)

    union REGS regs;
#if ( defined(M_I86CM) || defined (M_I86LM) || defined (M_I86HM) )
    struct SREGS sregs;
#endif

    /* stick in drive */
    if (drive == 0) {
	regs.h.ah = 0x19;
	intdos (&regs, &regs);
	drive = (char) (regs.h.al + 1);
	}
    *buf++ = (char) ('a' + drive - 1);
    *buf++ = ':';
    if (fSwitChr ('/')) *buf++ = '\\' ; else *buf++ =  '/';
    // *buf++ = fSwitChr ('/') ? '\\' : '/';

    // Get current dir. intdosx(), segread() are used to
    // correctly load segment registers (ds) depending upon
    // the "model".

    regs.h.ah = 0x47;
    regs.h.dl = drive;
#if (defined(M_I86CM) || defined(M_I86LM) || defined(M_I86LM))
    segread( &sregs);
    sregs.ds = FP_SEG(buf);
    regs.x.si = FP_OFF(buf);
    intdosx(&regs, &regs, &sregs);
#else
    regs.x.si = (unsigned) buf;
    intdos (&regs, &regs);
#endif
    if (!regs.x.cflag)
	strlwr (buf);
    else
	errno = 19;
    return regs.x.cflag;

#elif defined(OS2)

    int  DirPathLen = MAXPATHLEN;
    long LogicalDriveMap;

    if (drive == 0)
    {
	DosQCurDisk( (PUSHORT)&drive , (PULONG)&LogicalDriveMap );
    }

    *buf++ = (char) ('a' + drive - 1);
    *buf++ = ':';
    if (fSwitChr ('/')) *buf++ = '\\' ; else *buf++ =  '/';
    // *buf++ = fSwitChr ('/') ? '\\' : '/';

    if (_doserrno = DosQCurDir( drive, buf, (PUSHORT)&DirPathLen ))
    {
	switch (_doserrno) {
	    case ERROR_INVALID_DRIVE:
		errno = 19;
		break;
	    case ERROR_BUFFER_OVERFLOW:
		errno = 33;
		break;
	    default:
		break;
	    }
	return 1;
    }
    pname (buf - 3);
    return 0;

#elif defined(NT)

    DWORD dwLength;

    //assert( !drive );
    drive;	//NT doesn't use the concept internally

    dwLength = GetCurrentDirectory( MAXPATHLEN, (LPSTR)buf );

    return( !dwLength );

#endif
}
