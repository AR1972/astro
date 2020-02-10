/* start a child task but do it with stdin/stdout redirected
 * this relies on the assumption that by duping 0 and 1, the child will NOT
 * attempt to access handles other than 0,1 and those that it has opened
 *
 * OS/2 -   The process will be executed asynchronously.  In addition, support
 *	    for redirection from/to pipes is added.  If fin or fout is the
 *	    string <pipe>, then a new pipe will be created and attached to the
 *	    new processes input or ouput.  If the form is <ascii-number> then
 *	    the input/output will be attached to handle #ascii-number.	This
 *	    number will usually be the the read or write handle of an open
 *	    pipe.
 *		The values will be returned in a static structure.  Both read
 *	    and write handles for pipes will be open on return.
 */

#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <io.h>
#include <process.h>
#include <string.h>
#include <stdlib.h>
#include "..\h\tools.h"

#if defined(OS2)
struct spawnInfo * rspawnl (fin, fout, name, args)
#else
int rspawnl (fin, fout, name, args)
#endif
char *fin, *fout, *name, *args;
{
    return rspawnv (fin, fout, name, &args);
}

#if defined(OS2)
struct spawnInfo * rspawnv (fin, fout, name, parg)
#else
int rspawnv (fin, fout, name, parg)
#endif
char *fin, *fout, *name, *parg[];
{
#if defined(OS2)
    static struct spawnInfo retval;
    int inh = -1, outh = -1;
/*  extern unsigned char _osmode; */
#endif
    int savin, savout, saverr, ret;

    ret = -1;
#if defined(OS2)
	retval.inReadHndl = -1;
	retval.inWriteHndl = -1;
	retval.outReadHndl = -1;
	retval.outWriteHndl = -1;
#endif


    if (fin) {
#if defined(OS2)
	if (_osmode)
	{
	    if ( *fin == '<' )
	    {
		if ( !stricmp( fin, "<pipe>" ) )
		{
		    if ( pipe( &retval.inReadHndl ) ) goto error;
		}
		else retval.inReadHndl = atoi(fin + 1);

		inh = retval.inReadHndl;
	    }
	    else if ( (inh = open (fin, O_RDONLY | O_TEXT)) == -1 ) goto error;

	    savin = dup(0);
	    close(0);
	    dup(inh);
	    if ( *fin != '<' ) close(inh);
	}
	else
	{
#endif
	savin = dup (0);
	close(0);
	if (open (fin, O_RDONLY | O_TEXT) == -1)
	     goto done;
	}
#if defined(OS2)
	}
#endif

    if (fout) {
#if defined(OS2)
	if (_osmode)
	{
	    if ( *fout == '<' )
	    {
		if ( !stricmp( fout, "<pipe>" ) )
		{
		    if ( pipe(&retval.outReadHndl) ) goto error;
		}
		else retval.outWriteHndl = atoi(fout + 1);

		outh = retval.outWriteHndl;
	    }
	    else if ( (outh = open (fout, O_CREAT | O_TRUNC | O_WRONLY | O_TEXT,
					  S_IWRITE | S_IREAD)) == -1 ) goto error;

	    savout = dup(1);
	    saverr = dup(2);
	    close(1);
	    dup(outh);
	    close(2);
	    dup(1);
	    if ( *fout != '<' ) close(outh);
	}
	else
	{
#endif
	savout = dup (1);
	saverr = dup (2);
	close(1);
	if (open (fout, O_CREAT | O_TRUNC | O_WRONLY | O_TEXT,
			S_IWRITE | S_IREAD) == -1 )
	     goto done;
	close (2);
	dup (1);
	}
#if defined(OS2)
	}
#endif

#if defined(OS2)
    if (_osmode)
	retval.PID = spawnvp( P_NOWAIT, name, parg);
    else
#endif
    ret = spawnvp (P_WAIT, name, parg);

done:
    if (fin) {
	close(0);
	dup(savin);
	close(savin);
	}
    if (fout) {
	close(1);
	dup(savout);
	close(savout);
	close (2);
	dup (saverr);
	close (saverr);
	}
#if defined(OS2)
    if (_osmode)
    {
	if ( retval.PID == -1 )
	{
	    if ( retval.inReadHndl != -1 )
	    {
		close( retval.inReadHndl );
		close( retval.inWriteHndl );
		retval.inReadHndl = -1;
		retval.inWriteHndl = -1;
	    }
	    if ( retval.outReadHndl != -1 )
	    {
		close( retval.outReadHndl );
		close( retval.outWriteHndl );
		retval.outReadHndl = -1;
		retval.outWriteHndl = -1;
	    }
	}

    return &retval;
    }
#endif

#if defined(OS2)
error:
    retval.PID = 0xFFFF;
    return &retval;
#else
    return ret;
#endif
}
