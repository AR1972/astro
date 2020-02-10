/*
 *  pipe()
 *	Xenix pipe using 286DOS call.  Try to make a 16K pipe, then halve
 *	request until success.	Minimum pipe is 512 bytes.
 *	    If not enough core for pipe, set errno to ENOMEM.
 *	    If not enough file handles,  set errno to EMFILE.
 *
*/

#define INCL_DOSQUEUES

#include <os2.h>
#include "..\h\tools.h"
#include <errno.h>


// extern int errno;
#define ERROR_NOT_ENOUGH_MEMORY 8

pipe( pd )
int pd[2];
{
    unsigned psize = 16384;
    int retc;


	do
	{
	    retc = DosMakePipe( (unsigned far *)&pd[0],
				(unsigned far *)&pd[1],
				psize  );
	} while ( retc == ERROR_NOT_ENOUGH_MEMORY  &&  (psize /= 2) > 256);


	if ( retc )
	{
	    if ( retc == ERROR_NOT_ENOUGH_MEMORY )  errno = ENOMEM;
	    else				    errno = EMFILE;

	    return -1;
	}


	return 0;
}
