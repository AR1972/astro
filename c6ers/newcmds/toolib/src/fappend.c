/* fappend.c - fast append of one file to another */

#if defined (OS2)
#include <os2.h>
#endif

#include <io.h>
#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <malloc.h>
#include <stdlib.h>
#include <stdio.h>
#include <dos.h>
#include "..\h\tools.h"

#if defined(DOS)
#define IBUF 10240


char *fappend( src, dstfh )
char *src;
HANDLE dstfh;
{
    int srcfh, cnt, bufsiz;
    struct findType fbuf;
    char *copybuf, *result;

    result = "Out of memory";
    srcfh = -1;
    bufsiz = IBUF;
    while (!(copybuf = (*tools_alloc) (bufsiz) ))
        if ( !(bufsiz >>= 1) )
            goto done;

    result = "Unable to open source";
    if ( ffirst( src, ~A_V, (NPFIND)&fbuf ) )
        goto done;

    findclose(&fbuf);	/* Free dir handle for next ffirst */

        /* open source for read access */
    if ((srcfh = open (src, O_RDONLY | O_BINARY)) == -1)
        goto done;

        /* Loop, copying N bytes at a time until no more */
    lseek( dstfh, 0L, 2 );
    while ( ( cnt = read( srcfh, copybuf, bufsiz ) ) > 0)
        if ( write( dstfh, copybuf, cnt ) != cnt ) {
            result = "Unable to write destination";
            goto done;
            };

    if (cnt)
        result = "Unable to read source";
    else
        result = NULL;
done:
    cnt = errno;
    if (copybuf)
        free( copybuf );
    if (srcfh != -1)
        close( srcfh );
    errno = cnt;
    return result;
}


#elif defined(OS2) || defined(NT)

char *fappend(
    char *src,
    HANDLE dstfh
    )
{
    int srcfh;
    char *result;

    if ((srcfh = open (src, O_RDONLY | O_BINARY)) == -1)
	result = "Unable to open source";
    else {
	lseek( (int) dstfh, 0L, SEEK_END );
	result = fastcopy ((HANDLE)srcfh, dstfh);
        close( srcfh );
	}
    return result;
}

#endif
