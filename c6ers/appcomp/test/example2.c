#include <stdio.h>
#include <stdlib.h>
#include <io.h>
#include <string.h>
#include <fcntl.h>
#include <direct.h>
#include <ctype.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <dos.h>
#include <malloc.h>

#include "sutkcomp.h"


/* ************************************************************** */
LONG MyWrite(int fhDest, BYTE far * fpbBuf, LONG lcbBuf)
{
	BYTE pbBuf[100];
	LONG lcbBufSav = lcbBuf;

	while (lcbBuf > 0L)
		{
		unsigned int cbWrite = (lcbBuf >= 100L) ? 100 : (unsigned int)lcbBuf;
		unsigned int ib;

		for (ib = cbWrite; ib > 0; )
			pbBuf[--ib] = *(fpbBuf + ib);

		if (write(fhDest, pbBuf, cbWrite) != (int)cbWrite)
			return(-1L);

		fpbBuf += cbWrite;
		lcbBuf -= (LONG)cbWrite;
		}

	return(lcbBufSav);
}


/* ************************************************************** */
int main(int argc, char * argv[])
{
	int        fhDest     = -1;
	int        fhSrc      = -1;
	SHORT      wReturn;
	LONG       lcbDecomp;
	LONG       lcbSkip    = 0L;
	long       libHeader;
	BYTE far * fpbBuf     = NULL;

	if ((fhSrc = open("input", O_RDONLY | O_BINARY)) == -1)
		{
		printf("unable to open source file 'input'\n");
		goto LExit;
		}

	if ((wReturn = WReadHeaderInfo(fhSrc)) < rcNoError)
		{
		printf("WReadHeaderInfo() failed; code = %d\n", wReturn);
		goto LExit;
		}

	/* need to return fhSrc either to the point after header or
	** to the beginning of file between successive calls to
	** LcbDecompressToBuffer().  If seeking to beginning, then
	** last arg to LcbDecompressToBuffer is FALSE.
	*/
	if ((libHeader = tell(fhSrc)) == -1L)
		{
		printf("unable to tell() on src\n");
		goto LExit;
		}

	if ((fhDest = open("output", O_TRUNC | O_BINARY | O_CREAT | O_WRONLY,
			S_IREAD | S_IWRITE)) == -1)
		{
		printf("unable to open destination file 'output'\n");
		goto LExit;
		}

	if ((fpbBuf = (BYTE far *)_fmalloc((size_t)65504)) == NULL)
		{
		printf("unable to alloc buffer of 65504 bytes from far heap\n");
		goto LExit;
		}

	while (TRUE)
		{
		/* need to return fhSrc either to the point after header or
		** to the beginning of file between successive calls to
		** LcbDecompressToBuffer().  If seeking to beginning, then
		** last arg to LcbDecompressToBuffer is FALSE.
		*/
		lcbDecomp = LcbDecompressToBuffer(fhSrc, fpbBuf, 65504L, lcbSkip,
				(BOOL)TRUE);

		if (lcbDecomp < rcNoError)
			{
			printf("LcbDecompressToBuffer() failed; code = %ld\n", lcbDecomp);
			goto LExit;
			}

		if (lcbDecomp == 0L)
			break;

		if (MyWrite(fhDest, fpbBuf, lcbDecomp) != lcbDecomp)
			{
			printf("unable to write buffer to file 'output'\n");
			goto LExit;
			}

		lcbSkip += lcbDecomp;

		/* need to return fhSrc either to the point after header or
		** to the beginning of file between successive calls to
		** LcbDecompressToBuffer().  If seeking to beginning, then
		** last arg to LcbDecompressToBuffer is FALSE.
		*/
		if (lseek(fhSrc, libHeader, SEEK_SET) == -1L)
			{
			printf("unable to seek src\n");
			goto LExit;
			}
		}

LExit:
	if (fhSrc != -1)
		close(fhSrc);
	if (fhSrc != -1)
		close(fhDest);
	if (fpbBuf != NULL)
		_ffree(fpbBuf);

	exit(0);
	return(0);
}
