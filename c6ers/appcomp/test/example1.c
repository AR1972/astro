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
int main(int argc, char * argv[])
{
	int        fhDest     = -1;
	int        fhSrc      = -1;
	SHORT      wReturn;
	LONG       lcbDecomp;

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

	if ((fhDest = open("output", O_TRUNC | O_BINARY | O_CREAT | O_WRONLY,
			S_IREAD | S_IWRITE)) == -1)
		{
		printf("unable to open destination file 'output'\n");
		goto LExit;
		}

	lcbDecomp = LcbDecompressToFile(fhSrc, fhDest, NIL, 0L, (BOOL)TRUE);

	if (lcbDecomp < rcNoError)
		{
		printf("LcbDecompressToFile() failed; code = %ld\n", lcbDecomp);
		goto LExit;
		}

LExit:
	if (fhSrc != -1)
		close(fhSrc);
	if (fhSrc != -1)
		close(fhDest);

	exit(0);
	return(0);
}
