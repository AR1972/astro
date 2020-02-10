/*
	cowstip.c : CWINDOWS.H stripper

	cowstrip : given list of files, read then in, include from BEGIN to END
		NOTE : begin & end symbols MUST be on the start of a line
	Also strips whitespace lines (i.e. blank)
	Output to stdout
*/



#include <stdio.h>
#include "cowstrip.h"

char szBegin[] = "/***BEGIN_PUBLIC***/";
char szEnd[] = "/***END_PUBLIC***/";

int cchBegin, cchEnd;

void
PrUsage()
	{
	printf("%s [-w] {files ...}\n", szNameProgram);
	printf("Includes files from '%s' to '%s'\n", szBegin, szEnd);
	}



main(argc, argv)
int argc;
char *argv[];
	{
	char chFlag;
	SZ szFn;

	ProcessArgs(argc, argv);

	if (cargCur < 1)
		{
		PrUsage();
		exit(1);
		}

	Init();

	while ((szFn = SzGetArg()) != NULL)
		StripFile(szFn);

	exit(0);
	}



Init()
	{
	cchBegin = strlen(szBegin);
	cchEnd = strlen(szEnd);
	}


#ifdef STRIP_WHITE
BOOL
FWhiteLine(sz)
/*
  -- return TRUE if line is all whitespace
*/
SZ sz;
	{
	while (*sz)
		if (*sz == ' ' || *sz == '\t')
			sz++;
		else
			return FALSE;
	return TRUE;
	}
#endif /*STRIP_WHITE*/



StripFile(szFn)
/*
  -- strip file
*/
SZ szFn;
	{
	FILE *pfileIn;
	BOOL fOut = FALSE;
	char rgchBuffer[256];
	SZ szIn, fgets();

	if ((pfileIn = fopen(szFn, szROText)) == NULL)
		FatalError("can't open %s\n", szFn);

	while ((szIn = fgets(rgchBuffer, sizeof(rgchBuffer), pfileIn)) != NULL)
		{
		int cch = strlen(szIn);
		if (szIn[cch-1] != '\n')
			FatalError("file %s, line too long : %s\n", szFn, szIn);
		szIn[cch-1] = '\0';

		if (!fOut && strncmp(szIn, szBegin, cchBegin) == 0)
			fOut = TRUE;
		else if (fOut && strncmp(szIn, szEnd, cchEnd) == 0)
			fOut = FALSE;
#ifdef STRIP_WHITE
		else if (fOut && !FWhiteLine(szIn))
#else
		else if (fOut)
#endif
			/* we must output this line */
			puts(szIn);
		/* else ignore */
		}
	fclose(pfileIn);
	}
