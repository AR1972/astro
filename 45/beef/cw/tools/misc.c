/*
	-- COWSTRIP utilities
*/

#include <stdio.h>
#include "cowstrip.h"

/*	- PUBLIC VARIABLES - */
char *szNameProgram;	/* Program name (not extension) */
int cargCur;		/* # of args left */

#ifdef D86
char chDriveProgram;	/* Drive program was ran from */
#endif

static SZ *pszArgNext;


void CDECL
ProcessArgs(argc, argv)
int argc;
SZ argv[];
	{
	char *pch;
	char *szFn;
#ifdef D86
	char *strlwr();
#endif

	if (argc < 1)
		{
		printf("???? : fatal error argc = %d\n", argc);
		exit(1);
		}

	pszArgNext = &argv[1];
	cargCur = argc-1;

#ifdef D86
	pch = strlwr(argv[0]);
	if (pch[1] == ':' && pch[0] >= 'a' && pch[0] <= 'z')
		{
		chDriveProgram = pch[0];
		pch += 2;		/* skip drive */
		}
	else
		chDriveProgram = '\0';		/* no drive specified */
#endif

	szNameProgram = pch;		/* start of path (no drive) */

#ifdef D86
	/* First convert any '\\' into '/' */
	while (*pch != '\0')
		{
		if (*pch == '\\')
			*pch = '/';
		pch++;
		}
#endif

	for (pch--; pch >= pch; pch--)
		if (*pch == '/')
			{
			szNameProgram = pch+1;
			break;
			}
#ifdef D86
		else if (*pch == '.')
			{
			*pch = '\0';	/* forget extension */
			}
#endif
	}



SZ CDECL
SzGetArg()
/*
  -- return sz of next free arguement, return NULL if end
*/
	{
	SZ szRet;
	if (cargCur < 1)
		return NULL;
	cargCur--;
	szRet = pszArgNext[0];
	pszArgNext++;
	return szRet;
	}



void CDECL
FatalError(szFmt, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)
CONST char *szFmt;
int v1, v2, v3, v4, v5, v6, v7, v8, v9, v10;
	{
	printf("%s: ERROR : ", szNameProgram);
	printf(szFmt, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10);
	exit(1);
	}
