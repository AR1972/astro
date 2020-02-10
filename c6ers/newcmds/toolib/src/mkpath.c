#include <stdio.h>
#include <io.h>
#include <direct.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "..\h\tools.h"

/* strrchrs() -- find the last instance in a string of any one of
**	a set of characters.  Return a pointer into the string at
**	the matchpoint.  Analogous to strrchr() in the CRT.
*/

char *strrchrs(char *szText, char * szSSet)
	{
	register char *pchSSet;
	register char *pchEnd;

	if ((NULL == szText) || (NULL == szSSet))
		return NULL;
	for (pchEnd = szText + (strlen(szText) - 1); pchEnd >= szText; --pchEnd)
		for (pchSSet = szSSet; '\0' != *pchSSet; ++pchSSet)
			if (*pchSSet == *pchEnd)
				return pchEnd;
	return NULL;
	}

/* MkPath() -- Make any directories necessary to ensure that a
**	directory name passed in exists.  Essentially, if the
**	argument exists and is a directory, return success.  If
**	not, strip off the last path component and recurse,
**	creating the directories on returning up the stack.
*/

int MkPath(char *szPath)
	{
	char *pchSlash;
	char chSep;
	static struct stat stBuf;

	//Does it exist?
	if (!stat(szPath, &stBuf))
		if (S_IFDIR == (stBuf.st_mode & S_IFDIR))
			return 0;
		else
			return -1;
	//Can we create it?
	else if (!mkdir(szPath))
		return 0;
	// are we out of path components?
	else if (NULL == (pchSlash = strrchrs(szPath, "\\/")))
		return -1;
	// Can we make its parent directory?
	else if ((chSep = *pchSlash), (*pchSlash = '\0'), MkPath(szPath))
		{
#ifndef DEBUG
		*pchSlash = chSep;
#endif
		return -1;
		}
	// Can we make it now that we've made its parent?
	else if ((*pchSlash = chSep), ('\0' != pchSlash[1]))
		return mkdir(szPath);
	else				//don't try trailing slash
		return 0;
	}


#ifdef STANDALONE

/* Standalone testbed for MkPath() */

int main(int argc, char *argv[])
	{
	int i;
	int iRet = 0;
	int cbsz;

	for (i = 1; i <argc; ++i)
		{
		if ('\"' == *argv[i])		//strip quotes, if any
		    (argv[i])++;
		if ('\"' == argv[i][(cbsz = strlen(argv[i]) - 1)])
		    argv[i][cbsz] = '\0';
		if (MkPath(argv[i]))
			{
			fprintf(stderr,
				"%s: Cannot create %s\n", argv[0], argv[i]);
			iRet = -1;
			}
		}
	return iRet;
	}

#endif /* STANDALONE */
