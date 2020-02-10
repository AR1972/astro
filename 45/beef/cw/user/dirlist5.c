/*
	COW : Character Oriented Windows
	(COW USER DIALOG)

	dirlist.c : Directory dir list control
*/

#define COW
#include <cow.h>


#ifdef LISTBOX_DIR			/* entire file */

#define DIRLIST

#include <udialog.h>
#include <uevent.h>
#include <uwindow.h>
#include <uutil.h>
#include "dialog.h"
#include "event.h"
#include "listbox.h"
#include "window.h"
#include "util.h"

#include "dirlist.h"
#include "_dirlist.h"


extern char szWildAll[];


/* forward */
BOOL FAR PASCAL FBuildDirectoryList(PWND, char *, PWND, BOOL, char *);




BOOL FAR
FBuildDirectoryList(pwnd, szPath, pwndDir, fSetPath, szTemp)
/*
  -- DOS5 VERSION !!

  -- build up the directory list
  -- NOTE : if pwnd == NULL, then don't fill files
  -- return TRUE if ok, FALSE if illegal szPath (bad dir)
  -- if valid path and fSetPath, return in *szPath the path less the file name
*/
char *	szPath;
PWND	pwnd;
PWND	pwndDir;	/* where to put drives / directories */
BOOL	fSetPath;	/* TRUE => set path if ok */
char *	szTemp;		/* temporary storage passed down to save stack space */
	{
	FDE	fde;
	char	chDrive;
	BOOL	fRoot;			/* TRUE => root of directory tree */
	BOOL	fAbsPath;		/* TRUE => absolute path */
	char *	szFileName;		/* simple file name */
	char *	szPathOrig = szPath;	/* original path address */

	while (szPath[0] == ' ')
		szPath++;

	/* check to see if there is a valid directory */
	if ((szFileName = SzDirSpec(szPath, szTemp, &fRoot)) == NULL)
		{
		goto not_valid_dir;
		}

	if (!fRoot)
		{
		/* not a root directory, try looking for the directory
		    (like doing a stat()) */
		BOOL fOk;

		fOk = FFindFirst(&fde, szTemp, atrDir);
		FindClose();

		if (!fOk)
			{
			/* directory is bogus */
not_valid_dir:
			/* if we get an invalid directory,
			   we don't want to change (with the exception that
			   if this is a drives/directory only listbox
			   (i.e. pwnd == NULL) then we should fill it)
			*/
			if (pwnd != NULL)
				return FALSE;	/* bogus directory */
			fRoot = TRUE;
			fSetPath = FALSE;
			}
		}

	if (fSetPath)
		{
		SetCurrentPath(szPath);
		strcpy(szPathOrig, szFileName);
		szPath = szFileName = szPathOrig;	/* just filename left */
		fAbsPath = FALSE;
		}
	else
		{
		/* starting out, absolute if any prefix (relative if *.*) */
		fAbsPath = (szTemp[0] != '*');
		}
	
	/* we are going to move, reset list boxes */
	if (pwnd != NULL)
		SendMessageShort(pwnd, LB_RESETCONTENT);
	Assert(pwndDir != NULL)
	SendMessageShort(pwndDir, LB_RESETCONTENT);

	/* use FindFirst/Next twice */
	if (pwnd != NULL &&
	    *szPath != '\0')
		{
		if (FFindFirst(&fde, szPath, atrFile))
			{
			do
				{
				Assert(!(fde.atr & atrDir));
				AddListString(pwnd, fde.szName);
				}
			while (FFindNext(&fde));
			}
		FindClose();
		}

	if (FFindFirst(&fde, szWildAll, atrDir))
		{
		do
			{
			if ((fde.atr & atrDir) &&
			    !(fde.szName[0] == '.' && fde.szName[1] == '\0'))
				{
				if (pwndDir == pwnd)
					{
					/* 1 listbox => throw in [] */
					MakeDirName(fde.szName, szTemp);
					AddListString(pwndDir, szTemp);
					}
				else
					{
					/* add simple */
					AddListString(pwndDir, fde.szName);
					}
				}
			}
		while (FFindNext(&fde));
		}
	FindClose();

	/* lastly drives */
	strcpy(szTemp, "[- -]");
	for (chDrive = 'A'; chDrive <= 'Z'; chDrive++)
		{
		if (FValidDrive(chDrive))
			{
			szTemp[2] = chDrive;
			AddListString(pwndDir, szTemp);
			}
		}
	return TRUE;	/* all ok */
	}


#endif /*LISTBOX_DIR*/
