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

#ifdef LISTBOX_LIMIT_SIZE
#define	cchDirMax	80				/* 64 + file name */
#else
#define	cchDirMax	256
#endif


BOOL FAR PASCAL FBuildDirectoryList(PWND, char *, PWND, BOOL, char *);


/* forward */
STATIC BOOL FCorrectDriveDir(char *);
STATIC char *SzChopText(PWND, char *);


char szWildAll[] = "*.*";



PRIVATE VOID FARPRIVATE
DlgDirList(pwndListBox, szPath, pwndStatic, fDisplay, pwndListBox2)
/*
  -- construct a directory listing in the listbox.
  -- return TRUE if the supplied path could be parsed
  -- NOTE : do not change current directory unless redrawing
	(kludgy way to detect startup case)
  -- NOTE : if pwndListBox == NULL, then don't fill
*/
REGISTER PWND pwndListBox;	/* listbox window */
char *szPath;			/* path */
PWND pwndStatic;		/* static item for tracking text or NULL */
BOOL fDisplay;
REGISTER PWND pwndListBox2;	/* extra window for drives / dir */
	{
	char	szDir[cchDirMax];	/* buffer for drive names */

	/* fill in listbox */
	Assert(pwndListBox != NULL || pwndListBox2 != NULL);

	if (pwndListBox2 == NULL)
		pwndListBox2 = pwndListBox;

	if (!FBuildDirectoryList(pwndListBox, szPath, pwndListBox2, fDisplay,
					szDir))
		return;		/* illegal directory */

	if (fDisplay)
		{
		if (pwndListBox != NULL)
			DrawWindow(pwndListBox);
		if (pwndListBox2 != pwndListBox && pwndListBox2 != NULL)
			DrawWindow(pwndListBox2);
		}

	if (pwndStatic != NULL)
		{
		/* fill in static text item */
		GetCurDir(GetCurDrive(), szDir);
		SetDlgItemText(pwndStatic, SzChopText(pwndStatic, szDir),
		    fDisplay);
		}
	}



VOID FAR PASCAL
SetCurrentPath(szPath)
/*
  -- use path to set current drive / directory
  -- NOTE : does not modify szPath
*/
REGISTER char *szPath;
	{
	char *	szTop = szPath;
	REGISTER char * pch;
	REGISTER char *	pchDelim = NULL;	/* after last delimiter */

	while (szPath[0] == ' ')
		szPath++;

	if (szPath[1] == ':')
		{
		/* drive prefix */
		if (!FValidDrive(szPath[0]))
			return;		/* invalid drive */

		SetCurDrive(szPath[0]);
		szPath += 2;
		}

	/* copy path to buffer, record position of last delimiter */
	for (pch = szPath; *pch != '\0'; pch++)
		{
		if (*pch == '/' || *pch == '\\')
			pchDelim = pch+1;		/* point past delim */
		}

	if (pchDelim != NULL)
		{
		/* we have a prefixing drive or directory */
		char	chSave;

		if (pchDelim != szPath+1)
			pchDelim--;		/* lose last slash */
		chSave = *pchDelim;		/* save old */
		*pchDelim = '\0';		/* truncate to directory */
		FSetCurDir(szPath);
		*pchDelim = chSave;		/* restore string */
		}
	}



char * FAR PASCAL
SzDirSpec(szPath, szDir, pfRoot)
/*
  -- fill szDir with the path of the directory defined by szPath
  -- return NULL if bogus directory (invalid drive)
  -- otherwise return address of filename (inside szPath buffer).
  -- set *pfRoot if
	(1) the path is just a root directory (eg. "x:").
	(2) the path is a root directory followed by a wildcard
*/
REGISTER char *szPath;
REGISTER char *szDir;
BOOL *	pfRoot;			/* set if directory is root */
	{
	char *szSlash = szDir;
	BOOL	fSlash = FALSE;		/* => found a slash separator */
	BOOL	fWild = FALSE;		/* => found a wild card */

	*pfRoot = FALSE;
	while (*szPath != '\0')
		{
		switch (*szDir++ = *szPath++)
			{

		case '?':
		case '*':
			fWild = TRUE;
			/*fall through*/
		default:
			*pfRoot = FALSE;
			break;

		case ':':
			if (!FValidDrive(*(szPath-2)))
				return NULL;
			*pfRoot = TRUE;		/* we are at the root */
			goto normal_delim;
		case '\\':
		case '/':
			fSlash = TRUE;
normal_delim:
			szSlash = szDir;
			break;
			}
		}

	if (!fSlash && fWild)
		{
		/* no slashes and wildcards, probably root-like */
		*pfRoot = TRUE;
		}

	strcpy(szSlash, szWildAll);
	return (szPath + (szSlash - szDir));
	}



VOID FAR PASCAL
MakeDirName(sz, szDir)
REGISTER char *sz, *szDir;
	{
	char ch;

	*szDir = '[';
	szDir++;
	for (; (ch = *sz) != '\0'; sz++, szDir++)
		*szDir = ch;
	*szDir++ = ']';
	*szDir = '\0';
	}



PRIVATE BOOL FARPRIVATE
DlgDirSelect(pwndListBox, sz, pwndListBox2)
/*
  -- copy the selected file name to the specified buffer
  -- strip out directory punctuation
  -- return TRUE if a drive / directory only
*/
PWND pwndListBox;
REGISTER char *sz;
PWND pwndListBox2;
	{
	Assert((pwndListBox->style & WS_TYPE) == WS_LISTBOX);

	if (pwndListBox2 != NULL)
		{
		/* ListBox2 => directory / drive */
		WORD cch;
		BOOL fDrive;
		Assert((pwndListBox2->style & WS_TYPE) == WS_LISTBOX);

		GetDlgItemText(pwndListBox2, sz, cchDirMax);
		fDrive = FCorrectDriveDir(sz);
		sz += (cch = strlen(sz));
		if (!fDrive && cch != 0)
			{
			/* no drive & text => prefix directory */
			*sz++ = '\\';
			cch--;
			}
		GetDlgItemText(pwndListBox, sz, cchDirMax - cch);
		if (strlen(sz) == 0)
			return (TRUE); /* if no file name */
		}
	else
		{
		GetDlgItemText(pwndListBox, sz, cchDirMax);
		if (FCorrectDriveDir(sz))
			return(TRUE);
		}

	while( (*sz != '.') && (*sz != '\0'))
		sz++;
	if (*sz == '\0')
		{
		*sz = '.';
		*(sz+1) = '\0';
		}

	return(FALSE);
	}



STATIC BOOL
FCorrectDriveDir(sz)
/*
  -- correct file name if *sz contains a drive or directory
  -- return TRUE if it contained a drive / directory
*/
REGISTER char *sz;
	{
	REGISTER char *szNext = sz;

	if (*szNext++ != '[')
		return FALSE;	/* no drive / dir */

	if (*szNext == '-')
		{
		/* it is a drive */
		*sz++ = szNext[1];
		*sz++ = ':';
		}
	else
		{
		while (*szNext != ']' && *szNext != '\0')
			*sz++ = *szNext++;
		*sz++ = '\\';
		}
	*sz = '\0';

	return TRUE;
	}



STATIC char *
SzChopText(pwnd, szDir)
/*
  -- adjust Directory text "szDir" to fit in window pwnd
  -- munges "szDir"
*/
REGISTER PWND pwnd;
REGISTER char *	szDir;
	{
	WORD	cchField;
	BOOL	fChop = FALSE;
	WORD	cch;
	RRC	rrc;
	char	chDrive;

	/* Get length of static field */
	GetClientRrc(pwnd,&rrc);
	cchField = rrc.rxRight - rrc.rxLeft;

	/* Chop characters off front end of text until short enough */
	while (cchField < (cch = strlen(szDir)))
		{
		if (!fChop)
			{
			chDrive = *szDir;
			if (cchField <= 7)
				break;
			cchField -= 7;
			szDir += 7;
			}
		while (cch-- > 0 && *szDir++ != '\\')
			;
		fChop = TRUE;
		}

	/* if any characters chopped off, replace first three characters in
	   remaining text string with elipsis */
	if (fChop)
		{
		szDir--;
		*--szDir = '.';
		*--szDir = '.';
		*--szDir = '.';
		*--szDir = '\\';
		*--szDir = ':';
		*--szDir = chDrive;
		}

	return (szDir);
	}



BOOL FARPRIVATE
FMaybeDir(szPath)
/*
  -- return TRUE if "sz" might represent a directory
*/
char *	szPath;
	{
	REGISTER char *	sz = szPath;
	REGISTER char	ch;
	BOOL	fAllWhite = TRUE;
	WORD	atr;

	while ((ch = *sz) != '\0')
		{
		if (ch == ':' && !FValidDrive(*(sz-1)))
			return TRUE;	/* bogus drive : keep listbox up */
		if (ch == '*' || ch == '?')
			return TRUE;
		else if (ch != ' ')
			fAllWhite = FALSE;
		sz++;
		}
	/* leave with sz => terminating '\0' */

	if (fAllWhite)
		return TRUE;		/* all white => stay here */

	if (*(--sz) == ':')
		return TRUE;	/* probably a drive */
	if ((*sz == '.') && (*(sz-1) == '.'))
		{				/* probably a directory */
		*(++sz) = '\\';			/* make it look like one */
		*(++sz) = '\0';
		return TRUE;
		}

	/* if ending '\' or '/' at end of path - remove for test */
	if (*sz == '\\' || *sz == '/')
		{
		if ((*(sz-1) == '.') && (*(sz-2) == '.'))
			return TRUE;	/* probably a directory */
		if (*(sz-1) == ':')	/* drive:\ case */
			return TRUE;
		*sz-- = '\0';
		}
	/* sz points to the last character in the string */

	/* check to see if it is a real directory */
	if ((atr = AtrOfPath(szPath)) != atrError &&
	    (atr & atrDir) != 0)
		{
		/* it is a real directory */
		*(++sz) = '\\';			/* make it look like one */
		*(++sz) = '\0';
		return TRUE;
		}
	return FALSE;		/* probably not a directory */
	}


#endif /*LISTBOX_DIR*/
