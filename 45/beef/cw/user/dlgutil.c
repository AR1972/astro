/*
	COW : Character Oriented Windows

	dlgutil.c : dialog utility routines
*/

#define COW
#include <cow.h>

#define DIALOG

#include <udialog.h>
#include <uevent.h>
#include <uwindow.h>
#include <uedit.h>

#include "dialog.h"
#include "window.h"
#include "util.h"


PRIVATE VOID FARPRIVATE
SetDlgItemText(pwnd, szText, fDisplay)
/*
  -- set the text for a given item
  -- allow tilde characters for non-edit items
*/
REGISTER PWND pwnd;		/* the actual item */
char *szText;
BOOL fDisplay;
	{
	REGISTER char *pchSrc = szText;
	Assert(pwnd != NULL);

	if ((pwnd->style & WS_TYPE) == WS_EDIT)
		SetEditText(pwnd, pchSrc, fDisplay);
	else
		{
		WORD cch;			/* max # of chars */
		REGISTER char * pchDest;
		BOOL	fNoAccel;

		fNoAccel = (pwnd->style & WS_TYPE) == WS_STATIC_NOACCEL;
		cch = pwnd->cchDialog;
		Assert(cch > 0);
		pchDest = (char *) pwnd->szDialog;
		Assert(pchDest != NULL);

		pwnd->aclDialog = aclNil;
		while (1)
			{
			switch (*pchSrc)
				{
			default:
default_char:
				*pchDest++ = *pchSrc++;
#ifndef KANJI
add_char:
#endif
				if (--cch != 0)
					break;	/* continue */
				/* else fall through to terminate */
			case '\0':
				/* terminate & we are done */
				*pchDest = '\0';
				goto done_scan;
				
			case chPrefix1:
				if (fNoAccel)
					goto default_char;
				/* set accelerator, show character */
				AssertSz(pwnd->aclDialog == aclNil &&
				    *(pchSrc+1) != '\0',
				    "Bogus ~ name");
#ifndef KANJI
				Assert(pchSrc - szText < 256);
				if (*(pchSrc+1) == chPrefix1)
					{
					/* two accelerators => use one */
					pchSrc++;
					goto default_char;
					}
				pwnd->aclDialog = (((BYTE)
				    (pchSrc - szText)) << 8) +
				    *((unsigned char *)pchSrc+1);
				pchSrc++;
#else
				/* two choices for R/K accelerators */
				AssertSz(pchSrc == szText,
				   "Kana accelerators must be at start of string");
				/* Roman in lobyte, Kana in high */
				pchSrc++;	/* skip prefix */
				pwnd->aclDialog = *(WORD *)pchSrc;
				/* show proper dialog accelerator */
				*pchDest++ = *(fKanaAccel ? pchSrc+1 : pchSrc);
				pchSrc += 2;
#endif /*KANJI*/
				break;

#ifndef KANJI
			case chPrefix2:
				if (fNoAccel)
					goto default_char;
				/* set accelerator, show blank */
				AssertSz(pwnd->aclDialog == aclNil &&
				    *(pchSrc+1) != '\0',
				    "Bogus double~ name");
				Assert(pchSrc - szText < 256);
				pwnd->aclDialog = (((BYTE)
				    (pchSrc - szText)) << 8) +
				    *((unsigned char *)pchSrc+1);
				pchSrc += 2;	/* skip prefix & accel character */
				*pchDest++ = ' ';
				goto add_char;
				/*break;*/
#endif
				}
			}
		}
done_scan:
	if (fDisplay)
		DrawWindow(pwnd);
	}


PRIVATE WORD FARPRIVATE
GetDlgItemText(pwnd, sz, cchMax)
/*
  -- get the text associated with a listbox, edit or static text item;
	copies up to cchsz characters into the buffer sz; returns the number
	of characters copied into the buffer
*/
REGISTER PWND pwnd;	/* the actual item */
WORD cchMax;
REGISTER char *sz;
	{
	WORD cch;

	Assert(pwnd != NULL);
	switch(pwnd->style & WS_TYPE)
		{
	case WS_LISTBOX:
		return(GetListText(pwnd, sz, cchMax));
		break;
	case WS_EDIT:
		return(GetEditText(pwnd, sz, cchMax));
		break;
	default:
		{
		REGISTER char *szDlg = (char *) pwnd->szDialog;

		for (cch = 0; *szDlg != '\0' && cch < cchMax; cch++)
			*sz++ = *szDlg++;
		*sz = '\0';
		return(cch);
		break;
		}

		}
	}
