/*
	COW : Character Oriented Windows
	(COW USER EDIT)

	medit.c : multiline edit manager (C portion)
*/

#define COW
#include <cow.h>

#ifdef EDIT_FULLMGR		/* entire file */

#include <uedit.h>
#include <umedit.h>		/* public structures */
#include <uevent.h>
#include <uwindow.h>
#include <uisa.h>

#include "edit.h"		/* includes "medit.h" */
#include "util.h"
#include "strings.h"
#include "dialog.h"


PUBLIC DWORD FARPUBLIC
EditWndProc(pwnd, message, wParam, lParam)
/*
  -- for edit windows
*/
PWND	pwnd;
WORD	message;
WORD	wParam;
DWORD	lParam;
	{
	StartPublic();

	DWORD lRet = InternalEditWndProc(pwnd, message, wParam, lParam);

	ReturnPublic(lRet, DWORD);
	}



PUBLIC VOID FARPUBLIC
SetEditText(pwnd, sz, fDrawWindow)
PWND	pwnd;
char *	sz;
BOOL	fDrawWindow;
	{
	StartPublic();

	SendMessage(pwnd, WM_SETTEXT, fDrawWindow, (DWORD) ((char far *) sz));

	StopPublic();
	}



PUBLIC WORD FARPUBLIC
GetEditText(pwnd, sz, cchMac)
PWND	pwnd;
char *	sz;
WORD	cchMac;
	{
	WORD cchRet;
	StartPublic();

	cchRet = (WORD) SendMessage(pwnd, WM_GETTEXT, cchMac,
	    (DWORD) ((char far *) sz));

	ReturnPublic(cchRet, WORD);
	}



PUBLIC VOID FARPUBLIC
InitEditWnd(pwnd, pb, cchMac)
REGISTER PWND pwnd;
BYTE *	pb;
REGISTER WORD cchMac;
	{
	EF *	pef;
	StartPublic();

	pef = (EF *)(pwnd->rgwExtra[cwExtraMin] = (WORD) pb);
	pb += sizeof(EF);

	pef->pldCur = (LD *) pb;
	pb += sizeof(LD);
	cchMac -= sizeof(EF) + sizeof(LD);

	pef->ipCur.ob = 0;
	pef->ipCur.oln = 0;
	pef->ipAnchor.ob = 0;
	pef->ipAnchor.oln = 0;

	pef->Style = 0;
	pef->fSelection = FALSE;
	pef->attrCur = isaEdit;
	pef->hBuffer = hBufferUndefined;
	pef->pwndScrollV = NULL;
	pef->pwndScrollH = NULL;

	pef->pldCur->prgch = pb;
	pef->pldCur->cbMax = cchMac;
	pef->pldCur->cb = 0;

	SetEditText(pwnd, szEmptyString, FALSE);

	StopPublic();
	}

#endif /*EDIT_FULLMGR*/
