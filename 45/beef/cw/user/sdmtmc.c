/*
	COW : Character Oriented Windows

	sdmtmc.c : SDM TMC functions
*/

#define COW
#include <cow.h>

#define SDM

#include <sdmver.h>
#include <usdm.h>
#include <usdmtmpl.h>

#include <uevent.h>
#include <uedit.h>
#include <udialog.h>
#include <uwindow.h>
#include <uisa.h>

#include "dialog.h"
#include "event.h"
#include "button.h"
#include "util.h"

#include "sdm.h"

/* G L O B A L S */



PUBLIC VOID FARPUBLIC
SetTmcText(tmc, sz)
TMC tmc;
char *sz;
	{
	StartPublic();
	SetDlgItemText(PtifFromTmc(tmc)->pwnd, sz, fRedrawItem);
	StopPublic();
	}



PUBLIC VOID FARPUBLIC
GetTmcText(tmc, sz, cb)
TMC tmc;
char *sz;
WORD cb;
	{
	StartPublic();
	GetDlgItemText(PtifFromTmc(tmc)->pwnd, sz, cb);
	StopPublic();
	}



PUBLIC VOID FARPUBLIC
EnableTmc(tmc, fEnable)
TMC tmc;
BOOL fEnable;
	{
	REGISTER PTIF ptif = PtifFromTmc(tmc & ~tmcGrouped);
	REGISTER PWND pwnd;

	if (tmc & tmcGrouped)
		{
		Assert(ptif->ptm->tmtBase == tmtRadioButton);
		Assert(ptif->ptm->fFirstButton);
		do
			{
			pwnd = ptif->pwnd;
			Assert(pwnd != NULL);
			EnableWindow(pwnd, fEnable);
			if (fRedrawItem)
				DrawWindow(pwnd);
			ptif++;
			}
		while (ptif->ptm->tmtBase == tmtRadioButton &&
			!ptif->ptm->fFirstButton);
		}
	else
		{
		pwnd = ptif->pwnd;
		Assert(pwnd != NULL);
		EnableWindow(pwnd, fEnable);
		/* update window if necessary */
		if (fRedrawItem)
			DrawWindow(pwnd);
		}
	}



PUBLIC BOOL FARPUBLIC
FEnabledTmc(tmc)
/*
  -- return TRUE if item enabled
*/
TMC tmc;
	{
	StartPublic();
	ReturnPublic(PtifFromTmc(tmc)->pwnd->fEnabled, BOOL);
	}



PUBLIC VOID FARPUBLIC
SetTmcVal(tmc, val)
TMC tmc;
WORD val;
	{
	StartPublic();
	REGISTER PTIF ptif = PtifFromTmc(tmc & ~tmcGrouped);

	switch(ptif->ptm->tmtBase)
		{
#ifdef DEBUG
	default:
		Assert(FALSE);
		break;
#endif
	case tmtEdit:
		{
		WORD *pval = &val;
		char sz[cwEditBuffer * sizeof (WORD)];

		AssertExtension1(ptif);
		AssertSz(Ptm1OfTif(ptif)->pfnCtl != NULL, "SetTmcVal : edit with no Parse Proc");
		(*Ptm1OfTif(ptif)->pfnCtl)(tmmFormat, sz, (WORD) &pval, tmc, 0, 0);
		SetDlgItemText(ptif->pwnd, sz, fRedrawItem);
		}
		break;
	case tmtCheckBox:
		CheckDlgButton(ptif->pwnd, val, fRedrawItem);
		break;
	case tmtRadioButton:
		Assert(tmc & tmcGrouped);
		Assert(ptif->ptm->fFirstButton);
		CheckRadioButton(ptif->pwnd, val, fRedrawItem);
		break;
	case tmtListBox:
		/* we assume the right val for SendMessage */
		SendMessage(ptif->pwnd, LB_SETCURSEL, val, 0L);
		break;
		}
	StopPublic();
	}



PUBLIC WORD FARPUBLIC
GetTmcVal(tmc)
TMC tmc;
	{
	StartPublic();
	REGISTER PTIF ptif = PtifFromTmc(tmc & ~tmcGrouped);

	switch(ptif->ptm->tmtBase)
		{
#ifdef DEBUG
	default:
		Alert("GetTmcVal: bad tmt");
		ReturnPublic(0, WORD);
		/*break*/
#endif
	case tmtEdit:
		{
		WORD val;
		WORD *pval = &val;
		char sz[cwEditBuffer * sizeof (WORD)];

		AssertExtension1(ptif);
		AssertSz(Ptm1OfTif(ptif)->pfnCtl != NULL, "GetTmcVal : edit with no Parse Proc");
		GetDlgItemText(ptif->pwnd, sz, sizeof(sz)-1);
		(*Ptm1OfTif(ptif)->pfnCtl)(tmmParse, sz, (WORD) &pval, tmc, 0, 0);
		ReturnPublic(val, WORD);
		}
		/*break*/

	case tmtCheckBox:
		ReturnPublic(WButtonChecked(ptif->pwnd), WORD);
	case tmtRadioButton:
		Assert(ptif->ptm->fFirstButton);
		Assert(tmc & tmcGrouped);
		ReturnPublic(GetRadVal(ptif), WORD);
	case tmtListBox:
		/* we assume the right val for SendMessage */
		ReturnPublic((WORD) SendMessage(ptif->pwnd, LB_GETCURSEL,
		    0, 0L), WORD);
		}
	}



PRIVATE WORD
GetRadVal(ptif)
/*
  -- given a Radio Button (must be first) return radio value
  -- return -1 if none.
*/
REGISTER PTIF ptif;
	{
	WORD itm;

	Assert(ptif->ptm->tmtBase == tmtRadioButton);
	Assert(ptif->ptm->fFirstButton);

	itm = 0;
	do
		{
		if (WButtonChecked(ptif->pwnd))
			return itm;
		ptif++;
		itm++;
		}
	while (ptif->ptm->tmtBase == tmtRadioButton &&
		!ptif->ptm->fFirstButton);

	return((WORD)-1);	/* nothing set */
	}





PUBLIC VOID FARPUBLIC
SetTmcSel(tmc, ichSelFirst, ichSelLim)
/*
  -- set selection for edit item
*/
TMC tmc;			/* item code of Edit Item */
WORD ichSelFirst;		/* first character */
WORD ichSelLim;			/* last character + 1 */
	{
	StartPublic();
	REGISTER PTIF ptif = PtifFromTmc(tmc);

	SetFocus(ptif->pwnd);
	SendMessage(ptif->pwnd, EM_SETSEL, 0, MAKELONG(ichSelFirst, ichSelLim));
	StopPublic();
	}



PUBLIC VOID FARPUBLIC
SetFocusTmc(tmc)
/*
  -- move focus in dialog
*/
TMC tmc;
	{
	StartPublic();
	REGISTER PWND pwnd = PtifFromTmc(tmc)->pwnd;

	AssertSz(pwnd->fEnabled, "SetFocusTmc (item disabled)");

	SetFocus(pwnd);
	StopPublic();
	}


#ifndef LISTBOX_HORIZ	// only supported for vertical

PUBLIC VOID FARPUBLIC
GetTmcListBoxOrientation(tmc, pisz, pdisz)
/*
  -- gets the listbox orientation
*/
TMC tmc;
WORD	* pisz;
WORD	* pdisz;
	{
	StartPublic();
	REGISTER PTIF ptif;

	ptif = PtifFromTmc(tmc);

	Assert(ptif->ptm->tmtBase == tmtListBox);

	GetListBoxOrientation(ptif->pwnd, pisz, pdisz);

	StopPublic();
	}

PUBLIC VOID FARPUBLIC
RedisplayListBoxOriented(tmc,pisz,pdisz)
/*
  -- cause a listbox to be redisplayed
*/
TMC tmc;
WORD	* pisz;
WORD	* pdisz;
	{
	StartPublic();
	REGISTER PTIF ptif;

	ptif = PtifFromTmc(tmc);

	Assert(ptif->ptm->tmtBase == tmtListBox);

	InitListBoxOriented(ptif->pwnd, NULL, pisz, pdisz);
	StopPublic();
	}

#endif /*!LISTBOX_HORIZ*/

PUBLIC VOID FARPUBLIC
RedisplayListBox(tmc)
/*
  -- cause a listbox to be redisplayed
*/
TMC tmc;
	{
	StartPublic();
	REGISTER PTIF ptif;

	ptif = PtifFromTmc(tmc);

	AssertSz(fRedrawItem, "RedisplayListBox called in init");
	Assert(ptif->ptm->tmtBase == tmtListBox);

	FillListBoxTif(ptif, iszNinchList);
	DrawWindow(ptif->pwnd);
	StopPublic();
	}


PUBLIC PWND FARPUBLIC
PwndOfListbox(tmc)
/*
  -- sends a message to a listbox
*/
TMC tmc;
	{
	StartPublic();
	REGISTER PTIF ptif;

	ptif = PtifFromTmc(tmc);

	/*Assert(ptif->ptm->tmtBase == tmtListBox);*/

	ReturnPublic(ptif->pwnd, PWND);
	}

PUBLIC VOID FARPUBLIC
SetDefaultTmc(tmc)
/*
  -- change the default pushbutton
*/
TMC tmc;
	{
	StartPublic();
	PWND	pwnd;

	KillDefaultButton();
	if (tmc != tmcNull)
		{
		pwnd = PtifFromTmc(tmc)->pwnd;
		pwnd->style = (pwnd->style & WS_TYPE) | BS_DEFPUSHBUTTON;
		DrawWindow(pwnd);
		}
	}

#ifndef EDIT_FULLMGR

PUBLIC VOID FARPUBLIC
SetTmcEditWidth(tmc, cch)
/*
  -- set width of horizontal listbox
*/
TMC	tmc;
WORD	cch;
	{
	StartPublic();
	REGISTER PTIF ptif;

	ptif = PtifFromTmc(tmc);

	Assert(ptif->ptm->tmtBase == tmtEdit);

	SetEditWidth(ptif->pwnd,cch);

	StopPublic();
	}

#endif	/*EDIT_FULLMGR*/

#ifdef LISTBOX_HORIZ

PUBLIC VOID FARPUBLIC
SetTmcListWidth(tmc, citem)
/*
  -- set width of horizontal listbox
*/
TMC	tmc;
WORD	citem;
	{
	StartPublic();
	REGISTER PTIF ptif;

	ptif = PtifFromTmc(tmc);

	Assert(ptif->ptm->tmtBase == tmtListBox);

	SendMessage(ptif->pwnd, LB_SETWIDTH, citem, 0L);
	StopPublic();
	}

#endif /*LISTBOX_HORIZ*/
