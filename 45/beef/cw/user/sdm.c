/*
	CW : Character Oriented Windows

	sdm.c : Standard Dialog Manager (COW variant)

	from Windows SDM Version 1.3 (2/14/86)
*/

#ifndef LATER
#define	wParamLater	0
#endif

#define COW
#include <cow.h>

#define SDM
#define SDM_ENGINE

#include <sdmver.h>
#include <kmem.h>
#include <usdm.h>
#include <usdmtmpl.h>

#include <uevent.h>
#include <udialog.h>
#include <uwindow.h>
#include <uisa.h>

#include <uscroll.h>
#include <uedit.h>

#include "dialog.h"
#include "event.h"
#include "window.h"
#include "util.h"
#include "screen.h"
#include "shadow.h"

#include "edit.h"
#ifdef EDIT_FULLMGR
#include <umedit.h>
#endif

#include "scroll.h"
#include "button.h"
#include "static.h"
#include "general.h"
#include "listbox.h"
#include "_listbox.h"

#include "sdm.h"
#include "_sdm.h"

#include "strings.h"

#ifdef LISTBOX_LIMIT_SIZE
#define	cchDirMax	80				/* 64 + file name */
#else
#define	cchDirMax	256
#endif

#ifdef LISTBOX_LIMIT_SIZE
#define	cchListTextMax	64
#else
#define	cchListTextMax	256
#endif

/* G L O B A L S */

#ifdef DUAL
extern	WORD	fProtectMode;
#endif

STATIC  SDS sdsCur;			/* current SDM state */

static  BYTE *pbWorkCur;		/* current position in work buffer */


#ifdef DEBUG
static BYTE *pbWorkMac;		/* max value of pbWorkCur - for assertions */
#endif /*DEBUG*/


/* forward */
STATIC DWORD FARPUBLIC SdmDialogProc(PWND, WORD, TMC, DWORD);
STATIC PWND	PwndAllocArc(PARC, WORD);
STATIC PWND 	PwndBuildDialog(PDLG);
STATIC VOID	AllocDialogString(PWND, char *);
STATIC HCAB	HcabFromDialog(void);
STATIC BOOL	FSetDialogFromCab(void);

STATIC VOID	TextToCab(HCAB, PWND, WORD);
STATIC VOID	TextFromCab(PWND, WORD);
STATIC BOOL	FParseToCabTif(HCAB, PTIF);
STATIC VOID	FormatFromCabTif(HCAB, PTIF);

STATIC char *	SzWildCard(char *);
STATIC BOOL	FFillComboDir(PTIF, BOOL);
STATIC VOID	SelChangeComboListBox(PTIF);

#ifdef DEBUG
STATIC VOID	AssertTableOrder(void);
#endif



/* Important Tables : see AssertTableOrder for assertion */

/* item creation info :: should really go in CS */
STATIC TCI mptmttci[] =	/* # of bytes needed */
	{
	TciMake(cwExtraDialog,	0 /* n/a */,
		WS_TILED | WS_DIALOG | WS_BORDER),	/* tmtDialog !! */
	TciMake(cwExtraStatic,	cwRect,
		WS_CHILD | WS_STATIC),			/* tmtStaticText */
	TciMake(cwExtraStatic,	cwRect,
		WS_CHILD | WS_STATIC_NOACCEL),		/* tmtFormattedText */
	TciMake(cwExtraStatic,	cwRect,
		WS_CHILD | WS_STATIC_NOACCEL),		/* tmtTrackingText */
	TciMake(cwExtraButton,	cwRect,
		WS_CHILD | WS_BUTTON | BS_GROUP | WS_BORDER),
							/* tmtGroupBox */
	TciMake(cwExtraButton,	cwRect, wsPushButton),
							/* tmtPushButton */
	TciMake(cwExtraButton,	cwRect,
		WS_CHILD | WS_BUTTON | BS_AUTOCHECKBOX),
							/* tmtCheckBox */
	TciMake(cwExtraButton,	cwRect,
		WS_CHILD | WS_BUTTON | BS_RADIOBUTTON),	/* tmtRadioButton */
	TciMake(cwExtraButton,	cwRect,
		WS_CHILD | WS_BUTTON | 0xffff /*not implemented*/),
							/* tmtToggleBox */
	TciMake(cwExtraButton,	cwRect,
		WS_CHILD | WS_BUTTON | 0xffff /*not implemented*/),
							/* tmtToggleButton */
#ifdef EDIT_FULLMGR
	TciMake(cwExtraEdit,	cwEditBuffer + ((sizeof (EF) + sizeof (LD))/2),
		WS_CHILD | WS_EDIT),			/* tmtEdit */
#else
	TciMake(cwExtraEdit,	cwEditBuffer,
		WS_CHILD | WS_EDIT),			/* tmtEdit */
#endif
	TciMake(cwExtraListBox,	cwWnd + cwExtraScroll,
		WS_CHILD | WS_LISTBOX | WS_VSCROLL | WS_BORDER),
							/* tmtListBox */
	TciMake(cwExtraButton,	cwRect, wsPushButton),
							/* tmtStandardButton */
	TciMake(cwExtraStatic,	cwRect,
		WS_CHILD | WS_STATIC),			/* tmtDummyText */
	TciMake(cwExtraGeneral,	0,
		WS_CHILD),				/* tmtGeneralPicture */

	/* past end of normal tmts */
	TciMake(cwExtraScroll,	0 /* n/a */,
		WS_CHILD | SBS_VERT)			/* tmtScroll !! */

	};


#ifdef DEBUG
STATIC VOID
AssertTableOrder()
/*
  -- assert table ordering !!!
  -- isn't C wonderful
*/
	{
	Assert(sizeof(mptmttci) == sizeof(TCI) * (tmtNormalMax+1));
	Assert(tmtDialog == 0);		/* special tmt */
	Assert(tmtStaticText == 1);
	Assert(tmtFormattedText == 2);
	Assert(tmtTrackingText == 3);
	Assert(tmtGroupBox == 4);
	Assert(tmtPushButton == 5);
	Assert(tmtCheckBox == 6);
	Assert(tmtRadioButton == 7);
	Assert(tmtToggleBox == 8);
	Assert(tmtToggleButton == 9);
	Assert(tmtEdit == 10);
	Assert(tmtListBox == 11);
	Assert(tmtStandardButton == 12);
	Assert(tmtDummyText == 13);
	Assert(tmtGeneralPicture == 14);
	Assert(tmtScroll == 15);	/* special tmt */
	}
#endif /*DEBUG*/



PUBLIC TMC FARPUBLIC
TmcDoDlg(pdlg, hcab)
/*
  -- Dialog interpreter procedure.
  -- save old SDM state
  -- create a dialog tree (tree of windows), return tmcCancel if no memory
  -- call DialogBox() to start dialog
*/
VOID *pdlg;	/* Really PDLG */
HCAB hcab;
	{
	StartPublic();
	TMC tmc;
	SDS sdsSave;			/* save old state for re-entrancy */

	Debug(AssertTableOrder());

	/* re-entrant SDM */
	sdsSave = sdsCur;

	/* our global handle to the current dialog cab */
	sdsCur.hcab = hcab;
	sdsCur.pdlg = pdlg;

	if ((sdsCur.pwndDialog = PwndBuildDialog(pdlg)) == NULL)
		{
		/* out of memory, restore old state & return */
		sdsCur = sdsSave;
		ReturnPublic(tmcCancel, TMC);
		}

	tmc = DialogBox(sdsCur.pwndDialog, SdmDialogProc);
	FreeWork(sdsCur.pbWorkBuffer);

	/* restore old state */
	sdsCur = sdsSave;
	ReturnPublic(tmc, TMC);
	}



STATIC PWND
PwndAllocArc(parc, tmt)
/*
  -- allocate space for a window of given size and type
  -- zero fill, enable window
   special tmt's:
		tmtNull		: allocate the entire dialog window
		tmtNormalMax	: allocate a scroll bar window
*/
PARC parc;
WORD tmt;
	{
	REGISTER PWND pwnd;
	REGISTER TCI *ptci = &mptmttci[tmt];

	Assert(tmt <= tmtNormalMax);

	/* allocate room for window + any extra words */
 	pwnd = (PWND) pbWorkCur;
	pbWorkCur += ptci->cbWnd;
	Assert(pbWorkCur <= pbWorkMac);

	FillBuf((char *) pwnd, 0, ptci->cbWnd);
	pwnd->arcWindow	= *parc;
	pwnd->fEnabled	= TRUE;		/* default enabled */
	pwnd->style	= ptci->style;
	return(pwnd);
	}



STATIC PWND
PwndBuildDialog(pdlg)
/*
  -- build the dialog window tree
  -- return the top window, or NULL if out of memory
*/
PDLG pdlg;
	{
	/* Dialog Box info */
	REGISTER PWND	pwndDialog;
	WORD		ddxDialog, ddyDialog;	/* amount to grow by */
	ARC		arcDialog;
	AX		axLeft;
	AY		ayTop;

	/* General TM info */
	WORD		tmcNext;
	WORD		ctmSkip;
	/* TM scanning variables */
	REGISTER PTM	ptm;
	PTIF		ptif;
	WORD		tmt;			/* type */
	WORD		tmc;
	ARC		arcTm;

	/* Arg info */
	WORD		iagHandle;
	WORD		iagNonHandle;
	/* Radio Buttons */
	PWND		pwndRadioBase;		/* base for radio buttons */
	PWND		pwndRadioPrev;		/* previous radio buttons */
	TMC		tmcRadioLast;

	WORD		cbNeed;			/* the number of bytes needed */

	/* consistency variables */
	BOOL		fControlProc;		/* do we have a control proc ?*/

	/* Init */
	tmcNext		= tmcSysMin;	/* start here */
	ddxDialog	= ddyDialog = 0;

	Debug(pwndRadioBase = NULL);
	Debug(pwndRadioPrev = NULL);

	cbNeed = (cwWnd + cwExtraDialog) * sizeof(WORD);
			/* room for the actual dialog box */

	/* scan the array of TM's to
		a) find out must extra work buffer space we will need
		b) find sdsCur.ctif
	*/

	sdsCur.ctif = 0;
	for (ptm = pdlg->rgtm; (tmt = ptm->tmtBase) != tmtEnd; ptm++)
		{
		REGISTER TCI *ptci;

		if (FSpecialTmt(tmt))
			{
			if (tmt == tmtConditional)
				{
				/* check subdialog for larger rectangle */
				/* Must have a CAB */
				AssertSz(sdsCur.hcab != NULL,
				    "Sub Dialog without CAB");
				if (FUseSubDialog(ptm,
				    PcabxOfCab(sdsCur.hcab)->sab))
					{
#ifdef LATER /* resizing dialogs */
					if (PtmoOfTm(ptm)->ddxDialog > ddxDialog)
						ddxDialog =
						    PtmoOfTm(ptm)->ddxDialog;
					if (PtmoOfTm(ptm)->ddyDialog > ddyDialog)
						ddyDialog =
						     PtmoOfTm(ptm)->ddyDialog;
#endif /*LATER*/
					}
				}
			continue;
			}

		/* standard stuff for all items ************************/

		Assert(tmt < tmtNormalMax);
		ptci = &mptmttci[tmt];

		cbNeed += ptci->cbWnd;
		if (ptci->cbAdditional != cbRect)
			cbNeed += ptci->cbAdditional;
		else
			{
			/* figure extra size from ARC requirements */
			cbNeed += (PcrcOfTm(ptm)->dx + 2) & ~1;
			if (ptci->style & WS_BORDER)
				cbNeed -= 2;	/* because of border */
			}

		if (tmt != tmtDummyText)
			sdsCur.ctif++;
		}

	Assert(sdsCur.ctif != 0);

	/* figure out new dialog size */
	arcDialog.axRight  = (arcDialog.axLeft = pdlg->crcDlg.x)
	    + pdlg->crcDlg.dx + (BYTE) ddxDialog;
	arcDialog.ayBottom = (arcDialog.ayTop = pdlg->crcDlg.y)
	    + pdlg->crcDlg.dy + (BYTE) ddyDialog;

	cbNeed += (sdsCur.ctif + 1) * sizeof(TIF);
	pbWorkCur = sdsCur.pbWorkBuffer = PbAllocWork(cbNeed);
	if (pbWorkCur == NULL)
		return NULL;

	Debug(pbWorkMac = sdsCur.pbWorkBuffer + cbNeed);

	/* Now fill in the real stuff ************************/

	/* allocate the TIF info */
	ptif = sdsCur.rgtif = (PTIF) pbWorkCur;
	pbWorkCur += (sdsCur.ctif + 1) * sizeof(TIF);
	/* fill last tif with guard info */
	sdsCur.rgtif[sdsCur.ctif].ptm = NULL;
	sdsCur.rgtif[sdsCur.ctif].tmc = tmcNull;

	/* allocate main dialog window */
	pwndDialog = PwndAllocArc(&arcDialog, tmtDialog /*special*/);
	Assert(pwndDialog->style & WS_BORDER);

	/* Adjust for larger displays -- assumes dialogs are laid out to
	   look nice on an 80x25 display */
	if (pwndDialog->arcWindow.ayBottom < 25)	/* if small dialog */
		MoveWindow(pwndDialog,
		    pwndDialog->arcWindow.axLeft + ((axMac - 80) >> 1),
		    pwndDialog->arcWindow.ayTop + ((ayMac - 25) >> 1));

	pwndDialog->pfnWndProc = DialogWndProc;
	if (pdlg->bpTitle != NULL)
		{
		/* title for Window -- just point into dialog template */
		/* NOTE : kludge for BASE pointer */
		pwndDialog->szDialog = ((WORD) pdlg) + pdlg->bpTitle;
		}
	ValidateWindow(pwndDialog);

	axLeft	= pwndDialog->arcClipping.axLeft;
	ayTop	= pwndDialog->arcClipping.ayTop;

	/* prepare for real scan of tm's */
	Assert(ptif == sdsCur.rgtif);
	Assert(sdsCur.hcab != NULL);
	iagHandle	= 0;
	iagNonHandle	= PcabxOfCab(sdsCur.hcab)->cabh.cagHandle;
				/* check later */

	/* go thru all tm's, constructing the dialog template */
	ctmSkip = 0;
	for (ptm = pdlg->rgtm; (tmt = ptm->tmtBase) != tmtEnd; ptm++)
		{
		REGISTER PWND pwnd;
		PTM ptmSpecial;

		if (FSpecialTmt(tmt))
			{
			if (tmt == tmtConditional &&
			    !FUseSubDialog(ptm, PcabxOfCab(sdsCur.hcab)->sab))
				{
				/* don't use it */
				ctmSkip = PtmoOfTm(ptm)->ctmNormal;
				}
			continue;
			}

		/* standard stuff for all items ************************/

		/* Make rrc into an arc */
		arcTm.axRight	= (arcTm.axLeft = PcrcOfTm(ptm)->x + axLeft)
		    + PcrcOfTm(ptm)->dx;
		arcTm.ayBottom	= (arcTm.ayTop = PcrcOfTm(ptm)->y + ayTop)
		    + PcrcOfTm(ptm)->dy;

		pwnd = PwndAllocArc(&arcTm, tmt);	/* zero filled */
		pwnd->aclDialog = aclNil;

		ValidateWindow(pwnd);

		if (ptif->fReal = (ctmSkip == 0))
			{
			/* real window */
			AddChild(pwndDialog, pwnd);
			}
		else
			{
			/* fake window (skip due to sub-dialogs) */
			ctmSkip--;
			}

		if (tmt == tmtDummyText)
			{
			/* Dummy text does not have TMCs, extensions or
			  or anything special */
			Assert(!FExtensionTmt((ptm+1)->tmtBase));
			Assert(ptm->bpString != NULL &&
			    ptm->bpString != bpStringFromCab);

			pwnd->id = tmcNull;
			ptif--;	/* counteract ending ptif++ */
			goto FillDummyText;
			}

		/* scan ahead for extension records ********************/

		tmc = tmcNext;		/* next tmc in a row */
		Assert(ptif - sdsCur.rgtif == tmcNext - tmcSysMin);
			/* itif & tmc have weird arrangement */
		tmcNext++;

		fControlProc = FALSE;
		for (ptmSpecial = ptm+1;
		    FExtensionTmt(ptmSpecial->tmtBase);
		    ptmSpecial++)
			{
			switch (ptmSpecial->tmtBase)
				{
			default:
				Assert(FALSE);
				break;
			case tmtExtension1:
				/* Extension 1 must be right after normal */
				Assert(ptmSpecial == ptm+1);
				if (Ptm1OfTm(ptmSpecial)->tmcImport != tmcNull)
					{
					/* imported TMC */
					tmc = Ptm1OfTm(ptmSpecial)->tmcImport;
					AssertSz(tmc < tmcSysMin ||
					    tmc >= tmcSysMax,
					    "invalid Imported TMC");
					}

				fControlProc = Ptm1OfTm(ptmSpecial)->pfnCtl != NULL;
				break;

			case tmtExtension2:
				Assert(FALSE);	/* do later */
				break;

			case tmtExtension3:
				Assert(FALSE);	/* do later */
				break;

				}
			}
		/* fill in default info ********************************/

		/* Cursor : on, in upper left */
		pwnd->fCursorOn = TRUE;
		pwnd->axCursor = pwnd->arcClipping.axLeft;
		pwnd->ayCursor = pwnd->arcClipping.ayTop;
		pwnd->id = tmc;

		/* fill in TIF info ***********************************/

		ptif->tmc  = tmc;
		ptif->ptm  = ptm;
		ptif->pwnd = pwnd;

		/* check for string from cab */
		ptif->iagText = (ptm->bpString == bpStringFromCab) ?
		    iagHandle++ : iagNil;

		ptif->iagMain = iagNil;	/* assume no cab arg */

		/* special init cases ********************************/

		/* Vertical Scroll Bar */
		if (pwnd->style & WS_VSCROLL)
			{
			PWND pwndScroll;

#ifdef LISTBOX_ONELINE
			/* if only 1 line (+border) then no scroll bar */
			{
#ifdef LISTBOX_NOSCROLL
			extern BYTE fNoScroll;
			if (arcTm.ayBottom == arcTm.ayTop + 3 || fNoScroll)
#else
			if (arcTm.ayBottom == arcTm.ayTop + 3)
#endif
				{
				/* turn off style as well as eating allocated memory */

				pwnd->style &= ~WS_VSCROLL;
				pbWorkCur += (cwWnd + cwExtraScroll) * sizeof(WORD);
				Assert(pbWorkCur <= pbWorkMac);
				goto skip_add_scroll;
				}
			}
#endif /*LISTBOX_ONELINE*/

			/* vertical scroll bar on right hand side */
			arcTm.axLeft = arcTm.axRight - daxBorder;
			/* less top&bottom corners */
			arcTm.ayTop += dayBorder;
			arcTm.ayBottom -= dayBorder;

			pwndScroll = PwndAllocArc(&arcTm, tmtScroll);
			pwndScroll->pfnWndProc = ScrollBarWndProc;
			pwndScroll->ctickRepSb = ctickRepScrollDefault;
			ValidateWindow(pwndScroll);
			AddChild(pwnd, pwndScroll);
#ifdef LISTBOX_ONELINE
skip_add_scroll:	;
#endif /*LISTBOX_ONELINE*/
			}


		switch (tmt)
			{
		default:
			Assert(FALSE);
			break;

		case tmtStaticText:
		case tmtFormattedText:
		case tmtTrackingText:
FillDummyText:	/* case tmtDummyText: */
			Assert((pwnd->style & WS_TYPE) == WS_STATIC ||
			    (pwnd->style & WS_TYPE) == WS_STATIC_NOACCEL);
#ifndef KANJI	/* kanji mucks with coordinates */
			Assert(PcrcOfTm(ptm)->dx
			 == (pwnd->arcClipping.axRight - pwnd->arcClipping.axLeft));
						/* size must be exact */
#endif /*!KANJI*/
			pwnd->pfnWndProc = StaticWndProc;
			pwnd->fCursorOn = FALSE;

			Assert(SS_LEFT == 0);
			if (ptm->fNotLeftJustify)
				{
				pwnd->style |= (ptm->fNotRightJustify) ?
				    SS_CENTER : SS_RIGHT;
				}

			goto init_string;

#ifndef MUCH_LATER
		case tmtToggleBox:
		case tmtToggleButton:
			AssertSz(FALSE, "Toggleboxes/buttons not yet");
#endif
		case tmtGroupBox:
			pwnd->fEnabled = FALSE;
#ifndef KANJI	/* kanji mucks with coordinates */
			/* size check : with border */
			Assert(PcrcOfTm(ptm)->dx - 2
			 == (pwnd->arcClipping.axRight - pwnd->arcClipping.axLeft));
#endif /*!KANJI*/
			goto init_button;

		case tmtRadioButton:
			if (!ptif->fReal)
				{
				/* not a real button => don't link in */
				if (ptm->fFirstButton)
					iagNonHandle++;	/* bump iag anyway */
				goto init_button;
				}
			if (ptm->fFirstButton)
				{
				/* first button in group */
				pwndRadioBase = pwnd;
				pwnd->wButton = wButtonFirstRadio;
				ptif->iagMain = iagNonHandle++;
				tmcRadioLast  = tmc;
				}
			else
				{
				/* not first button */
				Assert(pwndRadioBase != NULL);
				Assert(pwndRadioPrev != NULL);
				pwndRadioPrev->pwndButtonNext = (WORD) pwnd;
				/* take into account imported tmcs */
				pwnd->id = ptif->tmc = ++tmcRadioLast;
				}
			/* fill with start (this will hook the last button) */
			pwnd->pwndButtonNext = (WORD) pwndRadioBase;
			pwndRadioPrev = pwnd;
			goto init_button;

		case tmtCheckBox:
			ptif->iagMain = iagNonHandle++;
			/* fall through to pushbutton */
		case tmtPushButton:
			Assert(pwnd->wButton == wButtonInit);
init_button:
			Assert(pwnd->style & WS_BUTTON);
#ifndef KANJI	/* kanji mucks with coordinates */
			Assert(PcrcOfTm(ptm)->dx
			 == (pwnd->arcWindow.axRight - pwnd->arcWindow.axLeft));
							/* size must be exact */
#endif /*!KANJI*/
			pwnd->pfnWndProc = ButtonWndProc;
			if (ptm->fDefaultPushButton)
				pwnd->style |= BS_DEFPUSHBUTTON;
init_string:
			if (ptm->bpString == bpStringFromCab)
				{
				/* string data from CAB : fill in later */
				Assert(ptif->iagText != iagNil);
				}
			else
				{
				/* normal base pointer */
				Assert(ptif->iagText == iagNil ||
				    tmt == tmtDummyText);
				}
			AllocDialogString(pwnd,
			    (ptm->bpString > bpStringFromCab) ?
			    (char *) ((WORD) ptm + ptm->bpString) : szEmptyString);
			break;

		case tmtListBox:
			Assert(pwnd->style & WS_LISTBOX);
			/* must have listbox proc or be directory */
			Assert(fControlProc || ptm->fDirListBox);
			Assert(!(fControlProc && ptm->fDirListBox));

			pwnd->pfnWndProc = ListBoxWndProc;
			pwnd->axCursor++;
			pwnd->isaColor = isaListBox;
			pwnd->isaHiliteColor = isaListBoxSelection;
			pwnd->ctickRepLb = ctickRepList;
			if (!ptm->fComboListBox)
				{
				ptif->iagMain = iagNonHandle++;
				}
#ifdef LISTBOX_DIR	/* Dir and or sorted */
#ifdef DEBUG /* remove later */
			if (ptm->fDirListBox)
				{
				AssertSz(ptm->fSortedListBox, "old SDM template format");
				}
#endif
			if (ptm->fSortedListBox)
				pwnd->style |= LBS_SORT;

			/* save listbox proc (if there is one) */
#endif /*LISTBOX_DIR*/
			PwfnCtlLb(pwnd) = Ptm1OfTif(ptif)->pfnCtl;
			break;

		case tmtEdit:
			Assert(pwnd->style & WS_EDIT);
			/* should set ES_ styles if we supported them */
			pwnd->pfnWndProc = EditWndProc;
			/* allocate string for edit control for work buffer */
#ifdef EDIT_FULLMGR
			InitEditWnd(pwnd, pbWorkCur,
				    (cwEditBuffer  * sizeof (WORD)) +
				    sizeof (EF) + sizeof (LD));
			pbWorkCur += (cwEditBuffer * sizeof (WORD)) +
				     sizeof (EF) + sizeof (LD);
#else
			pwnd->szDialog = (WORD) pbWorkCur;
			pwnd->cchDialog = cwEditBuffer * sizeof(WORD) - 1;
			Assert(pwnd->cchDialog == 255);	/* must be 255 */
			pwnd->chFillDialog = (WORD) chFillEdit;
			pwnd->isaEb = isaEdit;	/* set variable color */
			pwnd->isaSelEb = isaHiliteEdit;
			pwnd->cchMaxEb = pwnd->cchDialog;
			pbWorkCur += cwEditBuffer * sizeof(WORD);
#endif /*EDIT_FULLMGR*/

#ifdef EDIT_SECRET
			/* just SecretEdit for now */
			if (ptm->fCharValidated)
				pwnd->style |= ES_SECRET;
#endif /*EDIT_SECRET*/

			Assert(pbWorkCur <= pbWorkMac);
			ptif->iagMain = fControlProc ? iagNonHandle++ :
			    iagHandle++;
			break;

		case tmtStandardButton:
			Assert(pwnd->wButton == wButtonInit);
			Assert(pwnd->style & WS_BUTTON);
#ifndef KANJI	/* kanji mucks with coordinates */
			Assert(PcrcOfTm(ptm)->dx
			 == (pwnd->arcWindow.axRight - pwnd->arcWindow.axLeft));
							/* size must be exact */
#endif /*!KANJI*/
			Assert(ptm->bpString == NULL && ptif->iagText == iagNil);
			AllocDialogString(pwnd,
			    ptm->fStandardCancel ? szCancelString : szOkString);
			pwnd->pfnWndProc = ButtonWndProc;
			if (ptm->fStandardCancel)
				{
				/* Cancel */
				tmc = tmcCancel;
				}
			else
				{
				/* OK */
				tmc = tmcOk;
				Assert(ptm->fDefaultPushButton);
				pwnd->style |= BS_DEFPUSHBUTTON;
				}
			/* change tmc */
			pwnd->id = ptif->tmc = tmc;
			break;

		case tmtGeneralPicture:
			pwnd->fEnabled = FALSE;
			pwnd->pfnWndProc = GeneralWndProc;
			AssertSz(FExtension1(ptif), "General Picture without wnd proc");
			Assert(Ptm1OfTif(ptif)->pfnCtl != NULL);
			PwfnCtlGeneral(pwnd) = Ptm1OfTif(ptif)->pfnCtl;
			break;

			} /*switch*/

		ptif++;
		}

	/* all real tm's better be filled in "rgtif" array */
	Assert(ptif - sdsCur.rgtif == sdsCur.ctif);

	/* the work buffer should be all used up */
	AssertSz(pbWorkCur == pbWorkMac, "SDM alloc fail");

	/* the CAB & DialogTemplate better match */
#ifdef PROJECT_WORKS
	/* WORKS wan't to be sleazy and use overallocated Handles */
	AssertSz(PcabxOfCab(sdsCur.hcab)->cabh.cagHandle >= iagHandle,
	    "CAB / DLG mismatch (handles)");
#else
	AssertSz(PcabxOfCab(sdsCur.hcab)->cabh.cagHandle == iagHandle,
	    "CAB / DLG mismatch (handles)");
#endif
	AssertSz(PcabxOfCab(sdsCur.hcab)->cabh.cwData >= iagNonHandle,
	    "CAB / DLG mismatch (non-handles)");

	return(pwndDialog);
	}



STATIC VOID
AllocDialogString(pwnd, szText)
/*
  -- allocate string space for string specified by szText
  -- need room for largest text + zero terminator
  -- strip sz from szText to fill in acl
*/
REGISTER PWND pwnd;
char *szText;
	{
	pwnd->szDialog = (WORD) pbWorkCur;
	pwnd->cchDialog = pwnd->arcClipping.axRight - pwnd->arcClipping.axLeft;

	Assert(sizeof(WORD) == 2);
	pbWorkCur += (pwnd->cchDialog + 2) & ~1;	/* + 1 & make even */
	Assert(pbWorkCur <= pbWorkMac);

	Assert(pwnd->aclDialog == aclNil);
	SetDlgItemText(pwnd, szText, FALSE);
	}



STATIC HCAB
HcabFromDialog()
/*
  -- Allocates a cab and fills it in with values from the specified dialog box.
  -- get size info from global CAB
*/
	{
	REGISTER PTIF	ptif;
	WORD		ctif;

	HCAB		hcab;		/* work CAB */

	CABX *		pcabxSrc;	/* original */
	CABX *		pcabxDest;	/* destination */
	WORD		cagHandle;	/* # of handles in CAB */
	WORD		cwData;		/* # of words of data */

	pcabxSrc = PcabxOfCab(sdsCur.hcab);
	cagHandle = pcabxSrc->cabh.cagHandle;
	cwData = pcabxSrc->cabh.cwData;
	/* allocate a new cab the size of the original cab */
	if ((hcab = HcabAlloc((cagHandle << 8) + cwData)) == NULL)
		{
		/* out of memory - stay in dialog */
		return NULL;
		}

	/* headers must be the same */
	pcabxSrc = PcabxOfCab(sdsCur.hcab);
	pcabxDest = PcabxOfCab(hcab);
	Assert(pcabxDest->cabh.cagHandle == cagHandle);
	Assert(pcabxDest->cabh.cwData == cwData);

	/* copy data from old to new : skip over handles */
	pcabxDest->sab = pcabxSrc->sab;
	bltbyte(&pcabxSrc->rgh[cagHandle], &pcabxDest->rgh[cagHandle],
	    (cwData - cagHandle) * sizeof(WORD));

	/* go thru all tm's and store dialog values into cab */
	for (ctif = sdsCur.ctif, ptif = sdsCur.rgtif; ctif--; ptif++)
		{
		REGISTER PTM	ptm;
		PTM		ptmNext;	/* next item */
		WORD *		pwArg;		/* pointer to arg */

		/* first restore text back in CAB */
		if (ptif->iagText != iagNil)
			TextToCab(hcab, ptif->pwnd, ptif->iagText);

		if (ptif->iagMain == iagNil)
			continue;	/* no arg */

		pwArg = &PcabxOfCab(hcab)->rgh[ptif->iagMain];
		ptm = ptif->ptm;
		ptmNext = (ptif+1)->ptm;	/* next or end guard */

		switch (ptm->tmtBase)
			{
		default:
			continue;

		case tmtEdit:
#ifdef LISTBOX_DIR
			if (ptmNext != NULL &&
			     ptmNext->tmtBase == tmtListBox &&
			     ptmNext->fDirListBox)
				{
				/* Dir => Combo Dir */
				AssertSz(ptmNext->fComboListBox,
				   "Dir Listbox must be ComboDir");

				/* A Combo Dir ListBox, ptm=>edit item */
				if (!FFillComboDir(ptif+1, FALSE))
					{
					/* don't exit */
					FreeCab(hcab);
					return ((HCAB) -1);
					}
				}
#endif /*LISTBOX_DIR*/
			if (!FExtension1(ptif) ||
			    Ptm1OfTif(ptif)->pfnCtl == NULL)
				{
				/* no parse proc : normal edit */
				TextToCab(hcab, ptif->pwnd, ptif->iagMain);
				}
			else if (!FParseToCabTif(hcab, ptif))
				{
				/* error when parsing */
				FreeCab(hcab);
				return(NULL);
				}
			break;

		case tmtCheckBox:
			*pwArg = WButtonChecked(ptif->pwnd);
			break;

		case tmtRadioButton:
			/* must be first in group */
			if (ptm->fFirstButton);
			*pwArg = GetRadVal(ptif);
			break;

		case tmtListBox:
#ifdef LISTBOX_DIR
			if (ptm->fDirListBox)
				{
				if (ptm->fComboListBox)
					{
					continue;
					/* no CAB entry for combo directory
					    list boxes */
					}
				else
					{
#ifdef REMOVE_LATER
					... dir/drive listbox (dummy cab)...
					char szBuffer[cchDirMax];
					DlgDirSelect(ptif->pwnd, szBuffer,
					    (ptif+2)->ptm->tmtBase == tmtListBox ?
					    (ptif+2)->pwnd : NULL);
					SzToCab(hcab, szBuffer, ptif->iagMain);
#endif
					}
				}
			else
#endif /*LISTBOX_DIR*/
				{
				/* we assume the right val for SendMessage */
				*pwArg = (WORD) SendMessageShort(ptif->pwnd, LB_GETCURSEL);
				}
			break;

			}
		}
	return(hcab);
	}



STATIC BOOL
FSetDialogFromCab()
/*
  -- sets field values in dialog box with values in cab
  -- get global info from sdsCur
  -- returns false if initial selection was made
*/
	{
	WORD tmcSel;

	if (sdsCur.hcab != NULL)
		{
		REGISTER PTIF	ptif;
		WORD		ctif;

		for (ctif = sdsCur.ctif, ptif = sdsCur.rgtif; ctif--; ptif++)
			{
			REGISTER PTM	ptm;
			WORD		wArg;

			/* check for CAB text */
			if (ptif->iagText != iagNil)
				TextFromCab(ptif->pwnd, ptif->iagText);

			/* note : wArg will be bogus if iagMain == iagNil */
			ptm = ptif->ptm;
			wArg = (ptif->iagMain == iagNil) ? uNinch :
			    PcabxOfCab(sdsCur.hcab)->rgh[ptif->iagMain];

			switch (ptm->tmtBase)
				{
			default:
				continue;

			case tmtEdit:
				Assert(ptif->iagMain != iagNil);

				if (!FExtension1(ptif) ||
				    Ptm1OfTif(ptif)->pfnCtl == NULL)
					TextFromCab(ptif->pwnd, ptif->iagMain);
				else
					FormatFromCabTif(sdsCur.hcab, ptif);
				break;

			case tmtCheckBox:
				Assert(ptif->iagMain != iagNil);
				CheckDlgButton(ptif->pwnd, wArg, FALSE);
				break;

			case tmtRadioButton:
				/* must be first in group */
				if (wArg != uNinch)
					{
					Assert(ptif->ptm->fFirstButton);
					Assert(ptif->iagMain != iagNil);
					CheckRadioButton(ptif->pwnd, wArg, FALSE);
					}
				break;

			case tmtListBox:
				/* wArg may be bogus (if Combo) */
#ifdef DEBUG
				if (ptif->ptm->fComboListBox)
					{Assert(ptif->iagMain == iagNil);}
				else
					{Assert(ptif->iagMain != iagNil);}
#endif
				FillListBoxTif(ptif, wArg);
				break;
				} /*switch*/
			}
		}

	/* make initial selection */
	if ((tmcSel = sdsCur.pdlg->tmcSelInit) != tmcNull)
		{
		SetTmcSel(tmcSel, 0, 0x7fff);
		return(FALSE);
		}
	return(TRUE);
	}



STATIC VOID
TextToCab(hcab, pwnd, iag)
/*
  -- get string from Dialog item (window pwnd) put in CAB
*/
HCAB hcab;		/* destination CAB */
PWND pwnd;		/* window to place string in */
WORD iag;		/* arg in default CAB to get string from */
	{
	char sz[cwEditBuffer * sizeof (WORD)];

	Assert(hcab != NULL);
	Assert(pwnd != NULL);
	Assert(iag != iagNil);

	GetDlgItemText(pwnd, sz, sizeof(sz)-1);
	SzToCab(hcab, sz, iag);
	}



STATIC VOID
TextFromCab(pwnd, iag)
/*
  -- get string from default CAB, put in dialog item
*/
PWND pwnd;		/* window to place string in */
WORD iag;		/* arg in default CAB to get string from */
	{
	char sz[cwEditBuffer * sizeof (WORD)];

	Assert(sdsCur.hcab != NULL);
	Assert(pwnd != NULL);
	Assert(iag != iagNil);

	SzFromCab(sdsCur.hcab, sz, sizeof(sz), iag);
	SetDlgItemText(pwnd, sz, FALSE);
	}



STATIC BOOL
FParseToCabTif(hcab, ptif)
/*
  -- parses sz at itm using parse function *pfn.
  -- return TRUE if no error
*/
HCAB hcab;
REGISTER PTIF ptif;
	{
	char	sz[cwEditBuffer * sizeof (WORD)];
	WORD	cwVal;
	PWFN_CTL pfnCtl;
	TMC	tmc;

	/* Item must have an parse proc */
	AssertExtension1(ptif);
	pfnCtl = Ptm1OfTif(ptif)->pfnCtl;
	Assert(pfnCtl != NULL);

	tmc	= ptif->tmc;

	GetDlgItemText(ptif->pwnd, sz, sizeof(sz)-1);
	cwVal = (*pfnCtl)(tmmCwVal, sz, NULL, tmc, wParamLater, 0);
#ifdef LATER
	...allow arbitrary fixed & variable length things in CAB
#endif
	if (cwVal == 1)
		{
		/* parse value and store it into cab at iag */
		Assert(ptif->iagMain != iagNil);
		Assert(ptif->iagMain >= PcabxOfCab(hcab)->cabh.cagHandle);

		if (!(*pfnCtl)(tmmParse, sz, (WORD) hcab, tmc, wParamLater,
		    (WORD) &((CABX *) 0)->rgh[ptif->iagMain]))
			{
			/* parse error */
			return(FALSE);
			}
		}
	else
		{
		char **hval;

		if ((hval = HeapAlloc(cwVal * sizeof(int))) == NULL)
			{
			OutOfMemory();
			return FALSE;
			}

		/* parse large value and store it into cab at iag */
		if (!(*pfnCtl)(tmmParse, sz, (WORD) hval, tmc, wParamLater, 0))
			{
			/* parse error */
			HeapFree(hval);
			return FALSE;
			}

		/* ??????? real sleazy way to put argument into cab */
		RgbToCab(hcab, *hval, cwVal * 2, ptif->iagMain);
		HeapFree(hval);
		}
	return TRUE;
	}



STATIC VOID
FormatFromCabTif(hcab, ptif)
/*
  -- format information into CAB
*/
HCAB hcab;
REGISTER PTIF ptif;
	{
	REGISTER PCABX pcabx;
	WORD iag;
	char sz[cwEditBuffer * sizeof (WORD)];
	PWFN_CTL pfnCtl;

	/* Item must have an parse proc */
	/* Item must have an parse proc */
	AssertExtension1(ptif);
	pfnCtl = Ptm1OfTif(ptif)->pfnCtl;
	Assert(pfnCtl != NULL);

	pcabx = PcabxOfCab(hcab);
	iag = ptif->iagMain;
	if (iag < pcabx->cabh.cagHandle)
		{
		/* item with a handle */
		(*pfnCtl)(tmmFormat, sz, (WORD) &pcabx->rgh[iag], ptif->tmc,
		    wParamLater, 0);
		}
	else
		{
		(*pfnCtl)(tmmFormat, sz, (WORD) hcab, ptif->tmc, wParamLater,
		    (WORD) &((CABX *) 0)->rgh[ptif->iagMain]);
		}
	SetDlgItemText(ptif->pwnd, sz, FALSE);
	}



STATIC DWORD FARPUBLIC
SdmDialogProc(pwnd, message, tmc, lParam)
/*
  -- Dialog procedure for SDM dialogs
*/
PWND pwnd;
WORD message;
TMC tmc;		/* wParam = pwnd->id */
DWORD lParam;
	{
	REGISTER PTM ptm;
	PTIF	ptif;
	BOOL	fDismiss;
	DLM	dlm = dlmClick;

#ifdef	DIALOG_NOSAVE
	Assert(message == WM_PAINT || pwnd == sdsCur.pwndDialog);
#else	// DIALOG_NOSAVE
	Assert(pwnd == sdsCur.pwndDialog);
#endif	// DIALOG_NOSAVE

	switch (message)
		{

	default:
		return FALSE;

	case WM_DIALOG_SETFOCUS:
		if (FExistsDialogProc())
			FCallDialogProc(dlmSetFocus, tmc, 0, 0, 0);
		return FALSE;

	case WM_DIALOG_KILLFOCUS:
		if (FExistsDialogProc())
			FCallDialogProc(dlmKillFocus, tmc, 0, 0, 0);
		return FALSE;

	case WM_DIALOG:
#ifdef HELP_BUTTON
		if (tmc == tmcHelp)
			{
			tmc = 0;	/* No shift states for Help */
			goto do_help;
			}
#endif
		break;		/* fall through */

	case WM_INITDIALOG:
		{
		/* Initializing dialog */
		BOOL fRet;
		fRet = FSetDialogFromCab();
		if (FExistsDialogProc())
			{
			FCallDialogProc(dlmInit, tmcNull, 0, 0,
			    (WORD) sdsCur.hcab);
			}
		return(fRet);
		}
		/*break*/

	case WM_HELP:
#ifdef HELP_BUTTON
do_help:
#endif
		/* really the tmc contains the KK value */
		Help(hemDialog, sdsCur.pdlg->hid, sdsCur.pdlg, tmc);
		return FALSE;
		/*break*/

	case WM_CHAR:
		dlm = dlmKey;
		goto send_dlm;

	case WM_IDLE:
#ifdef DUAL
		/* for DOS 5, in idle return TRUE (=> dialog processor
		*  will put us to sleep
		*/
		if (fProtectMode && !FExistsDialogProc())
			return TRUE;		/* default idle */
#else /*!DUAL*/
#ifdef DOS5
		/* for DOS 5, in idle return TRUE (=> dialog processor
		*  will put us to sleep
		*/
		if (!FExistsDialogProc())
			return TRUE;		/* default idle */
#endif
#endif /*!DUAL*/
		dlm = dlmIdle;

send_dlm:
		/* call the dialog proc -- if it returns FALSE, cancel dialog */
		if (FExistsDialogProc() &&
		    !FCallDialogProc(dlm, tmc, tmc, 0, 0))
			{
			/* dismiss it */
			ptif = PtifFromTmc(tmc = tmcCancel);
			ptm = ptif->ptm;
			goto DoOK;
			}
		return FALSE;
		/* break */
		}

	/* else WM_DIALOG */
	Assert(message == WM_DIALOG);

	ptif	 = PtifFromTmc(tmc);
	ptm	 = ptif->ptm;
	fDismiss = ptm->fDismiss;	/* clear if not acknowledged */

	/* process regular tmc BEFORE calling dialog proc */
	switch (ptm->tmtBase)
		{

	case tmtRadioButton:
		/* check one button, clear rest */
		CheckRadioButton(ptif->pwnd, 0, TRUE);
		break;

	case tmtListBox:
		switch (HIWORD(lParam))
			{

		default:
			Assert(FALSE);
			break;

		case LBN_SELECT_DONE:
			dlm = dlmUnclick;
			break;

		case LBN_SELCHANGE:
			/* ListBox selection changed */
			if (ptm->fComboListBox)
				{
				if ((ptif+1)->ptm->tmtBase == tmtTrackingText &&
				    (ptif+2)->ptm->tmtBase == tmtListBox)
					/* clear selection in dir/drive */
					SendMessage((ptif+2)->pwnd,
					    LB_SETCURSEL, (WORD) -1, 0L);
				SelChangeComboListBox(ptif);
				}
			else if (ptm->fDirListBox)
				{
				/* change in split dir/drives */
				Assert((ptif-1)->ptm->tmtBase == tmtTrackingText);
				Assert((ptif-2)->ptm->tmtBase == tmtListBox);
				/* clear the file selection */
				SendMessage((ptif-2)->pwnd, LB_SETCURSEL,
				    (WORD) -1, 0L);
				SelChangeComboListBox(ptif-2);
				}
			break;

		case LBN_DBLCLK:	/* Double Click in listbox */
			{
			Assert(!fDismiss);	/* not normal dismiss */
			fDismiss = (FExistsDialogProc()) ?
			    FCallDialogProc(dlmDblClk, tmc, 0, 0, wParamLater) :
			    TRUE;

			if (fDismiss)
				{
				/* Dismiss via double click */
				PWND pwndT;
				if ((pwndT =
				    PwndDefaultPushButton(pwnd)) != NULL)
					{
					/* Change item */
					ptif = PtifFromTmc(tmc = pwndT->id);
					ptm = ptif->ptm;
					goto DoOK;
					}
				}
			}
			break;

			} /*switch(LBN_)*/
		} /*switch(tmt)*/

	if (ptm->fProc)
		{
		/* call client's dialog procedure */
		BOOL	fAck;

		AssertSz(FExistsDialogProc(),
		     "SdmDialogProc: missing dialog function");

		if (ptm->tmtBase == tmtEdit)
			{
			/* Check for change message */
			dlm = (HIWORD(lParam) == EN_CHANGE) ? dlmChange :
			    0;
			}

		fAck = (dlm == 0) ||
		    FCallDialogProc(dlm, tmc, LOWORD(lParam), HIWORD(lParam),
		     wParamLater);
#ifdef LATER
		... do something with fAck
#endif
		if (fDismiss)
			fDismiss = fAck;
		}


	if (fDismiss)
		{
		/* dismiss dialog (maybe) */
		HCAB hcabT;
DoOK:

		hcabT = NULL;	/* NULL => don't place changes in CAB */

		if (!FEnabledTmc(tmc))
			return TRUE;	/* exit button not enabled */
			/* NOTE : disabling a OK/CANCEL button is dangerous */

		if (ptm->fCabDismiss && sdsCur.hcab != NULL)
			{
			/* try to fill in the CAB */
			if ((hcabT = HcabFromDialog()) == NULL)
				{
				/* and error returned, cancel the dialog */
				EndDialog(pwnd, tmcCancel);
				return TRUE;
				}
			}
			if (hcabT == (HCAB) -1)
				return TRUE;

		if (FExistsDialogProc() &&
		    !FCallDialogProc(dlmTerm, tmc, LOWORD(lParam), HIWORD(lParam), 0))
			{
			/* Dialog proc does not want us to dismiss */
			if (hcabT != NULL)
				FreeCab(hcabT);
			return TRUE;
			}

		if (hcabT != NULL)
			{
			/* replace original cab's handles */
			PCABX pcabxDest;

			FreeCabData(sdsCur.hcab);
			pcabxDest = PcabxOfCab(sdsCur.hcab);
			bltbyte((char *) PcabxOfCab(hcabT)->rgh,
			    (char *) pcabxDest->rgh,
			    pcabxDest->cabh.cwData * sizeof(WORD));
			HeapFree(hcabT);
			}

		EndDialog(pwnd, tmc);
		}
	return(TRUE);
	}



PRIVATE PTIF
PtifFromTmc(tmc)
/*
  -- return TM for a given TMC
  -- read info about TIF's in header for special mapping
*/
TMC tmc;
	{
	REGISTER PTIF ptif;
	WORD ctif;

	if (tmc >= tmcSysMin && tmc < tmcSysMax)
		{
		/* in array if at all */
		AssertSz(ItifOfTmc(tmc) < sdsCur.ctif, "invalid TMC");
		ptif = &sdsCur.rgtif[ItifOfTmc(tmc)];
		AssertSz(ptif->tmc == tmc, "invalid TMC");
		return ptif;
		}

	ptif = sdsCur.rgtif;
	ctif = sdsCur.ctif;
	while (ctif--)
		{
		if (ptif->tmc == tmc)
			return ptif;
		ptif++;
		}
	AssertSz(FALSE, "invalid TMC");
	}


/*****************************************************************************/

/*	* ListBox Utilities */



STATIC char *
SzWildCard(sz)
/*
  -- return pointer to wildcard at end of long path
  -- return empty string (not NULL) if no wild cards
*/
REGISTER char *sz;
	{
	REGISTER char ch;
	char *pchAfterSlash = sz;
	BOOL	fWild = FALSE;

	while ((ch = *sz) != '\0')
		{
		sz++;
		if (ch == '\\' || ch == '/' || ch == ':')
			pchAfterSlash = sz;
		else if (ch == '*' || ch == '?')
			fWild = TRUE;
		}

	Assert(*sz == '\0');		/* sz => empty string */
	return (fWild ? pchAfterSlash : sz);
	}



#ifdef LISTBOX_DIR

STATIC BOOL
FFillComboDir(ptif, fInit)
/*
  -- fill CAB arg with thing selected from directory
  -- return TRUE if can exit, FALSE if wildcard (or directory)
	 (don't end dialog)
  -- ptif-1 => TIF of edit item
  -- ptif   => TIF of a combo dir listbox
  -- ptif+1 => TIF of a tracking text (optional)
  -- if fInit set => always fill listbox / don't move focus
*/
REGISTER PTIF ptif;
BOOL fInit;
	{
	BOOL fCantExit;
	char szBuffer[cwEditBuffer * sizeof (WORD)];
	Debug(PTM ptmListBox = ptif->ptm);
	Debug(PTM ptmEdit = (ptif-1)->ptm);

	Assert(ptmListBox->tmtBase == tmtListBox && ptmListBox->fDirListBox);
	if (!ptif->ptm->fComboListBox)
		return TRUE;		/* directory listbox */

	/* get edit item (path name) */
	GetDlgItemText((ptif-1)->pwnd, szBuffer, sizeof(szBuffer)-1);

	if ((fCantExit = FMaybeDir(szBuffer)) || fInit)
		{
		/* re-fill */
		PWND	pwnd = ptif->fReal ? ptif->pwnd : NULL;

		if ((ptif+1)->ptm->tmtBase != tmtTrackingText)
			DlgDirList(pwnd, szBuffer, NULL, fRedrawItem, NULL);
		else
			{
			DlgDirList(pwnd, szBuffer, (ptif+1)->pwnd,
			    fRedrawItem,
			    (ptif+2)->ptm->tmtBase == tmtListBox ?
			    (ptif+2)->pwnd : NULL);
			}

		if (!fInit)
			SetFocus((ptif-1)->pwnd);
		}

	/* real path, no wild-cards */
	SetDlgItemText((ptif-1)->pwnd, szBuffer, fRedrawItem);
	return !fCantExit;
	}
#endif /*LISTBOX_DIR*/



PRIVATE VOID
FillListBoxTif(ptif, iszInit)
/*
  -- fill listbox from CAB info
*/
REGISTER PTIF ptif;
WORD iszInit;		/* initial selection : if non-combo */
	{
	Assert(ptif->ptm->tmtBase == tmtListBox);
	Assert(sdsCur.hcab != NULL);
	Assert(ptif->pwnd != NULL);

#ifdef LISTBOX_DIR
	if (ptif->ptm->fDirListBox)
		{
		/* Directory ListBox */
		FFillComboDir(ptif, TRUE);
		}
	else
#else
	Assert(!(ptif->ptm->fDirListBox));
#endif /*!LISTBOX_DIR*/
		{
		/* Non-directory ListBox */
		WORD	isz;
		WORD	csz;
		PWFN_CTL pfnCtl;
		char szBuffer[cchListTextMax];

		AssertExtension1(ptif);
		pfnCtl = Ptm1OfTif(ptif)->pfnCtl;
		Assert(pfnCtl != NULL);

		SendMessageShort(ptif->pwnd, LB_RESETCONTENT);
		csz = (*pfnCtl)(tmmCount, NULL, NULL, ptif->tmc, wParamLater, 0);
		isz = 0;

		if (ptif->ptm->fSortedListBox || csz == cszUnknown)
			{
			/* we must fill in the listbox */
			while (csz == cszUnknown || isz < csz)
				{
				if ((*pfnCtl)(tmmText, szBuffer, isz++, ptif->tmc,
				    wParamLater, 0))
					{
					/* we have a string */
					SendMessage(ptif->pwnd, LB_ADDSTRING,
					    (WORD) szBuffer, 0L);
					}
				else if (csz == cszUnknown)
					break;	/* stop filling */
				}
			}
		else
			{
			/* fill on demand (fill with empties) */
			Assert(csz != cszUnknown);
			while (csz--)
				SendMessage(ptif->pwnd, LB_ADDSTRING, 0, 0L);
			}

		/* we assume the right val for SendMessage */
		if (!ptif->ptm->fComboListBox && iszInit != iszNinchList)
			SendMessage(ptif->pwnd, LB_SETCURSEL, iszInit, 0L);
		}
	}




STATIC VOID
SelChangeComboListBox(ptif)
/*
  -- respond to combo listbox selection change
  -- ptif-1  => Edit Item
  -- ptif    => Listbox
  -- ptif+2  => second listbox (if ptif+1 => Tracking text)
*/
PTIF ptif;
	{
	char szBuffer[cchDirMax];

	/* Combo listbox must be prefixed by edit item */
	Assert(ptif > sdsCur.rgtif && (ptif-1)->ptm->tmtBase == tmtEdit);
	Assert(ptif->ptm->tmtBase == tmtListBox);

#ifdef LISTBOX_DIR
	if (ptif->ptm->fDirListBox)
		{
		char szBuffer2[cchDirMax];

		/* select new item in directory */
		if (DlgDirSelect(ptif->pwnd, szBuffer
		    ,(ptif+2)->ptm->tmtBase == tmtListBox ?
		    (ptif+2)->pwnd : NULL))
			{
			GetDlgItemText((ptif-1)->pwnd, szBuffer2,
			    sizeof(szBuffer2)-1);
			/* skip directory or drive info */
			SzAppendSDM(szBuffer, SzWildCard(szBuffer2));
			}
		}
	else
#endif /*LISTBOX_DIR*/
		{
		/* Non directory listbox */
		WORD isz;

		AssertExtension1(ptif);
		isz = (WORD) SendMessageShort(ptif->pwnd, LB_GETCURSEL);
		/* call listbox proc to get the proper text */
		if (isz != iszNinchList)
			{
			if (!(*Ptm1OfTif(ptif)->pfnCtl)(tmmEditText, szBuffer,
			    isz, ptif->tmc, wParamLater, 0))
				{
				/* get string from listbox */
				GetListText(ptif->pwnd, szBuffer, sizeof(szBuffer));
				}
			}
		else
			szBuffer[0] = '\0';

		}

	Assert(fRedrawItem);
	SetEditText((ptif - 1)->pwnd, szBuffer, TRUE);
	}

/**************************/
/*      button utilities  */

PRIVATE VOID
KillDefaultButton()
	{
	PWND pwnd = sdsCur.pwndDialog;
	PWND pwndT;

	AssertSz((pwnd != NULL),"SetDefaultTmc : no dialog");
	while ((pwndT = PwndDefaultPushButton(pwnd)) != NULL)
		{
		pwndT->style = (pwndT->style & WS_TYPE) | BS_PUSHBUTTON;
		DrawWindow(pwndT);
		}
	}
