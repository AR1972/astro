/*	SCCSWHAT() */
/*
	COW : Character Oriented Windows

	menu.c : menu handler
*/

#define COW
#include <cow.h>

#include <umenu.h>
#include <uwindow.h>
#include <uisa.h>
#include <uevent.h>
#include <vkey.h>
#include <uscreen.h>
#include <uutil.h>
#include <kkeyboar.h>

#include "dialog.h"
#include "event.h"
#include "window.h"
#include "screen.h"
#include "util.h"
#include "case.h"
#include "overlap.h"
#include "shadow.h"

#ifdef DUAL
#include <os2.h>
extern	WORD	fProtectMode;
#else
#ifdef DOS5
#include <os2.h>
#endif
#endif

#include "menu.h"
#include "_menu.h"


#ifdef KANJI
PUBLIC BOOL fKanaAccel = FALSE;		/* Kana accelerators ?? */
#endif /*KANJI*/


#define FShowMenuBar()	(pmenubarCur != NULL)
STATIC PMENUBAR pmenubarCur; 		/* current menu bar being displayed */
STATIC ARC arcMenu;
STATIC PWND pwndMenu;

STATIC BYTE FAR *lpbMenu;	/* where the screen image under menu is kept */

#ifdef ACCEL_MULTIPLE
BOOL	fOneOfMultiple;		/* => if one of multiple matching accelerators */
#endif /*ACCEL_MULTIPLE*/

#ifdef	MULTIPLE_ACTION
WORD	vkActionPrim = VK_MENU;		/* Primary Action Key */
WORD	vkActionSec = VK_F11;		/* Secondary Action Key */
WORD	vkCancelPrim = LOBYTE(VK_ESCAPE);	/* Primary Cancel Key */
WORD	vkCancelSec = VK_F12;		/* Secondary Cancel Key */
#endif	/* MULTIPLE_ACTION */

/* Menu Info : see note in MENU.H */
extern	MNI mniCur;
/* PRIVATE MNI mniCur = { imenuNil, iitemNil, NULL, FALSE, FALSE, FALSE }; */



STATIC MENU *pmenuFound;
/* set by FindMenuItem to indicate which menu contains
   the item that was found */


/* forward */
STATIC PMENUITEM PmenuitemOfPmenu(PMENU);

/* menus */
STATIC VOID	SelectMenu(WORD);
STATIC VOID	HiliteMenuTitle(BOOL);
STATIC VOID	ReportMenuInactive(void);
STATIC VOID	OpenMenuCur(void);
#ifdef	DIALOG_NOSAVE
STATIC VOID	DrawMenuCur(void);
#endif	// DIALOG_NOSAVE
STATIC VOID	CloseMenuCur(BOOL);

/* menu title selection */
STATIC VOID	HiliteMenuMnem(BOOL);
STATIC BOOL	FMakeSelection(void);
STATIC WORD	ImenuPickVk(WORD);
STATIC WORD	ImenuPickMsp(MSP);

/* menu items */
STATIC VOID	DrawMenuItem(AX, AX, AY, PMENUITEM);
STATIC VOID	HiliteItem(BOOL);
STATIC BOOL	FSelectItem(WORD);
STATIC BOOL	FMatchItemVk(WORD);
STATIC BOOL	FSelectItemMsp(MSP);
STATIC BOOL	FInsideMenuMsp(AX, AY);

/* filter */
STATIC VOID	SetIdleState(void);

/* utilities */
STATIC BOOL	FTranslateAccelerator(WORD, WORD);
STATIC BOOL	FMatchVkCh(WORD, char);
STATIC char *	SzItem(PMENUITEM);

STATIC VOID	SendMenuHelp(WORD);

#ifdef	MENU_NOSAVE
extern VOID	FAR PASCAL	ScreenRedraw(VOID);
#endif	// MENU_NOSAVE




STATIC PMENUITEM
PmenuitemOfPmenu(pmenu)
/*
  -- return address of first menu item in menu
*/
REGISTER PMENU pmenu;
	{
	return (pmenu->fHandle ? *(pmenu->u.prgmenuitem) : pmenu->u.rgmenuitem);
	}



PUBLIC PMENUITEM FARPUBLIC
FindMenuItem(id)
WORD id;
/* FindMenuItem - Find specified menu item in tree */
	{
	StartPublic();
	PMENU pmenu = pmenubarCur->rgmenu;
	WORD cmenu = pmenubarCur->cmenu;

	while (cmenu--)
		{
		REGISTER PMENUITEM pmenuitem = PmenuitemOfPmenu(pmenu);
		REGISTER WORD citem = pmenu->citem;
		
		while (citem--)
			{
			if (pmenuitem->idItem == id &&
			    !pmenuitem->fSeparator)
				{
				pmenuFound = pmenu;
				ReturnPublic(pmenuitem, PMENUITEM);
				}
			pmenuitem++;
			}
		pmenu++;
		}
	ReturnPublic(NULL, PMENUITEM);
	}




PUBLIC VOID FARPUBLIC
InitMenu(pwnd, pmenubar)
/*
  -- initialize menu state
*/
REGISTER PWND pwnd;
PMENUBAR pmenubar;
	{
	StartPublic();

	CloseMenuCur(TRUE);
	ReportMenuInactive();

	AssertSz(mniCur.imenu == imenuNil, "Init Menu while another menu still active");
	mniCur.iitem = iitemNil;
	mniCur.fMouse = FALSE;
	pwndMenu = pwnd;
	pmenubarCur = pmenubar;
	FEnableMenuBar(pwnd != NULL);
	DrawMenubar();
	StopPublic();
	}



PUBLIC VOID FARPUBLIC
DrawMenubar()
/*
  -- Draw Menu Bar and optionally fill with MENU item strings
*/
	{
	StartPublic();
	REGISTER PMENU pmenu;
	REGISTER WORD imenu;

	if (!FShowMenuBar())
		return;

	DrawThisWnd(NULL);

	FillArc(axMenuMin, ayMenu, axMenuMac, ayMenu+1, ' ', isaMenu);

	for (imenu = 0, pmenu = pmenubarCur->rgmenu;
	    imenu < pmenubarCur->cmenu;
	    imenu++, pmenu++)
		{
		WORD di = pmenu->fEnabled ? dmTextOnly :
			DiNormal(isaDisabledMenuItem);

		TextOutAbs(axMenuMin + pmenu->rxTitle, ayMenu,
		    pmenu->pchTitle, pmenu->cchTitle, di);
#ifdef KANJI
		/* kana accelerator at start ? */
		if (fKanaAccel)
			CharOutAbs(axMenuMin + pmenu->rxTitle, ayMenu,
			    pmenu->chKanaAccel, di);
#endif
		}

#ifdef DEBUG
	/* special stuff if we want to redraw menubar while not idle,
		i.e. for the HELLO application.
	*/
	if (FMenuActive())
		HiliteMenuTitle(TRUE);
#endif /*DEBUG*/

	StopPublic();
	}

/*****************************************************************************/


STATIC VOID
SelectMenu(imenu)
/*
  -- select the specified menu (may be imenuNil)
  -- clears old menu title
  -- modifies mniCur.imenu
*/
WORD imenu;
	{
	Assert(mniCur.pmenuOpen == NULL);	/* no drop downs open */

	/* if we don't have a menubar -- get one !! */
	if (pmenubarCur == NULL)
		{
		SendMessageShort(pwndMenu, WM_MENUSTART);
		AssertSz(pmenubarCur != NULL, "WM_MENUSTART error");
		}

	if (imenu != mniCur.imenu)
		{
		/* close old */
		CloseMenuCur(TRUE);		/* close current */
		HiliteMenuTitle(FALSE);	/* & clear title */
		mniCur.imenu = imenu;
		mniCur.iitem = iitemNil;
		HiliteMenuTitle(TRUE);	/* hilite new title */
		}
	}



STATIC VOID
HiliteMenuTitle(fHilite)
/*
  -- hilite/unhilite the menu title "hilite" characters, for current menu
  -- do nothing if no current menu
  -- menu should not be open.
*/
BOOL fHilite;		/* hilite ? */
	{
	REGISTER AX ax;
	REGISTER PMENU pmenu;
	AX axRight;

	if (!FShowMenuBar() || mniCur.imenu == imenuNil)
		return;

	DrawThisWnd(NULL);

	Assert(mniCur.imenu != imenuNil);
	Assert(!FMenuOpen());
	MoveHardwareCursor(0, 0, FALSE);

	/* 1 character before & after */
	pmenu = &pmenubarCur->rgmenu[mniCur.imenu];
	ax = ((AX) pmenu->rxTitle) - daxBorder;
	axRight = ax + pmenu->cchTitle + 2;
	/* Pin hilite to edges of screen */
	if (pmenu->rxTitle == 0)
		ax = 0;
	if (axRight > axMax)
		axRight = axMax;

	FillArc(ax, ayMenu, axRight, ayMenu + 1, '\0',
	     (fHilite) ? (dmAttrOnly | isaMenuSelected) :
	      ((pmenu->fEnabled) ? (dmAttrOnly | isaMenu) :
				   (dmAttrOnly | isaDisabledMenuItem) ));
	ax = ((AX) pmenu->rxTitle) + (BYTE) pmenu->ichHilite;
	Assert(ax < axMax);	/* Hilited character must be on screen */
	/* only single width characters for accelerators */
	if (mniCur.fMenuMnem)
		FillArc(ax, ayMenu, ax + 1, ayMenu + 1, '\0',
		    (fHilite) ? (dmAttrOnly | isaMenuHiliteSel) :
		     (dmAttrOnly | isaMenuHilite));

	if (fHilite)
		SendMessage(pwndMenu, WM_MENUSELECT, pmenu->idMenu,
		    MAKELONG(pmenu, mmdMenu));
	}



STATIC VOID
ReportMenuInactive()
/*
  -- send a MENUSELECT message with NULL pmenuitem
  -- normally application will clear the help line
*/
	{
	Assert(mmdItem == 0);
	if (pwndMenu != NULL)
		SendMessageShort(pwndMenu, WM_MENUSELECT);
	}




STATIC VOID
OpenMenuCur()
/*
  -- open current menu (mniCur.imenu)
  -- all other menus must be closed
  -- turn of mnemonics in menu titles
  -- if imenuNil then do nothing
  -- if items, select first one
*/
	{
	REGISTER PMENU pmenu;
	AX axLeft, axRight;
#ifdef	DIALOG_NOSAVE
	AY ayBottom, ayTop;
#else
	AY ay, ayBottom, ayTop;
	WORD citem;
#endif

	Assert(mniCur.pmenuOpen == NULL);
	DrawThisWnd(NULL);
	HiliteMenuMnem(FALSE);

	if (mniCur.imenu == imenuNil)
		return;

	pmenu = mniCur.pmenuOpen = &pmenubarCur->rgmenu[mniCur.imenu];

	SendMessage(pwndMenu, WM_INITMENUPOPUP, pmenu->idMenu,
	    MAKELONG(pmenu, 0));	/* Must not enter dialogs */

	BeginDraw();

	if (pmenu->citem)
		{
#ifndef DIALOG_NOSAVE
		REGISTER PMENUITEM pmenuitem;
#endif	// DIALOG_NOSAVE
		WORD	cw;

		arcMenu.axLeft = axLeft = (AX) pmenu->rxTitle + daxMenuBox;
			/* to left side of box */
		arcMenu.axRight = axRight = (AX) pmenu->rxTitle +
		    pmenu->cchitemMax + 2;
			/* bord+gap+"text"+gap+bord */

		/* adjust for too near screen borders */
		if (axRight > axMac - daxShadow)
			{
			/* adjust for too large (too near right) */
			arcMenu.axLeft = axLeft = axLeft - (axRight - 
						(axMac - daxShadow));
			arcMenu.axRight = axRight = (axMac - daxShadow);
			}
		if (pmenu->rxTitle < -daxMenuBox)
			{
			/* adjust for too small (too near left) */
			arcMenu.axRight = axRight - axLeft;
			arcMenu.axLeft = 0;
			}

		arcMenu.ayTop = ayTop = ayMenu + 1;
			/* 1 for menu title */
		arcMenu.ayBottom = ayBottom = ayTop + pmenu->citem + 2;
			/* 2 for top and bottom sides of menu box */

		cw = (WORD) (ayBottom - ayTop + dayShadow) *
		    (WORD) (axRight - axLeft + daxShadow + daxDbcs * 2);
#ifdef SCREEN_FFONT
		if (fFontAvailable)
			cw *= 2;
#endif /*SCREEN_FFONT*/

#ifndef MENU_NOSAVE

		SaveArc(axLeft - daxDbcs, ayTop, axRight + daxShadow + daxDbcs,
		    ayBottom + dayShadow,
		    lpbMenu = LpbAllocWorkFar(cw * 2));

#endif	// MENU_NOSAVE

#ifdef	DIALOG_NOSAVE
		/* Hilite first item if not in mouse mode */
		if (!mniCur.fMouse)
			{
			mniCur.iitem = 0;
			}
		DrawMenuCur();
#else	// !DIALOG_NOSAVE
		FillArc(axLeft, ayTop, axRight, ayBottom, ' ',
		    DiNormal(isaEnabledMenuItem));
		DrawBoxArc(&boxSingle, &arcMenu, isaMenuBox,
			TRUE, TRUE, NULL);
		ShadowArc(&arcMenu);
		pmenuitem = PmenuitemOfPmenu(pmenu);
		citem = pmenu->citem;
		ay = ayTop + 1;
		while (citem--)
			{
			if (pmenuitem->fSeparator)
				{
				/* draw a separator */
				FillArc(axLeft + daxBorder, ay,
				    axRight - daxBorder, ay + dayBorder,
				    (ACHAR) chTopSide1,
				    DiNormal(isaMenuBox));
				CharOutAbs(axRight - daxBorder, ay,
				    (ACHAR) chMiddleRight1,
				     DiNormal(isaMenuBox));
				CharOutAbs(axLeft, ay, (ACHAR) chMiddleLeft1,
				    DiNormal(isaMenuBox));
				}
			else
				{
				DrawMenuItem(axLeft, ay,
				   (AX) axRight - daxBorder, pmenuitem);
				}
			ay++;
			pmenuitem++;
			}
		/* Hilite first item if not in mouse mode */
		if (!mniCur.fMouse)
			{
			mniCur.iitem = 0;
			HiliteItem(TRUE);
			}
#endif	// DIALOG_NOSAVE
		}
	else
		{
		/* no items => no mouse selection of items */
		arcMenu.ayTop = 0;
		arcMenu.ayBottom = 1;
		mniCur.iitem = iitemNil;
		}

	EndDraw();

	/* just to be sure */
	Assert(pmenu == mniCur.pmenuOpen);
	}

#ifdef	DIALOG_NOSAVE
STATIC VOID
DrawMenuCur()
	{
	WORD citem;
	REGISTER PMENUITEM pmenuitem;
	AX axLeft = arcMenu.axLeft;
	AX axRight = arcMenu.axRight;
	AY ayTop = arcMenu.ayTop;
	AY ayBottom = arcMenu.ayBottom;
	AY ay;

	if (mniCur.pmenuOpen == NULL)
		{
		HiliteMenuTitle(TRUE);
		return;
		}

	FillArc(axLeft, ayTop, axRight, ayBottom, ' ',
	    DiNormal(isaEnabledMenuItem));
	DrawBoxArc(&boxSingle, &arcMenu, isaMenuBox,
		TRUE, TRUE, NULL);
	ShadowArc(&arcMenu);
	pmenuitem = PmenuitemOfPmenu(mniCur.pmenuOpen);
	citem = mniCur.pmenuOpen->citem;
	ay = ayTop + 1;
	while (citem--)
		{
		if (pmenuitem->fSeparator)
			{
			/* draw a separator */
			FillArc(axLeft + daxBorder, ay,
			    axRight - daxBorder, ay + dayBorder,
			    (ACHAR) chTopSide1,
			    DiNormal(isaMenuBox));
			CharOutAbs(axRight - daxBorder, ay,
			    (ACHAR) chMiddleRight1,
			     DiNormal(isaMenuBox));
			CharOutAbs(axLeft, ay, (ACHAR) chMiddleLeft1,
			    DiNormal(isaMenuBox));
			}
		else
			{
			DrawMenuItem(axLeft, ay,
			   (AX) axRight - daxBorder, pmenuitem);
			}
		ay++;
		pmenuitem++;
		}
	HiliteItem(TRUE);
	}
#endif	// DIALOG_NOSAVE


STATIC VOID
CloseMenuCur(fMessage)
BOOL fMessage;	/* send WM_COMMAND message? */
/*
  -- close the body of current menu
  -- can be called if menu not open (do nothing)
*/
	{
	if (mniCur.imenu == imenuNil || mniCur.pmenuOpen == NULL)
		return;		/* not open */

	Assert(mniCur.pmenuOpen == &pmenubarCur->rgmenu[mniCur.imenu]);

#ifndef MENU_NOSAVE

	DrawThisWnd(NULL);
	if (mniCur.pmenuOpen->citem)
		{
#ifdef KANJI
		fRestoreDbcs = TRUE;
#endif	/*KANJI*/

		RestoreArc(arcMenu.axLeft - daxDbcs, arcMenu.ayTop,
		    arcMenu.axRight + daxShadow + daxDbcs,
		    arcMenu.ayBottom + dayShadow,
		    lpbMenu);
		FreeWorkFar(lpbMenu);

#ifdef KANJI
		fRestoreDbcs = FALSE;
#endif	/*KANJI*/

		}

	mniCur.pmenuOpen = NULL;
	mniCur.iitem = iitemNil;

#else	// MENU_NOSAVE
	mniCur.pmenuOpen = NULL;
	mniCur.iitem = iitemNil;

	ScreenRedraw();
#endif	// MENU_NOSAVE

	if (fMessage)
		SendMessage(pwndMenu, WM_COMMAND, mniCur.pmenuOpen->idMenu,
				MAKELONG(mniCur.pmenuOpen,mmdMenu));
	ReportMenuInactive();
	}



/********** MENU selection **********/


STATIC VOID
HiliteMenuMnem(fOn)
/*
  -- hilite or unhilite the menu mnemonics
  -- do nothing if menu is open
  -- update global state (mniCur.fMenuMnem)
*/
BOOL fOn;
	{
	REGISTER PMENU pmenu;
	REGISTER WORD imenu;
	WORD di;

	if (FMenuOpen())
		{
		/* menu is open, this should be a no op */
		Assert(mniCur.fMenuMnem == fOn);
		return;
		}

	if (!FShowMenuBar())
		return;

	DrawThisWnd(NULL);

	for (imenu = 0, pmenu = pmenubarCur->rgmenu;
	    imenu < pmenubarCur->cmenu;
	    imenu++, pmenu++)
		{
		AX ax = axMenuMin + pmenu->rxTitle + (BYTE) pmenu->ichHilite;

		if (imenu != mniCur.imenu)
			di = (fOn) ? (dmAttrOnly | isaMenuHilite) :
				( (pmenu->fEnabled) ? (dmAttrOnly | isaMenu) :
				 	(dmAttrOnly | isaDisabledMenuItem));
		else
			{
			di = (fOn) ? (dmAttrOnly | isaMenuHiliteSel) :
			   (dmAttrOnly | isaMenuSelected);
			}

		FillArc(ax, ayMenu, ax + 1, ayMenu + 1, '\0', di);
		}
	mniCur.fMenuMnem = fOn;
	}



STATIC BOOL
FMakeSelection()
/*
  -- attempt to make a selection
  -- if applicable send messages for menu selection
  -- valid selection closes menu and returns TRUE
  -- invalid selection (i.e. disabled item) optionally beeps,
	keeps menu open, and returns FALSE
  -- note : will set menu state to idle before returning TRUE
*/
	{
	WORD id;
	DWORD lParam;
	REGISTER PMENU pmenu = mniCur.pmenuOpen;
	
	Assert(mniCur.imenu != imenuNil);
	if (mniCur.iitem != iitemNil)
		{
		PMENUITEM pmenuitem = &(PmenuitemOfPmenu(pmenu)[mniCur.iitem]);
		if (!pmenuitem->fEnabled)
			{
			/* selection of disabled item */
			Beep();
			return FALSE;
			}

		id = pmenuitem->idItem;
		lParam = MAKELONG(pmenuitem, mmdItem);
		}
	else
		{
		id = pmenu->idMenu;	/* menu but no selection */
		lParam = MAKELONG(pmenu, mmdMenu);
		}

	/* valid selection : close menu - keep menu name hilited */
	CloseMenuCur(FALSE);
	/* send message while menu name hilited - the menu may change */
	mniCur.imenu = imenuNil;
	SendMessage(pwndMenu, WM_COMMAND, id, lParam);
	ReportMenuInactive();
	/* do both : unhilite menu title and redraw optionally changed bar */
	DrawMenubar();
	UpdateCursor();
	return TRUE;
	}



STATIC WORD
ImenuPickVk(vk)
/*
  -- Search for menu based on "hilite" character in menu title
  -- if found return imenu
  -- if not found, return imenuNil
*/
WORD vk;	/* virtual key of first character */
	{
	WORD imenu;
	REGISTER PMENU pmenu = pmenubarCur->rgmenu;
	
	for (imenu = 0; imenu < pmenubarCur->cmenu; imenu++, pmenu++)
		{
#ifndef KANJI
		if (FMatchVkCh(vk, *(pmenu->pchTitle + pmenu->ichHilite)))
#else
		if (FMatchVkCh(vk, fKanaAccel ? (char) pmenu->chKanaAccel :
		    *(pmenu->pchTitle + pmenu->ichHilite)))
#endif /*KANJI*/
			{
			/* go one */
			return imenu;
			}
		}
	return imenuNil;
	}



STATIC WORD
ImenuPickMsp(msp)
/*
  -- mouse is on menu title line
  -- return index of selected menu or imenuNil
*/
MSP msp;
	{
	REGISTER MENU *pmenu;
	REGISTER AX ax = msp.s.ax;
	WORD imenu;

	Assert(msp.s.ay == ayMenu);

	pmenu = pmenubarCur->rgmenu;
	
	for (imenu = 0; imenu < pmenubarCur->cmenu; imenu++, pmenu++)
		{
		if (ax + 1 >= (AX) pmenu->rxTitle &&
		    ax <= ((AX) pmenu->rxTitle + pmenu->cchTitle))
			{
			return imenu;
			}
		}
	return imenuNil;
	}




/********** MENUITEMS **********/

STATIC VOID
DrawMenuItem(axLeft, ay, axRight, pmenuitem)
/*
  -- output a menu item
  -- left justify the portion to the left of a tab
  -- right justify the portion after the tab
  -- output checkmark if on
  -- only called by OpenMenuCur().
*/
AX axLeft, axRight;
AY ay;
REGISTER PMENUITEM pmenuitem;
	{
	char * sz = SzItem(pmenuitem);
	WORD di = pmenuitem->fEnabled ?
	    DiNormal(isaEnabledMenuItem) :
	    DiNormal(isaDisabledMenuItem);
	AX ax = axLeft + rxMenuText;

#ifdef MENU_RIGHT_JUSTIFY
	REGISTER char * szT = sz;
	WORD cch;

	for (cch = 0; *szT != '\t' && *szT != '\0'; cch++)
		szT++;
	Assert(pmenuitem->ichHilite < cch);
	TextOutAbs(ax, ay, sz, cch, di);
#else
	TextOutAbs(ax, ay, sz, strlen(sz), di);
	Unreferenced(axRight);	/* right coordinate not used */
#endif

#ifdef KANJI
	/* kana accelerator at start ? */
	if (fKanaAccel)
		CharOutAbs(ax, ay, pmenuitem->chKanaAccel, di);
#endif /*KANJI*/

	/* select hilited character (if enabled) */
	if (pmenuitem->fEnabled)
		{
		ax += (BYTE) pmenuitem->ichHilite;
		FillArc(ax, ay, ax + 1, ay + 1, '\0',
		    (dmAttrOnly | isaMenuHilite));
		}

	if (pmenuitem->fChecked)
		CharOutAbs(axLeft + rxMenuMark, ay, chMenuMark, di);

#ifdef MENU_RIGHT_JUSTIFY
	/* output right aligned stuff if any */
	if (*szT != '\0')
		{
		cch = strlen(++szT);
		TextOutAbs(axRight - cch - 1, ay, szT, cch, di);
		}
#endif
	}



STATIC VOID
HiliteItem(fSelected)
/*
  -- highlight current menu item within current menu list
*/
BOOL fSelected;
	{
	if (mniCur.iitem != iitemNil)
		{
		REGISTER PMENUITEM pmenuitem;
		REGISTER ISA isa;
		AX ax;
		AY ay;

		Assert(mniCur.pmenuOpen != NULL && mniCur.pmenuOpen->citem != 0);
		Assert(mniCur.pmenuOpen == &pmenubarCur->rgmenu[mniCur.imenu]);

		DrawThisWnd(NULL);
		pmenuitem = &PmenuitemOfPmenu(mniCur.pmenuOpen)[mniCur.iitem];

		if (fSelected)
			isa = isaSelectedMenuItem;
		else
			isa = (ISA) (pmenuitem->fEnabled ?
			    isaEnabledMenuItem :
			    isaDisabledMenuItem);

		ay = arcMenu.ayTop + mniCur.iitem + 1;
		FillArc(arcMenu.axLeft + daxBorder, ay,
		    arcMenu.axRight - daxBorder,
		    ay + dayBorder, '\0', dmAttrOnly | isa);

		/* restore hilite character */
		if (pmenuitem->fEnabled)
			{
			ax = arcMenu.axLeft + rxMenuText + pmenuitem->ichHilite;
			FillArc(ax, ay, ax + 1, ay + 1,
			    '\0', fSelected ? (dmAttrOnly | isaItemHiliteSel) :
			     (dmAttrOnly | isaMenuHilite));
			}

		/* If Selected then update help line */
		if (fSelected)
			SendMessage(pwndMenu, WM_MENUSELECT, pmenuitem->idItem,
			    MAKELONG(pmenuitem, mmdItem));
		}
	}



STATIC BOOL
FSelectItem(iitem)
/*
  -- set specified item to be current selected item
  -- if item is valid, change current selection and return TRUE
  -- if item is invalid (separator), change to no selection, return FALSE
  -- return TRUE if selection made
  -- item may be beyond valid range, do wrapping here
*/
WORD iitem;
	{
	Assert(FMenuOpen());
	Assert(mniCur.pmenuOpen == &pmenubarCur->rgmenu[mniCur.imenu]);

	/* check for empty drop downs */
	if (mniCur.pmenuOpen->citem == 0)
		return TRUE;

	/* check for menu wrapping */
	if (iitem != imenuNil && iitem >= mniCur.pmenuOpen->citem)
		{
		/* perform wrap */
		if (iitem == (WORD) -1)	/* wrapped to -1 */
			iitem = mniCur.pmenuOpen->citem - 1;
		else
			{
			Assert(iitem == mniCur.pmenuOpen->citem);
			iitem = 0;
			}
		}

	Assert(iitem == iitemNil || iitem < mniCur.pmenuOpen->citem);

	if (iitem != mniCur.iitem)
		{
		HiliteItem(FALSE);

		if (PmenuitemOfPmenu(mniCur.pmenuOpen)[iitem].fSeparator)
			{
			mniCur.iitem = iitemNil;
			ReportMenuInactive();
			return FALSE;
			}
		else
			{
			/* real item : hilite even if disabled */
			mniCur.iitem = iitem;
			HiliteItem(TRUE);
			}
		}
	return TRUE;
	}



STATIC BOOL
FMatchItemVk(vk)
/*
  -- Search for menu item based on first character (using ichHilite)
  -- if found changed current selection
  -- if found and enabled : select item and return TRUE
  --  otherwise return FALSE
*/
WORD vk;
	{
	REGISTER PMENU pmenu = mniCur.pmenuOpen;
	WORD	citem;
	WORD	iitem;
	REGISTER PMENUITEM pmenuitem;
#ifdef ACCEL_MULTIPLE
	WORD	citemMatch;

	citemMatch = 0;
	pmenuitem = &(PmenuitemOfPmenu(pmenu)[0]);
	citem = pmenu->citem;
	while (citem--)
		{
#ifdef KANJI
		/* Kanji Accelerator matching */
		if (!pmenuitem->fSeparator &&
		    FMatchVkCh(vk, fKanaAccel ? (char) pmenuitem->chKanaAccel :
		    *(SzItem(pmenuitem) + pmenuitem->ichHilite)))
#else
		if (!pmenuitem->fSeparator &&
		    FMatchVkCh(vk, *(SzItem(pmenuitem) + pmenuitem->ichHilite)))
#endif
			citemMatch++;
		pmenuitem++;
		}
	if (citemMatch == 0)
		return FALSE;	/* no match */
	fOneOfMultiple = (citemMatch > 1);
#endif /*ACCEL_MULTIPLE*/

	citem = pmenu->citem;
	iitem = mniCur.iitem;
	pmenuitem = &(PmenuitemOfPmenu(pmenu)[++iitem]);

	while (citem--)
		{
		if (iitem >= pmenu->citem)
			{
			/* wrap to top */
			iitem = 0;
			pmenuitem = PmenuitemOfPmenu(pmenu);
			}
#ifdef KANJI
		/* Kanji Accelerator matching */
		if (!pmenuitem->fSeparator &&
		    FMatchVkCh(vk, fKanaAccel ? (char) pmenuitem->chKanaAccel :
		    *(SzItem(pmenuitem) + pmenuitem->ichHilite)))
#else
		if (!pmenuitem->fSeparator &&
		    FMatchVkCh(vk, *(SzItem(pmenuitem) + pmenuitem->ichHilite)))
#endif
			{
			/* found ! */
			return (FSelectItem(iitem) && pmenuitem->fEnabled);
			}

		pmenuitem++;
		iitem++;
		}
	return FALSE;	/* not found */
	}



STATIC BOOL
FSelectItemMsp(msp)
/*
  -- Mouse form to pick an item
  -- returns TRUE if picking an item (and changes item selection)
*/
MSP msp;
	{
	Assert(msp.s.ay != ayMenu);

	if (FInsideMenuMsp(msp.s.ax, msp.s.ay))
		{
		Assert(mniCur.pmenuOpen->citem != 0);
		FSelectItem(msp.s.ay - arcMenu.ayTop - 1);
		mniCur.fMouseOnTitle = FALSE;	/* selected something */
		return TRUE;
		}

	/* not selecting anything */
	HiliteItem(FALSE);
	mniCur.iitem = iitemNil;
	return FALSE;
	}



STATIC BOOL
FInsideMenuMsp(ax, ay)
/*
  -- return TRUE if inside Menu dropdown, or on menu title bar
*/
REGISTER AX ax;
REGISTER AY ay;
	{
	if (ay == ayMenu)
		{
		mniCur.fMouseOnTitle = TRUE;
		return TRUE;
		}
	if (!FMenuOpen())
		return FALSE;
	return (ay > arcMenu.ayTop &&
	    ay < arcMenu.ayBottom - 1 &&
	    ax >= arcMenu.axLeft &&
	    ax < arcMenu.axRight);
	}





/********** The Filter **********/


PRIVATE BOOL FARPRIVATE
MenuFilterProc(pmsg)
/*
  -- menu manager filter procedure
  -- returns true if message is to be filtered out
*/
REGISTER PMSG pmsg;  /* access pmsg-> with same speed as local auto vars */
	{
	REGISTER WORD kk;		/* Key bits + vk */
	MSP msp;
	BOOL fMenuAction;

	if (pwndCapture != NULL)
		return(FALSE);
	kk = HIWORD(pmsg->lParam);
	fMenuAction = FALSE;

	msp.lParam = pmsg->lParam;

	/* is it a valid (LEFT) menu mouse message ?? */
	if (pmsg->message >= WM_MOUSEFIRST && pmsg->message <= WM_LMOUSELAST &&
	    FShowMenuBar())
		{
		if ((pmsg->message == WM_LBUTTONDBLCLK ||
		    pmsg->message == WM_LBUTTONDOWN) && !mniCur.fMouse)
			{
			/* Menu Button */
			BOOL fInside = FInsideMenuMsp(msp.s.ax, msp.s.ay);

			if (!mniCur.fMouse && !fInside &&
			   (FMenuActive() || mniCur.fMenuMnem))
				{
				/* click not in drop down */
				SetIdleState();
				return FALSE; /* pass message through */
				}
			mniCur.fMouse |= fInside;
			}

		/* now see if we should do any mouse work */
		if (!mniCur.fMouse)
			return FMenuActive();	/* eat if menu active */

		if (msp.s.ay == ayMenu)
			{
			WORD imenuT;

			imenuT = ImenuPickMsp(msp);
			if (imenuT != imenuNil && !FMenuOpen())
				goto open_new_menu;
			else if (imenuT != mniCur.imenu)
				{
				CloseMenuCur(TRUE);
open_new_menu:
				mniCur.fMouseOnTitle = FALSE; /* selection */
				SelectMenu(imenuT);
				OpenMenuCur();
				}
			else if (FMenuActive())
				{
				/* clear any selection */
				Assert(FMenuOpen());
				FSelectItem(iitemNil);
				}
			/* re-send menu title message (for help) */
				if (mniCur.pmenuOpen->citem != 0)
			if (mniCur.pmenuOpen != NULL)
				SendMessage(pwndMenu, WM_MENUSELECT,
				    mniCur.pmenuOpen->idMenu,
				    MAKELONG(mniCur.pmenuOpen, mmdMenu));
			}
		else if (FMenuActive())
			{
			/* selecting an item (maybe) */
			if (!FSelectItemMsp(msp))
				ReportMenuInactive();
			}

		/* is he releasing the mouse button ?? */
		if (pmsg->message == WM_LBUTTONUP)
			{
			if (!FMenuActive())
				{
				/* do nothing (except cursor enable) */
				UpdateCursor();
				}
			else if (mniCur.iitem != iitemNil)
				{
				FMakeSelection();
				}
			else if (msp.s.ay == ayMenu)
				{
				/* selecting title - close or keep open */
				Assert(FMenuOpen());
				if (mniCur.fMouseOnTitle)
					{
					/* stay on title => close it */
					SetIdleState();
					}
				/* else : keep it open */
				else if (mniCur.pmenuOpen->citem != 0)
					{
					FSelectItem(0);
					}
				else
					{
					FMakeSelection();
					}
				}
			else
				SetIdleState();
			mniCur.fMouse = FALSE;
			}

		return TRUE;		/* eat it */
		}
	else if (mniCur.fMouse)
		{
		/* mouse control of the menu --> nothing gets through */
		return TRUE;
		}
#ifdef	MULTIPLE_ACTION
	else if ((pmsg->wParam == vkActionPrim ||
		  pmsg->wParam == vkActionSec) &&
		 (pmsg->wParam == VK_MENU) && 
		 (pmsg->message == WM_KEYUP))
#else	/* MULTIPLE_ACTION */
	else if (pmsg->wParam == VK_MENU && pmsg->message == WM_KEYUP)
#endif	/* MULTIPLE_ACTION */
		{
		/* convert keys to Menu Action events */
		if (fNonAltKey)
			{
			/* ignore it */
			if (!FMenuActive())
				HiliteMenuMnem(FALSE);
			return TRUE;
			}
		fMenuAction = TRUE;
		}
#ifdef	MULTIPLE_ACTION
	else if ((pmsg->wParam == vkActionPrim ||
		  pmsg->wParam == vkActionSec) &&
		 (pmsg->message == WM_CHAR))
		 {
		     fMenuAction = TRUE;
		 }   
#endif	/* MULTIPLE_ACTION */
	else if (pmsg->message == WM_CHAR)
		fNonAltKey = (BYTE) (pmsg->wParam != VK_MENU);

	/* allow all non-alt key shifted keys to slip by (if accelerators) */
	if (pmsg->message == WM_CHAR)
		{
		if (FTranslateAccelerator(pmsg->wParam, kk))
			return TRUE;	/* eat it */

#ifdef	MULTIPLE_ACTION
      		if ((!FMenuActive()) &&
		    (pmsg->wParam != vkActionPrim) &&
		    (pmsg->wParam != vkActionSec))
			return FALSE;	/* pass through if menu off */
		}
#else	/* ! MULTIPLE_ACTION */
		if (!(kk & KK_ALT) && !FMenuActive())
			return FALSE;	/* pass through if menu off */
		}
#endif	/* MULTIPLE_ACTION */

	if (!FMenuActive())
		{
		/* we are in idle - handle as fast as possible */
		Assert(mniCur.imenu == imenuNil);	/* no menu yet */

		if (fMenuAction)
			{
			/* Turn on first menu - don't open */
			SelectMenu(0);
			HiliteMenuTitle(TRUE);
			HiliteMenuMnem(TRUE);
			return TRUE;		/* eat it */
			}
		else if (pmsg->message == WM_CHAR)
			{
			/* a character */
#ifdef	MULTIPLE_ACTION
			if ((pmsg->wParam == vkActionPrim) ||
			    (pmsg->wParam == vkActionSec))
#else	/* MULTIPLE_ACTION */
			if (pmsg->wParam == VK_MENU)
#endif	/* MULTIPLE_ACTION */
				{
				HiliteMenuMnem(TRUE);
				return TRUE;	/* eat it */
				}

			if (kk & KK_ALT)
#ifdef	MULTIPLE_ACTION
				{
				WORD imenuT;
				/* otherwise a menu selection */
				if ((imenuT = ImenuPickVk(kk & KK_VK))
				    != imenuNil)
					{
					/* if we don't have a menubar -- get one !! */
					if (pmenubarCur == NULL)
					    SendMessageShort(pwndMenu, WM_MENUSTART);
					/* open that menu */
					SelectMenu(imenuT);
					OpenMenuCur();
					}
				else
					return FALSE;
				return TRUE;		/* eat it */
			    	}
#else	/* !MULTIPLE_ACTION */
			        {
				WORD imenuT;


				/* if we don't have a menubar -- get one !! */
				if (pmenubarCur == NULL)
					SendMessageShort(pwndMenu, WM_MENUSTART);
				/* otherwise a menu selection */
				if ((imenuT = ImenuPickVk(kk & KK_VK))
				    != imenuNil)
					{
					/* open that menu */
					SelectMenu(imenuT);
					OpenMenuCur();
					}
				else
					Beep();
				return TRUE;		/* eat it */
				}
#endif	/* MULTIPLE_ACTION */

			return FALSE;	/* pass through */
		        }

		return FALSE;	/* let the message pass */
		}

	/* not in idle !!!! */
	Assert(mniCur.imenu != imenuNil);
	AssertSz(pmenubarCur != NULL, "processing without a menu bar");

#ifdef DUAL
	/* if we get a null message while processing the menu, we are
	* therefore in an idle loop and should go to sleep for DOS 5
	*/
	if (fProtectMode && pmsg->message == WM_NULL)
		{
		SendMessageShort(pwndMenu, WM_MENUIDLE);
		DosSemWait(hsemaMessage, -1L);
		return TRUE;		/* eat the message */
		}
#else /*!DUAL*/
#ifdef DOS5
	/* if we get a null message while processing the menu, we are
	* therefore in an idle loop and should go to sleep for DOS 5
	*/
	if (pmsg->message == WM_NULL)
		{
		SendMessageShort(pwndMenu, WM_MENUIDLE);
		DosSemWait(hsemaMessage, -1L);
		return TRUE;		/* eat the message */
		}
#endif
#endif /*!DUAL*/

	if (fMenuAction)
		{
		/* Menu Action */
		if (FMenuOpen())
			{
			PMENU pmenu;
			/* close menu */
			CloseMenuCur(TRUE);
			HiliteMenuMnem(TRUE);	/* back to menu browse */
			/* update the help */
			pmenu = &pmenubarCur->rgmenu[mniCur.imenu];
			SendMessage(pwndMenu, WM_MENUSELECT, pmenu->idMenu,
			    MAKELONG(pmenu, mmdMenu));
			}
		else
			{
			/* go back to idle */
			SetIdleState();
			}
		}
	else if (pmsg->message == WM_CHAR)
		{
		WORD imenuT;
		BOOL fWasOpen;

		/* undo repeat before processing */
		UndoRepeat(pmsg->wParam, pmsg->lParam);

		imenuT = mniCur.imenu;

#ifdef	MULTIPLE_ACTION
		if (pmsg->wParam == vkActionPrim ||
      			pmsg->wParam == vkActionSec)
		      return(TRUE);

		if (pmsg->wParam == vkCancelPrim || pmsg->wParam == vkCancelSec) {
		    SetIdleState();
		    return TRUE;
		}
#endif	/* MULTIPLE_ACTION */

		/* Cases : similar whether menu is open or not */
		switch (pmsg->wParam)
			{
		default:
			break;		/* drop out & do second set of cases */

#ifndef	MULTIPLE_ACTION
		case LOBYTE(VK_ESCAPE):	/* escape character */
			SetIdleState();
			return TRUE;
#endif	/* !MULTIPLE_ACTION */

		case VK_LEFT:
			imenuT -= 2;
			/* fall through to right */
		case VK_RIGHT:
			imenuT++;	/* right +1, left -2+1 == -1 */
			/* check for wrap */
			if (imenuT >= pmenubarCur->cmenu)
				{
				if (imenuT == (WORD) -1) /* wrapped */
					imenuT = pmenubarCur->cmenu - 1;
				else
					{
					Assert(imenuT == pmenubarCur->cmenu);
					imenuT = 0;
					}
				}
			/* select new menu */
			fWasOpen = FMenuOpen();
			BeginDraw();
			CloseMenuCur(TRUE);
			ReportMenuInactive();
			HiliteMenuTitle(FALSE);
			SelectMenu(imenuT);
			if (fWasOpen)
				OpenMenuCur();
			EndDraw();
			return TRUE;

#ifndef	MULTIPLE_ACTION
		case VK_MENU:
			return TRUE;
#endif	/* !MULTIPLE_ACTION */

		case VK_HELP_KEY:
			SendMenuHelp(kk);
			MoveHardwareCursor(0, 0, FALSE);
			return TRUE;
			}

		/* cases which are different whether FMenuOpen() or not */
		if (!FMenuOpen())
			{
			/* Menu not open, let's try and open one */

			switch (pmsg->wParam)
				{
			default:
				/* select menu */
				if ((imenuT = ImenuPickVk(pmsg->wParam))
				    != imenuNil)
					{
					SelectMenu(imenuT);
					OpenMenuCur();
					}
				else
					Beep();	/* bad choice */
				break;

			case LOBYTE(VK_RETURN):
			case VK_UP:
			case VK_DOWN:	/* same as up */
				/* Open the current menu */
				OpenMenuCur();
				break;
				}
			}
		else
			{
			/* Menu open, keys effect item selection */
			WORD iitemT = mniCur.iitem;

			switch (pmsg->wParam)
				{
			default:
				/* select item */
				if (!FMatchItemVk(pmsg->wParam))
					{
					/* bad key */
					Beep();
					break;
					}
#ifdef ACCEL_MULTIPLE
				if (fOneOfMultiple)
					break;
#endif /*ACCEL_MULTIPLE*/
				/* fall through to accept */
			case LOBYTE(VK_RETURN):
				FMakeSelection();
				break;

			case VK_UP:
			case VK_DOWN:
				/* previous / next menu item */
				do
					{
					if (pmsg->wParam == VK_UP)
						iitemT--;
					else
						iitemT++;
					}
				while (!FSelectItem(iitemT));
				}
			}
		}
	return TRUE;	/* normally eat */
	}



STATIC VOID
SetIdleState()
/*
  -- jump to idle state
*/
	{
	/* do it all -- all routines handle nil cases */
	CloseMenuCur(TRUE);
	HiliteMenuTitle(FALSE);
	mniCur.imenu = imenuNil;
	HiliteMenuMnem(FALSE);
	ReportMenuInactive();
	mniCur.fMouse = FALSE;
	UpdateCursor();
	}



/********** Utilities **********/


STATIC BOOL
FTranslateAccelerator(vk, kk)
/*
  -- check to see if key "vk" should be treated as an accelerator
  -- send 2 messages if it is an accelerator (DO NOT POST!!!) and return TRUE
  -- return FALSE if not translated
*/
WORD vk;
WORD kk;
	{
	REGISTER MPVKEYID *pmpvkeyid = pmenubarCur->rgmpvkeyid;
	WORD vkeyCur, vkeyFind;

	/* look for correct "VKEY" value & proper shift states */
	vkeyFind = VkeyOfVkKk(vk, kk & (KK_ALT | KK_CONTROL | KK_SHIFT));

	while ((vkeyCur = pmpvkeyid->vkey) != 0)
		{
		if (vkeyCur == vkeyFind)
			{
			WORD idItem = pmpvkeyid->idItem;
			PMENUITEM pmenuitem = FindMenuItem(idItem);
			
			/* found an accelerator */
			if (FMenuActive())
				SetIdleState();

			if (pmenuitem != NULL)
				{
				SendMessage(pwndMenu, WM_INITMENUPOPUP,
				    pmenuFound->idMenu, MAKELONG(pmenuFound, 1));
				if (!pmenuitem->fEnabled)
					return TRUE;
				}
			/* send item message even if not with menu item */
			SendMessage(pwndMenu, WM_COMMAND, idItem,
			    MAKELONG(pmenuitem, mmdAccel));

			return TRUE;
			}
		else
			pmpvkeyid++;
		}
	return FALSE;
	}



STATIC BOOL
FMatchVkCh(vk, chMenu)
/*
  -- return TRUE if virtual key "vk" matches matches menu character "ch"
  -- (i.e. vk will be Upper case ASCII, menu char may be upper or lower case)
  -- for KANJI -- also check the alternate KANA/ROMAN key
*/
WORD vk;
char chMenu;
	{
	char ch = (char) LOBYTE(vk);

	if (vk >= 'a' && vk <= 'z')
		ch -= ('a' - 'A');
	else if ((unsigned) ch > 0x7f)
		ch = ChUpperFromChExt(ch);

	if (chMenu >= 'a' && chMenu <= 'z')
		chMenu -= ('a' - 'A');
	else if ((unsigned) chMenu > 0x7f)
		chMenu = ChUpperFromChExt(chMenu);

#ifdef KANJI
	if (ch != chMenu)
		{
		/* possibly the alternate character on that key -- try it */
		ch = ChAlternateKeytop(ch);
		}
#endif /*KANJI*/
	return(chMenu == ch);
	}



STATIC char *
SzItem(pmenuitem)
/*
  -- return the text of the menu item
*/
REGISTER PMENUITEM pmenuitem;
	{
	return (pmenuitem->fHandle ? *(pmenuitem->u.pszItem) :
	    pmenuitem->u.szItem);
	}



STATIC VOID
SendMenuHelp(kk)
/*
  -- send menu help message
*/
WORD	kk;
	{
	/* process HELP */
	WORD imenuSave;

	/* save old state, turn off menu for the help portion */
	imenuSave = mniCur.imenu;
	Assert(imenuSave != imenuNil);
	mniCur.imenu = imenuNil;

	if (mniCur.iitem != iitemNil)
		{
		PMENUITEM pmenuitem;
		Assert(FMenuOpen());

		pmenuitem = &PmenuitemOfPmenu(mniCur.pmenuOpen)[mniCur.iitem];
		Help(hemMenuItem, pmenuitem->idItem, pmenuitem, kk);
		}
	else
		{
		PMENU pmenu;

		pmenu = &pmenubarCur->rgmenu[imenuSave];
		Help(hemMenu, pmenu->idMenu, pmenu, kk);
		}
	mniCur.imenu = imenuSave;
#ifdef	DIALOG_NOSAVE
	DrawMenuCur();
#endif
	}




#ifdef MENU_UTILS
VOID FARPUBLIC
OpenMenu(idMenu)
/*
  -- application wants a menu open
*/
WORD idMenu;	/* id of menu to open */
	{
	REGISTER PMENU pmenu;
	WORD imenu;

	Assert(mniCur.imenu == imenuNil);	/* must be closed */

	for (imenu = 0, pmenu = pmenubarCur->rgmenu;
	    imenu < pmenubarCur->cmenu;
	    pmenu++, imenu++)
		{
		if (pmenu->idMenu == idMenu)
			{
			/* found it */
			SelectMenu(imenu);
			OpenMenuCur();
			fNonAltKey = FALSE;	/* allow up transition */
			return;
			}
		}
	AssertSz(FALSE, "Invalid call to OpenMenu()");
	}
#endif /*MENU_UTILS*/
