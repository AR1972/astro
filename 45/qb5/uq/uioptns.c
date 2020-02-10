/***
*uioptn.c - Support Options menu items.
*
*	Copyright <C> 1985-1988, Microsoft Corporation
*
*Purpose:
*	All Options menu support routines, and other routines that modify
*	the screen's appearance.
*
*******************************************************************************/

/* First, include version definition header */
#include <version.h>

/* Next, include TextWin's interface headers */
#include <cw/version.h>
#include <cw/windows.h>
#include <cw/edityp.h>
#include <cw/color.h>
#include <uiext.h>

/* include dialog information */
#include <cw/dlg.h>
#include "uioptns.hs"
#include "uioptns.sdm"

/* Next, include QBI's headers */
#ifndef CONTEXT_H
#include <context.h>
#endif

#ifndef QBIMSGS_H
#include <qbimsgs.h>
#endif

#ifndef UI_H
#include <ui.h>
#endif

#ifndef UIINT_H
#include <uiint.h>
#endif

#ifndef UTIL_H
#include <util.h>
#endif

#ifndef RTPS_H					/* [10] */
#include <rtps.h>				/* [10] */
#endif						/* [10] */

#ifndef HEAP_H					/* [10] */
#include <heap.h>				/* [10] */
#endif						/* [10] */

void near ColorResolution (void);		// [36]


void near SetFullMenus(void);

/* options have been changed with CmdViewOptions => write qb ini file */
boolean fOptionsChanged;
bool fSyntaxCheck = TRUE;  /* enabled by default */

BYTE fMono = FALSE;		// [25]

/********* View Options ******************************************************/

uchar rgMsgColors[] = {		//[30] change from word to byte
    MSG_Black,
    MSG_Blue,
    MSG_Green,
    MSG_Cyan,
    MSG_Red,
    MSG_Magenta,
    MSG_Brown,
    MSG_White,
    MSG_Gray,		/* [9] Add Bright colors */
    MSG_BrBlue, 	/* [9] */
    MSG_BrGreen,	/* [9] */
    MSG_BrCyan, 	/* [9] */
    MSG_BrRed,		/* [9] */
    MSG_Pink,		/* [9] */
    MSG_Yellow, 	/* [9] */
    MSG_BrWhite 	/* [9] */
};
#define	rgMsgColorsMax	(sizeof (rgMsgColors) / sizeof (BYTE))	//[30]

/* [9] Separate array is smaller than moving array around */
uchar rgMsgMono[] = {		//[30] change from word to byte
    MSG_Black,
    MSG_White,
    MSG_Gray,
    MSG_BrWhite
};
#define	rgMsgMonoMax	(sizeof (rgMsgMono) / sizeof (BYTE))	//[30]


/***
*WORD FAR WListProcColor (tmm, sz, isz, tmc, wParam, bArg)
*Purpose:
*	ListBox filling support procedure.
*
*Entry:
*	tmm		Dialog item message type.
*	sz		String pointer to return text in.
*	isz		Listbox item to return text for.
*	tmc		Unreferenced.
*	wParam		Unreferenced.
*	bArg		Unreferenced.
*
*Exit:
*	tmm = tmmCount, size of array of colors.
*	      tmmText or
*	      tmmEditText, Text of isz'th element of color array in sz.
*
*Exceptions:
*	None.
*******************************************************************************/
WORD FAR
WListProcColor (tmm, sz, isz, tmc, wParam, bArg)
WORD tmm;
char *sz;
WORD isz;
TMC tmc;
WORD wParam, bArg;
{
    Unreferenced (tmc);
    Unreferenced (wParam);
    Unreferenced (bArg);

    switch (tmm) {
	case tmmCount:
	    /* [5] [25] if fMono, use rgMsgMono */
	    return ((fMono ? rgMsgMonoMax : rgMsgColorsMax)	// [30]
		    >> ((cmdSwitches & CMD_SW_NOH) != 0)	// [21]
		   );

	case tmmText:
	case tmmEditText:
	    {
	    /* [5] [25] if fMono, use rgMsgMono */
	    DbAssert (fMono ? (isz<rgMsgMonoMax) : (isz<rgMsgColorsMax)); //[30]
	    ListStdMsg (fMono ? rgMsgMono[isz] : rgMsgColors[isz]);	//[30]
	    }
	    strcpy (sz, bufStdMsg);
	    break;
    }
    return (TRUE);		//[31] Was depending on return value of strcpy
				//[31] to be non-zero.
}



/* [7] Make MenuBarMain, rgMenuMain, and rgMenuMainEZ accessible */
extern MENUBAR MenuBarMain;
extern MENU rgMenuMain[], rgMenuMainEZ[];

/* [5] Array of colors for OptnsDisplay dialog */
static struct {
    WORD coFore, coBack;
} rgco[3];

/*** 
*BOOL FAR FDlgOptnsDisplay (dlm, tmc, wNew, wOld, wParam)
*Purpose:
*	Dialog procedure for dlgOptnsDisplay.
*
*	dlmInit:
*		Initialize the array of temporary colors for Normal Text,
*		Current Statement, and Breakpoint Line in rgco.
*
*	dlmClick:
*		If Changing foreground or background color, change
*		color array element, and redisplay the general picture
*		box for this element.
*
*		Else, get the color from the array, and change the
*		foreground and background listbox indices.
*
*Entry:
*	dlm		Dialog message.
*	tmc		Item code of action.
*	wNew		Unreferenced.
*	wOld		Unreferenced.
*	wParam		Unreferenced.
*
*Exit:
*	None.
*
*Exceptions:
*	None.
*******************************************************************************/
BOOL FAR
FDlgOptnsDisplay (dlm, tmc, wNew, wOld, wParam)
WORD dlm;
TMC tmc;
WORD wNew, wOld, wParam;
{
    ISA isa;
    register WORD oColor, i;
    WORD coFore, coBack;

    Unreferenced (wNew);
    Unreferenced (wOld);
    Unreferenced (wParam);

    if (dlm == dlmInit) {
	for (i = 0; i < 3; i++) {
	    GetIsaColor ((ISA) isaEditWindow+i, &rgco[i].coFore, &rgco[i].coBack);
	}
    }

    if (dlm == dlmInit || dlm == dlmClick) {
	oColor = (cmdSwitches & CMD_SW_ED) ? 0 : GetTmcVal (tmcColor);  //[38]
	DbAssert (oColor < 3);
	isa = (ISA) isaEditWindow + oColor;
	if (tmc == tmcCoFore || tmc == tmcCoBack) {
	    coFore = GetTmcVal (tmcCoFore);
	    coBack = GetTmcVal (tmcCoBack);
	    // [15] [25] If monochrome, translate index to attribute.
	    if (fMono) {
		rgco[oColor].coFore = ((coFore & 1) ? coWhite : coBlack) |
				      ((coFore >= 2) << 3);
		rgco[oColor].coBack = ((coBack & 1) ? coWhite : coBlack) |
				      ((coBack >= 2) << 3);
	    }
	    else {
		rgco[oColor].coFore = coFore;
		rgco[oColor].coBack = coBack;
	    }
            if (cmdSwitches & CMD_SW_ED)        //[38]
                RedisplayTmc (tmcQeditText);    //[38]
            else                                //[38]
	        RedisplayTmc (tmcNormalText + oColor);
	}
	else {
	    coFore = rgco[oColor].coFore;
	    coBack = rgco[oColor].coBack;
	    // [25] If no highlight bit, clear it for proper index
	    if (cmdSwitches & CMD_SW_NOH) {
		coFore &= 7;
		coBack &= 7;
	    }
	    // [15] [25] If monochrome, translate color to index.
	    if (fMono) {
		SetTmcVal (tmcCoFore, ((coFore & 7) == coWhite) |
				      ((coFore >= 8) << 1));
		SetTmcVal (tmcCoBack, ((coBack & 7) == coWhite) |
				      ((coBack >= 8) << 1));
	    }
	    else {
		SetTmcVal (tmcCoFore, coFore);
		SetTmcVal (tmcCoBack, coBack);
	   }
	}
    }

    return (TRUE);
}

#define QeditTextLen    24      //[38] length of " Set colors for the ",
                                           //[38]       and " text editor window:"
#define	ColoredTextLen	19	   //[12] length of " Normal Text       ",
				           //[12]           " Current Statement ",
				           //[12]       and " Breakpoint Lines  "

/***
*WORD FAR PASCAL DisplayQeditTextWndProc (tmm, pv, hObj, tmc, bArg, wParam)
*Purpose:
*	Display the the "QEDIT Text" color item in the appropriate color.
*
*Entry:
*	tmm		Unreferenced.
*	pv		pointer to general picture window.
*	hObj		Unreferenced.
*	tmc		Unreferenced.
*	bArg		Unreferenced.
*	wParam		Unreferenced.
*
*Exit:
*	returns TRUE.
*
*Exceptions:
*	None.
*******************************************************************************/
WORD FAR PASCAL
DisplayQeditTextWndProc (tmm, pv, hObj, tmc, bArg, wParam)      //[38]
TMM	tmm;
char *	pv;		/* really a pwnd */
WORD	hObj, bArg;	/* not used */
TMC	tmc;
WORD	wParam;
{
    REG1 PWND pwnd = (PWND) pv;

    Unreferenced (tmm);
    Unreferenced (hObj);
    Unreferenced (bArg);
    Unreferenced (tmc);
    Unreferenced (wParam);

    SetIsaColor (isaUserMax-1, rgco[0].coFore, rgco[0].coBack);
    ListStdMsg (MSG_QeditText1);
    TextOut (pwnd, 0, 0, bufStdMsg, QeditTextLen, isaUserMax-1);
    ListStdMsg (MSG_QeditText2);
    TextOut (pwnd, 0, 1, bufStdMsg, QeditTextLen, isaUserMax-1);

    return (TRUE);
}

/***
*WORD FAR PASCAL DisplayNormalTextWndProc (tmm, pv, hObj, tmc, bArg, wParam)
*Purpose:
*	Display the the "Normal Text" color item in the appropriate color.
*
*Entry:
*	tmm		Unreferenced.
*	pv		pointer to general picture window.
*	hObj		Unreferenced.
*	tmc		Unreferenced.
*	bArg		Unreferenced.
*	wParam		Unreferenced.
*
*Exit:
*	returns TRUE.
*
*Exceptions:
*	None.
*******************************************************************************/
WORD FAR PASCAL
DisplayNormalTextWndProc (tmm, pv, hObj, tmc, bArg, wParam)
TMM	tmm;
char *	pv;		/* really a pwnd */
WORD	hObj, bArg;	/* not used */
TMC	tmc;
WORD	wParam;
{
    REG1 PWND pwnd = (PWND) pv;

    Unreferenced (tmm);
    Unreferenced (hObj);
    Unreferenced (bArg);
    Unreferenced (tmc);
    Unreferenced (wParam);

    ListStdMsg (MSG_NormalText);
    SetIsaColor (isaUserMax-1, rgco[0].coFore, rgco[0].coBack);
    TextOut (pwnd, 0, 0, bufStdMsg, ColoredTextLen, isaUserMax-1);	//[12]

    return (TRUE);
}


/***
*WORD FAR PASCAL DisplayCurStmtWndProc (tmm, pv, hObj, tmc, bArg, wParam)
*Purpose:
*	Display the the "Current Statement" color item in the appropriate color.
*
*Entry:
*	tmm		Unreferenced.
*	pv		pointer to general picture window.
*	hObj		Unreferenced.
*	tmc		Unreferenced.
*	bArg		Unreferenced.
*	wParam		Unreferenced.
*
*Exit:
*	returns TRUE.
*
*Exceptions:
*	None.
*******************************************************************************/
WORD FAR PASCAL
DisplayCurStmtTextWndProc (tmm, pv, hObj, tmc, bArg, wParam)
TMM	tmm;
char *	pv;		/* really a pwnd */
WORD	hObj, bArg;	/* not used */
TMC	tmc;
WORD	wParam;
{
    REG1 PWND pwnd = (PWND) pv;

    Unreferenced (tmm);
    Unreferenced (hObj);
    Unreferenced (bArg);
    Unreferenced (tmc);
    Unreferenced (wParam);

    ListStdMsg (MSG_CurStmt);
    SetIsaColor (isaUserMax-1, rgco[1].coFore, rgco[1].coBack);
    TextOut (pwnd, 0, 0, bufStdMsg, ColoredTextLen, isaUserMax-1);	//[12]

    return (TRUE);
}

/*** 
*WORD FAR PASCAL DisplayBreakpointTextWndProc (tmm, pv, hObj, tmc, bArg, wParam)
*Purpose:
*	Display the the "Breakpoint Lines" color item in the appropriate color.
*
*Entry:
*	tmm		Unreferenced.
*	pv		pointer to general picture window.
*	hObj		Unreferenced.
*	tmc		Unreferenced.
*	bArg		Unreferenced.
*	wParam		Unreferenced.
*
*Exit:
*	returns TRUE.
*
*Exceptions:
*	None.
*******************************************************************************/
WORD FAR PASCAL
DisplayBreakpointTextWndProc (tmm, pv, hObj, tmc, bArg, wParam)
TMM	tmm;
char *	pv;		/* really a pwnd */
WORD	hObj, bArg;	/* not used */
TMC	tmc;
WORD	wParam;
{
    REG1 PWND pwnd = (PWND) pv;

    Unreferenced (tmm);
    Unreferenced (hObj);
    Unreferenced (bArg);
    Unreferenced (tmc);
    Unreferenced (wParam);

    ListStdMsg (MSG_Breakpoint);
    SetIsaColor (isaUserMax-1, rgco[2].coFore, rgco[2].coBack);
    TextOut (pwnd, 0, 0, bufStdMsg, ColoredTextLen, isaUserMax-1);	//[12]

    return (TRUE);
}

/*** 
*VOID NEAR CmdOptnsDisplay ()
*Purpose:
*	Called when OPTIONS/DISPLAY menu item is selected.
*
*Entry:
*	None.
*
*Exit:
*	None.
*
*Exceptions:
*	None.
*******************************************************************************/
VOID NEAR CmdOptnsDisplay ()
{
    ushort tabs;
    WORD i;
    HCABOptnsDisplay hcabOptnsDisplay;

    tabs = 0;
    DbAssert (uierr == 0);		//[24]
    hcabOptnsDisplay = (HCABOptnsDisplay) HcabAlloc (cabiCABOptnsDisplay);
    /* [5] Catch HcabAlloc errors */
    if (uierr)
	return;

    (*hcabOptnsDisplay)->oColor = 0;
    (*hcabOptnsDisplay)->oCoFore = (*hcabOptnsDisplay)->oCoBack = 0;

    itoa (GetTabs (), bufStdMsg, 10);
    SzToCab (hcabOptnsDisplay, bufStdMsg, Iag (CABOptnsDisplay, szTabStops));
    // [26] if SzToCab memory allocation failed, free cab and exit.
    if (uierr)
	goto EndCmdOptnsDisplay;
    (*hcabOptnsDisplay)->fScrollBars = fScrollBars;	/* [4] */
    (*hcabOptnsDisplay)->u.sab = (cmdSwitches & CMD_SW_ED) ? sabOptnsQedit : sabOptnsQbas;      //[38]

    if (TmcDoDlgFar (&dlgOptnsDisplay, sizeof (dlgOptnsDisplay), hcabOptnsDisplay) == tmcOk) {
	fOptionsChanged = TRUE;
	for (i = 0; i < 3; i++) {
	    SetIsaColor ((ISA) isaEditWindow+i, rgco[i].coFore, rgco[i].coBack);
	}
	ColorResolution ();			// [36]

	fScrollBars = (*hcabOptnsDisplay)->fScrollBars;
	// [21] Either RemoveScrollBars() or AddScrollBars() will have no effect
	// [21] (depending on the setting of fScrollBars).
	RemoveScrollBars ();
	AddScrollBars ();
      
	/* SetTabs only if greater than zero */
	SzFromCab (hcabOptnsDisplay, bufStdMsg, CB_bufStdMsg,
		   Iag (CABOptnsDisplay, szTabStops));
	/* [22] if user specified -1 convert to -2 so erroneous
	   [22] Can't Set Tabs dialog is not displayed.         */
	if ((tabs = atoi (bufStdMsg)) == -1)	//[22]
	     tabs--;				//[22]
	if (tabs != GetTabs()) {
	    /* user wants to change tab stop setting.
	     * Don't let them if any loaded files had tabs in them.
	     * Otherwise, the user won't get what was expected.  If the
	     * reload the file after changing TABs, everything is fine.
	     * QB doesn't keep TABs in its pcode.
	     */
	    UiRsActivate(UNDEFINED);
	    while (NextMrsFile() != UNDEFINED) {
		if (mrsCur.flags2 & FM2_EntabSource)
		    tabs = UNDEFINED;
	    }
	}
	DrawDebugScr();
    }

    if (tabs == UNDEFINED)
	MsgBoxStd (MB_OK, MSG_CantSetTabs);
    else if (tabs <= 99 && tabs > 0)
	SetTabs (tabs);
EndCmdOptnsDisplay:	// [26]
    FreeCab (hcabOptnsDisplay);
}

/*** 
*VOID NEAR CmdOptnsPaths ()
*Purpose:
*	Called when OPTIONS/PATHS menu item is selected.
*
*Entry:
*	None.
*
*Exit:
*	None.
*
*Exceptions:
*	None.
*******************************************************************************/
VOID NEAR CmdOptnsPaths ()
{
    extern bd bdLibPath, bdExePath, bdInclPath, bdHelpPath;
    extern char b$buf1; 		//[32]

    if (!BdRealloc (&bdHelpPath, MAX_SEARCH_PATH )) {
	SetUiErrOm ();
	goto CmdOptnsPathsEnd;
    } 

    DbChkHoldBuf1();
    strcpy (&b$buf1, bdHelpPath.pb);
    if (PromptForString (cmdSwitches & CMD_SW_ED ? MSG_SearchPathEdit : MSG_SearchPathInterp, //[39]
			 (char far *) &b$buf1,			//[39]
			 MAX_SEARCH_PATH) != tmcCancel) {	//[39]
	fOptionsChanged = TRUE;
        strcpy (bdHelpPath.pb, &b$buf1);
        bdHelpPath.cbLogical = (CbSzUi(&b$buf1)) + 1;
    }
    DbChkFreeBuf1();

CmdOptnsPathsEnd:
    return;


}



bool fRightMouseHelp = TRUE;	// [14] default to right mouse help


/*** 
*void near ColorResolution ()
*Purpose:
*	Resolve all color dependencies.
*
*Entry:
*	Global isa's have been just set to their proper values.
*
*Exit:
*	Sets isaCurBreakpoint based on isaCurStmt and isaBreakpoint, and
*	isaIncludeFileHilite based on isaIncludeFileHilite, and isaEditWindow.
*
*Exceptions:
*	None.
*******************************************************************************/
void near ColorResolution ()		// [36]
{
    WORD coFore, coBack, dummy;
    /* for those statements that are both Current & Breakpoint, use:
     *	   the Current statement forground,
     *	   the Breakpoint statement background
     */
    GetIsaColor (isaCurStmt, &coFore, &dummy);
    GetIsaColor (isaBreakpoint, &dummy, &coBack);
    SetIsaColor (isaCurBreakpoint, coFore, coBack);

    /* for those statements that are Highlighted Include file lines, use:
     *	   the Include File Line foreground,
     *	   the Edit Window background
     */
    GetIsaColor (isaIncludeFileHilite, &coFore, &dummy);
    GetIsaColor (isaEditWindow, &dummy, &coBack);
    SetIsaColor (isaIncludeFileHilite, coFore, coBack);
}
