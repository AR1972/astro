/***
*uirsrcc.c - Resources (isolated for internationalization).
*
*	Copyright <C> 1985-1988, Microsoft Corporation
*
*Purpose:
*	Menu data structure definitions.
*
*******************************************************************************/

/* First, include version definition header */
#include <version.h>

/* Next, include TextWin's interface headers */
#include <cw/version.h>
#include <cw/windows.h>
#include <cw/edityp.h>

/* Next, include QBI's headers */
#ifndef UI_H
#include <ui.h>
#endif

#ifndef UIINT_H
#include <uiint.h>
#endif

/*------------------------------ Menu Bar ------------------------------------*/

#define CBMAX_FILE 14
static char szFileNew[] 	= "New";		/* [21] */
static char szFileOpen[]	= "Open...";		/* [21] */
static char szFileSave[]	= "Save";		/* [27] */
static char szFileSaveAs[]	= "Save As..."; 	/* [3] */
static char szFilePrint[]	= "Print...";		/* [3] */
static char szFileExit[]	= "Exit";		/* [3] */

// a-emoryh:
// The title for the PrintSetup dialog (a sub-dialog of FilePrint).
// This seemed the best place to put it, seeing as all the other dialog titles
// are generated from the strings in this file.
//
char szPrintSetup[]         = "Printer Setup";


// NOTE: The last parameter to the menuitem and menuitemX calls is the
// NOTE: number of characters of the string to use for the dialog box
// NOTE: title.  If it doesn't have dialog box, or it doesn't have a title,
// NOTE: this should be 0.  Make sure the EZ and full menu versions match!

#define CBMAX_FILEQHELP   8	// Enough for Print... and Exit
MENUITEM rgMenuItemFileQhelp[] = {
    menuitemX (midFilePrint,	szFilePrint,	0, 5)
    menuitemSep
    menuitemX (midFileExitQH,	szFileExit,	1, 0)
};

MENUITEM rgMenuItemFile[] = {
    menuitemX (midFileNew,	szFileNew,	0, 0)
    menuitemX (midFileOpen,	szFileOpen,	0, 4)
    menuitemX (midFileSave,	szFileSave,	0, 4)		//[27]
    menuitemX (midFileSaveAs,	szFileSaveAs,	5, 7)
    menuitemSep
    menuitemX (midFilePrint,	szFilePrint,	0, 5)
    menuitemSep
    menuitemX (midFileExit,	szFileExit,	1, 0)
};

#define CBMAX_EDIT 20
static char szEditCut[] 	= "Cut\tShift+Del";		// [3] [19]
static char szEditCopy[]	= "Copy\tCtrl+Ins";		// [3] [19]
static char szEditPaste[]	= "Paste\tShift+Ins";		// [3] [19]
static char szEditClear[]       = "Clear\tDel";
static char szEditNewSub[]	= "New SUB..."; 		/* [3] */
static char szEditNewFunc[]	= "New FUNCTION...";		/* [3] */

MENUITEM rgMenuItemEditQedit[] = {
    menuitemX (midEditCut,	szEditCut,	2, 0)
    menuitemX (midEditCopy,	szEditCopy,	0, 0)
    menuitemX (midEditPaste,	szEditPaste,	0, 0)
    menuitemX (midEditClear,    szEditClear,    2, 0)
};

MENUITEM rgMenuItemEdit[] = {
    menuitemX (midEditCut,	szEditCut,	2, 0)
    menuitemX (midEditCopy,	szEditCopy,	0, 0)
    menuitemX (midEditPaste,	szEditPaste,	0, 0)
    menuitemX (midEditClear,    szEditClear,    2, 0)
    menuitemSep
    menuitemX (midEditNewSub,	szEditNewSub,	4, 7)
    menuitemX (midEditNewFunc,	szEditNewFunc,	4, 12)
};

#define CBMAX_VIEW 21
static char szViewSubs[]	= "SUBs...\tF2";		// [3] [19]
static char szViewSplit[]	= "Split";			/* [3] */
static char szViewOutScrn[]	= "Output Screen\tF4";		// [3] [19]

MENUITEM rgMenuItemView[] = {
    menuitemX (midViewSubs,	szViewSubs,	0, 4)
    menuitemX (midViewSplit,	szViewSplit,	1, 0)
    menuitemX (midViewOutScrn,	szViewOutScrn,	0, 0)
};

#define CBMAX_SEARCH 24
static char szSearchFind[]	= "Find...";			/* [3] */
static char szSearchNext[]	= "Repeat Last Find\tF3";	// [3] [19]
static char szSearchChange[]	= "Change...";			/* [3] */

MENUITEM rgMenuItemSearchQhelp[] = {
    menuitemX (midSearchFind,	szSearchFind,	0, 4)
    menuitemX (midSearchNext,	szSearchNext,	0, 16)
};

MENUITEM rgMenuItemSearch[] = {
    menuitemX (midSearchFind,	szSearchFind,	0, 4)
    menuitemX (midSearchNext,	szSearchNext,	0, 16)
    menuitemX (midSearchChange,	szSearchChange, 0, 6)
};

#define CBMAX_RUN 19
static char szRunStart[]	= "Start\tShift+F5";		// [3] [19]
static char szRunRestart[]	= "Restart";			/* [3] */
static char szRunContinue[]	= "Continue\tF5";		// [3] [19]

MENUITEM rgMenuItemRun[] = {
    menuitemX (midRunStart,	szRunStart,	0, 0)
    menuitemX (midRunRestart,	szRunRestart,	0, 0)
    menuitemX (midRunContinue,	szRunContinue,	0, 0)		//[23]
};

#define CBMAX_DEBUG 27
static char szDebugStep[]	 = "Step\tF8";			/* [21] */
static char szDebugPStep[]	 = "Procedure Step\tF10";	/* [21] */
static char szDebugTraceOn[]	 = "Trace On";			/* [3] */
static char szDebugToggleBp[]	 = "Toggle Breakpoint\tF9";	// [3] [19]
static char szDebugClearAllBp[]  = "Clear All Breakpoints";	/* [3] */
static char szDebugSetNextStmt[] = "Set Next Statement"; 	/* [3] */

// IF Instant watch EVER changes postion, then UICTL.C shift+rightmouse
// needs to change postion.

MENUITEM rgMenuItemDebug[] = {
    menuitemX (midStep,			szDebugStep, 		0, 0)
    menuitemX (midPStep,		szDebugPStep, 		0, 0)
    menuitemSep
    menuitemX (midDebugTraceOn,		szDebugTraceOn, 	0, 0)
    menuitemSep
    menuitemX (midDebugToggleBp, 	szDebugToggleBp,	7, 0)
    menuitemX (midDebugClearAllBp,	szDebugClearAllBp,	0, 0)
    menuitemX (midDebugSetNextStmt,	szDebugSetNextStmt,	4, 0)
};

#define CBMAX_OPTNS 15
static char szOptnsDisplay[]	= "Display...";
static char szOptnsPaths[]	= "Help Path...";		// [12]
static char szOptnsSyntax[]	= "Syntax Checking";		/* [3] */

MENUITEM rgMenuItemOptnsQedit[] = {
    menuitemX (midOptnsDisplay,	szOptnsDisplay,	0, 7)
    menuitemX (midOptnsPaths,	szOptnsPaths,	5, 9)		// [21]
};

MENUITEM rgMenuItemOptns[] = {
    menuitemX (midOptnsDisplay,	szOptnsDisplay,	0, 7)
    menuitemX (midOptnsPaths,	szOptnsPaths,	5, 9)		// [21]
    menuitemX (midOptnsSyntax,	szOptnsSyntax,	0, 0)
};


/*
   The current keyword is inserted in the MID_HELP_SYNTAX menu item.
*/
#define OB_HELP_SYNTAX 7	//offset into szHelpSyntax to insert the keyword
#define CB_HELP_SYNTAX 8	//# of chars of keyword that are shown
//[14]   The keyword is inserted here
//[14]			      vvvvvvvv
char szHelpSyntax[] = "Topic:                 F1";		//[14]

#define CBMAX_HELP 25
static char szHelpIndex[]	= "Index";			/* [7] */
static char szHelpTable[]	= "Contents";			/* [7] */
static char szHelpHelp[]	= "Using Help\tShift+F1";	//[26]
static char szHelpAbout[]	= "About...";			//[25]

MENUITEM rgMenuItemHelp[] = {
    menuitemX (midHelpIndex,	szHelpIndex,	0, 0 )		/* [5] */
    menuitemX (midHelpTable,	szHelpTable,	0, 0 )		/* [5] */
    menuitemX (midHelpSyntax,	szHelpSyntax,	0, 0 )
    menuitemX (midHelpHelp,	szHelpHelp,	6, 0 )		//[26]
    menuitemSep 						//[25]
    menuitemX (midHelpAbout,	szHelpAbout,	0, 0 )		//[25]
};

#define CBMAX_EDITHELP 15					//[25]
static char szHelpStarted[]	= "Getting Started";		//[25]
static char szHelpKeyboard[]	= "Keyboard";			//[25]

MENUITEM rgMenuItemEditHelp[] = {				//[25]
    menuitemX (midHelpStarted,	szHelpStarted,	0, 0 )		//[25]
    menuitemX (midHelpKeyboard,	szHelpKeyboard,	0, 0 )		//[25]
    menuitemSep 						//[25]
    menuitemX (midHelpAbout,	szHelpAbout,	0, 0 )		//[25]
};								//[25]

#define CBMAX_QHELPHELP 24
static char szHelpHowToUse[]	= "How to Use MS-DOS Help";

MENUITEM rgMenuItemQhelpHelp[] = {
    menuitemX (midHelpHowToUse, szHelpHowToUse, 0, 0 )
    menuitemSep
    menuitemX (midHelpAbout,	szHelpAbout,	0, 0 )
};


static char szFile[]		= "File";	/* [3] */
static char szEdit[]		= "Edit";	/* [3] */
static char szView[]		= "View";	/* [3] */
static char szSearch[]		= "Search";	/* [3] */
static char szRun[]		= "Run";	/* [3] */
static char szDebug[]		= "Debug";	/* [3] */
static char szOptns[]		= "Options";
static char szHelp[]		= "Help";	//[12]

MENU rgMenuQedit[] = {

    {
	midFile,					/* idMenu      */
	3,						/* rxTitle     */
	0,						/* ichHilite   */
	FALSE, TRUE, 0, 				/* fHandle, fEnabled, filler */
	4,						/* cchTitle    */
	szFile, 					/* pchTitle    */
	sizeof (rgMenuItemFile) / sizeof (MENUITEM),	/* cItem       */
	CBMAX_FILE,					/* cchItemMax  */
	rgMenuItemFile, 				/* rgMenuItem  */
	0						/* wParamUser  */
    },

    {
	midEdit,					/* idMenu      */
	9,						/* rxTitle     */
	0,						/* ichHilite   */
	FALSE, TRUE, 0, 				/* fHandle, fEnabled, filler */
	4,						/* cchTitle    */
	szEdit, 					/* pchTitle    */
	sizeof (rgMenuItemEditQedit) / sizeof (MENUITEM),	/* cItem       */
	CBMAX_EDIT,					/* cchItemMax  */
	rgMenuItemEditQedit, 				/* rgMenuItem  */
	0						/* wParamUser  */
    },

    {
	midSearch,					/* idMenu      */
	15,						/* rxTitle     */
	0,						/* ichHilite   */
	FALSE, TRUE, 0, 				/* fHandle, fEnabled, filler */
	6,						/* cchTitle    */
	szSearch,					/* pchTitle    */
	sizeof (rgMenuItemSearch) / sizeof (MENUITEM),	/* cItem       */
	CBMAX_SEARCH,					/* cchItemMax  */
	rgMenuItemSearch,				/* rgMenuItem  */
	0						/* wParamUser  */
    },

    {
	midOptns,					/* idMenu      */
	23,						/* rxTitle     */
	0,						/* ichHilite   */
	FALSE, TRUE, 0, 				/* fHandle, fEnabled, filler */
	7,						/* cchTitle    */
	szOptns,					/* pchTitle    */
	sizeof (rgMenuItemOptnsQedit) / sizeof (MENUITEM),	/* cItem       */
	CBMAX_OPTNS,					/* cchItemMax  */
	rgMenuItemOptnsQedit,				/* rgMenuItem  */
	0						/* wParamUser  */
    },

    {
	midHelp,					/* idMenu      [25] */
	74,						/* rxTitle     [25] */
	0,						/* ichHilite   [25] */
	FALSE, TRUE, 0, 				/* fHandle, fEnabled, filler */
	4,						/* cchTitle    [25] */
	szHelp, 					/* pchTitle    [25] */
	sizeof (rgMenuItemEditHelp) / sizeof (MENUITEM),/* cItem       [25] */
	CBMAX_EDITHELP,					/* cchItemMax  [25] */
	rgMenuItemEditHelp,				/* rgMenuItem  [25] */
	0						/* wParamUser  [25] */
    },


};

MENU rgMenuQhelp[] = {

    {
	midFile,					/* idMenu      */
	3,						/* rxTitle     */
	0,						/* ichHilite   */
	FALSE, TRUE, 0, 				/* fHandle, fEnabled, filler */
	4,						/* cchTitle    */
	szFile, 					/* pchTitle    */
	sizeof(rgMenuItemFileQhelp) / sizeof(MENUITEM), /* cItem       */
	CBMAX_FILEQHELP,				/* cchItemMax  */
	rgMenuItemFileQhelp,				/* rgMenuItem  */
	0						/* wParamUser  */
    },

    {
	midSearch,					/* idMenu      */
	9,						/* rxTitle     */
	0,						/* ichHilite   */
	FALSE, TRUE, 0, 				/* fHandle, fEnabled, filler */
	6,						/* cchTitle    */
	szSearch,					/* pchTitle    */
	sizeof(rgMenuItemSearchQhelp) / sizeof(MENUITEM), /* cItem	 */
	CBMAX_SEARCH,					/* cchItemMax  */
	rgMenuItemSearchQhelp,				/* rgMenuItem  */
	0						/* wParamUser  */
    },

    {
	midHelp,					/* idMenu      [25] */
	74,						/* rxTitle     [25] */
	0,						/* ichHilite   [25] */
	FALSE, TRUE, 0, 				/* fHandle, fEnabled, filler */
	4,						/* cchTitle    [25] */
	szHelp, 					/* pchTitle    [25] */
	sizeof (rgMenuItemQhelpHelp) / sizeof (MENUITEM),/* cItem	[25] */
	CBMAX_QHELPHELP,				/* cchItemMax  [25] */
	rgMenuItemQhelpHelp,				/* rgMenuItem  [25] */
	0						/* wParamUser  [25] */
    },


};

MENU rgMenuQbas[] = {

    {
	midFile,					/* idMenu      */
	3,						/* rxTitle     */
	0,						/* ichHilite   */
	FALSE, TRUE, 0, 				/* fHandle, fEnabled, filler */
	4,						/* cchTitle    */
	szFile, 					/* pchTitle    */
	sizeof (rgMenuItemFile) / sizeof (MENUITEM),	/* cItem       */
	CBMAX_FILE,					/* cchItemMax  */
	rgMenuItemFile, 				/* rgMenuItem  */
	0						/* wParamUser  */
    },

    {
	midEdit,					/* idMenu      */
	9,						/* rxTitle     */
	0,						/* ichHilite   */
	FALSE, TRUE, 0, 				/* fHandle, fEnabled, filler */
	4,						/* cchTitle    */
	szEdit, 					/* pchTitle    */
	sizeof (rgMenuItemEdit) / sizeof (MENUITEM),	/* cItem       */
	CBMAX_EDIT,					/* cchItemMax  */
	rgMenuItemEdit, 				/* rgMenuItem  */
	0						/* wParamUser  */
    },

    {
	midView,					/* idMenu      */
	15,						/* rxTitle     */
	0,						/* ichHilite   */
	FALSE, TRUE, 0, 				/* fHandle, fEnabled, filler */
	4,						/* cchTitle    */
	szView, 					/* pchTitle    */
	sizeof (rgMenuItemView) / sizeof (MENUITEM),	/* cItem       */
	CBMAX_VIEW,					/* cchItemMax  */
	rgMenuItemView, 				/* rgMenuItem  */
	0						/* wParamUser  */
    },

    {
	midSearch,					/* idMenu      */
	21,						/* rxTitle     */
	0,						/* ichHilite   */
	FALSE, TRUE, 0, 				/* fHandle, fEnabled, filler */
	6,						/* cchTitle    */
	szSearch,					/* pchTitle    */
	sizeof (rgMenuItemSearch) / sizeof (MENUITEM),	/* cItem       */
	CBMAX_SEARCH,					/* cchItemMax  */
	rgMenuItemSearch,				/* rgMenuItem  */
	0						/* wParamUser  */
    },

    {
	midRun, 					/* idMenu      */
	29,						/* rxTitle     */
	0,						/* ichHilite   */
	FALSE, TRUE, 0, 				/* fHandle, fEnabled, filler */
	3,						/* cchTitle    */
	szRun,						/* pchTitle    */
	sizeof (rgMenuItemRun) / sizeof (MENUITEM),	/* cItem       */
	CBMAX_RUN,					/* cchItemMax  */
	rgMenuItemRun,					/* rgMenuItem  */
	0						/* wParamUser  */
    },

    {
	midDebug,					/* idMenu      */
	34,						/* rxTitle     */
	0,						/* ichHilite   */
	FALSE, TRUE, 0, 				/* fHandle, fEnabled, filler */
	5,						/* cchTitle    */
	szDebug,					/* pchTitle    */
	sizeof (rgMenuItemDebug) / sizeof (MENUITEM),	/* cItem       */
	CBMAX_DEBUG,					/* cchItemMax  */
	rgMenuItemDebug,				/* rgMenuItem  */
	0						/* wParamUser  */
    },

    {
	midOptns,					/* idMenu      */
	41,						/* rxTitle     */
	0,						/* ichHilite   */
	FALSE, TRUE, 0, 				/* fHandle, fEnabled, filler */
	7,						/* cchTitle    */
	szOptns,					/* pchTitle    */
	sizeof (rgMenuItemOptns) / sizeof (MENUITEM),	/* cItem       */
	CBMAX_OPTNS,					/* cchItemMax  */
	rgMenuItemOptns,				/* rgMenuItem  */
	0						/* wParamUser  */
    },

    {
	midHelp,					/* idMenu      [12] */
	74,						/* rxTitle     [12] */
	0,						/* ichHilite   [12] */
	FALSE, TRUE, 0, 				/* fHandle, fEnabled, filler */
	4,						/* cchTitle    [12] */
	szHelp, 					/* pchTitle    [12] */
	sizeof (rgMenuItemHelp) / sizeof (MENUITEM),	/* cItem       [12] */
	CBMAX_HELP,					/* cchItemMax  [12] */
	rgMenuItemHelp, 				/* rgMenuItem  [12] */
	0						/* wParamUser  [12] */
    },

};

/*---------------------- Main Accelerator Keys ------------------------------*/

MPVKEYID rgmpvkeyidQedit[] = {
/* [2] Add ALT+ and ALT- */
    '+' | KK_ALT,		midWndGrow,		/* [2] */
    '=' | KK_ALT | KK_SHIFT,	midWndGrow,	//[17] main kbd '+' is shift '='
    '-' | KK_ALT,		midWndShrink,		/* [2] */
    '\x7f',			midEditClear2,		/* [2] */
    '\x7f' | KK_SHIFT,		midEditCut2,		/* [2] */
    VK_INSERT | KK_CONTROL,	midEditCopy,
    VK_INSERT | KK_SHIFT,	midEditPaste,
    '\x0c' | KK_CONTROL,	midSearchNext,		/* [2] Ctrl+L */
    VK_F1 | KK_ALT,		midHelpBack,		// [16]
    VK_F3,			midSearchNext,
    VK_F6,			midNextWindow,
    VK_F6 | KK_SHIFT,		midPreviousWindow,
    0,	  0
};

MPVKEYID rgmpvkeyidQhelp[] = {
/* Add ALT+ and ALT- */
    VK_F1 | KK_ALT,		midHelpBack,		// [16]
    VK_F3,			midSearchNext,
//
// LOCALIZATION -
//    Change this char in the following 3 lines if you need to change the
//            |         accelerator character.
//            |
//            v
//
    (VK_A+(  'C'  -'A')) | KK_ALT,   midHelpHelp,         // Contents
    (VK_A+(  'N'  -'A')) | KK_ALT,   midHelpNext,         // Next
    (VK_A+(  'B'  -'A')) | KK_ALT,   midHelpBack,         // Back
    0,	  0
};

MPVKEYID rgmpvkeyidQbas[] = {
/* [2] Add ALT+ and ALT- */
    '+' | KK_ALT,		midWndGrow,		/* [2] */
    '=' | KK_ALT | KK_SHIFT,	midWndGrow,	//[17] main kbd '+' is shift '='
    '-' | KK_ALT,		midWndShrink,		/* [2] */
//[21]    '\x08' | KK_ALT,		midEditUndo,		/* [2] */
    '\x7f',			midEditClear2,		/* [2] */
    '\x7f' | KK_SHIFT,		midEditCut2,		/* [2] */
    VK_INSERT | KK_CONTROL,	midEditCopy,
    VK_INSERT | KK_SHIFT,	midEditPaste,
    '\x0c' | KK_CONTROL,	midSearchNext,		/* [2] Ctrl+L */
//[21]    '\x1c' | KK_CONTROL,	midSearchSel,		/* [2] Ctrl+\ */
    VK_F1 | KK_ALT,		midHelpBack,		// [16]
    VK_F2,			midViewSubs,
    VK_F2 | KK_SHIFT,		midViewNextProc,
    VK_F2 | KK_CONTROL, 	midViewPrevProc,
    VK_F3,			midSearchNext,
    VK_F4,			midViewOutScrn,
    VK_F5,			midRunContinue,
    VK_F5 | KK_SHIFT,		midRunStart,
    VK_F5 | KK_CONTROL, 	midWndRestore,
    VK_F6,			midNextWindow,
    VK_F6 | KK_SHIFT,		midPreviousWindow,
    VK_F7,			midGoUntilCursor,
    VK_F8,			midStep,
//[21]    VK_F8 | KK_SHIFT,		midHistBack,
    VK_F9,			midDebugToggleBp,
    VK_F10,			midPStep,
//[21]    VK_F10 | KK_SHIFT,		midHistForward,
    VK_F10 | KK_CONTROL,	midWndMaximize,
    0,	  0
};

MENUBAR MenuBarQedit = {
    sizeof (rgMenuQedit) / sizeof (MENU),
    rgMenuQedit,	//[21]
    rgmpvkeyidQedit
};

MENUBAR MenuBarQhelp = {
    sizeof (rgMenuQhelp) / sizeof (MENU),
    rgMenuQhelp,
    rgmpvkeyidQhelp
};

MENUBAR MenuBarQbas = {
    sizeof (rgMenuQbas) / sizeof (MENU),
    rgMenuQbas,		//[21]
    rgmpvkeyidQbas
};


void NEAR SetHelpKeyword (szSrc)
char *szSrc;
{
    register ushort cb;
    register char ch;
    char *pchDst;

    cb = CB_HELP_SYNTAX;
    pchDst = szHelpSyntax + OB_HELP_SYNTAX;
    while(cb && (ch = *szSrc++)) {
	*pchDst++ = ch;
	cb--;
    }
    while (cb--) {
	*pchDst++ = ' ';
    }
}
