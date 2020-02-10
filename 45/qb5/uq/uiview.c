/*** 
*uiview.c - Support View menu items.
*
*	Copyright <C> 1985-1988, Microsoft Corporation
*
*Purpose:
*	All View menu support routines, and C Debug menu support.
*
*******************************************************************************/

/* First, include version definition header */
#include <version.h>

/* Next, include TextWin's interface headers */
#include <cw/version.h>
#include <cw/windows.h>
#include <cw/edityp.h>
#include <uiext.h>

/* include dialog information */
#include <cw/dlg.h>
#include "uiview.hs"
#include "uiview.sdm"

/* Next, include QBI's headers */
#ifndef CONTEXT_H
#include <context.h>
#endif

#ifndef NAMES_H				/* [1] */
#include <names.h>		       /* [1] */
#endif					/* [1] */

#ifndef PARSER_H
#include <parser.h>
#endif

#ifndef QBIMSGS_H
#include <qbimsgs.h>
#endif

#ifndef RTPS_H
#include <rtps.h>
#endif

#ifndef TXTMGR_H
#include <txtmgr.h>
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

ushort oRsOfAlphaMrs(ushort);
void NEAR DrawViewListBox(PWND, ushort, ushort);
STATICF(bool) UnloadFile(ushort);		//[32]
STATICF(void) DescribeORs(void);
WORD NEAR DoViewSub (TMC);

bool NEAR FChInSet(char ch, char *szSet);	//[26]
bool NEAR FValidId(char *szName);		//[26]

bool fPcodeOnly;

extern ushort iHelpId;		//[11]


ushort oRsInitial; /* set by CmdViewSubs() for ViewSubsDialog() */
ushort oRsView;    /* set by ViewSubsDialog() for CmdViewSubs() */
ushort oRsDst;	   /* set by ViewSubsDialog() for CmdViewSubs() */
ushort oProcOrModule;

#define CB_LISTBOX_COL 20
#define CBMAX_EXPLEN 40 /* [10] max length of expression for instant watch */

/**********************************************************************
* oRsOfAlphaMrs(iAlpha)
* Purpose:
*	Just like oRsOfAlpha, but it skips prs entries.
*	Returns the oRs of the iAlpha'th alphabetically sorted mrs.
*
**********************************************************************/
ushort oRsOfAlphaMrs(iAlpha)
ushort iAlpha;
{
    ushort i;
    ushort oRs;

    i = 0;
    while (TRUE) {
	if (((oRs = oRsOfAlpha (i++)) & 0x8000) == 0) {
	    if (oRs == 0x7fff) {
		return (UNDEFINED);
	    }
	    UiRsActivate (oRs);
	    if (!fPcodeOnly || !(mrsCur.flags2 & (FM2_NoPcode | FM2_Include))) {
		if (iAlpha-- == 0) {
		    return (oRs);
		}
	    }
	}
    }
}

WORD FAR
WListProcModuleAndSub (tmm, sz, isz, tmc, wParam, bArg)
WORD tmm;
char *sz;
WORD isz;
TMC tmc;
WORD wParam, bArg;
{
    WORD cRs, oRs;

    Unreferenced (wParam);
    Unreferenced (bArg);

    switch (tmm) {
	case tmmCount:
	    oProcOrModule = 0;
	    return ((WORD) -1);
	case tmmText:
	case tmmEditText:

	    cRs = UiAlphaORsBuild();
	    if (!cRs) {
		SetUiErrOm ();
	    }

	    if (uierr) {		//[27] if any errors occured
		*sz = '\0';		//[27] return a null string if needed
		return (isz == 0);	//[27] no error for first time
	    }

	    if (isz >= cRs) {
		return (FALSE);
	    }

	    switch (tmc) {
		case tmcListModulesAndSubs: oRs = oRsOfAlpha (isz); break;
	    }

	    if (oRs == UNDEFINED) {
		return (FALSE);
	    }

	    UiRsActivate (oRs);
	    if (oRs == oRsInitial) {
		oProcOrModule = isz;
	    }
	    GetRsName (grs.oRsCur,
		       (grs.oRsCur & 0x8000) ? RSN_fIndent : 0,
		       CB_LISTBOX_COL);
	    strcpy (sz, bufStdMsg);
	    break;
    }
    return (TRUE);
}



/* -------------------------------- *
 *          View Subs               *
 * -------------------------------- */

STATICF(bool) UnloadFile (oRsDiscard)		//[32]
ushort oRsDiscard;
{
    ushort result;

    EditMgrFlush1 ();	// [22] [16] CmdFileSave used editmgr buffer
    UiRsActivate (oRsDiscard);
    DbAssert ((mrsCur.flags2 & FM2_File) != 0);

    /* Give user a chance to back out of action, descan module's text
     * table to SS_RUDE (need to at least descan to SS_PARSE before
     * calling MrsDiscard().
     */
    if (!AskRudeEditFar ())
	return(FALSE);	 /* [32] user pressed CANCEL button */
    DoDrawDebugScr ();
    /* calling AskRudeEditFar can cause the output screen to be shown
     * (because of RunInit()), in which case, we need to re-draw the
     * list windows now.
     */

    if (mrsCur.flags2 & FM2_Modified) {
	/* give user a chance to save modified file */
	oRsDiscard = grs.oRsCur;
	if ((result = MsgBoxStd (MB_YESNOCANCEL, MSG_NotSaved)) == IDYES) {
	    if (!CmdFileSave()) {
		/* I/O error, or user pressed CANCEL button */
		if (uierr != 0)
		    ReportError ();   /* user didn't press CANCEL */
		return(FALSE);		//[32]
	    }
	}
	if (result == tmcCancel)
	    return(FALSE);		//[32]
	UiRsActivate (oRsDiscard);  /* CmdFileSave could activate other mrs's */
    }
    MrsDiscard ();
    return(TRUE);			//[32]
}

/**************************************************************************
* DescribeORs()
* Purpose:
*  Display the type of file currently selected in the ViewSubs dialog.
*  This message is one of the following (in 50 columns or less):
*       00000000011111111112222222222333333333344444444445 
*       12345678901234567890123456789012345678901234567890
*       xxxxxxxx.xxx is the Main Module
*       xxxxxxxx.xxx is a Module
*       xxxxxxxx.xxx is an Include File
*       xxxxxxxx.xxx is a Document
*       xxxxxxxxxxxxxxxxxx is a Subprogram in yyyyyyyy.yyy 
*       xxxxxxxxxxxxxxxxxx is a Function in yyyyyyyy.yyy
*       00000000011111111112222222222333333333344444444445
*       12345678901234567890123456789012345678901234567890
*
* Entry:
*  grs.oRsCur identifies currently selected module.
*
**************************************************************************/
STATICF(void)
DescribeORs()
{
    ushort msgId;
    char szDesc[71];   //IPG- from 51, length of buffer needed to increase.
		       // since functions names were being truncated in view box
    *szDesc = '\0';
    GetRsName(grs.oRsCur, 0, (grs.oRsCur & 0x8000) ? 18 : 12);
    strcat (szDesc, bufStdMsg);
    ListStdMsg(MSG_Is);
    strcat (szDesc, bufStdMsg);
    if (grs.oRsCur & 0x8000) {
	/* it is an prs, not a mrs */
	msgId = (prsCur.procType == PT_FUNCTION) ?
	    MSG_Function : MSG_Sub;
    }
    else {
	/* it is an mrs, not a prs */
	msgId = (mrsCur.flags2 & FM2_Include) ?
		MSG_IncludeFile : (mrsCur.flags2 & FM2_NoPcode) ?
		MSG_Document : (grs.oMrsMain == grs.oRsCur) ?
		MSG_MainModule : MSG_Module;
    }
    ListStdMsg(msgId);
    strcat (szDesc, bufStdMsg);
    if (grs.oRsCur & 0x8000) {
	/* it is an prs, not a mrs */
	GetRsName(grs.oMrsCur, 0, 12);
	strcat (szDesc, bufStdMsg);
    }

    SetTmcText (tmcViewSubsDescription, szDesc);
}


BOOL FAR
FDlgViewSubs (dlm, tmc, wNew, wOld, wParam)
WORD dlm;
TMC tmc;
WORD wNew, wOld, wParam;
{
    Unreferenced (wNew);
    Unreferenced (wOld);
    Unreferenced (wParam);
    Unreferenced (tmc);

    if (uierr && dlm != dlmTerm)	//[36] Return FALSE if OM error, unless
	return (FALSE); 		//[36]	we are trying to terminate

    switch (dlm) {
	case dlmInit:
	    SetTmcVal (tmcListModulesAndSubs, oProcOrModule);
	    SetTmcListWidth (tmcListModulesAndSubs, 3);       //[12]
	case dlmClick:
	    if (uierr)			//[27] if there was an error, the call
		break;			//[27] to oRsOfAlpha would be bogus.
	    oRsView = oRsOfAlpha (GetTmcVal (tmcListModulesAndSubs));
	    UiRsActivate (oRsView);
	    DescribeORs ();
	    break;

	case dlmTerm:			//[27] we always want to
	    return (TRUE);		//[27] allow termination.
    }

    return (!uierr);
}


/*** 
*VOID NEAR CmdViewSubs ()
*Purpose:
*	Perform View/Subs command.
*
*Entry:
*	None.
*
*Exit:
*	mrsCur is changed to new oRs from View/Subs dialog.
*
*Exceptions:
*	None.
*******************************************************************************/
VOID NEAR CmdViewSubs()
{
    REG1 ushort oRsReg;
    sd sdName;
    HCABViewSubs hcabViewSubs;
    TMC tmc;
    ushort ogNam;

/*********************************************************************
 *    If there is selected text then we want to jump directly to
 *    that procedure. If there is no selected text, but the cursor
 *    is on the name of a procedure, then we want to bring up the
 *    list box with that procedure name selected.
 *********************************************************************/
    oRsReg = oRsInitial = UNDEFINED;

    if (GetSelText(&bufStdMsg[0], CB_IDNAM_MAX + 1) != 0) {
	/* user has selected an identifier, make it the default */
	oRsReg = 0;
    }
    else if (GetEditWord( bufStdMsg, CB_IDNAM_MAX + 1) != 0) {
	/* cursor is after an identifier */
	oRsReg = oRsInitial = 0;
    }

    if (oRsReg == 0) {
	sdName.pb = bufStdMsg;
	sdName.cb = CbSzUi(bufStdMsg);
	if (!(ogNam = OgNamOfPsd(&sdName))) {
	   /* couldn't find the ogNam don't preselect a prs */
	   oRsReg = UNDEFINED;
	}
	else {
	    oRsReg = PrsFind(ogNam) | 0x8000;		/* [1] */
	    UiRsActivate(oRsReg);
	    if (txdCur.flags & FTX_mrs) {
		/* selected name was for a prs with no text table, like a
		 * DEF FN or SUB which is only called but has not text table.
		 * Treat this the same as if nothing was selected.
		 */
		oRsReg = UNDEFINED;
	    }
	    if (oRsInitial != UNDEFINED) {
		oRsInitial = oRsReg;
		oRsReg = UNDEFINED;
	    }
	}
    }

    if ((oRsView = oRsReg) == UNDEFINED) {
	/* Selected text (if any) didn't match any known prs.
	 * Let user select prs from a list */

	DbAssert(uierr == 0)	//[29]

	hcabViewSubs = (HCABViewSubs) HcabAlloc (cabiCABViewSubs);
	/* [5] Catch HcabAlloc errors */
	if (uierr)
	    return;

	(*hcabViewSubs)->oModuleOrSub = 0;

	while (DoViewSub (tmc = TmcDoDlgFar (&dlgViewSubs, sizeof (dlgViewSubs), hcabViewSubs))) ;

	FreeCab (hcabViewSubs);

	/* user pressed CANCEL or got error (like out-of-memory) */
	if (tmc == tmcCancel || uierr != 0) {
	    return;
	}

	/* user didn't press CANCEL button */
	UiRsActivate (oRsView); 	//[30] if DIALOG_NOSAVE, oRs may be wrong
    }

    /* [5] Fix QB4.1 PTR #107, fix View/Subs Split functionality */
    /* else user wants to edit new oRs in current window */
    UiRsActivate(oRsView);
    /* open list window if only command window is open.
     * make sure active window is a list window.
     * show text table identified by grs.oRsCur in that list window.
     */
    WndAssignList();
}

WORD NEAR
DoViewSub (tmc)
TMC tmc;
{

    if (tmc != tmcDelete)	//[33]
	return (FALSE);

    if ((oRsView & 0x8000) == 0) {
	/* user selected a module */
    }
    else {
	/* deleting or moving a procedure */
	    /* deleting a procedure, make sure its not a mistake */
	    if (MsgBoxStd(MB_OKCANCEL, MSG_DelProc) != IDDEFAULT)
		return (TRUE);	 /* user pressed cancel */
    } /* deleting or moving a procedure */
    AlphaORsFree();
    oRsInitial = grs.oMrsCur;

    if (tmc == tmcDelete) {
	if ((oRsView & 0x8000) == 0) {
	    /* user selected a module */

	    UnloadFile(oRsView);
	}
	else {
	    /* user selected a procedure */

	    /* Give user a chance to back out of action */
	    if (AskCantCONT()) {
		UiRsActivate(oRsView);
		PrsDiscard();
	    }
	}
    }


    /* redraw list window behind dialog box to reflect change */
    DoDrawDebugScr();
    return (uierr == 0);
}


/*** 
*bool NEAR FChInSet(char ch, char *szSet)
*Purpose:
*	Returns TRUE if ch is in the set specified by szSet.
*
*	new for revision [26]
*
*Entry:
*	ch - The char to check for.
*	szSet - a string of characters to check ch against.
*	        if szSet[n] == '-' then
*		    check if ch is in the interval szSet[n+1]...szSet[n+2]
*
*Exit:
*	returns TRUE if ch is in the specified set.
*
*******************************************************************************/
bool NEAR FChInSet(ch, szSet)
REG2 char ch;
REG1 char *szSet;
{

    while (*szSet != 0) {
	if (*szSet == '-') {
	    if (ch >= szSet[1] && ch <= szSet[2])
	       return(TRUE);
	    szSet += 3;
	}
	else if (ch == *szSet++)
	   return(TRUE);
    }
    return(FALSE);
}

/*** 
*bool NEAR FValidId(char *szName)
*Purpose:
*	Checks if szName is a valid identifier.
*
*	new for revision [26]
*
*Entry:
*	szName - The name to check.
*
*Exit:
*	returns TRUE if szName is valid
*
*******************************************************************************/
bool NEAR FValidId(szName)
REG1 char *szName;
{
    REG2 char *szCharsCur;
    static char szCharsId[] = "-09.-AZ-az";

    /* First character must be alphabetic */
    szCharsCur = szCharsId + 4;
    do {
	if (!FChInSet(*szName++, szCharsCur))
	    return(FALSE);
	/* Subsequent characters can also be numberic or period */
	szCharsCur = szCharsId;
    } while (*szName);
   
    return(TRUE);
}

sd sdProcName;

VOID NEAR CmdNewProc (rsType)
char rsType;
{
    ushort lnCursor;
    ushort oRsProc;
    char szProcName[CB_IDNAM_MAX + 1];
    HCABNewProc hcabNewProc;
    ushort oType;	//[26]

    /* New Sub/Function is a Rude Edit */
    if (!AskRudeEditFar())
	return;   /* user wants to abort action */

    DbAssert(uierr == 0);	 //[29]

    hcabNewProc = (HCABNewProc) HcabAlloc (cabiCABNewProc);
    /* [5] Catch HcabAlloc errors */
    if (uierr)
	return;

    sdProcName.pb = szProcName;
    bufStdMsg[0] = 0;
    if (GetSelText (&bufStdMsg[0], CB_IDNAM_MAX + 1) == 0) {
	GetEditWord (bufStdMsg, CB_IDNAM_MAX + 1);
	/* fill bufStdMsg with id if cursor is after an identifier */
    }
    strcpy (szProcName, bufStdMsg );
    sdProcName.cb = CbSzUi (szProcName);
    SzToCab (hcabNewProc, szProcName, Iag (CABNewProc, szProcName));
    // [18] if SzToCab memory allocation failed, free cab and exit.
    if (uierr)
	goto CmdNewProcEnd;

Retry:
    if (TmcDoDlgFar (&dlgNewProc, sizeof (dlgNewProc), hcabNewProc) != tmcCancel) {
	SzFromCab (hcabNewProc, szProcName, CB_IDNAM_MAX + 1,	// [15]
		   Iag (CABNewProc, szProcName));
	if ((sdProcName.cb = CbSzUi (szProcName)) == 0) {
	    MsgBoxStd (MB_OK, MSG_MustSpecifyName);
	    goto Retry;
	}

	oType = OTypeOfTypeCharFar(szProcName[sdProcName.cb-1]);	//[26]
	if (oType)							//[26]
	    szProcName[--(sdProcName.cb)] = 0;				//[26]

	/* don't let user try to put param list in with name */
	if ((rsType != RS_function && oType != ET_IMP)  //[26]
		|| !FValidId(szProcName)) {		//[26]
	    MsgBoxStd (MB_OK, MSG_BadId);
	    goto Retry;
	}

	UiGrabSpace ();
	/* Don't allocate memory that could steal
	 * from our ability to execute a SYSTEM, CLEAR, or SETMEM stmt.
	 */
	if ((oRsProc = RsMake (&sdProcName, rsType)) != UNDEFINED) {
	    WndAssignList ();	/* make new proc visible in list window */
	    /* RsMake encountered no errors */
	    TxtPrsInit (oType); /* Insert module's DEFxxx state in proc */
	    /* Place cursor at end of SUB/FUNCTION line */
	    lnCursor = txdCur.cLines - 2;
	    MoveCursorPwndCur (lnCursor,
			       cbGetLineBuf (oRsProc,
					     lnCursor,
					     ps.bdpSrc.cbLogical,
					     ps.bdpSrc.pb));
	    DrawDebugScr ();
	}
	UiReleaseSpace ();
    }

CmdNewProcEnd:	// [18]
    FreeCab (hcabNewProc);
}

/*************************************************************************
* CmdViewInclude(fShow)
* Purpose:
*	Make $INCLUDE files in all list windows visible if fShow!=0,
*	or invisible if fShow==0.
*	Cursor is kept as close to current line as possible.
*
*************************************************************************/
VOID FAR CmdViewInclude(fShow)
REG1 bool fShow;
{

    if (fShow != fViewInclude) {
	DbAssert(FALSE);	//[35] shouldn't ever get here
    }
} /* CmdViewInclude */
