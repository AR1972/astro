/***
*uisearch.c - Search and Replace code.
*
*       Copyright <C> 1985-1988, Microsoft Corporation
*
*Purpose:
*       Routines to support Search/Find, and Search/Change.
*
*******************************************************************************/

#include <version.h>

/* Next, include TextWin's interface headers */
#include <cw/version.h>
#include <cw/windows.h>
#include <cw/edityp.h>

#include <uiext.h>
#include <uinhelp.h>            //[17]

/* include dialog information */
#include <cw/dlg.h>
#include "uisearch.hs"
#include "uisearch.sdm"

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

#ifndef RTPS_H
#include <rtps.h>
#endif


/* Externals */
STATICF(boolean) StartSearch(void);         //[18]
STATICF(boolean) SearchNext(boolean);
STATICF(boolean) FEndOfSearch(void);
STATICF(boolean) SearchLine(void);
STATICF(ushort) obFind(char *, ushort, ushort, char *);
STATICF(ushort) obFindLabel(char *, ushort, ushort, char *);
STATICF(void) DoChange(void);
STATICF(void) MakeBdlFromBuf (bdl *, char *);  //[28]
STATICF(void) MakeBufFromBdl (char *, bdl *);  //[28]
STATICF(boolean) fSrchLastAlpha (ushort);
STATICF(ushort) SrchORsOfAlpha (ushort);
STATICF(TMC) DoDlgSearch (void far *, WORD, WORD, BOOL);
AY FAR PASCAL GetUILine (void); //[39]
void NEAR SelectTextPwndCur(ushort, ushort, ushort); //[39]

#define isInActiveWindow 0              //[39] taken from UISEARCH.DES
#define isInCurrentModule 1             //[39]

/* Static Data */
boolean fMatchCase = FALSE;
boolean fWholeWord = FALSE;
boolean fLabel;
ushort  oSearchInSave = isInCurrentModule;      //[36]

/*
  The following are used to determine when a search is complete.
  They are set by StartSearch and SearchNext, and are tested by FEndOfSearch.
  They are updated by SrchReAssign.
  No one else should touch them.
*/
ushort ioRsSrchStart;
ushort ioRsSrchCur;
bd bdSearchAlpha;
static ushort olnStart;
static ushort obStart;
static ushort olnCur;
static ushort obCur;
static boolean fWrapped;

/*
  scopeOfSearch controls the scope of the search.
  It is one of:
     isInActiveWindow, isInCurrentModule
*/
static ushort scopeOfSearch = isInCurrentModule;

/*
  bdlSaveFind is where the last search string is saved for later use
  (It is saved in the far heap to save DGROUP space).
*/
#define CBMAX_FIND      FILNAML         //[28]
#define CBMAX_CHANGE    FILNAML         //[28]
bdl bdlSaveFind = { 0, NOT_OWNER, 0, 0 };
bdl bdlSaveChange = { 0, NOT_OWNER, 0, 0 };     //[28]


/****
*STATICF(TMC) DoDlgSearch - Display dialog for standard searches
*
*Purpose:
*       This routine will display the dialog for any type of search and
*       also for search/change.  It will deturmine the proper defaults for
*       all the buttons from static variables.  After the dialog is displayed,
*       this routine will update the static variables to reflect what the
*       user actually selected.
*
*Entry:
*       pdlg    - Pointer to dialog structure
*       cbDlg   - size of dialog structure
*       cabi    - cab for dialog
*       fSearchChange - TRUE if this dialog is the Search/Change dialog
*
*       fMatchCase - static variable:default value of case matching
*       fWholeWord - static variable:default value of whole word searching
*       oSearchInSave - static variable:default value of search location
*                       if the window is not the help window.
*       fEZMenu    - static variable:use easy or full menus (!FK_KANJI only)
*       &b$Buf1    - default value to search for (may be empty).
*       &b$Buf2    - default value to change to (may be empty).
*
*Exit:
*       returns tmc value of dialog or FALSE for cancel or error.
*
*Uses:
*       Per C Convention.
*
*****/
STATICF(TMC) DoDlgSearch (pdlg, cbDlg, cabi, fSearchChange)
void far *pdlg;
WORD cbDlg, cabi;
BOOL fSearchChange;
{
    HCABSearchChange hcabSearch = (HCABSearchChange) HcabAlloc (cabi);
    TMC tmc = tmcCancel;        // [10]

    /* [2] Catch HcabAlloc errors */
    if (uierr)
        return (FALSE);

    (*hcabSearch)->fMatchCase = fMatchCase;
    (*hcabSearch)->fWholeWord = fWholeWord;

    SzToCab (hcabSearch, &b$Buf1, Iag (CABSearchChange, szFindWhat));   //[28][19]
    SzToCab (hcabSearch, fSearchChange ? &b$Buf2 : "",                  //[28]
             Iag (CABSearchChange, szChangeTo));
    // [10] if SzToCab memory allocation failed, free cab and exit.
    if (uierr)
        goto DoDlgSearchEnd;

DoSearchAgain:  /* [3] If the search string is empty, we should repeat */
    tmc = TmcDoDlgFar (pdlg, cbDlg, hcabSearch);

    if (tmc != tmcCancel) {
        scopeOfSearch = isInCurrentModule;              //[39]

        // Update default value for search scope if we are not in help window
        if (pwndAct != &wndHelp)                        //[36]
            oSearchInSave = scopeOfSearch;              //[36]

        fMatchCase = (*hcabSearch)->fMatchCase;
        fWholeWord = (*hcabSearch)->fWholeWord;
        SzFromCab (hcabSearch, &b$Buf1, CBMAX_FIND,                     //[28]
                   Iag (CABSearchChange, szFindWhat));
        if (!CbSzUi (&b$Buf1)) {                                        //[28]
            MsgBoxStd (MB_OK, MSG_NoSearchString);                      //[39]
            goto DoSearchAgain;         /* [3] */
        }

        if (fSearchChange) {
            SzFromCab (hcabSearch, &b$Buf2, CBMAX_CHANGE,               //[28]
                       Iag (CABSearchChange, szChangeTo));
        }
    }

DoDlgSearchEnd:         // [10]
    FreeCab (hcabSearch);                       /* [3] */
    return ((tmc == tmcCancel) ? FALSE : tmc);  /* [3] */
}


VOID near CmdSearchFind (fPrompt, fUseSelect, fLabelT)
boolean fPrompt;
boolean fUseSelect;
boolean fLabelT;
{
    TMC tmc;
    ushort cbSelection;
    boolean fGotSelect;
    boolean fNotFound;          //[21] was the item found?

    EditMgrFlush1 ();

    fLabel = fLabelT;

    if (fLabelT) {
        fWholeWord = TRUE;
        fMatchCase = FALSE;
        scopeOfSearch = isInCurrentModule;
    }

    DbChkHoldBuf1();            //[28]
    fGotSelect = ((cbSelection = GetSelText (&b$Buf1, CBMAX_FIND)) != 0);       //[28]
    if (!fGotSelect) {
        /* no text is selected, set default to what ever word cursor is on */
        cbSelection = GetEditWord (&b$Buf1, CBMAX_FIND);  //[28][7]
    }
    if (!((fUseSelect || bdlSaveFind.cbLogical == 0) && cbSelection)) {
        /* Don't use selected text, or word under cursor, as FIND pattern */
        MakeBufFromBdl (&b$Buf1, &bdlSaveFind);                 //[28]
    }
    if (fPrompt || b$Buf1 == '\0' || (fUseSelect && !fGotSelect) ||     //[35]
        (fLabelT && !fGotSelect)) {
        tmc = DoDlgSearch (&dlgSearchFind, sizeof (dlgSearchFind),
                           cabiCABSearchFind, FALSE);
    }
    else {
        tmc = tmcOk;
    }

    if (tmc == tmcOk) {

        MakeBdlFromBuf (&bdlSaveFind, &b$Buf1); //[28]

//
// a-emoryh - In Dos6 QHelp, search context strings first, then if not found,
//            search everything.
//
         if ((pwndAct == &wndHelp) && (cmdSwitches & CMD_SW_QHELP))
         {
            // Search context strings first.  Fast!!
            if (DisplayHlpWndSz(&b$Buf1) == 0)
               // Found string!  Exit now.
               goto CmdSearchFindEnd;
         }


         //
         // Didn't find string, so search everything now.  Slow!!
         //
         if (StartSearch() || uierr)             //[18] if any problems, exit
             goto CmdSearchFindEnd;

         fNotFound = !SearchNext (TRUE) && !uierr;               //[21]

                                                             //[21]
         // Reset the current help topic if it needs it.  This   //[21]
         // Must be done before the message box, as it will      //[21]
         // cause the screen to be redrawn with the wrong help   //[21]
         // help topic.                                          //[21]

         DbChkFreeBuf1();                                        //[28]
         if (pwndAct == &wndHelp) {                              //[21]
             if (fAbort)                                 //[21][27]
                 SendHelpMsg(WM_HELPRESTORETOPIC,0);             //[21]
             else if (ioRsSrchStart != ioRsSrchCur)//[7]         //[21]
                 SendHelpMsg(WM_HELPCHGTOPIC,ioRsSrchCur);       //[21]
         }                                                       //[21]

         if (fAbort)                               //[39]
             FlushMsgs();                          //[39] Get rid of ESCAPE
         else if (fNotFound) {                     //[21] don't put up dialog box
             MsgBoxStd (MB_OK, MSG_MatchNotFound); //[18] if error
         }
    }

   CmdSearchFindEnd:
    DbChkFreeBuf1();            //[28]
    CloseCurHelpFile();         //[15] close current help file if one opened.
    BdFree (&bdSearchAlpha);
}

/***
*fSrchLastAlpha - have we visited the last text table
*
*Purpose:
*       Indicates if i is the last text table of the sorted list of
*       text tables that we created.
*
*Entry:
*       i - index into bdSearchAlpha list (0 based)
*
*Exit:
*       TRUE - if i is at or beyond last text table
*       FALSE- otherwize
*
*****/
STATICF(boolean) fSrchLastAlpha (i)
ushort i;
{
    return (i >= (bdSearchAlpha.cbLogical >> 1));
}

/***
*SrchORsOfAlpha - get the ith entry from the sorted text table list.
*
*Purpose:
*       Returns the ith entry from the sorted text table list that
*       is assumed to already be created.  If fSrchLastAlpha(i) is
*       TRUE, then undefined results are returned.
*
*Entry:
*       i - index into bdSearchAlpha list (0 based)
*
*Exit:
*
*
*****/
STATICF(ushort) SrchORsOfAlpha (i)
ushort i;
{
    return (((ushort *) bdSearchAlpha.pb)[i]);
}

VOID near CmdSearchChange ()
{
    struct ef *pef = (struct ef *) pwndAct->pefExtra;   // [20]
    boolean fChange = FALSE;
    TMC tmc;
    CRC far *pcrc = &dlgConfirm.dlgHeader.crcDlg;
    HCABConfirm hcabConfirm;
    AY ay;

    fLabel = FALSE;                                             //[33]
    EditMgrFlush1 ();

    /* fReadOnlyBuf sets uierr if INCLUDEd lines are visible */
    DbAssert (pwndAct != NULL);
    if (fReadOnlyBuf(pef->hBuffer))
        return;

    DbChkHoldBuf1();                                            //[28]
    DbChkHoldBuf2();                                            //[28]
    if (!GetSelText(&b$Buf1, CBMAX_FIND))                       //[28]
        MakeBufFromBdl (&b$Buf1, &bdlSaveFind);                 //[28]
    MakeBufFromBdl (&b$Buf2, &bdlSaveChange);                   //[28]

    // [11] If Cancelled, exit.
    if (!(tmc = DoDlgSearch (&dlgSearchChange, sizeof (dlgSearchChange),
                             cabiCABSearchChange, TRUE)))
        goto CmdSearchChangeDone;
    MakeBdlFromBuf (&bdlSaveFind, &b$Buf1);                     //[28]
    MakeBdlFromBuf (&bdlSaveChange, &b$Buf2);                   //[28]

    if (tmc == tmcFindAndVerify) {
        if (StartSearch() || uierr) goto CmdSearchChangeDone;   //[18]
        if ( !SearchNext (TRUE) && !uierr) {
            if (fAbort)                                         //[39]
                FlushMsgs();                                    //[39]
            else                                                //[39]
                MsgBoxStd (MB_OK, MSG_MatchNotFound);
            goto CmdSearchChangeDone;
        }
        else {
            do {
                // [18] we must branch now if any errors, otherwise we
                // [18] will not dealloc the cab
                if (uierr)
                    goto CmdSearchChangeEnd;

                hcabConfirm = (HCABConfirm) HcabAlloc (cabiCABConfirm);
                /* [2] Catch HcabAlloc errors */
                if (uierr)
                    goto CmdSearchChangeEnd;

                ay = ayMac - (pcrc->dy + 1);
                pcrc->y = (GetUILine () < ay) ? ay : 2;

                DoDrawDebugScr();    // [8]
                tmc = TmcDoDlgFar (&dlgConfirm, sizeof (dlgConfirm), hcabConfirm);
                FreeCab (hcabConfirm);
                if (tmc == tmcCancel)
                    goto CmdSearchChangeEnd;
                if (tmc == tmcChange)
                    DoChange ();
                else if (tmc == tmcSkip)
                    obCur++;
                if (uierr) goto CmdSearchChangeEnd;
            } while (SearchNext (TRUE));

            if (fAbort)                 //[39]
                FlushMsgs();            //[39] get rid of ESCAPE, no msg box
            else if (!uierr)            //[18]
                MsgBoxStd (MB_OK, MSG_ChangeComplete);
        }
    }

    else if (tmc == tmcChangeAll) {
        if (StartSearch() || uierr) goto CmdSearchChangeDone;   //[18]
        SendMessage (pwndAct, WM_SETREDRAW, FALSE, 0L );        //[29]
        while (SearchNext (FALSE)) {
            fChange = TRUE;
            DoChange();
            if (uierr) break;
        }
        SendMessage (pwndAct, WM_SETREDRAW, TRUE, 0L);
        if (uierr) goto CmdSearchChangeEnd;
        if (fAbort)                                             //[39]
            FlushMsgs();                                        //[39]
        else                                                    //[39]
            MsgBoxStd (MB_OK, fChange ? MSG_ChangeComplete : MSG_MatchNotFound);
        if (!fChange) goto CmdSearchChangeDone;
    }

CmdSearchChangeEnd:
    if (obStart) {
        obStart--;
    }
    /* make initial oRs visible in active window */
    WndReset (SrchORsOfAlpha (ioRsSrchStart));
    MoveCursorPwndCur (olnStart, obStart);  /* restore initial cursor */

CmdSearchChangeDone:
    DbChkFreeBuf1();    //[28]
    DbChkFreeBuf2();    //[28]
    BdFree (&bdSearchAlpha);
}

STATICF(boolean) StartSearch ()
{
    struct ef *pef = (struct ef *) pwndAct->pefExtra;           // [20]
    ushort iLines;

    if (pwndAct == &wndHelp) {                                  //[7]
        ioRsSrchStart = ioRsSrchCur = (ushort) SendHelpMsg(WM_HELPCURTOPIC,0); //[17]
        if (ioRsSrchCur == UNDEFINED)           //[18]
            return (TRUE);                      //[18] indicate failure
        iLines = (ushort) SendHelpMsg(WM_HELPFILESIZE,0);       //[17]
    } else                                                      //[7]
    {
        if (!AlphaBuildORs ()) {
            SetUiErrOm ();
            return (TRUE);                      //[18] indicate failure
        }
        ioRsSrchStart = ioRsSrchCur = AlphaOfORsFar (pef->hBuffer);
        BdChgOwner (&bdAlphaRs, &bdSearchAlpha);
        iLines = LinesInBuf(SrchORsOfAlpha (ioRsSrchStart));    //[7]
    }
    olnCur = olnStart = GetEditLine();

    if (olnCur == iLines)                                       //[7]
        obCur = obStart = 0;
    else
        obCur = obStart = GetEditColumn () + 1;
    fWrapped = FALSE;

    if (!fMatchCase)
        lower (&b$Buf1);                        //[28]

    return (FALSE);                             //[18] return success
}

STATICF(boolean) FEndOfSearch ()
{
    fPollKeyboard = TRUE;
    PollKeyboard ();            // [16]
    return (fAbort ||           // [16][27]
            fWrapped && (ioRsSrchStart == ioRsSrchCur) &&
            (olnCur > olnStart || (olnStart == olnCur && obCur >= obStart)));
}

/***
*SrchReAssign - Reassign oRs during a search
*
*Purpose:
*       During a search and replace, we may end up changing oRss for text
*       tables (such as changing the names of a SUB or FUNCTION).  This
*       call back allows the search code to update its cached list of
*       oRss in this case.
*
*Entry:
*       oRsOld - value of oRs that is changing
*       oRsNew - new value for this register set.
*
*Exit:
*       None.
*****/
void near SrchReAssign(oRsOld, oRsNew)
ushort oRsOld, oRsNew;
{
    int i;

    for (i = 0; !fSrchLastAlpha (i); i++) {
        if (SrchORsOfAlpha (i) == oRsOld) {
            ((ushort *) bdSearchAlpha.pb)[i] = oRsNew;
        }
    }
}


STATICF(boolean) SearchNext(fSrchMsg)
boolean fSrchMsg;
{

    DbAssert ((scopeOfSearch == isInActiveWindow) ||
              (scopeOfSearch == isInCurrentModule));

    if (fSrchMsg) {
        StatusLineMsg (-MSG_Searching);
    }
    /* display intense 'Searching' msg on status line */
    while (TRUE) {
        if (FEndOfSearch())
            break;

        if (pwndAct == &wndHelp) {                              //[7]
            if (olnCur >= (ushort) SendHelpMsg(WM_HELPFILESIZE,0)) { //[17]
                fWrapped = TRUE;                                //[7]
                olnCur = 0;                                     //[7]
                if (scopeOfSearch != isInActiveWindow) {        //[7]
                    DbChkFreeBuf1();                            //[28]
                    ioRsSrchCur = (ushort) SendHelpMsg(WM_HELPNXTTOPIC,ioRsSrchCur); //[17]
                    //Check for errors                          //[7]
                    if (ioRsSrchCur == UNDEFINED)               //[7]
                        break;                                  //[7]
                                                                //[7]
                    DbChkHoldBuf1();                            //[28]

                    // Refresh the DGROUP copy of the search string,
                    // remapping it to lower case if needed.
                    MakeBufFromBdl (&b$Buf1, &bdlSaveFind);     //[28]
                    if (!fMatchCase)                            //[39]
                        lower (&b$Buf1);                        //[39]

                }                                               //[7]
            }                                                   //[7]
        } else                                                  //[7]
        if (olnCur >= LinesInBuf(SrchORsOfAlpha (ioRsSrchCur))) {
            fWrapped = TRUE;
            olnCur = 0;
            if (scopeOfSearch != isInActiveWindow) {
                if (fSrchLastAlpha (++ioRsSrchCur) ||
                    (SrchORsOfAlpha (ioRsSrchCur) & 0x8000) == 0) {
                while (SrchORsOfAlpha (--ioRsSrchCur) & 0x8000) ;
                UiRsActivate (SrchORsOfAlpha (ioRsSrchCur));
                }
            }
            if (FEndOfSearch ()) {
                break;
            }
        }

        if ((olnCur & 0xf) == 0)
            DrawTogglesLnCol (olnCur, obCur);
        if (SearchLine ())
            break;

        olnCur++;
        obCur = 0;
    } // end WHILE

    if (fSrchMsg) {
        DoStatusMsg (pwndAct);  //[5] restore normal status line message
    }
    /* tell DrawStatusLine it is ok to stop showing Searching msg */
    DrawToggles();

    /*
      Note: Call FEndOfSearch again 'cause even if we did find something,
            we could have gone too far.
    */
    return(!FEndOfSearch());
}

/****
* SearchLine - Scan a line for a particular string or label
*
*Entry:
*       ioRsSrchCur - oRs identifier of window/help topic to search
*       olnCur      - line in text table/help topic to search
*
*Exit:
*       TRUE - text was found
*       FALSE - text was not found
*
* NOTE: for a search of the help system, WM_HELPLINE will return 0 if
*       there is an error.  This routine ignores it, as there will be
*       no text found, and the next WM_HELPNEXTTOPIC will fail if
*       there is a real problem.
***********************************************************************/
STATICF(boolean) SearchLine ()
{
    ushort cb;
    ushort ob;

    if (pwndAct == &wndHelp)                    //[7]
        cb = (ushort) SendMessage(pwndAct, WM_HELPLINE, olnCur, MAKELONG(ldEMScratch.cbMax, ldEMScratch.prgch)); //[17]
    else                                        //[7]
        {
        cb = cbGetLineBuf (SrchORsOfAlpha (ioRsSrchCur), olnCur,
                           ldEMScratch.cbMax, ldEMScratch.prgch);
        }

    if (cb) {
        if (!fMatchCase)
            lower (ldEMScratch.prgch);

        if (fLabel)
            ob = obFindLabel (ldEMScratch.prgch, obCur, cb, &b$Buf1);   //[28]
        else
            ob = obFind (ldEMScratch.prgch, obCur, cb, &b$Buf1);        //[28]
        if (ob != UNDEFINED) {
            obCur = ob;

            /* make active text table visible in active list window */

            if (pwndAct != &wndHelp)            //[7]
                WndReset (SrchORsOfAlpha (ioRsSrchCur));
            if (!FEndOfSearch ()) {
                SelectTextPwndCur (olnCur, obCur, CbSzUi (&b$Buf1));    //[28]
            }
            return (TRUE);
        }
    }

    return (FALSE);
}

/**************************************************************************
*   obFind - check for matching string
*   Description:
*      This routine searchs the specified buffer for a match of the
*      specified pattern. It returns an offset from the start of the buffer
*      to the first character in the matched pattern. If the pattern is not
*      found it return the number of bytes originally specified as size of
*      the buffer.
*
*      pszPattern is assumed to be a non-null string.
*
*   Input:
*      pszTarget - pointer to start of buffer
*      ob - where to start search in pszTarget
*      cb - size of buffer
*      pszPattern - pointer to search pattern
*   Output:
*      return - offset from pszTarget to start of pattern
*               is cb if not found
**************************************************************************/
STATICF(ushort) obFind(pszTarget, ob, obMax, pszPattern)
char *pszTarget;
ushort ob;
ushort obMax;
char *pszPattern;
{
    register short cb;
    register char *psz;
    ushort cbPattern;
    char ch;

    ch = *pszPattern;
    psz = pszTarget + ob;
    cb = obMax - ob + 1;
    cbPattern = CbSzUi (pszPattern);
    while (cb > 0) {
        /* Look for match of first character in pattern */
        if (!(psz = memchr (psz, ch, cb)))
            /* starting character is not in buffer */
            return (UNDEFINED);

        /* compute remaining bytes */
        ob = psz - pszTarget;
        cb = obMax - ob + 1;

        /* if pattern can't fit into buffer return */
        if (cbPattern > cb)
            return(UNDEFINED);

        if (memcmp (psz, pszPattern, cbPattern) == 0)
            if (fWholeWord) {
                if (ob == 0 || !IsWordChar (*(psz-1)))
                    if (ob + cbPattern > obMax || !IsWordChar(*(psz+cbPattern)))
                        return (ob);
            }
            else
                return(ob);

        {                                               //[22]
            psz ++;                                     //[22]
            cb  --;                                     //[22]
        }                                               //[22]
    }
    return (UNDEFINED);
}

/**************************************************************************
*   obFindLabel - check for matching label
*
*   Description:
*      This routine searchs the specified buffer for a match of the
*      specified label. It returns an offset from the start of the buffer
*      to the first character in the matched label. If the label is not
*      found it returns -1
*
*      pszPattern is assumed to be a non-null string.
*
*   Input:
*      pszTarget - pointer to start of buffer
*      obStart   - where to start search in pszTarget
*      cbMax     - size of buffer
*      pszPattern - pointer to search pattern
*   Output:
*      return - offset from pszTarget to start of pattern
*               is cb if not found
**************************************************************************/
STATICF(ushort) obFindLabel (pszTarget, obStart, obMax, pszPattern)
register char *pszTarget;
ushort obStart;
ushort obMax;
char *pszPattern;
{
    register ushort ob;
    ushort cbPattern;
    register char *psz;

    cbPattern = CbSzUi( pszPattern );

    // Don't include ':' in the string to be searched for.  This allows
    // us to search for substrings of a label.

    if (pszPattern[cbPattern-1] == ':') {  //This is legal because : can not
        pszPattern[cbPattern-1] = 0;       //be part of a DBCS character
        cbPattern--;
    }

    ob = obFind(pszTarget, obStart, obMax, pszPattern);
    if (ob != UNDEFINED) {

        //We found the string.  Make sure that all characters on the
        //line up to the first ':' are legal characters for a label
        //If the line does not include a ':', then the 0 terminator
        //will cause the routine to exit, as it is not a legal Label
        //character.


        for (psz = pszTarget+ob; psz >= pszTarget; psz--)       //[26]
            if (!IsLabelChar(*psz))
                return(UNDEFINED);
        for (psz = pszTarget+ob+cbPattern; *psz != ':'; psz++)
            if (!IsLabelChar (*psz))
                return (UNDEFINED);
    }
    return (ob);

}

STATICF(void) DoChange ()
{
    ushort cbAdd, cbDel;

    cbAdd = CbSzUi (&b$Buf2);   //[28]
    cbDel = CbSzUi (&b$Buf1);   //[28]

    DbChkFreeBuf1();            //[28]
    DbChkFreeBuf2();            //[28]
    SendMessage (pwndAct, EM_REPLACESEL, (WORD) &b$Buf2, 0L );
    DbChkHoldBuf1();            //[28]
    DbChkHoldBuf2();            //[28]
    MakeBufFromBdl (&b$Buf1, &bdlSaveFind);     //[28]
    if (!fMatchCase)            //[28]
        lower (&b$Buf1);        //[28]
    MakeBufFromBdl (&b$Buf2, &bdlSaveChange);   //[28]
    if (ioRsSrchStart == ioRsSrchCur && olnCur == olnStart && obCur < obStart) {
        /*
           We have wrapped around to the line we started on, and we
           are adding/deleting characters before obStart. So, adjust
           obStart by the amount we add/delete.
        */
        if (obCur + cbDel > obStart) {
            /*
               we just deleted char at search-start position,
               so push obStart back to zero so we will terminate on the
               next iteration.
            */
            obStart = 0;
        }
        else
            obStart += cbAdd - cbDel;
        }
    obCur += cbAdd;
}

/**************************************************************************
* STATICF (void) MakeBdlFromBuf (pbdl, buf)
* Purpose:
*  Copies NEAR data in a buf to the FAR heap.
*
**************************************************************************/
STATICF (void) MakeBdlFromBuf (pbdl, buf)       //[28]
bdl *pbdl;
char *buf;
{
    ushort cb = CbSzUi (buf);

    if (pbdl->seg != NOT_OWNER)
        BdlFree (pbdl);

    if (cb && !BdlAlloc (pbdl, cb)) {
        cb = 0;                 //[30]
        SetUiErrOm ();
    }

    if (cb) {                   //[30]
        fmemcpy ((char far *) MAKELONG (0, GETSEG (pbdl->seg)),
                 (char far *) buf, cb);
    }
    else {
        pbdl->cbLogical = 0;    //[30]
    }
}

/**************************************************************************
* STATICF (void) MakeBufFromBdl( buf, pbdl )
* Purpose:
*  Copies FAR data in a bdl to NEAR data.
*
**************************************************************************/
STATICF (void) MakeBufFromBdl ( buf, pbdl )     //[28]
char *buf;
bdl *pbdl;
{
    ushort cb = pbdl->cbLogical;

    if (cb) {   //[29]
        DbAssert (pbdl->seg != NOT_OWNER);      //[29]
        fmemcpy ((char far *) buf,
                 (char far *) MAKELONG (0, GETSEG (pbdl->seg)), cb);
    }           //[29]
    buf[cb] = '\0';
}

AY FAR PASCAL
GetUILine ()
{
    struct ef *pef = (struct ef *) pwndAct->pefExtra;   // [20]

    return (((AY) (GetEditLine () - pef->pdCur.olntop)) +
            pwndAct->arcClipping.ayTop);
}


// Added back [39]
/*********************************************************************
* void NEAR SelectTextPwndCur(ln, col, cb)
* Purpose:
*  Selects a range of text on a single line in the current window.
*
*********************************************************************/

void NEAR SelectTextPwndCur(ln, col, cb)
ushort ln;
ushort col;
ushort cb;
{
    SendMessage (pwndAct, EM_SELCHARS, ln, MAKELONG(col, col+cb));
}
