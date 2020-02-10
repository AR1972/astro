/***
*uihelp.c - Help routines.
*
*       Copyright <C> 1985-1988, Microsoft Corporation
*
*Purpose:
*       Help management routines.
*
*******************************************************************************/

#include <version.h>
#include <cw/version.h>
#include <cw/windows.h>
#include <cw/edityp.h>
#include <cw/help.h>
#include <uiext.h>

/* And the dialog information */
#include <cw/dlg.h>

#include "uihelp.hs"
#include "uihelp.sdm"

#ifndef PARSER_H
#include <parser.h>
#endif

#ifndef HEAP_H
#include <heap.h>
#endif

#ifndef UIHELP_H
#endif

#ifndef PRSTAB_H
#include <prstab.h>
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

#ifndef RTPS_H                          /* [8] */
#include <rtps.h>                       /* [8] */
#endif                                  /* [8] */

#ifndef UINHELP_H                       /* [13] */
#include <uinhelp.h>                    /* [13] */
#endif                                  /* [13] */

ushort  iHelpId;
BYTE    fHelpAlloc;
BYTE    fNoScroll;                      //[41]
bdl     bdlHelp = {0, UNDEFINED, 0, 0};

uchar   NEAR toupper(uchar);            //internationalized case conv,
ushort  FAR HelpGetLine (ushort, ushort, char FAR *, char FAR *);

VOID    NEAR DisplayHlpDlgBdl (VOID);           /* [10] */
VOID    NEAR ResizeHelpDialog (short);          /* [10] */
VOID    NEAR DisplayHelpOOM (VOID);
VOID    NEAR CmdHelpAbout(VOID);

extern BYTE fNoHighlight;
char   *szDialogHelpContext;            //[27] current dialog box help szContext
extern char *szDialogTitle;             //[27]
extern WORD cbDialogTitle;              //[27]
extern BYTE HelpFlags;                  //[47]
extern BYTE HelpFlags2;                 //[47]
extern bool fHelpVisible;                       //[26]
extern WND  WndHelp;                            //[26]
extern WORD iFileSize;                  //[46]
extern BYTE fDirChanged;                //[50] TRUE if b$Buf1 contains the
                                        //[50] directory before dialog started

// a-emoryh - Dos6 QHelp
extern BOOL  fQhelpStarting;     // Hack flag for /QHELP; used to prevent edit
                                 // from being draw before Help comes up.


VOID    FAR PASCAL Help(WORD, WORD, VOID *, WORD);
extern char *(near SzGetIth(char *, ushort));
extern ushort FAR IRWofPbCb(char *, ushort);



/* massively rewritten with [13] */
/**************************************************************************
* VOID FAR PASCAL Help(WORD hem, WORD hid, VOID *pv, WORD kk)
* Purpose:
*  Help system entry point for COW.   COW will call this routine
*  whenever F1 (or SHIFT F1 or CTRL F1 or ...) is pressed unless it
*  is in an edit window (in which case we call it ourselved).
*
* Entry:
*  iHelpId : message number of message displayed if MENUITEM or MESSAGEBOX
*
*  hem     : main help item, either hemUserMin, hemMbox, hemMenuItem,
*            hemMenu or hemDialog.
*  hid     : help identifier for a dialog if hem = hemDialog
*  pv      : pointer to menu structur if hem = hemMenu
*  kk      : shift state
*
**************************************************************************/

VOID FAR PASCAL Help(hem, hid, pv, kk)
WORD    hem;
WORD    hid;
VOID    *pv;
register WORD   kk;
{
register char    *sz;                   //[48]
    ushort  iRetVal;
// a-emoryh
    BOOL    fCmdLineTopicFound;

    /* if we are in help system, or help system will not start, then return */

    if (HelpFlags & HLP_INHELP) //[30] make sure we aren't called recursively
        return;                                                                 /* Disallow recursive calls */

    fHelpAlloc++;               //[49] prevent allocs from shutting down help

    if (StartHelp())            /* if we can't start up, then exit */
        goto HelpDisplayed;

    switch (hem) {
        case hemAllWindow:

        //[29] parse F1 & various shift states
        //[29]   F1             = Keyword help
        //[29]   Shift+F1       = Default help
        //[29]   Ctrl+F1        = Help Next (next sequential help topic)
        //[29]   Shift+Ctrl+F1  = Help Prev (previous sequential help topic)
        //[29]   Alt+F1         = Help Back (last help displayed)
        //[29]                    NOTE: ALT is handled as an accelerator key

            if ((kk & KK_CONTROL) && (kk & KK_SHIFT))   //[29]
                CmdHelpPrev();                  //[29] previous sequential topic
            else                                //[29]
            if (kk & KK_CONTROL)                //[29]
                CmdHelpNext();                  //[29] next sequential topic
            else                                //[29]
            if (kk & KK_SHIFT) {                //[25] SHIFT-F1 for default help
                if (cmdSwitches & CMD_SW_QHELP)
                {
// a-emoryh - In Dos6 QHelp topics can be specified from commandline.
                    fCmdLineTopicFound = TRUE;
                    if (fQhelpStarting && (szCmdLineTopic[0] != 0))
                    {
                       if ((iRetVal = DisplayHlpWndSz(szCmdLineTopic)) == 0)
                       {
                          goto DisplayCmdLineTopic;
                       }
                       else
                       {
                          // Topic not found, so honk and goto normal contents
                          hid = helpQHContentsId;
                          fCmdLineTopicFound = FALSE;
                       }
                    }
                    else
                       hid = helpQHContentsId;
                }

                else if (cmdSwitches & CMD_SW_ED)  //[53]
                    hid = helpEditorHelp;       //[56]
                else                            //[53]
                    hid = helpOnHelpId;         //[25] default help
                goto DisplayHelphid;            //[25] display it in help window
            }                                   //[25]
            else {                              //[23] F1 for keyword help

// a-emoryh Fixed so F1 key always brings up HelpOnHelp (in QHelp mode)
//              if ((cmdSwitches & CMD_SW_QHELP) && pwndAct != &wndHelp) {

                if ((cmdSwitches & CMD_SW_QHELP) && pwndAct == &wndHelp) {
                    hid = helpHowToUseId;
                    goto DisplayHelphid;
                }                               //[53]
                if ((cmdSwitches & CMD_SW_ED) && pwndAct != &wndHelp) { //[55]
                    hid = helpSurvivalId;       //[53]
                    goto DisplayHelphid;        //[53]
                }                               //[53]

                //[30] if we are in the help window, then have F1 check
                //[30] for hot links as well as context sensitive help

                if (pwndAct != &wndHelp ||      //[31]
                        SelectHotLink(GetEditColumn(),GetEditLine(),FALSE))
                KeywordHelp();                  //[23]
            }
            goto HelpDisplayed;                 //[57]

        case hemWindowHid:      /* help menu items */
DisplayHelphid:                                 //[25] display help given hid
            iRetVal = DisplayHlpWndSz(CreateContext(hid));      //[40]

DisplayCmdLineTopic:                   // a-emoryh
            DbAssert (iRetVal != HELP_NF);                      //[42]

            //[26] we must set the focus into the help window here
            //[26] if displaying Survival, Index, or Table of Contents help
            if (iRetVal == HELP_OK && hid >= helpEditorHelp  //[56]
                                   && hid <= helpOnHelpId)   //[42]
                if (fHelpVisible)               //[26]
                    WndActivate(&WndHelp);      //[26]

// a-emoryh - If couldn't find topic requested on command-line, then put up
//    error dialog.
            if (fQhelpStarting && !fCmdLineTopicFound)
               MsgBoxStd (MB_OK, MSG_MatchNotFound);

            goto HelpDisplayed;                 //[26]

        case hemMenu:
            sz = "m.x";
            sz[2] = ((PMENU)pv)->pchTitle[((PMENU)pv)->ichHilite];
            break;

        case hemDialog:
            if (hid != hidDlgNull) {
                iHelpId = hid;                  //[24] Use HelpID from dlg box
            } /* otherwise use context number set up before Dialog was called */

        case hemMenuItem:
        case hemMbox:
            sz = CreateContext(iHelpId);        //[25]
            break;

        default:
            DbAssert(FALSE);
            break;
    }

    /* at this point, any help messages will go into a dialog box */

    if (fDirChanged) {                  //[50] if drive/dir modified from normal
        //[50] The only time this flag is set is for the duration of
        //[50] DoDlgLoadFile and DoDlgSaveFile.  b$Buf1 contains the current
        //[50] drive/directory before the dialog, which we must restore here
        //[50] before we attempt to do any Help file I/O.
        //[50] b$Buf2 is the return value from ValidateFile() (called by the
        //[50] Load/Save dialog procs), and is available for our use here.
        GetCurDriveDir(&b$Buf2);        //[50]   save current one in b$Buf2
        SetCurDir2(&b$Buf1);            //[50]   reset to original (in b$Buf1)
    }
    DbAssert(sz[0] != 0);               //[51] string must not be 0 length
    if (GetHelpMsgSz(sz,&bdlHelp) == HELP_OK) {                 //[42]

        szDialogHelpContext = sz;       //[27] save dialog box help *szContext
                                        //[27] for use as a title
        CloseCurHelpFile();             //[48] don't keep help file open during
                                        //[48] user input.  Probably not
                                        //[48] required here -- just being safe
        DisplayHlpDlgBdl();             // display the dialog box help
    }
    if (fDirChanged)                    //[50] if directory restored
        SetCurDir2(&b$Buf2);            //[50]   reset to current

HelpDisplayed:
    CloseCurHelpFile();                 //[48] don't keep help file open
    if (uierr == MSG_HelpOOM)
        DisplayHelpOOM();

    fHelpAlloc--;                       //[49] restore flag
}


#define cbMaxHlp 62             //[21] max text size = 60, plus some slop

/**************************************************************************
* VOID NEAR WListProcHelpText()
* Purpose:
*    Handles the scrollable help list box within the help dialog box.
*
*    Gets successive lines of help text.
*
* Exit:
*    Size of next line of help text for this context, or 0 if no more.
*
**************************************************************************/
WORD FAR WListProcHelpText (tmm, sz, isz, tmc, wParam, bArg)
WORD tmm;
char *sz;
WORD isz, tmc, wParam, bArg;
{

    Unreferenced (tmc);
    Unreferenced (wParam);
    Unreferenced (bArg);

    switch (tmm) {
        case tmmCount:          /* return desired size of listbox */
            return ((WORD)-1);          /* unknown size */

        case tmmText:
        case tmmEditText:
            // We are assured that the help system is running because
            // all allocations from the begining of the DisplayHlpDlgBdl
            // to the end are protected by a fHelpAlloc flag.

            DbAssert(HelpFlags & HLP_GOTBUF);  //[37]
            /* Call help engine to get text.  Returns 0 when all done. */
            return (HelpGetLine(isz+1, cbMaxHlp, sz,(char far *)MAKELONG(0,&(bdlHelp.seg))));
    }
}

/*
        These constants are pseudo-HACKS to allow dynamic resizing of help
        dialog boxes.  They are offsets into the dlgHelpText structure in
        UIHELP.SDM (created by DE.EXE from UIHELP.DES).  If UIHELP.DES changes
        radically, these constants MAY have to be changed.  Simple changes
        (like re-sizing the boxes) should not have an effect.   Adding boxes
        or buttons might cause rgsdmtm[2] to be something other than the
        OK button.
*/

#define DIALOGBOXPOS       dlgHelpText.dlgHeader.crcDlg.y
#define DIALOGBOXSIZE      dlgHelpText.dlgHeader.crcDlg.dy
#define DIALOGLISTBOXSIZE  (((uchar FAR *)&(dlgHelpText.rgsdmtm[0].l))[3])
#define DIALOGOKBUTTONPOS  (((uchar FAR *)&(dlgHelpText.rgsdmtm[2].l))[1])
#define LISTBOXBORDER      (int) 2      //[17] top line & scroll bar of list box

#define MAX_HELP_TITLE  56      //[27] biggest title allowed (including NULL)

/**************************************************************************
* VOID NEAR DisplayHlpDlgBdl ()
*
*   Added with revision [10].
*
* Purpose:
*   Brings up a scrollable help dialog box of minimum size.  The
*   help text is found in bdlHelp.
*
* Entry:
*       None.
*
**************************************************************************/
VOID NEAR DisplayHlpDlgBdl ()
{
    HCABHelpText hcabHelpText;
    char        *szDialogTitleSav;              //[27]
    WORD        cbDialogTitleSav;               //[27]
    char        TitleBuf[MAX_HELP_TITLE+1];     //[27]
    ushort      OldUiErr = uierr;               //[38]
register ushort OldFileSize;                    //[46]
register  int   ShrinkSize;                     //[46]
    PWND        pwndFocus;                      //[52]


    //[33] We must make sure that there is not already a recursion flag
    //[33] set, otherwise we will not be able to safely clear it at the
    //[33] end of this routine

    DbAssert(fHelpAlloc != 0);                  //[49] protect the Allocs

    uierr = 0;
    hcabHelpText = (HCABHelpText) HcabAlloc (cabiCABHelpText);
    if (uierr) {         /* Catch HcabAlloc failure */
        DisplayHelpOOM();
        goto DisplayHelpExit;                   //[37]
    }

    fNoHighlight++;                     /* don't highlight selected items */

    DbAssert(!(HelpFlags & HLP_INHELP));        //[58]
    HelpFlags |= HLP_INHELP;                    //[58] Add recursion lock

    OldFileSize = iFileSize;                    //[46]
    iFileSize = 0;                              //[46] must zero before
                                                //[46] GetHelpContextLen
    ShrinkSize = (int) DIALOGLISTBOXSIZE        /* current # lines in box */
                - (int) GetHelpContextLen(&bdlHelp) //[46] minus # for text
                - LISTBOXBORDER;                /* minus # needed for border */

    if (ShrinkSize < 0)                         //[41]
        ShrinkSize = 0;                         //[41] dialog box won't shrink
    else                                        //[46] less than full-size box
        if (iFileSize)                          //[46] ":l" NOT specified
            fNoScroll++;                        //[46] dialog box can't scroll
    iFileSize = OldFileSize;            //[46] restore original value
    ResizeHelpDialog (ShrinkSize);      /* trim initial box by ShrinkSize */

    szDialogTitleSav = szDialogTitle;   //[27] save old dialog box title info
    cbDialogTitleSav = cbDialogTitle;   //[27]

    GetHelpTitleStr(&bdlHelp,MAX_HELP_TITLE);   //[27] get title into bufStdMsg
    strcpy (TitleBuf, bufStdMsg);       //[27] can't keep title in bufStdMsg

    szDialogTitle = TitleBuf;           //[27] new dialog box title
    cbDialogTitle = 0;                  //[27] don't truncate title

    TmcDoDlgFar (&dlgHelpText, sizeof (dlgHelpText), hcabHelpText);

    szDialogTitle = szDialogTitleSav;   //[27] restore old dialog box title
    cbDialogTitle = cbDialogTitleSav;

    fNoHighlight--;                     /* highlighting OK again */
    pwndFocus = GetFocus();             //[52]
    if (pwndFocus != pwndAct)           //[52] If focus is a dialog window
        DrawWindow (pwndFocus);         //[52]  redraw, so listbox sel is right
    fNoScroll = 0;                      //[41] always want scroll bars
    ResizeHelpDialog(-ShrinkSize);      /* reset initial box size */

    FreeCab (hcabHelpText);

DisplayHelpExit:

    BdlFree(&bdlHelp);                  /*[58] delete the help text */

    uierr = OldUiErr;                   //[38] restore any error values

    HelpFlags &= ~HLP_INHELP;           //[35] remove recursion lock

}

/**************************************************************************
* VOID NEAR ResizeHelpDialog ()
*
*   Added with revision [10].
*
* Purpose:
*   Adjusts the initial scrollable help dialog box by a given amount.
*   Used to get the dialog box down to minimum size.
*
* Input:
*   ShrinkSize = # lines to trim dialog box by (negative if growing)
*
**************************************************************************/
VOID NEAR ResizeHelpDialog(ShrinkSize)
short   ShrinkSize;
{
    DIALOGBOXSIZE -= (BYTE) ShrinkSize;         /* shrink dialog box */
    DIALOGOKBUTTONPOS -= (BYTE) ShrinkSize;     /* move OK button up */
    DIALOGLISTBOXSIZE -= (BYTE) ShrinkSize;     /* shrink list box */
    DIALOGBOXPOS += (BYTE) (ShrinkSize/2);      /* re-center dialog box */
}

/**************************************************************************
* VOID NEAR CmdHelpAbout ()
*
*   Added with revision [54].
*
* Purpose:
*   Implements the Help.About... menu item.  This routine will pull up
*   a message box giving some very basic information about the program.
*
* Input:
*   None.
*
**************************************************************************/
VOID NEAR CmdHelpAbout()
{
    char szFirstLine[CB_bufStdMsg];
    char szSecondLine[CB_bufStdMsg];
    char szThirdLine[CB_bufStdMsg];

    if (cmdSwitches & CMD_SW_QHELP)
        ListStdMsg(MSG_HelpAboutQHelp);
    else if (cmdSwitches & CMD_SW_ED)
        ListStdMsg(MSG_HelpAboutEditor);
    else
        ListStdMsg(MSG_HelpAboutInterp);
    strcpy(szFirstLine, bufStdMsg);
    ListStdMsg(MSG_HelpAbout2);
    strcpy(szSecondLine, bufStdMsg);
    ListStdMsg(MSG_HelpAbout3);
    strcpy(szThirdLine, bufStdMsg);

    // We use the HLP_INHELP recursion flag to prevent anyone from
    // trying to get help on this message box.

    DbAssert(!(HelpFlags & HLP_INHELP));
    HelpFlags |= HLP_INHELP;

    UIMessageBox(szFirstLine, szSecondLine, szThirdLine, MB_OK | MB_NOHELP);

    HelpFlags &= ~HLP_INHELP;

}
