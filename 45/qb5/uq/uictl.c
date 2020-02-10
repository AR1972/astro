/***
*uictl.c - User interface management functions
*
*       Copyright <C> 1985-1988 Microsoft Corporation
*
*Purpose:
*       menu and main window management functions.
*
*******************************************************************************/

/* First, include version definition header */
#include <version.h>

/* Next, include TextWin's interface headers */
#include <cw/version.h>
#include <cw/windows.h>
#include <cw/edityp.h>
#include <uiext.h>

/* Next, include user-interface header */

#ifndef CONTEXT_H
#include <context.h>
#endif

#ifndef QBIMSGS_H
#include <qbimsgs.h>
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

#ifndef UINHELP_H
#include <uinhelp.h>
#endif
#include <uihelpid.h>         // definition of help identifiers


#ifndef IBMBASIC
// [QH1] - Binding Escape key to it in Dos6 QHelp
#endif
extern void HelpBack (void);


STATICF(void) ViewOutScrn(void);
STATICF(void) EnsFileRs(void);
void NEAR DoMenu(ushort, ulong);
void NEAR Quit(void);
void NEAR SetDialogTitle(PMENUITEM);
extern void MenuEnable(void);
void NEAR CmdHelpAbout(void);

extern void FAR PASCAL EndMouse(void);
extern bool FAR PASCAL FInitMouse(void);

extern bool   fHelpVisible;
extern bool   fExiting;
extern BYTE   HelpFlags;
extern ushort __acmdseg;
extern ushort __acmdln;
extern ushort __acmdln_orig;

extern unsigned char b_OrgLen;
/* Set by runtime to number of lines visible when QB first came up.
 * Possible values include 25, 43, 60 (others will be added in future).
 * This may be different than the number of lines per screen the
 * first time screen_setup is called, because the user program may
 * execute a SCREEN 1 statement before the user interface code
 * is ever invoked.
 */
extern BYTE fRecord, fPlayBack;
extern boolean fOptionsChanged;

extern WND wnd1, wnd2;     //  needed for WM_RBUTTONDOWN

extern BYTE fBreakOnErr;
boolean fGotCmd;
boolean fAccelCmd;
boolean fGotCmd;
/* TRUE when GetCmd() should return back to UserInterface to execute
   pcode in direct-mode statement buffer */

ushort uierr = 0;
/* 0 = no error needs to be reported by GetCmd,
   UNDEFINED = report error as indicated in txt mgr's txtErr structure,
   else = standard qbi error message code (usually out-of-memory)
   Reset by ReportError()
*/

extern MENUITEM rgMenuItemDebug[];
extern MENUITEM rgMenuItemSearch[];
extern MENUITEM rgMenuItemView[];
extern ushort iHelpId;
extern VOID FAR Help(WORD, WORD, VOID *, WORD);
extern VOID NEAR DoStatusButton(WORD);
char  *szDialogTitle;         //dialog box caption/menu name, or
                              //NULL if we're not to use our own
                              //title
WORD  cbDialogTitle;       //# bytes of szDialogTitle to
                           //display, or 0 if we're not to
                           //truncate it.

char  LQBFiller[17100]; // This reduces the amount of available
                        // DGROUP (FRE ("")) to about 32K for LQB.

BOOL  fQhelpStarting;   // Hack flag for /QHELP; used to prevent edit
                        // from being draw before Help comes up.

/***
*void NEAR GetCmd ()
*Purpose:
*       This is called to interact with the user, allowing him to edit text,
*       select menu options, etc., until some action needs to have pcode
*       executed, at which time, the function returns.  This routine is
*       an event driven state machine.
*
*       If a syntax or runtime error needs to be reported to the user, the
*       global static variable 'uierr' contains an offset into the standard
*       BASIC message table.  If it is UNDEFINED, the error information is
*       contained in the text manager's global structure 'txtErr'.  grs.oRsCur
*       identifies the current text table to be edited.
*
*Entry:
*       None.
*
*Exit:
*       grs.oRsCur may be changed as az result of editing.
*       grs.bdpDirect contains the ASCII source to be executed.
*
*Exceptions:
*       None.
*******************************************************************************/
void NEAR GetCmd()
{
    MSG msgCur;
    extern bool fRightMouseHelp;
    static bool fGreetingBox = TRUE;
    WORD idMsg = 0;
    ClrNonStickyBp ();
    DoStatusMsg(pwndAct);  // draw right status message

    /* This is the main message pump */
    fExiting = (bool)(fGotCmd = FALSE);
    while (!fGotCmd) {
        /* Must call DoDrawDebugScr before AND after ReportError, because
         * ReportError may call DrawDebugScr.
         */
        fAccelCmd = FALSE;

        // Do not paint main window!
        if (fGreetingBox && (cmdSwitches & CMD_SW_QHELP)) {
            fQhelpStarting = TRUE;  // Hack to prevent window drawing
            goto SkipDebugScr;
        }

        DoDrawDebugScr ();
        if (uierr != 0) {
            ReportError ();
            DoDrawDebugScr ();
        }
SkipDebugScr:

// if fGreetingBox not cleared yet, we have easy menus, and the
// we didn't start up with a file, so the current mrs is <UNTITLED>,
// do the greeting box dialog.

        if (fGreetingBox) {
            fGreetingBox = FALSE;

            if (cmdSwitches & CMD_SW_QHELP) { // Go Straight to Help!
                PostMessage (&wndMain, WM_COMMAND, midHelpHelp, 0L);
                goto SkipSetFocus;  // Keep edit window from being drawn
            }

            else {
                UiRsActivateWnd ();      // Make sure proper rs activated
                if (mrsCur.ogNam == OGNAM_UNNAMED) {
                    DoDlgGreetingBox ();
                    CmdFileNew();
                    DoDrawDebugScr();
                    if (fHelpVisible)
                        WndActivate(&wndHelp);
                }
            }
        }

        /* [42] make sure Focus is in pwndAct */
        DbAssert (pwndAct != NULL);
        if (pwndAct->fEnabled)
            SetFocus (pwndAct);
        else
            SetFocus(&wndMain);
SkipSetFocus:

        DbAssert (curHelpFile == 0);  //shouldn't have a help files open

        while (!PeekMessage (&msgCur)) {
            fPollKeyboard = TRUE;
            if (fGotCmd)
                goto ExitCmd;
            else if (fAccelCmd)
                goto ContinueCmd;
        }
        if (fGotCmd)
            goto ExitCmd;

#ifndef IBMBASIC
// [QH1] - Dos6 QHelp - Single click now activates hot-link button
#endif
        if ((cmdSwitches & CMD_SW_QHELP)     &&
            (msgCur.message == WM_LBUTTONUP) &&
            (msgCur.pwnd == &wndHelp))
            PostMessage (&wndHelp, WM_LBUTTONDBLCLK, msgCur.wParam, msgCur.lParam);

#ifndef IBMBASIC
// [QH1]
//    Disable right-mouse-button if in QHelp mode.  Left-mouse now
//    selects hotlinks, as per CUA.  Since F1 always brings up HelpOnHelp
//    now, right-mouse doesn't have a function anymore in QHelp.
//
#endif
        if (!(cmdSwitches & CMD_SW_QHELP))
        {
            if (msgCur.message == WM_RBUTTONDOWN
              || msgCur.message == WM_RBUTTONDBLCLK) {
                /*
                    All right button actions are equivalent
                    to an unshifted left mouse click followed
                    by an additional action.
                */
                msgCur.message += WM_LBUTTONDOWN - WM_RBUTTONDOWN;
                msgCur.wParam &= !MK_SHIFT;
            }
            else if (msgCur.message == WM_RBUTTONUP) {
                msgCur.message = WM_LBUTTONUP;

   //  Only do the translation if we are in a non-immediate edit window
   //  Allow right mouse help in help window
                if (msgCur.pwnd == &wnd1 || msgCur.pwnd == &wnd2
                  || msgCur.pwnd == &wndHelp || msgCur.pwnd == &wndCmd) {
                    EditMgrFlush();
                    uierr = 0;
                    if (fRightMouseHelp)
                        idMsg = midHelpSyntax;
                    else
                        if (msgCur.pwnd == &wnd1 || msgCur.pwnd == &wnd2)
                            idMsg = midGoUntilCursor;
                    if (idMsg)
                        PostMessage (&wndMain, WM_COMMAND, idMsg, 0L);
                }
            }
        }

    /* Move FilterSpecialMessages into Accelerator table in uirsrcc.c
     * leaving only carriage returns to handle.
     * Check for the F1 key.  If it has been hit, call the help system
     */

        if (msgCur.message == WM_CHAR) {
            if (msgCur.wParam == VK_HELP_KEY) {
                EditMgrFlush();
                uierr = 0;
                Help(hemAllWindow, 0, 0, HIWORD(msgCur.lParam));
                goto ContinueCmd;
            }
            if (msgCur.wParam == '\x0d' && CmdEnter())
                goto ContinueCmd;
// Check for ESC here, because accelerator catches it too often.
            if (msgCur.wParam == (VK_ESCAPE - VK_MIN) && fHelpVisible) {

#ifndef IBMBASIC
//
// [QH1] - Have the escape key go back to last topic
//
// (Old code):
#if 0
                // Esc does not cancel help for QHELP, does for EDIT/QBASIC
                if (!(cmdSwitches & CMD_SW_QHELP))
                {
                    CmdHelpClose ();
                }
#endif
#endif
                if (cmdSwitches & CMD_SW_QHELP)
                    HelpBack();
                else
                    CmdHelpClose ();

                goto ContinueCmd;
            }
        }
        DispatchMessage (&msgCur);

ContinueCmd:;
    } /* while */

ExitCmd:
/* make sure next SetFocus (pwndAct) reload ldCur, as DoCmd uses this buffer */
    if (!fExiting)
        SetFocus (&wndMain);
} /* GetCmd */

/***
*void NEAR WaitForEvent ()
*Purpose:
*       Wait until a CW event occurs.  Legal events are non-ALT shifted
*       characters, and left mouse clicks.
*
*Entry:
*       None.
*
*Exit:
*       None.
*
*Exceptions:
*       None.
*******************************************************************************/
void NEAR
WaitForEvent ()
{
    MSG msgCur;

    while (TRUE) {
        if (PeekMessage (&msgCur)) {
            if ((msgCur.message == WM_CHAR &&
              !(HIWORD(msgCur.lParam) & KK_ALT)) ||
              msgCur.message == WM_LBUTTONDOWN ||
              msgCur.message == WM_LBUTTONDBLCLK)
                break;
        }
    }
}

/***
*void NEAR FlushMsgs ()
*Purpose:
*  Flush all message that are in the message queue if we aren't
*  doing record or playback.
*
*Entry:
*  None.
*
*Exit:
*  None.
*
*Exceptions:
*  None.
*******************************************************************************/
void NEAR
FlushMsgs ()
{
    MSG msgCur;
    BOOL  fMenuState;

    if (fRecord || fPlayBack)
        return;

    fMenuState = FEnableMenuBar (FALSE);    // disable menu bar so
                                            // accelerators won't work
                                            // during PeekMessage
    while (PeekMessage (&msgCur)) ;
    FEnableMenuBar (fMenuState);            // restore original state
}

/***
*STATICF(void) ViewOutScrn ()
*Purpose:
*       Make the output screen visible until the user clicks the mouse or
*       presses a key.
*
*Entry:
*       None.
*
*Exit:
*       None.
*
*Exceptions:
*       None.
*******************************************************************************/
STATICF(void)
ViewOutScrn ()
{
    FEnableMouse(FALSE);
    FEnableMenuBar (FALSE);
    FlushMsgs ();

    RestoreUserScreen ();
    fDebugScr = FALSE;
    WaitForEvent ();
    SaveUserScreen ();
    fDebugScr = TRUE;

    FlushMsgs ();
    DrawDebugScr ();
    /*
     * Changing screen modes effects the mouse.
     * Turn it off, and re-initialise.
     */
    EndMouse();
    FInitMouse();
    FEnableMouse(TRUE);
}

/***
*STATICF(void) EnsFileRs ()
*Purpose:
*       If active window is the command window, activate a register set
*       for the top list window.
*
*       This is called when File/Save is selected.
*
*Entry:
*       pwndTop points to the top window.
*
*Exit:
*       None.
*
*Exceptions:
*       None.
*******************************************************************************/
STATICF(void)
EnsFileRs ()
{
    struct ef *pef;

    if (pwndAct == &wndCmd || pwndAct == &wndHelp) {
        if (pwndFull) {
            WndNoFullScreen ();
            DoDrawDebugScr ();
        }
        DbAssert(pwndTop != &wndHelp);
        DbAssert(pwndTop != NULL);
        DbAssert(pwndTop != &wndCmd);
        pef = (struct ef *) pwndTop->pefExtra;
        UiRsActivate (pef->hBuffer);
    }
}

/* Make MenuBarMain, rgMenuMain accessible */
extern MENUBAR MenuBarMain;
extern MENU rgMenuMain[];

/***
*void NEAR DoMenu (menuId, lParam)
*Purpose:
*       A menu item has been selected, take appropriate action.
*
*Entry:
*       menuId      ID of menu to perform action of.
*       lParam      lParam of WM_COMMAND message from menu selection.
*
*Exit:
*       None.
*
*Exceptions:
*       None.
*******************************************************************************/
void NEAR DoMenu (menuId, lParam)
REG1 ushort menuId;
ulong lParam;
{
    static ushort editMsg[] = { WM_UNDO, WM_CUT, WM_COPY, WM_CLEAR, WM_PASTE };


    if (menuId == EN_CURSORMOVED) {
        DrawStatusLine();  // update status line with new cursor pos
        return;     /* all we want do do is update status message */
    }
    if ((menuId == midRunStart) ||
      (menuId == midRunRestart) ||
      (menuId == midFileNew) ||
      (menuId == midFileCreate) ||
      (menuId == midFileOpen) ||
      (menuId == midFileLoad) ||
      (menuId == midFileExit)) {
        /* so we won't put up a pointless AskCantCont dialog box.
         * The reason FILE/OPEN, FILE/LOAD and FILE/CREATE are on the
         * list is because New Module or Include cause variable table allocation,
         * and thus, prevent continuing.
         */
        CantCont();
    }

    if (menuId == midSearchChange) {
        if (!AskCantCONT()) {
            /* changes may prevent CONTinue, ask while we can still back out.
             * We do it here rather than in CmdSearchChange() because AskCantCONT
             * can cause the output screen to be shown (because of RunInit())
             */
            return;   /* user wants to abort action */
        }
    }

    if ((menuId < miEditBase || menuId > midEditPaste) &&
      menuId != midEditCut2 && menuId != midEditClear2) {
        /* Update editor's dirty line (if any).
         * Since SaveFile(), and other functions, uses same buffer
         * that editor uses, we must force edit mgr to purge its cache */
        switch(menuId) {
            case midFileSave:
            case midFileSaveAs:
            case midFileNew:
            case midViewSubs:
                /* These menu functions alter ps.bdpSrc, which edit mgr
                 * also uses to keep its current line in.  EditMgrFlush1
                 * tells edit mgr to forget that ps.bdpSrc contains the
                 * current cached line
                 *
                 * midViewSubs does not modify ps.bdpSrc, but it can change
                 * the text tables (by deleting them) and the edit manager
                 * will think the non-dirty line it has is still valid.
                 */
                EditMgrFlush1 ();
                break;

            default:
                /* All other menu functions need edit mgr to flush its current
                 * cached line (in ps.bdpSrc) to the current text table (i.e.
                 * call ReplaceLineBuf if the current line is modified).
                 * EditMgrFlush does this. */
                EditMgrFlush ();
                break;

        }
        uierr = 0;
    }

    // Avoid drawing edit window if /QHELP is starting
    if (!fQhelpStarting)
        DoDrawDebugScr ();

    /* calling EditMgrFlush, AskCantCONT or CantCont can cause the
     * output screen to be shown (because of RunInit()), in which case,
     * we need to re-draw the list windows now.
     */

    /* load mrsCur, prsCur, txdCur with active window's info */
    UiRsActivateWnd();

    /* If any of the following change, editMsg[] needs to be updated */
    DbAssert (midEditCut - midEditUndo == 1);
    DbAssert (midEditCopy - midEditUndo == 2);
    DbAssert (midEditClear - midEditUndo == 3);
    DbAssert (midEditPaste - midEditUndo == 4);

    if ((menuId >= midEditUndo) && (menuId <= midEditPaste)) {
        SendMessage (pwndAct, editMsg[menuId - midEditUndo], 0, 0L);
    }
    else {
        SetDialogTitle((PMENUITEM)(LOWORD (lParam)));   // set box title
        DbAssert (curHelpFile == 0);        // Shouldn't have help file open
                                            // on entry
        switch (menuId) {

            case midHelpAbout:
                CmdHelpAbout();
                break;

            case midHelpHowToUse:       // QHELP Help.How To Use...
                Help(hemWindowHid,helpHowToUseId,0,0);
                break;

            case midHelpKeyboard:
                Help(hemWindowHid,helpKeyboardId,0,0);
                break;

            case midHelpStarted:
                Help(hemWindowHid,helpStartedId,0,0);
                break;

            case midHelpIndex:
                Help(hemWindowHid,helpIndexId,0,0);
                break;

            case midHelpTable:
                Help(hemWindowHid,helpTableId,0,0);
                break;

            case midHelpSyntax:
                Help(hemAllWindow, 0, 0, 0); //  same as F1
                break;

            case midHelpHelp:                       // [QH1]
                Help(hemAllWindow, 0, 0, KK_SHIFT);         // same as SHIFT-F1
                if (cmdSwitches & CMD_SW_QHELP)
                {
                    if ((HelpFlags & HLP_FAILFNF) || (HelpFlags & HLP_FAILOOM))
                    {   //
                        // If HELP.HLP not found, or out-of-memory, then exit.
                        //
                        CmdFileExit();
                    }
                    else if (fQhelpStarting)
                    {
                        fQhelpStarting = FALSE; // Done with hack
                        WndActivate(&wndHelp);  // Make sure Help window has focus
                    }
                }
                break;

            case midHelpBack:
                SendHelpMsg(WM_HELPBACK,0);
                break;

            case midHelpNext:
                SendHelpMsg(WM_HELPNEXT,0);
                break;


            case midHelpClose:
                CmdHelpClose ();
                break;

            case midFileNew:
                CmdFileNew ();
                break;

            case midFileOpen:
                CmdFileOpen ();
                break;


            case midFileSave:
                EnsFileRs ();
                CmdFileSave ();
                break;

            case midFileSaveAs:
                EnsFileRs ();
                CmdFileSaveAs ();
                break;


            case midFilePrint:
                CmdFilePrint ();
                break;


            case midFileExitQH:
            case midFileExit:
                CmdFileExit ();
                break;

            case midEditClear2:
                /*
                    midEditClear is tied to the menu item. It will not be
                    sent when the menu item is not enabled (i.e. no selection).
                    midEditClear is tied to the DEL key (it is always enabled).
                    EditMgr will distinguish between need to Cut or Del based
                    on existence of selected text.
                */
                SendMessage (pwndAct, WM_CLEAR, 0, 0L);
                break;

            case midEditCut2:
                /*
                    midEditCut is tied to the menu item. It will not be
                    sent when the menu item is not enabled (i.e. no selection).
                    midEditCut2 is tied to the SHIFT+DEL key (it is always enabled).
                    EditMgr will distinguish between need to Cut or Del based
                    on existence of selected text.
                */
                SendMessage (pwndAct, WM_CUT, 0, 0L);
                break;

            case midEditNewSub:
                CmdNewProc (RS_sub);
                break;

            case midEditNewFunc:
                CmdNewProc (RS_function);
                break;

/* keep this for KANJI versions */

            case midViewSubs:
                // Could be invoked from status line, so title set above would
                // be wrong.   Simplest just to always set dialog box title to
                // the proper string.
                DbAssert (rgMenuItemView[0].idItem == midViewSubs);
                SetDialogTitle(&rgMenuItemView[0]);      // set proper title
                CmdViewSubs();
                break;

            case midViewSplit:
                WndSplit();
                break;


            case midViewOutScrn:
                ViewOutScrn ();
                break;


/* keep this for KANJI versions. */


            case midSearchFind:
                CmdSearchFind (TRUE, TRUE, FALSE);
                break;


            case midSearchNext:
                CmdSearchFind (FALSE, FALSE, FALSE);
                break;

            case midSearchChange:
                CmdSearchChange ();
                break;

            case midRunStart:
                CmdGo (TRUE);
                break;

            case midRunRestart:
                CmdRestart ();
                break;

            case midRunContinue:
                CmdGo (FALSE);
                break;


            case midDebugTraceOn:
                fTraceOn = (bool) !fTraceOn;   /* toggle TRACE mode */
                break;


            case midDebugClearAllBp:
                ClrBpAll ();
                DrawDebugScr ();
                break;

            case midDebugSetNextStmt:
                CmdSetNextStmt ();
                break;

            case midDebugToggleBp:
                CmdToggleBp ();
                break;


            case midOptnsDisplay:
                CmdOptnsDisplay ();
                break;

            case midOptnsPaths:
                CmdOptnsPaths ();
                break;


            case midOptnsSyntax:
                fSyntaxCheck = (bool) !fSyntaxCheck;   /* toggle Syntax Checking */
                fOptionsChanged = TRUE;
                break;

            case midStep:
                CmdStep (FALSE);
                break;

            case midPStep:
                CmdStep (TRUE);
                break;


            case midGoUntilCursor:
                CmdGoUntilHere ();
                break;

            case midNextWindow:
                ViewNext ();
                break;

            case midPreviousWindow:
                ViewPrev ();
                break;

            case midViewNextProc:
                WndAssignNext (FALSE);
                break;

            case midViewPrevProc:
                WndAssignNext (TRUE);
                break;

/*  Since Accelerator keys will now handle ALT/? characters, add these
/*  to the list of cases to handle.
 */
            case midWndGrow:
                WndGrow ();
                break;

            case midWndShrink:
                WndShrink ();
                break;

/* Used to be System items */
            case midWndRestore:
                WndNoFullScreen ();
                break;

            case midWndMaximize:
                ViewFull ();
                break;
            case midEnter:
                CmdEnter();
                break;

        } /* switch */
        szDialogTitle = NULL;               // no title to print
        DbAssert (curHelpFile == 0);        // Shouldn't have help file open
                                            // on exit
    } /* if */
} /* DoMenu */


/***
*WORD FAR MainWndProc (pwnd, msg, wParam, lParam)
*Purpose:
*  This routine processes messages for the main window.
*
*Entry:
*  pwnd     window the message is for.
*  msg      message.
*  wParam      word parameter.
*  lParam      long parameter.
*
*Exit:
*      returns FALSE.
*
*Exceptions:
*  None.
*******************************************************************************/
WORD FAR
MainWndProc (pwnd, msg, wParam, lParam)
PWND pwnd;
WORD msg, wParam;
DWORD lParam;
{
    BYTE ax, ay;

    Unreferenced (pwnd);

    if (!fDebugScr) {
        fAccelCmd = TRUE;
        return (0);
    }

    if (msg >= WM_MOUSEFIRST && msg <= WM_MOUSELAST) {
        ax = LOBYTE (HIWORD (lParam));
        ay = HIBYTE (HIWORD (lParam));
        DoCaption (msg, ax, ay);
        return (0);
    }

    switch (msg) {

        case WM_PAINT:
            DrawCaptions ();
            break;

        case WM_VSCROLL:
        case WM_HSCROLL:
            EditWndProc (pwndAct, msg, wParam, lParam);
            break;

        case WM_INITMENUPOPUP:
            MenuEnable();               //  rewrote in asm to reduce size

            KeywordHelpLookup();        //  set up keyword if we have one
            break;

        case WM_MENUSELECT:


            switch (HIWORD(lParam)) {
                case 0:                 // menu item
                    if (wParam == 0) {  // termination
                        DoStatusMsg(pwndAct);   // draw right status message
                        goto StatusMsgShown; // get out of here
                    }
                    else    // display help on selected menu item on status line
                        wParam = wParam - midFileNew + MSG_HelpFileNew;
                    break;
                case 2:     // menu bar
                    wParam = MSG_StatusMenu;  //  display menu bar help
                    break;
            }

            iHelpId = wParam;
            StatusLineMsg (wParam);
StatusMsgShown:
            break;

        case WM_SEARCHFIND:
            wParam = midSearchFind;
            goto DoCmd;

        case WM_SEARCHCHANGE:
            wParam = midSearchChange;
DoCmd:
        case WM_COMMAND:
            DoStatusMsg(pwndAct);  // draw right status line message
            DoMenu (wParam, lParam);
            fAccelCmd = TRUE;
            break;

        case WM_SETBOOKMARK:
            SetBookMark (wParam - '0');
            break;
        case WM_GOTOBOOKMARK:
            GotoBookMark (wParam - '0');
            break;

    } /* switch (msg) */

    return (0);
} /* MainWndProc */

/***
*void NEAR Quit ()
*Purpose:
*  Post quit message to root.
*
*Entry:
*  None.
*
*Exit:
*  None.
*
*Exceptions:
*  None.
*******************************************************************************/
void NEAR Quit ()
{
    PostMessage (NULL, WM_QUIT, 0, 0L);
}


/***
*void NEAR SetDialogTitle ()
*Purpose:
*
*  Sets the dialog box title string from a given menu item.
*
*Entry:
*  pMenuItem = *menu item struture for dialog box we are to display.
*
*Exit:
*  szDialogTitle and cbDialogTitle are updated.
*
*Exceptions:
*  None.
*******************************************************************************/
void NEAR SetDialogTitle (pMenuItem)
PMENUITEM pMenuItem;
{
    if (pMenuItem) {
        szDialogTitle = pMenuItem->CC_USZ.szItem; // set dialog box title
        cbDialogTitle = pMenuItem->wParamUser;    // set # chars to show
    } else
        szDialogTitle = NULL;      // no title to print if null pointer
               // (just to be safe)
}
