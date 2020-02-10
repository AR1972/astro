/***
*uiwind.c
*
*  Copyright <C> 1985-1988, Microsoft Corporation
*
*Purpose:
*  Window Management Functions.
*
*******************************************************************************/

/* First, include version definition header */
#include <version.h>

/* Next, include COW's interface headers */
#include <cw/version.h>
#include <cw/windows.h>
#include <cw/edityp.h>
#include <uiext.h>
#include <uinhelp.h>

/* Next, include QBI's headers */
#ifndef CONTEXT_H
#include <context.h>
#endif

#ifndef PARSER_H
#include <parser.h>
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

#ifndef UTIL_H
# include "util.h"
#endif

STATICF(PWND) PwndPrev(PWND);
STATICF(PWND) PwndStealAbove(PWND);
STATICF(PWND) PwndGiveAbove(PWND);
STATICF(PWND) PwndStealBelow(PWND);
STATICF(void) SetWndFields(PWND, ushort);
STATICF(void) WndRecalc(void);
STATICF(void) WndSetEnable(PWND);
PWND(NEAR WndAlloc(void));
void NEAR WndDebugOpen(AY);
void FAR WnReAssign(ushort, ushort, boolean);
void FAR WndReAssign(ushort, ushort, boolean);
void NEAR WndAssignList1(void);
void NEAR DoWndAssignList(boolean);
STATICF(void) DragCaption(PWND, AY);
void FAR DrawDebugScrFar(void);
void FAR DoDrawDebugScrFar(void);
void FAR ScreenRedraw(void);
void NEAR PutChSetup(AX, AY);
void NEAR PutCh(char);
void NEAR PutChCbPb(ushort, char *);
void FAR StatusMsgFar(short);

/* Forward declarations */
STATICF(void) DrawPwndOutline(PWND);
STATICF(void) WndFullScreen(PWND);
void NEAR WndNoFullScreen(void);

extern ushort NEAR HStatusButton(ushort);    //[47]
extern VOID NEAR DoStatusButton(ushort);     //[47]
extern ushort NEAR ObStatusButton(ushort);      //[47]
extern short NEAR CbStatusButton(ushort);    //[47]
ushort hStatusButtonCur = NULL;           //[47]

WORD FAR DebugWndProc (PWND, WORD, WORD, DWORD);
DWORD FAR HelpWndProc (PWND, WORD, WORD, DWORD);
uchar * NEAR AppendBuf(uchar *, WORD);
WORD FAR StatusWndProc (PWND, WORD, WORD, DWORD);
DWORD FAR EditFilterWndProc (PWND, WORD, WORD, DWORD);

#define DayPwnd(pw)  ((AY)((pw)->arcClipping.ayBottom-(pw)->arcClipping.ayTop))
#define DaxPwnd(pw)  ((AX)((pw)->arcClipping.axRight-(pw)->arcClipping.axLeft))
#define GrowPwnd(pw,day)   SetWindowSize(pw,DaxPwnd(pw),DayPwnd(pw)+day)
#define ShrinkPwnd(pw,day) SetWindowSize(pw,DaxPwnd(pw),DayPwnd(pw)-day)

/* TRUE if scroll bars are to be visible */
boolean fScrollBars = TRUE;
/* TRUE when HELP is visible and WATCH is dispabled */
bool fHelpVisible = FALSE;
bool fAdjustCursor = FALSE;      //[52]

static bool fSignonDisplayed = FALSE;  //[64] TRUE if signon has been displayed
extern BYTE HelpFlags;        //[45]

#define cWindowsMax  4

static bool fDaySaveHelpOk = FALSE; //[34]
static ushort daySaveHelpWnd1;      //[34]
static ushort daySaveHelpWnd2;      //[34]
static ushort daySaveHelpCmd;    //[34]
static PWND pwndActSaveHelp;     //[41]


               //[72] so don't ever put it up
short iMsgStatusLine = MSG_StatusEdit; //[72] start with Edit window message
ISA isaStatusLineVal = isaStatusLine;  //[72] start in normal intensity
ushort cbStatusMsg;     //[10] size of current status message

struct ef EfTop, EfAltList, EfCmd, EfHelp;

WND wnd1 =
    wndGeneric (idWndEdit, WS_CHILD | WS_EDIT, TRUE, 0, 1, 80, 22,   //[55]
                EditFilterWndProc, &wndMain, NULL, NULL)
                { 0, (WORD) &EfTop }                  //[60]
    endWndGeneric;

WND wnd2 =
    wndGeneric (idWndEdit, WS_CHILD | WS_EDIT, TRUE, 0, 0, 0, 0,  //[55]
      EditFilterWndProc, &wndMain, NULL, NULL)
    { 0, (WORD) &EfAltList }                 //[60]
    endWndGeneric;

PWND pwndFull = NULL;
   /* points to the entry in twin which is currently full screen,
      NULL if no window is currently full screen */

WND wndCmd =
    wndGeneric (idWndEdit, WS_CHILD | WS_EDIT, TRUE, 0, 0, 0, 0,  //[55]
      EditFilterWndProc, &wndMain, NULL, NULL)
    { 0, (WORD) &EfCmd }                  //[60]
    endWndGeneric;

WND wndDebug =
    wndGeneric (idWndDebug, WS_CHILD, TRUE, 0, 0, 0, 0,
      DebugWndProc, &wndMain, NULL, NULL)
    { 0, 0 }                        //[60]
    endWndGeneric;

WND wndHelp =
    wndGeneric (idWndHelp, WS_CHILD, TRUE, 0, 0, 0, 0,
      HelpWndProc, &wndMain, NULL, NULL)
    { 0, (WORD) &EfHelp }                 //[60]
    endWndGeneric;


WND wndStatus =
    wndGeneric (idWndStatus, WS_CHILD, TRUE, 0, 0, 0, 0,
      StatusWndProc, NULL, NULL, NULL)
    { 0, 0 }                        //[60]
    endWndGeneric;

PWND pwndBegin = &wnd1;
PWND pwndTop = &wnd1;
PWND pwndAct = &wnd1;

bool fWndSplit = FALSE;    //[41]

/* default size of command window */
#define dayCmdMin 2
#define dayCmdMax 10

static bool fDrawDebugScr = FALSE;

/*------------------------- Window Definitions -------------------------------*/

/* root window for Debug Screen, with defaults set for CGA card */
WND wndMain =
   wndGeneric (idWndMain, WS_TILED | WS_HSCROLL | WS_VSCROLL, TRUE,
          0, 0, 80, 25, MainWndProc, NULL, NULL, NULL)
   { 0, 0 }                   //[60]
   endWndGeneric;


extern WND wndScrollV, wndScrollH;

/*------------------------------------------------------------------
          Window Searching Functions
 *----------------------------------------------------------------*/

/*******************************************************************
* PWND PwndPrev (pwnd)
* Purpose:
*  Return pointer to window above this window.  If pwnd is top
*  window on screen, return NULL.
*
*******************************************************************/
STATICF(PWND)
PwndPrev (pwnd)
PWND pwnd;
{
    REG1 PWND pwndPrev;
    REG2 PWND pwndCur;

    pwndPrev = NULL;
    pwndCur = pwndBegin;
    while (pwndCur != pwnd) {
        DbAssert (pwndCur != NULL);
        pwndPrev = pwndCur;
        pwndCur = pwndCur->pwndSibling;
    }
    return (pwndPrev);
}

/*******************************************************************
* PWND PwndStealAbove (pwnd)
* Purpose:
*  Find window above a given one which has free space to give up
*
*******************************************************************/
STATICF(PWND)
PwndStealAbove (pwnd)
PWND pwnd;
{
    REG1 PWND pwndCur;
    REG2 PWND pwndPrev;

    pwndPrev = NULL;
    pwndCur = pwndBegin;
    while (pwndCur != pwnd) {
        DbAssert (pwndCur != NULL);
        if (pwndCur != &wndCmd && DayPwnd (pwndCur))
            pwndPrev = pwndCur;
        pwndCur = pwndCur->pwndSibling;
    }
    return (pwndPrev);
}

/*******************************************************************
* PWND PwndGiveAbove (pwnd)
* Purpose:
*  Find window above a given one which can grow
*
*******************************************************************/
STATICF(PWND)
PwndGiveAbove (pwnd)
PWND pwnd;
{
    REG1 PWND pwndCur;
    REG2 PWND pwndPrev;

    pwndPrev = NULL;
    pwndCur = pwndBegin;
    while (pwndCur != pwnd) {
        DbAssert (pwndCur != NULL);
        if (pwndCur != &wndCmd)
            pwndPrev = pwndCur;
        pwndCur = pwndCur->pwndSibling;
    }
    return (pwndPrev);
}

/*******************************************************************
* PWND PwndStealBelow (pwnd)
* Purpose:
*  Find window at or below a given one which has free space to give up
*
*******************************************************************/
STATICF(PWND)
PwndStealBelow (pwnd)
PWND pwnd;
{
    REG1 PWND pwndCur = pwnd;

    while (pwndCur != NULL && !DayPwnd (pwndCur)) {
        pwndCur = pwndCur->pwndSibling;
    }
    return (pwndCur);
}

/*******************************************************************
* void SetWndFields (pwnd)
* Purpose:
*  Initialize fields in a Pwnd structure. Called when a window
*  is allocated and/or opened.
*
* Entry:
*  wndMain structure describes debug screen
*
*******************************************************************/
STATICF(void)
SetWndFields (pwnd, oRs)
REG1 PWND pwnd;
ushort oRs;
{
    struct ef *pef = (struct ef *) pwnd->pefExtra; // [60]

    /* Set window relative fields */
    pef->pdCur.olntop =
       pef->pdCur.obleft =
       pef->ipCur.ob =
       pef->ipCur.oln =
       pef->ipAnchor.ob =
       pef->ipAnchor.oln = 0;

    pef->pwndScrollV = pef->pwndScrollH = (PWND) NULL;

    pef->hBuffer = oRs;
    pef->pdCur.oln = UNDEFINED;
    pef->fSelection = FALSE;
    pef->attrCur = (pwnd == &wndHelp) ? isaHelpWindow : isaEditWindow;  // [18]

    if ((cmdSwitches & CMD_SW_QHELP) && (pwnd == &wndHelp))
       pef->Style = ES_MULTILINE | ES_NOSELECT;                     // [QH1]
    else
       pef->Style = ES_MULTILINE;


    pef->pldCur = (ld *) &ps.ldDummy[0];
}

extern MENUBAR MenuBarQedit, MenuBarQhelp, MenuBarQbas;       //[74]

/*******************************************************************
* void WndInit ()
* Purpose:
*  Called once during initialization to init the window manager
*  and construct the borders of wndMain, wndScrollH, and wndScrollV.
*
* Entry:
*
*******************************************************************/
void near
WndInit ()
{
    ushort oRs = rsNew;

    /* init scrap for editor.  If we don't do it here, the editor
     * may call it during CopyLines(), at which time heap movement
     * is not allowed.
     */
    hbufScrap ();
    hbufOfCmd = RsMake (NULL, RS_cmdWnd);
    /* RsMake causes rsNew to get set. Reset it so WndReset won't be called
     * again for the Command window */
    rsNew = UNDEFINED;

    /* full screen less menu and status */
    SetWindowSize (&wndMain, axMac, ayMac-1);
    /* move to top, leave 1 row for menubar */
    MoveWindow (&wndMain, 0, 1);
    /* Make a root window */
    AddChild (NULL, &wndMain);

    SetWindowSize (&wndDebug, axMac, 0);
    MoveWindow (&wndDebug, 0, 1);
    AddChild (&wndMain, &wndDebug);

    SetWindowSize (&wndHelp, axMac-2, 0);
    MoveWindow (&wndHelp, 0, 1);
    AddChild (&wndMain, &wndHelp);
    SetWndFields (&wndHelp, hbufHelp); /* [7] */

    SetWindowSize (&wnd1, axMac-2, (ayMac - 3) -
                   ((cmdSwitches & CMD_SW_ED) ? 0 : (dayCmdMin + 1)));  //[74]
    MoveWindow (&wnd1, 1, 2);
    AddChild (&wndMain, &wnd1);
    SetWndFields (&wnd1, oRs);
    AddScrollBars ();      // [16]

    SetWindowSize (&wndStatus, axMac, 1);
    /* Size and Position status window */
    MoveWindow (&wndStatus, 0, ayMac-1);
    /* Another root window */
    AddChild (NULL, &wndStatus);

    // [74] No command window for QEDIT
    if (!(cmdSwitches & CMD_SW_ED)) {
        SetWindowSize (&wndCmd, axMac-2, dayCmdMin);
        MoveWindow (&wndCmd, 1, ayMac-(dayCmdMin+1));
        AddChild (&wndMain, &wndCmd);
        SetWndFields (&wndCmd, hbufOfCmd);
    }

    /* [74] initialize the menu bar */
    if (cmdSwitches & CMD_SW_QHELP)
        InitMenu (&wndMain, &MenuBarQhelp);
    else if (cmdSwitches & CMD_SW_ED)
        InitMenu (&wndMain, &MenuBarQedit);
    else
        InitMenu (&wndMain, &MenuBarQbas);
    FEnableMenuBar (FALSE);   // [27]
}

/***
*void RemoveScrollBars ()
*Purpose:
*  Removes the scroll bars from the active window.  It looks at
*  cScrollBarAdded to see if we are nested into AddScrollBars, and if
*  we are, does nothing.
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
void NEAR RemoveScrollBars ()    // [16] cleaned up
{
    REG1 PWND pwndActReg = pwndAct;
    REG2 struct ef *pef = (struct ef *) pwndActReg->pefExtra;  // [60]

    /* If window has a horizontal scroll bar, resize the window */
    if (pef->pwndScrollH != NULL) {
        SetWindowSize (pwndAct, axMac-2, DayPwnd (pwndActReg) + 1);
        RemoveChild (&wndScrollH); // [26]
    }
    if (pef->pwndScrollV != NULL)   // [26]
        RemoveChild (&wndScrollV); // [26]

    pef->pwndScrollV = pef->pwndScrollH = NULL;
}

/***
*void AddScrollBars ()
*Purpose:
*  Adds the scroll bars to the active window if we have not done so
*  already.
*
*  This also makes sure that the cursor is within the window by calling
*  MoveCursorPwndCur with the current line and column.
*Entry:
*  None.
*
*Exit:
*  None.
*
*Exceptions:
*  None.
*******************************************************************************/
void NEAR AddScrollBars()     // [16] cleaned up
{
    REG1 PWND pwndActReg = pwndAct;
    REG2 struct ef *pef = (struct ef *) pwndActReg->pefExtra;  // [60]

    // [42] [43] Make sure cursor is within window
    fAdjustCursor++;       //[52] [68]

    /* [3] */
    if (!fScrollBars || pwndActReg == &wndCmd)  // [43]
        return;

    if (DayPwnd (pwndActReg) >= 2 && pwndActReg != &wndHelp) { //[75]
        SetWindowSize (pwndActReg, axMac-2, DayPwnd (pwndActReg) - 1);
        SetWindowSize (&wndScrollH, axMac-2, 1);
        MoveWindow (&wndScrollH, pwndActReg->arcClipping.axLeft,
                    pwndActReg->arcClipping.ayBottom);
        pef->pwndScrollH = &wndScrollH;
        // [26] Insert at front of list
        wndScrollH.pwndParent = &wndMain;
        wndScrollH.pwndSibling = wndMain.pwndChild;
        wndMain.pwndChild = &wndScrollH;
    }

    if (DayPwnd (pwndActReg) >= 3) {
        SetWindowSize (&wndScrollV, 1, DayPwnd (pwndActReg));
        MoveWindow (&wndScrollV, axMac-1, pwndActReg->arcClipping.ayTop);
        pef->pwndScrollV = &wndScrollV;
        // [26] Insert at front of list
        wndScrollV.pwndParent = &wndMain;
        wndScrollV.pwndSibling = wndMain.pwndChild;
        wndMain.pwndChild = &wndScrollV;
    }
}


STATICF(void)
WndRecalc()
{
    REG1 PWND pwnd;
    AY ayPrev = (DayPwnd (&wndDebug) == 0) ?
                 (AY) 2 : wndDebug.arcClipping.ayBottom + 1;

    RemoveScrollBars ();   // [16]
    for (pwnd = pwndBegin; pwnd != NULL; pwnd = pwnd->pwndSibling) {
        MoveWindow (pwnd, 1, ayPrev);
        ayPrev = pwnd->arcClipping.ayBottom + 1;
    }
    DbAssert (ayPrev == ayMac);
    AddScrollBars ();

    WndSetEnable(&wndHelp);      //[35]
    WndSetEnable(&wnd1);      //[35]
    WndSetEnable(&wnd2);      //[35]
    WndSetEnable(&wndCmd);    //[35]

    DrawDebugScr ();
    fDaySaveHelpOk = FALSE;      //[34]
}

/*******************************************************************
* void WndSetEnable (pwnd)
* Purpose:
*  Enables the specified window iff it has more than zero lines.
*
*  Renamed WndSetEnable from WndShow in revision [35]
*
*******************************************************************/
STATICF(void)
WndSetEnable (pwnd)
REG1 PWND pwnd;
{
    if (!DayPwnd (pwnd)) {
        /* window just has a title bar, no content is visible */
        if (pwnd->fEnabled) {
            /* window just became invisible */
            EnableWindow (pwnd, FALSE);
        }
    }

    /* window has some content visible */
    else if (!pwnd->fEnabled) {
        /* window just became visible */
        EnableWindow (pwnd, TRUE);
    }
}

/*******************************************************************
* void WndDeactivate ()
* Purpose:
*  If any window is currently active, make it inactive.
*  No changes are visible on the screen until DoDrawDebugScr is next called.
*
* Entry:
*  pwndAct points to active window
*
*******************************************************************/
void NEAR WndDeactivate ()
{
    UiRsActivateWnd (); /* load txdCur with active window's info */
    if (pwndAct != &wndHelp)        //[29]
        txdCur.lnCursor = GetEditLine ();   //[29]
    SetFocus (&wndMain);         //[43]
    RemoveScrollBars ();         // [16]
    DrawDebugScr ();
}

/*******************************************************************
* void WndActivate (pwnd)
* Purpose:
*  Make the specified window active (and deactivate the
*  currently active window (if any).
*  No changes are visible on the screen until DoDrawDebugScr is next called.
* Entry:
*  pwnd points to window to be made active.
*
*******************************************************************/
void NEAR WndActivate (pwndActNew)
REG1 PWND pwndActNew;
{
    DbAssert (pwndActNew != NULL);
    if (pwndAct != pwndActNew) {
        /* window isn't already active */
        WndDeactivate ();
        pwndAct = pwndActNew;

        if (txdCur.lnCursor == UNDEFINED
          || pwndActNew->style & UIWS_MOVECURSOR) { //[53]
            pwndActNew->style &= ~UIWS_MOVECURSOR;      //[53]
            MoveTxdCursor ();
        }

        UiRsActivateWnd ();     // [49]
        AddScrollBars ();    // [16]
        DoStatusMsg(pwndAct);      //[53]
        DrawDebugScr ();
    }
}

/*******************************************************************
* void WndDebugOpen (day, pwnd)
* Purpose:
*  Sets the Debug window size.   Used to allocate space for
*  Debug window.
* Entry:
*  day = number of lines in the Debug window
* Exit:
*
*******************************************************************/
void NEAR WndDebugOpen (day)
REG1 AY day;
{
    REG2 PWND pwnd;
    PWND pwndFullOld = NULL;

    pwndFullOld = pwndFull;
    WndNoFullScreen ();  /* disable full-screen for calculations */

    GrowPwnd (pwndBegin, DayPwnd (&wndDebug));
    SetWindowSize (&wndDebug, axMac, day);   /* [9] */
    while (day--) {
        pwnd = PwndStealBelow (pwndBegin);
        DbAssert(pwnd != NULL);
        DbAssert(DayPwnd (pwnd) != 0);

        ShrinkPwnd (pwnd, 1);
    }

    WndRecalc();
    WndFullScreen (pwndFullOld);
}


/*******************************************************************
* void WndHelpOpen (day, pwnd)
* Purpose:
*  Sets the Top window size.  Used to allocate space for
*  Watch and On-Line-Help windows.
* Entry:
*  day = number of lines in the Top window
* Exit:
*
*******************************************************************/
VOID NEAR WndHelpOpen (dayReq)         //[45]
REG1 WORD dayReq;
{
    REG2 PWND pwnd;
    PWND pwndCur;          //[28]
    AY dayWndGrow;            //[8]
    AY day = 0;            //[28]
    bool fDaySaveTmp;            //[34]

    if (dayReq != 0 && pwndFull == &wndHelp) //[41]
        return;           //[41]

    // If opening help window for /QHELP, make it as big as possible
    if ((dayReq != 0) && (cmdSwitches & CMD_SW_QHELP)) {
        dayReq = ayMac;
    }

    WndNoFullScreen ();          //[44]
    /***** Start of revision [34] *****/
    /*
     * If we are putting up fresh help save the sizes of the windows.
     *
     */
    if (!fHelpVisible && dayReq != 0) {
        fDaySaveHelpOk = TRUE;
        RemoveScrollBars();
        daySaveHelpWnd1 = DayPwnd(&wnd1);
        daySaveHelpWnd2 = DayPwnd(&wnd2);
        daySaveHelpCmd = DayPwnd(&wndCmd);
        pwndActSaveHelp = pwndAct;    //[41]
        /* Note: AddScollBars() will be done in WndRecalc below */
    }
    fDaySaveTmp = fDaySaveHelpOk;
    /***** End of revision [34] *****/

    pwnd = pwndTop;           //[41]
    dayWndGrow = DayPwnd(&wndHelp);    //[41]

    if (fHelpVisible) {          //[39]
        GrowPwnd (pwndTop, dayWndGrow+1);   //[8]
    }

    fHelpVisible = (bool) (dayReq != 0);  //[39] don't set until after
                  //[39] WndNoFullScreen();

    if (dayReq == 0) {
        pwndBegin = pwndBegin->pwndSibling;
        /***** Start of revision [34] *****/
        if (fDaySaveTmp) {
            RemoveScrollBars();
            SetWindowSize(&wnd1, axMac-2, (AY) daySaveHelpWnd1);
            SetWindowSize(&wnd2, axMac-2, (AY) daySaveHelpWnd2);
            SetWindowSize(&wndCmd, axMac-2, (AY) daySaveHelpCmd);
            /* Note: AddScollBars() will be done in WndRecalc below */
        }
        fDaySaveTmp = FALSE;
        /***** End of revision [34] *****/
    }
    else {
        dayReq++;
        while (dayReq--) {
            pwndCur = pwnd;        //[28]
            if ((pwnd = PwndStealBelow (pwndCur)) == NULL)  //[28]  //[34]
                break;            //[28]

            // DO Steal last line from active window!
            if (!(cmdSwitches & CMD_SW_QHELP)) {
                /***** Start of revision [34] *****/
                /*
                 * Don't steal the last line from the active window.
                 */
                if ((pwnd == pwndAct) && (DayPwnd(pwnd) <= 1)) {
                    pwnd = pwnd->pwndSibling;
                    if (pwnd == NULL)
                        break;
                    continue;
                }
            }
            /***** End of revision [34] *****/

            DbAssert(DayPwnd (pwnd) != 0);
            day++;

            ShrinkPwnd (pwnd, 1);
        }

        pwndBegin = &wndHelp;
        day --;           //[40]
    }

    DbAssert(day >= 0);          //[40]
    SetWindowSize (&wndHelp, axMac-2, day);  //[40]

    // Make /QHELP window full screen
    if (cmdSwitches & CMD_SW_QHELP)
        WndFullScreen(&wndHelp);
    else
        WndRecalc();          //[28]
    fDaySaveHelpOk = fDaySaveTmp;      //[34]
}

/*******************************************************************
* void WndHelpClose ()
* Purpose:                     1037
*  Restores the Debug window.
*
*******************************************************************/
void NEAR WndHelpClose ()
{
    PWND pwndFullSave = pwndFull;   //[32] save full window status

    WndNoFullScreen();        //[32] no full window
    if (pwndAct == &wndHelp) {      // [10] [16] Must be at beginning.
        DbAssert(pwndActSaveHelp != 0);     //[41]
        if (DayPwnd(pwndActSaveHelp) > 0)   //[41]
            WndActivate(pwndActSaveHelp);   //[41]
        else              //[41]
            WndActivate (pwndTop);    // [10]
    }
    if (fHelpVisible)
        WndHelpOpen (0);

    if (pwndFullSave && pwndFullSave != &wndHelp) {  //[32] restore full window
        WndFullScreen(pwndFullSave);          //[32] if it didn't close
        DrawDebugScr();
    }
}

/*******************************************************************
* void WndOpen (oRs)
* Purpose:
*  Create a new window in the debug screen.
*  Assumes the debug screen is already visible.
*  On exit, the newly created window is made active.
*  Factors which determine the window's size and location are:
*     - lines per screen (25 for CGA, 43 for EGA)
*     - whether menu bar and/or status line are enabled (visible)
*     - Current number of windows on screen
*
* Entry:
*  oRs identifies text table (register set) to be displayed in window
*
* Exit:
*  new window is linked into pwndTop's list and is made active,
*  ax points to new Pwnd structure.
*
*******************************************************************/
void NEAR WndOpen (oRs)
ushort oRs;
{
    REG1 PWND pwndNew;
    BYTE day;

    DbAssert (!fWndSplit && pwndTop != NULL);   //[41]

    /*
       [41] Make sure the window that is being split is big enough
    */
    WndActivate (pwndTop);        //[41]
    while (DayPwnd(pwndTop) < 3) {     //[41]
       WndGrow();          //[41]
    }                //[41]

    WndDeactivate ();
    /* Calculate day after WndDeactivate so scroll bars are not counted */
    day = DayPwnd(pwndTop);         //[41]
    pwndNew = (pwndTop == &wnd1) ? &wnd2 : &wnd1;

    SetWindowSize (pwndNew, axMac-2, day >> 1);
    SetWindowSize (pwndTop, axMac-2, (day - (day >> 1)) - 1);
    RemoveChild (&wndCmd);
    AddChild (&wndMain, pwndNew);
    AddChild (&wndMain, &wndCmd);
    SetWndFields (pwndNew, oRs);

    WndRecalc ();
    WndActivate (pwndNew);
    /* source line at top of window */
    // [16] Do this in WndOpen
    MoveCursorPwndCur (((struct ef *) pwndAct->pefExtra)->pdCur.olntop, 0);   // [60]
    WndActivate (pwndTop); // [16]
    fWndSplit = TRUE;
}

/*******************************************************************
* void WndClose ()
* Purpose:
*  This is called when the user asks to close a window.
*  All controls and edit fields in the window are released.
*  The window is removed from pwndTop list of visible windows.
*  All the closed window's lines are given to the window below.
*  If the closed window was active, the source window below
*  is made active.  If no source windows are left on the screen,
*  the command window is made active.
*
* Entry:
*  pwndAct - pointer to window to close
*
*******************************************************************/
void NEAR WndClose ()      // [16] clean up code.
{
    REG1 PWND pwndClose;
    REG2 PWND pwndGrow;

    DbAssert (pwndAct != &wndHelp); //[29]
    DbAssert (pwndAct != &wndCmd && pwndAct != NULL && fWndSplit);   //[41]
    if (pwndAct == &wnd2) {      //[41]
        pwndClose = &wnd1;      //[41]
        pwndGrow = &wnd2;    //[41]
    }             //[41]
    else {           //[41]
        pwndClose = &wnd2;      //[41]
        pwndGrow = &wnd1;    //[41]
    }             //[41]

    WndDeactivate ();

    /* actually give released space to other window */
    GrowPwnd (pwndGrow, DayPwnd (pwndClose) + 1);
    RemoveChild (pwndClose);

    pwndTop = pwndGrow;
    DbAssert (pwndTop != NULL); /* [24] */
    if (pwndBegin == pwndClose) // [16] Forgot to set pwndBegin.
        pwndBegin = pwndTop; // [16]
    WndRecalc();
    /* activate window which received the new space */
    WndActivate (pwndGrow);
    fWndSplit = FALSE;
}

/*******************************************************************
* void FAR WndReAssign (oRsOld, oRsNew, fRename)
* Purpose:
*  This function is called by the Text Manager when the user changes
*  the name of a procedure window by entering a SUB or FUNCTION statement
*  with an id name different from the window.
*  It is also called by MrsDiscard and PrsDiscard.
*  For each window, if the window is showing oRsOld, it is changed
*  to show oRsNew.
*  In addition, if rsNew == oRsOld, it is set to UNDEFINED (so WnReset
*  won't later try to show this now defunct register set.
*
* Entry:
*  oRsOld = register set which should no longer be displayed
*  oRsNew = register set to be shown in its place
*  fRename is TRUE if window is just being renamed (i.e. SUB FOO -> SUB FOO1)
*    if FALSE, cursor is moved to txdCur.lnCursor
*    (i.e. the place it was when oRsNew was last visible)
*
* Exit:
*  Existing grs.oRsCur is preserved
*
*******************************************************************/

void FAR WndReAssign (oRsOld, oRsNew, fRename)
ushort oRsOld;
ushort oRsNew;
boolean fRename;
{
    REG1 PWND pwndCur;
    ushort oRsSave;
    struct ef *pef;
    PWND   pwndFullOld;    //[66]

    oRsSave = grs.oRsCur;
    if (rsNew == oRsOld) {
        /* window is being removed before it ever got shown */
        rsNew = UNDEFINED;
    }

    pwndFullOld = pwndFull;   //[66]
    WndNoFullScreen();     //[66]

    for (pwndCur = pwndTop; pwndCur != NULL; pwndCur = pwndCur->pwndSibling) {
        pef = (struct ef *) pwndCur->pefExtra;    // [60]
        if (oRsOld == pef->hBuffer) {
            /* can't call EditMgrFlush1(), because it would end up calling
             * RsActivate for oRsOld, which no longer exists
             */
            pef->hBuffer = oRsNew;
            if (!fRename) {
                // [42] Cursor will be homed if txd.lnCursor is UNDEFINED (i.e.
                // [42] if oRsNew is for a new text table) the next time
                // [42] window is activated or DoDrawDebugScr is called.
                pwndCur->style |= UIWS_MOVECURSOR;
            }
            DrawDebugScr ();
        }
    }

    //[63] If we are renaming the pRs/oRs (fRename == TRUE) then we should
    //[63] update the bookmark list.  Otherwize, the bookmarks should be
    //[63] erased.

    ReAssignBookMark(oRsOld,(fRename ? oRsNew : UNDEFINED)); //[63]

    /* A Search/Change may be in progress.  The change may have caused
     * the oRs of a window to change.  If so, keep uisearch.c up to date.
     */
    SrchReAssign (oRsOld, oRsNew);

    WndFullScreen(pwndFullOld);  //[66]

}


void FAR WnReAssign (oRsOld, oRsNew, fRename)
ushort oRsOld, oRsNew;
boolean fRename;
{
    WndReAssign (oRsOld, oRsNew, fRename);
}

/**************************************************************************
* GetEditLine ()
* Purpose:
*  Returns the number of the current line (the line that has the cursor on it).
*
**************************************************************************/
ushort NEAR GetEditLine ()
{
    return (((struct ef *) pwndAct->pefExtra)->ipCur.oln);  //[43] [60]
}

/**************************************************************************
* GetEditColumn ()
* Purpose:
*  Returns the number of the current column (the column that has the
*  cursor on it).
*
**************************************************************************/
ushort NEAR GetEditColumn ()
{
    return (((struct ef *) pwndAct->pefExtra)->ipCur.ob);   //[43] [60]
}

/*******************************************************************
* void WndAssign ()
* Purpose:
*  Make active register set visible in active window.
* Entry:
*  grs.oRsCur identifies register set (text table) to show
*  pwndAct points to active window structure
* Exit:
*  grs.oRsCur will not change from caller's value (callers assume this)
*  Flag is set so debug screen will be refreshed next time
*  DoDrawDebugScr is called.
*
*******************************************************************/
void NEAR WndAssign ()
{
    ushort oRsNew;
    struct ef *pef = (struct ef *) pwndAct->pefExtra;    // [60]

    DbAssert (pwndAct != NULL);
    oRsNew = grs.oRsCur;

    /* rs isn't currently visible in active window OR
     * this is the first time we've shown this rs (TxtCurInit initializes
     * lnCursor to UNDEFINED.  This extra check is needed because register
     * sets are serially re-used as new file are unloaded and loaded.
     */
    if (oRsNew != pef->hBuffer || txdCur.lnCursor == UNDEFINED) {
        if (oRsNew != pef->hBuffer) {
            UiRsActivateWnd ();  /* activate pwndAct's register set */
            txdCur.lnCursor = GetEditLine ();
            /* so edit mgr doesn't think it already has current line cached */
            EditMgrFlush1 ();
            pef->hBuffer = oRsNew;
            UiRsActivate (oRsNew);
        }

        // position cursor where it was last time text was visible,
        // or at 0,0 if this txdCur.lnCursor == UNDEFINED
        MoveTxdCursor ();
        DrawDebugScr ();
    }

    DbAssert (oRsNew == grs.oRsCur);
}

/*******************************************************************
* void WndAssignList ()
* Purpose:
* Entry:
* Exit:
*
*******************************************************************/
void NEAR WndAssignList ()
{
    DoWndAssignList(FALSE);
}

/*******************************************************************
* void WndAssignList1 ()
* Purpose:
*
*******************************************************************/
void NEAR WndAssignList1 ()
{
    DoWndAssignList(TRUE);
}

/*******************************************************************
* void DoWndAssignList()
* Purpose:
*  Assigns grs.oRsCur to a list window.
*  If the active list window already is set to grs.oRsCur then this
*  is a no-op.
*  grs.oRsCur is assigned to:
*     wnd1 - If !fUseOtherWnd or !fWndSplit
*     wnd2 - Otherwise
*
*  Merge and rewrite of WndAssignList/WndAssignList1 in revision [54]
*
* Entry:
*  fUseOtherWnd - TRUE if other list window should be used if available.
* Exit:
*       grs.oRsCur is preserved.
*
*******************************************************************/
void NEAR DoWndAssignList(boolean fUseOtherWnd)
{
    REG1 ushort oRsNew;

    oRsNew = grs.oRsCur;

    if (pwndAct != &wnd1 && pwndAct != &wnd2) {
        WndNoFullScreen();
        WndActivate(pwndTop);
    }

    UiRsActivateWnd();
    if (grs.oRsCur == oRsNew)
        return;

    if (fWndSplit && fUseOtherWnd) {
        WndNoFullScreen();      //[62] must get out of full screen mode if we
                                //[62] are going to draw on the second window
        WndActivate((pwndAct == &wnd1) ? &wnd2 : &wnd1);
    }
    UiRsActivate (oRsNew);
    WndAssign();
}

/*******************************************************************
* void WndAssignNext (fPrev)
* Purpose:
*  Show next/previous procedure in current module in the active
*  list window.  If list window is not active, it is made active.
* Entry:
*  fPrev is TRUE if previous procedure is to be shown.
* Exit:
*  Flag is set so debug screen will be refreshed next time
*  DoDrawDebugScr is called.
*
*******************************************************************/
void NEAR WndAssignNext (fPrev)
boolean fPrev;
{
    REG1 ushort oRsPrev;
    ushort oRsCur;

    /* make sure a list window is active */
    DbAssert (pwndAct != NULL);
    if (UiAlphaORsBuild() == 0) {
        SetUiErrOm();
        return;
    }

    if (pwndAct == &wndCmd || pwndAct == &wndHelp) {  //[29]
        WndNoFullScreen ();           //[36]
        WndActivate (pwndTop);
    }
    UiRsActivateWnd ();    /* load mrsCur, prsCur with active window's info */
    oRsPrev = oRsCur = grs.oRsCur;
    NextAlphaPrs ();    /* activate next proc in current module */
    if (fPrev) {
        /* find prs that alphabetically precedes oRsCur */
        while (grs.oRsCur != oRsCur) {
            oRsPrev = grs.oRsCur;
            NextAlphaPrs (); /* activate next proc in current module */
        }
        UiRsActivate (oRsPrev);
    }

    /* make active proc visible in active window */
    WndAssign ();
}

/**************************************************************************
* WndSplit ()
* Purpose:
*  Split the active list window, or close the alternate list window.
*  On entry, we know that the active window is a list window.
*
**************************************************************************/
VOID NEAR WndSplit ()      // [16] Move functionality to WndOpen/Close
{
    WndNoFullScreen ();
    DbAssert (pwndTop != NULL);

    /* Command window is active.  Don't split it */

    if (pwndAct == &wndCmd || pwndAct == &wndHelp) {  //[29]
        WndActivate (pwndTop);
    }

    /* [37] only 1 list window is currently visible */
    if (!fWndSplit)
        WndOpen (grs.oRsCur);
    /* list window is already split - close inactive half */
    else
        WndClose ();
}

/*******************************************************************
* WnReset(oRs)
* Purpose:
*  We are either initializing, or the user has executed a NEW or LOAD
*  statement, which means windows we are displaying are no longer
*  valid.  This function makes the specified oRs visible in the
*  active list window (or any list window if no list window is active).
*
* Entry:
*  oRs identifies list window to show
*
*******************************************************************/
void FAR WnResetMove(oRs)
ushort oRs;
{
    WndReset (oRs);
    pwndAct->style |= UIWS_MOVECURSOR;    //[53]
}

/*******************************************************************
* WndReset (oRs)
* Purpose:
*  We are either initializing, or the user has executed a NEW or LOAD
*  statement, which means windows we are displaying are no longer
*  valid.  This function makes the specified oRs visible in the
*  active list window (or any list window if no list window is active).
*
* Entry:
*  oRs identifies list window to show
*
*******************************************************************/
void NEAR WndReset (oRs)
ushort oRs;
{
    DbAssert(oRs != UNDEFINED);
    UiRsActivate (oRs);
    WndAssignList ();
}

/*------------------------------------------------------------------
          Window Movement Functions
 *----------------------------------------------------------------*/

/*******************************************************************
* void WndFullScreen (pwndFullNew) - maximize or restore window
* Purpose:
*  Make window take up full screen, hiding all other windows on screen.
*  If pwndFullNew == NULL, restore all windows to their previous sizes.
*  Windows are toggled full screen by double clicking their caption bars,
*  or by clicking the maximize/restore icon.
*
*  Note: Since this routine is called from WndReAssign, neither this routine
*   nor anything that it calls can access any register sets.  Nor can
*   they call the text manager, as it is non-reentrant.
*
*
*Entry:
*  pwndFullNew: if non-NULL, window to maximize
*     if NULL, restore windows to original sizes
*
*Exit:
*  none
*
*******************************************************************/
STATICF(void)
WndFullScreen (pwndFullNew)
REG1 PWND pwndFullNew;
{
    REG2 PWND pwndCur;
    short i;
    static short cWindows = 0;
    static PWND rgPwndSave[cWindowsMax];
    static AY daySave;

    DbAssert (pwndFull == NULL || pwndFullNew == NULL);
    if (!((ushort) pwndFull | (ushort) pwndFullNew)) {
        return;
    }
    DbAssert (!(cmdSwitches & CMD_SW_ED))       //[74]

    RemoveScrollBars ();   // [16]

    if (pwndFullNew) {  /* maximize pwndFullNew */
        cWindows = 0;
        while ((pwndCur = wndDebug.pwndSibling) != NULL) {
            DbAssert (cWindows < cWindowsMax);
            rgPwndSave[cWindows++] = pwndCur;
            RemoveChild (pwndCur);
        }
        daySave = DayPwnd (pwndFullNew);
        AddChild (&wndMain, pwndFullNew);
        SetWindowSize (pwndFullNew, (axMac-2),
                       (ayMac-3) - DayPwnd (&wndDebug));
        pwndBegin = pwndTop = pwndFullNew;  /* [24] added pwndTop */
    }
    else { /* restore to original size */
        RemoveChild (pwndFull);
        for (i = 0; i < cWindows; i++) {
            AddChild (&wndMain, rgPwndSave[i]);
        }
        SetWindowSize (pwndFull, axMac-2, daySave);
        pwndBegin = fHelpVisible ? &wndHelp : wndHelp.pwndSibling;
        pwndTop = wndHelp.pwndSibling;
        DbAssert (pwndTop != NULL); /* [24] */
    }

    /* set pwndFull to NULL for minimize, pwndFullNew for maximize */
    pwndFull = pwndFullNew;

    WndRecalc(); // [31]
}

/*******************************************************************
* void WndNoFullScreen ()
* Purpose:
*  If the active window is in full-screen mode, restore it to its original
*  size and location.
*
*******************************************************************/
void NEAR WndNoFullScreen ()
{
    WndFullScreen (NULL);
}

/*******************************************************************
* ViewFull
* Purpose:
*  Called when F6 is pressed to toggle active window between normal
*  size and full-screen size.
*
*******************************************************************/
VOID NEAR ViewFull ()
{
    DbAssert (pwndAct != NULL);
    if (pwndFull == NULL)
        WndFullScreen (pwndAct);
    else
        WndNoFullScreen ();
}

/*******************************************************************
* void DragCaption(pwnd, ayNew)
*
* Purpose:
*  move the caption bar of a window, causing 1 window to grow, and
*  another to shrink.
*
* Entry:
*  pwnd = window whose title bar is to be moved
*  lnNew = new screen line for title bar
*
*******************************************************************/
STATICF(void)
DragCaption (pwnd, ayNew)
REG1 PWND pwnd;
AY ayNew;
{
    REG2 PWND pwndShrink;
    AY ayCur = pwnd->arcClipping.ayTop-1, ayTopCmd;
    PWND pwndPrev;

    RemoveScrollBars ();      // [16]

    /* Don't let command window grow larger than dayCmdMax lines */
    ayTopCmd = ayMac - dayCmdMax - 2;
    if (pwnd == &wndCmd && ayNew < ayTopCmd)
        ayNew = ayTopCmd;

    while (ayNew < ayCur) {
        /* title bar has been dragged up at least 1 line */
        /* find lowest window above pwnd that has any space to give up */
        if ((pwndShrink = PwndStealAbove (pwnd)) == NULL)
            break;   /* no space can be obtained */
        ShrinkPwnd (pwndShrink, 1);
        GrowPwnd (pwnd, 1);

        ayCur--;
    }

    while (ayNew > ayCur) {
        /* title bar has been dragged down at least 1 line */
        /* find lowest window above pwnd that can take space */
        if ((pwndPrev = PwndGiveAbove (pwnd)) == NULL)
            break;   /* no window above can take the space */

        /* we may need to move windows below this one down as well,
           set pwndShrink to highest window at or below current window
           which we can steal from */
        if ((pwndShrink = PwndStealBelow (pwnd)) == NULL)
            break;   /* this window and all windows below have no space to give */

        ShrinkPwnd (pwndShrink, 1);
        GrowPwnd (pwndPrev, 1);

        ayCur++;
    }

    WndRecalc ();
}

/*******************************************************************
* void ViewPrev()
* Purpose:
*  Called when Alt-Shift-Tab is pressed.  Makes previous window active.
*
*******************************************************************/
void NEAR ViewPrev ()
{
    REG1 PWND pwndCur;
    PWND pwndFullSave;

    WndDeactivate();       //[53]
    pwndFullSave = pwndFull;
    WndNoFullScreen ();

    pwndCur = pwndAct;     //[43]
    if ((pwndCur = PwndPrev (pwndCur)) == NULL) {
        /* top window is currently active, activate bottom window in screen */
        for (pwndCur = pwndAct;    //[53][54]Backout rev [53] change.
             pwndCur->pwndSibling != NULL;
             pwndCur = pwndCur->pwndSibling) ;
    }

    WndActivate (pwndCur);

    if (pwndFullSave != NULL) {
        WndFullScreen (pwndCur);
        DrawDebugScr ();
    }
}

/*******************************************************************
* void ViewNext()
* Purpose:
*  Called when VIEW/NEXT menu item is selected.  Makes next window
*  active.
*
*******************************************************************/
void NEAR ViewNext ()
{
    REG1 PWND pwndNext;
    PWND pwndFullSave;

    WndDeactivate();       //[53]
    pwndFullSave = pwndFull;
    WndNoFullScreen ();

    if ((pwndNext = pwndAct->pwndSibling) == NULL)
        pwndNext = pwndBegin;
    WndActivate (pwndNext);

    if (pwndFullSave != NULL) {
        WndFullScreen (pwndNext);
        DrawDebugScr ();
    }
} /* ViewNext */

/*******************************************************************
* void WndGrow ()
* Purpose:
*  Called when Ctrl+ is pressed to grow active window.
*  Moves active window's bottom line down 1 line if possible.
*  If no room at bottom, moves active window's top line up 1 line if possible.
*
*******************************************************************/
void NEAR WndGrow ()
{
    REG1 PWND pwnd;
    REG2 PWND pwndSteal;

    pwndSteal = PwndStealBelow (pwnd = pwndAct->pwndSibling);

    /* Only steal from command window as last resort */
    if (pwndSteal == &wndCmd && PwndStealAbove (pwndAct) != NULL) {
        pwndSteal = NULL;
    }

    /* there is a window below the active window - grow from bottom */
    if (pwndSteal != NULL) {
        DragCaption (pwnd, pwnd->arcClipping.ayTop);
    }
    /* there is no window below the active window - grow from top */
    else if (pwndAct != pwndBegin) {
        DragCaption (pwndAct, pwndAct->arcClipping.ayTop-2);
    }
}

/*******************************************************************
* void NEAR WndShrink ()
* Purpose:
*  Called when Ctrl- is pressed to shrink active window.
*  Moves active window's bottom line up 1 line if possible.
*  If no room at top, moves active window's top line down 1 line if possible.
*
*******************************************************************/
void NEAR WndShrink ()
{
    REG1 PWND pwndGive;

    WndNoFullScreen ();
    if (DayPwnd(pwndAct) > 0) {  //[43] [41]
        /* we have an active window */
        pwndGive = pwndAct->pwndSibling;

        /* Only give space to command window as last resort */
        if (pwndGive == &wndCmd && PwndGiveAbove (pwndAct) != NULL) {
            pwndGive = NULL;
        }

        /* there is a window below the active window */
        if (pwndGive != NULL) {
            DragCaption (pwndGive, pwndGive->arcClipping.ayTop-2);
        }
        else if (pwndAct != pwndBegin) {
            DragCaption (pwndAct, pwndAct->arcClipping.ayTop);
        }
    }
}


WORD FAR
StatusWndProc (pwnd, msg, wParam, lParam)
PWND pwnd;
WORD msg, wParam;
DWORD lParam;
{
    REG1 ushort hStatusButtonNew = NULL;     //[47]
    static ushort hStatusButtonStart = NULL;    //[47]
    ushort mouseX = (ushort) LOBYTE(LOWORD(lParam));  //[47]

    Unreferenced (pwnd);

    switch (msg) {
        case WM_PAINT:
            DrawStatusLine ();
            break;

        case WM_LBUTTONDBLCLK:  // [48]
        case WM_LBUTTONDOWN: // [10]
            /***** Begin revision [47] *****/
            hStatusButtonStart = HStatusButton(mouseX);
            if (hStatusButtonStart != NULL) {
                hStatusButtonNew = hStatusButtonStart;
                SetCapture(pwnd);
            }
            goto MouseCommon;

        case WM_MOUSEMOVE:
            if (hStatusButtonStart != NULL) {
                if ((wParam & MK_NONCLIENT) == 0) {
                    hStatusButtonNew = HStatusButton(mouseX);
                    if (hStatusButtonNew != hStatusButtonStart)
                        hStatusButtonNew = NULL;
                }
            }
            goto MouseCommon;

        case WM_LBUTTONUP:
            if (hStatusButtonStart != NULL) {
                ReleaseCapture();
                hStatusButtonStart = NULL;
            }
            if (hStatusButtonCur != NULL) {
                DoStatusButton(hStatusButtonCur);
            }
        MouseCommon:
            if (hStatusButtonNew != hStatusButtonCur) {
                hStatusButtonCur = hStatusButtonNew;
                DrawStatusLine();
            }
            /***** End revision [47] *****/
            break;

    } // switch (msg)

    return (0);
}

WORD FAR
DebugWndProc (pwnd, msg, wParam, lParam)
PWND pwnd;
WORD msg, wParam;
DWORD lParam;
{
    RRC rrc;

    Unreferenced (pwnd);
    Unreferenced (wParam);
    Unreferenced (lParam);

    if (msg == WM_PAINT) {
        GetClientRrc (&wndDebug, &rrc);
        FillRrc (&wndDebug, &rrc, ' ', isaWatchWindow);
        DrawWatch ();
    }
    return (0);
}


DWORD FAR
EditFilterWndProc (pwnd, msg, wParam, lParam)
PWND pwnd;
WORD msg, wParam;
DWORD lParam;
{
    switch (msg) {
        case WM_LBUTTONDOWN:
            WndActivate (pwnd);
            break;
    }

    return ((pwnd->fEnabled) ? EditWndProc (pwnd, msg, wParam, lParam) : 0L);
}


/*******************************************************************
* void NEAR DoStatusMsg ()
*
* Purpose:
*    Added with revision [10].
*
*    Called when to display the proper status line message.
*
*    if !FV_NEWHELP, there is only one status line message, so we
*    just do StatusLineMsg(0) to display the default.
*
* Entry:
*    pwnd = current window
*
* Exit:
*    None
*
*******************************************************************/
VOID NEAR DoStatusMsg (pwnd)
PWND pwnd;
{
    ushort StatusMsg;

    if (pwnd == &wndHelp)     // determine the proper message
        if (cmdSwitches & CMD_SW_QHELP)
            StatusMsg = MSG_StatusQHhelp;
        else if (cmdSwitches & CMD_SW_ED)   //[74]
            StatusMsg = MSG_StatusQHelp;        //[74]
        else                                    //[74]
            StatusMsg = MSG_StatusHelp;
    else if (pwnd == &wndCmd) {
        DbAssert(!(cmdSwitches & CMD_SW_ED));
        StatusMsg = MSG_StatusImmediate;
    }
    else if (fCanContUI()) {
        DbAssert(!(cmdSwitches & CMD_SW_ED));
        StatusMsg = MSG_StatusRunning;
    }
    else
        if (cmdSwitches & CMD_SW_QHELP)
            StatusMsg = MSG_StatusQHStart;
        else if (cmdSwitches & CMD_SW_ED)   //[74]
            StatusMsg = MSG_StatusQEdit;        //[74]
        else                                    //[74]
            StatusMsg = MSG_StatusEdit;

    StatusLineMsg (StatusMsg);      // display the right status line
}

/***
*DoCaption (msg, ax, ay) - Handle mouse messages for caption bar
*Purpose:
*  This function is called from MainWndProc when a mouse click or
*  movement is detected in the main window.
*
*  Depending on the message received the function may maximize or
*  minimize the window, or drag the caption bar.
*
*Entry:
*  WORD msg:   mouse message
*  AX ax:      x position of mouse
*  AY ay:      y position of mouse
*
*Exit:
*  none
*
*******************************************************************************/
void NEAR
DoCaption (msg, ax, ay)
WORD msg;
AX ax;
AY ay;
{
    PWND pwnd = NULL;
    /* [24] fMakeWinFullScreen should be FALSE until WM_LBUTTONDBLCLK
       or maximize icon is clicked */
    static bool fMakeWinFullScreen = FALSE;
    static PWND pwndTrack = NULL;

    DbAssert (pwndTop != NULL);
    // [26] Find caption message is in, leave NULL if not in caption

    for (pwnd = pwndBegin; pwnd != NULL; pwnd = pwnd->pwndSibling) {
        if (ay == pwnd->arcWindow.ayTop-1) {
            break;
        }
    }
    // [26] If we aren't in a caption or tracking, do nothing.
    if (pwnd == NULL && pwndTrack == NULL)
        return;

    switch (msg) {
        case WM_LBUTTONDBLCLK:
            /* make window full screen when button is released */
            fMakeWinFullScreen = TRUE;

        case WM_LBUTTONDOWN:
            if (!pwndTrack) {         // [61]
                SetCapture (&wndMain);
                pwndTrack = pwnd;
                WndActivate (pwnd);     // [26] Activate window
            }
            break;

        case WM_MOUSEMOVE:
            if (pwndTrack)
                DragCaption (pwndTrack, ay);
            break;

        case WM_LBUTTONUP:
            // [26] If in caption (pwnd != NULL) and the click is in
            // [26] the maximize location, or we had a double click, then
            // [26] [74] we must maximize (or minimize if already).
            if ((pwnd != NULL) && !(cmdSwitches & CMD_SW_ED) && (ax == 76 || fMakeWinFullScreen)) {
                if (pwndFull == NULL)
                    WndFullScreen (pwndTrack);
            else
                WndNoFullScreen ();
            DrawDebugScr ();
            }
            /* caption bar was dragged and released */
            else if (pwndTrack) {  // [58] May be second LBUTTONUP.
                DragCaption (pwndTrack, ay);
            }

            fMakeWinFullScreen = FALSE;
            ReleaseCapture ();
            pwndTrack = NULL;
            break;
    }
}


/*------------------------------------------------------------------
 *         Window Drawing Functions
 *----------------------------------------------------------------*/

/*******************************************************************
* void DrawDebugScr()
* Purpose:
*  Set flag so we remember to call DoDrawDebugScr before getting
*  the next event (key/mouse) from the user.
*  Tell's outer-loop to call DoDrawDebugScr().
*  eliminates redundant calls which make screen flicker needlessly.
*  caller of DrawDebugScr() need not worry if DebugScr is currently visible.
*
* DoDrawDebugScr returns non-zero if screen was updated.
*
*******************************************************************/
void NEAR DrawDebugScr ()
{
    fDrawDebugScr = TRUE;   /* remember to call DoDrawDebugScr */
}

bool NEAR DoDrawDebugScr ()
{
    bool result;
    ushort oRsSave;

    if (HelpFlags & HLP_COMPRESS) {       //[45]
        WndHelpClose ();           //[45]
        HelpFlags &= ~HLP_COMPRESS;         //[45]
    }                   //[45]

    if (!fDebugScr)
        return FALSE;

    oRsSave = grs.oRsCur;

    /* [3] We must resize this window here now, before we add scroll bars */
    /* [3] as window resizing will only work then */
    /* make cWatch (maybe 0) lines visible at top of screen */
    WatchInfoUI();   /* make sure cWatch is up to date */
    if (cWatch != DayPwnd (&wndDebug)) {
        WndDebugOpen ((AY) cWatch);
        DbAssert (cWatch == DayPwnd (&wndDebug));
    }

    if (rsNew != UNDEFINED) {
        /* We are either initializing, or the user has executed a NEW/LOAD
         * command, which means we need to show rsNew in a list window.
         */
        WndReset (rsNew);
        rsNew = UNDEFINED;
    }

    if (emFlags & EMF_IN_EDITMGR) {
        /* edit mgr is not reenterant.   If it is active, defer drawing
         * debug screen until edit mgr can respond to our requests.
         */
        return FALSE;
    }

    /* load mrsCur, prsCur with active window's info */
    UiRsActivateWnd ();
    if (
        (pwndAct != &wndHelp) &&         // [61]
        (txdCur.lnCursor == UNDEFINED || pwndAct->style & UIWS_MOVECURSOR)) {
        // [42] load mrsCur, prsCur with active window's info,
        // [42] position cursor where it was last time text was visible,
        // [42] or at 0,0 if txdCur.lnCursor == UNDEFINED
            pwndAct->style &= ~UIWS_MOVECURSOR;
            MoveTxdCursor ();
    }
    if (fAdjustCursor) {   //[52]
        fAdjustCursor = FALSE;
        MoveCursorPwndCur (GetEditLine (), GetEditColumn ());
    }


    if (result = fDrawDebugScr) {
        DrawWindow (NULL);
    }
    FEnableMenuBar (TRUE); // [27]

    fDrawDebugScr = FALSE;
    UiRsActivate (oRsSave);
    return (result);
}

/*******************************************************************
* void DrawDebugScrFar()
* Purpose:
*  Same as DrawDebugScr, above, but far rather than near.
*
*******************************************************************/
void FAR DrawDebugScrFar ()
{
    DrawDebugScr();
}

void FAR DoDrawDebugScrFar ()
{
    DoDrawDebugScr();
}

void FAR ScreenRedraw ()   // [56]
{
    extern BYTE emFlags;      // [70]

    if (emFlags && fWndSplit) {  // [70]
        DrawDebugScr ();     // [70]
    }             // [70]
    else {           // [70]
        fDrawDebugScr = FALSE;
        EditMgrFlush1 ();    // [65]
        DrawWindow (&wndMain);
        DrawWindow (&wndStatus);   // [67]
    }
}

static AY ayPutCh;
static AX axPutCh;
/*******************************************************************
* void PutChSetup(ax, ay)
* Purpose:
*  Cause the next call to PutCh() to output its char in the specified
*  column and line of the specified window.
*  This is just a code-size optimization for replacing lots of calls to
*     CharOut (&wndMain, rx, ry, ch, di)
*
*******************************************************************/
void NEAR PutChSetup(ax, ay)
AX ax;
AY ay;
{
    axPutCh = ax;
    ayPutCh = ay;
}

ISA isaOutline;

/*******************************************************************
* void PutCh (ch)
* Purpose:
*  Output the specified char to the window and location indicated by
*  the last call to PutChSetup.  Advance the current column afterward.
*  This is just a code-size optimization for replacing lots of calls to
*     CharOut (&wndMain, rx, ry, ch, di)
*
*******************************************************************/
void NEAR PutCh (ch)
char ch;
{
    CharOut (NULL, axPutCh++, ayPutCh, ch, isaOutline);
} /* PutCh */

/*******************************************************************
* void PutCbPb(cb, pb)
* Purpose:
*  Output the specified string to the window and location indicated by
*  the last call to PutChSetup.  Advance the current column afterward.
*
*******************************************************************/
void NEAR PutChCbPb (cch, pch)
WORD cch;
char *pch;
{
    TextOut (NULL, axPutCh, ayPutCh, pch, cch, isaOutline);
    axPutCh += (AX) cch;
} /* PutChCbPb */

/*******************************************************************
* void DrawPwndOutline(pwnd)
* Purpose:
*  Draw a particular window's caption (title) bar, left & right borders.
*
*******************************************************************/
STATICF(void) DrawPwndOutline(pwnd)
PWND pwnd;
{
    REG1 ushort cChLeft;
    ushort cChRight, cbName;
    AX ay, ayBottom;
    boolean fTopWnd = (pwnd == pwndBegin);
    struct ef *pef = (struct ef *) pwnd->pefExtra; // [60]

    if (pwnd == NULL)
        return;

    isaOutline = (ISA) pef->attrCur;      // [18]
    /* prepare to draw title line 1 line above top of window */
    PutChSetup (0, pwnd->arcClipping.ayTop - 1);

    /* copy name of register set to bufStdMsg */
    if (pef->hBuffer == hbufHelp)   /* [7] */
        cbName = (ushort)SendHelpMsg(WM_HELPTITLE,(ushort)(axMac - 12)); //[59]
    else
        cbName = GetRsName (pef->hBuffer, RSN_fFullName, axMac - 12);

    /* center the title */
    cChLeft = (axMac - cbName - 5);

    /* If odd number of extra bytes, extra goes to right of title bar */
    cChRight = (cChLeft + 1) >> 1;
    cChLeft = cChLeft >> 1;

    PutCh (fTopWnd ? chTopLeftCorner1 : chMiddleLeft1);
    while (cChLeft-- > 0)
        PutCh (chTopSide1);

    if (pwnd == pwndAct)
        /* highlight active window by showing its title in reverse video */
        isaOutline = SetInverseIsa (isaOutline);  // [18]

    PutCh(' ');
    PutChCbPb(cbName, bufStdMsg);   /* print window's title */
    PutCh(' ');

    if (pwnd == pwndAct)
        isaOutline = (ISA) pef->attrCur; // [18]

    cChLeft = cChRight - 2;
    if (pwnd == &wndCmd || (cmdSwitches & CMD_SW_ED)) { //[74]
        cChLeft += 4;
        while (--cChLeft > 0)
            PutCh(chTopSide1);
    }
    else {
        while (--cChLeft > 0)
            PutCh(chTopSide1);
        /* Draw MAXIMIZE/RESTORE Icon */
        PutCh(chMiddleRight1);
        isaOutline = SetInverseIsa (isaOutline);  // [18]

        PutCh ((pwnd == pwndFull) ?
               chUpDownArrow /* RESTORE Icon */ :
               chUpArrow /* MAXIMIZE Icon */);
        isaOutline = (ISA) pef->attrCur; // [18]
        PutCh(chMiddleLeft1);
        PutCh(chTopSide1);
    }

    PutCh (fTopWnd ? chTopRightCorner1 : chMiddleRight1);

    /* draw this window's vertical border (left & right )*/
    ayBottom = pwnd->arcClipping.ayBottom;
    if (pef->pwndScrollH)
        ayBottom++;
    for (ay = pwnd->arcClipping.ayTop; ay < ayBottom; ay++) {
        CharOut (NULL, 0, ay, chLeftSide1, isaOutline);
        if (!pef->pwndScrollV || ay == ayBottom-1)
            CharOut (NULL, axMac-1, ay, chRightSide1, isaOutline);
    }
}

/*******************************************************************
* void DrawCaptions() {
* Purpose:
*  Draw all window caption bars.  This gets called when wndMain
*  receives a WM_PAINT message.
*
*******************************************************************/
void NEAR DrawCaptions()
{
    REG1 PWND pwnd;

    if (pwndFull != NULL)
        DrawPwndOutline (pwndFull);
    else

        for (pwnd = pwndBegin; pwnd != NULL; pwnd = pwnd->pwndSibling)
            DrawPwndOutline (pwnd);
}

/**************************************************************************
* GetEditWord()
* Purpose:
*   Gets the word that the cursor in the current edit window.  NOTE: if
*   the current window is not active (i.e. it is 0 lines in size) then
*   EditFilterWndProc will return FALSE, but noone will have put the
*   zero terminator in the buffer.  We will do that here.
*
* Entry:
*  None.
*
* Exit:
*  return number of characters in the word.
*
**************************************************************************/
ushort NEAR GetEditWord(szWord, cbMax)
char *szWord;
ushort cbMax;
{
    *szWord = '\0';                 //[76]
    return  ((WORD) SendMessage (pwndAct, EM_GETWORD, cbMax,
                                 (DWORD) (char far *) szWord));
}

BOOL fCapLock = FALSE, fNumLock = FALSE;

#define  TOGGLES_START  62    /* extra room for "| " */

#define  BUF_LEN     (80-TOGGLES_START)
#define  BUF_COL     (BUF_LEN - 4)
#define  BUF_LINE (BUF_LEN - 10)
#define  BUF_NUMLOCK (BUF_LEN - 12)
#define  BUF_CAPSLOCK   (BUF_LEN - 13)
#define  BUF_WORDSTAR   (BUF_LEN - 16)
#define  BUF_VERTBAR (BUF_LEN - 18)

/**************************************************************************
* DrawTogglesLnCol
* Purpose:
*  Draw the "toggles" (caps lock, num lock, etc.) and current line
*  and column on the right-most portion of the status line if there is
*  enough room.  Re-written with revision [10].
* Entry:
*  ln = line # to display
*  col = column # to display
* Exit:
*  None
*
**************************************************************************/

void NEAR DrawTogglesLnCol (ln, col)
ushort ln;
ushort col;
{
    char buffer[BUF_LEN];
    char editState;
    register ushort i;
    register char *pch;

    if (fDebugScr &&       //[15] only if enough room
      ((cbStatusMsg <= TOGGLES_START)
      || iMsgStatusLine == MSG_StatusHelp
      )) {
        memset(buffer, ' ', BUF_LEN);

        buffer[BUF_VERTBAR] = 0xb3;      //[14] put in "|" separator
        /* set WordStar Ctrl+Q and Ctrl+K indicators */
        if ((editState = GetEditMgrState()) != 0) {
            buffer[BUF_WORDSTAR] = '^';
            buffer[BUF_WORDSTAR+1] = editState + 0x40;
        }

        /* Set Caps-Lock and Num-Lock indicators */
        if (fCapLock)
            buffer[BUF_CAPSLOCK] = 'C';
        if (fNumLock)
            buffer[BUF_NUMLOCK] = 'N';

        if (iMsgStatusLine == MSG_StatusHelp) {      //[20] write buffer NOW
            TextOut (&wndStatus, (RX) 72, (RY) 0, buffer, 7, //[20]
            DiMake (dmNormal, isaStatusLine));    //[20] normal color
            return;             //[20] and quit
        }                 //[20]
        /*
         * internally the line and column are zero based,
         * but we want to display them as one based, so bump them up.
         */
        ln++;
        col++;

        pch = &buffer[BUF_LINE+4];              /* stuff in line */
        i = 5;
        while (i--) {
            *pch-- = (char) ('0' + (ln % 10));
            ln /= 10;
        }

        buffer[BUF_COL-1] = ':';   /* separate with a colon */

        pch = &buffer[BUF_COL+2];  /* stuff in column */
        i = 3;
        while (i--) {
            *pch-- = (char) ('0' + (col % 10));
            col /= 10;
        }

// [21] Use isaStatusLock for lock status text
        TextOut (&wndStatus, (RX) TOGGLES_START, (RY) 0,
                 &buffer[0], BUF_LINE, isaStatusLock);
        TextOut (&wndStatus, (RX) TOGGLES_START + BUF_LINE, (RY) 0,
                 &buffer[BUF_LINE], BUF_LEN, isaStatusLine);
    }
}

/**************************************************************************
* DrawToggles
* Purpose:
*  Called by TWIN whenever the CAPS-LOCK or NUM-LOCK key is pressed.
*  Status line is updated to re-draw indicators.
*
**************************************************************************/
void COW DrawToggles ()
{
    DrawTogglesLnCol (GetEditLine (), GetEditColumn ());
}

/*******************************************************************
* void DrawStatusLine
* Purpose:
*  Update the status line at the bottom of the window, if the
*  status line is enabled.  Called by DoDrawDebugScr and EnsShowDebugScr.
*

 Main: <filename>     Context: Program not running              ^Q CN nnnnn:nnn

*  Column - length - content
*       0 -      1 - blank
*       1 -      6 - "Main: "
*       7 -     15 - <main module's name>
*      22 -      9 - "Context: "
*      31 -     32 - <active context's name> or "Program not running"
*      63 -      1 - blank
*
*      The following are drawn by DrawToggles
*      64 -      2 - ^Q or ^K for WordStar 2 letter sequences
*      66 -  1 - blank
*      67 -      2 - toggles (C and N for CapsLock and NumLock)
*      69 -  1 - blank
*
*      70 -  9 - line:column
*      79 -  1 - blank
*
* Exit:
*  grs.oRsCur is preserved
*
*******************************************************************/
void NEAR DrawStatusLine()
{
register char *pBuf;                // [10]
    char buffer[80];
    ushort oRsSave;


    if (fDebugScr) {
        oRsSave = grs.oRsCur;


        DbAssert (iMsgStatusLine != 0);

        pBuf = buffer;
        *pBuf++ = ' ';          // leave 1 space at start

        if (iMsgStatusLine >= MSG_HelpFileNew) {     //[12]
            if (iMsgStatusLine <= MSG_StatusDialog) {      //[12]
                // prefix message with "F1=Help   "    //[12]
                pBuf = AppendBuf(pBuf, MSG_StatusF1Help); //[12]
                if (iMsgStatusLine < MSG_StatusMenu)   //[14] menu item descr
                    *(pBuf-2) = 0xb3;       //[14] put in "|"
            } else if (iMsgStatusLine <= MSG_StatusHelp)   //[12]
            // prefix message with "<Shift+F1=Help> " //[19]
            pBuf = AppendBuf(pBuf, MSG_StatusShiftF1);   //[19]
        }                    //[12]

        pBuf = AppendBuf(pBuf, iMsgStatusLine);

        cbStatusMsg = pBuf-buffer;
        DbAssert (cbStatusMsg <= 80)     // verify msg not too long
        memset( pBuf, ' ', 80-cbStatusMsg );   // fill rest with spaces


        cbStatusMsg = (cbStatusMsg > TOGGLES_START) ? 80 : TOGGLES_START;

        TextOut (&wndStatus, (RX) 0, (RY) 0, buffer, cbStatusMsg,
                 DiMake (dmNormal, isaStatusLineVal));

        if (hStatusButtonCur != NULL) {
            RX obStart = (RX) ObStatusButton(hStatusButtonCur);

            TextOut(
                    &wndStatus,
                    obStart, (RY) 0,
                    buffer + obStart,
                    CbStatusButton(hStatusButtonCur),
                    DiMake(dmNormal, SetInverseIsa(isaStatusLineVal))
                   );

        }
        DrawToggles ();

        UiRsActivate(oRsSave);
    }
}

/**************************************************************************
* AppendBuf(buffer, iMsg)
* Purpose:
*  Append a message to a a buffer.  Added with revision [10] to save code.
*
* Entry:
*  iMsg = a standard message number (MSG_xxx) from qbimsgs.h
*
* Exit:
*  Returns number of bytes in the message
*
**************************************************************************/
uchar * NEAR AppendBuf(buffer, iMsg)
uchar *buffer;
WORD iMsg;
{
    WORD cbMsg;

    fmemcpy ((char far *)buffer, (char far *)bufStdMsg, (cbMsg = ListStdMsg (iMsg)));  //[51]
    return (buffer+cbMsg);
}

/**************************************************************************
* StatusLineMsg(iMsg)
* Purpose:
*  Display a message on the status line for a menu item on the status line.
*  The message will remain there until the next call to DrawToggles,
*  StatusLineMsg, or DrawStatusLine.
*
* Entry:
*  iMenu = a standard message number (MSG_xxx) from qbimsgs.h, or
*     -MSG_xxx if message is to be shown in intense video,
* Exit:
*  grs.oRsCur is preserved
*
**************************************************************************/
void NEAR StatusLineMsg(iMsg)
short iMsg;
{
    boolean fHighlight = TRUE;   //[73] default= hilited for LQB

    if (fSignonDisplayed) {   //[64] if not the first time

        if ((iMsgStatusLine = iMsg) < 0) {
            iMsgStatusLine = -iMsgStatusLine;
            fHighlight = !fHighlight;    //[14] switch isa's
        }
        isaStatusLineVal = (ISA) (fHighlight ? isaStatusAlert : isaStatusLine); //[14]
    } else        //[64] first call to StatusLineMsg should
        fSignonDisplayed++;  //[64] not change iMsgStatusLine, so Signon
            //[64] message stays visible

    hStatusButtonCur = 0;  //[47]
    DrawStatusLine();
}

void FAR StatusMsgFar(iMsg)
short iMsg;
{
    StatusLineMsg(iMsg);
}

/**************************************************************************
* UpdStatusLn(ln)
* Purpose:
*  Displays a number in the line number field of the status line
*  to indicate progress during ASCII Load, Search, Scan.
*  Since this can be called by scanner, when text tables are in
*  an indeterminate scan state, we don't want to do anything in this
*  function that could cause us to look at pcode (like LnOfOtx, OtxOfLn,
*  DoDrawDebugScr, ListLine, etc.)
*
* Entry:
*  fDebugScr - TRUE if debug screen is visible
*  ln - line numbers to display
*
**************************************************************************/
void FAR UpdStatusLn (ln)
ushort ln;
{
    if (fDebugScr)
        DrawTogglesLnCol (ln, 0);
}
