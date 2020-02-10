/*** 
*uidec.h - External declarations internal to UI
*
*	Copyright <C> 1985-1988 Microsoft Corporation
*
*Purpose:
*	Provided external declarations for C files in the UI directory.
*	This file is included only by UIINT.H.
*
* NOTE: When making changes to this file, be sure to make equivalent   
*	changes to file UIINT.INC
*
*******************************************************************************/

/* external declarations */

void near CwInit (void);
void near CwReInit (void);
void near CwTerm (void);
void near CwHook (void);
void near CwUnHook (void);
void near WndInit (void);

void near RestoreUserScreen (void);
void near SaveUserScreen (void);
void near TossUserScreen (void);

void near DoCaption (WORD, AX, AY);
void near WndDeactivate(void);
void near WndActivate(PWND);
void near WndOpen(ushort);
void near WndSplit(void);
void near WndClose(void);
void near WatchInfoUI(void);
void near WndHelpOpen(WORD);	//[10]
void near WndHelpClose(void);
void near WndReset(ushort);
void near WndSaveRestoreCursors(bool);
void near WndAssign(void);
void near WndAssignList(void);
void near WndAssignNext(boolean);
void near WndGrow(void);
void near WndShrink(void);
void near WndNoFullScreen(void);
void near WndCmdCLnMin(ushort);
void near MoveCursorPwndCur(ushort, ushort);
void near MoveCursorPwnd(PWND, ushort, ushort);
void near MoveTxdCursor(void);
void near ViewPrev(void);
void near ViewNext(void);
void near ViewFull(void);
void near ViewProcedure(void);
void near CmdNewProc(char);
VOID NEAR CmdGo(boolean);
VOID NEAR CmdStep(boolean);
VOID NEAR CmdRestart(void);
VOID NEAR CmdToggleBp(void);
VOID NEAR CmdGoUntilHere(void);
VOID NEAR CmdViewSubs(void);
VOID NEAR CmdOptnsDisplay(void);
VOID NEAR CmdOptnsPaths(void);
VOID NEAR ReadQbIni(void);
VOID NEAR WriteQbIni(void);
VOID NEAR CmdHelpKey(void);
VOID NEAR CmdHelpHelp(void);
VOID NEAR CmdHelpSyntax(void);
VOID NEAR CmdHelpClose(void);
VOID NEAR CmdSearchFind(boolean, boolean, boolean);
VOID NEAR CmdSearchChange(void);
VOID NEAR InitHelpMenu(void);
VOID NEAR SetHelpKeyword(char *);
VOID NEAR WatchNameId(ushort);

VOID NEAR CmdFileNew(void);
VOID NEAR CmdFilePrint(void);
VOID NEAR CmdFileOpen(void);
boolean NEAR CmdFileSave(void);
boolean NEAR CmdFileSaveAs(void);
boolean NEAR CmdFileSaveAll(boolean);
VOID NEAR CmdFileLoad(void);
VOID NEAR CmdFileExit(void);
void near CmdSetNextStmt(void);
void near ViewSourceFile(void);
void near WaitForEvent(void);
void near FlushMsgs(void);
void near DrawTogglesLnCol(ushort, ushort);
bool near DoDrawDebugScr(void);
void near DoLoadFile(ushort, bd *);
void near DrawWatch(void);
void near DrawSyntaxHelp(void);
void near DrawDebugScr(void);
void near DrawCaptions(void);
void near DrawStatusLine(void);
void near StatusLineMsg(short);
void near DoStatusMsg(PWND);
void near UiTerminate(void);
void near GetCmd(void);
void near EnsShowOutSaveRs(void);
void near EnsShowDebugScr(void);
void near EnsShowOutputScr(void);
void near CallsMenuInit(void);
void near CallsMenuTerm(void);
void near CmdCalls(ushort);
void near WnActivate(PWND);
void near EditMgrFlush(void);
void near EditMgrFlush1(void);
WORD FAR MainWndProc (PWND, WORD, WORD, DWORD);
void near ExitUserInterface(void);
void near EnterUserInterface(void);
void near UiGrabSpace(void);
void near UiReleaseSpace(void);
ushort near UiAlphaORsBuild(void);
ushort far AlphaOfORsFar(ushort);
void near Search(boolean, boolean, boolean);
void near Replace(void);
void near Run(void);
void far LongToStdMsg(ulong);
void far HexToStdMsg(ushort);
void near ActivateHbuf(ushort);
void near SetUiErr(ushort);
void near SetUiErrOm(void);
void near SetUiErrCond(ushort);			//[18]
void near ReportError(void);
void near SetBookMark(ushort);
void near GotoBookMark(ushort);
void near ReAssignBookMark(ushort, ushort);	//[1]
ushort NEAR UIMessageBox(char *, char *, char *, ushort);	//[14]
ushort near MsgBoxStd(ushort, ushort);
ushort NEAR MsgBoxBd(ushort, bd *);
ushort NEAR MsgBoxStdBd(ushort, ushort, bd *);
ushort NEAR PromptForString(ushort, char far *, ushort);
void NEAR DoDlgGreetingBox(void);
void NEAR DoCmd(char *);

void NEAR UnHookInt24(void);
void NEAR HookInt24(void);
boolean fInt24Err(void);

ushort near SeekFile(ushort, ulong);
ushort FAR FindAndOpenFile(char *, ushort, ushort);
char far * FAR FindFile(char *, ushort);

ushort NEAR GetEditLine(void);
ushort NEAR GetEditColumn(void);
ushort NEAR GetEditWord(char *, ushort);
ushort NEAR GetEditWordMask(char *, ushort, ushort);   //[6]
int NEAR FilterSpecialKeys(MSG *);
boolean NEAR fCodeWnd(void);
ushort NEAR GetSelText(char*, ushort);
void near CutAll(ushort);
void near PasteAll(void);

uchar NEAR DebugError(void);
uchar NEAR DebugStop(void);
uchar NEAR DebugEnd(void);
uchar NEAR DebugTrace(void);
void NEAR ShowStmt(ushort, ushort, ushort );

void NEAR AddExtension (char *, char *);
char *(FAR FileSpec (char *));

extern ushort uierr;
   /* 0 = no error needs to be reported by GetCmd,
      UNDEFINED = report parser built ASCII error message contained in ps.bdErr
      else its a standard qbi error message code.
      Other error information is in text mgr's txtErr structure.
      Reset by ReportError()
   */

extern boolean fGotCmd;
extern boolean NEAR CmdEnter(void);
extern boolean NEAR fCanContUI(void);
extern void NEAR ClrNonStickyBp(void);
extern bd bdAlphaRs;

extern VOID FAR DispatchCmd(void);

/* Called by WndReassign to update the register sets */
extern void NEAR SrchReAssign(ushort, ushort);

extern boolean NEAR ContContext(void);
extern boolean NEAR NeedContContext(void);

extern boolean fDoCmd;
extern bool fMenuInit;

extern boolean fScrollBars;
bool fSyntaxCheck;

ushort near RsMake(sd *, char);
void near UiRsActivate(ushort);
void near UiRsActivateWnd(void);
ushort near GetRsName(ushort, boolean, ushort);
PWND near pwndActive (void);
void near SetActiveFocus(void);
void near RemoveScrollBars(void);
void near AddScrollBars(void);

/* Windows stuff */
extern PWND pwndAct;
extern PWND pwndTop;
extern PWND pwndFull;
extern WND wndCmd;
extern ushort hbufOfCmd;
extern bool fWndSplit;		//[08]

void near GetCurDriveDir(char *);	//[17]
char near GetCurDrive2(void);
void near SetCurDrive2(char);
bool near SetCurDir2( char * );

short near CbSzUi (char near *);	//[11]
void near fstrcpy2 (char far *, char far *);
void near fmemcpy (char far *, char far *, ushort);

void near CowMoo(void);

/* help routines */

extern	ushort curHelpFile;		//[14]

ushort near GetHelpTitleStr(bdl *,ushort); //[2]
ushort near GetHelpContextLen(bdl *);	//[2]
ushort near GetHelpMsgSz(char *, bdl *);  //[2]
void   near KeywordHelp(void);		//[2]
ushort near KeywordHelpLookup(void);	//[2]
ushort near DisplayHlpWndSz(char *);	//[2]
void   near CmdHelpClose(void); 	//[2]
char * (near CreateContext(ushort));	//[2]
void   near GiveHelpOOM(void);		//[2]
ushort near StartHelp(void);		//[2]
void   near DiscardHelpBookMarks(void); //[14]
void   near CloseCurHelpFile(void);	//[14]
long   near SendHelpMsg(ushort, ushort);//[15]
ushort near SelectHotLink(ushort,ushort,boolean); //[3]
void   near CmdHelpPrev(void);		      //[2]
void   near CmdHelpNext(void);		      //[2]
