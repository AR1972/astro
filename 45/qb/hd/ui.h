/***
* File: ui.h - Defines which apply to the QBI User Interface
*
*   Copyright <C> 1987, Microsoft Corporation
*
* NOTE: When making changes to this file, be sure to make equivalent
*   changes to file UI.INC
*
*******************************************************************************/

#undef UI_H
#define UI_H ON            /* to prevent duplicate #include's */

EXTERNAL ushort rsNew INIT(0);

/*
   debugFlags's bits:

   DEBUG_ERROR  - set when a runtime error occurs and is not trapped.
          The runtime error code restores SI to the beginning
          of statement, sets this flag, then re-executes the
          statement, which invokes the debugger.  The error code
          is passed in the same static variable examined by the
          ERR intrinsic function.

   DEBUG_EXEC_CMD - Set when the runtime builds a command in the direct mode
          buffer, and wants it executed.

   DEBUG_STOP   - set when a STOP statement is executed,
          when Ctrl-BREAK is pressed and not trapped,
          or when a breakpoint is reached.

   DEBUG_END    - set by the executors for opEot and opStEnd to indicate
          end-of-program for QB.

          EB specific: when a direct mode statement is executed
          while a previous EB invocation is active, we return from
          TxtExeBreak to use the same frame. DEBUG_END is used
          to get us back.

   DEBUG_WATCHPOINT - set when a Stop-Watch-Expression evaluates to TRUE

   DEBUG_TRACE  - set while tracing statement execution either because
          of TRON, single-step, or procedure-step.

   DEBUG_WATCH  - set when any Watch Expressions are active in the program.

   DEBUG_CANT_CONT - Causes UserInterface() to set grs.otxCONT to
          UNDEFINED the next time it is called.
          This is used by executors, which cannot call CantCont
          because UserInterface sets grs.otxCONT every time
          we enter UserInterface, thus undoing their change.
          Since it sets otxCONT rather than calling CantCont(),
          stack tracing and variable printing are still possible
          from direct mode, just not continuing.
*/

EXTERNAL uchar debugFlags;

#define DEBUG_ERROR 0x01        /* [1] */
#define DEBUG_EXEC_CMD  0x02        /* [1] */
#define DEBUG_STOP  0x04
#define DEBUG_END   0x08
#define DEBUG_WATCHPOINT 0x10
#define DEBUG_TRACE 0x20
#define DEBUG_WATCH 0x40
#define DEBUG_CANT_CONT 0x80


extern bool fDebugScr;  /* non-zero when DEBUG SCREEN is active */
bool fSyntaxCheck;  /* non-zero if syntax errors reported by editor */

/* bit flags to test for command-line switches */
extern ushort cmdSwitches;

//
// a-emoryh - If you change this flag, you must also change it in
//      ..\..\beef\cw\user\meditasm.asm, in addition to changing it in ui.inc.
//      And you'll need to rebuild CW lib.
#define CMD_SW_QHELP    0x0800      /* invoked from QHELP.COM */


#define CMD_SW_EDCOM    0x0400      /*[6] invoked from EDIT.COM */
#define CMD_SW_ED   0x0200      /* in Editor mode */
#define CMD_SW_GLT  0x0100
#define CMD_SW_HAR  0x0040
#define CMD_SW_HIR  0x0020
#define CMD_SW_MBF  0x0010
#define CMD_SW_RUN  0x0008
#define CMD_SW_FLE  0x0004
#define CMD_SW_NOH  0x0002
#define CMD_SW_MNO  0x0001


// a-emoryh - Topic given on command-line gets stored here
extern char szCmdLineTopic[];


#define MIN_EDITLINE    256

#define WATCH_MAX 8
   /* max lines in screen for "Watch Window" */

/* [3] Watch types for CmdWatchAdd and WatchAdd */
#define WT_Watch    0   /* [3] */
#define WT_WatchPoint   1   /* [3] */

void FAR UserInterface(void);
void FAR WatchDeleted(void);
void FAR HistReset(void);
void FAR WnResetMove(ushort);
void far UpdStatusLn(ushort);
short FAR NotSaved(void);
short FAR NotSavedInc(void);
short FAR NotSavedIncSav(void);
boolean FAR AskCantCONT(void);  /* EB_API */
void FAR UiFlushCache(void);
void FAR CmdWatchDelAll(void);
void far CmdViewInclude(bool);
boolean far FileExists(char *);
EXTERNAL bool fTraceOn INIT(FALSE);         /* TRUE if TRACE is active */
EXTERNAL bool fHistOn INIT(FALSE);          /* TRUE if HISTORY is active */
ushort FAR DelFile(char *);
void FAR DiscardHistoryORs(ushort); //[5]


/* Runtime functions invoked by user interface - eliminate when
   we integrate with runtime. */
void FAR PLM InitRuntime();
void FAR PLM OutChar(char);
void FAR PLM OutSz(char *);
void FAR PLM OutSzN(char *);
void FAR PLM OutNl(void);
char FAR PLM Fin();
ushort FAR PLM ListStdMsgFar(ushort);
ushort near PLM ListStdMsg(ushort);

/*
   bdEMScratch and ldEMScratch overlap.
   ldEMScratch is used by the EditMgr of TWIN.
   bdEMScratch is used in prsmain.asm.
*/
extern bd bdEMScratch;

#define CB_bufStdMsg 80    /* size of static structure 'bufStdMsg' */
extern char bufStdMsg[];     /* set by ListStdMsg[Far] */
