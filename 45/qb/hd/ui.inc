;***
; File: UI.INC
;
;   Copyright <C> 1987, Microsoft Corporation
;
; NOTE: When making changes to this file, be sure to make equivalent
;   changes to file UI.H.  See UI.H for comments.
;
;
;*******************************************************************************

UI_INC = ON

sBegin  DATA
    extrn   fDebugScr:byte
    extrn   fSyntaxCheck:byte   ;non-zero if syntax checked by editor
    extrn   rsNew:word
    extrn   fTraceOn:byte       ;non-zero if TRACE is active (TRON)
    extrn   debugFlags:byte
ife UIDEBUG_ASM
    extrn   pWatchVal:word
endif   ;ife UIDEBUG_ASM

;--------------------------------------------------------------------------
;  debugFlags's bits:
;
;  DEBUG_ERROR  - set when a runtime error occurs and is not trapped.
;         The runtime error code restores SI to the beginning
;         of statement, sets this flag, then re-executes the
;         statement, which invokes the debugger.  The error code
;         is passed in the same static variable examined by the
;         ERR intrinsic function.
;
;  DEBUG_EXEC_CMD Set when the executor wants the user interface to cause
;         a newly loaded program to be scanned for errors, and if
;         none are found, the any pcode in the direct mode buffer
;         is executed (causing the program to RUN).
;
;  DEBUG_STOP   - set when a STOP statement is executed,
;         when Ctrl-BREAK is pressed and not trapped,
;         or when a breakpoint is reached.
;
;  DEBUG_END    - set by the executors for opEot and opStEnd to indicate
;         end-of-program for QB.
;
;         EB specific: when a direct mode statement is executed
;         while a previous EB invocation is active, we return from
;         TxtExeBreak to use the same frame. DEBUG_END is used
;         to get us back.
;
;  DEBUG_WATCHPOINT - set when a Stop-Watch-Expression evaluates to TRUE
;
;  DEBUG_TRACE  - set while tracing statement execution either because
;         of TRON, single-step, or procedure-step.
;
;  DEBUG_WATCH  - set when any Watch Expressions are active in the program.
;
;  DEBUG_CANT_CONT - Causes UserInterface() to set grs.otxCONT to
;         UNDEFINED the next time it is called.
;         This is used by executors, which cannot call CantCont
;         because UserInterface sets grs.otxCONT every time
;         we enter UserInterface, thus undoing their change.
;         Since it sets otxCONT rather than calling CantCont(),
;         stack tracing and variable printing are still possible
;         from direct mode, just not continuing.
;
;--------------------------------------------------------------------------
    DEBUG_ERROR EQU 01h     
    DEBUG_EXEC_CMD  EQU 02h     
    DEBUG_STOP  EQU 04h
    DEBUG_END   EQU 08h
    DEBUG_WATCHPOINT EQU 10h
    DEBUG_TRACE EQU 20h
    DEBUG_WATCH EQU 40h
    DEBUG_CANT_CONT EQU 80h



            ;     cmdSwitches are eliminated from EB
            ; For now we have to test different file name switches
            ; to determine whether cmdSwitches should be extrn'ed

   ife  INIT_ASM
    extrn   cmdSwitches:word    ;global bit flags for cmd-line switches
   endif    ;ife INIT_ASM



;;
;; a-emoryh - If you change this flag, you must also change it in
;;      ..\..\beef\cw\user\meditasm.asm, in addition to changing it in ui.inc.
;;      And you'll need to rebuild CW lib.
    CMD_SW_QHELP equ 0800h      ;/QHELP switch found


    CMD_SW_EDCOM equ 0400h      ;/EDCOM switch found
        CMD_SW_ED  equ  0200h           ;/Editor switch found
    CMD_SW_GLT equ  0100h       ;/G switch found (CGA speed, snow o.k.)
    CMD_SW_HAR equ  0040H       ;/AH switch found (Huge Arrays allowed)
    CMD_SW_HIR equ  0020H       ;/H switch found (hi-res)
    CMD_SW_MBF equ  0010h       ;/MBF switch found
    CMD_SW_RUN equ  0008h       ;execute given filename after loading
    CMD_SW_FLE equ  0004h       ;user gave a file name to load [& run]
    CMD_SW_NOH equ  0002h       ;/NOHI switch for no highlight colors
    CMD_SW_MNO equ  0001h       ;/B switch found (monochrome monitor)

    WATCH_MAX   EQU 8       ;max lines in screen for "Watch Window"

;   Watch types for CmdWatchAdd and WatchAdd
    WT_Watch    equ 0   
    WT_WatchPoint   equ 1   

    MIN_EDITLINE equ 256        ;min width for edit line

CB_bufStdMsg    EQU 80d     ;size of static structure 'bufStdMsg'
ife UIUTIL_ASM
    extrn   cbStdMsg:word       ;set by ListStdMsgFar
    extrn   bufStdMsg:byte      ;set by ListStdMsgFar
endif   ;UIUTIL_ASM

ife UIEDIT_ASM
    extrn   bdEMScratch:byte
endif

sEnd    DATA

EbAPIEnter MACRO            ;   
    ENDM                
                    
EbAPIExit MACRO             
    ENDM                


ife UIMAIN_ASM
    extrn   UserInterface:far
    extrn   AskCantCONT:far
endif   ;UIMAIN_ASM

    extrn   WnResetMove:far

sBegin  UI

    extrn   FileSpec:far
    extrn   FileExists:far
    extrn   CmdViewInclude:far
    extrn   DelFile:far
    extrn   MakeRelativeFileSpec:far
    extrn   UpdStatusLn:far
    extrn   ShowOutScr:far
    extrn   EnsMouseOff:far
    extrn   TossOutputScreen:far
    extrn   EnsShowDebugScrFar:far
    extrn   StatusMsgFar:far
    extrn   DrawDebugScrFar:far
    extrn   DoDrawDebugScrFar:far

ife UIMAIN_ASM
    extrn   NotSaved:far
    extrn   AskMakeRem:far
    extrn   UiInit:far
    extrn   UiReInit:far
    extrn   UiTerm:far
endif   ;UIMAIN_ASM

if NOT UIEDIT_ASM
    extrn   UiFlushCache:far
    extrn   fEditorActive:far
    extrn   fQhelpActive:far
endif
if NOT UIMAIN_ASM
    extrn   NotSavedInc:far
endif

ife UIUTIL_ASM
    extrn   ListStdMsg:near
    extrn   WnReAssign:far
endif   ;UIUTIL_ASM

ife UIDEBUG_ASM
    extrn   CmdWatchDelAll:far
    extrn   WatchDeleted:far
    extrn   HistReset:far
    extrn   PStepReset:far
    extrn   SetTronTroff:far
    extrn   DebugWatch:far
endif   ;UIDEBUG_ASM

ife UINHELP_ASM
    extrn   DiscardHistoryORs:far   
endif
sEnd    UI

sBegin  LOADRTM
ife UIUTIL_ASM
    extrn   ListStdMsgFar:far
endif   ;UIUTIL_ASM
sEnd    LOADRTM
