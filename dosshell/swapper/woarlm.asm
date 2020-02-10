;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */

     
;----------------------------------------------------------------------------;
; This file has the functions performaed by the real mode routine.           ;
;                                                                            ;
; History:                                                                   ;
;                                                                            ;
;        Fri June-15-1990.      -by-  Amit Chatterjee [amitc]                ;
;        Adapted for the Dos Task Switcher.                                  ;
;                                                                            ;
;        Tue May-08-1990.       -by-  Amit Chatterjee [amitc]                ;
;        Till now if 'fNoSwitch' was set all the hot keys would be disabled  ;
;        and the hot keys would be chained down the INT 9 interrupt. This    ;
;        was not the right behaviour. The hot keys should only be chained    ;
;        down if the hot key is specifically disabled in the PIF. The INT9   ;
;        ISR has now been modified to take care of this. (Fix for bug #2190) ;
;                                                                            ;
;        Tue June-20-1989.      -by-  Amit Chatterjee [amitc]                ;
;        Created for Windows. (Added the History legend)                     ;
;----------------------------------------------------------------------------;

	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	include macros.mac
	include woakeys.inc
	include woaerr.inc
	include woapif.inc
	include pdb.inc
	include njmp.mac
	include woaswch.inc
	include woaswapi.inc
	.list

	.286

;----------------------------------------------------------------------------;
; define the equates to access the users registers off the stack.            ;
;----------------------------------------------------------------------------;

USER_FLAGS      equ     wptr [bp+6]     ;the flag word
USER_FLAGLOW    equ     bptr [bp+6]     ;low byte of flags
USER_CS         equ     wptr [bp+4]     ;users return segment
USER_IP         equ     wptr [bp+2]     ;users return address
USER_BP         equ     wptr [bp]       ;bp saved here
USER_DS         equ     wptr [bp-2]     ;ds saved here
USER_DX         equ     wptr [bp-4]     ;dx saved here
USER_ES         equ     wptr [bp-6]     ;es saved here
USER_BX         equ     wptr [bp-8]     ;bx saved here
USER_AX         equ     wptr [bp-10]    ;ax saved here
USER_AL         equ     bptr [bp-10]    ;al saved here
USER_CX         equ     wptr [bp-12]    ;cx saved here
USER_SI         equ     wptr [bp-14]    ;si saved here
USER_DI         equ     wptr [bp-16]    ;di saved here

;----------------------------------------------------------------------------;
; define any public labels or eqautes here.                                  ;
;----------------------------------------------------------------------------;

		public  WoaPath             
		public  WoaParams           
		public  WoaFcb1             
		public  WoaFcb2             
		public  WoaParamBlock       
		public  WoaStubSize         
		public  WoaPSP          
		public  WoaAppNumber
		public  WoaBehavior
		public  WoaHotkeys
		public  WoaEmsFlag
		public  WoafBreak
		public  WoaStartScreenLines
		public  Woa6fValue
		public  WoaIrq9Global
		public  WoaNetAsyncSwitching
		public  AsyncNetPending
		public  WoaInt15UsershApp
		public  WoahApp
		public  WoafXmsInstalled
		public  WoaGrabberName
		public  WoaSegResizeBlock
		public  WoaSizeReservedArea
		public  WoaCpuType
		public  WoaGrabberSwapPath
		public  WoaSwapDrive
		public  WoaFileEntrySize
		public  RealModeWoa
		public  BackFromContextSwitch
		public  SwitchIfPossible
		public  WaitForKeyFromInt16
		public  WaitForKeyFromDosCalls
		public  DosCall
		public  ActualInt21
		public  ActualInt16
		public  ActualInt09
		public  Int16ISR
		public  Int09ISR
		public  TestWoaInt16InChain
		public  ConsoleRawMode
		public  GrabberParaSize
		public  WOAsInt16
		public  ActualXMS
		public  GetXmsHandler
		public  DoInt28
		public  WoaHotKeyState
		public  WoaNodeToSwitchTo
		public  WoaALtTabDisabled
		public  WoaShftALtTabDisabled
		public  WoaALtEscDisabled
		public  WoaShftALtEscDisabled
		public  WoaCtrlEscDisabled
		public  DisableInt15Mouse
		public  EnableInt15Mouse
		public  SwitcherDisabled
		public  WoaSwitcherID
		public  HInt09ChainAddr
		public  WoaSwapAreaParaSize

;----------------------------------------------------------------------------;
; define some local data structures.                                         ;
;----------------------------------------------------------------------------;

ScanCodeStruc   STRUC

SCS_ScanCode            db      ?       ;main scan code
SCS_ShiftState          db      ?       ;the shift state

ScanCodeStruc   ENDS

HotKeyInfoStruc STRUC

HKIS_State              db      ?       ;enabled if 0, else disabled
HKIS_PrefixScanCode     db      ?       ;a prefix for the scan code
HKIS_Type               db      ?       ;type of switch
HKIS_NodeNum            db      ?       ;logical node number

HotKeyInfoStruc ENDS

; at this moment, the code assumes that the size of the 'HotKeyInfoStruc'
; structure is double the size of 'ScanCodeStruc' structure. There are 
; 'errnzs' to assert this.

;----------------------------------------------------------------------------;




;----------------------------------------------------------------------------;
; define two macros for doing near and far returns and one for swapping words;
;----------------------------------------------------------------------------;

near_ret  macro
	local   dummy
dummy   proc    near
	ret     
dummy   endp
	endm

far_ret macro
	local   dummy
dummy   proc    far
	ret
dummy   endp
	endm

swap_a_word     macro

	mov     ax,[si]                 ;;get word from source
	xchg    es:[di],ax              ;;swap it with destination
	mov     [si],ax                 ;;save word in source
	add     si,2                    ;;next word
	add     di,2                    ;;next word
	endm

;----------------------------------------------------------------------------;

createSeg   _WOARLMSEG,StubSeg,word,public,code
sBegin  StubSeg

	assumes cs,StubSeg
	assumes ds,StubSeg

;----------------------------------------------------------------------------;
; declare external functions here.                                           ;
;----------------------------------------------------------------------------;

	externNP        XenixRead               ;(WOADOS.ASM)
	externNP        ReadConsoleBuffer       ;(WOADOS.ASM)
	externNP        LoadGrabberAndInit      ;(WOAGRB.ASM)
	externNP        GrabberSaveScreen       ;(WOAGRB.ASM)
	externNP        GrabberRestoreScreen    ;(WOAGRB.ASM)
	externNP        GrabberEnableSave       ;(WOAGRB.ASM)
	externNP        GrabberDisableSave      ;(WOAGRB.ASM)
	externNP        GrabberInitScreen       ;(WOAGRB.ASM)
	externNP        SaveWindowsHPState      ;(WOAHP.ASM)
	externNP        RestoreWindowsHPState   ;(WOAHP.ASM)
	externNP        SaveDosAppHPState       ;(WOAHP.ASM)
	externNP        RestoreDosAppHPState    ;(WOAHP.ASM)
	externNP        CheckWithApp            ;(WOAHP.ASM)
	externNP        NotifyApp               ;(WOAHP.ASM)
	externNP        ProcessTsr              ;(WOATSR.ASM)
	externNP        XmsInit                 ;(WOAXMS.ASM)
	externNP        HookOurXMSCode          ;(WOAXMS.ASM)
	externNP        UnHookOurXMSCode        ;(WOAXMS.ASM)
	externNP        SWAPICreateSession      ;(WOASWAPI.ASM)
	externNP        SWAPIResumeSession      ;(WOASWAPI.ASM)
	externNP        SWAPISessionActive      ;(WOASWAPI.ASM)
	externNP        SWAPIDestroySession     ;(WOASWAPI.ASM)
	externNP        OkToSuspend?            ;(WOASWAPI.ASM)
	externNP        SwitchAPICallIn         ;(WOASWAPI.ASM)
	externNP        DoDosWait               ;(WOADOS.ASM)
	externNP        SwapInstanceDataBlocks  ;(WOAINSTD.ASM)
	externNP        InitSwitcherAPI         ;(WOASWAPI.ASM)

ifdef	JAPAN
	externFP	KkcBusyCheck		;(WOAKKC.ASM)
endif

;----------------------------------------------------------------------------;
; declare variables defined in other files.                                  ;
;----------------------------------------------------------------------------;

externB PromptString                            ;(WOAMSG2.INC,WOATSR.ASM)
externB WoaAppUsesXMS                           ;(WOAXMS.ASM)
externW RCB_FileHandle                          ;(WOADOS.ASM)
externD lp3fMode                                ;(WOADOS.ASM)
externB b3fRCB                                  ;(WOADOS.ASM)

ifdef	JAPAN
externW	KkcBufferSize				;(WOAKKC.ASM)
externW	KkcBufferSeg				;(WOAKKC.ASM)
endif

;----------------------------------------------------------------------------;
; define a 256 byte of stack frame for the real mode stack                   ;
;----------------------------------------------------------------------------;

WoaGrabberName          db      80 dup (?)      ;load name of grabber
WoaGrabberSwapPath      db      80 dup (?)      ;grabber swap path
WoaPath                 db      80 dup (?)      ;file to execute
WoaParams               db      130 dup (?)     ;parameters to it
WoaFcb1                 db      32 dup (?)      ;first FCB
WoaFcb2                 db      32 dup (?)      ;second FCB
Woa6fValue              db      ?               ;int 6f to be done or not
WoaIrq9Global           db      ?               ;IRQ 9 to be global or not
WoaNetAsyncSwitching    db      ?               ;Network to be monitored or not
WoaCpuType              dw      ?               ;copy of __WINFLAGS
WoaFileEntrySize        dw      ?               ;size of SFT fcb


;----------------------------------------------------------------------------;
; now define some data variables that will be passed in from protected mode  ;
;                                                                            ;
; ********************************CAUTION**********************************  ;
; THE DEFINITION AND ORDERING OF VARIABLES IN THE FOLLOWING BLOCK MUST BE    ;
; EXACTLY IDENTICAL TO THE DEFINITION AND ORDERING OF VARIABLES IN THE       ;
; CORRESPONDING BLOCK NAMED 'STUBDATA' IN (WOAMGR.ASM)                       ;
; ************************************************************************** ;
;----------------------------------------------------------------------------;

		public  WoaStubData
		public  WoaStubDataLength

WoaStubData     label byte

WoafBreak               db      ?               ;startup state of CTRL+C flag
WoaSwapDrive            db      ?               ;swap drive to use
WoaPSP                  dw      ?               ;WOA PDB seg
WoaEmsFlag              db      ?               ;EMS present or not
WoaInt15UsershApp       dw      ?               ;ID of INT 15 user.
WoaSegResizeBlock       dw      ?               ;block to resize
WoaSizeReservedArea     dw      ?               ;size of reserved area

WoaStubDataLength       equ $ - WoaStubData

;----------------------------------------------------------------------------;
; Define the rest of the variables that are obtained from the main code seg. ;
;----------------------------------------------------------------------------;

WoaStartScreenLines     dw      ?               ;start up no of screen lines
WoahApp                 dw      ?               ;handle of window
WoafXmsInstalled        db      0               ;NZ if XMS code installed
WoaAppNumber            dw      ?               ;serial number of app
WoaParamBlock           dw      7  dup (?)      ;parameters for EXEC
WoaBehavior             db      ?               ;the behaviour bits
WoaHotkeys              db      ?               ;the hot key disable flags
WoaStubSize             dw      ?               ;stub code size
WoaSwitcherID           db      ?               ;ID of the switcher

;----------------------------------------------------------------------------;
; next have any variables needed by real mode code.                          ;
;----------------------------------------------------------------------------;

SwitcherDisabled        dw      0               ;following reasons to disable

  SD_LOCAL_DISABLE      equ     001h            ;local critical sections
  SD_PMODE_WINDOWS      equ     002h            ;pmode windows being run
; SD_SWAPI_DISABLE      equ     004h            ;defined in WOASWAPI.ASM

HardwareIntCount        db      0               ;number of hw ints being serviced
LocalInt15MouseHandler  db      0               ;mouse handler local or not
Int16GetStatusType      db      01h             ;Int 16h get status call to use
WoaSwapAreaParaSize     dw      ?               ;size of swap area
OriginalDosBlockSize    dw      ?               ;size of LOW heap block
OriginalBlockType       db      ?               ;type of the arena
WoaRlmEntrySS           dw      ?               ;stack seg at entry
WoaRlmEntrySP           dw      ?               ;stck pointer at entry
WoaSwitchOutSS          dw      ?               ;value of ss at switch out time
WoaSwitchOutSP          dw      ?               ;value of sp at switch out time
WoaSwitchOutESP         dd      ?               ;value of esp at switch out time
DosAppPSP               dw      ?               ;PSP of the DOS APP.
DosAppDMA               dd      ?               ;DMA address of dos app
MasterPICMask           db      ?               ;value of 21h (master PIC) port
PreExecSLavePICMask     db      ?               ;the preexec value of the mask
GlobalSlavePICMask      db      ?               ;value of slave PIC for windows
AppSlavePICMask         db      ?               ;value of slave PIC for app
AppUsesIRQ9             db      0               ;app uses IRQ 9 or not.
DosAppDisk              db      ?               ;current disk of dos app
DosPreExecDisk          db      ?               ;windows current disk
DosPreExecCD            db      '\'             ;specify the root
			db      79 dup (?)      ;rest of the name
DosAppCD                db      '\'             ;specify the root
			db      79 dup (?)      ;rest of the name
lpInDosFlag             dd      ?               ;long pointer to InDos flag
lpErrModeFlag           dd      ?               ;long pointer to err mode flag
AsyncNetPending         db      0               ;async net request done or not
PendingTime             dw      0               ;num ticks after switch req set
ActualInt1C             dd      ?               ;previous INT 1C ISR
ActualInt09             dd      ?               ;previous INT 09 ISR
ActualInt0A             dd      ?               ;pre-exec INT 0AH
ActualInt21             dd      ?               ;previous INT 21 ISR
ActualInt15             dd      ?               ;previous INT 15 ISR
ActualInt16             dd      ?               ;previous INT 16 ISR
ActualInt24             dd      ?               ;original INT 24 ISR
ActualInt27             dd      ?               ;original INT 27 ISR
ActualInt2A             dd      ?               ;original INT 2A ISR
ActualInt5C             dd      ?               ;original INT 5C ISR
ActualInt71             dd      ?               ;pre-exec INT 71H ISR
OtherActualInt2f        dd      ?               ;original INT 2f ISR
WoaHotkeyState          db      ?               ;state of hot keys
WoaNodeToSwitchTo       db      ?               ;node # to switch to
lpGlobalSwtchStr        dd      ?               ;pointer to global switch str.
LastScanCode            db      0               ;last scan code read
TsrActive               db      0               ;TSR activated or not
TestWoaInt16InChain     db      0               ;are we in INT 16 chain ?


SwitchInProgress        db      0               ;switch in progress or not

  SIP_SWITCHING         equ     01h             ;actually switching

WOAsInt16               db      0               ;INT 16 call from WOA code
PopRet                  dw      ?               ;variable to pop ret address
lpDosData               dd      ?               ;lptr to DOS data block
ConsoleRawMode          db      0               ;console in raw mode or not.
OriginalInt15MemSize    dw      ?               ;original int 15 memory size
OurInt15OffChain        db      ?               ;can we reach INT 15 isr ?
AppEmsSaveArea          db      256 dup (?)     ;EMS save area
WinIoctlData            db      5 dup (?)       ;win IOCTL data for shrd handles
AppIoctlData            db      5 dup (?)       ;app IOCTL data for shrd handles
WinPrinterPorts         dw      3 dup (?)       ;win printer port addresses
AppPrinterPorts         dw      3 dup (?)       ;app printer port addresses 
DevHeaderChain          db      26 dup (0)      ;device header pointers
GrabberParaSize         dw      ?               ;grabber context para size
PrtScInProgress         db      0               ;print screen in progress or not
TempStack               dw      ?               ;save temp stack segment
lpXmsEntry              dd      ?               ;entry to xms initializer
XMsFlag                 db      0               ;XMS installed or not.
ActualXms               dd      ?               ;HIMEM entry point
DosAllocationStrategy   db      ?               ;allocation strategy for app
DosLinkState            db      ?               ;link state
AppsLogicalNodeNum      db      0               ;node number of app
Woa4BSP                 dw      ?               ;saved value of SP of PSP
Woa4BSS                 dw      ?               ;saved value of SS of PSP

;----------------------------------------------------------------------------;
; reserve space for saving some old app state related variables.             ;
;----------------------------------------------------------------------------;

Buf8087                 db      98 dup (?)      ;8087 state saved here
Speaker                 db      0               ;speaker value,init to 0
ControlCFlag            db      ?               ;state of control c flag

;----------------------------------------------------------------------------;
; Miscellaneous flags.                                                       ;
;----------------------------------------------------------------------------;

MiscFlags               dw      0               ;random flags defined below

	TIMER_HOOKED    equ     1h              ;timer has been hooked.

;----------------------------------------------------------------------------;
; define the HOT KEY related structures and definitions.                     ;
;----------------------------------------------------------------------------;


MAX_FIXED_HOT_KEYS      equ     5               ;no of predefined hot keys.

		EVEN
ScanCodeTable   label word

		db      0fh             ;scan code for TAB (ALT-TAB)
		db      00001000b       ;shift state for ALT
		db      01h             ;scan code for ESC (ALT-ESC)
		db      00001000b       ;shift state for ALT
		db      01h             ;scan code for ESC (CTRL-ESC)
		db      00000100b       ;shift state for CTRL
		db      0fh             ;scan code for TAB (SHIFT-ALT-TAB)
		db      00001011b       ;shift state for ALT+SHIFT
		db      01h             ;scan code for ESC (SHIFT-ALT-ESC)
		db      00001011b       ;shift state for ALT+SHIFT

; now have the app specific scan code table

AppsScanCodeTable  db   MAX_NUM_PROGRAMS * SIZE ScanCodeStruc dup (0)

		EVEN

HotKeyInfoTable label byte

 WoaAltTabDisabled      db      0               ;ALT+TAB not disabled
			db      0               ;the extra scan code (none)
			db      WOA_ALT_TAB     ;type of switch.
			db      -1              ;logical node number

  WoaAltEscDisabled     db      0               ;ALT+ESC not disabled
			db      0               ;the extra scan code (none)
			db      WOA_ALT_ESC     ;type of switch.
			db      -1              ;logical node number

  WoaCtrlEscDisabled    db      0               ;ALT+ESC not disabled
			db      0               ;the extra scan code (none)
			db      WOA_CTRL_ESC    ;type of switch.
			db      -1              ;logical node number

  WoaShftALtTabDisabled db      0               ;SHIFT+ALT+TAB not disabled
			db      0               ;the extra scan code (none)
			db      WOA_SHIFT_ALT_TAB;type of switch.
			db      -1              ;logical node number

  WoaShftAltEscDisabled db      0               ;ALT+ESC not disabled
			db      0               ;the extra scan code (none)
			db      WOA_SHIFT_ALT_ESC;type of switch.
			db      -1              ;logical node number

; now have the app specific hot key info table

AppsHotKeyInfoTable  db MAX_NUM_PROGRAMS * SIZE HotKeyInfoStruc dup (0)

;----------------------------------------------------------------------------;
; define a 400 byte of stack frame for the real mode stack which will be     ;
; used while switching out                                                   ;
;----------------------------------------------------------------------------;

			db      400 dup (99h)   ;switch out stack

WoaSwitchOutStackTop    dw      99h             ;the top of temp stack

;----------------------------------------------------------------------------;
; define another 128 byte stack on which we will do the EXEC call to start the
; app. Some INT 21H handler may trap the 4BH call and chain down by calling  ;
; the previous handler. In this case they would have valid data on the stack ;
; that would be needed when the app exits.                                   ;
;----------------------------------------------------------------------------;

			db      128 dup (55h)   ;exec stack

WoaExecStackTop         dw      55h             ;top of exec time stack

;----------------------------------------------------------------------------;

WoaAppsSS               dw      ?               ;app's SS
WoaAppsSP               dw      ?               ;app's SP
WoaAppsESP              dd      ?               ;app's ESP

;----------------------------------------------------------------------------;
; define external constants.                                                 ;
;----------------------------------------------------------------------------;

externA PromptStringLength                      ;(WOAMSG2.INC,WOATSR.ASM)

;----------------------------------------------------------------------------;
; define equates for some hardware ports.                                    ;
;----------------------------------------------------------------------------;

timer0                  equ     40H             ;Timer port 0
SpeakerCtl              equ     61H             ; Speaker port
CoProcPort              equ    0F0H             ; 8087/80287 ports
mskreg                  equ     21H             ; IRQ int mask register
BEEP_ON                 equ     03h             ; beeping...

;----------------------------------------------------------------------------;
; define other needed constants.                                             ;
;----------------------------------------------------------------------------;

TIMEOUT_TICK_COUNT      equ     18*2+1          ;2 seconds timeout value


IS_WINOLDAP_ACTIVE      equ     4680h           ;woa's INT 2f hook
WIN_P_START_UP          equ     1605h           ;PMODE Windows start up
WIN_P_EXIT              equ     1606h           ;PMODE Windows exits
WOA_MIN_FILEHANDLES     equ     2               ;min free handles for WOA

; Define IBM PC port locations for the Intel 8243 Programmable Interval Timer
; chip

PIT_PORTA       =       040h
PIT_PORTB       =       041h
PIT_PORTC       =       042h
PIT_PORTD       =       043h

; Define IBM PC port locations for the Intel 8255 Programmable Peripheral
; Interface chip

PPI_PORTA       =       060h
PPI_PORTB       =       061h
PPI_PORTC       =       062h
PPI_PORTD       =       063h

; Define timimgs and divisors to make sounds via the PIT, PPI chips and the
; PC's speaker circuitry.

BEEP_TIME1      =       02400h
BEEP_TIME2      =       03400h
BEEP_TONE1      =       00533h
BEEP_TONE2      =       00633h


WF_CPU086088    =    0000h
WF_CPU186       =    0001h
WF_CPU286       =    0002h
WF_CPU386       =    0004h
WF_CPU486ORABV  =    0008h

;----------------------------------------------------------------------------;
; we need to save about 50 bytes from 0:4f0 for comm related stuff. Also,    ;
; Basic uses some of these areas to save some stuff, but then we will not be ;
; able to switch out from basic any way.                                     ;
;----------------------------------------------------------------------------;

CommSaveArea            db      50 dup (?)

;----------------------------------------------------------------------------;
; define the dispatch table for the INT 21 calls that we trap.               ;
;----------------------------------------------------------------------------;

DosTraps        label byte

		db      01h             ;conin with echo
		dw      DosFuncGetKey   ;routine which handles it
		db      07h             ;unfiltered input with echo
		dw      DosFuncGetKey   ;routine which handles it
		db      08h             ;conin without echo
		dw      DosFuncGetKey   ;routine which handles it
		db      0ah             ;buffered-keyboard-input
		dw      DosFunc0A       ;routine which handles that
		db      0ch             ;flush buffer, read keyboard
		dw      DosFunc0C       ;routine which handles it
		db      31h             ;terminate and stay resident
		dw      DosFunc31       ;routine which handles it
		db      3fh             ;read file or device
		dw      DosFunc3f       ;routine which handles it
		db      44h             ;IOCTL function
		dw      DosFunc44       ;routine which handles it
DosDefaultCode  db      ?               ;put AH here on entry to trap default
		dw      DosDefaultFunc  ;pass on to original handler
			
;----------------------------------------------------------------------------;

RealModeWoa proc far

	smov    ds,cs                   ;set up proper data segment

;----------------------------------------------------------------------------;
; save the current values of stack segment and stack pointer so that we can  ;
; return back to protected mode from one of the interrupt handlers in case of;
; a context switch.                                                          ;
;----------------------------------------------------------------------------;

	mov     WoaRlmEntrySS,ss        ;save stack segment
	mov     WoaRlmEntrySP,sp        ;save stack pointer

;----------------------------------------------------------------------------;
; Get the amount of extende memory available via the INT 15 call             ;
;----------------------------------------------------------------------------;

	mov     ah,88h                  ;get extended memory size call ?
	int     15h                     ;ax returns the available size
	mov     OriginalInt15MemSize,ax ;save it.

;----------------------------------------------------------------------------;
; First try to load in grabber and initialize it also reserving and marking  ;
; the loaction of the buffer needed for saving the screen.                   ;
;----------------------------------------------------------------------------;

	cCall   LoadGrabberAndInit      ;load and initialize grabber
	jnc     @f                      ;loading went off smoothly

; the grabber load has failed. Retuen back to real mode with proper error
; code.

	mov     ah,ER_GRABBER_LOAD      ;grabber load failure
	stc                             ;error indication
	ret                             ;go back to protected mode

@@:

; get and save a pointer to the global switch structure.

	mov     ax,4a05h                ;INT 2F code for the shell interface
	mov     si,CGET_GLOBAL_SWITCH_DATA
	int     2fh                     ;dx:ax has the long pointer
	mov     wptr [lpGlobalSwtchStr],ax;save offset
	mov     wptr [lpGlobalSwtchStr+2],dx;save segment

; prepare the table of hotkey information.

	call    PrepareAppHotKeyList

;----------------------------------------------------------------------------;
; We will now shrink the block size to just hold the stubs.                  ;
;                                                                            ;
; Also we must mark this block as the last dos block, so that dos does not   ;
; allocate any memory past the end of the block.                             ;
;----------------------------------------------------------------------------;

	mov     ax,WoaSegResizeBlock    ;segment of block to resize

; first we must save the size.
	
	dec     ax                      ;point to DOS arena segment
	mov     es,ax
	mov     ax,wptr es:[3]          ;get areana size in paras
	mov     OriginalDosBlockSize,ax ;save it

; at this point calculate the size of the swap area for the TestMemoryRegion
; switcher api call. The swapping starts at the current CS and the size in
; paras is:
;
;       OriginalDosBlockSize - (CS - WoaSegResizeBlock)

	mov     bx,cs                   ;get our CS
	sub     ax,bx                   ;based on above formula
	add     ax,WoaSegResizeBlock    ;total para size of swap area from CS:0
	mov     WoaSwapAreaParaSize,ax  ;save it

; also retrive the arena type and mark it as the last one

	mov     al,es:[0]               ;exchange with the current type
	mov     OriginalBlockType,al    ;save the actual arena type

	mov     es,WoaSegResizeBlock    ;want to resize the block
	mov     bx,WoaSizeReservedArea  ;size of any reserved area
	add     bx,GrabberParaSize      ;add in the size of the grabber context

;----------------------------------------------------------------------------;
; add WoaAppNumber to this value, so that the memory locked by the stub will ;
; be progressively more ensuring that none of the oldapps are given the same ;
; PSP by DOS                                                                 ;
;----------------------------------------------------------------------------;

	add     bx,WoaAppNumber         ;extra para, 2nd app onwards

ifdef	JAPAN
	push	cx			;
	mov	cx,KkcBufferSize	;
	or	cx,cx			; not installed KKC ?
	jz	@f			; yes

	mov	ax,WoaSegResizeBlock	;
	add	ax,bx			;
	mov	KkcBufferSeg,ax		; save buffer segment
	add	bx,cx			; add buffer size using to save KKC state
@@:
	pop	cx
endif

;----------------------------------------------------------------------------;

	mov     ax,4a00h                ;resize function
	int     21h                     ;area released for WOA

; initialize our Switcher API handler module.

	cCall   InitSwitcherAPI

; get and save long pointers to the InDos and ErrorMode flags.

	call    GetDosFlags             ;get the flag variables

; now let grabber hook INT 10 if it is loaded

	cCall   GrabberEnableSave       ;grabber hooks int 10

; hook all the vectors that WOA wants to trap

	call    HookIntVectors          ;hooks relevant vectors
	
; now set the mode to a known one and the requested no of lines per screen.

	mov     ax,WoaStartScreenLines  ;start up no of lines

; if the no of lines is not 25,43 or 50 validate it.

	call    PruneNumLines           ;get a valid value
	mov     WoaStartScreenLines,ax  ;save the one num we will use
	cCall   GrabberInitScreen       ;initialize the display

; if HIMEM is installed, then get the address of the XMS entry point. This 
; will be needed to set the correct state of the A20 line.

	call    GetXmsHandler           ;saves the call address

; if XMS handling is to be done, call the XMS initializer

	call    StartXms                ;initialize if needed

; set up ES:BX to point to the parameter block

	mov     bx,StubSegOFFSET WoaParamBlock
	smov    es,ds                   ;ES:BX points to parameter block

; set the environment segment in the parm block from the one in the heap 
; owners pdb.

	push    es                      ;save
	mov     es,WoaPSP               ;get the owners PDB
	mov     ax,es:[2ch]             ;get the environment segment
	pop     es                      ;es:bx  points to param block
	mov     es:[bx],ax              ;save the environment segment

; now we need to save various dos or bios related state befor we start the
; old app so that we can restore these states when we switch back to windows

	call    SaveWinDosBiosStates    ;save various states

; initialize the 'TsrActive' flag, it will be set only if a TSR is activated

	mov     cs:[TsrActive],0        ;is there a tsr to handle ?

; before invoking the Old App, restore the state of the HP system

	push    es                      ;save 
	cCall   SaveWindowsHPState      ;must be restored before going back
	popem   es                      ;restore 

; clear out pending CTRL+Cs, if any.

	mov     ax,0c00h                ;flush keyboard
	int     21h

; reset the CTRL+C flag state now.

	mov     ax,3301h                ;set CTRL+C flag
	mov     dl,WoafBreak            ;get the startup flag
	int     21h                     ;set the start up state

;----------------------------------------------------------------------------;

; do the Create_Session switch API call.

	cCall   SWAPICreateSession
	jz      CreateSessionSucceeded  ;ok to go ahead

; the create session call failed. Exit with an appropriate error code.

	stc                             ;EXEC fails
	mov     ax,ER_SWAPI_CREATE_FAILS;error subcode 
	jmp     short ExecReturns       ;skip over the EXEC call

CreateSessionSucceeded:

; do a resume session call.

	cCall   SWAPIResumeSession

; do a sesssion active call.

	cCall   SWAPISessionActive
;----------------------------------------------------------------------------;

; point DS:DX to the file to execute

	mov     bx,StubSegOFFSET WoaParamBlock
	mov     dx,StubSegOFFSET WoaPath;DS:DX points to file to execute

; switch to a temporary stack before execing.

	cli                             ;mask off interrupts
	smov    ss,cs                   ;stack is in our segment
	mov     sp,StubSegOFFSET WoaExecStackTop
	sti                             ;restore interrupts.
	mov     ax,4b00h                ;exec code
	int     21h                     ;pass control to child.

ExecReturns:

; make sure DS is same as CS at this point

	smov    ds,cs                   ;ds has stub segment

; the stack might have changed, so reload it

	mov     ss,WoaRlmEntrySS        ;get the entry point stack
	mov     sp,WoaRlmEntrySP        ;get the entry point stack pointer

; a terminate and stay resident call might have been made, if so handle it
; (we basically wait in a loop till the user actually exits the TSR)

	pushf                           ;push the flag before comparision
	push    ax                      ;save 4b return code

;----------------------------------------------------------------------------;
; make a DestroySession Switch API call.

	cCall   SWAPIDestroySession
;----------------------------------------------------------------------------;

; restore the state of the A20 line if the application had turned it off (done
; only if HIMEM is installed.

	call    SetCorrectA20State      ;sets A20 back on if HIMEM installed

; turn the CTRL+C flag off, so that if we decide to put up the TSR pop-up &
; decide to stay on for a while longer no one will forcibly abort us.

	mov     ax,3301h                ;set CTRL+C flag
	xor     dl,dl                   ;to off
	int     21h                     ;break flag turned off

	pop     ax                      ;restor return code

	cmp     cs:[TsrActive],0ffh     ;is there a tsr to handle ?
	jnz     NoTsrActive             ;no.
	popf                            ;donot need them now

; a TSR is active, allocate a fresh stack block and move the stack to it

	mov     ah,48h                  ;code to allocate memory block
	mov     bx,64                   ;1K for temporary stack
	int     21h                     ;do the allocation
	mov     cs:[TempStack],ax       ;save segment

; get back exec return code and flag, they must be saved on the new stack

	mov     bx,1000                 ;offset of start stack top
	mov     ss,ax                   ;switch stack
	mov     sp,bx                   ;ss:sp in temporary stack

; now put up the dialog box and wait

	call    ProcessTsr              ;handle the tsr if any
	smov    ds,cs                   ;go back to own segment

; go back to the entry level stack

	mov     ss,WoaRlmEntrySS        ;get the entry point stack
	mov     sp,WoaRlmEntrySP        ;get the entry point stack pointer

; release the stack space.

	mov     ah,49h                  ;release memory code
	mov     es,cs:[TempStack]       ;the allocated segment
	int     21h                     ;block released

; now flags to say there is no exec failure

	clc                             ;no failure
	pushf                           ;save the flag

NoTsrActive:

	push    ax                      ;save exec return code

; if 'destroy pif window' pif setting is not set, display message that we
; are ready to exit and wait for user to type in a key, else this routine
; will be a NOP.

	or      SwitcherDisabled,SD_LOCAL_DISABLE;prevent a switch out.
	call    PressKeyToExit          ;wait if necessary

; now initialize the display back to a know mode incase the old app leaves
; it in a graphics mode.Set back the start up no of lines

	mov     ax,WoaStartScreenLines  ;no of lines per screen
	cCall   GrabberInitScreen       ;initialize the display

; remove our XMS hook.

	cmp     WoafXmsInstalled,0      ;is there any xms handler
	jz      @f                      ;XMS code handler not installed.
	call    UnHookOurXMSCode        ;XMS hook removed
@@:

; we should disable the INT 15 mouse if a local handler is active

	call    DisableInt15Mouse

;----------------------------------------------------------------------------;
; we are back from execing the child, now grow back the original DOS block   ;
; allocated to windows to its original size.                                 ;
;----------------------------------------------------------------------------;

; set back ds to cs
	
	smov    ds,cs                   ;restore stub ds

; restore various dos & bios states that we had saved before the app had 
; started

	call    RestoreWinDosBiosStates ;restore dos and bios states

; if the machine is a vectra, restore the stae of the system for windows

	cCall   RestoreWindowsHPState   ;restores vectra state on a vectra

; restore the saved instance data image which includes IDT

	cCall   SwapInstanceDataBlocks

; let grabber de-install it's INT 10 hooks.

	cCall   GrabberDisableSave      ;grabber releases int 10 hooks

; restore the IRQ enable state in the PIC

	mov     al,MasterPICMask        ;get the value
	out     21h,al                  ;restored

; do the same for the slave PIC. We should restore the slave PIC mask only if
; the app used it else not. Also if the app used it, we should restore the 
; PreExec state.

	mov     al,PreExecSlavePICMask  ;get the value
	call    RestoreSlavePICMask     ;restore the slave pic mask

; go back to heap owner pdb before resizing (same as WOA's PSP)

	mov     bx,WoaPSP
	mov     ax,5000h                ;set PDB call
	int     21h                     ;we are now in DOS extenders PSP

	mov     es,WoaSegResizeBlock    ;this is the DOS block to resize
	mov     bx,OriginalDosBlockSize ;get the original size
	mov     ax,4a00h                ;resize call
	int     21h                     ;dos block resized

; now set back the original arena type.

	mov     ax,es                   ;currently pointing to the block
	dec     ax                      ;the arena segment
	mov     es,ax                   ;point to the arena
	mov     al,OriginalBlockType    ;get the actual arena type
	mov     es:[0],al               ;restore it

; now switch back to WOA PDB

	mov     bx,WoaPSP               ;WOA PDB segment
	mov     ax,5000h                ;set PDB call
	int     21h                     ;back to legal PDB

; we can now go back in protected mode and swap windows in.

	pop     ax                      ;get back exec code
	popf                            ;get back flags
	jc      @f                      ;if error, do not reset ax
	xor     ax,ax                   ;to signify that old app exited
	
@@:
	mov     bx,WoaInt15UsershApp    ;return ID of guy using INT 15
	ret

RealModeWoa     endp

;----------------------------------------------------------------------------;
;                        WOA HOOKED INTERRUPT HANDLERS                       ;
;----------------------------------------------------------------------------;

;----------------------------------------------------------------------------;
; Int1CISR:                                                                  ;
;                                                                            ;
; The Switcher hooks this interrupt to timeout Switch requests that cannot   ;
; be satisfied within a resonable frame of time. The number of timer ints    ;
; that we wait since a SwitchOut Request has been made is governed by a      ;
; constant.                                                                  ;
;                                                                            ;
; Also, if we are in the middle of a switch, we should reset the tickcount   ;
; and activate it again when we are done with the switch.                    ;
;----------------------------------------------------------------------------;

Int1CISR  proc far

	test    cs:[WoaHotkeyState],WOA_SWITCH
	jz      Int1CNoSwReq            ;no switch request has been registered

	inc     cs:[PendingTime]        ;we have seen one more tick

; have we waited long enough.

	cmp     cs:[PendingTime],TIMEOUT_TICK_COUNT
	jae     Int1CAbortSwitch        ;abort the switch.
	jmp     short ChainInt1COn      ;chain it on.

Int1CNoSwReq:

	mov     cs:[PendingTime],0      ;reset
	jmp     short ChainInt1COn      ;chain it on.

Int1CAbortSwitch:

; if we are in the process of switching, do not reset the hotkeystate.

	test    cs:[SwitchInProgress],SIP_SWITCHING
	jnz     ChainInt1COn            ;in the middle of switching

	mov     cs:[WoaHotkeyState],0   ;reset the switch request
	call    OEMBeep                 ;let user know about this

ChainInt1COn:

	jmp     cs:[ActualInt1C]        ;chain it on.

Int1CISR endp
;----------------------------------------------------------------------------;
; Int09ISR:                                                                  ;
;                                                                            ;
; This traps the keyboard hardware interrupt. It detects the types of keys   ;
; and sets flags when it detects the HOT KEYS. It also passes control to the ;
; original Int09 handler once it is done with the checkings.                 ;
;                                                                            ;
; We have a local list of HotKeys and each app in the shell stublet will have;
; it's own hotkeys. Both these lists have to be scanned.                     ;
;----------------------------------------------------------------------------;

Int09ISR  proc  far                     

; because of the hot key search, we might take some time in this routine. 
; we will enable interrupts here.

	sti                             ;enable interrupts      
	push    ds
	push    ax                      ;save callers data seg and ax

; if 'fNoSwitch' flag is set, simply chain on the interrupt.

	test    cs:[WoaBehavior],fNoSwitch;switch prevented ?
	jnz     ChainInt09ISR           ;chain on

; read the key and the scan code.

	xor     ax,ax                   ;need access to 0:
	mov     ds,ax                   ;DS points to seg at 0:

; read in the key code.

	in      al,60h                  ;al has the code for the key pressed

	mov     ah,ds:[KEYSTATE]        ;get the state flag
	and     ah,0fh                  ;mask off the unused bits

	smov    ds,cs                   ;get the code seg
	assumes ds,StubSeg

; we are assuming that we will not have any valid hotkey without SHIFT, CTRL
; or ALT. 

	jz      LeaveInt09ISR           ;can't be a hot key
	test    ah,3                    ;any shift key down ?
	jz      @f                      ;no
	or      ah,3                    ;both shift keys down
@@:


; try to find a match in the hotkey list

	pushem  ax,dx                   ;save
	call    MatchHotKeyList         ;try to match against know hotkeys
	popem   ax,dx                   ;restore scan code and shift state
	jnc     EndInt09ISR             ;a match was found, end the int

; we did not find a match in the hot key list, must chain the INT on.

LeaveInt09ISR:

; save the last key so that keys which need to check for the extended key
; sequences may look at it (like 0eh 37h  is PRTSC but just 37h is NUMPAD '*')

	mov     LastScanCode,al         ;save the last scan code

ChainInt09ISR:

	pop     ax
	pop     ds                      ;restore the registers
	assumes ds,nothing
	jmp     cs:[ActualInt09]        ;jump down the chain

EndInt09ISR:

	mov     LastScanCode,al         ;save the last scan code
	call    SwallowKey              ;eat up the interrupt
	pop     ax
	pop     ds                      ;restore the registers
	iret                            ;and go back

Int09ISR endp
;----------------------------------------------------------------------------;
; MatchLocalHotKeyList:                                                      ;
;                                                                            ;
; This takes a scan code in AL and a shift state in AH and looks for a match ;
; in the local hot key list. If a match is found, AL will return the switch  ;
; type and carry will be reset else carry will be set and AL will return 0.  ;
; This routine will also OR in the switch out flags if a match is found.     ;
;----------------------------------------------------------------------------;

MatchHotKeyList proc near

	assumes ds,StubSeg
	
	pushem  bx,cx,di,es             ;save
	smov    es,ds                   ;load our segment

; see if there is a match in the table of scan codes.

	mov     di,StubSegOFFSET ScanCodeTable
	mov     cx,MAX_NUM_PROGRAMS+MAX_FIXED_HOT_KEYS
	cld                             ;set proper direction flag

MHKL_Loop:

	repne   scasw                   ;look for a match
	jnz     MHKL_NoMatch            ;no match found

; get the index of the matching node.

	push    di                      ;save
	sub     di,2                    ;back over to the matching one
	.errnz  SIZE ScanCodeStruc - 2

	sub     di,StubSegOFFSET ScanCodeTable

; see if a prefix scan code is required and if it matches, if it does, get 
; the other details. Also, make sure that the key has not been disabled.

	shl     di,1                    ;offset into other table
	.errnz  SIZE HotKeyInfoStruc - 4
	add     di,StubSegOFFSET HotKeyInfoTable

; now there are two things to match here. The key must be enabled (that is,
; HKIS_State must be 0) and if the prefix scan code is not 0, it should match
; the LastScanCode.

	xor     bx,bx                   ;BL=enable state, BH needed for other
	mov     dx, wptr [di.HKIS_State];get enable flag and prefix scan code
	cmp     bh,dh                   ;carry set if DH is not 0
	sbb     bh,0                    ;BH=0 if DH = 0, else it is 0FFH
	and     bh,LastScanCode         ;BH = LastScanCode if DH !=0, else 0
	cmp     bx,dx                   ;do they match ?
	jnz     MHKL_Continue           ;no, look for another
	mov     dl,[di.HKIS_NodeNum]    ;logical node number

; if we are trying to hot-key to the current app, we should just pretend that
; the hot key did not match.

	cmp     dl,AppsLogicalNodeNum   ;is it the current app ?
	jz      MHKL_Continue           ;yes, look for another

	pop     bx                      ;balance the stack, don't need saved di

; we have found a hot key, get all the details.

	mov     al,[di.HKIS_Type]       ;type of the switch
	or      WoaHotkeyState,al       ;save the return value
	mov     WoaNodeToSwitchTo,dl    ;save the app's NodeID
	clc                             ;match obtained
	jmp     short MHKL_Ret          ;done.

MHKL_Continue:

	pop     di                      ;points after last match
	jcxz    MHKL_NoMatch            ;no more to search
	jmp     short MHKL_Loop         ;continue looking for more

MHKL_NoMatch:

	stc                             ;no match obtained

MHKL_Ret:

	popem   bx,cx,di,es             ;save
	ret

MatchHotKeyList endp
;----------------------------------------------------------------------------;
; PrepareAppHotKeyList:                                                      ;
;                                                                            ;
; This routine traverses the global switcher structure and fills in the table;
; of Scan codes and hotkey information in the two corresponding tables. This ;
; done once before starting the app and after every switch back to the app.  ;
;----------------------------------------------------------------------------;

PrepareAppHotKeyList proc near

	assumes ds,StubSeg

; first zero out the scan code and hotkey info tables that are specific to
; apps.

	push    es                      ;save
	cld
	smov    es,ds                   ;load our segment into es
	xor     ax,ax                   ;will zero out
	mov     di,StubSegOFFSET AppsScanCodeTable
	mov     cx,MAX_NUM_PROGRAMS * SIZE ScanCodeStruc
	rep     movsb                   ;zero out first table
	mov     di,StubSegOFFSET AppsHotKeyInfoTable
	mov     cx,MAX_NUM_PROGRAMS * SIZE HotKeyInfoStruc
	rep     movsb                   ;zero out first table

; now rebuild the tables.

	les     di,lpGlobalSwtchStr     ;get a pointer to the structure
	xor     bx,bx                   ;start at the top of the table
	mov     cx,MAX_NUM_PROGRAMS     ;number of entries
	mov     dl,SIZE Switch_Entry    ;size of each entry

; walk through the list gathering information about scan codes and hot keys.

	xor     ah,ah                   ;zero out for 'mul' below
	mov     al,es:[di].First_In_list;get the first entry
	mul     dl                      ;ax has the node offset
	lea     si,[di].Program_List    ;es:si points to the first program entry
	add     si,ax                   ;point to the right node.
	xor     dh,dh                   ;logical node number

PAHKL_Loop:

; stuff in the scan codes.

	mov     al,es:[si].HK_Scan_Code_2             ;second scan code
	mov     AppsScanCodeTable[bx.SCS_ScanCode],al ;save scan code.
	mov     al,es:[si].HK_Shift_State             ;get the shift state

; if any shift bit is set, set both.

	test    al,ST_RSHIFT+ST_LSHIFT                ;any shift set ?
	jz      @f                                    ;no.
	or      al,ST_RSHIFT+ST_LSHIFT                ;set both.
@@:
	mov     AppsScanCodeTable[bx.SCS_ShiftState],al ;save shift state


; save the rest of the information about the hotkeys in the other table

	shl     bx,1                                          ;index into the other table
	.errnz SIZE HotKeyInfoStruc - SIZE ScanCodeStruc * 2  ;assert the assumption
	mov     AppsHotKeyInfoTable[bx.HKIS_State],0          ;enabled
	mov     al,es:[si].HK_Scan_Code_1                     ;first scan code
	mov     AppsHotKeyInfoTable[bx.HKIS_PrefixScanCode],al;prefix scan code
	mov     AppsHotKeyInfoTable[bx.HKIS_NodeNum],dh       ;save shift state
	mov     AppsHotKeyInfoTable[bx.HKIS_Type],WOA_ALT_ESC ;type of switch
	shr     bx,1                                          ;restore index

; if the node is for the current app, save the logical node number.

	mov     ax,es:[si.Program_Id]   ;get the program ID
	cmp     ax,WoahApp              ;is it the current node ?
	jnz     PAHKL_Next              ;no.
	mov     AppsLogicalNodeNum,dh   ;save node number

PAHKL_Next:

; move onto the next node

	add     bx,SIZE ScanCodeStruc   ;next index
	xor     ah,ah                   ;zero out
	mov     al,es:[si].Next_In_List ;get the next entry
	cmp     al,es:[di].First_In_List;done ?
	jz      PAHKL_Done              ;yes
	mul     dl                      ;ax has the offset from the start
	lea     si,[di].Program_List    ;es:si points to the first program entry
	add     si,ax                   ;point to the right node.
	inc     dh                      ;one more fresh entry obtained
	loop    PAHKL_Loop              ;continue

PAHKL_Done:

	pop     es                      ;restore
	ret

PrepareAppHotKeyList endp
;----------------------------------------------------------------------------;
; Int15ISR:                                                                  ;
;                                                                            ;
; Hooks the AH=88H INT 15 function and if WoaInt15UsershApp is 0, returns    ;
; OriginalInt15MemSize in AX else returns 0.                                 ;
;                                                                            ;
; This code also watches for the INT 15/C207H call to set a mouse handler on ;
; PS/2 machines. If the callback CS is >= the Switcher's CS a flag is set.   ;
;----------------------------------------------------------------------------;

Int15ISR proc far

	assumes ds,nothing

	cmp     ah,88h                  ;function we want to trap ?
	jnz     CheckOther15Traps       ;no, check for other trap codes
	mov     OurInt15OffChain,0      ;we are still in chain.
	mov     ax,OriginalInt15MemSize ;get the original size
	cmp     WoaInt15UsershApp,0     ;is there an INT 15 user
	jz      @f                      ;no, return original size
	xor     ax,ax                   ;return no memory available
@@:
	iret

CheckOther15Traps:

	cmp     ax,0c207h               ;set mouse call back ?
	jnz     LetGoInt15              ;no, chain it on.
	mov     cs:[LocalInt15MouseHandler],0
	pushem  ax,bx                   ;save
	mov     ax,es                   ;call back segment
	mov     bx,cs                   ;Switcher code segment
	cmp     ax,bx                   ;address in global area ?
	popem   ax,bx                   ;restore
	jb      LetGoInt15              ;yes, address in global area
	mov     cs:[LocalInt15MouseHandler],0ffh

; fall through.

LetGoInt15:

	jmp     cs:[ActualInt15]        ;chain down

Int15ISR endp
;----------------------------------------------------------------------------;
;Int16ISR:                                                                   ;
;                                                                            ;
; Hooks the INT 16 Interrupt vector. It would do the context switch if the   ;
; appropriate hot key is set and we are not inside dos or the critical error ;
; handler.                                                                   ;
;----------------------------------------------------------------------------;

Int16ISR proc far

	assumes ds,nothing

; if we have reached our INT 16 handler we should set a flag byte to 0 so
; that the Pasting code will detect that pasting can occur.

	push    ax                      ;save
	xor     al,al                   ;want to reset the flag
	xchg    cs:[TestWoaInt16InChain],al
	cmp     al,0ffh                 ;was it just to test if on chain or not?
	pop     ax                      ;restore
	jz      ChainInt16              ;yes, bypass action on this call

; do a contextswitch if appropriate.

	call    SwitchIfPossible        ;ContextSwitch if possible

; if this is INT 16 was originated by our own code, pass it on to BIOS

	cmp     cs:[WOAsInt16],0ffh     ;call from own code ?
	jz      ChainInt16              ;yes, chain it on.

	cmp     ah,10h                  ;enhanced read key ?
	jz      Int16BlockRead          ;yes
	cmp     ah,0                    ;is it a 'READ KEY' call
	jnz     ChainInt16              ;no

Int16BlockRead:

; we have a read key call, wait till the key is pressed

	call    WaitForKeyFromInt16     ;wait for key to be pressed

ChainInt16:

	jmp     cs:[ActualInt16]        ;go back to original int 16 code

Int16ISR endp
;----------------------------------------------------------------------------;
; Int21ISR:                                                                  ;
;                                                                            ;
; Hooks the INT 21 vector. The code maintains a list of functions that WOA   ;
; traps and the corresponding handler routine addresses.                     ;
;----------------------------------------------------------------------------;

Int21ISR  proc near
  
; at this point we check to see if it is possible to do a context switch. If
; apprpriate flags are set and this is not a nested INT 21 call or call from
; within a critical error handler, we can do the switch

	call    SwitchIfPossible        ;switch out if possible

	push    bp                      ;save before trashing
	push    ds                      ;save ds
	mov     cs:[DosDefaultCode],ah  ;jam current code in
	mov     bp,StubSegOFFSET DosTraps
	sub     bp,3                    ;each entry of 3 bytes

; search for the address of the function which handles the call

@@:

	add     bp,3                    ;look at next entry
	cmp     ah,cs:[bp]              ;matches code ?
	jnz     @b                      ;continue searching

;  jump off to the function, BP is pushed on stack

	
	push    wptr cs:[bp+1]          ;save jump address
	mov     bp,sp                   ;will access stack with it
	add     bp,4                    ;ss:[bp] has original bp
	;sti                             ;enable interrupts
	near_ret                        ;jump to the handler

Int21ISR endp


	;----------------------------------------------------------;
	; DOS function handler top level routines are defined next ;
	;----------------------------------------------------------;


	;----------------------------------------------------------;
	; This one handles all calls that are not trapped by WOA   ;
	;----------------------------------------------------------;

DosDefaultFunc:

	pop     ds                      ;restore ds
	pop     bp                      ;restore bp
	cli                             ;shut of interrupts
	jmp     cs:[ActualInt21]        ;invoke actual handler

	;----------------------------------------------------------;
	; This one is used to return back to caller                ;
	;----------------------------------------------------------;

DosRet:

	pop     ds                      ;restore ds
	pop     bp                      ;retore bp
	iret                            ;return to user

	;----------------------------------------------------------;
	; This one just calls off to the original dos handler and  ;
	; retuns back. It is used for our internal calls.          ;
	;----------------------------------------------------------;

DosCall proc near

	pushf                           ;part of call-iret protocal
	cli                             ;part of calling protocal
	call    cs:[ActualInt21]        ;call off to dos
	ret                             ;return to caller

DosCall endp

	;----------------------------------------------------------;
	; This one handles all readkey board functions which wait  ;
	; for the key to be hit, like function numbers:            ;
	;       01 -- console input with excho.                    ;
	;       07 -- unfiltered char input with echo              ;
	;       08 -- character input without echo                 ;
	;----------------------------------------------------------;

DosFuncGetKey:

; wait till a character is ready and do a dos call to get the status to 
; take care of redirection.

	sti                             ;enable interrupts
	call    WaitForKeyFromDosCalls  ;wait for a key to be ready
	jmp     DosDefaultFunc          ;execute the function


	;----------------------------------------------------------;
	; This one handles Function 0A (BufferedKeyboardInput)     ;
	;----------------------------------------------------------;

DosFunc0A:

	sti                             ;enable interrupts
	call    SaveRegisters           ;save all resgisters
	mov     es,USER_DS              ;get users DS
	mov     di,USER_DX              ;es:di has users buffer
	mov     cx,[di]                 ;get the buffer length,template length
	smov    ds,cs                   ;load stubseg in ds
	add     dx,2                    ;es:dx points to proper buffer
	push    bp                      ;bp will be thrashed 
	mov     RCB_FileHandle,1        ;handle for STDOUT
	mov     b3fRCB,0                ;not a 3FH call
	call    ReadConsoleBuffer       ;handle the call
	pop     bp                      ;restore bp
	mov     ds,USER_DS              ;get back users ds
	mov     di,USER_DX              ;get back start of buffer
	or      al,al                   ;was the count zero
	jz      @f                      ;yes
	dec     al                      ;return 1 less for CR
@@:
	mov     [di+1],al               ;fill in new template length
	call    RestoreRegisters        ;restore all registers including BP
	iret                            ;get back to caller

	;----------------------------------------------------------;
	; This one handles Function 0C - Flush Buffer and read key ;
	; board, it first flushes the keyboard buffer and then dis-;
	; -patches any of the other calls if there is one.         ;
	;----------------------------------------------------------;

DosFunc0C:

	sti                             ;enable interrupts
	push    ax                      ;save calling code
	xor     al,al                   ;want to just flush the keyboard
	call    DosCall                 ;key board flushed
	pop     ax                      ;get back function
	xchg    ah,al                   ;get the function key in ah
	cmp     ah,0ah                  ;read console buffer call ?
	jz      DosFunc0A               ;yes, dispatch it
	cmp     ah,01h                  ;conin with echo call ?
	jz      DosFuncGetKey           ;yes,dispatch it
	cmp     ah,07h                  ;unfiltered input call ?
	jz      DosFuncGetKey           ;yes,dispatch it
	cmp     ah,08h                  ;con in without echo call ?
	jz      DosFuncGetKey           ;yes, dispatch it

; we don't want to handle the 'al=6' case as it does not wait for any key.

	cmp     ah,06h                  ;direct console i/o ?
	jz      DosDefaultFunc          ;let dos handle it.

; keyborad is flushed and AL does not have a valid function code, so we 
; get back to the user.

	jmp     short DosRet            ;go back, bypassing DOS

	;----------------------------------------------------------;
	; This one handles the terminate and stay resident call.   ;
	; A flag is set to indicate that a TSR has been run.       ;
	;----------------------------------------------------------;

DosFunc31:

	mov     cs:[TsrActive],0ffh     ;a TSR call done
	jmp     DosDefaultFunc          ;let the call go

	;----------------------------------------------------------;
	; This one handles function 3fh - Read From File Or Device.;
	; It would trap the call if the handle specified a console ;
	; device, else will pass it on to the actual handler.      ;
	;----------------------------------------------------------;

DosFunc3f:

	sti                             ;enable interrupts
	call    IsFileConsole?          ;check if the file is CON:
	njnz     DosDefaultFunc          ;no,excute actual ISR
	call    SaveRegisters           ;save all the users registers
	mov     es,USER_DS              ;load users ds
	smov    ds,cs                   ;ds has StubSeg segment
	push    bp                      ;may get thrashed
	mov     RCB_FileHandle,bx       ;save file handle
	mov     b3fRCB,-1               ;3F call
	call    XenixRead               ;trap and execute the call
	pop     bp                      ;restore
	mov     USER_AX,ax              ;save the return value
	call    RestoreRegisters        ;restore all the registers
	iret                            ;go back to user

	;----------------------------------------------------------;
	; This handles DOS function 44 and traps only subfunction, ;
	; 01 and tests to see if the console device is set into raw;
	; mode or not.                                             ;
	;-----------------------------------------------------------;

DosFunc44:

	sti                             ;enable interrupts
	cmp     al,01                   ;subfuntion number 01 ?
	jnz     default44               ;no, we will not trap it
	test    dx,1                    ;is it the standard input
	jz      default44               ;no, we are not interseted

; set the ConsoleRawMode flag to 1, if bit 5 of dl is set, else set it  0
	call    DosCall
	pushf
	jc      iret44     

	mov     cs:ConSoleRawMode,0     ;assume not in raw mode
	test    dl,00100000b            ;set to raw mode ?
	jz      iret44                  ;no, it is in ccoked mode

; set ConsoleRawMode flag on.

	dec     cs:ConsoleRawMode       ;set it to 0ffh
iret44: 
	popf
	pop     ds
	pop     bp
	retf 2
	
default44:
	jmp     DosDefaultFunc          ;let DOS handle the call to0!
;----------------------------------------------------------------------------;
; Int23ISR:                                                                  ;
;                                                                            ;
; If DS is same as CS, then we are in our stub code and we will ask Dos to   ;
; not abort us, else the app will be aborted.                                ;
;----------------------------------------------------------------------------;

Int23ISR  proc far      
  
	sti                             ;enable interrupts
	pushem  ax,bx                   ;save 
	mov     ax,cs                   ;get cs
	mov     bx,ds                   ;get ds
	cmp     ax,bx                   ;in our code
	jnz     @f                      ;no.
	popem   ax,bx                   ;restore
	clc                             ;do not abort
	ret
@@:
	mov     ah,0dh                  ;flush disk buffers call
	int     21h                     ;all buffers flushed
	popem   ax,bx                   ;restore
	stc                             ;set carry on
	ret                             ;leave flags on stack, dos will abort 

Int23ISR endp
;----------------------------------------------------------------------------;
; Int24ISR:                                                                  ;
;                                                                            ;
; This routine traps the critical error handler. If the user code when the   ;
; error happened is not the stub code, the original handler is called to pro-;
; -cess the call, else just the fail error code is passed back.              ;
;----------------------------------------------------------------------------;

Int24ISR   proc far

	pushf                           ;save the flags
	push    bp                      ;save bp, we need to access stack
	mov     bp,sp                   ;get the current stack pointer
	mov     ax,cs                   ;get stub code segment
	cmp     ss:[bp+1eh],ax          ;callers code segment is here
	pop     bp                      ;restore bp
	je      @f                      ;error in stub code
	call    cs:[ActualInt24]        ;call the original handler
	iret                            ;get back

@@:
	popf                            ;get back the flags
	mov     AL, 3                   ;Return FAIL code
	iret                            ;return back

Int24ISR  endp
;----------------------------------------------------------------------------;
; Int27ISR:                                                                  ;
;                                                                            ;
; This routine traps the old termonate and stay resident call. A flag is set ;
; in the code segment and the original int 27 handler is invoked.            ;
;----------------------------------------------------------------------------;

Int27ISR   proc far

	mov     cs:[TsrActive],0ffh     ;a tsr call going thru.
	jmp     cs:[ActualInt27]        ;invoke original handler

Int27ISR  endp
;----------------------------------------------------------------------------;
; Int2AISR:                                                                  ;
;                                                                            ;
; This hooks INT 2AH and looks for asynchronous Network requests. If the     ;
; command code in AH is 1 or 4 we set 'AsyncNetPending' to 0ffh if the       ;
; NCB command code implies a NoWait request. We never try to keep track of   ;
; when the request is completed though.                                      ;
;                                                                            ;
; If the NetRequest is from a CS loaded befor us or is above 0A000H we will  ;
; ignore it.                                                                 ;
;----------------------------------------------------------------------------;

Int2AISR proc far

	call    CheckCallersCS          ;check where the call came from
	jnc     Int2AISRChain           ;not from the app, chain it on.

	cmp     ah,1                    ;1 and 4 for async net requests
	je      IsAsync2ARequest        ;check for 'NoWait'NCB command
	cmp     ah,4                    ;1 and 4 for async net requests
	je      IsAsync2ARequest        ;check for 'NoWait'NCB command
	jmp     short Int2AISRChain     ;chain call on.

IsAsync2ARequest:

	test    bptr es:[bx],80h        ;is it a 'NoWait' request ?
	jz      Int2AISRChain           ;no, chain it on.

Int2aBlockSwitchOut:

; set a flag so that we will not try to switch out from this application.

	mov     cs:[AsyncNetPending],0ffh

Int2AISRChain:

	jmp     cs:[ActualInt2A]        ;pass on to original handler

Int2AISR endp
;----------------------------------------------------------------------------;
; OtherInt2fISR:                                                             ;
;                                                                            ;
; This hooks the INT 2FH vector. It just looks at the IS_WINOLDAP_ACTIVE     ;
; invovcation code and returns AX = 0 for it (it is active) else it chains   ;
; the INT on. Note: if XMS handling is being done, WOA's XMS handler will    ;
; hook INT 2fH ahain (in WOAXMS.ASM)                                         ;
;                                                                            ;
; It also traps the Windows Standard/Enhanced mode start up and exit calls & ;
; sets a flag so that the switcher does not try to switch out while Windows  ;
; in protected mode is active under it.                                      ;
;                                                                            ;
; We also look for the Switcher API INT 2F call to get the address of our    ;
; CallIn routine.                                                            ;
;----------------------------------------------------------------------------;

OtherInt2fISR   proc far

	cmp     ax,IS_WINOLDAP_ACTIVE   ;call of interest ?
	jz      WinoldapActive          ;yes
	cmp     ax,WIN_P_START_UP       ;Windows protected mode start up ?
	jz      WinPStarts              ;yes
	cmp     ax,WIN_P_EXIT           ;Windows protected mode exits ?
	jz      WinPExits               ;yes.

	cmp     ax,SWAPI_DETECT_SWITCHER;Detect_Switcher call ?
	jz      Int2FDetectSwitcher     ;yes.

Chain2fOn:             

; chain the call on.

	jmp     cs:[OtherActualInt2f]   ;chain on

WinoldapActive:

; Winoldap is active, terminate this call with an appropriate code in AX

	xor     ax,ax                   ;say winoldap is active
	iret                            ;terminate chain here

WinPStarts:

; set a flag bit saying that protected mode Windows is around.

	or      cs:[SwitcherDisabled], SD_PMODE_WINDOWS
	jmp     cs:[OtherActualInt2f]   ;chain on

WinPExits:

; reset the flag bit which was set when protected mode Windows started.

	and     cs:[SwitcherDisabled], NOT SD_PMODE_WINDOWS
	jmp     cs:[OtherActualInt2f]   ;chain on

Int2fDetectSwitcher:

	or      bx,bx                   ;must be zero
	jnz     Chain2fOn               ;chain it on
	push    ax                      ;save
	mov     ax,es                   ;es:di must be 0 
	or      ax,di                   ;is it zero ?
	pop     ax                      ;restore
	jnz     Chain2fOn               ;no, chain it on
	smov    es,cs                   ;ES=CS
	mov     di,StubSegOFFSET SwitchAPICallIn
	xor     ax,ax                   ;return code
	mov     bx,sp                   ;get pointer to IRET frame
	and     word ptr ss:[bx+4],NOT 1;reset the carry flag
	xor     bx,bx                   ;set it back to 0
	iret                            ;terminate the call here

OtherInt2fISR   endp
;----------------------------------------------------------------------------;
; Int5CISR:                                                                  ;
;                                                                            ;
; This hooks INT 5CH and looks for asynchronous Network requests. If the     ;
; command code in the NCB has the MSB set, we will set 'AsyncNetPending' to  ;
; 0ffh. We never try to keep track of when the request is completed though.  ;
;                                                                            ;
; If the NetRequest is from a CS loaded befor us or if the caller's CS is    ;
; above 0A000H we will ignore it.                                            ;
;----------------------------------------------------------------------------;

Int5CISR proc far

	call    CheckCallersCS          ;check where the call came from
	jnc     Int5CISRChain           ;not from the app, chain it on.

; test whether it is a 'No Wait' call.

	test    bptr es:[bx],80h        ;'NoWait' request ?
	jz      Int5CISRChain           ;no,chain call on.

Int5cBlockSwitchOut:

; set a flag so that we will not try to switch out from this application.

	mov     cs:[AsyncNetPending],0ffh

Int5CISRChain:

	jmp     cs:[ActualInt5C]        ;chain on.

Int5CISR endp
;----------------------------------------------------------------------------;
; Default hooks for all hardware vectors that the Switcher does not hooks for;
; its own purpose. These hooks increment a byte, call down stream and dec the;
; byte. The byte will tell the switchet how many hardware interrupts are     ;
; being serviced and the Switcher will not switch if the count is non zero.  ;
;----------------------------------------------------------------------------;

		;--------------------------------------;
		; INT 08H   ---- IRQ 0                 ;
		;--------------------------------------;

HInt08ISR proc far

	inc     cs:[HardwareIntCount]   ;one more hardware int being serviced
	pushf                           ;call far - iret protocol
	cli                             ;correct way to simulate int
	
; CAUTION: The following 5 bytes are for chaining down the interrupt. The
; chain address will be stuffed in at run time.

	db      9ah                     ;call far opcode

HInt08ChainAddr dd      ?               ;completes the callf instruction

	dec     cs:[HardwareIntCount]   ;interrupt has been serviced
	iret                            ;return

HInt08ISR endp

		;--------------------------------------;
		; INT 09H   ---- IRQ 1                 ;
		;--------------------------------------;

HInt09ISR proc far

	inc     cs:[HardwareIntCount]   ;one more hardware int being serviced
	pushf                           ;call far - iret protocol
	cli                             ;correct way to simulate int
	
; CAUTION: The following 5 bytes are for chaining down the interrupt. The
; chain address will be stuffed in at run time.

	db      9ah                     ;call far opcode

HInt09ChainAddr dd      ?               ;completes the callf instruction

	dec     cs:[HardwareIntCount]   ;interrupt has been serviced
	iret                            ;return

HInt09ISR endp

		;--------------------------------------;
		; INT 0AH   ---- IRQ 2                 ;
		;--------------------------------------;

HInt0AISR proc far

	inc     cs:[HardwareIntCount]   ;one more hardware int being serviced
	pushf                           ;call far - iret protocol
	cli                             ;correct way to simulate int
	
; CAUTION: The following 5 bytes are for chaining down the interrupt. The
; chain address will be stuffed in at run time.

	db      9ah                     ;call far opcode

HInt0AChainAddr dd      ?               ;completes the callf instruction

	dec     cs:[HardwareIntCount]   ;interrupt has been serviced
	iret                            ;return

HInt0AISR endp
		;--------------------------------------;
		; INT 0BH   ---- IRQ 3                 ;
		;--------------------------------------;

HInt0BISR proc far

	inc     cs:[HardwareIntCount]   ;one more hardware int being serviced
	pushf                           ;call far - iret protocol
	cli                             ;correct way to simulate int
	
; CAUTION: The following 5 bytes are for chaining down the interrupt. The
; chain address will be stuffed in at run time.

	db      9ah                     ;call far opcode

HInt0BChainAddr dd      ?               ;completes the callf instruction

	dec     cs:[HardwareIntCount]   ;interrupt has been serviced
	iret                            ;return

HInt0BISR endp
		;--------------------------------------;
		; INT 0CH   ---- IRQ 4                 ;
		;--------------------------------------;

HInt0CISR proc far

	inc     cs:[HardwareIntCount]   ;one more hardware int being serviced
	pushf                           ;call far - iret protocol
	cli                             ;correct way to simulate int
	
; CAUTION: The following 5 bytes are for chaining down the interrupt. The
; chain address will be stuffed in at run time.

	db      9ah                     ;call far opcode

HInt0CChainAddr dd      ?               ;completes the callf instruction

	dec     cs:[HardwareIntCount]   ;interrupt has been serviced
	iret                            ;return

HInt0CISR endp
		;--------------------------------------;
		; INT 0DH   ---- IRQ 5                 ;
		;--------------------------------------;

HInt0DISR proc far

	inc     cs:[HardwareIntCount]   ;one more hardware int being serviced
	pushf                           ;call far - iret protocol
	cli                             ;correct way to simulate int
	
; CAUTION: The following 5 bytes are for chaining down the interrupt. The
; chain address will be stuffed in at run time.

	db      9ah                     ;call far opcode

HInt0DChainAddr dd      ?               ;completes the callf instruction

	dec     cs:[HardwareIntCount]   ;interrupt has been serviced
	iret                            ;return

HInt0DISR endp
		;--------------------------------------;
		; INT 0EH   ---- IRQ 6                 ;
		;--------------------------------------;

HInt0EISR proc far

	inc     cs:[HardwareIntCount]   ;one more hardware int being serviced
	pushf                           ;call far - iret protocol
	cli                             ;correct way to simulate int
	
; CAUTION: The following 5 bytes are for chaining down the interrupt. The
; chain address will be stuffed in at run time.

	db      9ah                     ;call far opcode

HInt0EChainAddr dd      ?               ;completes the callf instruction

	dec     cs:[HardwareIntCount]   ;interrupt has been serviced
	iret                            ;return

HInt0EISR endp
		;--------------------------------------;
		; INT 0FH   ---- IRQ 7                 ;
		;--------------------------------------;

HInt0FISR proc far

	inc     cs:[HardwareIntCount]   ;one more hardware int being serviced
	pushf                           ;call far - iret protocol
	cli                             ;correct way to simulate int
	
; CAUTION: The following 5 bytes are for chaining down the interrupt. The
; chain address will be stuffed in at run time.

	db      9ah                     ;call far opcode

HInt0FChainAddr dd      ?               ;completes the callf instruction

	dec     cs:[HardwareIntCount]   ;interrupt has been serviced
	iret                            ;return

HInt0FISR endp
		;--------------------------------------;
		; INT 70H   ---- IRQ 8                 ;
		;--------------------------------------;

HInt70ISR proc far

	inc     cs:[HardwareIntCount]   ;one more hardware int being serviced
	pushf                           ;call far - iret protocol
	cli                             ;correct way to simulate int
	
; CAUTION: The following 5 bytes are for chaining down the interrupt. The
; chain address will be stuffed in at run time.

	db      9ah                     ;call far opcode

HInt70ChainAddr dd      ?               ;completes the callf instruction

	dec     cs:[HardwareIntCount]   ;interrupt has been serviced
	iret                            ;return

HInt70ISR endp
		;--------------------------------------;
		; INT 71H   ---- IRQ 9                 ;
		;--------------------------------------;

HInt71ISR proc far

	inc     cs:[HardwareIntCount]   ;one more hardware int being serviced
	pushf                           ;call far - iret protocol
	cli                             ;correct way to simulate int
	
; CAUTION: The following 5 bytes are for chaining down the interrupt. The
; chain address will be stuffed in at run time.

	db      9ah                     ;call far opcode

HInt71ChainAddr dd      ?               ;completes the callf instruction

	dec     cs:[HardwareIntCount]   ;interrupt has been serviced
	iret                            ;return

HInt71ISR endp
		;--------------------------------------;
		; INT 72H   ---- IRQ 10                ;
		;--------------------------------------;

HInt72ISR proc far

	inc     cs:[HardwareIntCount]   ;one more hardware int being serviced
	pushf                           ;call far - iret protocol
	cli                             ;correct way to simulate int
	
; CAUTION: The following 5 bytes are for chaining down the interrupt. The
; chain address will be stuffed in at run time.

	db      9ah                     ;call far opcode

HInt72ChainAddr dd      ?               ;completes the callf instruction

	dec     cs:[HardwareIntCount]   ;interrupt has been serviced
	iret                            ;return

HInt72ISR endp
		;--------------------------------------;
		; INT 73H   ---- IRQ 11                ;
		;--------------------------------------;

HInt73ISR proc far

	inc     cs:[HardwareIntCount]   ;one more hardware int being serviced
	pushf                           ;call far - iret protocol
	cli                             ;correct way to simulate int
	
; CAUTION: The following 5 bytes are for chaining down the interrupt. The
; chain address will be stuffed in at run time.

	db      9ah                     ;call far opcode

HInt73ChainAddr dd      ?               ;completes the callf instruction

	dec     cs:[HardwareIntCount]   ;interrupt has been serviced
	iret                            ;return

HInt73ISR endp
		;--------------------------------------;
		; INT 74H   ---- IRQ 12                ;
		;--------------------------------------;

HInt74ISR proc far

	inc     cs:[HardwareIntCount]   ;one more hardware int being serviced
	pushf                           ;call far - iret protocol
	cli                             ;correct way to simulate int
	
; CAUTION: The following 5 bytes are for chaining down the interrupt. The
; chain address will be stuffed in at run time.

	db      9ah                     ;call far opcode

HInt74ChainAddr dd      ?               ;completes the callf instruction

	dec     cs:[HardwareIntCount]   ;interrupt has been serviced
	iret                            ;return

HInt74ISR endp
		;--------------------------------------;
		; INT 75H   ---- IRQ 13                ;
		;--------------------------------------;

HInt75ISR proc far

	inc     cs:[HardwareIntCount]   ;one more hardware int being serviced
	pushf                           ;call far - iret protocol
	cli                             ;correct way to simulate int
	
; CAUTION: The following 5 bytes are for chaining down the interrupt. The
; chain address will be stuffed in at run time.

	db      9ah                     ;call far opcode

HInt75ChainAddr dd      ?               ;completes the callf instruction

	dec     cs:[HardwareIntCount]   ;interrupt has been serviced
	iret                            ;return

HInt75ISR endp
		;--------------------------------------;
		; INT 76H   ---- IRQ 14                ;
		;--------------------------------------;

HInt76ISR proc far

	inc     cs:[HardwareIntCount]   ;one more hardware int being serviced
	pushf                           ;call far - iret protocol
	cli                             ;correct way to simulate int
	
; CAUTION: The following 5 bytes are for chaining down the interrupt. The
; chain address will be stuffed in at run time.

	db      9ah                     ;call far opcode

HInt76ChainAddr dd      ?               ;completes the callf instruction

	dec     cs:[HardwareIntCount]   ;interrupt has been serviced
	iret                            ;return

HInt76ISR endp
		;--------------------------------------;
		; INT 77H   ---- IRQ 15                ;
		;--------------------------------------;

HInt77ISR proc far

	inc     cs:[HardwareIntCount]   ;one more hardware int being serviced
	pushf                           ;call far - iret protocol
	cli                             ;correct way to simulate int
	
; CAUTION: The following 5 bytes are for chaining down the interrupt. The
; chain address will be stuffed in at run time.

	db      9ah                     ;call far opcode

HInt77ChainAddr dd      ?               ;completes the callf instruction

	dec     cs:[HardwareIntCount]   ;interrupt has been serviced
	iret                            ;return

HInt77ISR endp
;----------------------------------------------------------------------------;
;                       UTILITY ROUTINES USED BY ABOVE CODE                  ;
;----------------------------------------------------------------------------;

;----------------------------------------------------------------------------;
; SwitchIfPossible:                                                          ;
;                                                                            ;
; Does a context switch if the apprpriate hot key is set and InDos and       ;
; ErrMode flags are both reset.                                              ;
;----------------------------------------------------------------------------;
SwitchIfPossible proc near

; if a switch request is already in progress, do not try to switch

	push    ax                      ;save
	mov     al,SIP_SWITCHING        ;set this when we switch.
	xchg    al,cs:[SwitchInProgress];for Test And Set
	or      al,al                   ;were we in middle of a switch ?
	pop     ax                      ;restore
	jnz     SwitchIfPossibleRet     ;do no try to switch

; test to see if the hot keys are set.

	test    cs:[WoaHotkeyState],WOA_SWITCH
	jz      SwitchIfPossibleStackOk ;switch not desired

; at this point switch to a temporary stack. Save the current stack pointer

	pushf                           ;save flags
	cli                             ;mask interrupts off
	mov     cs:[WoaAppsSS],ss
	test    cs:[WoaCpuType],WF_CPU386 + WF_CPU486ORABV ;386 or 486 cpu ?
	jnz     SIP_SaveESP                     ;need to save ESP
	mov     cs:[WoaAppsSP],sp       
	jmp     short SIP_SPSaved               
SIP_SaveESP:
	.386p
	mov     cs:[WoaAppsESP],esp     ;save esp
	.286
SIP_SPSaved:

; load a new stack pointer

	push    cs
	pop     ss                      ;stack's in our segment
	mov     sp,StubSegOFFSET WoaSwitchOutStackTop
	;;do not enable interrupts while old guys interrupts are still in! sti                             ;restore interrupts

; test to see if the switch is possible

	call    OkToSwitch?             ;is it ok to switch
	jc      SwitchIfPossibleRestoreStack;no.

ifdef	JAPAN
	cCall	KkcBusyCheck		; ok KKC
	jnc	@f			; yes
	call	OEMBeep			;
	mov     cs:[WoaHotKeyState],0   ;can't do it and don't try again later
	jmp	SwitchIfPossibleRestoreStack;
@@:
endif

; we are ready to switch, set a flag saying that we are switching

	call    ContextSwitch           ;do the context switch

SwitchIfPossibleRestoreStack:

; now restore the user's stack.

	cli                             ;disable interrupts
	test    cs:[WoaCpuType],WF_CPU386 + WF_CPU486ORABV ;386 or 486 cpu
	jnz     SIP_Restore386Stack     ;yes
	mov     ss,cs:[WoaAppsSS]       ;get back interruption time ss
	mov     sp,cs:[WoaAppsSP]       ;get back switch out time sp
	jmp     short SIP_SSSPRestored
SIP_Restore386Stack:
	.386p
	mov     ss,cs:[WoaAppsSS]       ;get back interrupt time ss
	mov     esp,cs:[WoaAppsESP]     ;getback interrupt time esp
	.286
SIP_SSSPRestored:
	popf                            ;restore interrupt state

SwitchIfPossibleStackOk:

	mov     cs:[SwitchInProgress],0 ;reset flag bits

SwitchIfPossibleRet:

	ret

SwitchIfPossible endp
;----------------------------------------------------------------------------;
; ContextSwitch:                                                             ;
;                                                                            ;
; Initiates the actual context switching.This routine save all the registers ;
; & switches back to protected mode WOA code, with a special code in AX      ;
; The protected mode code, when it decides that it is time to switch baclk,  ;
; transfers control tot the label named 'BackFromContextSwitch'              ;
;----------------------------------------------------------------------------;

ContextSwitch proc  near

	test    cs:[WoaCpuType],WF_CPU386 + WF_CPU486ORABV;is it a 386 or 486?
	jnz     Save386Registers        ;yes,need to save 386 pmode regs too.
	pushf                           ;slave interrupt state
	push    ds              
	push    es                      ;saves ES,DS as a part of the context
	pushem  ax,bx,cx,dx,bp,si,di    ;save all other resgisters
	jmp     short CpuRegistersSaved ;all registers saved

Save386Registers:

	.386p
	pushfd                          ;save interrupt state
	pushem  ds,es,gs,fs             ;save 386 segment registers
	pushad                          ;save all 32 bit registers
	.286

CpuRegistersSaved:

	cld                             ;users flag will be on IRET frame.
	smov    ds,cs                   ;make ds = cs to access variables

; set a flag if the app has hooked INT 08. This information is later needed
; by the SaveSoundState and RestoreSoundState routines.

	call    IsTimerHooked           ;see if app hooks timer over us.

; get and save the state of the control C flag and set it off for the switch.

	mov     ax,3300h                ;get state of CTRL C flag
	call    DosCall                 ;flag in dl
	mov     ControlCFlag,dl         ;save it
	mov     ax,3301h                ;set state of CTRL+C flag
	xor     dl,dl                   ;to off
	call    DosCall                 ;turned off.

; see if we can reach our Int 15 Hook.

	mov     OurInt15OffChain,0ffh   ;assume we are not in chain
	mov     ah,88h                  ;get memory size function.
	int     15h                     ;do the call ?
	cmp     ax,OriginalInt15MemSize ;equal to the original size ?
	jz      @f                      ;the above flag tells the story
	mov     OurInt15OffChain,0ffh   ;force it say it uses INT 15
@@:

; process the apps slave pic state. If the app is using IRQ2 or IRQ9, then
; shut off IRQ 9 in the slave PIC.

	call    ProcessAppSlavePICMask  ;save it and mask IRQ 9 if needed

; save relevant OLD APP vector like 9,16,21,24  and restore original value.
; We do this here so that the app does not see the screen being saved through
; it's hooks. Also all instance data images will also be swapped.

; restore the saved instance data image which includes IDT

	cCall   SwapInstanceDataBlocks

	call    SwapMasterPICMask       ;swaps APP's Master 8259 Mask
	sti                             ;enable interrupts

; try to save app screen and bail out if we cant

	cCall   GrabberSaveScreen       ;save the screen
	jnc     OldAppScreenSaved       ;old app screen has been saved

ContextSwitchFails:

; switch out failed, so clear switch flag and return. Also restore the apps
; vectors and other instance data areas. We must also do a RESUME_SESSION
; SWAPI call before restoring the local interrupt vectors.

	cCall   SWAPIResumeSession

; restore the saved instance data image which includes IDT

	cCall   SwapInstanceDataBlocks
	call    SwapMasterPICMask       ;swaps APP's Master 8259 Mask
	sti                             ;leaves int's disabled, but no need to

; restore the slave PIC state.

	mov     al,cs:[AppSlavePICMask] ;get the mask
	call    WriteSlavePICMask       ;restore it.

; local vectors are back, we must do a SESSION_ACTIVE SWAPI call

	cCall   SWAPISessionActive

; now restore the CTRL+C flag state for the application. Till now we had it 
; turned off so that it would not terminate us.

	mov     ax,3301h                ;set control c flag
	mov     dl,ControlCFlag         ;saved state of the flag
	call    DosCall                 ;set flag

	mov     cs:[WoaHotKeyState],0   ;reset switch out request
	jmp     ContextSwitchRet        ;go back

OldAppScreenSaved:

; ask grabber to de-install int 10 hooks.

	cCall   GrabberDisableSave      ;grabber releases INT 10 hooks.

; unhook our XMS hooks.

	cmp     WoafXmsInstalled,0      ;is there any xms handler
	jz      @f                      ;XMS code handler not installed.
	cCall   UnHookOurXmsCode        ;XMS code unhooked
@@:


; now initialize the disaplay to a known state and set it to 25 line mode for
; switcher screen

	mov     ax,25                   ;25 line mode for switcher screen
	cCall   GrabberInitScreen       ;screen initialized

; save dos sate like DMA address, current directory and drive, PSP etc.

	call    SaveDosState

; save other old app related stuff like 8087 state, sound h/w state etc.

	call    SaveOtherTaskVars       ;save some other old app state

; if the machine is a vactra, save the state of the vectra for the old app

	cCall   SaveDosAppHPState       ;must be restored on switch back

; if the machine is a vectra, restore the windows hp state

	cCall   RestoreWindowsHPState   ;restore saved state

; restore the state of the A20 line if the application had turned it off (done
; only if HIMEM is installed.

	call    SetCorrectA20State      ;sets A20 back on if HIMEM installed

; now go back to preexec disk and directory.

	call    ResetDosDirectory       ;set back original drive and dir

; before going back to the Switcher's PSP, we need to save the SS:SP value
; in the DWORD at address 2EH in the PSP and this will be restored at 4CH
; exit time.

	push    es                      ;save
	mov     es,WoaPSP               ;get to the Switcher's PSP
	mov     ax,wptr es:[2eh]        ;get the saved SP value
	mov     Woa4BSP,ax              ;save it
	mov     ax,wptr es:[30h]        ;get the saved SS value
	mov     Woa4bSS,ax              ;save it
	pop     es                      ;restore

; set back WINOLDAP's PSP

	mov     bx,WoaPSP               ;get WINOLDAP's PSP
	mov     ax,5000h                ;set PSP call
	int     21h                     ;WOA's PSP is current

; save value of SS,SP as a part of the oldapp context

	mov     WoaSwitchOutSS,ss
	test    cs:[WoaCpuType],WF_CPU386 + WF_CPU486ORABV ;386 or 486 cpu ?
	jnz     SaveESP                 ;need to save ESP
	mov     WoaSwitchOutSP,sp       
	jmp     short SPSaved           
SaveESP:
	.386p
	mov     WoaSwitchOutESP,esp     ;save esp
	.286
SPSaved:

; reaload SS,SP to whatever value they were set to at the time the stub was
; called

	mov     ss,WoaRlmEntrySS
	mov     sp,WoaRlmEntrySP

; get the flag byte in AX for context switch.

	xor     ax,ax                   ;set this to zero
	xchg    al,cs:[WoaHotkeyState]  ;load the hot key state and reset
	and     al,WOA_SWITCH           ;have ALT_ESC or ALT_TAB info,mask rest

; if 'OurInt15OffChain' is 0ffh then some instance has locked int 15. This
; will either be 'Int15UsershApp' is that is not zero, or else it will be
; 'WoahApp'.

	xor     bx,bx                   ;assume not locked.
	cmp     OurInt15OffChain,0ffh   ;is that true ?
	jnz     @f                      ;yes.
	mov     bx,WoaInt15UsershApp    ;assume this is not zero
	or      bx,bx                   ;is that true ?
	jnz     @f                      ;yes.
	mov     bx,WoahApp              ;this instance itself has locked it
@@:

; load the 'WoaAppUsesXMS' byte into CL (zero out XMS). If the application
; has no XMS allocated to it this variable would be in discarded code but
; that's OK, the main code seg will only check this byte if the app does
; have XMS allocated.

	xor     ch,ch
	mov     cl,WoaAppUsesXMS        ;whether app actually used XMS or not

; get the node of the app that we want to switch to in DX, if that is -1 we
; will do conventional switching.

	mov     dl,WoaNodeToSwitchTo    ;for directed hot keys

; do a far ret to protectd mode WOA

	clc                             ;mark as no error
	far_ret 

BackFromContextSwitch:

	cld                             ;take no chances with this.

; AX has the value for Int15UsershApp

	mov     bx,cs                   ;get segment value
	mov     ds,bx                   ;have it in ds too

	mov     WoaInt15UsershApp,ax    ;save it

; go back to stack on which we switched

	cli                             ;disable interrupts
	test    cs:[WoaCpuType],WF_CPU386 + WF_CPU486ORABV ;386 or 486 cpu
	jnz     Restore386Stack         ;yes
	mov     ss,WoaSwitchOutSS       ;get back interruption time ss
	mov     sp,WoaSwitchOutSP       ;get back switch out time sp
	jmp     short SSSPRestored
Restore386Stack:
	.386p
	mov     ss,WoaSwitchOutSS       ;get back interrupt time ss
	mov     esp,WoaSwitchOutESP     ;getback interrupt time esp
	.286
SSSPRestored:
	sti                             ;enable them

; have grabber put back it's INT 10 hooks

	cCall   GrabberEnableSave       ;grabber puts back int 10 hooks

; restore the dos app screen

	cCall   GrabberRestoreScreen    ;get back dos app screen

; restore our XMS Hook.

	cmp     WoafXmsInstalled,0      ;is there any xms handler
	jz      @f                      ;XMS code handler not installed.
	cCall   HookOurXmsCode          ;hook code if needed.
@@:

; we have to resume the oldapp. First restore dos state

	call    RestoreDosState         ;restores app specific dos state

; now restore other task state informations which we had saved

	call    RestoreOtherTaskVars

; if the machine is a vectra then save the current windows hp state

	cCall   SaveWindowsHPState      ;save the current state

; if the machine is a vectra, restore the state of the HP system

	cCall   RestoreDosAppHPState    ;restores HP state on a vectra

; reset the current disk and cd. We might get aborted if an INT 24 happens
; now. In that case switching and pasting will be disabled

	call    RestoreDiskAndCD        ;reset disk and current directory

; prepare the current hotkey information again

	call    PrepareAppHotKeyList

; now flush the buffer, incase any CTRL+C is pending.
	
	mov     ax,0c00h                ;flush keyboard only
	int     21h


;----------------------------------------------------------------------------;
; Make the ResumeSession Switch API call. We must make this call before we 
; restore the local interrupt vector table. This is to make sure that the
; global software gets ready before interrupts start coming in from the 
; local app.

	cCall   SWAPIResumeSession
;----------------------------------------------------------------------------;

; hook back all interrupt vectors -- we leave interrupts disabled since we
; are quickly going back to our Int 21 or 16 handler (wherever we decided
; to switch out from). Also swap back the saved instance data areas

; restore the saved instance data image which includes IDT

	cCall   SwapInstanceDataBlocks
	call    SwapMasterPICMask       ;restore app's master PIC mask

; restore the slave PIC state. We must restore it's state only if it has 
; IRQ 9 owned. Also we must save the current PIC mask as Windows mask

	call    ReadSlavePICMask        ;get the current mask
	mov     GlobalSlavePICMask,al   ;save it.
	mov     al,AppSlavePICMask      ;get the apps slave PIC mask
	call    RestoreSlavePICMask     ;restore mask if needed

; now notify the app that we are back from a context switch.
; NOTE: It is verry essential that we do the following call after restoring
; the saved interrupt vector. This is because the correspeonding check
; notification call that was done in the 'CheckWithApp' routine restores
; some vectors and we save the interrupt vectors after that call.

	call    NotifyApp               ;only if notification hook is in

;----------------------------------------------------------------------------;

; The local interrupt vectors are back, we must make the app know that he can
; resume its task.

	cCall   SWAPISessionActive
;----------------------------------------------------------------------------;
	
; now restore the CTRL+C flag state for the application. Till now we had it 
; turned off so that it would not terminate us.

	mov     ax,3301h                ;set control c flag
	mov     dl,ControlCFlag         ;saved state of the flag
	call    DosCall                 ;set flag
	
ContextSwitchRet:

	test    cs:[WoaCpuType],WF_CPU386 + WF_CPU486ORABV ;386or 486 processor ?
	jnz     Restore386Registers     ;yes, restore 386 pmode all registers

	popem   ax,bx,cx,dx,bp,si,di    ;save all other resgisters
	pop     es                      ;get back es
	pop     ds                      ;get back ds
	popf                            ;restore saved interrupt state
	jmp     short CpuRegistersRestored

Restore386Registers:

	.386p
	popad                           ;restore 32 bit registers
	popem   ds,es,gs,fs             ;restore segment registers
	popfd                           ;restore saved interrupt state
	.286

CpuRegistersRestored:
	
	ret                             ;return back to int handler


ContextSwitch endp
;----------------------------------------------------------------------------;
; SaveDosState:                                                              ;
;                                                                            ;
; This routine save app specific dos states like DMA address, PSP, current   ;
; directory, current drive etc.  It also saves EMS page mapping resgisters   ;
; is EMS is present.                                                         ;
;                                                                            ;
; Also we save the IOCTL Device data for the shared file handles 0 thru 4    ;
;                                                                            ;
; The Allocation strategy is also saved for UMB support.                     ;
;----------------------------------------------------------------------------;

SaveDosState proc near

; get the allocation strategy

	mov     ax,5800h                ;get allocation strategy
	int     21h
	mov     DosAllocationStrategy,al;save it

; get link state

	mov     ax,5802h                ;get link state info
	int     21h
	mov     DosLinkState,al         ;save it

; if UMBs are linked in, unlink them before swapping the app out.

	or      al,al                   ;unlinked ?
	jz      @f                      ;yes
	mov     ax,5803h                ;set links
	xor     bx,bx                   ;unlink.
	int     21h
@@:
	
; get and save the DMA address

	mov     ah,2fh                  ;get DTA address
	int     21h                     ;ES:BX has address
	mov     wptr [DosAppDMA+2],es
	mov     wptr [DosAppDMA],bx     ;save the DMA address

; get and save the DOS APP PSP

	mov     ax,5100h                ;get PSP call code
	int     21h                     ;BX has app psp segment
	mov     DosAppPSP,bx            ;save it

	mov     si,StubSegOFFSET DosAppCD
	inc     si                      ;skip the root symbol
	cCall   GetCurrentDiskAndCD,<si>;get the current disk
	mov     DosAppDisk,al           ;save it

; save the apps IOCTL data and restore pre exec IOCTL data

	mov     di,StubSegOFFSET AppIoctlData
	call    SaveIoctlData           ;save apps Ioctl data
	mov     si,StubSegOFFSET WinIoctlData
	call    RestoreIoctlData        ;restore pre-exec state

; save the printer port addresses of the app and restore windows printer port
; addresses.

	mov     di,StubSegOFFSET AppPrinterPorts
	call    SavePrinterPorts        ;save apps printer port addresses
	mov     si,StubSegOFFSET WinPrinterPorts
	call    RestorePrinterPorts 

; save the apps dev chain ptrs in case it has added some and restore pre exec
; chain

	call    SwapDeviceHeaders       ;swap 2 ptrs at start and 1 at end

; save EMS state if EMS drivers is present

	cmp     WoaEmsFlag,0ffh         ;is EMS driver present
	jnz     @f                      ;no, nothing to save
	push    es                      ;save
	mov     di,StubSegOFFSET AppEmsSaveArea
	smov    es,ds                   ;es:di has save buffer
	mov     ax,4e00h                ;want to get the mapper registers
	int     67h                     ;registers saved
	pop     es                      ;restore
@@:
	ret

SaveDosState    endp
;----------------------------------------------------------------------------;
; GetCurrentDiskAndCD:                                                       ;
;                                                                            ;
; A special routine to get the current Disk which hooks the INT 24 handler   ;
; in case the current drive is not accessible. The INT 24 handler is taken   ;
; away before the return.                                                    ;
;                                                                            ;
; This also gets the current directory.                                      ;
;----------------------------------------------------------------------------;

cProc   GetCurrentDiskAndCD,<NEAR,PASCAL,PUBLIC>,<es,bx>

	parmW   CurCDBuf                ;place to store currnet CD
	localD  OldInt24                ;original INT 24 handler.

cBegin

; first save the current INT 24 handler's address and hook a special INT 24
; handler.

	mov     ax,3524h                ;get original INT 24 vector
	int     21h                     ;es:bx has the vector
	mov     off_OldInt24,bx         ;save it
	mov     seg_OldInt24,es         ;save it
	mov     dx,StubSegOFFSET SpecialInt24ISR
	mov     ax,2524h                ;DS is = CS
	int     21h                     ;set the new vector

; now get the current directory

	mov     ah,47h                  ;get current directory code
	mov     dl,0                    ;for the default drive
	mov     si,CurCDBuf             ;get the buffer for current CD
	int     21h                     ;current directory obtained

; and the current disk

	mov     ah,19h                  ;current disk call
	int     21h                     ;AL has the return
	push    ax                      ;save
	mov     dx,off_OldInt24         ;get the original offset
	push    ds                      ;save
	mov     ds,seg_OldInt24         ;ds:dx points to original ISR
	mov     ax,2524h                ;set vector 24H call
	int     21h                     ;vector restored
	pop     ds                      ;restore own ds
	pop     ax                      ;get back return value

cEnd
;----------------------------------------------------------------------------;
; SetCurrentDiskAndCD:                                                       ;
;                                                                            ;
; A special routine to set the current disk which traps the INT 24 vector to ;
; fail the call if the drive is no more accessible.                          ;
;                                                                            ;
; This also gets the current directory.                                      ;
;----------------------------------------------------------------------------;
cProc   SetCurrentDiskAndCD,<NEAR,PASCAL,PUBLIC>,<es,bx>

	parmW   CurDrive                ;drive to set
	parmW   CurCD                   ;current directory.
	localD  OldInt24                ;original INT 24 handler.

cBegin


; first save the current INT 24 handler's address and hook a special INT 24
; handler.

	mov     ax,3524h                ;get original INT 24 vector
	int     21h                     ;es:bx has the vector
	mov     off_OldInt24,bx         ;save it
	mov     seg_OldInt24,es         ;save it
	mov     dx,StubSegOFFSET SpecialInt24ISR
	mov     ax,2524h                ;DS is = CS
	int     21h                     ;set the new vector

; restore the current disk

	mov     dx,CurDrive             ;get the drive to set
	mov     ah,0eh                  ;set drive code
	int     21h                     ;AL has the return

; now restore the current directory

	mov     ah,3bh                  ;change directory code
	mov     dx,CurCD                ;get the offset to the dir
	int     21h                     ;directory set back

; reset the original critical error handler

	mov     dx,off_OldInt24         ;get the original offset
	push    ds                      ;save
	mov     ds,seg_OldInt24         ;ds:dx points to original ISR
	mov     ax,2524h                ;set vector 24H call
	int     21h                     ;vector restored
	pop     ds                      ;restore own ds

cEnd
;----------------------------------------------------------------------------;
; SpecialInt24ISR:                                                           ;
;                                                                            ;
; This ISR is active only during the above get & set current drive routines  ;
; and fails any 'drive not accessible' error so that the user does not see   ;
; the message that DOS puts up in response to these error.                   ;
;----------------------------------------------------------------------------;

SpecialInt24ISR proc far

	assumes cs,StubSeg
	assumes ds,nothing

; we require DOS 3.1 or greater, so we can always fail the call

	mov     al,3                    ;fail the call
	iret

SpecialInt24ISR endp

;----------------------------------------------------------------------------;
; RestoreDiskAndCD:                                                          ;
;                                                                            ;
; Restores the current Disk and current directory.                           ;
;----------------------------------------------------------------------------;

RestoreDiskAndCD proc near

; restore the current disk

	xor     ah,ah
	mov     al,DosAppDisk
	mov     dx,StubSegOFFSET DosAppCD
	cCall   SetCurrentDiskAndCD,<ax,dx>

	ret

RestoreDiskAndCD endp
;----------------------------------------------------------------------------;
; RestoreDosState:                                                           ;
;                                                                            ;
; This routine restores the saved dos state.  It also restores the state of  ;
; the EMS page mapper registers if an EMS driver is present.                 ;
;----------------------------------------------------------------------------;

RestoreDosState proc near

; first set back the apps psp

	mov     ax,5000h                ;call to set PSP
	mov     bx,DosAppPSP            ;original PSP of the app
	int     21h                     ;PSP set back to one before switch 

; now restore the saved SS:SP value in the Switcher's PSP.

	push    es                      ;save
	mov     es,WoaPSP               ;Switcher's PSP
	mov     ax,Woa4BSP              ;get saved SP value
	mov     wptr es:[2eh],ax        ;restore it in PSP
	mov     ax,Woa4BSS              ;saved SS value
	mov     wptr es:[30h],ax        ;restore it in PSP
	pop     es                      ;restore

; restore the allocation strategy

	mov     ax,5801h                ;reset allocation strategy
	xor     bh,bh
	mov     bl,DosAllocationStrategy;get the saved strategy
	int     21h

; restore Dos link state

	mov     ax,5803h                ;reset link state info
	xor     bh,bh
	mov     bl,DosLinkState         ;get the saved state
	int     21h


; now set back the DMA address to what it was bebore the switch

	push    ds                      ;save ds
	mov     ah,1ah                  ;set DMA code
	mov     dx,wptr [DosAppDMA]
	mov     ds,wptr [DosAppDMA+2]   ;DS:DX has original DMA area
	int     21h                     ;DMA address set back
	pop     ds                      ;restore ds

; restore the apps IOCTL state for the 5 shared handles, after savibg the
; one for windows

	mov     di,StubSegOFFSET WinIoctlData
	call    SaveIoctlData
	mov     si,StubSegOFFSET AppIoctlData
	call    RestoreIoctlData

; restore the printer port addresses for the apps printer ports after saving 
; the current porst addresses for windows

	mov     di,StubSegOFFSET WinPrinterPorts
	call    SavePrinterPorts        ;save for windows
	mov     si,StubSegOFFSET AppPrinterPorts
	call    RestorePrinterPorts     ;restore them

; restore the installable device driver chain (2 ptrs at start and one at end)

	call    SwapDeviceHeaders       ;restore apps links

; restore EMS page mapper resgisters if an EMS driver is present.

	cmp     WoaEmsFlag,0ffh         ;is there an EMS driver
	jnz     @f                      ;no, nothing to restore
	mov     si,StubSegOFFSET AppEmsSaveArea
	mov     ax,4e01h                ;want to set the registers
	int     67h
@@:

	ret

RestoreDosState endp
;----------------------------------------------------------------------------;
; SaveOtherTaskVars:                                                         ;
;                                                                            ;
; This routine saves some important old app state related information which  ;
; donot require large amounts of memory. The information saved are:          ;
;                                                                            ;
;       . CoProcessor state if co processor present                          ;
;       . State of the sound generating hardware.                            ;
;----------------------------------------------------------------------------;

SaveOtherTaskVars  proc  near

; test to see if a math co processor is actually present or not

	int     11h                     ;returns equipment flag in ax
	test    al,00000010b            ;math coprocessor present ?
	jz      @f                      ;not present

; 8087 is present, save its state

	mov     si,StubSegOFFSET Buf8087;buffer where state is to be saved
	fsave   [si]                    ;save the state
@@:

; must save the sound state if necessary.

	call    SaveSoundState          ;saves the relevant sound state

; save 50 bytes of comm area from 0:4f0h

	mov     di,StubSegOFFSET CommSaveArea
	smov    es,cs                   ;es:di has save area
	push    ds                      ;save
	xor     ax,ax                   ;ds has to be loaded with 0
	mov     ds,ax                   ;ds points to segment 0
	mov     cx,50                   ;need to save 50 bytes
	mov     si,4f0h                 ;from 0:4f0
	rep     movsb                   ;bytes saved
	pop     ds                      ;restore
	ret

SaveOtherTaskVars endp
;----------------------------------------------------------------------------;
; RestoreOtherTaskVars:                                                      ;
;                                                                            ;
; This routine restores all the task state realated information that the abo-;
; -ve routine had saved.                                                     ;
;----------------------------------------------------------------------------;

RestoreOtherTaskVars    proc  near

; test to see whether a coprocessor is actually present

	int     11h                     ;returns equipment flag in AX
	test    al,00000010b            ;math coprocessor present ?
	jz      @f                      ;it is not, so do not try frstor

; restore the coprocessor state

	xor     ax,ax                   ;need to output zero to port
	out     CoProcPort,al           ; remove any pending exceptions
	mov     si,StubSegOFFSET Buf8087;buffer where state was saved
	frstor  [si]                    ;restore the coprocessor state
@@:

; restore the app sound state

	call    RestoreSoundState       ;restore sound hardware state

; now restore the state of the 50 byte comm area

	mov     si,StubSegOFFSET CommSaveArea
	push    es                      ;save
	xor     ax,ax                   ;es has to be loaded with 0
	mov     es,ax                   ;es points to segment 0
	mov     cx,50                   ;need to restore 50 bytes
	mov     di,4f0h                 ;to 0:4f0
	rep     movsb                   ;bytes restored
	pop     es                      ;restore
	ret

RestoreOtherTaskVars  endp
;----------------------------------------------------------------------------;
; SaveIoctlData:                                                             ;
;                                                                            ;
; This routine takes in ds:di a pointer to a 5 byte save area and saves the  ;
; current IOCTL data for the first 5 file handles in the block, saving just  ;
; the low byte of the data.                                                  ;
;----------------------------------------------------------------------------;

SaveIoctlData  proc near

; get the low byte of the file handles for the first 5 handles

	mov     cx,5                    ;5 handles to get data about
	push    es                      ;save
	smov    es,ds                   ;es has same segment
	xor     bx,bx                   ;start with handle 0

GetIoctlData:

	mov     ax,4400h                ;IOCTL get device data
	call    DosCall                 ;bypass our own traps
	mov     al,dl                   ;just save low byte
	stosb                           ;save it
	inc     bx                      ;next handle
	loop    GetIoctlData            ;save all 5 handles data
	pop     es                      ;restore
	ret

SaveIoctlData   endp
;----------------------------------------------------------------------------;
; RestoreIoctlData:                                                          ;
;                                                                            ;
; This routine takes in ds:si a pointer to a 5 byte save area and sets the   ;
; current IOCTL data for the first 5 file handles from the block.            ;
;----------------------------------------------------------------------------;

RestoreIoctlData  proc near

; set the low byte of the file handles for the first 5 handles

	mov     cx,5                    ;5 handles to get data about
	xor     bx,bx                   ;start with handle 0

SetIoctlData:

	lodsb                           ;get the data for the next handle
	mov     dl,al                   ;have it in dl
	xor     dh,dh                   ;must be zero for the call
	mov     ax,4401h                ;IOCTL set device data
	call    DosCall                 ;dx has data
	inc     bx                      ;next handle
	loop    SetIoctlData            ;save all 5 handles data
	ret

RestoreIoctlData        endp
;----------------------------------------------------------------------------;
; SavePrinterPorts:                                                          ;
;                                                                            ;
; This routine saves the 3 printer port addresses starting  at 40:8 into the ;
; 3 word buffer pointed to by es:di.                                         ;
;----------------------------------------------------------------------------;

SavePrinterPorts  proc near

	push    ds                      ;save data segment
	smov    es,ds                   ;have own data segment
	mov     ax,40h                  ;bios data segment
	mov     ds,ax                   ;have it in ds
	mov     bx,8                    ;first port
	mov     ax,[bx]                 ;get lpt1: base port address
	stosw                           ;save it     
	mov     ax,[bx+2]               ;get lpt2: base port address
	stosw                           ;save it
	mov     ax,[bx+4]               ;get lpt3: base port address
	stosw                           ;save it
	pop     ds                      ;restor data segment
	ret

SavePrinterPorts  endp
;----------------------------------------------------------------------------;
; RestorePrinterPorts:                                                       ;
;                                                                            ;
; This routine restores the 3 printer port word addresses at 40:8 from the   ;
; 3 word buffer pointed to by ds:si                                          ;
;----------------------------------------------------------------------------;

RestorePrinterPorts  proc near

	push    es                      ;save
	mov     ax,40h                  ;need to restore bios data area data
	mov     es,ax                   ;es points to bios data area
	lodsw                           ;get lpt1: base port address
	mov     es:[8],ax               ;restore it
	lodsw                           ;get lpt2: base port address
	mov     es:[10],ax              ;restore it
	lodsw                           ;get lpt3: base port address
	mov     es:[12],ax              ;restore it
	pop     es                      ;restore
	ret

RestorePrinterPorts  endp
;----------------------------------------------------------------------------;
; SaveDeviceHeaders:                                                         ;
;                                                                            ;
; Say, the device header chain is something like:                            ;
;                                                                            ;
;    A --> B --> C --> ....... --> Z --> -1.                                 ;
;                                                                            ;
; We assume that if the app hooks devices, it will hook in one of the three  ;
; following places:  Between A & B or between B & C or after Z.              ;
;                                                                            ;
; So, we will save the following information (each field a DWORD)            ;
;                                                                            ;
;        Node Address           Current Next Pointer                         ;
;             A                         B                                    ;
;             B                         C                                    ;
;             Z                        -1                                    ;
;                                                                            ;
; We may have only one or two lines if not a lot of devices are chained.     ;
;                                                                            ;
; Note that saving B or C twice is not redundant, because the lft coloumn is ;
; invariant. When we swap the old app or bring it back, we will swap the     ;
; values in the second coloumn (contents of the addresses in the 1st coloumn);
;                                                                            ;
; Irrespective of the no of rows saved above, we will terminate the table by ;
; a word in coloumn 1 being -1. So we need 26 bytes of storage.              ;
;----------------------------------------------------------------------------;

SaveDeviceHeaders  proc near

	push    ds                      ;save

; make es:di point to the save area

	smov    es,ds                   ;same segment
	mov     di,StubSegOFFSET DevHeaderChain

; get the location of the first device header, this is version dependant!

	call    GetDeviceChainHeader    ;ds:si has the header
	mov     cx,2                    ;2 pairs of dwords to save

SaveDevPtrLoop:

; save the address

	mov     ax,si                   ;get the offset
	stosw                           ;save the offset
	mov     ax,ds                   ;get the segment
	stosw                           ;save offset

; get and save the 'next' pointer

	lds     si,[si]                 ;get the next pointer
	mov     ax,si                   ;get the offset
	stosw                           ;save the offset
	mov     ax,ds                   ;get the segment
	stosw                           ;save offset

; if we have saved the last one, get out

	cmp     si,-1                   ;terminator ?
	jz      DevPtrsSaved            ;we are done.
	loop    SaveDevPtrLoop          ;save the next one too.   

; we have saved the first two rows (A,B & B,C) - now save the last one.

@@:
	cmp     wptr [si],-1            ;end of the chain 
	jz      SaveEndOfChain          ;yes.
	lds     si,[si]                 ;get the next in chain
	jmp     short @b                ;get to the end

SaveEndOfChain:

	mov     ax,si                   ;get the offset
	stosw                           ;save the offset
	mov     ax,ds                   ;get the segment
	stosw                           ;save offset
	mov     ax,-1                   ;save the tehrminator
	stosw                           ;save termonator
	stosw                           ;save it as dword

DevPtrsSaved:

	mov     ax,-1                   ;end of table
	stosw                           ;end of table in first coloumn.

	pop     ds                      ;restore
	ret

SaveDeviceHeaders  endp
;----------------------------------------------------------------------------;
; SwapDevPtrs:                                                               ;
;                                                                            ;
; This  routine swaps the contents of the (upto) 3 device header nodes as    ;
; explained in the above routine.                                            ;
;----------------------------------------------------------------------------;

SwapDeviceHeaders  proc near

	push    ds                      ;save
	smov    es,ds                   ;same segment
	mov     di,StubSegOFFSET DevHeaderChain
	cli                             ;shut interrupts off

@@:
	lds     si,es:[di]              ;get the address of next header
	cmp     si,-1                   ;end of table
	jz      @f                      ;yes.
	add     di,4                    ;es:di points to prior contents
	swap_a_word                     ;swap the offset
	swap_a_word                     ;swap the segment
	jmp     short @b                ;keep swapping
@@:
	pop     ds                      ;restore
	sti                             ;set back ints on.
	ret

SwapDeviceHeaders  endp
;----------------------------------------------------------------------------;
; GetDeviceChainHeader:                                                      ;
;                                                                            ;
; Returns the head of the device chain in DS:SI - this is DOS version        ;
; dependant and may have to be changed in future.  Windows 3.0 requires DOS  ;
; 3.1 or greater, and all these DOS versions currently have the device chain ;
; header at offset 22h from the address returned by Int 21/52h.              ;
;----------------------------------------------------------------------------;

GetDeviceChainHeader  proc near

	;!!! What about DOS 5+

	lds     si,lpDosData            ;pointer returned by Int 21h/52h
	add     si,22h                  ;add offset to device chain header
	ret

GetDeviceChainHeader  endp
;----------------------------------------------------------------------------;
; IsTimerHooked:                                                             ;
;                                                                            ;
; This routine sets a flag if the INT 08h vector has been hooked. Based      ;
; on this flag we would decide later whether we have to reprogram the timer  ;
; when we switch out. We will make the desicion based on the hook            ;
; segment alone.                                                             ;
;----------------------------------------------------------------------------;

IsTimerHooked   proc near

	pushem  es,bx                   ;save
	and     [MiscFlags],NOT TIMER_HOOKED;assume it is not hooked
	mov     ax,3508h                ;get INT 08 Vector
	call    DosCall                 ;vector in es:bx
	mov     ax,cs                   ;segment of our hook
	mov     bx,es                   ;get current hook segment
	cmp     ax,bx                   ;still our hook ?
	jz      IsTimerHookedRet        ;yes, timer has not been hooked
	or      [MiscFlags],TIMER_HOOKED;assume it is hooked

IsTimerHookedRet:

	popem   es,bx
	ret

IsTimerHooked   endp
;----------------------------------------------------------------------------;
; SaveSoundState:                                                            ;
;                                                                            ;
; This routine saves the state of the sound generating hardware.             ;
;----------------------------------------------------------------------------;

SaveSoundState  proc near

; if the timer has not been hooked, we should not reprogram the timer

	test    [MiscFlags],TIMER_HOOKED;is it hooked ?
	jz      SSS_TimerOK             ;do not reprogram the timer.

; the timer is hooked, we must reprogram the timer.

	xor     cx,cx                   ;timer to be set to 18.2 tics/sec
	call    SetTimer                ;set the timer

SSS_TimerOK:

	call    SaveRestoreSound        ;common processing
	ret                     

SaveSoundState  endp
;----------------------------------------------------------------------------;
; RestoreSoundState:                                                         ;
;                                                                            ;
; This routine restores the state of the sound generating hardware.          ;
;----------------------------------------------------------------------------;

RestoreSoundState proc near

; if the timer has not been hooked by the app, we should not reprogram the
; timer

	test    [MiscFlags],TIMER_HOOKED;is it hooked ?
	jz      RSS_TimerOK             ;no, just speaker port mask

; just another heuristic check. Was the speaker ON, when we switched out ?

	test    speaker,BEEP_ON         ;was it on
	jz      RSS_TimerOK             ;don't reprogram timer

; We have no clue what tha app had programmed the timer too. We are going 
; to jack it up to the rate that basic expects.

	mov     cx,2048                 ;new timer speed
	call    SetTimer                ;speed up the timer

RSS_TimerOK:

; now restore the sound state

	call    SaveRestoreSound        ;common processing
	ret

RestoreSoundState endp
;----------------------------------------------------------------------------;
; SaveRetoreSound:                                                           ;
;                                                                            ;
; Here we save or restore the speaker control value. We play with only the   ;
; low two bits which are the gate control bits and determine on/off status   ;
; of the speaker. Other bits are not touched.                                ;
;----------------------------------------------------------------------------;

SaveRestoreSound  proc  near

	in      al,SpeakerCtl           ;Get current value
	mov     ah,al                   ;save in ah
	xchg    Speaker,al              ;Swap with saved value
	and     ah,11111100b            ;High six bits from current value
	and     al,00000011b            ;Low two bits from saved value
	or      al,ah                   ;all 8 bits
	out     SpeakerCtl,al           ;restore
	ret

SaveRestoreSound endp
;----------------------------------------------------------------------------;
; SetTimer:                                                                  ;
;                                                                            ;
; This routine modifies the timer rate. CX contains the new timer rate for   ;
; timer 0 port.                                                              ;
;----------------------------------------------------------------------------;

SetTimer  proc near

	in      al,mskreg               ;get the timer port value
	or      al,00000001b            ;disable timer interrupt
	jmp     $+2                     ;i/o delay
	out     mskreg,al               ;timer now disabled
	mov     al,cl                   ;low 8 bits new timer rate
	out     timer0,al               ;Set new rate
	jmp     $+2                     ;i/o delay
	mov     al,ch                   ;high 8 bits new timer rate
	out     timer0,al               ;set new rate
	in      al,mskreg               ;get back enable/disable status
	and     al,1111110b             ;set diable bit off
	jmp     $+2                     ;i/o delay
	out     mskreg,al               ;re-enable timer
	ret

SetTimer  endp
;----------------------------------------------------------------------------;
; WaitForKeyFromInt16                                                        ;
;                                                                            ;
; This routine waits for a key to be hit, also checking to see if a context  ;
; switch can be done. It calls the original Int16 code to get the status.    ;
;                                                                            ;
; NOTE: This routine only expects to be called from an Int 16h interrupt     ;
; handler, and ah is expected to have the Int 16h function code (00 = normal ;
; read character, 10h = enhanced read character).                            ;
;                                                                            ;
;----------------------------------------------------------------------------;

WaitForKeyFromInt16  proc near

; If this is a normal read keyboard call, use normal get status, otherwise
; use extended call.

	mov     cs:[Int16GetStatusType],01h
	or      ah,ah
	jz      @f
	mov     cs:[Int16GetStatusType],11h
@@:
	call    SwitchIfPossible        ;do a context switch if appropriate
	call    IsKeyReady              ;is a key ready
	jz      @b                      ;continue waiting.
	ret

WaitForKeyFromInt16  endp

;----------------------------------------------------------------------------;
; WaitForKeyFromDosCalls:                                                    ;
;                                                                            ;
; This routine waits for a key to be hit, also checking to see if a context  ;
; switch can be done. It calls the a dos function to get the status.         ;
; This routine is called when we are waiting for a key from a DOS get key    ;
; call and does INT 28Hs from the idle loop.                                 ;
;----------------------------------------------------------------------------;

WaitForKeyFromDosCalls proc near

@@:
	call    DoInt28                 ;yield, not if in crit. error handler
	call    SwitchIfPossible        ;do a context switch if appropriate
	call    DoDosWait               ;is a key ready
	jz      @b                      ;continue waiting
	ret

WaitForKeyFromDosCalls endp
;----------------------------------------------------------------------------;
; IsKeyReady:                                                                ;
;                                                                            ;
; This key checks to see if a key is ready by doing INT 16 get status calls. ;
; If pasting is being done it will return with zero reset (meaning key ready);
;                                                                            ;
; NOTE: This routine expects the global variable Int16GetStatusType to be    ;
; setup with the type of Int 16h get keyboard status call to make.           ;
;                                                                            ;
;----------------------------------------------------------------------------'

IsKeyReady proc near

	push    ax                              ;save caller's AX
	mov     ah,cs:[Int16GetStatusType]      ;read status call to use

; get the status from BIOS.

; call int 16 code to get status, set a flag to tell the INT 16 hook that if
; this call does get down to our INT 16 handler, it should chain it to BIOS
; (This gives apps who do their own buffering to work)

	pushf                           ;CALL-IRET protocol
	cli                             ;correct simulation
	call    cs:[ActualInt16]        ;chain down

	pop     ax                      ;restore ax
	ret                             ;Z=1 means not ready, = 0 means ready

IsKeyReady endp
;----------------------------------------------------------------------------;
; HookIntVectors:                                                            ;
;                                                                            ;
; This routine hooks the interrupts that the switcher needs to see.          ;
; NOTE: The original state of these vectors have already been saved as a part;
; of the instance data management.                                           ;
;----------------------------------------------------------------------------;

	assumes ds,StubSeg

HookIntVectors proc near

; hook the 16 hardware vectors with a dummy ISR. This will keep track of
; whether the app has finished it's hardware ISR or not. After seting the
; vectors we will treat the new vectors to be the default for all these
; vectors. 

	.errnz  HInt09ISR - HInt08ISR - 18
	.errnz  HInt0AISR - HInt09ISR - 18
	.errnz  HInt0BISR - HInt0AISR - 18
	.errnz  HInt0CISR - HInt0BISR - 18
	.errnz  HInt0DISR - HInt0CISR - 18
	.errnz  HInt0EISR - HInt0DISR - 18
	.errnz  HInt0FISR - HInt0EISR - 18
	.errnz  HInt70ISR - HInt0FISR - 18
	.errnz  HInt71ISR - HInt70ISR - 18
	.errnz  HInt72ISR - HInt71ISR - 18
	.errnz  HInt73ISR - HInt72ISR - 18
	.errnz  HInt74ISR - HInt73ISR - 18
	.errnz  HInt75ISR - HInt74ISR - 18
	.errnz  HInt76ISR - HInt75ISR - 18
	.errnz  HInt77ISR - HInt76ISR - 18
	.errnz  HInt08ChainAddr - HInt08ISR - 8
	.errnz  HInt09ChainAddr - HInt09ISR - 8
	.errnz  HInt0AChainAddr - HInt0AISR - 8
	.errnz  HInt0BChainAddr - HInt0BISR - 8
	.errnz  HInt0CChainAddr - HInt0CISR - 8
	.errnz  HInt0DChainAddr - HInt0DISR - 8
	.errnz  HInt0EChainAddr - HInt0EISR - 8
	.errnz  HInt0FChainAddr - HInt0FISR - 8
	.errnz  HInt70ChainAddr - HInt70ISR - 8
	.errnz  HInt71ChainAddr - HInt71ISR - 8
	.errnz  HInt72ChainAddr - HInt72ISR - 8
	.errnz  HInt73ChainAddr - HInt73ISR - 8
	.errnz  HInt74ChainAddr - HInt74ISR - 8
	.errnz  HInt75ChainAddr - HInt75ISR - 8
	.errnz  HInt76ChainAddr - HInt76ISR - 8
	.errnz  HInt77ChainAddr - HInt77ISR - 8

	mov     cx,16                   ;16 vectors to be set
	mov     dx,StubSegOFFSET HInt08ISR; first vector -- IRQ 0
	mov     al,08h                  ;start with IRQ 0 vector

SetHWVectorLoop:

	mov     si,dx                   ;get a copy in bx
	mov     ah,35h                  ;get vector code
	int     21h                     ;current vector in es:bx
	mov     wptr [si][10],es
	mov     wptr [si][8],bx         ;save current vector
	mov     ah,25h                  ;set vector code
	int     21h                     ;set new vector
	add     dx,18                   ;next ISR
	inc     al                      ;next vector
	cmp     al,16                   ;past the first range ?
	jb      @f                      ;no.
	mov     al,70h                  ;on to the next range
@@:
	loop    SetHWVectorLoop

; get and save the segment of the current INT 1CH ISR. Also save the vector.
; This hook we need to time out pending switch requests that take too long.
; Note: We hook INT 1CH and not INT 08H bacause programs can program the 
; timer to interrupt faster but INT 1CHs are supposed to be generated at
; 18.2 ticks/sec

	mov     ax,351Ch                ;get vector for int 08
	int     21h                     ;es has the segment for the ISR
	mov     wptr [ActualInt1C+2],es
	mov     wptr [ActualInt1C],bx   ;save current vector

; hook the INT 1CH vector. This, to help timeout switch requests
; that take too long to be satisfied

	mov     dx,StubSegOFFSET Int1CISR
	mov     ax,251Ch                ;set code for vector 1C
	int     21h                     ;set vector for int 1C

; now set vectors 9,16,21,23,24 start with 09 also save some current values. 
; Int 09 will not be hooked if the pif file indicated all the hot keys  
; have been disabled

; decide if int 9 is to be hooked or not

	cmp     cs:[WoaHotkeys],00001111b
	jz      @f                      ;all hot keys disabled

; int 09 must be hooked

	mov     ax,3509h                ;code for get int 09 vctor
	int     21h                     ;current vector in es:bx
	mov     wptr [ActualInt09+2],es
	mov     wptr [ActualInt09],bx   ;save current vector

	mov     dx,StubSegOFFSET Int09ISR
	mov     ax,2509h                ;set code for vector 09
	int     21h                     ;set vector for int 09
@@:
	mov     cs:[WoaHotkeyState],0   ;reset hot key flag

; set the INT 15 vector, saving the original value

	mov     ax,3515h                ;code for get int 15 vctor
	int     21h                     ;current vector in es:bx
	mov     wptr [ActualInt15+2],es
	mov     wptr [ActualInt15],bx   ;save current vector

	mov     dx,StubSegOFFSET Int15ISR
	mov     ax,2515h                ;set code for vector 16
	int     21h                     ;set vector for int 16

; set the INT 16 vector, saving the original value

	mov     ax,3516h                ;code for get int 16 vctor
	int     21h                     ;current vector in es:bx
	mov     wptr [ActualInt16+2],es
	mov     wptr [ActualInt16],bx   ;save current vector

	mov     dx,StubSegOFFSET Int16ISR
	mov     ax,2516h                ;set code for vector 16
	int     21h                     ;set vector for int 16

; set the INT 21 vector, saving its original value

	mov     ax,3521h                ;code for get int 21 vector
	int     21h                     ;current vector in es:bx
	mov     wptr [ActualInt21+2],es
	mov     wptr [ActualInt21],bx   ;save current vector

	mov     dx,StubSegOFFSET Int21ISR
	mov     ax,2521h                ;set code for vector 21
	int     21h                     ;set vector for int 21

; int 23 must be hooked

	mov     dx,StubSegOFFSET Int23ISR
	mov     ax,2523h                ;set code for vector 23
	int     21h                     ;set vector for int 23

; int 24 must be hooked

	mov     ax,3524h                ;code for get int 24 vctor
	int     21h                     ;current vector in es:bx
	mov     wptr [ActualInt24+2],es
	mov     wptr [ActualInt24],bx   ;save current vector

	mov     dx,StubSegOFFSET Int24ISR
	mov     ax,2524h                ;set code for vector 24
	int     21h                     ;set vector for int 24

; int 27 must be hooked

	mov     ax,3527h                ;code for get int 27 vector
	int     21h                     ;current vector in es:bx
	mov     wptr [ActualInt27+2],es
	mov     wptr [ActualInt27],bx   ;save current vector

	mov     dx,StubSegOFFSET Int27ISR
	mov     ax,2527h                ;set code for vector 27
	int     21h                     ;set vector for int 27

; int 2f must be hooked. If XMS handling is being done, the handler will 
; hook int 2f again.

	mov     ax,352fh                ;code for get int 2f vector
	int     21h                     ;current vector in es:bx
	mov     wptr [OtherActualInt2f+2],es
	mov     wptr [OtherActualInt2f],bx;save current vector

	mov     dx,StubSegOFFSET OtherInt2fISR
	mov     ax,252fh                ;set code for vector 2f
	int     21h                     ;set vector for int 2f

; now get the vector for INT 0aH and 71h for slave PIC processing at switch
; out time.

	mov     ax,350ah                ;get vector for INT 0Ah
	int     21h                     ;vector in es:bx
	mov     wptr [ActualInt0A+2],es ;save segment
	mov     wptr [ActualInt0A],bx   ;save offset
	mov     ax,3571h                ;get vector for INT 71h
	int     21h                     ;vector in es:bx
	mov     wptr [ActualInt71+2],es ;save segment
	mov     wptr [ActualInt71],bx   ;save offset


; If 'WoaNetAsyncSwitching' is 0, we will need to hook INT 2AH and INT 5CH and
; look for asynchronous network requests.

	cmp     WoaNetAsyncSwitching,0  ;do we need to monitor network ?
	jnz     AllHooksInPlace         ;no.

; int 2A must be hooked

	mov     ax,352Ah                ;code for get int 2A vector
	int     21h                     ;current vector in es:bx

; if the vector is 0:0, do not hook it. Probably this is a redundant test
; and the default vector will never be 0:0 and this is just paranoia code.

	mov     ax,es                   ;check to see if the vector is 0:0
	or      ax,bx                   ;is it 0 ?
	jz      TryToHook5C             ;yes it is, try hooking INT 5CH

; save the current vector and put in our hook.

	mov     wptr [ActualInt2A+2],es
	mov     wptr [ActualInt2A],bx   ;save current vector

	mov     dx,StubSegOFFSET Int2AISR
	mov     ax,252Ah                ;set code for vector 2A
	int     21h                     ;set vector for int 2A

TryToHook5C:

; int 5C must be hooked (only if the default hook is not 0:0)

	mov     ax,355Ch                ;code for get int 5C vector
	int     21h                     ;current vector in es:bx

; it is possible that there is no network software installed on this machine
; and the vector points to 0:0. This happens on some EPSON machines. In this
; case we should not hook the vector.

	mov     ax,es                   ;check to see if it is 0
	or      ax,bx                   ;is it zero ?
	jz      AllHooksInPlace         ;it is, do not put in a hook

; save the current vector and place our hokk over it.

	mov     wptr [ActualInt5C+2],es
	mov     wptr [ActualInt5C],bx   ;save current vector

	mov     dx,StubSegOFFSET Int5CISR
	mov     ax,255Ch                ;set code for vector 5C
	int     21h                     ;set vector for int 5C

AllHooksInPlace:

	ret

HookIntVectors endp
;----------------------------------------------------------------------------;
; SwapMasterPICMask                                                          ;
;                                                                            ;
; This routine swaps the application's Master 8259 Interrupt Mask with       ;
; Window's.                                                                  ;
;                                                                            ;
;----------------------------------------------------------------------------;

SwapMasterPICMask  proc near

	in      al,21h                  ;get the current value
	xchg    al,MasterPICMask        ;swap it
	jmp     short $+2               ;i/o delay
	out     21h,al                  ;restored.

	ret

SwapMasterPICMask  endp

;----------------------------------------------------------------------------;
; OkToSwitch?:                                                               ;
;                                                                            ;
; We will not switch out from within any hardware interrupt handler.         ;
;                                                                            ;
; If the Switcher is disabled because of any reason we will not switch out.  ;
;                                                                            ;
; This routine looks at the InDos flag and the error mode flag and if they   ;
; are both zero, clears carry and gives permission to switch. Else carry is  ;
; set to imply we cannot switch out the old app.                             ;
;                                                                            ;
; If the indos and error flags are not set, this routine tries to get a      ;
; switch out approval from the app (if notification hooks are installed)     ;
;                                                                            ;
; This routine also checks to see if IRQ0 (timer) and IRQ1 (keyboard) are    ;
; unmasked. A 3270 emulation app, PSC actually masks this for a while and    ;
; switching out in this state would be disastrous.                           ;
;                                                                            ;
; The Switch API calls are also made to figure out if it is OK to suspend the;
; app.                                                                       ;
;----------------------------------------------------------------------------;

OkToSwitch?  proc near

; check to see if a we are within any hardware ISR

	cmp     cs:[HardwareIntCount],0 ;inside any hardware ISR
	jnz     OkToSwitchFailNoBeep    ;yes

; check to see if the switcher is disabled.

	cmp     cs:[SwitcherDisabled],0 ;are any bits set ?
	jz      SwitcherNotDisabled     ;no, it's OK to switch.
	jmp     short OkToSwitchFail    ;fail call and beep.

SwitcherNotDisabled:

; check for other conditions that may prevent a switch. 

	call    CheckCriticalRegions    ;sets carry if in some critical region
	jc      OkToSwitchRet           ;switch out not possible

; check to see if we have enough file handles free to complete a context switch

	call    TestForMinFreeHandles   ;are enough file handles available
	jc      OkToSwitchFail          ;not enough free handles to switch out.

; chect to see if it is OK to suspend the app.

	cCall   OkToSuspend?            ;ok to suspend the app ?
	jz      OkToSwitchRet           ;yes.

OkToSwitchFail:

	call    OEMBeep                 ;hoot the hooter.
	mov     cs:[WoaHotKeyState],0   ;can't do it and don't try again later

OkToSwitchFailNoBeep:

	stc                             ;can't do it at this time

OkToSwitchRet:

;----------------------------------------------------------------------------;
; NOTE:                                                                      ;
;       Interrupts are disabled now and will be enabled only after the apps  ;
;       interrupt hooks have been taken out.                                 ;
;----------------------------------------------------------------------------;

	ret

OkToSwitch? endp
;----------------------------------------------------------------------------;
; CheckCriticalRegions:                                                      ;
;                                                                            ;
; Tests for the situations in which it is not possible to switch out.        ;
; These situations are:                                                      ;
;                                                                            ;
;       . IRQ 0 or IRQ 1 are masked.                                         ;
;       . An interrupt is being serviced.                                    ;
;       . Dos 'InDos' flag is set.                                           ;
;       . Critical error handler flag is set.                                ;
;       . The app notifies that it is in a critical region.                  ;
;                                                                            ;
; In all these cases the carry flag would be set before return to imply that ;
; switch  not be serviced.                                                   ;
;                                                                            ;
; In the case where the critical error flag is set, we will also reset the   ;
; the switch request and beep.                                               ;
;----------------------------------------------------------------------------;

CheckCriticalRegions proc near

	pushem  es,bx,ax                ;save registers

; check to see whether IRQ1 and IRQ0 are unmasked. If any of these are 
; masked, we are not going to switch out.

	in      al,21h                  ;get the masks
	and     al,00000011b            ;mask only IRQ0 & 1 bits
	jnz     YesCritical             ;something masked, can't switch now

; do not switch out if any Interrupts are pending

	mov     al,00001011b            ;command byte to read status
	out     20h,al                  ;read status request made
	jmp     $+2                     ;i/o delay
	in      al,20h                  ;get the status
	or      al,al                   ;interrupt pending ?
	jnz     YesCritical             ;yes, can't switch now

; Can't switch if DOS is active

	les     bx,cs:[lpIndosFlag]     ;ES:BX point to indos flag
	cmp     byte ptr es:[bx],0
	jnz     YesCritical

; Can't switch if in a critical error handler, and make user try again later

	les     bx,cs:[lpErrModeFlag]   ;ES:BX points to error flag
	cmp     byte ptr es:[bx],0
	jnz     YesCriticalAbortRequest ;reset the request flag too.

; (Maybe) check with the app itself

	call    CheckWithApp            ;does th app allow switching out ?
	jnz     YesCritical             ;app says no

; Yes, it is okay to switch now!

	clc
	jmp     short CheckCriticalRet  

YesCriticalAbortRequest:

	call    OEMBeep                 ;hoot the hooter.
	mov     cs:[WoaHotKeyState],0   ;can't do it and don't try again later

YesCritical:

	stc                             ;can't do it at this time

CheckCriticalRet:

	popem   es,bx,ax                ;restore saved registers
	ret

CheckCriticalRegions endp
;----------------------------------------------------------------------------;
; IsFileConsole?:                                                            ;
;                                                                            ;
; This routine takes a file handle in BX and traverses the file table to see ;
; if the file name is CON or not. If it is, it sets the 'Z' flag.            ;
;                                                                            ;
; The SFT structure is dos version dependant and that is taken care of here. ;
;                                                                            ;
; The routine must preserve all registers.                                   ;
;----------------------------------------------------------------------------;

IsFileConsole?  proc near

	assumes cs,StubSeg
	assumes ds,nothing
	assumes es,nothing

	cmp     WoaFileEntrySize,0      ;if we don't have a valid SFT entry
	jnz     @f                      ;  size (like when under OS/2), don't
	cmp     bptr WoaFileEntrySize,-1;  grovel for the console, just say
	ret                             ;  it isn't (2nd cmp sets NZ)
@@:
	pushem  ax,si,es,bx             ;save resgister which we thrash                 ;save function
	mov     ax,bx                   ;get handle into ax
	push    ax                      ;Save file handle momentarily
	mov     ah,51H                  ;Get Current PDB code
	call    DosCall                 ;current PDB segment in BX
	mov     es,bx                   ;BX -> ES = Current PDB
	pop     bx                      ;get file handle back

; BX is a entry into the FileIndex table.

	les     si,es:[PDB_JFN_Pointer] ;get pointer to table for 3.00 & above

; ES:SI points to the file index table in the PDB, BX points to an entry in it
; get the entry value, which is the index in the Sytem File Table

	mov     al,es:[bx][si]          ;look up value in PDB_JFN_Table
	cmp     al,80h                  ;special close flag? (windows does this)
	jne     @f                      ;no,continue
	mov     bptr es:[bx][si],0ffh   ;yes,mark closed,& skip rest.

NotConsole:

	or      ax,ax                   ;reset zero flag
	jmp     short IsFileConsoleRet  ;exits with 'NZ', it is not console
@@:
	les     bx,cs:[lpDosData]       ;ES:BX points to Dos's data
	add     bx, 4                   ;ES:BX -> lpFileTable

TraverseSFTlink:

	les     bx,es:[bx+sftLink]      ;get next sft in chain...
	cmp     bx,-1                   ;0ffff in first word marks end of chain
	jz      NotConsole              ;End of chain of tables: return

; test to see if the file index is maintained by this SFT link or node

	sub     al,bptr es:[bx+sftCount];this alloc. has this many files
	jae     TraverseSFTlink         ;not this one, follow the chain

; finally we have located the link which supports this file.

	add     al,bptr es:[bx+sftCount];restore index into this link
	lea     bx,[bx+sftFile]         ;position to start of fcb area
	jz      @f                      ;no adjustment for first fcb in link

LocateCorrectEntry:

	add     bx,WoaFileEntrySize     ;position to next fcb
	dec     al                      ;is this the correct one ?
	jnz     LocateCorrectEntry      ;no, look ahead
@@:

; test for remote files under dos 3 or above

	test    es:[bx+sf_flags],sf_isnet
	jnz     IsFileConsoleRet                ;it is a remote file

; check for the name 'CON '

	cmp     wptr es:[bx+sf_name],"OC"       ;check for 'CON '
	jnz     IsFileConsoleRet                ;not a console device
	cmp     wptr es:[bx+sf_name+2]," N"
	jnz     IsFileConsoleRet                ;not a console driver.

; HACK! We will save a pointer to the SFT entry. In the 'ReadConsoleBuffer'
; routine which traps a 3FH/INT 21H call, we will use the input handle to
; do a write call (40H) to display each character on the output. At that
; time we will change the SFT sf_mode field to ensure that we have write
; access.

	add     bx,sf_mode                      ;es:bx points to sf_mode
	mov     wptr cs:[lp3fMode],bx           ;save offset of address
	mov     wptr cs:[lp3fMode+2],es         ;save segment of address
	xor     al,al                           ;set zero flag, file is CON

IsFileConsoleRet:

	popem   ax,si,es,bx             ;restore saved registers
	ret                     

IsFileConsole? endp
;----------------------------------------------------------------------------;
; GetDosFlags:                                                               ;
;                                                                            ;
; This routine gets and saves long pointers to the InDos flag and the error  ;
; mode flag used by the critical error handler. The location of the InDos    ;
; flag can be obtained by a DOS call, but the location of the ErrorMode flag ;
; is version dependent (but, since Windows 3.0 requires DOS 3.1 or greater,  ;
; the ErrorMode flag is now in the same location):                           ;
;       For Version 3.1 and greater, it is the byte prior to the InDos flag. ;
;----------------------------------------------------------------------------;

GetDosFlags  proc near

; get the address of the InDos flag

	mov     ah,34h                  ;DOS call function
	int     21h                     ;ES:BX points to InDosFlag
	mov     wptr [lpInDosFlag+2],es 
	mov     wptr [lpInDosFlag],bx

	dec     bx                      ;ES:BX now points to ErrorMode flag
	mov     wptr [lpErrModeFlag+2],es
	mov     wptr [lpErrModeFlag],bx

; also get the lptr of the dos data block

	mov     ah,52h                  ;call to get dos data block
	int     21h                     ;es:bx has the dos data block address
	mov     wptr [lpDosData+2],es
	mov     wptr [lpDosData],bx     ;save the address

	ret

GetDosFlags endp
;----------------------------------------------------------------------------;
; This routine save the current dos directory (the pre exec directory).      ;
;----------------------------------------------------------------------------;

SetDosDirectory proc  near

; save the current disk

	mov     ah,19h                  ;get current disk call
	call    DosCall                 ;al has disk id
	mov     DosPreExecDisk,al       ;save it

; now save the current directory

	mov     ah,47h                  ;get current directory code
	mov     dl,0                    ;for the default drive
	mov     si,StubSegOFFSET DosPreExecCD
	inc     si                      ;skip the root symbol
	call    DosCall                 ;current directory obtained

	ret

SetDosDirectory endp
;----------------------------------------------------------------------------;
; SaveWinDosBiosStates:                                                      ;
;                                                                            ;
; This routine saves various dos and bios related state that has to be resto-;
; -red any time we switch back to windows. These states are:                 ;
;                                                                            ;
;       . DOS current directory.                                             ;
;       . IOCTL state for first 5 handles                                    ;
;       . Printer port addresses ( 3 words at 40:8)                          ;
;       . PIC IRQ enable register                                            ;
;                                                                            ;
;----------------------------------------------------------------------------;

SaveWinDosBiosStates  proc near

; get and save the current directory and set the apps directory if needed

	call    SetDosDirectory         ;set up correct directory,saving current

; save the current IOCTL data of the first 5 shared handles

	mov     di,StubSegOFFSET WinIoctlData
	call    SaveIoctlData           ;save the state

; save the printer port information for windows

	mov     di,StubSegOFFSET WinPrinterPorts
	call    SavePrinterPorts        ;save

; save some pointers in installable device driver link

	call    SaveDeviceHeaders       ;old app may add some

; get and set the value of the PIC IRQ enable register.

	in      al,21h                  ;get the register value
	mov     MasterPICMask,al        ;save it

; get and save the slave PIC state as the PreExec state and also as the current
; global SlavePICMask.

	call    ReadSlavePICMask        ;read the mask
	mov     GlobalSlavePICMask,al   ;save it.
	mov     PreExecSlavePICMask,al  ;save it

	ret

SaveWinDosBiosStates  endp
;----------------------------------------------------------------------------;
; RestoreWinDosBiosStates:                                                   ;
;                                                                            ;
; This routine restore some of the dos and bios states for windows before    ;
; switching back to it. The states were saved in the above routine.          ;
;----------------------------------------------------------------------------;

RestoreWinDosBiosStates  proc near

; first reset the directory to what was current prior to exec call

	call    ResetDosDirectory       ;directory restored

; restore the state of the IOCTL data for the first 5 handles

	mov     si,StubSegOFFSET WinIoctlData
	call    RestoreIoctlData        ;IOCTL state restored.

; now restore the printer port addresses in the BIOS data area

	mov     si,StubSegOFFSET WinPrinterPorts
	call    RestorePrinterPorts     ;restore prn port addresses

; restore the device header list.

	call    SwapDeviceHeaders       ;restore the headers

	ret

RestoreWinDosBiosStates  endp
;----------------------------------------------------------------------------;
; The following routine restores the Dos Pre Exec directory after an EXEC    ;
; call is completed.                                                         ;
;----------------------------------------------------------------------------;

ResetDosDirectory proc near

; switch to the preexec drive

	xor     ah,ah                   ;init
	mov     al,DosPreExecDisk       ;initial drive
	mov     dx,StubSegOFFSET DosPreExecCD
	cCall   SetCurrentDiskAndCD,<ax,dx>
	ret

ResetDosDirectory endp
;----------------------------------------------------------------------------;
; ReadSlavePICMask:                                                          ;
;                                                                            ;
; Returns the value of the slave PIC mask in AL. If 'WoaIrq9Global' is       ;
; non zero then no port will be read.                                        ;
;----------------------------------------------------------------------------;

ReadSlavePICMask proc near

	cmp     WoaIrq9Global,0         ;is it disabled ?
	jnz     @f                      ;yes.
	in      al,0a1h                 ;read the slave PIC
@@:
	ret     

ReadSlavePICMask endp
;----------------------------------------------------------------------------;
; WriteSlavePICMask:                                                         ;
;                                                                            ;
; Reprograms the slave PIC mask with the value in AL.                        ;
; If 'WoaIrq9Global' is not zero, no port will be written.                   ;
;----------------------------------------------------------------------------;

WriteSlavePICMask proc near

	cmp     WoaIrq9Global,0         ;is it disabled ?
	jnz     @f                      ;yes.
	sti                             ;let pending ints go through
	out     0a1h,al                 ;write out the mask
@@:
	ret     

WriteSlavePICMask endp
;----------------------------------------------------------------------------;
; ProcessSlavePICMask:                                                       ;
;                                                                            ;
; This routine save the Slave PIC mask state. It then tries to restore the   ;
; Windows PIC state for the slave. However if we find that the App has hooked;
; either INT 0AH (IRQ 2) or INT 71H (IRQ 9) then we will disable the IRQ 9   ;
; bit in the slave.                                                          ;
;----------------------------------------------------------------------------;

ProcessAppSlavePICMask proc near

	push    es

	mov     AppUsesIRQ9,0           ;reset it.
	call    ReadSlavePICMask        ;get the current mask
	mov     AppSlavePICMask,al      ;save it.

	xor     ax,ax
	mov     es,ax
	mov     ax,wptr es:[0Ah*4+2]    ;get INT 0Ah vector segment
	cmp     ax,wptr [ActualInt0A+2] ;is the segment different ?
	jnz     MaskIrq9Off             ;yes, mask IRQ9 off
	mov     ax,wptr es:[0Ah*4]      ;get INT 0Ah vector offset
	cmp     ax,wptr [ActualInt0A]   ;is the offset any different ?
	jnz     MaskIrq9Off             ;yes, mask IRQ9 off

	mov     ax,wptr es:[71h*4+2]    ;get INT 71h vector segment
	cmp     ax,wptr [ActualInt71+2] ;is the segment different ?
	jnz     MaskIrq9Off             ;yes, mask IRQ9 off
	mov     ax,wptr es:[71h*4]      ;get INT 71h vector offset
	cmp     ax,wptr [ActualInt71]   ;is the offset any different ?
	jnz     MaskIrq9Off             ;yes, mask IRQ9 off

; the app has not hooked IRQ 2 or 9, so we will just restore the Windows
; state for the slave PIC.

	mov     al,GlobalSlavePICMask   ;get the mask
	jmp     short OutSlavePicMask   ;reprogram it

MaskIrq9Off:

	mov     al,GlobalSlavePICMask   ;get the mask
	or      al,00000010b            ;disable IRQ 9
	mov     AppUsesIRQ9,0ffh        ;set it.

OutSlavePicMask:

	call    WriteSlavePICMask       ;program the mask

	pop     es
	ret

ProcessAppSlavePICMask endp
;----------------------------------------------------------------------------;
; RestoreSlavePICMask:                                                       ;
;                                                                            ;
; If 'AppUsesIRQ9' is 0ffh, we should restore the apps mask for the slave    ;
; PIC else we should not touch the slave PIC at all.  The mask that is to be ;
; restored is in AL.                                                         ;
;----------------------------------------------------------------------------;

RestoreSlavePICMask proc near

	cmp     AppUsesIRQ9,0ffh        ;is it IRQ 9 owner ?
	jnz     @f                      ;no, do not reprogram IRQ9.
	call    WriteSlavePICMask       ;set the apps mask back.
@@:
	ret

RestoreSlavePICMask endp
;----------------------------------------------------------------------------;
; PressKeyToExit:                                                            ;
;                                                                            ;
; If fDestroy pif setting is set, this routine is set, this routine is a nop ;
; else in text mode it will display the message 'Press Any Key To Exit' on   ;
; the bottom right corner of the screen and wait till user types in a key.In ;
; graphics mode, it will still wait for the key, but will not display any    ;
; message.                                                                   ;
;                                                                            ;
; This routine also flushes the keyboard buffer before doing the read to take;
; away spurious characters from the buffer.                                  ;
;----------------------------------------------------------------------------;

PressKeyToExit  proc    near

	test    WoaBehavior,fDestroy    ;destroy window bit set ?
	jnz     PressKeyToExitRet       ;yes, do not wait

; test for text or graphics mode.

	mov     ah,0fh                  ;get mode call
	int     10h                     ;al has mode
	cmp     al,3                    ;text mode ?
	jbe     DisplayPromptString     ;yes, must prompt user
	cmp     al,7                    ;the other text mode ?
	jz      DisplayPromptString     ;yes,must prompt the user
	jmp     short HitKeyToExit      ;wait for a key stroke

DisplayPromptString:

; display the prompt string on the bottom right corner of the screen. At this
; point AH has the number of coloumns and BH has the active page number.

	pushem  es,bp                   ;save.
	mov     cx,40h                  ;want to access BIOS data area
	mov     es,cx                   ;es points to BIOS data area
	mov     dh,es:[84h]             ;get the last row number

; if the returned values is < 24 use 24. (On some machines like the HP vectra
; BIOS does not maintain the no of lines here so the number will most probably
; be 0.

	cmp     dh,24                   ;25-1?
	jae     @f                      ;40:84h is probably valid, use it.
	mov     dh,24                   ;assume 24 line mode
@@:
	mov     dl,ah                   ;get the last coloumn number
	smov    es,cs                   ;make es point to StubSeg
	mov     cx,PromptStringLength   ;get the length of prompt string
	sub     dl,cl                   ;compute starting coloumn for display
	dec     cx                      ;will not display last space
	mov     bl,70h                  ;will display black text over white
	mov     ax,1301h                ;display string code, move cursor
	mov     bp,StubSegOFFSET PromptString
	int     10h                     ;display the prompt string
	popem   es,bp                   ;restore the registers

HitKeyToExit:

; flush the keyboard buffer.

	mov     ax,0c00h                ;flush keyboard only
	call    DosCall                 ;make a dos call.

; wait for a key stroke, and then exit.

	mov     ah,07h                  ;read character without echo

; at this point do an actual int 21 call so that we will trap it and allow
; an ALT-PRINT-SCREEN to work (if supported).

	int     21h                     ;get the character

PressKeyToExitRet:

	ret

PressKeyToExit  endp
;----------------------------------------------------------------------------;
; This routine resets the keyboard ports and interrupts so that the interrupt;
; is effectively taken care of.                                              ;
;----------------------------------------------------------------------------;

SwallowKey      proc    near

	push    ax                      ;save register
	in      al,61h                  ;read in code
	mov     ah,al                   ;save it
	or      al,80h
	out     61h,al                  
	xchg    ah,al
	out     61h,al
	cli                             
	mov     al,20h                  ;need to output the eoi
	out     20h,al
	sti
	pop     ax                      ;restore
	ret

SwallowKey      endp
;----------------------------------------------------------------------------;
; this function saves all the registers, and here we define equates to access;
; the registers off the stack.                                               ;
;----------------------------------------------------------------------------;

SaveRegisters proc near

; BP is already on stack

	cli                             ;disable interrupts
	pop     cs:[PopRet]             ;save restuen address
	pushem  dx,es,bx,ax,cx,si,di    ;save registers
	and     USER_FLAGLOW,11111110b  ;clears users carry
	push    cs:[PopRet]             ;put back return address
	cld                             ;set proper direction
	sti                             ;enable interrupts
	ret

SaveRegisters  endp
;----------------------------------------------------------------------------;
; This function restores the registers saved by the 'SaveRegisters' call &   ;
; also restores ds and bp which where saved on entry to Int21ISR.            ;
;----------------------------------------------------------------------------;

RestoreRegisters  proc near

	pop     cs:[PopRet]             ;get the return address
	popem   dx,es,bx,ax,cx,si,di    ;restore registers
	pop     ds                      ;restore ds
	pop     bp                      ;restore bp
	push    cs:[PopRet]             ;push back the return address
	ret                             

RestoreRegisters endp
;----------------------------------------------------------------------------;
; StartXms:                                                                  ;
;                                                                            ;
; If an XMS handler has been loaded, it is invoked to set up its tables and  ;
; hook its vectors.                                                          ;
;---------------------------------------------------------------------------;

StartXms proc near

	cmp     WoafXmsInstalled,0      ;is there any xms handler
	jz      StartXmsRet             ;no.

; now invoke it to initialize itself and set up traps

	cCall   XmsInit                 ;invoke it

StartXmsRet:

	ret

StartXms endp
;----------------------------------------------------------------------------;
; PruneNumLines:                                                             ;
;                                                                            ;
; On entry AX has the number of lines that we want to set. Howver if this is ;
; we will parse it such that:                                                ;
;               >= 50 => use 50.                                             ;
;               <  43 => use 25                                              ;
;            else  use 25.                                                   ;
;----------------------------------------------------------------------------;

PruneNumLines proc near

	cmp     ax,50                   ;50 or above
	jae     PruneTo50               ;yes
	cmp     ax,43                   ;less than 43 ?
	jb      PruneTo25               ;yes, use 25
	mov     ax,43                   ;else use 43
	ret

PruneTo50:

	mov     ax,50                   ;50 line mode
	ret

PruneTo25:

	mov     ax,25                   ;25 line mode
	ret

PruneNumLines   endp
;----------------------------------------------------------------------------;
; CheckCallersCS:                                                            ;
;                                                                            ;
; This routine is called right after entering our INT 2AH & INT 5CH hooks &  ;
; it checks the value of the CS on the IRET frame. If the CS is below our CS ;
; or is above 0A000H, it returns with carry clear else carry is set.         ;
;----------------------------------------------------------------------------;

CheckCallersCS proc near

; at this time SS:SP points to <ret addr><iret ip><iret CS>....

	pushem  ax,bp                   ;save

; now SS:SP points to <bp><ax><ret addr><iret ip><iret CS>....

	mov     bp,sp                   ;get the current stack pointer
	mov     bp,[bp][8]              ;get the CS
	mov     ax,cs                   ;get our code seg
	cmp     ax,bp                   ;is callers CS above us ?
	ja      CheckCallersCSRet       ;carry reset, call not from app
	cmp     bp,0a000h               ;is it above 0A000H ?
	jae     CheckCallersCSRet       ;carry reset, call not from app
	stc                             ;call made from the Dos App.

CheckCallersCSRet:

	popem   ax,bp                   ;restore saved registers
	ret

CheckCallersCS endp
;----------------------------------------------------------------------------;
; TestForMinFreeHandles:                                                     ;
;                                                                            ;
; This routine gropes though the SFT nodes and tries to see if atleast       ;
; WOA_MIN_FILEHANDLES number of handles are available. If not carry flag is  ;
; set. These are the number of handles that are needed to ensure that WOA    ;
; will be able to switch out of this app and either get back to Windows or   ;
; start another Dos app.                                                     ;
;----------------------------------------------------------------------------;

TestForMinFreeHandles proc near

	pushem  es,bx,ax,cx,di          ;save
	xor     ax,ax                   ;no of free handles obt. thusfar
	les     bx,cs:[lpDosData]       ;ES:BX points to Dos's data
	les     bx,es:[bx][4]           ;es:bx -> first sft node

WalkSFTNodes:

	mov     cx,es:[bx+sftCount]     ;get the no of links in this node.
	lea     di,[bx+sftFile]         ;es:di points to the first link

WalkSFTLinks:

	cmp     bptr es:[di+sf_ref_count],0 ;is this link free ?
	jnz     @f                      ;no.
	inc     ax                      ;one more free entry obtained.
	cmp     ax,WOA_MIN_FILEHANDLES  ;do we enough ?
	jae     TestForHandlesRet       ;yes, carry clear
@@:
	add     di,cs:[WoaFileEntrySize];onto the next link
	loop    WalkSFTLinks            ;keep searching in this node.

; look at the next node if one exists.

	les     bx,es:[bx+sftLink]      ;get next sft in chain...
	cmp     bx,-1                   ;0ffff in first word marks end of chain
	jnz     WalkSFTNodes            ;search this node.

; not enough file handles are free, we must return with carry set.

	stc                             ;error return

TestForHandlesRet:

	popem   es,bx,ax,cx,di          ;save
	ret

TestForMinFreeHandles endp
;----------------------------------------------------------------------------;
; GetXmsHandler:                                                             ;
;                                                                            ;
; This function sets up the address of the actual XMS handler.               ;
;----------------------------------------------------------------------------;

GetXmsHandler   proc near

; check to see if XMS installed or not.

	mov     XmsFlag,0               ;assume not installed
	mov     ax,4300h                ;installation check.
	int     2fh
	cmp     al,80h                  ;is it installed
	jnz     GetXmsHandlerRet        ;no.
	mov     XmsFlag,0ffh            ;XMS is installed

	pushem  es,bx                   ;save
	mov     ax,4310h                ;get XMS handler address call
	int     2fh                     ;handler address is in ES:BX
	mov     wptr [ActualXms+2],es   ;save segment of original handler
	mov     wptr [ActualXms],bx     ;save offset of actual handler
	popem   es,bx                   ;restore

GetXmsHandlerRet:

	ret

GetXmsHandler   endp
;----------------------------------------------------------------------------;
; SetCorrectA20State:                                                        ;
;                                                                            ;
; If HIMEM is installed, Windows is always going to work with A20 on. The app;
; may however turn it off on its own (Paradox386 does it on some machines).  ;
; To restore the state of the A20 line we will do a 'LocalEnableA20' followed;
; by a 'LocalDisableA20'.                                                    ;
;                                                                            ;
; The assumption here is that as Windows operates with A20 on, the A20 enable;
; count in HIMEM will be 1 atleast and the 'LocalEnableA20' will increment it;
; and thus 'LocalDisableA20' will never set it off. However, if the app did  ;
; actually set A20 off, then 'LocalEnableA20' will set it back on.           ;
;                                                                            ;
; All these must be done only if HIMEM is installed, XmsFlag will have 0ffh  ;
; in this case.                                                              ;
;----------------------------------------------------------------------------;

SetCorrectA20State  proc near

	cmp     XmsFlag,0ffh            ;is HIMEM installed ?
	jnz     SetCorrectA20StateRet   ;no himem.

; do a 'LocalEnableA20' followed by a 'LocalDisableA20'.

	mov     ah,05h                  ;LocalEnableA20
	call    [ActualXms]             ;A20 set on if it was off.
	mov     ah,06h                  ;LocalDisableA20
	call    [ActualXms]             ;cancell prev call, A20 stays on

SetCorrectA20StateRet:

	ret

SetCorrectA20State endp
;----------------------------------------------------------------------------;
; DoInt28:                                                                   ;
;                                                                            ;
; This routine does INT 28H calls and is called from the Dos read keyboard   ;
; trap routines. However we will do the INT 28H only if the critical error   ;
; flag is reset. This is because some TSRs (like LANMAN) may have INT 28H    ;
; hooked and they might do other DOS calls from their hook. This should not  ;
; happen while we are in the critical error handler.                         ;
;----------------------------------------------------------------------------;

DoInt28 proc near

	pushem  es,bx                   ;save
	les     bx,cs:[lpErrModeFlag]   ;ES:BX points to error flag
	cmp     byte ptr es:[bx],0      ;are we in the critical error handler ?
	jnz     @f                      ;yes, don't do INT 28H
	int     28h                     ;ok to do the INT 28h
@@:
	popem   es,bx                   ;restore
	ret

DoInt28 endp
;----------------------------------------------------------------------------;
; DisableInt15Mouse:                                                         ;
;                                                                            ;
; This routine disables the INT 15H mouse if a flag says that the event      ;
; handler is in local memory.                                                ;
;----------------------------------------------------------------------------;
DisableInt15Mouse proc near

	cmp     LocalInt15MouseHandler,0;is there a local handler ?
	jz      DisableInt15MouseRet    ;no.
	pushem  ax,bx                   ;save
	mov     ax,0c200h               ;enable/disable code
	xor     bh,bh                   ;disable
	int     15h                     ;disable the mouse.
	popem   ax,bx                   ;restore

DisableInt15MouseRet:

	ret

DisableInt15Mouse endp
;----------------------------------------------------------------------------;
; EnableInt15Mouse:                                                          ;
;                                                                            ;
; This routine enables the INT 15H mouse if a flag says that the event       ;
; handler is in local memory.                                                ;
;----------------------------------------------------------------------------;
EnableInt15Mouse proc near

	cmp     LocalInt15MouseHandler,0;is there a local handler ?
	jz      EnableInt15MouseRet     ;no.
	pushem  ax,bx                   ;save
	mov     ax,0c200h               ;enable/disable code
	mov     bh,1                    ;enable
	int     15h                     ;disable the mouse.
	popem   ax,bx                   ;restore

EnableInt15MouseRet:

	ret

EnableInt15Mouse endp
;----------------------------------------------------------------------------;
; OEMBeep:                                                                   ;
;                                                                            ;
; This routine taken from the grabber sources hoots the hooter.              ;
;----------------------------------------------------------------------------;

OEMBeep         proc    near

	pushem  ax,cx                   ;save registers to be destroyed
	mov     al,0B6H                 ;select timer 2
	out     PIT_PORTD,al
	mov     ax,BEEP_TONE1           ;divisor for tone 1
	out     PIT_PORTC,al            ;write timer 2 count - lsb
	mov     al,ah
	out     PIT_PORTC,al            ;write timer 2 count - msb
	in      al,PPI_PORTB            ;get current setting of port
	mov     ah,al                   ;save setting
	or      al,00000011b            ;turn speaker on
	out     PPI_PORTB,al
	mov     cx,BEEP_TIME1           ;wait awhile
	loop    $
	mov     cx,BEEP_TONE2           ;divisor for tone 2
	mov     al,cl
	out     PIT_PORTC,al
	mov     al,ch
	out     PIT_PORTC,al
	mov     cx,BEEP_TIME2           ;wait again
	loop    $
	mov     al,ah
	out     PPI_PORTB,al
	popem   ax,cx                   ;restore saved registers
	ret

OEMBeep         endp

;----------------------------------------------------------------------------;
sEnd    StubSeg

end
