;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; This is the main code that controls most of the activities in WINOLDAP.    ;
; (Please refer to the documentation file WOA.DOC for a detailed description ;
; of the entire process )                                                    ;
;                                                                            ;
; History:                                                                   ;
;                                                                            ;
;        Fri June-15-1990.      -by-  Amit Chatterjee [amitc]                ;
;        Adapted for the Dos Task Switcher.                                  ;
;                                                                            ;
;        Tue June-20-1989.      -by-  Amit Chatterjee [amitc]                ;
;        Created for Windows. (Added the History legend)                     ;
;----------------------------------------------------------------------------;


	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	include woagrab.inc
	include macros.mac
	include njmp.mac
	include woaerr.inc
	include woakeys.inc
	include woaswch.inc
	.list

	.8086                           ;must have to run on 8086s too.

;----------------------------------------------------------------------------;
; define external function calss.                                            ;
;----------------------------------------------------------------------------;

	;-------------------------------------------------------;
	; external FAR OLDAPP procedures.                       ;
	;-------------------------------------------------------;

	externFP InitializeWoa                  ;(WOAINIT.ASM)
	externFP GetSwitcherInfo                ;(WOAINIT.ASM)
	externFP ParseCommandLine               ;(WOAINIT.ASM)

ifdef	JAPAN
	externFP KkcBusyCheck			;(WOAKKC.ASM)
endif

;----------------------------------------------------------------------------;

sBegin  Data

;----------------------------------------------------------------------------;
; define the global variables defined elsewhere                              ;
;----------------------------------------------------------------------------;

externW SwitcherStackTop                ;top of switchers stack
externB DataSegEnd                      ;end of _DATA segment
externB CodeSegEnd                      ;end of _TEXT segment
externB StubSegEnd                      ;end of _WOARLMSEG segment
externD lpXmsControl                    ;XMS control function address

	;-------------------------------------------------------;
	; define any locally used constants                     ;
	;-------------------------------------------------------;
		public  IS_WINOLDAP_ACTIVE

	INT_COM1                equ     0bh
	INT_COM2                equ     0ch
	MAXPATHLENGTH           equ     80
	IS_WINOLDAP_ACTIVE      equ     4680h

	;-------------------------------------------------------;
	; define any external constants.                        ;
	;-------------------------------------------------------;


;---------------------------------------------------------------------------;
; now define the other global variables that will be needed.                ;
;---------------------------------------------------------------------------;
		
		public DosAppSwapFileName
		public SwapBlock
		public CommArea
		public RS232Area
		public RestoreSwapBlock
		public GetDosAppSwapFileName
		public GetDosAppSwap1FileName
		public GetDosAppSwap2FileName
		public AppendUniqueNumber
		public FarAppendUniqueNumber
		public OldAppManagerEnd
		public OldAppManagerRet
		public ErrorHandler
		public FileTemplate
		public WoaSwap1Path
		public WoaSwap2Path
		public WoaSwapFilePrefix
		public SwitcherColors   

;-----------------------------------------------------------------------------;

globalB SwitcherID,0                    ;ID of this switcher
globalW AppUsesXMS,0                    ;app uses XMS or not.
globalB NodeToSwitchTo,-1               ;node to switch to for dir. hotkeys
globalW StartScreenLines,0              ;start up no of screen lines
globalW hApp,0                          ;app id
globalB StartRestartId,0                ;start or restart
globalW AppMinMem,0                     ;minimum mem
globalW AppMinXmsK,0                    ;minimum xms required
globalW AppMaxXmsK,0                    ;maximum xms required
globalW DosAppNumber,0                  ;serial app number
globalW ArenaWalkSel,?                  ;selector used for walking the arenas
globalW ArenaRWSel,?                    ;selector used to read/write arenas
globalW XmsHeapWalkSel,?                ;walks the XMS heap
globalB LowMemBlockType,?               ;type of the low mem dos block
globalW WoaCsSize,?                     ;woa code segment size
globalW WoaDsSize,?                     ;woa data segment size
globalW StubSegSize,?                   ;size of stub segment
globalW WoaStubParaSize,?               ;size of WOA stub in paras
globalD SwapFileOffset,?                ;start sekk offset in swap file
globalB ErrorType,0                     ;WOA error type
globalB ErrorSubType,0                  ;any subtypes
globalW WinMainSP,?                     ;save entry sp here
globalW CurrentDosSwapSeed,?            ;swap file seed being used now
globalB CurrentPathId,?                 ;oath id for current app
globalW AppDead,0                       ;app dead or alive indicator
globalW WoaStubSel,?                    ;selector where stub is to be found
globalB WinSaveXms,0                    ;tells whether win or app saves XMS
globalB ReturnToShellCode,0             ;code returned to shell
globalW HighMemXmsHandle,?              ;handle of the high heap XMS block
globalD XmsBankSize,?                   ;size of apps XMS bank
globalW LowMemParaSize,0                ;size of available low mem
globalD HighMemSize,0                   ;size of available xms memory
globalW LowMemArenaSel,0                ;sel to arena of available mem
globalW LowMemSel,0                     ;sel to start of available mem
globalW UsableLowHeapSel,0              ;?
globalB XmsFlag,0                       ;XMS present or not.
globalW Swap1MinK,0                     ;min size to be left on 1st swap path
globalW Swap2MinK,0                     ;min size to be left on 2nd swap path
globalB DiskSwap1Drive,?                ;fast swap drive for winoldap
SwitcherColors  label   byte            ;color for the switcher screen
	db      ?                       ;for the desk top back ground
	db      ?                       ;for the title bar
	db      ?                       ;text foreground

; define the registers that we need to communicate with the stub.

globalW RealMode_AX,0                   ;storage for AX
globalW RealMode_BX,0                   ;storage for BX
globalW RealMode_CS,0                   ;storage for CS
globalW RealMode_DS,0                   ;storage for DS
globalW RealMode_ES,0                   ;storage for ES
globalW RealMode_IP,0                   ;storage for IP

	;------------------------------------------------------;
	; define a chain of areas to swap out from low memory  ;
	;                                                      ;
	; SwapBlock defines an array of nodes with 4 fields:   ;
	;      . size of swap area (in words)                  ;
	;      . segment                                       ;        
	;      . offset in segment                             ;
	;      . long pointer to area where the data is to be  ;
	;        saved.                                        ;
	; A size of 0, marks the end of the structure.         ;
	;------------------------------------------------------;


SwapBlock       label word

		dw      (SIZE CommArea) / 2     ;BIOS comm area
		dw      4f0h                    ;at 0:4f0h
		dw      0                       ;seg 0
		dw      DataOffset CommArea     
		dw      SEG CommArea

	
		dw      (8*4)/2                 ;INT2-INT9 vector area size
		dw      2*4                     ;offset in 0:
		dw      0                       ;seg 0
		dw      DataOffset Int2to9      ;save here
		dw      SEG Int2to9

		dw      (SIZE RS232Area) / 2    ;port addresses in BIOS area
		dw      0                       ;offset at 0
		dw      40h                     ;segment at 40:
		dw      DataOffset RS232Area    ;save here
		dw      SEG RS232Area

		dw      0                       ;mark end of swap block
		
; define the known save areas

Int2to9         label   word

		dd      6 dup (?)               ;area for vectors 2-7
IntVect8        dd      ?                       ;area for vector 8
IntVect9        dd      ?                       ;area for vector 9

CommArea        db      50 dup (?)              ;save 0:400h - 0:521h
RS232Area       dw      4 dup (?)               ;4 words at 40:0

;----------------------------------------------------------------------------;
; now define two areas where we will hold the swap file for the windows heap ;
; file and the next dos app that we want to handle.                          ;
;----------------------------------------------------------------------------;

DosAppSwapFileName      db      80  dup (0)     ;leave 80 bytes for name
FileTemplate            db      80  dup (0)     ;for deleting files
WoaSwapFilePrefix       db      '~DOS',0        ;all swap file prefix
WoaSwap1Path            db      68  dup (0)     ;first swap file path
WoaSwap2Path            db      68  dup (0)     ;second swap file path
WinEmsSaveArea          db      256 dup (?)     ;save windows EMS context here

;----------------------------------------------------------------------------;
; define all the variables that can be clubbed together and passed on to the ;
; stub segment in one 'rep movs' instruction.                                ;
;                                                                            ;
; ***************************CAUTION**************************************** ;
; The definition or the order of the variables in the following block should ;
; not be altered unless the changes are duplicated for the corresponding     ;
; variables in 'WOARLM.ASM' (declared below)                                 ;
; ************************************************************************** ;
;----------------------------------------------------------------------------;

StubData label byte

globalB fBreak,?                            ;break status
globalB DiskSwap2Drive,?                    ;slow swap drive for winoldap
globalW WoaPDBSeg,0                         ;segment value for the above pdb
globalB EmsFlag,0                           ;EMS present or not.
globalW Int15UsershApp,0                    ;hApp of INT 15 user
globalW SegResizeBlock,?                    ;block to resize in real mode
globalW SizeReservedArea,?                  ;any reserved area ?, it's para size

StubDataLength equ $ - StubData
;----------------------------------------------------------------------------;
						

sEnd    Data

;----------------------------------------------------------------------------;
; now declare the existence of the realmode stub segment and the variables   ;
; that we want to reference here.                                            ;
;----------------------------------------------------------------------------;

createSeg   _WOARLMSEG,StubSeg,word,public,code
sBegin  StubSeg

;----------------------------------------------------------------------------;
; The following structure will be loaded from the corresponding variables in ;
; the 'StubData' structure defined in the main 'Data' segment. The variables ;
; in the structure will be transferred by a single 'rep movs' instruction.   ;
;----------------------------------------------------------------------------;

externB WoaStubData                     ;the structure which hold varaibles
externB WoaFcb1                         ;first FCB (defined in WoaStubData)
externB WoaFcb2                         ;second FCB (defined in WoaStubData)
externB WoaParams                       ;(defined in WoaStubData)

;----------------------------------------------------------------------------;
; Declare the rest of the varaibles that are needed by the stub code. These  ;
; will have to be loaded individually as it is not possible to arranges the  ;
; corresponding variables in the main data segment contiguously.             ;
;----------------------------------------------------------------------------;

externW XmsBaseHandle                           ;extended memory block handle
externD AppXmsSize                              ;size of apps XMS area
externD AppXmsBase                              ;base of apps XMS area
externW WoahApp                                 ;handle of the window
externB WoafXmsInstalled                        ;NZ if XMS code installed
externW WoaAppNumber                            ;serial number of the app
externW WoaParamBlock                           ;EXEC parameter block
externB WoaBehavior                             ;the behaviour bits
externB WoaHotkeys                              ;the hot key disable flags
externW WoaStubSize                             ;length of stub

;----------------------------------------------------------------------------;
sEnd    StubSeg

;----------------------------------------------------------------------------;
; we define the switch_cs macro here.                                        ;
;----------------------------------------------------------------------------;

switch_cs macro
	local   _x
	local   _y
_x      proc    far
	lea     ax,_y
	push    ax
	ret
_y:
_x      endp
	endm
;----------------------------------------------------------------------------;

sBegin  Code

	assumes cs,Code
	assumes ds,Data
	assumes es,nothing

;----------------------------------------------------------------------------;
; declare the winoldap external function calls.                              ;
;----------------------------------------------------------------------------;

	;-------------------------------------------------------;
	; declare public routine names referenced elsewhere     ;
	;-------------------------------------------------------;

	public  CopyBasicSwap1FileName
	public  CopyBasicSwap2FileName
	public  FarCopyBasicSwap1FileName
	public  FarCopyBasicSwap2FileName
	
	;-------------------------------------------------------;
	; external NEAR OLDAPP procedures.                      ;
	;-------------------------------------------------------;

	externNP CreateFile                     ;(WOAFILE.ASM)
	externNP OpnFile                        ;(WOAFILE.ASM)
	externNP CloseFile                      ;(WOAFILE.ASM)
	externNP DeleteFile                     ;(WOAFILE.ASM)
	externNP ReadFile                       ;(WOAFILE.ASM)
	externNP WriteFile                      ;(WOAFILE.ASM)
	externNP LseekFile                      ;(WOAFILE.ASM)
	externNP SetNormalAttributes            ;(WOAFILE.ASM)
	externNP SetHiddenAttributes            ;(WOAFILE.ASM)
	externFP RealModeWoa                    ;(WOARLM.ASM)   
	externNP BackFromContextSwitch          ;(WOARLM.ASM)
	externNP ErrorManager                   ;(WOAERR.ASM)
	externNP SwitchManager                  ;(WOASWCH.ASM)
	externNP GetSelectorBase                ;(WOAPMRM.ASM)
	externNP SetSelectorBaseLim64           ;(WOAPMRM.ASM)
	externNP SaveWinEmsContext              ;(WOAUTILS.ASM)
	externNP SaveAppXmsContext              ;(WOAPMRM.ASM)
	externNP GetRealModeSeg                 ;(WOAPMRM.ASM)
	externNP ShuffleSegments                ;(WOAPMRM.ASM)
	externNP InvokeStubCode                 ;(WOAPMRM.ASM)
	externNP ErrorManager                   ;(WOAERR.ASM)
	externNP GetSegSize                     ;(WOAPMRM.ASM)
	externNP SaveDosMemory                  ;(WOAUTILS.ASM)
	externNP GetBackAppXmsNeeds             ;(WOAUTILS.ASM)
	externNP RestoreWinEmsContext           ;(WOAUTILS.ASM)
	externNP RestoreXmsAndDosAllocatedBlocks;(WOAUTILS.ASM)
	externNP MoveWoaIntoStubArea            ;(WOAPMRM.ASM)
	externNP GetAppXmsBase                  ;(WOAPMRM.ASM)
	externNP RestoreSwappedGroups           ;(WOAPMRM.ASM)
	externNP AppSwapOutErrHandler           ;(WOAUTILS.ASM)
	externNP ResetClassInt15Word            ;(WOAUTILS.ASM)
	externNP SetInt15Word                   ;(WOAUTILS.ASM)
	externNP GetInt15Word                   ;(WOAUTILS.ASM)
	externNP GetNSetCtrlCFlag               ;(WOAUTILS.ASM)
	externNP RestoreCtrlCFlag               ;(WOAUTILS.ASM)
	externNP RecoverWindows                 ;(WOAERR.ASM)
	externNP DeleteFilesAndResources        ;(WOAERR.ASM)
	externNP GetSwapFileInformation         ;(WOAINIT.ASM)
	externNP DeleteApp                      ;(WOASWCH.ASM)
	externNP AppToTheTop                    ;(WOASWCH.ASM)
	externNP UpdateExitCode                 ;(WOASWCH.ASM)
	externNP WhichSwapPathToUse             ;(WOAUTILS.ASM)

ifdef	JAPAN
	externNP GetKkcStateSize		;(WOAKKC.ASM)
	externNP DisableKkc			;(WOAKKC.ASM)
	externNP EnableKkc			;(WOAKKC.ASM)
	externNP SaveKkcState			;(WOAKKC.ASM)
	externNP RestoreKkcState		;(WOAKKC.ASM)
endif

;----------------------------------------------------------------------------;


cProc   OldAppManager,<PUBLIC,FAR,PASCAL>,<si,di,bp>

cBegin

	cld                             ;do not take chances with this

; parse the command line. If DOSSWAP.EXE has been invoked with a command 
; line we should print a help message and exit.

	call    ParseCommandLine        ;is there a command line ?
	njc     OldAppManager4CExit     ;yes, exit 

; switch to a local stack.


; load ds

	mov     ax,DGROUP
	mov     ds,ax                   ;load proper ds
	assumes ds,Data

	cli
	mov     ss,ax
	mov     sp,DataOFFSET SwitcherStackTop;SS:SP -> top of switcher stack
	sti

; get info from the switcher data structure

	cCall   GetSwitcherInfo         
	njc     OldAppManagerEnd        ;error during initialization
		   
	mov     ax,hApp                 ;get the app id
	mov     CurrentDosSwapSeed,ax   ;save it

	cmp     StartRestartId,0        ;start the app
	jz      @f                      ;yes, have to launch it first
	mov     ax,1                    ;restart app context switch
	save    <ax>                    ;save
	cCall   InitializeWoa,<ax>      ;initialize for restart
	njc     OldAppManagerEnd        ;error during initialization

ifdef	JAPAN
	cCall	KkcBusyCheck		; KKC busy ?
	jnc	KkcNotBusy		; no
	mov	ErrorType,ER_APP_SWAP_IN ; set error type 'app swap in error'
	jmp	OldAppManagerEnd	;
KkcNotBusy:
	cCall	GetKkcStateSize		;
endif

	call    ContextSwitch           ;ready to restatrt app
	jmp     short invoke_real_mode_stub
@@:


; call the Initialization routine to set up various variables. This routine
; is in a separate code segment which will be ignored after the initialization
; is done.

	xor     ax,ax                   ;starting an app
	cCall   InitializeWoa,<ax>      ;initialize variables
	njc     OldAppManagerEnd        ;error during initialization

ifdef	JAPAN
	cCall	GetKkcStateSize		;
endif

; get the current int 15 users ID

	cCall   GetInt15Word            ;get the value of the word.
	mov     Int15UsershApp,ax       ;save it.

; at this point we must get the current CTRL+C flag and set it off.

	cCall   GetNSetCtrlCFlag        ;get current one and set it off.

;----------------------------------------------------------------------------;
; save the relevant interrupt vectors and the portions of the BIOS data area ;
; that have been linked into the SwapBlock.                                  ;
;----------------------------------------------------------------------------;

	call    SaveSwapBlock           ;save the areas needed in global heap

	mov     WinSaveXms,0ffh         ;need to save extended memory image

; swap windows out now.

	mov     ax,1                    ;all WOA segments to be relocated
	cCall   SwapWindowsOut,<ax>     ;swap the low heap to disk
	jnc     NoSwapError             ;no error

; delete all files and allocated selectors and terminate this instance.

	call    DeleteFilesAndResources ;free selectors etc.
	xor     ah,ah                   ;error code will be in AX
	mov     al,ER_WND_SWAP_OUT      ;Windows swap out failure
	jmp     short OldAppManagerEnd  ;return back

NoSwapError:

;----------------------------------------------------------------------------;
; And as we want a fresh app to be loaded, we set the AX value to be passed  ;
; to the stub to be 0                                                        ;
;----------------------------------------------------------------------------;

	mov     RealMode_AX,0

invoke_real_mode_stub:

	cCall   InvokeStubCode          ;invokes the stub code
	jnc     NoRealModeError         ;there was no error

; if AH is 0, the error is an exec error, else it is some other error

	or      ah,ah                   ;is it an exec error ?
	jz      ExecError               ;yes, it is

; we had some other error, like grabber load failure etc
	
	mov     ErrorType,ah            ;save the error code
	jmp     ErrorHandler            ;exit from here

ExecError:

	mov     ErrorSubType,al         ;save exec error code
	mov     ErrorType,ER_EXEC_FAILS ;exec failure error code
	jmp     ErrorHandler            ;get out and display error

NoRealModeError:

; the value of BX in the client register frame will tell us if any instance 
; has int 15 hooked and thus locked.

	mov     bx,RealMode_BX
	mov     Int15UsershApp,bx       ;save it

; if AX in the register frame at this point is !0 we have to do a context switch
; to some other application.

	mov     AppDead,0               ;assume app still alive
	cmp     RealMode_AX,0
	jz      @f                      ;old app finished execution

	xor     ax,ax                   ;full fleged context switch
	call    ContextSwitch           ;go to windows & stay till switch back

	jmp     short invoke_real_mode_stub

@@:

; we are back in protected mode and are executing in high heap so we can safely
; load windows back.

	cCall   SwapWindowsIn           ;windows restored to original state

;----------------------------------------------------------------------------;
; windows area has been restored, so restore the swap block and ask kernel to;
; regain whatever trap vectors it needs.                                     ;
;----------------------------------------------------------------------------;

	call    RestoreSwapBlock        ;restore the areas that we had saved

; re-enable kernels dos traps, before this we must return the state of the
; CTRL+C flag. 

	cCall   RestoreCtrlCFlag        ;resturn original state

; if any one is using INT 15 then set the class word.

	mov     ax,Int15UsershApp       ;get the id
	or      ax,ax                   ;no one using it ?
	jz      @f                      ;no one.
	cCall   SetInt15Word,<ax>       ;set the INT 15 id 
@@:

; if the app has been using INT 15, reset the class word

	cCall   ResetClassInt15Word,<hApp>

;----------------------------------------------------------------------------;
; mark this app as deleted in the global structure. It is very important that;
; we do this after we have gone past all areas where we can get an error.    ;
;----------------------------------------------------------------------------;

	mov     ax,CurrentDosSwapSeed   ;load the current dos app number
	cCall   DeleteApp,<ax>          ;delete the app.

; we are going back to the shell, we must maintain the integrity of the Z 
; ordering by making sure that the shell task is at the top of the task
; list. At this point that may not be true. The Z-ordering should be updated
; as if we are doing a directed hot ket switch to the shell.

	xor     ax,ax                   ;want the shell to be on top
	cCall   AppToTheTop,<ax>        ;make shell the top task

OldAppManagerRet:

	xor     ax,ax                   ;normal return path
	mov     ErrorType,al            ;initialize for no error
	mov     ErrorSubType,al         ;no error

OldAppManagerEnd:

; if the XMS block has been allocated, and there is only one app in the list
; (the shell) then unlock and release the block. This should also be done 
; if there are just two apps on the list and we have an error.

	cmp     HighMemXmsHandle,0      ;was it allocated ?
	jz      XmsBlockDone            ;no.
	mov     ax,4a05h                ;opcode
	mov     si,CGET_LIST_LENGTH     ;get the no of programs.
	int     2fh                     ;dx:ax has the long pointer
	or      ax,ax                   ;0 means 1
	jz      XmsBlockRelease         ;realease the xms block.
	cmp     ax,1                    ;just 2 apps left (including the shell)
	ja      XmsBlockDone            ;no, do not bother about XMS block
	cmp     ErrorType,0             ;did we have an error ?
	jz      XmsBlockDone            ;no, so really one old app is still alive

XmsBlockRelease:

; we must unlock and release the block. 

	mov     ah,0dh                  ;unlock xms block
	mov     dx,HighMemXmsHandle     ;handle of the allocated block
	call    lpXmsControl            ;AX has largest free block size in K

	mov     ah,0ah                  ;free xms block code.
	mov     dx,HighMemXmsHandle     ;handle of the allocated block
	call    lpXmsControl            ;AX has largest free block size in K

; update the values in the global list structure.

	pushem  es,di                   ;save
	mov     ax,4a05h                ;opcode
	mov     si,CGET_GLOBAL_SWITCH_DATA
	int     2fh                     ;dx:ax has the long pointer
	mov     es,dx                   ;load it into es
	mov     di,ax                   ;es:di -> info structure
	mov     wptr es:[di].XMS_Handle,0;no valid handle
	mov     wptr es:[di][0].XMS_Size,0
	mov     wptr es:[di][2].XMS_Size,0
	popem   es,di                   ;restore

XmsBlockDone:

; stuff in the exit code.

	cCall   UpdateExitCode,<ErrorType,ErrorSubType>

OldAppManager4CExit:

	mov     ax,4c00h
	int     21h

cEnd

;----------------------------------------------------------------------------;
;                       UTILITY ROUTINES                                     ;
;----------------------------------------------------------------------------;

;----------------------------------------------------------------------------;
; ContextSwitch:                                                             ;
;                                                                            ;
;   This routine is responsible for either switching to windows, or to the   ;
;   next or previous oldapp.                                                 ;
;                                                                            ;
;   The gist of the routine is as follows:                                   ;
;                                                                            ;
;          . swaps out part of all of memory which was occupied by windows   ;
;            low heap. This has the outgoing dos apps context                ;                                                       ;
;          . If switch is to an old app, determine which one to run and then ;
;            change the dosappnumber appropriately so that the correct swap  ;
;            file will be handled. If a next/previous dos app is found, then ;
;            jmp to label CS1:, else continue.
;          . swaps in windows low heap, enables kernel interrupt trappings & ;
;            enables the OEM layers. Restores real mode IDT                  ;
;          . sets a global flag indicating that the old app is an iconic     ;
;            state and goes into a loop, geting, translating and dispatching ;
;            windows messages. It stays here till user opens the old app     ;
;            icon which causes the above indicator to be reset.              ;
;          . disables all the OEM layers.                                    ;
;          . disables kernel trapping of the interrupt vectors .             ;
;          . saves real mode IDT                                             ;
;          . swaps windows out                                               ;
;       CS1:                                                                 ;
;          . swaps the old app in                                            ;
;          . calls back to real mode stub, setting up SS & SP to point to    ;
;            initial real mode stack and transfer control to the label named ;
;            'BackFromContextSwitch'                                         ;
;----------------------------------------------------------------------------;

ContextSwitch proc near

	cld                             ;do not take chances with this
	or      ax,ax                   ;first time start ?
	jz      @f                      ;yes
	jmp     short StartAgain        ;no, restart the app
@@:
	mov     ax,RealMode_AX          ;has type of switch

; now call the switch manager to process the type of switch desired. It would
; handle all types of switch requests and if it returns with carry set, we
; would need to switch to an oldapp else to some windows window.

	clc
	cCall   SwitchManager,<ax,CurrentDosSwapSeed>
	smov    es,ds                   ;restore es to be ds
	jnc     @f                      ;must go back to shell.
	cmp     ax,CurrentDosSwapSeed   ;want to restart same app ?
	jz      RestartApp              ;yes, no swapping needed.
	stc                             ;set carry to => restart old app
@@:
	pushf                           ;save state of carry flag.
	push    ax                      ;save app id.
	push    bx                      ;save swap path id

; now swap out the old app.

	cCall   SwapDosAppOut           ;save old app's context
	jnc     @f                      ;app swapped out OK.

; the dos app could not be swapped out, we must return to it. Bring it back to
; the top of the list.

	push    ax                      ;save
	mov     ax,CurrentDosSwapSeed   ;get the ID of the app.
	cCall   AppToTheTop,<ax>        ;move the app to the top of the list.
	pop     ax                      ;restore

; the dos swap out attempt failed, put up a message on the screen saying this
; and restart the app. 

	cCall   AppSwapOutErrHandler    ;display message.
	pop     bx                      ;discard saved swap path id
	pop     ax                      ;disacrd saved app id
	popf                            ;discard saved flag state
	jmp     short RestartApp        ;restart the same app
@@:
	pop     bx                      ;restore swap path id
	pop     ax                      ;restore saved app id
	popf                            ;restore saved flag state
	jnc     BackToWindows           ;go back to shell

; we have to load another oldapp, simply set the dos swap file seed to the
; handle of the window for the oldapp.

	mov     CurrentPathId,bl        ;save swap path ID
	mov     CurrentDosSwapSeed,ax   ;correct dos files seed
	jmp     short ReActivateOldApp  ;reaload another oldapp

BackToWindows:

; control going back to windows

	push    ax                      ;save the window to invoke
	cCall   SwapWindowsIn           ;restore windows low heap

	cCall   RestoreSwapBlock        ;restore last swap block

; re-enable kernels dos traps, before this we must return the state of the
; CTRL+C flag. This had been saved after doing the DisableDos call.

	cCall   RestoreCtrlCFlag        ;resturn original state

ActivateNewTask:

	pop     ax                      ;get back the window handle to activate

; if Int15UsershApp is not zero, we must set it in the class word.
	
	mov     ax,Int15UsershApp       ;get the variable
	or      ax,ax                   ;did the switched out app use it
	jz      @f                      ;no.
	cCall   SetInt15Word,<ax>       ;INT 15 word updated
@@:

	xor     ax,ax                   ;no error
	mov     ErrorSubType,al         ;no error

; if we are going back because of an app swap in error set the error type 
; appropriately. We can also have an error when we try to restart an app
; that has never been started before. In all cases the error code will be
; put in 'ReturnToShellCode'
	
	mov     al,ReturnToShellCode    ;will be zero if no error.
	mov     ErrorType,al            ;initialize for no error
	jmp     OldAppManagerEnd        ;go back.

StartAgain:

	cCall   GetInt15Word            ;get the current INT 15 word value
	mov     Int15UsershApp,ax       ;save it.

; at this point we must get the current CTRL+C flag and set it off.

	cCall   GetNSetCtrlCFlag        ;get current one and set it off.

	cCall   SaveSwapBlock           ;save this swap block

; at this point we are just reloading an already loaded application so while
; swapping windows out, we will assume no XMS is required. If app actually needs
; XMS memory, it will recreate the windows xms swap file. 

	mov     WinSaveXms,0            ;no need to save XMS image now.

; now swap Windows out.

	xor     ax,ax                   ;only main woa segments to be relocated
	cCall   SwapWindowsOut,<ax>     ;windows swapped out
	jnc     ReActivateOldApp        ;swap out successful.
	jmp     OldAppManagerEnd        ;go back, we got an error.

ReActivateOldApp:

; finally get old app context back

	cCall   SwapDosAppIn            ;get back the dos app
	jnc     RestartApp              ;context read in OK
	mov     al,ErrorType            ;get the error type
	mov     ReturnToShellCode,al    ;save it
	jmp     BackToWindows           ;go back to Windows.

RestartApp:

; now reload the register frame, setting AX to 1 this time.

	mov     ax,_WOARLMSEG           ;load the segment for the stub

; set up CS=DS=ES=SS to this value

	mov     RealMode_CS,ax
	mov     RealMode_DS,ax
	mov     RealMode_ES,ax

; set up IP

	mov     ax,StubSegOFFSET BackFromContextSwitch
	mov     RealMode_IP,ax

; set up the value of Int15UsershApp in ax in the client frame.

	mov     ax,Int15UsershApp       
	mov     RealMode_AX,ax

; we are ready to go back to real mode and resume the execution

	ret

ContextSwitch   endp
;----------------------------------------------------------------------------;
; SwapWindowsOut:                                                            ;
;                                                                            ;
; Swaps the low heap out to the disk.                                        ;
;                                                                            ;
; After swapping the low heap out, this routine also relocates winoldap segm-;
; -ents based on a parameter. If the Relocate flag is 1, all the segments are;
; relocated (this is when windows is being swapped out to start the old app  ;
; for the firts time) else only the main code and data segments are relocated;
; (this is when windows is being swapped out to restart the old app).        ;
;----------------------------------------------------------------------------;

cProc   SwapWindowsOut,<NEAR,PASCAL,PUBLIC>,<di,si>

	assumes cs,Code
	assumes ds,Data

	parmW   RelocationFlag          ;type of seg. relocation to be done

	localW  FileHandle              ;save the file handle here
	localD  lpSwapFile              ;name of the swap file

cBegin

	cld                             ;do not take chances with this

; set error type = 'windows swap out error' in case we fail

	mov     ErrorType,ER_WND_SWAP_OUT

; first save the state of the EMS map if EMS is present
		
	call    SaveWinEmsContext       ;save EMS context

	smov    es,ds                   ;make es point to data segment too

; based on the type of relocation of segments desired, do the relocation

	cmp     RelocationFlag,1        ;all segments to be relocated ?
	jz      RelocateAllSegments     ;yes

; reload woa code above into its resident area. Do not bother about the other
; segments.

;       cCall   MoveWoaIntoStubArea     ;relocate main code and data
	jmp     short SegmentRelocationDone

RelocateAllSegments:

;----------------------------------------------------------------------------;
; Windows has been swapped out and it is time now to rellocate the WOA       ;
; segments such that we can release as much space as possible for the oldapp ;
;                                                                            ;
; In protected mode the main code segment and the data segment will be moved ;
; to the begining of the high heap, the stub segment and, if needed, the  XMS;
; segment will be relocated to the start of the lowheap.                     ;
;                                                                            ;
; In Real mode, all winoldap segments will be relocated to the start of the  ;
; low heap after the top PDB area.                                           ;
;----------------------------------------------------------------------------;

	cCall   ShuffleSegments         ;position winoldap segments

SegmentRelocationDone:

	clc                             ;no error

SwapWindowsOutRet:
cEnd
;----------------------------------------------------------------------------;
; SwapWindowsIn:                                                             ;
;                                                                            ;
; Swaps the low heap in, relocates woa from high memory area and then swap in;
; the high heap part                                                         ;
;----------------------------------------------------------------------------;

cProc   SwapWindowsIn,<NEAR,PASCAL,PUBLIC>,<di,si>

	assumes cs,Code
	assumes ds,Data

	localD  lpSwapFile              ;pointer to the name of the swap file

cBegin

	cld                             ;do not take chances with this

; set error type = "windows swap in error" in case we fail

	mov     ErrorType,ER_WND_SWAP_IN

; at first restore the EMS state if EMS is present. We must have the right 
; banks mapped in befor we restore the heap.

	call    RestoreWinEmsContext

; restore the block type and size of the low memory block.

	push    es                      ;save
	mov     es,LowMemArenaSel       ;get the selector for the low mem blk
	mov     al,LowMemBlockType      ;get the saved block type
	mov     es:[0],al               ;restore it
	mov     ax,LowMemParaSize       ;get the size of the low mem block
	mov     es:[3],ax               ;save it.
	pop     es

SwapWindowsInRet:

cEnd
;----------------------------------------------------------------------------;
; SwapDosAppOut:                                                             ;
;                                                                            ;
; This swaps the DOS APP out into the disk in preparation of rolling windows ;
; back.                                                                      ;
;----------------------------------------------------------------------------;

cProc   SwapDosAppOut,<NEAR,PUBLIC,PASCAL>                                      
		
	localW  SwapHandle              ;save the swap file handle here
	localD  FileOffset              ;place to save offset of XMS area start
	localD  lpSwapFile              ;ptr to name of the swap file

cBegin

	cld                             ;do not take chances with this

; find out which swap drive to use and whether we will have space to swap
; out or not.

	call    WhichSwapPathToUse      ;find out which path to use
	njc     SwapDosAppOutRet        ;no space on either.

; get the dos app file name  to swap out

	mov     si,OFFSET DosAppSwapFileName;filled in above
	mov     seg_lpSwapFile,ds       ;save the pointer to the name
	mov     off_lpSwapFile,si

; swap out the allocated dos blocks to the disk

	mov     ax,0ffffh               ;opcode => do swapping
	cCall   SaveDosMemory,<ds,si,ax>
	
	jc      SwapDosAppOutRet        ;cannot proceed with error
	mov     SwapHandle,ax           ;save the handle of the file

; it's time to save the XMS context now, but the current offset of the file
; has to be written into the first 4 bytes of the file so that the XMS context
; could be read in first during swap in.

	xor     ax,ax                   ;need to lseek 0 to get seek offset
	mov     bx,1                    ;need to seek from current position
	cCall   LseekFile,<SwapHandle,ax,ax,bx>
	mov     seg_FileOffset,dx       ;save hiword of current offset
	mov     off_FileOffset,ax       ;save loword of current offset

; now seek back to the start of the file

	xor     ax,ax                   ;for offset/origin
	cCall   LseekFile,<SwapHandle,ax,ax,ax>

; and write the offset of the start of the XMS area

	lea     ax,FileOffset           ;ss:ax points to data to write
	mov     cx,4                    ;need to write 4 bytes
	xor     bx,bx                   ;need for high word of count
	cCall   WriteFile,<SwapHandle,ss,ax,bx,cx>
	jc      SwapDosAppOutRet        ;cannot proceed with error

; finally seek back to the start of the XMS swap out area

	xor     ax,ax                   ;need to seek from the begining
	cCall   LseekFile,<SwapHandle,FileOffset,ax>

; now save the size of the XMS block and the XMS block itself.

	cCall   SaveAppXmsContext,<SwapHandle>;saves apps XMS context
	jc      SwapDosAppOutRet        ;cannot proceed with error

ifdef	JAPAN
	cCall	DisableKkc		; disable KKC
	jc	@f			; no support interface of KKC
	cCall	SaveKkcState,<FileHandle> ; save state of KKC
	jc	SwapDosAppOutRet	; write error
	cCall	EnableKkc		; enable KKC
@@:
endif

; close the dos app file

	cCall   CloseFile,<SwapHandle>  ;the swap file is closed
	jc      SwapDosAppOutRet        ;cannot proceed with error

; mark the file to be read only and hidden

	cCall   SetHiddenAttributes,<lpSwapFile>

SwapDosAppOutRet:
	
cEnd
;----------------------------------------------------------------------------;
; SwapDosAppIn:                                                              ;
;                                                                            ;
; This routine swaps the dos file swap area back to where it was in memory   ;
;----------------------------------------------------------------------------;

cProc   SwapDosAppIn,<NEAR,PUBLIC,PASCAL>                                       

cBegin

	cld                             ;do not take chances with this

; set error type = 'app swap in error' in case we fail

	mov     ErrorType,ER_APP_SWAP_IN

; get the name of the file to swap in
	
	call    GetDosAppSwapFileName   ;DS:SI has the name

; now get back Xms image and the image for the dos app

	cCall   RestoreXmsAndDosAllocatedBlocks,<ds,si>
	jc      SwapDosAppInRet         ;can't read it in

ifdef	JAPAN
	cCall	DisableKkc		; disable KKC
	jc	@f			; no support interface of KKC
	cCall	RestoreKkcState,<ax>
	jc	SwapDosAppInRet
	cCall	EnableKkc		; enable KKC
@@:
endif

; now close the swap file, and delete it.

	cCall   CloseFile,<ax>          ;swap file closed

; get the name of the swap file and delete it

	mov     si,DataOFFSET DosAppSwapFileName
	cCall   DeleteFile,<ds,si>


SwapDosAppInRet:
	
cEnd
;----------------------------------------------------------------------------;
; MoveWoa:                                                                   ;
;               Relocate to low memory                                       ;
;----------------------------------------------------------------------------;

cProc   MoveWoa,<NEAR,PUBLIC,PASCAL>

	parmW   TargetHeapSel           ;area to relocate to
	parmW   NewCsSel                ;selector to use for copied CS
	parmW   NewDsSel                ;selector to use for copied DS

cBegin

	cld                             ;do not take chances with this

	mov     bx,TargetHeapSel        ;get the target selector
	call    GetSelectorBase         ;gets base address

	push    cx
	push    dx                      ;save base of target selector
	mov     bx,NewCsSel             ;load the selector for new CS
	call    SetSelectorBaseLim64    ;sets base and limit to 64k
	mov     NewCsSel,bx             ;save (needed in real mode)
	pop     dx
	pop     cx                      ;get base of copy area
	add     dx,WoaCsSize            ;add size of the code seg
	adc     cx,0
	mov     bx,NewDsSel             ;load the selector for the new data seg
	call    SetSelectorBaseLim64    ;sets base and then limit to 64k
	mov     NewDsSel,bx             ;save it (need in real mode)

; now copy the code segment

	cli                             ;shut off interrupts
	push    ds                      ;save
	mov     cx,WoaCsSize            ;get size in bytes
	mov     es,NewCsSel             ;target segment for copy
	smov    ds,cs                   ;source segment for copy
	xor     si,si
	xor     di,di                   ;offsets start at 0
	rep     movsb                   ;do the move
	pop     ds                      ;get back data segment

; now copy the data segment

	mov     cx,WoaDsSize            ;data segment size
	mov     es,NewDsSel             ;target selector
	xor     si,si
	xor     di,di                   ;start offsets are 0
	rep     movsb                   ;do the move
	sti                             ;enable interrupts

; change attribute of the code segment copy to be code segment

	mov     bx,NewCsSel             ;selector for code segment

; load the ss and ds selectors.

	mov     ax,NewDsSel             ;load the selector for data and stack
	mov     ds,ax
	mov     ss,ax                   ;DS and SS updated

	push    NewCsSel                ;push in the new cs
	switch_cs                       ;does the switch

MoveWoaRet:

; get a new data segment alias for the code segment here

;----------------------------------------------------------------------------;
; NOTE: SINCE KERNEL HAS BEEN SWAPPED OUT WE CANNOT MAKE USE OF THE KERNEL   ;
;       SELECTOR HANDLER ROUTINES, BECAUSE THEY WILL ALTER KERNEL'S STATE.   ;
;----------------------------------------------------------------------------;

cEnd

;----------------------------------------------------------------------------;
; LoadRealModeStub:                                                          ;
;                                                                            ;
; This routine loads all the variables that are needed by the real mode stub.;
;----------------------------------------------------------------------------;

cProc   LoadRealModeStub,<NEAR,PUBLIC,PASCAL>

cBegin

	cld                             ;do not take chances with this

; now copy the real mode WOA segment down into low memory area

	mov     ax,_WOARLMSEG           ;segment for the stub
	mov     es,ax                   ;load it in es
	assumes es,StubSeg
	assumes ds,Data

;----------------------------------------------------------------------------;
; Most of the variables that are needed by the stub segment will be loaded   ;
; by copying 'StubData' structure to the 'WoaStubData' structure. The        ;
; position and definition of individual variables in the two structure must  ;
; be identical.                                                              ;
;----------------------------------------------------------------------------;

	mov     si,DataOFFSET StubData  ;source structure
	mov     di,StubSegOFFSET WoaStubData
	mov     cx,StubDataLength       ;length of xfer
	rep     movsb                   ;move it over (cld done earlier)

;----------------------------------------------------------------------------;
; Now copy other variables (which could not be clubbed into the above        ;
; structure individually.                                                    ;
;----------------------------------------------------------------------------;

; pass the old app number

	mov     ax,DosAppNumber         ;the serial number of the app
	mov     WoaAppNumber,ax         ;pass it on

; pass on the handle of the window

	mov     ax,hApp                 ;handle of this instances window
	mov     WoahApp,ax              ;pass it on

; create the WOA exec parameter block

	xor     ax,ax
	mov     [WoaParamBlock],ax      ;environment seg = 0

; save the value of the stub segment in the client register frame

	mov     ax,es                   ;get the segment value

; also set the CS,DS,ES values in the register frame to this value

	mov     RealMode_CS,ax
	mov     RealMode_DS,ax
	mov     RealMode_ES,ax

	mov     [WoaParamBlock+4],ax    ;segment for parameter block
	mov     [WoaParamBlock+8],ax    ;segment for first FCB
	mov     [WoaParamBlock+12],ax   ;segment for the second FCB

; now load the 3 offsets

	mov     ax,StubSegOFFSET WoaParams
	mov     [WoaParamBlock+2],ax    ;osffet for parameters
	mov     ax,StubSegOFFSET WoaFcb1
	mov     [WoaParamBlock+6],ax    ;offset for first fcb
	mov     ax,StubSegOFFSET WoaFcb2
	mov     [WoaParamBlock+10],ax   ;offset for second fcb

; set up the initial ip in real mode into the resgister frame

	mov     ax,StubSegOFFSET RealModeWoa
	mov     RealMode_IP,ax

; The real mode stub is loaded and is ready to receive control.  Now
; setup data for XMS code if it's installed (it will be installed if
; the XmsBankSize is NZ).

; tell stub if XMS code is installed

	mov     es:[WoafXmsInstalled],0 ;assume not installed
	mov     ax,wptr [XmsBankSize]
	or      ax,wptr [XmsBankSize+2]
	jz      LRMSRet
	inc     es:[WoafXmsInstalled]   ;installed, set flag to NZ

; pass the handle of the high heap xms block

	mov     ax,HighMemXmsHandle     ;get the handle of the xms block
	mov     es:[XmsBaseHandle],ax   ;save it

; get the base of the XMS memory to be allocated to the app

	call    GetAppXmsBase           ;returns base in cx:dx

; pass on this linear address as the base of the apps XMS area

	mov     es:wptr [AppXmsBase+2],cx
	mov     es:wptr [AppXmsBase],dx

; and pass on the size of the apps xms block

	mov     cx,wptr [XmsBankSize+2] ;get size requested by the app
	mov     dx,wptr [XmsBankSize]
	mov     wptr es:[AppXmsSize+2],cx
	mov     wptr es:[AppXmsSize],dx

LRMSRet:

cEnd
;----------------------------------------------------------------------------;
; SaveSwapBlock:                                                             ;
;       Traverses the swap block and transfers areas from the source address ;
;       to the dsetination address. The swap block is an array of nodes with ;
;       the following sturcture:                                             ;
;               WORD   --   size in words of area to swap                    ;
;              DWORD   --   long pointer to source area                      ;
;              DWORD   --   long pointer to destination area                 ;
;                                                                            ;
;        A word count of zero marks the end of the block.                    ;
;----------------------------------------------------------------------------;

SaveSwapBlock   proc    near

	cld                             ;do not take chances with this

	mov     dx,ds                   ;save WOA ds
	mov     bx,DataOFFSET SwapBlock ;DS:BX points to the swap block

save_swap_loop:

	mov     ds,dx                   ;get back WOA data segment
	mov     cx,[bx]                 ;get the word count
	jcxz    save_swap_done          ;the block has been swapped away
	les     di,[bx+6]               ;load the destination in es:di
	lds     si,[bx+2]               ;load source in ds:si
	rep     movsw                   ;save one swap area
	add     bx,10                   ;point to the next entry
	jmp     save_swap_loop          ;move all areas

save_swap_done:

	ret                             
			
SaveSwapBlock   endp
;----------------------------------------------------------------------------;
; RestoreSwapBlock:                                                          ;
;           Restores the swap block. This is very similar to the routine     ;
;           above, except that it reverses the direction of copy.            ;
;----------------------------------------------------------------------------;
RestoreSwapBlock        proc    near

	cld                             ;do not take chances with this

	mov     dx,ds                   ;save WOA ds
	mov     bx,DataOFFSET SwapBlock ;DS:BX points to the swap block

restore_swap_loop:

	mov     ds,dx                   ;get back WOA data segment
	mov     cx,[bx]                 ;get the word count
	jcxz    restore_swap_done       ;the block has been swapped away
	les     di,[bx+2]               ;load the destination in es:di
	lds     si,[bx+6]               ;load source in ds:si
	cli                             ;mask off interrupts
	rep     movsw                   ;save one swap area
	sti                             ;allow interrupts
	add     bx,10                   ;point to the next entry
	jmp     restore_swap_loop               ;move all areas

restore_swap_done:

	ret                             
			
RestoreSwapBlock        endp
;----------------------------------------------------------------------------;
; SwapSwapBlock:                                                             ;
;               This exchanges the contents of the current swap area         ;
;               locations with the saved one.                                ;
;----------------------------------------------------------------------------;
;SwapSwapBlock   proc    near
;
;        cld                             ;do not take chances with this
;
;        mov     dx,ds                   ;save WOA ds
;        mov     bx,DataOFFSET SwapBlock ;DS:BX points to the swap block
;
;swap_swap_loop:
;
;        mov     ds,dx                   ;get back WOA data segment
;        mov     cx,[bx]                 ;get the word count
;        jcxz    swap_swap_done          ;the block has been swapped away
;        les     di,[bx+2]               ;load the destination in es:di
;        lds     si,[bx+6]               ;load source in ds:si
;        cli                             ;mask off interrupts
;swap_a_node:
;        mov     ax,[si]                 ;get a word from source
;        xchg    ax,es:[di]              ;swap it with destination
;        mov     [si],ax                 ;save it in the source
;        add     si,2                    ;next entry in source
;        add     di,2                    ;next entry in destination
;        loop    swap_a_node             ;complete swapping of the node
;        sti                             ;allow interrupts
;        add     bx,10                   ;point to the next entry
;        jmp     swap_swap_loop          ;move all areas
;
;swap_swap_done:
;
;        ret                             
;                        
;SwapSwapBlock   endp
;----------------------------------------------------------------------------;
; these routines create the name for the swap files to be used.              ;
;----------------------------------------------------------------------------;

		;----------------------------------;
		; based on the path ID it calls an ;
		; appropropriate function to create;
		; a swap file name.                ;
		;----------------------------------;
		
GetDosAppSwapFileName proc near

	cmp     CurrentPathId,1         ;first path to use ?
	jnz     @f                      ;no.
	call    GetDosAppSwap1FileName  ;use first path
	jmp     short GetDosAppSwapFileNameRet
@@:
	call    GetDosAppSwap2FileName  ;use second path

GetDosAppSwapFileNameRet:

	ret

GetDosAppSwapFileName endp

		;----------------------------------;
		; Creates Dos App Swap File Name   ;
		; (based on the first swap path)   ;
		;----------------------------------;

GetDosAppSwap1FileName proc near

	mov     di,DataOFFSET DosAppSwapFileName
	call    CopyBasicSwap1FileName  ;copy the invariant part of name
	mov     ax,CurrentDosSwapSeed   ;get the seed for the file
	call    AppendUniqueNumber      ;appens the unique number
	mov     si,DataOFFSET DosAppSwapFileName
	ret

GetDosAppSwap1FileName endp

		;----------------------------------;
		; Creates Dos App Swap File Name   ;
		; (based on the second swap path)  ;
		;----------------------------------;


GetDosAppSwap2FileName proc near

	mov     di,DataOFFSET DosAppSwapFileName
	call    CopyBasicSwap2FileName  ;copy the invariant part of name
	mov     ax,CurrentDosSwapSeed   ;get the seed for the file
	call    AppendUniqueNumber      ;appens the unique number
	mov     si,DataOFFSET DosAppSwapFileName
	ret

GetDosAppSwap2FileName endp

		;----------------------------------;
		;appends the unique number to name ;
		;----------------------------------;

AppendUniqueNumber proc near

	cld                             ;do not take chances with this

	mov     bx,ax                   ;save seed
	mov     al,bh                   ;get high byte of seed
	call    SaveLettersFromByte     ;convert into 2 hex letters and save
	mov     al,bl                   ;get the low byte of the seed
	call    SaveLettersFromByte     ;convert into 2 hex letters and save
	mov     ax,'T.'                 ;next two characters
	stosw                           ;x:\<path>\~DOShhhh.T
	mov     ax,'PM'                 ;final two characters
	stosw                           ;x:\<path>\~DOShhhh.TMP
	xor     al,al                   ;the zero terminator
	stosb                           ;save it too.
	ret

AppendUniqueNumber endp

		;----------------------------------;
		; Far entry point to above routine ;
		;----------------------------------;

FarAppendUniqueNumber proc far

	call    AppendUniqueNumber
	ret

FarAppendUniqueNumber endp

		;----------------------------------;
		; saves two ascii leters fro a byte;
		;----------------------------------;

SaveLettersFromByte proc near

	cld                             ;do not take chances with this

	push    ax                      ;save
	shiftr  al,4                    ;get high nibble
	call    GetLetterFromNibble     ;convert to letter
	stosb                           ;save it
	pop     ax                      ;get back high byte
	and     al,0fh                  ;isolate low nibble
	call    GetLetterFromNibble     ;convert to a letter
	stosb                           ;save it
	ret

SaveLettersFromByte endp

		;----------------------------------;
		;converts a nibble to a hex letter ;
		;----------------------------------;

GetLetterFromNibble proc near

	add     al,30h                  ;add base for 0
	cmp     al,39h                  ;past '9'
	jbe     @f                      ;no, we are ok
	add     al,7                    ;convert to letter
@@:
	ret                             ;AL has ascii letter

GetLetterFromNibble endp

		;---------------------------------;
		;copies the basic swap file name  ;
		;based on the first swap path     ;
		;---------------------------------;

CopyBasicSwap1FileName proc near

	cld                             ;do not take chances with this

	smov    es,ds                   ;es and ds should both be own DSEG
	mov     si,DataOFFSET WoaSwap1Path
@@:
	lodsb                           ;get the next character
	stosb                           ;save it
	or      al,al                   ;was the null copied ?
	jnz     @b                      ;no, continue
	dec     di                      ;back over the null
	ret

CopyBasicSwap1FileName endp

		;---------------------------------;
		;copies the basic swap file name  ;
		;based on the second swap path    ;
		;---------------------------------;

CopyBasicSwap2FileName proc near

	cld                             ;do not take chances with this

	smov    es,ds                   ;es and ds should both be own DSEG
	mov     si,DataOFFSET WoaSwap2Path
@@:
	lodsb                           ;get the next character
	stosb                           ;save it
	or      al,al                   ;was the null copied ?
	jnz     @b                      ;no, continue
	dec     di                      ;back over the null
	ret

CopyBasicSwap2FileName endp

		;-----------------------------------;
		; far entry point to above routines ;
		;-----------------------------------;

FarCopyBasicSwap1FileName proc far

	call    CopyBasicSwap1FileName
	ret

FarCopyBasicSwap1FileName endp

FarCopyBasicSwap2FileName proc far

	call    CopyBasicSwap2FileName
	ret

FarCopyBasicSwap2FileName endp


;----------------------------------------------------------------------------;
; This routine invokes the error manager.                                    ;
;----------------------------------------------------------------------------;

ErrorHandler:
	
	call    ErrorManager            ;this never returns

;----------------------------------------------------------------------------;

sEnd    Code

end OldAppManager
