.386p
page 58,132
;******************************************************************************
	title	EMM - Expanded Memory Manager interface for EMM386
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;
;    Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;    Module:	EMM - Expanded Memory Manager interface
;
;    Version:	2.05
;
;    Date:	June 28, 1991
;
;    Author:	Harish Naidu
;
;******************************************************************************
;
;	Change Log:
;
;	DATE	 REVISION	Description
;	-------- --------	--------------------------------------------
;	06/28/91 original
;
;******************************************************************************
;   Functional Description:
;
;	This module contains most of R1_CODE segment. The other files that 
;   contain R1_CODE are vcpi.asm and errhndlr.asm. This segment will be 
;   moved into UMBs if noems or ram is specifed in the emm386.exe device
;   line. The user can use nohi to prevent this segment from going into 
;   UMBs.
;
;******************************************************************************
	page
;******************************************************************************
;			P U B L I C   D E C L A R A T I O N S
;******************************************************************************
;
	public	UMBFARTABLE
	public	UMBADDRLEN
	public	EMM_rFarEntry
	public	StrategyEntry
	public	InterruptEntry
	public	rInt13HEntry
	public	ReqPtr
	public	OldInt13
	public	PrevInt4B
	public	rINT4BhHandler

	public	I13SectTrans		
	public	I13DriveTrap		
	public	SectorsInDMABuffer	
	public	LongSectorsInDMABuffer	

	public	CopyInstData
	public	ValidPathFar
	public	PrevXmm
	public	rXMMentry
	public	XMMAllocHMAFar
	public	XMMDeallHMAFar
	public	checkXMMFar

	public	PrevInt10
	public	PrevInt11
	public	Int10_Hook
	public	Int11_Hook

	public	chkMcStateFar
	public	EHFarReturn


	page
;******************************************************************************
;			L O C A L   C O N S T A N T S
;******************************************************************************
;
	include vdmseg.inc
	include vdmsel.inc
	include vm386.inc
	include emm386.inc
	include emmfunct.inc
	include emmdata.inc

	include driver.equ
	include driver.inc

	include i13.inc
	include	winemm.inc
	include	xmm.inc
	include	desc.inc

;******************************************************************************
;			E X T E R N A L    R E F E R E N C E S
;******************************************************************************
;

_TEXT	segment
	extrn	EMSDispatcher:far
	extrn	EnterVirtual:far
	extrn	VCPIDispatcher:far
_TEXT	ends

LAST	segment
	extrn	Init_CEMM386:far	; initializes VDM,EMM, and driver
	extrn	Inv_DOS_msg:byte
	extrn	InitMessage:byte
LAST	ends

R1_CODE	segment
	extrn	CEMMpath:byte
	extrn	EHReturnFar:byte
	extrn	end_of_R1_CODE:byte
R1_CODE	ends



R_CODE	segment

	;
	; This table contains dword ptrs thru which R_CODE accesses R1_CODE.
	; The actual segment value of R1_CODE is patched into this table 
	; if R1_CODE is moved in MovUmbSeg in init.asm
	;

	EVEN
UMBFARTABLE	LABEL	WORD
	
EMM_rFarEntry	DW	OFFSET R1_CODE:EMM_rEntryFar	, seg R1_CODE
EMM_rFarReturn	DW	OFFSET R1_CODE:EMM_rReturnFar	, seg R1_CODE
EMM_vFarReturn	DW	OFFSET R1_CODE:EMM_vReturnFar	, seg R1_CODE
StrategyEntry	DW	OFFSET R1_CODE:STRATEGY_Far	, seg R1_CODE
InterruptEntry	DW	OFFSET R1_CODE:Interrupt_Far	, seg R1_CODE
rInt13HEntry	DW	OFFSET R1_CODE:rFarINT13hHandler, seg R1_CODE
CopyInstData	DW	OFFSET R1_CODE:Copy_Inst_Data	, seg R1_CODE
ValidPathFar	DW	OFFSET R1_CODE:ValidatePath	, seg R1_CODE
XMMAllocHMAFar	DW	OFFSET R1_CODE:XMMAllocateHMA	, seg R1_CODE
XMMDeallHMAFar 	DW	OFFSET R1_CODE:XMMDeallocateHMA	, seg R1_CODE
checkXMMFar	DW	OFFSET R1_CODE:check_XMM	, seg R1_CODE
chkMcStateFar	DW	OFFSET R1_CODE:chk_machine_state, seg R1_CODE
EHFarReturn	DW	OFFSET R1_CODE:EHReturnFar	, seg R1_CODE

UMBADDRLEN	EQU	($ - UMBFARTABLE) / 4


	extrn	GoVirtualMode:near
	extrn	GoRealMode:near
	extrn	GoProtMode:near
	extrn	ChkA20:near
	extrn	Devname:byte

	extrn	ELIM_Entry:far		; general entry for CEMM functions
	extrn	SIG_LENGTH:abs
	extrn	Linkbuf_length:abs
	extrn	EMMGIDataVer:word
	extrn	XMMcontrol:dword
	extrn	DriverVersion:word

R_CODE	ends

R_CODE	segment
	assume	cs:R_CODE
;-------------------------------------------------------------------------
;
; Far interfaces to routines in R_CODE segment. Note that these far 
; interfaces are used only by the routine EMM_rEntryFar and hence their 
; returns are hard coded.
;
;------------------------------------------------------------------------


FarGoVirtualMode	proc	far
	call	GoVirtualMode
	jmp	dword ptr cs:[EMM_vFarReturn]
FarGoVirtualMode	endp

FarGoRealMode		proc	far
	call	GoRealMode
	jmp	dword ptr cs:[EMM_rFarReturn]

FarGoRealMode		endp

FarGoProtMode	proc	far
	call	GoProtMode
	PJmp	R1CODE_GSEL,R1_CODE:EMS_Entry
FarGoProtMode	endp

FarChkA20		proc	far
	call	ChkA20	
	ret
FarChkA20	endp
	
R_CODE	ends


R1_CODE	segment
	assume	cs:R1_CODE


;#########################################################################
;
; EMM_rEntryFar - real/virtual mode entry point for EMM function calls
;
; ENTRY:
;	real/virtual mode 
;	GS = R_CODE
;
; EXIT:
;	real/virtual mode
;
; DESCRIPTION:
;	If CEMM is off then an appropriate error code is returned in AH.
;	If CEMM is in auto mode and off then the EMS call is done from here
;	and CEMM is returned in either auto off state or ON state.  If CEMM
;	is on this this call simply reflects it back to the protected entry
;	point.
;
;##########################################################################

EMM_rEntryFar	proc	far

	; The interrupts are cleared in case the user didn't use the
	; int opcode but just pushed the flags and called.
	cli

	;
	; If CEMM is active (ON) then the call to here is done so that
	; any routines that patched out int 67h will be done first.
	; Now the EMMp_Entry routine is called with the flag set.
	;
	test	gs:[Current_State],fState_Active
	jz	short EMM_rChkOFF
	or	gs:[TrapFlags],fI67trap
	int	67h
	ret

	;
	; If CEMM is off then an appropriate error code needs to be set.
	;
EMM_rChkOFF:
	cmp	gs:[Current_Mode],MODE_OFF
	jne	short EMM_protected_mode
	mov	ah,EMM_SW_MALFUNCTION
	ret

	;
	; At this point EMM386 is inactive. Which means UMBs could not have 
	; been present. Hence this segment could not have been in UMBs. 
	; Therefore the following code is free to switch modes.
	;
EMM_protected_mode:

	;
	;  The user stack is changed to the CEMM Real Stack
	;
	mov	gs:[UserSS],ss
	mov	gs:[UserSP],sp
	push	seg R_STACK
	pop	ss
	lea	sp,R_STACK:RealStackTop

	;
	;  The user's segment registers are pushed on the stack.
	;
	push	gs
	push	fs
	push	es
	push	ds

	call	far ptr chk_machine_state
					; Q: is the machine in real mode
	jc	Emm_rError1		; N: return error

	mov	gs:[UserDS],ds
	mov	gs:[UserES],es

	;
	; The A20 line is activated and the system taken in protected
	; mode.
	;
	call	FarChkA20

	jmp	FarGoProtMode
EMS_Entry:
	jc	EMM_rError

	;
	; The DS and GS selectors are set up.
	;
	push	VDMD_GSEL
	pop	ds
	assume	ds:_DATA
	push	RCODEA_GSEL
	pop	gs
	assume	gs:R_CODE
	
	;
	;  Save CEMM real stack and change to protected mode stack
	;
	mov	gs:[RealSS],ss
	mov	gs:[RealSP],sp
	push	VDMS_GSEL
	pop	ss
	mov	sp,gs:[StackTop]

	push	gs:[UserES]
	push	gs:[UserDS]

	;
	; The function number is saved and the call done.
	;
	mov	[function_number],ah

	;
	; Check to see if its a VCPI function or EMS function. Call 
	; proper dispatcher.
	;
	cmp	ah,VCPI_FUNCTION_OPCODE
	je	short ErE_VCPI_function

	PCall	VDMC_GSEL,_TEXT:EMSDispatcher
	jmp	short ErEcont

ErE_VCPI_function:
	PCall	VDMC_GSEL,_TEXT:VCPIDispatcher
ErEcont:
	
	;
	;  Restore CEMM real stack
	;
	mov	ss,gs:[RealSS]
	mov	sp,gs:[RealSP]

	;
	; Failures (namely invalid functions) should not turn on.
	;
	cmp	ah,0
	jne	short EMM_rRetReal

	;
	; The function is checked for the status call.  If anything
	; but status is done then take CEMM in ON mode.
	;
	cmp	[function_number],EMS_STATUS_FUNCTION
	JE	SHORT EMM_rRetReal	; Y: Return to real mode	;@PIW

EMM_turn_CEMM_on:
	PCall	VDMC_GSEL, _TEXT:EnterVirtual
	PJmp	RCODE_GSEL, R_CODE:FarGoVirtualMode
EMM_vReturnFar:

	mov	gs:[Current_Mode],MODE_ON
	or	gs:[Current_State],fState_Active
	jmp	short EMM_rExit

EMM_rRetReal:
	;
	; At this point we're in protected mode hence we have to do a far
	; jmp
	;
	PJmp	RCODE_GSEL,R_CODE:FarGoRealMode
EMM_rReturnFar:

EMM_rExit:
	pop	ds
	pop	es
	pop	fs
	pop	gs

	;
	;  Restore user stack
	;
	mov	ss,gs:[UserSS]
	mov	sp,gs:[UserSP]
	ret

EMM_rError:
	mov	gs:[Current_Mode],MODE_OFF 	     ; set mode to OFF
	mov	gs:[Devname],'$'		     ; set to prevent presense detect
	and	gs:[Current_State],NOT fState_Active ; reset active flag &
EMM_rError1:
	mov     ah,EMM_SW_MALFUNCTION		     ; S/W cannot control A20
	jmp	short EMM_rExit

EMM_rEntryFar	endp


;===========================================================================
;
;	Routines associated with STRATEGY and INTERRUPT
;
;===========================================================================

;	Define the command dispatch table for the driver functions
;
Cmd_Table	LABEL		NEAR
	DW	Init_Call		;0 - Initialization
	DW	Null_Exit		;1 - Media Check
	DW	Null_Exit		;2 - Get BPB
	DW  	EMM_IOCTL_READ		;3 - IOCTL input
	DW	Null_Exit		;4 - Input (Destructive)
	DW	Null_Exit		;5 - No wait input
	DW	Null_Exit		;6 - Input status
	DW	Null_Exit		;7 - Input buffer flush
	DW	Null_Exit		;8 - Output (Write)
	DW	Null_Exit		;9 - Output with verify
	DW	Null_Exit		;A - Output status
	DW	Null_Exit		;B - Output buffer flush
	DW	Null_Exit		;C - IOCTL output

TBL_LENGTH	EQU		(THIS BYTE-CMD_TABLE)/2 


ReqPtr		label	dword	; dword ptr to Request Header
ReqOff		dw	0	; saved offset of Request Header
ReqSeg		dw	0	; saved segment of Request Header

fFirstTime	db	0	; first time initialization flag

;=============================================================================

STRATEGY_Far	proc	far

	mov	cs:[ReqOff],bx ;Save header offset
	mov	cs:[ReqSeg],es ;Save header segment
	ret

STRATEGY_Far endp


;******************************************************************************
;	Interrupt - device driver interrupt routine for CEMM
;
;	ENTRY: R1_CODE:ReqPtr = pointer to request header.
;
;	EXIT: Request completed.
;
;	USED: none
;
;******************************************************************************
Interrupt_Far	proc		far

	push	ax
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	ds
	push	es
	cld					;All strings forward
	lds	bx,CS:[ReqPtr]			;DS:BX pts to Request Header
	mov	al,[bx].COMMAND_CODE		;Get the command code
	cmp	al,TBL_LENGTH			;Check for validity
	jae	short Invalid 			;Jump if command invalid
	cbw					;Command to a full word
	shl	ax,1				;Compute dispatch index
	mov	si,OFFSET Cmd_Table		;Point to dispatch table
	add	si,ax				;Index based on command
	call	CS:[si] 			;Call correct routine
;
;   ENTRY:	AX = Status field for Request Header
;
FINISH:
	lds	bx,CS:[ReqPtr]	;Get request header ptr.
	or	ah,DON			;Set done bit in status
	mov	DS:[bx.STATUS_WORD],ax	;Save status in header
	pop	es			;Restore the ES register
	pop	ds
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
Invalid:
	mov	al,UNK_COMMAND		; unknown command
	mov	ah,ERR			; error
	stc
	jmp	SHORT Finish
Interrupt_Far	endp

;******************************************************************************
;	Null_Exit: do nothing
;
;	ENTRY: DS:BX pts to request header
;
;	EXIT:	No error returned.
;		CLC
;
;******************************************************************************
Null_Exit	proc	near
;
	xor	ax,ax
	clc
	ret
;
Null_Exit	endp

;******************************************************************************
;	Init_Call - call initialization routine
;
;	ENTRY: DS:BX pts to request header
;
;	EXIT: AX = status field for request header
;
;******************************************************************************
Init_Call	proc	near
;
	cmp	cs:[fFirstTime], 0	; Will be called twice if 2 device
	jne	init_done		;   headers used, only init once

	inc	cs:[fFirstTime] 	; Don't init again

	call	Init_CEMM386

init_done:
	ret
;
Init_Call	endp

;
; Dispatch Table for IOCTL read ax=4402
;
EMMRDTBL	DB	05
  		DW 	Rtn_Signet
  		dw 	EMMGlobImpIOCTL
		dw	GetVersion
		dw	GetEMSMinMax
		dw	GetUMBsegInfo

;###########################################################################
;
; Procedure name  : EMM_IOCTL_READ
;
; Entry:  The destination buffer pointed to by the request header points to
;         an 8 byte buffer whos first byte is a function code (either 0 or 1)
;
; exit:  the appropriate function is executed if wrong code then error.
;
;############################################################################

EMM_IOCTL_READ 	proc 	near

	push	ax
	push	si

 	les 	di,ds:[bx+14]		; es:di -> dest. buffer
 	mov 	al,es:[di]   	       	; al = command code
 	cmp 	al,cs:[EMMRDTBL]
 	jae 	IOCTLerr
	cbw
	mov	si,ax
	shl	si,1
	call 	cs:word ptr [EMMRDTBL][1][si]
	xor	ax,ax

IOCTLexit:
	pop	si
	pop	ax
 	ret

IOCTLerr:
 	mov al,UNK_COMMAND		; unknown command       *C
 	mov ah,ERR 		        ; error         	*C
 	mov word ptr DS:[bx+18],0	; no bytes transferred  *C
	jmp	short IOCTLexit
EMM_IOCTL_READ endp

;*****************************************************************************C
;	Rtn_Signet - return signature and entry address 		     *C
;									     *C
;	ENTRY: DS:BX pts to request header				     *C
;									     *C
;	EXIT: AX = status field for request header			     *C
;									     *C
;*****************************************************************************C
Rtn_Signet	proc	near	       ; special read request from CEMM.EXE  *C
	cmp	word ptr DS:[bx+18],Linkbuf_length ; make sure our length    *C
	jne	short Rerr		       ;			     *C

	mov	word ptr ES:[di],SIG_LENGTH ; send back signature length     *C
	mov	word ptr ES:[di+2],OFFSET ELIM_Entry ; entry offset	     *C
	mov	word ptr ES:[di+4],SEG ELIM_Entry ; entry segment	     *C

	xor	ax,ax		       ; good status			     *C
	ret			       ;				     *C
				       ;				     *C
Rerr:	mov	al,UNK_COMMAND	       ; unknown command		     *C
	mov	ah,ERR		       ; error				     *C
	mov	word ptr DS:[bx+18],0  ; no bytes transferred		     *C
	ret			       ;				     *C
Rtn_Signet	endp		       ;				     *C

;==============================================================================
;==
;==  GetVersion: Returns version of CEMM/EMM386 driver.
;==
;==  Entry: Buffer is pointed to by request header.
;==	DS:BX	= Request Header
;==	ES:DI	= Destination Buffer
;==
;==  Exit:  Fills buffer with version number of CEMM/EMM386 driver.
;==
;==============================================================================
GetVersion proc	near
	push	ax
	push	gs

	mov	ax, seg R_CODE
	mov	gs, ax

	cmp	word ptr ds:[bx][18],2	;Q: Must be a 2 byte buffer?
	jne	short GVerror		; N: error
;
;  Set version number of driver.
;
	mov	ax,gs:[DriverVersion]	; get driver version number
	mov	es:[di],ax		; return address

;  Also an entry point from GetEMSMinMax

GVexit:
	pop	gs
	pop	ax
	ret
;
;  Error return, also an entry point from GetEMSMinMax
;
GVerror:
 	mov 	al,UNK_COMMAND          ; unknown command
 	mov 	ah,ERR           	; error
 	mov	word ptr ds:[bx][18],0	; no bytes transferred
	jmp	short GVexit
GetVersion	endp

;==============================================================================
;==
;==  GetEMSMinMax:  Returns max/min EMS/VCPI size info
;==
;==  Entry: Buffer is pointed to by request header.
;==	DS:BX	= Request Header
;==	ES:DI	= Destination Buffer
;==
;==  Exit:  Fills buffer with max & min EMS/VCPI pool size
;==
;==============================================================================
GetEMSMinMax	proc near
	push	ax
	push	gs

	push	seg R_CODE
	pop	gs

	cmp	word ptr ds:[bx][18],4	;Q: Must be a 4 byte buffer?
	jne	short GVerror		; N: error

	mov	ax, gs:[MaxEMSpool]
	mov	es:[di], ax
	mov	ax, gs:[MinEMSpool]
	mov	es:[di+2], ax

	jmp	short GVexit

GetEMSMinMax	endp


;==============================================================================
;==
;==  GetUMBsegInfo:  Returns location and size of UMB segment
;==
;==  Entry: Buffer is pointed to by request header.
;==	DS:BX	= Request Header
;==	ES:DI	= Destination Buffer
;==
;==  Exit:  Fills buffer with UMB segment paragraph address & length, and
;==	    count of WIN= pages
;==
;==============================================================================
GetUMBsegInfo	proc near

	push	ax
	push	gs

	push	seg R_CODE
	pop	gs

	cmp	word ptr ds:[bx][18],6	;Q: Must be a 6 byte buffer?
	jne	short GVerror		; N: error

	mov	ax, gs:[segR1_CODE]
	mov	es:[di], ax
	mov	ax, offset R1_CODE:end_of_R1_CODE
	add	ax, 0fh
	shr	ax, 4			;round size up to nearest paragraph
	mov	es:[di+2], ax
	mov	ax, gs:[cntWinPages]	;count of WIN= pages
	mov	es:[di+4], ax

	jmp	short GVexit

GetUMBsegInfo	endp


;==============================================================================
;==
;==  EMMGlobImpIOCTL: Windows opens that "EMMXXX0" device and does a read IOCTL.
;==		      This routine fills up a buffer with version information
;==		      and the physical address of the EMM Global Import Data
;==		      Structure.
;==
;==  Entry: Buffer is pointed to by request header.
;==	DS:BX	= Request Header
;==	ES:DI	= Destination Buffer
;==
;==  Exit:  Fills buffer with version number and physical address of EMM
;==	    Global Import Data Structure.
;==
;==============================================================================
EMMGlobImpIOCTL	proc	near

	push	ebx
	push	gs

	push	seg R_CODE
	pop	gs

	cmp	word ptr ds:[bx][18],6	;Q: Must be a 6 byte buffer?
	jne	short EGIIerror		; N: error

	mov	ebx,gs:[PageD_Addr]	; get physical address of PD
;
;  Set physical address for EMM Global Import Data Structure
;
	add	ebx,PAGE_SIZE		; PF0 address: used for data structure
	mov	es:[di],ebx		; return address
;
;  Set version of EMM Global Import Interface to execute
;
	mov	bx,gs:[EMMGIDataVer]
	xchg	bh,bl
	mov	es:[di][4],bx

EMMGIIOCTLexit:
	pop	gs
	pop	ebx
	ret
;
;  Error return
;
EGIIerror:
 	mov 	al,UNK_COMMAND          ; unknown command
 	mov 	ah,ERR           	; error
 	mov	word ptr ds:[bx][18],0	; no bytes transferred
	jmp	short EMMGIIOCTLexit
EMMGlobImpIOCTL	endp



;==========================================================================
;
;	DATA and CODE for INT13Handler
;
;==========================================================================


I13SectTrans		db	0	; Number of sectors transferred
I13DriveTrap		dd	-1	; Initially, trap all drives

SectorsInDMABuffer	db	0	; Sectors in DMA buffer
	db	TotalDrives-1	dup (0)

LongSectorsInDMABuffer	db	0	; Long Sectors in DMA buffer
	db	TotalDrives-1	dup (0)

OldInt13		dd	?


;==============================================================================
;==
;==  rFarINT13hHandler: 
;==
;==	This is an interrupt 13h handler which monitors activity to
;==     the drives for DMA purposes.  If it detects a user buffer
;==     (ES:BX) in an EMS window and its corresponding  physical
;==     memory is discontiguous, the operation will be modified.
;==
;==	1) The operation will be broken down to multiple read/
;==	   write requests, each being less than the DMA buffer.
;==
;==	2) If a format request is encountered, a flag will be
;==	   set so the DMA programming by the ROM will be modified
;==	   not to use a 64K buffer.
;==
;==  Entry: (Real Mode)
;==	INT 13h interface
;==
;==  Exit:
;==
;==============================================================================

rFarINT13hHandler:

	test	cl,3Fh			;Q: Sector 0?
	jz	short rfI13oldHandler	; Y: let the ROM handle it!

	cmp	ah,READ			;Q: Read request?
	je	short rI13Buffered	; Y: process

	cmp	ah,WRITE		;Q: Write request?
	je	short rI13Buffered	; Y: process

	cmp	ah,LONG+READ		;Q: Long read request?
	je	short rI13Buffered	; Y: process

	cmp	ah,LONG+WRITE		;Q: Long write request?
	je	short rI13Buffered	; Y: process
	jmp	short rfI13oldHandler

rI13oldHandlerX:
	pop	edx

rfI13oldHandler:
	jmp	cs:[OldInt13]


rI13Buffered:
	push	edx
	
	and	edx,0FFh		; drive number
	btr	dx,7			;Q: Is it a hard drive?
	jc	short rI13index		; Y: continue
	add	dx,FixedDrives		; N: floppy data
rI13index:
	cmp	dx,TotalDrives		;Q: Trap this drive?
	jae	short rI13oldHandlerX	; N: let ROM handle it

	bt	cs:[I13DriveTrap],edx	;Q: Trap this drive?
	jnc	short rI13oldHandlerX	; N: let ROM handle it

					;M012: use ah to test for LONG
	test	ah,LONG			;Q: Long sectors?
	jnz	rI13LongIO		; Y: use long sectors

	cmp	al,cs:[SectorsInDMABuffer][edx] 
					;Q: Will it fit in DMA buffer?
	jbe	short rI13oldHandlerX	; Y: let ROM go ahead
	jmp	short rI13ProcessIO	; N: process IO

rI13LongIO:
	cmp	al,cs:[LongSectorsInDMABuffer][edx] 
					;Q: Fit in DMA buffer?
	jbe	short rI13oldHandlerX	; Y: let ROM go ahead

rI13ProcessIO:
	pop	edx
	push	ebx
	push	ecx
	push	edx
	push	esi
	push	es

	push	fs			; 
	push	seg R_CODE		; 
	pop	fs			; 


;
;  Need to trap into protected mode to check for a contiguous buffer for floppy.
;  If buffer is not contiguous, the I/O will be broken down to no larger than
;  the DMA buffer size.
;
rI13SetUpIO:
	or	fs:[TrapFlags],fI13trap	; QHKN:trap this next int 13h
	int	13h			; and set up for next I/O

	push	fs			; save fs as the NCR386SX's SCSI ROM
					; destroys it
	pushf
	cli
	call	cs:[OldInt13]		; Q: Error on the I/O
	pop	fs			; restore fs
	jc	short rI13Return	; Y: return the error

	test	fs:[TrapFlags],fI13trap	;Q: Is the I/O request complete?
	jnz	rI13SetUpIO		; N: set up for the next I/O
	mov	al,cs:[I13SectTrans]	; Y: return total sectors transferred

rI13Return:
	pushf				; need to preserve carry flag
	and	fs:[TrapFlags],not fI13trap ; don't trap next int 13h **128KDMA
	popf
	pop	fs			;
	pop	es
	pop	esi
	pop	edx
	pop	ecx
	pop	ebx
	retf	02				; and return to caller


;===========================================================================
;
;	INT 4B Handler and data
;
;============================================================================

PrevInt4B		dd	0	; The old int4B vector

;==============================================================================
;==
;==  rINT4BhHandler: This is an entry point to the DMA/bus master services.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================

rINT4BhHandler:

	push	bp
	mov	bp,sp
	pushf
	push	fs

	push	seg R_CODE
	pop	fs			
	assume	fs:R_CODE

	cmp	ah,81h			;Q: DMA/bus master service request
	jne	short rI4BoldHandler	; N: chain it down

;
;  Need to trap into protected mode handler.
;
	push	ds

	push	seg ABS0
	pop	ds
	assume	ds:ABS0

	test	[DBSflag],fDBSactive	;Q: Active?
	pop	ds			; - restore DS
	jz	short rI4BoldHandler	; N: chain it down

	or	fs:[TrapFlags],fI4Btrap	; trap into DMA/bus master services

	pop	fs
	popf

	int	4Bh

	push	ax			; save ax
	pushf
	pop	ax			; return flags

	xchg	ax,[bp].INT_FLAGS	; entry flag in AX, return flags on stack

	and	ax,FLAGS_IF		; restore entry IF
	or	[bp].INT_FLAGS,ax

	pop	ax
	pop	bp
	iret

rI4BoldHandler:

	test	fs:[GenFlags], fMCA	; Q: MCA
	jz	short rI4BISA		; N: check is old vector 0 

	push	ds

	push	40h
	pop	ds

	test	byte ptr ds:[7bh], 8	; Q is bit 3 of byte at 40:7b set
	pop	ds

	jnz	short rI4BChain		; Y: chaining required

					
	cmp	word ptr cs:[PrevInt4B+2], 0e000h
					; Q: is previous seg e000
	je	short rI14Biret		; Y: don't chain, return 

	cmp	word ptr cs:[PrevInt4B+2], 0f000h
					; Q: is previous seg f000
	je	short rI14Biret		; Y: don't chain, return 

rI4BISA:
	cmp	cs:[PrevInt4B],0	;Q: Is old vector ZERO?
	jz	short rI14Biret		; Y: don't chain, return

rI4BChain:
	pop	fs
	popf
	pop	bp
	jmp	cs:[PrevInt4B]

rI14Biret:
	pop	fs
	popf
	pop	bp
	iret

;==========================================================================
;
;	Windows Interface Code and data
;
;==========================================================================

	assume	ds:nothing,es:nothing,fs:nothing,gs:nothing

;===========================================================================
;
;   Copy_Inst_Data
;
;   DESCRIPTION:
;   This procedure will copy instance data fields from the Windows/386 3.00
;   initialization data structure that specify instance data in Upper Memory
;   Blocks into a table inside of a LIMulator.	When Copy_Inst_Data returns
;   it will have filled in the LIMulator's table and removed the entries from
;   the Win386 initialization data structure.
;
;   ENTRY:
;	CX = Maximum number of entries to copy (6 bytes each)
;	ES:DI -> Area to copy instance info into
;		 Table should be of size CX*6+4 (add 4 for term dword 0)
;	ESI = Value passed by Windows 3.00 to LIMulator shut down procedure
;
;   EXIT:
;	If carry flag is clear then
;	    Success!
;	    CX = Number of entries NOT used in table
;	    ES:DI -> Byte past null termination dword in table
;	else
;	    ERROR:  Not enough space in table for all instance items
;
;   USES:
;	DS, EAX, EBX, ECX, EDX, ESI, EDI, EBP, Flags
;
;   Version:	1.00
;
;   Date:	24-Jul-1990
;
;   Author:	RAL
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;   24-Jul-1990 RAL Original
;   25-Jul-1990 RAL Updated documentation
;   26-Jul-1990 RAL Fixed 2 bugs
;
;
;==============================================================================

Copy_Inst_Data PROC far

	cld

	mov	bx, si
	and	bx, 1111b
	shr	esi, 4
	mov	ds, si

	cmp	BYTE PTR [bx+4Bh], 1
	jne	CID_Success

	mov	esi, DWORD PTR [bx+26h]
	mov	ebx, esi
	shr	ebx, 4
	mov	ds, bx
	and	si, 1111b

;
;   At this point DS:SI -> Win386_Startup_Info_Struc
;
	push	ds
	push	si

	lds	si, [si.WSSInstData]
	mov	ax, ds				; Just to be paranoid...
	or	ax, si
	jz	CID_Success

;
;   Now move all instance fields from the current table into our private
;   table.  This code will shift all instance fields that are not in the
;   Upper Memory region down.  If there are no instance regions remaining
;   after the loop then it will zero out the SIS_Instance_Data_Ptr in the
;   init data area's fake Int 2Fh data structure.
;
	push	si

	mov	bx, si

CID_Move_Data_Loop:
	lodsd
	test	eax, eax			; Q: At the end?
	jz	CID_At_End_Of_List

	mov	ebp, eax
	shr	ebp, 16
	shl	ebp, 4
	movzx	edx, ax
	add	ebp, edx

	cmp	ebp, 0A0000h			; Q: Is this in a UMB?
	jae	CID_Found_One			;    Y: Copy data into us
						;    N: Shift into correct pos
;
;   This entry does NOT specify instance data in a UMB.  Leave it in the
;   init data table.
;
	mov	DWORD PTR [bx], eax
	lodsw
	mov	WORD PTR [bx+4], ax
	add	bx, 6
	jmp	CID_Move_Data_Loop

;
;   This entry specifies instance data in a UMB.  Copy it into our
;   internal table if enough room remains.
;
CID_Found_One:
	dec	cx				; Q: Enough room left?
	jl	CID_Out_Of_Copy_Space		;    N: ERROR!

	stosd
	movsw
	jmp	CID_Move_Data_Loop

;
;   We're at the end of the list of instance regions.  We may have copied
;   all of the instance regions into our internal table.  If so, then zero
;   the SIS_Instance_Data_Ptr in the fake Int 2Fh startup info data structure
;   to indicate that there is no instance data.  Otherwise, null terminate
;   the table and leave the SIS_Instance_Data_Ptr alone.
;
CID_At_End_Of_List:
	pop	si				; SI->Start of original table
	cmp	bx, si				; Q: Has copy pointer moved?
	je	CID_None_Left_In_List		;    N: The table is empty
						;    Y: Still some in table
	mov	DWORD PTR [bx], eax		; Terminate original table
	add	sp, 4				; Junk DS and SI on stack
	jmp	CID_Success			; Return success!

;
;   All of the instance fields were in UMB's -- Zap the entire table
;
CID_None_Left_In_List:
	pop	si
	pop	ds				; DS:SI -> Startup info struc
	mov	[si.WSSInstData], eax 		; Zero instance table ptr

;
;   In all cases where we return with success, we must null terminate the
;   table.
;
CID_Success:
	xor	eax, eax
	stosd					; Terminate internal list

	clc					; Indicate that it worked
	ret

;
;   ERROR:  Out of space in internal table.  Clear stack and return
;	    with carry flag set to indicate an error.
;
CID_Out_Of_Copy_Space:
	add	sp, 6

	stc					; Indicate an error
	ret

Copy_Inst_Data ENDP

;============================================================================
;
;	M010:
;
;	Procedure Name	: ValidatePath
;
;	Inputs		: None
;
;	Outputs		: CY if unable to access file
;			; NC otherwise
;
;============================================================================

ValidatePath	proc	far

	push	ds
	push	si
	push	bx
	push	dx
	push	cx
	push	ax

	push	cs
	pop	ds

	lea	si,R1_CODE:[CEMMpath]
				; ds:si -> ASCIZ file spec

	mov	bx, 2000h	; read only+compatibilty+supress I24.
	mov	dx, 1		; if file exits open it.
	xor	cx, cx

	mov	ax, 6c00h	; do extended open
	int	21h
       	jc	Vpdone		; error done.

	mov	bx, ax		; bx has handle
	mov	ah, 3eh		; close file
	int	21h
	clc			; it better succeed
Vpdone:
	pop	ax
	pop	cx
	pop	dx
	pop	bx
	pop	si
	pop	ds
	ret

ValidatePath	endp

	
;============================================================================
;
;	real mode XMS entry Code and Data
;
;============================================================================

;=============================================================================
;==	L O C A	L   D A T A
;=============================================================================
PrevXMM		dd	0	; previous XMM handler's entry point

XMMtrapTable	label word
		dw	offset GetVer		; 0
		dw	offset HMA	   	; 1
		dw	offset HMA   		; 2
ifdef MSFLAG
		dw	offset XeTrap	   	; 3
		dw	offset XeTrap		; 4
		dw	offset XeTrap		; 5
		dw	offset XeTrap	   	; 6
		dw	offset XeTrap   	; 7 M005
else
		dw	offset XePrev   	; 3
		dw	offset XePrev   	; 4
		dw	offset XePrev   	; 5
		dw	offset XePrev   	; 6
		dw	offset XePrev   	; 7 M005
endif
		dw	offset XePrev   	; 8
		dw	offset XePrev   	; 9
		dw	offset XePrev   	; 10
		dw	offset XePrev   	; 11
		dw	offset XePrev   	; 12
		dw	offset XePrev   	; 13
		dw	offset XePrev   	; 14
		dw	offset XePrev   	; 15
		dw	offset UMB	   	; 16
		dw	offset UMB   		; 17
MaxXMMtrap	equ	($-XMMtrapTable)/2
;==============================================================================
;==
;==  rXMMentry: CEMM XMM real mode entry point.
;==
;==  Enter:
;==
;==  Exit:
;==
;==============================================================================
rXMMentry proc	far

	jmp	short XeCont	; allow hooking of XMS
	nop
	nop
	nop
XeCont:
	pushf
	push	ecx
	push	es
	push	seg R_CODE
	pop	es
	assume	es:R_CODE

	test	es:[Current_State],fState_Active
	jz	short XePrev

	cmp	ah,MaxXMMtrap
	jae	short XePrev

	movzx	ecx,ah
	jmp	cs:[XMMtrapTable][ecx*2]

XePrev:
	pop	es
	pop	ecx
	popf
	jmp	dword ptr cs:[PrevXMM]	; previous XMS handler

GetVer:
	cmp	es:[HMAptr],100000h	;Q: Virtual HMA?
	je	short XePrev		; N: don't trap
	pop	es
	pop	ecx
	popf
	call	dword ptr cs:[PrevXMM]	; call previous XMS handler
	mov	dx,1			; indicate the HMA exists
	jmp	short XeExit

HMA:
	cmp	es:[HMAptr],100000h	;Q: Virtual HMA?
	je	short XePrev		; N: don't trap
	jmp	short XeTrap

UMB:
	cmp	es:[UMBptr],0		;Q: UMB manager?
	je	short XePrev		; N: don't trap

XeTrap:
	cli				; no interrupts allowed before TRAP
	or	es:[TrapFlags],fXMMtrap	; trap this function
	pop	es
	int	ProtTrap

	pop	ecx
	popf
XeExit:
	ret
rXMMentry	endp


;*****************************************************************************;
;***	XMMAllocateHMA - allocate HMA 					      ;
;									      ;
;	This routine is used to allocate HMA area		 	      ;
;									      ;
;	ENTRY	none		;ds = _DATA				      ;
;	EXIT	CY: set if not available				      ;
;	USES	ax, flags modified					      ;
;									      ;
;*****************************************************************************;
XMMAllocateHMA	proc	far


	push	ax
	push	dx
	push	fs
	mov	dx, seg R_CODE
	mov	fs, dx
	assume	fs:R_CODE

	mov	dx,0FFFFh		; requesting entire HMA
	mov	ah,XMM_REQUEST_HMA
	call	fs:[XMMcontrol]

	or	ax,ax			;Q: Error?
	jz	short XAHerr		; Y: set carry
	or	fs:[GenFlags],fHMA
	clc

XAHexit:
	pop	fs
	pop	dx
	pop	ax
	ret

XAHerr:
	stc
	jmp	short XAHexit

XMMAllocateHMA	endp

;*****************************************************************************;
;***	XMMDeallocateHMA - Deallocate HMA 				      ;
;									      ;
;	This routine is used to deallocate HMA area		 	      ;
;									      ;
;	ENTRY	none		;ds = _DATA				      ;
;	EXIT	CY: set if unable to deallocate HMA			      ;
;	USES	ax, flags modified					      ;
;									      ;
;*****************************************************************************;
XMMDeallocateHMA	proc	far

	push	ax
	push	fs
	mov	ax, seg R_CODE
	mov	fs, ax
	assume	fs:R_CODE

	mov	ah,XMM_RELEASE_HMA
	call	fs:[XMMcontrol]
	or	ax,ax
	jz	short XDHerr
	and	fs:[GenFlags],not fHMA
	clc
XDHexit:
	pop	fs
	pop	ax
	ret

XDHerr:
	stc
	jmp	short XDHexit

XMMDeallocateHMA	endp

;******************************************************************************
;	check_XMM: routine to check presence of XMM driver
;
;	ENTRY:	DS = _DATA
;	EXIT:	[msg_flag] set with error number if error occur
;	USED:	none
;
;******************************************************************************
check_XMM proc	far
;
; determine whether or not an XMM driver is installed
;
	push	ax
	push	fs
	mov	ax,seg R_CODE
	mov	fs,ax
	assume	fs:R_CODE

	and	fs:[GenFlags],not fXMM	; assume XMM not invoked
	mov	ax,XMM_MULTIPLEX SHL 8 + XMM_INSTALL_CHECK
	int	2Fh
	cmp	al,80h			; Q: installed
	jne	short cXMM_no_driver	;   N: set error, quit
;
; get the XMM control functions entry point, save it, we
; need to call it later.
;
	push	bx
	push	dx
	push	es
	mov	ax,XMM_MULTIPLEX SHL 8 + XMM_FUNCTION_ADDR
	int	2Fh
	mov	word ptr fs:[XMMcontrol], bx
	mov	word ptr fs:[XMMcontrol+2],es

;  M009 : EMM386 always uses XMS driver for A20 control
;		But we leave CEMM as it is with the 'ifndef'

ifndef	MSFLAG				; M009
;
;  Has XMM already stolen all extended memory?

	mov	ax,8800h
	int	15h
	or	ax,ax			;Q: XMM invoked?
	jnz	short cXMMexit		; N: don't use it
endif					; M009
	or	fs:[GenFlags],fXMM		; Y: use it for A20 control

cXMMexit:
	clc
	pop	es
	pop	dx
	pop	bx
	pop	fs
	pop	ax
	ret				; done
;
; set carry if XMM driver not present
;
cXMM_no_driver:
	stc
	pop	fs
	pop	ax
	ret

check_XMM	endp

;=========================================================================
;
;	Int 10 and int 11 handlers
;
;=========================================================================

PrevInt10		dd	0	; The old int10 vector
PrevInt11		dd	0	; The old int11 vector

;******************************************************************************
;	Int10_Hook - Int 10 hook when EGA present and using E000h page frame
;
;	This hook is necessary when the E000h segment is used for the page
;	frame and an EGA is in the system.  In this case CEMM "moves" the
;	EGA ROM back to C000h by modifying the page table entry for C000h
;	linear to point to E000h physical.   Additionally, CEMM changes the
;	int 10h, int 1fh, and int 43h vectors.	Changing the int 10h vector
;	could be a problem if a program had already chained into the int 10h
;	vector and had saved the E000 segment for the EGA vector.  If CEMM
;	subsequently "moved" the EGA ROM away from E000h, the int10h chain
;	would not work.  To fix this, CEMM attempts to placed itself at the
;	end of the int10h chain by patching int10h at device driver load
;	time.  This allows CEMM to change the EGA segment in it's in10h
;	hook when necessary and everything should work fine then.
;
;    ENTRY:	Real Mode/Virtual Mode
;
;    EXIT:	same as entry
;		chains on to current "true" int 10h vector.
;
;    USED:	none
;
;******************************************************************************
Int10_Hook	proc	near
	jmp	CS:[PrevInt10]	; chain to previous Int 10h
Int10_Hook	endp

;******************************************************************************
;	Int11_Hook - Int 11 hook when Weitek processor present
;
;	This hook is used when the Weitek processor is present.  It returns
;	bit 23 of EAX = 1 when the Weitek mapping is enabled.
;
;    ENTRY:	Real Mode/Virtual Mode
;
;    EXIT:	bit 32 of EAX = 0 => Weitek mapping is disabled
;		bit 32 of EAX = 1 => Weitek mapping is enabled
;
;    USED:	none
;
;******************************************************************************
Int11_Hook	proc	near
	pushf
	call	CS:[PrevInt11]	; call previous int 11 handler
		; default to Weitek not mapped
	and	eax,((NOT fI11h_Weitek_Map) SHL 16) + 0FFFFh

	push	fs
	push	seg R_CODE
	pop	fs
	assume	fs:R_CODE

	test	fs:[Current_State],fState_Active;Q: CEMM active ?
	jz	SHORT I11_iret			; N: then Weitek not mapped
	test	fs:[Weitek_State],fWeitek_Map	; Y:Q: Weitek mapped ?
	jz	SHORT I11_iret			;    N: say so  ...
	or	eax,(fI11h_Weitek_Map SHL 16)	;    Y: set bit 23
I11_iret:
	pop	fs
	iret
Int11_Hook	endp

;----------------------------------------------------------------------------
;
; Procedure name	: chk_machine_state
;
; ENTRY			:
;
; EXIT			: NC if real mode
;			  CY otherwise
;
;----------------------------------------------------------------------------
chk_machine_state	proc	far
	push	ax

	smsw	ax
        test    ax,MSW_PROTECT  ; Q: is bit 0 set
	pop	ax

	jz	cms_real	; N: machine in real mode
	stc
	ret

cms_real:
	clc
	ret
chk_machine_state	endp

R1_CODE	ends

	end

 	


