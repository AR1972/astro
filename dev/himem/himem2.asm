;/*
; *                      Microsoft Confidential
; *			 Copyright (C) Microsoft Corporation 1988-1992
; *                      All Rights Reserved.
; */
	page	95,160
	title   'HIMEM2 - Initialization code'

funky	segment word public 'funky'
	extrn	InstallMoveBlock:near	; in high segment
	extrn	textseg:word		; in high segment
	extrn	KiddValley:word
	extrn	KiddValleyTop:word
	extrn	segKiddValley:word
	extrn	cHandles:word
	extrn	pack_and_truncate:near
	extrn	MmovSegReinit:far
	extrn	end_of_funky_seg:byte
	extrn	LEnblA20:word
	extrn	LDsblA20:word
funky	ends

	.xlist
	include	himem.inc		; get structures, equ's, etc.
					;  and open segment
	.list

	public	f000
	public	InitInterrupt
	public	MachineNum

;************************************************************************
;*									*
;*  Global Variables							*
;*									*
;************************************************************************

	extrn	pPPFIRET:word
	extrn	dd_int_loc:word
	extrn	Interrupt:near
	extrn	hiseg:word
	extrn	call_hi_in_di:near
	extrn	fCanChangeA20:byte
	extrn	fHMAMayExist:byte

	extrn	fVDISK:byte
	extrn	IsVDISKIn:near

	extrn	A20Handler:near
	extrn	EnableCount:word
	extrn	MemCorr:word
	extrn	MinHMASize:word

ifdef debug_tsr
	extrn	MoveIt:dword
else
	extrn	MoveIt:word
endif

	extrn	pReqHdr:dword
	extrn	PrevInt2f:dword
	extrn	TopOfTextSeg:word

	extrn	InstallA20:near
	extrn	Int2fHandler:near
	extrn	Is6300Plus:near
	extrn	IsA20On:near

	extrn	pAddMem:word
	extrn	SignOnMsg:byte
	extrn	ROMDisabledMsg:byte
	extrn	UnsupportedROMMsg:byte
	extrn	ROMHookedMsg:byte
	extrn	BadDOSMsg:byte
	extrn	NowInMsg:byte
	extrn	On8086Msg:byte
	extrn	NoExtMemMsg:byte
	extrn	FlushMsg:byte
	extrn	StartMsg:byte
	extrn	HandlesMsg:byte
	extrn	HMAMINMsg:byte
	extrn	KMsg:byte
	extrn	NoHMAMsg:byte
	extrn	A20OnMsg:byte
	extrn	HMAOKMsg:byte
	extrn	VDISKInMsg:byte
	extrn	BadArgMsg:byte

	extrn	DevAttr:word
	extrn	Int15MemSize:word
	extrn	pInt15Vector:word

	extrn	PrevISAInt15:dword
	extrn	ISA15Handler:near

	extrn	EndText:byte

	extrn	XMMControl:far
	extrn	pfnEnabA20:word
	extrn	pfnDisabA20:word
	extrn	LocalEnableA20:near
	extrn	LocalDisableA20:near

;************************************************************************
;*									*
;*   Code/Data below here will be discarded after driver initialization *
;*									*
;************************************************************************

;	Discardable Initialization Data

pCOMPAQ		label	dword	; Pointer to COMPAQ specific for BIM
		dw	0FFE8h	;
		dw	0F000h	;
pBIMGDT 	dw	offset BIMGDT	    ; Offset to BIMGDT for BIM alloc'n
GetBIMMemProc	dw	offset GetBIMMemory ; Offset of GetBIMMemory procedure
BIMBase 	dw	0	; Base address and Lenght of remaining Compaq
BIMLength	dw	0	;   Built-In Memory (set at Init time)

BIMBuffer	dw	?	; Buffer for unlock/lock ROM for BIM
	public	fShadowOff, f1stWasWarning

fShadowOff	db	0	; NZ if shadow RAM should be disabled,
				;   0/1 set by command line switch, 0FFh
				;   set if little extended and hope to disable

f1stWasWarning	db	0	; NZ if 1st attempt to diddle A20 generated
				; a warning (and not an error)
	public	fA20Control

fA20Control	db	0ffh	; NZ if himem should take control of A20, even
				;   it was already on when himem loaded.

	public	fCPUClock

fCPUClock	db	0	; NZ if himem should try to preserve CPU clock
				;   speed when gating A20

fEISA		db	0	; NZ if himem should take all EISA memory
				;   blocks
		public	fQuiet
fQuiet		db	0ffh	; NZ if himem should be quiet during install

fSignOnMsg	db	0	; NZ if sign on msg has been displayed

fBigMem 	db	0ffh	; NZ if ISA memory above 16 meg allowed

fNumHandSet	db	0	; NZ if /NUMHANDLES= parameter used
fHMAminSet	db	0	; NZ if /HMAMIN= parameter used


; Parameter tables for GetParms

ParmEntry   struc
cParmChar   db	    0		; 1st char of option on cmd line
pParmRtn    dw	    0		; offset of routine to process option
ParmEntry   ends

; The 1st table is for parameters that have no arguments

ParmTbl1 label	 byte
	ParmEntry   <'E',GPGotEisa>		; /EISA
	ParmEntry   <'V',GPGotVerbose>		; /VERBOSE
	ParmEntry   <'N',GPGotBigMem>		; /NOABOVE16
	ParmEntry   <'Z',GPZSwitch>		; /Z
	db	0				; ** END OF TABLE **

; The 2nd table is for parameters with arguments

ParmTbl2 label	 byte
	ParmEntry   <'H',GPGotMin>		; /HMAMIN=
	ParmEntry   <'N',GPGotHands>		; /NUMHANDLES=
	ParmEntry   <'M',GPGotMachine>		; /MACHINE:
	ParmEntry   <'A',GPGotA20Control>	; /A20CONTROL:
	ParmEntry   <'S',GPGotShadow>		; /SHADOWRAM:
	ParmEntry   <'I',GPGotInt15>		; /INT15=
	ParmEntry   <'C',GPGotCPUClock> 	; /CPUCLOCK:
	db	0				; ** END OF TABLE **


	public	StringParm, MachineNum, MachineName

StringParm	db	13 DUP (' ')

MachineNum	dw	-1

;  Note: the following table MUST be in the same order as the entries in the
;  A20_Scan_Table!  If you add entries here, also add one there!

MachineName	label	byte
	db	'ptlcascade',0		; Phoenix Cascade BIOS
	db	'att6300plus',0 	; AT&T 6300 Plus
	db	'ps2',0 		; IBM PS/2
	db	'hpvectra',0		; HP 'Classic' Vectra (A & A+)
	db	'acer1100',0		; Acer 1100
	db	'toshiba',0		; Toshiba 1600 & 1200XE
	db	'wyse',0		; Wyse 12.5 MHz 286 machine
	db	'tulip',0		; Tulip machines
	db	'zenith',0		; Zenith ZBIOS
	db	'at1',0 		; IBM AT/delay 0
	db	'at2',0 		; IBM AT/delay 1
	db	'at3',0 		; IBM AT/delay 2
	db	'philips',0		; Philips machines
	db	'css',0			; CSS Lab machines
	db	'fasthp',0		; Single byte method for HP Vectras
	db	'ibm7552',0		; IBM 7552 Industrial Computer
	db	'bullmicral',0		; Bull Micral 60 M004
	db	'dell',0		; DELL XBIOS		; M010
	db	'at',0			; IBM AT
	db	0FFh			; end of table

;NOTE: there is code in GetParms which depends on AltNameTbl coming
;      after MachineName table.

	public	AltName1, AltName2, AltName3, AltName4, AltName5
	public	AltName6, AltName7, AltName8, AltName9, AltName10
	public	AltName11, AltName12, AltName13, AltName14, AltName15
	public	AltName16,AltName17	;M004

AltNameTbl	label	byte
AltName3    db	'3',0			; Phoenix Cascade BIOS
AltName5    db	'5',0			; AT&T 6300 Plus
AltName2    db	'2',0			; IBM PS/2
AltName4    db	'4',0			; HP 'Classic' Vectra (A & A+)
AltName6    db	'6',0			; Acer 1100
AltName7    db	'7',0			; Toshiba 1600 & 1200XE
AltName8    db	'8',0			; Wyse 12.5 Mhz 286 machine
AltName9    db	'9',0			; Tulip machine
AltName10   db	'10',0			; Zenith ZBIOS
AltName11   db	'11',0			; IBM AT/delay 0
AltName12   db	'12',0			; IBM AT/delay 1
AltName13   db	'13',0			; IBM AT/delay 2
	    db	'13',0			; Philips machines (same as AT3)
	    db	'12',0			; CSS machines
AltName14   db	'14',0			; Single byte HP Vectra m/cs
AltName15   db	'15',0			; IBM 7552 Industrial Computer
AltName16   db	'16',0			; Bull Micral 60          M004
AltName17   db	'17',0
AltName1    db	'1',0			; IBM AT
	    db	0FFh			; end of table

ifdef	debug_tsr	;-----------------------------------------------

;*----------------------------------------------------------------------*
;*									*
;*  ExeStart -								*
;*									*
;*	Entry point when himem is invoked as an .EXE.			*
;*									*
;*----------------------------------------------------------------------*

lpCmdLine	dd	81h		; far ptr to command tail

	public	ExeStart

ExeStart:

	mov	word ptr cs:[lpCmdLine+2],es	; save PSP segment in pointer

	mov	ax,cs		; Setup segment regs to all be the same
	mov	ds,ax
	mov	es,ax

	call	InitDriver	; Initialize...

	mov	ax,TopOfTextSeg	; TopOfTextSeg == 0 is error installing
	or	ax,ax
	jnz	@f

	mov	ax,4C03h	; error, so just terminate
	int	21h
@@:
	mov	di,offset pack_and_truncate
	jmp	call_hi_in_di	; terminate and stay resident

endif			;------------------------------------------------



;*----------------------------------------------------------------------*
;*									*
;*  InitInterrupt -							*
;*									*
;*	Called by MS-DOS immediately after Strategy routine		*
;*									*
;*  ARGS:   None							*
;*  RETS:   Return code in Request Header's Status field		*
;*  REGS:   Preserved							*
;*									*
;*	This entry point is used only during initialization.		*
;*	It replaces itself with a much shorter version which only	*
;*	serves to report the appropriate errors when this driver	*
;*	is called in error.						*
;*									*
;*----------------------------------------------------------------------*

InitInterrupt   proc    far

	; Save the registers including flags.

	push    ax		; We cannot use pusha\popa because
	push    bx		;	we could be on an 8086 at this point
	push    cx
	push    dx
	push    ds
	push    es
	push    di
	push    si
	push    bp
	pushf

	push	cs		; Set DS=CS for access to global variables.
	pop	ds

	les	di,[pReqHdr]	; ES:DI = Request Header

	mov     bl,es:[di].Command ; Get Function code in BL

	or	bl,bl		; Only Function 00h (Init) is legal
	jz	IInit

	cmp     bl,16		; Test for "legal" DOS functions
	jle     IOtherFunc

IBogusFunc:
	mov     ax,8003h	; Return "Unknown Command"
	jmp     short IExit

IOtherFunc:
	xor     ax,ax		; Return zero for unsupported functions
	jmp     short IExit

IInit:
	call    InitDriver	; Initialize the driver
	les	di,[pReqHdr]	; Restore es:di = Request Header

IExit:
	or	ax,0100h	; Turn on the "Done" bit
	mov	es:[di].Status,ax ; Store return code

	popff			; restore the registers
	pop	bp
	pop	si
	pop	di
	pop	es
	pop	ds
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	mov	dd_int_loc,offset Interrupt	; replace Interrupt with
	ret					; tiny permanent stub

InitInterrupt   endp

;*----------------------------------------------------------------------*
;*									*
;*  InitDriver -							*
;*									*
;*	Called when driver is Initialized.				*
;*									*
;*  ARGS:   ES:DI = Address of the Request Header			*
;*  RETS:   pHdr.Address = Bottom of resident driver code		*
;*  REGS:   AX, CX and Flags are clobbered				*
;*									*
;*----------------------------------------------------------------------*

	public	InitDriver

InitDriver  proc    near

	cld

ifndef	debug_tsr
	call	LocateHiSeg	; locate the hiseg in low memory properly

	mov	di, offset MMovSegReinit
	mov	es, hiseg
	push	cs
	call	call_hi_in_di	; re-initialize the mem mover

	mov	ax,cs
	push	es
	mov	es,hiseg
	assume	es:funky
	add	textseg,ax	; relocate text segment pointer
	add	LEnblA20+2, ax	; update ptrs to enble & disable a20 rtns
	add	LDsblA20+2, ax
	pop	es
	assume	es:nothing
endif

	mov	ah,30h		; make sure we've got DOS 3.00 or higher
	int	21h		; Get DOS versions number
	cmp	al,3
	jae     IDCheckXMS

	mov	dx,offset BadDOSMsg
	jmp	IDFlushMe

IDCheckXMS:
	mov	ax,(INT2F_ID SHL 8) OR INT2F_INS_CHK
	int	2Fh		; make sure there's no other XMS installed
	cmp	al,80h		; Is INT 2F hooked?
	jne     IDNotInYet
	mov	dx,offset NowInMsg
	jmp	IDFlushMe

IDNotInYet:
	xor     ax,ax			; Move 0 into the Flags register
	push    ax
	popf
	pushf				; Try and get it back out
	pop     ax
	and     ax,0F000h		; If the top four bits are set...
	cmp     ax,0F000h
	mov	dx,offset On8086Msg	; we're on an 8086
	jnz	@f
	jmp	IDFlushMe		; so crap out
@@:
	call	GetInt15Memory		; If Int 15h/88h reports < 384k of
	cmp	ax,384			;   extended memory available, then
	jae	@f			;   we will try to increase memory
	mov	fShadowOFF,0FFh 	;   by stealing shadow RAM
@@:
	call	GetParms		; process command line parameters

	mov	ah, 2			; Force VERBOSE mode if user is
	int	16h			;   holding down the Alt key
	test	al, 08h 		; Int 16h/2 == Get Keyboard Flags
	jz	@f			;   08h set if Alt key down
	mov	fQuiet, 0
@@:

ifndef	debug_tsr
	call	DispSignOnMsg
endif
	mov	di,offset InstallMoveBlock ; install moveblock function
	push	cs			; setup for far return
	call	call_hi_in_di		; call into high segment

	call	IsA20On 		; Is A20 already enabled?
	or	ax,ax			;   (may zap cx, si, di)
	jz	IDInsA20		;   no, go install A20 handler

	mov	dx,offset A20OnMsg	; "A20 already on" message
	cmp	fA20Control,0		; should we take control of A20 anyway?
	jne	IDInsA20		;   yes, go muck with it
	mov	[fCanChangeA20],0	;   no,  don't allow changing of A20
	call	DispInfoMsg		;	 and tell user about it
	jmp	short IDAfterA20

IDInsA20:
	call	InstallA20		; install proper A20 handler
	jnc	@f
	jmp	IDFlushMe		; CY means fatal error
@@:

;	Note:  A side affect of the previous InstallA20 is that MemCorr
;	  is set to reflect the adjustment factor if we're on an AT&T 6300+

IDAfterA20:
	call	InitHandles	; initialize handle table

	call	ScanEISA	; scan EISA memory into table

	call	CheckZBim	; Check for & allocate Zenith BIM

	call	CheckBIM	; Check for & allocate Compaq BIM

;	Turn off shadow RAM if desired/possible

	cmp	fShadowOFF,0	; should shadow RAM be turned off?
	jz	@f

	call	ShadowRAMOff	; try to turn it off--also adds to free list

	or	dx,dx		; display a msg if ShadowRAMOff is
	jz	@f		;   pointing to one
	call	DispInfoMsg
@@:
	call	IsVDISKIn		; Is a VDISK style allocator already
	cmp	[fVDISK],0		;   installed?	Don't bother to load
	jz	@f			;   if so, cause we'll just fail all
	mov	dx,offset VDISKInMsg	;   future calls anyway...
	jmp	short IDFlushMe
@@:

;	Looks like himem will install, allocate Big ISA memory (above 16 meg)
;	now if available

	cmp	[fBigMem], 0		; Was parameter specified?
	je	@f			; NO: don't bother trying to get it
	call	GetBIGMemory		; Get extra ISA memory
@@:

	call    GetInt15Memory	; how much extended memory is installed?
	cmp     ax,64		; Is there >= 64K of extended?
	jae	IDHMAOK

	push	es
	mov	es,hiseg
	assume	es:funky
	mov	bx,[KiddValley]	; get size of memory we already have in tables
	mov	cx,[cHandles]

IDAnyMem:
	cmp	[bx].Flags,FREEFLAG
	jnz	IDAnyMem_1	; brif not a valid free block
	mov	ax, [bx].Len.hi
	or	ax, [bx].Len.lo
	jnz	IDAnyMem_2
IDAnyMem_1:
	add	bx,SIZE Handle
	loop	IDAnyMem
IDAnyMem_2:
	pop	es
	assume	es:nothing

	mov	dx,offset NoHMAMsg
	or	ax,ax			; no HMA, any other memory to control?
	jnz	disp_hma_msg		; jmp if some memory

;	We can't find any memory to manage.

	mov	dx,offset NoExtMemMsg

;	Display the message in DX followed by the "Flush" message.

IDFlushMe:
	call	DispErrMsg
	mov	dx,offset FlushMsg
	call	DispErrMsg

	xor	ax,ax			; discard the driver
	mov	[TopOfTextSeg],ax

ifndef	debug_tsr			;-------------------------------
	les	di,[pReqHdr]
	mov	es:[di].Units,al
	and	cs:DevAttr,not 8000h	; clr bit 15 in attrib of driver header
endif
	jmp	short IDReturn		;-------------------------------

IDHMAOK:
	mov     [fHMAMayExist],1
	mov	dx,offset HMAOKMsg
disp_hma_msg:
	call	DispInfoMsg

	call    HookInt2F		; "turn on" the driver

;	Initialization finished (or failed) -- return to caller

IDReturn:

ifndef	debug_tsr			;-------------------------------
	mov	di,offset pack_and_truncate
	jmp	call_hi_in_di		; pack stuff down and terminate
else
	mov	ax, hiseg		; make sure far pointer to handle
	mov	es, ax			;   table has correct segment
	mov	es:[segKiddValley], ax
endif					;-------------------------------
	ret

InitDriver	endp

;----------------------------------------------------------------------------
;  DispXxxMsg Routines -- Display informational, error or sign on messages,
;			  if enabled by /VERBOSE parameter.  Error msgs
;			  are displayed regardless of /VERBOSE.
;  Entry:
;	DX = offset of msg to display
;  Exit:
;
;  Used:
;	AX
;----------------------------------------------------------------------------
	public	DispInfoMsg, DispInfoChar, DispErrMsg

DispInfoMsg	proc	near
	cmp	cs:[fQuiet], 0		; display an informational msg if
	jnz	short DIM_ret		;   not in QUIET mode
	call	DispSignOnMsg		; make sure user sees signon msg
	mov	ah, 9
	int	21h
DIM_ret:
	ret
DispInfoMsg	endp


DispInfoChar	proc	near
	cmp	cs:[fQuiet], 0
	jnz	short DIC_ret
	call	DispSignOnMsg		; make sure user sees signon msg
	mov	ah, 2
	int	21h
DIC_ret:
	ret
DispInfoChar	endp


DispErrMsg	proc	near		; display an error msg
	mov	cs:[fQuiet], 0		; no longer in QUIET mode
	call	DispSignOnMsg		; make sure user sees signon before
	mov	ah, 9			;   error msg
	int	21h
	ret
DispErrMsg	endp


DispSignOnMsg	proc	near
	cmp	cs:[fQuiet], 0		; don't signon if in QUIET mode
	jnz	short DSM_ret
	cmp	cs:[fSignOnMsg], 0	; don't signon more than once
	jnz	short DSM_ret
	push	dx			; display the signon message
	push	ds
	push	cs			; in case ds != _text
	pop	ds
	mov	ah, 9
	mov	dx,offset SignOnMsg
	int	21h
	mov	cs:[fSignOnMsg], 0FFh
	pop	ds
	pop	dx
DSM_ret:
	ret
DispSignOnMsg	endp

;
;----------------------------------------------------------------------------
; procedure : LocateHiSeg
;
;		Locate the movable segment properly in the low seg.
;		taking care of the stripped ORG zeroes. This function
;		calculates the segment at which the hiseg should run
;		with the ORG. If the segment cvalue goes below zero the
;		code is moved up high enough to run the code from a seg value
;		of zero.
;
;		This function assumes that the 'funky' segment follows
;		immediately after the text seg.
;
;----------------------------------------------------------------------------
; 
LocateHiSeg	proc	near
	push	ds
	mov	ax, cs				; para start of text seg
	mov	cx, offset _text:EndText	; end of text seg
	add	cx, 15				; para round it
	shr	cx, 1
	shr	cx, 1
	shr	cx, 1
	shr	cx, 1
	add	ax, cx				; para start of funky seg
	cmp	ax, (HISEG_ORG shr 4)		; will the seg go below zero?
	jb	MoveHiSeg			; yeah, we have to move it
	sub	ax, (HISEG_ORG shr 4)		; no, it fits in
	pop	ds
	mov	hiseg, ax			; update the segment in which
						;   it is going to run from.
	ret
MoveHiSeg:
	mov	ds, ax				; segment at which funky
						;  resides without the ORG
 	xor	ax, ax
 	mov	es, ax				; we want to movve the code
						;  to 0:HISEG_ORG
	mov	di, offset funky:end_of_funky_seg
	mov	si, di
	sub	si, HISEG_ORG
	mov	cx, si
	dec	di
	dec	si
  	std					; move backward (safe when
						;  source & dest overlap
    	rep	movsb
	cld
    	pop	ds
    	mov	hiseg, 0			; funky is going to run from
						;  segment zero
    	ret
LocateHiSeg	endp


;*----------------------------------------------------------------------*
;*									*
;*  HookInt2F -								*
;*									*
;*	Insert the INT 2F hook						*
;*									*
;*  ARGS:   None							*
;*  RETS:   None							*
;*  REGS:   AX, SI, ES and Flags are clobbered				*
;*									*
;*----------------------------------------------------------------------*

	public	HookInt2F

HookInt2F   proc    near

	cli
	xor	ax,ax
	mov	es,ax
	mov	si,2Fh * 4		; save previous int2f vector
	mov	ax,offset Int2FHandler	; and exchange with new one
	xchg    ax,es:[si][0]
	mov	word ptr [PrevInt2F][0],ax
	mov	ax,cs
	xchg    ax,es:[si][2]
	mov	word ptr [PrevInt2F][2],ax
	sti
	ret

HookInt2F   endp



;*----------------------------------------------------------------------*
;*									*
;*  ShadowRAMOff -							*
;*									*
;*	Attempt to turn off shadow RAM on selected machines.  Add the	*
;*	memory to our free pool if successful.				*
;*									*
;*  ARGS:   None							*
;*  RETS:   DX	- Msg to show user (or 0), &				*
;*	    CY clear - success, 					*
;*	    CY set   - warning/failed					*
;*  REGS:   AX destroyed						*
;*									*
;*----------------------------------------------------------------------*

	public	ShadowRAMOff

ShadowRAMOff	proc	near

	push	es
	push	di
	push	si
	push	cx
	push	bx

	call	Compaq386		; on a Compaq 386?
	jz	SOIsCompaq
	jmp	SONotCompaq

;	Disable shadow RAM on a Compaq 386

SOIsCompaq:
					; check if the video bios points
	xor	ax,ax			;   into the ROM copy, we don't
	mov	es,ax			;   want to disable if someone has
	mov	ax,es:[10h*4+2] 	;   already hooked Int 10h, 1Fh, 43h,
	call	IsC000orE000		;   or 6Dh
	jnz	SORomHooked
	mov	ax,es:[1Fh*4+2]
	call	IsC000orE000
	jnz	SORomHooked
	mov	ax,es:[43h*4+2]
	call	IsC000orE000
	jnz	SORomHooked

	cmp	word ptr es:[6dh*4+2],0C000h	; finally, make sure Int 6Dh
	jae	SOVideoOkay			;   points somewhere reasonable

SORomHooked:
	mov	dx,offset ROMHookedMsg	; somebody has video ROM hooked!
	jmp	short SONoCanDo

SOVideoOkay:

;	Check to see if this machine allows the ROM to be unmapped

	call	unmap_permited
	jc	SOROMNotSupported

;	Looks good, lets disable the RAM copy.	This is done by doing
;	an Int 15h blockmove of a command word to the Compaq diagnostic
;	register.  We share data structures with the Compaq BIM code.

	mov	si,pBIMGDT		; setup block move GDT
	mov	ax,cs
	mov	es,ax
	mov	cx,16
	mul	cx
	add	ax,offset BIMBuffer
	adc	dl,0
	mov	[si].S_BASE_LOW,ax
	mov	[si].S_BASE_HI,dl
	mov	cx,1

	mov	word ptr [BIMBuffer],0FFFFh	; disable ROM replace command

	mov	ah,87h
	int	15h

;	Point the video vectors back at the original ROM

	mov	ax,0
	mov	es,ax
	mov	ax,0C000h
	mov	es:[10h*4+2],ax
	mov	es:[1Fh*4+2],ax
	mov	es:[43h*4+2],ax

;	If Int 6Dh points at the now disabled ROM, change that too

	cmp	word ptr es:[6Dh*4+2],0E000h
	jnz	@f
	mov	es:[6Dh*4+2],ax
@@:

;	Add the ram space to our free list

	mov	ax,128				;128k @ FE0000h
	mov	cx,3F80h			;  (FE0000h / 1024)
	xor	dx, dx
	mov	bx, dx

	mov	di, pAddMem
	push	cs
	call	call_hi_in_di

	mov	dx,offset ROMDisabledMsg	; Success!
	clc

SODone:
	pop	bx
	pop	cx
	pop	si
	pop	di
	pop	es
	ret


SONotCompaq:
SOROMNotSupported:

	mov	dx,offset UnsupportedROMMsg	; don't know how to disable

SONoCanDo:					; only show msg if user wanted
	cmp	fShadowOFF,0FFh 		; to disable--if trying cause
	jnz	SOFailed			; of too little extended mem,
	xor	dx,dx				; keep quiet.

SOFailed:
	stc
	jmp	short SODone

ShadowRAMOff	endp


IsC000orE000	proc

	cmp	ax,0C000h			; Video ROM pointers can
	jz	@f				;   point to either of
	cmp	ax,0E000h			;   these locations, but
@@:	ret					;   no others.

isC000orE000	endp


;------------------------------------------------------------------------
;  UNMAP_PERMITED - This function will check if unmap roms is available.
;
;  Entry:    Nothing.
;
;  Returns:  Carry Flag Clear -  Can Unmap
;	     Carry Flag Set   -  Can Not Unmap
;
;------------------------------------------------------------------------

MACHINE_ID    equ  25h

unmap_permited	proc near		; == Check if unmap roms permitted ==

	mov	al, MACHINE_ID		; CMOS location to read
	out	70h, al 		; Out CMOS location address
	in	al, 71h 		; Read data at that location
	or	al, al			; Q: Is it Zero
	jz	unmap_permited_error	;  Y: Can not unmap
	test	al, 1000b		; Q: can we unmap (Soft drive type)
	jz	unmap_permited_ok	;  Y: continue

unmap_permited_error:
	stc				; Set error condition - Can not unmap

unmap_permited_ok:
	ret				; Done

unmap_permited	endp

;*----------------------------------------------------------------------*
;*									*
;*  GetBIMMemory -							*
;*									*
;*	Look for Compaq 'Built In Memory' and add it to the pool of	*
;*  available memory							*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = Amount of BIM memory found				*
;*  REGS:   AX, BX, CX, and Flags are clobbered				*
;*									*
;*----------------------------------------------------------------------*

; "Built In Memory" (BIM) starts at FE00:0000h and grows downward.  It is
; controlled by a data structure at F000:FFE0h.  Changing the data structure
; involves un-write-protecting the ROMs (!) by flipping bit 1 of 80C00000.

pBIMSTRUCT  equ     0FFE0h
AVAILABLE   equ     0		; Set to -1 if BIM isn't around
TOTALBIM    equ     2		; Total amount of BIM in the system
AVAILBIM    equ     4		; Amount of BIM available in paragraphs
LASTUSED    equ     6		; Paragraph address of last (lowest) used
				;  paragraph of BIM

GDT_TYPE    struc
	dw	0,0,0,0
	dw	0,0,0,0

S_LIMIT     dw	1
S_BASE_LOW  dw	0
S_BASE_HI   db	0
S_RIGHTS    db	93h
S_RESERVED  dw	0

D_LIMIT     dw	1
D_BASE_LOW  dw	0000h
D_BASE_HI   db	0C0h
D_RIGHTS    db	93h
D_RES386    db	0
D_BASE_XHI  db	080h

	dw	0,0,0,0
	dw	0,0,0,0
GDT_TYPE    ends

BIMGDT	GDT_TYPE <>


GetBIMMemory proc near

	xor     ax,ax

;	Are we on a Compaq 386 machine?

	push    bx
	push    es

;	Is there a 32-bit memory board installed?

	mov	es,cs:WORD PTR (pCOMPAQ+2) ; ROM segment
	mov     bx,pBIMSTRUCT
	mov     bx,es:[bx]		; De-reference the pointer
	mov     dx,es:[bx+AVAILABLE]    ; -1 means no board is installed
	inc     dx
	jz	FCMNoMem2		; Nope, return

;	How much memory is available and where does it start?

	mov     dx,es:[bx+AVAILBIM]     ; Size in paragraphs
	or	dx,dx			; Any left?
	jz	FCMNoMem2
	mov     cx,dx			; CX = Size in paragraphs
	mov     ax,es:[bx+LASTUSED]
	sub     ax,cx			; AX = Starting location - F0000h
					;  in paragraphs
	push    es			; Save for a rainy day...
	push    bx
	push    ax

	shr     ax,4			; get base address in KBytes
	add     ax,0f000h
	shr     ax,2

	shr     cx,6			; get the size in Kbytes

	mov     [BIMBase],ax		; store for use by HookInt15
	mov     [BIMLength],cx

;	Un-WriteProtect the ROMs.

	mov     si,pBIMGDT		; Set up the BlockMove GDT
	mov     ax,cs
	mov     es,ax
	mov     cx,16
	mul     cx
	add     ax,offset BIMBuffer
	adc     dl,0
	mov     [si].S_BASE_LOW,ax
	mov     [si].S_BASE_HI,dl
	mov     cx,1

	mov     word ptr [BIMBuffer],0fefeh ; 0feh unlocks the ROMs

	mov     ah,87h			; Do the BlockMove
	int     15h
	or	ah,ah			; Was there an error?
	jz	FCMReserve		; Nope - continue

;	Return error.

	pop     ax			; Clean up
	pop     bx
	pop     es
	xor     ax,ax
	mov     [BIMBase],ax
	mov     [BIMLength],ax
FCMNoMem2:
	jmp     short FCMNoMem

;	Change the ROM values to reserve the BIM stuff.

FCMReserve:
	pop     ax
	pop     bx
	pop     es
	mov     word ptr es:[bx+AVAILBIM],0 ; Reserve all remaining BIM
	mov     word ptr es:[bx+LASTUSED],ax

;	Re-WriteProtect the ROMs.

	push    cs
	pop     es
	mov     si,pBIMGDT		; Set up the BlockMove GDT

	mov     word ptr [BIMBuffer],0fcfch ; 0fch locks the ROMs

	mov     ah,87h			; Do the BlockMove
	int     15h

	mov     ax,1			; Return success

FCMNoMem:
	pop     es
	pop	bx
	ret
EndGetBIMMemory:

GetBIMMemory endp


;*----------------------------------------------------------------------*
;*									*
;*  CheckBIM -								*
;*									*
;*	Check if Compaq built in memory is available			*
;*									*
;*  ARGS:   AX = Kbytes of int 15 memory				*
;*  RETS:   AX = Amount of BIM and int 15 memory			*
;*  REGS:   AX and Flags are clobbered					*
;*									*
;*----------------------------------------------------------------------*

	public	CheckBIM

CheckBIM  proc	near
	push	es
	push	bx
	push	dx

	call	Compaq386	; Are we on a Compaq 386 machine?
	jnz	CBNoBIM 	; N: return

	mov	bx,pBIMSTRUCT	; ROM data sructure
	mov	bx,es:[bx]	; pointer to BIM data structure
	mov	dx,es:[bx+AVAILABLE] ; -1 means no board is installed
	inc	dx		;Q: Is there a 32-bit memory board installed?
	jz	CBNoBIM		; N: return
				; Y: How much memory is available?
	mov	dx,es:[bx+AVAILBIM] ; Size in paragraphs
	or	dx,dx		;Q: Any BIM available?
	jz	CBNoBIM		; N: return

	call	getBIMMemory	; allocate it and get base/size

;	Just allocate the damn BIM memory now.  (Used to be a big headache
;	  to install a deferred BIM driver, depending on whether Shadow
;	  RAM was disabled.)

	mov	cx,[BIMBase]		;   add Compaq BIM to our free list
	jcxz	CBNoBIM
	mov     ax,[BIMLength]
	xor	dx, dx
	mov	bx, dx

	mov	di, pAddMem
	push	cs
	call	call_hi_in_di

CBNoBIM:
	pop	dx
	pop	bx
	pop	es
	ret
CheckBIM  endp


;*----------------------------------------------------------------------*
;*									*
;*  Compaq386 - 							*
;*									*
;*	Check if running on a Compaq 386 system.			*
;*									*
;*  ARGS:   none							*
;*  RETS:   Z set - on a Compaq 386, NZ - not Compaq 386		*
;*	    ES = ROM segment						*
;*  REGS:   Flags are clobbered 					*
;*									*
;*----------------------------------------------------------------------*

szCOMPAQ    db	'03COMPAQ'

	public	Compaq386

Compaq386	proc	near

	push	si
	push	di
	push	cx

	les	di,cs:pCOMPAQ		; Are we on a Compaq 386 machine?
	mov	si,offset szCOMPAQ
	mov	cx,8
	cld
	rep	cmpsb			;Q: Are we?

	pop	cx
	pop	di
	pop	si

	ret

Compaq386	endp

;*----------------------------------------------------------------------*
;*									*
;*  CheckZBim -								*
;*									*
;*	Check if Zenith Bim (0fa0000h x 256K) is available		*
;*	   Call AddMem with it if so					*
;*									*
;*----------------------------------------------------------------------*

ZDS_string	db	'ZDS CORP'
ZDS_len	=	($-ZDS_string)

f000		dw	0f000h

	public	CheckZBIM

CheckZBim	proc	near

	call	GetInt15Memory		; This code added for old systems with
	cmp	ax, 2048		;   low memory, skip it if 'sufficient'
	jae	no_ZBIM 		;   ext memory cause it scribbles on
					;   memory just below 16 meg

	cld				; better safe than sorry

	mov	si,offset ZDS_string
	mov	es,f000
	mov	di,800ch		; look for ZDS CORP at f000:800c
	mov	cx,ZDS_len
	repz	cmpsb
	jnz	no_ZBim			; done if not Zenith machine

;	Now we've got to go out and look for 256KBytes at 0fa0000h
;	  Let's use the BlockMove function to help us with that so we
;	  don't have to write any messy protect mode code.  We'll
;	  snag a handle and set up a dummy block while we work.

	cli
	push	es
	mov	es,hiseg
	assume	es:funky
	mov	di,[KiddValley]
	mov	cx,[cHandles]

chk_zb0:
	cmp	[di].Flags,UNUSEDFLAG
	jz	ZBim_01			; brif found free handle
	add	di,SIZE Handle
	loop	chk_zb0
	pop	es
	sti				; restore interrupts
no_ZBim:
	ret

ZBim_01:
	mov	[di].cLock,1		; lock it
	mov	[di].Base.lo,3e80h	; number of K at 0fa0000h
	mov	[di].Base.hi,0
	mov	[di].Len.lo,256
	mov	[di].Len.hi, 0
	mov	[di].Flags, USEDFLAG
	pop	es
	assume	es:nothing
	sti

	mov	save_ss,ss		; get more elbow room for this
	mov	save_sp,sp
	push	cs
	pop	ss
	mov	sp,offset EISA_stack

;	allocate a move structure on the stack
;	  for now, we'll just test the first 512 bytes.  It might be
;	  a good idea to test the whole damn thing, but a lot more work.

	sub	sp,0fffeh and (1+SIZE MoveExtendedStruc)
	push    ss			; point es:si to stack frame
	pop	es
	mov	si,sp

	mov	es:[si].SourceHandle,0	; move from conventional memory
	mov	es:[si].DestHandle,di	; to our fake block at 0fa0000h
	mov	es:word ptr [si].DestOffset,0 ; move data to base of block
	mov	es:word ptr [si].DestOffset+2,0

	mov	es:word ptr [si].SourceOffset,8000h
	mov	es:word ptr [si].SourceOffset+2,0f000h	; move from ROM
	mov	es:word ptr [si].bCount,512	; move 512 bytes
	mov	es:word ptr [si].bCount+2,0

	push	si			; save move structure
	push    di			; save handle

	push	cs			; fake far call
	call	BlockMove

	pop	di
	pop	si

;	now move the 512 bytes back out into our buffer and compare

	push	ss
	pop	es
	mov	es:[si].SourceHandle,di	; move from the fake block
	mov	es:[si].DestHandle,0	; to conventional memory
	mov	es:word ptr [si].DestOffset,offset ZDS_Buffer
	mov	es:word ptr [si].DestOffset+2,cs
	mov	es:word ptr [si].SourceOffset,0
	mov	es:word ptr [si].SourceOffset+2,0

	push	di			; save handle for deallocation

	push	cs			; fake far call
	call	BlockMove


;	now all we've gotta do is see if the ZDS_Buffer matches the
;	  first 512 bytes of the ROM at 0f000:8000

	mov	es,f000
	mov	di,8000h
	mov	si,offset ZDS_Buffer
	mov	cx,256			; 256 words
	repz	cmpsw			; does it match?
	jnz	check_zb1		; done if not

;	Use AddMem to add the memory to our pool, even though we
;	  could have just marked the fake block we allocated as UNUSED.
;	  Why?  What if some other source (like and EISA BIOS) also
;	  told us about that same memory?  We might as well take advantage
;	  of all of the smarts in AddMem to eliminate overlaps.

	mov	cx,3e80h		; add memory to pool then
	mov	ax,256
	xor	dx, dx
	mov	bx, dx

	mov	di, pAddMem
	push	cs
	call	call_hi_in_di

;	Now we've got to fill the damn block, cuz the
;	  parity isn't initialized yet.

	push	ds
	pop	es
	mov	di,offset ZDS_Buffer
	mov	cx,256
	xor	ax,ax
	rep	stosw			; fill 512 byte block with zeros

	push	ss			; point es: to stack frame again
	pop	es
	mov	es:[si].SourceHandle,0	; move from conventional memory
	pop	di
	push	di			; get handle from stack
	mov	es:[si].DestHandle,di	; to our fake block at 0fa0000h
	mov	es:word ptr [si].DestOffset,0 ; move data to base of block
	mov	es:word ptr [si].DestOffset+2,0

	mov	es:word ptr [si].SourceOffset,offset ZDS_Buffer
	mov	es:word ptr [si].SourceOffset+2,cs	; move from ZDS_Buffer
	mov	es:word ptr [si].bCount,512	; move 512 bytes
	mov	es:word ptr [si].bCount+2,0
	mov	cx,512			; 256K/512 bytes
zfill_loop:
	push	es
	push	si
	push	cx

	push	cs			; fake far call
	call	BlockMove

	pop	cx
	pop	si
	pop	es
	add	es:word ptr [si].DestOffset,512
	jnc	zfill_skip
	inc	es:word ptr [si].DestOffset+2
zfill_skip:
	loop	zfill_loop

check_zb1:

;	Release the move structure stack frame

	pop	di			; restore temporary handle
	add	sp,0fffeh and (1+SIZE MoveExtendedStruc)
	push	es
	mov	es,hiseg
	assume	es:funky
	mov	[di].Flags,UNUSEDFLAG	; then handle is free again
	pop	es
	assume	es:nothing

	mov	ss,save_ss
	mov	sp,save_sp		; restore normal stack

	ret

CheckZBim	endp

;	Call MoveIt function in other segment.  Make sure we enable A20.

BlockMove	proc	near

ifdef debug_tsr
        jmp     MoveIt
else
	mov	di,MoveIt		; get function in funky segment
	jmp	call_hi_in_di
endif

BlockMove	endp

;*----------------------------------------------------------------------*
;*									*
;*  GetInt15Memory -							*
;*									*
;*	Returns the amount of memory INT 15h, Function 88h says is free	*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = Amount of free extended memory in K-bytes		*
;*  REGS:   AX and Flags are clobbered					*
;*									*
;*----------------------------------------------------------------------*


GetInt15Memory proc near

	mov	ah,88h			; snag the int 15h memory
	clc
	int	15h			; Is Function 88h around?
	jnc     xret_geti15
	xor	ax,ax			; No, return 0
xret_geti15:

ifdef WIN30COMPATIBLE
	cmp	ax,15*1024		; Limit himem.sys to using 15 meg
	jb	@f			;   of extended memory for apps
	mov	ax,15*1024		;   that don't deal with > 24 bit
@@:					;   addresses
endif
	ret

GetInt15Memory endp

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  GetBIGMemory -                                                          *
;*                                                                          *
;*      Returns the amount of memory Extended INT 15h, >16 Meg              *
;*      Uses new INT 15 function E801                                       *
;*                                                                          *
;*  ARGS:   None                                                            *
;*  RETS:   None							    *
;*  REGS:   AX and Flags are clobbered                                      *
;*                                                                          *
;*--------------------------------------------------------------------------*

GetBIGMemory proc near

        push    dx
        push    cx
        push    bx
        push    si
        push    es

        call    IsEISAMachine       ; Is this an EISA machine ?
	jc	short GBM_end	    ;  Y: Ignore parameter

	call	Compaq386	    ; is it a Compaq system?
	jnz	short GBM_end

        mov     ax,0E801h           ; determine >16M memory
        clc
	int	15h		    ; Is Function 88h around?
	jc	short GBM_end	    ; No, exit

        mov     ax,bx               ; DX:AX to = length of block in 1K blocks
        mov     dx,64               ; DX should always be zero (16M extra max)
        mul     dx

        mov     cx,BIGISA_START     ; BIGISA_START=16384 in (HIMEM.INC)
	xor	dx, dx
	mov	bx, dx

	mov	di, pAddMem
	push	cs
	call	call_hi_in_di	    ; add that memory to our tables

        les     si,dword ptr pInt15Vector   ; Save the current INT 15 vector.

	cli
        mov     ax,offset ISA15Handler      ; Exchange the old vector with
        xchg    ax,es:[si][0]               ;  the new one.
        mov     word ptr [PrevISAInt15][0],ax
        mov     ax,cs
        xchg    ax,es:[si][2]
	mov	word ptr [PrevISAInt15][2],ax
	sti

GBM_end:
        pop     es
        pop     si
        pop     bx
        pop     cx
        pop     dx
        ret

GetBIGMemory endp

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  IsEISAMachine                                           HARDWARE DEP.   *
;*                                                                          *
;*  Determine if we are on an EISA machine                                  *
;*                                                                          *
;*  ARGS:   None                                                            *
;*  RETS:   CY = 1 if we're on a EISA machine                               *
;*             = 0 otherwise                                                *
;*  REGS:   Flags clobbered                                                 *
;*                                                                          *
;*--------------------------------------------------------------------------*

pEISASTRUC  equ     0FFD9h      ; offset to EISA name in ROM
pROM	    equ     0F000h	; ROM segment

IsEISAMachine     proc    near                                            ;

        push    es                      ; save registers we'll use
        push    bx                      ;  ...

    ;-----------------------------------------
    ; Determine if this is an EISA system by
    ;  looking for ROM signature
    ;-----------------------------------------
        mov     bx,pROM
        mov     es,bx                   ; ROM segment
        mov     bx,pEISASTRUC           ; offset to EISA name
        cmp     word ptr es:[bx],'IE'   ; Q: is this an 'EISA' system
        jne     short IEM_NO            ;  N: set flag and return
        cmp     word ptr es:[bx+2],'AS' ;
        je      short IEM_YES           ;  Y: set flag and return

IEM_NO:
        clc                             ; Not an EISA machine...
        jmp     short IEM_Done

IEM_YES:
        stc                             ; Yes, it is an EISA box
IEM_Done:
        pop     bx
        pop     es

        ret

IsEISAMachine        endp

;*----------------------------------------------------------------------*
;*									*
;*  GetParms -								*
;*									*
;*	Get any parameters off of the HIMEM command line		*
;*									*
;*  ARGS:   None							*
;*  RETS:   None							*
;*  REGS:   AX, BX, CX, DX, DI, SI, ES and Flags clobbered		*
;*									*
;*  Side Effects:   cHandles and MinHMASize may be changed		*
;*									*
;*----------------------------------------------------------------------*

GPArgPtr	dd	?
GPRegSave	dw	?

	public	GetParms

GetParms    proc    near

	cld				; better safe than sorry

	push	ds

ifdef	debug_tsr			;-------------------------------
	lds	si,lpCmdLine
else					;-------------------------------
	les	di,[pReqHdr]		; Running as a device driver
	lds	si,es:[di].pCmdLine	; DS:SI points to first char
					;   after "DEVICE="
@@:	call	GPGetChar		; Skip over driver name, up to
	jc	GPDatsAll		;   first blank or / or eol
	jz	GPNextArg
	cmp	al,'/'
	jnz	@b
	dec	si			; Backup to get / again
endif					;-------------------------------

	assume	ds:nothing,es:nothing

;	Scan until we see a non-blank or the end of line.

GPNextArg:
	call	GPGetChar
	jc	GPDatsAll		; eol
	jz	GPNextArg		; blank

	mov	word ptr cs:[GPArgPtr], si	; save ptr to start of arg
	mov	word ptr cs:[GPArgPtr+2], ds	;   incase we want to complain
	dec	word ptr cs:[GPArgPtr]		;   (GPGetChar points at next)

	cmp	al,'/'			; better be a / or not a valid arg
	jz	GPGotOne

;	Detected invalid parameter or value, complain to user

GPBadParm:
	mov	ah,9			; tell'm something isn't right
	push	cs
	pop	ds
	mov	dx, offset BadArgMsg
	call	DispErrMsg

	lds	si,cs:[GPArgPtr]	; backup to last parameter

GPBadDisp:
	call	GPGetChar		; disp arg up to space or eol
	jc	GPDatsAll		;  skips over bad arg while we're at it
	jz	GPNextArg

	cmp	al,'/'				; start of next arg?
	jnz	@f
	dec	si				; maybe yes, maybe no--might
	cmp	si,word ptr cs:[GPArgPtr]	;   be same arg
	jnz	GPNextArg			;   next, go process new arg
	inc	si				;   same, keep displaying
@@:
	mov	dl,al
	call	DispInfoChar
	jmp	short GPBadDisp

;	Finished, we're outta here...

GPDatsAll:
	jmp	GPExit

;	Save what we found and try to process as parameter without argument

GPGotOne:
	lodsb
	mov	cs:[GPRegSave],ax

	mov	bx, offset ParmTbl1	; Process parameters with no arguments
	call	GPCheckParm
	jc	GPNeedParm		; CY means it wasn't processed

GPSkip2Next:				; Parameter was processed, skip
	call	GPGetChar		;   over any remaining text in the
	jc	GPDatsAll		;   parameter name
	jz	GPNextArg
	cmp	al,'/'
	jnz	GPSkip2Next
	dec	si			; backup to get / again
	jmp	GPNextArg

;	Must need an argument -- scan for a number, EOL, or a space.

GPNeedParm:
	call	GPGetChar
	jc	GPBadParm
	jz	GPBadParm	; blank
	cmp	al,':'		; start of string arg
	je	GPString
	cmp	al,'='
	jne	GPNeedParm

;	Read the number at DS:SI into DX

GPNeedNum:
	call	GPGetChar
	jc	GPDatsAll
	cmp	al,'0'
	jb	GPNeedNum
	cmp	al,'9'
	ja	GPNeedNum

	xor	dx,dx
GPNumLoop:
	sub	al,'0'
	cbw
	add	dx,ax
	call	GPGetChar
	jc	GPNumDone
	jz	GPNumDone
	cmp	al,'0'
	jb	GPBadParm
	cmp	al,'9'
	ja	GPBadParm
	shl	dx,1		; Stupid multiply DX by 10
	mov	bx,dx
	shl	dx,1
	shl	dx,1
	add	dx,bx
	jmp	short GPNumLoop

;	Move the string arg from ds:si to StringParm

GPString:
	mov	cx,(SIZE StringParm) - 1
	push	cs
	pop	es
	mov	di,offset _text:StringParm

GPStrLoop:
	call	GPGetChar
	jc	GPStrDone
	jz	GPStrDone
	stosb
	loop	GPStrLoop

GPStrDone:

	mov	byte ptr es:[di],0	; Null terminate the string
	mov	dx,-1			; In case parm expects a num, give'm
					;   a likely invalid one

;	Which parameter are we dealing with here?

GPNumDone:
	xchg	ax,cs:[GPRegSave]

	mov	bx, offset ParmTbl2	; parameters with arguments
	call	GPCheckParm		; process it valid
	jnc	GPNextParm
	jmp	GPBadParm

GPNextParm:
	mov	ax,cs:[GPRegSave]	; are we at the end of the line?
	cmp	al,13			; may not be needed any longer...
	je	GPExit
	cmp	al,10
	je	GPExit
	jmp	GPNextArg

GPExit:
	call	GPPrintInfo		; Display user messages
	pop	ds
	ret

GetParms    endp


;*----------------------------------------------------------------------*

; In:  AL =  1st character of parameter
;      BX -> Parameter table to use
; Out: CY set if parameter not matched
;      CY clear if parameter matched, and routine will be called

GPCheckParm	proc	near

GPCP_loop:
	cmp	cs:[bx].cParmChar, 0	; end of table?
	je	GPCP_failed

	cmp	cs:[bx].cParmChar, al	; match?
	je	GPCP_match

	add	bx, SIZE ParmEntry
	jmp	short GPCP_loop

GPCP_match:
	clc				; got it, call routine to process
	call	cs:[bx].pParmRtn	; (handler returns with CY clr or set)
	ret

GPCP_failed:
	stc				; no match
	ret

GPCheckParm	endp


;*----------------------------------------------------------------------*

; Routines to process specific command line parameters

GPDoParameters	proc	near


;---------------------------------------

GPGotEISA:				; /EISA
	mov	fEISA, 0FFh
	ret				; (CY already clear)

GPGotVerbose:				; /VERBOSE
	mov	fQuiet, 0
	ret				; (CY already clear)

GPGotBigMem:				; /NOABOVE16
	cmp	byte ptr [si], 'O'	; /NOABOVE16 & /NUMHANDLES= both start
	jne	GPBadParmRet		;   with 'N', make sure only /NOABOVE16
					;   is processed here.
	mov	fBigMem, 0
	ret				; (CY already clear)

;	The /Z switch is for compatibility with pervious version of himem.
;	By default, himem will now call it's own XMMControl entry point to
;	enable/disable A20 around calls to himem code in the HMA.  The
;	external calls allow XMS hookers (like EMM386) to see the A20
;	changes.  The previous behavior of calling internal A20 routines
;	can be selected with the /Z switch in case the external (recursive)
;	calls cause compatibility problems with other software.

GPZSwitch:				; /Z switch

	mov	cs:[pfnEnabA20],  offset _text:LocalEnableA20
	mov	cs:[pfnDisabA20], offset _text:LocalDisableA20

	ret				; (CY already clear)

;---------------------------------------
;	Process /A20CONTROL: parameter

GPGotA20Control:
	mov	ax,word ptr [StringParm]
	or	ax,2020h
	mov	bl,0FFh
	cmp	ax,'no' 		; ON ?	- means we take control
	jz	GPSetA20
	inc	bl
	cmp	ax,'fo' 		; OFF ? - means we leave alone if on
	jz	GPSetA20

GPBadParmRet:				; common failure exit
	stc
	ret

GPSetA20:
	mov	fA20Control,bl		; Z if A20 should be left alone if
					;   it's already on when we're loaded

GPGoodParmRet:				; common success exit
	clc
	ret

;---------------------------------------
;	Process /MACHINE: parameter.

GPGotMachine:
	push	si				; save current location
	push	ds				;    in param string

	push	cs
	pop	ds
	mov	di,offset _text:MachineName	; es:di -> MachineName

GPNextTbl:
	xor	bx,bx

GPNextName:
	mov	si,offset _text:StringParm	; ds:si -> StringParm

GPChkNext:
	cmp	byte ptr es:[di],0FFh		; end of name table?
	jz	GPNoName

	lodsb				; char from StringParm
	cmp	al,'A'			; force to lower case for match
	jb	@f			; (might be numeric, so don't just OR)
	cmp	al,'Z'
	ja	@f
	or	al,20h
@@:
	cmp	al,es:[di]		; match so far?
	jnz	GPFlushName

	or	al,al			; finished if matched up to & incl NULL
	jz	GPFoundName

	inc	di			; still matches, check next char
	jmp	short GPChkNext

GPFlushName:
	inc	bx
GPFN2:
	inc	di
	cmp	byte ptr es:[di],0FFh
	jz	GPNoName

	cmp	byte ptr es:[di],0
	jnz	GPFN2
	inc	di
	jmp	short GPNextName

GPFoundName:
	mov	cs:[MachineNum],bx	; found a match, remember which entry
	jmp	short GPNameDone	;   it is for later

GPNoName:

	cmp	di,offset _text:AltNameTbl
	ja	GPBadName
	mov	di,offset _text:AltNameTbl
	jmp	short GPNextTbl

GPNameDone:
	pop	ds			; recover parm line pointer
	pop	si
	jmp	GPGoodParmRet

GPBadName:
	pop	ds			; clear stack and error out...
	pop	si
	jmp	GPBadParmRet

;---------------------------------------
;	Process /NUMHANDLES= parameter.

GPGotHands:
	cmp	dx,MAXHANDLES
	jna	@f
	jmp	GPBadParmRet
@@:
	or	dx,dx		; Zero?
	jnz	@f
	jmp	GPBadParmRet
@@:
	push	es
	mov	es,hiseg
	assume	es:funky
	mov     [cHandles],dx ; Store it
	pop	es
	assume	es:nothing

	mov	fNumHandSet, 0FFh

	jmp	GPGoodParmRet

;---------------------------------------
;	 Process /HMAMIN= parameter

GPGotMin:
	cmp	dx,64
	jna	@f
	jmp	GPBadParmRet
@@:
	mov	cl,10		; Convert from K to bytes
	shl	dx,cl
	mov	cs:[MinHMASize],dx
	mov	fHMAminSet, 0FFh
	jmp	GPGoodParmRet


;---------------------------------------
;	Process /SHADOWRAM: parameter

GPGotShadow:
	mov	ax,word ptr [StringParm]
	or	ax,2020h
	xor	bl,bl
	cmp	ax,'no' 		; ON ?	- means we leave it alone
	jz	GPSetShadow
	inc	bl
	cmp	ax,'fo' 		; OFF ? - means we turn it off
	jz	GPSetShadow
	jmp	GPBadParmRet

GPSetShadow:
	mov	fShadowOff,bl		; NZ if Shadow RAM should be turned off
	jmp	GPGoodParmRet


;---------------------------------------
;	Process /CPUCLOCK: parameter

GPGotCPUClock:

	mov	ax,word ptr [StringParm]
	or	ax,2020h
	xor	bl,bl
	cmp	ax,'fo' 		; OFF ? - means we don't worry about it
	jz	GPSetClock
	inc	bl
	cmp	ax,'no' 		; ON ?	- means we preserve CPU clock
	jz	GPSetClock		;	  rate
	jmp	GPBadParmRet

GPSetClock:
	mov	fCPUClock,bl		; NZ if clock rate preserved
	jmp	GPGoodParmRet


;---------------------------------------
;	Process /INT15= parameter

GPGotInt15:
	cmp	dx, 64			; atleast 64K
	jae	@f
	jmp	GPBadParmRet
@@:	call	GetInt15Memory
	cmp	ax, dx			; enuf Ext Mem ?
	jae	@f
	jmp	GPBadParmRet
@@:	mov	[Int15MemSize], dx
	jmp	GPGoodParmRet


GPDoParameters	endp


;*----------------------------------------------------------------------*

; Get the next character from DS:SI, set CY if it's an EOL (CR, LF), set
; Z if it's a space

GPOffEOL	dw	-1

	public	GPGetChar

GPGetChar	proc	near

	cmp	si,cs:[GPOffEOL]	; are we already at EOL?
	jnb	GPAtEOL

	lodsb				; no, get next char
	cmp	al,10			; is this the EOL?
	je	GPHitEOL
	cmp	al,13
	je	GPHitEOL

	cmp	al,' '			; set Z if blank

	clc
	ret

GPHitEOL:
	mov	cs:[GPOffEOL],si	; save EOL offset once
GPAtEOL:
	stc
	ret

GPGetChar	endp


;*----------------------------------------------------------------------*

;  Print user msgs now instead of while processing so they can be
;  suppressed if /V (verbose) doesn't appear on command line.

GPPrintInfo proc   near

	; /NUMHANDLES=

	cmp	fNumHandSet, 0
	je	GPPI_HMAMin

	mov	dx,offset StartMsg ; display descriptive message
	call    GPPrintIt

	push	es
	mov	es,hiseg
	mov	ax,es:[cHandles]
	pop	es

	call    GPPrintAX
	mov	dx,offset HandlesMsg
	call	GPPrintIt

GPPI_HMAMin:

	; /HMAMIN=

	cmp	fHMAminSet, 0
	je	GPPI_exit

	mov	dx,offset HMAMINMsg ; print a descriptive message
	call    GPPrintIt
	mov	ax,cs:[MinHMASize]
	mov	cl, 10			; convert from bytes to k
	shr	ax, cl
	call    GPPrintAX
	mov	dx,offset KMsg
	call    GPPrintIt

GPPI_exit:
	ret

GPPrintInfo endp

;*----------------------------------------------------------------------*

GPPrintIt   proc    near

	push    ds		; Save current DS
	push    cs		; Set DS=CS
	pop	ds
	call	DispInfoMsg
	pop	ds		; Restore DS
	ret

GPPrintIt   endp

;*----------------------------------------------------------------------*

GPPrintAX   proc    near

	mov	cx,10
	xor	dx,dx
	div	cx
	or	ax,ax
	jz	GPAPrint
	push    dx
	call    GPPrintAX
	pop	dx
GPAPrint:
	add	dl,'0'
	call	DispInfoChar
	ret

GPPrintAX   endp

;*----------------------------------------------------------------------*
;*									*
;*  InitHandles -							*
;*									*
;*	Initialize the Extended Memory Handle Table			*
;*									*
;*  ARGS:   None							*
;*  RETS:   None							*
;*  REGS:   AX, BX, CX, and Flags are clobbered				*
;*									*
;*----------------------------------------------------------------------*

	assume	ds:_text

	public	InitHandles

InitHandles proc    near
	push	es
	mov	es,hiseg
	assume	es:funky
	mov	cx,[cHandles]

;	Init the Handle table.

	mov	bx,[KiddValley]

	xor	ax,ax
IHTabLoop:
	mov	[bx].Flags,UNUSEDFLAG
	mov	[bx].cLock,al
	mov	[bx].Base.lo,ax
	mov	[bx].Base.hi,ax
	mov	[bx].Len.lo,ax
	mov	[bx].Len.hi,ax
	if	keep_cs
	mov	[bx].Acs,ax
	endif
	add	bx,SIZE Handle
	loop    IHTabLoop

	mov	[KiddValleyTop],bx	; save top for handle validation
	pop	es
	assume	es:nothing
	ret

InitHandles endp


;*----------------------------------------------------------------------*
;*									*
;*  ScanEISA - poll any EISA devices through the BIOS's Int15(0d8h)	*
;*     and add any memory we find out about to our free memory table.	*
;*     Note:  this code (including a big buffer) gets thrown out after	*
;*     completion of the initialization sequence.			*
;*									*
;*	Note:  The COMPAQ BIOS uses up 1.5K of stack during int15(d80x) *
;*		so we'll set up a separate stack while we're here	*
;*									*
;*----------------------------------------------------------------------*

save_ss	dw	0
save_sp	dw	0

	public	ScanEISA

ScanEISA	proc	near
	assume	ds:_text

	mov	save_ss,ss
	mov	save_sp,sp
	push	cs
	pop	ss
	mov	sp,offset EISA_stack

	xor	cl,cl			; start with slot zero

SEISA_01:
	push    cx			; save slot number
	mov	ax,0d800h		; get summary of configuration
	int	15h
	pop	cx			; restore slot number
	jc	SEISA_09		;  skip if any kind of error
	test    dl,2			; does that slot have any memory?
	jz	SEISA_09		;  skip if not
	cmp	dh,0			; zero functions would be an
	jz	SEISA_09		;  error condition
	mov	ch,dh			; copy function number to ch

;	Now we've found a valid slot with some memory.  Let's find out
;	  what kind of memory it is, where its located, and how much there is.

SEISA_02:
	push    cx			; save slot and function
	mov	ax,0d801h		; read function information
	lea	si,EISABuffer		; pass pointer to our buffer
	int	15h
	jc	SEISA_04		; brif any error

;	Now look into buffer for memory information

	test    byte ptr EISA_FncInfo,80h ; function disabled?
	jnz	SEISA_04		;  brif so
	test    byte ptr EISA_FncInfo,2	; memory information follows?
	jz	SEISA_04		;  done if not

	lea	si,EISA_MemConfig	; point to memory information
SEISA_03:

; M007
	mov	al, ds:byte ptr [si]
	and	al, 079h
	cmp	al, 1			; Make sure that Reserved bit = 0
					;		 Shared bit   = 0
					;		 memory type = SYSTEM
					;		 Read/Write  = TRUE
	jne	SEISA_03a		; (See EISA spec for bit definitions)
; M007

	mov	cx,ds:word ptr 2[si]    ; get base address in 256 bytes
	mov	bl,ds:byte ptr 4[si]    ;  get highest byte
	xor	bh, bh
	mov	al,cl			; save lowest bits for later
	shr	bl,1			; convert to our internal 1k format
	rcr	cx,1
	shr	bl,1
	rcr	cx,1
	test    al,3			; was base not on a 1k boundary?
	mov	ax,ds:word ptr 5[si]    ; get length in k
	jz	SEISA_03b		;  brif base was on 1k boundary
	dec	ax			; sacrifice the partial k
	add	cx,1			; and bump base to first complete k
	adc	bx,0
SEISA_03b:
	xor	dx, dx
	or	ax, ax
	jnz	@f
	inc	dx
@@:
	push    si
	cmp	cx,1024			; is it below a meg?
	jb	seisa_0xx		; ignore it if so
ifdef WIN30COMPATIBLE				;M005
	or	bx, bx
	jnz	seisa_0xx
	cmp	cx,1024*16		; is it above 16 meg?
	jae	seisa_0xx		; ignore it if so
endif                                           ;M005
	mov	di, pAddMem
	push	cs
	call	call_hi_in_di		; add that memory to our tables
seisa_0xx:
	pop	si
SEISA_03a:
	add	si,7			; next entry
	test    ds:byte ptr -7[si],80h  ;  was that the last one?
	jnz	SEISA_03		;  loop if not
SEISA_04:
	pop	cx			; restore slot and function
	dec	ch			; next lower function
	jnl	SEISA_02		; valid functions are 0..n

SEISA_09:
	inc	cl			; next slot
	cmp	cl,16
	jae	@f
	jmp	SEISA_01
@@:

; Now check for EISA memory and Int 15h/88h overlap.  In the default case
; we'll ignore any EISA blocks starting at 1Meg under the assumption that
; this memory will be claimed later via the Int 15h hook.  If the Int 15h/88h
; call indicates less memory than the EISA scan, it may be that an Int 15h
; allocator has been loaded before us, and if we just grabbed all EISA
; memory starting at 1Meg, we would stomp all over them.  On the other hand,
; a number of BIOS' never return > 15 (16?) meg of extended memory via Int 15h
; (even if there is much more memory installed), so we provide the /EISA
; command line parameter to override this default and allow himem to steal
; all EISA memory.  If the /EISA switch is used, there better not be any
; Int 15h allocators loaded before himem!

;	Locate a free memory block in the handle table at 1 Meg (1024k)

	push	es
	mov	es,hiseg
	assume	es:funky
	mov	bx,[KiddValley]
	mov     cx,[cHandles]		; Loop through the handle table
SEISA_09a:
	cmp	[bx].Flags,FREEFLAG	; is this a valid free memory block?
	jnz	SEISA_09b		; branch if not
	cmp	[bx].Base.hi, 0
	jnz	SEISA_09b
	cmp	[bx].Base.lo,1024	; based at 1Meg?
	jz	SEISA_09c		; yup...

SEISA_09b:
	add	bx,SIZE Handle
	loop    SEISA_09a
	jmp	short SEISA_09e

SEISA_09c:
	cmp	fEISA, 0		; want all EISA memory, regardless?
	jz	SEISA_default		; no...

ifdef WIN30COMPATIBLE		;GetInt15Memory has an ifdef WIN30COMPATIBLE
.err				;  that would be silly to use with the
endif				;  following code

	call	GetInt15Memory		; yes, start EISA block above current
	add	[bx].Base.lo, ax	;   Int 15h/88h line.  The Int 15h mem
	adc	[bx].Base.hi, 0 	;   will be added when Int 15h hooked.
	sub	[bx].Len.lo, ax 	;   Existing hook and /INT15= code
	sbb	[bx].Len.hi, 0		;   will work as before.
	jc	SEISA_09d		; (just for safeties sake)
	mov	ax, [bx].Len.lo 	; Len will be 0 if Int 15h covers
	or	ax, [bx].Len.hi 	;   entire EISA block
	jz	SEISA_09d
	jmp	short SEISA_09e

SEISA_default:
	cmp	[bx].Len.hi, 0		; If the length is > 64 Meg (which is
	jz	SEISA_09d		;   the max that Int 15h/88h could
	inc	[bx].Base.hi		;   return), keep the memory above
	dec	[bx].Len.hi		;   that for XMS
	jmp	short SEISA_09e

SEISA_09d:
	mov	[bx].Flags,UNUSEDFLAG	; free the block at 1024

SEISA_09e:
	pop	es
	assume	es:nothing

SEISA_exit:
	mov	ss,save_ss
	mov	sp,save_sp		; restore normal stack
	ret
	assume	ds:nothing

ScanEISA	endp

;	The buffer for scanning EISA devices is big, but disposable

ZDS_Buffer	label	byte		; also use for Zenith memory check

EISABuffer	db	22h dup (0)
EISA_FncInfo	db	(73h-22h) dup (0)
EISA_MemConfig	db	(320-73h) dup (0)

		db	(512-320) dup (0)	; make it 512 for Zenith

	db	2000 dup (?)
EISA_Stack:

_text	ends

ifdef	debug_tsr			;-------------------------------

EndStmt equ	<end	ExeStart>

STACK	segment	stack 'STACK'
	db	1024 dup (?)
STACK	ends

else

EndStmt equ	<end>

endif					;-------------------------------

	EndStmt
