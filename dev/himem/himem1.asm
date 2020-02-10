;/*
; *                      Microsoft Confidential
; *			 Copyright (C) Microsoft Corporation 1988-1992
; *                      All Rights Reserved.
; */
	page	95,160
	title	himem1 - A20 Handler stuff

	.xlist
	include	himem.inc
	.list

	public	A20Handler
	extrn	ATA20Delay:byte
	extrn	InstldA20HndlrN:byte
	extrn	fQuiet:byte
	extrn	fCPUClock:byte
	extrn	fA20Check:byte
	extrn	pPPFIRET:word
	extrn	OldStackSeg:word
	extrn	f000:word
	extrn	MemCorr:word
	extrn	MachineNum:word
	extrn	TopOfTextSeg:word
	extrn	lpExtA20Handler:dword

	extrn	IsA20On:near
	extrn	DispInfoMsg:near
	extrn	DispInfoChar:near

	extrn	InsExtA20Msg:byte
	extrn	InsA20Msg:byte, InsA20EndMsg:byte, NoA20HandlerMsg:byte

	extrn	AltName1:byte, AltName2:byte, AltName3:byte, AltName4:byte
	extrn	AltName5:byte, AltName6:byte, AltName7:byte, AltName8:byte
	extrn	AltName9:byte, AltName10:byte,AltName11:byte,AltName12:byte
	extrn	AltName13:byte, AltName14:byte, AltName15:byte
	extrn	AltName16:byte, ALtName17:byte

; Define a direct call to the Phoenix Cascade BIOS for A20 handling

PTL_Seg	segment at 0f000h
	assume	cs:PTL_Seg
	org	0ff82h
PTL_A20_Bios_Entry	proc	far
PTL_A20_Bios_Entry	endp
PTL_Seg	ends

	assume	cs:_text,ds:nothing

;*--------------------------------------------------------------------------*
;*									    *
;* A20 Handler Section: 						    *
;*									    *
;* The Init code copies the proper A20 Handler in place.		    *
;*									    *
;* NOTE: the A20 handler may be called from the Int 15h hook which does     *
;*	 not set ds = _text.  DO NOT ASSUME DS == _TEXT!		    *
;*									    *
;*--------------------------------------------------------------------------*

A20Handler:

;*** A20 Handler is installed HERE! ***

TempA20Handler	dw	?	; address of A20 hander during installation

;****************************************************************************
;*									    *
;* A20 Handler Section: 						    *
;*									    *
;* The Init code copies the proper A20 Handler into place here.		    *
;*									    *
;* NOTE: A20 HANDLERS MUST ONLY HAVE RELATIVE JUMPS!  HOWEVER ANY CALLS TO  *
;*	 FUNCTIONS OUTSIDE OF THE HANDLER MUST BE NON-RELATIVE!		    *
;*									    *
;****************************************************************************

;*----------------------------------------------------------------------*
;*									*
;*  AT_A20Handler -					HARDWARE DEP.   *
;*									*
;*	Enable/Disable the A20 line on non-PS/2 machines		*
;*									*
;*  ARGS:   AX = 0 for Disable, 1 for Enable				*
;*  RETS:   AX = 1 for success, 0 otherwise				*
;*  REGS:   AX, CX and Flags clobbered					*
;*									*
;*----------------------------------------------------------------------*

NUM_ALT_A20	equ	2	; # Alternative delay methods supported

AT_A20Handler proc	near

; This entry point attempts to preserve the current CPU clock rate by
; maintaining the current value of the "unused" bits in the keyboard
; controller output port.

AT_A20CPUClock	label	near

	push	ax		; Save enable/disable parameter

	in	al,64h		; Ensure output buffer is empty
	test	al,1
	jz	AAHOutBufFree
	jmp	short $+2
	in	al,60h
AAHOutBufFree:

	call	Sync8042	; Make sure the Keyboard Controller is Ready
	jnz	AAHErr_pop

	mov	al,0D0h 	; Send D0h command (read output port)
	out	64h,al
	call	Sync8042
	jnz	AAHErr_pop

	call	AAHFullOutBuf	; Wait for data to appear
	jz	AAHErr_pop

	in	al,60h		; Read output port value

	mov	ah,0Ch		; Only want to keep the "unused" bits
	and	ah,al
	or	ah,0D1h 	; Assume disabling

	pop	cx		; Recover enable/disable parameter
	jcxz	AAHSetA20_1
	or	ah,02h		; Assumed wrong, actually enabling
	jmp	short AAHSetA20_1


; Routine to wait for 8042 output buffer to contain data.  Only needed by the
; CPU clock code, so it's stuck here before the 'normal' AT handler.

AAHFullOutBuf:
	mov	cx,12
AAH_FOB_1:
	push	cx
	xor	cx,cx
AAH_FOB_2:
	jmp	short $+2
	in	al,64h
	test	al,1
	loopz	AAH_FOB_2
	pop	cx
	loopz	AAH_FOB_1
	ret

AAHErr_pop:
	pop	ax		; Clear stack
	jmp	short AAHErr


; This is the entry point for the "normal" AT style A20 handler that does
; not worry about changing the CPU clock rate.

AT_A20Normal	label	near

	or	ax,ax
	mov	ah,0DFh 	; Assume enabling
	jnz	AAHSetA20
	mov	ah,0DDh 	; Actually disabling...

AAHSetA20:
	call	Sync8042	; Make sure the Keyboard Controller is Ready
	jnz     AAHErr

AAHSetA20_1:
	mov	al,0D1h 	; Send D1h (write output port)
	out	64h,al
	call    Sync8042
	jnz     AAHErr

	mov	al,ah		; Send enable/disable command
	out	60h,al
	call    Sync8042
	jnz	AAHErr

; Delay while waiting for the A20 line to settle--there are several methods
; to do this--the variable ATA20Delay indicates which one to use...
;
;	0 = output null command to 8042
;	1 = no delay at all
;	2 = delay on ram refresh bit toggle

	.errnz	NUM_ALT_A20 - 2 ; code below assumes max value of 2

	xor	cx,cx
	mov	cl,ATA20Delay
	jcxz	AAHDelay0
	dec	cx
	jcxz	AAHDelay1
	jmp	short AAHDelay2

; A20 Delay method 0 -- method used by original IBM PC/AT

AAHDelay0:
	mov	al,0FFh		; Send FFh (Pulse Output Port NULL)
	out	64h,al
	call    Sync8042
	jnz	AAHErr
	jmp	short AAHExit

; A20 Delay method 2

AAHDelay2:
	mov	cx,2		; Implement a delay by waiting for the
AAHWaitRam:			;   ram refresh bit to change state twice.
	jmp	short $+2	;   A long delay used by a few machines.
	in	al,61h
	test	al,10h
	jz	AAHWaitRam
@@:	jmp	short $+2
	in	al,61h
	test	al,10h
	jnz	@b
	loop	AAHWaitRam

AAHDelay1:			; Delay method 1 is actually no delay
AAHExit:
	mov	ax,1		; Successful...
	ret

AAHErr:
	xor	ax,ax		; Failure status
	ret

AT_A20Handler endp


Sync8042    proc    near

	xor	cx,cx
S8InSync:
	jmp	short $+2
	in	al,64h
	and	al,2
	loopnz  S8InSync
	ret

Sync8042    endp

EndAT_A20Handler:


;*----------------------------------------------------------------------*
;*									*
;*  Phoenix Cascade BIOS A20 Handler -			HARDWARE DEP.   *
;*									*
;*	Enable/Disable the A20 line on Phoenix Cascade BIOS		*
;*									*
;*  ARGS:   AX = 0 for Disable, 1 for Enable, 2 for On/Off check	*
;*  RETS:   AX = 1 for success, 0 otherwise				*
;*  REGS:   AX, CX and Flags clobbered					*
;*									*
;*----------------------------------------------------------------------*


PTL_A20_DisableFunc	equ 800h	; call to disable A20
PTL_A20_EnableFunc	equ 801h	; call to enable A20
PTL_A20_InquireFunc	equ 802h	; call to enquire about A20 status

PTL_String_Offset	equ 4		; offset of "PTL" in Phoenix extension
PTL_Feature1		equ 3		; offset from PTL to Feature Byte 1
PTL_A20Bit		equ 80h 	; Bit for A20 support in Feature Byte 1

PTL_CASCADE_A20Handler proc	near

	cmp	ax,1
	jz	PTLEnable		; 1 == Enable
	jb	PTLDisable		; 0 == Disable
					; 2 == On/Off Check
	mov	ax,PTL_A20_InquireFunc	; Inquire status
	call	PTL_A20_Bios_Entry
	or	ah,ah			; on or off?
	jz	PTLErr			; returns with ax = 0 (off)
	jmp	short PTLExit		; returns with ax = 1 (on)

PTLEnable:
	mov	ax,PTL_A20_EnableFunc	; call A20 enable function
	jmp	short PTLDoIt

PTLDisable:
	mov	ax,PTL_A20_DisableFunc	; Disable Function

PTLDoIt:
	call    PTL_A20_Bios_Entry

PTLExit:
	mov	ax,1			; Good status, return 1
	ret

PTLErr:
	xor	ax,ax			; Bad status, return 0
	ret

PTL_CASCADE_A20Handler endp

EndPTL_CASCADE_A20Handler:


;*----------------------------------------------------------------------*
;*									*
;*  PS2_A20Handler -					HARDWARE DEP.   *
;*									*
;*	Enable/Disable the A20 line on PS/2 machines			*
;*									*
;*  ARGS:   AX = 0 for Disable, 1 for Enable, 2 for On/Off check	*
;*  RETS:   AX = 1 for success, 0 otherwise				*
;*  REGS:   AX, CX and Flags clobbered					*
;*									*
;*----------------------------------------------------------------------*

PS2_PORTA   equ	0092h
PS2_A20BIT  equ	00000010b

PS2_A20Handler proc   near

	cmp	ax,1
	mov	ah,PS2_A20BIT		; Assume enabling
	jz	PAHEnable
	jb	PAHDisable

	in	al,PS2_PORTA		; 2 == On/Off check
	test	al,PS2_A20BIT
	jz	PAHErr			; returns ax = 0 (off)
	jmp	short PAHExit		; returns ax = 1 (on)

PAHDisable:
	xor	ah,ah			; Disabling...

PAHEnable:

	in	al,PS2_PORTA		; Get current port 92h bits
	and	al,NOT PS2_A20BIT	;    and clear A20
	or	al,ah			; Enable or Disable A20 bit
	jmp	short $+2
	jmp	short $+2
	out	PS2_PORTA,al

	xor	cx,cx			; Wait for port 92h to show
PAHWait:				;   desired A20 state
	jmp	short $+2
	in	al,PS2_PORTA
	and	al,PS2_A20BIT
	cmp	al,ah
	loopnz	PAHWait

	jnz	PAHErr

PAHExit:
	mov	ax,1
	ret

PAHErr:
	xor	ax,ax
	ret

PS2_A20Handler endp

EndPS2_A20Handler:


;*----------------------------------------------------------------------*
;*									*
;*  IC_A20Handler -							*
;*									*
;*	Enable/Disable the A20 line on the IBM 7552 Industrial		*
;*	Computer.							*
;*									*
;*  ARGS:   AX = 0 for Disable, 1 for Enable, 2 for On/Off check	*
;*  RETS:   AX = 1 for success, 0 otherwise				*
;*  REGS:   AX, CX and Flags clobbered					*
;*									*
;*----------------------------------------------------------------------*

IC_MISCPORT	equ 65h
IC_A20BIT	equ 04h

IC_A20Handler proc near

	cmp	ax,1
	mov	ah,IC_A20BIT		; Assume disabling
	jz	ICAHEnable
	jb	ICAHDisable

	in	al,IC_MISCPORT		; 2 == On/Off check
	test	al,IC_A20BIT
	jnz	ICAHErr 		; returns ax = 0 (off)
	jmp	short ICAHExit		; returns ax = 1 (on)

ICAHEnable:
	xor	ah,ah			; Enabling...

ICAHDisable:

	in	al,IC_MISCPORT		; Get current port 92h bits
	and	al,NOT IC_A20BIT	;    and clear A20
	or	al,ah			; Enable or Disable A20 bit
	jmp	short $+2
	jmp	short $+2
	out	IC_MISCPORT,al

	xor	cx,cx			; Wait for port 92h to show
ICAHWait:				 ;   desired A20 state
	jmp	short $+2
	in	al,IC_MISCPORT
	and	al,IC_A20BIT
	cmp	al,ah
	loopnz	ICAHWait

	jnz	ICAHErr

ICAHExit:
	mov	ax,1
	ret

ICAHErr:
	xor	ax,ax
	ret

IC_A20Handler endp

EndIC_A20Handler:


;*----------------------------------------------------------------------*
;*									*
;*  $6300Plus_A20Handler -				HARDWARE DEP.   *
;*									*
;*     Enable/Disable address lines A20-A23 on AT&T 6300 Plus		*
;*									*
;*  ARGS:   AX = 0 for Disable, 1 for Enable				*
;*  RETS:   AX = 1 for success, 0 otherwise				*
;*  REGS:   AX, BX, and Flags clobbered					*
;*									*
;*  Note:   Don't want to do two back to back disables on PLUS,		*
;*	    so we check to see if it is necessary.			*
;*  Warning:  The calcuation of the Rback label depends on the		*
;*	  expectation that this routine is being moved at init time.    *
;*									*
;*----------------------------------------------------------------------*

PLUS_PORT   equ		03F20h
PLUS_STATUS equ		03FA0h
PLUS_SET    equ		80h	; Turn on A20-A23
PLUS_RESET  equ		10h	; Turn off A20-A23 and point to our routine

$6300PLUS_A20Handler proc   near
	mov	bx,ax
	push	dx
	mov	dx,PLUS_STATUS
	in	al,dx
	pop	dx
	and	ax,1
	cmp	ax,bx
	jne	$6AHEnable
	mov	ax,1
	ret			; No, just return

$6AHEnable:
	pushf
	sti
	mov	al,PLUS_SET
	or	bx,bx		; Zero if disable
	jnz	$6AHNext
	mov	al,PLUS_RESET

$6AHNext:
	push	dx		; Set/reset the port
	mov     dx,PLUS_PORT
	out     dx,al
	pop     dx
	or      bx,bx
	jnz     $6AHNext1
	call    $6300Reset	; Reset the processor
$6AHNext1:
	popff
	mov	ax,1
	ret

$6300Plus_A20Handler endp



; In order to address memory above 1 MB on the AT&T 6300 PLUS, it is
; necessary to use the special OS-MERGE hardware to activate lines
; A20 to A23.  However, these lines can be disabled only by resetting
; the processor.  The address to which to return after reset are placed
; at 40:A2, noted here as RealLoc1.

BiosSeg SEGMENT  AT 40h		  ; Used to locate 6300 PLUS reset address

		org	00A2h
RealLoc1	dd	0

BiosSeg ends

;*----------------------------------------------------------------------*
;*									*
;* $6300Reset -						HARDWARE DEP.   *
;*									*
;* Reset the 80286 in order to turn off the address lines on the 6300   *
;* PLUS.  This is the only way to do this on the current hardware.	*
;* The processor itself is reset by reading or writing port 03F00h.	*
;*									*
;*  Uses flags.								*
;*									*
;*----------------------------------------------------------------------*

$6300Reset  proc    near

	pusha				; Save world
	push    ds			; Save segments
	push    es
	mov	ax,BiosSeg		; Point to the BIOS segment
	mov	ds,ax			; ds -> 40h

;	Setup the reset return address.

	assume	ds:nothing

	push    word ptr ds:[RealLoc1]  ; Save what might have been here
	push    word ptr ds:[RealLoc1+2]

;	Load our return address, remembering that we will be relocated
;	   at init time.

	mov     word ptr ds:[RealLoc1+2],cs
 mov ds:word ptr [RealLoc1],offset Rback-offset $6300Plus_A20Handler+offset A20Handler
	mov     cs:[OldStackSeg],ss     ; Save the stack segment, too

;	Reset the processor - turning off A20 in the process.

	mov	dx,03F00h
	in	ax,dx

;	We shouldn't get here.  Halt the machine if we do.

	nop
	nop
	nop
	nop
	cli
	hlt

Rback:
	mov	ss,cs:[OldStackSeg]		; Start the recovery
	pop	word ptr ds:[RealLoc1+2]	; ROM code has set ds->40h
	pop	word ptr ds:[RealLoc1]
	pop	es
	pop	ds

	xor     al,al
	mov     dx,PLUS_PORT
	out     dx,al
	popa
	ret

$6300Reset endp

End6300Plus_Handler:

;*----------------------------------------------------------------------*
;*									*
;*  HP_A20Handler -					HARDWARE DEP.   *
;*									*
;*	Enable/Disable the A20 line on HP Vectra machines		*
;*									*
;*  ARGS:   AX = 0 for Disable, 1 for Enable				*
;*  RETS:   AX = 1 for success, 0 otherwise				*
;*  REGS:   AX, CX and Flags clobbered					*
;*									*
;*----------------------------------------------------------------------*

HP_A20Handler proc   near

	or	ax,ax
	mov	ah,0DFh 	; Assume enabling
	jnz	HAHSetA20
	mov	ah,0DDh 	; Actually disabling...

HAHSetA20:

	call    HPSync8042	; Make sure the Keyboard Controller is ready
	jnz     HAHErr

	mov	al,ah		; Send enable/disable
	out	64h,al
	call    HPSync8042
	jnz     HAHErr

	mov	al,ah		; And send it again...
	out	64h,al
	call    HPSync8042
	jnz	HAHErr

HAHExit:
	mov	ax,1
	ret

HAHErr:
	xor	ax,ax
	ret

HP_A20Handler endp


HPSync8042  proc    near

	xor	cx,cx
H8InSync:
	jmp	short $+2
	in	al,64h
	and	al,2
	loopnz  H8InSync
	ret

HPSync8042  endp

EndHP_A20Handler:

;*----------------------------------------------------------------------*
;*									*
;*  NHP_A20Handler -					HARDWARE DEP.   *
;*									*
;*	Enable/Disable the A20 line on HP Vectra machines		*
;*									*
;*  ARGS:   AX = 0 for Disable, 1 for Enable				*
;*  RETS:   AX = 1 for success, 0 otherwise				*
;*  REGS:   AX, CX and Flags clobbered					*
;*									*
;*----------------------------------------------------------------------*

NHP_A20Handler proc   near

	or	ax,ax
	mov	ah,0DFh 	; Assume enabling
	jnz	NHAHSetA20
	mov	ah,0DDh 	; Actually disabling...

NHAHSetA20:

	call    NHPSync8042	; Make sure the Keyboard Controller is ready
	jnz     NHAHErr

	mov	al,ah		; Send enable/disable
	out	64h,al

	call    NHPSync8042
	jnz     NHAHErr

	mov	al,0ffh		; And send a NULL command
	out	64h,al

	call    NHPSync8042	; and wait
	jnz     NHAHErr

NHAHExit:
	mov	ax,1
	ret

NHAHErr:
	xor	ax,ax
	ret

NHP_A20Handler endp


NHPSync8042  proc    near

	xor	cx,cx
NH8InSync:
	jmp	short $+2
	in	al,64h
	and	al,2
	loopnz  NH8InSync
	ret

NHPSync8042  endp

EndNHP_A20Handler:

;M004 start
;*--------------------------------------------------------------------------*
;*									    *
;*  BM60_A20Handler -					    HARDWARE DEP.   *
;*									    *
;*	Enable/Disable the A20 line on BULL Micral BM60 machines	    *
;*									    *
;*  ARGS:   AX = 0 for Disable, 1 for Enable				    *
;*  RETS:   AX = 1 for success, 0 otherwise				    *
;*  REGS:   AX and Flags clobbered					    *
;*									    *
;*--------------------------------------------------------------------------*

BM60_A20Handler proc   near

	or	ax,ax
	jz	short BM60Disable

BM60Enable:
	mov	al,0DFh     ; Send DFh
	out	0E0h,al
	jmp	short $+2
	in	al,64h
	and	al,2
	jnz	short BM60Exit	  ; ok
	jmp	short BM60Err

BM60Disable:
	mov	al,0DDh     ; Send D1h
	out	0E0h,al
	jmp	short $+2
	in	al,64h
	and	al,2
	jnz	short BM60Err	  ; not ok

BM60Exit:
	mov	ax,1
	ret

BM60Err:
	xor	ax,ax
	ret

BM60_A20Handler endp

EndBM60_A20Handler:
;M004 end

;*----------------------------------------------------------------------*
;*									*
;*  ACER_A20Handler -							*
;*									*
;*	Enable/Disable the A20 line on ACER 1100 machines		*
;*									*
;*  ARGS:   AX = 0 for Disable, 1 for Enable				*
;*  RETS:   AX = 1 for success, 0 otherwise				*
;*  REGS:   AX and Flags clobbered					*
;*									*
;*----------------------------------------------------------------------*

ACER_A20Handler proc   near

	or	ax,ax
	mov	al,0DDh 		; Assume disable
	jz	@f
	mov	al,0DFh 		; Oh, it's actually enable
@@:
	push	dx			; ACERs do it by writing to
	mov	dx,329h 		;   this port
	out	dx,al
	pop	dx
	mov	ax,1
	ret

ACER_A20Handler endp

EndACER_A20Handler:


;*----------------------------------------------------------------------*
;*									*
;*  Wyse_A20Handler -							*
;*									*
;*	Enable/Disable the A20 line on Wyse 12.5 MHz 286 machines	*
;*									*
;*  ARGS:   AX = 0 for Disable, 1 for Enable				*
;*  RETS:   AX = 1 for success, 0 otherwise				*
;*  REGS:   AX, CX and Flags clobbered					*
;*									*
;*----------------------------------------------------------------------*

Wyse_A20Handler proc   near

	or	ax,ax
	mov	ah,0DFh 	; Assume enabling
	jnz	WAHSetA20
	mov	ah,0DDh 	; Actually disabling...

WAHSetA20:

	call	Wyse8042	; Make sure the Keyboard Controller is Ready
	jnz	WAHErr

	call	Wyse8042	; and do it again!
	jnz	WAHErr

	mov	al,0D1h		; Send D1h
	out	64h,al
	call	Wyse8042
	jnz	WAHErr

	mov	al,ah		; Send enable/disable...
	out	60h,al
	call	Wyse8042
	jnz	WAHErr

WAHExit:
	mov	ax,1
	ret

WAHErr:
	xor	ax,ax
	ret

Wyse_A20Handler endp


Wyse8042    proc    near

	xor	cx,cx
@@:	jmp	short $+2
	in	al,64h
	and	al,2
	loopnz	@b
	ret

Wyse8042    endp

EndWyse_A20Handler:


;*----------------------------------------------------------------------*
;*									*
;*  ZB_A20Handler -							*
;*									*
;*	Enable/Disable the A20 line on Zenith ZBIOS machines		*
;*									*
;*  ARGS:   AX = 0 for Disable, 1 for Enable				*
;*  RETS:   AX = 1 for success, 0 otherwise				*
;*  REGS:   AX, CX and Flags clobbered					*
;*									*
;*----------------------------------------------------------------------*



ZB_A20Handler	proc	near

	call	[lpExtA20Handler]	; It just so happens that AH/AL
					;   are setup just the way ZBIOS
	mov	ax,1			;   wants them to gate A20 on/off
	ret

ZB_A20Handler	endp

EndZB_A20Handler:

;*----------------------------------------------------------------------*
;*									*
;*  Dell_A20Handler							*
;*					                        	*				*
;*      Enable/disable A20 on Dell XBIOS-equipped machines              *
;*									*
;*  ARGS:   AX = 0 for Disable, 1 for Enable		 		*
;*  RETS:   AX = 1 (no failure conditions)                              *
;*  REGS:   All regs/flags preserved except AX				*
;*									*
;*  Note: Provided by Dell Computer Corp.                               *
;*									*
;*----------------------------------------------------------------------*
;
; M010
;
Dell_A20Handler PROC NEAR
	pushf					; BUGBUG all regs!?!?!?!?
	pusha
	push	ds
	push	es

	push	ax
	mov	ax,ss
	mov	es,ax
	mov	bx,sp
	call	DWORD PTR [lpExtA20Handler]
	add	sp,2

	pop	es
	pop	ds
	popa
	mov	ax,1
	popf
	ret

Dell_A20Handler ENDP
EndDell_A20Handler:


;*----------------------------------------------------------------------*
;*									*
;*  Ext_A20Handler -							*
;*									*
;*	Enable/Disable the A20 line using an external handler.		*
;*	Query A20 calls (AX=2) may also be supported by the handler.	*
;*									*
;*  ARGS:   AX = 0 for Disable, 1 for Enable				*
;*  RETS:   AX = 1 for success, 0 otherwise				*
;*									*
;*----------------------------------------------------------------------*


Ext_A20Handler	proc	near

	call	[lpExtA20Handler]
	ret

Ext_A20Handler	endp

EndExt_A20Handler:


;****************************************************************************
;*									    *
;* The following routines detect the special case systems supported by	    *
;* specific A20 handling routines.  This code is called once during	    *
;* initialization, and then is overwritten or discarded.		    *
;*									    *
;****************************************************************************

	assume	cs:_text, ds:_text

;*----------------------------------------------------------------------*
;*									*
;*  IsAT								*
;*									*
;*	Check for IBM PC/AT 'standard' A20 handling.			*
;*									*
;*	This routine detects AT style A20 handling by invoking the	*
;*	DiddleA20 routine to toggle A20 on & off.  The AT handler	*
;*	supports a number of different delay methods, controlled by	*
;*	the ATA20Delay global variable.  IsAT calls DiddleA20 with	*
;*	different ATA20Delay values until it find one that works, or	*
;*	runs out of choices.						*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = 1 if we can control A20 via the 8042, 0 otherwise.	*
;*									*
;*----------------------------------------------------------------------*

	public	IsAT

IsAT	proc	near

IATAgain:
	pushf				;disable ints while gating A20
	cli

	call	DiddleA20

	popff

	or	ax,ax
	jnz	IsAT_Yes

	inc	ATA20Delay
	cmp	ATA20Delay,NUM_ALT_A20
	jbe	IATAgain

IsAT_No:
	xor	ax,ax
	ret

IsAT_Yes:
	mov	ax,1
	ret

IsAt	endp

;*----------------------------------------------------------------------*
;*									*
;*  Trace_PTL_Func					HARDWARE DEP.	*
;*									*
;*	Check for Phoenix Cascade BIOS enabling interrupts on an A20	*
;*	manipulation function call.  Early copies of the Phoenix	*
;*	Cascade BIOS enabled interrupts on calls to manipulate the A20	*
;*	line.  Unfortunately, this BIOS was source-licensed to OEMs, to *
;*	there is no easy detection method.  This function installs a	*
;*	trace exception handler, then calls the ROM with the trace flag *
;*	set.  The trace exception handler inspects the flags image on	*
;*	the stack, and sets a variable if it is ever called with	*
;*	interrupts enabled.						*
;*									*
;*  ARGS:   AX = PTL BIOS function to call				*
;*  RETS:   Ints_Are_Disabled set to zero if interrupts have been	*
;*	    enabled.							*
;*  REGS:   AX, flags used.						*
;*									*
;*----------------------------------------------------------------------*

Ints_Are_Disabled	dw	1
Original_INT1_Hook	dd	?

	public	Trace_PTL_Func

Trace_PTL_Func	proc	near
;
; Save the original trace exception handler.
; Install our own trace exception handler.
;
	pushf
	cli
	push	bx
	push	ax		; AX = function number
	push	es

	xor	ax,ax
	mov	es,ax
	lea	ax,INT1_Hook
	xchg	ax,es:[4]
	mov	word ptr [Original_INT1_Hook],ax
	mov	ax,cs
	xchg	ax,es:[6]
	mov	word ptr [Original_INT1_Hook+2],ax
	pop	es
;
; Set the trace flag, then call the BIOS to perform the requested function.
;
	pop	bx
	pushf
	pop	ax
	or	ah,1			; Set the trace flag.
	push	ax
	mov	ax,bx
	popf
	call	PTL_A20_Bios_Entry

	pop	bx
	popf
	ret

Trace_PTL_Func	endp


;*----------------------------------------------------------------------*
;*									*
;*  INT1_Hook						HARDWARE DEP.	*
;*									*
;*	Trace exception handler.  Inspects the flags image on the stack *
;*	to determine whether the ROM BIOS enables interrupts.  If	*
;*	called from code with interrupts enabled, sets the variable	*
;*	Ints_Are_Disabled to zero.  De-installs itself if interrupts	*
;*	are enabled or interrupted CS is equal to our CS.		*
;*									*
;*----------------------------------------------------------------------*

	public	INT1_Hook

INT1_Hook	proc	near

	push	bp
	mov	bp,sp		    ; BP -> BP IP CS Flags
	push	ax
;
; Un-hook INT on entry, so we can debug (portions of) this routine.
;
	push	es
	pushf
	cli
	xor	ax,ax
	mov	es,ax
	mov	ax,word ptr [Original_INT1_Hook]
	mov	es:[4],ax
	mov	ax,word ptr [Original_INT1_Hook+2]
	mov	es:[6],ax
	and	byte ptr [bp+7],NOT 1	; Zap trace flag.
	popf
	pop	es

	test	byte ptr [bp+7],2   ; Interrupts enabled?
	jz	I1H_checkCS

	mov	[Ints_Are_Disabled],0
	jmp	short I1H_done

I1H_checkCS:

	mov	ax,cs
	cmp	[bp+4],ax
	je	I1H_done

I1H_continue:
;
; Re-hook INT 1, and IRET to traced code.
;
	push	es
	pushf
	cli
	xor	ax,ax
	mov	es,ax
	lea	ax,INT1_Hook
	mov	es:[4],ax
	mov	ax,cs
	mov	es:[6],ax
	popf
	pop	es
	or	byte ptr [bp+7],1	; Reset trace flag.

I1H_done:

	pop	ax
	pop	bp
	iret

INT1_Hook	endp

;*----------------------------------------------------------------------*
;*									*
;*  IsPTLCascade					HARDWARE DEP.   *
;*									*
;*	Check for Phoenix Cascade BIOS					*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = 1 if we're on an Phoenix Cascade BIOS, 0 otherwise	*
;*  REGS:   AX, flags used.						*
;*									*
;*----------------------------------------------------------------------*

	public	IsPTLCascade

IsPTLCascade  proc near

	mov	ah,0C0h		; Get System Description Vector
	stc			; using interrupt 15h, function C0h.
	int	15h
	jc	IPTLNo		; Error - Not a Phoenix Cascade BIOS

	add	bx,es:[bx]	; Get to system config. table extension
	add	bx,2+PTL_STRING_Offset ; add the offset to the PTL string
	cmp	es:[bx],'TP'	; Look for 'TP' (start of PTL)
	jne	IPTLNo		; Not a Phoenix Cascade BIOS
	cmp	BYTE PTR es:[bx+2],'L' ; Look for 'L' (end of PTL)
	jne	IPTLNo		; Not a Phoenix Cascade BIOS

	mov	al,es:[bx+PTL_Feature1] ; Get Phoenix feature byte 1
	test    al,PTL_A20Bit	; Test the A20 support bit
	jz	IPTLNo		; No support for A20 control

IPTLFoundIt:

;
; Set the trace flag, then call the ROM BIOS to "inquire" A20.
; Our trace handler will detect whether the ROM BIOS enables
; interrupts or not, by monitoring the state of the flags
; word during the call.
;
	mov	ax,PTL_A20_InquireFunc	 ; call to inquire A20 state
	call	Trace_PTL_Func
	push	ax			 ; save result

;
; Set the trace flag, then call the ROM BIOS to enable A20.
; Our trace handler will detect whether the ROM BIOS enables
; interrupts or not, by monitoring the state of the flags
; word during the call.
;
	mov	ax,PTL_A20_EnableFunc	 ; call to enable A20
	call	Trace_PTL_Func
;
; Set the trace flag, then call the ROM BIOS to disable A20.
; Our trace handler will detect whether the ROM BIOS enables
; interrupts or not, by monitoring the state of the flags
; word during the call.
;
	mov	ax,PTL_A20_DisableFunc	 ; call to disable A20
	call	Trace_PTL_Func
	pop	ax			 ; AL = 1: A20 was on before
	or	al,al
	jz	@F
	mov	ax,PTL_A20_EnableFunc	 ; call to enable A20
	call	PTL_A20_Bios_Entry
@@:

	mov	ax,[Ints_Are_Disabled]	 ; Yup! Phoenix A20 support, return 1
	ret

;	IsFail is an alternate entry point, used for machines that we
;	  don't know how to detect.

IsFail:
IPTLNo:
	xor	ax,ax		; Not Phoenix or No support, return 0
	ret

IsPTLCascade	endp


;*----------------------------------------------------------------------*
;*									*
;*  Is6300Plus						HARDWARE DEP.   *
;*									*
;*	Check for AT&T 6300 Plus					*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = 1 if we're on an AT&T 6300 Plus, 0 otherwise		*
;*  REGS:   AX, flags used.						*
;*									*
;*  Side Effects:   MemCorr value updated to 384 if necessary.		*
;*									*
;*----------------------------------------------------------------------*

	public	Is6300Plus

Is6300Plus  proc near

	xor	ax,ax
	push    bx
	mov	bx,0fc00h		; Look for 'OL' at fc00:50
	mov	es,bx
	cmp	es:[0050h],'LO'
	jne     I6PNotPlus		; Not found
	mov	es,f000
	cmp	word ptr es:[0fffdh],0fc00h ; Look for 6300 PLUS
	jne     I6PNotPlus

	in	al,66h			; Look for upper extended memory
	and	al,00001111b
	cmp	al,00001011b
	jne     I6PNoMem
	mov	[MemCorr],384		; Save value

I6PNoMem:
	mov	ax,1			; We found a PLUS
I6PNotPlus:
	pop	bx
	ret

Is6300Plus  endp


;*----------------------------------------------------------------------*
;*									*
;*  IsPS2Machine					HARDWARE DEP.   *
;*									*
;*	Check for PS/2 machine						*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = 1 if we're on a valid PS/2 machine, 0 otherwise	*
;*  REGS:   AX	and Flags clobbered					*
;*									*
;*----------------------------------------------------------------------*

	public	IsPS2Machine

IsPS2Machine proc   near

	mov	ax,2403h	; Query A20 gate support
	stc
	int	15h
	jc	IPMNotYet	; CY must be clear
	or	ah,ah		;   and AH must be 00h
	jnz	IPMNotYet

	test	bl,00000010b	; Is port 92h gating supported?
	jnz	IPMFoundIt	; Yes!

IPMNotYet:
	mov     ah,0c0h		; Get System Description Vector
	stc
	int	15h
	jc	IPMNoPS2  	; Error?  Not a PS/2.

;	Do we have a "Micro Channel" computer?

	mov     al,byte ptr es:[bx+5] ; Get "Feature Information Byte 1"
	test    al,00000010b    ; Test the "Micro Channel Implemented" bit
	jnz	IPMFoundIt	; MCA, use PS/2 A20 code

;	The PS/2 Model 30/286 also uses the PS/2 A20 code, but it isn't
;	a MCA machine, check for the 30/286 model/submodel.  Likewise
;	with a few others...

	cmp	word ptr es:[bx+2],19F8h	; Tortuga ?
	jz	IPMFoundIt

	cmp	word ptr es:[bx+2],23F8h	; Aloha?
	jz	IPMFoundIt

	cmp	word ptr es:[bx+2],25F8h	; Bounty?
	jz	IPMFoundIt

	cmp	word ptr es:[bx+2],09FCh	; Model 30/286?
	jz	IPMFoundIt

	cmp	word ptr es:[bx+2],0BFCh	; Type/subtype FC/0Bh?
	jnz	IPMNoPS2

IPMFoundIt:
	xor	ax,ax		; Disable A20. Fixes PS2 Ctl-Alt-Del bug
	call    PS2_A20Handler
	mov	ax,1
	ret

IPMNoPS2:
	xor	ax,ax
	ret

IsPS2Machine endp


;*----------------------------------------------------------------------*
;*									*
;*  IsHPMachine						HARDWARE DEP.   *
;*									*
;*	Check for HP Vectra Machine, Model A or A+			*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = 1 if we're on a HP Vectra machine, 0 otherwise		*
;*  REGS:   AX, ES and Flags clobbered					*
;*									*
;*----------------------------------------------------------------------*

HPVectraMask	equ	00011111b	; Bit mask to check for HP Vectra

	public	IsHPMachine

IsHPMachine proc   near

	mov	es,f000
;
; M009 - Begin
;
	mov	ax, NOT 'PH'		; lets not have the pattern 'PH'
					; in the bus
	not	ax			; generate it indirectly

	cld				; charge the bus with CLDs 0xFC
	cld				
	cld
	cld
	cld
	cld
	cld
	cld

	cmp	ax, word ptr es:[0f8h]

	cld				; Charge the bus with 0xFC even
	cld				; on instruction prefetch
	cld
	cld
	cld
	cld
	cld
	cld
;
; M009 - end
;
	jne	IHMNoHP

	mov	al,es:[0fah]		; In f000:fa, 0 means A or A+
	and	al,HPVectraMask		; other HP Vectras are treated as an AT
	jnz	IHMNoHP

IHMIsHP:
	mov	ax,1
	ret

IHMNoHP:
	xor	ax,ax
	ret

IsHPMachine endp


;*----------------------------------------------------------------------*
;*									*
;*  IsHPVectra						HARDWARE DEP.   *
;*									*
;*	Check for HP Vectra Machine, Other than Classic A, A+ & HP LS/12*			*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = 1 if we're on a HP Vectra machine, 0 otherwise		*
;*  REGS:   AX, ES and Flags clobbered					*
;*									*
;*  If this routine gets called then the machine has already failed to	*
;*  satisfy that it is a HP Classic Vectra A & A+. So we dont have to	*
;*  eliminate Classic Vectra explicitly in this routine			*
;*----------------------------------------------------------------------*

HPLS12MASK	equ	01011b

	public	IsHPVectra

IsHPVectra proc   near

	mov	es,f000
;
; M009 - Begin
;
	mov	ax, NOT 'PH'		; lets not have the pattern 'PH'
					; floating on the bus
	not	ax			; generate it indirectly

	cld				; charge the bus with CLDs 0xFC
	cld				
	cld
	cld
	cld
	cld
	cld
	cld

	cmp	ax,word ptr es:[0f8h]

	cld				; Charge the bus with 0xFC even
	cld				; on instruction prefetch
	cld
	cld
	cld
	cld
	cld
	cld
;
; M009 - end
;
	jne	IHVNoHP

	mov	al,es:[0fah]		; In f000:fa, 0 means A or A+
	and	al,HPVectraMask		; other HP Vectras are treated as an AT
	cmp	al, HPLS12MASK
	je	IHVNoHP
IHVIsHP:
	mov	ax,1
	ret

IHVNoHP:
	xor	ax,ax
	ret

IsHPVectra endp


;*----------------------------------------------------------------------*
;*									*
;*  IsToshiba								*
;*									*
;*	Check for Toshiba 1600 or 1200XE				*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = 1 if we're on a T1600 or T1200XE, 0 otherwise          *
;*  REGS:   AX, ES and Flags clobbered					*
;*									*
;*----------------------------------------------------------------------*

	public	IsToshiba

IsToshiba proc	 near

	mov	ax,0FFFFh
	mov	es,ax
	cmp	byte ptr es:[0Eh],0FCh	; PC/AT id?
	jne	ITNo			; can't be what we want then

	mov	al,byte ptr es:[0Ah]	; Toshiba puts their own id here
	cmp	al,02Bh 		; Special T1600 id?
	je	ITMaybe
	cmp	al,02Ah 		; Special T1200XE id?
	jne	ITNo
ITMaybe:
	mov	ax,0FE00h		; Do a further check, the Toshiba
	mov	es,ax			;   machines have a name string at
	cmp	word ptr es:[0],'1T'	;   FE00:0000 (T1600 or T1200XE)
	jne	ITNo
	mov	ax,word ptr es:[2]
	cmp	ax,'06' 		; T160...
	je	ITYes
	cmp	ax,'02' 		; T120...
	jne	ITNo
ITYes:
	mov	ax,1
	ret

ITNo:
	xor	ax,ax
	ret

IsToshiba endp


;*----------------------------------------------------------------------*
;*									*
;*  IsZBIOS								*
;*									*
;*	Check for Zenith ZBIOS machine					*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = 1 if we're on a Zenith ZBIOS system, 0 otherwise       *
;*  REGS:   BX, ES and Flags clobbered					*
;*									*
;*----------------------------------------------------------------------*

ZBIOS_Id	dw	0FFE3h, 0F000h
ZBIOS_Entry_Off equ	0FFE8h

	public	IsZBIOS

IsZBIOS proc	near

	push	cx

	les	bx,dword ptr cs:[ZBIOS_ID]	; ZBIOS Machines are
	cmp	es:[bx],'BZ'			;   identified by the
	jnz	IZB_No				;   string 'ZBIOS' at
	cmp	es:[bx+2],'OI'			;   a fixed location
	jnz	IZB_No
	cmp	byte ptr es:[bx+4],'S'
	jnz	IZB_No

	mov	bx,es:[ZBIOS_Entry_Off] 	; Build far pointer to
	mov	word ptr [lpExtA20Handler],bx	;   ZBIOS entry point for
	mov	word ptr [lpExtA20Handler+2],es ;   A20 handler

;	This machine has a ZBIOS.  Make sure it supports the Gate A20 function.

	mov	ax, 00FFh		; ZBIOS Gate A20 Query call
	call	[lpExtA20Handler]	; Is Gate A20 supported?
	jc	IZB_No			;   CY set if not...

	mov	ax,1		; Yes, it's a ZBIOS machine
	pop	cx
	ret

IZB_No: 			; Not a ZBIOS machine
	xor	ax,ax
	pop	cx
	ret

IsZBIOS endp


;*----------------------------------------------------------------------*
;*									*
;*  IsBM60								*
;*									*
;*	Check for Bull Micral BM60					*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = 1 if we're on a BM60, 0 otherwise                      *
;*  REGS:   BX, ES and Flags clobbered					*
;*									*
;*----------------------------------------------------------------------*

BullSA_ID	dw	0E01Eh, 0F000h

	public	IsBM60
IsBM60	proc	near

	xor	ax,ax				; Clear AX

	les	bx,dword ptr cs:[BullSA_ID]	; Check for "Bull S.A"
	cmp	es:[bx	],'uB'			; to determine correct
	jnz	IsBM60_No			; family of machines
	cmp	es:[bx+2],'ll'
	jnz	IsBM60_No
	cmp	es:[bx+4],'S '
	jnz	IsBM60_No
	cmp	es:[bx+6],'A.'
	jnz	IsBM60_No

	cmp	byte ptr es:[bx-14h],'V'	; Q: Is this a BM60?
	jne	IsBM60_No			;   N:
	inc	ax				;   Y: We found one

IsBM60_NO:
	ret

IsBM60	endp


;*----------------------------------------------------------------------*
;*									*
;*  IsPhilips								*
;*									*
;*	Check for Philips machines					*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = 1 if we're on a Philips machine, 0 otherwise           *
;*  REGS:   AX and Flags clobbered					*
;*									*
;*  Note: Philips code received from Philips Corp.			*
;*									*
;*----------------------------------------------------------------------*

IsPhilips proc	 near

	push	cx
	push	ds
	push	es
	push	si
	push	di

	mov	ax,0f000h
	mov	ds,ax
	mov	si,0e000h		; ds:si -> @ BIOS signature location

	push	cs
	pop	es
	mov	di,offset phl_name	; es:di -> @ signature to check
	mov	cx,phl_name_len
	repz	cmpsb

	mov	ax,1			; assume found it
	jz	IsPhilips_exit		; condition from 'cmpsb'
	
	mov	ax,0			; did not find it

IsPhilips_exit:
	pop	di
	pop	si
	pop	es
	pop	ds
	pop	cx
	ret

phl_name	db	' PHILIPS'
phl_name_len	equ	$-phl_name

IsPhilips endp


;*----------------------------------------------------------------------*
;*									*
;*  IsCSS								*
;*									*
;*	Check for CSS Lab machines					*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = 1 if we're on a CSS Lab machine, 0 otherwise           *
;*  REGS:   AX and Flags clobbered					*
;*									*
;*  Note: ID method given by CSS labs & Award Software Inc.
;*									*
;*----------------------------------------------------------------------*


ROMCSSPtr	dw	0e0c1h, 0f000h
CSSStr		db	'CSS LAB'
CSSSTRLEN	equ	$ - offset CSSStr

IsCSS	proc	near
	push	ds
	push	si
	push	es
	push	di
	push	cx
	xor	ax, ax				; assume it is not
	mov	si, offset CSSStr
	push	cs
	pop	ds
	les	di, dword ptr cs:ROMCSSPtr
	mov	cx, CSSSTRLEN
	rep	cmpsb
	jne	@f
	inc	ax
@@:
	pop	cx
	pop	di
	pop	es
	pop	si
	pop	ds
	ret
IsCSS	endp


;*----------------------------------------------------------------------*
;*									*
;*  IsDellXBIOS								*
;*					                        	*				*
;*	Check for Dell XBIOS machines					*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = 1 if we're on a Dell XBIOS machine, 0 otherwise        *
;*  REGS:   trashes SI, DI, CX, AX					*
;*									*
;*          Stores address of XBIOS A20 handler in lpExtA20Handler      *
;*									*
;*  Note: ID method given by Dell Computer Corp.                        *
;*									*
;*----------------------------------------------------------------------*
;
; M010
;
xb_signature    db 'DELLXBIOS',00       ;XBIOS signature string & len
xb_sig_len      equ 10                  

                PUBLIC IsDellXBIOS
IsDellXBIOS     PROC NEAR

	push	ds
	push	es
	cld
             
	mov	ax,0f000h                   
	mov	es,ax                       
	les	di,es:[0ed00h]
	push	ds

	push	cs
	pop	ds
	lea	si,cs:xb_signature
	mov	cx,xb_sig_len		;look for XBIOS signature text

;	repe	cmpsb			;at *(F000:ED00)
idx05:
	lodsb

	cld
	cld
	cld
	cld
	cld
	cld
	cld
	cld

	cmp	al, byte ptr es:[di]
	jne	idx10
	inc	di
	loop	idx05
idx10:
	pop	ds
	add	di,2			;skip version ID, if present
	jcxz	for_feature                

xbios_fail:
	xor	ax, ax			;no XBIOS A20 handler present
	jmp	short xb_init_done      

xbios_chain:
	les	di,es:[di+2]		;handle XFT chain feature...

for_feature:
	mov	ax,es:[di]		;get feature identifier
	add	di,2			;index attribute flags
	cmp	ax,0			;NULL feature?
	je	xbios_fail		;yes, end of feature table
	cmp	ax,65535		;CHAIN feature?
	je	xbios_chain		;yes, go find next fragment...
	cmp	ax,11			;A20 feature?
	jne	next_feature		;no, skip it           
	mov	ax,es:[di]		;else get feature attributes
	test	ax,00001000b		;standard seg:off pointer?
	jz	next_feature		;no, wouldn't be prudent at 
					;this juncture
	les	bx,es:[di+2]
	mov	WORD PTR lpExtA20Handler,bx	;else record handler addr...
	mov	WORD PTR lpExtA20Handler+2,es 
	mov	ax, 1
xb_init_done:
	pop	es			;C=1 if XBIOS OK, 0 otherwise
	pop	ds
	ret

next_feature:
	mov	ax,es:[di]		;get feature attributes
	add	di,6			;skip attribs & handler addr
	test	al,110000b		;keystroke or appendix?
	jz	for_feature		;no, nothing else to skip
	mov	bx,ax
	and	bx,10000b		;keystroke?
	shr	bx,1
	shr	bx,1			;BX=2 if keystroke present,
	shr	bx,1			;else 0
	add	di,bx
	test	ax,100000b		;appendix?
	jz	for_feature
	add	di,es:[di]		;yes, add blocklen+2 to skip
	add	di,2
	jmp	for_feature

IsDellXBIOS     ENDP




;*----------------------------------------------------------------------*
;*									*
;*  DiddleA20 - 							*
;*									*
;*	Test the operation of the A20 line.				*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = 1,   able to enable & disable A20			*
;*	    AX = 0 &							*
;*	       CY clear, able to enable, but not disable		*
;*	       CY set,	 unable to enable				*
;*									*
;*----------------------------------------------------------------------*

	public	DiddleA20

DiddleA20	proc	near

	call	IsA20On 		; Is A20 already on?
	or	ax,ax			;   no, go check it out...
	jz	DiddleIt

	xor	ax,ax			;   yes, try to start off clean
	call	[TempA20Handler]	;   (disabled)

;	Verify that we can turn A20 on and off on command.

DiddleIt:
	mov	cx,1024 		; Diddle A20 this many times to be
					;   somewhat assured it really works.
DiddleAgain:
	push	cx
	mov	ax,1
	call	[TempA20Handler]	; Try to enable A20
	or	ax,ax
	jz	DA20Error		; Nope - phoosh ourselves

	call    IsA20On			; Is A20 on?
	or	ax,ax
	jz	DA20Error		; Nope - phoosh ourselves

	xor	ax,ax
	call	[TempA20Handler]	; Try to disable A20
	or	ax,ax
	jz	DA20Warn

	call    IsA20On			; Is A20 off?
	or	ax,ax
	jnz	DA20Warn

	pop	cx
	loop	DiddleAgain

DA20Okay:
	mov	ax,1			; Able to control A20,
	or	ax,ax			;   success == AX != 0 & CY clear
	ret

DA20Warn:				; Able to enable, but not disable!
	pop	cx			;   warning == AX = 0 & CY clear
	xor	ax,ax
	ret

DA20Error:				; Unable to enable!
	pop	cx			;   error == AX = 0 & CY set
	xor	ax,ax
	stc
	ret

DiddleA20	endp

;****************************************************************************
;*									    *
;*  The following routines install the proper A20 handler for this system.  *
;*  This code is called once during initialization, and then is overwritten *
;*  or discarded.							    *
;*									    *
;****************************************************************************

A20Struc	struc
 pfnDetect	dw	0	; offset of detection routine
 pStart 	dw	0	; A20 handler starting offset
 pEnd		dw	0	; A20 handler ending offset
 fA20Query	db	0	; NZ if handler supports A20 on/off query
 fDelayType	db	0	; -1 normally, otherwise special delay logic
 szName 	dw	0	; offset of handler name string
 bNum		db	0	; A20 handler number			M008
A20Struc ends

;*----------------------------------------------------------------------*
;*									*
;*  InstallA20 -					HARDWARE DEP.   *
;*									*
;*	Attempt to install the proper A20 Handler			*
;*									*
;*  ARGS:   None							*
;*  RETS:   None							*
;*  REGS:   AX, BX, CX, DI, SI, ES and Flags are clobbered		*
;*									*
;*----------------------------------------------------------------------*

	public	InstallA20

InstallA20  proc near

;  If the user selected a specific A20 handler, then use it.

	mov	bx,MachineNum		; if a machine number has been
	cmp	bx,-1			;   selected, use the corresponding
	jz	IA20Detect		;   A20 handler, else try to detect

	mov	ax,size A20Struc	; point to selected handler entry
	mul	bx
	mov	bx,ax
	add	bx,offset A20_scan_table

					; invoke the handler's detection rtn
	call	InsA20Detect		;   in case it needs to set something
					;   up for later use by the handler
	jmp	short InstallA20_Foundit


;  User didn't select A20 hander, detect which one to use.  First look
;  for an external handler.

IA20Detect:
	mov	ax,(INT2F_ID SHL 8) OR INT2F_EXT_A20
	int	2Fh
	cmp	al,80h			; al set to 80h if external handler
	jne	IA20NotExt

	mov	word ptr [lpExtA20Handler],bx	; got one, es:bx = entry point
	mov	word ptr [lpExtA20Handler+2],es

	mov	bx,offset ExtA20Entry
	mov	[bx].fA20Query,cl		; cl = query support flag

	push	dx
	mov	dx, offset InsExtA20Msg ; "Installed external A20 handler."
	call	DispInfoMsg
	pop	dx

	jmp	short InstallA20_Foundit


;  No external A20 handler, go down our internal list of handlers,
;  and pick one.

IA20NotExt:
	mov	bx,offset A20_scan_table

InstallA20_scanloop:
	cmp	bx,offset EndA20_scan_table
	jae	InstallA20_none
	call	InsA20Detect		; is this the one for us?
	or	ax,ax
	jnz	InstallA20_Foundit
	add	bx,size A20Struc
	jmp	short InstallA20_scanloop

; We didn't detect a valid A20 handler, point to error msg & return with error

InstallA20_none:
	mov	dx, offset NoA20HandlerMsg
	stc
	ret


InstallA20_Foundit:
	mov	cl, [bx].bNum
	mov	InstldA20HndlrN, cl	; update the handler number
	xor	cx,cx
	mov	cl,[bx].fA20Query	; global flag for handler supporting
	mov	fA20Check,cl		;   it's own A20 on/off check

	mov	cl,[bx].fDelayType	; flag for special A20 delay routine
	cmp	cl,-1			;   -1 if no special delay required
	jz	@f
	mov	ATA20Delay,cl
@@:
	mov	si,[bx].pStart

	cmp	si,offset _text:AT_A20Handler
	jnz	InsA20AsIs		; if this is the AT handler, do
	cmp	fCPUClock,0		;   we need to install the CPU Clock
	jnz	InsA20AsIs		;   rate prefix?
	mov	si,offset _text:AT_A20Normal	;AT_A20Normal == No
InsA20AsIs:

	mov	cx,[bx].pEnd
	sub	cx,si			; get length of driver to install

	push	cs			; es = cs
	pop	es
	mov	di,offset A20Handler
	rep	movsb			; move it!
	mov	[TopOfTextSeg],di	; keep track of used space

ifndef debug_tsr

;  Display which A20 handler was selected.

	mov	bx,[bx].szName		; skip it if no name string to display
	or	bx,bx
	jz	IA20NoName

	cmp	fQuiet, 0		; skip it if quiet mode
	jnz	IA20NoName

	push	dx			; display which handler installed
	mov	dx, offset InsA20Msg	; "Installed A20 handler number "
	call	DispInfoMsg
@@:
	mov	dl,[bx]
	or	dl,dl			; name string is null, not $ terminated
	jz	@f
	call	DispInfoChar
	inc	bx
	jmp	short @b
@@:
	cmp	ATA20Delay,0		; display which delay code is in effect
	jz	@f			;   (if any)
	mov	dl,','
	call	DispInfoChar
	mov	dl,ATA20Delay
	or	dl,30h
	call	DispInfoChar
@@:
	mov	dl,'.'
	call	DispInfoChar
	pop	dx

IA20NoName:

endif

	ret

; Common routine to invoke A20 handler detect routine

InsA20Detect:
	push	bx			; put A20 handler start address in
	mov	ax,[bx].pStart		;   known location for detect routine
	cmp	ax,offset _text:AT_A20Handler
	jnz	InsA20AsIs_1		; if this is the AT handler, do
	cmp	fCPUClock,0		;   we need to install the CPU Clock
	jnz	InsA20AsIs_1		;   rate prefix?
	mov	ax,offset _text:AT_A20Normal	;AT_A20Normal == No
InsA20AsIs_1:
	mov	TempA20Handler,ax
	call	[bx].pfnDetect		; is this the one for us?
	pop	bx
	ret

;  Note: the following table MUST be in the same order as the entries in
;  the MachineName table.  If you add one here, also add one there!

A20_scan_table	  label byte
	A20Struc <IsPTLCascade,PTL_CASCADE_A20Handler,EndPTL_CASCADE_A20Handler,1,-1,AltName3,3>
	A20Struc <Is6300Plus,$6300PLUS_A20Handler,End6300PLUS_Handler,0,-1,AltName5,5>
	A20Struc <IsPS2Machine,PS2_A20Handler,EndPS2_A20Handler,1,-1,AltName2,2>
	A20Struc <IsHPMachine,HP_A20Handler,EndHP_A20Handler,0,-1,AltName4,4>
	A20Struc <IsFail,ACER_A20Handler,EndACER_A20Handler,0,-1,AltName6,6>
	A20Struc <IsToshiba,AT_A20Handler,EndAT_A20Handler,0,2,AltName7,7>
	A20Struc <IsFail,Wyse_A20Handler,EndWyse_A20Handler,0,-1,AltName8,8>
	A20Struc <IsFail,Wyse_A20Handler,EndWyse_A20Handler,0,-1,AltName9,9>
	A20Struc <IsZBIOS,ZB_A20Handler,EndZB_A20Handler,0,-1,AltName10,10>
	A20Struc <IsFail,AT_A20Handler,EndAT_A20Handler,0,0,AltName11,11>
	A20Struc <IsFail,AT_A20Handler,EndAT_A20Handler,0,1,AltName12,12>
	A20Struc <IsFail,AT_A20Handler,EndAT_A20Handler,0,2,AltName13,13>
	A20Struc <IsPhilips,AT_A20Handler,EndAT_A20Handler,0,2,AltName13,13>
	A20Struc <IsCSS,AT_A20Handler,EndAT_A20Handler,0,1,AltName12,12>
	A20Struc <IsHPVectra,NHP_A20Handler,EndNHP_A20Handler,0,-1,AltName14,14>
	A20Struc <isFail,IC_A20Handler,EndIC_A20Handler,1,-1,AltName15,15>
	A20Struc <IsBM60,BM60_A20Handler,EndBM60_A20Handler,0,-1,AltName16,16>
	A20Struc <isDellXBIOS,Dell_A20Handler,EndDell_A20Handler,0,-1,AltName17,17>
	A20Struc <IsAT,AT_A20Handler,EndAT_A20Handler,0,-1,AltName1,1>
EndA20_scan_table label byte

ExtA20Entry	label	byte
	A20Struc <IsFail,Ext_A20Handler,EndExt_A20Handler,0,-1,0,0>

InstallA20  endp


_text	ends
	end


