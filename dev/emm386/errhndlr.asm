.386p
page	58,132
;******************************************************************************
	TITLE	ErrHndlr - Error Handler
;******************************************************************************
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;    Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;    Module:	ErrHndlr - Recover from exception and priveledged operation errors
;
;    Version:	2.00
;
;    Date:	June 6,1986
;
;    Authors:	Brad Tate
;
;******************************************************************************
;
;  CHANGES:
;
;    DATE     REVISION			DESCRIPTION
;  --------   --------   ------------------------------------------------------
;  06/06/86   Original
;  06/28/86   0.02	Name changed from CEMM386 to CEMM (SBP).
;  06/28/86   0.02	Removed STI at end of ErrHndlr (SBP).
;  06/28/86   0.02	Changed error # display to leading zeroes (SBP).
;  07/06/86   0.04	Changed assume to _DATA (SBP).
;  01/12/87   0.09	Added defines BOOT_KEY and CONTINUE to define
;			key make code for error handler input (SBP).
;  05/13/87   2.00	Moved to R_CODE segment (SBP).
;
;******************************************************************************
	page
;******************************************************************************
;
;  Functional description:
;
;	This module contains the code that displays an error message and
;	asks the user to continue or reboot.
;
;******************************************************************************
;
 	include	vdmseg.inc
 	include	vdmsel.inc
	include	emm386.inc
	include	kbd.inc
	include	emmfunct.inc
	include	emmdata.inc
	include oemdep.inc
	include	vm386.inc

;******************************************************************************
;  	Public Declarations
;******************************************************************************
;
	public	ErrHndlr		; Display message and continue or reboot
	public	Error_Flag
	public	EHReturnFar

;******************************************************************************
;  	Externs
;******************************************************************************
_DATA	segment
	extrn	pLastVMTF:word
_DATA	ends

R_CODE	segment
	extrn	ExitVirtual:far		; exit virtual mode
	extrn	RetReal:near		; return to real mode
;LEO	extrn	JumpReal:near		; continue in real mode

	Extrn	GoRealMode:near
	extrn	GoVirtualMode:near
	extrn	EHFarReturn:dword

ErrGoVirtualMode	proc	far
	call	GoVirtualMode
	jmp	dword ptr cs:[EHFarReturn]
ErrGoVirtualMode	endp


R_CODE	ends

R1_CODE	segment

	extrn	POE_Mess:byte		; privileged operation error
	extrn	POE_Num:byte 		; where to put error code
	extrn	POE_Len:abs		; length of message
	extrn	PEN_Mess:byte		; unrecoverable privileged error
	extrn	PEN_Num:byte 		; where to put error code
	extrn	PEN_Len:abs		; length of message
	extrn	EXCPE_Mess:byte		; exception error message
	extrn	EXCPE_Num:byte		; where to put error code
	extrn	EXCPE_Len:abs		; length of message
	extrn	DMASIZE_Mess:byte	; exception error message
	extrn	DMASIZE_Num:byte	; where to put error code
	extrn	DMASIZE_Len:abs		; length of message
	extrn	DMAMODE_Mess:byte	; exception error message
	extrn	DMAMODE_Num:byte	; where to put error code
	extrn	DMAMODE_Len:abs		; length of message
	extrn	egetc:near		; get keyboard character
	extrn	WaitKBD:near		; wait for keyboard ready

R1_CODE	ends

ATTR		EQU	07		; attribute for write string
WRSTR		EQU	1301h		; write string function code (format 1)
CPOSN		EQU	5*256+0		; cursor position to write

;
R1_CODE	segment
	ASSUME	CS:R1_CODE, DS:R1_CODE, ES:R1_CODE, GS:R1_CODE
;
;******************************************************************************
;		LOCAL DATA
;******************************************************************************

EUserDS		dw	0
EUserES		dw	0
EUserFS		dw	0
EUserGS		dw	0
EUserSS		dw	0
EUserSP		dw	0

Error_Flag	dw	0		; flags for Instruction Prefixes
masterp		db	0		; save master port value
slavep		db	0		; save slave port value
mode		db	0		; save mode
boot		db	0		; value to reboot
continue	db	0		; value to continue
;
;******************************************************************************
;
;	ErrHndlr - displays the appropriate error message and prompts the
;		    user for a character to continue or reboot.  The screen
;		    is cleared by this routine.  If the user chooses to
;		    continue, the system is in real mode.
;
;	*** This routine must be entered by a JUMP ***
;
;	entry:	ax = 0 => Privileged operation error
;		ax = 1 => Exception error
;		ax = 2 => Fatal DMA errors
;		bx = error number to display
;		STACK:
;		SS:[BP] -> points to saved EBP on GP fault stack frame
;		SS:[SP] -> bottom of pushad
;
;			=> to unwind:
;				popad
;				pop	ebp	; entry BP pts to here
;				add	sp,4	; throw away error code
;				iret
;
;
;
;
;	exit:	either reboot, or continue the system in real mode
;		via the JumpReal routine.
;
;	used:	none
;
;	stack:
;
;******************************************************************************
ErrHndlr	proc	near
;
	push	VDMD_GSEL	;LEO  access _DATA data
	pop	ds              ;LEO
	push	RCODEA_GSEL	;LEO  access R_CODE data
	pop	gs              ;LEO
	push	R1CODEA_GSEL
	pop	fs
	assume	ds:_DATA,gs:R_CODE, fs:R1_CODE

;
;  If a privilige error occurs while CEMM is nested more than one level deep
;  during interrupt processing, the user may not continue by turning CEMM off.
;  Thus, the following will change the privilige error to an exception which
;  will force a reboot.  This condition needs an error message which needs to
;  be documented.
;

	cmp	ax,PrivErr		; Q: Privilige error?
	jne	short EHcont		; N: continue
	cmp	[pLastVMTF],-1		; Q: Is CEMM nested more than one 
					; level?
	jne	short EHunrec		; Y: unrecoverable privileged error

	cmp	gs:[UMBHMA], TRUE	; Q: are we providing UMBs
	jne	EHcont			; N: continue

EHunrec:
	mov	ax,UnRecPrivErr		; Y: unrecoverable privileged error

EHcont:

;
;  Save error type and number
;
	mov	gs:[ErrType],ax	;LEO
	mov	gs:[ErrNum],bx  ;LEO

;
;  Setup RSS_GSEL in GDT to have same base as user stack
;
	mov	ax,GDTD_GSEL		; ES access to GDT
	mov	es,ax

	movzx	ebx,[bp][VTFOE].VMTF_SS	; get base address of user stack
	mov	fs:[EUserSS],bx		; save user stack segment
	shl	ebx,4
	mov	es:[RSS_GSEL][2],bx	; bits 0-15 of base address
	shr	ebx,16
	mov	es:[RSS_GSEL][4],bl	; bits 16-23
	mov	es:[RSS_GSEL][7],bh	; bits 24-31
;
;  Setup user stack with proper return address and CS:IP & flags inorder
;  to return to the user code.
;
	mov	bx,[bp][VTFOE].VMTF_ESP	; get offset for user stack

	mov	ax,RSS_GSEL		; access user stack via DS
	mov	es,ax

	mov	ax,[bp][VTFOE].VMTF_EFLAGS ; get user flags
	sub	bx,2			    ; and place it on the user stack
	mov	es:[bx],ax

	mov	ax,[bp][VTFOE].VMTF_CS	; get code segment
	sub	bx,2			; and place it on the user stack
	mov	es:[bx],ax

	mov	ax,[bp][VTFOE].VMTF_EIP; get instruction pointer
	sub	bx,2			; and place it on the user stack
	mov	es:[bx],ax

	movzx	esp,sp			; access only 64K
;
;  Save all segment registers for real mode
;
	mov	ax,[bp][VTFOE].VMTF_DS	; get user DS
	mov	fs:[EUserDS],ax

	mov	ax,[bp][VTFOE].VMTF_ES	; get user ES
	mov	fs:[EUserES],ax

	mov	ax,[bp][VTFOE].VMTF_FS	; get user FS
	mov	fs:[EUserFS],ax

	mov	ax,[bp][VTFOE].VMTF_GS	; get user GS
	mov	fs:[EUserGS],ax
;
;  Save user stack values
;
	mov	fs:[EUserSP],bx
;
	;
	; change to emm386's real mode stack. 
	;

	mov	ax,GDTD_GSEL		; ES access to GDT
	mov	es,ax

	xor	ebx,ebx
	mov	bx, seg R_STACK		; get emm386's real stack

	shl	ebx,4
	mov	es:[RSS_GSEL][2],bx	; bits 0-15 of base address
	shr	ebx,16
	mov	es:[RSS_GSEL][4],bl	; bits 16-23
	mov	es:[RSS_GSEL][7],bh	; bits 24-31

	lea	bx, R_STACK:RealStackTop
;
;  Switch to EMM386's stack
;
	mov	ax, RSS_GSEL
	mov	ss, ax
	mov	sp, bx
;
;  Return to virtual mode
;

	PJmp	RCODE_GSEL, R_CODE:ErrGoVirtualMode
EHReturnFar:

	pushad                  ;LEO
	push	ds              ;LEO
	push	es		;LEO

	push	seg R_CODE
	pop	es
	assume	es:R_CODE

	mov	ax, cs
	mov	ds, ax		; es = R1_CODE
	assume	ds:R1_CODE

	mov	ax,es:[ErrType]    ;LEO
	mov	bx,es:[ErrNum]     ;LEO

;
;  CEMM is OFF now
;
	mov	es:[Current_Mode],MODE_OFF
;

	push	bp			; save entry BP

	push	ax			; save input to this routine
	in	al,MASTER		; get value of master interrupt port
	mov	[masterp],al		; save it
	in	al,SLAVE		; get value of slave interrupt port
	mov	[slavep],al		; save it
	mov	al,DIS_MSTSLV		; value to disable master/slave int
	out	MASTER,al		; disable master
	out	SLAVE,al		; disable slave
	mov	al,DISABLE_NMI		; value to disable NMI
	out	NMI_CMD,al
kbdbusy:
	call	egetc			; q: is there stuff in keyboard buffer?
	jnz	short kbdbusy		; y: get it and pitch it
					; n: continue
	pop	ax			; get entry condition
	cmp	ax,PrivErr		 ;Q: Privileged error?
	jne	short excep		 ; N: try exception error
	mov	bp,offset R1_CODE:POE_Mess; Y: privileged error
	mov	cx,POE_Len
	mov	di,offset R1_CODE:POE_Num; error number location
	mov	[boot],BOOT_KEY		; key to boot
	mov	[continue],CONTINUE_KEY	; key to continue
	jmp	short print_msg
excep:
	cmp	ax,ExcpErr		    ;Q: Exception error?
	jne	short DMAsize		    ; N: try fatal DMA error
	mov	bp,offset R1_CODE:EXCPE_Mess ; Y: load up exception error
	mov	cx,EXCPE_Len		; length of msg
	mov	di,offset R1_CODE:EXCPE_Num	; error number location
	mov	[boot],ENTER_KEY		; key to reboot
	mov	[continue],0ffh		; can't continue
	jmp	short print_msg

DMAsize:
	cmp	ax,DMASizeErr		      ;Q: DMA buffer too small?
	jne	short DMAmode		      ; N: DMA mode
	mov	bp,offset R1_CODE:DMASIZE_Mess ; Y: load up DMASIZE error
	mov	cx,DMASIZE_Len		      ; length of msg
	mov	di,offset R1_CODE:DMASIZE_Num  ; error number location
	mov	ax,bx			      ; print new D parameter
	call	PrintAX
	mov	di,offset R1_CODE:DMAMODE_Num  ; fake b2asc
	mov	[boot],ENTER_KEY	      ; key to reboot
	mov	[continue],0ffh		      ; can't continue
	jmp	short print_msg
DMAmode:
	cmp	ax,DMAModeErr		      ;Q: DMA mode error?
	jne	short UnRecPriv		      ; N: Unrecoverable privileged
	mov	bp,offset R1_CODE:DMAMODE_Mess ; Y: load up DMAMODE error
	mov	cx,DMAMODE_Len		      ; length of msg
	mov	di,offset R1_CODE:DMAMODE_Num  ; fake b2asc
	mov	[boot],ENTER_KEY	      ; key to reboot
	mov	[continue],0ffh		      ; can't continue
	jmp	short print_msg
UnRecPriv:
	mov	bp,offset R1_CODE:PEN_Mess	; load up message
	mov	cx,PEN_Len			; length of msg
	mov	di,offset R1_CODE:PEN_Num	; error number location
	mov	[boot],ENTER_KEY		; key to reboot
	mov	[continue],0ffh			; can't continue

print_msg:
	mov	ax, ds
	mov	es, ax
	mov	ax,bx			; error number in ax
	call	b2asc			; convert to ascii
	mov	ah,0fh			; read video state
	int	10h
	mov	[mode],al		; save mode
	mov	ax,3			; set to mode 3
	int	10h			; standard 80 x 25 color
	mov	dx,CPOSN		; cursor position
	mov	bl,ATTR			; attribute
	mov	ax,WRSTR		; write string function code
	int	10h			; do it
	cli				; make sure int 10 didn't enable
key_loop:
	call	egetc			; get a character
	jz	short key_loop		; nothing there yet

	cmp	al,[continue]		; q: continue?
	je	short err_cont		; y
	cmp	al,[boot]		; q: boot?
	jne	short key_loop		; n: try again
;******************************************************************************
;
;		Reboot system
;
;******************************************************************************
	assume	ds:romdata
	mov	ax,romdata
	mov	ds,ax			; ds = romdata segment
	mov	[freset],0		; cold restart
	mov	al,0fh or DISABLE_NMI		; shutdown byte address/disable NMI
	out	NMI_CMD,al			; write CMOS address
	jmp	short $+2		; delay
	mov	al,0h			; shutdown code 0 = processor reset
	out	71h,al			; write shutdown code to shutdown byte
	call	WaitKBD			; wait for 8042 to accept command
	mov	al,0feh			; feh = pulse output bit 0 (reset)
	out	KbStatus,al		; reset processor
	db	0eah
	dw	0ffffh
	dw	0f000h
	hlt

	assume	ds:R1_CODE
;
;   here if USER elects to continue in real mode
;
err_cont:
	xor	ah,ah			; ah = 0 to set video mode
	mov	al,[mode]		; restore their mode
	int	10h
	cli				; turn them off...
;
;	restore master, slave, and NMI
;
	mov	al,[masterp]		; get value of master interrupt port
	out	MASTER,al		; restore it
	mov	al,[slavep]		; get value of slave interrupt port
	out	SLAVE,al		; restore it
	mov	al,ENABLE_NMI		; value to enable NMI
	out	NMI_CMD,al
;
	pop	bp			; restore entry BP

	pop	es	;LEO
	pop	ds	;LEO
	popad		;LEO

	mov	al,0Fh
	out	84h,al		; port 84/85 return to real sequence
	mov	al,00h
	out	85h,al
	jmp	$+2		; clear prefetch/avoid race cond

;
;  Restore segment resiters
;
	mov	ds,cs:[EUserDS]
	mov	es,cs:[EUserES]
	mov	fs,cs:[EUserFS]
	mov	gs,cs:[EUserGS]

	;
;  Switch to user stack
;
	mov	ss,cs:[EUserSS]
	mov	sp,cs:[EUserSP]

	iret		;LEO

;LEO	jmp	JumpReal


ErrHndlr	endp

page
;******************************************************************************
;
;	b2asc - converts binary to ascii decimal and store at DS:DI.
;		Stores 2 ascii chars.
;		Decimal # is right justified and filled on left with 0s.
;
;	entry:	ax = binary number
;		ds:di = place to store ascii chars.
;
;	exit:	ASCII decimal representation of number stored at R_CODE:DI
;
;	used:	none
;
;	stack:
;
;******************************************************************************
;
b2asc	proc    near
	push	ax
	push	cx
	push	dx
;
	xor	dx,dx			; clear word extension
	mov	cx,10
	div	cx          		; divide by power of 10
	add	al,'0'			; put into ascii format
	mov	ds:[di],al		; put ascii number into string
	add	dl,'0'
	mov	ds:[di+1],dl
;
	pop	dx
	pop	cx
	pop	ax
	ret				; *** return ***
b2asc	endp

;==============================================================================
;==
;==  PrintAX: Output the value in AX
;==
;==  Enter: AX 	    = Binary number to convert to decimal
;==	    DS:[DI] = place to store ascii chars.
;==
;==  Exit:  DS:[DI] = next character position
;==
;==============================================================================
PrintAX	proc	near
	push	ax
	push	cx
	push	dx

	mov	cx,10		; divide value
	xor	dx,dx		; clear dx
	div	cx		; ax/10 remainder in dx
	or	ax,ax		;Q: Was AX < 10?
	jz	short AXPrint	; Y: print decimal value
	push	dx		; N: Save result
	call	PrintAX 	; divide again
	pop	dx		; get previous result
AXPrint:			;
	add	dl,'0'          ; make it printable
	mov	ds:[di],dl		; place in buffer
	inc	di		; increment cursor position

	pop	dx
	pop	cx
	pop	ax
	ret
PrintAX	endp

R1_CODE	ENDS
	END
