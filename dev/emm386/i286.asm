.386p
page	58,132
;******************************************************************************
	TITLE	i286.asm - Support Routines for protected mode system
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:    EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;   Module:   i286.asm - Support Routines for protected mode system
;
;   Version:  0.02
;
;   Date:     January 31, 1986
;
;   Author:
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   01/31/86  Original
;   02/05/86  A 	added is286, is386
;   05/12/86  B-RRH	Cleanup and segment reorganization
;   06/03/86  C-SBP	added push/pop es to Init_GDT and changed Ring 0
;			stack to STACK0 and STACK0_SIZE.
;   06/28/86  0.02	Name changed from CEMM386 to CEMM (SBP).
;   02/22/88  3.30 (*B) Added IsP9 routine to check for 80P9 processor (RDV).
;   07/13/88  3.31 (*C) Change IsP9 routine to Is386s and add Is386c (RDV).
;   08/19/88  3.31 (*C) Fix Is386s & Is386c to only set carry for non-386 (RDV).
;   01/15/89  4.00 (*D) Add generic password 8042 detect (RDV)
;
;******************************************************************************
;
;   Functional Description:
;
;	Anthony Short
;	26th Dec 1985
;
;	DESCRIPTION
;
;	These routines manage the various 286 memory management
;	tables and manipulate descriptors and selectors.
;
;	The routines which deal with descriptors use the following
;	register usage conventions:
;
;	BX	- selector of required descriptor. The selector may
;		  have RPL bits present, the routines ignore them.
;
;	CX	- SIZE IN BYTES of segment. NOTE: descriptors contain
;		  limits, not sizes (limit = size - 1). Since everyone
;		  else talks sizes, these routines do too, and do their
;		  own conversion.
;
;	DX	- second selector when needed
;
;	AH	- access rights byte
;
;	AL, DX	- 24 bit physical address
;
;	ES:0	- pointer to the desired descriptor table.
;
;	All the routines which manipulate descriptors are callable
;	in both real and protected mode.
;
;	In general all registers are preserved.
;
;	The following routines are provided:
;
;		SetDescInfo	- set descriptor information
;		SetSegDesc	- set segment descriptor information
;
;		SegTo24 	- convert segment number to 24 bit addr
;		SegOffTo24	- convert seg:offset to 24 bit addr
;
;		InitGdt 	- set up parts of GDT which cannot easily
;				  be initialised statically.
;
;	WARNING This code is 286 specific, it will NOT run on an 8088.
;
;******************************************************************************
.lfcond 				; list false conditionals


	include VDMseg.inc
	include VDMsel.inc
	include desc.inc

;******************************************************************************
;		E X T E R N A L  R E F E R E N C E S
;******************************************************************************
GDT	segment
	extrn	GDTLEN:abs
GDT	ends

IDT	segment
	extrn	IDTLEN:abs
IDT	ends

LAST SEGMENT

    assume cs:LAST

;**	SetDescInfo - set descriptor information
;
;	The limit field of a specified descriptor is set.
;	  (limit = size - 1).
;	The base address of the specified descriptor is set.
;	The access field of the specified descriptor is set.
;
;	ENTRY	BX = selector
;		ES:0 = descriptor table to use
;		CX = limit
;		AL, DX = 24 bit base address
;		AH = access rights byte
;	EXIT	None
;	USES	Flags, other regs preserved
;
;	WARNING This code only works on a 286. It can be called in
;		either mode.

	public SetDescInfo
SetDescInfo proc near
	push	bx			; save selector
	and	bl,SEL_LOW_MASK

;	fill in the limit field

	mov	es:[bx],cx

;	fill in base address

	mov	es:[bx + 2],dx
	mov	es:[bx + 4],al

;	fill in access rights byte

	mov	es:[bx + 5],ah
	pop	bx
	ret
SetDescInfo endp


;**	SetSegDesc - set segment descriptor information
;
;	The limit field of a specified descriptor is set.
;	  (limit = size - 1).
;	The base address of the specified descriptor is set.
;	The access field of the specified descriptor is set.
;
;	ENTRY	BX = selector
;		ES:0 = descriptor table to use
;		CX = size
;		AL, DX = 24 bit base address
;		AH = access rights byte
;	EXIT	None
;	USES	Flags, other regs preserved
;
;	WARNING This code only works on a 286. It can be called in
;		either mode.

	public SetSegDesc
SetSegDesc proc near
	dec	cx			; convert size to limit
	call	SetDescInfo		; set descriptor information
	inc	cx			; restore size
	ret
SetSegDesc endp


;**	SegTo24 - convert segment to 24 bit physical address
;
;	The real mode segment number is convert to a 24 bit addr
;
;	ENTRY	AX = segment
;	EXIT	AL, DX = 24 bit physical address
;	USES	AH, Flags, other regs preserved
;
;	WARNING This code only works on a 286, it can be called in
;		either mode.
	public	SegTo24
SegTo24 proc near
	mov	dl,ah
	shr	dl,4			; DH = high byte of 24 bit addr
	xchg	ax,dx			; AH = high byte, DX = segment
	shl	dx,4			; DX = low word of 24 bit addr
	ret
SegTo24 endp


;**	SegOffTo24 - convert seg:off to 24 bit physical address
;
;	The specified real mode segment:offset is converted to
;	a 24 bit physical address.
;
;	ENTRY	AX = segment
;		DX = offset
;	EXIT	AL, DX = 24 bit physical address
;	USES	AH, Flags, other regs preserved.
;
;	WARNING This code only works on a 286. It can be called in
;		either mode.

	public SegOffTo24
SegOffTo24 proc near
	push	cx

;	Convert AX:DX into 24 bit addr in AL, DX

	mov	ch,ah
	shl	ax,4
	shr	ch,4			; CH = high byte
	add	dx,ax			; DX = low word
	mov	al,ch			; AL = high byte
	adc	al,0			; propagate cy from low word

	pop	cx
	ret
SegOffTo24 endp

;******************************************************************************
;   Is386 - return type of processor (386 vs. 8088/86/286).
;	This routine relies on Intel-approved code that takes advantage
;	of the documented behavior of the high nibble of the flag word
;	in the REAL MODE of the various processors.  The MSB (bit 15)
;	is always a one on the 8086 and 8088 and a zero on the 286 and
;	386.  Bit 14 (NT flag) and bits 13/12 (IOPL bit field) are
;	always zero on the 286, but can be set on the 386.
;
;	For future compatibility of this test, it is strongly recommended
;	that this specific instruction sequence be used.  The exit codes
;	can of course be changed to fit a particular need.
;
;	CALLABLE FROM REAL MODE ONLY - FAR ROUTINE
;
;   ENTRY:  (none)
;   EXIT:   STC if 8088/86/286
;	    CLC if 386
;   USED:   none
;   STACK:  6 bytes
;------------------------------------------------------------------------------
	public	Is386
Is386	proc	FAR
	push	ax
	pushf				; save entry flags
;
	xor	ax,ax			; 0000 into AX
	push	ax
	popf				; try to put that in the flags
	pushf
	pop	ax			; look at what really went into flags
	test	ax,08000h		;Q: was high bit set ?
	jnz	short IsNot386_exit	;  Y: 8086/8088
	mov	ax,07000h		;  N: try to set the NT/IOPL bits
	push	ax
	popf				;      ... in the flags
	pushf
	pop	ax			; look at actual flags
	test	ax,07000h		; Q: any high bits set ?
	jz	short IsNot386_exit	;   N: 80286
					;   Y: 80386
Is386_exit:
	popf				; restore flags
	clc				;  386
	jmp	short I386_exit 	; and leave

IsNot386_exit:
	popf				; restore flags
	stc				; not a 386

I386_exit:
	pop	ax
	ret				; *** RETURN ***

Is386	endp

;*****************************************************************************C
;   Is386s - returns equal if Taurus. (Original code name for 386s)	     *C
;	This routine relies on the "COMPAQ 386" ID in the ROM and then       *C
;	checks the ROM family code for the Taurus system ('F').              *C
;   ENTRY:  (none)							     *C
;   EXIT:   ZF = 1 if Taurus (equal)					     *C
;	    ZF = 0 if not (not equal)					     *C
;	    CY set if ROM image not COMPAQ 386				     *C
;----------------------------------------------------------------------------*C
	public	Is386s			;				     *C
Is386s	proc	FAR			; check for Taurus		     *C
	push	ax
	push	bx			;				     *C
	push	es			;				     *C

	mov	bx,0F000h		; ROM segment			     *C
	mov	es,bx			;				     *C

	stc				; set carry			     *C
	cmp	word ptr es:[0FFE9h],'C3' ; COMPAQ 386 ROM?                  *C
	jne	short exit386s 		; nope				     *C
	clc				; clear carry			     *C
	cmp	byte ptr es:[0FFE4h],'F';Q: is it Taurus ROM?                *C
	je	short exit386s
	cmp	byte ptr es:[0FFE4h],'R';Q: is it CARRERA ROM?
	je	short exit386s
	cmp	byte ptr es:[0FFE4h],'D';Q: is it EAGLE ROM?
	je	short exit386s
	cmp	byte ptr es:[0FFE4h],'B';Q: is it a TITAN
	je	short exit386s
;
;  For future CPQ machines, checks for a 386s processor via CMOS
;
	call	chk_cmos		; check CMOS
	cmp	ax,0			;Q: Valid CMOS?
	jne	short exit386s		; N: assume not a 386s
	mov	al,24h
	call	R_CMOS			; read byte 24 of CMOS

	shr	ax,4			; get CPU type in low 3 bits
	and	ax,0111b		; clear all but low oder 3 bits
	cmp	ax,0101b		;Q: Is this a 386sx processor?
					; Y: indicate via ZF
					; N: indicate via NZ
exit386s:pop	es			;				     *C
	pop	bx
	pop	ax
	ret				; *** RETURN ***		     *C
Is386s	endp				;				     *C

;*****************************************************************************D
;   IsP8042 - returns equal if password 8042.                                *D
;   ENTRY:  (none)							     *D
;   EXIT:   ZF = 1 if 8042 (equal)					     *D
;	    ZF = 0 if not (not equal)					     *D
;----------------------------------------------------------------------------*D
	public	IsP8042		 	;				     *D
IsP8042 proc	FAR
	PUSH	AX
	CLI
	CALL	test_8042_type
	STI
	OR	AX, AX
	POP	AX
	RET
IsP8042 endp				;				     *D

;****************************************************************************
; The following code for detecting password 8042's is from the KP utility
;****************************************************************************
;*** Begin 8042.EQU and KP.EQU
BAD_CHAR		equ	042h
BIG_WAIT		equ	0ffffh
COMMAND_REG_8042	equ	064h
DATA_REG_8042		equ	060h	; 8042 IBUF & OBUF must be empty*/
GET_D_TO		equ	-4	; get data time out
IBUF_FULL_8042		equ	002h	; must be 0 before sending data
STATUS_REG_8042 	equ	064h
OBUF_FULL_8042		equ	001h	; must be 0 before sending data
WRITE_KBD_OBUF		equ	0d2h
;*** End 8042.EQU and KP.EQU
;*********************************************************************
;   test_8042_type - Determine the type of 8042 in this machine.
;
;   Function Prototype:
;	extern int far pascal test_8042_type( void );
;
;   ENTRY: None.
;   EXIT: Returns AX=0 (8042 of TEXAS/HORIZON/PS2)
;		  AX<>0 (see 8042.equ and 8042.h)
;   USED:
;   STACK:
;---------------------------------------------------------------------
	PUBLIC	test_8042_type
test_8042_type	PROC

	mov	al, WRITE_KBD_OBUF	; determine whether this is TEXAS or
	call	send_8042_cmd		; ZEBRA hardware by sending a new
					; 8042 command
	and	ax, ax			; Q: send successful
	JZ	SHORT t_8_t_3
	mov	al, BAD_CHAR		; this character should be echoed
	call	send_8042_data		; when read
	and	ax, ax			; Q: send successful
	JZ	SHORT t_8_t_3
	call	get_8042_data		; was the bad character echoed
	cmp	ax, BAD_CHAR		; Q: echo
	je	SHORT t_8_t_3 		;  Y: continue
	mov	ax, -1			; return bad h/w error code
	ret				; *** Return ***
t_8_t_3:
	xor	ax, ax			; ZERO -> success
	ret				; *** Return ***
test_8042_type	ENDP

;*********************************************************************
;   send_8042_cmd - send a command to the 8042.  Waits for the
;			 input & output buffers to be empty.
;
;		Not callable by 'C'.  Internal function used by IOPL
;		subroutines.
;
;   ENTRY:	AL - contains the command
;   EXIT:	AX = 0 command unsuccessfully sent
;		AX <> 0 command successfully sent
;   USED:
;   STACK:
;---------------------------------------------------------------------
send_8042_cmd	PROC	NEAR
	push	bx
	push	ax			; save the command
	call	wait_for_iobuf_empty	; wait for in & out buffers to empty
	pop	bx			; restore the command
	and	ax, ax			; Q: wait fail
	jz	SHORT s_8_c_ret		;  Y: don't output
	xchg	ax, bx			; move command into AX
	out	COMMAND_REG_8042, al
	mov	al, 1			; data sent successfully
s_8_c_ret:
	pop	bx
	ret				; *** Return ***
send_8042_cmd	ENDP

;*********************************************************************
;   send_8042_data - send data to the 8042.  Waits for the
;			 input & output buffers to be empty.
;
;		Not callable by 'C'.  Internal function used by IOPL
;		subroutines.
;
;   ENTRY:	AL - contains the data
;   EXIT:	AX = 0 data unsuccessfully sent
;		AX <> 0 data successfully sent
;   USED:
;   STACK:
;---------------------------------------------------------------------
send_8042_data	PROC	NEAR
	push	bx
	push	ax			; save the data
	call	wait_for_iobuf_empty	; wait for in & out buffers to empty
	pop	bx			; restore the data
	and	ax, ax			; Q: wait fail
	je	SHORT s_8_d_ret		;  Y: don't send data
	xchg	ax, bx			; move the data into AX
	out	DATA_REG_8042, al
	mov	al, 1			; data sent successfully
s_8_d_ret:
	pop	bx
	ret				; *** Return ***
send_8042_data	ENDP

;*********************************************************************
;   get_8042_data - get data from the 8042.  Waits for the
;		    output buffer to be full.
;
;		Not callable by 'C'.  Internal function used by IOPL
;		subroutines.
;
;   ENTRY:
;   EXIT:	AX >= 0 contains the data, AX < 0 failure.
;   USED:
;   STACK:
;---------------------------------------------------------------------
get_8042_data	PROC	NEAR
	call	wait_for_obuf_full	; wait for data to read
	and	ax, ax			; did the wait "time out"
	jz	SHORT g_8_d_f 		; jump if so
	in	al, DATA_REG_8042	; get the data
	xor	ah, ah			; remove hi-byte
	ret				; *** Return ***
g_8_d_f:
	mov	ax, GET_D_TO		; error return code
	ret				; *** Return ***
get_8042_data	ENDP

;*********************************************************************
;   wait_for_obuf_full - Wait for the 8042's output buffer to become
;			'full'.
;
;		Not callable by 'C'.  Internal function used by IOPL
;		subroutines.
;
;   ENTRY:
;   EXIT:	AX = 0 obuf never became full
;		AX <> 0 obuf is full
;   USED:	AX, CX
;   STACK:
;---------------------------------------------------------------------
wait_for_obuf_full	PROC	NEAR
	push	cx
	mov	cx, BIG_WAIT		; store a 'wait' value
loop_ofull_01:
	in	al, STATUS_REG_8042	; get the 8042 status
	test	al, OBUF_FULL_8042	; Q: Output buffer full
	loopz	loop_ofull_01		; keep looping until obuf is full
	xchg	ax, cx			; use count as return code
	pop	cx
	ret				; *** Return ***
wait_for_obuf_full	ENDP

;*********************************************************************
;   wait_for_iobuf_empty - Wait for the 8042's input and output
;			   buffer to become 'empty'.
;
;		Not callable by 'C'.  Internal function used by IOPL
;		subroutines.
;
;   ENTRY:
;   EXIT:	AX = 0 ibuf & obuf never became empty
;		AX <> 0 ibuf & obuf are empty
;   USED:	AX, CX
;   STACK:
;---------------------------------------------------------------------
wait_for_iobuf_empty	PROC	NEAR
	push	cx
	mov	cx, BIG_WAIT		; store a 'wait' value
loop_iompte_01:
	in	al, STATUS_REG_8042	; get the 8042 status
					; Q: Input & Output buffer full
	test	al, (IBUF_FULL_8042 OR OBUF_FULL_8042)
	loopnz	loop_iompte_01		;  Y: keep looping until ibuf&obuf empty
	xchg	ax, cx			; use count as return code
	pop	cx
	ret				; *** Return ***
wait_for_iobuf_empty	ENDP

;/*****************************************************************************
;
; Synopsis    : state = chk_cmos()
;               int state;      CMOS invalid configuration bit
;
; Description : Check CMOS invalid configuration bit.
;
; Returns     : CMOS invalid configuration bit.
;                       0 - CMOS configuration OK.
;                      !0 - CMOS configuration invalid.
;                           or CMOS checksum bad
; Alters      : AX
;
; History     : Kelan Silvester       04/01/1988
;
;*****************************************************************************/
public	chk_cmos
chk_cmos proc near

        mov     al,0EH                  ; read CMOS diagnostics port
        call    r_cmos                  ; get CMOS byte 0EH
        and     ax,0000000001010000B    ; Is CMOS good ?
        ret
chk_cmos endp

    page
;******************************************************************************
;
;   R_CMOS  - Read a byte from cmos RAM
;
;   Entry:  al = cmos byte to read
;
;   Exit:   al = value of cmos byte read
;
;   Regs:   None
;
;******************************************************************************
public	r_cmos
r_cmos  proc    near
        out     70h, al
        jmp     $+2
        jmp     $+2
        in      al, 71h
        ret             ; *** Return ***
r_cmos  endp

    page
;******************************************************************************
;
;   W_CMOS - Write a byte to cmos RAM
;
;   Entry:  al = cmos byte to write
;           ah = value to write
;
;   Exit:   None
;
;   Regs:   ax
;
;******************************************************************************
public	w_cmos
w_cmos  proc    near

        out     70h, al
        jmp     $+2
        jmp     $+2
        mov     al, ah
        out     71h, al

        ret             ; *** Return ***
w_cmos  endp

CMOS_CHK_HIGH   equ 2Eh
CMOS_CHK_LOW    equ 2Fh

    page
;******************************************************************************
;   W_CMOS_CHECKSUM -- update the CMOS Checksum
;
;       Read system configuration from CMOS (if CMOS valid)
;   This logic reads system configuration values from CMOS
;   and verifies that the corresponding checksum is correct.
;
;   Entry: none
;   Exit: none
;   Registers changed: none
;******************************************************************************
public	w_cmos_checksum
w_cmos_checksum proc near
        push    ax              ; save registers
        push    bx              ;  ...
        push    cx              ;  ...
        push    dx              ;  ...

        mov     ah, 10h                             ; Start with byte 10H.
        mov     cx, 1Eh                             ; Reads this many bytes.
        xor     bx, bx          ; Init checksum value.
        xor     dx, dx          ;
chk_1:
        mov     al, ah          ; Which byte to read.
        call    r_cmos          ; Read cmos byte
        mov     dl, al          ;
        add     bx, dx          ; Add value to checksum
        inc     ah              ; Point to next byte in CMOS.
        loop    chk_1           ; Loop until all bytes read.

    ; Write high byte of checksum.
chk_2:
        mov     al, CMOS_CHK_HIGH
        mov     ah, bh          ; Checksum (Hi)
        call    w_cmos          ; write cmos byte

    ; Write low byte of checksum.
        mov     al, CMOS_CHK_LOW
        mov     ah, bl          ; Checksum (Lo)
        call    w_cmos          ; write cmos byte

        pop     dx              ; restore registers
        pop     cx              ;  ...
        pop     bx              ;  ...
        pop     ax              ;  ...

        ret
w_cmos_checksum endp


LAST	ends
	end
