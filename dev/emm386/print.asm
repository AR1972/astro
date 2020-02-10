.386p
page	58,132
;******************************************************************************
	title	PRINT.ASM - Protected Mode Print Routines
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:    EMM386D.EXE - MICROSOFT Expanded Memory Manager 386 DEBUG Driver
;
;   Module:   PRINT.ASM - Protected Mode Print Routines
;
;   Version:  0.08
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
;   05/12/86  A-RRH	Cleanup and segment reorganization
;   06/28/86  0.02	Name changed from CEMM386 to CEMM (SBP).
;   07/05/86  0.04	Moved to DCODE segment (SBP).
;   07/06/86  0.04	moved data to DDATA segment (SBP).
;   09/09/86  0.08	Added code to decide MONO vs. COLOR screen (SBP).
;
;******************************************************************************
;
;   Functional Description:
;
;******************************************************************************
.lfcond 				; list false conditionals

	public	PrintString
	public	kputc
	public	RowCol
	public	IsReal

	include VDMseg.inc
	include VDMsel.inc
	include desc.inc

DDATA	 segment
RowCol	dw	0
DDATA	 ends

CR	equ	0dh				; carriage return
LF	equ	0ah				; line feed
TAB	equ	9				; tab
BS	equ	8				; backspace

MONO_PARA	equ	0b000h
COLOUR_PARA	equ	0b800h


;***	kputc - write character/attribute to screen
;
;	This is a simple character output routine. It does not
;	interleave writes to video memory with the 6845. It
;	does not update the cursor position. It does not understand
;	anything except a CGA in 80x25 text mode.
;
;	ENTRY	PROTECTED MODE
;		AX - character-attribute to write
;		DS - kernel data selector
;
;	EXIT	none - character written
;
;	USES	AX, flags
;
DCODE	 segment

	assume	cs:DCODE, ds:DDATA, es:nothing, ss:nothing

kputc	proc	far

	push	bx				; save callers regs
	push	cx
	push	dx
	push	si
	push	di
	push	es
	push	ds

	call	IsReal				; need dual mode access
	jnz	SHORT kp10
	mov	dx, DEBD_GSEL			; selector for protected mode
	jmp	SHORT kp11
kp10:
	mov	dx, DDATA			 ; para for real mode
kp11:
	push	dx				; save this segment for later

	mov	ds, dx				; DS -> data segment
	mov	dx, ds:[RowCol] 		; DX = current row/col

	cmp	al, CR				; is character a CR?
	jne	SHORT kp1

	mov	dl, 0				; yes, go to column 0
	jmp	SHORT kp3			; jump to common code
kp1:

	cmp	al, LF				; is character a LF?
	jne	SHORT kp2

	inc	dh				; yes, go to next row
	jmp	SHORT kp3			; jump to common code
kp2:

	cmp	al, TAB 			; is it a tab
	jne	SHORT kp12
	and	dl, 0f8h			; mask off low 3 bits (8 ch)
	add	dl, 8				; move to next tab position
	jmp	SHORT kp3			; jmp to common code
kp12:

	cmp	al, BS				; is it backspace
	jne	SHORT kp13
	dec	dl				; back up one column
	jmp	SHORT kp3			; goto common code
kp13:
;	Must be ordinary character. Write it to screen, update position

	push	ax			; save char/attr

	mov	al, dh			; AL = row
	mov	ah, 80			; multiplier, 80 char per row
	mul	ah			; AX = cell at start of row
	mov	bh, 0
	mov	bl, dl			; BX = column
	add	bx, ax			; BX = cell
	shl	bx, 1			; BX = byte offset of cell

	call	GetVidSel		;(0.08) get video mem screen sel/seg
	mov	es, ax			; ES -> screen

	pop	es:[bx] 		; write character
	inc	dl			; update column
kp3:
;	Common code, first check for line wrap:

	cmp	dl, 80			; beyond rhs of screen?
	jl	SHORT kp4
	mov	dl, 0			; go to col 0
	inc	dh			; and move to next line
kp4:
;	Now check for scroll needed:

	cmp	dh, 25			; are we off end of screen?
	jl	SHORT kp5

;	Now scroll screen

	call	GetVidSel		;(0.08) get video mem screen sel/seg
	mov	ds, ax			; DS -> screen
	mov	es, ax			; ES -> screen

	mov	di, 0			; ES:DI = copy destination
	mov	si, 160 		; DS:SI = copy source
	mov	cx, 2000-80		; copy word count
	cld
	rep	movsw			; scroll

;	Blank bottom line

	mov	al, ' '
	mov	ah, 7			; AX = blank character

	mov	cx, 80			; number of cells to blank
	mov	di, 4000-160		; ES:DI = start point
	rep	stosw

;	Update position

	mov	dh, 24			; new row
kp5:
	pop	ds			; set DS to data again
	mov	ds:[RowCol], dx 	; update row/col

	call	SetCursor

	pop	ds			; restore regs
	pop	es
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx

	ret

kputc	endp
DCODE	 ends


;***	SetCursor - updates cursor position
;
;	This routine reprograms the 6845 cursor position, and
;	stores the new cursor position in the ROM bios data area.
;
;	ENTRY	DUAL MODE
;		DH, DL = row, col
;
;	EXIT	cursor updated
;
;	USES	ax, bx, cx, flags
;

CRT_COLS	equ	04ah
CURSOR_POSN	equ	050h
CRT_START	equ	04eh
ADDR_6845	equ	063h


DCODE	 segment

	assume	cs:DCODE, ds:DDATA, es:nothing, ss:nothing

SetCursor	proc	near

	push	ds
	mov	bx, 40h
	mov	ds, bx

;	Save new position in BIOS data area

	mov	ds:[CURSOR_POSN], dx

;	Calculate offset on screen

	mov	al, dh				; row
	mul	byte ptr ds:[CRT_COLS]		; row * cols
	mov	bl, dl				; bl = column
	mov	bh, 0				; bx = column
	add	ax, bx				; ax = offset in screen
	sal	ax, 1				; double for attribute bytes
	mov	cx, ds:[CRT_START]		; cx = start point of screen
	add	cx, ax				; cx = offset of cursor
	sar	cx, 1				; convert to char count only

;	Now program 6845

	mov	al, 14				; 6845 register
	mov	dx, ds:[ADDR_6845]		; base port #
	out	dx, al
	inc	dx
	jmp	short $+2

	mov	al, ch
	out	dx, al
	dec	dx
	jmp	short $+2

	mov	al, 15
	out	dx, al
	inc	dx
	jmp	short $+2

	mov	al, cl
	out	dx, al

	pop	ds

	ret

SetCursor	endp
DCODE		 ends


;***	PrintString - prints a message on console
;
;	This routine calls the "kernel" to print a string
;	one character at a time.
;
;	ENTRY	286 PROTECTED MODE
;		DS - DATA3_SEL
;		ES - DATA3_SEL
;		SI - offset in DS of null terminated string to print
;
;	EXIT	String printed
;
;	USES	Flags
;

DCODE	 segment

	assume	cs:DCODE, ds:DDATA, es:nothing, ss:nothing

PrintString	proc	near

	cld					; set up for string ops
	push	si				; save callers regs
	push	ax

pr1:						; loop printing until null
	lodsb					; al = char to print
	and	al, al				; terminator ?
	je	SHORT pr2
	mov	ah, 7				; attribute

	db	9ah				; far call
	dw	offset DCODE:kputc		 ; offset
	dw	DEBC_GSEL			; selector

	jmp	pr1				; back for more
pr2:
	pop	ax				; restore callers regs
	pop	si

	ret

PrintString	endp

;***	GetVidSel - Get video selector/segment
;
;	This routine sets AX to the selector/segment for the current
;	primary monitor - MONO or COLOR
;
;	ENTRY	DUAL MODE
;
;	EXIT	AX = selector / segment for primary monitor display memory
;
;	USES	flags
;
GetVidSel	proc	near

	call	IsReal			;Q: Real mode ?
	jnz	SHORT GVS_Real		; Y: determine MONO vs COLOR
	mov	ax, COLOUR_GSEL 	; N: Protected mode -> assume COLOR
	call	IsMono			;   Q: primary is MONO ?
	jnz	SHORT GVS_Exit		;     N: COLOR and Protected mode
	mov	ax, MONO_GSEL		;     Y: MONO and Protected mode
	jmp	SHORT GVS_Exit
GVS_Real:
	mov	ax, COLOUR_PARA 	; screen para for real mode COLOR
	call	IsMono			;   Q: primary is MONO ?
	jnz	SHORT GVS_Exit		;     N: COLOR and Real mode
	mov	ax, MONO_PARA		;     Y: MONO and Real mode
	jmp	SHORT GVS_Exit
GVS_Exit:
	ret

GetVidSel	endp

;***	IsReal - determine mode of cpu
;
;	This routine is useful when writing dual mode code.
;	It returns with 'Z' = 1 if the cpu is in protected mode,
;	otherwise 'Z' = 0.
;
;	ENTRY	DUAL MODE
;
;	EXIT	'Z' = 1 (protected mode), 0 (real mode)
;
;	USES	flags
;

IsReal	proc	near

	push	ax
	smsw	ax
	xor	al, 0ffh
	and	al, 1
	pop	ax
	ret				; returns with flags set

IsReal	endp

;***	IsMono - determine mode of cpu
;
;	Determines if the primary monitor is Monochrome.
;	It returns with 'Z' = 1 if the monitor is monochrome,
;	otherwise 'Z' = 0.
;
;	ENTRY	DUAL MODE
;			(selector 40h must point to 40:0 physical).
;
;	EXIT	'Z' = 1 (primary is monochrome), 0 (else)
;
;	USES	flags
;

IsMono	proc	near

	push	ds
	push	ax
	mov	ax,40h
	mov	ds,ax
	ASSUME	DS:romdata
	mov	ax,[EqStatus]
	and	ax,30h			; keep only primary display bits
	cmp	ax,30h			;Q: monochrome ?
					;   ZR => yes   NZ => no
	pop	ax
	pop	ds
	ASSUME	DS:DDATA
	ret				; returns with flags set

IsMono	endp

DCODE	 ends
	end

