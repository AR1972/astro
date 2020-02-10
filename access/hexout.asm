;----------------------------------------------------------------------------
; HexCharsOut
;
; Expects: 	portout = the byte
;		portid	= the ID of where the byte originated
;
; Returns:	Nothing
;
; Changes:	Nothing
;----------------------------------------------------------------------------

	TITLE HexCharsOut

INCLUDE	keyboard.inc

	EXTRN	_comp_id:byte				; from Handicap.asm
	EXTRN	old_row:byte
	EXTRN	old_col:byte
	EXTRN	fserial_debugging:byte

IFDEF	BUG

	EXTRN	portid:byte
	EXTRN	portout:byte

ENDIF; BUG

_TEXT	segment	word public 'CODE'

	assume CS:_TEXT
	assume DS:NOTHING
	assume ES:NOTHING
	assume SS:NOTHING

;-----------------------------------------------------------------------------

temp_col	DB	00				; temporary col. count

HexCharsTable	DB	"0123456789ABCDEF"



serialout	PROC	NEAR

	assume	DS:_TEXT
	
	mov	ah,1
;	cmp	_comp_id,8
;	je	SO_10
	mov	dx,0					; for COMM 1
	jmp	short SO_20

SO_10:

	mov	dx,1					; for COMM 2
SO_20:

	int	14h
	ret	  

serialout	endp
;-----------------------------------------------------------------------------

print	proc	near

	assume	DS:_TEXT

	push	bx
	push	cx
	push	dx
	push	ax


	mov	ah,3					; read current cursor position
	mov	bh,0					; page 0
	int	10h					; video interrupt
	mov	old_row,dh				; save current row
	mov	old_col,dl				; save current column

	cmp	temp_col,70
;	cmp	temp_col,130
	jl	print_5
	mov	temp_col,0

print_1:

	mov	ah,2					; prepare to set current position
	mov	dh,23					; row 23
;	mov	dh,43
	mov	dl,temp_col    				; column 0 to start
	int	10h

	mov	al,32
	mov	cx,1					; line of spaces
	inc	temp_col
	mov	ah,0ah
	int	10h

	cmp	temp_col,70
;	cmp	temp_col,130
	jle	print_1
	mov	temp_col,0

print_5:
	mov	ah,2					; prepare to set current position
	mov	dh,23					; row 23
;	mov	dh,43
	mov	dl,temp_col    				; column 0 to start
	int	10h
	inc	temp_col

; character to write is in al

print_10:

	pop	ax
	mov	dx,ax
	push	ax

	mov	al,dl
	mov	ah,0ah					; write a character to the screen Text
	mov	cx,1					; write 1 character
	int	10h


	mov	ah,2					; restore current cursor position
	mov	dh,old_row
	mov	dl,old_col
	mov	bh,0
	int	10h

	pop	ax
	pop	dx
	pop	cx
	pop	bx


	ret

print	endp

	PUBLIC	HexCharsOut

HexCharsOut	PROC
	
;	push	cs
;	pop	ds
	assume DS:_TEXT


	push	ax
	push	dx
	push	si
	push	bx
	push	cx
	push	ds

;	mov	ax,@data
;	mov	ds,ax



;--------------------------------------------------------------------

	mov	cl,4
	xor	ax,ax				; clear ax register

	mov	ah,portid			; put byte in AH
	and	ah,0f0h				; high nibble first
	mov	al,ah
	rcr	al,cl				; rotate right 4 bits
	xor	ah,ah
	mov	si,ax
	mov	bx,Offset HexCharsTable
	mov	al,[bx+si]			; move contents of address bx + si into  al

	cmp	fserial_debugging,true		; are we to send out serial port ?
	jne	HCO_15

	call	serialout
	jmp	short HCO_20
HCO_15:
	call	print

HCO_20:
	xor	ax,ax

	mov	ah,portid		
	and	ah,0fh				; low nibble second
	mov	al,ah
	xor	ah,ah
	mov	si,ax
	mov	bx,Offset HexCharsTable
	mov	al,[bx+si]

	cmp	fserial_debugging,true		; are we to send out serial port ?
	jne	HCO_25

	call	serialout
	jmp	short HCO_30

HCO_25:
	call	print
HCO_30:

;---------------------------------------------------------------------
	mov	al,32

	cmp	fserial_debugging,true		; are we to send out serial port ?
	jne	HCO_35

	call	serialout
	jmp	short HCO_40

HCO_35:
	call	print

HCO_40:
;---------------------------------------------------------------------

	xor	ax,ax

	mov	ah,portout			; put byte in AH
	and	ah,0f0h				; high nibble first
	mov	al,ah
	rcr	al,cl				; rotate right 4 bits
	xor	ah,ah
	mov	si,ax
	mov	bx,Offset HexCharsTable
	mov	al,[bx+si]

	cmp	fserial_debugging,true		; are we to send out serial port ?
	jne	HCO_45

	call	serialout
	jmp	short HCO_50
HCO_45:
	call	print
HCO_50:

	xor	ax,ax

	mov	ah,portout	
	and	ah,0fh				; low nibble second
	mov	al,ah
	xor	ah,ah
	mov	si,ax
	mov	bx,Offset HexCharsTable
	mov	al,[bx+si]

	cmp	fserial_debugging,true		; are we to send out serial port ?
	jne	HCO_55

	call	serialout
	jmp	short HCO_60
HCO_55:
	call	print
HCO_60:
;---------------------------------------------------------------------
	mov	al,32

	cmp	fserial_debugging,true		; are we to send out serial port ?
	jne	HCO_65

	call	serialout
	jmp	short HCO_70
HCO_65:
	call	print
HCO_70:
;----------------------------------------------------------------------
	
	mov	ah,portout
	cmp	ah,0e0h				; hidden E0 ?
	je	HexCharsOutEnd

	cmp	ah,0e1h				; hidden E1 ?
	je	HexCharsOutEnd

	and	ah,80h				; is it the Break of a Key
	cmp	ah,0
	je	HexCharsOutEnd

	mov	al,0dh				; carriage return

	cmp	fserial_debugging,true		; are we to send out serial port ?
	jne	HCO_75

	call	serialout
	jmp	short HCO_80
HCO_75:
;	call	print

HCO_80:

	mov	al,0ah				; line feed

	cmp	fserial_debugging,true		; are we to send out serial port ?
	jne	HCO_85

	call	serialout
	jmp	short HCO_90

HCO_85:
;	call	print

HCO_90:

;-----------------------------------------------------------------------	

HexCharsOutEnd:	

	pop	ds

	assume DS:NOTHING
	pop	cx
	pop	bx
	pop	si
	pop	dx
	pop	ax	

	ret

HexCharsOut	endp

_TEXT	ends

	end












