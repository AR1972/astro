
R_CODE	SEGMENT

	extrn	dbgr:near

R_CODE	ENDS

_TEXT	SEGMENT

	extrn	pdbgr:near

_TEXT	ENDS

dump	macro	message, reg
	local	x,y,z,a,b,c,d

	push	ebp
	push	reg
	mov	ebp, esp
	push	eax
	push	edx
	push	ecx
	push	esi
	
	xor	eax, eax
	mov	esi, eax
c:	mov	al, byte ptr cs:[a+esi]
	cmp	al, 0
	je	d
	dbg	al
	inc	esi
	jmp	short c
	
d:	

	mov	eax, [bp]
	mov	cx, 8
z:	xor	edx, edx
	shld	edx, eax, 4
	shl	eax, 4
	cmp	dl, 0ah
	jb	x
	sub	dl, 09h
	add	dl, 040h
	jmp	short y
x:	add	dl, 030h
y:	dbg	dl
	loop	z

	dbg	0ah
	dbg	0dh

	pop	esi
	pop	ecx
	pop	edx
	pop	eax
	pop	ebp
	pop	ebp
	
	jmp	short b
a	DB	message,0
b:

endm

dbg	macro	char

	push	ax
	mov	al, char
	call	dbgr
	pop	ax

endm

pdump	macro	message, reg
	local	x,y,z,a,b,c,d

	push	ebp
	push	reg
	mov	ebp, esp
	push	eax
	push	edx
	push	ecx
	push	esi
	
	xor	eax, eax
	mov	esi, eax
c:	mov	al, byte ptr cs:[a+esi]
	cmp	al, 0
	je	d
	pdbg	al
	inc	esi
	jmp	short c
	
d:	

	mov	eax, [bp]
	mov	cx, 8
z:	xor	edx, edx
	shld	edx, eax, 4
	shl	eax, 4
	cmp	dl, 0ah
	jb	x
	sub	dl, 09h
	add	dl, 040h
	jmp	short y
x:	add	dl, 030h
y:	pdbg	dl
	loop	z

	pdbg	0ah
	pdbg	0dh

	mov	ecx, 0ffffh
	loop	$	
	mov	ecx, 0ffffh
	loop	$	
	mov	ecx, 0ffffh
	loop	$	


	pop	esi
	pop	ecx
	pop	edx
	pop	eax
	pop	ebp
	pop	ebp
	
	jmp	short b
a	DB	message,0
b:

endm

pdbg	macro	char

	push	ax
	mov	al, char
	call	pdbgr
	pop	ax

endm



