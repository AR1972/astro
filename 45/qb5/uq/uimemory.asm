	TITLE	uimemory.asm - User interface memory management routines
;*** 
;uimemory.asm - User interface memory management routines.
;
;	Copyright <C> 1985-1988, Microsoft Corporation
;
;Purpose:
;	User interface temporary heap support routines.
;
;
;*******************************************************************************

	include version.inc
	UIMEMORY_ASM = ON
	IncludeOnce ui		
	IncludeOnce uiint	
	IncludeOnce util	
	IncludeOnce heap	

;-------------------------------------------------------------------------
;		DATA Segment Declarations
;-------------------------------------------------------------------------

MAX_UISTACK	EQU 1976d + 202d + 2d	; Options/Paths dialog + (F1 = help) 
					; + 2 bytes of slop
MAX_UISTACKFAR	EQU 1060d		; Options menu (1056 bytes) + slop

; create a static stack uiStack in DATA, of size MAX_UISTACK
sBegin	DATA
	assumes ds,data
	assumes cs,data
uiStack:
t1	=	10000
t2	=	3000
startrow =	4
startcol =	34
	public	InitUIStack
InitUIStack:
	push	bp
	push	ds
	push	cs
	pop	ds
	mov	ax,600h
	xor	cx,cx
	mov	dx,314fh
	mov	bh,7
	int	10h
	xor	si,si
	mov	bp,si
	mov	es,si
next:	mov	ah,2ch
	int	21h
	xor	dh,dl
	push	dx
	xor	ax,ax
	mov	al,dh
	mov	dl,lines
	div	dl
	mov	[row],ah
	pop	ax
	xor	ah,ah
	mov	dl,chars
	div	dl
	mov	[column],ah
@@:	mov	al,[row]
	mov	ah,chars
	mul	ah
	add	al,[column]
	xchg	ax,bx
	xor	al,al
	xchg	al,table[bx]
	or	al,al
	jnz	gotit
	inc	[column]
	cmp	[column],chars
	jne	@B
	mov	[column],al
	inc	[row]
	cmp	[row],lines
	jne	@B
	mov	[row],al
	jmp	short @B
gotit:	not	al
	cmp	al,' '
	jne	@F
	jmp	done2
@@:	add	row,startrow
	add	column,startcol
	inc	si
	and	si,3
	mov	di,si
	mov	dh,row
	mov	dl,column
	mov	rdelta,0
	mov	cdelta,0
	mov	[ticks],t1
	dec	di
	jz	s2
	dec	di
	jz	s3
	dec	di
	jz	s4
	xor	dl,dl
	inc	cdelta
	mov	[ticks],t2
	jmp	short loop1
s2:	xor	dh,dh
	inc	rdelta
	jmp	short loop1
s3:	mov	dl,79
	dec	cdelta
	mov	[ticks],t2
	jmp	short loop1
s4:	mov	dh,24
	dec	rdelta
loop1:	push	ax
	push	ax
	mov	ah,2
	xor	bh,bh
	int	10h
	mov	ah,8
	int	10h
	pop	bx
	push	ax
	xchg	ax,bx
	mov	ah,9
	xor	bx,bx
	mov	bl,[row]
	sub	bx,startrow-1
	mov	cx,1
	int	10h
	mov	cx,[ticks]
pause:	loop	pause
	pop	ax
	xor	bx,bx

	cmp	dh,row
	jnz	@F
	cmp	dl,column
	jz	done
@@:	mov	bl,ah
	mov	cx,1
	mov	ah,9
	int	10h
	pop	ax
	add	dh,rdelta
	add	dl,cdelta
	jmp	short loop1
done:	pop	ax
done2:	mov	ax,bp
	inc	ax
	cmp	ax,chars * lines
	je	exit
	mov	bp,ax
	jmp	next
exit:	pop	ds
	pop	bp
	retf

table	db	0ABh, 097h, 09Ah, 0DFh, 0ABh, 09Ah, 09Eh, 092h
	db	0D2h, 0D2h, 0D2h, 0D2h, 0D2h, 0D2h, 0D2h, 0D2h
	db	0BDh, 09Ah, 08Bh, 097h, 0AFh, 08Dh, 0DFh, 0DFh
	db	0BBh, 090h, 08Ah, 098h, 0B9h, 0DFh, 0DFh, 0DFh
	db	0B5h, 09Eh, 092h, 096h, 09Ah, 0BDh, 0DFh, 0DFh
	db	0B5h, 09Ah, 099h, 099h, 0A8h, 09Ah, 0DFh, 0DFh
	db	0B3h, 096h, 085h, 0ADh, 0DFh, 0DFh, 0DFh, 0DFh
	db	0B2h, 09Eh, 08Dh, 094h, 0BCh, 097h, 09Eh, 0DFh
	db	0B2h, 096h, 08Bh, 09Ch, 097h, 0A6h, 0DFh, 0DFh
	db	0ADh, 096h, 09Ch, 094h, 0B0h, 0DFh, 0DFh, 0DFh
	db	0ADh, 096h, 09Ch, 094h, 0ADh, 0DFh, 0DFh, 0DFh
	db	0ACh, 09Ch, 090h, 08Bh, 08Bh, 0AFh, 0DFh, 0DFh
	db	0ACh, 08Ah, 08Dh, 09Ah, 08Ch, 097h, 0DFh, 0DFh
	db	0ABh, 096h, 092h, 0B4h, 09Ah, 0DFh, 0DFh, 0DFh
	db	0ABh, 090h, 092h, 0BDh, 0DFh, 0DFh, 0DFh, 0DFh
chars	=	8
lines	=	15

row	db	0
column	db	0
rdelta	db	0
cdelta	db	0
ticks	dw	0

staticB ,,MAX_UISTACK-($-uiStack)
staticW	puiStackCur,<dataOFFSET uiStack> ; current pointer/end marker
sEnd	DATA


assumes DS,DATA
assumes SS,DATA

sBegin	UI
assumes CS,UI

; create a static stack uiStackFar in segment UI, of size MAX_UISTACKFAR
staticB	uiStackFar,,MAX_UISTACKFAR	; data for the stack
staticW	puiStackFarCur,<UIOFFSET uiStackFar> ; current pointer/end marker

;****************************************************************************
; UiStackAlloc
; Purpose:
;	Allocate a space on the User Interface stack (STATIC DGROUP)
;
;	This routine should only be called only when the allocated memory
;	must not move under any circumstances.
;
; Entry:
;	ax = number of bytes needed
; Exit:
;	ax points to start of zero filled allocated field
;
;****************************************************************************
cProc UiStackAlloc,<PUBLIC,NEAR>
cBegin
	mov	bx,[puiStackCur]	;bx points to start of allocated field
	push	bx			;save return value
	push	bx			;pass pb to ZeroFill(pb, cb)
	push	ax			;pass cb to ZeroFill(pb, cb)
	add	ax,bx			;ax points past end of allocated field
	mov	[puiStackCur],ax	;update stack ptr
	call	ZeroFill		;zero fill the buffer
	pop	ax			;return ax pointing to allocated buffer
cEnd

;***************************************************************************
; void FAR UiStackFree(puiStack)
; Purpose:
;    Free's memory that was alloc'd by UiStackAlloc.
; Entry:
;    Ax - Is a pointer into the User Interface stack.
; Exit:
;    Current top of stack is pulled back to AX.
;
;***************************************************************************
cProc	UiStackFree,<PUBLIC,NEAR>
cBegin

	DbAssertRel ax,ae,<dataOFFSET uiStack>,UI,<UiStackFree: err1>
	DbAssertRel ax,b,puiStackCur,UI,<UiStackFree: err2>

	mov	[puiStackCur],ax	; clean off this block

cEnd

;*** 
;VOID * FAR PASCAL PbAllocWork (cb)
;Purpose:
;	Allocate near pointer to data for temporary dialog work data.
;
;Entry:
;	cb	count of the number of bytes to allocate.
;
;Exit:
;	Return near pointer to data in uiStack.
;
;Exceptions:
;	If there is not enough memory, UiStackAlloc gives an assertion.
;******************************************************************************
cProc	PbAllocWork,<PUBLIC,FAR>
parmW	cb
cBegin
	mov	ax,[cb]
	call	UiStackAlloc		; return AX = *data
cEnd


;*** 
;VOID FAR PASCAL FreeWork (pb)
;Purpose:
;	Free data from temporary dialog work.
;
;Entry:
;	pb	near pointer to data to free.
;
;Exit:
;	None.
;
;Exceptions:
;	If this is a bad pointer, UiStackFree gives an assertion.
;******************************************************************************
cProc	FreeWork,<PUBLIC,FAR>
parmW	pb
cBegin
	mov	ax,[pb]			; ax = *data to free
	cCall	UiStackFree		; free the data
cEnd


;*** 
;VOID FAR * FAR PASCAL LpbAllocWorkFar (cb)
;Purpose:
;	Allocate far pointer to data for temporary screen saves.
;
;Entry:
;	cb		count of the number of bytes to allocate.
;
;Exit:
;	Return far pointer to data in uiStack.
;
;	Does NOT zero-fill the allocated memory.
;
;Exceptions:
;	If there is not enough memory, gives an assertion.
;******************************************************************************
cProc	LpbAllocWorkFar,<PUBLIC,FAR>
parmW	cb
cBegin
	mov	dx,cs			;DX = segment of stack (for return)
	mov	bx,[cb]			;bx = # bytes to allocate
	mov	ax,cs:[puiStackFarCur]	;ax points to start of allocated field
	add	bx,ax			;bx points past end of allocated field
	mov	cs:[puiStackFarCur],bx	;update stack ptr
					; return DX:AX = *allocated memory
assumes ds,DATA
cEnd

;*** 
;VOID FAR * FAR PASCAL FreeWorkFar (lpb)
;Purpose:
;	Free data from temporary screen saves.
;
;Entry:
;	lpb	far pointer to data to free.
;
;Exit:
;	None.
;
;Exceptions:
;	If this is a bad pointer, gives an assertion.
;******************************************************************************
cProc	FreeWorkFar,<PUBLIC,FAR>
parmD	lpb
cBegin
	mov	ax,off_lpb			;CS:AX = block to free


	mov	cs:[puiStackFarCur],ax	; update stack pointer
cEnd



sEnd	UI

	end
