;	TITLE	CURSOR - page and cursor control for MSHERC
;***
;CURSOR
;
;	Copyright <C> 1987, 1988, Microsoft Corporation
;
;Purpose:
;	Page and cursor control for MSHERC.
;
;Revision History:
;
;******************************************************************************

	include	hgcdefs.inc

code     segment para public 'code'
         assume  cs:code,ds:code

Public	SetActivePage
Public	SetCursor
Public	ReadCursor
Public	GSetCursorType

Extrn	DMC_Save:byte
Extrn	ConfigMode:byte			;[1] FULL or HALF mode

;-------------------------------------------------------------------------------
; 5  SELECTACTIVEPAGE
;	 AL => Page Number
;-------------------------------------------------------------------------------
SetActivePage	proc	near
	mov	ah,DMC_Save		;get saved DMC_Port value
	and	ah,not GraphicsPage1	;clear page bit
	xor	cx,cx			;Assume Graphics Page 0
	and	al,1			;page 0 or 1?
	jz	SetGSeg 		;go if page 0
	dec	al			;[1] set al = 0 in case not FULL mode
	cmp	[ConfigMode],FULL	;[1] make sure FULL mode
	jne	SetGSeg			;[1] brif not, use page 0
	inc	al			;[1] reset al = 1 for page 1
	mov	cx,GraphBufferSize	;no, use page 1
	or	ah,GraphicsPage1	;set page 1
SetGSeg:
	mov	DMC_Save,ah		;Save in Mode Record
	mov	byte ptr es:BIOSPAGE,al ;set page number BIOS variable
	mov	word ptr es:BIOSSTART,cx;set buffer offset BIOS variable

	push	ax
	mov	dx,DMC_Port		;Use the Display Mode Control Port
	mov	al,ah			;output new DMC_Port value
	out	dx,al			;Set DMC Port and turn on screen
	pop	ax

	xor	ah,ah			;set proper cursor loc for this page
	shl	ax,1
	mov	di,ax
	mov	bx,es:BIOSCURS[di]
	ret
SetActivePage	endp

;-------------------------------------------------------------------------------
; 2  SET CURSOR POSITION
;	 BH => Page Number
;	 DH,DL => Row, Column, (0,0) is upper left
;-------------------------------------------------------------------------------
SetCursor	proc	near
	xor	ax,ax
	mov	al,bh
	and	ax,1
	shl	ax,1
	mov	di,ax
	mov	es:BIOSCURS[di],dx
	Ret	;Finished Set Cursor Procedure
SetCursor	endp

;-------------------------------------------------------------------------------
; 3  READ CURSOR POSITION
;	 BH => Page Number
;	 DH,DL <= Row, Column, (0,0) is upper left
;	 CX <= BIOS Cursor Type (N/A for graphics mode)
;-------------------------------------------------------------------------------
ReadCursor	proc	near
;------Put the current cursor position on stack------
	xor	ax,ax
	mov	al,bh
	shl	ax,1
	mov	di,ax
	mov	dx,es:BIOSCURS[di]	;Fetch Cursor Position
	mov	FunData.FunDX,dx	;Save in Stack Parameter Area
	mov	cx,es:BIOSCURSMODE	;Fetch Cursor Start/Stop Scans
	mov	FunData.FunCX,cx	;Save in Stack Parameter Area
	Ret	;Finished Read Cursor Procedure
ReadCursor	endp

;-------------------------------------------------------------------------------
; 1  SET CURSOR TYPE
;	 CX => Cursor type (saved, but no action taken)
;-------------------------------------------------------------------------------
GSetCursorType	proc	near
	mov	word ptr es:BIOSCURSMODE,cx
	Ret	;Finished Set Cursor Type Procedure
GSetCursorType	endp

code     ends
	 end
