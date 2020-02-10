;	TITLE	FCOMMON - Common utility routines for MSHERC
;***
;FCOMMON
;
;	Copyright <C> 1987, 1988, Microsoft Corporation
;
;Purpose:
;	Common utility routines for MSHERC.
;
;******************************************************************************

	include	hgcdefs.inc

code            segment para public 'code'
                assume  cs:code,ds:code

Public	Pause			;Execute a Pause...
Public	MapGraphXYToVideoOffset	;Map graphics coordinates to video offset

Extrn	YTable:word		;Y Offset Table area

;------Pause procedure - uses vsync pulse for timer--------
;Input:	cx = delay count
;
Pause	proc	near
	mov	dx,DS_Port
vson:
	in	al,dx
	or	al,al
	js	vsoff
	jmp	vson
vsoff:
	in	al,dx
	or	al,al
	jns	vsync
	jmp	vsoff
vsync:
	loop	vson
	ret
Pause	endp

;=================================================================
;			MapGraphXYToVideoOffset
;
;	Converts x,y coordinates to graphics buffer offset and
;	bit mask.
;
;	Input:	cx = x coordinate
;		dx = y coordinate
;
;	Output:	cx = graphics buffer offset
;		dx = bit mask
;
;=================================================================
MapGraphXYToVideoOffset	proc	near
	push	si

;fetch the y offset from the table
	mov	si,dx
	shl	si,1
	mov	dx,YTable[si]

;calculate x offset
	push	cx

	shr	cx,1
	shr	cx,1
	shr	cx,1	;x/8
	add	cx,dx	;cx = offset into graphics buffer

;calculate bit mask: 7 - (X MOD 8)
	pop	dx	;Retrieve X coordinate
	and	dx,7	;X MOD 8
	neg	dx	;- (X MOD 8)
	add	dx,7	;7 - (X MOD 8) = bit position

;finished
	pop	si
	Ret
MapGraphXYToVideoOffset	endp

code     ends
         end
