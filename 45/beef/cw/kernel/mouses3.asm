;*
;*	COW : Character Oriented Windows
;*
;*	mouses3.asm : DOS 3 version for mouse save routines

	title	mouses3 - Mouse save stuff

	include kernel.inc


;*****************************************************************************

sBegin	DATA
    assumes DS,DGROUP

externB	fMousePresent				;* TRUE => mouse present

sEnd	DATA

;*****************************************************************************

IFDEF MOUSE_EXTRAS

sBegin	MOUSE			;* discardable MOUSE segment
    assumes CS,MOUSE
    assumes SS,DATA
    assumes DS,DATA


;********** CbSizeMouseState **********
;*	entry:	n/a
;*	* determine size of save buffer for mouse save
;*	exit:	AX = size in bytes (0=> not supported).

cPublic	CbSizeMouseState,<ATOMIC>
cBegin	CbSizeMouseState

	xor	ax,ax				;* assume failure
	cmp	fMousePresent,al
	je	end_cbsize

	xor	bx,bx			; Won't be changed if not supported
	mov	ax,21				;* save mouse state
	int	33H
	mov	ax,bx				;* AX = cb
end_cbsize:	;* ax = cb

cEnd	CbSizeMouseState



;********** SaveMouseState **********
;*	entry:	lpbBuffer = buffer of proper size (from CbSizeMouseState()).
;*	* save mouse state
;*	exit: n/a	

cPublic	SaveMouseState,<ATOMIC>
    parmD lpbBuffer
cBegin	SaveMouseState
	les	dx,lpbBuffer
	mov	ax,22
	int	33H
cEnd	SaveMouseState



;********** RestoreMouseState **********
;*	entry:	lpbBuffer = buffer previously save (by SaveMouseState)
;*	* save mouse state
;*	exit: n/a	

cPublic	RestoreMouseState,<ATOMIC>
    parmD lpbBuffer
cBegin	RestoreMouseState
	les	dx,lpbBuffer
	mov	ax,23
	int	33H
cEnd	RestoreMouseState


sEnd	MOUSE

ENDIF ;MOUSE_EXTRAS

;*****************************************************************************

	END
