;*
;*	COW : Character Oriented Windows
;*
;*	sdmasm.asm : SDM specific hand coded stuff

	title	sdmasm - low level utilities for COW SDM

.xlist
	include	user.inc
.list


sBegin	SDM
    assumes CS,SDM
    assumes DS,DATA
    assumes SS,DATA


; FillBuf
; pch: character string
; chFill: fill character
; cch: number of characters

cProc	FillBuf,<NEAR,PUBLIC,ATOMIC>,<DI>
    parmDP pch
    parmB  chFill
    parmW  cch

cBegin	FillBuf
;;;	cld			-- assumed
	push	ds
	pop	es		;* size optimize
	mov	di,pch		; set up for fill
	mov	al,chFill
	mov	cx,cch
	rep	stosb
cEnd	FillBuf

sEnd	SDM

;*****************************************************************************

	END

