;*
;*	COW : Character Oriented Windows
;*
;*	util.asm : user utilities (DOS 3 & 5)

	title	util - low level utilities for COW USER

.xlist
	include	user.inc
.list
	include insyd.inc


sBegin	DATA

externB <inyj>				;* SYD jump vector

sEnd	DATA


sBegin USER_CORE
    assumes CS,USER_CORE
    assumes DS,DATA
    assumes SS,DATA



;********** BltBackwards **********
;*	entry : DS:SI => source, ES:DI => dest, CX = size
;*	* blt backwards

cProc	BltBackwards, <NEAR, ATOMIC>
cBegin	BltBackwards

	mov	ax,cx
	sub	ax,2			;* end less 1 word
	add	si,ax
	add	di,ax			;* past end of each + 1 word

	std
	shr	cx,1			;* save lower bit in CY flag
	rep	movsw			;* move words
	cld				;* restore D
	jnc	end_blt_back		;* all done
	inc	si
	inc	di
	movsb				;* and last byte
end_blt_back:

cEnd	BltBackwards



; bltbyte
; pchSource: source string
; pchDest: destination string
; cb: number of bytes to copy
; copies both backwards and forwards
; returns pointer to end of destination string.

cPrivate bltbyte,<ATOMIC>,<SI, DI>
    parmDP  pchSource
    parmDP  pchDest
    parmW   cb
cBegin	bltbyte

	mov	ax,ds
	mov	es,ax
	mov	cx,cb
	mov	si,pchSource
	mov	di,pchDest

	cmp	si,di
	jb	blt_backwards	; source less than destination

	shr	cx,1			;* save lower bit in CY flag
	rep	movsw			;* move words
	rcl	cx,1			;* restore last bit
	rep	movsb			;* optional 1 byte clean up

blt_end:
	mov	ax,pchDest
	add	ax,cb

cEnd	bltbyte

blt_backwards:
	cCall	BltBackwards
	jmp	short blt_end



;***
;
;  bltbytex(lpSrc, lpDst, cb)
;
;  Move cb bytes from Src to Dst
;
;  Inputs:
;		far pointer source.
;		far pointer destination.
;		unsigned integer for number of bytes to move.
;
;  Outputs:	none.
;
;****

cPrivate bltbytex,<ATOMIC>,<SI, DI>
    parmD lpSrc
    parmD lpDst
    parmW cb
cBegin	bltbytex

	push	ds			;* Cmacros BUG
	lds	si,lpSrc
	les	di,lpDst
	mov	cx,cb

	mov	ax,es
	mov	bx,ds
	cmp	ax,bx
	jne	bltx_forward		;* different segments => no overlap

	cmp	si,di
	jb	bltx_backwards	; source less than destination

bltx_forward:
	shr	cx,1			;* save lower bit in CY flag
	rep	movsw			;* move words
	rcl	cx,1			;* restore last bit
	rep	movsb			;* optional 1 byte clean up

bltx_end:
	pop	ds

cEnd	bltbytex

bltx_backwards:
	cCall	BltBackwards
	jmp	short bltx_end


;********** stringcpy **********
;*	entry : szDest = destination buffer (cchMac + 1 bytes long)
;*		szSource = source string
;*		cchMac = max # of characters to copy
;*	* Copy at most "cchMac" characters, always zero terminate destination
;*	exit : n/a

cPrivate stringcpy,<ATOMIC>,<SI,DI>
    parmDP  szDest
    parmDP  szSource
    parmW   cchMac

cBegin	stringcpy

	mov	ax,ds
	mov	es,ax
	mov	si,szSource
	mov	di,szDest
	mov	cx,cchMac
	jcxz	stuff_zero

ncopy_loop:
	lodsb
	stosb
	or	al,al
	jz	end_ncopy
	loop	ncopy_loop
	dec	di		;* back to last character copied
stuff_zero:
	xor	al,al		;* must be room for 1 more
	stosb
end_ncopy:

cEnd	stringcpy



;***
;
;  fstrcpy( fpDst, fpSrc )
;
;  Move null terminated string from Src to Dst
;
;  Inputs:
;		far pointer destination.
;		far pointer source.
;
;  Outputs:	none.
;
;****

cPrivate fstrcpy,<ATOMIC>,<SI, DI>
    parmD fpDst
    parmD fpSrc
cBegin	fstrcpy

	push	ds			;* Cmacros BUG
	les	di,fpDst
	lds	si,fpSrc
fstrcpy1:
	lodsb
	stosb
	or	al,al
	jnz	fstrcpy1
	pop	ds

cEnd	fstrcpy

;***
;
;  fstrcmp( string1, string2 )
;
;  Compare two null terminated strings
;
;  Inputs:
;		far pointer string1.
;		far pointer string2.
;
;  Outputs:	ax < 0 if string1 < string2
;				ax = 0 if string1 = string2
;				ax > 0 if string1 > string2
;
;****

cPrivate fstrcmp,<ATOMIC>,<SI, DI>
    parmD string1
    parmD string2
cBegin	fstrcmp

	push	ds			;* Cmacros BUG
	les	di,fpDst
	lds	si,string1
	les	di,string2

; Find the length of string2 (to limit the cmpsb)
	mov	bx,di
	xor	ax,ax
	mov	cx,0ffffH
	repne scasb
	inc	cx
	neg	cx
	mov	di,bx

; Compare the strings.
	repe cmpsb
	xor	cx,cx
	mov	al,[si-1]
	cmp	al,es:[di-1]
	je	equal
	jb	below
above:
	dec	cx		; cx <-- FFFF
	dec	cx		; cx <-- FFFE
below:
	not	cx		; if cx == 0000 then cx <-- FFFF (-1)
				; if cx == FFFE then cx <-- 0001
equal:
	mov	ax, cx
	pop	ds

cEnd	fstrcmp



;***
;
;  fstrlen( string1 )
;
;  return length of string
;
;  Inputs:
;		far pointer string1.
;
;  Outputs:	ax - length of string.
;
;****

cPrivate fstrlen,<ATOMIC>,<DI>
    parmD string1
cBegin	fstrlen

	les	di,string1
	xor	ax,ax
	mov	cx,0ffffH
	repne scasb
	mov	ax,cx
	inc	ax
	not	ax

cEnd	fstrlen


sEnd	USER_CORE

;*****************************************************************************

sBegin	USER_SCROLL
    assumes CS,USER_SCROLL
    assumes DS,DATA
    assumes SS,DATA

; Translates a point in the first range range to a point in the second range.
; Rounds up or down.
;* see listbox.c for usage

cProc	TranslatePosition,<NEAR,PUBLIC,ATOMIC>
    parmW wRange1Position
    parmW wRange1Min		
    parmW wRange1Max
    parmW wRange2Min
    parmW wRange2Max
    parmW fRoundUp
cBegin	TranslatePosition

	mov	ax,wRange1Position
	sub	ax,wRange1Min
	mov	bx,wRange2Max
	sub	bx,wRange2Min
	mul	bx
	cmp	fRoundUp,0
	jz	noroundup
	mov	bx,wRange1Max
	sub	bx,wRange1Min
	dec	bx
	add	ax,bx
	adc	dx,0

noroundup:
	mov	cx,wRange1Max
	sub	cx,wRange1Min
	jcxz	dont_divide		;* don't really divide (/1)
	div	cx
dont_divide:
	add	ax,wRange2Min

cEnd	TranslatePosition

sEnd	USER_SCROLL

;*****************************************************************************
;* Installable SYD driver routines

sBegin USER_CORE
    assumes CS,USER_CORE
    assumes DS,DATA
    assumes SS,DATA

;********** DoSound **********
;*	entry : req = sound request
;*		0 => beep
;*		1 => click
;*	* Beep
;*	exit : n/a

labelFP	<PUBLIC, DoSound>
	jmp	inyj.lpfnDoSoundSydInyj



;********** ClockTicks **********
;*	entry : n/a
;*	* Get the clock time
;*	exit : DX:AX = long time (signed) (1/18th of a second increments)
;*	!! DO NOT PROFILE : called by interrupt handlers !!!

labelFP	<PUBLIC, ClockTicks>
	jmp	inyj.lpfnLGetTimeSydInyj

sEnd	USER_CORE

;*****************************************************************************

	END
