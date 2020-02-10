;*
;*	CW : Character Oriented Windows
;*
;*	kanji.asm : Kanji (DBCS) support routines

.xlist
	include	user.inc

IFDEF KANJI	;* entire file

	include kanji.inc
.list


sBegin USER_CORE
    assumes CS,USER_CORE
    assumes DS,DATA
    assumes SS,DATA

;*****************************************************************************
;* single character checks

;********** FIsDbcsChar(ch) **********
;*	entry: ch = char which may be first byte of double byte character
;*	* Check for double byte character
;*	exit: AX != 0 if LOBYTE(ch) is in DBCS range
;*		else AX == 0 => single byte character

cPublic	FIsDbcsChar, <FAR, PUBLIC, ATOMIC>
    parmW  chTest	;* CHAR
cBegin	FIsDbcsChar
	xor	cx,cx				;* return value (assume 0)
	mov	ax,chTest
	JmpNotDbc not_dbc
	inc	cx				;* return TRUE
not_dbc:
	mov	ax,cx

cEnd	FIsDbcsChar



;*****************************************************************************
;* String length


;********** CchLenDbcs(sz) **********
;*	entry:	sz = string (CHAR *)
;*	* count number of real characters
;*	exit: AX == # of real characters
;*	NOTE: does not check 2nd byte of DBCS characters.

cPublic	CchLenDbcs, <FAR, PUBLIC, ATOMIC>, <SI>
    parmDP sz
cBegin	CchLenDbcs

	mov	si,sz
	xor	cx,cx				;* cch
loop_cchlen:
	lodsb
	or	al,al
	jz	end_cchlen
	inc	cx
	JmpNotDbc loop_cchlen
;*	* is double byte character
	inc	si				;* bump si, don't bump cx
	jmp	short loop_cchlen
end_cchlen:
	mov	ax,cx

cEnd	CchLenDbcs


;*****************************************************************************
;* String navagation

;********** PchNextDbcs(sz) **********
;*	entry:	sz = string (CHAR *)
;*	* navigate to next character
;*	exit: AX => next character
;*	NOTE: does not check 2nd byte of DBCS characters.
;*	NOTE: if positioned on '\0', then will return '\0'

cPublic	PchNextDbcs, <FAR, PUBLIC, ATOMIC>
    parmDP sz
cBegin	PchNextDbcs
	mov	bx,sz
	mov	al,ds:[bx]
	or	al,al
	jz	end_pch_next			;* stay pointing to end
	inc	bx				;* assume single byte
	JmpNotDbc end_pch_next
	inc	bx
end_pch_next:	;* bx = pch to return
	mov	ax,bx
cEnd	PchNextDbcs



;********** PchPrevDbcs(pchChar, szMin) **********
;*	entry:	pchChar => current character position
;*		szMin => start of string (to prevent going back too far)
;*	* navigate to next character
;*	exit: AX => next character, NULL if error (sz not in range of szMin)
;*	NOTE: does not check 2nd byte of DBCS characters.

cPublic	PchPrevDbcs, <FAR, PUBLIC, ATOMIC>
    parmDP pchChar
    parmDP szMin
cBegin	PchPrevDbcs

;*	* Note: we could do this intelligently, but easier to do it from
;*	*  the start of the string
	mov	bx,szMin
	mov	cx,pchChar			;* try to get here
	cmp	bx,cx
	mov	ax,bx
	je	end_pch_prev			;* stay at start
loop_pch_prev:
	mov	al,ds:[bx]
	or	al,al
	jz	err_pch_prev			;* return error if not found
	mov	dx,bx				;* start of char
	inc	bx				;* assume single byte
	JmpNotDbc skip_pch_prev
	inc	bx
skip_pch_prev:
	cmp	bx,cx				;* at the specified character ?
	jl	loop_pch_prev
	mov	ax,dx
end_pch_prev:	;* ax = pch to return

cEnd	PchPrevDbcs

err_pch_prev:
	xor	ax,ax				;* assume error
	jmp	short end_pch_prev







;*****************************************************************************

sEnd	USER_CORE

;*****************************************************************************

ENDIF ;KANJI (entire file)

	END
