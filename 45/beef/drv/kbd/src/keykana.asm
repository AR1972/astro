IFDEF KANJI
;*	* Translation table for OAX
;*	* maps 'A' -> 'Z' into Kanas on same key
rgchKana:
	DB	0c1H, 0baH, 0bfH, 0bcH, 0b2H, 0caH, 0b7H, 0b8H, 0c6H, 0cfH
	DB	0c9H, 0d8H, 0d3H, 0d0H, 0d7H, 0beH, 0c0H, 0bdH, 0c4H, 0b6H
	DB	0c5H, 0cbH, 0c3H, 0bbH, 0ddH, 0c2H

ENDIF ;KANJI


;********** ChAlternateKeytopKbd **********
;*	entry:	chIn = character to find alias to
;*	* find alias to character (KANA<->ROMAN) for keys on keyboard
;*	exit:	AX = alias character (or 0)

cProc	ChAlternateKeytopKbd, <FAR, PUBLIC, ATOMIC>
    parmB  chIn
cBegin	ChAlternateKeytopKbd

IFDEF KANJI
	push	di
	mov	di,drvOffset rgchKana
	mov	al,chIn
	cmp	al,0C0H			;* Kana or Roman ?
	jb	roman_to_kana
;*	* kana to roman (look up in table)
	push	cs
	pop	es
	mov	cx,26
	repnz scasb
	jz	bad_convert
;*	* DI points past the character that matched
	sub	di,drvOffset rgchKana - 1 - 'A'	;* map to 'A' -> 'Z'
	mov	ax,di			;* AL = ROMAN character
	jmp	short end_convert

roman_to_kana:
;*	* convert from ROMAN to KANA
	sub	al,'A'
	jb	bad_convert
	cmp	al,26
	jae	bad_convert
	xor	ah,ah
	mov	bx,ax
	mov	al,cs:[di+bx]		;* AL = KANA character
	jmp	short end_convert

bad_convert:
	xor	ax,ax			;* return 0
end_convert:
	pop	di
ELSE
	xor	ax,ax				;* non-kanji
ENDIF ;!KANJI

cEnd	ChAlternateKeytopKbd

