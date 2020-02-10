;*
;*	CW : Character Windows
;*
;*	csd_kanj.inc : contains the default routines for kanji
;*
;*

	include	kanji.inc


ifndef	PrepUpdateCsd
;*****************************************************************************
;********** PrepUpdateCsd **********
;*	* CSD entry point (see documentation for interface)
;*	* For KANJI -- erase any dangling 1/2 characters

cProc	PrepUpdateCsd,<FAR, PUBLIC, ATOMIC>, <DS, SI, DI>
    parmB ayLine
    parmB axFirst
    parmB dax
    parmW offFirst
    parmW fRestoreDbcs		;* => restore DBCS 1/2 characters
cBegin	PrepUpdateCsd

	mov	di,OFF_lpwDataCsd		;* Data in data segment
	mov	bx,[di].pinstDrv
	mov	ds,ds:[bx].psPrimInst		;* DS:SI => primary buffer
	mov	si,offFirst

;*	* DS:SI => start character in primary buffer
;*	* SS:BX => INST info
;*	* SS:DI => Driver data

;*	* if not restoring: 1/2 of double wide characters must be erased
	cmp	fRestoreDbcs,0
	jne	done_half_char_check

;*	* if first character is in middle of character, erase previous half
	mov	al,axFirst
	or	al,al
	jz	check_end_character		;* start of line is always safe
	cmp	byte ptr ds:[si+1],0		;* is this a second byte char ?
	jne	check_end_character
;*	* erase previous character (CW will restore this later)
	mov	byte ptr ds:[si-2],' '
check_end_character:
;*	* AX = axFirst, DS:SI => primary buffer position of first char
	xor	bx,bx
	mov	bl,axFirst
	add	bl,dax
	cmp	bl,ss:[bx].axMacInst
	jae	done_half_char_check		;* end of screen is safe
	mov	bl,dax
	dec	bx
	shl	bx,1
	cmp	byte ptr ds:[si+bx+1],0		;* after last a 1/2 char ?
	jne	done_half_char_check
;*	* erase the character at end of line
	mov	byte ptr ds:[si+bx],' '
	mov	al,ds:[si+bx-1]			;* attribute of first half
	mov	byte ptr ds:[si+bx+1],al

done_half_char_check:

cEnd	PrepUpdateCsd
;*****************************************************************************
endif	;* PrepUpdateCsd


ifndef	DoUpdateCsd
;*****************************************************************************
;********** DoUpdateCsd **********
;*	* CSD entry point (see documentation for interface)
;*	* for Kanji -- parse for DBCS boundaries (OAX)
;*	exit: n/a

cProc	DoUpdateCsd,<FAR, PUBLIC, ATOMIC>, <DS, SI, DI>
    parmB ayLine
    parmB axFirst
    parmB dax			;* will never be 0
    parmW offFirst
    parmW fRestoreDbcs		;* => restore DBCS 1/2 characters
    localB attrMaskT
cBegin	DoUpdateCsd

	mov	di,OFF_lpwDataCsd		;* Data in data segment
	mov	bx,[di].pinstDrv
	mov	al,[di].attrMask
	mov	attrMaskT,al
	mov	ds,ds:[bx].psPrimInst		;* DS:SI => primary buffer
	mov	si,offFirst

;*	* DS:SI => start character in primary buffer
;*	* attrMaskT is set up for attribute mask
;*	* SS:BX => INST info
;*	* SS:DI => Driver data

	cmp	ss:[di].fBios,0
	je	done_bios_update		;* no update for non-bios

;*	* BIOS Update
;*	* move to proper position
	mov	dl,axFirst
	mov	dh,ayLine

;*	* copy all characters to screen

	xor	bx,bx				;* BH = 0, page #0 always
	mov	bl,dax
	mov	di,bx				;* count
	mov	cx,1				;* 1 char always
loop_update:
;*	* set cursor & bump column for next
	mov	ah,2
	int	10h				;* SetCursorPosition
	inc	dl				;* next position

	lodsw					;* al = char, ah = attr
	mov	bl,ah
	and	bl,attrMaskT
	mov	ah,9
	int	10h				;* WriteCharAttr

	dec	di
	jnz	loop_update

done_bios_update:

;*	* For the OAX, we parse what we have printed and set the attribute
;*	*  byte to 0 for the second byte of double byte characters.
	cmp	fRestoreDbcs,0
	jne	end_update
	mov	si,offFirst
	xor	cx,cx
	mov	cl,dax				;* # of bytes
loop_parse_dbcs:
	lodsw					;* get char(al) + attribute(ah)
	JmpNotDbc parse_single
;*	* second byte follows
	inc	si				;* skip 2nd char
	mov	byte ptr ds:[si],0		;* clear attrib
	inc	si
	AssertNE cx,1				;* must be enough
	dec	cx
parse_single:
	loop	loop_parse_dbcs

end_update:

cEnd	DoUpdateCsd
;*****************************************************************************
endif	;* DoUpdateCsd


ifndef	DoneUpdateCsd
;*****************************************************************************
;********** DoneUpdateCsd **********
;*	* CSD entry point (see documentation for interface)
;*	* Restore cursor

cProc	DoneUpdateCsd,<FAR, PUBLIC, ATOMIC>,<DI>
cBegin	DoneUpdateCsd

;*	* restore old cursor position
	mov	di,OFF_lpwDataCsd		;* Data in data segment
	cmp	[di].fBios,0
	je	end_refresh			;* no update for non-bios

	mov	dx,[di].posCurs
	mov	ah,2
	int	10h				;* SetCursorPosition
end_refresh:

cEnd	DoneUpdateCsd
;*****************************************************************************
endif	;* DoneUpdateCsd
