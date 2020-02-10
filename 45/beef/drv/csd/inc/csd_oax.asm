;*
;*	CW : Character Windows
;*
;*	csd_oax.asm : contains the OAX routines for direct
;*			video RAM access (primary buffer = video RAM)
;*			(update routines are no-ops)
;*


;*****************************************************************************
;********** PrepUpdateCsd **********
;*	entry:
;*		ayLine = ay of line drawn
;*		axFirst = first character drawn
;*		dax = # of characters drawn
;*		offFirst = offset in primary buffer where started
;*		fRestoreDbcs = bool to tell us to restore double byte
;*			characters or not.
;*	* prepare for screen update
;*	* For KANJI -- erase any dangling 1/2 characters
;*	exit: n/a

cProc	PrepUpdateCsd,<FAR, PUBLIC, ATOMIC>, <DS, SI, DI>
    parmB ayLine
    parmB axFirst
    parmB dax
    parmW offFirst
    parmW fRestoreDbcs
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
	mov	byte ptr ds:[si],' '		; ['88/07/18 KazuK]
check_end_character:
;*	* AX = axFirst, DS:SI => primary buffer position of first char
	xor	dx,dx				; ['88/07/18 KazuK]
	mov	dl,axFirst			; ['88/07/18 KazuK]
	add	dl,dax				; ['88/07/18 KazuK]
	cmp	dl,ss:[bx].axMacInst		; ['88/07/18 KazuK]
	jae	done_half_char_check		;* end of screen is safe
	xor	bx,bx				; ['88/07/18 KazuK]
	mov	bl,dax
;	dec	bx				; ['88/07/18 KazuK]
	shl	bx,1
	cmp	byte ptr ds:[si+bx+1],0		;* after last a 1/2 char ?
	jne	done_half_char_check
;*	* erase the character at end of line
	mov	byte ptr ds:[si+bx],' '
	mov	byte ptr ds:[si+bx-2],' '	; ['88/07/18 KazuK]
	mov	al,ds:[si+bx-1]			;* attribute of first half
	mov	byte ptr ds:[si+bx+1],al

done_half_char_check:

cEnd	PrepUpdateCsd
;*****************************************************************************


;*****************************************************************************
;********** DoUpdateCsd **********
;*	entry:
;*		parameters just like PrepUpdateCsd
;*	* after primary buffer has been updated
;*	* For BIOS version -- send to screen
;*	* for Kanji -- parse for DBCS boundaries (OAX)
;*	exit: n/a

cProc	DoUpdateCsd,<FAR, PUBLIC, ATOMIC>, <DS, SI, DI>
    parmB ayLine
    parmB axFirst
    parmB dax			;* will never be 0
    parmW offFirst
    parmW fRestoreDbcs		;* => restore DBCS 1/2 characters
cBegin	DoUpdateCsd

	mov	di,OFF_lpwDataCsd		;* Data in data segment
	mov	bx,[di].pinstDrv
	mov	ds,ds:[bx].psPrimInst		;* DS:SI => primary buffer
	mov	si,offFirst

;*	* DS:SI => start character in primary buffer
;*	* SS:BX => INST info
;*	* SS:DI => Driver data

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
	
	cmp	cx, 1			; ['88/07/18 KazuK] This step is
	je	parse_single		; ['88/07/18 KazuK]   illegal cording
	
	AssertNE cx,1				;* must be enough
	dec	cx
parse_single:
	loop	loop_parse_dbcs

end_update:

cEnd	DoUpdateCsd

;*****************************************************************************


;*****************************************************************************
;********** DoneUpdateCsd **********
;*	entry: n/a
;*	* Update complete
;*	exit: n/a

cProc	DoneUpdateCsd,<FAR, PUBLIC, ATOMIC>
cBegin	DoneUpdateCsd

cEnd	DoneUpdateCsd

;*****************************************************************************


ifndef SpecialUpdateCsd_NonDefault
;*****************************************************************************
;********** SpecialUpdateCsd **********
;*	* CSD entry point (see documentation for interface)
;*	??? to be defined

cProc	SpecialUpdateCsd,<FAR, PUBLIC, ATOMIC>
cBegin	SpecialUpdateCsd

cEnd	SpecialUpdateCsd

;*****************************************************************************
endif	;* SpecialUpdateCsd_NonDefault


ifndef BltArcCsd_NonDefault
;*****************************************************************************
;********** BltArcCsd **********
;*	* CSD entry point (see documentation for interface)
;*	entry : axSrc, aySrc : upper left of source
;*		axDest, ayDest : upper left of destination
;*		dax, day : shift amount
;*	* Move a rectangle from one portion of the screen to another.
;*	exit : n/a

cProc BltArcCsd,<FAR, PUBLIC, ATOMIC>
    parmB axDest
    parmB ayDest
    parmB dax 
    parmB day
    parmB axSrc
    parmB aySrc
cBegin	BltArcCsd

cEnd	BltArcCsd
;*****************************************************************************
endif	;* BltArcCsd_NonDefault


;*****************************************************************************

