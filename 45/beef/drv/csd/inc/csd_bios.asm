;*
;*	CW : Character Windows
;*
;*	csd_bios.asm : contains the default routines for BIOS video I/O
;*


ifndef PrepUpdateCsd_NonDefault
;*****************************************************************************
;********** PrepUpdateCsd **********
;*	* CSD entry point (see documentation for interface)
;*	not used for BIOS version

cProc	PrepUpdateCsd,<FAR, PUBLIC, ATOMIC>
    parmB ayLine
    parmB axFirst
    parmB dax
    parmW offFirst
    parmW fRestoreDbcs
cBegin	PrepUpdateCsd
cEnd	PrepUpdateCsd

;*****************************************************************************
endif	;* PrepUpdateCsd_NonDefault


ifndef DoUpdateCsd_NonDefault
;*****************************************************************************
;********** DoUpdateCsd **********
;*	* CSD entry point (see documentation for interface)
;*	* BIOS version: update the screen

cProc	DoUpdateCsd,<FAR, PUBLIC, ATOMIC>, <DS, SI, DI>
    parmB ayLine
    parmB axFirst
    parmB dax
    parmW offFirst
    parmW fRestoreDbcs
cBegin	DoUpdateCsd

	mov	di,OFF_lpwDataCsd		;* Data in data segment
	mov	bx,[di].pinstDrv
	mov	ds,ds:[bx].psPrimInst		;* DS:SI => primary buffer
	mov	si,offFirst

;*	* DS:SI => start character in primary buffer
;*	* SS:BX => INST info
;*	* SS:DI => Driver data

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
	mov	ah,9
	int	10h				;* WriteCharAttr

	dec	di
	jnz	loop_update

cEnd	DoUpdateCsd
;*****************************************************************************
endif	;* DoUpdateCsd_NonDefault


ifndef DoneUpdateCsd_NonDefault
;*****************************************************************************
;********** DoneUpdateCsd **********
;*	* CSD entry point (see documentation for interface)
;*	* for BIOS version: restore cursor

cProc	DoneUpdateCsd,<FAR, PUBLIC, ATOMIC>,<DI>
cBegin	DoneUpdateCsd

;*	* restore old cursor position
	mov	di,OFF_lpwDataCsd		;* Data in data segment

	mov	dx,[di].posCurs
	mov	ah,2
	int	10h				;* SetCursorPosition

cEnd	DoneUpdateCsd
;*****************************************************************************
endif	;* DoneUpdateCsd_NonDefault



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

