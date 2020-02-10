;*
;*	CW : Character Windows
;*
;*	csd_vram.asm : contains the default routines for direct
;*			video RAM access (primary buffer = video RAM)
;*			(update routines are no-ops)
;*


ifndef PrepUpdateCsd_NonDefault
;*****************************************************************************
;********** PrepUpdateCsd **********
;*	* CSD entry point (see documentation for interface)

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

cProc	DoUpdateCsd,<FAR, PUBLIC, ATOMIC>
    parmB ayLine
    parmB axFirst
    parmB dax
    parmW offFirst
    parmW fRestoreDbcs
cBegin	DoUpdateCsd

cEnd	DoUpdateCsd

;*****************************************************************************
endif	;* DoUpdateCsd_NonDefault


ifndef DoneUpdateCsd_NonDefault
;*****************************************************************************
;********** DoneUpdateCsd **********
;*	* CSD entry point (see documentation for interface)

cProc	DoneUpdateCsd,<FAR, PUBLIC, ATOMIC>
cBegin	DoneUpdateCsd

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


;*****************************************************************************
