;*
;*	CW : Character Windows
;*
;*	inos.asm : the INOS data structure

	include kernel.inc
	include indrv.inc

;*****************************************************************************

;*****************************************************************************

sBegin	DATA
    assumes CS,DGROUP
    assumes DS,DGROUP


	PUBLIC	inosDrv
inosDrv:
IFDEF DUAL
	DB	cbInosMin DUP (?)		;* inos fixed at runtime
ELSE ;!DUAL
IFDEF DOS5
	include	_inos.asm
ENDIF ;DOS5
ENDIF ;!DUAL

sEnd	DATA

;*****************************************************************************

sBegin	DRV
    assumes CS,DRV
    assumes DS,DRV

;*	* pointer in code space to driver OS/2 jump routines
IFDEF DOS5
globalW pinos, <dataOffset inosDrv>
ELSE
globalW pinos, 0
ENDIF

sEnd	DRV

;*****************************************************************************
;* IOPL hack

IFDEF DOS5
createSeg   IOPL_TEXT,IOPL_TEXT,BYTE,PUBLIC,CODE	;* IOPL segment

sBegin	IOPL_TEXT
    assumes CS,IOPL_TEXT

;********** CwBeginIO **********
;*	entry: called from driver with CPL = 3
;*	exit:  return to driver with CPL = 2 (so it can do IO)
;*	* NOTE: the following is kludgey
;*	* NOTE: between CwBeginIO and CwEndIO the stack is different

cProc	CwBeginIO, <PUBLIC, FAR>
cBegin	nogen ;CwBeginIO -- called through call gate from PL3

	push	bp			;* save BP
;*	* At this point the PL2 stack looks like
;*	SP =>	old BP
;*		IP (caller)
;*		CS (caller) -- PL3
;*		SP (caller)
;*		SS (caller) -- PL3
	mov	bp,sp			;* point to PL2 stack
	and	word ptr ss:[bp+4],NOT 1;* turn return address into PL2
	pop	bp			;* restore BP
	retf				;* return (remain at PL2)

cEnd	nogen ;CwBeginIO -- exit at PL2


;********** CwEndIO **********
;*	entry: in PL2, stack is still set up from original call gate
;*	exit:  return to driver with CPL = 3
;*	* CwBeginIO and CwEndIO must be matched
;*	* NOTE: the following is kludgey
;*	* NOTE: between CwEndIO and CwEndIO the stack is different

cProc	CwEndIO, <PUBLIC, FAR>
cBegin	nogen ;CwEndIO -- enter from PL2 (not call gate)

	push	bp			;* save BP
;*	* At this point the PL2 stack looks like
;*	SP =>	old BP
;*		IP (caller)
;*		CS (caller) -- PL3
;*		SP (caller)
;*		SS (caller) -- PL3
	mov	bp,sp			;* point to PL2 stack
	or	word ptr ss:[bp+4],1	;* turn return address into PL3
	pop	bp			;* restore BP
	retf				;* return (return to PL3)

cEnd	nogen ;CwEndIO -- return through gate to PL3

sEnd	IOPL_TEXT

ENDIF ;DOS5

;*****************************************************************************

	END
