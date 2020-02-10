;*
;*	CW : Character Oriented Windows
;*
;*	getchar.asm : get character at a given position.

	include	user.inc
	include	screen.inc
	include	inscr.inc

;----------------------------------------------------------------------------

sBegin	DATA
    assumes DS,DGROUP

externB <instCur>		;* INST type
externB	<axMac>			; Maximum column number

sEnd	DATA

;----------------------------------------------------------------------------

sBegin	SCREEN
    assumes CS,SCREEN
    assumes DS,DGROUP
    assumes SS,DGROUP
    assumes ES,nothing

;******  GetChar  ********
;*	entry : ax, ay
;*	exit : n/a

cProc	GetChar,<FAR,PUBLIC,ATOMIC>
parmB	axGetChar
parmB	ayGetChar

cBegin	GetChar

	mov	es,instCur.psPrimInst
	CalcCoord axGetChar,ayGetChar
	mov	bx,ax
	mov	al,es:[bx]

cEnd	GetChar

sEnd	SCREEN

	END

