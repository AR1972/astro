;*
;*	COW : Character Oriented Windows
;*
;*	mousedat.asm : data for mouse input control

	include kernel.inc


;----------------------------------------------------------------------------

sBegin	DATA
    assumes DS,DGROUP

ayNil	EQU	253				;* see user\screen.inc
globalB	ayMouse,ayNil

sEnd	DATA

;----------------------------------------------------------------------------

sBegin	BSS
    assumes DS,DGROUP

;* PUBLIC
globalB	fMousePresent,0				;* TRUE => mouse present

;* PRIVATE
globalB	axMouse,0
globalW	sstMouse,0

globalB	fMouseOn,0				;* TRUE => mouse on

IFDEF MOUSE_SWAP
globalB fSwapButton,0				;* TRUE => reverse buttons
ENDIF ;MOUSE_SWAP

sEnd	BSS

;----------------------------------------------------------------------------

	END
