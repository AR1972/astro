;*
;*	COW : Character Oriented Windows
;*
;*	prtsc.asm : ^PrtSc stuff.

	include kernel.inc
	include inscr.inc		;* for inst


;----------------------------------------------------------------------------

sBegin	DATA
    assumes DS,DATA

sEnd	DATA

;----------------------------------------------------------------------------

sBegin	INIT
    assumes CS,INIT
    assumes SS,DATA
    assumes DS,DATA

;----------------------------------------------------------------------------
;
; Reroute interrupt 05 (^PrtSc) if necessary.
;
;   entry:  ds:bx -> inst structure.
;
;   preserve BX!

    assumes CS,INIT
    assumes DS,DGROUP
    assumes SS,DGROUP
    assumes ES,NOTHING

cProc	ReroutePrtSc,<FAR,PUBLIC,ATOMIC>
cBegin	nogen	; ReroutePrtSc

	retf

cEnd	nogen	; ReroutePrtSc

sEnd	INIT

;----------------------------------------------------------------------------

	END
