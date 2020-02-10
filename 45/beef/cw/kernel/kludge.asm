include cmacros.inc

sBegin	DATA
    assumes DS,DGROUP

globalW	fFillBlock,-1
globalW	wFillBlock,0CCCCH
globalW	fNew,0

sEnd


	PUBLIC	kludge_sn, kludge_q
kludge_sn	EQU	0

sBegin	code

labelFP	kludge_q

cProc	SbMgrError, <FAR, PUBLIC>
cBegin	SbMgrError

	int	3

cEnd	SbMgrError

;********** StartCowMeas **********
;*	* Kludge for COW measurements - StartNMeas if crefCow++ == 0
cProc	StartCowMeas,<FAR, PUBLIC>
cBegin	nogen ;StartCowMeas

	RET

cEnd	nogen;StartCowMeas

cProc	StartNMeas,<FAR, PUBLIC>
cBegin	nogen ;StartCowMeas

	RET

cEnd	nogen;StartCowMeas


;********** StopCowMeas **********
;*	* Kludge for COW measurements - StopNMeas if --crefCow == 0

cProc	StopCowMeas,<FAR, PUBLIC>
cBegin	nogen ;StopCowMeas

	RET

cEnd	nogen;StopCowMeas


cProc	StopNMeas,<FAR, PUBLIC>
cBegin	nogen ;StopCowMeas

	RET

cEnd	nogen;StopCowMeas


;********** LTrickCall **********
;*	entry : n/a
;*	* Kludge for making non-continuous measurements look continuous
;*	* set the fTrickCall flag
;*	* Sets next old call address to last new call address.
;*	exit : DX:AX = DWORD value that should be passed to TrickReturn
;*		(This is the sn:bpc that should be used for the return).

cProc	LTrickCall,<FAR, PUBLIC, ATOMIC>
cBegin	LTrickCall

cEnd	LTrickCall



;********** TrickReturn **********
;*	entry : lTrick = trick value (sn:bpc)
;*	* Kludge for making non-continuous measurements look continuous
;*	* set the fTrickReturn flag
;*	* Sets next new return address to value passed.
;*	exit : n/a

cProc	TrickReturn,<FAR, PUBLIC, ATOMIC>
    parmW snTrick
    parmW bpcTrick
cBegin	TrickReturn

cEnd	TrickReturn

sEnd
	end

