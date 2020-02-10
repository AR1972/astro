;*
;*	CW : Character Windows
;*
;*	setjmp.asm : SetJmp/DoJmp for non-pcode fixed segments (CMerge)

	title	setjmp - setjmp/dojmp for non-pcode

	include kernel.inc
	include galloc.inc


;*****************************************************************************

;*	* Native Code ENV structure (see "qsetjmp.h")
ENVN	STRUC
	spEnv		DW	?	;* saved sp (must be even !)
	ipEnv		DW	?	;* ip to jump to in case of DoJmp
	psEnv		DW	?	;* segment to jump to (must be fixed)
	bpEnv		DW	?	;* saved bp
ENVN	ENDS

;*****************************************************************************

sBegin	DATA
    assumes DS,DGROUP

sEnd	DATA


sBegin	BSS
    assumes DS,DGROUP

sEnd	BSS

;*****************************************************************************

externFP ThrowStack

sBegin	KERNEL		;* code must be fixed for SetJmp/DoJmp
    assumes CS,KERNEL
    assumes DS,DGROUP
    assumes SS,DGROUP


;********** SetJmpFixed **********
;* FAR PASCAL ENTRY POINT
;*	entry: penv = near pointer to ENV structure
;*	* fill ENV structure, return 0

cProc	SetJmpFixed,<FAR,PUBLIC,ATOMIC>
cBegin	nogen

	pop	ax
	pop	dx			;* DX:AX = return address
	pop	bx			;* BX => env

;*	* stack is now empty
	mov	ds:[bx].spEnv,sp
	mov	ds:[bx].ipEnv,ax
	mov	ds:[bx].psEnv,dx
	mov	ds:[bx].bpEnv,bp

	push	dx
	push	ax

	xor	ax,ax			;* return 0
	RETF

cEnd	nogen



;********** DoJmpFixed **********
;* FAR PASCAL ENTRY POINT
;*	entry:	penv = pointer to ENV
;*		wRet = word return value (better not be 0)
;*	* Jump to state saved in ENV
;*	exit:	never!

cProc	DoJmpFixed,<FAR,PUBLIC,ATOMIC>
cBegin	nogen

	pop	ax
	pop	ax			;* forget return address

	pop	ax			;* return value
	AssertNE ax,0
	pop	bx			;* penv

;*	* Throw the stuff off the stack
	Save	<bx,ax>
	cCall	ThrowStack,<ds:[bx].bpEnv>

	mov	bp,ds:[bx].bpEnv	;* set BP (stack has been thrown)
	mov	sp,ds:[bx].spEnv	;* set SP
;*	* push jump address on new stack
	push	ds:[bx].psEnv		;* return segment
	push	ds:[bx].ipEnv		;* return offset

;*	* ax = return value
	RETF

cEnd	nogen


sEnd	KERNEL

;*****************************************************************************

	END
