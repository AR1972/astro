;*
;*	CW : Character Windows
;*
;*	sstack.inc : Stack save/restore macros


;********* SetStackToDds **********
;*	entry:	SS:SP/BP => either main or linked stack
;*		bpSaveThunk must be 0
;*		parm fMaySwap => swap may occur => make real frame
;*	* if linked stack then move stack to DDS
;*	exit:	ssSaveThunk, spSaveThunk and bpSaveThunk set if stack changed
;*		save ax and bx in axSaveThunk and bxSaveThunk

SetStackToDds	MACRO	fMaySwap
	LOCAL	ok_stack
	AssertEQ bpSaveThunk,0
	mov	axSaveThunk,ax
	mov	bxSaveThunk,bx
	mov	ax,SEG DGROUP			;* DDS
	mov	bx,ss
	cmp	ax,bx
	je	ok_stack
;*	* make frame if may swap
IF fMaySwap
	inc	bp
	push	bp
	mov	bp,sp
ENDIF
;*	* save stack etc
	mov	spSaveThunk,sp
	mov	ssSaveThunk,ss
	mov	bpSaveThunk,bp
;*	* switch stacks (AX => DDS)
	AssertEQ ss:[ssOldStack],ax
	mov	bp,ss:[bpOldStack]
	cli
	mov	ss,ax
	mov	sp,ss:[pStackMin]
	sti

ok_stack:
ENDM ;SetStackToDds


;********** RestoreStackFromDds **********
;*	entry:	state as setup from SetStackToDds
;*		parm fMaySwap => swap may occur => remove real frame
;*	* restore stack if necessary
;*	exit:	SS:SP/BP => real stack, ax and bx restored from ax/bxSaveThunk
;*	* does not trash any other registers

RestoreStackFromDds	MACRO fMaySwap
	LOCAL	ok_stack

	cmp	bpSaveThunk,0
	je	ok_stack
	xor	bp,bp
	xchg	bp,bpSaveThunk
	cli
	mov	ss,ssSaveThunk
	mov	sp,spSaveThunk
	sti

;*	* restore frame if may have swapped
IF fMaySwap
	pop	bp
	dec	bp
ENDIF

ok_stack:
	mov	ax,axSaveThunk
	mov	bx,bxSaveThunk
ENDM ;RestoreStackFromDds
