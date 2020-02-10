page	49,132
	TITLE	exstmisc.asm - misc. statement executors
;***
;exstmisc.asm - statement executors for QBI
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;
;****************************************************************************

	.xlist
	include		version.inc
EXSTMISC_ASM = ON
	IncludeOnce	architec
	IncludeOnce	context
	IncludeOnce	executor
	IncludeOnce	extort
	IncludeOnce	exint
	IncludeOnce	opstmt
	IncludeOnce	opmin
	IncludeOnce	opcontrl
	IncludeOnce	opintrsc
	IncludeOnce	opaftqb4
	IncludeOnce	qbimsgs
	IncludeOnce	rtinterp
	.list

sBegin	DATA
sEnd	DATA

assumes es, NOTHING
assumes ss, DATA

	EXTRN	B$SERR:FAR

sBegin	CODE
assumes cs, CODE

	    extrn   I4ToU2:near



;==============================================================================
;	Error-related Statements
;==============================================================================
MakeExe	exStError,opStError
	;note that error code is on stack here - - -
	CALLRT	B$SERR,DispMov		;Never returns. DispMov variant used
					;just so non-RELEASE case won't have
					;non-RELEASE static data screwed up.
					;Note that calling via ExToRt ensures
					;that grs.GRS_oTxCur is set up for
					;B$IONERR

;==============================================================================
;	Sound-related Statements
;==============================================================================
MakeExe exStBeep0,opStBeep
	    CALLRT  B$BEEP,Disp



MakeExe	exStPlay,opStPlay
	CALLRT	B$SPLY,Disp

MakeExe	exStSound,opStSound
;Added with [11]
	sub	sp,4			;Make room for argument
	mov	bx,sp
	fstp	dword ptr DGROUP:[bx]	;Copy to local stack
	fwait
;End of [11]
	CALLRT	B$SOND,Disp

;==============================================================================
;	Heap-related Statements
;==============================================================================

MakeExe	exStDefSeg0,opStDefSeg0
	CALLRT	B$DSG0,Disp

MakeExe	exStDefSeg1,opStDefSeg1
	call	I4toU2				;coerce I4 addr on stack to a U2
	CALLRT	B$DSEG,Disp

MakeExe	exStPoke,opStPoke
	pop	bx				;preserve value to be poked
	call	I4toU2				;coerce I4 addr on stack to a U2
	push	bx				;(I4toU2 preserves bx)
	CALLRT	B$POKE,Disp


;==============================================================================
;	Math-related Statements
;==============================================================================
MakeExe	exStRandomize0,opStRandomize0
	CALLRT	B$RNZ0,Disp

MakeExe	exStRandomize1,opStRandomize1
;Added with [11]
	sub	sp,8			;Make room for argument
	mov	bx,sp
	fstp	qword ptr DGROUP:[bx]	;Copy to local stack
	fwait
;End of [11]
	CALLRT	B$RNZP,Disp

;==============================================================================
;	READ/DATA Support
;==============================================================================
;NOTE: The actual READ executors are exactly the same as the INPUT executors,
;NOTE: so they share code in exio.asm

;NOTE: The otxFirst field is a pointer to the link field in the first DATA
;NOTE: statement in the module. The otxCur field, however, is a pointer to
;NOTE: the exStData pcode - - - this inconsistency is required by the scanner,
;NOTE: which treats otxFirst as an otx head-of-chain, and otxCur as a 'normal'
;NOTE: otx - - - each of which is updated differently for Edit & CONTinue.
;NOTE: The oLineCur initialization value is 6 bytes to account for the fact
;NOTE: that otxCur points to the exStData pcode.

MakeExe	exStRestore0,opStRestore0
	xor	cx,cx				;remember this is exStRestore0
	jmp	short Restore_Common
MakeExe	exStRestore1,opStRestore1
	DbAssertRel ax,nz,0,CODE,<exStRestore1 finds AX == 0>
	xchg	ax,cx				;cx != 0 indicates exStRestore1
	LODSWTX					;ax = oTx. We want the next READ
						;  to use the first piece of 
						;  data in the first DATA stmt
						;  found subsequent to this oTx
Restore_Common:
	call	GetDataSeg			;set up bx and es (preserves ax)
	mov	di,PTRRS[bx.MRS_data_otxFirst]	; oTx of link field of first DATA
						;  statement
	mov	es,dx				
	jcxz	Restore_Common1			;brif exStRestore0

Walk_Data_Stmts:
	cmp	ax,di
	jbe	Restore_Common1			;brif di contains oTx we want to
						;  restore to
	DbAssertRel di,nz,UNDEFINED,CODE,<exStRestore1 error2>
		;if di == UNDEFINED, the above branch should have been taken
	mov	di,es:[di]			;move to next DATA stmt, if any
	jmp	short Walk_Data_Stmts

Restore_Common1:				;di = oTx to the link field
						;  of a DATA statement
	inc	di
	.errnz	UNDEFINED - 0FFFFH
	jz	Restore_Common2			;brif no DATA statements

	sub	di,5				;di = oTx to exStData
	GETRS_SEG  es				
	mov	PTRRS[bx.MRS_data_otxCur],di	
	mov	PTRRS[bx.MRS_data_oLineCur],6	; offset into current DATA stmt
Restore_Common2:
	jmp	DispMov				;NOTE: just using the 'Mov'
						;NOTE: varient of 'Disp' so that
						;NOTE: DI & ES will get reloaded


;Helper routine for B$IRDPTR and B$IRDPTRUPD, below - - - returns
;  the segment part of the address of mrsCur in ES. Note that DATA statements
;  are NEVER found at proc level, because QBI moves all DATA statements to
;  module level at text insertion.
;Output:
;	dx = segment of current mrs table
;	bx = pMrsCur
;	es = segment of the global Rs table
;Preserves: ax, cx
GetDataSeg  PROC NEAR				
	mov	bx,[grs.GRS_oMrsCur]		
	RS_BASE add,bx				
	GETRS_SEG  es				
	GETSEG	dx,PTRRS[bx.MRS_txd.TXD_bdlText_seg] ;[4][2]
	ret
GetDataSeg  ENDP				

;***
;B$IRDPTR - call back to give far ptr to runtime for next data item to read
;Input:
;	none
;Output: 
;	far ptr to data item in DS:SI
;Destroys:
;	ES
;Exceptions:
;	Runtime error if no more data to be read
;*******************************************************************************
PUBLIC B$IRDPTR
B$IRDPTR	PROC FAR
	mov	bx,[grs.GRS_oMrsCur]		
	RS_BASE add,bx				
	GETRS_SEG es				; es:bx points to mrsCur
	mov	si,PTRRS[bx.MRS_data_otxCur]	
	inc	si
	.errnz	UNDEFINED - 0FFFFH
	jz	Out_Of_Data

	dec	si
	add	si,PTRRS[bx.MRS_data_oLineCur]	; ax now oTx of next item to read
	call	GetDataSeg			; returns seg in dx
	push	dx				
	pop	ds
	RET

Out_Of_Data:
	mov	al,ER_OD			;Out of Data error
	call	RtErrorNoSi			;never returns - runtime error
B$IRDPTR	ENDP

;***
;B$IRDPTRUPD - call back to allow us to update data ptr after a READ occurs
;Input:
;	AL = 0 if last item was just read in current DATA statement, non-zero
;		otherwise
;	BX = Number of bytes 'eaten' by the latest READ.
;Output:
;	current mrs (in the global Rs table) updated with correct data pointer
;Destroys:
;	ES
;*******************************************************************************
PUBLIC B$IRDPTRUPD
B$IRDPTRUPD	PROC FAR
	xchg	ax,cx				;remember if @ end of DATA stmt
	xchg	ax,bx
	mov	bx,[grs.GRS_oMrsCur]		
	RS_BASE add,bx				
	GETRS_SEG  es				
	add	PTRRS[bx.MRS_data_oLineCur],ax	; assume not end of data stmt
	or	cl,cl
	jnz	RTPTRUPD_Exit			;brif not end of DATA stmt

	call	GetDataSeg			;seg address is in dx, Rs seg in es
	mov	ax,bx
	mov	bx,PTRRS[bx.MRS_data_otxCur]	
	DbAssertRel bx,nz,UNDEFINED,CODE,<B$IRDPTRUPD error in exstmisc.asm>
	mov	es,dx				; mrs text seg
	mov	dx,es:[bx+4]			;dx = pointer to link field of
						; next DATA stmt or UNDEFINED
	inc	dx
	.errnz	UNDEFINED - 0FFFFH
	jnz	Not_EndOf_Data			;brif not end of data

	mov	dl,4				;so sub dx,5 will set dx = FFFF
Not_EndOf_Data:
	xchg	ax,bx
	sub	dx,5				;dx = UNDEFINED or pointer to
						;  an exStData pcode
	GETRS_SEG es				
	mov	PTRRS[bx.MRS_data_otxCur],dx	
	mov	PTRRS[bx.MRS_data_oLineCur],6	
RTPTRUPD_Exit:
	RET
B$IRDPTRUPD	ENDP


;==============================================================================
;	SWAP
;==============================================================================

	.errnz	SizeD

MakeExe exStSwap2,opStSwap
	inc	si
	inc	si
	CALLRT	B$SWP2,Disp

MakeExe	exStSwap4,opStSwap
	inc	si
	inc	si
	CALLRT	B$SWP4,Disp

MakeExe	exStSwap8,opStSwap
	inc	si
	inc	si
	CALLRT	B$SWP8,Disp

MakeExe	exStSwapSD,opStSwap
	inc	si
	inc	si
	pop	ax
	pop	bx
	xor	cx,cx
	push	ds
	push	bx
	push	cx
	push	ds
	push	ax
	push	cx
	CALLRT	B$SWPN,Disp

MakeExe	exStSwapTyp,opStSwap
	LODSWTX				;Get size to swap
	pop	dx
	pop	bx
	push	ax			;Add length to arg on stack
	push	bx
	push	dx
	push	ax
	CALLRT	B$SWPN,Disp



	;seg_code = segment address for the CODE segment
	;It can be referenced from any module as follows:

	;	EXTRN	seg_code:abs
	;	mov	ax,SEG seg_code
	
	PUBLIC	seg_code
	seg_code	EQU	SEG B$IRDPTRUPD

sEnd	CODE
end
