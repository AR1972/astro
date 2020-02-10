page	49,132
	TITLE	EXPROC - Executors for procedures
;***
;exproc.asm - executors for procedures
;
;	Copyright <C> 1986, Microsoft Corporation
;
;
;****************************************************************************

	.xlist
	include		version.inc
EXPROC_ASM = ON
	IncludeOnce	architec
	IncludeOnce	context
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	extort
	IncludeOnce	opcontrl
	IncludeOnce	opid
	IncludeOnce	opstmt
	IncludeOnce	qbimsgs
	IncludeOnce	rtinterp
	IncludeOnce	rtps
	IncludeOnce	scanner
	IncludeOnce	ui
	IncludeOnce	variable
	IncludeOnce	pcode		
	.list


PUSHSS	MACRO
	endm

assumes cs, CODE
assumes es, NOTHING
assumes ss, DATA

sBegin	DATA
	externB	b$ErrInfo

ParamCnt	dw	0		
sEnd	DATA



extrn	B$STMakeTemp:far		;Copy string to temp

	externFP __fpmath		

sBegin	CODE

subttl	exStCall,exStCallLess,exStCallS
page


;***
;exStCall - Execute CALL statement
;
;Purpose:
;	1.  Activate target PRS
;
;	2.  Push return address, allocate and zero frame variables.
;
;	3.  Execute the SUB.
;
;	The stack frame looks like this:
;	<arg 1>
;	<arg 2>
;	. . .
;	<arg n>
;	<oRS of return address>
;	<oText of return address>
; BP-->	<old BP>
;	<old b$curframe>			[2]
;	<local variables and FOR temps>
;
;
;******************************************************************

MakeExe exFuncNArgSD,opAIdLd,ET_SD
	SkipExHeader
MakeExe exFuncNArgI4,opAIdLd,ET_I4
	SkipExHeader
MakeExe exFuncNArgR8,opAIdLd,ET_R8
	SkipExHeader
MakeExe exFuncNArgI2,opAIdLd,ET_I2
	SkipExHeader
MakeExe exFuncNArgR4,opAIdLd,ET_R4
	SkipExHeader
MakeExe exFuncNArgImp,opAIdLd,ET_Imp
	inc	si
	inc	si			;Skip over arg count
	jmp	short Func0Arg

MakeExe exFunc0ArgSD,opIdLd,ET_SD
	SkipExHeader
MakeExe exFunc0ArgI4,opIdLd,ET_I4
	SkipExHeader
MakeExe exFunc0ArgR8,opIdLd,ET_R8
	SkipExHeader
MakeExe exFunc0ArgI2,opIdLd,ET_I2
	SkipExHeader
MakeExe exFunc0ArgR4,opIdLd,ET_R4
	SkipExHeader
MakeExe exFunc0ArgImp,opIdLd,ET_Imp
Func0Arg:
	LODSWTX				;Get oVar
	xchg	ax,bx
	mov	ax,[pVarBx]		;Get oPRS
	jmp	short FuncCall

CallCompiledJ:
	jmp	CallCompiled

MakeExe	exStCallS,opStCallS
	SkipExHeader
MakeExe	exStCallLess,opStCallLess
	SkipExHeader
MakeExe	exStCall,opStCall
	inc	si
	inc	si			;Skip over count of arguments
	LODSWTX				;Get oPRS
FuncCall:
;ax = oPRS of target
	GETRS_SEG es,bx,<SPEED,LOAD>	
	mov	bx,ax
	RS_BASE add,bx			; es:bx points to prs
	test	BPTRRS[bx.PRS_flags],FP_DEFINED   ; Interpreted?
	jz	CallCompiledJ		

CallInterpreted:			

	inc	[b$CurLevel]		;Save any temps from deallocation
	push	[pGosubLast]		;Save current head of Gosub list
	push	[grs.GRS_oRsCur]	;Push current oRS
	xchg	dx,ax			;Save oPRS in dx
	mov	al,0
	xchg	al,[grs.GRS_fDirect]	;Get fDirect and set to FALSE
	or	al,al			;Leaving direct mode?
	jz	NotInDirect
	inc	si			;Set LSB to indicate direct mode
	or	[grs.GRS_flags],FG_RetDir
					;remember there's a ret adr to
					; direct mode buffer on stack.
NotInDirect:
; Warning:	SI is assumed to be at [bp+2] for out of stack error recovery.
; We need to make sure that fDirect gets reset correctly if we couldn't
; transfer control to the procedure.

	push	si			;Push current oTx
	push	bp
	mov	bp,sp			;Set up frame pointer
	push	[b$curframe]		; set up BASIC-frame chain
	mov	cx,PTRRS[bx].PRS_cbFrameVars ; Local variables
	add	cx,PTRRS[bx].PRS_cbFrameTemp ; Plus temps
	dec	cx			; cbFrameVars includes 2 bytes to
	dec	cx			;  account for pushed b$curframe
	mov	di,sp
	sub	di,cx			;Allocate local variables and temps
	jc	OutOfStack		;Underflow?
	cmp	di,[b$pEndChk]
	jbe	OutOfStack		;Not enough stack space left
	mov	sp,di
	mov	[b$CurFrame],bp 	;must change b$CurFrame AFTER this chk
					; there will always be a block fill
					; needed
	jcxz	NoLocals
	xor	ax,ax
	push	es			
	push	ss
	pop	es			;es = ss
	shr	cx,1			;Word count
	rep	stosw			;Zero out local variables
	pop	es			
NoLocals:
	mov	si,PTRRS[bx.PRS_otxDef] ; si points to procedure header
	xchg	ax,dx			;Restore oPRS to ax
	or	ah,80H			;oPrs ==> oRs
	call	RsActivateCODE		;Activate called proc
	RestorePcodeVar 		
	inc	si
	inc	si
	LODSWTX				;Get cbEOS
	add	si,ax			;Start execution after definition
	DispMac

OutOfStack:
	mov	ax,[bp+2]		;LSB is fDirect flag before
					;we started building the frame
	ror	al,1			;move lsb to msb
	cbw				;replicate msb into ah
	mov	[grs.GRS_fDirect],ah	;Set fDirect back to entry state
	mov	al,ER_OM
	mov	[b$ErrInfo],OMErr_STK	;note that this is really Out of Stack
					;  space, not Out of Memory
	call	RtErrorCODE

;Branched to when user is tracing through a native code call.  Since
;native code can cause screen output, we need to swap to output screen
;just in case.
;
ShowOutputScr:
	push	bx			;save pPrs
	call	ShowOutScr
	GETRS_SEG es,bx,<SIZE,LOAD>	;[4] restore seg of Rs table
	pop	bx			;restore pPrs
	DbAssertRelB [fDebugScr],e,0,CODE,<exproc: ShowOutScr failed>
;bx = pPrs of procedure to be called
CallCompiled:
	cmp	[fDebugScr],0
	jne	ShowOutputScr		;brif debug screen is visible
	DbAssertRel fNonQBI_Active,z,0,CODE,<exproc: fNonQBI_Active should be 0>
	mov	[fNonQBI_Active],bp	;save pointer to most recent QBI frame
	inc	[b$cNonQBIFrames]
	mov	ax,[b$curlevel]
	mov	[bcurlevel_QBI],ax	;if we end up having to blast the stack
					;  back to the most recent QBI frame,
					;  we need this to restore the proper
					;  setting of b$curlevel
;Save si and cbParam in last temp
	mov	di,[grs.GRS_oRsCur]	
	and	di,07FFFH		; mask off high bit
	RS_BASE add,di			
.errnz	MRS_cbFrameTemp - PRS_cbFrameTemp
	mov	ax,PTRRS[di].MRS_cbFrameTemp ; Get no. of temps for module
	neg	ax			;Make it oBP
	add	ax,bp
	sub	ax,PTRRS[di.MRS_cbFrameVars] ; Point to last temp
	xchg	di,ax
	mov	[di],si
	xor	ax,ax			;Always leave ParamCnt zero
	xchg	ax,[ParamCnt]		
	mov	[di+2],ax		;Amount of stack to clean up
	mov	[grs.GRS_otxCur],si	;Save here in case of error

	cmp	BPTRRS[bx.PRS_procType],PT_SUB	
	jnz	CallCompiledFunc	;call a User Library SUB
	call	dword ptr [bx.PRS_txd.TXD_oCompiled]	 
	call	RestoreFromCall
CallX:
	DispMac

CallCompiledFunc:
	test	BPTRRS[bx].PRS_flags,FP_CDECL	
	jnz	MakeCall			;Don't push addr. for C
	mov	cl,BPTRRS[bx.PRS_oType] 	
	and	cl,M_PT_OTYPE			; mask out possible flags
	cmp	cl,ET_SD			;[15][4] Value returned in regs?
	jae	MakeCall			;[14]
	cmp	cl,ET_I4			;[15][4] Value returned in regs?
	jbe	MakeCall			
; Type is R4, R8, or CY
;Return value is always just before saved si in temps
;di = pointer to last temp
;cl = PRS_oType
	add	di,4			;Make room for si and cbParam
	push	di
	    cmp     cl,ET_R4		;[14] R4 ? Before ES loaded
	    mov     ax,0		;NOTE: CY flag preserved

	push	ss			
	pop	es			;es:di points to return temp
	stosw
	stosw				;Zero return value if R4
	    je	    @F			;brif R4. Only clear two words
	stosw
	stosw				;Zero rest of R8 or CY
@@:
MakeCall:
	    call    dword ptr [bx.PRS_txd.TXD_oCompiled]
	call	RestoreFromCall
;Returning from function - restore 8087 stack
	pop	cx			;Get no. of 8087 registers saved
	jcxz	NoTemps
RestoreLoop:
	mov	bx,sp
	fld	tbyte ptr DGROUP:[bx]	;Put back on stack
	add	sp,10			;Move pointer up
	loop	RestoreLoop
NoTemps:
	mov	bx,PTRTX[si-2]		;Get oVar
	mov	cl,[pVarBx].VAR_flags	;oTyp in low bits
	and	cl,FV_TYP_MASK
	cmp	cl,ET_I4		;Value in (dx:)ax?
	jb	PushI2
	je	PushI4
	cmp	cl,ET_SD
	jae	PushI2			;If string, push near address in ax
;ax points to R4 or R8
	xchg	ax,bx			;ds:bx points to return value
	cmp     cl,ET_R4
	jnz	PushR8			
	fld	dword ptr [bx]		; copy retval to 87 stack
	jmp	CallX

PushR8:
	fld	qword ptr [bx]		; copy retval to 87 stack
	jmp	CallX

PushI4:
	push	dx
PushI2:
	push	ax
	jmp	CallX

RestoreFromCall:
;Restore es, si, di and NonQBI flags after native code call
;Possible return value in dx:ax preserved
;cx preserved
;Restore return oTx to si
	GETRS_SEG es,bx,<SPEED,LOAD>	;[4] restore seg of Rs table
	mov	bx,[grs.GRS_oRsCur]	
	and	bh,07FH 		; mask off high bit
	RS_BASE add,bx			
.errnz	MRS_cbFrameTemp - PRS_cbFrameTemp
	mov	si,PTRRS[bx].MRS_cbFrameTemp	; Get no. of temps for module
	neg	si			;Make it oBP
	sub	si,PTRRS[bx.MRS_cbFrameVars]	; Point to last temp
	pop	bx			; bx = ret address
	add	sp,[si+bp+2]		;Clean off parameters
	push	bx			; push ret address back
	mov	si,[si+bp]		;Restore return oTx
	DbAssertRel b$cNonQBIFrames,nz,0,CODE,<exproc: b$cNonQBIFrames == 0>
	dec	[b$cNonQBIFrames]
	DbAssertRel fNonQBI_Active,z,bp,CODE,<exproc: fNonQBI_Active not == bp>
	mov	[fNonQBI_Active],0	;reset - - QBI code is active again
	call	GetEsDi
	ret

MakeExe	exStFunction,opStFunction
	SkipExHeader
MakeExe	exStSub,opStSub
	LODSWTX				;Get cntEOS
	add	si,ax
	DispMac


MakeExe	exStDefFN,opStDefFN
	mov	si,PTRTX[si+2]		;Get link to EndDef
	inc	si
	inc	si			;Skip past link to next DefFn
	DispMac

MakeExe	exEndSingleDef,opEndSingleDef
	;Move return value from top of stack to expected location

	mov	bx,[grs.GRS_oPrsCur]	; bx points to active prs in table
	RS_BASE add,bx			
	GETRS_SEG  es			
	mov	cx,PTRRS[bx].PRS_cbFrameVars	;[2] Size of return value + 2
	mov	di,bp
	sub	di,cx			;Location for return value
.erre	FR_FirstVar EQ -2
	dec	cx			; make cx the size of the return 
	dec	cx			; value
	mov	al,BPTRRS[bx].PRS_oType 
	and	al,M_PT_OTYPE		; mask out possible flag bits
	cmp	al, ET_SD		;[4] Returning a string?
	jae	CopySD			
	mov	si,sp			;for integer ret vals
;Added with [35]
	cmp	al,ET_R4
	jb	DefFnInt
	jz	DefFnR4
	fstp	qword ptr [di]		;Store R8 return value in stack
	jmp	short EndProc

DefFnR4:
	fstp	dword ptr [di]		;Store R4 return value in stack
	jmp	short EndProc

DefFnInt:
;End of [35]
	push	ss			
	pop	es
rep	movsb				;Move up return value
	jmp	short EndProc

CopySD:
	;pSD already on stack
	push	di			;Assign string to here
	CALLRT	B$SASS,Mov		;es is now invalid!!

	;Fall into EndDef

	SkipExHeader
MakeExe	exStEndDef,opStEndDef
	SkipExHeader


MakeExe exStEndProc,opStEndProc



	mov	bx,[grs.GRS_oPrsCur]	; bx points to active prs in table
	RS_BASE add,bx			

EndProc:
	GETRS_SEG es,di,<SPEED,LOAD>	;[4] fetch seg of Rs table
	dec	[b$CurLevel]
	mov	ax,[bp].FR_pGosubLast
	mov	[pGosubLast],ax
	mov	al,BPTRRS[bx.PRS_cwParams]	; cw of parameters
	xor	ah,ah
	shl	ax,1			;Change to cb
	add	ax,FR_MinFrame		;Remove frame
	add	ax,bp			;Compute sp w/o arguments
	xchg	ax,di			;destination in stack of return value
	mov	al,BPTRRS[bx.PRS_oType] ; oTyp of return value
	and	al,M_PT_OTYPE		; mask out possible flag bits
	mov	ah,BPTRRS[bx.PRS_procType]	
	push	ax
	mov	si,[bp].FR_otxRet	;Get oTx of return addr
	test	si,1			;LSB = returning to direct mode?
	jz	NotDirect
	mov	[grs.GRS_fDirect],-1	;Going back to direct mode
	dec	si			;Back to true oTx
	and	[grs.GRS_flags],NOT FG_RetDir
					;remember there's no ret adr to
	push	ss
	pop	ds			;ds=DGROUP
					; direct mode buffer on stack.
NotDirect:
	mov	ax,[bp].FR_oRsRet	;oRs of return addr
	call	RsActivateCODE		;Activate caller
	pop	ax			;ah=procType, al=oTyp of RetVal
	lea	bx,[bp-2].FR_FirstVar	;Point to high end of return value
	push	[bp].FR_basBpLink	;[33] Restore old b$curframe
	pop	[b$CurFrame]		
	mov	bp,[bp].FR_bpLink	;Restore old bp
	cmp	ah,PT_SUB		;Is it a SUB?
	jz	NoRetVal		;If a SUB, no return value to copy
	xchg	bx,si			;Save return oTx in bx
;Added with [32]
;Restore values to 8087 stack
	mov	cx,[di]			;Number of temp reals to restore
	jcxz	No87Temps
Restore87Loop:
	fld	tbyte ptr DGROUP:[di+2]	;Put back on stack
	add	di,10			;Move pointer up
	loop	Restore87Loop
No87Temps:
;End of [32]
	push	ss
	pop	es			;es = ss
	std				;Reverse order--possibly overlapping
	cmp	al,ET_SD		;String?
	jae	ReturnSD		
	.erre	ET_SD EQ (ET_MaxNum+1)	
;Rewritten with [24]
	    cmp     al,ET_R4
	    je	    RetR4
	cmp	al,ET_R8
	jz	RetR8

	;Have I2, I4, or CY

	cbw
	xchg	cx,ax
.erre	ET_I2 EQ 1			;cw of I2
.erre	ET_I4 EQ 2			;cw of I4

rep	movsw				;Copy return value

SetSi:
	mov	si,bx			;Return oTx
ValInPlace:
	inc	di
	inc	di
	cld
NoRetVal:
	mov	sp,di			;Return val on top of stack
	RestorePcodeVar 		
	DispMac

ReturnSD:
	dec	si
	dec	si			;Point to low byte of SD
	push	si
	mov	si,bx			;Return oTx
	mov	[grs.GRS_otxCur],si	;Save in case of error
	call	B$STMakeTemp		;Make SD a temp - ax = pSD
	stosw				;pSD/handle to its place in stack
	jmp	ValInPlace

;Added with [24]
RetR4:
	fld	dword ptr DGROUP:[si-2]
	jmp	SetSi

RetR8:
	fld	qword ptr DGROUP:[si-6]
	jmp	SetSi
;End of [24]



MakeExe	exNoList1,opNoList1
	inc	si
	inc	si			;Eat one operand
	SkipExHeader
MakeExe	exSeg,opSeg
	SkipExHeader
MakeExe	exByVal,opByVal
	DispMac

;Added with [32]
MakeExe exSave87,opNoList0
	;The following special interface is used to obtain the number
	;of items on the 87/em stack.  This allows us to account for
	;the extended stack with the emulator, with or without an 80x87.
	mov	bx,10			;arg to __fpmath to get count
	call	__fpmath		;AX = # items on the 87/em stack
	mov	cx,ax			;Count to cx
	jcxz	EmptyStack		;go if nothing to do
Save87Loop:
	sub	sp,10			;Make room for a temp real
	mov	bx,sp
	fstp	tbyte ptr DGROUP:[bx]
	loop	Save87Loop
EmptyStack:
	push	ax			;Remember how much we saved
	DispMac

MakeExe	exParamCnt,opNoList1
	LODSWTX
	mov	[ParamCnt],ax		;Remember how much to release
	DispMac
;End of [32]

MakeExe	exAddStack,opNoList1
	LODSWTX
	add	sp,ax			;De-allocate stack space
	DispMac

MakeExe	exDeallocArray,opNoList1	;oBP
;De-allocate local array vaþÿable
	LODSWTX				;Get oBP
	add	ax,bp			;point to array descriptor
	xchg	bx,ax			;pDescriptor to bx
	and	[bx].AD_fFeatures,not FADF_STATIC	;Make array dynamic
	push	bx
	CALLRT	B$ERAS,DispMov		;Erase the array - CAN cause heap mvmnt!

MakeExe	exDelLocSD,opNoList1		;oBP
;De-allocate local SD variable
	LODSWTX				;Get oBP
	add	ax,bp			;pSD
	push	ax			;Address of SD to stack
	CALLRT	B$STDL,Disp		;De-allocate string space


	;Start of [44]


MakeExe exR4ToStack,opNoList0
	sub	sp,4			;Make room for Single on 80x86 stack
	mov	bx,sp
	fstp	dword ptr DGROUP:[bx]	;Move to 80x86 stack from 80x87 stack
	fwait				;Wait for it
	DispMac


MakeExe exR8ToStack,opNoList0
	sub	sp,8			;Make room for Double on 80x86 stack
	mov	bx,sp
	fstp	qword ptr DGROUP:[bx]	;Move to 80x86 stack from 80x87 stack
	fwait				;Wait for it
	DispMac

	;End of [44]


MakeExe	exDelTmpSD,opNoList1		;oTemp
;De-allocate temp SD
	call	GetTempAddr		;Get addr of temp, adjusted for locals
	push	ax			;Address of SD to stack
	CALLRT	B$STDL,Disp		;De-allocate string space

MakeExe	exStTmpSD,opNoList1		;oTemp
	call	GetTempAddr		;Get addr of temp, adjusted for locals
	pop	bx			;Re-order stack
	push	ax			;Leave address of temp on stack
	push	bx			;1st arg - address of expression SD
	push	ax			;2nd arg - variable to assign to
	xchg	ax,bx			;Destination to bx
	mov	word ptr [bx],0		;Current length of dest. is zero
	CALLRT	B$SASS,DispMov		;Get runtime to do string assign


;Added with [24]
MakeExe	exStTmpR4,opNoList1		;oTemp
	call	GetTempAddr		;Get addr of temp, adjusted for locals
	fstp	dword ptr [pFrame]
	jmp	short PushPtr
	;DispMac

MakeExe	exStTmpR8,opNoList1		;oTemp
	call	GetTempAddr		;Get addr of temp, adjusted for locals
	fstp	qword ptr [pFrame]
PushPtr:
	PUSHSS
	push	ax			;Address of temp on stack
	fwait
	DispMac
;End of [24]

MakeExe	exStTmp4,opNoList1		;oTemp
	call	GetTempAddr		;Get addr of temp, adjusted for locals
	pop	[pFrame]		;Pop stack into the temp
	pop	[pFrame+2]		
	PUSHSS				
	push	ax			;Address of temp on stack
	DispMac

MakeExe	exStTmp2,opNoList1		;oTemp
	call	GetTempAddr		;Get addr of temp, adjusted for locals
	pop	[pFrame]		;Pop stack into the temp
	PUSHSS				
	push	ax			;Address of temp on stack
	DispMac



public	GetTempAddr
GetTempAddr:
;Get oTemp from pcode, compute pTemp to ax
	LODSWTX
	push	es			
	GETRS_SEG es,bx,<SPEED,LOAD>	;[4] fetch seg of Rs table
	mov	bx,[grs.GRS_oRsCur]	
	and	bh,07Fh 		
	RS_BASE add,bx			
	sub	ax,PTRRS[bx.MRS_cbFrameVars]	;ax is oBP
	add	ax,bp
	mov	bx,ax			;pTemp to bx too
	pop	es			
	ret

;***
;SetSP, SetSpFar
;Purpose:
;	Used to restore SP to what it was at the beginning of a statement
;	which caused a runtime error. Calculates SP based on the assumption
;	that BP is set to the proper frame, using the current context to
;	determine the offset between SP and BP.
;Inputs:
;	BP set to current frame
;	static structs assumed disabled
;	pGosubLast set appropriately
;	grs.GRS_oRsCur set appropriately
;	current mrs/prs structure has the cbFrameTemp and cbFrameVars
;		field(s) correctly set
;Outputs:
;	AX is set to appropriate location for SP to be set to at each
;		BOS based on BP.
;	PSW flags are set so caller can do a JA to jump if no stack overflow.
;	For SetSpFar, dx is a copy of PSW flags, because windows can trash
;		PSW on exit from far routines.
;Exceptions:
;	None.
;****
	PUBLIC	SetSP
SetSP	PROC NEAR
	call	far ptr SetSpFar
	push	dx			
	popf				; ensure flags set correctly
	ret
SetSP	ENDP

sEnd	CODE
sBegin	CP
assumes cs, CP

cProc	SetSpFar,<PUBLIC,FAR>		
cBegin	SegSpFar			
	DbChk	ConNoStatStructs
	mov	dx,[b$curframe] 	
	mov	ax,[pGosubLast]
	or	ax,ax
	jz	No_Gosub		;brif no gosub frame active

	cmp	ax,dx			; exists a gosub frame below?
	jbe	SetSP_Exit		;brif so - set SP to pGosubLast
No_Gosub:
	mov	bx,[grs.GRS_oRsCur]
	and	bh,07FH			;turn off high bit in oRs
	RS_BASE add,bx			; bx points to active Rs in table
	GETRS_SEG es,cx,<SIZE,LOAD>	;[4] fetch seg of Rs table
	.errnz	MRS_cbFrameTemp - PRS_cbFrameTemp
	.errnz	MRS_cbFrameVars - PRS_cbFrameVars
	mov	cx,PTRRS[bx.PRS_cbFrameTemp]	
	add	cx,PTRRS[bx.PRS_cbFrameVars]	
	mov	ax,dx			
	sub	ax,cx
	jc	SetSP_Exit1		;brif pathetic case - - frame size
					;  is greater than BP value
SetSP_Exit:
	cmp	ax,[b$pendchk]		;set status word flags for caller
SetSP_Exit1:
	pushf				
	pop	dx			
cEnd	SetSpFar			

sEnd	CP
end
