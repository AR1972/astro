page	49,132
	TITLE	ssrefarg - Scan pcodes for executors that require Rf Arguments
;***
;ssrefarg - Scan pcodes for executors that require Rf Arguments
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   The pcodes scanned by this module have executors that require the
;   address of a variable as an argument.
;
;
;****************************************************************************

	.xlist
	include 	version.inc
SSREFARG_ASM = ON
	IncludeOnce	context
	IncludeOnce	qbimsgs
	IncludeOnce	rtps
	IncludeOnce	ssint
	IncludeOnce	variable
	.list


assumes DS, DATA
assumes es, NOTHING
assumes ss, DATA

subttl	opcode to executor maps for opcodes with executors with Rf Args
page
;These tables are used by scan routines to map opcodes to executors.

sBegin	SCAN
assumes cs, SCAN


;INPUT Statement
PUBLIC	mStInputOpExe
mStInputOpExe:
mStInputFirst:
	DWEXT	exStInputI2
	DWEXT	exStInputI4
	DWEXT	exStInputR4
	DWEXT	exStInputR8
	DWEXT	exStInputSD
	DWEXT	exStInputFS		    ;In the table twice for easy mapping

;Table index offset for near references
omNear	= $ - mStInputFirst

	DWEXT	exStInputI2Near
	DWEXT	exStInputI4Near
	DWEXT	exStInputR4Near
	DWEXT	exStInputR8Near
	DWEXT	exStInputSD		    ;In the table twice for easy mapping
	DWEXT	exStInputFS


	;READ Statement

	public	mStReadOpExe
mStReadOpExe:
	DWEXT	exStReadI2
	DWEXT	exStReadI4
	DWEXT	exStReadR4
	DWEXT	exStReadR8
	DWEXT	exStReadSD
	DWEXT	exStReadFS

	DWEXT	exStReadI2Near
	DWEXT	exStReadI4Near
	DWEXT	exStReadR4Near
	DWEXT	exStReadR8Near
	DWEXT	exStReadSD
	DWEXT	exStReadFS




	;Maps for LSET/RSET/MID use LSB only for indexing

	;LSet

	public	mStLsetOpExe
mStLsetOpExe:
	DWEXT	exStLset,
	DWEXT	exStLsetFS

	;RSet

	public	mStRsetOpExe
mStRsetOpExe:
	DWEXT	exStRset
	DWEXT	exStRsetFS

	;Mid$

	public	mStMid_2OpExe
mStMid_2OpExe:
	DWEXT	exStMid_2
	DWEXT	exStMid_FS2

	;Mid$

	public	mStMid_3OpExe
mStMid_3OpExe:
	DWEXT	exStMid_3
	DWEXT	exStMid_FS3
					

	;Swap


	public	mStSwapOpExe
mStSwapOpExe	label	word
	DWEXT	exStSwapTyp
	DWEXT	exStSwap2
	DWEXT	exStSwap4
	DWEXT	exStSwap4
	DWEXT	exStSwap8
	DWEXT	exStSwapSD


sEnd	SCAN

sBegin	DATA

oTxInputType	DW (?)
cInputType	DW 0

sEnd	DATA

sBegin	CODE

	extrn	exPushOp:near
	extrn	exStLSetRec:near
	extrn	exFnLenTyp:near
	extrn	exStLineInputFS:near
	extrn	exAddStack:near 	;Add constant to sp


sEnd	CODE

sBegin	SCAN
assumes cs, SCAN

	subttl	Ss_FPutGet<2|3>
	page
;***
;Ss_FPutGet<2|3>
;Purpose:
;	Scan file PUT and GET varients that require an Rf
;
;	Special tasks include:
;	- make the variable an Rf
;	- Make sure the Rf will result in a far address
;	- emit the size of the variable as an operand
;Input:
;	standard scanner entry
;Output:
;	standard scanner exit
;
;*******************************************************************************
SsProc	FPutGet3
	call	FPutGetCom
	mov	ax,ET_I4
	call	EnsureArgType		;Ensure stack has an I4 variable
	jmp	short FPutGetI2

SsProc	FPutGet2
	call	FPutGetCom
FPutGetI2:
	mov	ax,ET_I2
	call	EnsureArgType		;Ensure stack has an I2 variable
	jmp	[ScanRet]		;And back to main loop

;***
;FPutGetCom
;Purpose:
;	Emit the executor
;	Make stack variable an Rf
;	Make sure the Rf will result in a far address
;	Emit the stack variable size as the operand
;Input:
;	ax = executor
;Output:
;	none
;
;*******************************************************************************

FPutGetCom:
	STOSWTX 			    ;Emit executor
	inc	si
	inc	si			    ;Skip source side SIZE operand
	pop	dx			    ;Get return address
	pop	ax			    ;Get oType
	pop	bx			    ;  and oTx
	push	dx			    ;Put return address back
	mov	dx,ax			    ; DX = oTyp w/flags
	.erre	ST_Typ_Mask EQ 0ffh	    ; Assure we can use AL
	cmp	dl,ET_SD		    ;SD/TX/FS/FT handled special
	jb	NotString		    ; Brif not a string type
	xor	ax,ax			    ;Signal SD with length of zero
	.erre	ST_Typ_Mask EQ 0ffh	    ; Assure we can use DL
	cmp	dl,ET_FS		    ; FS/FT?
	jb	GotSize 		    ; Brif not fixed
	dec	ax			    ;Signal FS with length FFFF
	jmp	short GotSize		    

NotString:				    
	call	GetTypeSize		    ;AX = size, CX = oTyp of variable
GotSize:				    
	STOSWTX 			    ;Emit size
	xchg	ax,dx			    ;AX = oTyp from scan stack
	    mov     dh,FarArg+FScb+Lvalue   ;Signal that it's far, real ptr to FS
	    jmp     SsRefArg		    ;Make a reference argument

;***
;GetTrueType
;Purpose:
;	Get the true type and size of a variable whose scan stack entry is 
;	in ax/bx.
;
;Input
;	ax - type word from scan stack
;	bx - oTx of oVar from scan stack
;Output:
;	cx = True oTyp of variable
;Preserves:
;	ax,bx,dx
;
;***************************************************************************

	;Added with [11]

	public	GetTrueType
GetTrueType:
	mov	cl,al
	mov	ch,0			;Set up type in cx
	jcxz	RecordType		;If not record, that's all there is
	ret

RecordType:
	push	bx
	    mov     bx,PTRTX[bx-2]	;Load oVar/oElem
	    add     bx,[mrsCur.MRS_bdVar.BD_pb] ;Dereference
	mov	cx,[bx].VAR_oTyp	;Assume oVar
	TestX	ax,ST_Record?		;oVar or oElem?
	jz	@F			;Not record variable
	mov	cx,[bx].ELEM_oTyp
@@:
	pop	bx
	ret

	;End of [11]

;***
;GetTypeSize
;Purpose:
;	Get the true type and size of a variable whose scan stack entry is 
;	in ax/bx.
;
;Input
;	ax - type word from scan stack
;	bx - oTx of oVar from scan stack
;Output:
;	ax = size
;	cx = true type
;Preserves:
;	bx,dx
;***************************************************************************
GetTypeSize:

	push	bx				
        mov     bx,PTRTX[bx-2]			; Load oVar/oElem
	add     bx,[mrsCur.MRS_bdVar.BD_pb] 	; Dereference

	TestX	ax,ST_Var?			; Is this a Var or Const?
	jz	@F				; brif const

	TestX	ax,ST_Record?			; oVar or oElem?
	jz	@F				; Brif not record variable
	mov	ax,[bx].ELEM_oTyp		
	mov	cx,ax				; CX = oTyp
	call	CbTypOTypSCAN			;[15]
	jnz	GotTypeSize			; Brif Fixed string
	mov	ax,[bx].ELEM_cbFixed		; Get correct size
	jmp	short GotTypeSize		;[J2] Go get the size and exit

@@:						
	DbChk	pVar,bx 			; Verify this is a variable
	GetOtyp ax,[bx] 			
	mov	cx,ax				; CX = oTyp
	call	CbTypOTypSCAN			;[15]
	jnz	GotTypeSize			; Brif Fixed string
	mov	ax,[bx].VAR_cbFixed		; Get correct size
GotTypeSize:					
	pop	bx				
	ret					


	subttl	Ss_LRSetMid
	page
;***
;Ss_LRSetMid
;Purpose:
;*******************************************************************************

SsProc	Lset
	xchg	cx,ax			;exe map address to cx
	pop	ax			;AX = oTyp of LHS
	.erre	ST_Typ_Mask EQ 0ffh	; Assure we can use AL
	.erre	ET_RC EQ 0		; Assure OR/JNZ is sufficient
	or	al,al			; Is this a record?
   DJMP jnz	LRSetMid		; Brif not a record

	;LSET for records

	pop	bx			; BX = oTx of LHS
	call	GetTypeSize		; AX = Size, CX = oTyp
	DbAssertRel cx,a,ET_MAX,SCAN,<Ss_Lset: LHS should be a record>	  
	xchg	ax,dx			; DX = Size of LHS
	pop	ax			;AX = oTyp of RHS
	pop	bx			; BX = oTx of RHS
	.erre	ST_Typ_Mask EQ 0ffh	; Assure we can use AL
	.erre	ET_RC EQ 0		; Assure OR/JZ is sufficient
	or	al,al			; Is this a record?
	call	TMErrorNZ		; Error if not a record

	push	ax			; Save oTyp on stack
	call	GetTypeSize		; AX = Size, CX = oTyp
	xchg	ax,cx			; CX = Size of RHS
	pop	ax			; Restore oTyp w/flags
	    push    dx			; MakeFarRef trashes dx
	    call    MakeFarRef
	    pop     dx			

	cmp	cx,dx			;Need smallest byte count
	jb	@F			
	xchg	dx,cx			;Smallest in CX
@@:					
	mov	ax,codeOFFSET exPushOp	;Executor for pushing operand
	mov	bx,di			;Insert at di
	call	Insert1Op		;Insert executor and operand
	mov	ax,codeOFFSET exStLsetRec
	STOSWTX 			;Emit executor for rec version of LSET
	jmp	[ScanRet]


SsProc	LRSetMid
	xchg	cx,ax			;exe map address to cx
	pop	ax			;AX = oTyp of LHS (Record = ET_RC)
LRSetMid:
	shr	bx,1			;bx = opcode
	mov	bl,mpOpRule[bx] 	;bx = count of integer exp's needed
	xor	bh,bh			
	xchg	bx,cx			;bx = exe map address, cx = rule byte
	mov	dx,ax			;Save copy of oTyp
	.erre	ST_Typ_Mask EQ 0ffh	; Assure we can use AL
	sub	al,ET_SD		; Maps start with ET_SD
	    .erre   ET_FS EQ ET_SD+1	; Assure LSB distinguishes fixed
	    shl     ax,1		; Convert to word offset
	and	ax,2			;[13] String = 0 / Fixed = 2
	add	bx,ax

	mov	ax,cs:[bx]		;Load and emit the executor
	STOSWTX
	pop	bx
	push	bx
	push	dx
	mov	ax,ET_SD
	call	EnsureArgType
	xchg	dx,ax			    ;oTyp to ax
	    mov     dh,Lvalue+FScb+FarArg
	    call    SsRefArg		    ;Make the id an RfId
	mov	ax,ET_SD
	call	EnsureArgType
	jcxz	LRSetMidArg		;No I2 expressions to eat
NextI2:
	mov	ax,ET_I2
	call	EnsureArgType		;Eat an I2 argument
	loop	NextI2			;Go get next I2 arg

LRSetMidArg:
	jmp	[ScanRet]		;and exit to main loop

subttl	Ss_Input
page
;***
;Ss_Input
;Purpose:
;	Scan routine for INPUT and READ
;
;	Algorithm:
;
;	1. Eat an Rf
;	   produce the Rf from an Ld
;
;	3. Copy operands
;*******************************************************************************
SsProc	Input
	xchg	bx,ax		 	;Get exe map address
	pop	ax			;AX = oTyp of operand (Record = ET_RC)
	push	ax

	;Get coercion index for near/far explosion

	    mov     cx,ax		; Save oTyp with flags
	and	ax,ST_Typ_Mask		; Clear scan stack flags
	.erre	ET_RC EQ 0		; Assure JNZ is sufficient
	jnz	InputTypOk		; Brif not a record
	call	TMError
	inc	ax			; Use any valid type (ET_I2)
InputTypOK:
	    .erre   ST_Typ_Mask EQ 0ffh ; Assure we can use CH
	    or	    ch,ch		; Is this an expression?
	    jz	    NearRef		; Expr means fcn RetVal (always near)
	    cmp     ch,HIGH ST_SimpVar	;Is it a far reference?
	    jnz     FarRef		;No special FAR executor
NearRef:				
	    add     ax,omNear SHR 1	;Adjust for near/far explosion
FarRef:
	shl	ax,1			;To word offset
	add	bx,ax
        mov     ax,cs:[bx-2]            ;Load executor
	STOSWTX 			;Emit the executor

	cmp	[cInputType],0		;Is there an active type list?
	jz	NoInPrompt		;Brif not.  Must be Read or Input #n.

	pop	ax			;ax = scan stack variable type entry
	push	ax
	call	RTTypETTyp		;Map ET Type to RT Type
	mov	bx,[oTxInputType]	;oTx of next type byte
	mov	es:[bx],al		;Put current type in type list
	inc	[oTxInputType]		;Move to next type byte
	dec	[cInputType]		;Indicate 1 fewer types
NoInPrompt:
	pop	ax
	pop	bx
	call	MakeRef 		;Make id a Rf type id
	jmp	[ScanRet]


;***
;RTTypETTyp
;Purpose:
;	Map ET types to RT types.
;
;Input:
;	ax = ET type
;Output:
;	ax = RT type
;Preserves:
;	cx,dx
;****************************************************************************
Public	RTTypETTyp
RTTypETTyp:
	mov	bx,SCANOFFSET mRTTyp - 1 ;Adjust for 1 relative indexing
	xlat	cs:[bx]
	ret

	;Runtime constants for ET types

mRtTyp:
	db	VT_I2			
	db	VT_I4			
	db	VT_R4			
	db	VT_R8			
	db	VT_SD			
	db	VT_SD			; Pass SD type for FS


	subttl	Ss_Swap
	page
;***
;Ss_Swap
;
;	When swapping FS types, they are always assigned to temporary SD
;	variables in the stack.  This costs nothing if one of arguments was
;	SD, the other FS.  If both were FS, this should only be done if
;	evaluation of the second argument could cause heap movement
;	(invalidating the pointer to the first arguement).  However, the
;	existing mechanism cannot determine if this is the case--all FS 
;	operations are assumed to cause heap movement.  So SD is always used.
;
;*******************************************************************************


	.errnz	SizeD			; Won't work in SizeD

SsProc	Swap
	inc	si
	inc	si			;Ignore operand to opStSwap
	pop	ax			;AX = oTyp of 2nd arg (Record = ET_RC)
	push	ax			; Restore oTyp
	mov	dh,FarArg+Lvalue	;Assume far references are desired
	cmp	al,ET_MaxNum		;Is this a numeric or record operand?
	jbe	@F			;Brif yes, use far executor
	mov	al,ET_SD		;Use SD for FS
	mov	dh,Lvalue		;Create near references
@@:
	;Look up executor for oTyp in al

	cbw				; Clear flags
	xchg	bx,ax			;For indexing into executor table
	shl	bx,1			;Index by words
	mov	ax,mStSwapOpExe[bx]	
	STOSWTX 			;Emit executor
	pop	ax			;AX = oTyp of 2nd arg (Record = ET_RC)
	pop	bx			;BX = oTx of 2nd argument
	push	ax			; Save oTyp w/flags on stack
	call	GetTypeSize		; AX = Size, CX = oTyp of 2nd arg
	STOSWTX 			; Emit length (ET_RC) or garbage
	pop	ax			; AX = oTyp w/flags of 2nd arg
	cmp	al,ET_FS		; Is 2nd arg a fixed string?
	jb	@F			; Brif not
	.erre	ET_SD EQ ET_FS-1	
	dec	cx			; Use SD for FS
@@:
	call	SsRefArg		;Make reference to 2nd arg

	pop	ax			;AX = oTyp of 1st arg (Record = ET_RC)
	pop	bx			;BX = oTx of 1st argument
	push	ax			;Save oTyp w/flags
	push	dx			; Save SsRefArg flags
	mov	dx,cx			; DX = oTyp of 2nd argument
	call	GetTrueType		; CX = oTyp of 1st argument
	cmp	al,ET_FS		; AX is preserved across GetTrueType
	jb	@F			
	.erre	ET_SD EQ ET_FS-1	
	dec	cx			; Use SD for FS
@@:
	cmp	cx,dx			; Types match?
	call	TMErrorNZ
	pop	dx			; Restore SsRefArg flags
	pop	ax			;AX = oTyp w/flags of 1st argument
	call	SsRefArg		;Make reference to 1st arg
	jmp	[ScanRet]		; and exit to scan loop


	subttl	Ss_LineInput
	page
;***
;Ss_LineInput
;Purpose:
;**********************************************************************

SsProc	LineInput
	pop	bx			;BX = oTyp of input variable w/flags
	.erre	ST_Typ_Mask EQ 0ffh	; Assure we can use BL
	cmp	bl,ET_FS		; Is this fixed ?
	jb	@F			; Brif not fixed
	mov	ax,codeOFFSET exStLineInputFS
@@:
	.erre	ST_Typ_Mask EQ 0ffh	; Assure we can use BL
	cmp	bl,ET_SD		; Does type match the SD executor?
	jae	@F			; Brif input variable is a string
	call	TMError 		
@@:
	STOSWTX
	LODSWTX 			;Pick up the operand
	STOSWTX 			; and emit it
	xchg	ax,bx			;AX = oTyp of input variable w/flags
	pop	bx			;BX = oTx of input variable
	    mov     dh,Lvalue+FScb
	    call    SsRefArg		;Make the id an RfId
	test	word ptr es:[si-2],FINP_Prompt	;Test for another SD arg
	jz	@F			;Exit - all done
	pop	ax			;Discard oTyp of prompt
	pop	ax			;Discard oTx of prompt
@@:
	jmp	[ScanRet]		;Continue

subttl	Ss_InputPrompt
page
;***
;Ss_InputPrompt
;Purpose:
;**********************************************************************

SsProc	InputPrompt
	STOSWTX 			;Emit executor
	LODSWTX 			;Load count
	STOSWTX 			; and emit it
	mov	cx,ax			;Save count in CX
	LODSWTX 			;Load prompt
	STOSWTX 			; and emit it
	TestX	ax,FINP_Prompt		;Test for SD prompt argument present
	jz	@F
	pop	ax			;Discard oTyp of prompt
	pop	ax			;Discard oTx of prompt
@@:
	mov	ax,di
	dec	ax
	mov	oTxInputType,ax ;Save address of first type byte
	dec	cx		;Account for flag byte
	mov	cInputType,cx	;Save number of types
	shr	cx,1		;Round to words, 1st type was already copied

	    cli				; Double prefix! No interrupts!
	rep	movs	PTRTX[si],PTRTX[di]	; Copy remaining operands
	    sti 			
	mov	[SsOTxStart],di 	; In case of exSave87 insertion
	jmp	[ScanRet]		


subttl	VARPTR & SADD
page
;***
;Ss_Sadd
;*********************************************************************


SsProc	Sadd
	STOSWTX
	pop	ax			;Get oTyp
	pop	bx			;Get oTx
	mov	cx,ET_I2		;Function return type
	.erre	ST_Typ_Mask EQ 0ffh	; Assure we can use AL
	    cmp     al,ET_FS		;[7] FS/FT ?
	    je	    CantUseFS		; Special message for that case
	.erre	ST_Typ_Mask EQ 0ffh	; Assure we can use AL
	cmp	al,ET_SD		
	    .erre   ET_FS EQ ET_SD+1	
	jae	MakeArg 		
SaddTM: 				
	call	TMError 		; Must be SD
MakeArg:
	    xor     dh,dh		; For SsRefArg: near, not FS, not Lvalue
	    call    SsRefArg
RetEntry:
	push	di			;oTx
	push	cx			;Function return type
	jmp	[ScanRet]

CantUseFS:
	mov	ax,MSG_InvFixStr
	call	SsErrorBx
	pop	ax			
	jmp	short RetEntry

SsProc	Varptr				    ;Varptr, Varseg
	STOSWTX
	pop	ax			    ;Get oTyp
	pop	bx			    ;Get oTx
	    mov     dh,FarArg+FScb
	    call    SsRefArg
	    test    dh,FScb		;Was it FS? (FScb reset by SsRefArg if not)
	    jz	    SetFuncI2
	mov	cx,2			;Clean 2 bytes off stack (length of FS)
	mov	ax,codeOFFSET exAddStack
	call	Insert1Op
SetFuncI2:
	mov	cx,ET_I2
	jmp	short RetEntry

SsProc	Varptr$
	STOSWTX
	pop	ax
	mov	cx,ax			;Save oTyp w/Flags
	and	ax,ST_Typ_Mask		; Clear flags
	.erre	ET_RC EQ 0		; Assure JNZ is sufficient
	jnz	MapToRT 		
	call	TMError
	inc	ax			; Leave something valid (ET_I2)
MapToRT:
	call	RTTypETTyp		;Map to runtime type
	STOSWTX				;Runtime type is operand
	inc	si
	inc	si			;Skip source operand
	pop	bx			;oTx of argument
	xchg	ax,cx			;Restore oTyp to ax
	mov	cx,ET_SD		;Result type
	TestX	ax,ST_Var?		;Is it a variable?
	jz	MakeArg			;If not, let SsRefArg sort it out
	TestX	ax,ST_Array?		;Is it array?
	jnz	MakeArg 		;No

	;Have an array.  Only SD arrays allowed.

	cmp	ax,ST_ArrVar+ET_SD	;Is it SD array?
	jz	MakeArg			;SD array is OK
	push	ax
	mov	ax,MSG_NoNumArr
	call	SsErrorBx		;Argument can't be far array
	pop	ax
	jmp	short MakeArg


	subttl	Ss_FnLen
	page
;***
;Ss_FnLen
;Purpose:
;**********************************************************************

SsProc	FnLen
	inc	si
	inc	si			    ;Eat source operand
	pop	ax			    ;AX = oTyp of operand
	pop	bx			    ;BX = oTx of operand

	.erre	ST_Typ_Mask EQ 0ffh	    ; Assure we can use AL
	    cmp     al,ET_SD		    ;Is this a string
	    je	    FnLenSd		    ;Brif operand is a string
	    cmp     al,ET_FS		    ;Is this a string
	    je	    FnLenSd		    ;Brif operand is a string


	;Note:	The test of whether the operand is an expression is bypassed
	;for strings since string expressions are valid operands.  However,
	;numeric expressions and not valid.

	TestX	ax,ST_Var?		    ; Is this a variable?
	jz	NotAVar 		    ; Brif not

	mov	dx,ax			    ;DX = oTyp of operand
	call	GetTypeSize		    ;AX = Size of operand
	xchg	cx,ax			    ;Preserve size in cx
	xchg	ax,dx			    ;oTyp w/flags to ax

	;MakeRef is called to convert the operand load to an operand reference.
	;Having a reference allows a single executor to be used for all types
	;without having to worry about the size of the expression on the stack.


	    call    MakeFarRef
	mov	dx,codeOFFSET exFnLenTyp
	    mov     ax,ET_I4		    ;Result type for non-strings is I4
	    jmp     short EmitFnLen
FnLenSD:
	    mov     ax,ET_I2		    ;Result type for strings is I2
EmitFnLen:
	xchg	ax,dx			    ;AX = Executor, DX = Result type
	STOSWTX 			    ;emit executor...
	xchg	ax,cx
	STOSWTX 			    ;...and operand
	push	di			    ;Scan stack - expression address
	    push    dx			    ;Push result type
	jmp	[ScanRet]

NotAVar:				    
	mov	ax,ER_VarReq		    ; Len() accepts a Var or ST exp
	call	SsError 		    
	jmp	FnLenSD 		    ; Return to emit executor


page
;*** SsRefArg - Pass argument by reference
;
;Purpose:
;	Parser-generated pcode can only load (IdLd/AIdLd) arguments,
;	not generate addresses for them.  In order to pass by reference,
;	those pcodes are changed to a sequence of executors that
;	produce the near or far address of the argument.
;
;	A major "gotcha" is that while calculating subsequent arguments,
;	the heap could move, invalidating the references already on the
;	stack.  This applies only to (and all) array elements.  The
;	solution to this problem is to insert additional executors that
;	copy the element to a temporary which can't move.  To
;	determine when this is necessary, the scanner keeps the oText
;	of the last place that could cause heap movement in otxHeapMove.
;	If the argument be converted to a Rf is before this, then it
;	is susceptible to the heap movement problem.
;
;	The temporaries used are normally allocated out of the stack.  The
;	exception is an SD which does not return a value (R-value).  This
;	type is copied to a string temp, which is deallocated automatically
;	by the runtime at its first use.  If the lifetime of the temp SD
;	would be too short, the caller to SsRefArg must insert a copy executor.
;
;	Some references are L-values, i.e., they need to return a value.
;	If the argument is copied to a stack temp, and must also return
;	a value, then an additional executor is inserted at the current
;	scan position (presumably after the CALL we're preparing arguments
;	for) to copy the value back to its source.
;
;	Some Rf executors naturally produce near refs, and some do far
;	refs.  Appropriate executors are inserted after the Rf to convert
;	it to the proper type if possible.  In the case of a near ref to
;	huge array element, the element must be copied (just as if the
;	heap could move) to make it near.
;
;	There are two ways to reference FS types:  1) as near ref to an SD
;	2) as a far ref to FS with length.  Which method is determined by
;	the FScb flag: non-zero means use far ref with length ("cb").
;	NO COPYING IS EVER DONE FOR FScb (because it would only be needed
;	by SWAP).  When an L-value FS is referenced as an SD, executors will
;	assign the FS to a stack SD variable.  It cannot be left as IdLdFS,
;	which produces a string temp, because the temp would be deallocated 
;	on its first usage, which is undesirable for an L-value.  For R-value 
;	FS, the reference is left as a temp SD.
;
;Inputs:
;	ax = Type of argument from scan stack
;	bx = oTx of end of argument
;	dh = Type of ref required (optimized for SsProc)
;		FarArg <> 0 means far
;		Lvalue <> 0 means L-value
;		FScb <> 0 means far ref with length for FS
;Outputs:
;	bx updated to end of argument after any insertions
;	dx almost preserved: FScb bit in dh reset if not FS
;Preserves:
;	cx

	public	SsRefArg

	extrn	exCopyTmpAr:near,exRestoreTmpAr:near
	extrn	exCopyTmpArSD:near,exRestoreTmpArSD:near
	extrn	exCopyTmpArFS:near,exRestoreTmpArFS:near
	extrn	exCopyTmpFS:near,exRestoreTmpFS:near
	extrn	exPopPopPush:near
	extrn	exStringTemp:near
	extrn	exPushSeg:near	;executor to coerce near reference to far ref.

CheckFS:
	test	dh,Lvalue+FScb		;Zero means R-value, not FScb
	jnz	Ref			;If L-value or FScb, go process FS
SegCheckJ:
	jmp	SegCheck

MakeFarRef:
	mov	dh,FarArg		;Signal that it's far
SsRefArg:
	push	cx
	mov	cx,ax			;Save flags
	.erre	ST_Typ_Mask EQ 0ffh	; Assure we can use AL
	cmp	al,ET_FS		; FS type?
	    .erre   ET_FS EQ ET_MAX	; Assure that JAE is sufficient
	jae	CheckFS 		
	and	dh,not FScb		;Not a full FS ref
Ref:
	call	MakeRef			;Convert Ld to Rf
	test	dh,FScb			;FS with length is to left alone
	jnz	RefArgX
	xchg	ax,cx			;Restore type w/flags to ax
	TestX	ax,ST_Var?		;Is it a variable? (else fcn retval)
	jz	SegCheckJ		;Handle retval like simple variable
	TestX	ax,ST_ArrayBit		;Is it an array reference?
	jz	RefArray		;If so, check further
;Not an array reference
	.erre	ST_Typ_Mask EQ 0ffh	; Assure we can use AL
	cmp	al,ET_FS		; Is it a basic type (not incl. FS)?
	    .erre   ET_FS EQ ET_MAX	; Assure that JB is sufficient
	jb	ScalRec 		
;Handle L-value FS (R-value exited through CheckFS)
	mov	ax,4+4+2		;SD, Far address, cb
	call	AllocTemp
	mov	ax,codeOFFSET exCopyTmpFS
	call	Insert1Op
	mov	ax,codeOFFSET exRestoreTmpFS
	jmp	short CopyBack

ScalRec:				
	TestX	ax,ST_Record?		;Is it a "scalar" record element?
	jnz	Rec			; Brif record element
	.erre	ST_Typ_Mask EQ 0ffh	; Assure we can use AL
	or	al,al			; Record?
	.erre	ET_RC EQ 0		; Assure JNZ is sufficient
	jnz	SegCheckJ		; Brif not a record

Rec:
;Rf is far, but item is in DS
;Check whether near or far ref wanted
	test	dh,FarArg
	jnz	RefArgX			;Leave it far
	mov	ax,codeOFFSET exPopPopPush
	call	Insert
RefArgX:
	pop	cx
	ret

RefArray:
	.erre	ST_Typ_Mask EQ 0ffh	; Assure we can use AL
	cmp	al,ET_FS		; Always copy FS
	    .erre   ET_FS EQ ET_MAX	; Assure that JAE is sufficient
	jae	AssignFS
	cmp	bx,[SsOtxHeapMove]	;Followed by heap movement?
	jb	CopyArg 		;Yes, go copy arg

	;Might have to copy anyway if need near ref to far element

	.erre	ST_Typ_Mask EQ 0ffh	; Assure we can use AL
	cmp	al,ET_SD		;Strings are already near ref
	    .erre   ET_FS EQ ET_SD+1	
	jae	SegCheck		; Go check if seg must be added
	test	dh,FarArg
	jnz	RefArgX			;Leave it far
CopyArg:
	.erre	ST_Typ_Mask EQ 0ffh	; Assure we can use AL
	cmp	al,ET_SD		;Must use assignment for strings
	jae	AssignSD
	call	GetTypeSize		;[11] AX = Size of element
	push	ax			;[11]
	mov	ax,codeOFFSET exCopyTmpAr ;Copy to temp
	mov	cx,8			;Need room for 3 operands
	call	InsertCx
	pop	ax			;Size of element
	jc	RefArgX			;If OME, don't overwrite pcode!
	mov	PTRTX[bx-4],ax		;Save as 2nd operand
	add	ax,8			;Need space for far addr, length, oVar
	call	AllocTemp
	mov	PTRTX[bx-6],cx
	mov	ax,codeOFFSET exRestoreTmpAr
;Back up through pcode to find oVar of array
;ax = executor to insert if copy back needed
;[bx-2] = location requiring oVar operand
;cx = oTemp
;dx = input flags (near/far, L-value/R-value)
	push	ax
	push	bx
	sub	bx,8			;Back up to AIdLd/OffLd
SearchBack:
	sub	bx,4			;Assume backing up over opOffLd
	cmp	PTRTX[bx],64		;Is it a count of indices or pcode?
	ja	SearchBack
        mov     ax,PTRTX[bx+2]          ;Get oVar
        pop     bx
        mov     PTRTX[bx-2],ax          ;Save oVar as operand
	pop	ax
;Will we need to copy back?
	test	dh,Lvalue		;Zero means R-value
	jz	SegCheck		;Don't copy back
CopyBack:
;Now add executor to copy back the value
;ax has executor, cx has oTemp
	push	bx
	mov	bx,di
	call	Insert1Op
	pop	bx
SegCheck:
	test	dh,FarArg
	jz	RefArgX
	mov	ax,codeOFFSET exPushSeg	;Executor to add segment
	call	Insert
RefArgXj:
	jmp	RefArgX

AssignFS:
	mov	ax,codeOFFSET exCopyTmpArFS
	mov	cx,6			;Two operands
	call	InsertCx
	jc	RefArgX
	mov	ax,4+4+2+2		;SD, array position, oVar, cb
	call	AllocTemp
	mov	PTRTX[bx-4],cx		;oTemp is 1st operand
	mov	ax,codeOFFSET exRestoreTmpArFS
FindOVar:
	push	ax
	push	bx
	sub	bx,6			;Point back to end of AIdLd/OffLd
	jmp	SearchBack

AssignSD:
	test	dh,Lvalue		;Copy back needed?
	jz	UseStringTemp
	mov	ax,codeOFFSET exCopyTmpArSD
	mov	cx,6			;Two operands
	call	InsertCx
	jc	RefArgXj
	mov	ax,4+2+2		;SD, array position, oVar
	call	AllocTemp
	mov	PTRTX[bx-4],cx		;oTemp is 1st operand
	mov	ax,codeOFFSET exRestoreTmpArSD
	jmp	FindOVar

UseStringTemp:
;No copy back, so just copy to string temp (automatic deallocation)
	mov	ax,codeOFFSET exStringTemp
	call	Insert
	jmp	SegCheck


public	AllocTemp
AllocTemp:
;Allocate ax bytes of temp space (always even).
;Return oTemp in cx, move old cx to ax
	inc	ax
	and	al,not 1		;Round up to even
	add	ax,[SsCbFrameTemp]	;Get new total of temps
	mov	[SsCbFrameTemp],ax
	neg	ax			;Turn into oBP
	xchg	cx,ax
	ret

sEnd	SCAN
	end
