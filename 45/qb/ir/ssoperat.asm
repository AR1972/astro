	page	49,132
	TITLE	ssoperat - scan intrinsic unary functions and operators
;***
;ssoperat - scan support for intrinsic unary operantors and functions
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   Scan unary operators and intrinsic functions.
;   Also contains scan dispatches for functions that have no arguments.
;
;
;****************************************************************************

	.xlist
	include 	version.inc
SSOPERAT_ASM = ON
	IncludeOnce	pcode		
	IncludeOnce	qbimsgs
	IncludeOnce	ssint
	IncludeOnce	variable
	.list


assumes ds, DATA
assumes es, DATA
assumes ss, DATA


	subttl Ss_FnString
	page

sBegin	CODE

	extrn	exFnString_SD:near
	extrn	exFnString_I2:near


sEnd	CODE

sBegin	SCAN


	extrn	tCo1toNotSD:byte	


assumes cs, SCAN

	;Table to map ET type to coercion table index
	;Added with [11]

Index	=	0
BadType	=	-1
Next	MACRO	Cond
IFNB	<Cond>
IFE	Cond
	exitm
ENDIF
ENDIF
	db	Index
Index	=	Index+1
	ENDM

mpScanType	label	byte
	db	BadType		;Records
I2Ind	=	Index		; Needed in MSdFs to map Forms to I2
	Next			;I2
	Next			;I4
	Next			;R4
	Next			;R8
SdInd	=	Index		;Need for FS
	Next			;SD
TxInd	=	Index		;Need for FT
	db	SdInd		;FS
	.erre	ET_MAX EQ ($-mpScanType-1)

	;Index = number of row/columns in coercion table
	;End of [11]


SsProc	FnString
	pop	ax			;AX = oTyp of argument. Record = ET_RC
	push	ax			;Restore on stack for EnsureArgType
	.erre	ST_Typ_Mask EQ 0ffh	; Assure we can use AL
	cmp	al,ET_MaxNum		; Is it numeric?
	jbe	FnStringI2		; Brif numeric or record
	mov	ax,ET_SD		; Result type is source type SD/TX
	call	EnsureArgType		;Eat the SD
	mov	ax,codeOFFSET exFnString_SD ;Load appropriate executor
	jmp	short FnStringEmitExe	;Go emit the exe and continue

FnStringI2:
	mov	ax,ET_I2		;Require an I2 for last argument
	call	EnsureArgType		;Ensure top argument is correct
	mov	ax,codeOFFSET exFnString_I2	;Was I2
FnStringEmitExe:
	STOSWTX 			;Emit the executor
	    mov     ax,ET_I2		;Eat the I2 count
	call	EnsureArgType
	push	di			;Frame -> oTx
	PushI	ax,ET_SD		;Frame -> oTyp
	jmp	[ScanRet]		;Go handle first arg and type explosion



SsProc	UMi
	xchg	bx,ax			;Executor map to bx
	pop	ax			;AX = oTyp of argument. Record = ET_RC
	pop	dx			;Discard oTx
	mov	cx,ax			;Save copy of type high bits
	.erre	ST_Typ_Mask EQ 0ffh	; Assure CBW is sufficient
	.erre	ET_MAX LT 80h		; Assure CBW is sufficient
	cbw				; Clear flags in scan stack
	dec	ax			; ET_RC --> 0FFFFh
	.erre	ET_RC EQ 0		; Assure JB is sufficient
	cmp	al,ET_MaxNum		;[3] Must be numeric
	jb	MapUM			;[3]
	call	TMError
	.erre	ET_I2 EQ 1		; Assure XOR is sufficient
	xor	ax,ax			; Force valid type (ET_I2)
MapUM:
	add	bx,ax
	add	bx,ax
	mov	ax,cs:[bx]		; Get executor
	STOSWTX 			;Emit executor
	push	di			;Build stack entry
	.errnz	LOW ST_Lit?		; Assure we can use CH
	xor	ch,HIGH ST_Lit? 	;High bits now zero if literal/const

	test	ch,0C0H			;Literal or constant?
	jnz	NotLit
	.errnz	LOW ST_LitX		; Assure we can use CH
	or	ch,HIGH ST_LitX		;Make it a constant
	jmp	short UMiX

NotLit:
	.erre	ST_Typ_Mask EQ 0ffh	; Assure XOR is sufficient
	xor	ch,ch			; Clear flags to make expression
UMiX:
	push	cx
	jmp	[ScanRet]	


	subttl	Ss_0FnETExe
	page
;***
;Ss_0FnETExe - Scan functions with 0 args
;
;Purpose:
;
;   Scan functions with 0 args.
;   These functions always have only one executor and one emitted type.
;   mpOpRule contains the emitted type.
;   mpOpExe contains the executor.
;
;Input:
;
;   ax = opcode
;   bx = opcode * 2
;   es:si   = source code address
;   es:di   = destination code address
;
;Output:
;
;   none
;
;**********************************************************************

SsProc	0FnETExe
	shr	bx,1			;Get rule address
	xor	cx,cx
	mov	cl,mpOpRule[bx] 	;Rule table index is the emitted type
					;cx = type emitted
	jmp	short EmitDxIdFrameX	;Exit through Ss_1FnETETExe


	subttl	Ss_2FnSamePExe
	page
;***
;Ss_2FnSamePExe - Scan routine for functions with 2 arguments and many executors
;
;Purpose:
;
;   This scan routine handles intrinsic functions and operators that:
;	- have 2 arguments
;	- mpOpExe is a pointer to a list of executors entered by type
;	- output type is either the input type or a constant (as
;	  indicated by LOW UNDEFINED in the mpOpRule entry)
;
;   This routine is dispatched by a JMP.
;
;Algorithm:
;
;   Get rule
;   Calculate coercion table address
;      Coerce table mpOpRule[(mpOpRule[opcode].irule)*sizeof(Rule)].pCoerce
;      Coercion table dimension is 2.
;   Enter coercion table based on operand entries on stack
;   Insert any needed coercion
;      If type from table does not match the input types AND
;	    required coercion is integer constant to single constant
;	    change the integer constant to the appropriate single constant
;	Else
;	    Enter the coercion executor table to find the coercion
;	    routine (or an error, if the types are not compatible).
;   Emit executor for current opcode
;   Push emit type entry on stack
;
;Input:
;
;   es:si = pcode emission address
;
;Output:
;
;   si updated
;
;Modifies:
;Exceptions:
;	Ss_Error
;******************************************************************
	page


SsProc	2FnSamePExe
	push	bp			
	mov	bp,sp			

	;Get left side type - records are always errors.

	mov	ax,[bp+6]		; AX = oTyp of left operand
	call	MSdFs			;Record? Map FS to SD

	; Multiply zero relative index by Index, the number of different types

	mov     cx,ax
	shl     cx,1			;*2
IF	Index AND 8
	shl     cx,1
ENDIF
IF	Index AND 2
	add     cx,ax			;*3
ENDIF
	shl     cx,1			;*4 or *6
IF	Index AND 1
	add     cx,ax			;*5 or *7
ENDIF
	.erre	(Index LE 11) AND (Index GE 4)

	;Get right side type - records are always errors.

	mov	ax,[bp+2]		; CX = oTyp of right operand
	pop	bp
	call	MSdFs			;Record? Map FS to SD

	;Get target type from coercion table

	add	ax,cx			;Coercion table index
	mov	cx,bx			;Save opcode*2
	shr	bx,1			;Back to opcode (byte index)
	mov	bl,mpOpRule[bx] 	;Get the rule table index
	xor	bh,bh
	mov	dx,bx			;Preserve tRuleByte index for exit
	shl	bx,1			;To word index
	mov	bx,tRuleWord[bx]	;Coercion table address
	xlat	byte ptr cs:[bx]	;Get result type
	cbw
	call	EnsureArgType		; Coerce right side expression
	call	EnsureArgType		; Coerce left side expression
	mov	bx,cx			; Opcode*2 back to bx
	mov	bx,mpOpExe[bx]		;Get executor address map
	xchg	dx,bx
	mov	cl,tRuleByte[bx]	;cx = rule table byte
	xor	ch,ch

EmitTypeExeIdFrameX:
	;ax = argument type (UNDEFINED if type mismatch)
	;cx = rule table byte
	;dx = executor map address

	;Emit executor for current opcode

	cmp	ax,UNDEFINED
	jnz	EmitValidTypeExe
	mov	ax,ET_I2		;This type valid for all users
EmitValidTypeExe:
	mov	bx,dx			;Exe map address to bx
	dec	ax			;To zero relative
	add	bx,ax
	add	bx,ax
	inc	ax
	mov	dx,WORD PTR cs:[bx]	;Executor address

EmitDxIdFrameX:
	;ax = argument type
	;cx = rule table byte
	;dx = executor

	xchg	dx,ax			;ax = executor, dx = input type
	STOSWTX

	xor	ch,ch			;cx = word oType or LOW UNDEFINED
	jcxz	UpdateOTxStart		; Not a function (nothing to emit)

	;Build stack entry for expression location and type

	cmp	cl,LOW UNDEFINED	;Test for output type = input type
	jne	@F			;Output type not input type (cx = type)
	mov	cx,dx			;Output type is input type (saved in dx)
@@:
	push	di			;Stack expression fragment address
	push	cx			;Stack type for expression fragment
	DbAssertRel cx,be,ET_MAX,SCAN,<EmitIdFrameX:  Unexpected return type>  
	    cmp     cl,ET_SD		; SD result?
	    jb	    @F			;Exit if not
	    mov     [SsOTxHeapMove],di	;Assume it can move the heap
@@:
X1FnPExe:
	jmp	[ScanRet]		;Return to scan loop

UpdateOTxStart: 			
	mov	[SsOTxStart],di 	; Update clear stack location
	jmp	X1FnPExe		


	subttl	Ss_1FnPExe
	page
;***
;Ss_1FnPExe - Scan routine for functions with 1 argument and many executors
;
;Purpose:
;
;   This scan routine handles intrinsic functions and operators that:
;      - have 1 argument
;      - mpOpExe is a pointer to a list of executors entered by type
;      - output type depends on the tRuleByte entry as follows:
;	    the input type (indicated by LOW UNDEFINED)
;	    a constant (as indicated by an ET_xx constant)
;	    no emitted type (as indicated by 0)
;
;Algorithm:
;
;   Get rule
;   Calculate coercion table address
;      Coercion table tRuleWord[mpOpRule[opcode].irule].pCoerce
;      Coercion table dimension is 1.
;   Pop operand from stack.
;   Enter appropriate coercion table to find input type.
;   Insert any needed coercion
;      If type from table does not match the input types AND
;	    required coercion is integer constant to single constant
;	    change the integer constant to the appropriate single constant
;	Else
;	    Enter the coercion executor table to find the coercion
;	    routine (or an error, if the types are not compatible).
;   Emit executor for current opcode
;   Push emit type entry on stack
;
;Input:
;
;   es:si = pcode emission address
;
;Output:
;
;   si updated
;
;Modifies:
;
;Exceptions:
;
;   Ss_Error
;
;******************************************************************
;
	page


SsProc	PaintWidth
	mov	ax,ET_I2
	call	EnsureArgType		;Eat an integer from the stack
	SKIP2_PSW			;Complete through Ss_1FnPExe


SsProc	1FnPExe
	;Get EXE map address and rule address for this opcode

	;Get output type indicator and coercion table address for this rule

	shr	bx,1			;Get opcode (for StWidth2, too)
	call	GetRuleInfo		;Get rule byte and word for opcode bx
					;ax = rule byte
					;bx = rule word
	xchg	ax,cx			; CX = Rule byte

	;Determine coercion required by expression fragment in stack entry

CoerceOk:
	pop	ax			;Type of consumed expression + high bits
	push	ax			; and preserve
	call	MSdFs
	xlat	BYTE PTR cs:[bx]	;Enter coercion table on type in al
	cbw
CoerceField:
	call	EnsureArgType		;Perform the coercion as required
	jmp	EmitTypeExeIdFrameX	;ax = argument type
					;cx = rule table byte
					;dx = executor map address

SsProc	1FnPExeR8
	    pop     ax			;AX = Type of operand + high bits
	    push    ax			;Preserve for EnsureArgType
	    .erre   ET_I2 and 1
	    .errnz  ET_I4 and 1
	    .erre   ET_R4 and 1
	    .errnz  ET_R8 and 1
	    shr     ax,1		;Carry set for I2, R4, and SD
	    mov     ax,ET_R8
	    .erre   ET_R4 EQ ET_R8 - 1
	    sbb     al,0		;AX = ET_R4 if source is I2, R4, or SD
	call	EnsureArgType		;Perform the coercion as required
	xchg	ax,cx			;Result type is operand type
	jmp	EmitDxIdFrameX		;Emit executor


	subttl	Ss_Coerce
	page
;***
;Ss_Coerce - Scan routine for explicit type coercion functionsd
;
;Purpose:
;
;   This scan routine handles intrinsic coercion functions.
;
;Algorithm:
;
;   Get target type from opcode
;   Index into executor map and fetch either new map or executor address
;   Get source operand type from scan stack entry
;   If target type is numeric
;      Coerce operand and emit executor.  Force error if source not numeric.
;   Else target type is string
;      Report error if source type is not string
;      Emit executor
;
;Input:
;
;   es:si = pcode emission address
;
;Output:
;
;   si updated
;
;Modifies:
;
;Exceptions:
;
;   Ss_Error
;
;******************************************************************

SsProc	Coerce
	xchg	ax,bx			;BX = Executor map map address
	mov	cl,byte ptr es:[si-1]	;CL = MSB of opcode
	shr	cl,1
	shr	cl,1
	xor	ch,ch			;CX = ET type of target
	add	bx,cx
	add	bx,cx
	mov	dx,cs:[bx]		;DX = address of executor or map


	mov	bx,scanOFFSET tCo1toNotSD


	jmp	short CoerceOk		;Continue

	subttl	Ss_Format
	page
;***
;Ss_Format - Scan routine for the Format$ function
;
;Purpose:
;
;   This scan routine handles opFnFormat_
;
;Algorithm:
;
;
;Input:
;
;   es:si = pcode emission address
;
;Output:
;
;   si updated
;
;Modifies:
;
;Exceptions:
;
;   Ss_Error
;
;******************************************************************



	subttl	MSdFs
	page
;***
;MSdFs	- map FS to SD
;Purpose:
;	Map type ET_FS to ET_SD
;
;Input:
;	ax = type with high bits set
;Output:
;	ax = input type - 1, but FS mapped to SD for
;	valid lookup in coercion tables
;Preserves:
;	All
;Exceptions:
;	If record type, type mismatch error is generated
;*******************************************************************************
;Rewritten with [11]

	public	MSdFs
MSdFs:
	push	bx
	xor	ah,ah
	mov	bx,scanOFFSET mpScanType
	xlat	mpScanType		;Map to coercion table index
	pop	bx
	or	al,al			;Valid type?
	js	TmErrorJ
	ret

TmErrorJ:
	xor	al,al			;Return valid index
	jmp	TmError

	;End of [11]


;===============================================================================
subttl	opcode to executor maps for math routines
page
;These tables are used by scan routines to map opcodes to executors.


	public	mptyppexFnStr_
mptyppexFnStr_	label	word
	DWEXT	exFnStr_I2
	DWEXT	exFnStr_I4
	DWEXT	exFnStr_R4
	DWEXT	exFnStr_R8

	public	mptyppexFnEnviron_
mptyppexFnEnviron_  label   word
	DWEXT	exFnEnviron_I2
	DWFILL					; placeholder
	DWFILL					; placeholder
	DWFILL					; placeholder
	DWEXT	exFnEnviron_SD



	public	mptyppexStWidth2
mptyppexStWidth2    label   word
	DWEXT	exStWidth2I2
	DWFILL					; placeholder
	DWFILL					; placeholder
	DWFILL					; placeholder
	DWEXT	exStWidth2SD

	public	mptyppexFnFre
mptyppexFnFre	label	word
	DWEXT	exFnFreI2
	DWFILL					; placeholder
	DWFILL					; placeholder
	DWFILL					; placeholder
	DWEXT	exFnFreSD


	public	mptyppexPrintItemComma
mptyppexPrintItemComma	label	word
	DWEXT	exPrintItemCommaI2
	DWEXT	exPrintItemCommaI4
	DWEXT	exPrintItemCommaR4
	DWEXT	exPrintItemCommaR8
	DWEXT	exPrintItemCommaSD

	public	mptyppexPrintItemSemi
mptyppexPrintItemSemi	label	word
	DWEXT	exPrintItemSemiI2
	DWEXT	exPrintItemSemiI4
	DWEXT	exPrintItemSemiR4
	DWEXT	exPrintItemSemiR8
	DWEXT	exPrintItemSemiSD

	public	mptyppexPrintItemEos
mptyppexPrintItemEos	label	word
	DWEXT	exPrintItemEosI2
	DWEXT	exPrintItemEosI4
	DWEXT	exPrintItemEosR4
	DWEXT	exPrintItemEosR8
	DWEXT	exPrintItemEosSD

	public	mptyppexStPaint
mptyppexStPaint label	word
	DWEXT	exStPaint2
	DWFILL					; placeholder
	DWFILL					; placeholder
	DWFILL					; placeholder
	DWEXT	exStPaint2Tile

	public	mptyppexCoord
mptyppexCoord	label	word
	DWEXT	exCoordI2
	DWFILL					; placeholder
	DWEXT	exCoordR4

	public	mptyppexCoordStep
mptyppexCoordStep   label   word
	DWEXT	exCoordStepI2
	DWFILL					; placeholder
	DWEXT	exCoordStepR4

	public	mptyppexCoordSecond
mptyppexCoordSecond label   word
	DWEXT	exCoordSecondI2
	DWFILL					; placeholder
	DWEXT	exCoordSecondR4

	public	mptyppexCoordStepSecond
mptyppexCoordStepSecond label	word
	DWEXT	exCoordStepSecondI2
	DWFILL					; placeholder
	DWEXT	exCoordStepSecondR4

	public	mptyppexFnPoint2
mptyppexFnPoint2    label   word
	DWEXT	exFnPoint2I2
	DWFILL					; placeholder
	DWEXT	exFnPoint2R4


	public	mptyppexWatchExp
mptyppexWatchExp    label   word
	DWEXT	exWatchExpI2
	DWEXT	exWatchExpI4
	DWEXT	exWatchExpR4
	DWEXT	exWatchExpR8
	DWEXT	exWatchExpSD

	public	mptyppexFnHex_
mptyppexFnHex_	label	word
	DWEXT	exFnHex_I2
	DWEXT	exFnHex_I4

	public	mptyppexFnOct_
mptyppexFnOct_	label	word
	DWEXT	exFnOct_I2
	DWEXT	exFnOct_I4




	public	mptyppexCoerce				
mptyppexCoerce	 equ	 $-2				
	DW	scanOFFSET mptyppexFnCInt		
	DW	scanOFFSET mptyppexFnCLng		
	DW	scanOFFSET mptyppexFnCSng
	DW	scanOFFSET mptyppexFnCDbl		

mptyppexFnCInt	label	word
	DWEXT	exFnCIntI2
	DWEXT	exFnCIntI4
	DWEXT	exFnCIntR8
	DWEXT	exFnCIntR8

mptyppexFnCLng	label	word
	DWEXT	exFnCLngI2
	DWEXT	exFnCLngI4
	DWEXT	exFnCLngR8
	DWEXT	exFnCLngR8

mptyppexFnCSng	label	word
	DWEXT	exFnCSngI2
	DWEXT	exFnCSngI4
	DWEXT	exFnCSngR8
	DWEXT	exFnCSngR8

mptyppexFnCDbl	label	word
	DWEXT	exFnCDblI2
	DWEXT	exFnCDblI4
	DWEXT	exFnCDblR8
	DWEXT	exFnCDblR8

sEnd	SCAN
end
