page    49,132
	TITLE	ssaid	- Scan support for array id opcodes
;***
;ssaid.asm - Scan support for array id opcodes
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   This module contains the scanner dispatch routines for array id opcodes.
;   These routines perform all tasks related to moving array id opcodes from
;   SS_PARSER to SS_EXECUTE scan state.
;
;   Scan routines for IdLd opcodes make a stack entry that describes the type
;   and location of the id in the scanned pcode.  The scan stack entry is
;   created by:
;
;	push oTx  - emitted pcode address of byte following id
;		     (address at which coercion would be inserted)
;	push oTyp - type of expression or ET_RC for records
;
;   See scanner.inc for a complete definition of the id stack entry.  The
;   oTyp word contains flags that uniquely distinguish variable references,
;   literals, and intermediate expression values.
;
;   Routines named Ss_<name> are dispatched by a jmp.  These routines
;   all return to the scan loop by an indirect jmp through variable scanret.
;
;
;****************************************************************************

	.xlist
	include 	version.inc
SSAID_ASM = ON
	IncludeOnce	context
	IncludeOnce	executor	
	IncludeOnce	opid
	IncludeOnce	pcode
	IncludeOnce	qbimsgs
	IncludeOnce	ssint
	IncludeOnce	txtmgr
	IncludeOnce	variable
	.list

assumes ds, DATA
assumes es, NOTHING
assumes ss, DATA


sBegin	SCAN
assumes cs, SCAN

	subttl	Ss_AIdLd
	page
;***
;Ss_AId Scan Ld and St variants of array Id opcodes
;
;Purpose:
;
;   Scan the id variants opAIdLd<type> and opAIdSt<type>.
;
;   Functions are referenced using opAIdLd opcodes.  The scanner must detect
;   that the reference is a function reference from flags in the variable
;   table entry.
;
;   These routines expect only fundamental BASIC data types.  User types are
;   handled by opIdRf variants followed by OpIdOff opcodes.
;
;Parsed state opcode format:
;
;   (Nexp,...) opAId<Ld|St><Imp|I2|I4|R4|R8|CY|SD|TX>(cnt,oVar)
;
;      The expressions preceeding the opcode represent the indices whose
;   count is given by the argument cnt.
;
;Algorithm:
;
;   Calculate exe map table offset for data type from type in vt.
;   Calculate exe map offset for <I|S|F|C> from oPrsCur and vt flags
;   Load and emit executor
;   Copy operand
;   Coerce index arguments to I2.
;   If this is a Ld variant then
;	    Push stack entry
;	       operand address + 2
;	       type (with variable reference bit set)
;   Else
;	    Ensure the right side variable is of the correct type.
;   Set $STATIC flag in the variable table (if this is 1st ref).
;   Return to main loop
;
;Input:
;
;   ax	  = executor map address
;   bx	  = 2 * opcode
;   es:di = pcode emission address
;   es:si = pcode source address
;
;Output:
;
;	si,di updated
;
;Modifies:
;Exceptions:
;	Ss_Error
;******************************************************************
page
StRedirect:
	mov	ax,[bx].VAR_Value	    ;Value field has new oVar
	mov	PTRTX[si+2],ax		    ;Replace old one in pcode
	jmp	short AIdStRetry

SsProc	AIdSt,Rude
	.erre	LOW OPCODE_MASK EQ 0ffh 	    
	test	byte ptr es:[si-1],HIGH (NOT OPCODE_MASK)
	jnz	AIdStRetry			    
	mov	dx,scanOFFSET mpAStImpOpExe	    
AIdStRetry:					    
		mov	bx,[MrsCur.MRS_bdVar.BD_pb] 
	    add     bx,PTRTX[si+2]		    

	DbChk	pVar,bx 			    ;Verify this is a variable

	;Check flags to see if we're really doing a store

	    mov     ax,[bx].VAR_flags	; Fetch flags
	    TestX   ax,FVREDIRECT	;Test for redirected variable
	    jnz     StRedirect		;Go fix the redirected case
	    TestX   ax,FVFUN		;Test for function reference
	jz	@F
	mov	ax,ER_DD		;It is an error to store into a
	call	SsError 		;function with arguments.
@@:
	DbAssertTst [bx].VAR_flags,nz,FVARRAY+FVFUN,SCAN,<Ss_AIdSt: Non-Array>

	call	cDimsAndIndex		;Check cDims, index into executor map
					;CX = scan stack oTyp of variable
	jcxz	NeedOtyp		;Is it a record?
HavOtyp:				
	call	CoerceIndices		;Also restores DS if SizeD
assumes ds,DATA				
	xchg	ax,cx			;AX = oTyp of target array
	call	EnsureArgType		;Pop stack frame test for coerce
	jmp	[ScanRet]

NeedOtyp:				
	mov	cx,[bx].VAR_oTyp	;Get real oTyp for coercion
	jmp	HavOtyp			

;*****************************************************************************

LdRedirect:
	mov	ax,[bx].VAR_Value	    ;Value field has new oVar
	mov	PTRTX[si+2],ax		    ;Replace old one in pcode
	jmp	short AIdLdRetry

SsProc	AIdLd,Rude
	.erre	LOW OPCODE_MASK EQ 0ffh 	    
	test	byte ptr es:[si-1],HIGH (NOT OPCODE_MASK)
	jnz	AIdLdRetry			    
	mov	dx,scanOFFSET mpALdImpOpExe	    
AIdLdRetry:					    
		mov	bx,[MrsCur.MRS_bdVar.BD_pb] 
	    add     bx,PTRTX[si+2]		    

	DbChk	pVar,bx 			    ;Verify this is a variable

	;Check flags to see if we're really doing a load

	    mov     ax,[bx].VAR_flags	; Fetch flags
	    TestX   ax,FVREDIRECT	;Test for redirected variable
	    jnz     LdRedirect		;Go fix the redirected case
	    TestX   ax,FVFUN		;Test for function reference
	jnz	ExecFunc		;This is a function

	DbAssertTst [bx].VAR_flags,nz,FVARRAY,SCAN,<Ss_AIdLd: Non-Array>

	call	cDimsAndIndex		;Check cDims, index into executor map
	call	CoerceIndices		;Also restores DS if SizeD
assumes ds,DATA				
	or	ch,HIGH ST_ArrVar	;Indicate this is a var reference
	push	di			;oTx for coercion insertion
	push	cx			;oTyp of array element or ET_RC
	jmp	[ScanRet]

ExecFunc:
	mov	cx,PTRTX[si]		;Get count of args
	jmp	SsCallFunc

subttl	Subroutines for <AId|Id><|Off><Ld|St>
page

;cDimsAndIndex
;
;   Used by AId<Ld|St>
;   Check count of indices, then fall into SsIndexType
;
;Inputs:
;
;   dx = base of executor map
;   ds:bx = pVar
;   es:si = pointer to first operand (cDims)
;
;Exceptions:
;
;   Generate MSG_SubCnt if incorrect number of dimensions
;   Undefined Array if first ref as whole array


FirstReference:

	;It's ok to be undefined if this is a ReDim

	cmp	PTRTX[si+4],opStReDimTo 
	je	ArIndexType		


	    ;For QB4 this is the first ref to a previously undefined array.
	    ;Lets treat this as an implicit dim.  However, it is an error if
	    ;this is a whole array ref.

	    jcxz    ArrayNotDefined	; Brif whole array reference

	    test    byte ptr [bx].VAR_fStat,FV_STATIC	; Is this $Static
	    jz	    ArrayNotDefined	


	    cmp     [bx].ASTAT_ad.FHD_hData,0	;Allocated already?
	    jne     ArrayCmpCDims	;Brif yes.  Bypass allocation.

	    cmp     [SsErr],0		; Have any errors occured?
	    jne     ArrayCmpCDims	; Brif yes.  Bypass allocation.

	    push    cx			;Save CX = cDims
	    push    dx			;Save DX = Executor map

	    mov     [SsScanExStart],1	;Indicate implicit Dim
	    push    ax			;Dummy parameter
	    push    ax			;Dummy parameter
	    call    ExecuteFromScan	;Allocate array.  AX = RT error code.

	    pop     dx			;Restore DX = Executor map
	    pop     cx			;Restore CX = cDims

		mov	bx,[MrsCur.MRS_bdVar.BD_pb]
	    add     bx,PTRTX[si+2]	;BX = pVar

	    or	    ax,ax		;Was there an error?
	    jnz     ArrayError		;Brif error code <> 0

	    jmp     short ArrayCmpCDims 


ArrayNotDefined:
	mov	ax,ER_UA		; Array not defined

ArrayError:
	call	SsError 		
	jmp	short ArIndexType	

cDimsAndIndex:
	mov	cx,PTRTX[si]		; Get actual cDims
	and	ch,7fh			; Clear MSB which indicates no ()


	    mov     [f_StaticCalc],TRUE ;$Static array if this is first ref

	;There is a problem with ReDim where the FV_STATIC bit is being set
	;when the first array reference is a ReDim with constant indices.
	;This causes the SetArrayType call in Ss_ReDim to complain about
	;ReDim of a $Static array.  The resetting of f_StaticCalc is a
	;solution.  For EB, this is done above as a default.  For QB4, implicit
	;arrays are $Static and only if this is a ReDim is $Dynamic selected.

	cmp	PTRTX[si+4],opStReDimTo 
	jne	@F			
	shr	cx,1			; AIdLd in ReDim has cDims * 2

	    mov     [f_StaticCalc],FALSE;Dynamic array if this is first ref
@@:					

	call	SetArrayType		;CY set if first ref
	jc	FirstReference		; Brif this is first reference
	jcxz	ArIndexType		; Whole array ref. Bypass cDim chk.


ArrayCmpCDims:
	DbAssertRelB	ch,e,0,SCAN,<cDimsAndIndex: cDims GT 255>
	mov	ax,MSG_SubCnt
	cmp	cl,[bx].VAR_value.ASTAT_cDims	;Cmp cDims from variable table
	jne	ArrayError		;Correct no. of indices?

ArIndexType:

	;Look for special case of integer array with 1 dimension

	dec	cx			    ; One dimension?
	jnz	SsIndexType		    ;brif no
	mov	ax,[bx].VAR_flags	    ; Fetch flags
	    TestX   ax,FVCOMMON+FVFORMAL    ;Don't optimize these
	    jnz     SsIndexType
	and	ax,FV_TYP_MASK		    ;Mask flags down to oTyp
	.erre	ET_I2 EQ 1		    
	dec	ax			    ; I2?
	jnz	SsIndexType		    ;Brif no, only optimize I2s
	add	dx,A1SOffset		    ;Point to optimized executors

	;Note:	The constant A1SOffset represents the distance between the
	;executor map for static arrays and the map for one dimension
	;static arrays.  If this is a frame array we are optimizing, the
	;map address must be adjusted again to account for the different
	;seperation.

	    TestM   [bx].VAR_flags,FVVALUESTORED    
	    jnz     SsIndexType 	;Pointing to correct executor map
	add	dx,FrameOffset+A1FrameOffset	;Correct for later ISFC calc.
; fall into SsIndexType

;SsIndexType
;
;	Used by all <AId|Id|Off><Ld|St>
;	Compute index into executor map based on type
;	Executor map is organized as follows:
;		Record
;		I2
;		I4
;		R4	    Only if R4s enabled (FV_R4)
;		R8
;		CY	    Only if currency enabled (FV_CURRENCY)
;		SD
;		TX	    Only if text enabled (FV_TEXT)
;		FS
;		FT	    Only if text enabled (FV_TEXT)
;Inputs:
;	ds:bx = pVar
;	cs:dx = base of executor map
;Outputs:
;	cx = Scan stack type
;	cs:dx = modified executor map address
;Preserves:
;	ax,bx

	public	SsIndexType,SsIndexTypeCx
SsIndexType:
	mov	cl,[bx].VAR_flags
	and	cx,FV_TYP_MASK		;CX = oTyp
SsIndexTypeCx:
	cmp	cx,ET_MAX		;Record type?
	jbe	StdType
.errnz	ET_RC
	xor	cx,cx			;Replace oType with ET_RC
StdType:
	    jb	    IndexType		
	    mov     [SsOtxHeapMove],di	;FS/FT can cause heap movement
IndexType:
;Calculate offset due to type (cx)
	add	dx,cx
	add	dx,cx			;One word per type in table
	ret



;CoerceIndices
;
;   Used by AId<Ld|St>
;   Calls SsIndexISFC, copies oVar, then coerces array indices
;
;Inputs:
;
;   dx = current executor map address
;
;Preserves:
;
;   cx

CoerceIndices:
	call	SsIndexISFC		;Locate executor, emit and copy cDims
	MOVSWTX 			;Copy oVar

	;Coerce  array indices

	pop	[SsCbParmCur]		;Get return address out of the way
	mov	bx,cx			;Preserve cx in bx
	mov	cx,PTRTX[di-4]		;count of indices
	and	ch,7fh			;clear MSB when no parens listed
	mov	ax,ET_I2		;Target type for indices
        call    SsCoerceN               ;Coerce indices to I2
	mov	cx,bx			
	jmp	[SsCbParmCur]		;Return to caller

;SsIndexISFC
;
;	Used by <AId|Id><Ld|St>
;	Call SsGetISFC to index into executor map, then fetches and emits
;	executor and one operand
;Inputs:
;	bx = pVar
;	dx = current executor map address
;Preserves
;	cx

	public	SsIndexISFC		;Restores DS if necessary


SsIndexISFC:
	push	cx
	call	SsGetISFC		;Calculate <I|S|F|C> offset from
					;   bx (MSV flags) and oPrsCur
					;Offset returned as modified dx
	pop	cx

;SsEmitExecutor
;
;   Called by Off<Ld|St>, fallen into by all others
;   Fetches executor from map, emits and copies one operand
;
;Inputs:
;
;   dx = current executor map address
;
;Preserves:
;
;   cx

	public	SsEmitExecutor
SsEmitExecutor:
	mov	bx,dx
	mov	ax,WORD PTR cs:[bx]	;Load executor
	STOSWTX 			;Emit the executor
	MOVSWTX 			;Copy the operand
	ret

subttl	Executor map for AIdLd variants
page
;Table mpALdImpOpExe is a list of AIdLdImp executors.  The list is ordered
;as follows:
;	exAId<I|E><I|S|F|C>Ld<type>
;After the Ld executors is Rf excutors for numeric types only.
;
;This table is then followed by AIdLdExp executors.
;Type "0" is used by record executors (implicits only).


	;Optimized 1-dimension load

mpA1IdISLd	equ	$ - 2			
	DWEXT	exA1IdISLdI2

	;Additional types may be added here

mpA1IdIFLd	equ	$ - 2			
	DWEXT	exA1IdIFLdI2

A1FrameOffset	=   mpA1IdIFLd - mpA1IdISLd

	;Note:	The following word fills space used by MakeRef
	;before the explicit map to find the implicit map.

	DW	0				

	public	mpALdImpOpExe
mpALdImpOpExe	label	word			


A1SOffset   =	mpA1IdISLd - $			

mpAIdISLd	label	word			
	DWEXT	exAIdISRf
	DWEXT	exAIdISLd2
	DWEXT	exAIdISLd4
	DWEXT	exAIdISLdR4
	DWEXT	exAIdISLdR8			
	DWEXT	exAIdISRfSD
	DWEXT	exAIdISLdFS

cbContext   =	$ - mpAIdISLd

mpAIdICLd	label	word			
	DWEXT	exAIdICRf
	DWEXT	exAIdICLd2
	DWEXT	exAIdICLd4
	DWEXT	exAIdICLdR4
	DWEXT	exAIdICLdR8			
	DWEXT	exAIdICRfSD
	DWEXT	exAIdICLdFS
	.erre	cbContext EQ ($-mpAIdICLd)	

mpAIdIILd	label	word			
	DWEXT	exAIdIIRf
	DWEXT	exAIdIILd2
	DWEXT	exAIdIILd4
	DWEXT	exAIdIILdR4
	DWEXT	exAIdIILdR8			
	DWEXT	exAIdIIRfSD
	DWEXT	exAIdIILdFS
	.erre	cbContext EQ ($-mpAIdIILd)	

FrameOffset =	mpAIdISLd - $			

mpAIdIFLd	label	word			
	DWEXT	exAIdIFRf
	DWEXT	exAIdIFLd2
	DWEXT	exAIdIFLd4
	DWEXT	exAIdIFLdR4
	DWEXT	exAIdIFLdR8			
	DWEXT	exAIdIFRfSD
	DWEXT	exAIdIFLdFS
	.erre	cbContext EQ ($-mpAIdIFLd)	

	;AIdRfImp executor map

mpAIdIRf	label	word			


mpAIdISRf	equ	$-2			
	DWEXT	exAIdISRf
	DWEXT	exAIdISRf
	DWEXT	exAIdISRf
	DWEXT	exAIdISRf
	DWEXT	exAIdISRfSD
	DWEXT	exAIdISRfFS
	.erre	cbContext EQ ($-mpAIdISRf)	

mpAIdICRf	equ	$-2			
	DWEXT	exAIdICRf
	DWEXT	exAIdICRf
	DWEXT	exAIdICRf
	DWEXT	exAIdICRf
	DWEXT	exAIdICRfSD
	DWEXT	exAIdICRfFS
	.erre	cbContext EQ ($-mpAIdICRf)	

mpAIdIIRf	equ	$-2			
	DWEXT	exAIdIIRf
	DWEXT	exAIdIIRf
	DWEXT	exAIdIIRf
	DWEXT	exAIdIIRf
	DWEXT	exAIdIIRfSD
	DWEXT	exAIdIIRfFS
	.erre	cbContext EQ ($-mpAIdIIRf)	

mpAIdIFRf	equ	$-2			
	DWEXT	exAIdIFRf
	DWEXT	exAIdIFRf
	DWEXT	exAIdIFRf
	DWEXT	exAIdIFRf
	DWEXT	exAIdIFRfSD
	DWEXT	exAIdIFRfFS
	.erre	cbContext EQ ($-mpAIdIFRf)	

	;Function call executors

	DWFILL					
	DWEXT	exFuncNArgImp
	DWEXT	exFuncNArgImp
	DWEXT	exFuncNArgImp
	DWEXT	exFuncNArgImp
	DWEXT	exFuncNArgImp


	;Optimized 1-dimension load

mpA1IdESLd	equ	$ - 2			
	DWEXT	exA1IdESLdI2

mpA1IdEFLd	equ	$ - 2			
	DWEXT	exA1IdEFLdI2

	.erre	A1FrameOffset EQ (mpA1IdEFLd - mpA1IdESLd)

	;Note:	The following word is used by MakeRef to
	;find the implicit map from the explicit map.

	DW	mpALdImpOpExe			

	public	mpALdExpOpExe
mpALdExpOpExe	label	word			


	.erre	A1SOffset EQ (mpA1IdESLd - $)	

mpAIdESLd	label	word			
	DWFILL					
	DWEXT	exAIdESLdI2
	DWEXT	exAIdESLdI4
	DWEXT	exAIdESLdR4
	DWEXT	exAIdESLdR8
	DWEXT	exAIdESRfSD
	DWEXT	exAIdESLdFS
	.erre	cbContext EQ ($-mpAIdESLd)	

mpAIdECLd	label	word			
	DWFILL					
	DWEXT	exAIdECLdI2
	DWEXT	exAIdECLdI4
	DWEXT	exAIdECLdR4
	DWEXT	exAIdECLdR8
	DWEXT	exAIdECRfSD
	DWEXT	exAIdECLdFS
	.erre	cbContext EQ ($-mpAIdECLd)	

mpAIdEILd	label	word			
	DWFILL					
	DWEXT	exAIdEILdI2
	DWEXT	exAIdEILdI4
	DWEXT	exAIdEILdR4
	DWEXT	exAIdEILdR8
	DWEXT	exAIdEIRfSD
	DWEXT	exAIdEILdFS
	.erre	cbContext EQ ($-mpAIdEILd)	

	.erre	FrameOffset EQ (mpAIdESLd - $)	

mpAIdEFLd	label	word			
	DWFILL					
	DWEXT	exAIdEFLdI2
	DWEXT	exAIdEFLdI4
	DWEXT	exAIdEFLdR4
	DWEXT	exAIdEFLdR8
	DWEXT	exAIdEFRfSD
	DWEXT	exAIdEFLdFS
	.erre	cbContext EQ ($-mpAIdEFLd)	

	;AIdRfExp executor map


mpAIdESRf	equ	$-2			
	DWEXT	exAIdESRfI2
	DWEXT	exAIdESRfI4
	DWEXT	exAIdESRfR4
	DWEXT	exAIdESRfR8
	DWEXT	exAIdESRfSD
	DWEXT	exAIdESRfFS
	.erre	cbContext EQ ($-mpAIdESRf)	

mpAIdECRf	equ	$-2			
	DWEXT	exAIdECRfI2
	DWEXT	exAIdECRfI4
	DWEXT	exAIdECRfR4
	DWEXT	exAIdECRfR8
	DWEXT	exAIdECRfSD
	DWEXT	exAIdECRfFS
	.erre	cbContext EQ ($-mpAIdECRf)	

mpAIdEIRf	equ	$-2			
	DWEXT	exAIdEIRfI2
	DWEXT	exAIdEIRfI4
	DWEXT	exAIdEIRfR4
	DWEXT	exAIdEIRfR8
	DWEXT	exAIdEIRfSD
	DWEXT	exAIdEIRfFS
	.erre	cbContext EQ ($-mpAIdEIRf)	

mpAIdEFRf	equ	$-2			
	DWEXT	exAIdEFRfI2
	DWEXT	exAIdEFRfI4
	DWEXT	exAIdEFRfR4
	DWEXT	exAIdEFRfR8
	DWEXT	exAIdEFRfSD
	DWEXT	exAIdEFRfFS
	.erre	cbContext EQ ($-mpAIdEFRf)	

	;Function call executors

	DWFILL					
	DWEXT	exFuncNArgI2
	DWEXT	exFuncNArgI4
	DWEXT	exFuncNArgR4
	DWEXT	exFuncNArgR8
	DWEXT	exFuncNArgSD


;Table mpAStImpOpExe is a list of AIdStImp executors.  The list is ordered
;as follows:
;	exAId<I|E><I|S|F|C>St<type>
;This table is then followed by AIdStExp executors.
;Type "0" entries are used for record executors (implicit types only).

	;Optimized 1-dimension store

mpA1IdISSt	equ	$ - 2			
	DWEXT	exA1IdISStI2

mpA1IdIFSt	equ	$ - 2			
	DWEXT	exA1IdIFStI2

	.erre	A1FrameOffset EQ (mpA1IdIFSt - mpA1IdISSt)

	;Note:	The following word fills space used by MakeRef
	;before the explicit map to find the implicit map.

	DW	0				

	public	mpAStImpOpExe
mpAStImpOpExe	label	word


	.erre	A1SOffset EQ (mpA1IdISSt - $)	

mpAIdISSt	label	word			
	DWEXT	exAIdISStTyp
	DWEXT	exAIdISSt2
	DWEXT	exAIdISSt4
	DWEXT	exAIdISStR4
	DWEXT	exAIdISStR8			
	DWEXT	exAIdISStSD
	DWEXT	exAIdISStFS
	.erre	cbContext EQ ($-mpAIdISSt)	

mpAIdICSt	label	word			
	DWEXT	exAIdICStTyp
	DWEXT	exAIdICSt2
	DWEXT	exAIdICSt4
	DWEXT	exAIdICStR4
	DWEXT	exAIdICStR8			
	DWEXT	exAIdICStSD
	DWEXT	exAIdICStFS
	.erre	cbContext EQ ($-mpAIdICSt)	

mpAIdIISt	label	word			
	DWEXT	exAIdIIStTyp
	DWEXT	exAIdIISt2
	DWEXT	exAIdIISt4
	DWEXT	exAIdIIStR4
	DWEXT	exAIdIIStR8			
	DWEXT	exAIdIIStSD
	DWEXT	exAIdIIStFS
	.erre	cbContext EQ ($-mpAIdIISt)	

	.erre	FrameOffset EQ (mpAIdISSt - $)	

mpAIdIFSt	label	word			
	DWEXT	exAIdIFStTyp
	DWEXT	exAIdIFSt2
	DWEXT	exAIdIFSt4
	DWEXT	exAIdIFStR4
	DWEXT	exAIdIFStR8			
	DWEXT	exAIdIFStSD
	DWEXT	exAIdIFStFS
	.erre	cbContext EQ ($-mpAIdIFSt)	


	;Optimized 1-dimension store

mpA1IdESSt	equ	$ - 2			
	DWEXT	exA1IdESStI2

mpA1IdEFSt	equ	$ - 2			
	DWEXT	exA1IdEFStI2

	.erre	A1FrameOffset EQ (mpA1IdEFSt - mpA1IdESSt)

	;Note:	The following word fills space used by MakeRef
	;before the explicit map to find the implicit map.

	DW	0				

	public	mpAStExpOpExe
mpAStExpOpExe	label	word


	.erre	A1SOffset EQ (mpA1IdESSt - $)	

mpAIdESSt	label	word			
	DWFILL					
	DWEXT	exAIdESStI2
	DWEXT	exAIdESStI4
	DWEXT	exAIdESStR4
	DWEXT	exAIdESStR8
	DWEXT	exAIdESStSD
	DWEXT	exAIdESStFS
	.erre	cbContext EQ ($-mpAIdESSt)	

mpAIdECSt	label	word			
	DWFILL					
	DWEXT	exAIdECStI2
	DWEXT	exAIdECStI4
	DWEXT	exAIdECStR4
	DWEXT	exAIdECStR8
	DWEXT	exAIdECStSD
	DWEXT	exAIdECStFS
	.erre	cbContext EQ ($-mpAIdECSt)	

mpAIdEISt	label	word			
	DWFILL					
	DWEXT	exAIdEIStI2
	DWEXT	exAIdEIStI4
	DWEXT	exAIdEIStR4
	DWEXT	exAIdEIStR8
	DWEXT	exAIdEIStSD
	DWEXT	exAIdEIStFS
	.erre	cbContext EQ ($-mpAIdEISt)	

	.erre	FrameOffset EQ (mpAIdESSt - $)	

mpAIdEFSt	label	word			
	DWFILL					
	DWEXT	exAIdEFStI2
	DWEXT	exAIdEFStI4
	DWEXT	exAIdEFStR4
	DWEXT	exAIdEFStR8
	DWEXT	exAIdEFStSD
	DWEXT	exAIdEFStFS
	.erre	cbContext EQ ($-mpAIdEFSt)	

	;AdRf executor map

	public	mpAdRf				
mpAdRf	label	word				
	DWEXT	exAdRfImp			
	DWEXT	exAdRfI2			
	DWEXT	exAdRfI4			
	DWEXT	exAdRfR4
	DWEXT	exAdRfR8			
	DWEXT	exAdRfSD			

	page
;***
;MakeArrayRef
;
;Purpose:
;
;   This procedure converts an exAIdLd with cArgs == 0 to an exAdRf.
;
;Input:
;
;   BX = oTx from scan stack of pointer after exAIdLd
;
;Output:
;
;   standard exit
;
;Preserves
;
;   BX, CX, DX
;
;***************************************************************************

	public	MakeArrayRef
MakeArrayRef	proc

	mov	ax,PTRTX[bx-6]		;AX = exAIdLd executor
	xchg	ax,bx			;AX = oTx, BX = executor
	GetCodeIntoDs	SCAN		
	mov     bl,byte ptr [bx-1]	;Get HIGH byte of opcode
	push	ss			
	pop     ds

	.erre	OPCODE_MASK EQ 03ffh
;	and	bx,HIGH (NOT OPCODE_MASK)
	and	bx,0FCh 		;Mask off garbage leaving oTyp * 4
	shr	bx,1			;Convert to word offset
	mov	bx,mpAdRf[bx]		;BX = AdRf executor
	xchg	ax,bx			;AX = executor, BX = oTx
	mov	PTRTX[bx-6],ax
	ret

MakeArrayRef	endp

	page
;***
;Ss_Erase
;
;Purpose:
;
;   Scan Erase statement.
;
;   AIdLd scanning has left a stack entry consisting of:
;	oType
;	oTx of point after AIdLd
;
;Parsed state opcode format:
;
;   (AIdLd,...) opStErase(cnt)
;
;      The cnt argument represents the number of preceeding AIdLd opcodes
;   each of which will have left an entry on the stack.
;
;Input:
;
;   opStErase operand has count of AIdLd arguments.
;
;Output:
;
;   standard exit
;
;***************************************************************************

SsProc	Erase
	STOSWTX 			;Emit executor
	LODSWTX 			;Load operand count
	STOSWTX 			;And emit it
	xchg	cx,ax			;CX = operand count
EraseLoop:
	pop	bx			;Discard oType
	pop	bx			;BX = oTx after exAIdLd
	call	MakeArrayRef		;Convert to exAdRf
	loop	EraseLoop		;Go process next array
	jmp	[ScanRet]		; and back to the main loop

	page
;***
;Ss_LUbound - scan LBOUND and UBOUND
;
;Purpose:
;
;   Scan opFn<L|U>bound<1|2>
;
;   Scan stack contains:
;	I2 (for op<L|U>Bound2 case)
;	AIdLd entry:
;	    oType
;	    oTx of point after AIdLd (location of exFn<L|U>Bound<1|2>)
;
;Parsed state opcode format:
;
;   (AIdLd)	 opFn<L|U>bound1
;   (AIdLd,Nexp) opFn<L|U>bound2
;
;Input:
;
;   standard entry
;
;Output:
;
;   standard exit
;
;***************************************************************************

SsProc	LUBound2
	mov	ax,ET_I2		;Index must be I2
	call	EnsureArgType		;Perform the coercion as required
	xchg	ax,dx			
	SKIP2_PSW			; Skip over descan routine address
SsProc	LUBound1			
	STOSWTX
	pop	bx			;Discard oType
	pop	bx			;BX = oTx after exAIdLd
	push	di			;Push oTx of result for coercion
	PushI	ax,ET_I2		;Push oTyp of result. Always I2
FixNoDimArray:
	call	MakeArrayRef		;Convert to exAdRf
	jmp	[ScanRet]		; and back to the main loop

	page
;***
;Ss_GPutGet,PaletteUsing
;
;Purpose:
;
;   Scan graphics PUT, GET and PALETTE USING
;
;Parsed state opcode format:
;
;   (I2exp,...,AIdLd)	opStPalletteUsing
;   (I2exp,...,AIdLd)	opStGraphicsGet
;   (I2exp,...,AIdLd)	opStGraphicsPut(function)
;
;Input:
;
;   standard entry
;
;Output:
;
;   standard exit
;
;***************************************************************************


SsProc	PaletteUsing
	mov	dl,ET_I4		;Max type for PALETTE USING
	jmp	short PutGetUsing

SsProc	GPutGet
	mov	dl,ET_MaxNum		; Max type for PUT/GET
PutGetUsing:
	call	EmitExCopyOps		;Emit executor, copy operands for PUT
	pop	cx			; Get oTyp
	or	cl,cl
	jz	TMErr			;If not simple type, always wrong
	cmp	cl,dl			;Test for valid array type (numeric)
	jbe	PutGetX
TMErr:
	mov	ax,ER_TM		;Type mismatch error
	call	SsError
PutGetX:
	pop	bx			;BX = oTx of insertion point
	cmp	byte ptr es:[bx-4],0	;Is cDims == 0
	je	FixNoDimArray		;Brif yes, convert to AdRf
	xchg	ax,cx			;AX = oTyp w/Flags
	call	MakeRef 		;AX = AIdRf executor
	mov	PTRTX[bx-6],ax		;Update emitted code
	jmp	[ScanRet]		; and back to the main loop


sEnd	SCAN
	end
