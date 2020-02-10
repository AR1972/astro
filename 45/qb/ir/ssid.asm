page	49,132
	TITLE	ssid	- Scan support for simple Id opcodes
;***
;ssid.asm - Scan support for simple Id opcodes
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   This module contains the scanner dispatch routines for simple id opcodes.
;   These routines perform all tasks related to moving simple id opcodes from
;   one scan state to another.
;
;   Scan routines for IdLd opcodes make a stack entry that describes the
;   type and location of the id in the scanned pcode.  The scan stack
;   entry appears as:
;
;	push oTx  - emitted pcode address of byte following id
;		     (address at which coercion would be inserted)
;	push oTyp - type of expression or ET_RC for records
;
;   See scanner.inc for a complete definition of the id stack entry.  The
;   oTyp word contains constants that uniquely distinguish variable
;   references, literals and intermediate expression values.
;
;   Routines named Ss_<name> are dispatched by a jmp.  These routines
;   all return to the scan loop by an indirect jmp through variable scanret.
;
;
;****************************************************************************

	.xlist
	include		version.inc
SSID_ASM = ON
	IncludeOnce	context
	IncludeOnce	opid
	IncludeOnce	pcode		
	IncludeOnce	qbimsgs
	IncludeOnce	ssint
	IncludeOnce	txtmgr
	IncludeOnce	variable
	.list


assumes DS, DATA
assumes es, NOTHING
assumes ss, DATA


sBegin	SCAN
assumes cs, SCAN

subttl	Ss_IdLd
page
;***
;Ss_IdLd	Scan Ld variants of simple Id opcodes
;Purpose:
;
;   Scan the id variants opIdLd<type>.
;
;Algorithm:
;
;   Load opcode data type from the variable table
;   Calculate offset for <I|S|F|C> based on oPrsCur and variable table flags
;   Load and emit executor
;   Copy operand
;   Push stack entry
;      operand address + 2
;      type (with origin bits set)
;   Return to main loop
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
;
;******************************************************************
page
LdRedirect:
	    mov     ax,[bx].VAR_Value		    ;Value field has new oVar
	    mov     PTRTX[si],ax		    ;Replace old one
	    jmp     short IdLdRetry

SsProc	IdLd,Rude
	.erre	LOW OPCODE_MASK EQ 0ffh 	    
	test	byte ptr es:[si-1],HIGH (NOT OPCODE_MASK)
	jnz	IdLdRetry			    
	mov	dx,scanOFFSET mpLdImpOpExe	    
IdLdRetry:					    
		mov	bx,[MrsCur.MRS_bdVar.BD_pb] 
	    add     bx,PTRTX[si]		    
	DbChk	pVar,bx 			    ;Verify this is a variable

	;Check flags to see if we're really doing a load

	or	WORD PTR [bx].VAR_Flags,FV_STATICSET	; Note that we've seen it
	mov	ax,[bx].VAR_Flags		; Fetch flags
	    TestX   ax,FVREDIRECT		;Test for redirected variable
	    jnz     LdRedirect			;Go fix the redirected case
	    xor     ax,FVFUN+FVVALUESTORED
	    TestX   ax,FVFUN+FVVALUESTORED	;Test for function call
	jz	ExecFunc		;This is a function call
	call	SsIndexType		;DX = Executor address based on oTyp
					;CX = oTyp of variable
	TestX	ax,FVCONST		;Loading a constant?
	jnz	ConstLoad
	or	ch,HIGH ST_SimpVar	;Indicate the stack item is a var ref.
ScanTypeSet:
	call	SsIndexISFC		;Index by ISFC. Restores DS if SizeD
assumes ds,DATA				

ScanStack:
;Make stack entry
	push	di			;oTx for coercion insertion
	push	cx			;oTyp of variable or ET_RC
	jmp	[ScanRet]

ConstLoad:
	.errnz	LOW ST_LitX		; Assure we can use CH
	or	ch,HIGH ST_LitX		;Constant same as literal w/operand
	jmp	ScanTypeSet

ExecFunc:
	xor	cx,cx			;No operands
	jmp	SsCallFunc


subttl	Executor map for IdLd variants
page
;Table mpLdImpOpExe is a list of IdLdImp executors.  The list is ordered
;as follows:
;	exIdLd<I|S|F|C><type>
;After the Ld executors is Rf excutors for numeric & FS types only.
;
;This table is then followed by IdLdExp executors.
;Type "0" is used by record executors (implicits only).

	public	mpLdImpOpExe
mpLdImpOpExe	label	word			



mpIdISLd	label	word			
	DWEXT	exIdISRfTyp
	DWEXT	exIdISLd2
	DWEXT	exIdISLd4
	DWEXT	exIdISLdR4
	DWEXT	exIdISLdR8			
	DWEXT	exIdISLdSD			
	DWEXT	exIdISLdFS

cbContext   = $ - mpIdISLd		    ;Offset to next frame of executors

mpIdICLd	label	word			
	DWEXT	exIdICRfTyp
	DWEXT	exIdICLd2
	DWEXT	exIdICLd4
	DWEXT	exIdICLdR4
	DWEXT	exIdICLdR8			
	DWEXT	exIdICLdSD			
	DWEXT	exIdICLdFS
	.erre	cbContext EQ ($-mpIdICLd)	

mpIdIILd	label	word			
	DWEXT   exIdIIRfTyp
	DWEXT	exIdIILd2
	DWEXT	exIdIILd4
	DWEXT	exIdIILdR4
	DWEXT	exIdIILdR8			
	    DWEXT   exIdIFLd2			
	DWEXT	exIdIILdFS
	.erre	cbContext EQ ($-mpIdIILd)	

mpIdIFLd	label	word			
	DWEXT	exIdIFRfTyp
	DWEXT	exIdIFLd2
	DWEXT	exIdIFLd4
	DWEXT	exIdIFLdR4
	DWEXT	exIdIFLdR8			
	DWEXT	exIdIFLdSD			
	DWEXT	exIdIFLdFS
	.erre	cbContext EQ ($-mpIdIFLd)	

	;IdRfImp executor map

	public	IdLdtoRfMap
IdLdtoRfMap=	$-mpLdImpOpExe


mpIdISRf	label	word			
	DWEXT	exIdISRf
	DWEXT	exIdISRf
	DWEXT	exIdISRf
	DWEXT	exIdISRf
	    DWEXT   exIdISRf			
	DWEXT	exIdISRfFS
	.erre	cbContext EQ ($-mpIdISRf+2)	

mpIdICRf	label	word			
	DWEXT	exIdICRf
	DWEXT	exIdICRf
	DWEXT	exIdICRf
	DWEXT	exIdICRf
	    DWEXT   exIdICRf			
	DWEXT	exIdICRfFS
	.erre	cbContext EQ ($-mpIdICRf+2)	

;  The executors for loading two byte and four byte frame variables are
;  also used for referenced to indirect variables.  For a load of a frame
;  variable, the variable table contains the value which is pushed on the
;  stack.  For a reference to an indirect variable, the variable table
;  contains the address which is then pushed on the stack.  Since addresses
;  are either two or four bytes, only these entries serve two purposes.

mpIdIIRf	label	word			
	DWEXT	exIdIIRf
	DWEXT	exIdIIRf
	DWEXT	exIdIIRf
	DWEXT	exIdIIRf
	    DWEXT   exIdIFLd2			
	DWEXT	exIdIIRfFS
	.erre	cbContext EQ ($-mpIdIIRf+2)	

mpIdIFRf	label	word			
	DWEXT	exIdIFRf
	DWEXT	exIdIFRf
	DWEXT	exIdIFRf
	DWEXT	exIdIFRf
	    DWEXT   exIdIFRf			
	DWEXT	exIdIFRfFS
	.erre	cbContext EQ ($-mpIdIFRf+2)	

	;Function call executors

	public	IdLdtoFuncMap
IdLdtoFuncMap=	$-mpLdImpOpExe

	DWFILL					
	DWEXT	exFunc0ArgImp
	DWEXT	exFunc0ArgImp
	DWEXT	exFunc0ArgImp
	DWEXT	exFunc0ArgImp
	DWEXT	exFunc0ArgImp

	;Note:	The following word is used by MakeRef to
	;find the implicit map from the explicit map.

	DW	mpLdImpOpExe			

	public	mpLdExpOpExe
mpLdExpOpExe	label	word			;No record type


mpIdESLd	label	word			
	DWFILL
	DWEXT	exIdESLdI2
	DWEXT	exIdESLdI4
	DWEXT	exIdESLdR4
	DWEXT	exIdESLdR8
	DWEXT	exIdESLdSD
	DWEXT	exIdESLdFS
	.erre	cbContext EQ ($-mpIdESLd)	

mpIdECLd	label	word			
	DWFILL					
	DWEXT	exIdECLdI2
	DWEXT	exIdECLdI4
	DWEXT	exIdECLdR4
	DWEXT	exIdECLdR8
	DWEXT	exIdECLdSD
	DWEXT	exIdECLdFS
	.erre	cbContext EQ ($-mpIdECLd)	

mpIdEILd	label	word			
	DWFILL					
	DWEXT	exIdEILdI2
	DWEXT	exIdEILdI4
	DWEXT	exIdEILdR4
	DWEXT	exIdEILdR8
	DWEXT	exIdEIRfSD
	DWEXT	exIdEILdFS
	.erre	cbContext EQ ($-mpIdEILd)	

mpIdEFLd	label	word			
	DWFILL					
	DWEXT	exIdEFLdI2
	DWEXT	exIdEFLdI4
	DWEXT	exIdEFLdR4
	DWEXT	exIdEFLdR8
	DWEXT	exIdEFRfSD
	DWEXT	exIdEFLdFS
	.erre	cbContext EQ ($-mpIdEFLd)	

	;IdRfExp executor map

	.erre	IdLdtoRfMap EQ ($-mpLdExpOpExe) 


mpIdESRf	label	word			
	DWEXT	exIdESRfI2
	DWEXT	exIdESRfI4
	DWEXT	exIdESRfR4
	DWEXT	exIdESRfR8
	DWEXT	exIdESRfSD
	DWEXT	exIdESRfFS
	.erre	cbContext EQ ($-mpIdESRf+2)	

mpIdECRf	label	word			
	DWEXT	exIdECRfI2
	DWEXT	exIdECRfI4
	DWEXT	exIdECRfR4
	DWEXT	exIdECRfR8
	DWEXT	exIdECRfSD
	DWEXT	exIdECRfFS
	.erre	cbContext EQ ($-mpIdECRf+2)	

mpIdEIRf	label	word			
	DWEXT   exIdEIRfI2
	DWEXT   exIdEIRfI4
	DWEXT   exIdEIRfR4
	DWEXT	exIdEIRfR8
	DWEXT	exIdEIRfSD
	DWEXT	exIdEIRfFS
	.erre	cbContext EQ ($-mpIdEIRf+2)	

mpIdEFRf	label	word			
	DWEXT	exIdEFRfI2
	DWEXT	exIdEFRfI4
	DWEXT	exIdEFRfR4
	DWEXT	exIdEFRfR8
	DWEXT	exIdEFRfSD
	DWEXT	exIdEFRfFS
	.erre	cbContext EQ ($-mpIdEFRf+2)	

;Function call executors
	DWFILL					
	DWEXT	exFunc0ArgI2
	DWEXT	exFunc0ArgI4
	DWEXT	exFunc0ArgR4
	DWEXT	exFunc0ArgR8
	DWEXT	exFunc0ArgSD


subttl	Ss_IdSt
page
;***
;Ss_IdStExp, Ss_IdStImp	Scan St variants of simple Id opcodes
;Purpose:
;
;   Scan the id variants opIdSt<type>.
;
;   These routines expect only fundamental BAsiC data types.  User types
;   are handled by opIdRf variants followed by OpIdOffset opcodes.
;	
;Algorithm:
;	
;   Load exe map address
;   Load opcode data type from the variable table
;   Compare with type on stack, coerce if different
;   Calculate offset for <I|S|F|C> based on oPrsCur and variable table flags
;   Load and emit executor
;   Copy operand
;   Return to main loop
;
;Input:
;	es:si = pcode emission address
;Output:
;	si updated
;Modifies:
;Exceptions:
;	Ss_Error
;******************************************************************
page

StRedirect:
	    mov     ax,[bx].VAR_Value		    ;Value field has new oVar
	    mov     PTRTX[si],ax		    ;Replace old one
	    jmp     short IdStRetry

SsProc	IdSt,Rude
	.erre	LOW OPCODE_MASK EQ 0ffh 	    
	test	byte ptr es:[si-1],HIGH (NOT OPCODE_MASK)
	jnz	@F				    
	mov	dx,scanOFFSET mpStImpOpExe	    
@@:						    
	test	[SsFlags],SSF_ScanAndExec	    ;Storing a constant?
	jnz	StoreConst
IdStRetry:
		mov	bx,[MrsCur.MRS_bdVar.BD_pb] 
	    add     bx,PTRTX[si]		    
	DbChk	pVar,bx 			    ;Verify this is a variable

	;Check flags to see if we're really doing a store

	or	WORD PTR [bx].VAR_Flags,FV_STATICSET ; Note that we've seen it
	mov	ax,[bx].VAR_Flags		; Fetch flags
	    TestX   ax,FVREDIRECT		;Test for redirected variable
	    jnz     StRedirect			;Go fix the redirected case
	    xor     ax,FVFUN+FVVALUESTORED
	    TestX   ax,FVFUN+FVVALUESTORED	;Test for function call
	jnz	IdStore 		    ;Either not function or not static

;Trying to store into function RetVal when not within function defintion
	mov	ax,ER_DD			;Duplicate definition
	call	SsError
IdStore:
	call	SsIndexType		;DX = Executor address based on oTyp
					;CX = scan stack oTyp of variable
	jcxz	NeedOtyp		;Is it a record?
HavOtyp:				
	call	SsIndexISFC		;Index by ISFC
assumes ds,DATA				
StoreCoerce:
	xchg	ax,cx			;AX = oTyp of assignment target
	call	EnsureArgType		;Check type and possibly insert coercion
	test	[SsBosFlags],SSBOSF_Const ;In a CONST statement?
	jz	IdStX
	mov	al,[SsExecTmp]
	mov	[SsExecFlag],al		;Restore OPA_fExecute flag
IdStX:
	jmp	[ScanRet]

NeedOtyp:				
	mov	cx,[bx].VAR_oTyp	;Get real oTyp for coercion
	jmp	HavOtyp			

StoreConst:
	mov	bl,byte ptr es:[si-1]   ; Get HIGH opcode
	.erre	OPCODE_MASK EQ 3ffh	
	shr	bl,1			
	shr	bl,1			;Align explicit type
	jnz	HavConstType		;If explicit, type is in bx
	pop	bx			;If implicit, get type from stack
	push	bx			;Restore oTyp on stack
HavConstType:
	xor	bh,bh
	mov	mkVar.MKVAR_oTyp,bx
	mov	ax,PTRTX[si]		;Get oNam
	mov	mkVar.MKVAR_oNam,ax
	or	mkVar.MKVAR_flags,FVI_LVAL+FVI_CONST	
	push	dx			;Save executor map
	call	MakeVariableFar 	
	GETSEGTXTCUR			
	pop	dx
	or	ax,ax			;Error?
	js	ConstErr
	mov	PTRTX[si],ax		;Set oVar
	mov	bx,si
	sub	bx,[SsCbTxExpand]	;Compute position in unscanned code
	sub	bx,[SsScanExStart]	;Relative beginning of this scan
	add	bx,[SsScanExSrc]	;Position in original text
	mov	PTRTX[bx],ax		;Set oVar in original source
	jmp	IdStRetry

ConstErr:
	call	SsError			;Save MakeVariable error code
	MOVSWTX				;Copy oNam field
	pop	ax
	pop	bx			;Pop off stack entry
	jmp	[ScanRet]


;Table mpStImpOpExe is a list of IdStImp executors.  The list is ordered
;as follows:
;	exIdSt<I|S|F|C><type>
;This table is then followed by IdStExp executors.
;Type "0" entries are used for record executors (implicit types only).

	public	mpStImpOpExe
mpStImpOpExe	label	word			


mpIdISSt	label	word			
	DWEXT	exIdISStTyp
	DWEXT	exIdISSt2
	DWEXT	exIdISSt4
	DWEXT	exIdISStR4
	DWEXT	exIdISStR8			
	DWEXT	exIdISStSD
	DWEXT	exIdISStFS
	.erre	cbContext EQ ($-mpIdISSt)	

mpIdICSt	label	word			
	DWEXT	exIdICStTyp
	DWEXT	exIdICSt2
	DWEXT	exIdICSt4
	DWEXT	exIdICStR4
	DWEXT	exIdICStR8			
	DWEXT	exIdICStSD
	DWEXT	exIdICStFS
	.erre	cbContext EQ ($-mpIdICSt)	

mpIdIISt	label	word			
	DWEXT	exIdIIStTyp
	DWEXT	exIdIISt2
	DWEXT	exIdIISt4
	DWEXT	exIdIIStR4
	DWEXT	exIdIIStR8			
	DWEXT	exIdIIStSD
	DWEXT	exIdIIStFS
	.erre	cbContext EQ ($-mpIdIISt)	

mpIdIFSt	label	word			
	DWEXT	exIdIFStTyp
	DWEXT	exIdIFSt2
	DWEXT	exIdIFSt4
	DWEXT	exIdIFStR4
	DWEXT	exIdIFStR8			
	DWEXT	exIdIFStSD
	DWEXT	exIdIFStFS
	.erre	cbContext EQ ($-mpIdIFSt)	

	public	mpStExpOpExe
mpStExpOpExe=	$-2				;No record type

	;Note:	One word is saved by basing the executor maps at the
	;current position minus one.  This is acceptable since there
	;are no explicit references to records.  However, when publics
	;are present the static map is no longer first and must have
	;a word of filler for record references.  This word is defined
	;inside the conditional for publics.


mpIdESSt	label	word			
	DWEXT	exIdESStI2
	DWEXT	exIdESStI4
	DWEXT	exIdESStR4
	DWEXT	exIdESStR8
	DWEXT	exIdESStSD
	DWEXT	exIdESStFS
	.erre	cbContext EQ ($-mpIdESSt+2)	

mpIdECSt	label	word			
	DWFILL					
	DWEXT	exIdECStI2
	DWEXT	exIdECStI4
	DWEXT	exIdECStR4
	DWEXT	exIdECStR8
	DWEXT	exIdECStSD
	DWEXT	exIdECStFS
	.erre	cbContext EQ ($-mpIdECSt)	

mpIdEISt	label	word			
	DWFILL					
	DWEXT	exIdEIStI2
	DWEXT	exIdEIStI4
	DWEXT	exIdEIStR4
	DWEXT	exIdEIStR8
	DWEXT	exIdEIStSD
	DWEXT	exIdEIStFS
	.erre	cbContext EQ ($-mpIdEISt)	

mpIdEFSt	label	word			
	DWFILL					
	DWEXT	exIdEFStI2
	DWEXT	exIdEFStI4
	DWEXT	exIdEFStR4
	DWEXT	exIdEFStR8
	DWEXT	exIdEFStSD
	DWEXT	exIdEFStFS
	.erre	cbContext EQ ($-mpIdEFSt)	



subttl	SsGetIsfc
;SsGetISFC - return exe map offset for scope
;Input:
;	bx = pVar
;	dx = index calculated so far
;Output:
;	dx = exe map offset updated for scope
;Modifies:
;	none
;Preserves:
;	none
public	SsGetISFC,GetCxISFC


SsGetISFC:
	mov	cx,cbContext
GetCxISFC:
	    mov     ax,[bx].VAR_Flags		; Fetch flags
	    TestX   ax,FVVALUESTORED		;Test for STATIC
	    jnz     @F				;STATIC variable
		add	dx,cx			;Move to COMMON executors
		TestX	ax,FVCOMMON
		jnz	@F			;COMMON variable (oCommon
						;allocated inCOMMON statement)
	    add     dx,cx			;Move to INDIRECT executors
	    TestX   ax,FVFORMAL
	    jnz     @F				;INDIRECT variable

	add	dx,cx				;Must be a FRAME variable
	call	SsAllocOFrame			;Frame variables have an oFrame
@@:
	ret

sEnd	SCAN
	end
