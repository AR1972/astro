page	49,132
	TITLE	ssrec	- Scan support for records
;***
;ssrec.asm - Scan support for records
;
;	Copyright <C> 1986, Microsoft Corporation
;
;
;****************************************************************************

	.xlist
	include 	version.inc
SSREC_ASM = ON
	IncludeOnce	context
	IncludeOnce	pcode		
	IncludeOnce	qbimsgs
	IncludeOnce	ssint
	IncludeOnce	txtmgr		
	IncludeOnce	variable
	.list


assumes DS, DATA
assumes es, NOTHING
assumes ss, DATA

extrn	oVarOfRetVal:far		

sBegin	CODE				

	extrn	exSave87:near		
	extrn	exNop0:near		

sEnd	CODE				


sBegin	SCAN
assumes cs, SCAN

subttl	Ss_Off
page
;***
;Ss_Off<Ld|St> - Scan record offset ID's
;Purpose:
;	Scan the id variants opOff<Ld|St>.
;
;******************************************************************

SsProc	OffLd,rude
	.erre	LOW OPCODE_MASK EQ 0ffh 	    
	test	byte ptr es:[si-1],HIGH (NOT OPCODE_MASK)
	jnz	@F				    
	mov	dx,scanOFFSET mpOffLdImpOpExe	    
@@:						    
		mov	bx,[MrsCur.MRS_bdVar.BD_pb] 
	    add     bx,PTRTX[si]		    ; BX = pElem

	mov	cx,[bx].ELEM_oTyp	;CX = oTyp
MapOffLd:
	call	SsIndexTypeCx		;DX = executor address based on oTyp
	call	SsEmitExecutor		;Also restores DS if SizeD
assumes ds,DATA				
	pop	ax			;Get old type
	.errnz	LOW ST_ArrayBit 	; Assure we can use AH
	and	ah,HIGH ST_ArrayBit
	or	ch,ah			;Remember if it came from array
	.errnz	LOW ST_RecVar		; Assure we can use CH
	or	ch,HIGH ST_RecVar
	pop	ax			;Clean off old insertion address
	push	di			;oTx for coercion insertion
	push	cx			;oTyp of record element or ET_RC
	jmp	[ScanRet]



SsProc	OffSt,rude
	.erre	LOW OPCODE_MASK EQ 0ffh 	    
	test	byte ptr es:[si-1],HIGH (NOT OPCODE_MASK)
	jnz	@F				    
	mov	dx,scanOFFSET mpOffStImpOpExe	    
@@:						    
		mov	bx,[MrsCur.MRS_bdVar.BD_pb] 
	    add     bx,PTRTX[si]		    ; BX = pElem

	mov	cx,[bx].ELEM_oTyp	;CX = oTyp of element
	call	SsIndexTypeCx		;DX = executor address based on oTyp
					;CX = scan stack oTyp of variable
	jcxz	NeedOtyp		;Is it a record?
HavOtyp:				

		add	sp,4			; Remove parent stack frame

	call	SsEmitExecutor		;Also restores DS if SizeD
assumes ds,DATA				

	xchg	ax,cx			; AX = oTyp of target element
	call	EnsureArgType		; Pop stack frame test for coerce
	jmp	[ScanRet]

NeedOtyp:				
	mov	cx,[bx].ELEM_oTyp	;Get real oTyp for coercion
	jmp	HavOtyp			


	public	mpOffLdImpOpExe
mpOffLdImpOpExe label	word			    
	DWEXT	exOffIRf
	DWEXT	exOffILdI2
	DWEXT	exOffILd4
	DWEXT	exOffILdR4
	DWEXT	exOffILdR8			    
	DWEXT	exOffILdSD			    
	DWEXT	exOffILdFS

	;Rf opcodes for numeric & FS types

OffLdtoRfMap=	$-mpOffLdImpOpExe

mpOffIRf    label   word			    ;[10]
	DWEXT	exOffIRf
	DWEXT	exOffIRf
	DWEXT	exOffIRf
	DWEXT	exOffIRf
	DWEXT	exOffIRfSD			    
	DWEXT	exOffIRfFS

RfCnt	=	$-mpOffIRf			    

	;Note:	The following word is used by MakeRef to
	;find the implicit map from the explicit map.

	DW	mpOffLdImpOpExe 		    

	public	mpOffLdExpOpExe
mpOffLdExpOpExe label	word			    
	DWFILL					    
	DWEXT	exOffELdI2
	DWEXT	exOffELdI4
	DWEXT	exOffELdR4
	DWEXT	exOffELdR8
	DWEXT	exOffELdSD			    
	DWEXT	exOffELdFS

	;Rf opcodes for numeric & FS types

	.erre	OffLdtoRfMap EQ ($-mpOffLdExpOpExe) 

mpOffERf    label   word			    
	DWEXT	exOffERfI2
	DWEXT	exOffERfI4
	DWEXT	exOffERfR4
	DWEXT	exOffERfR8
	DWEXT	exOffERfSD			    
	DWEXT	exOffERfFS
	.erre	RfCnt EQ ($-mpOffERf)		    

mpOffStImpOpExe label	word			    
public	mpOffStImpOpExe
	DWEXT	exOffIStTyp
	DWEXT	exOffIStI2
	DWEXT	exOffISt4
	DWEXT	exOffIStR4
	DWEXT	exOffIStR8			    
	DWEXT	exOffIStSD			    
	DWEXT	exOffIStFS


	public	mpOffStExpOpExe
mpOffStExpOpExe label	word			    
	DWFILL					    
	DWEXT	exOffEStI2
	DWEXT	exOffEStI4
	DWEXT	exOffEStR4
	DWEXT	exOffEStR8
	DWEXT	exOffEStSD			    
	DWEXT	exOffEStFS



page
;***
;MakeRef - Turn Id/Off Ld into Rf
;
;   Converts IdLd, AIdLd, and OffLd executors to Rf variants.
;   Record types are already Rf's so they're not changed.
;
;Inputs:
;	ax = Type word (from stack) of operand
;	bx = oTx of word after opcode to convert
;Outputs:
;	ax = New Id/Off Rf executor (if no error)
;Preserves:
;	bx,cx,dx,es
;Modifies:
;	none

public	MakeRef
extrn	IdLdtoRfMap:abs,GetCxISFC:near

RefArgErrPop:
	pop	ax
RefArgError:
	mov	ax,ER_VarReq
	call	SsErrorBx
	jmp	SameEx

MakeRef:
	push	bx			
	push	cx
	push	dx			;save caller's dx
	TestX	ax,ST_Var?		;Is it a variable?
	jnz	RefVariable
;Not a variable. Check to see if it's a function return value.
;Note that [bx-2] has the oVar, and [bx-4] is either the opcode (if simple
;IdLd) or the count of indices (if AIdLd).  AIdLd would imply a function
;call with arguments, and is never legal here.  The max number of arguments
;is 64, but the none of these executors are in the first 64 bytes of their
;segment.  Thus by comparing [bx-4] with 64 we can determine if it is a count
;of indices or an executor, that is, whether it is an AIdLd or IdLd.
	cmp	PTRTX[bx-4],64		;Is it a count of indices or pcode?
	jbe	RefArgError		;Error if count--must be function call
	push	ax			;Save oTyp
	mov	ax,PTRTX[bx-2]		;Get oVar of function
	DbChk	oVar,ax 		;Verify that this is a variable
	push	bx			
	PUSH_ES 			
	call	oVarOfRetVal		;Convert to oVar of return value
	POP_ES				
	pop	bx			
	or	ax,ax			
	js	RefArgErrPop		;Not function return value

	mov	PTRTX[bx-2],ax		;Update oVar
	pop	ax			;Get oTyp back
	.errnz	LOW ST_SimpVar		; Assure we can use only AH
	or	ah,HIGH ST_SimpVar	;Treat function RetVal like simple var.

	;Because this reference was originally handled as a function call,
	;an exSave87 executor was inserted prior to the call.  Not having
	;any parameters, the exSave87 will reside immediately before the
	;call executor.

	DbAssertRel PTRTX[bx-6],e,<codeOFFSET exSave87>,SCAN,<MakeRef: exSave87>
	mov	PTRTX[bx-6],codeOFFSET exNop0	

;This pcode was originally scanned to a recursive function call with no 
;arguments.  This may have generated a delayed Argument Count Mismatch 
;error if this function actually has parameters.  Check to see if it has
;parameters and clean up the error if so.

	cmp	[prsCur.PRS_cwParams],0	;Any parameters on this function?
	je	@F			;If not, no Arg Cnt Err would have occured
	dec	[SsDelayCnt]		;Peel back most recent error
@@:
	    ;A delayed error will also occur if this is a DefFn, caused
	    ;by logic that prevents recursive calls in a DefFn.

	    cmp     [prsCur.PRS_procType],PT_DEFFN  ;In DefFn?
	    jne     RefVariable 	;If not, no error was recorded
	    dec     [SsDelayCnt]	;Peel back the ER_UF error
RefVariable:
	mov	dx,PTRTX[bx-2]		;Get oVar/oElem
	sub	bx,4			;Point to opcode if not array
	TestX	ax,ST_Array?		;Is it array?
	jnz	opPoint
	dec	bx
	dec	bx			;Must back up over another operand
opPoint:
	push	bx			;Save oTx of executor address
	mov	bx,PTRTX[bx]		;Get executor
	GetCodeIntoDs	SCAN		
	mov	bx,[bx-2]		;Get opcode in front of executor
	push	ss
	pop	ds
	mov	cx,bx				    ; Save opcode
	and	bx,OPCODE_MASK			    ; Clear upper bits
	shl	bx,1				    ;Use it as word table index
	mov	bx,mpOpExe[bx]			    ;Executor address map for Ld
	TestX	cx,<NOT OPCODE_MASK>		    ; Implicit?
	jnz	@F				    ; Brif explicit
	mov	bx,word ptr cs:[bx-2]		    ; Fetch implicit map
@@:						    
	xchg	dx,bx				    ;Now dx=map, bx=oVar/oElem
	TestX	ax,ST_Record?			    ;Is it a record?
	jnz	RecVar				    ;Record - no flags
	push	ax
		add	bx,[MrsCur.MRS_bdVar.BD_pb] ; oVar --> pVar
	DbChk	pVar,bx 			    
	mov	cx,RfCnt
	call	GetCxISFC		;Calculate <I|S|F|C> offset from
					;   bx (MSV flags) and oPrsCur
					;Offset returned as modified dx
	pop	ax
	add	dx,IdLdtoRfMap-OffLdtoRfMap ;Shift to Rf map from Ld map
RecVar:
	add	dx,OffLdtoRfMap-2*ET_I2
	pop	bx			;Get oTx of executor
	and	ax,ST_Typ_Mask		; Clear flag bits
	.erre	ET_RC EQ 0		; Assure JZ is sufficient
	jz	SameEx			; Brif record. Ex is already an Rf
MapRef:
	add	dx,ax			;1 word per type in table
	add	dx,ax
	xchg	bx,dx
	mov	ax,WORD PTR cs:[bx]	;Load executor
	xchg	dx,bx
	mov	PTRTX[bx],ax		;Set new opcode
SameEx:
	pop	dx			;restore caller's dx,cx
	pop	cx
	pop	bx			
	ret

sEnd	SCAN
	end
