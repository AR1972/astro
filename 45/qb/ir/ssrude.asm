page	49,132
	TITLE	ssrude - Scan and Descan support routines for SS_RUDE mode
;***
;ssrude - Scan and descan support for SS_RUDE mode
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;	Certain opcodes require work to scan from SS_RUDE to SS_PARSE and
;	back.  This module contains most of the dispatched entrypoints
;	for performing this work.
;
;	The connection between scan routines and descan routines is as
;	follows:
;	- Each scan routine is declared with macro SsVProc.  If there is
;	  work required to get to/from SS_RUDE then the optional parameter
;	  "rude" is specified on the SsVProc invocation.  Each scan dispatch
;	  so declared will have the address of the rude mode scan/descan
;	  routine stored in the scanner segment at the SsVProc entrypoint -
;	  SsVProcRude.
;	- The scan and descan main loop will vector through the above address
;	  when the mpOpAtr entry for the opcode indicates that work is required
;	  to get to or from SS_RUDE mode.
;	- The dispatched routine will determine whether the current operation
;	  is scan or descan by branching on PSW.Z.  PSW.Z set indicates
;	  descan from SS_PARSE to SS_RUDE.  PSW.Z clear indicates scan from
;	  SS_RUDE to SS_PARSE.
;
;	Rude scan/descan dispatches are entered with the following convention:
;	ax = oTyp from rule table (for opId's only)
;	cx = opcode
;	dx = pVariable table  (Descan to SS_RUDE only)
;	si = descan source
;	di = descan destination
;	es = text segment
;	PSW.Z set indicates SS_PARSE -> SS_RUDE
;
;       Special Cases:
;	-------------
;	CONSTants
;		When an opStConst statement is seen, we continue scanning
;		IdLd pcodes normally, except that we give an error if 
;		MakeVariable finds one of these that is not already created
;		as a CONSTant.  Each time we find an IdSt pcode, we call
;		ScanAndExec with the pcode expression we've just scanned
;		ScanAndExec calls MakeVariable to create the new CONSTant
;		variable, scans the expression to SS_EXECUTE, executes it
;		(thus storing the value of the expression in the CONSTant),
;		and then descans the expression back to SS_PARSE.
;
;
;*****************************************************************************

	.xlist
	include		version.inc
SSRUDE_ASM	= ON
	IncludeOnce	architec
	IncludeOnce	context
	IncludeOnce	names
	IncludeOnce	opcodes
;	IncludeOnce	opid
;	IncludeOnce	opmin
	IncludeOnce	optables
	IncludeOnce	parser
	IncludeOnce	pcode
	IncludeOnce	qbimsgs
	IncludeOnce	scanner
	IncludeOnce	ssint
	IncludeOnce	txtmgr
	IncludeOnce	ui
	IncludeOnce	variable
	.list

;Invoked after calling any function which could cause heap movement
; and thus, movement of the current text table being scanned.
; This macro preserves all registers except ES (including flags)
;
SsRefreshES MACRO
	GETSEGTXTCUR			; ES = current pcode segment,
					; all other registers preserved,
					; including flags
	ENDM


assumes ds, DATA
assumes es, NOTHING
assumes SS, DATA

sBegin	DATA
	globalW	oNamOfPrsCur,UNDEFINED	;Speed optimization for rude scan
					;  set up here, used by varmgr
	staticW	otxConstCur,0		;otx to start of current CONST expr
					;  if scanning a CONST stmt, else 0
	staticW	varFlags_Reset,0	;Normally 0, set to FVI_SHARED, 
					;  FVI_STATIC, FVI_COMMON, FVI_AUTO,
					;  or FVI_PUBLIC whenever one of those 
					;  bits must be temporarily reset
					;  in mkVar.flags by IdLd. 
					;  Always reset by HandleId.
	staticB fErrWithinOp,0		;set to 1 if an error occurred within
					;  the operand of a big opcode (like
					;  opStDeclare), so error can be
					;  reported at the operand.
	staticW spErrorRestore,0	;used for error recovery
sEnd	DATA

sBegin	SCAN
assumes cs,SCAN
page

;***
;SsRudeScan
;Purpose:
;	Move text between SS_PARSE and SS_RUDE.
;
;	This routine dispatches to the same entrypoint for moving
;	text in either direction.  The individual entrypoints must
;	examine PSW to determine which direction to take the current
;	opcode.
;
;	Rude mode register convention is:
;	ax	 = opcode
;	dx	 = mrsCur.MRS_bdVar_pb (if not SizeD)
;	es:si/di = source and destination pointers
;	PSW.Z	   set indicates descan from SS_PARSE to SS_RUDE
;		   reset indicates scan from SS_RUDE to SS_PARSE
;
;	Dispatching is performed for all opcodes.  Table space is
;	conserved by placing the rude dispatch as a prefix to the non-rude
;	scan dispatch entrypoint for the opcode.  Note that this restriction
;	limits the resolution of the rude scan dispatching to that of the
;	standard scan dispatching.  In other words, there are no two opcodes
;	with individual rude dispatches that have a common standard scan
;	dispatch.
;
;	A further space savings is that not all standard scan dispatches
;	have rude dispatch prefixes.  The scan entrypoint declaration macro
;	SsProc has a second parameter that determines whether the rude
;	dispatch is required.  The opcode atribute table mpOpAtr contains
;	a bit (OPA_fSsRude) that indicates to the rude scan loop whether
;	there is work to go to/from SS_RUDE for that opcode.  All opcodes
;	that do not have this bit set are dispatched to a common entrypoint
;	that skips operands to the next opcode.
;
;	The rude scans are wholly space invarient.  The rude scan is
;	a scan in place - it is not necessary for dispatch points to emit
;	text that is not modified (for instance, it is not necessary to
;	emit the opcode).
;
;	Descanning from SS_PARSE to SS_RUDE can cause no errors.
;
;	Scanning from SS_RUDE to SS_PARSE can cause errors.  When a dispatch
;	point finds an error it must:
;	- record the variable in a static variable.
;	- recover the opcode that it is processing to a consistent (RUDE or
;	  PARSE mode state (all operands must be in the same state).
;	- replace the first SS_RUDE mode opcode in the text table with an
;	  opEot.
;	- restart SsRudeScan to descan the pcode up to the opEot.
;	- replace the opEot with the saved opcode.
;	- return, reporting the error.
;
;NOTE:	This is not a recursive scheme; an alternate reentry point is
;	used to descan the pcode, and the opEot detects whether an error
;	has occurred or not. Aside from the above mechanism, this routine
;	is NOT reentrant.
;
;	Error control variables are:
;	SsErrOTx	- text offset of pcode which was replaced by opEot
;				descanner expects this to be UNDEFINED if no
;				error has occured.
;	SsErr		- error code to be returned by scanner.
;	SsErrOpcode	- opcode which was replaced by opEot
;
;Entry:
;	parmW	target scan state (must be SS_RUDE or SS_PARSE)
;Exit:
;	ax == 0  ----> Output registers are the same as for GetTXDInfo
;	ax != 0  ----> ax = error code
;                      [grs.GRS_otxCur] = text offset to error.  If low
;		       bit is set, error is within an operand (like opStSub,
;		       opStDeclare, etc.), so caller can position cursor
;		       exactly to the error token.
;
;************************************************************
cProc	SsRudeScan,<PUBLIC,FNEAR>,<si,di>	
	parmB	TargetState
cBegin

	mov	[spErrorRestore],sp	;in case an error occurs with 
					;  stack in an intermediate state
	sub	ax,ax
	mov	[fErrWithinOp],al
	dec	ax			;ax = UNDEFINED
	mov	[SsErrOTx],ax		;initialize in case we're descanning	
	cmp	[TargetState],SS_RUDE	;Indicate scan or descan
	jz	SsRudeDeScan		;brif descanning to SS_RUDE

	call	OtxDefType0Far		; set all 26 letter default
					;	types to initial default
SkipDT:
	cmp	[grs.GRS_fDirect],FALSE
	jz	Not_Direct		;brif not scanning direct mode buffer

	push	[grs.GRS_otxCONT]	; In direct mode,
	call	OtxDefTypeCurFar	; set all 26 letter defaults to
					; values at the current program counter.
					; If cant CONT (i.e. UNDEFINED), set
					; them to their value at EOT
Not_Direct:
	DbAssertRelB [TargetState],z,SS_PARSE,SCAN,<ssrude: invalid target state>
	cmp	[grs.GRS_oPrsCur],UNDEFINED
	jz	SsRudeScanErr		;brif not scanning a procedure table

	cmp	[grs.GRS_fDirect],FALSE
	jnz	SsRudeScanErr		;ignore prsCur stuff if in Direct Mode

	push	[prsCur.PRS_ogNam]	
	call	ONamOfOgNamFar		;must succeed (as prs already exists)
	DbAssertRel  ax,nz,0,SCAN,<SsRudeScan: [6] ONamOfOgNam returned an error>
	mov	[oNamOfPrsCur],ax	;speed optimization - used by varmgr

	call	MakePrsTVar		;make var hash table for prsCur
	or	ax,ax			;error return?
	jnz	SsRudeScanErr		;  brif not
	
	PUSHI	ax,ER_OM		;insufficient memory - - exit
	jmp	ErrExit

SsRudeDeScan:				;only get here if we're descanning
	DbAssertRelB [grs.GRS_fDirect],z,FALSE,SCAN,<ssrudedescan: fDirect TRUE>
	;don't want to reset otxCONT when dealing with the Direct Mode buffer,
	;  so we're counting on the fact that we never descan the D.M. buffer
	mov	[grs.GRS_otxCONT],UNDEFINED
					;ensure this is correctly set whenever
					;  we descan any text table
SsRudeScanErr:				;reentry point in case of scan error
	SsRefreshES			;es = cur pcode seg (heap movement)
	mov	dx,[mrsCur.MRS_bdVar.BD_pb] ;Address of variable table
	SetStartOtx	si		;Start scan from the top
	mov	di,si			;Destination = source

RudeLoop:
	LODSWTX 			;Pick up opcode
	mov	bx,ax
	and	bx,OPCODE_MASK		; ax is actual opcode, bx is
					; masked opcode
	DbAssertRel	bx,be,op_max,SCAN,<Rude Scan Loop: opcode out of range.>
	DbAssertRel	spErrorRestore,e,sp,SCAN,<Rude Scan Loop: Stack Use Err.>
	DbAssertRel	si,ae,di,SCAN,<Rude Scan Loop: Emit overran source.>
	DbAssertRel	es,z,EScheckRude,SCAN,<Rude Scan Loop: ES not preserved>
	mov	cl,mpOpAtr.[bx]		;load attribute byte
	test	cl,OPA_fSsRude		;Test for scan work for rude mode
	jnz	RudeScanWork		;Work required to get to SS_RUDE

Ssv_NOps:				;Skip opcode and operands
	and	cx,OPA_CntMask		;get the operand count from attribute
	.errnz	OPA_CntMask AND 0FF00H	;must use cx in next line if non-zero
	cmp	cl,OPA_CntMask		;check for cnt field in operand
	jz	Fetch_Cnt		;  brif there is a cnt field
Bump_TxtPtr:
	add	si,cx
	jmp	RudeLoop		;start over for next operand

Fetch_Cnt:
	LODSWTX				;load the cnt field
	mov	cx,ax
	inc	cx
	and	cl,0FEH			;round up to even byte count
	jmp	short Bump_TxtPtr

RudeScanWork:
	mov	di,si			;Bring destination in line with source
	mov	al,ah			; al contains oTyp for opId's,
					; but left shifted
	SHIFT	H,R,al,2		; shift al right two bits
	.errnz	OPCODE_MASK - 03FFH	; 03FF mask implies it's shifted
					; left two bits.
	cbw				;ax = oTyp for opId's
	mov	cx,bx			; cx = masked opcode
	shl	bx,1			;To word offset
	mov	bx,[bx].mpOpScanDisp	;Scan dispatch address
	DbPub DispSSR			;a handy public for debugging
	cmp	[TargetState],SS_RUDE	;Indicate scan or descan
DispSSR:
	jmp	word ptr cs:[bx].SsProcRude	;Dispatch to RUDE scanner

;***
;SsVProc Eot
;Purpose:
;	Rude scan/descan dispatch.
;
;Input:
;	Standard rude dispatch
;Output:
;************************************************************
SsVProc Eot
	jnz	SsRudeOkX		;if scanning and got to opEot, no errors

	mov	ax,UNDEFINED
	cmp	[grs.GRS_fDirect],FALSE
	jnz	Eot_Cont1

	mov	bx,dataOFFSET mrsCur
	cmp	[grs.GRS_oPrsCur],ax
	jz	Eot_Cont		;brif not scanning a procedure table

	mov	bx,dataOFFSET prsCur
	mov	[prsCur.PRS_oVarHash],ax
					;must ensure this gets reset here, in
					;  case we were called to descan a
					;  prs with an empty text table
Eot_Cont:
	.errnz	PRS_cbFrameVars - MRS_cbFrameVars
	mov	[bx.PRS_cbFrameVars],-FR_FirstVar	
					; reset to init. value. This value
					; is 2 to account for the fact that
					; b$curframe is always pushed on the
					; stack after bp, so we treat this
					; word as a frame var for ref'ing
					; the real frame vars off of bp
Eot_Cont1:
					;reset to default
	cmp	[SsErrOTx],ax		;did we just descan due to a scan err?
	jz	SsRudeOkX		;No error - exit.

	mov	di,[SsErrOTx]
	mov	ax,[SsErrOpcode]
	STOSWTX				;restore the saved opcode in text table
	push	[SsErr]			;in case ModuleRudeEdit changes this
ErrExit:
	cmp	[grs.GRS_fDirect],FALSE
	jnz	Eot_Err_Exit		;don't call ModuleRudeEdit if error
					;  in scanning Direct mode buffer - -
					;  no need to do so.
	call	ModuleRudeEditFar	;discard vartable, reset name table bits
	mov	[spErrorRestore],sp	;refresh (ModuleRudeEdit recursively
					; calls SsRudeScan which changes this)
Eot_Err_Exit:
	pop	ax			;retval
SsRudeErr:				;ax = error code, set carry and return
	jmp	short SsRudeX

SsRudeOkX:
	mov	al,[TargetState]
	cmp	[grs.GRS_fDirect],FALSE
	jnz	SsRudeX1		;brif just scanned direct mode buffer
	mov	[txdCur.TXD_scanState],al

	;in case we had a DEF FN without an END DEF (an error the execute
	;  scanner will catch), deactivate prsCur if the current text table
	;  is for mrsCur
	test	[txdCur.TXD_flags],FTX_mrs
	jz	SsRudeX1		;brif a prs is active

	call	PrsDeActivateFar	; in case we had a DEF FN w/o an END DEF
SsRudeX1:
	DbMessTimer	SCAN,<Leave SsRudeScan - >
	sub	ax,ax			; Indicate success
SsRudeX:
	mov	[oNamOfPrsCur],UNDEFINED ;reset to default value
cEnd

;***
;HandleError
;Purpose:
;	This routine is called when an error occurs in scanning from
;	SS_RUDE to SS_PARSE. It saves away the first unscanned pcode,
;	replacing it with opEot. It saves away the error code for later
;	reporting. It sets SI & DI to zero, sets the TargetState to
;	SS_RUDE for descanning, and returns to the scan/descan loop.
;Input:
;	SI-4 points to the first unscanned pcode.
;	AL contains the qbimsgs error code; AH is likely non-zero.
;	ES is still set up for text table being scanned.
;	DI Assumed != UNDEFINED, EXCEPT for the special case where a
;		statement might be fully scanned to parse and THEN the
;		error is noticed. Example: CONST x = 1/0.
;		In this case, the otx for where the error actually occured
;		is on the stack
;Exit:
;	[SsErrOTx] = text offset to error
;
;*****************************************************************************
DbPub	HandleError
HandleError:
	SsRefreshES			; es = cur pcode seg (heap movement)
	xor	ah,ah			;convert error code to a word
	mov	[SsErr],ax
	cmp	[grs.GRS_oPrsCur],UNDEFINED
	jz	HandleErr_Cont		;brif no prs active

	cmp	[prsCur.PRS_procType],PT_DEFFN
	jnz	HandleErr_Cont		;brif active prs is not a DEF FN

	call	PrsDeActivateFar	; deactivate the DEF FN
HandleErr_Cont:
	sub	si,4			;move back to first unscanned opcode
	mov	[SsErrOTx],si		;save oTx of opcode we're replacing
	mov	ax,si			
	inc	di			;special case?
	jnz	HandleErr_Cont1		;  brif not

	pop	ax
HandleErr_Cont1:
	mov	sp,[spErrorRestore]	;in case stack in an intermediate state
	or	al,[fErrWithinOp]	;tell caller if error was at operand
	mov	[grs.GRS_otxCur],ax	;tell SsRudeScan's caller where error is
	LODSWTX				;get original opcode
	mov	[SsErrOpcode],ax	;  and save it for later restoration
	mov	WORD PTR es:[si-2],opEot
	mov	[TargetState],SS_RUDE	;so we'll descan portion scanned so far
	jmp	SsRudeScanErr		;return to the main loop

;***
;HandleId
;Purpose:
;	Given an oTyp in ax, an oNam at es:[si], and mkVar.flags
;	set up appropriately, call MakeVariable to search for
;	(and create if necessary) the variable, replacing the
;	oNam with an oVar.
;Input:
;	ax = oTyp for var
;	es:si and es:di both point to oNam in pcode
;	mkVar.flags and mkVar.cDimensions (if appropriate)
;		are already set up per varmgr spec.s.
;Output:
;	The oVar is substituted for the oNam
;	si & di point at the following word
;Preserves:
;	ES
;*****************************************************************************
DbPub	HandleId
HandleId	PROC	NEAR
	mov	[mkVar.MKVAR_oTyp],ax
	LODSWTX				;Pick up oNam
	mov	[mkVar.MKVAR_oNam],ax
	call	MakeVariableFar 	
	SsRefreshES			;es = cur pcode seg (heap movement)
	or	ah,ah			;an error return?
	js	Id_Mkvar_Error		;  brif so

	cmp	[otxConstCur],0		;scanning a CONST statement?
	jnz	ConstCheck		;  brif so

HandleId_Exit:
	STOSWTX				;emit the oVar

	xor	ax,ax
	xchg	ax,[varFlags_Reset]	; reset flag, fetch previous
					;   settings of certain flag bits
	or	[mkVar.MKVAR_flags],ax	
					;for special case; see IdLd, below
	ret

Id_Mkvar_Error:
	TESTM	mkVar.MKVAR_exitFlags,FVI_INDEXED	
	jz	HandleError5		;brif not an array pcode

Id_Error:
	dec	si			;so si points 4-bytes past opcode
	dec	si
HandleError5:
	jmp	HandleError

ConstCheck:
	test	[mkVar.MKVAR_flags2],MV_fConstFound
	jnz	HandleId_Exit		;brif things are okay, i.e., we're in
					;  a CONST statement and the variable
					;  we just found is really a constant
	mov	al,MSG_InvConst		;"Invalid Constant"
	jmp	short Id_Mkvar_Error
	
HandleId	ENDP

	page
;***
;SsVProc Unsupported
;
;Purpose:
;
;   This routine reports an error when unsupported opcodes are encountered.
;   This will occur from a binary load of a file from another basic product
;   that supports statements not supported by this product.
;
;Input:
;
;   standard rude scan dispatch.
;
;Output:
;
;   standard rude scan dispatch.
;
;**********************************************************************

SsVProc Unsupported			
	mov	al,Msg_Unsupported	
HandleError7:				
	inc	si			; Set SI-4 to point to this opcode
	inc	si			
	jmp	short HandleError5	

;***
;SsVProc Id<Ld|St>
;Purpose:
;	Simple Id SS_RUDE scan/descan dispatch points.
;
;	The only rude mode issue is to move the single operand between
;	oVar and oNam.
;
;	Scanning:
;		For each of these, the oNam in in the pcode stream,
;		the oTyp can be fetched from the rule table (mpOpRule).
;Input:
;	standard rude scan dispatch.
;Output:
;	standard rude scan dispatch.
;*****************************************************************************
SsVProc	IdSt
	jz	DIdRude
	mov	cx,[otxConstCur]
	jcxz	IdSt			;brif not scanning a CONST statement

	inc	si			;advance txt ptr to next pcode
	inc	si
	;cx = oTx of start of CONST expression
	mov	ax,si			
	sub	ax,cx			; ax = count of bytes in CONST expr.
	cCall	ScanAndExec,<cx,ax>	; creates CONST variable, scans &
					;  executes expression, setting
					;  value field in the new CONST variable
	SsRefreshES			;es = text segment
					; (ScanAndExec can cause far heap
					;  movement)
	mov	[spErrorRestore],sp	;refresh (ScanAndExec changes this)
	or	ax,ax
	jz	IdSt_NoErr		;brif no error
	js	HandleError5		;brif pcode not modified by ScanAndExec

	sub	si,4			;so we report error on correct line
	push	si
	mov	di,UNDEFINED		;signal special case, so UI will report
					;  the error on the correct line
	add	si,8			;advance oTx to reflect that the IdSt
					;  pcode did get bound to an oVar by
					;  ScanAndExec
	jmp	short HandleError5	;brif error
IdSt_NoErr:
	mov	[otxConstCur],si	;in case there are more CONSTant
					;  expressions in this statement
	jmp	short RetToScan5	;no error

IdSt:					;start of code shared for array St's
	or	[mkVar.MKVAR_flags],FVI_LVAL	
	;We know the zero flag won't be set after the above 'or', so fall thru
SsVProc	IdLd
	jz	DIdRude

	;The following tests for ReDim scalar which can appear because QB4
	;incorrectly allowed it.  The binary translator mucks with the code
	;but must leave it so it can be listed to and editted by the user.


	cmp	PTRTX[si+2],opStReDimTo+OPCODE_MASK+1	; ReDimScalar
	je	ReDimScalar
IdLd_1: 				
	;The below code is for a case like STATIC X,Y(A),Z
	;  where 'A' will be an IdLd.   We preserve and HandleId restore's the
	;  previous state of the FVI_STATIC flag in case of variables like 'Z'
	mov	cx,[mkVar.MKVAR_flags]	
	KEYWORD_FLAGS	EQU    FVI_SHARED OR FVI_COMMON OR FVI_STATIC OR FVI_DIM
	and	cx,KEYWORD_FLAGS	
	mov	[varFlags_Reset],cx	;0 or some flag bit
	and	[mkVar.MKVAR_flags],NOT KEYWORD_FLAGS	

IdLd:					;start of code shared for array Ld ID's
	call	HandleId
RetToScan5:
	jmp	RudeLoop		;return to the main loop

ReDimScalar:				
	mov	al,Msg_SubCnt		
	jmp	short HandleError7	

DIdRude:				;Common ID rude descan code
	LODSWTX				;Pick up operand
	add	ax,dx			; ax = pVariable
	xchg	bx,ax			
	mov	ax,[bx].VAR_oNam	;ax = oNam for variable
	STOSWTX				;emit the oNam
	jmp	RudeLoop		;And return to the main loop


SsVProc	VtRf
	jz	DIdRude
IdVtRf:					;start of code shared for array VtRf's
	TestM	[mkVar.MKVAR_flags],FVI_ASCLAUSE    
	jz	IdLd			;brif no 'AS' clause found

	cmp	ax,ET_IMP
	jnz	IdLd			;brif explicitly typed VtRf opcode

	mov	ax,[mkVar.MKVAR_oTyp]	;desired oTyp is already in mkVar
	jmp	short IdLd


;***
;SsVProc AId<Ld|St>
;Purpose:
;	Array Id SS_RUDE scan/descan dispatch points.
;
;	The only rude mode issue is to move the single operand between
;	oVar and oNam.
;Input:
;	standard rude scan dispatch.
;Output:
;	standard rude scan dispatch.
;*****************************************************************************
SsVProc	AIdSt
	call	AIdPreamble		;common code to all Array ID's
	jmp	short IdSt		;shared code for all St ID's

SsVProc	AIdLd
	call	AIdPreamble		;common code to all Array ID's
	cmp	PTRTX[si+2],opStReDimTo ; Is this a ReDim statement?
	je	ArrayDeclare		; Yes, need to set FVI_ARRAY
	jmp	short IdLd_1		; shared code for all Ld ID's

SsVProc	AVtRf
	call	AIdPreamble		;common code to all Array ID's
ArrayDeclare:
	shr	[mkVar.MKVAR_cDimensions],1
					;divide cDimensions by two in case this
					;  is an array ref., since the 'cnt' for
					;  that pcode counts lower & upper
					;  bounds for each dimension
	or	[mkVar.MKVAR_flags],FVI_ARRAY	
	jmp	short IdVtRf		;shared code for all VtRf ID's

;***
;AIdPreamble
;Purpose:
;	Perform actions common to all array ID opcodes. This
;	includes checking to see if we're scanning or descanning,
;	(and handling the descan case entirely), setting up 
;	mkVar.cDimensions, and setting the FVI_INDEXED bit in mkVar.flags.
;
;Input:
;	standard rude scan dispatch.
;Output:
;	ax = type of ID
;	mkVar.MKVAR_cDimensions field set up (though it could be 2  cDim's)
;	di = si
;	mkVar.flags has FVI_INDEXED set
;*****************************************************************************
DbPub	AIdPreamble
AIdPreamble	PROC	NEAR
	jz	DAIdRude
	
	xchg	ax,bx			;save array type in bx
	LODSWTX				;Pick up (assumed) cDimensions
					;  (note that this might be 2 * cDim's)
	or	ah,ah			;special value in lieu of 0?
	jns	Save_cDims		;  brif not

	xor	al,al			;set cDims to zero
Save_cDims:
	mov	[mkVar.MKVAR_cDimensions],al
	xchg	ax,bx			;common code expects type in ax
	inc	di			;set di = si
	inc	di
	or	[mkVar.MKVAR_flags],FVI_INDEXED	
	ret
AIdPreamble	ENDP

DAIdRude:
	pop	ax			;throw away return addr from AIdPreamble
	inc	si			;skip index count
	inc	si
	mov	di,si			;Move to oVar operand
	jmp	DIdRude 		; and dereference just like an Id


;***
;SsVProc Off<Ld|St>
;Purpose:
;	Record offset Id SS_RUDE scan/descan dispatch points.
;
;	Scan work:	Convert oNam to oElem 
;	Descan work:	Convert oElem to oNam
;Input:
;	standard rude scan dispatch.
;	Count on the oTyp of parent being in mkVar.oTyp.
;Output:
;	standard rude scan dispatch.
;	Leave the oTyp of found element in mkVar.oTyp.
;*****************************************************************************
SsVProc	OffLd
SsVProc OffSt
	xchg	ax,bx			; bx == explicit type constant
	LODSWTX				;fetch oNam/oElem from pcode
	jz	D_Off			;brif descanning

	cmp	[mkVar.MKVAR_oTyp],ET_MAX
	jbe	Not_User_Defined	;[26] brif not user defined type

	cCall	RefElem,<ax,bx> 	;[36]
	SsRefreshES			;es = cur pcode seg (heap movement)
	or	ax,ax			
	js	HandleError2		;brif error return

Off_Exit:
	STOSWTX				;emit the oElem/oNam (common exit point)
RetToScan7:				
	jmp	RudeLoop		;return to the main loop

D_Off:
	mov	bx,dx			    ; get start of type table (bdVar)
	add	bx,ax			    ;bx = pElem
	mov	ax,[bx.ELEM_oNam]	    ;fetch oNam of the element
	jmp	short Off_Exit		; emit the oNam/iCE and return
					; to main loop

Not_User_Defined:			
OffError_oTyp:
	mov	al,MSG_BadElemRef	;in case this isn't of 
					;     user defined type
HandleError2:
	jmp	HandleError		;descan up to this opcode, report error
	

;***
;SsVProc AsType - handle opAsType and opAsTypeExp
;Purpose:
;	Set the mkVar flag to note that an 'AS' clause has been
;	seen, and set mkVar.oTyp to the appropriate type.
;	op[A]IdVtRfImp dispatch points must check to see if
;	the FVI_ASCLAUSE bit is set in mkVar.flags, and leave the
;	oTyp alone if so (rather than loading it from the rule
;	table index).
;Input:
;	standard rude scan dispatch.
;Output:
;	standard rude scan dispatch.
;
;*****************************************************************************
SsVProc	AsType				;Includes AsTypeExp
	jz	D_AsType		;no descanning work here
	
	or	[mkVar.MKVAR_flags],FVI_ASCLAUSE	
	LODSWTX				;ax = 1st operand
	cmp	cx,opAsType		;is this for opAsType?
	jnz	AsTypeExp		;  brif not - opAsTypeExp

	cCall	RefTyp,<ax,di>
	SsRefreshES			; es = cur pcode seg (heap movement)
	or	ah,ah			;an error return?
	jns	AsType_Cont		;  brif not
HandleError6:
	jmp	short HandleError2
AsTypeExp:
	cmp	cx,opAsTypeExp
	jz	AsType_Cont		;ax contains a pre-defined oTyp

	DbAssertRel cx,z,opAsTypeFixed,SCAN,<SsVProc AsType: unexpected opcode>
	xchg	ax,dx
	LODSWTX 			;ax contains cbFS or oNam of const
					;  or oNam of FORM/MENU
	or	dh,dh			;length, or oNam?
	jns	AsTypeFixed_Cont	;  brif ax == length

	or	[mkVar.MKVAR_flags2],MV_fONamInOTyp
	and	dh,07FH 		;mask to make this a normal oTyp
AsTypeFixed_Cont:
	.errnz	MKVAR_fsLength - MKVAR_oNamForm 
	mov	[mkVar.MKVAR_fsLength],ax
	xchg	ax,dx
AsType_Cont:				;have type in ax
	mov	[mkVar.MKVAR_oTyp],ax
	inc	si
	inc	si			;Skip listing column
	jmp	short RetToScan2

D_AsType:
	add	si,4			;skip to next pcode if descanning
	cmp	cx,opAsTypeFixed
	jnz	RetToScan2		;brif only two-bytes to skip

	inc	si
	inc	si			;Skip listing column
	jmp	short RetToScan2

;***
;SsVProc StDefTyp
;Purpose:
;	Grab the 4-byte mask from the pcode; pass this to SetDefBits to
;	reset the default type for specified alphabet letters.
;Input:
;	standard rude scan dispatch.
;Output:
;	standard rude scan dispatch.
;*****************************************************************************
SsVProc	StDefType
	jz	D_DefType		;no descanning work here

	inc	si			;skip link field - - point to
	inc	si			;  I4mask in pcode
	LODSWTX				;Pick up low word
	xchg	ax,dx			;DX = Low word of mask
	LODSWTX 			;AX:DX = I4mask
	push	ax			
	push	dx			
	and	dl,FV_TYP_MASK		; mask out all but type constant
	push	dx			
	call	SetDefBits		;set new type default(s)
	SsRefreshES			; es = cur pcode seg (heap movement)
RetToScan2:
	jmp	RudeLoop		;return to the main loop


D_DefType:
	;fall through to StStatic for return to descan loop

;***
;SsVProc StCommon, StShared, StStatic, Shared
;Purpose:
;	Set flags for later variable references. Each of these
;	opcodes come before the associated Id opcodes, so these
;	flags are simply set up, and left until BOS/BOL.
;
;	To save code, don't bother to detect if we're scanning or
;	descanning; just set the flags regardless.
;
;Input:
;	standard rude scan dispatch.
;Output:
;	standard rude scan dispatch.
;*****************************************************************************
SsVProc	StStatic
	or	[mkVar.MKVAR_flags],FVI_STATIC	
RetToScanOps:
	jmp	Operand_Skip_Ret	;skip operands and return to main loop

SsVProc	StCommon
	or	[mkVar.MKVAR_flags],FVI_COMMON	
	jmp	short	RetToScanOps

SsVProc	StShared
SsVProc	Shared
	or	[mkVar.MKVAR_flags],FVI_SHARED	
	jmp	short	RetToScanOps	

SsVProc StDim
	or	[mkVar.MKVAR_flags],FVI_DIM	
	jmp	short	RetToScanOps	


;***
;HandleProcName
;Purpose:
;	Shared code for handling the procedure name in a
;	DECLARE, FUNCTION, or DEF FN statement. Calls MakeVariable.
;
;	NOTE: See qbipcode.txt for a description of the procAtr word in
;		the pcode in order to better understand this routine.
;Input:
;	es:[si+2] = oNam or oPrs for procedure
;	If this is for a DEF FN, bits cl = PT_DEFFN, else cx = 0
;Exit:
;	Should not be called for a SUB definition. If called for DECLARE SUB, 
;		does nothing, only return value is bl = PT_SUB.
;	Otherwise,
;		no Prs is active (i.e. module level text tbl is active)
;		mkVar.MKVAR_oTyp contains oTyp returned by MakeVariable
;		bl contains the procType of the prs
;		cx = oPrs of FUNCTION/DEF FN
;Preserves:
;	SI,DI,ES
;Exceptions:
;	This routine handles MakeVariable error return
;*****************************************************************************
DbPub	HandleProcName
HandleProcName PROC NEAR
	mov	ax,es:[si.DCL_atr]	;ax = procAtr operand
	and	ah,DCLA_procType / 100h	;ah = procType (PT_SUB etc.)
	mov	bl,PT_SUB		;put procType in bl in case of a SUB
	cmp	ah,bl			;is this a SUB?
	DJMP	jz HandleProc_Exit	;  brif so - do nothing, exit

HandleProcName_Cont:
	push	[grs.GRS_oRsCur]	;save caller's oRs
	test	al,DCLA_Explicit	;explicitly typed DEF or FUNCTION?
	jnz	HandleProc_Type		;  brif so

	mov	al,ET_IMP		;proc was implicitly typed
HandleProc_Type:
	and	ax,DCLA_oTyp		;ax = oTyp returned by function
	mov	[mkVar.MKVAR_oTyp],ax
	mov	bx,es:[si.DCL_oPrs]	;fetch oPrs (or oNam) from pcode stream
	jcxz	NotDefFn

	mov	[oNamOfPrsCur],bx	;speed optimization - used by varmgr
					;  This is safe because varmgr only uses
					;  this when grs.oPrsCur != UNDEFINED
	push	bx			;oNam of DEF FN
	push	cx			;procType
	cmp	al,ET_IMP		;implicitly typed DEF FN?
	jnz	Not_Implicit		;  brif not

	cCall	oTypOfONamDefault,<bx>	; returns ax = default oTyp
					;	for this oNam
Not_Implicit:
	push	ax			;oTyp of DEF FN (could be ET_IMP)
	xor	dx,dx
	push	dx			;fNotDefine
	call	PrsDefine
	SsRefreshES			;es = cur pcode seg (new txt tbl)
	or	ax,ax
	jnz	ProcNameError		;error in defining prs for DEF FN

	or	[prsCur.PRS_flags],FP_DEFINED
	mov	ax,si
	sub	ax,4			;ax == otx of opStDefFn
	mov	[prsCur.PRS_otxDef],ax
        mov     ax,[grs.GRS_oMrsCur]
        mov     [prsCur.PRS_oRsDef],ax  ;oRsDef is the module's oRS

	mov	bx,[grs.GRS_oPrsCur]	;oPrs for DEF FN
	mov	es:[si.DCL_oPrs],bx	;replace oNam in pcode with oPrs
NotDefFn:
	push	bx			; save oPrs across calls
	push	bx			;pass oPrs to FieldsOfPrs below
	call	PrsDeActivateFar	; make main level variable for
					;  DEF FN or FUNCTION
	cCall	FieldsOfPrsFar		; get oNam in ax, procType in dl
					; parm was already pushed above
	mov	[mkVar.MKVAR_oNam],ax
	push	dx			;save procType for retval
	cmp	dl,PT_FUNCTION		;is this prs for a FUNCTION?
	jnz	ProcFlagsSet		;  brif not - - must be for a DEF FN

	or	[mkVar.MKVAR_flags],FVI_FUNCTION	
ProcFlagsSet:
	call	MakeVariableFar 	;search for and create var if not found
	pop	bx			;restore procType for retval
	pop	cx			; restore oPrs
	or	ah,ah			;an error return?
	js	HandleProc_Error	;  brif so

	DbAssertRel [mkVar.MKVAR_oTyp],nz,ET_IMP,SCAN,<HandleProcName:ET_IMP oTyp>
	pop	dx			;discard caller's grs.oRsCur
HandleProc_Exit:
	ret

HandleProc_Error:
	TESTM	mkVar.MKVAR_exitFlags,FVI_FNNAME	
					;Note: could use DX instead if
					;	MakeVariable in native code
	jnz	ProcNameError		;brif an opStDefFn pcode
	inc	si			;so si points 4-bytes past opcode
	inc	si			; as expected by HandleError
ProcNameError:
	pop	dx			;dx = caller's grs.oRsCur
	push	ax			;save error code
	cCall	RsActivate,<dx> 	; re-activate caller's oRs
	pop	ax			;restore ax = error code
HandleError1:
	jmp	HandleError
HandleProcName	ENDP

;***
;SsVProc StDefFn, StFunction, StSub
;Purpose:
;	Handle the pcodes for SUB, FUNCTION, and DEF.
;
;	For SUB, the proc name is ignored, and we share
;	code with the others for handling each formal
;	parameter (all given as operands to the opcode).
;
;	For DEF, we must bypass the link field; otherwise,
;	it is treated the same as FUNCTION (common code
;	will set the FVI_FUNCTION flag as appropriate).
;
;Inputs:
;	standard rude scan dispatch.
;Outputs:
;	standard rude scan dispatch.
;*****************************************************************************
SsVProc	StDefFn
	pushf
	inc	si			;move to 2-bytes prior to oPrs
	inc	si			;  DECLARE & FUNCTION have no 'link'
	cmp	[grs.GRS_oPrsCur],UNDEFINED
	jne	DefInDefErr		;brif found in another prs
	mov	cl,PT_DEFFN		;tell HanldeProcName this is a DefFn
	popf
	jmp	short Func_Or_Def

DefInDefErr:
	mov	ax,MSG_InvProc		;"Invalid within procedure"
	jmp	SHORT HandleError1

SsVProc	StFunction
	mov	cx,0			;'mov' doesn't affect flags
Func_Or_Def:
	jz	StProc_DeScan		;skip the following if descanning

	call	HandleProcName		;calls MakeVariable for FUNCTION/DEF
	push	bx			;save bl=procType
	cCall	PrsActivate,<cx>	; make FUNCTION/DEF FN active
	pop	bx			;restore bl=procType
	SsRefreshES			;es = cur pcode seg (new txt tbl)
	mov	ax,es:[si.DCL_atr]	;ax = procAtr operand
	and	ax,NOT DCLA_oTyp	;mask out existing oTyp
	mov	dx,[mkVar.MKVAR_oTyp]	;get actual oTyp of DEF FN or FUNCTION
	DbAssertRel  dx,be,ET_FS,SCAN,<SsV_StFunction: oTyp is invalid>
	or	ax,dx			;set correct oTyp in pcode, for later
	mov	es:[si.DCL_atr],ax	;  checking by execute scanner
	and	[prsCur.PRS_oType],NOT M_PT_OTYPE   ; turn off existing oTyp bits
	or	[prsCur.PRS_oType],dl	; ensure oType field set correctly in prs
	DJMP	jmp SHORT StProc_Scan

SsVProc	StSub
	;at this point, es:[si+2] is the proc oPrs; PSW.Z set if descanning
	mov	bl,PT_SUB
	jz	Not_A_DefFn		;brif descanning to SS_RUDE

	jmp	StProc_Scan1		;brif scanning to SS_PARSE

StProc_DeScan:
	;descanning: walk through parms, converting each oVar to an oNam

	;first, reset prs.oVarHash and replace oPrs with oNam in pcode if this
	; is a DEF FN
	jcxz	Not_A_DefFn
	mov	ax,es:[si.DCL_oPrs]	;get oPrs
	push	dx			;save pVarTable
	cCall	FieldsOfPrsFar,<ax>	;ax = oNam, dl = proctype
	DbAssertRelB dl,z,PT_DEFFN,SCAN,<rude descan: dl should == PT_DEFFN here>
	mov	es:[si.DCL_oPrs],ax	;replace oPrs for DEF FN with oNam
	pop	dx			;restore pVarTable
Not_A_DefFn:
	xor	bx,bx			;bx = zero indicates we have oVar's that
					;  must be converted to oNam's
	add	si,DCL_cParms		;mov to parm count
D_FormalParm_Descan:
	LODSWTX				
	mov	cx,ax			;cx = count of parm sets (3 words/set)
	inc	ax			;no parms? (test for UNDEFINED)
	jz	D_Formal_Exit		;  exit if so - si points to next opcode
	jcxz	D_Formal_Exit		;exit if parm count of zero
D_FormalParm_Loop:
	mov	di,si			;point to the oVar in pcode
	cmp	[SsErrOTx],si
	jz	RetToScan3		;special case: if a scan error occured
					;  in this formal parm, the 'oNam' field
					;  will now contain opEot; just dispatch
					;  to it now if we've descanned to error
	LODSWTX				;fetch oVar (or oNam if DECLARE)
	or	bx,bx			;are we descanning a DECLARE statment?
	jnz	D_ONam_Okay		;  brif so - - oNam field fine as is

	add	ax,dx			; ax = pVariable
	xchg	bx,ax			
	mov	ax,[bx].VAR_oNam	;ax = oNam for variable
	STOSWTX				;emit the oNam
	xor	bx,bx			;bx == 0 ===> not descanning a DECLARE
D_ONam_Okay:
	inc	si			;move source pointer to
	inc	si			;  the oTyp field
	LODSWTX
	cmp	ax,ET_MAX		;user-defined oTyp?
	jbe	D_Formal_Cont		;  brif not - - leave it alone

	push	bx
	cCall	ONamOTyp,<ax>		; ax = oNam for name of this type
	pop	bx
	mov	es:[si-2],ax		;replace oTyp with oNam of type in pcode
D_Formal_Cont:
	loop	D_FormalParm_Loop	;loop for each parm set
D_Formal_Exit:
	or	bx,bx			;descanning a DECLARE?
	jz	RetToScan3		;  brif not
	
	mov	si,bx			;skip past alias text in pcode
RetToScan3:
	jmp	RudeLoop		;return to the main loop

OM_ProcError:
	mov	al,ER_OM		;insufficient memory for var hash table
	dec	si			;[si-4] = opStDefFn
	dec	si
	jmp	short HandleError3

StProcError:
	;Found an error while scanning a formal parameter for a SUB, FUNCTION,
	;  or DEF. SI points past this formal parameter (that was itself
	;  unmodified). 
	;For StProcError, [si-4] = oNam. This will cause oNam of the formal
	;  to be temporarily replaced with opEot; wierd, but saves code.
	
	mov	[fErrWithinOp],1	;tell caller error was in operand
HandleError3:
	jmp	HandleError

StProc_Scan:				;scan each formal parameter for the proc
	;es:si points 2-bytes before the oPrs for FUNCTION/DEF statement
	;bl = procType of proc

	cmp	bl,PT_DEFFN		;are we scanning a DEF FN statement?
	jnz	Not_Def_Fn		;  brif not

	push	bx			
	call	MakePrsTVar		;make variable hash table for DEF FN
	pop	bx			
	SsRefreshES			;es = cur pcode seg (heap movement)
	or	ax,ax			;error return?
	mov	al,ER_OM		
	jz	HandleError3		;  brif so

Not_Def_Fn:
	;At this point, we make a RetVal entry for the FUNCTION/DEF FN.
	;This is so there is space in the variable table for this, even
	;if not used, in case we determine at execute scan time that we
	;have a RetVal where we thought we had a reference. Note that this
	;logic depends on the fact that we always end up in SS_RUDE when a
	;FUNCTION or DEF FN is added.
	;Note that we assume that the oTyp and oNam are still valid in mkVar
	;for this FUNCTION or DEF FN
	mov	[mkVar.MKVAR_flags],FVI_LVAL	
	push	bx			
	call	MakeVariableFar 	;search for and create var if not found
	pop	bx			
	;Note that we don't use the return value, i.e., it doesn't go in the
	;pcode - - - the goal is just to create the retval entry.
	SsRefreshES			;es = cur pcode seg (heap movement)
	or	ah,ah			;an error return?
	jns	StProc_Scan1		; brif not

	cmp	bl,PT_DEFFN		
	jz	HandleError3		; si-4 points to opStDefFn

	inc	si			
	inc	si			
	DbAssertRelB bl,z,PT_FUNCTION,SCAN,<ssrude:opStFunction, invalid PT_ type>
	jmp	HandleError3		; si-4 points to opStFunction

StProc_Scan1:				;SUB support joins up w/FUNCTION & 
					;  DEF FN support here
	add	si,6			; point si past oPrs and procAttr 
	.errnz	DCL_oPrs - 2		; operands
	LODSWTX				;get count of parameters
	.errnz	DCL_cParms - 6
	mov	cx,ax			;cx = count of parm sets (3 words/set)
	jcxz	RetToScan3		;brif zero parms
	inc	ax			;test for UNDEFINED
					;special value indicating no parameters?
	jz	RetToScan3		;  brif so

	or	[mkVar.MKVAR_flags],FVI_FORMAL	
	mov	BYTE PTR [mkVar.MKVAR_cDimensions],0
					;set this to default for any array parms
DbPub	FormalParm_Loop
FormalParm_Loop:
	mov	di,si			;reset di to point to oNam of parm
	LODSWTX				;ax = oNam of parm
	mov	[mkVar.MKVAR_oNam],ax
	LODSWTX				;ax = flags for parm
	mov	dx,ax
	and	ax,(FVI_ARRAY OR FVI_ASCLAUSE)	
	or	[mkVar.MKVAR_flags],ax
.erre	FVI_ARRAY AND PATR_array	
.erre	FVI_ASCLAUSE AND PATR_asClause	
	mov	ax,ET_IMP
	TESTX	dx,<PATR_asClause OR PATR_explicit>
	jz	Set_Typ			;brif must set oTyp to default based on
					;  oNam
	mov	ax,es:[si]		;oTyp or oNam
	DbAssertRel ax,nz,0,SCAN,<ssrude formal parm found with AS ANY oTyp>
	;(AS ANY clause only valid for DECLARE statement)
	cmp	ax,ET_MAX		;is this a predefined type?
	jbe	Set_Typ			; brif so (CAN'T be fixed-length string)
					;ax is the oNam of a user type
	push	cx			;save count of formals across call
	cCall	RefTyp,<ax,si>		;returns oTyp or error code	
	pop	cx
	or	ah,ah			;an error return?
	jns	Set_Typ			;  brif no error
StProcError1:
	jmp	StProcError
Set_Typ:
	mov	[mkVar.MKVAR_oTyp],ax
	push	cx			;preserve count of formals
	call	MakeVariableFar 	; create the parm entry
	pop	cx
	SsRefreshES			;es = cur pcode seg (heap movement)
	or	ah,ah			;was an error code returned?
	js	StProcError1		;  brif so
	
	STOSWTX				;replace oNam with oVar
	mov	ax,[mkVar.MKVAR_oTyp]
	mov	es:[si],ax		;put oTyp of found/created var in pcode
@@:
	inc	si			;skip past oTyp field
	inc	si
	DJMP loop FormalParm_Loop  	;continue for each parameter

	and	[mkVar.MKVAR_flags],NOT FVI_FORMAL	
					;turn off this flag for subsequent
					;  scanning
RetToScan1:
	jmp	RudeLoop		;return to the main loop

;***
;SsVProc StDeclare
;Purpose:
;	Call varmgr with FUNCTION name only - - we don't create variables
;	for SUB's, and the parser guarantees that we'll never see a DECLARE
;	for a DEF FN (invalid syntax).  
;	Note that the oVar returned by MakeVariable is discarded
;	we need to call this to create var entry if it doesn't
;	already exist.
;
;	For each formal parameter, if oTyp not correctly set (based on flags
;	in pcode), set it based on current def type.
;
;Input:
;	standard rude scan dispatch.
;Output:
;	standard rude scan dispatch.
;*****************************************************************************
SsVProc	StDeclare
	pushf
	LODSWTX				;ax = cntEos operand
	add	ax,si			;ax points to next opcode
	popf
	jnz	Scan_StDeclare		;brif scanning, not descanning

	;descanning - - must convert each user-defined oTyp in formal parm
	;  list to an oNam, since the type table is gone when in SS_RUDE.
	;share code with StSub/StDefFn/StFunction for this
	add	si,DCL_cParms-2		;point to count of formal parms	
	xchg	ax,bx			;tell shared code we there are no
					;  oVar's to convert to oNam's
	jmp	D_FormalParm_Descan
Scan_StDeclare:
	push	ax			;save, so we can skip alias text @ end
	dec	si
	dec	si			;si points to cntEos operand
	xor	cx,cx			;can't be a DEFFN
	call	HandleProcName		;make the var if it's DECLARE FUNCTION
	SsRefreshES			;es = cur pcode seg (heap movement)
	cmp	bl,PT_FUNCTION
	jnz	StDeclare_Cont		;only change pCode for DECLARE FUNCTION

	mov	ax,es:[si.DCL_atr]	;ax = procAtr operand
	and	ax,NOT DCLA_oTyp	;mask out existing oTyp
	mov	dx,[mkVar.MKVAR_oTyp]	; get actual oTyp of FUNCTION
	DbAssertRel  dx,be,ET_FS,SCAN,<SsV_StDeclare: oTyp is invalid>
	or	ax,dx			; set correct oTyp in pcode for later
	mov	es:[si.DCL_atr],ax	;	checking by execute scanner
	xchg	ax,cx			; ax = oPrs (from HanldeProcName)
	call	PPrsOPrsSCAN		; bx = pPrs
	and	PTRRS[bx.PRS_oType],NOT M_PT_OTYPE ; turn off existing oTyp bits
	or	BPTRRS[bx.PRS_oType],dl ; ensure oTyp field set correctly in prs
StDeclare_Cont:
	add	si,DCL_cParms		;point to count of formal parms	
	LODSWTX				;ax = cParms operand
	mov	cx,ax			;cx = count of parms in pcode
	inc	ax			;no parms? (test for UNDEFINED)
	jz	StDeclare_Exit		;  exit if so - si points to next opcode
	jcxz	StDeclare_Exit		;brif no formals to scan
Declare_Formal_Loop:
	;loop for each formal parm in DECLARE pcode, setting the oTyp to
	;  the appropriate default if required
	LODSWTX				;ax = oNam of parameter
	xchg	ax,dx
	LODSWTX				;ax = flags, si points to oTyp field
	push	cx			;preserve count of formals
	TESTX	ax,<PATR_asClause OR PATR_explicit>
	jz	Formal_Type_Def_Set	;brif must set oTyp to default based on
					;  oNam
	mov	ax,es:[si]		;oTyp or oNam
	cmp	ax,ET_MAX		;is this a predefined type?
	jbe	Formal_Type_Set		; brif so (CAN'T be fixed-length string)
					;ax is the oNam of a user type
	cCall	RefTyp,<ax,si>		;returns oTyp or error code	
	SsRefreshES			; es = cur pcode seg (heap movement)
	or	ah,ah			;an error return?
	jns	Set_Formal_Typ		;  brif not
	mov	[fErrWithinOp],1	;tell caller error was in operand
HandleError4:
	jmp	HandleError

Formal_Type_Def_Set:
	cCall	oTypOfONamDefault,<dx>	; default oTyp for given oNam
	SsRefreshES			; es = cur pcode seg (heap movement)
Set_Formal_Typ:
	mov	es:[si],ax		;put default oTyp in pcode
Formal_Type_Set:
	pop	cx			;restore count of formals
	inc	si			;skip past this formal
	inc	si
	loop	Declare_Formal_Loop	;brif another formal to scan	
StDeclare_Exit:
	pop	si			;pointer to next pcode
RetToScan4:
	jmp	RetToScan1

;***
;SsVProc StEndDef
;Purpose:
;	This dispatch point is required for END DEF
;	to deactivate the current prs. Keeping prsCur correct during
;	rude scanning is important so that variable search & creation
;	is performed correctly.
;
;Input:
;	standard rude scan dispatch.
;Output:
;	standard rude scan dispatch.
;*****************************************************************************
SsVProc	EndSingleDef
SsVProc	StEndDef
	jz	Operand_Skip_Ret	;do nothing in descan case

	mov	[oNamOfPrsCur],UNDEFINED ; reset to default - just in case
	push	cx			;save opcode for operand skipping
	cCall	PrsDeActivateFar	
	pop	cx
	jmp	short Operand_Skip_Ret


;***
;SsVProc StType
;Purpose:
;	Rude scan an entire TYPE/END TYPE block
;Exit:
;	si points to 1st opcode after opStEndType
;
;*****************************************************************************
SsVProc	StType
	jz	Operand_Skip_Ret	;brif descanning to SS_RUDE

	push	si			;in case of error return
	call	ScanTypeBlock
	pop	cx			;previous (pushed) oTx
	SsRefreshES			;es = cur pcode seg (heap movement)
	or	ax,ax			
	je	RetToScan4		;brif no error

	inc	cx			
	inc	cx			;cx points 4-bytes past opStType
	xchg	cx,si			;si for descanning up to opStType
	push	cx			;oTx of where error really occured
	mov	di,UNDEFINED		;signal special case to HandleError
	jmp	SHORT HandleError4	;descan up to this opcode, report error

;***
;SsVProc StEndType
;Purpose:
;	Report "END TYPE without TYPE error, since all balanced opStEndType
;	opcodes are consumed by ScanTypeBlock.
;
;*****************************************************************************
SsVProc	StEndType
	jz	Operand_Skip_Ret	;brif descanning to SS_RUDE
	inc	si			;bump si by 2 for HandleError
	inc	si
	mov	al,MSG_NoType		;give "END TYPE without TYPE error"
	jmp	SHORT HandleError4

;***
;SsVProc StConst
;Purpose:
;
;Input:
;	standard rude scan dispatch.
;Output:
;	standard rude scan dispatch.
;*****************************************************************************
SsVProc	StConst
	mov	[otxConstCur],si	;remember we're scanning a CONSTant stmt
					;  and save current text pointer
	jmp	short RetToScan4
	
;***
;SsVProc For, Next, NextId
;Purpose:
;
;Input:
;	standard rude scan dispatch.
;Output:
;	standard rude scan dispatch.
;*****************************************************************************
SsVProc	For
SsVProc	Next
SsVProc	NextId
	mov	word ptr es:[si],UNDEFINED
					;replace oBP with UNDEFINED,
					;  so it will be so reset by the time
					;  the scanner sees it again (i.e.,
					;  in case we are descanning or have
					;  descanned from SS_PARSE to SS_RUDE)
	jmp	short Operand_Skip_Ret
	
;***
;SsVProc Bos,Bol,BolLabDef
;Purpose:
;	At the start of each statement, reset mkVar.flags to
;	all zeroes (default bit-flag values). This is necessary,
;	since some flags (such as STATIC, COMMON) must stay in
;	effect for the duration of a statement.
;
;Input:
;	standard rude scan dispatch.
;Output:
;	standard rude scan dispatch.
;*****************************************************************************
SsVProc	Bol
SsVProc	BolEmit
SsVProc	BolLabDef
	mov	ax,[SsLineCount]
	inc	ax
	mov	[SsLineCount],ax
	test	al,LineUpdate-1		;Time to update line count on screen?
	jz	UpdateLine		;brif so
SsVProc	Bos
Bos:
	sub	ax,ax
	mov	[mkVar.MKVAR_flags],ax
	mov	[otxConstCur],ax	;in case we were scanning a CONST stmt
Operand_Skip_Ret:
	mov	bx,cx			;put opcode into bx
	mov	cl,mpOpAtr.[bx]		;load attribute byte
	jmp	Ssv_NOps		;skip past operands, return to loop

UpdateLine:
	cmp	[TargetState],SS_RUDE	;Are we descanning?
	jz	Bos			;  brif so - no line count

	push	cx
	PUSH_ES 			
	push	dx
	cCall	UpdStatusLn,<ax>
	pop	dx
	POP_ES				
	pop	cx
	jmp	short Bos

sEnd	SCAN
end
