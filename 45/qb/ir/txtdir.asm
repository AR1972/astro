	TITLE	txtdir.asm - Text Mgr's Direct Mode Statement support

;==========================================================================
;
;Module:  txtdir.asm - Text Mgr's Direct Mode Statement support
;System:  Quick BASIC Interpreter
;
;=========================================================================*/
;
;		How a direct-mode statement gets executed
;
;                               User-Interface
;                                   /    |
;                          TxtDirect     |
;                              |   \     |
;               +--------------+    \    |
;               |              |     \   |
;               |         ParseLine  SystemScan
;               |                        |
;               |                    ForEach...
;               |                        |
;               |        +---------------+
;               |        |               |
;         +-----+-+  ScanTxtTbl      ReParseTbl
;         |       |   |
;         |  +----)---+
;         |  |    |   |
;     SsRudeScan  SsScan
;
;                                                  
;   User-Interface calls SystemScan for Single-Step, in case user edited
;     program between single steps.  It also calls SystemScan for the
;     Compile... menu, so we don't try to compile a program with errors.
;   TxtDirect calls ParseLine, SsRudeScan and SsScan for the direct
;     mode statement.
;   ReParseTbl calls DoReParse for any statements not yet accepted
;     by the parser, then ScanTypeBlocks, so variable manager knows
;     about all TYPE/END TYPE blocks.
;   ScanTxtTbl then calls SsRudeScan (if necessary) and SsScan (if necessary)
;     to get the text table ready to execute.
;   Some direct mode statements (like SYSTEM) don't require all loaded pcode
;     to be scanned.  In these cases, TxtDirect never calls SystemScan.
;
;=========================================================================*/

	.xlist
	include		version.inc
	TXTDIR_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	lister
	includeOnce	opcontrl
	includeOnce	opid
	includeOnce	opmin
	includeOnce	opstmt
	IncludeOnce	opaftqb4
	includeOnce	parser
	includeOnce	pcode
	includeOnce	qbimsgs
	includeOnce	rtinterp		
	includeOnce	scanner
	includeOnce	txtint
	includeOnce	txtmgr
	includeOnce	ui
	.list

	assumes	DS,DATA
	assumes	SS,DATA
	assumes	ES,NOTHING



sBegin	DATA
scanTo		DB	0	;used by ScanTxtTbl


sEnd	DATA

sBegin	CODE

;These opcodes are of special interest whenever they appear in direct mode:
;
tOpDirect LABEL WORD
	opTabStartAll	DIR
	;list of opcodes which are illegal in direct mode
	;many more illegal stmts are caught by NtStatement()
	opTabEntry	DIR,opBolLab
	opTabEntry	DIR,opBolLabSp
	opTabEntry	DIR,opBolInclude
	opTabEntry	DIR,opBolIncludeSp
	opTabEntry	DIR,op_Static
	opTabEntry	DIR,op_Dynamic
	opTabEntry	DIR,op_Include
	opTabEntry	DIR,opStData
	opTabEntry	DIR,opStDefFn
	opTabEntry	DIR,opStEndDef
	opTabEntry	DIR,opStEndIfBlock
	opTabEntry	DIR,opStEndProc
	opTabEntry	DIR,opStEndType
		DIR_ILLEGAL EQU DIR_opStEndType
;All opcodes until end of list cause entire program to be scanned
	;list of opcodes which have direct mode variants
	opTabEntry	DIR,opStGosub
	opTabEntry	DIR,opStGoto
	opTabEntry	DIR,opStElseLab
	opTabEntry	DIR,opStIfLab
		DIR_DM_VARIANT EQU DIR_opStIfLab
	;list of opcodes which cause entire program to be scanned & CantCont
	opTabEntry	DIR,opStRunLabel
	opTabEntry	DIR,opStRunMain
		DIR_CANTCONT EQU DIR_opStRunMain
	opTabEntry	DIR,opStChain
	opTabEntry	DIR,opStCallS
	opTabEntry	DIR,opStCall
	opTabEntry	DIR,opStCallLess
	opTabEntry	DIR,opStRead
	opTabEntry	DIR,opStResume0
	opTabEntry	DIR,opStResumeNext
	opTabEntry	DIR,opStReturn0
	opTabEntry	DIR,opEot

sEnd	CODE

sBegin	CP
assumes	cs,CP


;--------------------------------
; TxtDirect and support functions
;--------------------------------

;**************************************************************
; boolean ScanTxtTbl()
; Purpose:
;	Called by SystemScan via ForEachTxtTbl(ScanTxtTbl).
;
;	Make sure all SUBs and FUNCTIONs are defined and terminated.
;	Note this isn't called for DEF FNs.  Any change to a DEF FN
;	causes a RUDE descan, so rude-scanner is responsible for
;	making sure all DEF FNs are terminated.
;
;	If the text table's scan-state > [scanTo],
;	   First, it makes sure all TYPE/END types have been made known to
;	 the variable manager.
;	   Then it traverses the text table's ReParse linked list,
;	 calling TxtChange() for each line.
;	   Then it calls SsRudeScan(SS_PARSE) to scan the text table to SS_PARSE
;	   or SsScan() to scan the text table to SS_EXECUTE
;
; Entry:
;	grs.fDirect = FALSE
;	Text table to be scanned has been loaded by context mgr.
;
; Exit:
;	ps.bdpSrc (parser's input buffer) is used.
;	If no errors were encountered by either TxtChange or SsScan,
;	   return value is TRUE (so ForEach... will continue)
;	else
;	   return value is FALSE,
;	   it is caller's responsibility to set grs.fDirect = FALSE
;	   txtErr.errCode = an offset into the QBI Message Table (MSG_xxx) or,
;	     if high-bit set, ps.bdpError contains the parser-built
;	     ASCII error message,
;	   txtErr.fDirect is set to FALSE,
;	   txtErr.oRs identifies the text table with the error,
;	   txtErr.otx contains the offset into the text table to the offending
;	     source line (otxDelFirst).
;	   txtErr.oSrcErr contains the column offset into the source line
;	     to the offending token.
;
;Alters:
;	ps.bdpSrc (parser's source buffer)
;
;**************************************************************
DbPub ScanTxtTbl 			
cProc	ScanTxtTbl,<NEAR>
cBegin


	test	txdCur.TXD_flags,FTX_mrs
	jne	ScanTxt 		;brif not a SUB/FUNCTION text table

	test	[prsCur.PRS_flags],FP_DEFINED
	je	ScnSubErr		;brif proc does not have a SUB/FUNC
	test	[prsCur.PRS_flags],FP_ENDPROC
	jne	ScanTxt			;brif proc has END DEF/SUB/FUNCTION

	;error - this SUB/FUNCTION has no END SUB/FUNCTION
	mov	ax,MSG_ProcNoEnd	;SUB/FUNCTION/DEF without END SUB/F...
	mov	dx,[prsCur.PRS_otxDef]
	jmp	short ScnSubErrCom

ScnSubErr:
	;Give "Statement cannot precede SUB/FUNCTION definition" error
	mov	ax,MSG_InvBeforeProcDef	;ax = error code
	sub	dx,dx			;text offset of error = 0

ScnSubErrCom:
	mov	[txtErr.TXER_oSrc],0
DJMP	jmp	SHORT ScanErrCommon	

ScanTxt:
	test	[mrsCur.MRS_flags3],FM3_NotFound 
	je	FileLoaded

	;The user loaded a .MAK file which named this module, but
	;the module could not be found.
	
	mov	[grs.GRS_otxCur],0
	mov	ax,[grs.GRS_oMrsCur]
	call	SetPsErrMsg		;set ps.bdErr to name of module
	;if static err buf, OM err not possible
	mov	ax,ER_OM
   DJMP je	ScanErr			;brif out-of-memory
	mov	ax,MSG_MrsNotFound
	; 'Module not found.  Unload from program? <OK/Cancel>.
	; ReportError() will Unload mrs if OK and MSG_MrsNotFound.
   DJMP jmp	SHORT ScanErr

FileLoaded:
	and	[mrsCur.MRS_flags],NOT FM_AllSsRude ;we have at least one non
					;SS_RUDE text table in this module.
	mov	al,[scanTo]		;al = desired state
	cmp	al,[txdCur.TXD_scanState]
   DJMP jae	SctGoodExit		;brif this table is already scanned
					; to at least [scanTo]
;***** Start Revision [41]
;***** End Revision [41]

STT_DoScan:
	cmp	al,SS_EXECUTE
	je	ToExecute		;brif scanning to SS_EXECUTE
	PUSHI	ax,SS_PARSE		;pass target state to SsRudeScan
	call	SsRudeScan		;scan to SS_PARSE
	or	ax,ax			
	jz	SctGoodExit		; brif no error
					;ax = error code
	jmp	SHORT ScanErr

;Scan current text table from SS_PARSE to SS_EXECUTE
ToExecute:
	call	SsScan			;scan text table to SS_EXECUTE
					;ax = scanner error (0 if none)
					;grs.otxCur = location of error
	DbChk	TxdOps			;see if scanner inserted bad opcode
	or	ax,ax			
	jne	ScanErr			;brif scanner encountered errors
	and	[flagsTm],NOT FTM_NoSsExecute ;we now have a table in
					;SS_EXECUTE

SctGoodExit:

	    mov     ax,UNDEFINED	;double duty - - - non-zero == TRUE
	    cmp     [grs.GRS_otxCONT],ax
	    jnz     SctExit		;brif can CONTinue

	    call    ResetData		;ensure the next READ is to the
					;  first DATA stmt in this module
					;  this is needed only for READ in
					;  direct mode buffer after we set
					;  Cant CONTinue.
	    ;AX already set non-Zero by call to ResetData

SctExit:
	or	ax,ax			
cEnd

;ax = error code, grs.otxCur = text offset to error
ScanErr:
	mov	dx,[grs.GRS_otxCur]	;dx = otx of reported error
	mov	[txtErr.TXER_oSrc],UNDEFINED
					;for scanner detected errors, we cant
					;detect the column, compute from otx
;ax = error code, dx = text offset to error
ScanErrCommon:
	mov	[txtErr.TXER_errCode],ax
	

	
	cmp	dx,UNDEFINED		
	je	SetOtx			;brif otx of error is UNDEFINED

	;Scanner sometimes leaves grs.otxCur pointing within an opcode
	;Force it to an opcode boundary for accurate error reporting.
	;Scanner sets low bit of otx if the otx points to an oVar
	;within an opStDeclare opcode.
	
	mov	bx,dx			;bx = dx = otx of err
	and	dl,0FEh			;turn off low bit
	cmp	bx,dx
	jne	SetOtx			;brif error was within opStDeclare
	sub	ax,ax			;tell TxtSkipOp to start at StartOTx
SeOtxLoop:
	push	bx			;save otxErr
	push	ax			;save otxPrev
	cCall	TxtSkipOp		;ax = otx to next opcode after ax
	pop	dx			;dx = otxPrev
	pop	bx			;bx = otxErr
	cmp	ax,bx
	jbe	SeOtxLoop		;brif current opcode's otx <= otxErr
SetOtx:
	mov	[txtErr.TXER_otx],dx	;save real text offset to error
	mov	ax,[grs.GRS_oRsCur]
	mov	[txtErr.TXER_oRs],ax
	mov	al,[grs.GRS_fDirect]	
	mov	[txtErr.TXER_fDirect],al
	call	ChkWatchErr		;modify txtErr if error in Watch expr
;txtErr.otx = text offset of error
;txtErr.oSrc = column of error
;txtErr.errCode = error code
	sub	ax,ax			;return FALSE
	jmp	SHORT SctExit


;**************************************************************
; boolean SystemScan
; Purpose:
;	Scan all text tables to SS_EXECUTE
; Exit:
;	if any scanner or reparse errors were encountered:
;	   txtErr.errCode = an offset into the QBI Message Table (MSG_xxx) or,
;	     if high-bit set, ps.bdpError contains the parser-built
;	     ASCII error message,
;	   txtErr.fDirect is FALSE,
;	   txtErr.oRs identifies the text table with the error,
;	   txtErr.otx is an offset into the text table where the error
;	      was detected.
;	   txtErr.oSrc identifies the column within the source line where
;	      the error was detected.
;	else
;	   txterr.errCode = 0
;	condition codes set based on value in txterr.errCode == 0
;
;**************************************************************
cProc	SystemScan,<PUBLIC,FAR>
cBegin
	mov	[txtErr.TXER_errCode],0	;assume no errors

	test	[grs.GRS_flags],FG_allSsExecute
	jnz	SsExit			
NotDone:
	SetfDirect	al,FALSE	;tells scanner, parser we're not
					;scanning direct mode stmt,
					;if we find any errors, they won't be
					;in the direct mode buffer
	;See if user entered/loaded a $INCLUDE <filename> statement
	; and the file was not found, or saved an Include file.
	;If so, re-evaluate all $INCLUDE statements in all text tables.
	
	call	NotSavedInc		;save all unsaved INCLUDE files
	or	ax,ax			
					; if files saved ok, ax=0
					; if no need to save any, ax=-1
					; if CANCEL, ax=MSG_GoDirect
					; if NO, ax=-2
					; if I/O error, ax = error code
	jg	SsError			; brif I/O error or cancel
	inc	ax			;test for -1
	je	SsNoSave		;brif no files needed to be saved
	jl	SsCancel		;brif user decided not to save
					; one or more INCLUDE files
SsNoSave:
	call	TxtReInclude		;re-parse all $INCLUDE lines if
					; any INCLUDE mrs's have been saved
	or	ax,ax			;ax = txtErr.errCode
	jne	SsDone			;brif error (errors like FileNotFound
					; would not get caught in ReParse loop)

	call	TxtMoved		;remember pcode has moved


	mov	ax,-MSG_Compiling	;display Loading msg in intense video
	call	StatusMsgCP		; to tell user we're compiling

	;First make sure all DEFFNs/SUBs/FUNCTIONs are defined and terminated

	mov	al,FE_PcodeMrs+FE_CallMrs+FE_SaveRs
	mov	bx,CPOFFSET ReParseTbl
	call	ForEachCP		;re-parse until no more opReParse
					; opcodes in the system.
	je	SsDone			;brif error

	xor	ax,ax
	mov	[SsLineCount],ax
	cCall	UpdStatusLn,<ax>

	;Next, scan all text tables to SS_PARSE state
	mov	[scanTo],SS_PARSE
	mov	bx,CPOFFSET ScanTxtTbl
	call	ForEachTxtTbl
	je	SsDone			;brif error

	mov	[SsLineCount],0

	;Next, scan all text tables to SS_EXECUTE state
	;If we didn't do it in this order, it would be possible to
	;scan one text table to SS_EXECUTE, which contained references
	;to a function in another text table which was still in SS_RUDE
	;state.  This would be trouble, because the function's return
	;type (needed by the execute-scanner) would not have been
	;stored in the prs table entry by the rude scanner yet.
	
	dec	[scanTo]		;scanTo = SS_EXECUTE
.errnz	SS_PARSE - 1
.errnz	SS_EXECUTE
	mov	bx,CPOFFSET ScanTxtTbl
	call	ForEachTxtTbl
	je	SsDone			;brif error
	call	SsTrimCommon		;call scanner to trim back common tbls
	sub	bx,bx			;iWatch = 0
	call	WatchInfo		;can't update WatchInfo when static
					; structures are disabled
SsDone:
	call	StatusMsg0CP		;tell user we're done compiling

	cmp	[txtErr.TXER_errCode],0
	jne	SsExit
	or	[grs.GRS_flags],FG_allSsExecute
SsExit:
cEnd

SsCancel:
	mov	ax,MSG_GoDirect
SsError:
	mov	[txtErr.TXER_errCode],ax
	jmp	short SsDone



;**************************************************************
; ushort FAR TxtDirect()
;
; Purpose:
;	TxtDirect is called by the user interface to parse and
;	scan a direct mode statement.
;	It first parses direct mode statement to SS_RUDE,
;	then if stmt contains any opcodes which require the
;	  program to be scanned, SystemScan is called to scan
;	  all loaded text tables to SS_EXECUTE.
;	then direct mode buffer is scanned to SS_EXECUTE.
;	The reason we parse to SS_RUDE, instead of SS_PARSE,
;	direct mode statements like PRINT A(1) would cause the
;	array A() to be implicitly dimensioned, where the program,
;	which may still be in SS_RUDE, may have defined it AS STRING,
;	resulting in an error.
;	The reason we scan the entire program for statements like
;	A=1 is because the variable table wouldn't exist if the
;	module was in SS_RUDE state.
;
; Entry:
;	ps.bdpSrc contains the 0-terminated source line to be parsed.
;	grs.oMrsCur and grs.oPrsCur identify the scope of the direct
;	   mode statement.
;
; Exit:
;	If no errors were encountered,
;          [txtErr.TXER_errCode]  = return value = 0
;	   grs.bdlDirect contains the executable pcode,
;	   grs.fDirect = TRUE
;	   grs.otxCur = 0
;	If an error was encountered,
;	   return value = txtErr.errCode = an offset into the QBI Message
;	     Table (MSG_xxx) or, if high-bit set, ps.bdpError contains
;	     the parser-built ASCII error message,
;	   If the error was in the direct mode buffer,
;	      txtErr.fDirect is TRUE,
;	      txt.oSrc identifies the column where the error occurred.
;	   Else, the error was encountered in some program text as a result of
;	      ReParsing or Scanning.
;	      txtErr.fDirect is FALSE,
;	      txtErr.oRs identifies the text table with the error,
;	      txtErr.otx is an offset into the text table where the error
;	         was detected.
;	      txtErr.oSrc identifies the column within the source line where
;	         the error was detected.
;	   If direct mode command would have prevented CONT, and user
;	      wants to backout of the command, error code = UNDEFINED.
;
;**************************************************************
cProc	TxtDirect,<PUBLIC,FAR>,<si>
localW	oPrsPreParse
cBegin	TxtDirect

	mov	ax,[grs.GRS_oPrsCur]	;remember oPrs before parseline
	mov	[oPrsPreParse],ax	;in case an error occurs and
					;ParseLine moves us into a new RS.

	;Setting ps.flags.PSF_fBindVars to 0 tells the parser
	;not to bind variable references.  Direct mode statement
	;variable references are bound by rude scanner after entire program
	;has been scanned (and all variables declared).  Otherwise, a stmt
	;like "print a(1)" would cause array a to be implicitly dimensioned
	;when the program has an explicit DIM a(n) statement.
	
TdRetry:
	sub	ax,ax			;ax = 0
	mov	[ps.PS_flags],al
	mov	[txtErr.TXER_errCode],ax;assume no error
	dec	ax			;ax = UNDEFINED
	mov	[ps.PS_otxLine],ax
	SetfDirect al			;tells parser we're parsing a
					; direct mode stmt, if we find an error,
					; it will be in direct mode stmt

	call	ParseLine		;parse the direct mode stmt
	jnc	TdNoErr			;brif parser encountered no error
	jmp	TdParseErr

TdNoErr:
	push	[ps.PS_bdpDst.BDP_pb]
	PUSHI	ax,<CODEOFFSET tOpDirect>
	call	TxtFindOpDS		;ax = adr of 1st interesting opcode
					;dl = index into tOpDirect for opcode

;	*--------------------------------------------------------------------
;	* examine the pcode opcode by opcode:
;	*
;	*  -  Giving an error if any of the opcodes are illegal in direct mode
;	*	 (like opBolLab, opStDefFn, opStCommon etc.)
;	*
;	*  -  Converting any opcodes which are different in direct mode
;	*	 like opGoto->opGotoDirect etc.
;	*
;	*  -  Remembering if the opcode requires a system-scan.
;	*	 ParseLine() helps by setting ps.flags & PSF_fRef TRUE for all
;	*     opcodes which access variables/labels.
;       *
;	*--------------------------------------------------------------------
;
;NOTE: Since otxCur in this loop is an absolute pointer into DS, it is
;      important that no heap movement occur during the loop
;
TdLoop:
	xchg	si,ax			;si = otxCur
	xchg	ax,dx			;al=[txtFindIndex]
.errnz	DIR_opEot AND 0FF00H	;if tOpDirect has > 255 entries, must use ax
	cmp	al,DIR_opEot
	je	TdDone			;brif done with loop (at or beyond eot)

	cmp	al,DIR_ILLEGAL
	ja	TdLegal			;brif opcode is valid in direct mode

	;got opcode which is illegal in direct mode
	mov	ax,ER_ID		;Illegal in direct mode
J1_TdErr:
	jmp	TdErr

TdLegal:
	or	[ps.PS_flags],PSF_fRef	;remember to scan whole program
	cmp	al,DIR_DM_VARIANT
	ja	TdNotDmVar		;brif not a direct mode specific opcode

	;Got an opcode which has a direct mode variant.
	;Convert opStGoto -> opStGotoDirect etc.
	
	inc	WORD PTR [si]		;bump the opcode
	jmp	SHORT TdNext

TdNotDmVar:
	cmp	al,DIR_CANTCONT
	ja	TdNext			;brif opcode doesn't cause CantCont
;Since CantCont can cause movement we must convert si to an offset from
;the start of ps.bdpDst then after calling CantCont reconvert it back to
;a DGROUP offset
	sub	si,[ps.PS_bdpDst.BDP_pb] 
	call	CantCont
	add	si,[ps.PS_bdpDst.BDP_pb] 
TdNext:
	push	si			;pass otxCur (DS pointer)
	PUSHI	ax,<CODEOFFSET tOpDirect>
	call	TxtFindNextOpDS
	jmp	SHORT TdLoop

TdDone:
	test	[grs.GRS_flags],FG_RetDir
	je	TdNoDirRet		;brif no return address to direct
					; mode buffer is on the stack.
	call	AskCantCont_CP		;ask user "Want to back out?"
	jne	TdNoDirRet		;brif user doesn't want to back out
TdUndo:
	mov	ax,UNDEFINED
J2_TdErr:
	jmp	SHORT J1_TdErr

TdNoDirRet:

;If the parser saw a label in the direct mode statement then set the
;FG_OtxInDir grs flag; otherwise, clear it.
	and	[grs.GRS_flags],NOT FG_OtxInDir ;clear the flag
	test	[ps.PS_flags],PSF_fLabelRef	
	jz	@F				;brif no label ref parsed
	or	[grs.GRS_flags],FG_OtxInDir	;set the flag
@@:						

	;Make sure there's enough free space in the direct-mode
	;text table for the pcode we want to execute
	sub	ax,ax
	mov	[grs.GRS_bdlDirect_cbLogical],ax
	dec	ax			;ax = UNDEFINED
	SetfDirect al			;tells TxtFree we're dealing direct
					; mode buffer
	push	[ps.PS_bdpDst.BDP_cbLogical] 
	call	TxtFree
	mov	ax,ER_OM
	je	J1_TdErr		;brif Out-of-memory error
	PUSHI	ax,<dataOFFSET grs.GRS_bdlDirect>
	SetStartOtx ax			
	push	ax
	push	[ps.PS_bdpDst.BDP_pb]
	mov	ax,[ps.PS_bdpDst.BDP_cbLogical]
	push	ax		;pass [ps.PS_bdpDst.BDP_cbLogical]
	mov	[grs.GRS_bdlDirect_cbLogical],ax ;init logical size of dir buff
	call	BdlCopyTo

	test	[ps.PS_flags],PSF_fRef
	je	TdAllScanned

	;If AskCantCont was called above, current module (grs.oMrsCur)
	;may not be current context (grs.oMrsMain).  Make sure current
	;context is active before scanning, giving error if no main
	;module exists.
	
	mov	dx,UNDEFINED
	mov	ax,[grs.GRS_oMrsCur]
	cmp	[grs.GRS_otxCONT],dx
	jne	CanCont			;brif not starting program from scratch
	mov	ax,[grs.GRS_oMrsMain]	;else, activate MAIN module
	xchg	ax,dx			;ax=UNDEFINED, dx=oMrsMain
	cmp	ax,dx
	mov	ax,MSG_NoMainProg
	je	J2_TdErr		;brif no main module
	cCall	RsActivateCP,<dx>	;activate main module
	and	[grs.GRS_flags],NOT FG_allSsExecute ;force scantxttbl
					; invocation so all Data statments get
					; reset, even if all tables are in
					; SS_EXECUTE.
CanCont:

	;This command contains statements which require a system
	;scan to be performed.
	
	call	SystemScan		;scan all text tables to SS_EXECUTE

;  If SystemScan detected an error in the program, we still want to 
;scan the direct-mode-statement, so if the user types something like
;CALL FOO, and FOO is undefined, we will report that error before
;reporting something like Duplicate Definition of an array in the program.
;If there are no errors in the direct mode statement, we will still
;report any errors found by SystemScan, because txtErr.errCode is non-zero.

; If the current module is in SS_RUDE, we can not scan the direct mode 
; statement because there is no variable table.
	cmp	[txdCur.TXD_scanState],SS_RUDE
	je	TdExit

TdAllScanned:
	SetfDirect al,0FFh		;tells scanner we're scanning direct
					; mode buffer
	PUSHI	ax,SS_PARSE		;pass target state to SsRudeScan
	call	SsRudeScan		;scan direct mode stmt to SS_PARSE
	or	ax,ax			
	jz	NotCantCont		; brif no rude scanner error

	cmp	al,ER_CN
	jne	JNE1_TdErr		;brif not a "Cant CONT" type error
					; like creating a new variable when
					; variable tables are locked and can't
					; grow
	call	AskCantCont_CP		;ask user "Want to back out?"
	jne	J1_TdRetry		;brif user doesn't want to back out
J1_TdUndo:
	jmp	TdUndo			;brif user wants to back out
J1_TdRetry:
	jmp	TdRetry			;brif user doesn't want to back out
					; reparse line - we won't get Retry
					; again because AskCantCont called
					; CantCont.

NotCantCont:
	call	SsScan			;static scan direct mode statement
					;ax = scanner error (0 if none)
					;grs.otxCur = location of error
	or	ax,ax			
JNE1_TdErr:
	jne	TdErr			;brif parse->execute scanner got error

	mov	[grs.GRS_otxCur],0	;set otxCur to 0 (for executor)

;If no error occurred, txtErr.errCode = 0
;else
; tErr.errCode, txtErr.fDirect, txtErr.oRs, txtErr.otx, txtErr.oSrc
; must all be set up
;
TdExit:
	mov	ax,[txtErr.TXER_errCode]
cEnd	TxtDirect

;ParseLine encountered an error - handle it
; The parser could have moved us to a new rs for
; two cases. 1). It could have created a prs for
; a SUB/FUNCTION entered in direct mode, or 2)
; a DEF FN could have been active, and a rude
; edit occurred which the prs for the DEF FN to
; be freed, and its mrs to be activated.
;
; If oPrsCur has changed, one of the above cases
; has occured.	If txdCur is for an prs, then it must
; have been case 1.  If txdCur is for an Mrs, then it
; must have been case 2.

TdParseErr:
	mov	ax,[oPrsPreParse]	;get oPrs on entry for TxtParseUndo
	test	[txdCur.TXD_flags],FTX_mrs ;are we at module level?
	je	TdInSubFunc		;brif not
	mov	ax,[grs.GRS_oPrsCur]	;if so, don't reactivate oPrsPreParse,
					; since it could only have been for
					; a DEF FN, which we have freed.
TdInSubFunc:
	call	TxtParseUndo		;back out of the parser changes

	mov	ax,[ps.PS_oSrcErr]
	mov	[txtErr.TXER_oSrc],ax	;save column error occurred in
	test	[ps.PS_flags],PSF_UndoEdit
	jne	J1_TdUndo		;brif user said he wants to back out
					; of the edit while we were in ParseLine
					; (i.e. ParseLine called AskCantCont)

	;See if the parser wants us to try parsing this line again.  This can
	;happen when:
	; We saw something that made us need to ModuleRudeEdit, but part
	;     of the line's pcode had already been emitted in SS_PARSE
	; Variable manager could not add a variable, because variable heap
	;     was locked (because we can CONTinue).  Parser called AskCantCont
	;     and now wants us to try again (much easier than trying to call
	;     varmgr again from within parser).
	
	test	[ps.PS_flags],PSF_fRetry
	jne	J1_TdRetry		;brif ParseLine wants us to try again
	mov	ax,[ps.PS_errCode]
	and	ah,(PSERR_fAsciiMsg + PSERR_errCode) / 100h
					;mask off parser internal flags
	.errnz  PSERR_fAsciiMsg - 8000h	;caller assumes this is high bit
	jmp	SHORT TdErrOSrc

;Error occurred in direct mode statement, position cursor at column 0
;ax = error code
TdErr:
	mov	[txtErr.TXER_oSrc],0
	mov	[txtErr.TXER_otx],0
;ax = error code
TdErrOSrc:
	mov	[txtErr.TXER_errCode],ax
	mov	[txtErr.TXER_fDirect],1
	jmp	SHORT TdExit







;------------------------------------------------------------
;   WATCH Window Text Manager functions
;------------------------------------------------------------

;************************************************************
; OtxEndProg
; Purpose:
;	Return the text offset in the current text table
;	where WATCH pcode begins
; Exit:
;	grs.fDirect = FALSE
;	ax = text offset to opEndProg opcode
;
;************************************************************
PUBLIC	OtxEndProg
OtxEndProg PROC NEAR
	SetfDirect al,0			;don't search direct mode buffer
	mov	ax,[txdCur.TXD_bdlText_cbLogical]
	sub	ax,CB_EMPTY_TEXT-StartOtx ;default oTxEndProg if no watch
					  ; pcode is active is 
					  ; cbLogical - (CB_EMPTY_TEXT-StartOtx)
	test	[flagsTM],FTM_WatchPcode ;is there any watch pcode anywhere?
	je	OtxEndProgExit		;brif not

	sub	ax,ax
	push	ax			;search text offset = 0
	PUSHI	ax,<CODEOFFSET tOpWatch>
	call	TxtFindOp		;ax = text offset to opEndProg
OtxEndProgExit:
	ret
OtxEndProg ENDP


sEnd	CP

end
