	TITLE	RTTERM - Termination module for BASIC runtime
;***
;RTTERM.ASM - Termination module for BASIC runtime
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module contains the termination routines for the BASIC 3.0
;	core runtime.  This module will always be present.
;
;******************************************************************************

	INCLUDE switch.inc
	INCLUDE rmacros.inc

;
;	Code Segments
;
	USESEG	<RT_TEXT>	;runtime core
	USESEG	<DV_TEXT>	; DV component I/O deinstall
	USESEG	<ER_TEXT>	;runtime error handler

;
;	Data Segments
;
	USESEG	<_DATA> 	;runtime data (initialized)
	USESEG	<_BSS>		;runtime data (uninitialized)
	USESEG	<BC_SAB>	;beginning of table of module start addresses
	USESEG	<BC_SA> 	;table of module start addresses
	USESEG	<BC_SAE>	;end of table of module start addresses
	USESEG	<XCB>		;beginning of C terminators
	USESEG	<XC>		;C terminators
	USESEG	<XCE>		;end of C terminators

	INCLUDE seg.inc
	INCLUDE messages.inc	;message/error definitions
	INCLUDE addr.inc
	INCLUDE files.inc
	INCLUDE event.inc	

	SUBTTL	Runtime data definitions for BASIC Runtime Core
	PAGE
;
;	Code externals
;
	externFP	B$IStop ;executor for STOP/BREAK in interpreter.
	externFP	exStEnd ;executor for SYSTEM/END in interpreter
	externFP	B$ULTerm ;user library termination
	externFP	UiTerm	;unhook UI interrupt vectors

	externFP	B$RTMDeinstall ;deinstall the RTM interrupt vector

	externFP	_exit	; C startup exit routine

sBegin	ER_TEXT 		
	externNP	B$ERR_NR	;No Resume error
sEnd	ER_TEXT 		


sBegin	RT_TEXT 		
	externFP	B$COMP_DISP	;common component dispatcher
	externNP	B$PUTNUM	;put numbered message
sEnd	RT_TEXT

sBegin	BC_SAE			
	DD	0		;terminate the start address list
sEnd	BC_SAE			

sBegin	XC			
	DD	B$TERM 		;stuff address of BEND in c-term vectors
sEnd	XC			

sBegin	_DATA

	externW b$term_disp	;"ONE" time termination dispatch vectors
	externW b$end_disp	;"END" time termination dispatch vectors
	externB b$inonerr	;on error in progress flag
	externW b$errnum	;Error number
	externB b$EventFlags	;Break key hit flag

sEnd	_DATA

sBegin	_BSS

	externB b$fRTInit

sEnd	_BSS


	SUBTTL	Runtime Core Termination
	PAGE
assumes CS,RT_TEXT		
sBegin	RT_TEXT			

;***
;B$CENP - Default termination routine for the runtime core.
;PLM B$CENP()
;
;Purpose:
;	Runtime entry point.  The last call in any basic module is a
;	call to B$CENP.  This is the default end procedure when no
;	explicit END or SYSTEM is performed.  This routine checks
;	to make sure that we are not in the middle of an ON ERROR.  If
;	ON ERROR is in progress then we issue a NO RESUME error and
;	let the error handler take over.  Otherwise we call a common
;	routine to clean up and terminate.
;
;Entry:
;	b$inonerr - non zero if ON ERROR is in progress.
;
;Exit:
;	None.
;
;Uses:
;	None.
;
;Exceptions:
;	Does not return.
;****
cProc	B$CENP,<PUBLIC,FAR,FORCEFRAME> 
cBegin
	CMP	b$inonerr,0	;Is error in progress?
	JZ	NOTERR		;continue if not
	JMP	FAR PTR B$ERR_NR ;No RESUME error
NOTERR:
	MOV	b$errnum,0	;reset error number for non error exit.
	JMP	exStEnd 	;branch to interpreter END executor
cEnd	nogen			
	PAGE
;***
;B$CEND,B$IEND - Termination routine for the runtime core.
;PLM B$CEND()
;PLM B$IEND()
;
;Purpose:
;	Runtime entry point for SYSTEM and END.  Calls common routine
;	to clean up and terminate runtime.
;	B$CEND is specific to the compiler, B$IEND is specific to
;	the interpreter.  Note that compiled code executing an END in
;	the interpreter environment will cause the runtime to JMP to
;	the interpreter END executor.
;
;Entry:
;	None.
;
;Exit:
;	None.
;
;Uses:
;	None.
;
;Exceptions:
;	Does not return.
;****
cProc	B$CEND,<PUBLIC,FAR,FORCEFRAME> 
cBegin
	MOV	b$errnum,0	;reset error number for non error exit.
	JMP	exStEnd 	;branch to interpreter END executor
				;fall into B$END (doesn't return)
cEnd	nogen			

labelFP <PUBLIC,B_IEND> 	;Intepreter reachable label
cProc	B$IEND,<PUBLIC,FAR,FORCEFRAME> 
cBegin				
	CALL	CloseFiles	; close all files NOW, while we can still
				; print error messages
	cCall	B$ULTerm	;terminate user libraries
	MOV	b$errnum,0	;reset error number for non error exit.
	JMP	SHORT EndNoClose ; go into B$END (doesn't return)
cEnd	nogen			
	PAGE
;***
;B$END - Common termination routine for the runtime core.
;PLM B$END()
;DBCS-callback
;
;Purpose:
;	Common routine to perform runtime termination.	Indirectly
;	dispatches to all routines in the b$rtterm dispatch vector.
;	This routine exits to DOS by calling the c termination routine
;	_exit.	_exit will then call B$TERM to finish runtime termination
;
;Entry:
;	None.
;
;Exit:
;	None.
;
;Uses:
;	None.
;
;Exceptions:
;	Does not return.
;****
cProc	B$END,<PUBLIC,NEAR>			
cBegin

	CALL	CloseFiles	; close all files NOW, while we can still
				; print error messages.  If we do get an
				; error, we will eventually make it back
				; here.

EndNoClose:			; entry point for B$IEND
	XOR	AX,AX		;0 termination code
	cCall	_exit,<AX> 	; does not return
cEnd	<nogen> 		

cProc	CloseFiles,<NEAR>	
cBegin
	MOV	b$inonerr,-1	; don't allow ON ERROR after this point
	CMP	b$fRTInit,0	; was RT ever initiliazed?
	JZ	@F		; brif no, the following would hang
	MOV	SI,OFFSET DGROUP:b$end_disp ; "END" time termination vecs
	cCall	B$COMP_DISP	; indirectly call END time term, which
				; will close all files.  Do this NOW so
				; that errors are recoverable.


@@:				
cEnd				
	PAGE
;***
;B$TERM - Common termination routine for the runtime core.
;PLM B$TERM ()
;
;Purpose:
;	Common routine to perform runtime termination.	Indirectly
;	dispatches to all routines in the b$rtterm dispatch vector.
;	This routine is called by _exit (c termination) when all XC
;	segment routines are dispatched to.
;
;Entry:
;	b$enddisp - contains dispatch addresses for component specific
;		    "END" time termination routines.  Points to B$NearRET if
;		    no specific termination is needed.
;	b$termdisp - contains dispatch addresses for component specific
;		    "One" time termination routines.  Points to B$NearRET if
;		    no specific termination is needed.
;
;Exit:
;	None.
;
;Uses:
;	None.
;
;Exceptions:
;	Does not return.
;****
cProc	B$TERM,<PUBLIC,FAR>,<SI>		
cBegin

;	If chaining in progress, just return.  No BASCOM termination
;	should be done.


;	NOTE: QBI calls UiInit after B$IINIT.	This is the matching
;	call to UiTerm.  It is placed in the runtime for code size,
;	as all exit paths will come through this code.

	cCall	UiTerm				;Unhook UI interrupts
@@:						

;	Deinstall the RTM interrupt vector.  This is placed first to
;	to ensure that the vector is always installed and deinstalled
;	once under all circumstances.

	cCall	B$RTMDeinstall 		;deinstall the RTM interrupt vector

	CMP	b$errnum,FE_NOTHINGBASE	;check for fatal init errors
	JAE	EXITQUICK		;brif fatal init error
	CALL	CloseFiles		; "End" time termination.  Should
					; already be done.  Just doing
					; it to be safe.
	MOV	SI,OFFSET DGROUP:b$term_disp	;"One" time termination vecs
	cCall	B$COMP_DISP			;do ONE time termination
EXITQUICK:
	mov	b$fRtInit,0		; Runtime no longer initialized
cEnd						


;***
; B$BREAK
;
;Purpose:
; Moved with revision [16].
; Internal entry point to print Break message and terminate.
;
;Entry:
; none
;
;Exit:
; Doesn't
;
;Modifies:
;	ALL
;*****
cProc	B$BREAK,<NEAR,PUBLIC>
cBegin

	AND	b$EventFlags,NOT CNTLC ; reset break flag
	JMP	B$IStop		; let interpter stop execution

cEnd	nogen

;***
; B$IEndSt   - END Statement support for interpreter.
;PLM B$IEndSt()
;
;Purpose:
;	Added as part of [13].
;	This routine supports the interpreter end statement.  This routine
;	closes files, resets devices etc.
;
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	None.
;Exceptions:
;	None.
;****
cProc	B$IEndSt,<PUBLIC,FAR>,<SI>
cBegin
	MOV	SI,OFFSET DGROUP:b$end_disp	;"END" time termination vecs
	cCall	B$COMP_DISP			;indirectly call END time term.
	MOV	b$inonerr,0			;reset on error flag
cEnd
sEnd	RT_TEXT
	END
