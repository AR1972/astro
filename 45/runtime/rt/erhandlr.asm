	TITLE	ERHANDLR - Error trapping for 8086 Basic Compiler
        page ,132
;***
; ERHANDLR - Error trapping for 8086 Basic Compiler
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
; This module contains the run-time support for on-error handling.
;
;******************************************************************************
	INCLUDE switch.inc	;Rutime switch file
	INCLUDE rmacros.inc	;General runtime macros

	useSeg	_BSS		
	useSeg	_DATA		
	useSeg	ER_TEXT 	
	useSeg	NH_TEXT 	

	INCLUDE seg.inc 	;Segment definitions
	INCLUDE addr.inc	;Frame and module specific data definitions
	INCLUDE	baslibma.inc	; need the SKIP macro
	INCLUDE idmac.inc	
	INCLUDE messages.inc	;error message label definitions
	INCLUDE stack.inc	


sBegin	_BSS			

externW b$errnum		; Error number encountered
externB b$inonerr		; flag indicating we're in an on error
externW b$curlevel		; current program level
externW	b$curframe		; current BASIC frame parameter
externD b$errmod		; error module name address
externW	b$cCSubs		
externW	b$cNonQBIFrames		

sEnd	_BSS			

sBegin	NH_TEXT 		
externNP B$STDALCALLTMP 	
sEnd	NH_TEXT 		

externFP	B$CONERR	
externFP	B$ClearRange	

sBegin	ER_TEXT 		

externNP B$GETMODCODE		;Get module specific code data.
externNP	B$IONERR	; interpreter-specific on error handler
externNP	B$IErrSaveState	
externNP	B$IErrRestState	
externNP B$ERR_RE		


assumes CS,ER_TEXT		

;******************************************************************************
;
; Runtime Error Handling
;
; ON ERROR processing
;
; When an error occurs, the common runtime error handler will first call
; B$ONERR to allow us to attempt to process any ON ERROR statement that
; may apply. To get the runtime context, we rely on the
; fact that the passed frame pointer points to stack entries as follows:
;
;	  +-->	+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;	User	|						|
;	Program \   Defined by Compiler/Interpreter & Runtime	\
;	Frame	|						| <-+
;	  +-->	+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+   |
;		|						|   |
;		\ 	User program local storage		\   |
;		\   Gosubs on stack on common runtime entry	\   |
;		|						|   |
;	  +-->	+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+   |
;	  |	|						|   |
;	  |	\ 	Parameters to Common Runtime Entry	\   |
;	Common	|						|   |
;	Runtime +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+   |
;	Entry	| CS						|   |
;	Frame	+  Far return address into BASIC program code	+   |
;	  |	| IP						|   |
;	  |	+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+   |
;	  |	|	BP value on entry to Common Runtime	| --+ <- pframe
;	  +-->	+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;
; From this frame we get the the user program frame (defined elsewhere), from
; which we can find the data structure containing the ON ERROR address for the
; user program if it exists. If this does exist we can perform the ON ERROR.
;
; To process the ON ERROR, we first use the frame to determine the line number
; at which the error occured. The stack is then reset to an appropriate level,
; and we perform the GOSUB.
;
; See erproc.asm for more information on error handling.
;
;******************************************************************************

	SUBTTL	B$ONERR -- Find ON ERROR handler if possible
	PAGE
;
;*** 
;B$ONERR - Find most recent BASIC error handler if any
; Call via [b$pONERR] for /O granularity.
;
;Purpose:
;	This routine will search through BASIC stack frames starting with
;	the one in which the error occurred until it finds one with an
;	error handler or until there are no more BASIC stack frames left 
;	to check.  It will also stop if it encounters a BASIC event handler
;	at the previous level.  If it finds an error handler, it will adjust
;	the environment to look as if the error occurred at the current 
;	statement of that module.  Otherwise, nothing is changed. 
;
;Entry:
;	[b$curframe] valid.
;	[BX] = frame parameter preceding [b$curframe] in BP chain.
;	IF EI_QB THEN [b$cCSubs], [b$cNonQBIFrames] valid.
;	[b$errnum] <> 98xxh.
;
;Exit:
;	No error handler found.
;
;Uses:
;	Per convention.
;
;Preserves:
;	None.
;
;Exceptions:
;	None.
;
;******************************************************************************
cProc	B$ONERR,<PUBLIC,NEAR>
cBegin

DbAssertRelB	<BYTE PTR [b$errnum+1]>,NE,<98h>,ER_TEXT,<Bad errnum in B$ONERR>
DbAssertRel	<[BX]>,E,<[b$curframe]>,ER_TEXT,<Bad frame parameter in B$ONERR>

	PUSH	[b$curframe]
	PUSH	[b$curlevel]
	PUSH	[b$cCSubs]		; save original [b$cCSubs]
	PUSH	[b$cNonQBIFrames]	; save original [b$cNonQBIFrames]
	cCall	B$IErrSaveState		; allow interpreter to save state
;
; This is the main loop that walks the BASIC frame chain and calls either
; B$IONERR or B$NCONERR as appropriate.
;
ONERR_10:
	CMP	[b$curframe],0
DJMP	JZ	ONERR_EXIT		; reached the end of BASIC chain
;
; Skip over non-BASIC frames in the BP-chain to find the frame immediately
; preceding the BASIC frame in the BP chain.
;
ONERR_20:
	MOV	DX,BX			;DX = old value of BX
	MOV	BX,[BX]			;use bp chain
DbAssertTsT	BX,Z,1,ER_TEXT,<Odd BP value found in B$ONERR> 
DbAssertRel	BX,NZ,0,ER_TEXT,<Bad BP chain in B$ONERR>
	CMP	BX,[b$curframe]
	JNE	ONERR_20
	; [DX] = frame preceding [BX]
	PUSH	DX			; parameter to B$xONERR
	CMP	[b$cCSubs],0		; check for interpreting module
	JNZ	ONERR_30		
	cCall	B$IONERR		;returns AX = 0 => quit
	JMP	SHORT ONERR_40
ONERR_30:
	cCall	B$NCONERR		; returns AX = 0 => quit
ONERR_40:
	OR 	AX,AX			;AX <> 0 => no error handler
	JZ	ONERR_EXIT		;quit - restore org. env. & return
DbAssertRelB	<[b$inonerr]>,Z,0,ER_TEXT,<b$inonerr was TRUE in B$ONERR loop>
	DEC	[b$curlevel]
	JS	ONERR_EXIT		; kind of redundent check
; addjust [b$cCSubs] to reflect current frame
;
	CMP	[b$cCSubs],0		; are we already interpreting?
	JZ	ONERR_60		; brif interpreting
	DEC	[b$cCSubs]		; adjust [b$cCSubs]
	JNZ	ONERR_60		; need to adjust b$cNonQBIFrames?
	DEC	[b$cNonQBIFrames]	; yes -- decrement
ONERR_60:
;
; Walk the BASIC frame chain to check for an error handler in the next
; previous BASIC module.
;
	MOV	BX,[b$curframe]
	MOV	AX,[BX].FR_BFRAME	;[AX] = NEXT BASIC FRAME
	MOV	[b$curframe],AX		;update b$curframe
DJMP	JMP	SHORT ONERR_10		

					; error handler not found
ONERR_EXIT:				; restore original error state
	cCall	B$IErrRestState		; allow interpreter to restore state
	POP	[b$cNonQBIFrames]	; restore original [b$cNonQBIFrames]
	POP	[b$cCSubs]
	POP	[b$curlevel]
	POP	[b$curframe]
cEnd

	SUBTTL	B$CONERR - Compiler ON ERROR Processing - Enhanced version
	PAGE
;***
; B$CONERR - Handle ON ERROR Processing -new version
; B$NCONERR - New error handling COMPILER ON ERROR processing
; void B$CONERR(U2 pframe)
;
;Purpose:
; If enabled, performs ON ERROR transfer of control within the user program.
; This is the NEW version of B$CONERR.  Changes and enhancements should only be
; made to this version.  The old version in erbcproc.asm should not be changed.
; It is necessary for backward compatibility with QB4 QLB's.
;
; We check the module data area for the address of an ON ERROR routine. If
; one is available, we:
;	- Save the error address (offset & segment)
;	- Get and save the statement number
;	- If the error was "Out Of Memory" reset the gosub count to zero
;	- Deallocate local storage and temp. string descriptors as necessary
;	- Reset the stack (SP & BP) to execution levels:
;		BP = [b$curframe].
;		SP = [b$curframe] - <local storage> - (<GOSUB level> X 2).
;	- Perform the GO TO
;
;Entry:
; pframe	= Pointer to a frame one level below b$curframe
; [b$curframe]	= basic-level frame.
; [b$errnum]	= error number
;
;Exit:
; [b$errmod]	= far address of module name
; IF AX = 0 THEN
;	no error trapping is possible
; ELSE
; 	no error handler found
; ENDIF
; IF an error handler is found THEN
;	[b$erradr]	= far address of error in the compiled code
;	[b$errlin]	= line number of error
; ENDIF
;
; Doesn't return if an active BASIC error handler is available.
;
;******************************************************************************

cProc	B$NCONERR,<PUBLIC,NEAR>	; new name for B$CONERR

parmW	pframe			; frame pointer one level below error time
cBegin

	XOR	AX,AX		; Get first byte
	cCall	B$GETMODCODE	; AL = first byte, [BX] = code segment
	MOV	AX,CX		; CX = module header offset (0 for DOS3)
	ADD	AX,OF_MOD	
	MOV	WORD PTR [b$errmod],AX ; Far pointer to module name
	MOV	WORD PTR [b$errmod + 2],BX 

	CMP	[b$inonerr],FALSE ; Are we in on-error handler?
DJMP	JNZ	BCONERR_90	; Jump if we are, error-within-error occurred
	MOV	AX,b$errnum	; get error number
	OR	AH,AH		; See if valid trappable error
	JNZ	BCONERR_90	; Jump if [b$errnum] > 255, internal error

	XCHG	DX,BX		; DX = user code segment
	PUSH	CX		; save module header offset
	POP	BX		; restore module header offset
	PUSH	DS		; save DGROUP
	MOV	DS,DX		; [DS] = user code seg
	MOV	BX,DS:[BX].OF_DAT ; [BX] = mod specif data offset
	POP	DS		; [DS] = DGROUP restored
	MOV	CX,[BX].OFD_ONERROR ; [CX] = on error address
	MOV	BX,pframe	; [BX] = pointer to rt entry frame
DbAssertRel	DX,E,<[BX].FR_RETSEG>,ER_TEXT,<Bad user code seg in B$NCONERR>
	PUSH	BX		; push frame parameter for old B$CONERR
	JCXZ	BCONERR_80	; Jump if no address specified - quit
;
; The error can be trapped. Set the "in on error handler" flag, and call the
; appropriate on error handler.
;
	MOV	AX,[BX] 	; AX = [b$curframe]
	MOV	BX,AX		; BX = AX = [b$curframe]
	SUB	AX,[BX].FR_CLOCALS ; [AX] = [b$curframe] - locals/temps
	SUB	AX,[BX].FR_GOSUB ; [AX] = [b$curframe] - locals - gosubs/2
	SUB	AX,[BX].FR_GOSUB ; [AX] = [b$curframe] - locals - gosubs
	SUB	AX,FR_SIZE+2	; can combine 3 instructions for QB case
	PUSH	BP		; old_BP = FROM parameter to B$ClearRange
	PUSH	AX		; new_SP-2 = TO parameter to B$ClearRange
	CALL	B$ClearRange	; deallocate local variables on STACK
	MOV	AX,[b$curlevel]	; deallocate temp strings > [b$curlevel]
	CALL	B$STDALCALLTMP	; dealloc temp strings no longer needed

BCONERR_80:			; no error handler - check for event handler
	cCALL	B$CONERR	; frame parameter pushed way above
	MOV	[b$inonerr],FALSE ; necessary after call to old B$CONERR
; check if the curent frame is for an event handler
	MOV	BX,[b$curframe]	
	CALL	B$CEvtHndlr	; returns AX = 0 if an event handler frame
	SKIP	2		; JMP SHORT BCONERR_100
BCONERR_90:			
	XOR	AX,AX		; return AX = 0 means process fatal error
BCONERR_100:			
cEnd

	SUBTTL	RESUME helpers
	PAGE
;***
; B$RESUMED - Set up for RESUME of some sort
; Interpreter Entry
; Moved here from erproc.asm
;
;Purpose:
; If RESUME [linenumber|linelabel], reset program level to 0 and deallocate
;	all temporary strings.
; Ensure that error is in progress, and then clean flags
;
;Entry:
; [AX]	= RESUME type, 0     = RESUME [linenumber|linelabel]
;		       non-0 = other RESUME
;	(parameter on stack for OE_WIN)
;
;Exit:
; if ready to resume. Flags cleared
;
;Exceptions:
; Aborts if no error in progress
;
;******************************************************************************

cProc	B$RESUMED,<PUBLIC,FAR,FORCEFRAME> ; moved here from erproc.asm
cBegin				
	OR	AX,AX		;resume to level 0?
	JNZ	NotLev0 	;go if not
	MOV	b$curlevel,AX	;set current level to main level (0)
	CALL	B$STDALCALLTMP	;dealloc all temp strings
NotLev0:			
	cCall	B$RES_SETUP	; Perform Function
cEnd				


;***
; B$RES_SETUP - Set up for RESUME of some sort
; Moved here from erproc.asm
;
;Purpose:
; Ensure that error is in progress, and then clean flags
;
;Entry:
; none
;
;Exit:
; if ready to resume. Flags cleared
;
;Exceptions:
; Aborts if no error in progress
;
;******************************************************************************
cProc	B$RES_SETUP,<NEAR,PUBLIC> ; moved here from erproc.asm
cBegin				

	XOR	CX,CX		
	MOV	[b$errnum],CX	; if error below, this gets reset anyway
	XCHG	CL,[b$inonerr]	
	JCXZ	RES_SETUP_OOPS	; brif wern't in error handler
cEnd				

RES_SETUP_OOPS: 		
	POP	AX		; Discard local return
	JMP	B$ERR_RE	

;*** 
;B$CEvtHndlr - Checks if a frame is an event handler
;
;Purpose:
;	Determine whether the frame is for an active compiled event handler.
;	Moved this code back with [6].
;
;	NOTE: this code is duplicated in ERROR.ASM.
;
;Entry:
;	BX = pointer to the COMPILED BASIC frame to check (ie. [b$curframe]).
;
;Exit:
;	AX = 0 IFF an event handler is detected.
;
;Uses:
;	AX, BX.
;
;Exceptions:
;	None.
;
;******************************************************************************
cProc	B$CEvtHndlr,<NEAR>	
cBegin
;
; This is the compiler-specific logic to check for a BASIC event handler:
;
	MOV	AX,[BX].FR_GOSUB ; AX = count of GOSUBS
DbAssertRel	AX,GE,0,ER_TEXT,<More than 32767 GOSUBs in B$CEvtHndlr> ;
	DEC	AX		; if AX = 0, return AX <> 0
	JS	No_handler	; no gosubs => no event handler
	SUB	BX,[BX].FR_CLOCALS ; BX = BX - number of local bytes
	MOV	AX,[BX-FR_SIZE-2] ; AX = 0 IFF event handler
No_Handler:
cEnd				; return

sEnd	ER_TEXT 		


	END
