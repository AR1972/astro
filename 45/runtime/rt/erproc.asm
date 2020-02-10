	TITLE	ERPROC.ASM - Common Runtime Error Processor
	PAGE	56,132
;***
; ERPROC.ASM - Common Runtime Error Processor
;
;	Copyright <C> 1986, 1987 Microsoft Corporation
;
;Purpose:
; Contains the common runtime entry point for generalized error handling. Also
; includes the runtime internal entry point for error handling.
;
;******************************************************************************
	INCLUDE switch.inc	;Runtime switch file
	INCLUDE rmacros.inc	;General runtime macros

	useSeg	ER		;We use the error code segments
	useSeg	GR_TEXT 	
	useSeg	RT_TEXT 	
	useSeg	NH_TEXT 	
	useSeg	_TEXT 		
	useSeg	_BSS		; we use _BSS
	useSeg	_DATA		;And we use _DATA

	INCLUDE seg.inc 	;Segment definitions
	INCLUDE baslibma.inc	; Skip macro
	INCLUDE compvect.inc	; component dispatch vectors structures
	INCLUDE idmac.inc	;  Internal debugging stuff
	INCLUDE rtps.inc	; constants shared with QBI
	INCLUDE const.inc	; misc constants
	INCLUDE files.inc	; CALLOS macro and constants

ASCLF	=	0AH		;Linefeed character
	SUBTTL	Data Definitions
	PAGE
sBegin	_DATA
; *****************************************************************************
;
;	Data
;
; *****************************************************************************

	externW b$curframe	; current user frame pointer
	externW b$errnum	;Error number
	externW b$cCSubs	;flag indicating compiler in execution
	externB	b$messages	; drag in C startup messages
	externD	b$errmod	; error module name address
	externB b$inonerr	;flag indicating on-error in progress

	externW b$err_disp	; error dispatch table
	externB b$PRFG		; print using active flag
	externW __aexit_rtn	; terminating function


sEnd	_DATA

sBegin	_BSS			

	globalB b$errinfo,,1	; Supplemental error info
	globalW b$errvec,,1	; error vector (0=none, else address)

	globalW b$pSTDALCTMP,B$NearRet ; indirect B$STDALCTMP vector
	globalW b$pSTDALCALLTMP,B$NearRet ; indirect B$STDALCALLTMP vector


	externW b$curlevel	;current program level

sEnd				

	SUBTTL	Code Definitions
	PAGE
; *****************************************************************************
;
;	External Routines
;
; *****************************************************************************
	externFP __FMSG_TEXT	; Get message text from number


	externFP __fpreset	; reset mathpack

	externFP _exit		

sBegin	RT_TEXT

	externNP B$NearRet	; stub routine

	externFP B$COMP_DISP	; component dispatcher
sEnd	RT_TEXT

sBegin	ER_TEXT 		;All code goes in this segment

	externNP B$FERROR	;Product Fatal Runtime Error handler
	externNP B$ONERR	; don't drag it in for /O modularity

	externFP B$CONERR	;Compiler ON-ERROR handler
	externNP B$GETDS 	

assumes CS,ER_TEXT		;only cs

	SUBTTL	Error Handling Overview
	PAGE
;******************************************************************************
;
;	Common Runtime Error Handling
;
;	The common runtime error handler serves primarily as a funnel for all
;	error processing. Actual error processing, such as ON ERROR or fatal
;	error handling, is specific enough to each environment, that the
;	details of that are left to the compiler or interpeter specific error
;	handling routines.
;
;	There are two entry points:
;
;	B$SERR is the common runtime error handling routine. This is used by
;	code external to the common runtime (like the interpeter or compiler)
;	and accepts as a parameter a valid BASIC error number.
;
;	B$RUNERR is the runtime internal entry point, and accepts an extended
;	range of error numbers, to allow for non-trappable non-BASIC errors
;	(such as initialization errors, internal errors and the like), that we
;	still want to funnel through here.
;
;	If we have an error during the C startup initialization, we jump
;	directly to the routine FatalError.  If we were in an input statement
;	when the error occurred, we jump to a special input error handler.
;	Otherwise, we call B$ONERR and try to find a BASIC error handler.
;
;	B$ONERR will save the initial error state and then walk the chain of
;	BASIC frames until it finds an active error handler or it gives up and
;	returns to B$RUNERR.  If B$ONERR returns, we call B$FERROR to print a
;	fatal error message and die.  B$ONERR will return if the the error is
;	not a valid trappable BASIC error or if we were already in a BASIC 
;	error handler when the error occurred.  It will also return if it 
;	reaches the end of the BASIC frame chain without finding an active
;	BASIC error handler or if it finds an active event handler first.
;	In any of these situations where an error handler is not found, we
;	restore the original error state and call B$FERROR.
;
;	At each stage of walking the BASIC frame chain we determine whether
;	that particular BASIC frame was from a compiled or interpreted module
;	and call either B$IONERR (interpreter-specific routine) or B$NCONERR
;	(compiler-specific routine) as appropriate.  These routines do most
;	of the actual work for B$ONERR.  If they find a BASIC error handler,
;	they clean up the stack, deallocating any necessary local storage,
;	and jump to the BASIC error handler.  Otherwise, they return to 
;	B$ONERR with a flag set to indicate what the problem was.  B$ONERR uses
;	this flag in along with other information to decide whether to keep
;	walking the BASIC frame chain or to give up and return to B$RUNERR.
;
;	Note:  the environment specific ON ERROR code MUST make use of the
;	frame to determine context for error recovery.
;
;	BP points to the runtime entry frame as below:
;
;		^	Stack on common runtime entry		^
;		|						|
;		+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;		|						|
;		\ 	Parameters to Common Runtime Entry	\
;		|						|
;		+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;	BP-4 -> | CS						|
;		+	Far return address to calling code	+
;	BP-2 -> | IP						|
;		+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;	BP   -> |		BP value on entry		|
;		+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;		|						|
;
;	As noted above, the common runtime error handler can be invoked either
;	of two ways, and the contents of the stack differ slightly. If B$SERR
;	is called, then the "Parameters to Common Runtime Entry" is the error
;	number, and the far return is to the routine which called B$SERR. If
;	B$RUNERR is called from within the runtime, then the parameters and
;	return address relate to the original common runtime entry point which
;	was called.
;
;	If B$ONERR does return to B$RUNERR, then the error was not trappable.
;	The common error handler then branches to B$FERROR, the fatal error
;	handler with the error number as a parameter, and with BP as above.
;	(Note that this implies BP was not altered by the routine B$ONERR.)
;	B$FERROR is a product specific routine which is to handle the fatal
;	error as appropriate, either by printing and terminating in the case of
;	the compiler; or by printing a message, cleaning off the stack, and
;	returning to a known state in the case of the intepreter.
;
;	External environment specific routines required:
;
;	void pascal B$NCONERR(U2 errnum)   /* new compiler ON ERROR handler */
;	void pascal B$CONERR(U2 errnum)    /* old compiler ON ERROR handler */
;	void pascal B$FERROR(U2 errnum)    /* product fatal error handler   */
;	void pascal B$IONERR(U2 errnum)    /* interpeter ON ERROR handler   */
;
;******************************************************************************
	SUBTTL	B$ERR_?? - Runtime internal error vectors
	PAGE
;***
; B$ERR_?? - Runtime Internal Errors
;OEM-callback routine
;
;Purpose:
; Declare a particular error. Used internal to the runtime, these vectors
; are jumpped to to declare an error of a particular type.  If applicable,
; the ON ERROR handler will be called, otherwise the program will terminate
; (compiler) or return to the interpreter.
;
; The error that is defined is specified by a 2 or 3 digit code that replaces
; the ?? in B$ERR_??.	Thus, the error for illegal function call (FC) would
; be B$ERR_FC.  All the defined error functions are listed below, along
; with the basic error number and the text of the error.
;
; These entries do NOT modify the stack or frame in any way.
;
; The following errors are defined:
;
;		  Basic
;     Error	Error Code	Text of Error
;     -----	----------	-------------
;     B$ERR_SN     2		"Syntax error"
;     B$ERR_RG     3		"RETURN without GOSUB"
;     B$ERR_OD     4		"Out of DATA"
;     B$ERR_FC     5		"Illegal function call"
;     B$ERR_OV     6		"Overflow"
;     B$ERR_OM     7		"Out of memory"
;     B$ERR_BS     9		"Subscript out of range"
;     B$ERR_DD     10		"Duplicate definition"
;     B$ERR_DV0    11		"Division by zero"
;     B$ERR_TM     13		"Type mismatch"
;     B$ERR_OS     14		"Out of string space"
;     B$ERR_ST     16		"String formula too complex"
;     B$ERR_NR     19		"No RESUME"
;     B$ERR_RE     20		"RESUME without error"
;     B$ERR_DTO    24		"Device timeout"
;     B$ERR_DVF    25		"Device fault"
;     B$ERR_OTP    27		"Out of paper"
;     B$ERR_RVR    40		"Variable required"
;     B$ERR_FOV    50		"FIELD overflow"
;     B$ERR_INT    51		"Internal error"
;     B$ERR_IFN    52		"Bad file name or number"
;     B$ERR_FNF    53		"File not found"
;     B$ERR_BFM    54		"Bad file mode"
;     B$ERR_FAO    55		"File already open"
;     B$ERR_FSA    56		"FIELD statement active"
;     B$ERR_IOE    57		"Device I/O error"
;     B$ERR_FAE    58		"File already exists"
;     B$ERR_BRL    59		"Bad record length"
;     B$ERR_DFL    61		"Disk full"
;     B$ERR_RPE    62		"Input past end of file"
;     B$ERR_BRN    63		"Bad record number"
;     B$ERR_BFN    64		"Bad file name"
;     B$ERR_TMF    67		"Too many files"
;     B$ERR_DNA    68		"Device unavailable"
;     B$ERR_CBO    69		"Communication-buffer overflow"
;     B$ERR_FWP    70		"Permission denied"
;     B$ERR_DNR    71		"Disk not ready"
;     B$ERR_DME    72		"Disk-media error"
;     B$ERR_AFE    73		"Advanced feature unavailable"
;     B$ERR_RAD    74		"Rename across disks"
;     B$ERR_ACD    75		"Path/File access error"
;     B$ERR_PNF    76		"Path not found"
;     B$ERR_UPE    255 	"Unprintable error"
;
;Entry:
; None. (Entry itself determines error)
;
;Exit:
; None. Transfer of control is either handled by environment specific ON ERROR
; handler, or we fatal error, again to an environment specific handler.
;
;******************************************************************************

RTEDEF	MACRO	code,label,number,text,noskip
	IFNB	<label>
label	EQU	number		;;Define message symbol
	ENDIF
	IFNB	<code>		;;if there is an entry code
	PUBLIC	B$ERR_&code	;;entry point
B$ERR_&code:
	IF	number LE 255	;
	MOV	BL,number	;;load up number
	IFB	<noskip>	;; If needed
	DB	0B8H		;;MOV AX, ... to skip next two instruc bytes
	ENDIF			;; IFB <noskip>
	ELSE			;; IF number LE 255
	MOV	BL,0		;; indicate fatal (skipped on fall in)
	MOV	CX,number	;; Now create number
	JMP	SHORT DUMMY_10	;; now jump around the rest
	ENDIF			;; IF number LE 255
	ENDIF
	ENDM


cProc	DUMMY,<NEAR>
cBegin

	INCLUDE messages.inc	;Get the messages, and generate the entries

DUMMY_10:			
	MOV	BH,0		; [BX] = error message number
	OR	BX,BX		; see if fatal error
	JNZ	DUMMY_20	; jump if not
	XCHG	BX,CX		; else get fatal error number in BX
DUMMY_20:			; [BX] = error number
	JMP	SHORT B$RUNERR	

cEnd	nogen			

;******************************************************************************
;
; Entry points which provide additional information for b$errinfo
;
;******************************************************************************

cProc	B$ERR_OM_NH,PUBLIC	; Out of memory in near heap
cBegin	nogen			
	MOV	AL,OMErr_NH	
	SKIP	2		
cEnd	nogen			

cProc	B$ERR_OM_FH,PUBLIC	; Out of memory in far heap
cBegin	nogen			
	MOV	AL,OMErr_FH	
cEnd	nogen			

	MOV	b$errinfo,AL	; save additional info
	MOV	BX,BE_MEMORY	; error code
	JMP	SHORT B$RUNERRINFO 



cProc	B$FrameFC,<PUBLIC,NEAR,FORCEFRAME> ; FORCEFRAME for error recovery
cBegin				
	JMP	B$ERR_FC	; illegal function call error
cEnd				

cProc	B$FrameAFE,<PUBLIC,NEAR,FORCEFRAME> ; FORCEFRAME for error recovery
cBegin				
	JMP	B$ERR_AFE	; advanced feature error
cEnd				

	SUBTTL	B$SERR - Declare runtime error
	PAGE
;***
; B$SERR - Declare runtime error
; void pascal B$SERR(U2 errnum)
;
;Purpose:
; Common Runtime Entry Point: ERROR statement
; Declare an error. Transfers control to the appropriate environment handler to
; process ON ERROR if appropriate.
;
; This routine DOES set up a frame.
;
;Entry:
; errnum	= BASIC error number
;
;Exit:
; None. Transfer of control is either handled by environment specific ON ERROR
; handler, or we fatal error, to the product specific handler.
;
;******************************************************************************
cProc	B$SERR,<PUBLIC,FAR>
	parmW	errnum		;single parameter, the error number.
cBegin

	MOV	BX,errnum	;Pick up the error code
	OR	BX,BX		; Check if BX = 0
	JZ	ILL_FC		;Brif so - Illegal function call error
	OR	BH,BH		; See if gt 255
	JZ	SERR_5		;Jump if okay
ILL_FC:				
	MOV	BX,BE_ILLFUN	; Else change to illegal function call
SERR_5: 			
	JMP	SHORT B$RUNERR ; Jump to runtime error handler

cEnd	nogen

	SUBTTL	8086 Error Interrupt Vectors
	PAGE
;***
; B$DIV0, B$OVFR - Divide by zero and Overflow interupt/exceptions
;
;Purpose:
; Hardware exception handlers for Divide by Zero and Overflow processor
; execptions.
;
;Entry:
; Return address and flags on the stack.
;
;Exit:
; Doesn't. Vectors to error handler.
;
;Uses:
;
;******************************************************************************
cProc	B$DIV0,<FAR,PUBLIC>
cBegin
	MOV	CL,BE_DIVIDE0	;[BL] = Basic error code
	JMP	SHORT EXCEPTION ;Go process
cEnd	nogen

cProc	B$OVFR,<FAR,PUBLIC>
cBegin
	MOV	CL,BE_OVERFLOW	;[BL] = Basic error code
	CMP	[b$cCSubs],0	;Executing within interpeted code?
	JZ	EXCEPTION	;Then the error code is correct
	CMP	BP,[b$curframe] ;Executing within compiled code?
	JZ	EXCEPTION	;Then the error code is correct
	MOV	CL,BE_ILLFUN	;Else error within runtime, Illegal Function

EXCEPTION:
	POP	AX		; [AX] = IP of exception
	POP	DX		; [DX] = CS of exception
	POPF			; Restore interrupt status

	PUSH	DX		; CS of exception
	PUSH	AX		; IP of exception

	CALL	B$GETDS		; [BX] = Data segment
	.erre	ID_SSEQDS	; Assert SS = DS
	MOV	ES,BX		; restore extra segment
	MOV	DS,BX		; restore data segment

	XOR	BH,BH		
	MOV	BL,CL		; [BX] = error number

cEnd	nogen			; fall into B$RUNERR

	SUBTTL	B$RUNERR - Runtime internal error handler
	PAGE
;***
; B$RUNERR - Runtime Internal Error Handler
;
;Purpose:
; Declare an error. Transfers control to the appropriate environment handler to
; process ON ERROR if appropriate.
;
;
; NOTE: FAR PROC's in Windows save an odd BP value in the frame.
;	The actual pointer is 1 less.  AND w/NOT 1 realigns if even OR odd.
;
;Entry:
; [BP]	= frame pointer. If equal to [b$curframe], the far address of the
;	  error is assumed to be on the top of the stack, and a new frame is
;	  created.
; [BX]	= BASIC or internal error number.
;
;Exit:
; None. Transfer of control is either handled by environment specific ON ERROR
; handler, or we jump to an environment specific fatal error handler, B$FERROR. 
;
;******************************************************************************
labelNP <PUBLIC,__BRUNERR>	; Quicklib compatibility label
cProc	B$RUNERR,<NEAR,PUBLIC>
cBegin
	MOV	b$errinfo,0	; Entry this way assumes no supplemental
labelNP <PUBLIC,B$RUNERRINFO>	




	MOV	CX,[b$errvec]	; see if error during input
	JCXZ	BRUNERR_5	; Jump if not
	JMP	CX		; [CX] = address to branch to, [BX] = err#
BRUNERR_5:			

	MOV	DX,BX		; [DX] = the error number

	MOV	CX,BP		; [CX] = current frame pointer
	CMP	CX,[b$curframe]; See if it is the top level frame
	JNZ	BRUNERR_20	; Jump if not, we already have a frame

BRUNERR_15:			
	PUSH	BP		; create the frame
	MOV	BP,SP		
	MOV	BX,BP		; [BX] = pointer to error frame
	JMP	SHORT BRUNERR_30

;
; Walk the frame chain to look for the top level frame.
;
BRUNERR_20:			
	MOV	BX,CX		; [BX] = current frame pointer
	JCXZ	BRUNERR_15	; Jump if end of chain found (make best ofit)
	MOV	CX,[BX] 	; [CX] = next frame in chain
	CMP	CX,[b$curframe]	; See if top level frame found
	JNZ	BRUNERR_20	; jump if not done with stack walk
;
; Terminate any I/O operation that was in progress, and determine whether this
; error can be trapped.
;
; [BX] = pointer to frame containing return address of basic-level error
; [DX] = error number
;
BRUNERR_30:			
	PUSH	BX		; Push frame parameter to B$FERROR
	MOV	[b$errnum],DX	; Save error number
	PUSH	BX		; save frame parameter around call

	MOV	SI,OFFSET DGROUP:b$err_disp	; get dispatch table addr
	CALL	FAR PTR B$COMP_DISP		; do table dispatches
	MOV	b$PRFG,0	; clear print using flag

	CALL	__fpreset	; Unconditionally reset Math Support

	POP	BX		; restore BX = frame parameter
	CMP	BYTE PTR [b$errnum+1], HIGH FE_NOTHINGBASE  ; = 98h?
	JE	RUNERR_40	; skip on error handling for 9800h range

	cCALL	B$ONERR		; do on error handling
	CMP	[b$cCSubs],0	;See if we were interpreting at the time
	JNZ	RUNERR_35	
	MOV	WORD PTR [b$errmod+2],0 ; 0 = error in interpreter
	JMP	SHORT RUNERR_40 
RUNERR_35:			
	POP	BX		; Restore frame parameter to BX
	PUSH	BX		; Push frame parameter to B$FERROR again
	PUSH	BX		; Push frame parameter to B$CONERR
	cCALL	B$CONERR	; set up b$errmod, b$errlin, etc.

;
; On error handler returned. The error could not be trapped. Handle fatal error
; as appropriate.
;
RUNERR_40:

	MOV	[b$inonerr],FALSE ;No longer in on error
	cCall	B$FERROR	;Process fatal error (param pushed way above)
DbHalt ER_TEXT,<B$FERROR returned>

cEnd	B$RUNERR

	SUBTTL	C Startup error handler
	PAGE


;*** 
; FatalError -- Print fatal error message to STDOUT and die. [40]
;
;Purpose:
;	Added with revision [29] to handle C startup errors.
;	Prints specified C startup message to STDOUT.
;	Jumps to the termination routine specified by __aexit_rtn.
;	Currently, the possible routines are:
;		_exit, __exit, and B$terminate (all in CRT0DAT.ASM)
;	If the routines are moved to another segment besides _TEXT,
;	__aexit_rtn will have to be made into a FAR pointer.
;
;	Also handles Non-C startup errors such as those produced in the
;	CHAIN function, when memory is so screwed up that normal recovery
;	is impossible.
;
;Entry:
;	DS = SS = DGROUP
;	BX = error number
;Exit:
;	If C startup message, returns via routine specified by __aexit_rtn.
;	Otherwise, returns directly to DOS without doing any C termination.
;	
;Uses:
;	All
;Exceptions:
;	None
;******************************************************************************
cProc	FatalError,<FAR>
cBegin

	PUSH	BX		; save error number

	MOV	AX,FE_STARTUPEND ; CR/LF,"runtime error"
	CALL	PUTNUM		; print the message
	
	POP	AX		; AX = error message number
	CALL	PUTNUM		; print the message

	MOV	AL,255		; return code = -1
	PUSH	AX		; put it on the stack for termination routine
				; (must be preserved, too).
	PUSH	AX		; space on stack as if we CALLED exit routine
	PUSH	AX		; 	FAR (to set up its frame correctly)

	MOV	BX,SEG _exit	; termination function segment
	PUSH	BX		;    (functions are FAR, but called NEAR)
	PUSH	__aexit_rtn	; termination function offset
	RET			; RETF will jump to error routine
cEnd	<nogen>


;*** 
; B$PUTNUM  -- print error message to STDOUT..
;
;Purpose:
;	Used to print messages using DOS when PRINT support may not be there.
;	Added with revision [29].
; 	Modified to be used with all fatal errors [31].
;
;	Gets the message text associated with an error message number, and
;	prints the text to STDOUT.
;
;Entry:
;	DS = SS = DGROUP
;	AX = error number
;Exit:
;	None
;Uses:
;	Per convention
;Preserves:
;	SI
;Exceptions:
;
;******************************************************************************

cProc	PUTNUM,<NEAR>,<SI>	
cBegin

	cCall	__FMSG_TEXT,AX	; returns DX:AX = message address
	mov	si,ax		; DX:SI = message address
PUTNUM_15:			
	push	ds		; save DS
	mov	ds,dx		; DS:SI = message address
	lodsb			; AL = next character to print
	pop 	ds		; restore DS
	or	al,al		; check for null
	jz	PUTNUM_20	; brif at end of string
	cCall	B$PUTCHR	; print the character
	jmp 	SHORT PUTNUM_15	; and go back for more...
PUTNUM_20:			
cEnd


;*** 
; B$PUTCHR -- print char to stdout
;
;Purpose:
;	print char to stdout
;Entry:
;	AL = char to print
;Exit:
;	None
;Uses:
;	AX
;Exceptions:
;	None
;******************************************************************************
cProc	B$PUTCHR,<PUBLIC,NEAR>,<DX> 
cBegin				
	mov	dx,ax		
	callos	DCIO		; display the character through DOS
cEnd				


;***
; B$PUTUI - Write an unsigned 16-bit integer to console
; Moved here with revision [62]
;
;Purpose:
; Write an unsigned 16-bit integer to console via DOS
;
;Entry:
; [AX]	= 16-bit unsigned integer to be printed
; [CX]	= Near offset of routine to call to print characters.
;
;Exit:
; None.
;
;Uses:
; Per convention.
;****
cProc	B$PUTUI <PUBLIC,NEAR>	
cBegin
	MOV	BX,10		; prepare to divide by 10
	XOR	DX,DX		; null marks end of string
PUTUI_5:
	PUSH	DX		; save digit on stack
	XOR	DX,DX
	DIV	BX		; using BX forces 16-bit quotient capability
	ADD	DL,'0'		; add 30H for ASCII '0'
	OR	AX,AX		; check if there are more digits to print
	JNZ	PUTUI_5		; brif
	MOV	AX,DX
PUTUI_10:			; print the string
	cCall	CX		; print a digit
	POP	AX		; get next digit
	OR	AX,AX		; more digits to print?
	JNZ	PUTUI_10	; brif
cEnd





sEnd	; ER_TEXT

sBegin	_TEXT			
assumes	CS,_TEXT		

;*** 
;__amsg_exit, B$amsg_exit -- print C fatal startup message and terminate
;
;Purpose:
;	Moved here with revision [48]
;
;Entry:
;	AX = C startup error message number
;
;Exit:
;
;Uses:
;
;Preserves:
;
;Exceptions:
;
;******************************************************************************

labelNP	<PUBLIC,__amsg_exit>		

cProc	amsg_exit,<NEAR>		
cBegin
 	XCHG	AX,BX			; BX = BASIC error number
 	MOV	BH,FE_STARTUPBASE SHR 8	; BH = 9Ah (C startup error)
 	PUSH	SS			; set DS = SS = DGROUP
 	POP	DS
 	JMP	FAR PTR FatalError	; print error message and terminate
cEnd	<nogen>

sEnd	; _TEXT

END
