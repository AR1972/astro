	TITLE	DBTRACE - 8086 Basic Compiler Debug and Trace Routines
;***
; DBTRACE - 8086 Basic Compiler Debug and Trace Routines
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Debug trace routines for use with the /D switch in the compiler.
;
; BASIC Syntax mapping to included runtime entry points:
;
; - Array References - Bounds check generated when /D (debug) in effect:
;
;    Example:
;
;      a$(4,5) = "foo"
;      -------
;	  |
;	$AEDA
;
; - TROFF Statement:
;
;	TROFF
;	  |
;      B$TROF
;
; - TRON Statement:
;
;	TRON
;	  |
;      B$TRON
;
;******************************************************************************
	INCLUDE switch.inc	;Rutime switch file
	INCLUDE rmacros.inc	;General runtime macros

	USESEG	INIT_CODE	;Initialization
	useSeg	ER_TEXT 	
	USESEG	DB_TEXT 	;User Debug component
	USESEG	DV_TEXT 	
	USESEG	_DATA		;Initialized Data
	USESEG	_BSS		;Uninitialized Data
	USESEG	<XIB>		; XIB and XIE must bracket XI!
	USESEG	XI		;initializer segment
	USESEG	<XIE>		

	INCLUDE seg.inc 	;Segment definitions

	INCLUDE compvect.inc	;component vectors
	INCLUDE messages.inc	;Text message number definitions

	INCLUDE dc.inc
	INCLUDE event.inc
	INCLUDE addr.inc	;module header definitions
	INCLUDE const.inc	; bit flag definitions

	INITIALIZER	B$xDBINI	;put B$xDBINI in initializer list

sBegin	_DATA			

	externW b$ini_disp	;One time initialization dispatch table

	externW	b$IPOLKEY	; vector for B$POLKEY for /O granularity.
	externW	b$pInitKeys1	; vector for B$InitKeys1 for /O granularity.
	externB	b$IOFLAG	; Misc. IO flags.
	externB	b$CtrlFlags	; contains trace on bit
	externB	b$EventFlags	; misc event flags

sEnd	_DATA			

sBegin	_BSS			

	externW	b$userflags	;user specified compile flags

sEnd	_BSS			

sBegin	DV_TEXT 		
	externNP B$NearRet	;for disp vectors in compvect.inc
sEnd	DV_TEXT 		


sBegin	DB_TEXT 		
	externNP B$$WCHT
	externNP B$RDKYBD
	externNP B$BREAK_CHK

sEnd	DB_TEXT 		

sBegin	ER_TEXT 		

	externNP B$ERR_INT	
	externNP B$PUTUI	; Type an unsigned integer to console.

sEnd	ER_TEXT 		



	SUBTTL	Runtime User Debugging Initialization
	PAGE
assumes CS,INIT_CODE		; from here to sEnd INIT_CODE
sBegin	INIT_CODE

;***
;B$xDBINI - User Debug initializer
;void pascal B$xDBINI()
;
;Purpose:
;	Initializer for User Debugging(/D) component.  This routine is called
;	by the Crt0 startup before _main is called.  It will update the
;	indirect dispatch tables for the user debug routines.  This
;	insures that the only time that user debug is accessed is when
;	this module is linked into the user program.
;
;Entry:
;	None.
;
;Exit:
;	b$ini_disp.DB_IVEC	- contains pointer to B$DBINI
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;****
cProc	B$xDBINI,<FAR>
cBegin
;
;	update "ONE" time initialization dispatch address to B$DBINI
;
	MOV	WORD PTR [b$ini_disp].DB_IVEC,DB_TEXTOFFSET B$DBINI
cEnd
sEnd	INIT_CODE		; from here to previous sBegin
	PAGE

sBegin	DB_TEXT 		

	assumes CS,DB_TEXT	

;***
; B$LINA - Print trace message if trace flag is set
;
; Purpose:
;	Runtime Entry Point, if /D in effect.
;	Called at the start of each statement when /D in effect, to print
;	'[n]' where n is the line number of the statement, iff the trace
;	flag is set. Also checks (first) for ^break.
; Input:
;	NONE
; Output:
;	NONE
; Modifies:
;	F
;****

cProc	B$LINA,<PUBLIC,FAR,FORCEFRAME> 
cBegin
	call	B$BREAK_CHK	; won't return if ^break occured
cEnd				


;***
; B$DBINI - start trapping for Printer Echo, Pause key, & ^Break.
;
; Purpose:
;	Runtime Entry Point.
; Input:
;	NONE
; Output:
;	NONE
; Modifies:
;	ALL
;****
cProc	B$DBINI,<NEAR,PUBLIC>,<SI>	
cBegin				

	mov	[b$IPOLKEY],offset $dpolkey ; may be changed by B$InitKeys1

	MOV	AL,ENABLE_TRAP	
	CALL	[b$pInitKeys1]	; initialize key trapping if present
	OR	b$CtrlFlags,DSwitch ; indicate /d present after call
NoInit: 			
cEnd				

Dpolkey proc	near
;***
; $DPOLKEY -  dummy polkey routine
;
; Purpose:
;	Same as real POLKEY except function keys are ignored. Used when /D
;	is set but not /V
;Entry:     none
;Exit:	    none
;Modifies:  none
;****

$Dpolkey:			;this routine was completely
	push	ax		;rewritten - mostly taken from
	push	bx		;the interpreter
Pollop:
	xor	ax,ax		;Read trapped keys function code
	call	B$RDKYBD 	;OEM dependent key trap routine
	dec	bx
	jz	errint		;Fn, cursor, or user key -- error
	inc	bx
	jz	polkyx		;No key was trapped
	inc	bx
	jz	wasctc		;^C / <break> function
	inc	bx
	jz	wascts		;^S / <pause> function
	inc	bx
	jnz	pollop		;Not <print-screen> func. - ignore, poll
				;next

Wasctp: 			;<printer echo> function
	XOR	b$IOFLAG,LPR_ECHO ; Toggle the printer echo flag
	jmp	short pollop	;Key found, so there may be more

Wascts: 			;^S / <pause> function
	XOR	b$EventFlags,PAUSEF ; Toggle the pause flag
	jmp	short pollop	;Key found, so there may be more

Wasctc: 			;flag ^Break found
	OR	b$EventFlags,CNTLC	; ^Break is reported last so
	AND	b$EventFlags,NOT PAUSEF	; turn off pause and exit

Polkyx:
	TEST	b$EventFlags,PAUSEF ; Test for pause processing
	jnz	pollop		;Pause in process, wait for "unpause"
poldon:
	pop	bx
	pop	ax
	ret

errint: jmp	B$ERR_INT	

dpolkey endp


;***
; B$TRON - Set trace flag
;
; Purpose:
;	Runtime Entry Point, if /D in effect.
;	Set trace flag.
; Input:
;	NONE
; Output:
;	bit traceon of b$CtrlFlags is set
; Modifies:
;	NONE
;****
cProc	B$TRON,<PUBLIC,FAR>	
cBegin				
cEnd

;***
; B$TROF - Clear trace flag
;
; Purpose:
;	Runtime Entry Point, if /D in effect.
;	Clear trace flag.
; Input:
;	NONE
; Output:
;	bit traceon of b$CtrlFlags is cleared
; Modifies:
;	NONE
;****
cProc	B$TROF,<PUBLIC,FAR>	
cBegin				
cEnd				

;
;Routine recreated with [27] for backward compatibility with old QLIBS.
;
;***
;B$TYPUI - Write an unsigned 16-bit integer to console via print code
;
;Purpose:
;	Write AX as a 16-bit unsigned integer for linenumbers. Note that
;	this routine can not be put with B$PUTUI as this routine will
;	always pull in PRINT code because of B$$WCHT.
;
;Entry:
;	[AX] = integer to be printed
;
;Exit:
;	None.
;
;Uses:
;	Per Convention
;
;Exceptions:
;	None.
;****
	PUBLIC	B$TYPUI				
cProc	B$TYPUI,<NEAR>				
cBegin
	MOV	CX,DB_TEXTOFFSET B$$WCHT	;Use Print routines
	CALL	B$PUTUI				;And display the number
cEnd

sEnd	DB_TEXT 		

	END
