	TITLE	ERROR - Error trapping for 8086 Basic Compiler
        page ,132
;***
; ERROR - Error trapping for 8086 Basic Compiler
;
;	Copyright <C> 1986, 1987, Microsoft Corporation
;
;Purpose:
; This module contains the run-time support for error trapping in addition to
; the standard routines.
;
; BASIC Syntax mapping to included runtime entry points:
;
; - ERR Variable:
;
;      v = ERR
;	    |
;	  B$FERR
;
;
; - ERL Variable:
;
;      v = ERL
;	    |
;	  B$FERL
;
;
; - ON ERROR Statement:
;
;      ON ERROR GOTO line
;      ------------------
;	       |
;	     B$OEGA
;
;
; - RESUME Statement
;
;      RESUME [0 | NEXT | line]
;
;    Examples:
;
;      RESUME	       RESUME 0 	 RESUME NEXT	      RESUME 10
;	  |	       -------- 	 -----------	      ---------
;	  |		   |		      | 		   |
;      B$RES0	       B$RES0		   B$RESN		B$RESA
;
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
	INCLUDE idmac.inc	
	INCLUDE messages.inc	;error message label definitions
	INCLUDE stack.inc	

sBegin	_BSS			

externD b$erradr		; Far ptr to basic error address
externW b$errnum		; Error number encountered
externW b$errlin		; Basic line number of error
externB b$inonerr		; flag indicating we're in an on error
externW b$curlevel		;current program level
externW b$curframe		; pointer to current BASIC frame
externW	b$cCSubs		; <> 0 means compiled code

sEnd	_BSS			

externFP B$EXSA		; clear frame state info

externFP B$SERR		
externFP B$CEND		

sBegin	NH_TEXT 		
externNP B$STDALCALLTMP 	
sEnd	NH_TEXT 		

sBegin	ER_TEXT 		

externNP B$CALCLINE		;Get line number & start address of stmnt
externNP B$GETMODCODE		;Get module specific code data.
externNP B$RES_SETUP		;Clear flags for RESUME

assumes CS,ER_TEXT		

	SUBTTL	B$OEGA - ON ERROR GOTO statement handler
	PAGE
;***
; B$OEGA - ON ERROR GOTO statement handler
;
;Purpose:
; sets the address to which an error is to branch. If zero, and we are in an
; on error handler, then declare a fatal error.
;
;Entry:
; erradr = statement address or 0
;
;Exit:
; Sets error vector with line number address
;
;Exceptions:
; erradr == 0 and an error is in progress, will jump to B$SERR instead of
; returning.
;
;******************************************************************************
cProc	B$OEGA,<FAR,PUBLIC>	
parmD	erradr			
cBegin				

	MOV	AL,OF_DAT	;Get location of module specific data
	PUSH	SI		
	CALL	B$GETMODCODE	;From the module specific code segment
	XCHG	AX,SI		;Into SI

	MOV	CX,off_erradr	; [CX] = offset of on error target
	INC	CX		
	JCXZ	OEGA_3		; disable error handling if CX = -1 or 0
	DEC	CX		
OEGA_3:				
	MOV	[SI].OFD_ONERROR,CX ; Set error vector
	POP	SI
	JCXZ	OEGA_10 	; jump if not setting a valid address
OEGA_5: 			

cEnd				

;
;	In error handling routine, must pretend we just had last error
;
OEGA_10:			
	CMP	[b$inonerr],CL ; Test if error in progress
	JE	OEGA_5		; No - just return
	MOV	BP,[BP] 	;restore to user program frame
	ADD	SP,8		;toss return address and param
	PUSH	[b$errnum]	;parameter is the error number
	PUSH	[WORD PTR b$erradr +2] ;"Return Address" use error segment
	PUSH	[WORD PTR b$erradr]	;"Return Address" Use error offset
	JMP	B$SERR 	;Process error (Never Returns)

	SUBTTL	B$RESA - RESume At line number
	PAGE
;***
; B$RESA - RESume At line number
;
;Purpose:
; Runtime entry for the RESUME <linenumber> statement
;
;Entry:
; [AX] = Address to RESUME to.
;
;Exit:
; to address. Clears on-error-in-progress
;
;Exceptions:
; Aborts if no error in progress
;
;******************************************************************************
cProc	B$RESA,<FAR,PUBLIC,FORCEFRAME> 
cBegin				

; Warning!  This assumes B$RES_SETUP never returns with AX trashed
	CALL	B$RES_SETUP	;perform initial RESUME stuff

	PUSH	AX		; save address to resume to
	MOV	BX,[b$curframe]	; current BASIC frame
	MOV	BX,[BX]		; want previous BASIC frame
	CALL	B$CEvtHndlr	; Was it an event handler?
	OR	AX,AX		; AX = 0 if an event handler
	POP 	BX		; [BX] = address to resume to
	JZ	RESRET		; treat event handlers specially

	XOR	AX,AX		; set to zero for string dealloc
	CALL	B$STDALCALLTMP	;dealloc all temp strings
	CMP	[b$curlevel],1	; check for main module
	JE	RESRET		; At main level - use old frame
	MOV	[b$curlevel],AX	; set [b$curlevel] to main level (AX=0)
	DEC	[b$cCSubs]	; INCed in B$ENSA
	MOV	[BP].FR_RETOFF,BX ; replace return offset with resume addr.
cEnd				; RETF to resume address

	SUBTTL	B$RES0 - RESUME [0]
	PAGE
;***
; B$RES0 - RESUME [0]
; void pascal B$RES0()
;
;Purpose:
; Runtime Entry Point for RESUME [0]
;
;Input:
; NONE
;
;Output:
; Clears error flags and error number
;
;******************************************************************************
cProc	B$RES0,<FAR,PUBLIC,FORCEFRAME> 
cBegin				

	CALL	B$RES_SETUP	; clear error flags and error number

	MOV	AX,[WORD PTR b$erradr]  ;Get error address
	MOV	BX,[BP] 	;Get User program frame pointer
	cCall	B$CALCLINE	; [BX] = address of start of statement
DbAssertFlags  NZ,ER_TEXT,<Statement start not found in B$RES0>       

RESRET: 			; moved here from B$RESA
	POP	BP		; Restore saved BP
	POP	AX		; Throw away return offset
	XCHG	AX,BX		; [AX] = return offset
	POP	DX		; [DX:AX] = resume address
	CALL	B$EXSA		; Preserves DX:AX
	PUSH	DX		
	PUSH	AX		; Save line number as offset
	RET			; Return

cEnd	nogen			

	SUBTTL	B$RESN - RESUME NEXT
	PAGE
;***
; B$RESN - RESUME NEXT
; void pascal B$RESN
;
; Purpose:
;	Runtime Entry Point - RESUME NEXT
;
; Input:
;	NONE
;
; Output:
;	NONE
;
;******************************************************************************
cProc	B$RESN,<FAR,PUBLIC,FORCEFRAME>,SI 
cBegin				

	CALL	B$RES_SETUP	; clear error flags and error number

	PUSH	DS		;Save DS
	MOV	AL,OF_STA	
	CALL	B$GETMODCODE	; Get statement number address table
	XCHG	AX,SI
	MOV	CX,[WORD PTR b$erradr]	; Get return offset
	MOV	DS,BX		; Get module CS
	MOV	BX,-1		;Start out with maximum address

rsnlop: LODSW			;(AX) = next address to check
	OR	AX,AX		;Check if end of table
	JZ	rsnend		;  Yes

	CMP	AX,CX		;Are we less than original address
	JB	rsnnxt		;  Yes - skip this one
	CMP	AX,BX		;Are we less than current closest
	JAE	rsnnxt		;  No - skip this one

	XCHG	AX,BX		;Save address in (BX) as new closest

rsnnxt: INC	SI		;Skip line number entry
	INC	SI
	JMP	rsnlop		;Keep looping

rsnend:
	POP	DS		;restore DGROUP to DS
	INC	BX		;Bump by 1 to map -1 to 0 (maybe)
	JNZ	rsnret		;  Not 0 - return to user program
	JMP	B$CEND 	; Resume off the bottom is an END

rsnret:
	DEC	BX		;Restore to correct values
	POP	SI		;Restore SI
	JMP	RESRET		; go return

cEnd	nogen			

	SUBTTL	ERR and ERL funcitons
	PAGE
;***
; B$FERR - ERR function
; I2 pascal B$FERR()
;
;Purpose:
; Runtime Entry Point - ERR function
;
;Input:
; NONE
;
;Output:
; [AX] = error number
;
;******************************************************************************
cProc	B$FERR,<FAR,PUBLIC>	
cBegin				
	MOV	AX,[b$errnum]	;Get error number
cEnd				

;***
; B$FERL - ERL function
; I4 pascal B$FERL()
;
;Purpose:
; Runtime Entry Point - ERL function
;
;Input:
; NONE
;
;Output:
; [DX:AX] = error line number
;
;******************************************************************************
cProc	B$FERL,<FAR,PUBLIC>	
cBegin				
	MOV	AX,[b$errlin]	
	XOR	DX,DX		
cEnd				

;*** 
;B$CEvtHndlr - Checks if a frame is an event handler
;
;Purpose:
;	Determine whether the frame is for an active compiled event handler.
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
cProc	B$CEvtHndlr,<PUBLIC,NEAR> 
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
