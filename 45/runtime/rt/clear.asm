	TITLE	CLEAR - CLEAR for BASCOM-86
;***
; CLEAR - CLEAR for BASCOM-86
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - CLEAR Statement
;
;      CLEAR [,[n][,m]]
;	     |
;	   B$SCLR
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	useSeg	RT_TEXT 	
	useSeg	_BSS		
	useSeg	_DATA		

	INCLUDE seg.inc 	
	INCLUDE stack2.inc	
	INCLUDE idmac.inc	
	INCLUDE const.inc	

sBegin	_BSS			

externW b$clrt_disp		; CLEAR "termination" dispatch table
externW b$clr_disp		; CLEAR statement dispatch table
externB b$inonerr		; flag indicating on-error in progress
externW b$mainframe		; main level frame
externW b$NH_first		
externW b$NH_last		
externW b$pend 			; End of user data
externW __atopsp		; top word allocated to STACK.
externW b$curlevel		;current program level
externB b$CtrlFlags		;BASIC generic flags
externB b$Clearing		; flag to indicate processing CLEAR stmt

staticW saveSI,,1		; preserved SI

sEnd	_BSS			

sBegin	RT_TEXT

externFP B$COMP_DISP		; Dispatch table utility
externNP B$ERR_OM_NH		
externNP B$ERR_OSS		
externNP B$ERR_FC		

externNP B$NHMOVALL		; juggle all heap space

assumes CS,RT_TEXT		
	SUBTTL	B$StackReset - Stack reset support for QBI
	PAGE
;*** 
; B$StackReset - Reset Stack to initial location (EI_QB only)
; void pascal B$StackReset(void)
;
;Purpose:
; Special entry point for QBI to set stack back to initial location
; before a RUN/RESTART.  Blasts stack back to main level, without
; doing all the other CLEAR related garbage.  This is necessary to
; ensure that .qlb XI initializers which nmalloc won't lose their
; allocated memory.
; Added with revision [18]
;
;Entry:
; none
;
;Exit:
; none
;
;Uses:
; per convention
;
;******************************************************************************

cProc	B$StackReset,<FAR,PUBLIC>
parmW	stack
parmW	fStack
cBegin
	MOV	AX,STACK_SIZE	;default size
.erre	STACK_SIZE AND 0FF00H
	OR	b$CtrlFlags,NoInitBcVars ;We have already inited BcVars,
	JMP	SHORT CLR_RUN
cEnd	<nogen>

	SUBTTL	B$RUNL - RUN Linenumber support for compiled code
	PAGE
;*** 
; B$RUNL - RUN Linenumber support for compiled code
; void pascal B$RUNL (U2 stack, U2 fStack)
;
;Purpose:
; Special entry point for compiler RUN [linenumber]. Performs all functions
; of B$SCLR(0), and blasts stack back to the main level.
; Interface added revision [13]
;
;Entry:
; none
;
;Exit:
; none
;
;Uses:
; per convention
;
;******************************************************************************
cProc	B$RUNL,<FAR,PUBLIC>
parmW	stack
parmW	fStack
cBegin
	MOV	AX,STACK_SIZE	;default size
	JMP	SHORT CLR_RUN
cEnd	<nogen> 		

ERCOSS:
DbAssertTst [b$CtrlFlags],Z,NoInitBcVars,RT_TEXT,<Unexpected OSS error in B$SCLR> 
	JMP	B$ERR_OSS	; memory allocation error

ERCFC:
DbAssertTst [b$CtrlFlags],Z,NoInitBcVars,RT_TEXT,<Unexpected IFC error in B$SCLR> 
	JMP	B$ERR_FC	; function error

;*** 
; B$SCLR - CLEAR statement
; void pascal B$SCLR (U2 stack, U2 fStack)
;
;Purpose:
; Clear variables and file system.  Closes all files, clears all variables
; (including COMMON and LNA), resets stack and optionally changes its size,
; initializes the string and local heaps, flushes the play queues and sets
; music to foreground, and sets $SEG to DGROUP.
;
; The compiler should ensure that CLEAR does not appear inside multiline
; functions. The runtime needs to ensure that CLEAR only occurs in the main
; program. This is done by testing whether the initial frame pointer is active.
;
; Interface added revision [9]
;
;Entry:
; stack = user requested stack size delta
; fStack = Flag indicating stack parameter is valid. If false, default is
;	   assumed.
;
;Exit:
; none
;
;Uses:
; per convention
;
;******************************************************************************
cProc	B$SCLR,<FAR,PUBLIC>	
parmW	stack			
parmW	fStack			
cBegin				
	MOV	AX,STACK_SIZE	; default size (in case fStack false)
	MOV	CX,fStack	
	JCXZ	CLEARIT 	; jump if default to be used
	MOV	AX,stack	; else get specified size
	ADD	AX,STACK_MIN	; Add base stack size
	JB	ERCOSS		; if too big, then error
;
;	Test if in main module, if not, then CLEAR is illegal.
;
CLEARIT:
	MOV	BX,[BP] 	; Get current user frame
	TEST	[b$inonerr],0ffH ; in an on error handler?
	JZ	CLEAR_10	; then BX is correct
	MOV	BX,[BX] 	; else BX is one removed
CLEAR_10:			
	CMP	BX,[b$mainframe] ;test if in main module
	JNE	ERCFC		;if not, then error
;
;	Compute new stack pointer by adding stack size to end of user
;	variables-2 (-2 so that there is no possiblity of segment wrap-around)
;	This calculation is the same as that in the C startup.
;
CLR_RUN:			; Entry point for RUN from above
	SUB	AX,2		; subtract 2 for __asizds limit
				; (won't cause carry)
	ADD	AX,b$pend	; add end of variables
	JC	ERCOSS		; if carry, then out of memory
	AND	AL,0FEh		; make even if not already

;
;	Execute clear-time termination routines for various runtime components
;

	INC	b$Clearing	; indicate processing CLEAR statement
	MOV	saveSI,si	;preserve SI
	PUSH	AX		; Save Me!
	MOV	SI,OFFSET DGROUP:b$clrt_disp	; get dispatch table addr
	CALL	FAR PTR B$COMP_DISP		; do table dispatches
	POP	AX		; [AX] = new atopsp
;
;	Initialize the new stack including the frame.
;
	SUB	AX,[__atopsp]	; [AX] = delta, Carry = moving down

	PUSHF			; save flags for later
	CMP	AX,8		; >= 8 bytes?
	JGE	AdjustStack	; brif so -- adjust stack
	CMP	AX,-8		; <= -8 bytes?
	JLE	AdjustStack	; brif so -- adjust stack
				; Don't move heaps since the algorithm
				; presently used by B$NHMOV will hose
				; things if the adjustment is -8 < x < 8
	POPF			; discard flags
	XOR	AX,AX		; pretend no adjustment specified
				; (clears carry so heaps won't be moved)
	PUSHF			; save the flags (reset SP & return)

AdjustStack:			
	POPF			; restore flags
	JBE	CLEARIT_DOWN	; brif no change or moving down
	CALL	B$NHMOVALL	; Move heaps up BY AX BEFORE setting stack
	JC	ERCOM		; Jump if no memory

CLEARIT_DOWN:			
	POP	DX		; Discard previous frame pointer
	POP	BX		; [BX] = return address offset
	POP	DX		; [DX] = return address segment

	ADD	[__atopsp],AX	; adjust top of stack pointer BY AX
	MOV	SP,[__atopsp]	; set the new stack
	PUSH	DX		; replace return address
	PUSH	BX		
	JNC	CLEARIT_UP	; Jump if moving up

	CALL	B$NHMOVALL	; Move heaps down BY AX AFTER setting stack
	JC	ERCOM		; Jump if no memory


CLEARIT_UP:			

;
;	Execute clear-time re-initialization routines for runtime components
;
	MOV	SI,OFFSET DGROUP:b$clr_disp	; get dispatch table addr
	CALL	FAR PTR B$COMP_DISP		; do table dispatches
	MOV	SI,saveSI	;recover SI

	AND	[b$CtrlFlags],NOT NoInitBcVars ;reset static flag
	MOV	b$Clearing,0	; indicate done processing CLEAR statement
	RET			;return to interp
cEnd	nogen			

ERCOM:
	AND	[b$CtrlFlags],NOT NoInitBcVars ;reset static flag
	JMP	B$ERR_OM_NH	; memory allocation error


sEnd	RT_TEXT

	END
