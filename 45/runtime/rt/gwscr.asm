      TITLE   GWSCR - GENERAL support for LOCATE, SCREEN stmt & fct,COLOR, CLS
;***
; GWSCR - GENERAL support for LOCATE, SCREEN stmt & fct,COLOR, CLS
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; - CLS Statement:
;
;      CLS [0,1,2]
;	|
;     B$SCLS
;
;
; - COLOR Statement:
;
;    Text:
;      COLOR [foreground] [,[background] [,border]]
;
;    Graphics:
;      COLOR [background] [,[palette]]
;
;      COLOR 1,2,3
;	|
;     B$COLR
;
;
; - CSRLIN Function:
;
;      v = CSRLIN
;	      |
;	    B$CSRL
;
;
; - KEY Statement - calls B$KFUN or B$KMAP depending on syntax:
;
;      KEY (ON | OFF | LIST | (n,x$))
;
;    The first 3 possibilities cause $KY0 to be called, $KY1 for the 4rth:
;
;      KEY (ON | OFF | LIST)		 KEY n,x$
;      ---------------------		 --------
;		 |			     |
;	       B$KFUN			   B$KMAP
;
;
; - LOCATE Statement:
;
;      LOCATE [row][,[col][,[cursor][,[start][,stop]]]]
;
;      LOCATE 1,1
;	|
;     B$LOCT
;
;
; - POS Function:
;
;      POS(I)
;	|
;   B$FPOS
;
;
; - SCREEN Function:
;
;      v = SCREEN(row, col [,z])
;	      |
;	    B$FSCN
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	;Runtime Macro Defintions

	USESEG	_DATA		
	USESEG	_BSS		
	USESEG	CN_TEXT 	
	USESEG	RT_TEXT 	

	INCLUDE seg.inc 	
	INCLUDE ascii.inc	;must follow switch.inc
	INCLUDE idmac.inc	

sBegin	_DATA			

	externW	b$CURSOR	; (1,1)-relative screen cursor
	externB	b$CSRY		; 1-relative y-coordinate cursor
	externB	b$CSRX		; 1-relative x-coordinate cursor
	externB b$LINCNT 	
	externB b$WDOBOT 	
	externB b$WDOTOP 	
	externB b$CRTWIDTH 	

	externW b$VWCLR_PTR	

sEnd	_DATA			

sBegin	_BSS			

	externB b$RcoFlg	;NZ if Ronco keyboard present

	externB b$KEY_SW 	;defined in GWDATA.ASM
	externW b$STRTAB 	;defined in GWDATA.ASM

	externB B$VIEWSW 	; defined in GWDATA.ASM

sEnd	_BSS			


assumes CS,CN_TEXT		
sBegin	CN_TEXT 		

	externNP B$SETCLR	
	externNP B$GetParm	
	externNP B$KEYDSP	
	externNP B$WHOME 	
	externNP B$SCNLOC	
	externNP B$ERR_FC	
	externNP B$CSRATR	
	externNP B$CLRSCN	
	externNP B$GRMODE	
	externNP B$$WCHT 	
	externNP B$SCREEN	; new name for $SCRATR & $SCRINP
	externNP B$SCINIT	; init screen if not already done
	externNP B$CRLF		; perform CR/LF processing


	SUBTTL	COLOR statement
	PAGE
;***
;B$COLR - execute COLOR  Statement
;void pascal B$COLR([I2 flag,<I2 parm>]...,cwParams)
;
;Purpose:
;	Pass parameters on to low level COLOR statement routine (SetColor).
;	There is one flag per parameter.  If flag is zero, then the parameter
;	was defaulted.	If flag is non-zero, the next word in parameter
;	block contains user specifed value.  No dummy values are specified.
;Entry:
;	parameter block is on stack.  First word is count of params, excluding
;	the count parameter.
;Exit:
;	None.
;Uses:
;	Per convention
;Exceptions:
;	Could result in a jump to B$ERR_FC if parameter is too large
;	or if some error occurs in SetColor
;****
cProc	B$COLR,<PUBLIC,FAR>	
cBegin	<nogen> 		
	cCall	B$ScSetup	;set up frame, parms
	CALL	B$SETCLR	;call low level COLOR statement support
	JB	FUNC_ERROR	;brif error occurred
cEnd	<nogen> 		;fall into routine to clean up and return

	SUBTTL	COLOR/SCREEN/LOCATE statement support
	PAGE

;***
;CleanUpParms - clean a variable number of parameters off of the stack and ret
;
;Purpose:
;	This routine cleans a variable length parameter block off of the
;	stack and does a far ret back to the caller.
;Entry:
;	Stack contains -
;
;		|  first parm	|
;		+---------------+
;		|		|
;		\ 		/
;		|		|
;		+---------------+
;		|  last  parm	|
;		+---------------+
;		|  cw parms	|
;		+---------------+
;		|  return seg	|
;		+---------------+
;		|  return off	|
;		+---------------+
;		|  old BP	|
;		+---------------+
;	 SP ->	|  saved SI	|
;		+---------------+
;Exit:
;	Stack cleaned
;Uses:
;	Per convention
;Exceptions:
;	None.
;NOTE:
;	This routine should be jumped into...
;****
cProc	B$ScCleanUpParms,<FAR,PUBLIC>  
cBegin				
	POP	SI		
	POP	ES		
	POP	BP		;recover BP
	POP	AX		;get return offset
	POP	DX		;get return segment
	POP	CX		;get count of parms
	SHL	CX,1		;convert word count to byte count
	ADD	SP,CX		;clean up parms
	PUSH	DX		;replace segment
	PUSH	AX		;replace offset
cEnd				
	PAGE

;***
;B$ScSetup - Preamble for LOCATE, COLOR, and SCREEN statements
;
;Purpose:
;	Does initial setup of parameters for LOCATE, COLOR,
;	and SCREEN statements.
;Entry:
;	BX - CS relative address to exit to.
;	parameter block is on stack.  First word is count of params, excluding
;	the count parameter.
;	Stack contains -
;
;		|  first parm	|
;		+---------------+
;		|		|
;		\ 		/
;		|		|
;		+---------------+
;		|  last  parm	|
;		+---------------+
;		|  cw parms	|
;		+---------------+
;		|  return seg	|
;		+---------------+
;		|  return off	|
;		+---------------+
;		|  near ret	|
;		+---------------+
;Exit:
;	initial SI is pushed on stack
;	CX - count of words in parameter block
;	SI - ptr to start of parameter block (grows down in memory).
;Uses:
;	Per convention
;Exceptions:
;	None.
;****
cProc	B$ScSetup,<NEAR,PUBLIC> 
parmW	parmBlock		;beginning of parameter block
parmW	cwParms 		;word count of parameter block
parmW	retAddr 		;return address of compiled/interp code
cBegin	<nogen> 		
	POP	BX		;get near return addr
	PUSH	BP		;set up ...
	MOV	BP,SP		;... stack frame
	PUSH	ES		;save ES
	PUSH	SI		;preserve SI
	MOV	CX,cwParms	;get param count
	MOV	AX,CX		;convert cwParms to cbParms
	SHL	AX,1		
	LEA	SI,cwParms	;get pointer to end of block
	ADD	SI,AX		;point to start of block
	JMP	BX		;return to caller
cEnd	<nogen> 		

FUNC_ERROR:
	JMP	B$ERR_FC	;Bad Parm, complain.

	SUBTTL	LOCATE statement
	PAGE

;***
;B$LOCT - execute the LOCATE Statement
;
;Purpose:
;   LOCATE -	Moves the Cursor to the Specified position
;		on the Active Screen.  Optionally:
;		1).  Turns the Cursor on/off.
;		2).  Allows setting of the Cursor Start/Stop
;		     Raster Lines.
;
;   Syntax: LOCATE [row] [, [col] [,[cursor] [, [start] [, [stop] ]]
;
;	WHERE:	row is Screen line number 1 to B$LINCNT-1 if KEY ON.
;		row is Screen line number 1 to B$LINCNT	 if KEY OFF.
;		col is Screen column number.  1 to 40 or 1 to 80
;		depending on WIDTH.
;
;		If Row and/or Column are missing, the current
;		value(s) are used.
;
;		cursor is a boolean value indicating whether
;		the cursor is visible or not.
;		0 for off,  non-zero for on.
;
;		start is the Cursor Start Raster Line (0-31).
;		stop  is the Cursor End   Raster Line (0-31).
;		If omitted, stop assumes start value.
;
;		The Cursor Raster Start/Stop parms are optional.
;		Cursor Blink is not selectable and always set
;		at 1/16 Rate...
;
;Entry:
;	parameter block is on stack.  First word is count of params, excluding
;	the count parameter.
;Exit:
;	None.
;Uses:
;	None.
;Exceptions:
;	Control may be transfered to B$ERR_FC
;****
cProc	B$LOCT,<PUBLIC,FAR>	
cBegin	<nogen> 		
	cCall	B$ScSetup	;set up frame, parms
	cCall	GetCsrParm	;not psw.z if got parm in [AL],flg in [AH]
	JNZ	LOCT20		;brif got parm
	MOV	AL,b$CSRY	;get current y coord
LOCT20: 			
	PUSH	AX		;save flag/row
	cCall	GetCsrParm	;get column if specified
	JNZ	LOCT30		;brif got column
	MOV	AL,b$CSRX	;get current column
LOCT30: 			
	PUSH	AX		;save flag/column
	cCall	GetCsrParm	;get cursor parm if specified
	XCHG	AX,DX		;[DX]=cursor flag/type
	cCall	GetCsrParm	;get start raster if specified
	XCHG	AX,BX		;[BX]=cursor flag/start raster
	cCall	GetCsrParm	;get stop raster if specified
	XCHG	AX,CX		;[CX]=cursor flag/stop raster
	XCHG	AX,DX		;[AX]=cursor type for ll call
	cCall	B$CSRATR 	;let low level do it's thing with cursor
	JB	FUNC_ERROR	;brif ll error
	POP	CX		;[CX]=flag/column
	POP	DX		;[DX]=flag/row
	OR	DH,DH		;was row specified?
	JZ	ChkCol		;brif not - go check column

;  here if the row parameter was specified

	OR	DL,DL		;check if new row zero
	JZ	FUNC_ERROR	;if so, then error
	MOV	AL,DL
	CMP	AL,b$WDOBOT	;don't allow locate to outside text window
	JBE	StoreRow	; if row < wdobot check against top
	CMP	b$KEY_SW,0	; function keys on ?
	JNE	FUNC_ERROR	; Brif so
	CMP	AL,B$LINCNT	; is row same as B$LINCNT ?
	JNE	FUNC_ERROR	; Brif not (This is the only time
				; a locate outside textt window
				; is allowed

StoreRow:
	CMP	AL,b$WDOTOP	; don't allow locate above text window
	JB	FUNC_ERROR	; brif above text window

ChkCol:
	DEC	CH		;was new column specified?
	JNZ	SetPos		;brif not - go set cursor position

;  Here if new column was specified...

	JCXZ	FUNC_ERROR	;Must be 1 to Width (40 or 80).
	CMP	CL,b$CRTWIDTH	; Test against physical screen width
	JA	FUNC_ERROR	;Error if Col is GT than Width.

SetPos:
	CALL	B$SCINIT	; Init screen if not already done.  Makes
				; sure that if this is the first screen
				; operation, that the next locate operation
				; can go to any line it wants to.
	MOV	DH,CL		;DH=col,DL=row
	CALL	B$SCNLOC		; Store DX into b$CURSOR, and display
				; user cursor at new position
	JMP	B$ScCleanUpParms;clean up stack and exit...
cEnd	<nogen> 		


	PAGE
;***
;GetCsrParm - Get a locate statement parameter if specified by user
;
;Purpose:
;	Gets a parameter for locate statement if one was specified
;	by the user.
;Entry:
;	SI - points to next parameter flag in parameter block
;	CX - count of words left in parameter block
;Exit:
;	SI,CX - advanced over parmeter flag and parameter
;	AH - 0 if no parameter specified, else 1
;	AL - parameter specified by user iff AH <> 0
;	psw.z - set if no parameter was specified
;Uses:
;	None.
;Exceptions:
;	None.
;****
cProc	GetCsrParm,<NEAR>,<DX>	
cBegin				
	MOV	DH,1		;default is parm specified
	cCall	B$GetParm	;call low level routine to get parm
	JNZ	GCP10		;brif got parm in [AL]
	DEC	DH		;no parm was specified (sets psw.z)
GCP10:				
	MOV	AH,DH		;[AH]=flag, [AL]=parm iff AH<>0
cEnd

	SUBTTL	CLS statement
	PAGE
;***
; B$SCLS - clear text/graphics/both screens.  Re-written as revision [15].
;
; Purpose:
;	This statement may have an optional byte parameter.  The value
;	of this parameter indicates what area of the screen is to be cleared.
;
;       CLS (no param)
;		if text mode:
;			clear text window (VIEW PRINT)
;			refresh function keys (if on, else blank the line)
;			home the cursor in text window
;		if graphics and viewport (VIEW):
;			clear defined graphics viewport
;			initialize graphics viewport variables
;		if graphics, but no viewport (no VIEW):
;			clear all text and graphics
;			initialize graphics viewport variables
;			refresh function keys (if on, else blank the line)
;			home the cursor in text window
;	CLS 0
;		clear all text and graphics
;		initialize graphics viewport variables
;		refresh function keys (if on, else blank the line)
;		home the cursor in text window
;	CLS 1
;		if text mode:
;			no-operation (return immediately)
;		if graphics and viewport (VIEW):
;			clear defined graphics viewport
;			initialize graphics viewport variables
;		if graphics, but no viewport (no VIEW):
;			clear all text and graphics
;			initialize graphics viewport variables
;			refresh function keys (if on, else blank the line)
;			home the cursor in text window
;	CLS 2
;		clear text window (VIEW PRINT)
;		home the cursor in text window
;			
; Entry:
;	ScnNum - screen to clear
; Exit:
;	None.
; Uses:
;	Per convention.
; Exceptions:
;	B$ERR_FC
;****
cProc	B$SCLS,<PUBLIC,FAR>
	parmW	ScnNum		;screen number to clear
cBegin

	MOV	BX,ScnNum	;keep screen number in BX
	INC	BX		;test if parameter was given
	JNZ	ClsParamGiven	;if given, then jump

	MOV	AX,0602H	;assume CLS 2 with key and text refresh
	CALL	B$GRMODE		;test if graphics mode
	JZ	ClsStart	;if not, use assumed value
	MOV	AX,0101H	;assume CLS 1 with only graphic vp init
	CMP	B$VIEWSW,0	;test if viewport active
	JNZ	ClsStart	;if so, use assumed value
	MOV	AX,0700H	;else CLS 0 with full initialization
	JMP	SHORT ClsStart	;process the initialization

ClsParamGiven:
	DEC	BX		;restore parameter
	CMP	BX,2		;test upper limit of parameter
	JA	ClsError	;if over, then report error

	MOV	AL,BL		;get lower byte of parameter
	MOV	AH,7		;assume full initialization
	CMP	AL,1		;test the parameter value
	JB	ClsStart	;if CLS 0, then full initialization
	MOV	AH,4		;assume just redisplay keys after clear
	JA	ClsStart	;if CLS 2, then text initialization
	CALL	B$GRMODE		;else CLS 1, test for graphics mode
	JZ	ClsReturn	;if no graphics mode, then just return
	MOV	AH,1		;just erase the viewport
	CMP	B$VIEWSW,0	;test if viewport active
	JNZ	ClsStart	;if so, then just redisplay keys
	MOV	AH,7		;else erase screen and redisplay keys

;	AL contains the mode for the call to B$CLRSCN.
;	AH contains a bit mask to execute the proper combination
;	    of routines after B$CLRSCN.
;		1 - clear the graphics viewport
;		2 - refresh the function key display
;		4 - home the cursor to the current text window

ClsStart:
	CALL	B$CLRSCN	; clear the screen with value in AL
	JB	ClsError	;if error, then jump to report it

	TEST	AH,1		;test if graphics viewport is initialized
	JZ	ClsNoViewInit	;if not, then jump
	PUSH	AX		;save bit mode and bit mask over call
	CALL	[b$VWCLR_PTR]	;initialize viewport variables
	POP	AX		;and restore it
ClsNoViewInit:

	TEST	AH,2		; test if keys are to be redisplayed
	JZ	ClsNoKeys	; brif not
	CALL	B$KEYDSP	; redisplay key line or blank last line
ClsNoKeys:			

	TEST	AH,4		; test if text cursor to be homed
	JZ	ClsReturn	; brif not
	CALL	B$WHOME		; DX has home position
	CALL	B$SCNLOC	; display user cursor.  TRASHES AX, so
				; don't get any ideas about moving this
				; up before the call to B$KEYDSP.
ClsReturn:
cEnd

ClsError:
	JMP	B$ERR_FC	;Bad Parm, complain.

	SUBTTL	CSRLIN and POS functions
	PAGE
;***
;B$CSRL - return current line (row) of the cursor
;
;Purpose:
;	Runtime enty point.
;	Return current 1-relative line (row) of the cursor on the screen.
;Entry:
;	None.
;Exit:
;	AX = screen row
;Uses:
;	Per convention
;Exceptions:
;	None.
;****
cProc	B$CSRL,<PUBLIC,FAR>	
cBegin				

	MOV	DX,b$CURSOR	; place screen row in DL, column in DH
	CMP	DH,b$CRTWIDTH	; past last physical column?
	JBE	VALUE_OK	; brif not -- row is ok
	CALL	B$CRLF		; adjust DL, DH trashed by CBW anyway
VALUE_OK:			
	XCHG	AX,DX		; return line in AL
	CBW			; clear high byte
cEnd				


;***
; B$FPOS - POS(n) function
;
; Purpose:
;	Runtime Entry Point.
;	Return the 1-relative horizontal screen position of the cursor.
; Input:
;	NONE
; Output:
;	AX= position (1-relative)
; Modifies:
;	F
; Exceptions:
;	NONE
;****
cProc	B$FPOS,<PUBLIC,FAR>	
parmW	Dummy			
cBegin				
	MOV	AL,b$CSRX	; get 1-relative column position
	CMP	AL,b$CRTWIDTH	; past last physical column?
	JBE	POS_OK		; brif not -- value OK
	MOV	AL,1		; assume first column
POS_OK:				
	CBW			; clear high byte
cEnd				

	SUBTTL	KEY statement
	PAGE
;***
;B$KFUN - KEY ON, KEY OFF, KEY LIST
;
;Purpose:
;	Runtime Entry Point.
;	Executes KEY ON, KEY OFF, or KEY LIST depending on input
;Entry:
;	fAction = 0 - KEY OFF
;		  1 - KEY ON
;		  2 - KEY LIST
;Exit:
;	None.
;Uses:
;	Per convention.
;Exceptions:
;	None.
;****
cProc	B$KFUN,<PUBLIC,FAR>	
parmW	fAction 		
cBegin				
	MOV	AX,100H 	;AH=scroll delta, AL=scroll flag (off)
	MOV	CX,fAction	;get action flag
	JCXZ	KEYOFF		;key off
	LOOP	KEYLST		;brif Key list

;	KEY ON...
	MOV	AX,-1		;AH=scroll delta, AL=scroll flag (ON)
KEYOFF:
	CMP	AL,b$KEY_SW	;state change?
	MOV	b$KEY_SW,AL	
	JZ	KEYXX		;Brif same, do nothing
	CALL	B$KEYDSP 	;On, Display on 25th line.
	JMP	SHORT KEYXX	;exit
KEYLST: 			
DbAssertRel	CX,E,1,CN_TEXT,<Illegal flag as input for B$KFUN>
	CALL	KEYLIST 	
KEYXX:
cEnd				

	PAGE
;***
;KEYLIST - List function key definitions
;
;Purpose:
;	Prints all currently defined function keys.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	Per convention.
;Exceptions:
;	None.
;****
cProc	KEYLIST,<NEAR>,<SI,DI>
cBegin
	MOV	DI,OFFSET DGROUP:b$STRTAB ;function key str descriptors
	MOV	CX,NUM_FKEYS	; Keys to the Screen, default 12 for Ronco
	XOR	DL,DL		;DL = FN key counter
	TEST	b$RcoFlg,0FFH	; is Ronco keyboard presented ?
	JNZ	KEY_LIST_0	; Brif yes
	DEC	CX		
	DEC	CX		; AT keyboard has 10 function keys
KEY_LIST_0:
	INC	DL		
	MOV	AL,"F"
	CALL	B$$WCHT
	CALL	KEY_DSP_NUM	;Display the Key number.
	MOV	AL," "
	CALL	B$$WCHT
	PUSH	CX
	MOV	CX,[DI] 	;get char count of string
	JCXZ	KEY_LIST_3
	MOV	SI,[DI+2]	;get ptr to string
KEY_LIST_1:
	LODSB
	OR	AL,AL
	JZ	KEY_LIST_3
	CMP	AL,ASCCR
	JNZ	KEY_LIST_2
	MOV	AL," "
KEY_LIST_2:
	CALL	B$$WCHT
	LOOP	KEY_LIST_1
KEY_LIST_3:
	MOV	AL,ASCCR
	CALL	B$$WCHT
	POP	CX
	ADD	DI,4		;point to next FN key SD
	LOOP	KEY_LIST_0	
cEnd				

	PAGE
;***
;KEY_DSP_NUM - convert function key number to 2 chars and display them
;
;Purpose:
;	Converts function key number in DL to ascii chars and
;	displays 2 chars (leading space if num < 10
;Entry:
;	DL - function key number
;Exit:
;	None.
;Uses:
;	Per convention.
;Preserves:
;	BX,CX,DX
;Exceptions:
;	None.
;****
cProc	KEY_DSP_NUM,<NEAR>	
cBegin				
	MOV	AL,DL		;get fn key number
	AAM
	OR	AX,"00"
	CMP	AH,"0"
	JNZ	KEY_DSP_NUM2
	XCHG	AH,AL
	MOV	AL," "
KEY_DSP_NUM2:
	PUSH	AX
	MOV	AL,AH
	CALL	B$$WCHT		;Output MSD
	POP	AX
	JMP	B$$WCHT		;Output LSD
cEnd	nogen			;returns through B$$WCHT

	SUBTTL	Screen function
	PAGE
;***
;B$FSCN - SCREEN Function
;
;Purpose:
;	Syntax: x = SCREEN(row,col [,z])
;
;		Returns the Ordinal of the char on the screen
;		at row,col.
;
;	If z is given, and non-zero, then:
;		Returns the Color Attribute of the character
;		on the screen at row,col.
;Entry:
;	Row
;	Column
;	fAttr -     If non-zero then return Attribute.
;		    If zero then return character Ordinal.
;Exit:
;	[AX] -	Contains character ordinal or attribute value.
;Uses:
;	Per Convention
;Exceptions:
;	B$ERR_FC
;****
cProc	B$FSCN,<PUBLIC,FAR>	
parmW	Row			
parmW	Column			
parmW	fAttr			
cBegin				
	MOV	DX,Row		
	MOV	AX,Column	

	OR	AH,DH		
	JNZ	SCF_ERROR	;Out of range if either GT 255

;	It is assumed that both b$CRTWIDTH & B$LINCNT will be less than 255

	DEC	DX		; For comparison purposes make AX & DX
	DEC	AX		; 0-relative

	CMP	AL,b$CRTWIDTH	; Is Y greater than the number of rows?
	JAE	SCF_ERROR	;Error if so.
	CMP	DL,B$LINCNT	;Check for parameter range
	JAE	SCF_ERROR	;1..B$LINCNT-1 if keys on, 1..B$LINCNT if off

	INC	DX		; Bring AX & DX back
	INC	AX		; to 1-relative values

	MOV	DH,AL		;DH=col, DL=row
	cCALL	B$SCREEN 	; [AX] = Char read at position
				; [BX] = Attribute at position

	XCHG	AX,BX		; Get Attribute in AX
	CMP	fAttr,0		; Was attribute wanted?
	JNE	FSCN_RET	; Brif so - AX contains attribute
	XCHG	AX,BX		; AX = Character
				; else return character in AX

FSCN_RET:			; Common exit point

cEnd				

SCF_ERROR:
	JMP	B$ERR_FC	;Generic Complaint

sEnd	CN_TEXT 		
END
