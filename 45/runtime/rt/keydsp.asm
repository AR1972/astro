
	TITLE	KEYDSP - Display function keys/ Clear last line
;***
; KEYDSP - display function keys / Clear last line / utility routines
;
;	Copyright <C> 1987 Microsoft Corporation
;
;Purpose:
;
;  This module is used to display the values of the function keys on the
;  last line of the screen.  It is also used to clear the last line.
;  This module is not generally linked in unless an INPUT or KEY statement
;  occurs in the BASIC program.  Called in by B$INPP and B$KFUN.
;
;******************************************************************************

	INCLUDE switch.inc
	INCLUDE rmacros.inc	; general runtime macros

	UseSeg	_BSS
	UseSeg	_DATA
	UseSeg	RT_TEXT
	UseSeg	INIT_CODE
	UseSeg	<XIB>		; XIB and XIE must bracket XI!
	UseSeg	<XI>		; initializer segment
	UseSeg	<XIE>		

	INCLUDE seg.inc		; segment definitions
	INCLUDE baslibma.inc
	INCLUDE files.inc
	INCLUDE nhutil.inc
	INCLUDE ascii.inc
	INCLUDE idmac.inc	; Internal debugging macros
	INCLUDE const.inc	; bit flag constants
	INCLUDE rtps.inc	; constants shared with QBI

	.radix	10

INITIALIZER	B$xFNKYDSPINI	

sBegin	INIT_CODE
assumes	CS,INIT_CODE

cProc	B$xFNKYDSPINI,<FAR>
cBegin
	mov	[b$vKEYDSP],RT_TEXTOFFSET B$KEYDSP ;;initialize B$KEYDSP vector
cEnd
sEnd	INIT_CODE

;#***********************************************************************
;
sBegin	_DATA

externW	b$vKEYDSP		;vector for B$KEYDSP indirect calls
externW	b$CURSOR		; (1,1)-relative screen cursor
externB b$LINCNT
externB	b$IOFLAG		; general-purpose IO bits 

sEnd	_DATA
;
;#***********************************************************************

;#***********************************************************************
;
sBegin	_BSS

externW   b$PTRFIL	  ;GOSTOP -
externW   B$FKCNUM	  ;GWDATA -
externW   b$STRTAB	  ;GWDATA -
externB   b$KEY_SW	  ;GWDATA -


sEnd	_BSS
;
;#***********************************************************************

sBegin	RT_TEXT

assumes cs,RT_TEXT

;#*************************************
;	Equates

	FKEYCR=27D		;IBM CR FKey display line graphic

;#*************************************
;	Externals

externNP   B$SCNLOC
externNP   B$CLREOL
externNP   B$OFFCSR		; turn off cursor (B$CSRDSP with AL=0)
externNP   B$FKYFMT
externNP   B$GETFBC		; Get and set
externNP   B$SETFBC		; foreground/background attributes
	externNP   B$TTY_SOUT

;#*************************************
;	Publics

;***
; B$LastLine -- return values of last line.
;
;Purpose:
;	Return cursor position of first column of last line.
;Entry:
;	None.
;Exit:
;	DX = b$CURSOR = cursor position of first column of last line.
;Modifies:
;	none.
;Exceptions:
;	none.
;****

cProc	B$LastLine,<NEAR,PUBLIC>
cBegin
	MOV	DH,1		; column 1
	MOV	DL,B$LINCNT 	; last line
	MOV	b$CURSOR,DX	; update cursor variable
cEnd


	SUBTTL	KEYON,	KEYOFF, and KEYDSP

;***
;B$KEYDSP -	Display Softkeys on last line of Screen.
; Entry:
;	None
; Exit:
;	None.
; Modifies:
;	none.
;****

cProc	B$KEYDSP,<NEAR,PUBLIC>,<AX,BX,CX,DX,SI>
cBegin
				
	OR	b$IOFLAG,F_KDSP ; Tell B$TTY_SOUT we want $FKYMAP

	PUSH	b$CURSOR	; Save current cursor position (row and col)
	CALL	B$LastLine	; DX = b$CURSOR = first column of last line

	MOV	AL,b$KEY_SW
	OR	AL,AL		;Key on or off?
	JNZ	KEYDS0		;Softkey display switch on
	CALL	B$CLREOL	;Clear from (DH,DL) to EOL,return cursor
keyret:
	POP	DX		;get back original cursor position.
	CALL	B$SCNLOC 	; Reset cursor position and display user
				; cursor
	AND	b$IOFLAG,NOT F_KDSP ; Turn off function key display mode

cEnd


KEYDS0:
	CALL	B$OFFCSR 	;Turn the cursor off, and position

	CALL	GETFMT		;Get function key display format
KNXTST: PUSH	AX		;Save Key disp no.
	CMP	AH,"0"		;Single digit case?
	JZ	SINDIG		;Print only one digit
	XCHG	AH,AL
	CALL	KEYDCH		;Display first digit
	XCHG	AH,AL
SINDIG: CALL	KEYDCH		;Display last digit
	PUSH	SI
	LODSW
	XCHG	BX,AX		;Get length of this function key string
	LODSW
	XCHG	SI,AX		;SI := address of string.
	MOV	CL,BYTE PTR B$FKCNUM ;Count of chars per fun. key (set by GET
	OR	BL,BL		;test if softkey string is null
	JZ	FKCNL1		;if so, do not invert the video
	CALL	$XFGBG		;Swap Forground & background colors
FKCNL1:
;BX=length of string, CL=number of chars to display
KNXTCH: 			; Write the next key character
	OR	BX,BX		; if length<=0 then print a blank
	jg	PrintChar	; else print char from string
	xor	al,al
	jmp	SHORT NOCR	
PrintChar:
	LODSB

	CMP	AL,ASCCR	;     If char is CR then print graphic char
	JNZ	NOCR
	MOV	AL,FKEYCR
NOCR:
	CALL	KEYDCH		;Display char, adv cursor in DX
	dec	bx
	DEC	CL		;Repeat until FCKNUM chars/fkey displayed
	JNZ	KNXTCH		;Loop for next character

	ADD	BL,BYTE PTR B$FKCNUM ;BL is 0 if softkey string null
	JZ	FKCNL2		;if null, then do not invert video
	CALL	$XFGBG		;Swap Forground & background colors
FKCNL2:
	XOR	AL,AL
	CALL	KEYDCH
	POP	SI
	POP	AX
	CALL	KEYADV		;Advance to next key
	DEC	CH
	JNZ	KNXTST		;Loop for next key string
	jmp	keyret		;Return cursor to original position and exit


KEYDCH:
	PUSH	AX
	PUSH	BX
	OR	AL,AL		;Separating keys?
	JNZ	KEYDC1		;Brif not.
	MOV	AL," "		;else write space
KEYDC1:
	CMP	AL,ASCLF	;Line feed?
	JNZ	KEYNLF		;Not line feed
	MOV	AL,74O		;Substitute Greater-Than-Sign
KEYNLF:

	PUSH	CX
	CALL	B$TTY_SOUT	; Write the character, skipping redirection.

	POP	CX
	POP	BX
	POP	AX		;Restore key number
	RET

	PAGE
;Get function key display format

GETFMT: PUSH	BX
	cCALL	B$FKYFMT 	; OEM routine
	MOV	CX,WORD PTR 0[BX] ;CH=key count, CL=Chrs/key
	MOV	BYTE PTR B$FKCNUM,CL
	PUSH	CX
	MOV	SI,OFFSET DGROUP:b$STRTAB ;SI=address of first fkey in table
	MOV	AL,BYTE PTR 2[BX]
	CBW
	PUSH	AX		; Save number of first function key
	DEC	AX		; Set to zero relative
	SHL	AX,1		; Multiply by  4 (bytes/key descriptor)
	SHL	AX,1		; AX = index of first display key
	ADD	SI,AX
	POP	AX
	CALL	INTOCH		;Get key number to character code
	CMP	AH,"0"
	JZ	ONEDIG		;Only one digit
	DEC	BYTE PTR B$FKCNUM ;Adjust function key format for two digits
ONEDIG: CALL	KADNRM		;Normalize key address
	POP	CX
	POP	BX
	RET

;INTOCH: Translate integer AL to characters in AX.
;	 Integers must be in the range (100,0].
;	 Radix is 10.
;USES -  none


INTOCH: PUSH	CX
	XOR	AH,AH
	MOV	CL,10D		;Load radix
	DIV	CL
	ADD	AX,"00"		; 3030H forms character codes
	XCHG	AH,AL		;AH represents significant digit
	POP	CX
	RET

;KEYADV - Advance to next key

KEYADV: ADD	SI, 4D		;Move to next key table entry
	INC	AL
	CMP	AL,"9"
	JLE	KADNRM
	MOV	AL,"0"
	INC	AH
	CMP	AH,"1"		;Test for first two digit key number
	JNZ	KADNRM
	DEC	BYTE PTR B$FKCNUM ;And adjust format for it
KADNRM: CMP	SI,OFFSET (DGROUP:b$STRTAB)+NUM_FKEYS*4
	JBE	KADNMX		; brif less than end address
				; (UNSIGNED compare required!!!)
	MOV	SI,OFFSET DGROUP:b$STRTAB ;Wrap around to the first function key
	MOV	AH,"0"
	MOV	AL,"1"
	INC	BYTE PTR B$FKCNUM ;Re-adjust format
KADNMX: RET

	PAGE
	SUBTTL	Swap Forground & Background Colors

;Swap Forground & Background Colors (Toggle Reverse Video Mode)

$XFGBG:
	push	ax
	push	bx
	CLC			;Signal text attributes
	CALL	B$GETFBC	;Get foreground/background attributes
	XCHG	AX,BX
	CALL	B$SETFBC	;Set foreground/background attributes
	pop	bx
	pop	ax
	RET

sEnd	RT_TEXT

	END
