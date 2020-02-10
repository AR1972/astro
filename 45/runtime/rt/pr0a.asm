	TITLE	PR0A - Prepare for printing
	page	56,132
;***
; PR0A - Prepare for printing
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - LPRINT [USING] Statement :
;
;      LPRINT [USING "...";] [list of expressions]
;      ------ -------------- ---------------------
;      |	 |		   |
;     B$LPRT  B$USNG	    (see comments in prnval.asm header)
;
; - PRINT [#] [USING] Statement:
;
;      PRINT [#filenumber,] [USING "...";] [list of expresions]
;	      ------------- -------------- --------------------
;	       |	     |			  |
;	     B$CHOU	  B$USNG      (see comments in prnval.asm header)
;
; - WRITE [#] Statement:
;
;      WRITE [#filenumber,] [list of expressions]
;      -----  ------------  ---------------------
;	 |	   |		  |
;      B$WRIT	 B$CHOU   (see comments in prnval.asm header)
;
;	PRINT		-- no preamble
;	WRITE		-- B$WRIT
;	LPRINT		-- B$LPRT
;
;	#		-- B$CHOU
;	USING		-- B$USNG
;
;	Note:	Currently, there are two routines which set up channels,
;		namely, B$CHAN, and B$CHOU.  B$CHAN sets up channel
;		number without checking mode, and B$CHOU excludes the
;		input mode.
;
;******************************************************************************

	INCLUDE switch.inc
	INCLUDE rmacros.inc

;Code segments:
	useSeg	DV_TEXT 	;device independent I/O
	useSeg	DK_TEXT 	;disk I/O
	useSeg	NH_TEXT 	;near heap
	useSeg	ER_TEXT 	;error handling
;Data segments:
	useSeg	CONST		;constant definitions
	useSeg	_DATA		;initialized variables
	useSeg	_BSS		;uninitialized variables

	INCLUDE seg.inc
	INCLUDE baslibma.inc
	INCLUDE devdef.inc
	INCLUDE files.inc
	INCLUDE rtps.inc	; constants shared with QBI

	SUBTTL	local constant definitions
	page

	TTY	EQU	0	;default for b$PTRFIL is TTY

	PRSTM	EQU	0	;print statement
	CHANL	EQU	1	;#
	USING	EQU	2	;using
	WRSTM	EQU	4	;write statement
	LPSTM	EQU	8	;lprint statement


	SUBTTL	data definitions
	page

sBegin	CONST
	labelW	<PUBLIC,b$FILVEC> ;vector for PRINT
	staticW ,B$$POS		;cursor position
	staticW ,B$$WID		;line width
	staticW ,B$$WCLF 	;force EOL
	staticW ,B$$WCH		;write one character
	staticW ,B$PRTSTR	;write a string with BX=*sd
	staticW ,B$PRT_TYPCNT	;write a string with CX=length
	staticW ,B$PRT_OUTCNT	;write a string char. by char.
	staticW ,B$PRTCHK	; force EOL if string too long
sEnd				;end of CONST

sBegin	_DATA

	externB b$PRFG		;flag for PRINT/LPRINT/WRITE [#][USING]
	externW B$ENTRY_BUFCNT	; fdb position before exec of current
				; statment.
sEnd	;_DATA

sBegin	_BSS
	externW b$PTRFIL	;defined in GOSTOP.ASM

sEnd	;_BSS

	SUBTTL	code segment definitions
	page

sBegin	DK_TEXT
	externNP	B$WCHSET	; in prnval.asm, set dispatch vector
sEnd	;DK_TEXT

sBegin	DV_TEXT
	externNP	B$$POS
	externNP	B$$WID
	externNP	B$$WCLF
	externNP	B$$WCH
	externNP	B$PRTSTR
	externNP	B$PRT_TYPCNT
	externNP	B$PRT_OUTCNT
	externNP	B$PRTCHK	
sEnd	;DV_TEXT

sBegin	NH_TEXT
	externNP	B$LHFDBLOC	
sEnd

sBegin	ER_TEXT
	externNP	B$ERR_IFN	
	externNP	B$ERR_BFM	
sEnd	;ER_TEXT

	assumes CS,DK_TEXT
sBegin	DK_TEXT

	SUBTTL	interface for set up channel -- B$CHAN & B$CHOU
	page
;***
;B$CHAN -- set up channel for statements
;void B$CHAN(I2 channel)
;
;Purpose:
;	Check and make the I/O channel active for current statement.
;Entry:
;	Parameter is in stack.
;	int	Channel
;Exit:
;	AL = device type
;	[b$PTRFIL] is set to the active FDB
;Uses:
;	none
;Exceptions:
;	illegal file number -- ERC_IFN
;*******************************************************************************

cProc	B$CHAN,<PUBLIC,FAR>,<SI>	;save SI
	ParmW	Channel 		;channel #
cBegin
	MOV	BX,Channel	;BX has the channel #
	cCall	B$ChkFNUM	;check file number, must be 1-255
	CALL	B$LHFDBLOC	; [SI] = *FDB & NZ if channel is active
	JZ	ERCIFN		;illegal file number
	MOV	[b$PTRFIL],SI	;store the pointer/handle
	FDB_PTR ES,SI,SI	;(ES:)[SI] = *FDB
	MOV	AX,FileDB.FD_BUFCNT ;Save current fdb position
	MOV	B$ENTRY_BUFCNT,AX   ;Used to reset on error
	MOV	AL,FileDB.FD_DEVICE ;return device number
cEnd				;pop SI, exit to caller

;***
;B$CHOU -- set up channel for statements
;void B$CHOU(I2 channel)
;
;Purpose:
;	Check and make the I/O channel active for current print statement.
;	It also checks whether the file mode is correct.  If the mode is
;	input, this routine gives error message.  If mode is O.K., it
;	sets up flag, b$PRFG, to indicate a special channel is using.
;Entry:
;	Parameter is in stack.
;	int	Channel
;Exit:
;	[b$PTRFIL] is set to the active FDB
;	[b$PRFG] OR CHANL (=1)
;Uses:
;	none
;Exceptions:
;	illegal file number (B$ERR_IFN)
;	Bad file mode -- B$ERR_BFM
;*******************************************************************************

cProc	B$CHOU,<PUBLIC,FAR>,<SI>	;save SI
	ParmW	Channel 		;channel #
cBegin
	MOV	BX,Channel	;BX has the channel #
	cCall	B$ChkFNUM	;go check file number, must be 1-255
	CALL	B$LHFDBLOC	; [SI] = *FDB & NZ if channel is active
	JZ	ERCIFN		;illegal file number
	MOV	AX,FileDB.FD_BUFCNT ; Save current fdb position
	MOV	B$ENTRY_BUFCNT,AX   ; Used to reset on error
	CMP	FileDB.FD_MODE,MD_SQI ;can't be input
	JE	ERCBFM		;give bad file mode
	MOV	[b$PTRFIL],SI	;store the pointer/handle
	OR	[b$PRFG],CHANL ;set the flag for print
				; (note: can't use MOV here)
	MOV	SI,OFFSET DGROUP:b$FILVEC
	cCall	B$WCHSET	;set the dispatch vector for print items
cEnd				;pop SI, exit to caller

ERCBFM: JMP	B$ERR_BFM
ERCIFN: JMP	B$ERR_IFN

;***
;B$ChkFNUM -- check file number
;
;Purpose:
;	Check file number is within 1-255.  Otherwise give "illegal file
;	number"
;Entry:
;	[BX]	= file number
;Exit:
;	none
;Uses:
;	none
;Preserves: (optional)
;	all
;Exceptions:
;	illegal file number -- ERC_IFN
;*******************************************************************************

cProc	B$ChkFNUM,<PUBLIC,NEAR>

cBegin
	OR	BX,BX		;can't be 0
	JZ	ERCIFN		;Brif yes, give "illegal file number"
	OR	BH,BH		;can't exceed 255
	JNZ	ERCIFN		;Brif yes, give "illegal file number"
cEnd				;if no error, exit to caller

sEnd	;DK_TEXT
	END
