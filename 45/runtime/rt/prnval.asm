	TITLE	PRNVAL - Print values
	page	56,132
;***
; PRNVAL - Print values
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;	This module contains B$P<term><type> and B$PEOS interfaces.
;
;	The PRINT, WRITE, and LPRINT statements have a large number of
;	runtime calls that can be generated, based on the list of
;	expressions given. There is a unique preamble call for each such
;	statement (except PRINT), to correctly set up flags, vectors, etc.
;	The actual printing call itself is one of 15 possibilities, based
;	on the argument type, and one of three possible ways of terminating
;	the print: ',', ';', or EOL. Each of these 15 calls is in the
;	form B$P<term><type> where <term> specifies the print termination
;	method, and <type> specifies the type of the argument.
;
;	<term>:
;		C == ',' (i.e., something like 'PRINT X,' was specified)
;		S == ';'
;		E == EOL (neither ',' nor ';' was specified)
;
;	<type>:
;		I2 == two byte integer
;		I4 == four byte integer
;		R4 == single precision real
;		R8 == double precision real
;		SD == string
;
;	Thus, for example, a call to B$PER4 would be used to print a s.p.
;	value and terminate with a EOL.
;
;	The list of expressions can be of any size; for each additional
;	expression, another one of the fifteen possible runtime calls is
;	generated. After the last such expression, a call to B$PEOS is
;	generated (if not terminated with EOL). If no expression is
;	specified, a NULL string is printed, so 'PRINT' would generate a
;	call to B$PESD with a null string as the parameter (print a string,
;	terminate with EOL).
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

;Code segments:
	useSeg	DK_TEXT 	;disk I/O
	useSeg	MT_TEXT 	;floating point math.
	useSeg	NH_TEXT 	;near heap
	useSeg	ER_TEXT 	;error handling
	useSeg	CN_TEXT 	;concole I/O

;Data segments:
	useSeg	CONST		;constant definitions
	useSeg	_DATA		;initialized variables
	useSeg	_BSS		;uninitialized variables

	INCLUDE seg.inc
	INCLUDE baslibma.inc
	INCLUDE devdef.inc
	INCLUDE files.inc
	INCLUDE rtps.inc	; constants shared with QBI
	INCLUDE idmac.inc

	SUBTTL	local constant definitions
	page

	CLMWID	EQU	14	;column width

	COMA	EQU	0	;comma
	SEMI	EQU	1	;semicolumn
	EOL	EQU	2	;forced EOL

	TTY	EQU	0	;default for b$PTRFIL is TTY

	PRSTM	EQU	0	;print statement
	CHANL	EQU	1	;#
	USING	EQU	2	;using
	WRSTM	EQU	4	;write statement
	LPSTM	EQU	8	;lprint statement

	InpDefault	EQU	0FFH ;default for b$Finput


	SUBTTL	data definitions
	page

sBegin	CONST
	labelW	TTYVEC		;vector for PRINT
	staticW ,B$TTY_GPOS	;cursor position
	staticW ,B$TTY_GWID	;line width
	staticW ,B$$TCR		;force EOL
	staticW ,B$TTY_SOUT	;write one character
	staticW ,B$TYPSTR	;write a string with BX=*sd
	staticW ,B$TYPCNT	;write a string with CX=length
	staticW ,B$OUTCNT	;write a string char. by char.
	staticW ,TTY_PRTCHK	; check if EOL needs to be forced
sEnd

sBegin	_DATA
	externB b$PRFG		;print flag, may be combined from below
				;0: PRINT stmt
				;1: # (channel)
				;2: USING
				;4: WRITE stmt
				;8: LPRINT stmt
				;e.g. 3 means PRINT # USING

	labelW	<PUBLIC,b$VECS> ; print value vectors
	staticW VPOS,B$TTY_GPOS		;cursor position
	staticW VWID,B$TTY_GWID		;line width
	staticW VWCLF,B$$TCR		;force EOL
	staticW VWCH,B$TTY_SOUT		;write one character
	staticW VTYP,B$TYPSTR		;BX=*sd
	staticW VTYPCNT,B$TYPCNT 	;BX=*sd, CX=length
	staticW VOUTCNT,B$OUTCNT 	;SI=point to string, CX=length
	staticW VPRTCHK,TTY_PRTCHK	; check if EOL needs to be forced
SizeOfVecs	EQU	(($-b$VECS)/2)	;length of b$VECS table in words
	globalB b$FInput,InpDefault	;Where is input comming from
	globalW b$GetOneVal,<DK_TEXTOFFSET NoGetValAssert> ; vector
					;to routine which INPUT will
					;will use to get next data
	globalW b$pGetValDefault,<DK_TEXTOFFSET NoGetValAssert> 
					;if no input statement is
					;present, attempts to call
					;indirect through b$GetOneVal
					;will die by assertion in
					;non-release runtimes.
	globalW b$pFLUSH,Near_Ret,1	;conditional vector for B$FLUSH
	externB b$CRTWIDTH 	; screen width
sEnd

sBegin	_BSS
	globalW b$PUSG,,1	;print using routine
EVEN				; for safety
	externW b$TempSD	; static string descriptor

	globalW b$StkBottom,,1	; used to clean the stack after TTY inp

	externW b$?TYP		; [b$VTYP|b$TTYP] (defined in global.inc)
	externB b$VTYP		; value type (defined in global.inc)
	externB b$TTYP		; terminator type (defined in global.inc)

	externW b$PTRFIL	; print channel #
	externW b$pSTDALCTMP	; indirect B$STDALCTMP vector

	externB b$LPTFDB 	; FDB for line printer
	externB b$Buf1		
sEnd

	SUBTTL	code externals
	page


sBegin	DK_TEXT
	externNP	B$CNTRL		; needed during B$WRIT
sEnd

sBegin	MT_TEXT
	externNP	B$FOUTBX
sEnd

sBegin	CN_TEXT
	externNP	B$TTY_GPOS	;cursor position
	externNP	B$TTY_GWID	;line width
	externNP	B$$TCR		;force EOL
	externNP	B$TTY_SOUT	;write one character
	externNP	B$TYPSTR 	;write a string with BX=*sd
	externNP	B$TYPCNT 	;write a string with CX=length
	externNP	B$OUTCNT 	;write a string char. by char.
sEnd

	assumes CS,DK_TEXT
sBegin	DK_TEXT


	SUBTTL	print interfaces -- B$P<term><type>(<param>)
	page
;***
; B$P<term><type>(<param>) -- print an item for :
;	PRINT, PRINT #, PRINT USING, PRINT # USING,
;	WRITE, WRITE #,
;	LPRINT, LPRINT USING.
;
;Purpose:
;	These are interfaces to the compiler.  Each entry point sets up
;	(1) types of value & terminator, and
;	(2) a pointer to that item,
;	 and then fall through PRINT, which performs actual printing job.
;
;	<term>, <type> & <param> may vary as follows:
;	<term>:
;		C:	Comma used as terminator
;		S:	Semi used as terminator
;		E:	End of statement used as terminator
;	<type>:
;		I2:	Two byte integer
;		I4:	Four byte integer
;		SD:	String (string descriptor)
;	<param>:
;		A parameter of type <type> to be printed.
;
;
;Entry:
;	Parameter was pushed in stack.
;	<type>	Val = Number if <type> is numerical type
;		    = String descriptor if <type> is string
;
;Exit:
;	through B$PRINT
;Uses:
;	none
;Exceptions:
;
;*******************************************************************************

;========================
;	Print I2,	|
;========================

cProc	B$PCI2,<PUBLIC,FAR>

cBegin	nogen			;no stack frame generated
	MOV	AX,COMA SHL 8 + VT_I2 ;AX=[terminator type | value type]
	JMP	SHORT B$PRINT	;print this item
cEnd	nogen			;no code generated

;========================
;	Print I2;	|
;========================

cProc	B$PSI2,<PUBLIC,FAR>

cBegin	nogen			;no stack frame generated
	MOV	AX,SEMI SHL 8 + VT_I2 ;AX=[terminator type | value type]
	JMP	SHORT B$PRINT	;print the item
cEnd	nogen			;no code generated

;========================
;	Print I2	|
;========================

cProc	B$PEI2,<PUBLIC,FAR>

cBegin	nogen			;no stack frame generated
	MOV	AX,EOL SHL 8 + VT_I2 ;AX=[terminator type | value type]
	JMP	SHORT B$PRINT	;print the item
cEnd	nogen			;no code generated

;========================
;	Print I4,	|
;========================
cProc	B$PCI4,<PUBLIC,FAR>

cBegin	nogen			;no stack frame generated
	MOV	AX,COMA SHL 8 + VT_I4 ;AX=[terminator type | value type]
	JMP	SHORT B$PRINT	;print the item
cEnd	nogen			;no code generated

;========================
;	Print I4;	|
;========================
cProc	B$PSI4,<PUBLIC,FAR>

cBegin	nogen			;no stack frame generated
	MOV	AX,SEMI SHL 8 + VT_I4 ;AX=[terminator type | value type]
	JMP	SHORT B$PRINT	;print the item
cEnd	nogen			;no code generated

;========================
;	Print I4	|
;========================
cProc	B$PEI4,<PUBLIC,FAR>

cBegin	nogen			;no stack frame generated
	MOV	AX,EOL SHL 8 + VT_I4 ;AX=[terminator type | value type]
	JMP	SHORT B$PRINT	;print the item
cEnd	nogen			;no code generated

;========================
;	Print a$,	|
;========================

cProc	B$PCSD,<PUBLIC,FAR>

cBegin	nogen			;no stack frame generated
	MOV	AX,COMA SHL 8 + VT_SD ;AX=[terminator type | value type]
	JMP	SHORT B$PRINT	;print the item
cEnd	nogen			;no code generated

;========================
;	Print a$;	|
;========================

cProc	B$PSSD,<PUBLIC,FAR>

cBegin	nogen			;no stack frame generated
	MOV	AX,SEMI SHL 8 + VT_SD ;AX=[terminator type | value type]
	JMP	SHORT B$PRINT	;print the item
cEnd	nogen			;no code generated

;========================
;	Print a$	|
;========================

cProc	B$PESD,<PUBLIC,FAR>

cBegin	nogen			;no stack frame generated
	MOV	AX,EOL SHL 8 + VT_SD ;AX=[terminator type | value type]
cEnd	nogen			;no code generated

	SUBTTL	actual routine to print an item
	page
;***
;B$PRINT - continue the print interfaces to print an item
;
;Purpose:
;	Print an item for PRINT/WRITE/LPRINT statement.
;
;	Printing an item is affected by:
;	(1) whether it's a 'print using'
;	(2) whether it prints a string (numbers need to be translated)
;	(3) whether it is a print stmt or is a write stmt.
;		In case of write stmt, the following are different:
;		(a) no leading blank for a positive number if '+' omitted
;		(b) no ending space for a number
;		(c) string delimited by '"'
;		(d) comma delimiting items
;
;	One major difference here is that numbers also use the string output
;	routine to print on a device.  The string output routine outputs one
;	string a time, if it fits the rest of the line, which is faster than
;	iteratively outputs one character a time.
;
;	Certainly, one piece of code, which serves such many functions, can't
;	be as clean as some simple routines do.  However, there still has its
;	logic as follows:
;
;	B$PRINT (TerminatorType_&_ValueType)
;	{
;	    if channel on, test the mode is not input
;		if error then give "bad file mode"
;	    get pointer of the item to be printed from stack
;	    set up types
;
;	    /* print the item */
;
;	    if the request was PRINT USING, then
;		call print using routine
;	    else	/* not a PRINT USING */
;		if the request was printing a number, then
;		    translate the number into digital string
;		    if the request was a WRITE statement, then
;			special process the leading & ending space
;		else	/* printing a string */
;		    if the request was a WRITE statement, then
;			special process the string delimitor
;		print the string
;
;	    /* process the terminator */
;
;	    if the terminator was a EOL, then force a carrage return
;	    if the terminator was a COMMA, then fill the rest of the column
;		with blanks
;	    if the terminator was a SEMI-COLUMN and the request was a WRITE
;		statement, then print a ','
;
;	    /* handle the exit process explicitly */
;
;	    play around with BP and SP
;
;	    /*	When exit the stack looks like :	*/
;	    /*			__________________	*/
;	    /*			|	SI	 |	*/
;	    /*			|----------------|	*/
;	    /*			|  DS (if ?WIN)  |	*/
;	    /*		  BP -->|----------------|	*/
;	    /*			|	BP	 |	*/
;	    /*			|----------------|	*/
;	    /*			|    Ret offset  |	*/
;	    /*			|----------------|	*/
;	    /*			|    Ret segment |	*/
;	    /*		 Val -->|----------------|	*/
;	    /*			|    Parameter	 |	*/
;	    /*	(variable len)	\................\ 	*/
;	    /*			\................\ 	*/
;	    /*	      old SP -->|----------------|	*/
;	    /*			|________________|	*/
;	    /*						*/
;	}
;Entry:
;	[AH] = [terminator type]
;	[AL] = [value type]
;	in stack,
;	Val = first word of item, where item is either I2,I4,R4,R8 or sd/psd
;Exit:
;	none
;Uses:
;	none
;Exceptions:
;	bad file mode
;	I/O error or Disk full error when flush the buffer if a EOL encountered
;*******************************************************************************

				;NOTE!: This routine has a manually
				;generated epilogue due to variable number
				;of parameters.  If you change the prologue,
				;here you must also change the epilogue
				;further down.

cProc	B$PRINT,<PUBLIC,FAR>,<SI> ;common routine, has to be declared
				; as FAR for setting up stack frame correctly.
				; SI saved
	ParmW	Val		;dummy parameter, which points to the first
				; byte of the actual parameter regardless how
				; many bytes it is;  this also forces to set
				; up the stack frame
cBegin				;stack frame is set up and SI is saved
	MOV	[b$?TYP],AX	;set up b$VTYP & b$TTYP
	LEA	BX,Val		;get the address of parameter

	CMP	AL,VT_SD	; is this a string parm?
	JNZ	ChkString	; brif not - BX has the pointer to the value
	MOV	BX,[BX] 	; if SD, BX has the value, not pointer
ChkString:

	TEST	[b$PRFG],USING	;is print using on ?
	JNZ	PRTUSG		;Brif yes
	CMP	AL,VT_SD	;printing a string ?
	JZ	PRTSTR		;Brif yes

;[2] At this moment, [BX]=*I2,*I4,*R4 or *R8. 
;	Currently B$FOUTBX changes all registers except BP.  This has to be
;	changed later on.

	CALL	B$FOUTBX	; Hopefully, the return from B$FOUTBX
				;  are BX containing the address of the
				;	string and AX is the length
	MOV	SI,BX		; SI=*Number
	TEST	[b$PRFG],WRSTM	;is write statement on ?
	JNZ	WRTVAL		;Brif is write stmt
	ADD	BX,AX		;bx points to the terminate byte (00)
	INC	AX		;increment the length of the digital string
	MOV	BYTE PTR [BX]," " ;Number always followed by " "
				;Note:	the static buffer for translating a
				;	number must at least one byte longer
				;	than the maximum possible length.
	cCall	MAKESD		;[BX]=*sd on return
PRTIT1: 			;at this entry, check whether a EOL is needed
	PUSH	BX		;save psd
	cCall	[VPRTCHK]	; CY if room is not enough (possibly a EOL
				; was already forced), needs AX=len of string
	POP	BX		;get back psd
PRTIT2: 			;at this entry, no check for EOL (write)
	PUSH	BX		; save psd
	CALL	[VTYP]		;print the string, needs [BX]=psd
	POP	BX		; get back psd
	CALL	[b$pSTDALCTMP]	; deallocate the temp if it is
PRTEND:
	MOV	AL,[b$TTYP]	; get delimiter type
	CBW			; extend to use word ops
	DEC	AX		; test for EOL delimiter
	JG	PRTCLF		; Brif end with LF-CR
	TEST	[b$PRFG],WRSTM	;is write stmt on ?
	JZ	PRTEND2 	; Brif not
	MOV	AL,","		; here if comma or semi and write stmt
	CALL	[VWCH]		;write the character
	JMP	SHORT PRINTX	;exit to caller

WRTVAL: 			;write stmt take off the leading " "
	CMP	BYTE PTR [BX]," " ;is first a blank ?
	JNZ	WTMKSD		;Brif not, go make sd & print it
	INC	SI		;skip the leading blank if write statement
	DEC	AX		;decrement the length
WTMKSD:
	cCall	MAKESD		;[BX]=*sd
	JMP	SHORT PRTIT2	;go print it
PRTUSG:
				; print using, so make QBI compatable.
	CMP	AH,COMA		; PRINT USING COMMA?
	JNE	NotComma	; brif not -- don't alter terminator
	MOV	b$TTYP,SEMI	; change comma to semicolon
NotComma:			
	CALL	[b$PUSG]	;print using is handled specially
	JMP	SHORT PRTEND	;process the terminator
PRTSTR: 			;when enter, BX=*sd
	MOV	AX,[BX] 	; AX = length of string
	TEST	[b$PRFG],WRSTM	; is write stmt on ?
	JZ	PRTIT1		;Brif not, go printing string
	MOV	AL,'"'		;'"' is the delimitor of write string
	PUSH	BX		; save psd
	CALL	[VWCH]		;print '"'
	POP	BX		; restore psd
	CALL	[VTYP]		;output the string
	CALL	[b$pSTDALCTMP]	; deallocate the temp if it is
	MOV	AL,'"'		;'"' is the delimitor of write string
	CALL	[VWCH]		;print it
	JMP	SHORT PRTEND	;process the terminator
PRTCLF:
	CALL	[VWCLF] 	;force a EOL
	cCall	BPEOS		;epilog for PRINT
	JMP	SHORT PRINTX	;exit to caller
PRTEND2:			; here if comma or semi and NOT write stmt
	INC	AX		; test for semi delimitor
	JNZ	PRINTX		; go exit, done if semi-colon
PRTCMA:
	CALL	[VPOS]		;[AH]=current cursor position
	MOV	AL,AH		;position in AL
	XOR	AH,AH		;prepare for DIV
				;Note: can't use CBW here, since the range
				;	is 0 - 255 (unsigned)
	MOV	CL,CLMWID	;get field length
	DIV	CL		;AH=remainder, is the position in this column
	SUB	CL,AH		;spaces needed to fill this column
	XCHG	AX,CX		; put count in AL
	CBW			;extend to a word
	PUSH	AX		;save count of patching spaces
	ADD	AX,CLMWID	;account for width of next column
	cCall	[VPRTCHK]	; CY if room is not enough to fit (possibly
				; a EOL was forced)
	POP	CX		;get back count in CX
	JB	PRINTX		;no need to patch spaces
	CALL	B$OutBlanks	; output CX blanks
PRINTX: 			;pop SI & exit to caller

	POP	SI		; restore
	POP	BP		
	POP	CX		; [CX] = return offset
	POP	DX		; [DX:CX] = return address
	POP	BX		; discard 1st word of parameter
	MOV	AL,BYTE PTR [b$?TYP]	; [AL] = type byte
	TEST	AL,VT_SD	; NZ if I2 or SD, i.e., if 1-word parm
	JNZ	PRINTX_5	; Jump if we don't need to pop more
	POP	BX		; Discard 2nd word of parameter
	TEST	AL,8		; R8 or currency (8-byte values)?
	JZ	PRINTX_5	; no, don't pop any more
	POP	BX		; discard 3rd word of parameter
	POP	BX		; discard 4th word of parameter
PRINTX_5:			
	PUSH	DX		; put back seg...
	PUSH	CX		; ... and offset of far return address
	RET			; and return

cEnd	nogen			;no code generated

	SUBTTL	supporting routines for print an item
	page
;***
;PRTCHK -- check whether there is room for the printing string
;
;Purpose:
;	Check whether there is room for the printing string.  If it isn't,
;	force a EOL if the current position is not in col. 1.
;
;	Prtchk(len_of_str)
;	register int len_of_str
;	{
;	    register int d_width
;	    register int current_pos
;
;	    if ((d_width=vwid()) != 255)
;		if ( (len_of_str > 255) ||
;		     ((d_width - (current_pos=vpos()) - len_of_str) < 0) )
;		{   if (current_pos != 0) /* 0-relative */
;			vclf()
;		    Set_CY
;		}
;	}
;
;Entry:
;	register AX = len_of_str
;
;Exit:
;	CY if room left is not enough (may or may not force a EOL)
;
;Uses:
;	None
;
;Exceptions:
;	None
;*******************************************************************************

cProc	B$PRTCHK,<NEAR,PUBLIC>	

cBegin
	XCHG	DX,AX		; length in DX
	CALL	[VWID]		;[AH] = device width
	CMP	AH,255		;is device a file ?
	JZ	CHKEXT		;exit (with NC)
	MOV	AL,AH		;[AL] = device width
	JMP	SHORT PRTCHK1	
cEnd	<nogen>

cProc	TTY_PRTCHK,<NEAR>	
cBegin				
	XCHG	DX,AX		; length in DX
	MOV	AL,b$CRTWIDTH	; AL = device width

PRTCHK1:			
	CALL	[VPOS]		;[AH] = current position
	OR	DH,DH		; more than 255 char to print?
	JNZ	FORCE		; force a EOL
	SUB	AL,AH		;amount left on line
	JB	FORCE		;If no room on line, need new line
	CMP	AL,DL		;will amount requested fit?
	JAE	CHKEXT		;exit (with NC)
FORCE:
	OR	AH,AH		;is current position 0 (at col 1)
	JZ	NOCRLF		;do not print EOL if at col 1
	CALL	[VWCLF] 	;force EOL
NOCRLF:
	STC			;indicate room is not enough
Near_Ret:			;near return for vector
CHKEXT:
cEnd				;end of PRTCHK

;***
;MAKESD -- make a static string descriptor
;
;Purpose:
;	Make a static string descriptor (in b$TempSD) which points to the
;	input string.  Major changes with revision [38].
;
;	WARNING !!! This routine assumes that the word preceding the string
;	WARNING !!! is available to be used as the string header
;
;Entry:
;	[SI] = address of the string
;	[AX] = length of the string
;
;Exit:
;	[BX] = address of static descriptor (b$TempSD)
;	b$TempSD & b$TempStrPtr set up as SD.
;
;Uses:
;	Backs up SI by 2.
;
;*******************************************************************************

cProc	MAKESD,<NEAR>		;private local routine

cBegin
	MOV	BX,OFFSET DGROUP:b$TempSD	; get the offset
	MOV	WORD PTR [BX],AX		; length goes first
	MOV	WORD PTR [BX+2],SI		; string pointer next
cEnd

;***
;BPEOS -- actual code to terminate a print statement.
;
;Purpose:
;	This routine is called by either the interface B$PEOS or B$PExx
;	(print an item which terminated with a EOL)
;	If FV_FARSTR, deallocate "using" string here if it was a temp.
;Entry:
;	b$PTRFIL is the pointer/handle to FDB
;Exit:
;	b$PTRFIL & b$PRFG are reset
;Uses:
;	none
;Exceptions:
;	I/O error or disk full error (when flush the buffer)
;*******************************************************************************

cProc	BPEOS,<NEAR>,<SI>	;was part of $PV4, SI saved

cBegin
	MOV	SI,[b$PTRFIL]	;get the pointer to FDB
	OR	SI,SI		;is file 0 ? (TTY)
	JZ	PEOSX		;Brif tty output, reset flags and exit
	MOV	[b$PTRFIL],TTY ; clear out active FDB block
	FDB_PTR ES,SI,SI	;(ES:)[SI] = *FDB
	CMP	SI,OFFSET DGROUP:b$LPTFDB ;is line printer ?
	JE	PEOSX		;Brif LPRINT
	TEST	FileDB.FD_FLAGS,FL_CHAR ;check for character device file
	JZ	PEOSX		;Brif not character device
	CALL	[b$pFLUSH]	; is character device, flush buffer
				; was save and restore registers AX,BX,CX &
				; DX in $PV4
PEOSX:
	TEST	[b$PRFG],WRSTM+LPSTM+CHANL ;was LPRINT, WRITE or # ?
	MOV	[b$PRFG],PRSTM ;reset the print flag
	JZ	NoSetVec
	MOV	SI,OFFSET DGROUP:TTYVEC
				;get source to fill in
	cCall	B$WCHSET	;reset vector to default, needs SI
NoSetVec:
cEnd				;pop SI & exit to caller

	SUBTTL	print/input interface -- B$PEOS [4]
	page
;***
;B$PEOS -- epilog for PRINT/WRITE/LPRINT/INPUT[#]/READ
;
;Purpose:
;	if print, then clear out active FDB block, and flush buffer in case
;		of a character device, also reset print flags
;	if input, reset input flag & variable
;
;	NOTE: this routine plays around with the stack pointer and stack frame
;		pointer (BP).  Be really careful when save something in the
;		stack.
;Entry:
;	[b$PTRFIL] is the pointer to FDB
;	[b$FInput] is the flag for input
;	[b$PRFG] is the flag for print
;Exit:
;	b$PTRFIL & b$PRFG & b$FInput (also b$GetOneVal) are reset
;Uses:
;	none
;Exceptions:
;	I/O error or disk full error (when flush the buffer (BPEOS))
;*******************************************************************************

cProc	B$PEOS,<PUBLIC,FAR>	

cBegin
	PUSH	BP		
	MOV	BP,SP		;set up stack frame explicitly, since error
				; could happen when flushing the buffer
	PUSH	ES		
	MOV	AL,[b$Finput]	; get flag
	OR	AL,AL		; test it
	JS	TryPrint	; either READ stmt or default, try print
	JNZ	RstFlags	; was disk input, just reset flags
	PUSH	SI		; save SI
	PUSH	DI		; save DI
	MOV	DI,[b$StkBottom]	; DI is the stack bottom
	DEC	DI		; one word above
	DEC	DI
	MOV	SI,SP		; SI points to the BP
	ADD	SI,10		;[12] SI points to ret_seg
	MOV	CX,3		; 3 words to move
	STD			; the move has to be from memory high to
				;  low to avoid overlapping
	PUSH	DS		
	POP	ES		
	REP	MOVSW		; mov BP and return addr to new location
	CLD			; clear direction

	MOV	BP,DI		; stack frame has to be changed
	ADD	BP,2		; new locaton of old BP (later on,
				;  "MOV SP,BP" will clean the stack)
	POP	DI		; get back DI
	POP	SI		; get back SI
RstFlags:			
	cCall	B$InpReset	; reset input flag if this is input stmt
	JMP	SHORT EosExit	
TryPrint:			; either print or READ stmt may be here,
				;  however, next call, BPEOS, won't hurt
				;  if it is READ stmt
	cCall	BPEOS		;try print flush the buffer and reset flags
EosExit:			


	POP	ES		
	MOV	SP,BP		;remove stack frame
	POP	BP		
cEnd				;pop BP & exit

	SUBTTL	set up print dispatch vector
	page
;***
;B$WCHSET -- set up print dispatch vector
;
;Purpose:
;	b$VECS is the dispatch vector for PRINT/WRITE/LPRINT, which contains
;	the address for different functions.  The functions are:
;		b$VECS:
;			VPOS	-- current cursor position
;			VWID	-- device line width
;			VWCLF	-- force EOL
;			VWCH	-- write one character
;			VTYP	-- write a string with a given *sd
;			VTYPCNT -- write a string with length in CX
;			VOUTCNT -- write a string char. by char.
;			VPRTCHK -- check if EOL needs to be forced
;Entry:
;	[SI] points to the source which will fill in the b$VECS
;Exit:
;	b$VECS is set up
;Uses:
;	SI
;Exceptions:
;	none
;*******************************************************************************

cProc	B$WCHSET,<PUBLIC,NEAR>,<DI,ES> ;save ES,DI

cBegin
	PUSH	DS		
	POP	ES		; can't assume ES=DS, set them equal
	MOV	DI,OFFSET DGROUP:b$VECS
	MOV	CX,SizeOfVecs	;count of words to copy
	REP	MOVSW
cEnd				;pop DI,ES and exit to caller


;*** 
; B$OutBlanks -- output a string of blanks.  Added with [22].
;
;Purpose:
;	Save some code, and eliminate a static buffer of blanks.
;	Doesn't print anything if count not in range 1-32767.
;
;Entry:
;	CX = number of blanks to output
;Exit:
;	None
;Uses:
;	Per convention
;Exceptions:
;	None
;
;******************************************************************************

cProc	B$OutBlanks,<PUBLIC,NEAR>,<SI,DI>	; Save SI,DI

cBegin
	OR	CX,CX		; negative or zero byte count
	JLE	OutExit 	; brif no blanks to output


DbAssertRel CX,BE,2*FILNAML,DK_TEXT,<B$OutBlanks: cnt too large>    

	PUSH	CX		;save cnt
	PUSH	SS		;ES = DGROUP
	POP	ES		
	MOV	DI,OFFSET DGROUP:b$Buf1 
	PUSH	DI		
	INC	CX		;round byte count to word count
	SHR	CX,1		
	MOV	AX,'  ' 	
    REP STOSW			;fill with spaces
	POP	SI		;DS:SI = string
	POP	CX		;CX = count
	CALL	[VOUTCNT]	;print the spaces


OutExit:
cEnd				;exit to caller

	page
;***
;NoGetValAssert - Assertion code for calls through [b$GetOneVal].
;
;Purpose:
;	The variable [b$GetVal] is reset with common code to point to
;	the default READ/INPUT entry point.  When no INPUT/READ statement
;	is used in a program linked /O, there is no need to drag in the
;	READ code.  So, b$GetVal is initialized to point to this routine
;	for this case.	If someone inadvertently tries to call through
;	b$GetVal when no READ/INPUT code is loaded, it will be caught here.
;	Added with revision [27].
;Entry:
;	none
;Exit:
;	none
;Uses:
;	none
;Exceptions:
;	none
;*******************************************************************************

cProc	NoGetValAssert,<NEAR>
cBegin
	DbHalt <DK_TEXT>,<Tried to call B$ReadVal when it wasn't loaded>
cEnd
	page
;***
;B$InpReset -- reset flag and variables for INPUT related statement
;
;Purpose:
;	Reset flag and variables for input related statement.
;	Moved to this file with [23]
;
;Entry:
;	none
;
;Exit:
;	[b$FInput]	= InpDefault
;	[b$GetOneItem] = OFFSET B$ReadItem
;	[b$PTRFIL]	= TTY
;
;Uses:
;	none
;
;Exceptions:
;	none
;*******************************************************************************

cProc	B$InpReset,<PUBLIC,NEAR>

cBegin
	MOV	[b$FInput],InpDefault
				;reset input flag

; If input was pulled in, then this points to B$ReadVal, if no READ/INPUT
; statement was used, then this really points to the assertion routine
; NoGetValAssert

	PUSH	AX		
	MOV	AX,[b$pGetValDefault] ;get default value for READ/INPUT

	MOV	[b$GetOneVal],AX ;reset get item routine
	POP	AX
	MOV	[b$PTRFIL],TTY ;reset to TTY
cEnd				;exit to caller

	SUBTTL	interface for WRITE preamble
	page
; B$WRIT moved here from PR0A.ASM and modified for greater /O modularity.
;Revision number [25] applies to entire routine.
;***
;B$WRIT -- preamble for WRITE statement [25]
;void B$WRIT(void)
;
;Purpose:
;	This is the only preamble for WRITE statement.	This routine sets up
;	flag for WRITE statement.  BASCOM 2.0 uses two preambles,
;	$WRI for WRITE and $WRD for WRITE #.
;
;	The flag, b$PRFG, is set to 4 (WRSTM) to indicate a WRITE statement
;	is processing.	If it is a WRITE #, then B$CHOU will OR the flag,
;	b$PRFG, with 1 (CHANL) to indicate a special channel is being used.
;Entry:
;	none
;Exit:
;	b$PRFG is set to WRSTM (WRITE statement is on going)
;	uses default vectors, same as PRINT.
;Uses:
;	none
;Exceptions:
;	none
;*******************************************************************************

cProc	B$WRIT,<PUBLIC,FAR,FORCEFRAME>	; stack frame generated explicitly

cBegin
	OR	[b$PRFG],WRSTM	; set flag indicating WRITE stmt is on going
	CALL	B$CNTRL		; check for control chars during print
cEnd				; return to caller

sEnd				;end of DK_TEXT
	END
