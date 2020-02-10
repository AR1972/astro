	TITLE	INPDSK - disk input
	page	,132
;***
; INPDSK - disk input
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;BASIC Syntax mapping to included runtime entry points:
;
;i)	INPUT[;]["prompt";|,]variable[,variable]...
;
;	BASCOM 2.0 generates calls to IN0A, IPUA, and a call to IPUB for
;	each variable.	Now it will generate the following calls :
;
;	1.	void B$INPP (sd *psdPrompt, I2 FAR *pBlock)
;
;			Where pBlock is a far pointer to a block
;			with the following information :
;
;			word 1 .... word count of the length of this block
;			byte 3 .... an encoded flag having information about
;					CRLF and the ?. It is described below.
;			bytes 4 to n+3 .... have the n types.
;
;			the block would look like :
;
;			1  2  3 	  4
;			----------------------------------------------
;			| n+3 | fCRLFqMark|<---------n types-------->|
;			______________________________________________
;			^
;			|
;			|
;			|
;			pBlock
;
;		The CRLFqMark flag can take the following values :
;
;		0 ....	no ';' before prompt and ';' after prompt.
;		1 ....	no ';' before prompt and ',' after prompt.
;		2 ....	   ';' before prompt and ';' after prompt.
;		3 ....	   ';' before prompt and ',' after prompt.
;
;		Note: the above has the same meaning as:
;			bit 0 set if NO question mark will be displayed
;			bit 1 set if NO no carriage return will be forced.
;
;		The types are as follows :
;
;		I2 .... 2H     \
;		I4 .... 14H	\    This is what the runtime is
;		R4 .... 4H	     using.
;		R8 .... 8H	/
;		SD .... 3H     /
;
;		(INPP stands for INPut Preamble)
;
;		The value of b$FInput will be:
;
;		default 0FFH
;		inptty	0H
;		inpdsk	1H
;
;	2. void B$RD<type> (<type> *pDest)
;
;		Where,
;			<type>	=	I2:	Two byte integer
;					I4:	Four byte integer
;					R4:	Single precision real
;					R8:	Double precision real
;					SD:	String descriptor
;
;		This will be called once for each variable.
;
;		The B$RD<type> routines are going to be shared between
;		the READ and INPUT statements and they need to know whether
;		they are called from an INPUT statement or a READ
;		statement. This is done by setting a flag (say b$FInput)
;		in B$DSKI.
;		(The default value of b$FInput would be used for READ)
;
;		(Note:
;		In the case of interpreted code, the B$RD<type>
;		routines will call the interpreter to get a pointer to
;		the next DATA item and return the # of bytes consumed.)
;
;	3. void B$PEOS(void)
;
;		The flag b$FInput gets cleared in B$PEOS.
;
;ii)	INPUT #filenum, variable [,variable]...
;
;	BASCOM 2.0 generates calls to IN0B, IPUA and IPUB. Now it will
;	generate the following calls :
;
;
;	1. void B$DSKI (I2 channel)
;
;	2. void B$RD<type> (<type> *pDest)
;
;		Refer to the description above.
;
;	3. void B$PEOS(void)
;
;		Refer to the description above.
;
;iii)	LINE INPUT [;]["prompt";|,] stringvar
;
;	BASCOM 2.0 generates calls to IN0A and LIPA. Now it will generate
;	a call to B$LNIN. (see routine documentation).
;
;iv)	LINE INPUT #filenum, stringvar
;
;	BASCOM 2.0 generates calls to IN0B and LIPA. Now it will generate
;	calls to B$DSKI followed by B$LNIN.
;
;	Since B$LNIN is shared between LINE INPUT and LINE INPUT #, B$DSKI
;	sets b$FInput telling B$LNIN that this is disk input. b$FInput gets
;	cleared before exiting B$LNIN.
;
;	B$INPP is in inptty.asm
;	B$DSKI is in inpdsk.asm
;	B$LNIN is in lininp.asm
;	B$RD<type> is in read.asm
;
;******************************************************************************

	INCLUDE switch.inc
	INCLUDE rmacros.inc

;Code segment
	useSeg	DK_TEXT
	useSeg	NH_TEXT
	useSeg	ST_TEXT
	useSeg	MT_TEXT
	useSeg	ER_TEXT
;Data segment
	useSeg	_DATA
	useSeg	_BSS

	INCLUDE seg.inc
	INCLUDE baslibma.inc
	INCLUDE devdef.inc
	INCLUDE files.inc
	INCLUDE ascii.inc
	INCLUDE rtps.inc	; constants shared with QBI
	INCLUDE idmac.inc	

	SUBTTL	local constant definitions
	page


	NEWLINE= ASCCR

	InpDefault	EQU	0FFH	;default READ statement
	InpTTY		EQU	0H	;console input
	InpDsk		EQU	1H	;device input

	SUBTTL	data definitions
	page

sBegin	_DATA

	externW B$ENTRY_BUFCNT	;buffer position in fdb before current stmt.
	externB b$FInput	;default InpDefault
	externW b$GetOneVal	;default OFFSET ReadVal
	externW b$SDBuf1	; sd of b$Buf1
	externW B$AC

sEnd	;_DATA

sBegin	_BSS

	externW b$PTRFIL	;defined in GOSTOP.ASM
	externB b$Buf1		; defined in GWINI.ASM
	externB b$VTYP 	;defined in GLOBAL.INC

sEnd	;_BSS

	SUBTTL	code externals
	page


sBegin	NH_TEXT
	externNP	B$STDALCTMP
	externNP	B$STALCTMP
	externNP	B$STGETSTRLEN
	externNP	B$STALCTMPCPY
	externNP	B$LHFDBLOC	
sEnd	;NH_TEXT

sBegin	DK_TEXT
	externNP	B$$RCH
	externNP	B$PtrDispatch	
	externNP	B$ChkFNUM	
sEnd	;DK_TEXT

sBegin	ER_TEXT
	externNP	B$ERR_IFN
	externNP	B$ERR_RPE
	externNP	B$ERR_BFM	
sEnd	;ER_TEXT

sBegin	ST_TEXT
	externFP	B$SCAT 	
sEnd	;ST_TEXT

sBegin	MT_TEXT
	externNP	B$FIN
	externNP	B$STRSCAN	
sEnd	;MT_TEXT

	assumes CS,DK_TEXT
sBegin	DK_TEXT
	SUBTTL	disk input interface -- B$DSKI & B$RD<type>
	page
;***
;B$DSKI -- preamble for disk input
;void B$DSKI(I2 channel)
;
;Purpose:
;	This is the preamble of disk input.  It sets up flags and variables
;	for subsequent calls to B$RD<type>.
;Entry:
;	Parameter is in stack.
;	int	channel
;Exit:
;	[b$PTRFIL]	= *FDB
;	[b$FInput]	= InpDsk
;	[b$GetOneVal]	= OFFSET DskInpVal
;Uses:
;	none
;Exceptions:
;	illegal file number -- B$ERR_IFN
;	read pass end -- B$ERR_RPE
;*******************************************************************************

cProc	B$DSKI,<PUBLIC,FAR>,<SI>
	ParmW	Channel 	;I2 filenum
cBegin
	MOV	BX,Channel	;BX has the file number
	CALL	B$ChkFNUM	; make sure in range 1-255

	CALL	B$LHFDBLOC	;NZ & SI= *FDB if file exists
	JZ	ERCIFN		;Brif not exist, give "illegal file number"
	FDB_PTR ES,SI,SI	;(ES:)[SI] = *FDB
	MOV	AX,FileDB.FD_BUFCNT ; Save current buffer postion
	MOV	B$ENTRY_BUFCNT,AX   ; Used to reset on error
	TEST	FileDB.FD_MODE,MD_SQO+MD_APP	
	JNZ	ERCRPE		;if output mode, give "read pass end"
	TEST	FileDB.FD_MODE,MD_BIN ; binary mode?
	JNZ	ERCBFM		; brif so --  give "bad file mode"

	MOV	[b$PTRFIL],SI	;set up PTRFIL
	MOV	[b$GetOneVal],OFFSET DskInpVal
				;set routine to input one item
	MOV	[b$FInput],InpDsk
				;indicate disk input
cEnd				;pop si and exit to caller

ERCIFN: JMP	B$ERR_IFN	;illegal file number
ERCRPE: JMP	B$ERR_RPE	;read pass end
ERCBFM: JMP	B$ERR_BFM	;bad file mode

	SUBTTL	READ/disk [LINE] INPUT supporting routines
	page
;***
;DskInpVal -- input one item from a disk file or from a device
;
;Purpose:
;	This routine is to get one value for disk (device) input.
;
;	After the input preamble, the succeeding calls will assign values
;	into variables.  The functionality of those assignment routines,
;	B$RD<type>, may be roughly split into two parts -- namely, getting
;	one value and then assigning it to the variable.  In BASCOM 3.0,
;	those assignment routines are shared by READ stmt, input from TTY
;	and input from a disk file (or a device).  Generally speaking,
;	the second part of the assignment routines DOESN'T discriminate
;	among the statements which share its use.  However, the first part
;	of the assignment routines MUST discriminate among the statements
;	which share its use.  In order to achieve this, the first part of
;	the assignment routines uses an indirect call, CALL [b$GetOneVal],
;	to get one value.  [b$GetOneVal] is default to the address of
;	ReadVal, which gets one value for READ stmt, or contains either
;	the address of TTYInpVal for TTY input or the address of DskInpVal
;	for disk (device) input.
;Entry:
;	[b$VTYP]	= types
;Exit:
;	[SI]		= pointer to source
;Uses:
;	none
;Exceptions:
;	B$ERR_RPE -- read pass end (B$FillBuf generates)
;	B$RUNERR handles it
;*******************************************************************************

cProc	DskInpVal,<NEAR>

cBegin
	MOV	DX,"," SHL 8 + " "	;default delimiters for number
	CMP	[b$VTYP],VT_SD	; is string ?
	JNZ	GetVal		;Brif not
	MOV	DL,DH		;both delimiters be "," for string
GetVal:
	cCall	B$FillBuf	;get one value (in the form of character
				; stream in buffer), on return, SI=*sd
	cCall	DeviceFin	;convert it into number if it is
				; B$AC or B$DAC has the result
	MOV	SI,OFFSET DGROUP:B$AC
	TEST	[b$VTYP],8	;R8 or currency (8-byte values)?
	JZ	DskInpExit	;go if not
	SUB	SI,4		;points to B$DAC
DskInpExit:
cEnd				;exit to caller

	page
;***
;B$FillBuf -- fill one input item into the buffer
;
;Purpose:
;	According to the delimiters, fill in the buffer with one input item,
;	which is still in the form of character stream, from a disk file or
;	from a device.
;
;	A data file for input could be created by any editor or by a BASIC
;	program.  If it is created by BASIC, we should keep our eyes on the
;	continuation mark, LF, and the end-of-line mark, "CR LF."
;
;	This routine abides by two rules.  The first rule is for inputing
;	a quoted string and the second is for unquoted string and numbers
;	as well.  The first rule says to pick up all characters between
;	two '"'s inclusively.  The second rule says that, when a combination
;	of continuation marks and end-of-line marks has been encountered,
;	within the input character stream, if a LF precedes a CR, the CR is
;	ignored.  These rules are for INPUT # stmt.  LINE INPUT # statement
;	follows the second rule, except that, if a LF precedes a CR, the CR
;	is still stored as part of the character stream.
;
;	This routine checks for the situation mentioned above.
;
;	Note: The continuation mark, LF, is treated as a regular character
;		and is stored in the buffer as part of the item if the input
;		is for a number; whereas the LF is NOT stored if the input
;		is for a unquoted string.  One reason for this is that the
;		next call, B$FIN, will take care of the LF when converting
;		the digital stream into a number.
;
;Entry:
;	[DX]		= 0		if LINE INPUT #
;			= ","+" "	if INPUT #, numberVar
;			= ","+","	if INPUT #, stringVar
;	[b$FInput]	= DskInp
;	[b$PTRFIL]	= *FDB
;Exit:
;	[SI]		= *sd of the character stream of the input item
;Uses:
;	none
;Exceptions:
;	read pass end -- _BERR_RPE
;*******************************************************************************

cProc	B$FillBuf,<PUBLIC,NEAR>,<DI,ES> ;save DI,ES

cBegin
	PUSH	DS		
	POP	ES		; make sure ES=DS

SkipLeading:
	CALL	B$$RCH		;get next char in AL
	JC	ERCRPE		;Brif no char got, give "read pass end"
	OR	DL,DL		;line input?
	JZ	Get1stChar	;Brif yes
	CMP	AL," "		;is blank ?
	JZ	SkipLeading	;skip it

Get1stChar:
	MOV	SI,OFFSET DGROUP:b$SDBuf1 ; initialze string to b$Buf1
	MOV	DI,OFFSET DGROUP:b$Buf1 
	MOV	CL,255		;allow max of 255 characters per buffer

	CMP	AL,'"'		;quoted string?
	JNZ	CharLoop	;Brif not
	CMP	DL,","		;looking for a string?
	JNZ	CharLoop	;Brif not
	cCall	StoreChar	;save '"' at start of string so FIN can match it
	MOV	DX,'""' 	;Set delimiters to '"'
	CALL	B$$RCH		;get next char
	JC	Quit		;Brif null string

CharLoop:
	CMP	DH,'"'		;getting a quoted string?
	JZ	NotCRLF 	;Brif yes, don't check LF CR

	CMP	AL,NEWLINE	;end of a line? (unquoted string or number)
	JZ	EndCR		;Brif yes

	CMP	AL,ASCLF	;check LF CR, the CR will be ignored
	JNZ	NotCRLF 	;Brif not
GotLF:				;have a continuation mark and not a quoted str
	CMP	DL,","		;getting an unquoted string?
	JZ	ChkLFCR 	;don't store linefeeds if unquoted strings
	cCall	StoreChar	;store LF for numbers
ChkLFCR:
	CALL	B$$RCH		;get next char
	JC	QUIT		; quit if eof.
	CMP	AL,ASCLF	;check for LF after LF
	JZ	GotLF		;Brif yes
	CMP	AL,NEWLINE	;ignore CR after LF if NOT LINE INPUT
	JNZ	NotCRLF 	;Brif not
	OR	DL,DL		;is LINE INPUT ?
	JNZ	NextChar	;Brif not (ignore CR)
NotCRLF:
	OR	AL,AL
	JZ	NextChar	;ignore nulls
	CMP	AL,DH		;check for first terminator
	JZ	Quit		;Brif yes
	CMP	AL,DL		;check for second terminator
	JZ	Quit		;Brif yes
	cCall	StoreChar	;store the character
NextChar:
	CALL	B$$RCH		;read next char
	JNC	CharLoop	;loop again
Quit:				;AL="," or '"' or " "
	CMP	AL,","		;must has more item
	JZ	PutZero
SkipEnding:			;skip ending blanks
	CALL	B$$RCH
	JC	PutZero
	CMP	AL," "
	JZ	SkipEnding
	CMP	AL,","		;has more item to go
	JZ	PutZero
	CMP	AL,NEWLINE	;ends ?
	JNZ	BackOneChar	;back up one char for next time
EndCR:
	MOV	BX,[b$PTRFIL]	;get file pointer (never 0000)
	MOV	AL,[BX].FD_DEVICE	; Get device type
	CMP	AL,DN_PIPE		; are we piping?
	JZ	ENDCR_PIPE		; Then just process like a file
	OR	AL,AL			; See if other special device
	JS	PutZero 	;do not look for LF if special device
ENDCR_PIPE:			
	CALL	B$$RCH
	JC	PutZero
	CMP	AL,ASCLF
	JZ	PutZero
BackOneChar:
	PUSH	SI		; Save SI
	MOV	SI,[b$PTRFIL]	; Get File pointer (not 0)
	MOV	AH,DV_BAKC	; Back up one char
	CALL	B$PtrDispatch	
	POP	SI		; Restore SI
PutZero:

	XOR	AL,AL
	STOSB			;terminated by 00H
	MOV	BX,OFFSET DGROUP:b$Buf1 
	CALL	B$STGETSTRLEN	;find length of string in input buffer
	INC	AX		;ensure room for terminating zero
	MOV	b$SDBuf1,AX	; make sd
	cCall	Concat		;concatenate buffer to string, if necessary
				;on return SI=*sd of concatenated string
cEnd				;exit to caller

	page
;***
;StoreChar -- store char in buffer
;
;Purpose:
;	Store character in AL into location ES:DI.  If the buffer is full,
;	then concatenate the buffer into a string, and reset the buffer for
;	next input.
;Entry:
;	[SI]		= *sd of result string
;	[AL]		= char
;	ES:DI		= buffer location
;	[CL]		= buffer count
;Exit:
;	if buffer is not full then
;		SI	= *sd of result string
;		character is stored in buffer pointed by ES:DI
;		DI is incremented by one (buffer pointer)
;		CL is decremented by one (buffer count)
;	else
;		SI	= *sd of concatenated string
;		DI	= *b$Buf1
;		CL	= 255
;	endif
;Uses:
;	none
;Preserves: (optional)
;	DX
;Exceptions:
;	none
;*******************************************************************************

cProc	StoreChar,<NEAR>

cBegin
	OR	AL,AL
	JZ	StoreExit	;no char, exit
	STOSB			;store in ES:DI, DI incremented
	DEC	CL		;Buffer full?
	JNZ	StoreExit	;no, continue
	MOV	b$SDBuf1,255	; yes, note buffer length
	PUSH	DX		; Save DX Flags (trashed in Concat + below)
	cCall	Concat		;concatenate buffer to string, if necessary
	JNZ	BufferReset	;already had a string, return concatenated one
				;first buffer overflow, need initial string
	MOV	BX,255		;string length
	MOV	DX,[SI+2]	;point to the string
	CALL	B$STALCTMPCPY	;copy contents of b$Buf1 into temp string
	MOV	SI,BX		;note string as start of input data
BufferReset:
	POP	DX		; Restore Flags
	MOV	DI,OFFSET DGROUP:b$Buf1 
	MOV	CL,255		;reset input to start of buffer
StoreExit:
cEnd				;exit to caller

	page
;***
;Concat -- concatenate the buffer into a string
;
;Purpose:
;	Concatenate the buffer b$Buf1, whose sd is b$SDBuf1, into the
;	current string pointed by SI, if the current one is not b$Buf1.
;Entry:
;	SI	= *sd of current string
;Exit:
;	ZR	if no concatenation happened (SI = addr of b$SDBuf1)
;	NZ & SI=*sd of concatenated string
;Uses:
;	none
;Exceptions:
;	none
;*******************************************************************************
DbPub	Concat				  
cProc	Concat,<NEAR>

cBegin
	MOV	BX,OFFSET DGROUP:b$SDBuf1	
	CMP	BX,SI		;is the current string the input buffer
	JZ	ConcatExit	;yes, just return it with ZR
	cCall	B$SCAT,<SI,BX> ; concatenate buffer to current string
	MOV	SI,AX		;SI has the concatenated string
ConcatExit:
cEnd				;exit to caller

	page
;***
;DeviceFin
;
;Purpose:
;	Device (disk) float or string input.
;
;	Allocating the string could make all temp string offsets changed.
;	Only string descriptor is still valid in that case.
;
;	For device (disk) input, if an input string is longer than 255
;	bytes, we use a temp string instead of the static buffer.  (Refer
;	to the above routine, B$FillBuf)  This caused problem when we use
;	B$FIN for disk input.
;
;	B$FIN takes the pointer of the given string (NOT *SD) and does
;	the string move using that string pointer after allocating a temp.
;	This will be fine if the pointer passed to B$FIN is a static one.
;	However, device (disk) input isn't the case if the length of input
;	string is longer than 255.
;
;	In this routine, it takes *SD instead of *string for a string input.
;
;Entry:
;	[b$VTYP]	= type of var
;	[SI]		= *sd of variable character string
;Exit:
;		AC has integer value ([b$VTYP] = VT_I2) (calls B$FIN)
;		AC has integer value ([b$VTYP] = VT_I4) (calls B$FIN)
;		AC has the address of string descriptor ([b$VTYP] = VT_SD)
;		AC has S.P. value ([b$VTYP] = VT_R4) (calls B$FIN)
;		DAC has D.P. value ([b$VTYP] = VT_R8) (calls B$FIN)
;		DAC has CY value ([b$VTYP] = VT_CY) (calls B$FIN)
;	endif
;Uses:
;	none
;Exceptions:
;	if error, runtime error handler is invoked, and this never returns
;*******************************************************************************

DbPub	DeviceFin		
cProc	DeviceFin,<NEAR>,<SI,DI,ES>
cBegin
	PUSH	SI		;save *sd one more copy for later use
	MOV	SI,[SI+2]	;B$FIN likes string offset...
	CMP	[b$VTYP],VT_SD	; test if string
	JZ	GetString	;jump to special case
	CALL	B$FIN		;process non-string value
	JMP	SHORT FinExit	;jump to finish up

GetString:
	PUSH	DS		
	POP	ES		; make ES=DS
	CALL	B$STRSCAN	; get string from buffer pointed by ES:SI
				; do all of $GETSTR except for temp string
				; creation, on return DX=*string, CX=length
	POP	DI		;get input string description (pushed as SI)
	PUSH	DI		;and put it back on stack
	MOV	AX,DX		;get offset of the string
	SUB	AX,[DI+2]	;skip count for leading white spaces
	MOV	BX,CX		;get length of new string
	CALL	B$STALCTMP	;allocate a temporary string

	MOV	DI,[BX+2]	;get new string pointer
	POP	SI		;get old descriptor again
	PUSH	SI		;still want it on stack
	MOV	SI,[SI+2]	;get pointer of the old string
	ADD	SI,AX		;get pointer of the old string without white
				; white spaces
	INC	CX		;roundup byte count
	SHR	CX,1		;make a word count for transfer
	REP	MOVSW		;move from old to new string
	MOV	[B$AC],BX	;put output string desc in BX
FinExit:
	POP	BX		;done - restore descriptor
				;this was pushed as SI
	CALL	B$STDALCTMP	;delete if temporary
cEnd				;pop registers and return to caller


sEnd	;DK_TEXT

	END
