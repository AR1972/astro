	TITLE	GWCOM - OEM-indepedent Communications Routines

	PAGE	56,132

;***
; GWCOM.ASM - OEM-indepedent Communications Routines
;
;	Copyright <C> 1986 - 1988, Microsoft Corporation
;
; Purpose:
;	To provide the GW I/O dispatch routines for the serial
;	communications ports through calls to the OEM routines.
;
;******************************************************************************

	INCLUDE switch.inc
	INCLUDE rmacros.inc	

	useSeg	_BSS		
	useSeg	_DATA		
	useSeg	CONST		
	useSeg	DV_TEXT 	

	INCLUDE seg.inc 	
	INCLUDE baslibma.inc
	INCLUDE files.inc
	INCLUDE devdef.inc
	INCLUDE ascii.inc	;must follow devdef.inc
	INCLUDE addr.inc	;usercode header structure
	INCLUDE comdcb.inc	;DCB structure for comm device
	INCLUDE rtps.inc	; constants shared with QBI
	INCLUDE idmac.inc	

;	The following macros generate the dispatch target labels used
;	by the the I/O system one-time initialization(I) and dispatch
;	dispatch table entry(D).

ComDspLabels	MACRO	Ctype
	Cindex	= 0		;;value to pass to item generation macro

	REPT	NUM_RS232	;;one item for each comm device defined
	Cindex	= Cindex + 1	;;update the counter
	ComItem	Ctype,%Cindex	;;define item with type (I,C,D) and counter
	ENDM

	ENDM	;;ComDspLabels

ComItem	MACRO	Ctype,Cindex
	PUBLIC	B$&Ctype&_COM&Cindex ;;define PUBLIC (e.g., PUBLIC B$I_COM2)
B$&Ctype&_COM&Cindex:		 ;;define label (e.g., B$I_COM2:)
	ENDM	;;ComItem

sBegin	CONST			

CMERRT	DW	OFFSET B$ERR_IOE ;Other IO Error
	DW	OFFSET B$ERR_CBO ;buffer overflow error
	DW	OFFSET B$ERR_IOE ;parity error
	DW	OFFSET B$ERR_DTO ;device timeout
	DW	OFFSET B$ERR_DTO ;device timeout
	DW	OFFSET B$ERR_DTO ;device timeout

sEnd	CONST			

sBegin	_BSS			

	externB	B$CM1DCB		; defined in GWDATA.ASM
	externB	b$FILMOD	; defined in GWINI.ASM
	externB	b$PATHNAM	; defined in GWINI.ASM
	externW b$ComPort	;LLPARAM - save area for COMx I/O ports
	externB b$COM_DCB	;DCB for COM1 device

externD b$RECPTR		; pointer to random record
externW b$cbComBuffer		;combuffer size specified on QB command line
	staticW COMSAV,,2	;save area for b$ComPort across SHELL
	globalB b$COFlag,0,1	; Flag to indicate COM OPEN in progress

sEnd	_BSS			


sBegin	DV_TEXT 		
assumes CS,DV_TEXT		

	externNP B$FHHighAlloc
	externNP B$FHHighDealloc



;   Device Independent Interface

DSPMAC	MACRO	func
	DW	COM_&func	;;Define Dispatch Table for Specific Device
	ENDM

;	Communications Dispatch Table - must not be at offset 0.

ComDspLabels	D		;define dispatch table labels

	DSPNAM			;define dispatch table

	SUBTTL	Communications Generalized I/O Routines

	externNP B$DEVOPN
	externNP B$LHDALC_CPCT	
	externNP B$INICOM	
	externNP B$RECCOM	
	externNP B$SNDCOM	
	externNP B$STACOM	
	externNP B$TRMCOM	
	externNP B$BREAK_CHK
	externNP B$ERR_BFN	;Error routines - Bad File Name
	externNP B$ERR_DNA	;device unavailable error
	externNP B$ERR_FAO	;   File Already Open
	externNP B$ERR_CBO	;   Com Buffer Overflow
	externNP B$ERR_DTO	;   Device Time Out
	externNP B$ERR_IOE	;   Device Input Output Error
	externNP B$ERR_FC	;   Bad Function Call
	externNP B$ERR_OM_FH	; out of memory error

;	$I_COMx - called during BASIC initialization

;	Entry - DI = -2*device id

ComDspLabels	I		;define initialization labels

	PUSH	AX		;save registers...
	PUSH	DI
	CALL	GetComDCB	;DI points to device control block
	MOV	[DI].CD_CMWID,0FFH ;initial width is infinite
	MOV	[DI].CD_CMPOS,0	;initial position is zero
	MOV	[DI].CD_CMFDB,0 ;mark this device as available for open
	POP	DI		;restore registers...
	POP	AX
	RET			;return to caller

;***
; COM_OPEN - open an asynchronous communications device
;
; Entry:
;	AL = device id (negative number from DN_COM1 to DN_COMn)
;	BX = file number (0..n)
;	CX = random record size if [b$FILMOD] = random
;		 (if [CX] = 0, use default record size)
;	DI = device offset (2=COMD, 4=SCRN, etc.)
;	b$FILMOD = file mode
;		MD_SQI = 1 - sequential input
;		MD_SQO = 2 - sequential output
;		MD_RND = 4 - random
;		MD_APP = 8 - append
;	b$PATHNAM = null-terminated com options string (including "COMn:")
; Exit:
;	SI = pointer to FDB allocated
;*****************************************************************************


; Syntax:	OPEN "COMn:[SPEED][,[PARITY][,[DATA][,[STOP][,nonpos]]]]"

; Positional parameters:
;
;	SPEED	baud rate in bits per second
;	PARITY  N(none), E(even), O(odd), M(mark), S(space)
;	DATA    5, 6, 7, or 8 bits per character
;	STOP	1, 1.5, or 2 stop bits
;		Default for slower than 110 baud and 6, 7, or 8 data bits is 2.
;		Default for slower than 110 baud 5 data bits is 1.5.
;		Default for 110 baud or faster is 1.
;
; Nonpositional parameters:
;
;	RS	Suppress RTS (Request To Send).
;	OP[n]	Open timeout in milliseconds.
;	CS[n]   Clear-to-send (CTS) timeout in milliseconds.
;	DS[n]   Data-set-ready (DSR) timeout in milliseconds.
;	CD[n]	Carrier-detect (CD) timeout in milliseconds.
;	RB[n]	Size of receive buffer in bytes.
;	TB[n]	Size of transmit buffer in bytes.
;	LF	Transmit a line-feed following a carriage-return.
;	ASC	Open COM file in ASCII mode  | none => open in
;	BIN	Open COM file in binary mode | IBM mode

DbPub	<COM_OPEN>		
COM_OPEN:
	MOV	AH,BL		;[AH]=file number
	PUSH	AX		;save file number, device id
	PUSH	CX		;save variable record len (if random)
	CALL	GetComDCB	;AX = 0, 1, ... for COM1, COM2, ...
				;DI points to Device Control Block
	CMP	[DI].CD_CMFDB,0 ;see if device is opened to another file
	JE	NERFNO		;No ERror, File Not Open
	JMP	B$ERR_FAO	;File Already Opened
NERFNO:
	MOV	[DI].CD_DEVID,AL ;set unit field of Device Control Block

	MOV	SI,OFFSET DGROUP:b$PATHNAM+5 ; point to options
	CALL	ParseOpt	;parse options (fill in DCB fields)

	CALL	B$ComAlloc	;[2]allocate the comm buffer and fill in DCB

	MOV	[DI].CD_CMPOS,0 ;reset column to 0
	MOV	BX,DI		;BX points to DCB
DbAssertRelB	<[b$COFlag]>,E,0,DV_TEXT,<b$COFlag non-zero in COM_OPEN> ;
	INC	[b$COFlag]	; non-zero indicates COM OPEN in progress
	CALL	B$INICOM 	;call machine dependent OPEN routine
				;destroys FLAGS, AX..DX
DbAssertRelB	<[b$COFlag]>,B,2,DV_TEXT,<bad b$COFlag value in COM_OPEN> ;
	MOV	[b$COFlag],0	; be sure this is CLEARed on exit
	OR	AH,AH		;check for error during OPEN
	JZ	COMOPEN_NOERR	;if no error, then jump

	CALL	B$ComDealloc	;[2]deallocate the comm buffers

	INC	AH		;see if B$INICOM didn't like options
	JZ	ERCIFN		;bad file name error if illegal mode
	INC	AH		;test if device unavailable
	JZ	ERCDNA		;if so, then process error
	SUB	AH,2		;get back error code other than -1 or -2
	CALL	CKCMER		;check for device error code (will not return)

COMOPEN_NOERR:			
	POP	CX		;[CX]=record length
	CMP	b$FILMOD,MD_RND ; random open?
	JNE	NOTRND		; brif not
	INC	CX		; LEN parameter specified?
	LOOP	OPENIT		; brif so -- use that value for length
	MOV	CX,REC_LENGTH	; use default record length
	SKIP	2		; JMP OPENIT
NOTRND:				
	XOR	CX,CX		; don't need any buffer space for seq files
OPENIT:				
	POP	AX		; [AL]=device id, AH = file number
	MOV	DL,[DI].CD_CMWID ; [DL]=width from Device Control Block
	MOV	BL,AH
	XOR	BH,BH		; [BX]=file number
	MOV	AH,MD_RNDCH+MD_RND+MD_APP+MD_SQO+MD_SQI ; allow all file
				; modes except BINARY
	CALL	B$DEVOPN 	;Initialize FDB, SI has addr of FDB
	JCXZ	NO_BUFFER	; brif no buffer allocated
	MOV	[SI].FD_VRECL,CX ; save buffer size in FDB
NO_BUFFER:			
	OR	[SI].FD_FLAGS,FL_NEOF ;not at eof
	MOV	[DI].CD_CMFDB,SI ;save FDB pointer in DCB
	MOV	[DI].CD_CMEVT,0 ;init event signal counter
	TEST	[DI].CD_CMFLG,CF_CMBIN
	JZ	CMOPNX		;branch if user wants ASCII mode
	OR	[SI].FD_FLAGS,FL_BIN ; indicate BINARY mode
CMOPNX:
	RET

ERCIFN:
	JMP	B$ERR_BFN	;Bad File Name
ERCDNA:
	JMP	B$ERR_DNA	;device unavailable

;***
; ParseOpt - parse communications option string
;
; Purpose:
;	[Rewritten as part of revision 6].
;	To parse the communication option string and set the
;	appropriate values in the device control block (DCB) for
;	the specified device.
; Inputs:
;	SI = pointer to start of option string.
;	DI = pointer to device control block (DCB).
; Outputs:
;	None.
; Modifies:
;	AX, BX, CX, DX.
; Exceptions:
;	B$ERR_BFN - bad file name error if option string syntax error.
;******************************************************************************

ParseOpt:

;	Parse the first field as a numeric field for the baud rate.

	MOV	[DI].CD_BAUDR,300 ;set the default baud rate
	CALL	GetOptVal	;get the numeric value in DX
	JC	ParseBaudDone	;if null field, then use default
	MOV	[DI].CD_BAUDR,DX ;set the baud rate to the value read
ParseBaudDone:

;	Parse the second field as one letter for the parity type.
;	None(N)=0, Odd(O)=1, Even(E)=2, Mark(M)=3, and Space(S)=4.

	MOV	[DI].CD_PARIT,2	;set the default parity as even
	CALL	GetOptChar	;read the next character in AL
	JC	ParseParDone	;if null field, then use default

	MOV	CX,5		;five parity type to test
	XOR	BX,BX		;initialize the table index
ParseParLoop:
	CMP	AL,CS:ParityTable[BX] ;test field against parity types
	JE	ParseParFound	;if found, then jump to set it
	INC	BX		;increment index to next table entry
	LOOP	ParseParLoop	;loop until last type is tested
ParseError:
	JMP	B$ERR_BFN	;jump to report error in option string

ParityTable	DB	"NOEMS"	;available parity types in order

ParseParFound:
	MOV	[DI].CD_PARIT,BL ;put index as the parity type

	CALL	GetOptChar	;read next character looking for terminator
	JNC	ParseError	;if not terminator, then error
ParseParDone:

;	Parse the third field as a numeric value for the data
;	length (valid range is 5 to 8).

	MOV	[DI].CD_BYTSZ,7 ;default data length is 7 bits
	CALL	GetOptVal	;get the numeric value in DX
	JC	ParseSizeDone	;if null, then jump to use default

	CMP	DX,5		;test lower bound of legal range
	JB	ParseError	;if less, then error
	CMP	DX,8		;test upper bound of legal range
	JA	ParseError	;if more, then error
	MOV	[DI].CD_BYTSZ,DL ;set the size field with the value read
ParseSizeDone:

;	Parse the fourth field as the number of stop bits.  Legal
;	values are 1, 1.5, and 2.  Values are read as characters,
;	not as a numeric value (no leading zeroes).  If no value
;	is given, the default value is:
;		1 stop bit    - 0 - if over 110 baud
;		1.5 stop bits - 1 - if 110 or less baud and 5 bit data length
;		2 stop bits   - 2 - if 110 or less baud and 6-8 bit data length

	XOR	DX,DX		;assume 1 stop bit
	CMP	[DI].CD_BAUDR,110 ;test computed baud rate
	JA	ParseStopDef	;if over 110 baud, jump to set default
	INC	DX		;now assume 1.5 stop bits
	CMP	[DI].CD_BYTSZ,5	;test if 5 bit data length
	JE	ParseStopDef	;if 5 bit data, set default to 1.5 stop bits
	INC	DX		;6-8 bit data, set default to 2 stop bits
ParseStopDef:

	CALL	GetOptChar	;get the first character in field
	JC	ParseStopSet	;if null, jump to use default value in DL
	MOV	DL,AL		;move the character read
	SUB	DL,"1"		;map "1" to 0 and "2" to 1
	CMP	DL,1		;test for valid range (0 to 1)
	JA	ParseError	;if not in range, then error
	SHL	DL,1		;shift so values are 0(1 bit) and 2(2 bits)
	JNZ	ParseStopTerm	;if 2 bits, then jump to test for termination
	CALL	GetOptChar	;get the next char - either "." or terminator
	JC	ParseStopSet	;if terminator, then done - jump
	MOV	AH,AL		;move character for double test
	CALL	GetOptChar	;get next character - should be "5"

	CMP	AX,"5." 	;*WRONG* test if last two characters of "1.5"
	JNE	ParseError	;if not, then syntax error
	INC	DL		;make value 1 for 1.5 stop bits
ParseStopTerm:
	CALL	GetOptChar	;get the terminator character
	JNC	ParseError	;if not, then syntax error
ParseStopSet:
	MOV	[DI].CD_STOPB,DL ;set the stop bit value

;	The fifth and successive fields contain the order-independent
;	parameters, which are identified by their leading characters.
;	These parameters are:
;		RS  - turn off request-to-send
;		OPn - set open timeout to n milliseconds
;		DSn - set data-set-ready timeout to n milliseconds
;		CSn - set clear-to-send timeout to n milliseconds
;		CDn - set carrier-detect timeout to n milliseconds
;		RBn - set size of receive buffer in bytes
;		TBn - set size of transmit buffer in bytes
;		LF  - send line-feed after carriage-return at line end
;		BIN - send data without conversion
;		ASC - send data with conversion
;		PE  - enable reporting of parity errors on received data

	XOR	AX,AX		;clear register for setting values
	MOV	[DI].CD_CMFLG,CF_CMBIN ;clear all flags except for BIN
	MOV	[DI].CD_RLSTO,AX ;set carrier-detect timeout to zero
	MOV	[DI].CD_CTSTO,1000 ;set clear-to-send timeout to 1 second
	MOV	[DI].CD_DSRTO,1000 ;set data-set-ready timeout to 1 second
	MOV	[DI].CD_RXSIZ,AX ;clear receive buffer size for default
	MOV	[DI].CD_TXSIZ,AX ;clear transmit buffer size for default

;	The first character of each parameter is read.  If a null field,
;	it is ignored.  If at the option string end, then return.

ParseLoop:
	CALL	GetOptChar	;read the first character in AL
	JNC	ParseNext	;if not a separator, then start processing
	JNZ	ParseLoop	;if comma, then just ignore it
	JMP	ParseEnd	;else process end of statement

;	Test for valid prefix on parameter.  To reduce code size, read the
;	second character and use both for testing.

ParseNext:
	MOV	AH,AL		;move first character
	CALL	GetOptChar	;first character in AH, second in AL

;	First, test for "RS".  If so, then set RS flag and set default
;	CSn value to zero.

	CMP	AX,"RS"		;test if RS
	JNE	ParseOP		;if not, try OP next
	OR	[DI].CD_CMFLG,CF_CMRTS ;set the RS flag in the DCB
	TEST	[DI].CD_CMFLG,CF_CMCTS ;test if CS value is still default
	JNZ	ParseTerm	;test for termination after parameter
	MOV	[DI].CD_CTSTO,0	;set the CS timeout value to 0 (disable)

;	For parameters that do not have a numeric argument that is
;	processed with GetOptVal, the character after the parameter
;	must be a terminator (comma or end-of-string).

ParseTerm:
	CALL	GetOptChar	;get what should be terminator
	JC	ParseLoop	;if so, then loop for the next parameter
ParseTermErr:
	JMP	B$ERR_BFN	;jump to report error in option string

;	Second, test for "OP".  If so, then read the following numeric
;	value and put it in the DCB and set the flag.

ParseOP:
	CMP	AX,"OP"		;test for OP
	JNE	ParseDS		;if not, then test for DS
	CALL	GetOptVal	;get numeric value in DX
	JNC	ParseOPNoDef	;if numeric value specified, then jump
	MOV	DX,10000D	;otherwise set to 10 seconds
ParseOPNoDef:
	MOV	[DI].CD_OPNTO,DX ;put value in DCB
	OR	[DI].CD_CMFLG,CF_CMOPN ;set flag for OP parameter given
	JMP	SHORT ParseLoop	;jump to process next parameter

;	Third, test for "DS".  If so, then read the following numeric
;	value and put it in the DCB.

ParseDS:
	CMP	AX,"DS"		;test for DS
	JNE	ParseCD		;if not, then test for CD
	CALL	GetOptVal	;get numeric value in DX
	MOV	[DI].CD_DSRTO,DX ;put value in DCB
	JMP	SHORT ParseLoop	;jump to process next parameter

;	Fourth, test for "CD".  If so, then read the following numeric
;	value and put it in the DCB.

ParseCD:
	CMP	AX,"CD"		;test for CD
	JNE	ParseCS		;if not, then test for CS
	CALL	GetOptVal	;get numeric value in DX
	MOV	[DI].CD_RLSTO,DX ;put value in DCB
	JMP	SHORT ParseLoop	;jump to process next parameter

;	Fifth, test for "CS".  If so, then read the following numeric
;	value and put it in the DCB.
	
ParseCS:
	CMP	AX,"CS"		;test for CS
	JNE	ParseRB		;if not, then test for RB
	CALL	GetOptVal	;get numeric value in DX
	MOV	[DI].CD_CTSTO,DX ;put value in DCB
	OR	[DI].CD_CMFLG,CF_CMCTS ;set flag for nondefault CS timeout
	JMP	SHORT ParseLoop	;jump to process next parameter

;	Sixth, test for "RB".  If so, then read the following numeric
;	value and put it in the DCB.

ParseRB:
	CMP	AX,"RB"		;test for RB
	JNE	ParseTB		;if not, then test for TB
	CALL	GetOptVal	;get numeric value in DX
	MOV	[DI].CD_RXSIZ,DX ;put value in DCB
	JMP	SHORT ParseLoop	;jump to process next parameter

;	Seventh, test for "TB".  If so, then read the following numeric
;	value and put it in the DCB.

ParseTB:
	CMP	AX,"TB"		;test for TB
	JNE	ParseLF		;if not, then test for LF
	CALL	GetOptVal	;get numeric value in DX
	MOV	[DI].CD_TXSIZ,DX ;put value in DCB
	JMP	ParseLoop	;jump to process next parameter

;	Eighth, test for "LF".  If so, then set the flag in the DCB.

ParseLF:
	CMP	AX,"LF"		;test for LF
	JNE	ParseBIN	;if not, then test for BIN
	OR	[DI].CD_CMFLG,CF_CMCLF ;set the LF flag
	JMP	SHORT ParseTerm	;jump to check for terminator

;	Ninth, test for "BIN".  If so, then set the flag in the DCB.

ParseBIN:
	CMP	AX,"BI"		;test for BI
	JNE	ParseASC	;if not, then test for ASC
	CALL	GetOptChar	;get the third character
	CMP	AL,"N"		;test if parameter was BIN
	JNE	ParseTermErr	;if not, then error
ParseCOD:
	TEST	[DI].CD_CMFLG,CF_CMCOD ;test if ASC or BIN specified before
	JNZ	ParseTermErr	;if so, then error
	OR	[DI].CD_CMFLG,CF_CMCOD ;set the flag for ASC or BIN specified
	JMP	ParseTerm	;jump to test for terminator

;	Tenth, test for "ASC".  If so, then clear the flag in the DCB.

ParseASC:
	CMP	AX,"AS"		;test for AS
	JNE	ParsePE		;if not, then test for PE
	CALL	GetOptChar	;get the third character
	CMP	AL,"C"		;test if parameter was ASC
	JNE	ParseTermErrJmp	;if not, then error
	AND	[DI].CD_CMFLG,NOT CF_CMBIN ;clear the binary mode flag
	JMP	SHORT ParseCOD	;jump to check CF_CMCOD in DCB

ParseTermErrJmp:
	JMP	ParseTermErr	;short jump to process error

;	Finally, test for "PE".  If so, then set the flag in the DCB.

ParsePE:
	CMP	AX,"PE"		;test for PE
	JNE	ParseTermErrJmp	;if not, then unknown parameter - error
	OR	[DI].CD_CMFLG,CF_CMPEN ;set the flag PE specified
	JMP	ParseTerm	;jump to test for terminator

;	The end of the statement has been reached.  If not specified,
;	compute the open timeout value to be ten times the maximum of the
;	other timeouts, with a maximum value of 64K-1 milliseconds.

ParseEnd:			
	TEST	[DI].CD_CMFLG,CF_CMOPN ;test if nondefault OP timeout
	JNZ	ParseRet	;if so, then use the one specified
	MOV	AX,[DI].CD_RLSTO ;get the CD timeout value
	CMP	AX,[DI].CD_DSRTO ;test against the DS timeout value
	JA	ParseKeep1	;jump to keep greater value in AX
	MOV	AX,[DI].CD_DSRTO ;else update the maximum value
ParseKeep1:			
	MOV	CX,0FFFFH	;assume maximum value
	MOV	DX,10		;factor to multiply maximum time
	MUL	DX		;compute OP timeout in DX:AX
	JC	ParseSetOP	;if over 64K-1, then jump to set max
	MOV	CX,AX		;set the product
ParseSetOP:			
	MOV	[DI].CD_OPNTO,CX ;set the OP timeout value
ParseRet:			
	RET			;if statement end, then finished, so return


;***
; GetOptChar - get a character in the option string
;
; Purpose:
;	[Rewritten as part of revision 6].
;	The next character in the option string is read into AL.
;	If lower case, it is mapped to upper case and the carry and
;	zero flags are set accordingly.  Blank and tab characters
;	are ignored.
; Input:
;	SI = pointer to next character in the option string.
; Outputs:
;	SI = pointer to the following character (except for string end).
;	AL = character (mapped if lower case).
;	CF set and ZF set = end of string
;	CF set and ZF clear = comma character
;	CF clear and ZF clear = not comma or end of string
; Modifies:
;	None.
; Exceptions:
;	None.
;*****************************************************************************

GetOptChar:
	LODSB			;get the next character from the string
	CMP	AL," "		;test if blank character
	JE	GetOptChar	;if so, then ignore it
	CMP	AL,ASCHT	;test if tab character
	JE	GetOptChar	;if so, then also ignore it
	CMP	AL,","		;test if comma
	JE	GetOptStc	;if so, then return with CF set and ZF clear
	CMP	AL,"a"		;test if character is lower case
	JB	GetOptNoMap	;if not, then skip the mapping
	ADD	AL,"A"-"a"	;map lower case letter to upper
GetOptNoMap:
	OR	AL,AL		;test if at the end of the string
	JNZ	GetOptRet	;if not end, then return with CF and ZF clear
	DEC	SI		;backup pointer to the statement end
	OR	AL,AL		;clear ZF again for exit condition
GetOptStc:
	STC			;set carry for end-of-statement or comma
GetOptRet:
	RET			;near return to caller

;***
; GetOptVal - get value from option string
;
; Purpose:
;	[Rewritten as part of revision 6].
;	To read the option string for characters to be interpreted
;	as decimal digits.  A value from these digits is computed
;	and returned.  This routine then checks for a proper terminating
;	character (either a comma or end-of-string) after the value.
;	If no terminator, then an error is reported.
; Inputs:
;	SI = pointer to option string at value.
; Outputs:
;	SI = pointer to option string after the terminator.
;	DX = computed value.
;	CY set = null value (first character terminator).
;	   clear = nonnull value.
; Modifies:
;	AX.
; Exceptions:
;	B$ERR_BFN - bad file name if improper terminator.
;*****************************************************************************

GetOptVal:
	PUSH	BX		;save register
	XOR	DX,DX		;initialize result
	CALL	GetOptChar	;get the first character of the value
	JC	GetOptValRet	;return with CY set for null value
	DEC	SI		;back up over first character read
GetOptValLoop:
	CALL	GetOptChar	;get option string character in AL
	CMC			;complement so CY clear for terminator
	JNC	GetOptValRet	;return with CY clear for nonull value
	SUB	AL,"0"		;subtract bias for decimal digit
	CMP	AL,9		;test if digit or not
	JA	GetOptValErr	;if not digit or terminator, then error
	CBW			;zero AH for word value
	SHL	DX,1		;present result * 2
	JC	GetOptValErr	;jump if overflow error
	MOV	BX,DX		;save result * 2
	SHL	DX,1		;present result * 4
	JC	GetOptValErr	;jump if overflow error
	SHL	DX,1		;present result * 8
	JC	GetOptValErr	;jump if overflow error
	ADD	DX,BX		;present result * 10
	JC	GetOptValErr	;jump if overflow error
	ADD	DX,AX		;add in new digit - new value
	JNC	GetOptValLoop	;if no overflow, then loop for next
GetOptValErr:
	JMP	B$ERR_BFN	;if error, then bad file name
GetOptValRet:
	POP	BX		;restore register
	RET			;near return to caller

;
;Written as part of revision [6]
;
;***
;B$ComAlloc - Allocate memory for a communications device
;
;Purpose:
;	From high memory, allocate two blocks for use as communications
;	queues, one for the receive queue and the other for the transmit
;	queue.	The size of the two queues is passed into this procedure
;	as an arguement.
;
;	If the size specified for the receive queue is 0, then the size
;	given on the command line with the /C:xxx option is used.  If this
;	value is -1 (not specified), then the queue will be cbComBuf bytes
;	long.  If the size specified for the transmit queue is 0, then
;	it will be cbComBuf bytes long.
;
;	On exit, the DCB of the communications device that was passed
;	in as a parameter will have the segment which contains the
;	receive queue and the transmit queue.  The queues will be the
;	first (and only) things in the segments.  The fields of the
;	DCB are defined in comdcb.inc.
;
;	For OS/2, this routine will also allocate a 2K-byte block for the
;	stack of the thread.
;
;Entry:
;	DI = communications device control block (DCB)
;	[DI].CD_RXSIZ = size of the receive queue in bytes
;	[DI].CD_TXSIZ = size of the transmit queue in bytes
;
;Exit:
;	[DI].CD_RXSEG = segment of memory allocated for the receive queue
;	[DI].CD_TXSEG = segment of memory allocated for the transmit queue
;	[DI].CD_SPSIZ = size of the thread stack in bytes (2048+slop)
;	[DI].CD_SPSEG = segment of memory allocated for the thread stack
;
;Uses:
;	Per convention.
;
;Exceptions:
;	B$ERR_DNA - device not available if /C:0 for compatibility
;		     This error will be given if the option's value is
;		     needed or not.
;	B$ERR_OM  - out of memory if allocation failed for any reason.
;****************************************************************************

	PUBLIC	B$ComAlloc	

B$ComAlloc PROC NEAR		

;	Get the C parameter value for comm buffer size.

	MOV	AX,b$cbComBuffer ;get /C:nnnn value saved from command line

;	If buffer size is zero, then error.

	OR	AX,AX		;test if zero buffer size
	JZ	ComAllocDNAErr	;if so, then give "device not available" error

;	Compute the size of the receive queue.  If [DI].CD_RXSIZ is
;	nonzero, use the size specified in the RB[n] parameter.

	MOV	DX,[DI].CD_RXSIZ ;get the receive buffer size
	OR	DX,DX		;test if specified
	JNZ	ComAllocRB	;if so, then use it

;	Use size in AX from command line, except for -1 where the
;	default is cbComBuf bytes.

	MOV	DX,cbComBuf	;default size for receive buffer
	CMP	AX,-1		;test if -1 for default
	JE	ComAllocRBSet	;if default, use cbComBuf bytes
	MOV	DX,AX		;move size from command line for use
ComAllocRBSet:
	MOV	[DI].CD_RXSIZ,DX ;set the block size

;	Allocate the receive queue and set buffer segment in DCB.

ComAllocRB:
	MOV	AX,DX		;move the size into AX
	XOR	DX,DX		;size in bytes now in DX:AX
	CALL	B$FHHighAlloc	;allocate the comm buffer with segment in CX
	JCXZ	ComAllocOMErrR	;out of memory if couldn't allocate buffer
	MOV	[DI].CD_RXSEG,CX ;[8]set the DCB segment

;	Compute the size of the transmit queue.  If [DI].CD_TXSIZ is
;	nonzero, use the size specified in the TB[n] parameter.

	MOV	AX,[DI].CD_TXSIZ ;get the transmit buffer size
	OR	AX,AX		;test if specified
	JNZ	ComAllocTB	;if so, then use it

;	If not specified, use the size of cbComBuf bytes.

	MOV	AX,cbComBuf	 ;default size for transmit buffer
	MOV	[DI].CD_TXSIZ,AX ;set the default size of the buffer

;	Allocate the transmit queue and set buffer segment in DCB.

ComAllocTB:
	XOR	DX,DX		;size in bytes now in DX:AX
	CALL	B$FHHighAlloc	;allocate the comm buffer with segment in CX
	JCXZ	ComAllocOMErrT	;out of memory allocation failed
	MOV	[DI].CD_TXSEG,CX ;[8]set the DCB segment

;	Allocate the stack for the thread and set segment in DCB.


	RET			;near return to caller

ComAllocDNAErr:
	JMP	B$ERR_DNA	;give DNA error for /C:0 for interpreter compat


ComAllocOMErrT:
	MOV	AX,[DI].CD_RXSEG ;get segement allocated for the receive queue
	CALL	B$FHHighDealloc ;and deallocate it before giving error
ComAllocOMErrR:			
	JMP	B$ERR_OM_FH	;give OM error if couldn't allocate buffer

B$ComAlloc ENDP		

;***
;B$ComDealloc - Deallocate memory for a communications queue
;
;Purpose:
;	Deallocate the blocks of memory used as a communications queue.
;	The queues to be deallocated are specified by passing the DBC of
;	the communications device.  This device must have queues
;	allocated to it (with B$ComAlloc) otherwise there is a possibility
;	of corrupting memory.
;
;	For OS/2, this routine will also deallocate the stack segment for
;	the thread.
;
;Entry:
;	DI = communications device control block (DCB)
;
;Exit:
;	None.
;
;Uses:
;	Per Convention.
;
;Preserves:
;	AX, BX, CX, DX
;
;Exceptions:
;	May generate B$ERR_MEM or B$ERR_FHC if memory has been
;	corrupted or the segment descriptors in the FDB have been
;	modified.
;******************************************************************************

	PUBLIC	B$ComDealloc	

B$ComDealloc PROC NEAR		

;	Deallocate the block defined in the comm DCB.

	PUSH	AX		;save the register


	MOV	AX,[DI].CD_RXSEG ;get the segment of the block
	CALL	B$FHHighDealloc ;deallocate the segment
	MOV	AX,[DI].CD_TXSEG ;get the segment of the block
	CALL	B$FHHighDealloc ;deallocate the segment


	POP	AX		;restore the register
	RET			;near return to caller

B$ComDealloc ENDP		

;***
; B$ComPreChain - prepare for DOS 5 chaining
; Purpose:
;	Added as part of revision [16].
;	Since DOS 5 CHAIN (with RTM) starts a new process, the current
;	emptier/filler threads must be terminated to be restarted later
;	in the new process itself.  Also, since the receive and transmit
;	buffers are in far memory, new selectors must be mapped using
;	DOSGIVESEG and provided to the new process.
;
; Inputs:
;	CX = pid of the new process
; Outputs:
;	Com buffer selectors are mapped.
; Modifies:
;	None.
; Exceptions:
;	B$ERR_OM - if mapping if selectors fail.
;******************************************************************************


;***
; B$ComPostChain - finish up after DOS 5 chaining
;
; Purpose:
;	Added as part of revision [16].
;	After the chaining is complete, reenable the thread for each
;	active device.
; Inputs:
;	None.
; Outputs:
;	None.
; Modifies:
;	None.
; Exceptions:
;	None.
;******************************************************************************


	SUBTTL	-  EOF, LOC, LOF etc (std file functions)

;COM_EOF - test for End-Of-File on device.
; Entry - SI points to File-Data-Block.
;	  [DI] = device offset (2=COMD, 4=SCRN, etc.)
; Exit	- [AX] = -1 if EOF, 0 if not EOF [1]

; This EOF function will hang waiting for input, but
; presumably, user wants device to look like sequential file, not
; a dynamic COM-I/O device if he is using EOF function.
; EOF(binary_file) is always false.

COM_EOF:
	TEST	[SI].FD_FLAGS,FL_BIN ; binary file?
	JNZ	bineof		;branch if not ascii file
	XOR	BX,BX		;assume no EOF
	CALL	INCHSI		;[AL]=next byte from COM device
	JB	YCMEOF		;branch if next char = EOF
	CALL	COM_BAKC	;put this back in queue
	INC	BX		;BX=0,	end-of-file is false
YCMEOF: DEC	BX		;BX=-1, end-of-file is true
	MOV	AX,BX		; result in AX
	RET

;   In BINARY mode, GW-BASIC EOF is compatible with IBM-PC Basic
;   That is, EOF is true when no data is in input queue.

BINEOF:
	push	dx		;5.40
	push	ax
	call	GetComUnitID	;AX = CD_DEVID in DCB
	mov	ah,al		;AH = CD_DEVID in DCB
	CALL	B$STACOM 	;[DX]=number of bytes in input queue
	call	ckcmer		; check for com error
	mov	bx,dx
	pop	ax
	pop	dx		;5.40
	OR	BX,BX
	JZ	YCMEOF		;branch if input queue is empty
	XOR	AX,AX		; return with [AX]=0 (false)
	RET			; AX=-1 if end-of-file is true

;INCHSI - get next byte from file SI (CTL Z = end-of-file)
; Exit	- Carry set if EOF, else [AL]=byte.
;	  All other regs preserved

INCHSI:
	TEST	[SI].FD_FLAGS,FL_NEOF
	JZ	INCEOF		;branch if EOF already reached
	TEST	[SI].FD_FLAGS,FL_BKC
	JNZ	GETBKC		;branch if char backed up
	CALL	COM_SINP	;[AL]=next input from file
	JB	INCEOF		;branch if device detected EOF
	RET

GETBKC: AND	[SI].FD_FLAGS,NOT FL_BKC ;Flag char as no longer backed up
	MOV	AL,[SI].FD_BAKC	;Get backup char
	CLC			;Make sure EOF isnt set
	RET

INCEOF: AND	[SI].FD_FLAGS,NOT FL_BKC ;Tell the world no char backed up
	STC			;Set carry to indicate EOF
	RET

;COM_BAKC - backup sequential input file
;	    original name: BCHRSI
;   Entry - [AL] = char to be backed up
;	    [SI] points to FDB of file to be backed up

COM_BAKC:
	MOV	[SI].FD_BAKC,AL
	OR	[SI].FD_FLAGS,FL_BKC ;set flag indicating char backed up
	RET

;B$COMLOC -- entry point to B$STACOM for $polcom in gwaevt
;entry: same as B$STACOM
;exit:	same as B$STACOM

	PUBLIC	B$COMLOC
B$COMLOC:
	JMP	B$STACOM	

;COM_LOC - Number of Bytes in input buffer for device.
; Entry - SI points to File-Data-Block.
;	  DI = device offset
; Exit	- [DX|AX] has I4 result
;	  uses DI

COM_LOC:
	CALL	GetComUnitID	;AL=Unit ID
	MOV	AH,AL
	CALL	B$STACOM 	;[DX]=number of bytes in input buffer
	CALL	CKCMER		;Check for Com Error
	TEST	[SI].FD_FLAGS,FL_BKC
	JZ	CMLOCX		;branch if char not backed up
	INC	DX
CMLOCX:
	MOV	AX,DX		; result in [DX|AX]
	JMP	SHORT DBLEX

;COM_LOF - number of bytes free in OUTPUT buffer.
; Entry - SI points to File-Data-Block.
;	  [DI] = device offset (2=COMD, 4=SCRN, etc.)
; Exit	- [DX|AX] has I4 result
;	  uses DI
COM_LOF:
	CALL	GetComUnitID	;AL=Unit ID
	MOV	AH,AL
	CALL	B$STACOM 	;Result in CX
	CALL	CKCMER		;Check for Com error
	MOV	AX,CX		; Put result in DX, now Zero the Regs:
DBLEX:
	XOR	DX,DX		; DX=0
	RET			;return result in FAC

;COM_CLOSE - perform any device dependent close functions.
; Entry - SI points to File-Data-Block.
;	  [DI] = device offset (2=COMD, 4=SCRN, etc.)
; Exit	- All registers preserved.
;	  This routine releases the fdata-block associated with this file.

DbPub	<COM_CLOSE>		
COM_CLOSE:
DbAssertRelB	<[b$COFlag]>,E,0,DV_TEXT,<b$COFlag non-zero in COM_CLOSE> ;
	CALL	GetComDCB	;DI points to device control block
	TEST	[SI].FD_FLAGS,FL_BIN ; binary file?
	JNZ	COMCLX		; Branch if BINARY file mode
	CMP	[SI].FD_MODE,MD_SQI
	JE	COMCLX		;don't send EOF if input mode
	MOV	AL,EOFCHR	;else send CTL-Z indicating EOF
	CALL	CMROUT
COMCLX:
	MOV	AH,[DI].CD_DEVID ;get device id
	PUSH	CX		; must preserve CX for dispatch count
	XOR	CX,CX		; CX = 0 to RESET DTR and RTS (DOS 3 only)
	CALL	B$TRMCOM 	;close device
	POP	CX		; Restore CX = dispatch count
	CALL	B$ComDealloc	;[2]deallocate the comm buffer
	MOV	[DI].CD_CMFDB,0 ;mark device as not-in-use
	CALL	B$LHDALC_CPCT	; Deallocate FDB and compact local heap
	JMP	CKCMER		; check for B$TRMCOM error and return


;COM_WIDTH - set file width
; Entry - SI points to File-Data-Block.
;	  [DX] = new device width
;	  [DI] = device offset (2=COMD, 4=SCRN, etc.)
; Exit	- All registers preserved

COM_WIDTH:
	MOV	[SI].FD_WIDTH,DL ;set DCB value to be effective now
	RET


;*** 
;COM_DWID - Set device width.  Added with [20].
;
;Purpose:
;
;Entry:
;	[DL] = new device width
;	[DI] = device offset
;Exit:
;	None
;Uses:
;	Per convention
;Exceptions:
;	None
;
;******************************************************************************
cProc	COM_DWID,<NEAR>,<DI>
cBegin
	CALL	GetComDCB	; get DCB in DI
	MOV	[DI].CD_CMWID,DL ; update DCB value
cEnd


;***
;COM_RANDIO -- low level routine to access comm for GET/PUT
;
;Purpose:
; This routine reads from/writes to a comm port a record.
;
;Entry:
;	[AL]		= flags
;			  Bit 0: 1 if PUT, else GET
;			  BIT 1: 1 if explicit record number specified
;			  BIT 2: 1 if record variable specified
;	[BX]		= record length to be written
;	[CX|DX] 	= user specified record number if GET/PUT (This number
;			  is used to override the length to be read/written)
;	[DI]		= Device Offset
;	[SI]		= *FDB
;	[b$RECPTR]	= Pointer to the data to be written
;
;Exit:
;	[AX]		= number of bytes actual read/write
;	one buffer is filled/flushed
;Uses:
;	Per convention
;
;Exceptions:
;	B$ERR_FC  -- illegal function call
;
;*******************************************************************************
cProc	COM_RANDIO,NEAR,ES	
cBegin				
	TEST	AL,RelFlg	; See if record specified
	JNZ	CRAND_5 	; Jump if record specified
	MOV	DX,BX		; [DX] = length to be read/written
	JMP	SHORT CRAND_20	; Go perform operation

CRAND_5:			; Record specified
	JCXZ	CRAND_15	; Jump if rec num < 64k (valid)
ERRFC:				
	JMP	B$ERR_FC	; Record number too big
CRAND_15:			
	CMP	BX,DX		; Record number must be less than buffer
	JC	ERRFC		; Jump if record number exceeds buff size

; [BX] = size of the buffer (size to clear for GET)
; [DX] = number of characters to actually deal with

CRAND_20:			
	MOV	CX,BX		; [CX] = size of buffer
	LES	BX,[b$RECPTR]	; [ES:BX] = pointer to buffer
	TEST	AL,PutFlg	; See if put or get
	JNZ	CRAND_30	; Jump if PUT

	PUSH	DI		; Save dispatch offset
	MOV	DI,BX		; [ES:DI] = pointer to buffer
	MOV	AL," "		; [AL] = a space to place
	REP	STOSB		; Fill buffer
	POP	DI		; [DI] = dispatch offset

	MOV	CX,DX		; [CX] = number of characters to get
CRAND_25:			
	PUSH	DI		; Save dispatch offset
	CALL	COM_SINP	; [AL] = next byte from comm port
	POP	DI		; [DI] = dispatch offset
	MOV	ES:[BX],AL	; Place into buffer
	INC	BX		; Move on
	LOOP	CRAND_25	; For all characters desired
	JMP	SHORT CRAND_90	; exit

CRAND_30:			; Put
	MOV	CX,DX		;get number of characters to output
	CALL	GetComDCB	; [DI] = DCB pointer
CRAND_35:			
	MOV	AL,ES:[BX]	; [AL]=next byte from buffer
	CALL	CMROUT		; output to com port
	INC	BX		; bump buffer pointer
	LOOP	CRAND_35	; For all characters desired

CRAND_90:			
	XCHG	AX,DX		; return count in AX

cEnd


;COM_SINP - Sequential Input.
; Entry - SI points to File-Data-Block.
;	  [DI] = device offset (2=COMD, 4=SCRN, etc.)
; Exit	- [AL] = next byte from file,
;	  carry set if EOF.
;	  All other registers preserved

COM_SINP:
	TEST	[SI].FD_FLAGS,FL_BKC ;Is a char backed up?
	JZ	CSWAIT		;No - go get one
	MOV	AL,[SI].FD_BAKC	;Yes get ch from backup
	AND	[SI].FD_FLAGS,NOT FL_BKC ;char is no longer backed up
	JMP	SHORT CMNEOF	;indicate no eof and return

CSWAIT:
	CALL	GetComUnitID	;[AL]=Com Unit ID(DI)
	MOV	AH,AL		;[AH]=Com Unit ID(DI)
	CALL	B$RECCOM 	;[AL]=input byte (if data is ready)
	PUSHF
	CALL	CKCMER		;check for Com Error
	POPF
	JNZ	CSGOT1		;wait if none ready to be read
	CALL	B$BREAK_CHK	;allow ^Break to break in
	JMP	SHORT CSWAIT
CSGOT1:
	PUSH	AX		;save registers...
	PUSH	DI
	CALL	GetComDCB	;get DCB in DI
	DEC	[DI].CD_CMEVT	;one less char in input queue
	POP	DI		;restore registers
	POP	AX
	CMP	AL,EOFCHR 	;check for CTL-Z
	JNE	CMNEOF		;branch if not
	TEST	[SI].FD_FLAGS,FL_BIN ; binary file?
	JNZ	CMNEOF		;CTL-Z is not EOF for Binary files
	STC			;CTL-z is EOF for ASCII files
	AND	[SI].FD_FLAGS,NOT FL_NEOF ;flag as eof
	JMP	SHORT CMSINX	;restore registers and exit
CMNEOF:
	CLC			;clear carry (no eof)
CMSINX:
	RET

;COM_SOUT - Sequential Output.
; Entry - SI points to File-Data-Block.
;	  [DI] = device offset (2=COMD, 4=SCRN, etc.)
;	  [AL] = byte to be output.
; Exit	- SI, DI can be changed.
;	  All other registers preserved
;	  This routine keeps track of column position,
;	  expands tabs, and forces a carriage return when line width
;	  is exceeded.


COM_SOUT:
	PUSH	AX
	CALL	GetComDCB	;DI points to COMx DCB
	POP	AX
	TEST	[DI].CD_CMFLG,CF_CMCOD ;did user explicity specifiy BIN/ASC?
	JZ	NOTBIN		;No
	TEST	[SI].FD_FLAGS,FL_BIN
	JNZ	CMROUT		;if binary, branch to Raw-Output routine
NOTBIN:
	PUSH	DX		;NOT binary
	MOV	DL,[SI].FD_WIDTH ;[DL]=device width (from FDB)
	MOV	DH,[DI].CD_CMPOS ;[DH]=current column (from DCB)
	CALL	XTABCR		;Expand tabs, force CR, do output, etc.
	MOV	[DI].CD_CMPOS,DH ;save new column position
	POP	DX
RET11:
	RET


;CLRCRF - Clear <CR> flag and output character
;CMROUT - Raw output routine
; Exit	- All registers preserved

CLRCRF:
	AND	[DI].CD_CMFLG,NOT CF_CMCRF ;clear CR flag

CMROUT: 			;output character/raw output routine
	MOV	AH,[DI].CD_DEVID ;[AH]=device ID
	CALL	B$SNDCOM 	;output [AL] to COM
	JMP	CKCMER		;check for successful transmit and rtn

;XTABCR     expand tabs,force <CR> if EOL
;entry->    [AL] = Char to be output
;	    [DX] = [current column,file width]
;	    [DI] = device offset
;exit ->    [DH] = new column
;	    all other regesters preserved


XTABCR:
	INC	DH		; send character and then check
				;if beyond width specified
	CMP	AL,ASCSP 	;Is it printable
	JB	OTHERS		;No  - start checking alternatives
	CALL	CLRCRF		;Yes - print it
	CMP	DH,DL		;Are we in range?
	JB	INRANG		;YES
	CMP	DL,0FFh 	;NO, but do we care?
	JE	INRANG		;We dont care so pretend were in range
	MOV	AL,ASCCR 	;	  and print a
	CALL	TRYCR		;     carriage return
	AND	[DI].CD_CMFLG,NOT CF_CMCRF ;clear CR flag
INRANG:
	RET
OTHERS:
	DEC	DH
TRYTAB:
	CMP	AL,ASCHT 	;is it a tab
	JNE	TRYCR		;no so try <CR>
	TEST	[DI].CD_CMFLG,CF_CMCOD ;test if IBM mode
	JZ	CMROUT		;if so, just output tab
	MOV	AL,ASCSP 	;expand to spaces
MORSPC:
	CALL	XTABCR
	TEST	DH,7		;Are we on a multiple of 8?
	JNZ	MORSPC		;Nope, try again
	MOV	AL,ASCHT 	;Restore AL
	RET			;and return
TRYCR:
	CMP	AL,ASCCR 	;is it a <CR>?
	JNE	TRYLF		;No  - see if its a line feed
	CALL	CMROUT		;Yes - put it out
	XOR	DH,DH		;      and update pos.
	TEST	[DI].CD_CMFLG,CF_CMCLF ;  add an <LF>?
	JZ	SETCRF		;      no
	MOV	AL,ASCLF	;	   yes, so put one out
	CALL	CMROUT
	MOV	AL,ASCCR	;	   restore AL
SETCRF:
	OR	[DI].CD_CMFLG,CF_CMCRF ;set CR flag
	RET

TRYLF:
	CMP	AL, ASCLF	;Is it an <LF>
	JNE	TRYBS		;No - try a <BS>
	TEST	[DI].CD_CMFLG,CF_CMCRF ;look to see this follows CR
	JZ	CMROUT		;not preceded by a <CR>
	AND	[DI].CD_CMFLG,NOT CF_CMCRF ;pred by CR - ignore LF, clear flag
	TEST	[DI].CD_CMFLG,CF_CMCOD ;test if IBM mode
	JZ	CMROUT		;if so, no LF suppression
	RET			;   and return

TRYBS:
	CMP	AL,ASCBS 	;is it a <BS>
	JNE	CLRCRF		;no - write it out
	DEC	DH		;back up pos
	JNC	CLRCRF		;send it out unless pos backed up past 0
	INC	DH
	JMP	CLRCRF


;COM_GPOS - return current file position.
; Entry - SI points to File-Data-Block.
;	  [DI] = device offset (2=COMD, 4=SCRN, etc.)
; Exit	- [AH] = current file column. (0-relative)
;	  All other registers preserved

COM_GPOS:
	PUSH	AX		;save contents of AL
	CALL	GetComDCB	;DI points to Device Control Block
	POP	AX		;restore contents of AL
	MOV	AH,[DI].CD_CMPOS
	RET

;COM_GWID - get device width
; Entry - SI points to File-Data-Block.
;	  [DI] = device offset (2=COMD, 4=SCRN, etc.)
; Exit	- [AH] = device width as set by xxxSWD

COM_GWID:
	MOV	AH,[SI].FD_WIDTH ;get current width
	RET

;COM_SCW - set device comma width	****Apparently not used by Bascom****
; Entry - [BX] = new device comma width
;	  SI points to File-Data-Block.
;	  [DI] = device offset (2=COMD, 4=SCRN, etc.)
; Exit	- SI, DI can be changed.
;	  All other registers preserved
;
;COM_SCW:
;	 RET

;COM_GCW - get device comma width	****Apparantly not used by Bascom****
; Entry - SI points to File-Data-Block.
;	  [DI] = device offset (2=COMD, 4=SCRN, etc.)
; Exit	- [BX] = device comma width as set by xxxSCW
;	  All other registers preserved
;
;COM_GCW:
;	RET

;**	com_blkin - block input
;**	com_blkout  - block output			--DMA support for BLOAD
;	NOT IMPLEMENTED

;	Entry	[bx] =	offset of destination
;		[cx] =	maximum number of bytes to read
;		[dx] =	DS of destination
;		[di] =	device number
;		[si] -> FDB of file to be loaded
;	exit	?      (This routine is provided to allow the user
;			to trash himself beyond all recognition!
;	Exit	[bx] =	1 past last byte read
;		CF	set if EOF encountered
;	Uses	AX
com_blkin equ	B$ERR_FC
com_blkout equ	B$ERR_FC

;***
; GetComDCB - get pointer to COM Device Control Block
;
; Purpose:
;	To compute the offset the device control block (DCB) of
;	the communications device ID given.  The number of the
;	device is also given (0-relative).
; Entry:
;	DI = -2 * device ID (positive value)
; Exit:
;	DI = offset to the device control block
;	AX = 0 for COM1, 1 for COM2, ...
; Modifies:
;	None.
; Exceptions:
;	None.
;*****************************************************************************

GetComDCB:
	CALL	GetComUnitId	;[AX]=unit id (0..n)

	MOV	DI,OFFSET DGROUP:B$CM1DCB ;assume COM1: first
	OR	AX,AX		;test if really first device
	JZ	GetComDCBDone	;if so, then jump
	ADD	DI,SIZE COMDCB	;add to get start of DCB for COM2:
GetComDCBDone:
	RET

;***
; GetComUnitID - get communications unit ID
;
; Purpose:
;	To get the zero-relative unit identification number from
;	the device ID.
; Entry:
;	DI = -2 * device ID (positive number)
; Exit:
;	AX = 0 for COM1:, 1 for COM2:,...
; Modifies:
;	None.
; Exceptions:
;	None.
;***************************************************************************

GetComUnitID:
	MOV	AX,DI
	ADD	AX,2*DN_COM1
	SHR	AX,1		;[AX]=0, 1. for COM1, COM2, ...
ERRRET:
	RET

; Check for COM I/O error and output COM Error Message if error occured.
; Entry - [AH] = non-zero if error occured

CKCMER:
	OR	AH,AH
	JZ	ERRRET		;branch if no COM I/O error detected
	MOV	BL,AH
	XOR	BH,BH		;[BX]=error code 1..n
	MOV	DI,OFFSET DGROUP:CMERRT ;DI has dipatch table for error routines
	CMP	BL,5
	JBE	ERROK
	XOR	BX,BX		;use default error
ERROK:
	SHL	BX,1		;convert error to word offset
	JMP	[DI+BX] 	;Handle error

;***
;B$CommSave - Save communications state
;
;Purpose:
;	This routine will be called whenever a process is going to
;	be shelled.   The communications ports, if opened, are
;	disabled, but their DCB's and opened status are kept for
;	subsequent reopening in B$CommRestore.  Note that
;	interrupt handlers will not have to be terminated.
;
;	This is a DOS only routine.
;
;Entry:
;	None.
;
;Exit:
;	None.
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX, BX, CX, DX
;
;Exceptions:
;	None.
;********************************************************************

cProc	B$CommSave,<PUBLIC,NEAR>,<AX,BX,CX,DX,SI,DI> 
cBegin				

DbAssertRelB	<[b$COFlag]>,E,0,DV_TEXT,<b$COFlag non-zero in B$CommSave> ;
	MOV	CX,b$ComPort	;get value of I/O port for COM1
	MOV	COMSAV,CX	;put into save area
	JCXZ	NoActCOM1	;if no active COM1, then jump

	MOV	DI,OFFSET DGROUP:b$COM_DCB ;get offset to COM1 DCB
; CX must be non-zero so DTR and RTS are unaffected (DOS 3 only)
	MOV	AH,0		; AH = 0 indicates COM1
	CALL	B$TRMCOM	;[3]terminate COM1 device
	CALL	B$ComDealloc	;and dellocate the COM1 buffer
NoActCOM1:			

	MOV	CX,b$ComPort+2	;get value of I/O port for COM2
	MOV	COMSAV+2,CX	;put into save area
	JCXZ	NoActCOM2	;if no active COM2, then jump

	MOV	DI,OFFSET DGROUP:b$COM_DCB+SIZE COMDCB ;offset to COM2 DCB
; CX must be non-zero so DTR and RTS are unaffected (DOS 3 only)
	MOV	AH,1		; AH = 1 indicates COM2
	CALL	B$TRMCOM	;[3]terminate COM2 device
	CALL	B$ComDealloc	;and dellocate the COM2 buffer
NoActCOM2:			
cEnd				

;***
;B$CommRestore - Restore communications state
;
;Purpose:
;	This routine will be called when a shelled process returns.
;	The communications ports disabled in B$CommSave are
;	reopened with their preSHELL values.  Note that interrupt
;	handlers will not have to be restarted.
;
;	This is a DOS only routine.
;
;Entry:
;	None.
;
;Exit:
;	None.
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX, BX, CX, DX
;
;Exceptions:
;	None.
;********************************************************************

cProc	B$CommRestore,<PUBLIC,NEAR>,<AX,BX,CX,DX,SI,DI> 
cBegin				

DbAssertRelB	<[b$COFlag]>,E,0,DV_TEXT,<b$COFlag non-zero in B$CommRestore> ;
	MOV	CX,COMSAV	;get saved I/O port for COM1
	JCXZ	NoRestCOM1	;if none, the skip over restore

	MOV	DI,OFFSET DGROUP:b$COM_DCB ;get offset to COM1 DCB
	CALL	B$ComAlloc	;allocate the COM1 buffer
	MOV	BX,DI		;move DCB address to BX
	CALL	B$INICOM	;[3]initialize COM1 device
NoRestCOM1:			

	MOV	CX,COMSAV+2	;get saved I/O port for COM2
	JCXZ	NoRestCOM2	;if none, then skip over restore

	MOV	DI,OFFSET DGROUP:b$COM_DCB+SIZE COMDCB ;offset to COM2 DCB
	CALL	B$ComAlloc	;allocate the COM2 buffer
	MOV	BX,DI		;move DCB address to BX
	CALL	B$INICOM	;[3]initialize COM2 device
NoRestCOM2:			

cEnd				

sEnd	DV_TEXT 		

	END
