	TITLE	INPTTY - console input
	page	,132
;***
;inptty - console input
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;BASIC Syntax mapping to included runtime entry points
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
; - RANDOMIZE Statement - calls B$RNZ0 if no parm
;
;      RANDOMIZE
;      ---------
;	   |
;	B$RNZ0
;
;******************************************************************************

	INCLUDE switch.inc
	INCLUDE rmacros.inc

;Code segments
	useSeg	DK_TEXT
	useSeg	CN_TEXT
	useSeg	MT_TEXT
	useSeg	ST_TEXT
	useSeg	ER_TEXT
	useSeg	RT_TEXT
;data segments
	useSeg	_DATA
	useSeg	_BSS

	INCLUDE seg.inc
	INCLUDE messages.inc
	INCLUDE const.inc	; b$IOFLAG field definitions
	INCLUDE rtps.inc	
	INCLUDE string.inc	
	INCLUDE idmac.inc	

	SUBTTL	local constants
	page

	InpTTY		EQU	0H	;console input

	SUBTTL	data definitions
	page

sBegin	_DATA

	externW b$GetOneVal
	externB b$FInput
	externW B$AC
	externB	b$IOFLAG	; Misc. IO flags.  Defined in GWINI.ASM
	externW b$nuldes	; Null string descriptor
	externW b$RndVar	; random number seed.

labelW	RandpBlock		; information block needed by B$INPP
	staticW ,2		; total 4 bytes
	staticB ,0		; need '?' and CR
	staticB ,2		; var type is I2

sEnd	;_DATA

sBegin	_BSS

	globalW b$DATAPT,,1
	externW b$StkBottom	; for clean stack

	externB b$Buf1		; 256+1 bytes of temp storage

	labelD	BlockPt
	staticW BlockOff,,1	;points to the block of type list
	staticW BlockSeg,,1

	staticW SaveDI,,1	;save registers
	staticW SaveSI,,1
	staticW SaveES,,1

	staticW SaveBP,,1	; save old BP, NOTE: SaveBP, RetOff & RetSeg
				;  should be in this order
	labelD	RetAddr
	staticW RetOff,,1	;return address
	staticW RetSeg,,1

	staticW TYPCNT,,1
	staticW VALCNT,,1

	staticW PROMPT,,1
	staticB INFLG,,1

	externW b$errvec	; error vector (0=none, else address)
	externW b$PTRFIL

	externW b$curlevel	
	externB b$VTYP 	

sEnd	;_BSS

	SUBTTL	code externals
	page

sBegin	CN_TEXT
	externNP	B$TYPSTR
	externNP	B$$WCHT
	externNP	B$$TCR
	externNP	B$KEYDSP	; Trick to pull in B$KEYDSP code 
	externNP	B$INPCRLF	; moved to out.asm
	externNP	B$RDLIN

sEnd	;CN_TEXT

sBegin	MT_TEXT
	externNP	B$FIN
sEnd

sBegin	ER_TEXT
	externNP	B$PUTNUM
sEnd

sBegin	ST_TEXT 			
	externNP	B$STDALCTMP	; deallocate if temp string
	externNP	B$STDALCALLTMP	; deallocate all string temps
sEnd					

sBegin	RT_TEXT
sEnd

	assumes CS,DK_TEXT
sBegin	DK_TEXT
	externFP	B$PEOS		; input/output generate reset routine
	externFP	B$RDI2		; read one item

	SUBTTL	console input interfaces -- B$INPP & B$INPA
	page
;***
;B$INPP -- console input preamble
;void B$INPP(sd sdPrompt, I2 far *pBlock)
;
;Purpose:
;	This is the preamble for console input.  This routine sets up some
;	flags, and checks types of inputs.  If anything goes wrong, print
;	"?Redo from start" and start over. Values will be saved in stack,
;	and will be assigned to variables by the succeeding calls to
;	B$RD<type>.
;
;	We need special care to handle the stack, since all input values
;	are stored in stack and are used across the interfaces.  In order
;	to have a clean stack for storage, registers and stack frame, which
;	are usually saved in stack, are stored in memory.  Note that the
;	stack frame has to be in stack whenever there is the possiblity
;	that an uncontrolled error would happen.  In this routine, the read
;	line routine, B$RDLIN, is the possible troulbe spot.
;
;	The following is the illustration of the stack frame and the
;	stack situation when the routine exits.
;
;		|		|		|		|
;	High	|---------------|	High	|---------------|
;		|		|		|		|
;		|---------------|[b$StkBottom]>|---------------|
;		|		|	     /	|     value	|
;		|---------------| [b$DATAPT]	|---------------|
;		|		|		|		|
;		\ 		\ 		\ 		\
;		\ 		\ 		\ 		\
;		|---------------|		|---------------|
;		|		|		|     value	|
;		|---------------|		|---------------|
;		| Return Segment|		|     value	|
;		|---------------|		|---------------|
;		| Return Offset |		|     (R4 high) |
;		|---------------|		|---------------|
;		|	BP	|		|     (R4 low)	|
;	   BP ->|---------------|		|---------------|
;		\ 		\ 		\ 	.	\
;		\ 		\ 		\ 	.	\
;		|		|		|     value	|
;	Low	|---------------|	Low	|---------------|
;
;	Note: the way to store the value here (in stack) is the same as
;		to store in any memory location.  Look the example for R4.
;
;	The algorithm is split into three parts:
;	(I) Set up
;		save registers (e.g. ES,SI,DI) in memory location
;		save stack frame (BP RetOff RetSeg) in memory location
;		save the pointer of the stack bottom in [b$StkBottom] &
;						     in [b$DATAPT]
;		turn user cursor off (in order to print prompt)
;		save # of values in VALCNT
;		save input flag ("?" and/or CR)
;
;	(II) Parse each value
;		print prompt with/out "? " <CR>
;	     ** read line
;		clean the stack by using [b$StkBottom]
;
;		loop each input value and check the type (B$FIN)
;			Note that around the call to B$FIN (for as short a
;			period as possible) the error vector is set such that
;			all errors encounted will vector back here.
;		if no error then
;			push value into the stack
;		else /* error happened */
;			give error message
;			reconstruct stack frame on the TOP OF THE STACK
;			goto (II)
;	(III) epilog
;		give error if the number of values is not correct (goto
;			error handling routine and start over)
;		restore registers from memory location
;		far jump to the caller
;
;	Note: ** means stack frame has to be in stack.
;
; NOTE: This routine can take a far pointer to a movable item in a heap. This
; routine cannot directly or indirectly cause heap movement. (The interpeter
; places the parameter block in a far heap entry).
;
;Entry:
;	Parameters in stack.
;	sd	sdPrompt
;	int	far*pBlock
;
;	Refer the source head comments for the structure of Block.
;Exit:
;	values are in stack.
;
;	[b$DATAPT] = [b$StkBottom] points to the bottom of var list & stack
;	NOTE: the direction of the list is auto-decrement.
;
;	[BlockPt] contains the far pointer to the types list
;	NOTE: the type list is the subset of the Block described above.
;
;	[VALCNT] is the count of how many variables
;Uses:
;	none
;Exceptions:
;
;*******************************************************************************

cProc	B$INPP,<PUBLIC,FAR>
	ParmSD	sdPrompt	;sd of prompt string
	ParmD	pBlock		;far point to Block
				;NOTE: we may use the symbols of off_pBlock
				;	and seg_pBlock
cBegin				;generate stack frame
	MOV	[SaveES],ES	;save ES
	MOV	[SaveSI],SI	;save SI
	MOV	[SaveDI],DI	;save DI

	LEA	BX,[BP+12]	;bottom of the stack and var list
	MOV	[b$DATAPT],BX	; [b$DATAPT] = [b$StkBottom]
	MOV	[b$StkBottom],BX 

.erre	ID_SSEQDS		; give error if SS <> DS
	PUSH	DS		
	POP	ES		; let ES=DS
	MOV	SI,BP		; source addr
	MOV	DI,OFFSET DGROUP:SaveBP ; destination addr
	MOV	CX,3		; 3 words
	REP	MOVSW		; save in data area, stack frame still in
				;  stack
	MOV	[b$PTRFIL],CX	; Set output to TTY (CX=0)

	GetpSD	BX,sdPrompt	;BX = psdPrompt
	MOV	[PROMPT],BX	;Save prompt string descriptor address
	LES	DI,pBlock	;ES:DI points to the block
	MOV	AX,ES:[DI]	;get block length
	DEC	AX		; block length is n+1
	MOV	[VALCNT],AX	;Remember number of values
	INC	DI
	INC	DI		;increment the pointer
	MOV	AL,ES:[DI]	;get the flag byte
	MOV	[INFLG],AL	;save flag byte
	INC	DI		;so DI points to the list of types now
	MOV	[BlockOff],DI	
	MOV	[BlockSeg],ES	;save segment:offset to the Block

	
PrtPrompt:			;Enter here if error to re-prompt and
				; re-read data
	MOV	BX,[PROMPT]	;BX=*sd of Prompt
	CALL	B$TYPSTR 	;Print the prompt
	TEST	[INFLG],FINP_QSupress ;Need a "?" after prompt?
	JNZ	GetLin		;Brif not
	MOV	AL,"?"		;output "?"
	CALL	B$$WCHT
	MOV	AL," "		; and one blank
	CALL	B$$WCHT
GetLin:
	CALL	B$RDLIN	;get input line
	TEST	[INFLG],FINP_CrLf ;is it INPUT "semi-colon" ?
	JNZ	CleanStk	;Brif yes, don't send <CR><LF>
	CALL	B$INPCRLF	; force CR to all places where input was
				; written (screen and/or redirected file)

CleanStk:
	MOV	SP,[b$StkBottom]	; clean the stack
				; at this point, stack frame is destroyed.
				;  Since B$FIN return error code and error
				;  handler will restore the stack frame, it
				;  should be fine.
	MOV	AX,[VALCNT]	;get # of types
	MOV	[TYPCNT],AX	;Initialize count of values
	MOV	SI,OFFSET DGROUP:b$Buf1
				;DS:SI points to the value list
	LES	DI,[BlockPt]	;ES:DI points to the type list

; At this point, [TYPCNT] has the number of values
;		 ES:DI points to the next type in the type list
;		 DS:SI points to the next input in the input stream
;		 stack is ready to put values.

EachVal:
	MOV	AL,ES:[DI]	;get next type
	INC	DI		;increment the pointer
	MOV	[b$VTYP],AL	;save the type (used by B$FIN)

	MOV	[b$errvec],DK_TEXTOFFSET InpErr	; set err vec
	PUSH	ES		
	PUSH	DS		; set ES = segment of source
	POP	ES		
	INC	[b$curlevel]	; Increment level for possible dealloc
	CALL	B$FIN		;get number or string (AL=Next Char)
	DEC	[b$curlevel]	; restore b$curlevel
	POP	ES		
	MOV	[b$errvec],0	; reset error vector

	MOV	CL,[b$VTYP]	;get the type
	AND	CL,0FH		;get rid of upper half
	CMP	CL,VT_R4	;is 2 byte value ?
	MOV	BX,OFFSET DGROUP:B$AC
				;BX has the result
	JB	PushAC		;Don't do high word if integer or string
	PUSH	[BX+2]		;Push highest word
PushAC:
	PUSH	[BX]
	JBE	NextVal 	;Only push low words if D.P.
	PUSH	[BX-2]
	PUSH	[BX-4]
NextVal:
	DEC	[TYPCNT]	;any values left?
	JZ	ChkEnd		;Brif no more
	CMP	AL,","		;values must be separated by ","
	JZ	EachVal 	;process next value

;Come here to restart if any error occurred

RedoFromStart:			
	CALL	B$$TCR		;force CR/LF
	MOV	AX,MS_REDO	;message number
	cCall	B$PUTNUM	; output message
	CALL	B$$TCR		;force CR/LF

	MOV	AX,[b$curlevel]	; setup for string temp deallocation...
	INC	AX		; ...only above current level!
	CALL	B$STDALCALLTMP	; deallocate all string temps.

	MOV	SP,[b$DATAPT]	;roughly clean the stack, this is only to
				; avoid stack overflow when we restore the
				; stack frame.	the thorough clean will be
				; done after read line.
	PUSH	[RetSeg]	;restore stack fram on stack
	PUSH	[RetOff]
	PUSH	[SaveBP]
	MOV	BP,SP
	JMP	PrtPrompt	;redo from start

;Come here from error trap if overflow in $FIN

InpErr: 			; give error message
	DEC 	[b$curlevel]	; restore [b$curlevel] after B$FIN error
	MOV	[b$errvec],0	; don't allow infinite loop
	PUSH	BX		; save error number
	CALL	B$$TCR		;force CR/LF
	POP	AX		; get back error number
	cCall	B$PUTNUM	; output message
	JMP	SHORT RedoFromStart	; redo from start

ChkEnd:
	OR	AL,AL		;Did we reach end of line?
	JNZ	RedoFromStart	; Brif not, redo from the start

	MOV	BX,[PROMPT]	; BX = pointer to prompt string
	cCall	B$STDALCTMP	; deallocate it if it was temp


	MOV	[b$GetOneVal],OFFSET TTYInpVal
				;set up for get one input item
	MOV	[b$FInput],InpTTY
				;set input flag
	MOV	DI,[SaveDI]	;restore registers
	MOV	SI,[SaveSI]
	MOV	ES,[SaveES]
	MOV	BP,[SaveBP]
	JMP	[RetAddr]	;return to caller
cEnd	nogen			;no code generated

	SUBTTL	TTY input supporting routine -- TTYInpVal
	page
;***
;TTYInpVal -- get one input value
;
;Purpose:
;	This routine gets one value from the buffer (in stack).
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
;
;Entry:
;	[b$VTYP]	= types
;	[b$DATAPT]	points to the var list
;Exit:
;	[SI]		= pointer to the value
;	[b$DATAPT]	is updated
;Uses:
;	none
;Exceptions:
;	none
;*******************************************************************************

cProc	TTYInpVal,<NEAR>	;assume DS=SS, ES=DS when enter

cBegin
.erre	ID_SSEQDS		;assume SS=DS (used in B$RD<type>)
	MOV	SI,[b$DATAPT]	;get pointer to next value
	MOV	AL,[b$VTYP]	; AL has the type
	CBW			;AX has the type
	AND	AL,0EH		;AX has the length
	SUB	SI,AX		;SI points to the begining of the source
	MOV	[b$DATAPT],SI	;save for next call
cEnd				;return to caller


	SUBTTL	RANDOMIZE interface -- B$RNZ0
	page
;***
;B$RNZ0 -- randomize without parameter specified.
;void B$RNZ0(void)
;
;Purpose:
; Reseeds the random number generator.
; Moved here from RANDOM.ASM [21]
;
;Entry:
; This routine prompts for the user to enter the new random number seed'
;
;Exit:
; User specified seed is stored in b$RndVar+1 (the middle word of the random
; number (DWORD)).
;
;Uses:
; Per convention
;
;Exceptions:
; B$ERR_OV -- overflow (in B$INPP)
;
;*******************************************************************************
cProc	B$RNZ0,<PUBLIC,FAR,FORCEFRAME>
cBegin
	MOV	AX,MS_SEED
	cCall	B$PUTNUM	;Output seed message
	MOV	AX,OFFSET DGROUP:b$nuldes
	PUSH	AX		;push *sd of prompt
	PUSH	DS		;far pointer
	MOV	AX,OFFSET DGROUP:RandpBlock
	PUSH	AX		;push *pBlock
	cCall	B$INPP		;on return, input value is in stack
	PUSH	DS
	MOV	AX,OFFSET DGROUP:b$RndVar+1 ;destination
	PUSH	AX
	cCall	B$RDI2		;read user's seed into destination
	cCall	B$PEOS		;reset flags & variables
cEnd				;exit to caller

sEnd	;DK_TEXT

	END
