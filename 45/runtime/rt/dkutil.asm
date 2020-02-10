	TITLE	DKUTIL - Disk/Device I/O Utility Routines
	page	56,132
;***
; DKUTIL - Disk/Device I/O Utility Routines
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	This module contains utility routines and dispatch tables
;	used by both disk and device I/O routines.  It will be
;	present in a user program containing any disk or device
;	I/O statements.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

;Code segments:
	useSeg	NH_TEXT 	;near heap
	useSeg	ER_TEXT 	;error handling
	useSeg	CN_TEXT 	;console I/O
	useSeg	DV_TEXT 	;device independent I/O
	useSeg	RT_TEXT 	;runtime core

;Data segments:
	useSeg	_DATA		;initialized variables
	useSeg	_BSS		;uninitialized variable

	INCLUDE seg.inc 	;set up segments
	INCLUDE baslibma.inc
	INCLUDE devdef.inc
	INCLUDE files.inc
	INCLUDE ascii.inc
	INCLUDE idmac.inc
	INCLUDE const.inc
	INCLUDE rtps.inc	;constants shared with QBI

	SUBTTL	local constant definitions
	page

	TTY	EQU	0	;default b$PTRFIL is TTY

	PRSTM	EQU	0	;print statement
	CHANL	EQU	1	;#
	USING	EQU	2	;using
	WRSTM	EQU	4	;write statement
	LPSTM	EQU	8	;lprint statement


	SUBTTL	data definitions
	page

sBegin	_DATA			;initialized variables
	globalW b$pDevTable,DevTable,1 ;pointer to device table
	externB b$PRFG 	;print flag (in PRNVAL.ASM), may be combined
				; from below:
				;0: PRINT stmt
				;1: # (channel)
				;2: USING
				;4: WRITE stmt
				;8: LPRINT stmt
				;e.g. 3 means PRINT # USING

	externB b$IOFLAG	;Misc. IO flags.  Defined in GWINI.ASM
	externB b$BAS_EXT	;defined in FILENAME.ASM

sEnd				;end of _DATA

sBegin	_BSS			;uninitialized variables

	globalW b$RECPTR,,2	;User define type (record) pointer
	staticW DispAddr,,1	;kept the dispatch address


	globalB b$EOL,,1	;set when in B$$WCLF

	externW b$PTRFIL	;defined in global.inc
	externB b$FILMOD	;defined in GWINI.ASM
	externB b$PATHNAM	;defined in GWINI.ASM
	externW b$Buf2		;defined in GWINI.ASM
	PATH_LEN EQU b$Buf2	;save area for pathname length
				;sort of a waste, but convenient

	externB b$LPTFDB
	externB b$Chaining
	externW b$CLOS_FDB
	externW b$CLOS_HANDLE	
sEnd	;_BSS

	SUBTTL	code segments externals
	page

sBegin	NH_TEXT 		;near heap
	externNP	B$LHFDBLOC
	externNP	B$LHNXTFIL
	externNP	B$STDALCTMP	
sEnd

sBegin	ER_TEXT 		;error code component
	externNP B$ERR_BFM
	externNP B$ERR_IFN
	externNP B$ERR_BFN
	externNP B$ERR_FAO
	externNP B$ERR_FC

	externNP B$ERR_RVR	;Record variable required
sEnd

sBegin	CN_TEXT 		;console I/O
	externNP	B$TTY_SINP
	externNP	B$TTY_SOUT
	externNP	B$TTY_GPOS
	externNP	B$TTY_GWID
	externNP	B$TTY_BLKIN
	externNP	B$TTY_BLKOUT
	externNP	B$TYPSTR1
sEnd

sBegin	DV_TEXT 		;device I/O
	externNP	b$dkused	;to pull in dkinit...
	externNP	B$GET_PATHNAME
	externNP	B$GET_DEV_NUM
	externNP	B$ADD_EXT
	externNP	B$STDGET
sEnd

sBegin	RT_TEXT 			;runtime core
	externNP	B$LHDALC_CPCT	;Deallocate FDB and compact
sEnd


assumes cs,DV_TEXT
sBegin	DV_TEXT


;*** 
;DoClose -- perform device close function.
;
;Purpose:
;	Closes a file.  Saves a bit of code by setting close function flag
;	and falling into B$PtrDispatch.
;
;Entry:
;	[SI] =	pointer/handle to FDB
;Exit:
;	None
;Uses:
;	Per convention
;
;Exceptions:
;	Many -- from individual close routines
;
;******************************************************************************
cProc	DoClose,<NEAR>
cBegin
	MOV	AH,DV_CLOSE	; dispatch to close routine
cEnd	<nogen>			; fall into B$PtrDispatch

;***
;B$PtrDispatch -- dispatch according to the device number in the FDB
;B$DevDispatch -- dispatch according to the device number specified
;
;Purpose:
;	Dispatch to the desired I/O routine for devices.
;	B$PtrDispatch first gets device number from file control block, and
;	then does the same thing as B$DevDispatch.
;Entry:
;	[AH] =	function number (minor #)
;
;	if B$PtrDispatch then
;		[SI] =	pointer/handle to FDB
;		BX,CX,DX may contain other parameters needed by dispatched
;		function
;
;	if B$DevDispatch then
;		[AL] =	device number (major #)
;		when B$DevDispatch is called from Open functions:
;			[BX] =	file number
;			[CX] =	record length (0 is default)
;			[DX] =	*sd of file name
;			[b$ACCESS]  =	access right
;			[b$LOCKTYPE] = lock type
;
;	AX,BX,CX,DX & SI are reserved and passed to the dispatched routine.
;	DI contains the address of dispatched routine.
;
;Exit:
;	AX-DX as returned by dispatched routines
;Uses:
;	none
;Exceptions:
;	Many -- by the dispatched routines
;*******************************************************************************

cProc	B$PtrDispatch,<NEAR,PUBLIC>,<DI,SI> 
cBegin
DbAssertRel SI,NE,0,DV_TEXT,<B$PtrDispatch: NULL FDB pointer>	
	FDB_PTR ES,SI,SI	;(ES:)[SI] = *FDB
	PUSH	AX		;save AX (popped later)
	MOV	AL,FileDB.FD_DEVICE ;[AL] = device number (0 for disks)
CommDisp:			;common routine for Ptr/Dev dispatch
	NEG	AL		;since device number is -1 to -n
	CBW			;extend to word (i.e. make AH=0)
	SAL	AX,1		;AX = device table offset (word entries)
	MOV	DI,[b$pDevTable] ;DI = device table address
				; [DI] = byte count of table 
	CMP	AX,CS:[DI]	; is offset within range of device table?
	JB	TableOK		; brif so -- no problem
DbHalt	DV_TEXT,<Uninitialized dispatch table in B$PtrDispatch>	
externNP B$ERR_DNA		
	JMP	B$ERR_DNA	; give device unavailable error
				; NOTE:  THIS SHOULD NEVER HAPPPEN!!!

TableOk:			
	INC	DI		; DI = pointer to device table
	INC	DI		
	XCHG	AX,BX		;use BX for table reference
	MOV	DI,CS:[DI+BX]	;get pointer to dispatch table for device
	XCHG	BX,DI		;DI = offset, BX = pointer to dispatch table
	XCHG	AX,BX		;restore BX to entry value, AX = disp table ptr
	MOV	[DispAddr],AX	;save it in mem-loc

	POP	AX		;[AH] = function number
	PUSH	AX		;save function number & device number
	XCHG	AH,AL		;[AL] = function number
	CBW			;convert to word (make AH=0)
	PUSH	DI		;save DI
	MOV	DI,[DispAddr]	;get addr of device dispatch table
	ADD	DI,AX		;add function code offset to dispatch
	MOV	AX,CS:[DI]	;get the address of routine
	MOV	[DispAddr],AX	;store in [DispAddr]
DbAssertRel	AX,NZ,0,DV_TEXT,<0 value in dispatch table in B$PtrDispatch>
	POP	DI		;get back DI
	POP	AX		;get back AX
	CALL	[DispAddr]	;dispatch & execute routine
				;[AL]=device #, [AH]=function #
				;[DI]=offset to dispatch table
				;if B$PtrDispatch, (ES:)[SI]=*FDB
				;if B$DevDispatch called from B$OPENIT,
				;  [BX]=file number, [CX]=record length,
				;  [DX]=*sd of file name
cEnd				;exit to caller

cProc	B$DevDispatch,<NEAR,PUBLIC>,<DI,SI> ;match to B$PtrDispatch
cBegin
	PUSH	AX			;save AX (popped later)
	JMP	SHORT CommDisp		;jump to common code
cEnd	<nogen> 			;exit via B$PtrDispatch


	SUBTTL	open supporting routines
	page
;***
;B$OPENIT -- common routine for both open interfaces
;
;Purpose:
;	This routine checks the validity of the file name and file number,
;	and also checks whether the channel has already been opened.  If
;	none has problem, it dispatches to the actual device opening routine.
;Entry:
;	AX		= open mode
;	BX		= channel (file number)
;	CX		= record length
;	DX		= *sd of file name
;	[b$ACCESS]	= access right
;	[b$LOCKTYPE]	= locking mode
;
;	Please refer to the procedure head comments of B$OPEN for the detail
;	description of the values for channel, record length, access & lock.
;Exit:
;	b$PTRFIL is reset
;	string decscriptor deallocated
;Uses:
;	per convention
;Exceptions:
;	(1) file name error	-- Bad file name (B$ERR_BFN)
;	(2) access & locking	-- Path/file access error
;				-- Permission denied
;	(3) file number 	-- Illegal file number (B$ERR_IFN)
;	(4) general		-- File already open (B$ERR_FAO)
;				-- File not found (B$ERR_FAO)
;	(5)record number	-- illegal function call (B$ERR_FC)
;*******************************************************************************

cProc	B$OPENIT,<PUBLIC,NEAR>,<SI>	;save SI

cBegin


	MOV	[b$FILMOD],AL	;save mode for further use
	PUSH	CX		;save record length
	cCall	B$CHKNAM	;check valid file name and file number,
				; on return BX = valid file number and
				; AL = device # (major #) for dispatch
				; file name is saved in b$PATHNAM
	CALL	B$LHFDBLOC	;if file found for the given file number (in
				; BX), SI=*FDB and NZ
	JNZ	ERCFAO		;Brif file already open
	POP	CX		;record length in CX
	INC	CX		;test for rec len = -1 (default)
	JZ	DefRec		;go for default converted to 0 value
	DEC	CX		;restore true rec len
	OR	CX,CX		; Overflow flag might have been set by
				; decrement (for 8000 ==> 7FFF).
	JLE	ERCFC		;go if len <= 0 (illegal func call)
DefRec: 			
	MOV	AH,DV_OPEN	;AH = routine # (minor #) for dispatch
	CALL	B$DevDispatch	;dispatch it
	MOV	[b$PTRFIL],TTY	;reset pointer to FDB


cEnd				;pop SI and exit

ERCFC:	JMP	B$ERR_FC	;illegal function call

;***
;B$CHKNAM -- scan file name & file number
;
;Purpose:
;	Check the validity of file name & file number, if both are good,
;	get the device number (major #) for dispatching.
;	If the name is for file 0 (BLOAD/BSAVE) then this routine appends the
;	extention ".BAS" to the filename if it doesn't already have one.
;Entry:
;	BX	= file number
;			-1		internal file
;			1-255		valid file number
;			0,256-65534	invalid file number
;	DX	= *sd of file name
;Exit:
;	AL	= device number (result from B$GET_DEV_NUM)
;	BX	= 0	if used as an internal file (BLOAD/BSAVE ...)
;		= 1-255 valid file number
;	CX	= count of chars (including null) in pathname
;	for disk files, 
;		[b$PATHNAM] = fully-specified, null-terminated pathname
;	for devices,
;		[b$PATHNAM] = null-terminated option string (including "XXXX:")
;Uses:
;	AH
;Exceptions:
;	Bad file name		(B$ERR_BFN)
;	Illegal file number	(B$ERR_IFN)
;*******************************************************************************

cProc	B$CHKNAM,<NEAR>,<SI,DI,ES>

cBegin
	XCHG	DX,BX		;DX = file number, BX = psdFileName
	MOV	DI,OFFSET DGROUP:b$PATHNAM ;destination to store pathname
	PUSH	DS		; set ES=DS
	POP	ES		
	MOV	CX,[BX] 	; get count of chars in string
	JCXZ	ERCIFN		;brif null filename -- Bad file number
				;Yes, this is a stupid message, but its
				;required for compatability reasons.
	CALL	B$GET_DEV_NUM	;check for device, placing device # in AL
				;and device option string in b$PATHNAM
	JZ	NOT_DEVICE	;brif not device -- process name

	MOV	SI,[BX+2]	; get start address
	PUSH	CX		;save count
	REP	MOVSB		;copy the string into b$PATHNAM
	MOV	[DI],CL		;null-terminate the string
	POP	CX		;restore count (doesn't include null)
	INC	DX		;file # = -1 (BLOAD/BSAVE)?
	JZ	ERCFC		;brif so -- devices not valid for BLOAD/BSAVE
	JMP	SHORT CHK_NUMBER

NOT_DEVICE:
	CALL	B$GET_PATHNAME	;scan filename and store pathname in *[DI]
				;AL = return flags
				;CX = len of pathname (including null)
	TEST	AL,FN_WILD	;wildcard in pathname?
				;filename detected?
	JNZ	ERCBFN		;Brif yes, give "Bad file name"

	INC	DX		;-1 is interal use (BSAVE/BLOAD), change to 0
	JNZ	NotFile0	;Brif not for Bload/Bsave
				;BLOAD and BSAVE need to append '.BAS' to
				;pathname if it doesn't have an extention

				;CX and AL preserved from B$GET_PATHNAME
	MOV	SI,OFFSET DGROUP:b$BAS_EXT ;append ".BAS" extention to
	CALL	B$ADD_EXT	;pathname if no extention present.  Also
				;checks for pathname overflow, and updates
				;count in CX
	XOR	AL,AL		;indicate disk device
	JMP	SHORT SCANX	;exit

NotFile0:			;check file number
	XOR	AL,AL		;indicate disk device
CHK_NUMBER:
	DEC	DX		;get back file number, file # = 0?
	JZ	ERCIFN		;Brif so, give "illegal file number"
	OR	DH,DH		;is greater than 255 ?
	JNZ	ERCIFN		;illegal file number
SCANX:
	CALL	B$STDALCTMP	; dealloc string if temp (all regs saved)
	MOV	PATH_LEN,CX	;save count until dispatched to xxxx_OPEN
	XCHG	DX,BX		;return file number in BX
cEnd


ERCFAO: JMP	B$ERR_FAO	;File already open
ERCBFN: JMP	B$ERR_BFN	;Bad file name
ERCIFN: JMP	B$ERR_IFN	;illegal file number

	SUBTTL	close interface -- B$CLOS
	PAGE
;***
;B$CLOS -- close statement
;viod B$CLOS(I2 channel, I2 channel, ..., cParams)
;
;Purpose:
;	This is the interface of CLOSE statement.  In BASCOM 2.0, there are
;	two interfaces, $DCA and $DKC to handle the close.  $DCA closes all
;	files (no parameter given in CLOSE stmt), and $DKC closes the files
;	given.	In BASCOM 3.0, only one interface is used.  Parameters were
;	pushed in stack with the parameter count followed.
;
;	Note: cProc with PLM convention can't handle variable parameters.  In
;	this routine, enter and exit have to be special handled.
;
;	The following is one of the proposed method to handle variable
;	parameters.
;
;cProc	Name,<PUBLIC,FAR>,<DI>	;save DI
;	ParmW	UnknownParam	;potential parameter, NOTE:  MUST use
;				; "cEnd nogen" at the end of the procedure
;				; and handle exit process explicitly
;	ParmW	cParam		;parameter count, last parameter
;
;cBegin 			;use DI as the pointer to walk in the stack
;				;(SI or BX will be another candidate as the
;				; walking register, however if BX is used,
;				; user has to make sure BX won't be changed
;				; within the routine)
;	.
;	.			;save other registers
;	.
;	LEA	DI,UnknownParam ;DI points to one below the parameter count
;	MOV	CX,cParam	;CX has the parameter count
;	JCXZ	NoParm		;no parameter
;ParmLoop:
;	MOV	BX,[DI] 	;get parameter in BX (for example)
;	.
;	.			;do whatever needed for each parameter
;	.
;	ADD	DI,2		;DI points to next parameter
;	LOOP	Parameter_Loop
;NoParm:
;	.
;	.			;do whatever needed for no parameter
;	.
;ExitProc:
;	.			;restore registers if any
;	.
;	MOV	SP,DI		;clean stack
;	MOV	DI,[BP-2]	;get back DI
;	LEA	BX,[BP+2]	;bx=*return addr
;	MOV	BP,[BP] 	;get back BP
;	JMP	DWORD PTR [BX]	;return to caller
;cEnd	nogen			;no code generated
;
;Entry:
;	Parameters were pushed in stack.
;	int	channel
;		.
;		.
;		.
;	int	channel
;	ushort	cParams
;Exit:
;	clears b$PTRFIL
;	none
;Uses:
;	none
;Exceptions:
;	list of possible abnormal exit points (i.e. FCERR)
;*******************************************************************************

				;NOTE!: This routine has a manually
				;generated epilogue due to variable number
				;of parameters.  If you change the prologue,
				;here you must also change the epilogue
				;further down.

cProc	B$CLOS,<PUBLIC,FAR>	; don't save any registers here
	ParmW	UnknownParam	;potential parameter
	ParmW	cParam		;parameter count
cBegin				;generate stack frame
	PUSH	SI		;save SI and DI explicitly to avoid confusion
	PUSH	DI		
	LEA	DI,UnknownParam ;DI points to the one below parameter count
	MOV	CX,cParam	;CX=*cParam
	JCXZ	NoParm		;no parameter, close all files
ParmLoop:
	MOV	BX,[DI] 	;file number in BX
	CALL	B$LHFDBLOC	;SI=*FDB (NZ) if file found
	JZ	NextParm	;file not opened, go next one
	CALL	DoClose 	;process the close
NextParm:
	INC	DI		;next parameter
	INC	DI
	LOOP	ParmLoop	;loop until all done
	JMP	SHORT ExitProc	;clear stack and exit
NoParm:
	CALL	B$CLOSF		;close all files (destroys all registers)
ExitProc:
	MOV	BX,DI		; BX = old SP
	POP	DI		; get back DI
	POP	SI		;get back SI
	MOV	AX,[BP+2]	; [DX:AX] = return address
	MOV	DX,[BP+4]	
	MOV	BP,[BP] 	; get back old BP
	MOV	SP,BX		; clean the stack LAST, so that interrupts
				; don't bash stuff below the SP
	MOV	[b$PTRFIL],0	;clear b$PTRFIL
	PUSH	DX		;put return address back on stack
	PUSH	AX
	RET			;return to caller
cEnd	nogen			;no code generated

	SUBTTL	Runtime Internal routines
	page

;_____	File Dispatch Routines	-------------------------------------------

	page
;***
;B$$RCH - Read a byte
;DBCS-callback
;
;Purpose:
;	Reads a byte of data from the device designated by b$PTRFIL.
;	If we are handling KANJI characters, we have to disassemble
;	them as they come from B$STDGET (when b$PTRFIL == 0).  B$STDGET
;	will return the entire 2 byte character, while we only want to
;	return a single byte.
;
;Entry:
;	b$PTRFIL set properly
;
;Exit:
;	AL = character
;	AH = trashed
;	PSW.C set if no char ready
;	PSW.Z set if no char ready and FO_REDIRECT
;		(this will only happen if redirected input completely read)
;Uses:
;	Per Convention
;
;Preserves:
;	BX, CX, DX
;
;Exceptions:
;	None.
;****
cProc	B$$RCH,<NEAR,PUBLIC>
cBegin
	PUSH	SI
	MOV	SI,b$PTRFIL
	OR	SI,SI
	JNZ	getch1

	TEST	b$IOFLAG,RED_INP ;Input redirected ?
	jz	getch0		;brif not
	call	B$STDGET	;AX = value, PSW.Z set = no valid byte
	POP	SI
	ret
getch0:
	CALL	B$TTY_SINP	;Console input
	JMP	SHORT GETCH2

GETCH1:
	MOV	AH,DV_SINP	;file input
	CALL	B$PtrDispatch

GETCH2:
	POP	SI		; restore register
	JC	GETCH3		;branch if PSW.C set
	OR	SP,SP		; Clear PSW.Z
	RET
GETCH3:
	OR	SP,SP		; clear PSW.Z (and PSW.C)
	STC			; but set PSW.C again
cEnd				; return to caller

;***
; B$$WCH - Output character in (AL)
;
;	Inputs:
;		al = character to output
;		b$PTRFIL
;		DV_SOUT
;	Outputs:
;		none
;	Modifies:
;		ah
;	Exceptions
;		exit through B$TTY_SOUT
;		Bad file mode for binary files
;****
cProc	B$$WCH,<NEAR,PUBLIC>
cBegin
	PUSH	SI
	MOV	SI,b$PTRFIL
	OR	SI,SI
	JNZ	putch1

; Redirected output must be done after two byte codes collected. Move check
; for redir out to B$TTY_SOUT.

	POP	SI
	JMP	B$TTY_SOUT	;Console output

PUTCH1:
	FDB_PTR ES,SI,SI		;(ES:)[SI] = *FDB
	TEST	FileDB.FD_MODE,MD_BIN	;binary mode?
	JZ	NotBinary
	JMP	B$ERR_BFM	;brif so -- bad file mode
NotBinary:
	MOV	AH,DV_SOUT

ptrdsp: CALL	B$PtrDispatch	;Call Dispatch |Moved here from
	POP	SI		;Restore SI    |B$$BCH which is
cEnd				;and return    |no longer in existance

	page
;***
; B$$POS - Get current file position
;
;	Inputs:
;		b$PTRFIL
;		DV_GPOS
;	Outputs:
;		ah = file position
;	Exceptions
;		Exit through B$TTY_GPOS or PTRDSP
;****
cProc	B$$POS,<NEAR,PUBLIC>
cBegin
	PUSH	SI
	MOV	SI,[b$PTRFIL]
	OR	SI,SI
	JNZ	getps1

	POP	SI
	JMP	B$TTY_GPOS	;Get TTY position

getps1: MOV	AH,DV_GPOS	;Get position
	JMP	ptrdsp
cEnd	<nogen>

	page
;***
; B$$WID - Get current file width
;
;	Inputs:
;		b$PTRFIL
;		$DV_GWID
;	Outputs:
;		ah = file width
;	Exceptions:
;		Exit though B$TTY_GWID or PTRDSP
;****
cProc	B$$WID,<NEAR,PUBLIC>
cBegin
	PUSH	SI
	MOV	SI,[b$PTRFIL]
	OR	SI,SI
	JNZ	getwd1

	POP	SI
	JMP	B$TTY_GWID

getwd1: MOV	AH,DV_GWID
	JMP	ptrdsp
cEnd	<nogen>

	SUBTTL
	PAGE
;***
;   B$BIN - block input
;							;block transfer support
;	Entry	[bx] =	offset of destination		;for bload, bsave
;		[cx] =	maximum number of bytes to read
;		[dx] =	DS of destination
;	exit	?
;****
cProc	B$BIN,<NEAR,PUBLIC>
cBegin
	PUSH	SI
	MOV	SI,[b$PTRFIL]
	OR	SI,SI
	JNZ	blkin1

	POP	SI
	JMP	B$TTY_BLKIN

blkin1: MOV	AH,DV_blkin
	JMP	ptrdsp
cEnd	<nogen>

;***
;B$BOT - Block output
;							;block transfer support
;	Entry	[bx] =	offset of destination		;for bload, bsave
;		[cx] =	number of bytes to write
;		[dx] =	DS of destination
;****

cProc	B$BOT	<NEAR,PUBLIC>
cBegin
	PUSH	SI
	MOV	SI,[b$PTRFIL]
	OR	SI,SI
	JNZ	blkot1

	POP	SI
	JMP	B$TTY_BLKOUT

blkot1: MOV	AH,DV_blkout
	JMP	ptrdsp
cEnd	<nogen>

	PAGE
CRLF_LEN= 2			;Length of CR/LF sequence on disk


;   Moved here from out.asm

;***
;B$$WCLF - Write new line
;
;	USES	F
;
;****

cProc	B$$WCLF,<NEAR,PUBLIC>,<AX,BX,SI>
cBegin
	MOV	b$EOL,1		; processing end-of-line
	MOV	SI,[b$PTRFIL]
	OR	SI,SI		;Check for keyboard output
	JZ	wcspec		;  Yes - do special CR/LF
	FDB_PTR ES,SI,SI	;(ES:)[SI] = *FDB

	TEST	FileDB.FD_DEVICE,80H	;Test for special device
	JZ	WCLF_NOT_SPEC		;If not, then jump

	MOV	AL,ASCCR	;get carriage-return
	CALL	B$$WCH		;and output it
	CMP	FileDB.FD_DEVICE,DN_SCRN ;Test if SCRN device
	JE	NO_OUTPUT_LF	;If so, do not output Linefeed

	CMP	FileDB.FD_DEVICE,DN_COM1 ;test against first COM
	JG	OUTPUT_LF	;if before, then output LF
	CMP	FileDB.FD_DEVICE,DN_LPT1 ;test against last COM
	JG	NO_OUTPUT_LF	;if within COM, jump
OUTPUT_LF:
	mov	al,asclf	; get line feed
	call	B$$WCH		; and output it
NO_OUTPUT_LF:
NOTSPEC:
	JMP	SHORT WCLXT	;jump to exit
WCLF_NOT_SPEC:

	CMP	FileDB.FD_MODE,MD_RND	;Check for random
	JNE	wcdisk		;  No - do disk CR/LF
	TEST	b$PRFG,WRSTM	;WRITE statement?
	JZ	wcdisk		;brif not -- just do disk CR/LF

	MOV	BX,FileDB.FD_VRECL  ;Get field length
	SUB	BX,FileDB.FD_BUFCNT ;Subtract off bytes already written
	SUB	BX,CRLF_LEN	;    and two more for CR/LF
	MOV	AL,' '		;Write (BX) spaces

;	This is tricky because DISK_SOUT will give FOV error if
;	(BX) is negative.

wspc:	JZ	wcdisk		;(BX) = 0 - done
	CALL	B$$WCH		;Write space
	DEC	BX		;Decrement (BX)
	JMP	wspc		;Keep on looping until (BX) = 0

wcdisk:
	MOV	AL,ASCCR
	CALL	B$$WCH		;Output CR
	MOV	AL,ASCLF
	CALL	B$$WCH		;Output LF
	JMP	SHORT wclxt

wcspec: MOV	AL,ASCCR	;Output CR only
	CALL	B$$WCH

wclxt:
	MOV	b$EOL,0		; reset to FALSE
cEnd

;***
; B$PRTSTR, B$PRT_TYPCNT, B$PRT_OUTCNT - Type string given descriptor
; Inputs:
;	BX = Address of string descriptor
; Outputs:
;	None.
; Modifieis:
;	CX,SI,AX,F destroyed.
;****

cProc	B$PRTSTR,<NEAR,PUBLIC>
cBegin
	MOV	CX,[BX]		; [CX] = string length

labelNP <PUBLIC,B$PRT_TYPCNT>
	MOV	SI,[BX+2]	; DS:SI = data ptr
	JCXZ	RETL
	push	si		
	MOV	SI,b$PTRFIL
	OR	SI,SI		;is there an open file?
	JZ	PRT_STR_SCRN	;brif not - - - print to screen
	FDB_PTR ES,SI,SI	;(ES:)[SI] = *FDB
	CMP	FileDB.FD_DEVICE,DN_SCRN    
	JZ	PRT_STR_SCRN	;brif file is opened to the screen
	pop	si		

labelNP <PUBLIC,B$PRT_OUTCNT>
	LODSB			; AL = char
	CALL	B$$WCH		; output char
	LOOP	B$PRT_OUTCNT
RETL:				
cEnd				

PRT_STR_SCRN:
	pop	si		
	JMP	B$TYPSTR1	;print this to the screen

;*** 
; B$CLOSF - Close all files.
;
;Purpose:
;	Close all open files.
;Entry:
;	None
;Exit:
;	None
;Uses:
;	Per convention
;Exceptions:
;	None
;******************************************************************************
cProc	B$CLOSF,<NEAR,PUBLIC>,<SI>
cBegin
	CMP	b$Chaining,0	; Are we chaining?
	JNZ	CLOSF_EXIT	;scram if so - don't close em
CLOSF_LOOP:
	XOR	SI,SI		; flag to get first file
	CALL	B$LHNXTFIL	; SI = next FDB pointer
	JZ	CLOSF_END	; brif no more files -- exit loop
CLOSF_CLOSE:
	CALL	DoClose		; process the close
	JMP	SHORT CLOSF_LOOP ; do the next one

CLOSF_END:
	MOV	SI,OFFSET DGROUP:b$LPTFDB ; SI = LPRINT FDB
	TEST	FileDB.FD_FLAGS,FL_LPT_OPN  ;LPRINT open?
	JE	CLOSF_EXIT	; brif not -- just exit
	AND	FileDB.FD_FLAGS,NOT FL_LPT_OPN	;clear the open flag
	CALL	DoClose		; do the close (doesn't deallocate b$LPTFDB)
CLOSF_EXIT:
cEnd


;***
;DevTable -- device dispatch table
;
;Purpose:
;	Device dispatch table for disk only, generated by DEVMAC macro,
;	is used to select the individual device dispatch table (which
;	can only be disk in this case).  If devices are brought in as
;	well, this table is replaced (during one-time initialization in
;	dvinit) with a full disk/device table.
;	Note:	DEVMAC redefins DEVMAC in devdef.inc.
;Entry:
;	none
;Exit:
;	DevTable is set up
;Uses:
;	none
;*******************************************************************************

DEVMAC	MACRO	DeviceName
	externW	B$D_&DeviceName
	DW	B$D_&DeviceName	;;each entry is the address of
	ENDM			;; individual device table

labelW	DevTable		;device table
	DW	2		; # of bytes in table (for error checking)
	DEVMAC	DISK		; generate entry for disks


;*** 
;B$TEST_CLOSE.
;
;Purpose:
;
;Entry:
;
;Exit:
;
;Uses:
;
;Preserves:
;	All
;Exceptions:
;	Possible INT 24 error upon close (?)
;
;******************************************************************************
cProc	B$TEST_CLOSE,<NEAR,PUBLIC>,<SI,BX>	
cBegin

	MOV	BX,b$CLOS_HANDLE ; get handle to close
	OR	BX,BX		; coming from the open code?
	JNZ	CloseIt		; brif so
	MOV	SI,b$CLOS_FDB	;get FDB entry if closing file
	OR	SI,SI		;test if truly closing the file
	JZ	TEST_CLS_DONE	;if not, then just return
	FDB_PTR ES,SI,SI	;(ES:)[SI] = *FDB
	MOV	BX,FileDB.FD_HANDLE ;get handle from FDB
CloseIt:
	PUSH	AX		;save register
	CALLOS	close,,BX	;Close the file, like it or not....
	POP	AX		;restore register
	XOR	SI,SI		
	XCHG	SI,b$CLOS_FDB	;get close flag and clear it
	OR	SI,SI		;test flag, dealloc it?
	JZ	NoDealloc	
	CALL	B$LHDALC_CPCT	;deallocate FDB and compact local heap
NoDealloc:			
	MOV	b$CLOS_HANDLE,0	; clear flag
TEST_CLS_DONE:
cEnd

sEnd	;DV_TEXT

	END
