	TITLE	DVSTMT - Device Independent I/O Statements
	page	56,132
;***
; DVSTMT - Device Independent I/O Statements
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - EOF Function:
;
;     EOF(file number)
;      |
;    I2 B$FEOF
;
; - LOC Function:
;
;      LOC(file number)
;	|
;     I4 B$FLOC
;
; - LOF Function:
;
;      LOF(file number)
;	|
;     I4 B$FLOF
;
; - CLOSE Statement - B$CLOS, no matter having parameters or not
;
;      CLOSE [[#]file number [,[#]filenumber...]]
;
;    Examples:
;
;      CLOSE		     CLOSE #1		     CLOSE 1,2
;	 |			    |			    |
;    B$CLOS			B$CLOS 		B$CLOS
;    parameter count 0		parameter count 1	parameter count 2
;    no parameter		1 in stack		2 & 1 in stack
;
; - WIDTH Statement:
;
;    Four different Syntax possibilities map to four runtime entry points:
;
;      WIDTH size			 WIDTH LPRINT size
;	 |				   |
;      B$WIDT				 B$LWID
;
;
;      WIDTH filenumber, size		 WIDTH device, size
;	 |				   |
;      B$DWID				 B$DWID
;
; - GET Statement - calls B$GET1 if no record number specified, or
;			  B$GET2 if a record number specified
;			  B$GET3 if record variable, but no record number
;			  B$GET4 if record variable, and record number
;
;      GET [#]filenumber [,[record number][,record variable]]
;
;    Examples:
;
;	GET #1		GET #2,4	GET #2,,FOO	GET #2,4,FOO
;	|		|		|		|
;	B$GET1 	B$GET2 	B$GET3 	B$GET4
;
;	Record Number is I4.
;
; - PUT Statement - calls B$PUT1 if no record number specified, or
;			  B$PUT2 if a record number specified
;			  B$PUT3 if record variable, but no record number
;			  B$PUT4 if record variable, and record number
;
;      PUT [#]filenumber [,[record number][,record variable]]
;
;    Examples:
;
;	PUT #1		PUT #2,4	PUT #2,,FOO	PUT #2,4,FOO
;	|		|		|		|
;	B$PUT1 	B$PUT2 	B$PUT3 	B$PUT4
;
; - OPEN Statement:
;
;    Two syntaxes are allowed:
;
;      OPEN mode,[#]filenumber,"filespec" [,reclength]
;	B$OOPN, which has C definition as follows with parameters in stack.
;	B$OOPN(U2 mode, I2 channel, sd *psdName, I2 cbRecord)
;	(refering the procedure head comments of B$OPEN for detail)
;
;      OPEN "filespec" [FOR mode][locktype] AS [#]filenumber [LEN=reclength]
;	B$OPEN, which has C definition as follows with parameters in stack.
;	B$OPEN(sd *psdName,I2 channel,I2 cbRecord,U2 mode,I2 access,I2 lock)
;	(refering the procedure head comments of B$OPEN for detail)
;
; - FILEATTR function
;
;	FILEATTR(file number, field)
;	|
;	I4 B$FATR
;
; - FREEFILE function
;
;	FREEFILE
;	|
;	I2 B$FREF
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

;Code segments:
	useSeg	NH_TEXT 	;near heap
	useSeg	ER_TEXT 	;error handling
	useSeg	DV_TEXT 	;device independent I/O

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
	INCLUDE rtps.inc	; constants shared with QBI

	SUBTTL	local constant definitions
	page

	InpSts	EQU	6	;input status for IOCTL

	TTY	EQU	0	;default b$PTRFIL is TTY

	PRSTM	EQU	0	;print statement
	CHANL	EQU	1	;#
	USING	EQU	2	;using
	WRSTM	EQU	4	;write statement
	LPSTM	EQU	8	;lprint statement

	SUBTTL	data definitions
	page

sBegin	_DATA			;initialized variables
	externB	b$IOFLAG	; Misc. IO flags.  Defined in GWINI.ASM
sEnd				;end of _DATA

sBegin	_BSS			;uninitialized variables

	staticW DispAddr,,1	; kept the dispatch address


	externW b$Buf2		; defined in GWINI.ASM
	PATH_LEN EQU b$Buf2	; save area for pathname length
				; sort of a waste, but convenient
	externW b$RECPTR
sEnd	;_BSS

	SUBTTL	code segments externals
	page

sBegin	DV_TEXT
	externNP	B$PtrDispatch
sEnd

sBegin	NH_TEXT 		;near heap
	externNP	B$LHFDBLOC	
	externNP	B$LHNXTFIL	
	externNP	B$LHLOCFDB	

	externFP	B$STDL		
	externNP	B$STALC 	

sEnd

sBegin	ER_TEXT 		;error code component
	externNP B$ERR_RVR
	externNP B$ERR_BFM
	externNP B$ERR_BRL	; Bad record length
	externNP B$ERR_FSA
	externNP B$ERR_BRN
	externNP B$ERR_IFN
	externNP B$ERR_FC
sEnd

	assumes CS,DV_TEXT	
sBegin	DV_TEXT 		; device I/O

	SUBTTL	LOC interface -- B$FLOC
	page
;***
;B$FLOC -- the current file location
;I4 B$FLOC (I2 channel)
;
;Purpose:
;	This function returns the current file location.  For a random or
;	sequential file, it returns the last record number been read/written
;	(a sequential has fix record length -- 128 bytes).  For a comm.
;	file, it returns the number of bytes in the input buffer waiting to
;	be read.
;Entry:
;	Parameter is in stack.
;	int	Channel
;Exit:
;	[DX|AX] = file location
;Uses:
;	none
;Exceptions:
;	illegal file number -- B$ERR_IFN
;*******************************************************************************

cProc	B$FLOC,<PUBLIC,FAR>
	ParmW	Channel 	;I2, channel #
cBegin
	MOV	AH,DV_LOC	;LOC function
LocLof: 			;common for LOC & LOF
	MOV	BX,Channel	;BX has the file number
	cCall	ComDsp		;dispatch to the actual working routine
				;(xxxx_LOC return results in DX:AX)
cEnd				;exit to caller

	SUBTTL	LOF interface -- B$FLOF
	page
;***
;B$FLOF -- return the length of the file
;I4 B$FLOF(I2 channel)
;
;Purpose:
;	For a disk file, it returns the size of the file.  For a comm.
;	file, it returns the amount of free space in the input buffer.
;Entry:
;	Parameter is in stack.
;	int	channel
;Exit:
;	[DX|AX] = bytes allocated for the file
;Uses:
;	none
;Exceptions:
;	illegal file number -- B$ERR_IFN
;*******************************************************************************

cProc	B$FLOF,<PUBLIC,FAR>
	ParmW	Channel 	;I2 channel #
cBegin
	MOV	AH,DV_LOF	;LOF Function
	JMP	SHORT LocLof	;dispatch to the actual working routine and
				; exit via B$FLOC
cEnd	nogen			;no code generated

	SUBTTL	WIDTH interface -- B$FWID
	page
;***
;B$FWID -- change the width of a file
;void B$FWID(I2 channel, I2 size)
;
;Purpose:
;	This routine changes the file width while the file is opened.
;	This is meaningful for LPT?.
;
;	Syntax is: WIDTH #filenum,filesiz
;
;	The routine sets the file width BEFORE file open is B$DWID.
;	The routine sets the screen width is B$WIDT.
;	The routine sets the LPRINT width is B$LWID.
;Entry:
;	Parameters in stack.
;	int	channel
;	int	wid
;Exit:
;	none
;Uses:
;	none
;Exceptions:
;	illegal file number -- B$ERR_IFN
;*******************************************************************************

cProc	B$FWID,<PUBLIC,FAR>
	ParmW	Channel 	;I2, channel #
	ParmW	Wid		;I2, width
cBegin
	MOV	BX,Channel	;BX has the file number
	MOV	DX,Wid		;DL has the width
	or	dh,dh		; if width > 255 error
	jnz	ercfc		; and
	or	dl,dl		; if width = 0 error
	jz	ercfc		

	MOV	AH,DV_WIDTH	;File width function
	cCall	ComDsp		;dispatch to the actual working routine
cEnd				;exit to caller

ercfc:	JMP	B$ERR_FC


	SUBTTL	Sequential random I/O interfaces -- B$GET1 & B$PUT1
	page
;***
;B$GET1 -- get a record sequentially
;void B$GET1(I2 channel)
;
;Purpose:
;	Get a record into random buffer if there is no record number specified.
;Entry:
;	Parameter in stack.
;	int	Channel
;Exit:
;	a record is read into the random buffer
;
;	Note: while calling RndDsp,
;		[BX]	= file number (used by B$LHFDBLOC/B$LocateFDB)
;		[AL]	= GET + Sequential flag (refer to RndDsp)
;Uses:
;	none
;Exceptions:
;	illegal file number -- B$ERR_IFN
;	bad file mode -- B$ERR_BFM
;*******************************************************************************

cProc	B$GET1,<PUBLIC,FAR>
	ParmW	Channel 	;I2, file number
cBegin
	XOR	AL,AL		;indicate GET without a specified RecNum
RndIO1:
	MOV	BX,Channel	;BX has the channel #
	cCall	RndDsp		;do it
cEnd				;exit to caller

;***
;B$PUT1 -- put a record sequentially
;void B$PUT1(I2 Channel)
;
;Purpose:
;	Put a record into random buffer with no specified record number.
;Entry:
;	Parameter in stack.
;	int		Channel
;Exit:
;	a record is put into the random buffer
;
;	Note: while calling RndDsp,
;		[BX]	= file number (used by B$LHFDBLOC/B$LocateFDB)
;		[AL]	= PUT + Sequential flag (refer to RndDsp)
;Uses:
;	none
;Exceptions:
;	illegal file number -- B$ERR_IFN
;	bad file mode -- B$ERR_BFM
;*******************************************************************************

cProc	B$PUT1,<PUBLIC,FAR>
	ParmW	Channel 	;I2 file number
cBegin
	MOV	AL,PutFlg	;indicate PUT without a specified RecNum
	JMP	SHORT RndIO1	;do it
cEnd	nogen			;exit to caller via B$GET1

	SUBTTL	Relative random I/O interfaces -- B$GET2 & B$PUT2
	page
;***
;B$GET2 -- get the record with record number specified
;void B$GET2(I2 Channel, I4 recnum)
;
;Purpose:
;	Get the record specified into random buffer.
;Entry:
;	Parameters in stack.
;	int		Channel
;	long int	RecNum
;Exit:
;	the record is read into random buffer
;
;	Note: while calling RndDsp,
;		[BX]	= file number (used by B$LHFDBLOC/B$LocateFDB)
;		[AL]	= GET + Relative flag (refer to RndDsp)
;		[CX|DX] = I4 specified record number
;Uses:
;	none
;Exceptions:
;	illegal file number -- B$ERR_IFN
;	bad file mode -- B$ERR_BFM
;	bad record number -- B$ERR_BRN
;*******************************************************************************
cProc	B$GET2,<PUBLIC,FAR>
	ParmW	Channel 	;I2 file number
	ParmD	RecNum		;I4 record number
cBegin
	MOV	AL,RelFlg	;indicate GET with record number specified
RndIO2:
	MOV	CX,Seg_RecNum	;CX = RecNum high
	OR	CX,CX		;can't be negative number
	JS	ERCBRN		;Brif negative, give "bad record number"
	MOV	DX,Off_RecNum	;DX = RecNum low
	MOV	BX,CX		;another copy of RecNum high in BX
	OR	BX,DX		;can't be zero
	JZ	ERCBRN		;Brif zero, give "bad record number"
	MOV	BX,Channel	;BX has the channel number
	cCall	RndDsp		;do the work
cEnd				;exit to caller

ERCBRN: JMP	B$ERR_BRN	;bad record number

;***
;B$PUT2 -- put a record with specified record number
;void B$PUT2(I2 Channel, I4 RecNum)
;
;Purpose:
;	Put a record into random buffer with specified record number.
;Entry:
;	Parmaters in stack.
;	int		Channel
;	long int	RecNum
;Exit:
;	a record is put into the random buffer.
;
;	Note: while calling RndDsp,
;		[BX]	= file number (used by B$LHFDBLOC/B$LocateFDB)
;		[AL]	= PUT + Relative flag (refer to RndDsp)
;		[CX|DX] = I4 specified record number
;Uses:
;	none
;Exceptions:
;	illegal file number -- B$ERR_IFN
;	bad file mode -- B$ERR_BFM
;	bad record number -- B$ERR_BRN
;*******************************************************************************

cProc	B$PUT2,<PUBLIC,FAR>
	ParmW	Channel 	;I2 file number
	ParmD	RecNum		;I4 record number
cBegin
	MOV	AL,RelFlg+PutFlg
				;indicate this is PUT with specified Rec Num
	JMP	SHORT RndIO2	;do it
cEnd	nogen			;exit via B$GET2

	SUBTTL	Relative random I/O interfaces -- B$GET3 & B$PUT3
	page
;***
;B$GET3 -- get the record with a record variable specified
;void B$GET3(I2 Channel, TYP far *recptr, I2 reclen)
;
;Purpose:
;	Get the record specified into the users record variable
;
; NOTE: This routine can take a far pointer to a movable item in a heap. This
; routine cannot directly or indirectly cause heap movement.
;
;Entry:
;	Parameters in stack.
;	int		Channel
;	typ far 	*RecPtr
;	int		RecLen
;
;Exit:
;
;Note: while calling RndDsp,
;	[BX]	= file number (used by B$LHFDBLOC/B$LocateFDB)
;	[AL]	= GET + Record flag (refer to RndDsp)
;	[SI]	= record length
;	[ES:DI] = Pointer to the users record
;
;Uses:
;	none
;
;Exceptions:
;	illegal file number -- B$ERR_IFN
;	bad file mode -- B$ERR_BFM
;	bad record number -- B$ERR_BRN
;*******************************************************************************

cProc	B$GET3,<PUBLIC,FAR>
ParmW	Channel 		; I2 file number
ParmD	RecPtr			; far record pointer
ParmW	RecLen			; i2 record length
cBegin
	MOV	AL,RecFlg	; indicate GET with record variable specified
RndIO3: 			
	MOV	BX,Channel	; [BX] = channel number
	PUSH	SI		
	PUSH	DI		
	PUSH	ES		

	MOV	SI,RecLen	; [SI] = Record length
	LES	DI,RecPtr	; [ES:DI] = Record Pointer
	cCall	RndDsp		; do the work

	POP	ES		
	POP	DI		
	POP	SI		
cEnd				; exit to caller

;***
;B$PUT3 -- put a record from specified record variable
;void B$PUT3(I2 Channel, TYP far *recptr, I2 reclen)
;
;Purpose:
;	Put a record from record var
;
; NOTE: This routine can take a far pointer to a movable item in a heap. This
; routine cannot directly or indirectly cause heap movement.
;
;Entry:
;	Parameters in stack.
;	int		Channel
;	typ far 	*RecPtr
;	int		RecLen
;
;Exit:
;
;Note: while calling RndDsp,
;	[BX]	= file number (used by B$LHFDBLOC/B$LocateFDB)
;	[AL]	= PUT + Record flag (refer to RndDsp)
;	[SI]	= record length
;	[ES:DI] = Pointer to the users record
;
;Uses:
;	none
;
;Exceptions:
;	illegal file number -- B$ERR_IFN
;	bad file mode -- B$ERR_BFM
;	bad record number -- B$ERR_BRN
;*******************************************************************************
cProc	B$PUT3,<PUBLIC,FAR>	
ParmW	Channel 		; I2 file number
ParmD	RecPtr			; far record pointer
ParmW	RecLen			; i2 record length
cBegin				
	MOV	AL,RecFlg+PutFlg; indicate this is PUT with record var
	JMP	SHORT RndIO3	; do it
cEnd	nogen			; exit via B$GET3

	SUBTTL	Relative random I/O interfaces -- B$GET4 & B$PUT4
	page
;***
;B$GET4 -- get the record with record number specified to record var
;void B$GET4(I2 Channel, I4 recnum, TYP far *recptr, I2 reclen)
;
;Purpose:
;	Get the record specified into user record
;
; NOTE: This routine can take a far pointer to a movable item in a heap. This
; routine cannot directly or indirectly cause heap movement.
;
;Entry:
;	Parameters in stack.
;	int		Channel
;	long int	RecNum
;	typ far 	*RecPtr
;	int		RecLen
;
;Exit:
;
;Note: while calling RndDsp,
;	[BX]	= file number (used by B$LHFDBLOC/B$LocateFDB)
;	[AL]	= GET + Relative flag (refer to RndDsp)
;	[CX|DX] = I4 specified record number
;	[SI]	= record length
;	[ES:DI] = Pointer to the users record
;
;Uses:
;	none
;
;Exceptions:
;	illegal file number -- B$ERR_IFN
;	bad file mode -- B$ERR_BFM
;	bad record number -- B$ERR_BRN
;*******************************************************************************
cProc	B$GET4,<PUBLIC,FAR>	
ParmW	Channel 		; I2 file number
ParmD	RecNum			; I4 record number
ParmD	RecPtr			; far record pointer
ParmW	RecLen			; I2 record length
cBegin

	MOV	AL,RelFlg+RecFlg; indicate GET with record number & var
RndIO4:
	MOV	CX,Seg_RecNum	; CX = RecNum high
	OR	CX,CX		; can't be negative number
	JS	ERCBRN		; Brif negative, give "bad record number"
	MOV	DX,Off_RecNum	; DX = RecNum low
	MOV	BX,CX		; another copy of RecNum high in BX
	OR	BX,DX		; can't be zero
	JZ	ERCBRN		; Brif zero, give "bad record number"
	MOV	BX,Channel	; BX has the channel number
	PUSH	SI		
	PUSH	DI		
	PUSH	ES		

	MOV	SI,RecLen	; [SI] = Record length
	LES	DI,RecPtr	; [ES:DI] = Record Pointer
	cCall	RndDsp		; do the work

	POP	ES		
	POP	DI		
	POP	SI		
cEnd				; exit to caller

;***
;B$PUT4 -- put a record with specified record number from record var
;void B$PUT4(I2 Channel, I4 RecNum, TYP far *recptr, I2 reclen)
;
;Purpose:
;	Put a record from record var with specified record number.
;
; NOTE: This routine can take a far pointer to a movable item in a heap. This
; routine cannot directly or indirectly cause heap movement.
;
;Entry:
;	Parmaters in stack.
;	int		Channel
;	long int	RecNum
;	typ far 	*RecPtr
;	int		RecLen
;
;Exit:
;
;Note: while calling RndDsp,
;	[BX]	= file number (used by B$LHFDBLOC/B$LocateFDB)
;	[AL]	= GET + Relative flag (refer to RndDsp)
;	[CX|DX] = I4 specified record number
;	[SI]	= record length
;	[ES:DI] = Pointer to the users record
;
;Uses:
;	none
;
;Exceptions:
;	illegal file number -- B$ERR_IFN
;	bad file mode -- B$ERR_BFM
;	bad record number -- B$ERR_BRN
;*******************************************************************************
cProc	B$PUT4,<PUBLIC,FAR>	
ParmW	Channel 		; I2 file number
ParmD	RecNum			; I4 record number
ParmD	RecPtr			; far record pointer
ParmW	RecLen			; I2 record length
cBegin				
	MOV	AL,RelFlg+PutFlg+RecFlg ; indicate PUT Rec Num & Rec Var
	JMP	SHORT RndIO4	; do it
cEnd	nogen			; exit via B$GET4

Locals	PROC	NEAR
	SUBTTL	B$FREF - Return next available file number
	PAGE
;***
; B$FREF - Return next available file number
; int pascal B$FREF(void)
;
;Purpose:
; Runtime entry point to return the next available file number. This is done
; by a pretty dumb repetative linear search of all FDBs, trying all file
; numbers starting at 1. Anything more intelligent is probably more effort
; than it's worth, since the call to FREEFILE will more often than not be
; followed by an OPEN call, which is probably I/O bound.
;
;Entry:
; None.
;
;Exit:
; [AX]	= Next available file number.
;
;Uses:
; Per convention.
;
;Exceptions:
; B$ERR_SSC, if the heap is screwed up.
;
;******************************************************************************
cProc	B$FREF,<FAR,PUBLIC,FORCEFRAME>,<SI> 
cBegin				

	XOR	AX,AX		; [AX] = potential "next" file number
FREF_5: 			; Loop to restart at beging of FDB chain
	XOR	SI,SI		; [SI] = Flag to get first FDB pointer
	INC	AX		; [AX] = next potential "next" filenumber
FREF_10:			; Loop for each FDB
	CALL	B$LHNXTFIL	; [SI] = pointer to next FDB
	JZ	FREF_90 	; Jump if no more FDB's (we're done)
	CALL	B$LHLOCFDB	; [BL] = filenumber associated with FDB
	CMP	AL,BL		; See if our "guess" is being used
	JNZ	FREF_10 	; loop to go check next FDB if not used
	JMP	FREF_5		; else loop to attempt a new guess
FREF_90:			; FDB chain end found, and guess not used

cEnd				

	SUBTTL	EOF interface -- B$FEOF
	page
;***
;B$FEOF -- function which detects the EOF for a file
;I2 B$FEOF(I2 Channel)
;
;Purpose:
;	Detect whether the EOF is reached.  This function is only significant
;	for  a input or communication file.
;
;	Note: EOF won't return true (-1) for a random file, even if you GET
;		a record beyond the file end.  However, that GET gets a null
;		record.
;
;Entry:
;	Parameter in stack.
;	int	Channel
;Exit:
;	[AX]	= -1	EOF is reached for a sequential input file, or
;			communication buffer is empty
;		= 0	EOF is not reached yet
;Uses:
;	none
;Exceptions:
;	illegal file number -- B$ERR_IFN
;*******************************************************************************

cProc	B$FEOF,<PUBLIC,FAR>
	ParmW	Channel 	;I2, file number
cBegin
	MOV	BX,Channel	;BX has file number

	OR	BX,BX		;special for standard input ?
	JZ	REDIR		;Brif yes
	MOV	AH,DV_EOF	;end of file function
	cCall	ComDsp		; do it, on return [AX] has the result

	JMP	SHORT EOFExit	;exit to caller
REDIR:
	TEST	b$IOFLAG,RED_INP ; is input file redirected ?
	JZ	ERCIFN		; Brif not, give "illegal file number"
	MOV	AX,C_IOCTL SHL 8 + InpSts ; get input status
	INT	21h		; is the end of a file ?
				;[AL] = 0FFH if not end of file
				;[AL] = 0    if end of file
	CBW			;result in AX
	NOT	AX		; and pervert
EOFExit:

cEnd				;clear stack and return

	SUBTTL	B$FATR - FILEATTR function
	PAGE
;***
; B$FATR - FILEATTR function
; I4 pascal B$FATR(I2 channel, I2 fieldid)
;
;Purpose:
; Returns specific information from an FDB, based on the fieldid value. This
; interface allows us to muck with the FDB, and still provide the same info
; to the user in a stable fasion.
;
;Entry:
; channel	= File number to be queried
; fieldid	= field number to be returned
;
;Exit:
; [DX:AX]	= requested information
;
;Uses:
; Per convention
;
;Exceptions:
; B$ERR_FC	= Bad field number
;
;******************************************************************************
cProc	B$FATR,<FAR,PUBLIC>,<SI> 
parmW	channel 		; file to futz with
parmW	fieldid 		; field desired
cBegin				

	MOV	BX,channel	; [BX] = File's channel #
	cCall	B$LHFDBLOC	; [SI] = FDB pointer (NZ if found)
	JNZ	FATR_3		; Jump if so
	JMP	B$ERR_IFN	; else bad file number

FATR_3: 			
	MOV	BX,fieldid	; [BX] = user requested field
	DEC	BX		; Make it zero-relative
	CMP	BX,FIELDID_MAX	; See if in range
	JB	FATR_5		; Jump if valid request
	JMP	B$ERR_FC	; Else illegal function call

FATR_5: 			
	ADD	BX,BX		; [BX] = word offset into table
	XOR	AX,AX		; Init I4 return value
	CWD			
	FDB_PTR ES,SI,SI	;(ES:)[SI] = * FDB
	ADD	SI,CS:[BX].FIELDOFF_TABLE ; FDB field by fun. (zero rel)
	JMP	CS:[BX].FIELDDISP_TABLE   ; Dispatches by fun. (zero rel)

FATR_TWO:			; Dispatch point for two byte fields
	LODS	WORD PTR FileDB ; Load FDB word
	JMP	SHORT FATR_90	

FATR_ONE:			; Dispatch point for one byte fields
	LODS	BYTE PTR FileDB ; Load FDB byte

FATR_90:			

cEnd				

ERCIFN: JMP	B$ERR_IFN	;illegal file number

	SUBTTL	general I/O supporting routines
	page
;***
;ComDsp -- common dispatch routine
;
;Purpose:
;	This common dispatch routine checks the legality of the channel
;	and then dispatch to the actual working routine.
;Entry:
;	[AH]		= fucntion number (minor #)
;	[BX]		= file number
;Exit:
;	depends on function
;Uses:
;	none
;Exceptions:
;	illegal file number -- B$ERR_IFN
;*******************************************************************************

cProc	ComDsp,<NEAR>,<SI>	;save SI

cBegin
	CALL	B$LHFDBLOC	;[SI] = file data block pointer
	JZ	ERCIFN		;Error - illegal file number
	CALL	B$PtrDispatch	; dispatch to the working routine
cEnd				;pop si and exit to caller

;***
;RndDsp -- dispatch for random I/O
;
;Purpose:
;	Check the legality of the file number and the file mode.  If both
;	OK, dispatch to the working routine.
;Entry:
;	[AL]		= flags
;			  Bit 0: 1 if PUT, else GET
;			  BIT 1: 1 if explicit record number specified
;			  BIT 2: 1 if record variable specified
;	[BX]		= file number
;	[CX:DX] 	= record number if RelFlg is on. Note: use [CX|DX] here
;			  for the consistence with DOS function call
;	[ES:DI] 	= record variable address, if RecFlg is on
;	[SI]		= record variable length, if RecFlg is on
;
;
;Exit:
;	a record is in random buffer
;Uses:
;	none
;Exceptions:
;	illegal file number -- B$ERR_IFN
;	bad file mode -- B$ERR_BFM
;*******************************************************************************

cProc	VarStrLenDsp,<NEAR>,<SI,ES,DI>	
cBegin					
	jmp	VarStrLen_Entry 	
cEnd	<nogen> 			

cProc	RndDsp,<NEAR>,<SI,ES,DI> 
cBegin
	PUSH	SI		; preserve record length
	CALL	B$LHFDBLOC	;NZ & [SI]=*FDB if file number found
	POP	BX		; [BX] = length of record variable
	JZ	ERCIFN		;Brif not found, give "illegal file number"

	FDB_PTR ES,SI,SI	;(ES:)[SI] = *FDB
	TEST	FileDB.FD_MODE,MD_RND+MD_BIN ; random or binary?
	JNZ	RND_OR_BIN	
	JMP	ERCBFM		; brif not - bad file mode
RND_OR_BIN:
	TEST	AL,RecFlg	; See if record variable passed
	JNZ	RndDsp_5	; Jump if it was

	TEST	FileDB.FD_MODE,MD_BIN ; binary mode?
	JZ	NOT_BIN 	
	JMP	ERCRVR		; brif so -- record variable required
NOT_BIN:

	PUSH	DS		; Form...
	POP	ES		;   pointer in...
	LEA	DI,FileDB.FD_BUFFER ;[ES:DI] ptr to record (field buffer)
	MOV	BX,FileDB.FD_VRECL  ;[BX] = length of record (LEN= length)
	JMP	SHORT RndDsp_15 ; go finish dispatch

RndDsp_5:			; record variable was passed in
	OR	BX,BX		; 0-length read/write?
	JNZ	NotSD		; brif not -- not a string descriptor
				; A 0-length means that we have a string
				; descriptor and that we must dereference
				; ES:DI = string descriptor address


	TEST	FileDB.FD_MODE,MD_BIN ; binary mode?
	JNZ	NotVarLenStr	; brif so -- no special string handling


;	Special code to handle puts/gets from/into variable-length strings.
;	For random files, the length of the string will be placed in the file
;	before the string data.

	push	ax		; save flags
	push	cx		; save optional record #
	push	dx		

	mov	bx,2		; length to read/write
	or	al,VarStrLen	; don't increment record # after next op.
	test	AL,PutFlg	; PUT statement?
	jnz	PutStrLen	; brif so -- output length

				; GET statement
	;(othewise, ES should be equal to ds/ss)

	push	bx		; save '2'
	push	di		; save SD offset
	push	ax		; space for length on stack
	mov	di,sp		; ES:DI = addr of length word
	call	VarStrLenDsp	; read length of string into ES:DI
	pop	ax		; AX = length to read (new str length)
	pop	di		; DI = SD offset
	pop	bx		; BX = 2 (total # to read)

	FDB_PTR  ES,si,si	 ; restore (ES:)[SI] = *FDB for ChkRandom
	add	bx,ax		
	call	ChkRandom	; do verification

	push	ax		; save length
	cCall	B$STDL,<DI>	; deallocate existing string
	pop	cx		; restore CX = length

	jcxz	RestoreState	; brif zero-length string -- don't do ALLOC

	mov	bx,cx		; BX = new string length
	call	B$STALC 	; alloc string with new length, BX=*str data
	mov	[di],cx 	; set SD string length
	mov	[di+2],bx	; set SD string address
	mov	[bx-2],di	; set back pointer
	jmp	short RestoreState 

PutStrLen:			; PUT statement, DS:[DI] = str len (from SD)
	push	bx		; save '2'
	add	bx,[di] 	; bx = 2+string length

	call	ChkRandom	; do verification
	pop	bx		; restore bx = 2
	call	VarStrLenDsp	; write length of string to file


RestoreState:			
	pop	dx		; restore optional record #
	pop	cx		
	pop	ax		; restore flags
	or	al,VarStrData	; don't seek before next get/put

NotVarLenStr:			


	MOV	BX,ES:[DI]	; BX = string length
	MOV	DI,ES:[DI+2]	; DS:DI = string address
	PUSH	DS		; ES:DI = string address
	POP	ES		
	OR	BX,BX		; null string?
	JZ	RndExit 	; brif so -- return without doing anything
	FDB_PTR ES		;restore FDG SEG in ES

NotSD:				
	TEST	FileDB.FD_MODE,MD_BIN ; binary mode?
	JNZ	RndDsp_15	; brif so -- skip checks for bad record
				; length and field statement
VarStrLen_Entry:
	call	ChkRandom

RndDsp_15:			
	MOV	[b$RECPTR],DI	
	MOV	[b$RECPTR+2],ES ; [b$RECPTR] = record pointer
	MOV	AH,DV_RANDIO	;AH=function number (minor #)
	CALL	B$PtrDispatch	; dispatch to the working routine
RndExit:			
cEnd				;pop si, exit to caller


;*** 
;ChkRandom -- verify stuff before RANDOM file I/O
;
;Purpose:
;	Added with revision [43] to save code.
;
;Entry:
;	BX = length to read/write (includes count word for variable-length
;		strings as record variables).
;Exit:
;	None
;Uses:
;	None
;Preserves:
;	All
;Exceptions:
;	Bad record length, Field statement active
;
;******************************************************************************
cProc	ChkRandom,<NEAR>
cBegin
	CMP	BX,FileDB.FD_VRECL	; record too long?
	JA	ERCBRL			; brif so -- Bad record length
	TEST	FileDB.FD_FLAGS,FL_FIELD ; FIELD stmt active for this FDB?
	JNZ	ERCFSA			; brif so -- FIELD statement active
cEnd

ERCBRL: JMP	B$ERR_BRL
ERCFSA: JMP	B$ERR_FSA

; FIELDOFF_TABLE
;
; Table of FDB fields offsets that FILEATTR will return. Each entry contains
; the location within the FDB of the field to be returned.
;
; FIELDDISP_TABLE
; table of routine offsets to execute to fetch a particular field.
;
labelW	FIELDOFF_TABLE		
	DW	FD_MODE 	; 1: File Mode
	DW	FD_HANDLE	; 2: DOS file handle

labelW	FIELDDISP_TABLE 	
	DW	FATR_ONE	; 1: File Mode: one byte
	DW	FATR_TWO	; 2: DOS file handle: two byte

FIELDID_MAX	=	2		; last entry in table

Locals	ENDP

ERCBFM: JMP	B$ERR_BFM	;bad file mode
ERCRVR: JMP	B$ERR_RVR	;record variable required

sEnd	;DV_TEXT

	END
