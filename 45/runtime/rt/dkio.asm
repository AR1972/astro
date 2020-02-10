	TITLE	DKIO - Disk I/O Drivers
	page	56,132
;***
; DKIO - Disk I/O Drivers
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module came from iodisk.asm.  Iodisk.asm has been split into
;	three files, -- diskio.asm, dkopen.asm and fileio.asm.	This file
;	contains all dispatching routines (drivers) for a disk or a device
;	I/O, except disk open.	Disk open is in dkopen.asm.  Fileio.asm
;	contains the file I/O interfaces, for example, LOCK, UNlOCK, etc.
;
;	The following is the disk dispatch table.  Each entry is the addr.
;	of the actual working routine.	The routine name is composed by the
;	prefix "DISK_" and the function name, e.g., DISK_EOF is the routine
;	name for EOF function.
;
;	B$D_DISK:
;		 ________
;		|	 |
;		| EOF	 |
;		|--------|
;		| LOC	 |
;		|--------|
;		| LOF	 |
;		|--------|
;		| CLOSE  | = $DISK_CLOSE in gwini.asm
;		|--------|
;		| WIDTH  |
;		|--------|
;		| RANDIO |
;		|--------|
;		| OPEN	 | = B$DISKOPEN in dkopen.asm
;		|--------|
;		| BAKC	 |
;		|--------|
;		| SINP	 |
;		|--------|
;		| SOUT	 |
;		|--------|
;		| GPOS	 |
;		|--------|
;		| GWID	 |
;		|--------|
;		| DWID   |		
;		|--------|
;		| BLKIN  |
;		|--------|
;		| BLKOUT |
;		|________|
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	;Runtime Macro Defintions

;Code segmetns
	useSeg	DK_TEXT
	useSeg	ER_TEXT
	useSeg	NH_TEXT
	useSeg	RT_TEXT
;Data segments
	useSeg	_DATA
	useSeg	_BSS

	INCLUDE seg.inc
	INCLUDE baslibma.inc
	INCLUDE devdef.inc
	INCLUDE files.inc
	INCLUDE ascii.inc
	INCLUDE idmac.inc	
	INCLUDE rtps.inc	; constants shared with QBI

	SUBTTL	data definitions
	page
sBegin	_DATA
	externW b$CLOS_FDB	
	externW B$ENTRY_BUFCNT	
sEnd;_DATA

sBegin	_BSS

externD b$RECPTR		; pointer to random record

	externW b$PTRFIL	
sEnd;_BSS

	SUBTTL	code externals
	page
sBegin	DK_TEXT
	externNP	B$GETEXTERR ;defined in GWIO.ASM
	externNP	B$DISKOPEN
sEnd;DK_TEXT

sBegin	RT_TEXT
	externNP	B$TEST_CLOSE	; close file and deallocate FDB
	externNP	B$Mul32x16	; 32 by 16 bit multiply
sEnd;RT_TEXT

sBegin	NH_TEXT
	externNP	B$LHDALC_CPCT	
	externNP	B$LHFDBLOC	
sEnd;NH_TEXT

sBegin	ER_TEXT
	externNP	B$ERR_BFM
	externNP	B$ERR_BRN
	externNP	B$ERR_DFL
	externNP	B$ERR_FOV
	externNP	B$ERR_IFN
	externNP	B$ERR_IOE
	externNP	B$ERR_ACD
	externNP	B$ERR_FWP
	externNP	B$ERR_FC	
sEnd;ER_TEXT


	assumes CS,DK_TEXT
sBegin	DK_TEXT
	SUBTTL	disk dispatch table
	page

DSPMAC	MACRO	func
	DW	DISK_&func
	ENDM

labelNP <PUBLIC,B$D_DISK>
	DSPNAM

	SUBTTL	disk dispatch routines
	SUBTTL	backup one character
	page
;***
;B$DISK_BAKC -- bakcup one character
;B$IDISK_BAKC -- Far (QB3) interface to backup one character
;
;Purpose:
;	Backup one character for a disk file.  Only valid for input files.
;Entry:
;	(ES:)[SI]   = *FDB
;Exit:
;	file position is updated.
;Uses:
;	none
;Preserves: (optional)
;	all, except flags
;Exceptions:
;	none
;*******************************************************************************

labelNP  DISK_BAKC			

cProc	B$DISK_BAKC,<NEAR,PUBLIC>	

cBegin
	DEC	FileDB.FD_BUFCNT	; one less read/unwritten character
cEnd					;exit

cProc	B$IDISK_BAKC,<FAR,PUBLIC>,<SI> 

cBegin				
	FDB_PTR ES,SI,b$PTRFIL	;get FDB pointer
	DbAssertRel	SI,NE,0,DK_TEXT,<Invalid FDB ptr in B$IDISK_BAKC> 
	cCall	B$DISK_BAKC	;Back up a char
cEnd				
	SUBTTL	disk close
	page
;***
;DISK_CLOSE -- close disk file.  Moved here with [24].
;
;Purpose:
;	close disk file.
;*******************************************************************************
DISK_CLOSE:
	PUSH	BX		; save registers
	PUSH	CX
	PUSH	DX
	MOV	b$CLOS_FDB,SI	; mark that we are closing a file
				; in case write prot error
	MOV	BX,FileDB.FD_HANDLE ; argument is file descriptor
	TEST	FileDB.FD_FLAGS,FL_CHAR ; is this a character dev?
	JNZ	CLOSE1		; brif so -- do not do flushes
	TEST	FileDB.FD_MODE,MD_SQO OR MD_APP ; Seq. or append output ?
	JZ	CLOSE1		; no, nothing to do
	CALL	B$FLUSH	; flush buffer if it contains data,
				; and clear FD_BUFCNT
	CALL	B$FILESIZE	; seek to end of file before closing


CLOSE1:
	CALL	B$CLOSE	; close the file and
	MOV	b$CLOS_FDB,0	; clear closing file flag
	POP	DX		; restore registers
	POP	CX
	POP	BX
	JMP	B$LHDALC_CPCT	; deallocate FDB, compact local heap,
				; and return

	SUBTTL	file EOF
	page
;***
;DISK_EOF -- detect if EOF
;
;Purpose:
;	Return true (-1) if a end-of-file is encountered, otherwise returns
;	false (0).  This is only significant for a file opened for sequential
;	input or for a communications file.
;
;	A true (-1) for a communication file means the buffer is empty.
;Entry:
;	(ES:)[SI]   = *FDB
;Exit:
;	[AX]	= 0 or -1
;Uses:
;	none
;Preserves: (optional)
;	all, except flags
;Exceptions:
;	B$ERR_BFM -- bad file mode
;*******************************************************************************

cProc	DISK_EOF,<NEAR>

cBegin
	TEST	FileDB.FD_MODE,MD_SQO+MD_APP	; output or append?
	JNZ	ERCBFM		; brif so -- bad file mode
	MOV	AX,-1		;set to true (AX=-1)

	TEST	FileDB.FD_FLAGS,FL_NEOF ;bit set if not EOF
	JZ	DskEofExit	;Brif EOF

	TEST	FileDB.FD_MODE,MD_RND+MD_BIN ; random or binary?
	JNZ	Not_Eof		; Brif so -- just return flag value

	PUSH	AX		;save AX
	cCall	B$DISK_SINP	;]Get one char, on return [AL]=char
	POP	AX		;get back AX
	JC	DskEofExit	;Brif EOF
	cCall	DISK_BAKC	;backup over it (all register preserved)

Not_Eof:			
	INC	AX		;set to false
DskEofExit:			;result in AX
cEnd				;exit

ERCBFM: JMP	B$ERR_BFM

	SUBTTL	disk LOC
	page
;***
;DISK_LOC -- file LOC
;
;Purpose:
;	Returns current file position.
;
;	For a disk file, LOC returns the last record number been referred.
;	A record for a sequential file has 128 bytes.  For a character device,
;
;	NOTE:  This routine takes the advantage of 128-byte record length of
;		a sequential file.  Should this be changed, the part, Div128,
;		has to be changed to "call $div32".
;Entry:
;	(ES:)[SI]   = *FDB
;Exit:
;	[DX|AX] = current location in the file
;Uses:
;	Per Convention
;Exceptions:
;	B$ERR_BRN -- bad record number
;*******************************************************************************

cProc	DISK_LOC,<NEAR>

cBegin
	CALL	GetPosition		; get byte position for seq files
					; and record number for random.
	JNC	LOCExit			; brif char device or random/binary

; Commented the subtraction out.  With the ability to seek before the
; initial position, LOC could return a negative result.  Also commented
; out code in DISK_OPEN.
;	CMP	FileDB.FD_MODE,MD_APP	; is it append ?
;	JNE	NotApp			; Brif not -- don't add initial pos.
;	SUB	AX,FileDB.FD_LOGREC	; subtract lower and upper words of
;	SBB	DX,FileDB.FD_HIGHLR	; initial position
; NotApp:					
				; divide result by 128
	MOV	CX,7			;div 128 = shr 7
	XOR	BX,BX			;BX is for detect whether there is
					; the remainder.  CLEARS CARRY
RCRLoop:				;divide loop
	RCR	DX,1			;rotate upper word one bit
	RCR	AX,1			;rotate lower word one bit
	RCR	BX,1			;BX is for detect whether there is
					; the remainder
					;NOTE: carry is reset
	LOOP	RCRLoop 		;loop until done

	; The below code needed for compatability reasons. What a waste!
	CMP	FileDB.FD_MODE,MD_SQI	;is input ? (input is pre-read)
	JNZ	LOCExit 		;Brif not

	OR	BX,BX			;has partial block ?
	JZ	TestZero		;Brif not

	ADD	AX,1			;include partial block
	ADC	DX,0
	JMP	SHORT LOCExit		;exit to caller
TestZero:				;test if LOC is 0
	OR	BX,DX			;give one if result is zero
	OR	BX,AX
	JNZ	LOCExit 		;Brif already nonzero
	INC	AX			;set LOC result to 1

LOCExit:
cEnd					;exit to caller

	SUBTTL	disk seek supporting routines
	page

;*** 
; GetPosition -- get current byte-position/record number in file.
;
;Purpose:
;	Return current byte position for sequential files, or current
;	record number for random/binary files.
;
;Entry:
;	(ES:)[SI]   = *FDB
;Exit:
;	[DX|AX] = current location in the file
;	Carry set if Sequential file and not FL_CHAR
;	Carry clear otherwise
;Uses:
;	Per Convention
;
;Preserves:
;
;Exceptions:
;	B$ERR_BRN -- bad record number
;
;******************************************************************************
cProc	GetPosition,<NEAR>
cBegin

	TEST	FileDB.FD_MODE,MD_RND+MD_BIN ; random or binary?
	JNZ	RndPosition		; Brif so

	MOV	AX,1			; assume LOC = 1
	XOR	DX,DX
	TEST	FileDB.FD_FLAGS,FL_CHAR ; is character device ?
	JNZ	NotSeqFile		; Brif yes -- Exit with LOC = 1

	CALL	GetFilePos 		; [DX|AX] = position

	TEST	FileDB.FD_MODE,MD_SQI	; mode = input?
	JZ	OutputMode		; brif not -- just add count
	SUB	AX,FileDB.FD_INBUFCNT	; subtract # chars placed in
	SBB	DX,0			; buffer by last disk read

OutputMode:				
	ADD	AX,FileDB.FD_BUFCNT	; add # read/unwritten chars in
	ADC	DX,0			; buffer to get current position
	STC				; set carry for test in DISK_LOC
	JNS	GetPosExit		; exit
	JMP	B$ERR_BRN		; brif overflow -- bad record number

RndPosition:
	MOV	AX,FileDB.FD_LOGREC	; get lower word
	MOV	DX,FileDB.FD_HIGHLR	; get upper word
	TEST	FileDB.FD_FLAGS,FL_CHAR ; is character device ?
	JZ	NotSeqFile		; Brif not -- clear carry and exit
	XCHG	AX,CX			; CX = FD_LOGREC
	MOV	AX,FileDB.FD_VRECL	; record length
	CALL	B$MUL32			; [DX|CX]=[DX|CX]*[AX]
	XCHG	AX,CX			; [DX|AX]=result
NotSeqFile:
	CLC				; clear carry for test in DISK_LOC

GetPosExit:				;exit with flags set
cEnd


	SUBTTL	disk SEEK
	page
;*** 
; B$FSEK -- get current byte offset into file.  Added with revision [19].
;
;Purpose:
;	Return the current record number (1-relative).  Sequential files
;	and BINARY files are considered to have a record length of 1.
;Entry:
;	None
;Exit:
;	[DX|AX] = 1-relative current record number or byte ofset for the
;		  next operation.
;		0 for non-disk devices.
;Uses:
;	Per convention
;Preserves:
;
;Exceptions:
;
;******************************************************************************
cProc	B$FSEK,<FAR,PUBLIC>,<SI>
parmW	FileNum		; file number
cBegin
	MOV	BX,FileNum	; BX = file number
	XOR	AX,AX		; assume zero value
	XOR	DX,DX
	CALL	SeekCommon	;(ES:)[SI] = FDB, do error checking
	JNZ	FSeekExit	; brif not disk -- exit with value = 0.
	CALL	GetPosition	; [DX|AX] = current record number
				; or byte offset for sequential files
	ADD	AX,1		; make 1-relative
	ADC	DX,0
	JS	ERCBRN2		; brif overflow -- bad record number
FSeekExit:
cEnd

;*** 
; B$SSEK -- Seek to a record in file.  Added with revision [19].
;
;Purpose:
;
;Entry:
;	None
;Exit:
;	FL_NEOF bit of FD_FLAGS set appropriately
;Uses:
;	Per convention
;Preserves:
;
;Exceptions:
;	B$ERR_BRN -- Bad record number
;
;******************************************************************************
cProc	B$SSEK,<FAR,PUBLIC>,<SI>
parmW	FileNum		; file number
parmD	RecNum		; record number
cBegin
	MOV	BX,FileNum	; BX = file number
	CALL	SeekCommon	;(ES:)[SI] = FDB, do error checking
	JNZ	SSeekExit	; brif not disk -- exit without doing anything
	TEST	FileDB.FD_FLAGS,FL_CHAR ; char device?
	JNZ	SSeekExit	; brif so -- exit without doing anything
	TEST	FileDB.FD_MODE,MD_SQO+MD_APP ; output or append?
	JZ	NoFlush		; brif not -- don't flush buffer
	CALL	B$FLUSH	; flush buffer if it contains data

NoFlush:			
	TEST	FileDB.FD_MODE,MD_SQI ; input?
	JZ	DoSeek		; brif not -- don't touch count
	MOV	WORD PTR FileDB.FD_INBUFCNT,0 ; clear input buffer count
DoSeek:				
	MOV	CX,off_RecNum	; low word of record number/byte offset
	MOV	DX,seg_RecNum	; high word of record number/byte offset
	MOV	AL,RELFLG	; tell Locate record number is specified
	CALL	Locate		; translate record # to 0-relative byte offset
				; in [DX|CX]
	CALL	B$SeekFromStart ; seek from start of file, and check for error
	OR	FileDB.FD_FLAGS,FL_NEOF ; assume no EOF after seek
SSeekExit:
cEnd


ERCBRN2: JMP	B$ERR_BRN	; bad record number


;*** 
; SeekCommon -- Common routine used by B$SSEK and B$FSEK to save code.
;
;Purpose:
;	Added with [19]
;
;Entry:
;	BX = file number
;Exit:
;	(ES:)[SI]   = *FDB
;	ZF if disk file -- NZ if non-disk device
;Uses:
;	None
;Preserves:
;	All
;Exceptions:
;	B$ERR_IFN -- bad file number
;
;******************************************************************************
cProc	SeekCommon,<NEAR>
cBegin
	CALL	B$LHFDBLOC	; SI = *FDB
	JZ	ERCIFN		; brif FDB not found -- bad file number
	FDB_PTR ES,SI,SI	;get FDB ptr from handle in SI
	CMP	FileDB.FD_DEVICE,0 ; non-disk device?
cEnd

ERCIFN:	JMP	B$ERR_IFN	




	page

;***
;B$SeekFromStart -- seek from start of file to specified byte position
;
;Purpose:
;	Set file position by seeking from the begining of the file with the
;	offset given in [DX|CX]
;Entry:
;	(ES:)[SI]   = *FDB
;	[DX|CX] = offset
;Exit:
;	file position is set.
;	[DX|AX] = file position
;Uses:
;	Per convention
;Preserves:
;	BX
;Exceptions:
;	B$ERR_BRN -- bad record number
;*******************************************************************************
cProc	B$SeekFromStart,<PUBLIC,NEAR>
cBegin
	XOR	AX,AX			;seek from the begining of the file
	JMP	SHORT DosSeek		;do seek, and check for error
cEnd	nogen				;exit via DosSeek

	page
;***
;B$FILESIZE -- get file size
;
;Purpose:
;	Get file size by seeking from the end of the file with offset 0.
;Entry:
;	(ES:)[SI]   = *FDB
;Exit:
;	file position is set to EOF.
;	[DX|AX] = file size
;Uses:
;	Per convention
;Preserves:
;	BX
;Exceptions:
;	B$ERR_BRN -- bad record number
;*******************************************************************************

cProc	B$FILESIZE,<PUBLIC,NEAR>

cBegin
	MOV	AX,2		;seek to end + (zero) offset
	JMP	SHORT SeekZeroOffset 
cEnd	nogen			;exit via SeekZeroOffset

	page
;***
;GetFilePos -- get file position
;
;Purpose:
;	Get file position by seeking from the current position with offset 0.
;Entry:
;	(ES:)[SI]   = *FDB
;Exit:
;	[DX|AX] = current file position
;Uses:
;	Per convention
;Preserves:
;	BX
;Exceptions:
;	B$ERR_BRN -- bad record number
;*******************************************************************************

cProc	GetFilePos,<NEAR>
cBegin
	MOV	AX,1		; ask for seek to current + (zero) offset
	SKIP	2		; skip over SeekToStart
cEnd	<nogen>			; fall into SeekZeroOffset


;***
;SeekToStart -- seek to start of file.  Added with [19].
;
;Purpose:
;	Seek from start of file with offset 0
;Entry:
;	(ES:)[SI]   = *FDB
;Exit:
;	file position is set to start of file.
;	[DX|AX] = start file position
;Uses:
;	Per convention
;Preserves:
;	BX
;Exceptions:
;	B$ERR_BRN -- bad record number
;*******************************************************************************

cProc	SeekToStart,<NEAR>
cBegin
	XOR	AX,AX		; seek from start
cEnd	<nogen>			; fall into SeekZeroOffset

;***
;SeekZeroOffset -- Do seek from offset zero.  Added with [19].
;
;Purpose:
;	Get file position by seeking with method in AX with offset 0.
;Entry:
;	(ES:)[SI]   = *FDB
;	AX = seek method (described in DosSeek)
;Exit:
;	[DX|AX] = current file position
;Uses:
;	Per convention
;Preserves:
;	BX
;Exceptions:
;	B$ERR_BRN -- bad record number
;*******************************************************************************

cProc	SeekZeroOffset,<NEAR>
cBegin
	XOR	CX,CX		; set offset to zero
	MOV	DX,CX
cEnd	<nogen>			; fall into DosSeek


;***
;DosSeek
;
;Purpose:
;	Seek to the location desired.
;Entry:
;	(ES:)[SI]   = *FDB
;	[DX|CX] = distance to move
;	[AX]	= seek method (0, 1 or 2)
;			0 = seek from start
;			1 = seek from current
;			2 = seek from end
;Exit:
;	If no error
;		[DX|AX] = has the result (seg:off)
;Uses:
;	Per convention
;
;Preserves:
;	BX
;Exceptions:
;	B$ERR_BRN -- bad record number
;
;*******************************************************************************

cProc	DosSeek,<NEAR>,<BX>
cBegin

	XCHG	CX,DX		; [CX|DX] = position
	CALLOS	LSeek,ERCBRN2,<FileDB.FD_HANDLE> 
	FDB_PTR ES		;refresh FDB SEG jic global memory moved

cEnd

	SUBTTL	disk LOF
	page
;***
;DISK_LOF -- length of the file
;
;Purpose:
;	Returns the length of the file.
;
;	If the file is a communications file, LOF returns the free spaces in
;	the buffer.
;Entry:
;	(ES:)[SI]   = *FDB
;Exit:
;	[DX|AX] = length of the file
;Uses:
;	none
;Exceptions:
;	B$ERR_BRN -- bad record number
;*******************************************************************************

cProc	DISK_LOF,<NEAR>

cBegin
	TEST	FileDB.FD_FLAGS,FL_CHAR ;is character device ?
	JNZ	CharDevLOF		;Brif yes
	TEST	FileDB.FD_MODE,MD_SQO OR MD_APP ; output or append?
	JZ	NotOutApp		; Brif not -- don't flush buffer
	CALL	B$FLUSH		; flush buffer if it contains data
					; (this MIGHT increase file size)
NotOutApp:
	CALL	GetFilePos 		; [DX|AX] = position
	PUSH	DX			; save current position
	PUSH	AX			
	cCall	B$FILESIZE		; get file size in [DX|AX]
	POP	CX			; CX = low word of current pos
	POP	BX			; BX = high word of current pos
	PUSH	DX			; save file size
	PUSH	AX			
	MOV	DX,BX			; [DX|CX] = original position
	CALL	B$SeekFromStart	; set file original position
	POP	AX			; get file size back
	POP	DX			

	JMP	SHORT LOFExit
CharDevLOF:
	MOV	AX,1		;non-random length is 1
	XOR	DX,DX
	TEST	FileDB.FD_MODE,MD_RND+MD_BIN ; random or binary?
	JE	LOFExit			; Brif not -- exit
	MOV	AX,FileDB.FD_FSIZ_LO	
	MOV	DX,FileDB.FD_FSIZ_HI	

LOFExit:
cEnd				;exit

	SUBTTL	disk WIDTH
	page
;***
;DISK_WIDTH -- set file width
;
;Purpose:
;	Set file width while the file is open.
;Entry:
;	(ES:)[SI]   = *FDB
;	[DL]	= file width
;Exit:
;	file width is set.
;Uses:
;	none
;Preserves: (optional)
;	all
;Exceptions:
;	none
;*******************************************************************************

cProc	DISK_WIDTH,<NEAR>

cBegin
	MOV	FileDB.FD_WIDTH,DL	;set file width
cEnd

	SUBTTL	get output position
	page
;***
;DISK_GPOS -- get output position
;
;Purpose:
;	Get current output position.
;Entry:
;	(ES:)[SI]   = *FDB
;Exit:
;	[AH]	= current output position
;Uses:
;	none
;Preserves: (optional)
;	all
;Exceptions:
;	none
;*******************************************************************************

cProc	DISK_GPOS,<NEAR>

cBegin
	MOV	AH,FileDB.FD_OUTPS	;get file output position
cEnd

	SUBTTL	get file width
	page
;***
;DISK_GWID -- get file width
;
;Purpose:
;	Get file width.
;Entry:
;	(ES:)[SI]   = *FDB
;Exit:
;	[AH]	= file width
;Uses:
;	none
;Preserves: (optional)
;	all
;Exceptions:
;	none
;*******************************************************************************

cProc	DISK_GWID,<NEAR>

cBegin
	MOV	AH,FileDB.FD_WIDTH	;get file width
cEnd

DISK_DWID	EQU	B$ERR_FC	; illegal funtion call error

	SUBTTL	disk serial input
	page
;***
;B$DISK_SINP --disk serial input
;B$IDISK_SINP -- Far (QB3) interface for disk serial input
;
;Purpose:
;	Input one character from disk.
;
;	This routine is terrible.  It has four exit points -- namely,
;
;	SetEofFlag -- when read null from disk, set flag and carry and exit
;	EofFlag    -- when an EOF character is read, or the EOF flag has been
;			set, set carry and exit
;	NotEofExit -- when a normal character is get, clear carry and exit
;	SinpExit   -- final exit point, just exit (usually the carry has been
;			processed and exit via this point)
;Entry:
;Exit:
;	[AL]	= char
;	[CX]	= 0 if EOF detected (B$IDISK_SINP only)
;	FDB deallocated and file closed if error and called from OPEN
;Uses:
;	none
;Preserves:
;	BX,CX,DX,DI
;Exceptions:
;	B$ERR_FWP -- permission denied
;	B$ERR_ACD -- path/file access error
;	B$ERR_FOV -- field overflow (from FovChk)
;*******************************************************************************

cProc	B$IDISK_SINP,<PUBLIC,FAR>,<SI> 

cBegin					
	FDB_PTR ES,SI,b$PTRFIL		;get FDB pointer
	DbAssertRel	SI,NE,0,DK_TEXT,<Invalid FDB ptr in B$IDISK_SINP> 
	OR	FileDB.FD_FLAGS,FL_BIN	; specify binary access (no ^Z EOF)
	cCall	B$DISK_SINP		;call disk seq input
	MOV	CX,0			; don't touch flags
	JC	BISINPExit		;brif end of file
	DEC	CX			;set CX non zero
BISINPExit:				
cEnd					


labelNP  DISK_SINP			
cProc	B$DISK_SINP,<PUBLIC,NEAR>,<BX,CX,DX,DI> ;save BX,CX,DX,DI

cBegin

	TEST	FileDB.FD_FLAGS,FL_NEOF ;had EOF encountered ?
	JZ	EofExit 		;Brif EOF

	CMP	FileDB.FD_MODE,MD_RND	
	JE	RndGetChar		;input from random

	LEA	DI,FileDB.FD_BUFFER	;DI=*buffer

	MOV	BX,FileDB.FD_BUFCNT	;BX = next unread char position
	CMP	BX,FileDB.FD_INBUFCNT	;any unread chars left ?
	JNE	GetOneChar		;Brif yes, go get it

	MOV	AL,NOT PUTFLG		;indicate read
	MOV	BX,FileDB.FD_VRECL	;length to be read

	PUSH	ES			;preserve ES
	PUSH	DS			;set ES=DS
	POP	ES			

	cCall	DosReadWrite		;fill one buffer, on return
					; if CY then error, & AX=error code
					; else AX has the actual read/write
					; bytes
	POP	ES			;recover ES
	JB	B$AnalyzeErr		; Brif error

	FDB_PTR ES			;freshen FDB seg
	OR	AX,AX			; is end-of-file ?
	JZ	SetEofFlag		;Brif yes, set FD_FLAGS

	MOV	FileDB.FD_INBUFCNT,AX	; set # bytes in input buffer
	XOR	BX,BX			; get from the begining of buffer
	MOV	FileDB.FD_BUFCNT,BX	; # read bytes in buffer = 0

GetOneChar:
	MOV	AL,FileDB.FD_BUFFER[BX] ;get byte from buffer[BX]
	INC	FileDB.FD_BUFCNT	;one less in the buffer

	TEST	FileDB.FD_FLAGS,FL_CHAR+FL_BIN ;EOF is nonsensical for char
					; devices or binary files
	JNZ	SinpExit		;Brif yes (exit with NC)
	CMP	AL,EOFCHR		;is EOF char ?
	JNZ	NotEofExit		;exit

	cCall	DISK_BAKC		;backup to keep EOF

SetEofFlag:
	AND	FileDB.FD_FLAGS,NOT FL_NEOF ;set EOF flag
EofExit:				;EOF encountered
	MOV	AL,EOFCHR		;return EOF character
	STC				;set carry
	JMP	SHORT SinpExit		;exit

RndGetChar:				;input from random file
	CALL	FovChk			;field overflow check, on return
					; BX has the current offset
	MOV	AL,FileDB.FD_BUFFER[BX-1] ;get character
NotEofExit:				;clear carry and exit
	CLC				;clear carry

SinpExit:
cEnd					; pop bx,di, and exit

	PAGE
;*** 
; B$AnalyzeErr, B$OpenErr -- analyze error code, and give proper error.
;
;Purpose:
;	Save some code and standardize error reporting by file I/O routines.
;Entry:
;	AX = error code from DosReadWrite
;	For B$AnalyzeErr, SI = *FDB (or 0 if no FDB)
;Exit:
;	None
;Uses:
;	None
;
;Exceptions:
;	B$ERR_IOE -- device I/O error
;	B$ERR_FWP -- permission denied
;	B$ERR_ACD -- path/file access error
;
;******************************************************************************
cProc	B$AnalyzeErr,<NEAR>		
cBegin

	OR	SI,SI			; valid FDB?
	JZ	B$OpenErr		; brif not
	FDB_PTR ES			; restore FDB SEG in ES
	TEST	FileDB.FD_FLAGS,FL_CHAR ; char device ?
	PUSHF				; save this test
	CALL	B$TEST_CLOSE		; if closing flag set, close file
					; and deallocate FDB
	POPF				; char device?
	JNZ	DeviceIOError		; brif so -- device I/O error

labelNP	<PUBLIC,B$OpenErr>		

	cCall	B$GETEXTERR		; ZF if DOS 3 and sharing/locking error
	JNZ	PathAccessError		; brif not (DOS 3 and sharing/locking)

PermissionDenied:
	JMP	B$ERR_FWP		; Permission denied error
PathAccessError:
	JMP	B$ERR_ACD		; Path/File access error
DeviceIOError:
	JMP	B$ERR_IOE		; device I/O error

cEnd	<nogen>			; nothing leaves from here


;***
;FovChk -- field overflow check
;
;Purpose:
;	Check whether the field is overflow.
;Entry:
;	(ES:)[SI]   = *FDB
;Exit:
;	[BX]	= updated position in the buffer
;Uses:
;	none
;Exceptions:
;	B$ERR_FOV -- field overflow
;*******************************************************************************

cProc	FovChk,<NEAR>

cBegin
	MOV	BX,FileDB.FD_BUFCNT	;get current position
	CMP	BX,FileDB.FD_VRECL	;check for end of record
	JE	ERCFOV			;Yes - field overflow
	INC	BX			;bump pointer
	MOV	FileDB.FD_BUFCNT,BX	;update position
cEnd					;exit

ERCFOV:
	MOV	AX,B$ENTRY_BUFCNT	; Restoring the buffer position
	MOV	FileDB.FD_BUFCNT,AX	; to what it was before execution
					; of current statement

	JMP	B$ERR_FOV		; Field Over Flow Error
	page

;*** 
; B$CHK_WRITE -- Check to see that we got what we requested.
;		Added as part of [20].
;
;Purpose:
;
;Entry:
;	(ES:)[SI]   = *FDB
;
;Exit:
;	None
;Uses:
;	Flags
;
;Preserves:
;	All
;
;Exceptions:
;	B$ERR_DFL -- disk full
;	B$ERR_IOE -- I/O error
;
;******************************************************************************
cProc	B$CHK_WRITE,<NEAR>		
cBegin
	CMP	AX,CX			; bytes written the same as required ?
	JE	GotAll			; Brif yes -- no error

;	Check for DOS 2.0 bug when writing to CONOUT.  DOS 2.0 reports
;	n-1 bytes written when requested to write n bytes to CON when
;	CON is in raw mode.

	TEST	FileDB.FD_FLAGS,FL_CONOUT ; Is this CON OUT?
	JZ	Shortchanged		; brif not -- error
	PUSH	AX			; save return count
	INC	AX			; Add one to count
	CMP	AX,CX			; did DOS report 1 less requested count?
	POP	AX			; restore register
	JE	GotAll			; Brif so -- no error

Shortchanged:
	TEST	FileDB.FD_FLAGS,FL_CHAR ; char device?
	PUSHF				; save this test
	MOV	b$CLOS_FDB,SI		; make B$TEST_CLOSE close file
	CALL	B$TEST_CLOSE		; close file with NO ERROR CHECKING,
					; and deallocate FDB
	POPF				; was it a char device?
	JNZ	DeviceIOError		; brif so -- I/O error
	JMP	B$ERR_DFL		; give disk full error

GotAll:
cEnd

	SUBTTL	disk serial output
	page
;***
;DISK_SOUT -- disk serial output
;
;Purpose:
;	Output one char to a file.
;Entry:
;	(ES:)[SI]   = *FDB
;	[AL]	= character
;Exit:
;	none
;Uses:
;	none
;Preserves:
;	BX
;Exceptions:
;	B$ERR_FOV -- field overflow (from FovChk)
;*******************************************************************************

;***
;B$UpdateOUTPS - Update the OUTPS field of a FDB
;
;Purpose:
;	This shared code correctly sets the OUTPS field of a FDB for
;	a new character that is entered into the output buffer.
;
;Entry:
;	AL - character being entered into FDB output buffer
;	(ES:)[SI]   = *FDB
;
;Exit:
;	FileDB.FD_OUTPS set properly
;
;Uses:
;	Flags.
;
;Preserves:
;	None.
;
;Exceptions:
;	None.
;*******************************************************************************

DbPub	DISK_SOUT
cProc	DISK_SOUT,<NEAR>

cBegin
	CMP	FileDB.FD_MODE,MD_RND	
	JE	RndWrtChar	;serial output to random


	TEST	FileDB.FD_FLAGS,FL_CHAR ;can't write EOF char to a char device
	JZ	NotCharDev
	CMP	AL,EOFCHR		;is EOF char ?
	JE	SoutExit		;Brif yes, exit
NotCharDev:
	PUSH	BX			; save BX
	MOV	BX,FileDB.FD_BUFCNT	;# in buffer
	CMP	BX,FileDB.FD_VRECL	;is full ?
	JB	BufferReady		;Brif not, no flush buffer
	PUSH	CX			; B$FLUSH destroys CX
	CALL	B$FLUSH		; flush the buffer
	POP	CX			
	XOR	BX,BX			; new offset into buffer is 0
	; prior to this change, the buffer was cleared with ClearBuffer
	; each flush.  This should not be needed for output files.

BufferReady:
	MOV	FileDB.FD_BUFFER[BX],AL ;store char in buffer
	INC	FileDB.FD_BUFCNT	;update buffer count
	JMP	SHORT UpdateCurloc	

RndWrtChar:				;serial output to random file
	PUSH	BX			; save BX
	cCall	FovChk			;check for field overflow, on return
					; BX is the updated buffer offset
	MOV	FileDB.FD_BUFFER[BX-1],AL   ;store character

UpdateCurloc:
	POP	BX			; get back BX


	CMP	AL,NEWLINE		;is new line ?
	JNZ	UpdateOutpos		;Brif not
	MOV	FileDB.FD_OUTPS,0	;zero position
	JMP	SHORT SoutExit		;exit

UpdateOutpos:				;update current location
	CMP	AL,' '			;is printable character ?
					; (NC if yes)
	CMC				;reverse the carry
	ADC	FileDB.FD_OUTPS,0	;add 1 if printing character
SoutExit:
cEnd					;exit


	SUBTTL	disk OPEN
	page
;***
;DISK_OPEN -- open a file
;
;Purpose:
;	Open a file.  Code is in dkopen.asm.
;*******************************************************************************

DISK_OPEN	EQU	B$DISKOPEN

	SUBTTL	disk block input/output
	page
;***
;DISK_BLKIN -- disk block input
;DISK_BLKOUT -- disk block output
;
;Purpose:
;	Rewritten as part of revision [11].
;	DISK_BLKIN - disk block input, used for BLOAD only.
;	DISK_BLKOUT - disk block output, used for BSAVE only.
;		File position is assumed as already set, the read
;		or write will position the file to the next byte
;		after the operation.
;Entry:
;	(ES:)[SI]   = DGROUP offset of FDB of file to be loaded/saved
;	DX:BX   = source (segment:offset)
;	CX	= maximum number of bytes to read/write
;Exit:
;	CY set - error in read or write.
;	   clear - no error.
;Uses:
;	AX,CX,DX.
;Exceptions:
;	None.
;*******************************************************************************


cProc	DISK_BLKOUT,<NEAR>
cBegin
	MOV	AH,C_WRITE	;set DOS 3 function to write file
	SKIP	2		; skip over DISK_BLKIN
cEnd	nogen

cProc	DISK_BLKIN,<NEAR>
cBegin
	MOV	AH,C_READ	;set DOS 3 function to read file
cEnd	nogen

cProc	DISK_BLK,<NEAR>,<BX,DS>
cBegin
	PUSH	FileDB.FD_HANDLE ;handle on stack while DS=DGROUP
	MOV	DS,DX		;set DS to segment of memory to operate upon

	ASSUME	DS:NOTHING	

	MOV	DX,BX		;set to offset of memory
	POP	BX		;pop handle to register for call
	INT	21H		;perform either DOS 3 read or write
	JC	DISK_BLK_ERR	;if error in operation, then jump
	CMP	AX,CX		;test completion - carry set if not complete
DISK_BLK_ERR:
cEnd

	ASSUME	DS:DGROUP	


	SUBTTL	clear buffer
	page
;***
;ClrRestBuf -- fill remainder of buffer with 0's.  Added with [21].
;
;Purpose:
;	Fill the remainder of buffer with 0's after a short read.
;Entry:
;	[AX]	= number of valid bytes in buffer
;	[CX]	= number of bytes to clear (# requested - # read)
;	[ES:DI] = *buffer
;Exit:
;	End of buffer filled with 0's
;Uses:
;	CX
;Preserves:
;	AX,BX,DX
;Exceptions:
;	none
;*******************************************************************************

cProc	ClrRestBuf,<NEAR>,<AX,CX,DI>

cBegin
	ADD	DI,AX		; [ES:DI] = address to start clear
	XOR	AX,AX		; fill with nulls
	SHR	CX,1		; make length word
	REP	STOSW		; store word
	JNC	ClrBufExit	; brif no odd byte -- exit
	STOSB			; store odd byte
ClrBufExit:
cEnd				; return to caller

	SUBTTL	random I/O supporting routine
	page
;***
; B$MUL32 - 16x32 bit multiply with error checking
;
;Purpose:
;
;Entry:
;	[DX:CX] = multiplicand
;	[AX]	= multiplier
;
;Exit:
;	[DX:CX] = [DX:CX] * [AX]
;
;Uses:
;	AX
;
;Preserves:
;	BX
;
;Exceptions:
;	B$ERR_BRN -- bad record number upon overflow
;
;******************************************************************************
cProc	B$MUL32,<NEAR,PUBLIC>,BX 
cBegin				
	xchg	ax,cx		; get multiplier into cx, low word into ax
	call	B$Mul32x16	; [DX|AX] = [DX|AX] * [CX]
	xchg	ax,cx		; [DX|CX] = result
	jc	ERCBRN		; brif overflow
cEnd				


;***
;Locate -- locate the positon in a file
;
;Purpose:
;	This routine checks whether a given record number is legal.  If it is
;	fine, translate it into the number of bytes.
;Entry:
;	(ES:)[SI]   = *FDB
;	[DX|CX] = record number if user specified
;	[AL]	= flag
;	the flag is defined as the follows:
;		RELFLG ==> record number specified
;
;Exit:
;	[DX|CX] = the location in bytes
;	FileDB.FD_BUFCNT = 0
;Uses:
;	None
;Preserves:
;	AX,BX
;Exceptions:
;	B$ERR_BRN -- bad record number
;*******************************************************************************
cProc	Locate,<NEAR>
cBegin
	TEST	AL,RELFLG		;record number specified?
	JZ	RelRecNum		; Brif not -- use next record # 

	OR	DX,DX			;test upper word of record
	JS	ERCBRN			;Brif negative, "bad record number"
	JNZ	RecNumOK		;Brif positive, record number is OK
	JCXZ	ERCBRN			;Brif low word = 0, "bad record number"
RecNumOK:				
	SUB	CX,1			;make it 0-relative
	SBB	DX,0
	TEST	FileDB.FD_MODE,MD_BIN+MD_RND ; random or binary?
	JZ	LocateExit		; brif not -- exit with [DX|CX] =
					; byte offset
	MOV	FileDB.FD_LOGREC,CX	;save the new referred record #
	MOV	FileDB.FD_HIGHLR,DX	

RelRecNum:
	MOV	CX,FileDB.FD_LOGREC	;[DX|CX] = current logical record
	MOV	DX,FileDB.FD_HIGHLR	
	TEST	FileDB.FD_MODE,MD_BIN	; (SPEED) binary mode?
	JNZ	LocateExit		; brif so -- exit w/o multiplying 
					; by VRECL, since it is 1

	PUSH	AX			; save register
	MOV	AX,FileDB.FD_VRECL	;multiplier
	CALL	B$MUL32			; [DX|CX] = [DX|CX] * [AX]
	POP	AX			; restore register
LocateExit:
	MOV	FileDB.FD_BUFCNT,0	;no read/unwritten bytes in buffer
cEnd					;exit to caller


ERCBRN: JMP	B$ERR_BRN	; bad record number

	SUBTTL	disk random I/O
	page
;***
;DISK_RANDIO -- low level routine to access a file for GET/PUT
;
;Purpose:
;	This routine reads from/writes to a file a record.
;Entry:
;	[AL]		= flags
;			  Bit 0: 1 if PUT, else GET
;			  BIT 1: 1 if explicit record number specified
;			  BIT 2: 1 if record variable specified
;	[BX]		= record length to be written
;	[CX|DX] 	= user specified record number if GET/PUT
;	(ES:)[SI]	= *FDB
;	[b$RECPTR]	= Pointer to the data to be written
;
;Exit:
;	[AX]		= number of bytes actual read/write
;	one buffer is filled/flushed
;Uses:
;	none
;Exceptions:
;	B$ERR_IOE -- device I/O error
;	B$ERR_FWP -- permission denied
;	B$ERR_ACD -- path/file access error
;	B$ERR_BRN -- bad record number (from Locate)
;*******************************************************************************

cProc	DISK_RANDIO,<NEAR>,<CX,DI> 

cBegin
	XCHG	DX,CX		
	CALL	Locate		;[DX|CX] = DWORD byte position of record

	TEST	FileDB.FD_FLAGS,FL_CHAR 
	JNZ	GetPutCharDev	;Brif it is a character device
	test	al,VarStrData	; processing data of var len string?
	jnz	GiveDosCall	; brif so -- don't do the seek.  File is
				; correctly positioned past count word.
	push	ax		; save flags
	CALL	B$SeekFromStart ;seek to set file location (preserves BX)
	pop	ax		; restore flags

GiveDosCall:			;issue the Dos call (AL = Flags)
	PUSH	AX		;save flags
	LES	DI,[b$RECPTR]	; address of buffer
	cCall	DosReadWrite	;issue the Dos call, on return if CY then
				; error happened, AX=error code, else
				; [AX] has the actual number of
				; read/write
	POP	DX		;get back flag in DX
	JB	JMP_AnalyzeErr1	; error return

	FDB_PTR ES			;freshen FDB seg
	TEST	FileDB.FD_MODE,MD_BIN	; binary mode?
	JZ	NotBin			; brif not -- don't adjust count
	ADD	FileDB.FD_LOGREC,AX	; add number actually read/written
	JMP	SHORT IsBin		; to record count
NotBin:					
	test	dl,VarStrLen		; length of variable length string?
	jnz	NoIncrement		; brif not -- don't increment rec #
	ADD	FileDB.FD_LOGREC,1	; increment record number
IsBin:					
	ADC	FileDB.FD_HIGHLR,0	
	JS	ERCBRN			; brif overflow -- bad record number
NoIncrement:				
	OR	FileDB.FD_FLAGS,FL_NEOF ; assume no EOF
	TEST	DL,PUTFLG		; is put ?
	JNZ	PutBuf			; Brif yes

	MOV	CX,BX			; requested count
	SUB	CX,AX			; CX = number we didn't get
	JCXZ	GetPutExit		; Brif got all we requested
	AND	FileDB.FD_FLAGS,NOT FL_NEOF ;clear bit to indicate EOF
	CALL	ClrRestBuf		; clear rest of buffer
	JMP	SHORT GetPutExit	;exit

GetPutCharDev:
	TEST	AL,PUTFLG		; test if PUT or GET
					;   (bug fix -- was: NOT PUTFLG)
	JZ	GiveDosCall		;if GET just do the dos call

	; For random char devices, LOF is just total # bytes 'PUT'
	ADD	FileDB.FD_FSIZ_LO,BX	; add amount to write to file size
	ADC	FileDB.FD_FSIZ_HI,0	; don't bother doing overflow check.
					; So what if size wraps to 0?
	JMP	SHORT GiveDosCall

PutBuf:				
	MOV	CX,BX		; CX = requested count, AX = # written
	CALL	B$CHK_WRITE	; check if all chars written

GetPutExit:
cEnd				;exit to the caller

JMP_AnalyzeErr1:		
	JMP	B$AnalyzeErr	; give appropriate error message


	SUBTTL	low level file I/O -- file access
	page
;***
;DosReadWrite -- issue the DOS call for file read/write
;
;Purpose:
;	Read/write a buffer via DOS call.
;Entry:
;	[AL]	= flag
;	[BX]	= Length of data to be read
;	[SI]	= *FDB
;	[ES:DI] = *buffer
;Exit:
;	b$CSRPOS = 0 (invalid) if console output.
;	if CY
;		[AX] = error code
;	else
;		[AX] = actual number of bytes read/written
;	ES:[SI] = *FDB (for FV_FARFDB only)
;Uses:
;	none
;Preserves: (optional)
;	BX,CX,DX
;Exceptions:
;	none
;*******************************************************************************

cProc	DosReadWrite,<NEAR>,<BX,CX,DX>

cBegin
	TEST	AL,PUTFLG	;is put/write ?

	MOV	CX,BX		; [CX] = buffer length
	MOV	BX,FileDB.FD_HANDLE ; file handle
	MOV	DX,DI		;buffer address
	JNZ	DosCallWrite	;Brif PUT/WRITE
	MOV	AH,C_READ	;read the record in GET
	SKIP	2		
DosCallWrite:
	MOV	AH,C_WRITE	;write the record in PUT
DosCallExit:			
	PUSH	DS		; save DS
	PUSH	ES		
	POP	DS		; [DS:DX] = pointer to data

	ASSUME	DS:NOTHING	


	INT	21H		;do the call
	POP	DS		; restore DS

	ASSUME	DS:DGROUP	


cEnd				;exit to caller

	PAGE
;***
;B$SEEKSTART - seek to beginning of current file.
;
;Purpose:
;	Added as part of revision [12].
;	Seeks to beginning of file pointed to by b$PTRFIL.
;	Used to back up file ptr when a Binary load has
;	been detected.
;Entry:
;	b$PTRFIL - points to current fdb.
;Exit:
;	file ptr repositioned to beginning of current file.
;Uses:
;	Per convention.
;Exceptions:
;	Bad file number (from SeekToStart)
;****

cProc	B$SEEKSTART,<FAR,PUBLIC>,<SI>
cBegin
	FDB_PTR ES,SI,b$PTRFIL	;get FDB pointer
	DbAssertRel	SI,NE,0,DK_TEXT,<Invalid FDB ptr in B$SEEKSTART> 
	CALL	SeekToStart	; seek to start of file & check for errors
cEnd
	PAGE
;***
;B$BUFI, B$BUFO - Binary/Ascii Load/Save support
;
;Purpose:
; Added as part of revision [12].
; B$BUFI reads a block of data from the current file. B$BUFO writes a block
; of data to the current file. Requests for zero length reads/writes will be
; ignored. QB3 must call OPEN code to open file, followed by a call to B$CHAN
; to set the channel number. QB3 must also ensure that no heap movement occurs
; while B$BUFI and B$BUFO are active. QB3 is responsible for closing file
; (B$CLOS also clears b$PTRFIL) even if error occurs.
;
; NOTE: This routine can take a far pointer to a movable item in a heap. This
; routine cannot directly or indirectly cause heap movement.
;
;Entry:
;	b$PTRFIL - contains current FDB ptr.
;	stack parms:
;	fpData - far pointer to data buffer.
;	cbData - count of bytes to read/write.
;Exit:
;	CX = 0 if CbBytes couldn't be read by B$BUFI
;Uses:
;	Per convention.
;Exceptions:
;	None.
;Note: Possible speed optimizations
;	DosReadWrite currently CLEARS buffer before input. (could eliminate)
;
;****

cProc	B$BUFI,<FAR,PUBLIC>
cBegin	<nogen>
	XOR	AL,AL		;[AL]=I/O flag
	SKIP	2		;skip next instruction
cEnd	<nogen>

cProc	B$BUFO,<FAR,PUBLIC>
cBegin	<nogen>
	MOV	AL,PUTFLG	;[AL]=I/O flag
cEnd	<nogen> 		;fall into BufIOCommon

cProc	BufIOCommon,<FAR>,<ES,DI,SI>
parmD	fpData			;ptr to IO buffer
parmW	cbData			;count of bytes to read/write
cBegin
	CBW			;[AX] = I/O flag
	MOV	CX,cbData	;[CX] = count to read/write
	JCXZ	BufIOExit	;brif no data to read/write
	FDB_PTR ES,SI,b$PTRFIL	;get FDB pointer
	LES	DI,fpData	;[ES:DI] = fpData
	MOV	BX,CX		;[BX] = count of data to read/write
DbAssertRel	SI,NE,0,DK_TEXT,<Invalid fdb ptr in B$BUFI/B$BUFO>
	PUSH	AX		;save I/O type
	PUSH	BX		;save requested byte count
	CALL	DosReadWrite	;do block input
	POP	BX		;recover requested count of bytes
	JC	JMP_AnalyzeErr2	; exit if error (PSW.C set)
	CMP	AX,BX		;did we read 'em or writ 'em all
	POP	CX		;recover I/O type 0 for input.
	JC	BufError	;issue error
BufIOExit:
	INC	CX		;return CX=0 eof on read
cEnd

BufError:
	JCXZ	BufIEof 	;EOF if couldn't read all chars
	JMP	B$ERR_DFL	;Disk full if output
BufIEof:
	DEC	CX		;make CX = FFFF so incremented to 0 for EOF
	JMP	SHORT BufIOExit ;return with CX=0 for eof on read

;***
; B$FLUSH - Flush the sequential output buffer.  Moved here with [27].
;
;Purpose:
;	Flush a buffer if it contains any data.
;	No write is done unless buffer contains data
;	If bit FL_TRUNC of FD_FLAGS is set (set at open time for OUTPUT files),
;	a 0-length write is done before the buffer is flushed in order to set
;	the file size.
;
;Entry:
;	(ES:)[SI]   = *FDB
;
;Exit:
;	One buffer is flushed
;	FileDB.FD_BUFCNT = 0
;
;Uses:
;	CX
;Preserves:
;	AX,BX,DX
;
;******************************************************************************
cProc	B$FLUSH,<NEAR,PUBLIC>
cBegin

	XOR	CX,CX		; CX = 0
	TEST	FileDB.FD_FLAGS,FL_TRUNC ; truncate file at this time?
	JZ	NoTrunc		; brif not -- don't truncate file
	AND	FileDB.FD_FLAGS,NOT FL_TRUNC ; reset truncate flag
	CALL	B$FlushCount	; write 0 bytes to set file size
NoTrunc:

	XCHG	CX,FileDB.FD_BUFCNT ; clear count in FDB and get count in CX
	JCXZ	FlushExit	; if nothing in buffer, just return
cEnd	nogen			; fall into B$FlushCount


;***
;B$FlushCount - Flush n bytes of the sequential output buffer.
;
;Purpose:
;	Write a specified number of chars from the sequential file buffer
;	without checking for a zero buffer count.
;
;Entry:
;	(ES:)[SI]   = *FDB
;	CX = number of chars to write
;
;Exit:
;	One buffer is flushed
;	FileDB.FD_BUFCNT IS NOT CHANGED!
;
;Uses:
;	None
;Preserves:
;	All
;
;******************************************************************************
cProc	B$FlushCount,<NEAR,PUBLIC> ; Rename and make public
cBegin

	PUSH	AX		; save registers
	PUSH	BX
	PUSH	DX
	LEA	DX,FileDB.FD_BUFFER ; DX = sequential access buffer address
	MOV	BX,FileDB.FD_HANDLE ; BX = file descriptor
	CALLOS	write		;write it
	JB	JMP_AnalyzeErr2	;go if error
	CALL	B$CHK_WRITE	; check to see that we got all we requested
	POP	DX		; restore registers
	POP	BX
	POP	AX
FlushExit:
cEnd


JMP_AnalyzeErr2:		
	JMP	B$AnalyzeErr	; give appropriate error message
				; file forced close and FDB deallocated
				; if closing flag set.

;*** 
;B$CLOSE -- close a file
;
;Purpose:
;
;Entry:
;	[ES:]si = *FDB
;	BX = handle
;
;Exit:
;	File closed.
;
;Uses:
;	None
;
;Preserves:
;	All
;
;Exceptions:
;	All those generated by B$AnalyzeErr.
;
;******************************************************************************

cProc	B$CLOSE,<NEAR,PUBLIC>,<AX>
cBegin

	CALLOS	close,JMP_AnalyzeErr2,bx ; make sure we don't try to close
				; this file again if we have error

cEnd

sEnd	;DK_TEXT
	END
