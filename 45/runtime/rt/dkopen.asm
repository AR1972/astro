	TITLE	DKOPEN - DISK OPEN routines
;***
; DKOPEN - Disk open and other utility routines
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;	The routines in this module are used while opening files
;	in all modes (disk and device files).
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; general runtime macros

	UseSeg	_BSS
	UseSeg	RT_TEXT
	UseSeg	ER_TEXT
	UseSeg	NH_TEXT 	
	UseSeg	DV_TEXT 	
	UseSeg	_DATA		

	INCLUDE seg.inc 	; Segment definitions

	INCLUDE baslibma.inc
	INCLUDE files.inc
	INCLUDE devdef.inc
	INCLUDE rtps.inc	; constants shared with QBI
	INCLUDE nhutil.inc	
	INCLUDE string.inc	
	INCLUDE idmac.inc	

	.xall
	.radix	10

;************************************************************************
;
sBegin	_BSS

externB b$FILMOD		
externB b$PATHNAM		
externW b$Buf2			
PATH_LEN EQU b$Buf2		

globalW b$CLOS_HANDLE,0		; file handle for B$TEST_CLOSE to close
globalW b$CLOS_FDB,0		; FDB for B$TEST_CLOSE to close the file
				; and deallocate the FDB.

externB b$LOCKTYPE
externB b$ACCESS

sEnd	_BSS
;
;************************************************************************


;************************************************************************
;
sBegin	ER_TEXT
externNP	B$ERR_AFE	
externNP	B$ERR_BFM	
externNP	B$ERR_FNF
externNP	B$ERR_ACD
externNP	B$ERR_TMF
externNP	B$ERR_IOE
externNP	B$ERR_FAO
externNP	B$ERR_PNF	

sEnd	ER_TEXT
;
;************************************************************************

;************************************************************************
;
sBegin	NH_TEXT 			
	externNP	B$LHALC_CPCT	; compact LH and allocate entry
	externNP	B$LHDALC	; deallocate FDB
	externNP	B$STDALCTMP	
sEnd	NH_TEXT 			

sBegin	DV_TEXT 			
	externNP	B$DOS3CHECK	
	externNP	B$OPENIT	
sEnd	DV_TEXT 			

sBegin	RT_TEXT

externNP	B$GETEXTERR	; Get extended error, if available
externNP	B$CLOSE
externNP	B$LHNXTFIL	
externNP	B$FILESIZE
externNP	B$SeekFromStart 
externNP	B$DISK_SINP	
externNP	B$OpenErr	
;
;************************************************************************

assumes cs,RT_TEXT

	SUBTTL	disk OPEN
	page

;***
;  B$DISKOPEN - DISK device-dependent OPEN routine.
;
;Purpose:
;	This routine does the actual opening of the files.
;	It tries to open the file in the mode specified and
;	if that fails it will then try to reopen the file
;	in some of the other modes. (The reopen does not
;	happen if the first try was for READ_ONLY).
;	It also checks to see if it opened a character
;	device and if so sets the appropriate fields in the
;	FDB to reflect this. If the open is in the append mode
;	it seeks to the end of the file and backs up over the
;	first CTRL Z.
;
;Entry:
;
;	AX = output from B$GET_PATHNAME
;	BX = file number
;	CX = record size
;	DX = file name string descriptor (ignored)
;
;Exit:
;	File opened and FDB filled in OR
;	appropriate error message given.
;
;Uses:
;	AX, BX, CX, DX
;
;****


cProc	B$DISKOPEN,<NEAR,PUBLIC>,<ES,DI>
cBegin

	PUSH	DS		; ES = DS
	POP	ES

;	First, allocate FDB with extra space for all disk files and
;	space for the field buffer for random files.

	INC	CX		; LEN specified?
	LOOP	LEN_SPEC	; brif so -- use specified record length
	MOV	CX,REC_LENGTH	; CX = default random record length
	TEST	b$FILMOD,MD_RND ; random mode?
	JNZ	LEN_SPEC	; brif so -- use random default
	MOV	CX,SEQ_BUF_LEN	; use default sequential buffer length
LEN_SPEC:			
	TEST	b$FILMOD,MD_BIN ; BINARY mode?
	JZ	NOT_BIN		; brif not -- keep specified buffer size
	MOV	CX,1		; buffer size = 1
NOT_BIN:			
	PUSH	CX		; save buffer size
	PUSH	BX		; save file number
	PUSH	AX		; save output from B$GET_PATHNAME
	MOV	DI,OFFSET DGROUP:b$PATHNAM ; DI = pathname address for
				; B$CHKFOPEN
	XOR	SI,SI		; clear SI for B$AnalyzeErr, so it doesn't
				; try to access a non-existing FDB.

;	If the file is not random or sequential input, check if
;	the file has been already opened in BASCOM.  If CF = 1, then
;	give FILE ALREADY OPEN error.

	TEST	b$FILMOD,MD_RND+MD_SQI+MD_BIN ; random or binary or seqin?
	JNZ	DOPEN_NOCHK	;if so, then jump
	cCALL	B$CHKFOPEN	; check if already open -- give
				;  "file already open" error if so
DOPEN_NOCHK:

;	Determine the initial file access for the open.  The accesses
;	attempted are based on the file mode and ACCESS clause type:

;	[b$FILMOD] = MD_SQI (sequential input)
;	    [b$ACCESS] = ACCESS_NONE	read
;	    [b$ACCESS] = ACCESS_READ	read

;	[b$FILMOD] = MD_SQO (sequential output)
;	    [b$ACCESS] = ACCESS_NONE	write
;	    [b$ACCESS] = ACCESS_WRITE	write

;	[b$FILMOD] = MD_RND OR MD_BIN (random or binary)	[13]
;	    [b$ACCESS] = ACCESS_NONE	read/write, write, read
;	    [b$ACCESS] = ACCESS_READ	read
;	    [b$ACCESS] = ACCESS_WRITE	write
;	    [b$ACCESS] = ACCESS_READ_WRITE	read/write

;	[b$FILMOD] = MD_APP (append)
;	    [b$ACCESS] = ACCESS_NONE	read/write, write
;	    [b$ACCESS] = ACCESS_WRITE	write

	XOR	BX,BX		;assume 0 for OPEN read
	CMP	[b$ACCESS],ACCESS_READ ;test if ACCESS READ
	JE	DOPEN_OPEN	;if so, jump to open file
	CMP	b$FILMOD,MD_SQI ; test if input sequential
	JE	DOPEN_OPEN	;if so, jump to open file
	INC	BX		;assume 1 for OPEN write
	CMP	[b$ACCESS],ACCESS_WRITE ;test if ACCESS WRITE
	JE	DOPEN_OPEN	;if so, jump to open file
	CMP	b$FILMOD,MD_SQO ; test if output sequential
	JE	DOPEN_OPEN	;if so, jump to open file
	INC	BX		;otherwise, read/write file
				;  The file is opened with the mode in BX.	It is either
;	opened initially in the mode computed above, or reopened
;	in the next mode determined from the above table after
;	an open failure.

;
;		File
;-----------------------------------------------
;    |	doesn't  |      exists     |  OpenFlag
;    |	exist	 |		   |	in HEX
;-----------------------------------------------
;sqi |	fail	 |	open file  |	01
;sqo |	create	 |	replace    |	12
;rnd |	create	 |	open file  |	11
;-----------------------------------------------
;

DOPEN_OPEN:

	MOV	DI,BX		;save mode for possible reopen
	CALL	DiskOpenHelper	; open file with mode in BX
				; AX = error code if error
				; BX = file handle
	JC	OPEN_FAILED	; brif open failed

;	The OPEN succeeded.  The only error condition that could
;	occur is if a file is to be appended with only a write
;	access.  If this is so, close the file and fake a
;	PATH/FILE ACCESS ERROR.

	CMP	b$FILMOD,MD_APP ; test if append mode
	JNE	JMP_DOPEN_PROCESS ; brif not -- process open
	CMP	DI,1		;test if WRITE access
	JNE	JMP_DOPEN_PROCESS ; brif not -- process open
	CALL	DO_CLOSE	; file is append with write mode and
				;exists cannot be accessed...
	JMP	SHORT ERCACD	; path/file access error

;	The OPEN failed.  If mode was sequential input, either give
;	correct access error (PERMISSION DENIED if sharing conflict
;	or PATH/FILE ACCESS ERROR if directory access conflict) or
;	FILE NOT FOUND.

OPEN_FAILED:			
	CMP	AX,ERRPNF	;test if PATH NOT FOUND error
	JE	ERCPNF		;if so, then report it
	CMP	b$FILMOD,MD_SQI ; test if input sequential
	JNE	DOPEN_NOT_SQI_FAIL ;if not, then jump
	CMP	AX,ERRACD	; test if access error
	JE	DOPEN_ACCESS_ERROR ;if so, jump to process it
ERCFNF:				
	JMP	B$ERR_FNF	;jump to FILE NOT FOUND

ERCPNF:				
	JMP	B$ERR_PNF	; jump to PATH NOT FOUND

DOPEN_NOT_SQI_FAIL:
	CMP	AX,ERRFNF	; test if FILE NOT FOUND
	JNE	DOPEN_NEXT_ACCESS ;else error forces next one
	MOV	DX,OFFSET DGROUP:b$PATHNAM ; address of file pathname
	XOR	CX,CX		;attributes - none
	MOV	AH,C_CREAT	;create file function
	CALLOS			;and make the DOS call
				; AX = error code or handle
	JC	DOPEN_CREATE_ERROR ; if creation error, jump
	XCHG	BX,AX		; get handle in BX
	CALL	DO_CLOSE	; close the file
	MOV	BX,DI		;get the same access type
	CALL	DiskOpenHelper	; and reopen the file
				; AX = error code if CF
				; BX = file handle
	JNC	DOPEN_PROCESS	;jump if success

;	The OPEN failed with the current access.  If an access
;	failure occurred and the OPEN is allowed to retry using
;	another access ([b$ACCESS]=ACCESS_NONE), determine the
;	new access and retry the OPEN.

DOPEN_NEXT_ACCESS:
	CMP	AX,ERRACD	; test if access error
	JNE	DOPEN_NOT_ACCESS_ERROR ;if not, then branch
	CMP	[b$ACCESS],ACCESS_NONE ;is a retry possible?
	JNE	DOPEN_ACCESS_ERROR ;if not, just process error
	TEST	b$FILMOD,MD_RND+MD_BIN ; test for random or binary modes
	JE	DOPEN_TRY_APPEND_RETRY ; if not, then jump
	OR	DI,DI		;is access READ?
	JE	DOPEN_ACCESS_ERROR ;if so, then jump
	MOV	BX,DI		;get access code
	DEC	BX		;r/w->write or write->read
	JMP	DOPEN_OPEN	;retry random OPEN now

JMP_DOPEN_PROCESS:		; Code is too big and ugly for relative
	JMP	SHORT DOPEN_PROCESS ; jumps to DOPEN_PROCESS.

DOPEN_TRY_APPEND_RETRY:
	CMP	b$FILMOD,MD_APP ; test for append mode
	JNE	DOPEN_ACCESS_ERROR ;jump if not
	CMP	DI,2		;test if read/write
	JNE	DOPEN_ACCESS_ERROR ;jump if not
	MOV	BX,1		;make access WRITE
	JMP	DOPEN_OPEN	;retry append OPEN now

;	OPEN failed with error code 5 (carry set).  Use extended error
;	call (if DOS 3 or later) to determine reported error.
;	"PATH/FILE ACCESS ERROR" is reported when failure is due to
;	directory restriction.	"PERMISSION DENIED" is for DOS 2 or
;	OPEN access conflict.

DOPEN_ACCESS_ERROR:
	JMP	B$OpenErr	; give proper error message, with SI <> FDB.
				; (FL_CHAR bit of FD_FLAGS always clear)

;	Error not access-related.

DOPEN_NOT_ACCESS_ERROR:
	CMP	AX,ERRFNF	; error was FILE NOT FOUND?
	JE	ERCFNF		; brif so
	CMP	b$FILMOD,MD_SQO ; interpreter compatability
	JE	ERCTMF		; brif sequential output -- too many files
ERCACD:				
	JMP	B$ERR_ACD

DOPEN_CREATE_ERROR:		
	CMP	AX,ERRACD	; test if access error
	JNE	DOPEN_NOT_ACCESS_ERROR ; if not, then branch
	CALL	B$GETEXTERR	; get extended error after creation
	JC	ERCTMF		; if DOS 2, then give too many files
	CMP	AX,52H		; test if directory entry creation failed
	JNE	ERCACD		; if not, then path/access error

ERCTMF:				
	JMP	B$ERR_TMF	; too many files

;	OPEN has been completed.  Finish details needed for BASCOM.
;	BX = file handle

DOPEN_PROCESS:
	POP	AX		; AX = output from B$GET_PATHNAME
	POP	DX		; DX = file number
	POP	CX		; restore buffer size
	XCHG	DX,BX		; BX = file #, DX = file handle
	PUSH	DX		; save file handle

				; allocate the FDB, and fill some fields
	PUSH	CX		; save buffer size
	ADD	CX,PATH_LEN	; add extra space for pathname
	MOV	DL,255 		; (DL) = width
				; fields
	MOV	AH,255		; (AH) = all file modes legal
	CALL	B$DEVOPN 	; Allocate file block, set up some fields
	FDB_PTR ES,SI,SI	; (ES:)SI = *FDB
	POP	CX		; get back buffer size
	MOV	FileDB.FD_VRECL,CX  ; set record length/buffer size

	LEA	DI,FileDB.FD_BUFFER ; ES:DI = address to put pathname in the
	ADD	DI,CX		;		FDB (after I/O buffer)
	MOV	CX,PATH_LEN	; CX = length of pathname (including null)
	PUSH	SI		; save FDB address
	MOV	SI,OFFSET DGROUP:b$PATHNAM ; DS:SI = pathname address
	REP	MOVSB		; and copy pathname into FDB
	POP	SI		; SI = FDB address
	POP	BX		; BX = file handle

	MOV	FileDB.FD_HANDLE,BX ; save file handle
	OR	FileDB.FD_FLAGS,FL_NEOF ;set flag for no EOF
	mov	al,0
	callos	ioctl		;discover whether character device
	mov	al,dl
	test	al,fl_char	;Is it a device?
	jz	nochar		;Brif not a character device

	AND	AL,FL_CHAR+FL_CONOUT+FL_CONINP ; Clean all bits except
				; FL_CHAR, FL_CONINP, and FL_CONOUT
	OR	FileDB.FD_FLAGS,AL  ;update FDB flags

	mov	al,1
	or	dx,32		; set raw - preserve bits
	mov	dh,0		; Must set high byte to zero !
	CALLOS	IOCTL,ERCIOE	; set raw mode -- I/O error upon failure

				;Don't make CON out RAW if DOS 2.+
	TEST	b$FILMOD,MD_RND+MD_BIN ; random or binary char device?
	JNZ	NOCHAR		; brif so -- these files have a buffer size
	MOV	FileDB.FD_VRECL,1 ;yes, set block size to one
	CMP	b$FILMOD,MD_APP ; if append, convert to MD_SQO
	jne	nochar
	MOV	b$FILMOD,MD_SQO 

nochar:
	MOV	AL,b$FILMOD	
	mov	FileDB.FD_MODE,al ;set mode
	TEST	FileDB.FD_FLAGS,FL_CHAR ; char device?
	JNZ	NoTruncate	; brif so -- don't truncate file
	TEST	AL,MD_SQO	; output?
	JZ	NoTruncate	; brif not -- don't truncate file
	OR	FileDB.FD_FLAGS,FL_TRUNC ; truncate file upon first write
NoTruncate:			
	cmp	al,MD_APP	; is it append?
	JNE	OpenExit	; brif not -- exit
	cCall	B$FILESIZE	; seek to end of file [DX|AX] = file size/pos
	MOV	CX,AX		; [DX|CX] = file size
	OR	AX,DX		; empty file?
	JZ	EmptyApp	; brif so -- don't seek back

;	An empty file opened by BASCOM10 has a 1A (^Z) at the begining of
;	a block of 128 characters which constitutes the file. Before this
;	change the SEEK routine ]was seeking to one past the end of 128
;	characters thereby retaining a ^Z at the beginning.  The new
;	algorithm first seeks to the LAST BLOCK of 128 characters and
;	then searches for the first available ^Z or end of file as the
;	case may be and starts appending from that point.

	SUB	CX,128		; get the last block of 128
	SBB	DX,0		; characters
	JNB	appfi1		;brif file is more than 128 chars
	XOR	CX,CX		; set postion to zero
	XOR	DX,DX		
appfi1:				; [DX|CX] = new position
	CALL	B$SeekFromStart ; position file pointer ([DX|AX] = result)
	XCHG	AX,CX		; [DX|CX] = file position

	MOV	b$CLOS_FDB,SI	; deallocate FDB and close file if error
	CALL	B$DISK_SINP	; read a char.  Destroys AX.
	MOV	b$CLOS_FDB,0	; finished with critical section

	JB	appfi2		; brif got a ctrl z
	ADD	CX,1		; check the next guy
	ADC	DX,0		
	JMP	SHORT appfi1	; loop around

appfi2:				; [DX|CX] = last position
	CALL	B$SeekFromStart ; back up over CTRL Z ([DX|AX] = result)
	XCHG	AX,CX		; [DX|CX] = current position

EmptyApp:
	MOV	FileDB.FD_BUFCNT,0 ; nothing in buffer
				; No need to clear FD_INBUFCNT, since it
				; will never be referenced again.
; commented out this code and associated code in DISK_LOC because LOC
; could return negative values if we SEEK to before where we started.
;	MOV	FileDB.FD_LOGREC,CX ; save starting position for append files
;	MOV	FileDB.FD_HIGHLR,DX 
OpenExit: 			; done
cEnd

ERCIOE: 				
	CALL	B$LHDALC	; deallocate FDB
	JMP	B$ERR_IOE	; device I/O error

;*** 
; DO_CLOSE -- close a file.
;
;Purpose:
;	Closes a file whose handle is in BX.  Added with [15] to save code.
;	Closes file but does not dealocate FDB upon INT 24 error.
;Entry:
;	BX = file handle
;Exit:
;	None
;Uses:
;	None
;Preserves:
;
;Exceptions:
;	None
;
;******************************************************************************

cProc	DO_CLOSE,<NEAR>
cBegin
	MOV	b$CLOS_HANDLE,BX ; close file, no FDB dealloc on INT24 error
	CALL	B$CLOSE		; close the file
	MOV	b$CLOS_HANDLE,0	; finished with critical section
cEnd


;*** 
; DiskOpenHelper -- open a file.
;
;Purpose:
;	Actually open a file whose pathname is in b$PATHNAM.
;
;Entry:
;	b$PATHNAM = pathname of file to open.
;	BX	=  mode of open
;	CX	= special open flag if OS/2
;
;Exit:
;	AX	= error code if error, 0 otherwise
;	BX	= file descriptor (if no error)
;	IF	NOT OM_DOS5
;		Carry set if error; clear otherwise.
;Uses:
;	DX
;Preserves:
;	None
;Exceptions:
;	None
;
;******************************************************************************

cProc	DiskOpenHelper,<NEAR>
cBegin

	MOV	DX,OFFSET DGROUP:b$PATHNAM ; address of pathname
	OR	BL,[b$LOCKTYPE];add access code for DOS 3.0

	CALLOS	OPEN,,,,DX,BX
	JC	OpenRet		; brif error
	XCHG	BX,AX		; BX = file descriptor
	XOR	AX,AX		; AX = 0 (no error -- clears carry)
OpenRet:
cEnd

	PAGE
;***
;B$CHKFOPEN -- check if file already open.
;
;Purpose:
;	Re-written as part of [11].
;
;	Checks for file already open by searching all FDBs, and comparing
;	their pathname fields, if they exist.
;
;Entry:
;	DI = pointer to standard processed pathname (from B$GET_PATHNAME)
;	ES = DS (only for NOT FV_FARFDB case)
;
;Exit:
;	None
;Uses:
;	AX,BX
;Exceptions:
;	B$ERR_FAO -- file already open
;****

cProc	B$CHKFOPEN,<NEAR,PUBLIC>,<SI>
cBegin
	XOR	SI,SI		;prepare to get first FDB

CHKFO_LOOP:
	CALL	B$LHNXTFIL	; point to next FDB in SI
	JZ	CHKFO_DONE	;if no more, then jump to exit
	FDB_PTR ES,SI,SI	; (ES:)SI = *FDB

;check pathname field of FDB pointed to by [SI] for match with pathname in [DI]
	CMP	FileDB.FD_DEVICE,0 ;does this FDB have a pathname field?
	JNE	CHKFO_LOOP	;brif not -- pathnames don't match
	LEA	BX,FileDB.FD_BUFFER ;BX = pathname2 address
	ADD	BX,FileDB.FD_VRECL  

	XCHG	BX,SI		;SI = pathname2 address
	PUSH	DI		;save pathname1 address

CMP_LOOP:			;compare the two pathname strings for equality
	LODSB			;load byte from filename2 into AL
	SCASB			;compare bytes
	JNZ	NEXT_FDB	;brif not equal -- try next FDB
	OR	AL,AL		;last byte?
	JNZ	CMP_LOOP	;brif not -- check another character
	JMP	B$ERR_FAO	; names matched. File already open error

NEXT_FDB:			;ZF if match, NZ if not a match
	POP	DI		;restore pathname1 address
	XCHG	BX,SI		;restore SI to FDB pointer
	JMP	SHORT CHKFO_LOOP ;try next FDB

CHKFO_DONE:
cEnd				;End of B$CHKFOPEN

;*** 
; B$OPEN_DEV and B$DEVOPN -- Special device open common routines.
;	 Re-wrote as part of [32].
;
;Purpose:
;	Allocates an FDB for a device.
;	B$DEVOPN is called when a buffer is to be allocated (if a non-zero
;	buffer size is specified), while B$OPEN_DEV is called when no buffer
;	is to be allocated (sets CX to 0)
;
;Entry:
;	[AL] = file device
;	[AH] = valid file modes
;	[CX] = size of buffer(s) (not including basic FDB size)
;	[DL] = file width
;	[BX] = file number
;
;Exit:
;	[SI] = pointer/handle to allocated FDB.
;	FD_DEVICE, FD_WIDTH, and FD_MODE fields of FDB initialized.
;
;Uses:
;
;Preserves:
;	AX,BX,DX (B$DEVOPN preserves CX)
;
;Exceptions:
;	B$ERR_BFM -- Bad file mode
;
;******************************************************************************


cProc	B$OPEN_DEV,<PUBLIC,NEAR> 
cBegin				
	XOR	CX,CX		; specify no buffer to be allocated
cEnd	nogen			; fall through into B$DEVOPN

cProc	B$DEVOPN,<PUBLIC,NEAR>,<CX> 
cBegin				

	TEST	[b$FILMOD],AH	;Check for valid file mode
	JZ	ercbfm1 	;  Bad file mode

	JCXZ	NO_BUFFER	;brif buffer not requested
	ADD	CX,FDB_EXTRA	;add in space for extra FDB fields
NO_BUFFER:
	XCHG	BX,CX		;put length in BX, file number in CX
	ADD	BX,FDB_SIZE	;set size of data block needed
	PUSH	DX		; save file width
	MOV	DL,LH_FILE	;set type of entry to allocate
	CALL	B$LHALC_CPCT	; compact local heap, and allocate FDB
	POP	DX		;restore file width
	XCHG	BX,CX		;[BX] = file number
	MOV	CL,b$FILMOD	;get file mode of open
	MOV	FileDB.FD_MODE,CL ;and save it in the FDB

	MOV	FileDB.FD_DEVICE,AL ;set file device
	MOV	FileDB.FD_WIDTH,DL  ;set file width
cEnd				


ercbfm1: JMP	B$ERR_BFM	;Bad file mode

	SUBTTL	open interface -- B$OPEN & B$OOPN
	page
;***
;B$OPEN -- open a disk file using new syntax
;void B$OPEN(sd *psdName,I2 channel,I2 cbRecord,U2 ModeAccessLock)
;
;Purpose:
;	This interface is for opening a file using the syntax as follows:
;
;	OPEN "filespec" [FOR mode] [ACCESS access] [locking] AS [#] filenum
;								    [LEN=recl]
;	where	mode	={INPUT, OUTPUT, APPEND, RANDOM},
;		access	={READ, WRITE, READ WRITE}
;		locking ={SHARED, LOCK {READ | WRITE | READ WRITE} }.
;	If FOR clause is omitted, the default file mode is random.
;
;	In BASCOM 2.0, multi-interfaces are used.  The usage is as follows:
;
;	OPEN "filespec" [FOR mode] [sharing] AS [#]filenum [LEN=record len]
;		|	    |		|	    |		|
;		|	  $DKO	      $DKA	    |		|   $DKM
;		|___________________________________|___________|_____|
;
;	Where $DKO & $DKA set flags and do some checks, and $DKM dispatchs
;	to the open routine.
;
;	In BASCOM 3.0, a single interface is used, B$OPEN, which has all
;	opening information in the stack when entering.  The order of
;	parameters are different from the one above.  The correct order is
;	defined in "Entry" section.
;
;	When entering from the compiler, the value of access is different
;	from the actual needs, so that the modification is necessary.  The
;	reason for making the value different is to detect more easily the
;	legal use of access & locking clauses (network features).
;
;	sdName/*psdName	sd for file name
;
;	channel 	1-255		valid
;			0,256-65535	illegal function call
;
;	cbRecord	0		illegal function call
;			1-32767 	valid
;			32767-65534	illegal function call
;			65535		default (-1)
;
;	* Note: LEN=0 in the statement now gives 'illegal function call'
;
;	ModeAccessLock (the following input values are adjusted to
;		    correspond to those used in the runtime)
;
;	    mode	MD_SQI		=1h	input
;			MD_SQO		=2h	output
;			MD_RND		=4h	random
;			MD_DEFAULT	=4h	default (to random)
;			MD_APP		=8h	append
;			MD_BIN		=20h	binary		[29]
;
;	    access	ACCESS_DEFAULT	=  0h	no access clause
;			ACCESS_READ	=100h	access read
;			ACCESS_WRITE	=200h	access write
;			ACCESS_BOTH	=300h	access read write
;
;	* the actual access value needed by DOS func. call for opening are
;	  0, 1 & 2 for access read, access write and access read write
;	  respectively.
;
;	    lock
;			LOCK_COMPATIBLE =0000h	compatible mode
;			LOCK_DEFAULT	=0000h	no lock clause
;			LOCK_BOTH	=1000h	lock read write
;			LOCK_WRITE	=2000h	lock write
;			LOCK_READ	=3000h	lock read
;			LOCK_SHARED	=4000h	shared (lock none)
;
;	Symbols listed above will be available in a runtime include file
;	called "runtime.inc".
;
;	Algorithm -- Pseudo C code
;
;	B$OPEN(file_name,file_num,record_length,ModeAccessLock)
;	sd	*file_name
;	int	file_num,record_length
;	ushort	ModeAccessLock
;	{
;	    b$ACCESS = ModeAccessLock >> 8 & 0x0F;	/* save access */
;	    b$LOCKTYPE = ModeAccessLock >> 8 & 0xF0;	/* save lock */
;	    if (access || lock) 	/* has access or locking clause ? */
;		if DOSVER < 3.0 	/* dos version has to be above 3.0 */
;		    error("illegal function call")
;	    openit(AX_open_mode,BX_file_num,CX_rec_length,DX_sd_filename)
;	}
;Entry:
;	Parameters were pushed in the stack.
;	sd	sdName		(file name)
;	int	Channel 	(file number)
;	int	cbRecord	(record length)
;	ushort	ModeAccessLock	(file mode, access right, locking status)
;Exit:
;	b$PTRFIL is reset
;Uses:
;	none
;Exceptions:
;	(1) file name error	-- Bad file name (B$ERR_BFN)
;	(2) access & locking	-- Illegal function call (B$ERR_AFE)
;				-- Path/file access error
;				-- Permission denied
;	(3) file number 	-- illegal file number (B$ERR_INF)
;	(4) general		-- File already open (B$ERR_FAO)
;				-- File not found (B$ERR_FAO)
;*******************************************************************************

cProc	B$OPEN,<PUBLIC,FAR>
	ParmSD	sdName		; sd to filename
	ParmW	Channel 	;I2, file #
	ParmW	cbRecord	;I2, record len
	ParmW	ModeAccessLock	;U2, file mode, access method, sharing status
cBegin
	MOV	AX,ModeAccessLock	; get lock/access/mode info
	MOV	BL,AH		;get the lock type
	AND	BL,0F0H 	;isolate lock from access
	AND	AH,0FH		;isolate access from lock
	MOV	[b$ACCESS],AH	;save access
	MOV	[b$LOCKTYPE],BL	; save it
	OR	AH,BL		;has access clause or locking clause ?
	JZ	DISPATCH	;Brif not, go dispatch
	cCall	B$DOS3CHECK	;must be 3.0 or above
	JB	ERCAFE		;Brif not, give advanced feature call
DISPATCH:
	XOR	AH,AH		;isolate mode from lock/access
	MOV	BX,Channel	;file number in BX
	MOV	CX,cbRecord	;record length in CX
	GetpSD	DX,sdName	; [DX] = psdName
	cCall	B$OPENIT	;dispatch to actual open routine
cEnd				;end of B$OPEN

ERCAFE: JMP	B$ERR_AFE	;give advanced feature error

	page
;***
;B$OOPN -- old open form
;void B$OOPN(U2 mode, I2 channel, SD sdName, I2 cbRecord)
;
;Purpose:
;	Simular to B$OPEN, B$OOPN is the single interface for old open form.
;	(refer to the procedure head comments of B$OPEN for detail)
;
;	In BASCOM 2.0, the mode passed in is a *sd to the mode string, whereas
;	in BASCOM 3.0, the mode passed in is an U2, which is simular to B$OPEN.
;
;	Algorithm -- pseudo C code
;
;	B$OOPN(psdMode, channel, sdName, cbRecord)
;	ushort	mode
;	int	channel,cbRecord
;	SD	sdName
;	{
;	    b$ACCESS=b$LOCKTYPE=0;	/* no access or locking clause */
;
;	    substitute mode of "I" into MD_SQI
;	    substitute mode of "O" into MD_SQO
;	    substitute mode of "R" into MD_RND
;	    substitute mode of "A" into MD_APP
;	    substitute mode of "B" into MD_BIN;		[29]
;
;	    openit(AX_open_mode,BX_file_num,CX_cbRecord,DX_sd_filename)
;	}
;Entry:
;	Parameters were pushed in stack.
;	SD	sdMode		(file mode)
;	int	Channel 	(file number)
;	SD	sdName		(file name)
;	int	cbRecord	(record length)
;Exit:
;	b$PTRFIL is reset
;Uses:
;	per convention
;Exceptions:
;	(1) file name error	-- Bad file name (B$ERR_BFN)
;	(2) access & locking	-- Illegal function call (B$ERR_AFE)
;				-- Path/file access error
;				-- Permission denied
;	(3) file number 	-- illegal file number (B$ERR_INF)
;	(4) general		-- File already open (B$ERR_FAO)
;				-- File not found (B$ERR_FAO)
;*******************************************************************************

cProc	B$OOPN,<PUBLIC,FAR>

	ParmSD	sdMode		:[33] sd, file mode
	ParmW	Channel 	;I2, file number
	ParmSD	sdName		:sd, file name
	ParmW	cbRecord	;I2, record length
cBegin
	MOV	[b$ACCESS],ACCESS_DEFAULT	;default compatible mode
	MOV	[b$LOCKTYPE],LOCK_DEFAULT	; default no locking clause
	GetpSD	BX,sdMode	; DX = *sd of file mode
	MOV	CX,[BX] 	;get the length
	JCXZ	ERCBFM1 	;Brif length = 0 -- "bad file mode"
	PUSH	BX		;save sd
	MOV	BX,2[BX]	;get the pointer to the first character
	MOV	BL,BYTE PTR [BX];BL has the character
	AND	BL,0DFH 	;convert lower case to upper case
	MOV	AX,MD_SQI	; MD_SQI = 1
	CMP	BL,"I"		;is input ?
	JZ	ModeSet 	;Brif yes
	INC	AX		; MD_SQO = 2
	CMP	BL,"O"		;is output ?
	JZ	ModeSet 	;Brif yes
	MOV	AL,MD_RND	; MD_RND = 4
	CMP	BL,"R"		;is random ?
	JZ	ModeSet 	;Brif yes
	MOV	AL,MD_APP	; MD_APP = 8
	CMP	BL,"A"		;is append ?
	JZ	ModeSet 	; Brif yes
	CMP	BL,"B"		; is binary ?
	JNZ	ERCBFM1 	;Brif not, give "Bad file mode"
	MOV	AL,MD_BIN	; MD_BIN = 20H
ModeSet:
	POP	BX		;recover sd
	cCall	B$STDALCTMP	;dealloc if temp sd
	MOV	BX,Channel	;file number in BX
	MOV	CX,cbRecord	;record length in CX
	GetpSD	DX,sdName	; DX = *sd of file name
	cCall	B$OPENIT	;dispatch to actual open routine
cEnd				;end of B$OOPN

sEnd	RT_TEXT

end
