	TITLE	RTMLOAD - Runtime module loader
;***
; RTMLOAD - DOS 2/3 Runtime Module Loader
;
;	Copyright <C> 1986, Microsoft Corporation
;
; Purpose:
;	This module is linked to a BASCOM-generated object
;	file created with the RTM option (no /O).  This code
;	will load the RTM EXE directly and place the code in
;	high memory and the data to the compiled program DGROUP.
;	The RTM code will then finish the initialization and
;	start execution of the compiled program.
;
;	This module is also used to load a user libraries EXE file
;	into high memory, along with it's symbol table, for the
;	QB4 interpreter.  QB4 user libraries are very similar to the
;	compiler DOS 3 RTM except that user libraries also contain
;	a symbol table concatenated to the end of the file.
;
;******************************************************************************

INCLUDE	switch.inc
INCLUDE	rmacros.inc			

	USESEG	<_DATA>			

INCLUDE seg.inc				
INCLUDE	addr.inc
INCLUDE baslibma.inc
INCLUDE files.inc			
INCLUDE array.inc			
INCLUDE rtps.inc			
INCLUDE ulib.inc			
INCLUDE const.inc			
INCLUDE string.inc


CondEnd	MACRO	entrypoint		; generate END
	END				
	ENDM				

	CreateSeg	LOADRTM,LOADRTM,BYTE,	PUBLIC, CODE		
	CreateSeg	LOADRTMEND, LOADRTMEND, PARA, PUBLIC, CODE	
	CreateSeg	CODE,	UL_TEXT,BYTE,	PUBLIC, CODE		


INCLUDE idmac.inc			

;	QBI message numbers used to call QBI message mapper

	EXTRN	ER_DMA:ABS		;Dos Arena error
	EXTRN	ER_ULM:ABS		;Memory allocation error
	EXTRN	ER_ULO:ABS		;Out of memory error
	EXTRN	ER_ULI:ABS		;Invalid user library format error
	EXTRN	ER_ULD:ABS		;Disk IO error
	EXTRN	ER_ULF:ABS		;Cannot find file error (if redirected IO)
	EXTRN	ER_ULE:ABS		;Error message preamble
	EXTRN	ER_ULG:ABS		;Error message postamble
	EXTRN	ER_ULP:ABS		;Path prompt preamble
	EXTRN	ER_ULT:ABS		;Path prompt postamble

	externFP B$END			;Quick exit routine
	externFP B$Pause		;Displays - Hit any key to continue
	externFP B$Shell		;QBC Exec helper
	externFP B$ShellSetup		;Prepares runtime for QBC Exec
	externFP B$ShellRecover 	;Recovers state after QBC exec
	externFP B$ULDataRelease	;releases data images
	externFP B$ULTerm		;calls UL terminators
	externFP B$ULGetCommon		;search for compiled common
	externFP B$ITCR 		; print CR-LF 
	externFP OpenCheckDrive		
	externFP FindAndOpenFile
	externFP B$FHAllocFAR		
	externFP B$FHDeallocFAR 	
	externFP __ctermsub		; terminates math and restores
					; divide-by-zero interrupts


INCLUDE	exehdr.inc		;structure for EXE file header area



sBegin	_DATA			
	externW	__psp		;program prefix
	externB bufStdMsg	;QBI message buffer
	externW b$pBC_SAB	;ptr to BC start address table
	externW __asizds	;max DGROUP space
	externW STKHQQ		;C low water mark for stack
	externW __atopsp	;top of allocated stack space
	externW b$pend 	;bottom of allocated stack space
	externW b$pendchk	;low water mark for stack
	externW b$ULSymSeg	;User Library symbol table segment
	externW b$ULDataLength ;length of UL static data
	externW b$ULDataStart	;offset of UL static data in DGROUP
	externW b$FHDStart	;terminating Far heap descriptor
	globalW b$pULVars,0	;address of shared data defined in UL
	externB fDebugScr	; address of shared data defined in UL
	externB b$Buf1		; general purpose filename buffer
	externB qbIniFName	; Literal "qb.ini"
	externW bdExePath	; bd for user-specified exe path
	externW bdLibPath	; bd for user-specified lib path
	externW bdInclPath	;bd for user-specified include path
QBINC	db	'_QB_INC='	;our own environment variable name
	cbQBINC = $ - QBINC	;length of the string

	public	CB_bufPath	
	public	CB_bufLib	

	globalB bufPath,<"PATH">	
	CB_bufPath	=   $ - bufPath 
	globalB bufLib,<"LIB">
	CB_bufLib	=   $ - bufLib	
	globalB bufCurDir,<".\",0>      



sEnd	DATA



assumes	CS,LOADRTM
assumes	DS,LOADRTM
assumes	ES,NOTHING

sBegin	LOADRTM


;	Data for runtime module loader.

EHBuffer	DB	SIZE ExeHeader DUP(?) ;buffer for RTM EXE header

	externNP ListStdMsgRtmNear	;QBI message mapper

ULBuffer	DB	SIZE UserLib_Hdr DUP(?) ;buffer for UL header


ProgramPSP	DW	1 DUP(?)	;PSP of program

RtmCodeSeg	DW	1 DUP(?)	;local copy of code Seg
RtmCodeSize	DW	1 DUP(?)	;size of RTM code

ULSymSeg	DW	1 DUP(?)	;segment of user lib symbol table
ULSymSize	DW	1 DUP(?)	;size of user lib symbol table
DbPub		ULSquishSize		
DbPub		ULCodeSquishSize	
ULSquishSize	DW	1 DUP(?)	;byte count of compressed leading
					;  zeros in QLB, both DATA and CODE
ULCodeSquishSize DW	1 DUP(?)	;byte count of compressed leading
					;  zeros at start of QLB code

ULCheckSum	DW	1 DUP(?)	;check sum for user library
fQBCExec	DB	0		;non-zero if QBC compile in progress
fULQBCReload	DB	0		;non-zero if we need to reload
					;UL because of QBC exec.
fNoIntHooked	DB	0		; non-zero if no interrupts are
					; hooked (during make-exe) and
					; __ctermsub shouldn't be done upon
					; error.

PathEnvPtr	LABEL	DWORD		
RelocBufPtr	LABEL	DWORD

SavedSSinCS	LABEL	WORD		
PathEnvOffset	LABEL	WORD
RelocBufOffset	LABEL	WORD
		DW	1 DUP(?)	;offset to ENV PATH or reloc item

SavedSPinCS	LABEL	WORD		
PathEnvSegment	LABEL	WORD
RelocBufSegment	LABEL	WORD
		DW	1 DUP(?)	;segment to ENV PATH or reloc item

QBCDataBlock	STRUC
	CommandList	DW	?	;Set by UI - psd of commands to exec.
	fErrorCode	DW	?	;Set by Loader - non zero if an error
					;occurred in one of the execs (contains
					;error code returned by the exec.
QBCDataBlock	ENDS

PUBLIC	b$QBCData
b$QBCData	QBCDataBlock<>		;QBCData block - defined in LOADRTM seg


QBExe	DB	'QB.EXE',0		;name of QB interpreter
	EXEExt	EQU OFFSET QBExe + 2	;pts to .EXE extension


	QBExeLength = $ - QBExe 	;length of the file name
LibString	DB	"LIB="		;environment variable for LIB
	LibStringLength = $ - LibString 


Filename	DB	"QB.QLB",0	;default userlib filename
	FilenameLength	= $ - Filename	;length of just the filename
	QLBExt EQU OFFSET Filename + 2	;pts to .QLB extension

;	The following three items are order-dependent.

PUBLIC	b$ULfsd
PUBLIC	b$ULNameLen
PUBLIC	b_ULfsd 			;Interpreter Reachable Label
PUBLIC	b_ULNameLen			;Interpreter Reachable Label

b_ULfsd LABEL	WORD			;Interpreter Reachable Label
b$ULfsd	DW	OFFSET Filename		;Offset of user lib name
		DW	SEG LOADRTM	;Segment of user lib name
b_ULNameLen	LABEL	WORD		;Interpreter Reachable Label
b$ULNameLen	DW	FilenameLength	;Length of user lib name


;
;Definition of Bits for fUserPrompt
;

F_SAVED  = 1 ; We have saved the QLIB name (TRUE) vs not been saved (FALSE)
F_PRPATH = 2 ; We should print path with msgs (TRUE) vs. base name only (FALSE)

fUserPrompt	DB	0		; Flags for loading a file.


;	The following string descriptors are order dependent.

sdNull	DW	szNullLength,OFFSET szNull ;string desc for NULL
sdStack DW	szStackLength,OFFSET szStack ;string desc for STACK
sdSym	DW	szSymLength, OFFSET szSym  ;string desc for SYMBOLS
sdBSA	DW	szBSALength, OFFSET szBSA  ;string desc for BC_SAB


szNull	DB	'NULL',0
	szNullLength = $ - szNull

szStack DB	'STACK',0		
	szStackLength = $ - szStack	

szSym	DB	'SYMBOL',0
	szSymLength  = $ - szSym

szBSA	DB     'BC_SAB',0
	szBSALength = $ - szBSA

;	String descriptor for b_ULVars (shared data defined in UL).

sdULVars DW	szVarLength,OFFSET szVars ;string desc for b_ULVars

szVars	DB	'B_ULVARS',0		
	szVarLength = $ - szVars	

; Note:  The Name Buffer Length only needs to be a byte.  However by
; wasting one byte here, we save several bytes later in the file by
; not having to clear CH when we do a MOV CX,FNameBuffLen

	MaxFNameBuff = 13		; Maximum length of a filename

	DbPub	FNameBuff
	DbPub	FNameBuffLen
FNameBuff	DB  MaxFNameBuff DUP(?) ;Buffer for filename as it is created
FNameBuffLen	DW  1 DUP(0)		;Length of FNameBuff (must be inited)

;	The following three items are order-dependent.
	DbPub	SpecBuffer
InputMaxLen	DB	SpecBufLength	;max length for DOS 0AH input
InputStrLen	DB	1 DUP(?)	;length returned by DOS 0AH input
SpecBuffer	DB	FILNAML DUP(?)	;buffer for RTM file specification
	SpecBufLength	= FILNAML -1 -FilenameLength ;max path that will fit

;	Error messages are defined in the include file for localization.

INCLUDE	messages.inc

;***
;StartupStub - runtime module startup code (RTM Only).  Added with [41].
;
;Purpose:
;	This code is executed BEFORE the C startup.  The RTM is loaded
;	(if the program has not been chanined to), and the C startup (in
;	the RTM is jumped to).
;Entry:
;	DS = ES = PSP of new program.
;Exit:
;	RTM loaded if not chaining.
;Uses:
;	All.
;Exceptions:
;	None
;******************************************************************************


;***
; B$RtmLoad - runtime module loader (RTM Only)
;
;Purpose:
;	This code is executed BEFORE the C startup if the program has
;	not been chained.  The RTM is loaded and DGROUP is built.
;Entry:
;	SS = DGROUP of new program.
;	ES = LOADRTM
;Exit:
;	DS, ES destroyed.
;Uses:
;	All
;Exceptions:
;	Various errors given if error occurs in loading RTM.
;******************************************************************************


;***
; B$ULLoad - User library loader and initialization (UL Only)
;
;Purpose:
;	Added as part of revision [10].
;	This code is executed after the C startup.  QB4 has parsed the
;	command line and called B$IINIT to init the runtime.  B$IINIT
;	calls B$ULLOAD to load a user library.  B$ULLoad will determine
;	if a library is to be loaded, load it, and build DGROUP.
;Entry:
;	DS = DGROUP of new program.
;	SS:SI = ptr to command parameters
;		[SI]   - UlibNameOffset
;		[SI+2] - UlibNameSegment
;		[SI+4] - cbLibName
;	If [SI] = 0, then no /L was found
;	If [SI] = FFFF, then /L without filename found
;	else [SI+2]:[SI] = far pointer to filename, [SI+4] = length of filename
;
;Exit:
;	DS = ES = DGROUP of new program.
;	BP = 0
;	b$pULSyms = far pointer to symbol table for user library.
;
;Uses:
;	All.
;Exceptions:
;	Initialization errors:
;		"memory allocation" error
;		"disk I/O" error
;		"invalid format" error
;		"out of memory" error
;		"DOS memory arena corrupted" error
;******************************************************************************

;	Assume C startup has been done - program memory has been
;	cut back to top of STACK.

;	Code for user library loader.  Set up segment registers.


;*** 
; ULLoad - Core user library loader (UL Only)
;
;Purpose:
;	Added as part of revision [18].
;	Does the core user library loading.
;Entry:
;	DX:SI = Filename to load
;	CX    = Length of that file
;Exit:
;	AX - end of static data after load.
;Uses:
;	All.
;Exceptions:
;	Initialization errors:
;		"memory allocation" error
;		"disk I/O" error
;		"invalid format" error
;		"out of memory" error
;		"DOS memory arena corrupted" error
;******************************************************************************


;***
;	Standalone error messages - output and exit to DOS. (RTM and UL)
;
;******************************************************************************

NoFileError:			
	MOV	AX,ER_ULF	;get error number
	JMP	SHORT PrintError ;jump to give "cannot find file" error
MemAllocError:
	MOV	AX,ER_ULM	 ;get error number
	JMP	SHORT PrintError ;jump to give "memory allocation" error
OutOfMemError:
	MOV	AX,ER_ULO	 ;get error number
	JMP	SHORT PrintError ;jump to give "out of memory" error
ArenaBadError:
	MOV	AX,ER_DMA	 ;get error number
	JMP	SHORT PrintError ;jump to give "Arena trashed" error
InvalidError:
	MOV	AX,ER_ULI	 ;get error number
	JMP	SHORT PrintError ;jump to give "invalid userlib" error
DiskIOError:
	MOV	AX,ER_ULD	 ;get error number give "UL disk I/O" error

assumes DS,LOADRTM

PrintError:
	PUSH	SS		
	POP	DS		;set DS to current DGROUP

assumes DS,DGROUP


	PUSH	AX		;save error number
	MOV	AX,ER_ULE	;get number of UL error message preamble
	CALL	PrintMessage	;print message preamble
	CALL	PrintFileName	;print file name
	MOV	AX,ER_ULG	;get number of postamble
	CALL	PrintMessage	;print message postamble
	POP	AX		;recover error number
	CALL	PrintMessage	;Print error message
	CMP	b$ULSymSeg,0	;non zero if we have finished initialization
	JZ	Scram		;brif we haven't finished initialization
	JMP	B$END		;print error message and terminate

labelFP <PUBLIC,B$IScram>	;Kill QBI entry point

Scram:
	cmp	cs:fNoIntHooked,0	; do we have any interrupts hooked?
	jnz	NoIntHooked		; brif not
	push	ss			; DS = DGROUP
	pop	ds			
	cCall	__ctermsub		; de-install math and divide by zero
NoIntHooked:				; interrupts.
	MOV	AX,(C_EXIT SHL 8)+1;dos exit with error return code
	INT	21H		;exit

assumes DS,NOTHING



;*** 
; B$ShellEntry - runtime EXEC routine for SHELL (RTM only)
;
;Purpose:
;	[Added within revision 5.]
;	Since the runtime module code is discarded during a SHELL
;	operation, this routine deallocates the RTM code, performs
;	the EXEC specified, and then reloads the RTM code with fixups.
;Entry:
;	Parameters set for DOS EXEC call.
;Exit:
;	AX = post-INT 21 value.
;	CF = post-INT 21 value.
;Uses:
;	All.  RTM saved the registers it needs.
;Exceptions:
;	InvalidError if bad EXE format.
;	DiskIOError if error during RTM disk operations.
;******************************************************************************


;***
; B$ULReload - reload a user library. (UL only).
;
;Purpose:
;	Added as part of revision [10].
;	Reloads user library code and data, effectively
;	reinitializing static data for subsequent execution.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	AX, BX, CX, DX.
;Exceptions:
;	Initialization errors:
;		"disk I/O" error
;		"invalid format" error
;****


cProc	B$ULReload,<PUBLIC,FAR>	
cBegin				
cEnd				


;***
; ULSaveName - Allocate a high memory block for UL name (UL Only)
;
;Purpose:
;	Added as part of revision [12].
;	Allocate a high memory block to save user library
;	file name in.  Update b$ULfsd to point to the
;	allocated block.
;Entry:
;	Filename is in SpecBuffer
;	Length is in InputStrLen
;
;Exit:
;	[b$ULfsd] = 0
;	[b$ULfsd+2] = allocated segment
;	[b$ULNameLen] = byte size of user library name string
;Uses:
;	BX, CX, DX, SI, DI, BP
;
;Preserves
;	AX
;
;Exceptions:
;	DOS Memory allocatation error.
;	Out of memory error.
;****


;***
; ULAllocHighSeg - Allocate high memory block <= 64K from (UL only)
;
;Purpose:
;	Added as part of revision [12].
;	Allocate a high memory block from DOS.	The block
;	will be a max of 64K in length.
;Entry:
;	DX = byte size of block to allocate
;Exit:
;	BP = segment of allocated block
;Uses:
;	CX, DX, DI.
;Exceptions:
;	DOS Memory allocatation error.
;	Out of memory error.
;****


;***
; PrintMessage - print a numbered message. (UL only)
;
;Purpose:
;	Added with revision [19].
;	Takes the numbered message, calls a QB helper routine to
;	put the message in bufStdMsg and then prints the message to
;	through DOS.
;Entry:
;	AX - number of message to print.
;Exit:
;	None.
;Uses:
;	AX,CX,SI.
;Exceptions:
;	None.
;******************************************************************************

cProc	PrintMessage,<NEAR>,<DS>
cBegin

	PUSH	SS
	POP	DS

assumes DS,DGROUP

;	AX has message number.

	PUSH	ES		;preserve ES across ListStdMsgRtmNear
	cCall	ListStdMsgRtmNear ;AX = char count, bufStdMsg = msg text
	POP	ES
	MOV	SI,OFFSET DGROUP:bufStdMsg ;[DS:SI] = ptr to message text
	XCHG	AX,CX		;[CX] = message length
	JMP	SHORT PrintFileLoop ;jump and print message
cEnd	<nogen>

;***
; PrintFileName - print file name in SpecBuffer or FNameBuff. (UL only)
;
;Purpose:
;	This routine prints the current UserLib Filename.  If F_PRPATH
;	in fUserPrompt is true, then we will a print path with the name
;	(both comming from SpecBuffer).  Otherwise, we will print the
;	name only (from FNameBuff).
;
;	Since FNameBuffLen is statically initialized to 0, this routine
;	will work even if no name has yet been parsed.
;
;Entry:
;	if (fUserPrompt & F_PRPATH)
;	    FNameBuff	 - filename to print.
;	    FNameBuffLen - length of file name.
;	else
;	    SpecBuffer	 - path and filename to print.
;	    InputStrLen  - Length of SpecBuffer.
;Exit:
;	None.
;Uses:
;	AX,CX,SI.
;Exceptions:
;	None.
;******************************************************************************

cProc	PrintFileName,NEAR,DS ; Added
cBegin
	PUSH	CS
	POP	DS

assumes DS,LOADRTM

	TEST	fUserPrompt,F_PRPATH	;Should we print path?
	JE	NoPrintPath		;No, get filename from FNameBuff

	MOV	SI,OFFSET SpecBuffer	;get ptr to file name and path
	XOR	CH,CH			;Clear CH for length
	MOV	CL,InputStrLen		;Get length of filename and path
	JMP	SHORT PrintFileMain	;Go print the string

NoPrintPath:
	MOV	SI,OFFSET FNameBuff	;get ptr to filename
	MOV	CX,FNameBuffLen 	;get length of filename

PrintFileMain:
	JCXZ	EmptyFileName	;Eliminate degenerate case

PrintFileLoop:
	LODSB			;get char in AL
	MOV	DL,AL		;[DL]=char to output
	MOV	AH,2		;DOS char output function
	INT	21H		;print char
	LOOP	PrintFileLoop	;repeat until all chars are printed

EmptyFileName:
cEnd

;***
; B$IInitPrint - print a numbered message before runtime inited. (QBI only)
;
;Purpose:
;	Added with revision [20].
;	Takes the numbered message, calls a QB helper routine to
;	put the message in bufStdMsg and then prints the message to
;	through DOS.
;Entry:
;	AX - number of message to print.
;Exit:
;	None.
;Uses:
;	AX,CX,SI.
;Exceptions:
;	None.
;******************************************************************************

cProc	B$IInitPrint,<PUBLIC,FAR>
cBegin

;	AX has message number.

	cCall	PrintMessage	;print the numbered message
cEnd


LoadRtmInfoInvalid:
	JMP	InvalidError	;jump to report invalid EXE error

;***
; LoadRtmData - load the runtime data segment (RTM only)
;
;Purpose:
;	[Extracted from LoadRtmFile in revision 5.]
;	From the start of the load image, load the data segment
;	into the program DGROUP.  The region loaded starts at
;	segment CONST up to, but not including, segment COMMON.
;Entry:
;	BX - handle of opened RTM file.
;Exit:
;	DGROUP from CONST up to COMMON overlaid.
;Uses:
;	AX, CX, DX, SI, DI, BP.
;Exceptions:
;	None.
;******************************************************************************


;*** 
; LoadULData - load the user library data into DGROUP (UL only)
;
;Purpose:
;	Added as part of revision [10].
;	Load data from the user library data segment into DGROUP.
;	The region loaded starts from the end of the DGROUP
;	static data to the end of the user library static data.
;	DGROUP will be grown by the size of the user library
;	static data, and the stack will be moved into the highest
;	memory of the grown DGROUP.  Any pointers into the stack
;	will be invalidated.
;Entry:
;	BX - handle of opened RTM file.
;	ES = DS - LoadRtm segment
;Exit:
;	AX = new end of DGROUP static data
;	Stack moved, and user library data read into hole vacated
;	by the stack.
;	__atopsp - points to new beginning of stack
;	b$pend  - points to new end of stack
;	b$pendchk - points to new stack check boundary
;Uses:
;	AX, CX, DX, SI, DI, BP.
;Exceptions:
;	Out of memory.	Invalid UL format.
;******************************************************************************


;***
;ReloadULData - load/reload UL data into hole created in DGROUP (UL Only)
;
;Purpose:
;	Added with revision [10].
;	Loads initial copy of data, or reloads copy of data for each RUN.
;	The user library data is recopied into DGROUP.	This will
;	cause any preinited static data to be correct for a subsequent
;	run.
;
;	If blank COMMON is in preinited data, this procedure would
;	reinitialize it, trashing user's values across CHAIN.  To avoid
;	this, a check is done for the existence of blank COMMON.  If found,
;	the data is moved in two chunks: "Part1" is the data before the
;	COMMON block, "Part2" is the data after the COMMON block.  This
;	will preserve the blank COMMON in the middle for CHAIN.
;
;	Note: Not loading COMMON here is ok for RUN also, because it
;	      would only be zeros, and the RUN initialization will
;	      zero the common for us.
;
;Entry:
;	CX - start of DGROUP area to copy UL data into
;	DI - byte size of UL data
;	DS - DGROUP
;Exit:
;	data copied into DGROUP (unfixed up)
;Uses:
;	AX, CX, DX, SI, DI, BP, DS, ES.
;Exceptions:
;	None.
;****

;*** 
; GetRtmCodeSize - compute the runtime module code size (RTM only)
;
;Purpose:
;	(Mostly rewritten with revision [60].)
;	[Extracted from LoadRtmFile in revision 5.]
;	Compute the size of the load image from the EXE header
;	information.  Then use the CODE segment value from the
;	frame buffer to compute the code size of the runtime module.
;Entry:
;	EHBuffer has EXE header.
;	RLBuffer has entry point frame.
;Exit:
;	DX:AX - code size in bytes.
;Uses:
;	AX, CX, DX.
;Exceptions:
;	None.
;******************************************************************************


;*** 
; LoadRtmCode - load the rtm code, or user lib syms (RTM Only)
;
;Purpose:
;	[Extracted from LoadRtmFile in revision 5.]
;	Read the code section of the runtime module
;	into memory.
;Entry:
;	BX - handle of opened RTM file.
;	DX:AX - size in code segment in bytes.
;	BP - segment to load code.
;Exit:
;	Block at segment BP has RTM code with no fixups.
;Uses:
;	AX, CX, DX, SI, DI.
;Exceptions:
;	DiskIOError if error during RTM file reading.
;******************************************************************************


;***
; LoadULCode - load the user lib code into high memory (UL Only)
;
;Purpose:
;	Added as part of revision [10].
;	Compute the size of the user library code. Allocate
;	memory from DOS as high as possible.
;	Read the code sections of the user lib
;	into their appropriate locations in memory.
;Entry:
;	BX - handle of opened RTM file.
;Exit:
;	Block at segment RtmCodeSeg has UL code with no fixups.
;	RtmCodeSize has paragraph size of UL code.
;Uses:
;	AX, CX, DX, SI, DI, BP.
;Exceptions:
;	Memory error if memory could not be allocated.
;	DiskIOError if error during RTM file reading.
;******************************************************************************


;***
;ReloadULCode - Load/Reload UL code into allocated space (UL only)
;
;Purpose:
;	Added as part of revision [10].
;	Loads/Reloads user library code into allocated block.
;Entry:
;	BP - segment to load code into.
;	DX - paragraph size of code to load.
;Exit:
;	The unfixed up code is loaded into the specified location.
;Uses:
;	AX, CX, DX, SI, DI, ES
;Exceptions:
;	None.
;****


;***
; LoadULSymbols - load the user lib symbol table into high memory (UL Only)
;
;Purpose:
;	Added as part of revision [10].
;	Compute the size of the user library symbol table.
;	Allocate memory from DOS as high as possible.
;	Read the symbol table of the user lib into memory.
;Entry:
;	BX - handle of opened RTM file.
;Exit:
;	Block at segment ULSymSeg has UL symbol table with no fixups.
;	ULSymSize has byte size of symbol table.
;Uses:
;	AX, CX, DX, SI, DI, BP.
;Exceptions:
;	Memory error if memory could not be allocated.
;	DiskIOError if error during RTM file reading.
;******************************************************************************


;***
; ULUpcaseSymbols - convert all symbol table names to upper case (UL only)
;
;Purpose:
;	Added as part of revision [10].
;	Converts all symbol table names to uppercase to simplify
;	and speedup symbol table lookup routines.
;Entry:
;	BP - segment of symbol table
;Exit:
;	None.
;Uses:
;	AX, SI, DI, ES.
;Exceptions:
;	None.
;****


;***
; ULAdjustDXAX - back up far ptr in DX:AX by number of bytes in ULSquishSize
;
;Purpose:
;	Because of leading-zero compression in QLib files, the file pointers
;	need to be adjusted before calling LoadRtmBlock to load the Data
;	and Symbol segments.  The number of bytes compressed from the file
;	is in ULSquishSize.  This routine takes a far pointer in DX:AX and
;	backs it up ULSquishSize bytes. (Added with revision [38].)
;Entry:
;	DX:AX - far pointer to be adjusted
;	DS:[ULSquishSize] - number of bytes for adjustment
;	DS - LOADRTM
;Exit:
;	DX:AX - adjusted far pointer
;Uses:
;	None
;Exceptions:
;	None
;******************************************************************************


;***
; FixupRtmFile - apply fixups to RTM load  (RTM only)
;
;Purpose:
;	Read the fixup items from the RTM file and apply them
;	appropriately to the code and data section loaded into
;	memory.
;Entry:
;	BX - handle of opened RTM file
;	SI - zero to fixup both code and data (used in init)
;	     nonzero to fixup just code (used in SHELL)
;	BP - segment RTM is loaded in
;Exit:
;	None.
;Uses:
;	AX, CX, DX, DI, BP, ES.
;Exceptions:
;	DiskIOError if error during disk read of RTM file.
;******************************************************************************


;*** 
; FixupULFile - apply fixups to UL load  (UL only)
;
;Purpose:
;	Added as part of revision [10].
;	Read the fixup items from the UL file and apply them
;	appropriately to the code, data, and segment sections
;	loaded into memory.
;Entry:
;	BX - handle of opened RTM file
;	CX - symbol table fixup value (0 if already done).
;Exit:
;	None.
;Uses:
;	AX, CX, DX, SI, DI, BP, ES.
;Exceptions:
;	DiskIOError if error during disk read of RTM file.
;******************************************************************************




;*** 
; AppendExt - Append def extension to Filename in SpecBuffer if needed (UL Only)
;
;Purpose:
;	Added with revision [24].
;	Appends appropriate extension to the file in SpecBuffer if an extension
;	does not already exist.  If we are opening a user library, then .QLB
;	will be appended, otherwise .EXE is appended.
;
;	In addition to appending an extension to the file in SpecBuffer,
;	this routine will also save a copy of the basename and extension
;	in FNameBuff
;
;	If SpecBuffer contains a pathname without a filename (recognized by
;	the last char being a path char), then append the filename in FNameBuff
;	to the path in SpecBuffer.
;
;	No matter what the entry conditions, on exit from this routine the
;	following will be set up:
;	  FNameBuff has null-terminated filename part of filespec
;	  FNameBuffLen has length of what is in FNameBuff
;	  SpecBuffer has null-terminated filespec (ready for dos open)
;	  InputStrLen has length of what is in SpecBuffer
;
;Entry:
;	ES:DI = pointer in spec buffer after last byte of filespec.
;Exit:
;	ES:DI = pointer to first byte of filename part of filespec.
;		(if DI = OFFSET SpecBuffer, then there is no path specified)
;Uses:
;	AX, CX, DX, SI.
;Exceptions:
;	None.
;******************************************************************************


SearchChars	DB ':','/','\','.'      ;List of chars in order of search
cbSearchChars = $ - SearchChars 	;size of list

DbPub	AppendExt
cProc	AppendExt,<NEAR>
cBegin
	MOV	AX,DI			; In case buffer is empty
	CMP	DI,OFFSET SpecBuffer	; Is the buffer empty?
	JE	JE_NoFileName		; Yes, go special-case it

	MOV	CX,cbSearchChars	;get number of chars to search for
	MOV	SI,OFFSET SearchChars	;point to list
	XOR	DX,DX

;	Search for path chars, '/' and '\'.  Remember the address of
;	the last such char found.  Then search for an extension char
;	'.'.  If the address of this char is > than the address of the
;	last path char, then the filename had an extension.  This is
;	true since it is impossible to have a '.' after a path char
;	and not have it be part of a filename (eg it cannot be part
;	of a directory spec.


AppendSearchLoop:
	PUSH	CX			;save loop count
	PUSH	DI			;save end of file name
	MOV	CX,DI
	DEC	DI			;point to last char in name
	SUB	CX,OFFSET SpecBuffer	;compute char count in SpecBuffer
	LODSB				;[AL] = search char
	STD				;search backwards in file name
	REPNE	SCASB			;search backward for char
	CLD				;go forward again
	JNE	CharNotFound		;brif we did not find character
	INC	DI			;Set DI to char before last
CharNotFound:
	INC	DI			; REPNE SCASB subtracts 1 at end.
	XCHG	AX,DI			;save address of char/start of buffer

;NOTE:	Be careful here.  The results of the Next compare are used after
;	the loop is terminated. The current algorithm ensures that the
;	instructions used between this compare and the next branch after
;	the loop is terminated do not alter the carry and/or zero flag.


	CMP	AX,DX			;Is this char after prev char?
	JB	AppendSearchNext	;brif not
	XCHG	AX,DX			;remember address of char

AppendSearchNext:
	POP	DI			;recover end of filename
	POP	CX			;recover loop count
	LOOP	AppendSearchLoop	;continue until all chars searched for


;NOTE:	See NOTE above.

	JA	AppendExtExit		;brif already have the extension

; There are two conditions in which we do not have a filename:
;	 1.) An empty buffer, in which case we wont get here.
;	 2.) A path without a name, when DX = DI

	MOV	AX,DX			; Set AX = ptr to last path char
	CMP	DI,DX			; Check for no name specified
JE_NoFileName:
	JE	NoFileName		; None given, append the old one

;	File name needs an extension appended.	If we are loading a user lib,
;	then the extension is .QLB, otherwise, we must be loading an .EXE
;	file as the result of a QBC exec.  All of these files are .EXE
;	files.

	MOV	SI,EXEExt		;get ptr to .EXE extension
	CMP	fQBCExec,CL		;are we loading a user lib?
	JNZ	AddExt			;brif not - QBC exec loads .EXE files
	MOV	SI,QLBExt		;get ptr to .QLB extension for ulib

AddExt:
	MOVSW				; add 4-byte extension to file name
	MOVSW				

AppendExtExit:

DbAssertRel	CX,Z,0,LOADRTM,<AppendExt: trying to use CX for 0 and it's not>

	XCHG	CX,AX			; AL = zero, saving AX in CX
	STOSB				; null terminate the filename
	XCHG	CX,AX			; restore AX

; The starting point to copy is AX	=> SI
; The number of characters in the name is DI - AX => CX

	MOV	CX,DI			; Calculate Length of name
	SUB	CX,AX			;	without path
	CMP	CX,MaxFNameBuff 	; Are there too many characters
					;	in filename?
	JBE	ValidLength		; No, name is valid

;	There are a couple cases where we will have a double-null terminated
;	name here.  That could create a name 1 byte longer than ValidLength,
;	but it's not really a problem.  The release code will fall through
;	and cut back the length by 1, removing 1 null.	If there is a name
;	even longer than that, it's a bug.  Assert it.

DbAssertRel	CX,E,<MaxFNameBuff+1>,LOADRTM,<AppendExt: filename is too long>

	MOV	CX,MaxFNameBuff 	; Truncate name to max length
ValidLength:
	MOV	FNameBuffLen,CX 	; Save Length of FName
	MOV	SI,AX			; DS:SI points to begining of name
	MOV	DI,OFFSET FNameBuff	; ES:DI points to FNameBuff
	REP	MOVSB			; Copy the name only into FNameBuff
	MOV	DI,SI			; DI points 1 past end of filespec

;	Set InputStrLen = length of filespec in SpecBuffer

Return:
	SUB	DI,OFFSET LOADRTM:SpecBuffer ; DI = length of filespec
	XCHG	AX,DI			; DI = return value, AX = length
	MOV	[InputStrLen],AL	; Store the length

DbAssertRelB	AH,Z,0,LOADRTM,<AppendExt: filespec longer than 127 characters>

cEnd

NoFileName:
;	If we get here, SpecBuffer has a pathname (terminated with a path char)
;	but no filename.  If we already have a filename in FNameBuff, we'll
;	append it to the path in SpecBuffer

	MOV	CX,FNameBuffLen 	; length of name in FNameBuff
	MOV	SI,OFFSET LOADRTM:FNameBuff ; source pointer
	REP	MOVSB			; append the filename
	JMP	SHORT Return		; set return value and exit

;***
; OpenFile - Open the file specified in SpecBuffer.
;
;Purpose:
;	Added with revision [13].
;	Opens the filespec in SpecBuffer
;Entry:
;	SpecBuffer set up.
;Exit:
;	if open worked
;	    PSW.C reset
;	    AX = file handle
;	else
;	    PSW.C set
;	endif
;Uses:
;	Per convention.
;Exceptions:
;	Disk IO Error.
;******************************************************************************
cProc	OpenFile,<NEAR> 	;common routine to open file
cBegin				
	MOV	DX,OFFSET SpecBuffer ;DS:DX has pointer to file specification
OpenFile2:
	cmp	ss:fDebugScr,0		; brif not initialized and not
	jz	NoCheckDrive		; displaying QB's screen
	push	ds			; save LOADRTM segment

	mov	bx,dx			; put ptr into index register
	push	ds:[bx]			; OpenCheckDrive parm = first 2
					; of file spec (possible DRIVE:)

	push	ss			; DS = DGROUP for UI call
	pop	ds			
	call	OpenCheckDrive		; if required, switch DOS's logical
					; disk drive, and display message
					; "insert disk in drive X:"
					; PRESERVES ALL REGISTERS
	pop	ds			; restore register
NoCheckDrive:				
	MOV	AX,(C_OPEN SHL 8)+0  ;DOS function to open the file for input only
	INT	21H		;perform the open - carry set if failed
cEnd

;***
; CloseFile - Close the file handle in BX (RTM and UL)
;
;Purpose:
;	Added with revision [18].
;	Closes the file handle in BX.
;Entry:
;	BX - file handle to close.
;Exit:
;	None.
;Uses:
;	AX.
;Exceptions:
;	Disk IO Error.
;******************************************************************************

cProc	CloseFile,<NEAR>
cBegin
	MOV	AH,C_CLOSE
	INT	21H			;close the file
	JC	DiskError		;brif error
cEnd

DiskError:
	JMP	DiskIOError		;report the error and die.


assumes	DS,DGROUP


;***
; ULConsisCheck - performs consistency checks on user libs (UL only)
;
;Purpose:
;	Added as part of [10].
;	Does user library consistency checking.  Verifies correct
;	segment ordering and size of hole allocated in UL for
;	QB4 static data.
;Entry:
;	SI - size of QB4 static data
;Exit:
;	SI - offset of XIB in UL data
;	DI - offset of XIE in UL data
;	CX - offset of BC_SAB in UL data
;Uses:
;	AX, BX, DX
;Exceptions:
;	Consistency errors.
;****


;***
; ULCheckNextName - Verify that next segment is expected. (UL only)
;
;Purpose:
;	Added as part of [10].
;	Checks the next segment name in the symbol table and see
;	if it matches the expected segment name.  If not then
;	segments are out of order, and a consistency error is issued.
;Entry:
;	DX - points to expected segment name string descriptor
;	ES:BX - points to previous symbol table entry
;Exit:
;	DX - advanced to next sd in segment name list
;	ES:BX - advanced to next symbol table entry
;Uses:
;	CX, SI, DI
;Exceptions:
;	Consistency error if next segment isn't the expected segment.
;****


;***
; ULFindName - Finds a specified segment name in symbol table (UL Only)
;
;Purpose:
;	Added as part of revision [10].
;	Finds the specified segment name in the symbol table.
;	If the segment name is not found, a consistency error
;	is detected.
;Entry:
;	ES:BX - points to next symbol table entry
;	SI - points to segment name to search for
;	CX - count of characters in segment name
;Exit:
;	ES:BX - points to symbol table entry containing segment name
;Uses:
;	DI
;Exceptions:
;	Consistency error if name not found.
;****



;***
; ULFindNameBegin - Finds a specified segment name in symbol table (UL Only)
;
;Purpose:
;	Added as part of revision [10].
;	Finds the specified segment name in the symbol table.
;	This routine starts searching from beginning of symbol table
;	If the segment name is not found, a consistency error
;	is detected.
;Entry:
;	ES - symbol table segment
;	DX - points to sd of segment to search for
;Exit:
;	ES:BX - points to symbol table entry containing segment name
;	AX - offset of segment that was found from DS:0
;	DX - advanced to next sd in segment name list
;Uses:
;	CX, DI
;Exceptions:
;	Consistency error if name not found.
;****


;***
; ULNextName - Get ptr to next expected segment name. (UL only)
;
;Purpose:
;	Added as part of revision [10].
;	Get next segment name from list of segment name string
;	descriptors.  Set up a ptr to the actual name in SI
;	and the length of the name in CX.  Advance DX to point
;	to next sd in list.
;Entry:
;	DX - points to expected segment name string descriptor
;Exit:
;	SI - points to zero terminated segment name.
;	CX - contains count of bytes in name pointed to by SI.
;	DX - advanced to next sd in segment name list.
;Uses:
;	None.
;Exceptions:
;	None.
;****


;***
; ULFindData - Get ptr to Data item. (UL only)
;
;Purpose:
;	Added as part of revision [15].
;	Find a data item in user library.
;Entry:
;	DX - points to string descriptor for data item.
;Exit:
;	AX - DGROUP offset of data item (0 if not found).
;Uses:
;	BX, CX, DX, SI, DI
;Exceptions:
;	Consistency error if not found.
;****

	SUBTTL QBC Exec support
	PAGE
;***
; LoadQBExe - Reload the interpreter into memory (QBI only)
;
;Purpose:
;	Added with revision [18].
;	Reloads the interpreter into memory after completing a COMPILE
;	Dialog.
;Entry:
;	BX - QB.EXE file handle.
;Exit:
;	None.
;Uses:
;	AX,CX,DX,SI,DI,BP.
;Exceptions:
;	DOS Memory allocation error.
;	Out of memory error.
;******************************************************************************

	PAGE
;***
; SearchForChar - Search for specified char in buffer (QBI Only)
;
;Purpose:
;	Added with revision [18].
;	Searches for a specified char in a buffer and returns
;	the distance from buffer start to the specified char.
;	ASSUMES THAT CHAR WILL ALWAYS BE FOUND.
;Entry:
;	AL - Char to search for.
;	ES:DI - points to buffer
;Exit:
;	CX - distance from ES:DI on entry to specified Char (not including char)
;	ES:DI - points to requested char.
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************

	PAGE
;***
; ExecCommands - Exec list of commands for COMPILE dialog (QBI Only)
;
;Purpose:
;	Added with revision [18].
;	Execs the list of commands built from the COMPILE dialog box.
;Entry:
;	envSeg = ENV seg to pass on to B$SHELL
;Exit:
;	b$QBCData.fErrorCode - non-zero if error occurred.
;Uses:
;	AX,BX,CX,DX,SI,DI,ES,BP.
;Exceptions:
;	Disk IO Error.
;******************************************************************************


	PAGE
;***
; B$ExecQBC - Exec list of commands as requested from COMPILE Dialog (QBI Only)
;
;Purpose:
;	Added with revision [18].
;	Invokes list of commands generated by the user interface in
;	response to a COMPILE dialog box.  The interpreter will be
;	released except for this loader, the error messages, and DGROUP.
;	The list of commands will then be processed.
;Entry:
;	psdCommands - ptr to sd containing command list.
;Exit:
;	AX = 0 if no error occurred.
;Uses:
;	Per Convention.
;Exceptions:
;	Out of memory error.
;	Disk IO error.
;****


;****
;B$FindFileOnPaths (pszFile, pszUserPath, pEnvVar, cbEnvVar)
;
;Purpose:
;	Moved here from uipaths.asm with revision [69].
;	Searches the current directory, the appropriate user-specified
;	search path, and the appropriate environment variable for the
;	file pszFile and returns a pointer to a fully-qualified pathfile
;	name where the file was found.	If the filename contains a path
;	then we just look in that one place and no further searching is
;	done.
;
;Input:
;	pszFile     - far pointer to null-terminated file name to open
;	pszUserPath - near pointer to user-specified path from Options menu
;	pEnvVar     - far pointer to environment variable name specifying
;			  last-chance search path
;	cbEnvVar    - length of environment variable pointed to by pEnvVar
;
;Output:
;	if successful
;	   DX:AX == pointer to pathfile name where file was found
;	else
;	   AX == 0
;
;****
cProc	B$FindFileOnPaths,<PUBLIC,FAR>,<SI>
	parmD	pszFile
	parmW	pszUserPath
	parmD	pEnvVar
	parmW	cbEnvVar
cBegin
ASSUMES DS,NOTHING

; First check for a path/file name.

	LES	SI,[pszFile]		; es:si points to file name
SearchLoop:
	LODS	BYTE PTR ES:[SI]	; read a byte
	OR	AL,AL			; end of string?
	JZ	NotPathFile		; yes, no path found, go do search
	CMP	AL,'\'                  ; found a path specifier?
	JE	PathSpecified		; yes, go try open without search
	CMP	AL,'/'			; found a path specifier?
	JE	PathSpecified		; yes, go try open without search
	CMP	AL,':'			; found a path specifier?
	JNE	SearchLoop		; no, keep searching

; If we fall through to here then the filename contains an explicit path.
; Check for existence of the specified path/file and do no more searching.
PathSpecified:
	PUSH	DS
	LDS	DX,[pszFile]		; ds:dx points to path/file name
	CALL	OpenFile2		; try to open the file
	XCHG	AX,BX			; BX = file handle (if open worked)
	MOV	AX,0			; can't use XOR because we need PSW.C
	JC	FileNotFound		; it's not there, exit with ax = 0
	PUSH	DX			; save offset of successful filespec
	CALL	CloseFile		; close the file
	MOV	DX,DS
	POP	AX			; DX:AX = ptr to successful filespec
FileNotFound:
	POP	DS
	JMP	SHORT FindFileExit	; exit

; Look for file in current directory first

NotPathFile:
; Note: We have to use fully-qualified pathspec to current directory for
;	many reasons, including checking for "File already loaded" and
;	disabling DPATH.

	PUSH	DS			; preserve ds
	PUSH	CS			
	POP	DS			; DS = LOADRTM
	MOV	SI,OFFSET LOADRTM:SpecBuffer ; point to target buffer
	PUSH	SI			; save ptr to SpecBuffer for call
	MOV	AH,19h			; "get current drive" function
	INT	21h			
	PUSH	AX			; save drive number (0 relative)
	ADD	AL,'A'			; drive number -> drive letter
	MOV	[SI],AL 		; store drive letter
	MOV	WORD PTR [SI+1],"\:"	; store ":" and "\"
	ADD	SI,3			; DS:SI points to target
	POP	DX			; DL = drive number (0 relative)
	INC	DX			; make 1 relative
	MOV	AH,47h			; "get current dir" function
	INT	21h			; get the current directory
	POP	BX			; CS:BX = ptr to SpecBuffer for call
	POP	DS			; restore DS
	JC	NotInCurDir		; current dir is invalid, the file
					;  we're looking for can't be there
	XOR	AX,AX
	cCall	B$SearchOnePath,<seg_pszFile,off_pszFile,AX,CS,BX>
	OR	AX,AX			; find file in current directory?
	JNZ	FindFileExit		; yes, exit with DX:AX = pointer

NotInCurDir:				
; Didn't find it in the current directory, try the user-specified
; search path from the options menu.

	cCall	B$SearchOnePath,<seg_pszFile,off_pszFile,AX,SS,pszUserPath>
	OR	AX,AX			; find file on that path?
	JNZ	FindFileExit		; yes, exit with DX:AX = pointer

; Didn't find it there either, try the path defined by the
; specified environment variable.

	cCall	B$SearchEnv,<seg_pEnvVar,off_pEnvVar,cbEnvVar>
	OR	AX,AX			; find the environment variable?
	JZ	FindFileExit		; no, exit with AX = 0

	XOR	BX,BX			; get a zero
	cCall	B$SearchOnePath,<seg_pszFile,off_pszFile,BX,DX,AX>

; B$SearchOnePath returns exactly what we want to return here,
; so no further processing is necessary.

FindFileExit:
cEnd

;****
;B$SearchOnePath - find a file on a search path
;
;Purpose:
;	Searches the specified path for the specified file.
;	Return a pointer to a fully-qualified pathname if search
;	is successful, otherwise return AX = 0.
;
;Input:
;	pFileName	far pointer to filename to open
;	cbFileName	length of filename
;	pSearchPath	far pointer to search path (list of directories
;				separated by ";" terminated by null)
;Output:
;	if successful
;	   DX:AX = far pointer to fully-qualified pathname
;	else
;	   AX = 0
;
;****

cProc	B$SearchOnePath,<NEAR>,<DS,SI,DI>
ParmD	pFileName
ParmW	cbFileName
ParmD	pszSearchPath

CalcLength:
	XOR	AX,AX			; get a zero for scanning
	DEC	CX			; CX = -1 for long search
	LES	DI,[pFileName]		; ES:DI points to filename
	REPNE	SCASB			; scan for a 0
	NOT	CX			; length of string (including null)
	DEC	CX			; length without null
	MOV	[cbFileName],CX 	; store that length
	JMP	SHORT HaveLength
cBegin
	MOV	CX,[cbFileName]
	JCXZ	CalcLength
HaveLength:

ASSUMES DS,NOTHING

	LDS	SI,[pszSearchPath]	; DS:SI points to search path
SearchPathLoop:
	PUSH	CS
	POP	ES
	MOV	DI,OFFSET LOADRTM:SpecBuffer ; ES:DI points to buffer
CopyPathToBuf:
	XOR	CX,CX			; initialize count of bytes moved
CopyLoop:
	LODSB				; get next byte
	OR	AL,AL			; done with list?
	JE	CopyEnd 		; yes

	CMP	AL,';'			; is it path separator?
	JE	CopyDone		; yes, copy is done
	STOSB				; put the byte in the buffer
	INC	CX			; count this byte
	XCHG	AH,AL			; save stored byte in AH
	JMP	SHORT CopyLoop

CopyEnd:
	XOR	DX,DX			; if CX=0, we want to jump with DX=0
	JCXZ	OpenError		; ran out of paths, file not found
	DEC	SI			; point to null terminator

CopyDone:
	JCXZ	CopyPathToBuf		; skip null path, go get next one
	MOV	AL,'\'
	CMP	AH,AL			; was last char backslash?
	JE	AppendFileName		; yes, don't add another
	CMP	AH,'/'			; was last char forward slash?
	JE	AppendFileName		; yes, don't add backslash
	CMP	AH,':'			; was last char from drive spec?
	JE	AppendFileName		; yes, don't add backslash
	STOSB				; append a backslash to pathname

AppendFileName:
	PUSH	DS
	PUSH	SI			; save pointer to next pathname in list
	LDS	SI,[pFileName]		; DS:SI points to filename
	MOV	CX,[cbFileName] 	; CX = length of filename
	REP	MOVSB			; append filename to pathname
	XCHG	AX,CX			; AX = zero
	STOSB				; null terminate the filename
	PUSH	ES
	POP	DS			; DS = LOADRTM
	CALL	OpenFile		; attempt to open the file
	MOV	DI,DS			; save DS in DI in case open worked
	POP	SI
	POP	DS			; get back pointer to next path in list
	JNC	FoundIt 		; found it, stop looking
	XOR	DX,DX			; DX=0 in case we go to OpenError
	CMP	[off_pszSearchPath],OFFSET LOADRTM:SpecBuffer 
	JNE	SearchPathLoop		; brif input path not in SpecBuffer
	MOV	AX,CS			; can't use CS for CMP
	CMP	[seg_pszSearchPath],AX	
	JNE	SearchPathLoop		; brif input path not in SpecBuffer

;	If we fall through to here, [pszSearchPath] was really pointing to a
;	single-directory path which had been placed in SpecBuffer.  Now that
;	we have changed SpecBuffer, we can't loop back up.  But we don't have
;	to because we have already checked the single directory and not found
;	the file.  So we just exit with "Not Found".

	JMP	SHORT OpenError 	

; DI:DX = pointer to pathspec that worked
; AX = file handle returned by open
FoundIt:				
	XCHG	AX,BX			; put handle in BX
	CALL	CloseFile		; close the file

DbAssertFlags	nc,LOADRTM,<B$SearchOnePath: error on close>

OpenError:
	XCHG	AX,DX
	MOV	DX,DI			; DX:AX = ptr to filespec that worked,
					; or AX = 0 if got here via OpenError
cEnd

;****
;B$SearchEnv - search the environment table
;
;Purpose:
;	Searches the environment table for the specified variable.
;	If found, return a pointer to the value associated with the
;	variable.  Otherwise return 0.
;
; Note: we can make no assumptions about the value of DS on input.
;
;Input:
;	pEnvVar 	far pointer to environment variable name
;	cbEnvVar	length of EnvVar
;
;Output:
;	if EnvVar found in environment table
;	    DX:AX = ptr to value associated with the variable
;	else
;	    AX = 0
;
;****
cProc	B$SearchEnv,<NEAR>,<DS,DI,SI>
parmD	pEnvVar
parmW	cbEnvVar
cBegin
	MOV	ES,SS:[__psp]	; ProgramPSP doesn't get set up for LQB
	MOV	ES,ES:[2Ch]	   ;...get segment of environment table
	XOR	AX,AX		;zero AX for scan test and zero test
	XOR	DI,DI		;ES:DI = ptr to start of environment table

SearchEnvLoop:
	LDS	SI,[pEnvVar]	;DS:SI = ptr to string to search for
	MOV	CX,[cbEnvVar]	;CX = length of string to search for
	REP	CMPSB
	JE	SearchEnvFound	;brif found a match
TryAgain:
	MOV	CX,0FFFFh	;search for a long time
	REPNZ	SCASB		;scan to past next zero byte
	CMP	ES:[DI],AL	;test for double zero for end of table
	JNZ	SearchEnvLoop	;brif not end of table

; Got to end of table without finding a match.	Return zero.

	JMP	SHORT SearchEnvExit

SearchEnvFound:
	CMP	BYTE PTR ES:[DI],'=' ;did we match the whole name or just part?
	JNE	TryAgain	;brif partial match, not good enough
	INC	DI		;bump pointer past equal sign to value
	MOV	DX,ES
	MOV	AX,DI		;DX:AX = pointer to value
SearchEnvExit:

cEnd

sEnd	LOADRTM

;	The data below is used when the QBI interpreter is reloaded
;	after a compile dialog.  The values are used prior to fixup, to
;	determine what the Fixup boundaries of the reloaded code should
;	be.
sBegin	LOADRTMEND			;end of Resident loader

QBVersion DW	BINSAV_BASICVersion	;QB version
QBRevision DB	BINSAV_REVISION_BYTE	;QB revision number
QBCode	DW	SEG LOADRTMEND		;start of code that gets reloaded
QBData	DW	DGROUP			;start of DGROUP for QB
sEnd	LOADRTMEND			

	CondEnd	StartupStub		; END StartupStub if NOT EI_QB
