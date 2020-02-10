;***
; files.inc - 26-Aug-86
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
; This include file specifies Macros and Equates used in common system library
; routines used by Xenix and DOS (C-Merge).
;
;******************************************************************************

	INCLUDE FDB.INC 	;[17]
;
;	These equates set up common definitions for things like stdin,
;	stdout, stderr, and error return values from library functions.
;
STDIN	=	0
STDOUT	=	1
STDERR	=	2
LIBERR	=	-1
;
;	MSDOS Operating System Function Codes
;
;	Note that these are also the names of the Xenix functions.  The
;	CALLOS macro for Xenix generates a 'call <name>', using the name.
;
C_TERM	=	0
C_LPTO	=	5		;output to line printer (PRN) device
C_DCIO	=	6
C_DCIN  =	7		;[20] wait for keyboard input
C_PRINT =	09
C_RDLIN =	10
C_FBRK  =	12		;[20] flush keyboard buffer, read keyboard
C_REST	=	13
C_SDRV	=	14
C_FOPEN =	15
C_FCLOS =	16
C_SEAR	=	17
C_DELE	=	19
C_SEQR	=	20
C_SEQW	=	21
C_MAKE	=	22
C_RENA	=	23
C_GDRV	=	25
C_BUFF	=	26
C_RNDR	=	33
C_RNDW	=	34
C_SETIV =	37
C_BLKR	=	39
C_BLKW	=	40
C_PARS	=	41
;
;	DOS 2.0 additions
;
C_VERSN =	48		;Get version number
C_CTRLC =	51		;Set control-c trapping
C_GETIV =	53		;Set interrupt vector
C_FREES =	54		;Get disk  free space
C_CHROP =	55		;Get character operators (switch char.)
C_MKDIR =	57		;Make directory
C_RMDIR =	58		;Remove directory
C_CHDIR =	59		;Change default directory
C_CREAT =	60		;Create file
C_OPEN	=	61		;Open file
C_CLOSE =	62		;Close file
C_READ	=	63		;Read file
C_WRITE =	64		;Write file
C_UNLINK =	65		;Remove file from directory
C_LSEEK =	66		;Seek to location
C_CHMOD =	67		;Change mode
C_IOCTL =	68		;i/o control
C_XDUP	=	69		;duplicate handle
C_XDUP2 =	70		;force duplicate handle
C_CURDIR =	71		;Current directory
C_ALLOC =	72		;allocate memory block
C_DEALLOC =	73		;deallocate memory block
C_SETBLK =	74		;set memory block
C_EXEC	=	75		;Execute program
C_EXIT	=	76		;exit program - return to calling
C_WAIT	=	77		;wait on something
C_FINDF =	78		;find first
C_FINDN =	79		;find next
C_RENAME =	86		;Rename source to dest.
C_LOCKING =	92		;Lock/Unlock system call
;
;	MS-DOS 2.0 error codes
;	Carry bit set on return from system call means the value in
;	AX indicates the following:
;
ERRIVF	=	1		;INVALID FUNCTION
ERRFNF	=	2		;FILE NOT FOUND
ERRPNF	=	3		;PATH NOT FOUND
ERRTMF	=	4		;TOO MANY OPEN FILES
ERRACD	=	5		;ACCESS DENIED
ERRIVH	=	6		;INVALID HANDLE
ERRART	=	7		;ARENA TRASHED
ERROEM	=	8		;NOT ENOUGH MEMORY
ERRIVB	=	9		;INVALID BLOCK
ERRBEN	=	10		;BAD ENVIRONMENT
ERRBFM	=	11		;BAD FORMAT
ERRIVA	=	12		;INVALID ACCESS
ERRIDT	=	13		;INVALID DATA
ERRIVD	=	15		;INVALID DRIVE
ERRCDR	=	16		;CURRENT DIRECTORY
ERRNSD	=	17		;NOT SAME DEVICE
ERRNMF	=	18		;NO MORE FILES
;
;	OS/2 additions
;
ERRSHR =	32		;[2] sharing error
ERRLCK =	33		;[2] locking error
ERROPF =	110		;[2] open/create failed due to explicit fail
MODE_HIGH_BYTE =60H		;[2] This is used in the high byte of the mode
				;[2] word for DOS 5.0 opens. It sets the File
				;[2] Write Through bit and the Fail bit.
;
;	MSDOS File attributes
;
ATTR_READONLY = 01H
ATTR_HIDDEN =	02H
ATTR_SYSTEM =	04H
ATTR_VOLUME =	08H
ATTR_SUBDIR =	10H
ATTR_ARCHIVE =	20H
;
;	Operating System dependent field sizes
;
EOFCHR	=	'Z' AND 1FH

	SUBTTL	CALLOS macro
	PAGE
;******************************************************************************
;
; CALLOS macro
;

CALLOS	MACRO	what,error,arg1,arg2,arg3,arg4
ifnb	<arg1>
ifdif	<arg1>,<bx>
	mov	bx,arg1
endif
endif
ifnb	<arg2>
ifdif	<arg2>,<cx>
	mov	cx,arg2
endif
endif
ifnb	<arg3>
ifdif	<arg3>,<dx>
	mov	dx,arg3
endif
endif
ifnb	<arg4>
ifdif	<arg4>,<ax>
	mov	ax,arg4
endif
endif
ifnb	<what>
ifdif	<what>,<ah>
	mov	ah,c_&what
endif
endif
	int	33
ifnb	<error>
	jb	error
endif
	ENDM
;

	SUBTTL	BASIC FDB
	PAGE

;******************************************************************************
;
;	Equates related to File Data Block usage
;

REC_LENGTH  =	128		;default FDB buffer size for random files
SEQ_BUF_LEN =	512		;default FDB buffer size for seq files

;	Equates for COMn:
FD_BAKC     =	FD_FSIZ_LO	;Holds backup char if FD_FLAGS & FL_BKC

;	Equates for PIPE:

FD_PIPEOUT  =	FD_HANDLE	;output pipe file handle
FD_PIPEIN   =	FD_FSIZ_LO	;input pipe file handle
FD_PID	    =	FD_FSIZ_HI	;process id for spawned pipe process
FD_CBIN     =	FD_HIGHLR	;count of bytes in input queue
FD_INOFF    =	FD_LOGREC	;[21] input position (cur pos=FD_CBIN-FD_INOFF)
FD_INBUF    =	FD_BUFFER + 1	;[22] beginning of pipe input buffer

;	FDB-related constants

;[19] FDB_SIZE    =	FD_VRECL ;size of basic FDB, without buffer(s) or
				;  extra fields.
FDB_EXTRA   =	FD_BUFFER - FD_VRECL ;additional space needed for extra fields

;
;	FD_FLAGS flags
;
FL_CONINP   =	01h		;[14] File is CONsole INPUT.
				;[14] DO NOT MOVE FL_CONINP FROM THE FIRST BIT.
FL_CONOUT   =	02h		;File is CONsole OUT.  This flag is needed
				;because DOS 2.0 handles DOS 2.0 (raw) writes
				;to CON out incorrectly.  DOS always reports
				;n-1 bytes written for a write of n bytes
				;[11] DO NOT MOVE FL_CONOUT FROM THE SECOND BIT.
FL_NEOF     =	04h		;[11] Not(eof reached on file) - used by com
FL_FIELD    =	08H		;[7] FIELD statement has been encountered
FL_BKC	    =	10H		;[11] FD_BAKC holds backup character (COMn:)
FL_LPT_OPN  =	10H		;[11] LPRINT FDB has been opened
				;[11] NOTE:  OK for these to share the same bit.
FL_BIN	    =	20H		;[14] File encoding [0..1]=[ASCII..BINARY]
FL_TRUNC    =	40H		;[19] Trunctate output file upon first write.
FL_CHAR     =	80h		;File is connected to character device
				;Note that this is the same bit as that
				;returned by the ioctl call for char/blk
				;[11] DO NOT MOVE FL_CHAR FROM THE EIGHTH BIT.

;	Equates for use with flag returned by B$GET_PATHNAME

FN_WILD     =	01H		;[11] wild card present in filename
FN_HAS_EXT  =	02H		;[11] file name has extention
FN_IS_DIR  =	04H		;[24] file name has ends with '\'


;	Equates for use with random I/O code (GET/PUT).

PutFlg	    =	01h		;[23] PUT call
RelFlg      =	02h		;[23] Record number specified
RecFlg      =	04h		;[23] Record variable specified
VarStrLen   =	08h		;[23] processing length of var len string
VarStrData   =	10h		;[23] processing data portion of var len string

	.LIST
