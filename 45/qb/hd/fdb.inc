;***
; fdb.inc - 31-Jul-87
;***
	.XLIST
;***
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	This include file contains the definition of the BASIC File Data Block.
;
;	NOTE: This is a SHARED INCLUDE FILE, used by both the RUNTIME and
;	NOTE: the interpreter projects.  Any changes made to one should
;	NOTE: be carried over (COPIED) to the other!!!
;
;
;******************************************************************************

FDB_INC = -1			;remember that this file has been included


FILE_DATA_BLOCK	STRUC

FD_MODE    	DB	0	;file mode of open
FD_HANDLE   	DW	0	;file handle
FD_DEVICE  	DB	0	;device number (0 for disks)
FD_WIDTH   	DB	0	;file width
FD_FLAGS   	DB	0	;miscellaneous bit flags (defined below)

; Storage for the the following fields, as well as for a buffer, is only
; present for disk files, PIPE:, COMn:, and XENIX LPRINT.

FD_VRECL   	DW	0	;record length/buffer size
FD_FSIZ_LO   	DW	0	;low word of file size (char dev only)
FD_FSIZ_HI   	DW	0	;high word of file size (char dev only)
FD_LOGREC  	DW	0	;current logical record number
FD_HIGHLR  	DW	0	;to be used for 32-bit record numbers
FD_BUFCNT	DW	0	;# of read/unwritten bytes in seq buffer
				;  (offset of next char in buffer)
FD_OUTPS  	DB	0	;output position for tab expansion
FD_BUFFER   	DB	1 DUP(0) ;buffer for random and sequential access.
				;complete pathname for disk files is stored
				;after the buffer.
FILE_DATA_BLOCK ENDS

;	FDB-related constants

FDB_SIZE    =	FD_VRECL	; size of basic FDB, without buffer(s) or
				; extra fields.
FD_INBUFCNT  =	FD_LOGREC	;# of bytes in the sequential input buffer

;
;FileDB used for transparent reference to FDB structure
;
FileDB	EQU	[SI]		

;***
;FDB_PTR - macro to create an FDB pointer in registers
;
;Purpose:
;	Rewritten with [3].
;	This macro allows us to create a either a WORD or DWORD pointer
;	for FDB's depending on the FV_FARFDB switch.
;Entry:
;	seg  -	seg register to receive FDB segment address
;	reg  -	index register to receive actual FDB offset from handle/ptr
;		in srce; if not present, register is not loaded
;	srce -	source of handle/ptr for FDB
;		if not present, register is not loaded
;
;	NOTE:  The equate for FileDB above assumes ES:SI for seg:reg!
;
;Exit:
;	seg:reg or reg contains the physical FDB pointer.
;Preserves:
;	All.
;
;**************************************************************************
FDB_PTR MACRO	seg,reg,srce
ifnb	<reg>
	.errb <srce>			;reg specified without srce
  ifdifi <reg>,<srce>
	mov	reg,srce		;;load reg from ptr/handle in srce
  endif
else
	.errnb <srce>			;;srce specified without reg
endif
	ENDM

	.LIST
