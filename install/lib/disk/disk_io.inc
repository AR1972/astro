;========================================================
COMMENT #

	DISK_IO.INC

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	johnhe - 06/06/89

END COMMENT #
;========================================================


INVALID_DRIVE_TYPE	EQU -300
DISK_TIMEOUT		EQU 80h
DISK_RETRIES		EQU 5
BPB_SIZE		EQU 25


ABSIO_PACKET	STRUC

	StartSec	dd	?
	NumSecs		dw	?
	SecBuffer	dd	?

ABSIO_PACKET	ENDS


DOS_FCB		STRUC

	FcbDrive	db	(?)
	FcbName		db	"NUL     "
	FcbExt		db	"   "
	FcbCurBlock	dw	(?)

	FcbRecSize	dw	(?)
	FcbFileSize	dd	(?)
	FcbDate		dw	(?)
	FcbTime		dw	(?)
	
	FcbReserved	db	8 DUP (?)
	FbcCurRecord	db	(?)
	FcbRelRecord	dd	(?)

DOS_FCB		ENDS

;========================================================

