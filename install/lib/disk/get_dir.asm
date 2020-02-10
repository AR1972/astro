;========================================================
COMMENT #

	GET_DIR.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Gets the current directory path from DOS.

	int _dos_getdir( char *Buffer, int Drive );

	ARGUMENTS:	Buffer	- Ptr to 64 byte memory
				  area
			Drive	- DOS drive number (0=default, 1=A, 2=B, ...)
	RETURN: 	int	- 0 if successfull else
				  !0 if error
	=================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

; =======================================================

.CODE

; =======================================================

IF @DataSize
  _dos_getdir	PROC USES DS SI, Buffer:PTR, Drive:BYTE
ELSE
  _dos_getdir	PROC USES    SI, Buffer:PTR, Drive:BYTE
ENDIF

;	lds	SI,Buffer		; DS:SI -> buffer
	LoadPtr	DS, SI, Buffer		; DS:SI -> buffer

	mov	DL,Drive		; DL = disk drive (0=default,1=A)
	mov	AH,47h			; Get current dir function
	int	21h
	mov	AX,1			; Assume may may be errors
	jc	GetDirReturn		; Error check
	xor	AX,AX			; Signal no errors

GetDirReturn:
	ret

_dos_getdir ENDP

; =======================================================

	END

; =======================================================
