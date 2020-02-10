;========================================================
COMMENT #

	FCB_PARS.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Uses DOS FCB call 29h to parse a file name

	The following rules are observed based on the filename
	string argument. These are specifed by the bits in AL.
	when the int 21h is done.

	0. Ingores leading filename seperator
	1. Sets the drive number to 0 if not found in string
	2. Sets the filename to blanks if filename == ""
	3. Sets the file extension to blanks if not found

	int FcbParse( char *OldName, struct FCB *Fcb );

	ARGUMENTS:	OldName - Ptr to string to parse
			Fcb	- Ptr to FCB struct to store
				  parsed name and drive.
	RETURNS:	int	- 00 - No wild card characters
				  01 - Wild card characters used
				  -1 - Invalid drive or filename
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
  FcbParse PROC USES DS ES DI SI, FileName:PTR, Fcb:PTR
ELSE
  FcbParse PROC USES    ES DI SI, FileName:PTR, Fcb:PTR
ENDIF
	mov	AX,2901h
;	lds	SI,FileName		; Load ptr to string to parse
	LoadPtr	DS, SI, FileName	; DS:SI --> String to parse

;	les	DI,Fcb			; Load ptr to FCB
	LoadPtr	ES, DI, Fcb		; ES:DI --> FCB

	int	21h
	cbw				; Convert byte in AL to word in AX
	ret

FcbParse ENDP

; =======================================================

	END

; =======================================================
