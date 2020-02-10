;========================================================
COMMENT #

	FCB_REN.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Uses DOS FCB call 17h to rename a file.

	int FcbRename( struct FCB *Fcb );

	ARGUMENTS:	Fcb	- Ptr to Special FCB structure of
				  the following form.

			BYTE(s) - CONTENTS
			0h	  - Drive number
			01h-08h - Old file name, padded with blanks
			09h-0bh - Old file ext., padded with blanks
			0ch-10h - All zeros
			11h-18h - New file name, padded with blanks
			19h-1bh - New file ext., padded with blanks
			1ch,24h - All zeros

	RETURNS:	int	- 0 if successfull else -1
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
  FcbRename PROC USES DS, FcbBuf:PTR
ELSE
  FcbRename PROC          FcbBuf:PTR
ENDIF

	mov	AH,17h
;	lds	DX,FcbBuf
	LoadPtr	DS, DX, FcbBuf		; DS:DX --> FCB 
	int	21h
	mov	AH,AL
	ret

FcbRename ENDP

; =======================================================

	END

; =======================================================
