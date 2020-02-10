;========================================================
COMMENT #

	DSKREAD.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Uses the ROM BIOS int 13h write sectors function
	to read the specified number of sectors.

	int PhyDiskRead( char *pchBuf, int iSecCyl,
			  int iHead, char chDrive,
			  int cSecs);
	=================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

; =======================================================

.CODE

; =======================================================

PhyDiskRead PROC USES ES BX, pchBuf:PTR, iSecCyl:WORD, iHead:BYTE, \
			      chDrive:BYTE, cSecs:BYTE
	mov	AH,02
	mov	AL,cSecs
	mov	CX,iSecCyl
	mov	DH,iHead;
	mov	DL,chDrive

;	les	BX,pchBuf
	LoadPtr	ES, BX, pchBuf		; ES:BX --> caller's buffer

	int	13h
	mov	AX,1
	jc	ReadExit
	xor	AX,AX
ReadExit:
	ret

PhyDiskRead ENDP


END
