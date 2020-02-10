;========================================================
COMMENT #

	DSK_ILNK.ASM

	Copyright (c) 1992 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Checks a DOS drive to see if it is an Interlnk drive

	int IsInterlnk( int Drive );

	ARGUMENTS: Drive - DOS drive number (0=A, 1=B, 2=C, ...)
	RETURNS:   int	- TRUE if drive is Interlnk; else FALSE

	================================================

	05/31/92 - created

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

FALSE	EQU 0
TRUE	EQU 1

; =======================================================

.CODE

; =======================================================

IsInterlnk PROC Drive:BYTE

	mov	AX,5600h
	xor	BX,BX
	mov	CX,BX
	mov	DX,0FFFFh
	int	2Fh
	cmp	AL,0FFh		; Interlnk Installed?
	jne	II10		;  No - jump.	

	mov	AX,5601h
	xor	BX,BX
	mov	CX,BX
	mov	BH,Drive
	mov	DX,0FFFFh
	int	2Fh
	cmp	AL,0FFh		; Interlnk drive?
	mov	AX,TRUE		;  Assume yes.
	je	IIX		;  Yes - jump.

II10:	mov	AX,FALSE	; Not Interlnk.

IIX:	ret

IsInterlnk ENDP


; ========================================================

END
