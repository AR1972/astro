;========================================================
COMMENT #

	DSK_VALD.asm

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential


	=================================================
	Returns TRUE if specified drive is valid. Determines
	if a drive letter exists by making that drive the
	current drive and then getting the current drive
	to see if it really was changed. Restores the
	current drive to what it was on entry.

	int IsValidDrive( char Drive );

	ARGUMENTS: Drive - DOS drive letter (A,B,C,...,Z)
	RETURNS:   int	- TRUE if disk exists else FALSE


	================================================

	johnhe - 09/01/90

END COMMENT #
;========================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

;========================================================

COMMENT #
.DATA

Fcb	DOS_FCB	< 0, "NUL     " , "   ",,,,,,,, >

END COMMENT #

;========================================================

.CODE

COMMENT #
;========================================================

IsValidDrive PROC Drive:BYTE

	mov	AL,Drive	; AL == Drive letter
	sub	AL,('A'-1)	; Convert to drive number (1=A,2=B,3=C,...)
	mov	Fcb.FcbDrive,AL	; Put the drive number in the FCB

	mov	AH,0fh		; FCB open function
	mov	DX,OFFSET Fcb
	int	21h

	not	AL		; If error AL == 0ffh we now have 0
	xor	AH,AH		; 
	cmp	AL,AH		; See if AL is really 0
	je	@f		; Drive was invalid so return 0
	mov	AX,1		; else return TRUE
@@:
	ret

IsValidDrive ENDP

END COMMENT #
; ========================================================

IsValidDrive PROC Drive:BYTE

	mov	DL,Drive	; DL == Drive letter
	sub	DL,'A'		; Convert to drive number (1=A,2=B,3=C,...) 

	mov	AH,19h		; AH == Get current drive
	int	21h
	push	AX

	mov	AH,0eh		; Select drive in DL
	int	21h

	mov	AH,19h		; Get current drive
	int	21h

	cmp	AL,DL		; See if drive was changed
	mov	BX,0		; Assume it wasn't
	jne	@f
	inc	BX		; But it was, so drive is valid
@@:
	pop	DX		; Restore original current drive
	mov	AH,0eh
	int	21h

	mov	AX,BX
	ret

IsValidDrive ENDP

; ========================================================

END
