;========================================================
COMMENT #

	HD_ISFMT.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	johnhe 02-18-90


	NOTE:

	Becasue of bugs in the DOS BIOS this function doesn't work so
	use a function which reads the boots sector to find out.

END COMMENT #
; ===========================================================================

INCLUDE	model.inc

; ===========================================================================

diskaccess struc

	dac_special_func	db	?
	dac_access_flag 	db	?

diskaccess ends

GET_ACCESS EQU 0867h

; =======================================================

.CODE

; ===========================================================================
;
; This routine will invoke INT21 44h (Block Generic IOCTL Subfunction)
; call to see if the drive has been previously formatted by using an
; undocumented call.
;
; int IsFormatted( char DriveLetter, struct diskaccess *Dac )
;
; ARGUMENTS:	DriveLetter	- DOS drive letter (MUST be uppercase)
;
; RETURNS:	int         - TRUE if drive is formatted
;                            FALSE if not formatted
; ===========================================================================

; BUGBUG - (jah) - No need to preserver BX or CX in a C callable function

IF @DataSize
  HdIsFormatted PROC USES DS BX CX, DriveLetter:BYTE, Dac:PTR
ELSE
  HdIsFormatted PROC		    DriveLetter:BYTE, Dac:PTR
ENDIF
	mov	AX,440dh		; DOS generic IOCTL check media
	xor	BH,BH			; BH == 0
	mov	BL,DriveLetter		; BL == Drive letter
	sub	BL,'A'			; BL == DOS drive number
        inc     BL             	 	; 1 ==> 'A', 2==> 'B', etc.
	mov	CX,GET_ACCESS		; CX == Get format status

;	lds	DX,Dac			; DS:DX --> Dac struc
	LoadPtr	DS, DX, Dac		; DS:DX --> Dac struc

	int	21h			; DOS call

	mov	BX,DX			; DS:BX --> Dac struc
	xor	AX,AX			; Assume not formatted

	or	[BX].dac_access_flag,11111111b ; Check format status flag
	jz	HdIsFormattedReturn
	mov	AX,01			; Signal disk if formatted

HdIsFormattedReturn:
	ret

HdIsFormatted ENDP

END


