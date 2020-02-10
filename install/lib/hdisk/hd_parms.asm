;========================================================
COMMENT #

	DSK_PARM.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================

	Gets the drive parameters for the specified hard
	and uses them to fill in the HdParms structure
	passed as an argument.

	int GetDrvParms( struct HdParms *Parms, int DrvNum )

	ARGUMENTS:	Parms	- Ptr to hdParms structure
			DrvNum	- Physical hard disk number
	RETURNS:	int	- 0 if successful
				- !0 if error (invalid drive)

	=================================================

	STRATEGY:
		Does an int 13h function 8h which returns:
			CH = Low 8 bits of max cylinder num
			CL = Bits 6&7 high order bytes of
			     max cylinder num
			     Bits 0-5 max sector num
			DH = Max head num
			DL = Number of drives (ignored)

	=================================================

	johnhe - 02/07/99

END COMMENT #
; =======================================================

INCLUDE	model.inc

; =======================================================

HdParms STRUC

	MaxHead 	dw	?
	MaxSec		dw	?
	MaxCyls 	dw	?

hdParms ENDS

; =======================================================

.CODE

; =======================================================
;  NOTE:
;	ES and DIS must be saved on entry because the int
;	13h call return a ptr to the disk parameters in
;	these registers.
; =======================================================

IF @DataSize
  GetDrvParms PROC USES DS ES DI, Parms:PTR, DrvNum:BYTE
ELSE
  GetDrvParms PROC USES    ES DI, Parms:PTR, DrvNum:BYTE
ENDIF
	mov	AH,8h			; Get drive parms function
	mov	DL,DrvNum		; Load drive number
	int	13h			; BIOS disk access

	jc	GetDrvParmsExit 	; Error check

;	lds	BX,Parms		; Load ptr to parms structure
	LoadPtr	DS, BX, Parms		: DS:BX --> Drive parameters struct

					; AH == 0 if we got here
	xor	AH,AH			; but I don't trust all BIOSs
	mov	AL,DH			; Move heads to DL
	mov	[BX].MaxHead,AX		; Store max head in struc

	mov	AL,CL			; Put max sect number in AL
	and	AL,00111111b		; Mask off low 6 bits
	mov	[BX].MaxSec,AX		; Store max sector in struc

	xchg	CH,CL			; Move CH to low byte
	rol	CH,1			; Make bits 6&7 low 2 bits
	rol	CH,1
	and	CH,00000011b		; Mask off 2 low bits
	mov	[BX].MaxCyls,CX 	; Save max cylinder in struc
	xor	AX,AX			; Signal OK

GetDrvParmsExit:
	ret

GetDrvParms ENDP

END
