; ========================================================

COMMENT #

	DATAVERS.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	johnhe - 03/03/89

END COMMENT #

;========================================================

include BIOS_IO.INC
include	MODEL.INC


.CODE

; ========================================================
;
; Returns the DOS Data version from DOSDATA:04h
;
; int	GetDosDataVersion( void );
;
; ========================================================

GetDosDataVersion PROC USES ES

	mov	AH,34h			; DOS get critical flag address
	int	21h
	mov	BX,4			; Put address of DATA version in BX
	mov	AL,ES:[BX]		; Get DATA version byte
	cbw				; Convert AL to an integer
	ret

GetDosDataVersion ENDP	

; ========================================================

	END

; ========================================================
