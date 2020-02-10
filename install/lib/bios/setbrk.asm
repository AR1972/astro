; ========================================================

COMMENT #

	SETBREAK.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	johnhe - 03/03/89

END COMMENT #

;========================================================


include	MODEL.INC

.CODE

; ========================================================
; Sets the DOS internal break check setting to on or off.
;
; void UpdateBreakSetting( int OnOffFlag );
;
; ARGUMENTS:	OnOffFlag - TRUE if turning on else FALSE
; RETURNS:	void
; ========================================================

UpdateBreakSetting PROC  OnOff:WORD

	mov	AX,3301h
	xor	DX,DX
	or	OnOff,0			; Test for zero
	jz	SetState
	mov	DL,1			; Request was for turn it on
SetState:
	int	21h
	ret

UpdateBreakSetting ENDP

; ========================================================

	END

; ========================================================
