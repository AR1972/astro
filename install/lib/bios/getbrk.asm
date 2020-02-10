; ========================================================

COMMENT #

	GETBREAK.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	johnhe - 03/03/89

END COMMENT #

;========================================================

include BIOS_IO.INC
include	MODEL.INC

;========================================================

.CODE

; ========================================================
; Returns the setting of DOS's internal Ctrl C check.
;
; int GetBreakSetting( void );
;
; ARGUMENTS:	NONE
; RETURNS:	int	- 0 if check is off else 1
;
; ========================================================

GetBreakSetting PROC 

	mov	AX,3300h		; Request Ctrl C check state
	int	21h
	mov	AL,DL			; Put state in AL
	cbw				; Convert AL to a word
	ret

GetBreakSetting ENDP


END
