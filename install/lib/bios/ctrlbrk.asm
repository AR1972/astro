; ========================================================

COMMENT #

	CTRLBRK.ASM

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
; New int 1bh and 23h which just do an iret
; ========================================================

	public NewInt1b
NewInt1b LABEL FAR

	public NewInt23
NewInt23 LABEL FAR

	iret


;========================================================

	END

;========================================================
