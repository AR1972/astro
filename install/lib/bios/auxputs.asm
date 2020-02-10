; ========================================================

COMMENT #

	AUXPUTS.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	Outputs a string to the STDAUX device. (com1)

	void AuxPuts( char *Str )

	ARGUMENTS: Str	- Ptr to string to display
	RETURNS:   void

	johnhe - 02/24/90

END COMMENT #

;========================================================

include BIOS_IO.INC

include	MODEL.INC

; ========================================================

.CODE

; ========================================================

IF @DataSize
	AuxPuts PROC  USES DS, Str:PTR
ELSE
	AuxPuts PROC Str:PTR
ENDIF
	mov	AX,40h			; DOS write file

	LoadPtr	DS DX Str
	mov	BX,3			; AUX handle
	int	21h
	ret

AuxPuts ENDP

; ========================================================

	PUBLIC GetCallerAddr
GetCallerAddr:

	mov	AX,[BP+2]
	ret

; ========================================================

END
