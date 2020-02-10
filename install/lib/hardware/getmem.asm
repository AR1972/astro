;========================================================
COMMENT #

	GETMEM.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Returns number of Kbytes of convential memory which
	is returned by int 12h.

	unsigned GetConvMem( void );

	ARGUMENTS:	NONE
	RETURNS:	unsigned - Kbytes of convential memory
	=================================================

	johnhe - 06/06/89

END COMMENT #

; =======================================================

INCLUDE model.inc

.CODE

; =======================================================

GetConvMem PROC

	int	12h
	ret

GetConvMem ENDP

END
