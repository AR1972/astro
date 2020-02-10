; ========================================================

COMMENT #

	DOINT2F.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	Does an int 2f with AX = 4400h to update the format
        complete gage. This function is mainly for getting
        the gage setup before the format program is spawned.

	DoInt2f( unsigned Percent );

	ARGUMENTS: Percent - Percentage of format complete
	RETURNS:   void

	johnhe - 02/24/90

END COMMENT #

;========================================================

include BIOS_IO.INC
include	MODEL.INC

; ========================================================

.CODE

; ========================================================

DoInt2f PROC  Value:WORD

	mov	AX,4900h
	mov	BX,Value
	int	2fh
	ret

DoInt2f ENDP

; ========================================================

END
