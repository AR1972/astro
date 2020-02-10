; Copyright 1987-1991 Central Point Software, Inc. All rights reserved.
;------------------------------------------------------
; This is the last module for CPS 5.0 MIRROR.COM
; When linking, this must be the last OBJ file.
;
; Stack reduced from 512 to 320 bytes (we use only 80).  v6.0  02-16-90.
;------------------------------------------------------
;
CODE	SEGMENT para public 'CODE'
	ASSUME	CS:CODE
	PUBLIC	stak_end, end_prog
		DB	320/13 DUP ("Mirror Stack ")	;Non-resident stack.
		EVEN
STAK_END	LABEL near		;That's the way Jim defined it.
END_PROG	DB	0FFH
CODE	ENDS
	END
