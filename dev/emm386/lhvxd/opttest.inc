;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1990-1991
;
;   Title:	OPTTEST.INC - Macros to optimize test instructions
;
;   Version:	1.00
;
;   Date:	28-Jun-1990
;
;   Author:	RAL
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;   28-Jun-1990 RAL
;
;==============================================================================


TestMem MACRO MemPtr, Mask
IFE Mask AND 0FFFFFF00h
	test	BYTE PTR [MemPtr], Mask
ELSE
IFE Mask AND 0FFFF00FFh
	test	BYTE PTR [MemPtr+1], (Mask SHR 8)
ELSE
IFE Mask AND 0FF00FFFFh
	test	BYTE PTR [MemPtr+2], (Mask SHR 16)
ELSE
IFE Mask AND 000FFFFFFh
	test	BYTE PTR [MemPtr+3], (Mask SHR 24)
ELSE
IFE Mask AND 0FFFF0000h
	test	WORD PTR [MemPtr], Mask
ELSE
IFE Mask AND 0FF0000FFh
	test	WORD PTR [MemPtr+1], (Mask SHR 8)
ELSE
IFE Mask AND 00000FFFFh
	test	WORD PTR [MemPtr+2], (Mask SHR 16)
ELSE
	test	DWORD PTR [MemPtr], Mask
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
	ENDM





TestReg MACRO R, Mask

_@TR MACRO Ltr
IFE Mask AND 0FFFFFF00h
	test	Ltr&&l, Mask
ELSE
IFE Mask AND 0FFFF00FFh
	test	Ltr&&h, (Mask SHR 8)
ELSE
IFE Mask AND 0FFFF0000h
	test	Ltr&&x, Mask
ELSE
	test	e&&Ltr&&x, Mask
ENDIF
ENDIF
ENDIF
	ENDM


IFIDNI <R>, <EAX>
	_@TR a, Mask
ELSE
IFIDNI <R>, <EBX>
	_@TR b, Mask
ELSE
IFIDNI <R>, <ECX>
	_@TR c, Mask
ELSE
IFIDNI <R>, <EDX>
	_@TR d, Mask
ELSE
IFIDNI <R>, <ESI>
IFE Mask AND 0FFFF0000h
	test	si, Mask
ELSE
	test	esi, Mask
ENDIF
ELSE
IFIDNI <R>, <EDI>
IFE Mask AND 0FFFF0000h
	test	di, Mask
ELSE
	test	edi, Mask
ENDIF
ELSE
%OUT ERROR:  Bad param to TestReg macro
.ERR
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
	ENDM
