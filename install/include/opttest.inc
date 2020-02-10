;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1990
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


TestMem MACRO MemPtr, M
IFE (M) AND 0FFFFFF00h
	test	BYTE PTR [MemPtr], M
ELSE
IFE (M) AND 0FFFF00FFh
	test	BYTE PTR [MemPtr+1], ((M) SHR 8)
ELSE
IFE (M) AND 0FF00FFFFh
	test	BYTE PTR [MemPtr+2], ((M) SHR 16)
ELSE
IFE (M) AND 000FFFFFFh
	test	BYTE PTR [MemPtr+3], ((M) SHR 24)
ELSE
IFE (M) AND 0FFFF0000h
	test	WORD PTR [MemPtr], M
ELSE
IFE (M) AND 0FF0000FFh
	test	WORD PTR [MemPtr+1], ((M) SHR 8)
ELSE
IFE (M) AND 00000FFFFh
	test	WORD PTR [MemPtr+2], ((M) SHR 16)
ELSE
	test	DWORD PTR [MemPtr], M
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
IFE (Mask) AND 0FFFFFF00h
	test	Ltr&&l, Mask
ELSE
IFE (Mask) AND 0FFFF00FFh
	test	Ltr&&h, ((Mask) SHR 8)
ELSE
IFE (Mask) AND 0FFFF0000h
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
IFE (Mask) AND 0FFFF0000h
	test	si, Mask
ELSE
	test	esi, Mask
ENDIF
ELSE
IFIDNI <R>, <EDI>
IFE (Mask) AND 0FFFF0000h
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




SetFlag MACRO MemPtr, M
IFE (M) AND 0FFFFFF00h
	or	BYTE PTR [MemPtr], M
ELSE
IFE (M) AND 0FFFF00FFh
	or	BYTE PTR [MemPtr+1], ((M) SHR 8)
ELSE
IFE (M) AND 0FF00FFFFh
	or	BYTE PTR [MemPtr+2], ((M) SHR 16)
ELSE
IFE (M) AND 000FFFFFFh
	or	BYTE PTR [MemPtr+3], ((M) SHR 24)
ELSE
IFE (M) AND 0FFFF0000h
	or	WORD PTR [MemPtr], M
ELSE
IFE (M) AND 0FF0000FFh
	or	WORD PTR [MemPtr+1], ((M) SHR 8)
ELSE
IFE (M) AND 00000FFFFh
	or	WORD PTR [MemPtr+2], ((M) SHR 16)
ELSE
	or	DWORD PTR [MemPtr], M
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
	ENDM



ClrFlag MACRO MemPtr, M
IFE (M) AND 0FFFFFF00h
	and	BYTE PTR [MemPtr], NOT (M)
ELSE
IFE (M) AND 0FFFF00FFh
	and	BYTE PTR [MemPtr+1], NOT ((M) SHR 8)
ELSE
IFE (M) AND 0FF00FFFFh
	and	BYTE PTR [MemPtr+2], NOT ((M) SHR 16)
ELSE
IFE (M) AND 000FFFFFFh
	and	BYTE PTR [MemPtr+3], NOT ((M) SHR 24)
ELSE
IFE (M) AND 0FFFF0000h
	and	WORD PTR [MemPtr], NOT (M)
ELSE
IFE (M) AND 0FF0000FFh
	and	WORD PTR [MemPtr+1], NOT ((M) SHR 8)
ELSE
IFE (M) AND 00000FFFFh
	and	WORD PTR [MemPtr+2], NOT ((M) SHR 16)
ELSE
	and	DWORD PTR [MemPtr], NOT (M)
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
	ENDM
