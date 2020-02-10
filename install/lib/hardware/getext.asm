;==========================================================================
COMMENT #

 GETEXT.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

 Functions to read ext memory size from CMOS RAM

END COMMENT #
;==========================================================================

; constants used by _get_ext

EXT_MEM_LOW	EQU	17h
EXT_MEM_HIGH	EQU	18h

;==========================================================================

INCLUDE model.inc


EXTRN	GetCpuType	:PROC
EXTRN	ReadCmos	:PROC
EXTRN	WriteCmos	:PROC

.CODE

;==========================================================================
; Get extended memory amount.
; First do an int 15h function 88h to be sure that extended memory is
; supported on this machine (don't worry about size returned just if
; the function is supported).
;==========================================================================

GetExtMemSize PROC

	call_M	GetCpuType		; First check to be sure not
	or	AX,AX			; an 8088 CPU type (type == 0)
	mov	AX,0
	jz	GetExtMemRet		; If CPU type 0 then 0 ext memory

	mov	AH,EXT_MEM_LOW
	call_M	ReadCmos		; Read low byte of memory size
	mov	BX,AX			; BL == Byte from CMOS
	not	AL			; Flip the bits
	call_M	WriteCmos		; Write back the flipped bits
	call_M	ReadCmos		; Read back the byte just written
	cmp	AX,BX			; See if bits in CMOS changed

	mov	AX,0			; Assume may not have CMOS
	je	GetExtMemRet

	mov	AX,BX			; Restore original CMOS contents
	call_M	WriteCmos		; Write back original low ext mem byte
	mov	AH,EXT_MEM_HIGH
	call_M	ReadCmos		; Now read the high ext memory byte
	mov	AH,AL			; Set with AH = high && AL == LOW
	mov	AL,BL

GetExtMemRet:
	ret

GetExtMemSize ENDP

;==========================================================================

	END
