; ========================================================

COMMENT #

	MEMFREE.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	johnhe - 07/01/89

END COMMENT #

;========================================================

DOSSEG

include	MODEL.INC

.CODE

; ========================================================
; Returns number of free memory paragraphs available
; as a contigous block from DOS.
;
; unsigned GetMemoryFree( void )
;
; ========================================================

GetMemoryFree PROC

	mov	BX,0ffffh		; Set for max 1 meg memory
	mov	AH,48h			; Dos allocate memory function
	int	21h
	push	BX			; Put free para count on the stack
	jc	GetMemoryFreeRet	; If no carry need to free memory
	push	ES			; Save ES
	mov	ES,AX			; Put segment address in ES
	mov	AH,49h			; Dos free memory function
	int	21h
	pop	ES			; Restore ES
GetMemoryFreeRet:
	pop	AX			; Put max memory paragraphs in AX
	ret

GetMemoryFree ENDP

; ========================================================

	END

; ========================================================
