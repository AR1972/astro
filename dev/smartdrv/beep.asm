
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

include bambi.inc


BEEP = 0


zseg    segment public 'CODE'

	assume  cs:zseg
	assume	ds:nothing

if BEEP
if 0
PUBLIC	flush_beep
PUBLIC  invalidate_beep

Test_Procs PROC NEAR

	call	Flush_Beep

	mov	cx, 9
	call	Wait_Ticks

	call	Invalidate_Beep


	mov	ax, 4c00h
	int	21h

Test_Procs ENDP

endif
;******************************************************************************
;
;   Flush_Beep
;
;   DESCRIPTION:
;	Produces a standard beep to indicate a cache flush.
;
;   ENTRY:
;	None
;
;   EXIT:
;	None
;
;   USES:
;	Flags
;
;==============================================================================

Flush_Beep PROC NEAR

	push	ax
	push	cx

	mov	cx, 1336			; Standard beep tone
	call	Set_Sound_Freq
	mov	cx, 9				; Standard beep length
	call	Wait_Ticks
	call	Set_Sound_Freq			; Turn off sound (CX = 0)

	pop	cx
	pop	ax
	ret

Flush_Beep ENDP


;******************************************************************************
;
;   Invalidate_Beep
;
;   DESCRIPTION:
;	Generates a long, high-to-low beep to indicate a cache invalidation.
;
;   ENTRY:
;	None
;
;   EXIT:
;	None
;
;   USES:
;	Flags
;
;==============================================================================

Invalidate_Beep PROC NEAR

	push	ax
	push	cx
	push	dx

	mov	dx, 1336
IB_Loop:
	mov	cx, dx
	call	Set_Sound_Freq
	mov	cx, 1
	call	Wait_Ticks
	add	dx, 100
	cmp	dx, 5000
	jb	IB_Loop

	call	Set_Sound_Freq

	pop	dx
	pop	cx
	pop	ax
	ret

Invalidate_Beep ENDP




;******************************************************************************
;
;   Set_Sound_Freq
;
;   DESCRIPTION:
;	Sets the sound to the desired frequency or turns sound off if
;	input of 0
;
;   ENTRY:
;	CX = Frequency to program counter to or 0 to turn sound off
;
;   EXIT:
;	None
;
;   USES:
;	AX, CX, Flags
;
;==============================================================================

Set_Sound_Freq PROC NEAR

	in	al, 61h
	jcxz	SSF_Turn_Sound_Off

	xchg	ax, cx

	jmp	$+2
	jmp	$+2

	out	42h, al

	jmp	$+2
	jmp	$+2

	mov	al, ah
	out	42h, al

	mov	ax, cx				; AL = Old port 61h value
	or	al, 11b 			; Turn on sound bits
	jmp	SHORT SSF_Set_Sound_Bits

SSF_Turn_Sound_Off:
	and	al, NOT 11b
SSF_Set_Sound_Bits:
	jmp	$+2
	out	61h, al

	ret

Set_Sound_Freq ENDP


;******************************************************************************
;
;   Wait_Ticks
;
;   DESCRIPTION:
;
;   ENTRY:
;	CX = # ticks to delay before returning
;
;   EXIT:
;	CX = 0
;
;   USES:
;	AX, CX, Flags
;
;==============================================================================

Wait_Ticks PROC NEAR

	sti					; Paranoia...

	push	ds
	xor	ax, ax
	mov	ds, ax

Wait_Reset_Base:
	mov	al, BYTE PTR ds:[46Ch]		; AL = Current tick counter
Wait_Loop:
	cmp	al, BYTE PTR ds:[46Ch]		; Q: Has counter changed?
	je	Wait_Loop			;    N: Wait some more
	loop	Wait_Reset_Base 		;    Y: One more tick gone!

	pop	ds
	ret

Wait_Ticks ENDP

endif

zseg ends

end

