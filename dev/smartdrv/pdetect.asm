;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

include bambi.inc

zseg    segment public 'CODE'

	assume  cs:zseg
	assume	ds:nothing


public detect_processor

.386
;******************************************************************************
;   DetectProcessor - return type of processor (386/486 vs. 8088/86/286).
;       This routine relies on Intel-approved code that takes advantage
;       of the documented behavior of the high nibble of the flag word
;       in the REAL MODE of the various processors.  The MSB (bit 15)
;       is always a one on the 8086 and 8088 and a zero on the 286 and
;       386/486.  Bit 14 (NT flag) and bits 13/12 (IOPL bit field) are
;       always zero on the 286, but can be set on the 386.
;
;       For future compatibility of this test, it is strongly recommended
;       that this specific instruction sequence be used.  The exit codes
;       can of course be changed to fit a particular need.
;
;       CALLABLE FROM REAL MODE ONLY 
;
;       this routine was stolen from EMM/386 source code
;
;   ENTRY:  (none)
;
;   EXIT:   0 = 8086/8088
;           2 = 286
;           3 = 386
;          -4 = >=486
;------------------------------------------------------------------------------
;CPU086088       equ     0
;CPU186          equ     1       ;not detected here
;CPU286          equ     2
;CPU386          equ     4
;CPU486ORABV     equ     8

detect_processor proc near
	push si
	push di
	push ds
	push es

	pushf                           ; save entry flags

	xor     ax, ax                  ; 0000 into AX
	push    ax
	popf                            ; try to put that in the flags
	pushf
	pop     ax                      ; look at what really went into flags
	test    ax, 08000h              ;Q: was high bit set ?
	jnz     short cpu_is_8086       ;  Y: 8086/8088
	mov     ax, 07000h              ;  N: try to set the NT/IOPL bits
	push    ax
	popf                            ;      ... in the flags
	sti                             ; (for VDMM/IOPL0)
	pushf
	pop     ax                      ; look at actual flags
	popf                            ; restore entry flags
	test    ax, 07000h              ; Q: any high bits set ?
	jz      short cpu_is_286        ; N: 80286
    ;
    ; 386 or 486? See if we can set the AC (Alignment check) bit in Eflags
    ;   Need to insure stack is DWORD aligned for this to work properly
    ;
	push    cx
	xor     cx,cx                   ; Assume stack aligned
	mov     ax,sp
	and     ax,000011B
	jz      short NoStkAdj          ; Stack is aligned
	mov     cx,ax                   ; set "pop" count
	sub     sp,ax                   ; Move to DWORD aligned
NoStkAdj:
	pushfd                          ; save entry flags (DWORD)
	push    dword ptr 40000h        ; AC bit
	popfd
	pushfd
	pop     eax
	popfd                           ; Recover entry flags (DWORD)
	test    eax,40000h              ; Did AC bit set?
	jnz     short cpu_is_486        ; Yes, 486
	inc     ax                      ; Make AX non-zero
	add     sp,cx                   ; pop off alignment bytes
	pop     cx                      ; Entry cx
	mov     ax,CPU386               ; 386 detected
	jmp     short donedetection

cpu_is_486:
	add     sp,cx                   ; pop off alignment bytes
	pop     cx                      ; Entry cx
	mov     ax,CPU486ORABV          ; 486 detected
	jmp short donedetection

cpu_is_8086:
	popf                            ; restore flags
	mov ax,CPU086088                ; 8086/8088 detected
	jmp short donedetection
cpu_is_286:
	mov ax,CPU286                   ; 286 detected
	;;jmp short donedetection fall through
donedetection:
	pop	es
	pop	ds
	pop	di
	pop	si

	ret
detect_processor endp

zseg ends

end





















