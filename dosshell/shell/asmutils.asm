;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

; ZZZZZZ Are there bugs with offset calculations?? Is DGROUP the right
; assume for DS? I am unable to access variable DoQuickCompare from 'C'
; The offset is off by 64 bytes!!

?WIN = 0                ;Not windows;
?PLM = 1                ;DO use pl/m
include cmacros.inc

DOS_NAMETRANS   EQU             60h

sBegin data
	crlf    db  0dh, 0ah, '$'
sEnd data

sBegin code
    assumes cs, code
    assumes ds, DGROUP

int10chain dd 0         ;; int 10 chain pointer 


;;;XMS calls are hear so dosshell can delete XMS when
;;;user deletes an app in tasklist
xmscallptr dd  0
;;; initialized XMS
;;; returns 0 if no XMS
;;; otherwise returns 1 and sets global ptr
cProc InitXMS, PUBLIC, <si,di>
cBegin InitXMS
	mov ax,4300h
	int 2fh
	cmp al,80h
	jne notinstalled
	mov ax,4310h    ;; 
	int 2fh
	mov WORD PTR xmscallptr[0],bx
	mov WORD PTR xmscallptr[2],es
	mov ax,1
	jmp short isinstalled
notinstalled:
	xor ax,ax
isinstalled:
	
cEnd

cProc FreeXMSHandle, PUBLIC, <si,di>
parmW handle
cBegin FreeXMSHandle
	;; unlock first
	mov     ah,0Dh
	mov     dx,handle
	call    DWORD PTR [xmscallptr]
	mov     ah,0Ah
	mov     dx,handle
	call    DWORD PTR [xmscallptr]
cEnd FreeXMSHandle

cProc  Get_CWD, PUBLIC,  <si,di>
parmW   destination
cBegin Get_CWD
	;;; first get current drive
	mov ah,19h
	int 21h
	;;; al has drive letter zero based; 

	mov dl,al      ; save drive letter
	add al,'A'
	mov si,destination
	mov [si],al
	inc si
	mov BYTE PTR [si],':'
	inc si
	mov BYTE PTR [si],'\'
	inc si
	
	inc dl          ;next calls wants one based drive letter
	mov ah,47h      ;getcwd
	int 21h

cEnd Get_CWD

cProc  Dos_Version, PUBLIC,  <si,di>
cBegin Dos_Version
	mov ax,3000h
	int 21h
	xor ah,ah
cEnd  Dos_version

;
; This call replaces the puts C function.
;
;
cProc  Shell_TTY_Out, PUBLIC,  <si,di,ds,es>
parmW  outstring
cBegin Shell_TTY_Out
	push    ds
	pop     es

	xor     al,al
	mov     dx,outstring
	mov     di,dx   
	mov     cx,-1
	cld
	repne scasb
	dec     di
	mov     BYTE PTR [di],'$'
	mov     ah,09h
	int     21h
	mov     BYTE PTR [di],0
	lea     dx,crlf
	;mov    ah,09h
	int     21h

cEnd   Shell_TTY_Out


;;;
;;;BUG BUG BUG HACK HACK HACK
;;;Standard hook/unhook code, see int10hook below
;;;
cProc  HookISR10, PUBLIC,  <si,di,ds,es>
cBegin HookISR10
	    push cs
	    pop  ds

	    mov ax,3510h                 ; save interrupt 10 location
	    int 21h

	    mov WORD PTR cs:int10chain,bx
	    mov ax,es
	    mov WORD PTR cs:int10chain[2],ax

	    mov dx,offset Int10Hook      ;patch interrupt 10 vector with
	    ;;; ds already holds code segment
	    mov ax,2510h                 ;our own routine
	    int 21h
cEnd  HookISR10

cProc  UnHookISR10, PUBLIC,  <si,di,ds,es>
cBegin UnHookISR10

	    mov dx,WORD PTR cs:int10chain        ;patch interrupt 10 vector with
	    mov ds,WORD PTR cs:int10chain[2]
	    mov ax,2510h                 ;the old handler
	    int 21h
nothooked:
cEnd  UnHookISR10

;;;BUG BUG BUG HACK HACK HACK
;;;We were having problems with graphics drawing when the mouse was
;;;loaded.  We traced the problem down to int10 function FA; the device
;;;drivers call this to get mouse info. However, we turn off the mouse
;;;before all calls to the driver, so it doesn't need to know.
;;;the drivers mouse code calls the ROM BIOS that is pathetically slow from hell.
;;;If we lie to the device drivers, graphics performance is enhanced
;;;significantly since the BIOS calls are no longer made.
;;;Note we only need to bracket the driver initialization with this hook
;;;as the drivers set internal flags
Int10Hook proc far
	cmp     ah,0FAh
	je      mousehack
	jmp     DWORD PTR cs:[int10chain]
mousehack:
	xor     bx,bx
	iret
Int10Hook endp


;;;;  BOOL translate_name(char far *src, dest) ;
;;;;  returns 0 if call succeeds, else 1.
cProc  translate_name, PUBLIC,  <si,di,ds,es>
parmW src_segment
parmW src_offset
parmW dest_segment
parmW dest_offset
cBegin  translate_name
    mov di, dest_offset
    mov es, dest_segment

    mov si, src_offset
    mov ds, src_segment
    assumes     ds,nothing

	mov     ah, DOS_NAMETRANS
	int     21h
	
	mov ax, 0       ; Zero out ax without affecting CY flag.
	rcl ax, 1

cEnd  translate_name

;;;; This function returns TRUE (AX = 1) if the two ALT keys should be
;;;; treated differently by CW, else it returns FALSE. We want to treat
;;;; the left ALT key and the right ALT key (also known as ATL-GR)
;;;; differently if KEYB is installed and the country is other than
;;;; US and we are on the other side of the "fence" (i.e., in the
;;;; Ctrl+Alt+F2 side).
;;;;
cProc  FTreatAltsDifferently, PUBLIC,  <si,di,ds,es>
cBegin FTreatAltsDifferently
	mov	ax, 0ad80h
	int	2fh	; see if KEYB is installed, return value of AL=0ffh
	inc	al	; means KEYB installed, else AL remains 80h
	jnz	FTADTreatSame
	
	; If we are here, it means that KEYB is installed
	cmp	WORD PTR es:[di+16h], 'SU'	; Is it US?
	jz	FTADTreatSame

	; If we are here, KEYB is loaded and the country is not US!
	mov	ax, 0ad83h	; ah is not 0adh any more!
	int	2fh		; If COUNTRY_FLAG is enabled, BL=0ffh, else
	inc	bl		; BL=00 meaning it is in a Ctrl+Alt+F1'd state.
	jnz	FTADTreatSame

	mov	al, 01
	jmp 	short FTADDone

FTADTreatSame:
	xor	al, al

FTADDone:
	xor	ah, ah
cEnd  FTreatAltsDifferently


;---------------------------------------------
; RepeatMove - very fast repeat move with long args
;       Syntax   RepeatMove(char far *dest, char far *src, cnt)
;---------------------------------------------
cProc   RepeatMove,<PUBLIC,FAR>,<di,si>
	parmD   lpDest
	parmD   lpSrc
	parmW   cnt
cBegin
	push    ds
	les     di,lpDest
	lds     si,lpSrc
	mov     cx,cnt
	mov     BX,CX
;see which is bigger
	cmp     SI,DI           ;is source bigger than dest?
	ja      repm1           ;this is only a 16 bit guess, but...
				;if they differ in segments it should be
				;irrelevant
	std
	add     si,cx
	add     di,cx
	dec     si
	dec     di
	shr     BX,1
	jnc     repm0a
	movsb
repm0a:
	shr     CX,1
	jcxz    repm2
	dec     si
	dec     di
	rep     movsw
	jmp     short repm2
;move upwards
repm1:
	cld
	shr     CX,1
	jcxz    repm1a
	rep     movsw
repm1a:
	shr     BX,1
	jnc     repm2
	movsb
repm2:
	cld
	pop     ds
cEnd



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
CPU086088       equ     0
CPU186          equ     1       ;not detected here
CPU286          equ     2
CPU386          equ     4
CPU486ORABV     equ     8

cProc DetectProcessor, PUBLIC <si,di,ds,es>
cBegin DetectProcessor
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
	;debug int 3
cEnd DetectProcessor
	

;; for profiler
;
;cProc  ClockOn, PUBLIC,        <si,di,ds,es>
;cBegin ClockOn
;       push ax
;       mov ax,1
;       int 3
;       pop ax
;cEnd  ClockOn
;
;cProc  ClockOff, PUBLIC,       <si,di,ds,es>
;cBegin ClockOff
;       push ax
;       mov ax,0
;       int 3
;       pop ax
;cEnd  ClockOff

;;;WARNING .386 is still on!

sEnd   code

end
