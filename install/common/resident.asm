;***************************************************************************
;*
;*  RESIDENT.ASM   - Resident code and data used for passing information
;*                   from one executable to another.
;*
;*	Copyright (c) 1991 - Microsoft Corp.
;*	All rights reserved.
;*	Microsoft Confidential
;*
;*
;*  NOTE this segment MUST be first!!!
;*
;***************************************************************************

ifndef	UPGRADE

KB_INTERCEPT	EQU	4fh

DEL_KEY		EQU	53h
ALT_SHIFT	EQU	08h
CTL_SHIFT	EQU	04h

WARM_BOOT_CODE	EQU	1234h	

;-----------------------------------------------------------------------------;
;	BIOS DATA AREA LOCATED AT 40:00
;-----------------------------------------------------------------------------;

ROM_DATA SEGMENT AT 040h

	org	17h
KB_FLAG		LABEL BYTE

	org	072h
WarmBootFlag	LABEL WORD

ROM_DATA ENDS

;-----------------------------------------------------------------------------;
;	CPU POWER-ON STARTUP LOCATION AT ffff:00
;-----------------------------------------------------------------------------;

ROM_BIOS SEGMENT AT 0ffffh
	org	0

PowerOnReset	LABEL FAR

ROM_BIOS ENDS

endif

.MODEL LARGE,C

.XLIST
include install.inc

ifdef UPGRADE
include global.inc
FIRST_HARD_DISK		EQU	80h
extrn	IOMEGA_END:word
extrn	IOMREM:far
endif
.LIST

;***************************************************************************

MAX_CMD_TAIL_LEN equ 126d       ; Used to check for overflow when appending
                                ; switches to Setup's command line.


STACKSIZE        = 2048d

WriteAux    macro   dstr
        local WriteAuxStr, WriteAuxStrLen

ifdef   DEBUG

        jmp     short @F

WriteAuxStr     db  dstr, 10, 13
WriteAuxStrLen  equ $-WriteAuxStr

@@:
        push    ax
        push    bx
        push    cx
        push    dx
        push    ds

        mov     ax, cs                  ;Get segment of string into ds
        mov     ds, ax
        lea     dx, WriteAuxStr
        mov     cx, WriteAuxStrLen
        mov     bx, 0003h               ;standard aux device
        mov     ah, 040h                ;Write String
        int     21h

        pop     ds
        pop     dx
        pop     cx
        pop     bx
        pop     ax
endif
        endm

MYFARPTR      STRUC
        off     dw      ?
        sel     dw      ?
MYFARPTR      ENDS

SLOADPARAMS  STRUC
        segEnv      dw 0        ; WORD  segEnv
        lpCmdLine   dd 0        ; LPSTR lpCmdLine
        lpFCB1      dd 0        ; LPSTR lpFCB1
        lpFCB2      dd 0        ; LPSTR lpFCB2
SLOADPARAMS   ENDS

arena   struc
        sig     db      0       ; 'M' or 'Z' for last block
        own     dw      0       ; PSP value of owner process or 0 if free
        asiz    dw      0       ; size of block (not including header)
arena   ends

codeOFFSET equ OFFSET _TEXT:

;***************************************************************************


.DATA

EXTRN _psp:WORD, _osmajor:WORD


;***************************************************************************
;***************************************************************************

.CODE _TEXT
	ASSUME CS:_TEXT, DS:DGROUP

;********************** BEGIN CONTIGUOUS BLOCK ******************************
;The Install and vInfo structures must be contiguous.  Note that the
;vInfo structure is omitted in the non-upgrade case.

	PUBLIC Install
Install db SIZE INSTALL dup (?)  ; Global state structure

ifdef UPGRADE
	PUBLIC vInfo
vInfo 	db SIZE INFO dup (?)	 ; Global information structure.

DiskChange	db	0

	include	newint13.asm
endif

;********************** END CONTIGUOUS BLOCK ********************************

szCopyCmdLine  db 128 dup (?)    ; save copies to use after DS is gone
szCopyProgName db 128 dup (?) 

loadparams    db SIZE SLOADPARAMS dup (?) ; Exec block info struct.
MyPSP         dw ?                        ; Need stroage for PSP.

OldInt2Fh	dd	0	 ; Original Int 2Fh address.

ifndef	UPGRADE

;
; This is a 2 dimensional buffer of width MAX_SCRN_LINE_LEN=76+3(for a CR,LF
; and '$') and length MAX_STRINGS=26. This buffer is filled in by the 
; message WINSETUP_EXIT_TEXT defined in nudoswin.txt by programinitialize 
; in initial.c. This message is printed if control is returned to ResExec
; in the non-upgrade case.
;
	PUBLIC	ExitMsg
ExitMsg	LABEL 	BYTE
	REPT	26
	DB	79 dup(0)
	ENDM
endif


;****************************************************************************
;
; BOOL FAR ResExec(char far *szProgName,char far *szCmdLine)
;
; Start program with the command line we were passed.
;
;****************************************************************************

ResExec PROC FAR USES DS SI DI, szProgName:FAR PTR, szCmdLine:FAR PTR

    mov     ax, _psp           	; Save pointer to PSP, we may need it later.
    mov     MyPSP, ax

    call    FAR PTR InitNewInt2Fh ; Init. Int 2Fh vector if not done yet.

    ;
    ; Free all of our memory.
    ;

    WriteAux 'Calling Fremem Now !!'

    call   FreeMem

    WriteAux 'We have returned from fremem'

    ;Copy input parameters into code segment before changing stack.

    mov     es, szCmdLine.sel
    mov     di, szCmdLine.off     ;es:di -> szCmdLine (cmd line for exec)
    call    CopyCmd		  ;szCopyCmdLine now contains cmd line.

    mov     ds, szProgName.sel
    mov     dx, szProgName.off    ;ds:dx -> szProgName (program name to exec)
    call    CopyProgName	  ;ds:dx -> szCopyProgName

    ; DS == CS beyond this point!

    ;
    ;   Change to the NEW stack.
    ;

    mov	    cx, codeOFFSET resize
ifdef	UPGRADE
    cmp	    cs:[vInfo].info_chBernoulliDrv, 0
    je	    ResNoBern1
    mov	    cx, codeOFFSET IOMEGA_END
ResNoBern1:
endif
    
    cli
    mov     ax, cs
    mov     ss, ax
    mov     sp, STACKSIZE
    add	    sp, cx
    sti

    ;
    ; Init an execblock structure.
    ;

    mov     ax, cs
    mov     es, ax

    mov     di, codeOFFSET szCopyCmdLine ; es:di --> command line for Init

    call    Init
    push    es                          ; Save the return params from Init.
    push    bx


    ;
    ;    Shrink OUR program to include ONLY PSP + EXEC + STACK.
    ;

    mov     ax, cs:MyPSP                ; Blow away all memory beyond resize.
    mov     es, ax
    mov	    bx, cx
    add     bx, 100h + STACKSIZE
    mov     cl, 4
    shr     bx, cl
    inc     bx
    mov     ah, 4Ah
    int     21h                         ; Everything in your DS is now gone.

    pop     bx                          ; Restore exec parameters.
    pop     es

restart:

    WriteAux 'About to exec!'

    mov     ax, 4b00h                   ; DOS exec call
    int     21h

    WriteAux 'Back from the exec!'

    mov     ah, 4Dh		; Get exit code.
    int     21h
    push    ax

    call    FAR PTR RestoreOldInt2Fh ; Clean-up Int 2Fh vector

ifdef UPGRADE
    cmp	    cs:[vInfo].info_chBernoulliDrv, 0
    je	    ResNoBern2
    call    IOMREM
ResNoBern2:
    call    RestoreOld13
else
    call    PrintMsgAndReboot    ; Never returns.
endif

; Clear bottom 2 lines of display (assume 25 lines: numbered 0 to 24) and
; position cursor there. (By placing cursor on line 23, COMMAND.COM will
; do a line feed and place it on next line, line 24.)

    mov     ax,0602h             ; Scroll up 2 lines.
    mov     bh,07h               ; Normal video.
    mov     cx,1700h             ; Scroll last 2 lines.
    mov     dx,184fh
    int     10h

    mov     ah,0fh               ; Get Video mode.
    int     10h                  ; BH = current page.
    mov     ah,02h               ; Set cursor position.
    mov     dx,1700h             ; Next to last line.
    int     10h

    mov     ah,01h               ; Set Cursor type
    mov     cx,0607h             ; Assume text mode.
    int     10h

    pop     ax                   ; AL = WINSETUP return value
    mov     ah, 4ch              ; Terminate
    test    cs:[Install.inst_Flags],flag_fINT2FChained ; Q: INT 2F chained?
    jz      SHORT RE_Exit        ; No, go ahead and exit

; We have hooked INT 2F and cannot unhook since XMSMMGR has hooked
; it after us so we must terminate and stay resident to maintain INT 2F chain.
    mov     bx, codeOFFSET resize
ifdef	UPGRADE
    cmp	    cs:[vInfo].info_chBernoulliDrv, 0
    je      SHORT ResNoBern3
    mov     bx, codeOFFSET IOMEGA_END
ResNoBern3:
endif
    add     bx, 100h + STACKSIZE
    mov     cl, 4
    shr     bx, cl
    inc     bx

    or      cs:[Install.inst_Flags],flag_fINT2FOrphan  ; We are INT 2F orphan
    mov     dx,bx                ; DX = # of para.
    mov     ah, 31h              ; Terminate and stay resident (INT 2F chained)

RE_Exit:

    int     21h

    ret

ResExec ENDP


;****************************************************************************
;
; FAR NewInt2Fh()
;
; Int 2Fh, function INT2F_INSTALL_DATA -- Get pointer to resident
; installation data.
;
; Entry: AX = INT2F_INSTALL_DATA (constant)
;
; Exit:  ES:BX -> _Install structure
;
;****************************************************************************

NewInt2Fh PROC FAR
        test    cs:[Install.inst_Flags],flag_fINT2FOrphan  ; We are INT 2F orphan
        jnz     SHORT n2f40
	cmp	ax,INT2F_INSTALL_DATA	;My function?
	je	n2f50			; -yes, jump.
n2f40:
        jmp     DWORD PTR cs:OldInt2Fh	; -no, chain to original handler.

n2f50:	mov	ax,cs
	mov	es,ax
	mov	bx,codeOFFSET Install	;es:bx -> Install.
	mov	ax,0FFFFh		;Indicate presence.

	iret
NewInt2Fh ENDP


;****************************************************************************
;
; FAR InitNewInt2Fh()
;
; Hook Int 2Fh vector.
;
; Entry: none
;
; Exit:  none
;
;****************************************************************************

InitNewInt2Fh PROC FAR USES DS ES,

	mov	ax,WORD PTR CS:OldInt2Fh.off	
	or	ax,WORD PTR CS:OldInt2Fh.sel	;Already initialized?
	jnz	i2f50				; -yes, don't re-init.

	mov	ax,352Fh			;Get vector 2Fh
	int	21h
	mov	WORD PTR CS:OldInt2Fh.off,BX	;Save the offset
	mov	WORD PTR CS:OldInt2Fh.sel,ES 	;Save the segment

	mov	dx,codeOFFSET NewInt2Fh	
	push	cs			
	pop	ds				;ds:dx -> NewInt2Fh
	mov	ax,252Fh			;Set vector 2Fh
	int	21h

i2f50:	ret
InitNewInt2Fh ENDP


;****************************************************************************
;
; FAR RestoreOldInt2Fh()
;
; Restore Int 2Fh vector to its original value.
;
; Entry: none
;
; Exit:  none
;
;****************************************************************************

RestoreOldInt2Fh PROC FAR USES DS CX,

	mov	dx,WORD PTR CS:OldInt2Fh.off	
	mov	ax,WORD PTR CS:OldInt2Fh.sel
	mov	cx,ax
        or      cx,dx                   ;Did we hook INT 2F?
	jz	r2f50			; -no, don't restore.

        push    ax
        push    dx
        mov     ax,352Fh                ;Get current vector 2Fh
        int     21h                     ; ES:BX = current INT 2Fh
        pop     dx
        pop     ax
        cmp     bx,codeOFFSET NewInt2Fh ; Q: Did INT 2Fh change?
        jnz     SHORT r2F_Chained       ;    Y: set flag and don't restore
        push    cs
        pop     cx
        push    es
        pop     bx
        cmp     cx,bx                   ; Q: Did segment change?
        jnz     SHORT r2F_Chained       ;    Y: set flag and don't restore

r2f_Restore:
	mov	ds,ax			;ds:dx -> Original Int 2Fh handler.
	mov	ax,252Fh		;Set vector 2Fh
	int	21h

	xor	ax,ax			;Reset vector copy.
        and     cs:[Install.inst_Flags],NOT flag_fINT2FChained
	mov	WORD PTR CS:OldInt2Fh.off,ax
	mov	WORD PTR CS:OldInt2Fh.sel,ax
r2f50:	ret

r2F_Chained:
        or      cs:[Install.inst_Flags],flag_fINT2FChained ; INT 2F chained
        jmp     SHORT r2f50


RestoreOldInt2Fh ENDP


ifdef UPGRADE
;****************************************************************************
;
; FAR RealModeInt13h()
;
; Real-mode Int 13h buffer and function for use by GUI Dos Setup;
; called via DPMI.  The 512-byte buffer is immediately followed by the
; Int 13h function; there can be no intervening pad.
;
; Entry: Setup up by DPMI server.
;
; Exit:  Far return back to DPMI application.
;
;****************************************************************************

	ALIGN 4			;Ensure RMInt13hFunc is on WORD boundary.
	PUBLIC RealModeInt13h

RealModeInt13h db 512 dup (?)

RMInt13hFunc PROC FAR		;Must immediately follow buffer -- NO PAD.
	int	13h
	retf
RMInt13hFunc ENDP
endif


ifndef	UPGRADE
;****************************************************************************
;
;	Proc	:	PrintMsgAndReboot	
;
;	Description:
;		Prints out each string in the 2D buffer ExitMsg. Then waits
;	for a keyboard input and then reboots.
;
;****************************************************************************
PrintMsgAndReboot	proc	near

	mov	dx, offset ExitMsg
	mov	ax, cs
	mov	ds, ax


PEMGetNext:
	mov	si, dx
	cmp	byte ptr [si], 0
	je  	PEMdone
	mov	ah, 09h
	int	21h
	add	dx, 80
	jmp	PEMGetNext

PEMdone:
	mov	ah, 07h		; wait for keyboard input
	int	21h

	;
	; On PS/2 machines just doing a reset here brings up some 
	; mouse errors when the machine reboots. So we'll try and reset
	; the pointing device before we proceed with the reboot.
	;
	mov	cx, 5			; let's do it a max of 5 times
PEMresetmouse:
	mov	ax, 0C201h
	int	15h
	jnc	PEMcont			; mouse successfully reset
	dec	cx
	jcxz	PEMcont		
	cmp	ah, 04h			; Q: was the error a resend
	je	PEMresetmouse		; Y: try again

PEMcont:

	;
	; BUGBUG: This code is taken from reboot.asm in ..\lib\bios. 
	; Why can't we set up DS here instead of setting it up in 
	; 2 different places below. Is this because DS can be destroyed
	; by the int 21 ax=3515h or the int 15h ax=4f53h? DS should not
	; be destroyed by these calls in any case.
	;
	mov	AX,3515h
	int	21h			; Get int 15h vector in ES:BX
	mov	AX,ES			; AX == Segment
	or	AX,BX			; Is this a NULL ptr
	jz	PEMWarmBoot		; If zero we can't do an int 15h

	mov	ax, seg WarmBootFlag
	mov	ds, ax
	assume	DS:ROM_DATA

	mov	KB_FLAG,ALT_SHIFT OR CTL_SHIFT
	mov	AX,(KB_INTERCEPT SHL 8) OR DEL_KEY
	int	15h			; Put Ctrl/Alt/Del into key buffer

PEMWarmBoot:

	cli
	cld

	mov	ax, seg WarmBootFlag
	mov	ds, ax
	assume	DS:ROM_DATA
	mov	WarmBootFlag, WARM_BOOT_CODE
	jmp	PowerOnReset
		; Jump to the processor power-on address FFFF:0000h
     
PrintMsgAndReboot	endp
endif

;****************************************************************************
;
;   ---- EXEC RESIZE FENCE ----
;
;   ALL CODE PAST HERE IS TOAST!
;
;****************************************************************************

resize:


;****************************************************************************
;
;   Copy program name to szProgName.
;
;   Entry: ds:dx -> program name to copy.
;
;   Exit:  ds:dx -> szCopyProgName
;
;****************************************************************************

CopyProgName PROC NEAR

    ;
    ; How long is the file name?
    ;
    or      dx, dx
    jz      nofile

    mov     ax, ds
    mov     es, ax
    mov     di, dx			;es:di -> file name
    mov     cx, 0FFFFh                  ;cx must be <> 0 for REP
    xor     ax, ax
    repnz   scasb
    not     cx				;Length in cx, including null.

    mov     si, dx			;ds:si -> file name
    mov     ax, cs
    mov     es, ax
    mov     di, codeOFFSET szCopyProgName    ;es:di -> szProgName
    push    es
    push    di
    rep     movsb                       ;mov string at ds:si to es:di

    pop     dx
    pop     ds				;ds:dx -> szProgName
nofile:
    ret

CopyProgName ENDP



;****************************************************************************
;
;   Copy CMD line to szCopyCmdLine.
;
;   es:di = pointer to cmd line to copy.
;
;****************************************************************************

CopyCmd PROC NEAR USES DS

    ;
    ; How long is the command line?
    ;
    or      di, di
    jz      nocmd
    push    di
    mov     cx, 0FFFFh                  ;cx must be <> 0 for REP
    xor     ax, ax
    repnz   scasb
    not     cx
    dec     cx                          ;Length in cx

    ;
    ; copy command line to out PSP
    ; form the length prefixed command line that DOS wants
    ;
    pop     si

    mov     ax, es
    mov     ds, ax                      ;ds:si == command line
    mov     ax, cs
    mov     es, ax
    mov     di, codeOFFSET szCopyCmdLine ;es:di == szCopyCmdLine
    mov     ax, cx
    stosb                               ;Length byte           => es:di
    rep     movsb                       ;.. followed by string => es:di
    mov     ax, 0DH                     ;.. followed by a \n   => es:di
    stosb
nocmd:
    ret

CopyCmd ENDP


;****************************************************************************
;
; void Init(void);
;
; Sets up an execblock structure in preparation for a Dos Exec Int 21h / 4bh.
;
; Passes current environment.
;
; Entry: es:di --> command line
;
; Exit:  es:bx --> execblock structure
;
;****************************************************************************

Init PROC NEAR

    mov     cs:loadparams.lpCmdLine.sel, es     ; store command line
    mov     cs:loadparams.lpCmdLine.off, di


KeepEnv:
    mov     ax, cs:MyPSP
    mov     es, ax
    mov     ax, es:[002Ch]

GotEnv:
    mov     cs:loadparams.segEnv, ax            ;loadparms.segEnv = ENV;
    mov     ax, cs
    mov     cs:loadparams.lpFCB1.off, 5ch       ;loadparms.lpFCB1 = parent's fcb0
    mov     cs:loadparams.lpFCB1.sel, ax
    mov     cs:loadparams.lpFCB2.off, 6ch       ;loadparms.lpFCB2 = parent's fcb1
    mov     cs:loadparams.lpFCB2.sel, ax

    mov     es, ax
    mov     bx, codeOFFSET loadparams           ;es:bx --> param block

    ret

Init ENDP


;****************************************************************************
;
; FreeAll() - free all DOS memory allocated by the current program.
;
;****************************************************************************

FreeMem PROC NEAR

;
;       IMPORTANT NOTE:
;
;       DOS 3.30 and later allow the user to increase the size of the
;       file handle table.  If the file handle count > 20, then a far
;       segment is allocated to store the table.  This segment must be
;       freed up before the child program is exec-ed.  This code relies
;       on the fact that DOS will free up that far segment if the handle
;       count is set back to 20.  If DOS ever changes, this code will fail.
;
        mov     ax, word ptr [_osmajor]
        xchg    ah, al          ; AH = _osmajor, AL = _osminor
        cmp     ax, (3 shl 8) + 30
        jb      pre_DOS330      ; _osmajor:_osminor < 3.30?

        mov     bx, 20          ; IF >= DOS 3.30 THEN
        mov     ah, 67h         ; Set Handle Count on principle
        int     21h             ; set number of handles to default value

pre_DOS330:
        mov     bx, _psp
        mov     es, bx
        mov     dx, es:[002Ch]  ; get the environment segment
        mov     cx, 50h

;       WARNING - any errors past this point - must go to execpanic
;       now free all blocks of memory belonging to this process
;       except the PSP and new environment block

;       cx = Paranoia count.
;       bx = PSP
;       dx = environment segment

freemore:

;        WriteAux 'We're in the freemore loop now.'

        dec     cx
        jcxz    free_done       ; If paranoia count goes to zero panic !!
        push    bx              ; save PSP
        mov     ah, 52h         ; magic DOS call
        int     21h
        mov     ax, es:[bx-2]   ; ax = first arena segment!!!
        pop     bx              ; restore PSP

;       ax = current segment to check

findused:

;        WriteAux 'We're in the findused loop.'

        mov     es, ax
        inc     ax
        cmp     es:[sig], 'M'   ; Is this really an arena header ?
        jne     free_done       ; If not, lets get out !!
        cmp     ax, bx          ; is it PSP?
        je      atend           ;   yes - skip
        cmp     ax, dx          ; is it environment?
        je      atend           ;   yes - skip

        cmp     es:[own], bx    ; do we own it?
        jne     atend           ;   no -skip

        mov     es, ax          ; es = must be one past arena header
        mov     ah, 49h         ; free up the segment
        int     21h
        jmp     freemore        ; and start over - even if error

atend:
        add     ax, es:[asiz]   ; add in segment size

        jnc     @f
        jmp     freemore        ;   error - changed arena - retry

@@:
        cmp     es:[sig], 'Z'   ; last segment?
        jne     findused        ;   no - check next segment

free_done:
	ret

FreeMem ENDP


_TEXT ENDS

END
