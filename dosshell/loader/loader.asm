;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

seg_size equ (((offset last) - (offset start)) + 10fh)/16
seg_swtch_size equ (((offset last_switcher) - (offset start)) + 10fh)/16

SWITCHER EQU 0FFFFh
;;;QLOADER EQU 0FFFFh
;;;
;;;This is the start-up code for the shell.
;;;
IFNDEF QLOADER
extrn __astart:FAR               ;;; address of C startup
ENDIF
MULTMULT_INT equ 4A05h            ; interrupt 2f interface
stderr  equ 2                ; standard error
cr      equ 0dh              ; ASCII carriage return
lf      equ 0ah              ; ASCII linefeed
VOODOO  equ 02h              ; voodoo character that cannot be typed at command
DOPAUSE equ 01h              ; pause before erasing screen?
COMMANDLINEIN equ 80h        ; offset of command line passed in (PSP)
;;;COMMANDLINEOUT equ 0C0h      ; offset of the command line passed to shell
COMMANDLINEOUT equ 080h      ; offset of the command line passed to shell

COMMANDMAXLENGTH equ 121

EXITOFFSET equ 0
DMAOFFOFFSET equ EXITOFFSET+2
DMASEGOFFSET  equ DMAOFFOFFSET+2
VOODOOOFFSET equ DMASEGOFFSET+2
PAUSEOFFSET  equ VOODOOOFFSET+1

cseg    segment para public 'CODE'

	assume  cs:cseg
IFDEF QLOADER
	org 100h
ENDIF
start   proc
;;;     PROGRAM EXECUTION BEGINS HERE
;;;     ES:0 is the PSP
;;;     Get my PSP to look at command line
	mov     ax,es
	mov     cs:myPSP,ax
	;;; scoot the command line over
	call scoot_command

	;;; remember our start-up name
	call find_startup_name
	;;; remember our allocation strategy (for UMB support)
	mov ax,5800h    ; Get allocation Strategy ;output in ax
	int 21h
	mov WORD PTR cs:myAllocationStrategy,ax
	mov ax,5802h    ; Get link state; output in ax
	int 21h
	mov WORD PTR cs:myUmbLinkState,ax

IFDEF QLOADER
	call startup_to_exe
ENDIF
	mov     bx,COMMANDLINEIN
	mov     al,BYTE PTR es:[bx+VOODOOOFFSET]
	cmp     al,VOODOO
IFNDEF QLOADER
	je      do_cshell
ENDIF
	call print_copyright
	jmp     do_resident
IFNDEF QLOADER
	;;;; run the shell interface program
	;;;; the parameters indicate where to place the data for the
	;;;; next program to launch--client_name
do_cshell:
	mov     al,BYTE PTR es:[bx+PAUSEOFFSET]       ; put pause byte into global
	mov     cs:WaitBeforeErase,al
	mov     ax,WORD PTR es:[bx+DMAOFFOFFSET]
	mov     WORD PTR cs:dma_ptr[0],ax           ;; offset
	mov     ax,WORD PTR es:[bx+DMASEGOFFSET]
	mov     WORD PTR cs:dma_ptr[2],ax            ;; segment
	jmp    __astart
ENDIF
main_loop:
	;;;     re-exec the shell interface
	;;; grow the resident back to max size
	mov     ax,cs:myPSP     ; ES = segment to shrink
	mov     es,ax
	mov     bx,seg_swtch_size     ; BX = new segment size
	mov     ah,4ah          ; AH = modify memory block
	int     21h             ; grab excess memory
	cmp     cs:reinitswitchervars,1
	jne     dontreinitswitchervars
	mov     cs:gresSwitchInfo.Global_Flags,0        ;dosshell will resinit switcher data
	mov     cs:reinitswitchervars,0
dontreinitswitchervars:
	;;;Restore our Allocation Strategy
	mov     cs:inShell,0    ;;not yet in shell (for ctrl+c,etc)
	mov     ax,5801h ;Set Allocation Strategy
	mov     bx,cs:myAllocationStrategy
	int     21h
	mov     ax,5803h ;Set Link state
	mov     bx,cs:myUmbLinkState
	;;;BUG BUG shouldn't need to do this harishn!
	xor     bh,bh
	int     21h

	mov     cs:cmd_loc,COMMANDLINEIN
	;;;     setup dma location (offset,segment)
	mov     ax,cs:myPSP
	mov     es,ax
	mov     BYTE PTR es:[COMMANDLINEOUT],127
	mov     BYTE PTR es:[COMMANDLINEOUT+EXITOFFSET],0
	mov     WORD PTR es:[COMMANDLINEOUT+DMAOFFOFFSET],COMMANDLINEIN
	mov     WORD PTR es:[COMMANDLINEOUT+DMASEGOFFSET],ax
	mov     BYTE PTR es:[COMMANDLINEOUT+VOODOOOFFSET],VOODOO
	cmp     BYTE PTR cs:dopauseflag,1
	mov     BYTE PTR es:[COMMANDLINEOUT+PAUSEOFFSET],0
	jne     dontpause
	mov     BYTE PTR es:[COMMANDLINEOUT+PAUSEOFFSET],DOPAUSE
dontpause:
	mov     BYTE PTR cs:dopauseflag,1

	mov     ax,cs
	;mov     ds,ax
	mov     es,ax
	mov     dx,cs:startup_name_off
	mov     ds,cs:startup_name_seg
	;mov     dx,offset cseg:shell_name
	mov     bx,offset cseg:par_blk
	mov     ax,4b00h        ; AX = EXEC/run program
	mov     cs:inShell,1
	int     21h             ; carry = EXEC failed
	mov     cs:inShell,0
	jc      cant_load_shell
	mov     ax,cs:myPSP
	mov     es,ax

	;;exit
	mov al, BYTE PTR es:[COMMANDLINEIN+EXITOFFSET]
	cmp al,0FFh
	jne dont_go_away

go_away:
	cmp     cs:isBallPoint,1
	je      nohookspresent
	call    Mouse_Unhook33
	call    Mouse_Unhook15
nohookspresent:
IF SWITCHER
	;;;***
	call    Switcher_UnHook

	;;; We need to exit entirely, set magic EXIT field!
	mov BYTE PTR es:[COMMANDLINEIN+EXITOFFSET], 0FFh
	;;;***
ENDIF
	mov ax,04C00h
	int 21h
	;;
 dont_go_away:
	;;; we are running somebody else now
	;;; we always turn off UMBs for launching because the
	;;; switcher cannot be loaded in a UMB, and command.com
	;;; doesn't need it. Users can still load programs in UMBS,
	;;; however they need to turn this on explicitely for each
	;;; application.
	mov     bx,cs:myAllocationStrategy
	and     bl,07Fh  ;mask off high bit to mean turn off high allocs
	mov     ax,5801h         ;Set Allocation Strategy input in BX
	int     21h
	mov     ax,5803h         ;link/unlink UMBs bx = 0 is unlink,bx =1 is link
	xor     bx,bx
	int     21h
	;;; is command line magic switcher command?
	cmp     BYTE PTR es:[COMMANDLINEOUT+42h],0FEh
	je      continueexec
	call    shrink_resident_size
continueexec:

	mov     cs:cmd_loc,COMMANDLINEIN+40h
	push    cs
	pop     es
	;mov     ax,cs
	;mov     es,ax
	mov     bx,offset cs:par_blk
	mov     dx,COMMANDLINEIN
	mov     ax,cs:myPSP
	mov     ds,ax

	mov     ax,4b00h        ; AX = EXEC/run program
	int     21h             ; carry = EXEC failed
	jc      unable_to_launch
cant_load_second:
cant_load:
	jmp     main_loop       ; execute forever
unable_to_launch:
	mov     dx, offset Cant_Run_It
	call    Get_User_Response
	or      ax,ax
	jz      cant_load_second
	jmp     short dont_go_away
cant_load_shell:
	mov     dx, offset Query_string
	call    Get_User_Response
	or      ax,ax
	;;;     0 = no
	jz      go_away
	jmp     short cant_load

start   endp

shrink_resident_size proc near
	;;; shrink the resident back to min size
	mov     ax,cs:myPSP     ; ES = segment to shrink
	mov     es,ax
	mov     bx,seg_size     ; BX = new segment size
	mov     ah,4ah          ; AH = modify memory block
	int     21h             ; free excess memory
	mov     BYTE PTR cs:reinitswitchervars,1
	ret
shrink_resident_size endp

;; input dx is message

Get_User_Response proc near

doquery:
	mov     ah, 09h
	push    cs
	pop     ds
	int     21h

	; flush all characters from keyboard queue
FlushKbdLoop:
	mov     ah,1            ; command code for check status
	int     16h             ; Is there a char in the key board buffer?
	jz      FlushDone       ; No, Flush complete

	xor     ah,ah           ; Remove the character from Keyboard buffer
	int     16h             ; Get char
	jmp     FlushKbdLoop

FlushDone:
	mov     ax, 0c01h
	int     21h
	cmp     al, NO_KEY         
	je      user_said_no
	cmp     al, YES_KEY        
	je      user_said_yes
	jmp short doquery
user_said_yes:
	mov     ax,1
	ret
user_said_no:
	mov     ax,0
	ret

Get_User_Response endp

;;;Ctrl_C handler; jumped to--no return!
Ctrl_C proc
	cmp cs:inShell,1
	jne reload_shell        ; near jump out of range
	iret                    ; are in the shell-startup-window so ignore
reload_shell:
	jmp main_loop           ; re-load shell
Ctrl_C endp


;;;; Fail all criticals during startups
fail_crit proc
    mov     al,3                ;;; fail the int21 call
    iret
fail_crit endp

num_inits db 0
int33ret dd ?
saveax dw ?


chain_int33 DD  ?
chain_int15 DD  ?

ifdef JAPAN

COPYRIGHT_LEN   =       40

ms_copyright    db      COPYRIGHT_LEN dup (0)

endif

;;; BUG BUG BUG  BUG BUG this is evil code as it bypasses
;;; hardware initialization! This is done because the
;;; initialization is toooo slow on switches; we allow init
;;; to happen once, then assume its okay. 
;;;  
;;; This is a hook on the mouse int 33 chain that keeps track of
;;; when function 0 (hard reset) occurs
handle_33 proc far
    cmp ax,0                            ; function 0?
    je  special_33
chain_33:
    jmp DWORD PTR [chain_int33]         ; chain back onto the chain
special_33:                             ; keep track of num inits (roll over at 512!)
    pushf
    call DWORD PTR[chain_int33]         ; call through -- we must see return
    or  ax,ax                           ; ax=0 if no driver
    jz special_cleanup  
    inc cs:num_inits                    ;
special_cleanup:
    iret
handle_33 endp

;;; handle_15
;;; This is a hook on the int 15 chain that blows off calling
;;; the first 6 hardware mouse functions when we are in the mouse
;;; function 33 hard reset. We do this because these functions
;;; are unacceptably slowwww.
handle_15 proc far
    cmp ax,0c200h                       ;is one of the six main mouse calls?
    jae possible_mouse                  ;usually fall through
chain_15:
    jmp DWORD PTR [chain_int15]         ;chain onto the chain
reset_mouse_hacks:
    mov cs:num_inits,0
    jmp short chain_15
possible_mouse:
    cmp ax,0c206h                       ;is it one of the six mouse calls?
    ja  chain_15                        ; if not, chain on
    ;else fall through to special case

special_15:
    ;;;enhanced mode windows needs these bacause it needs to turn
    ;;;off i/o since it is virtualizing i/o
    push    ax
    mov     ax,1600h ;enhanced mode callout
    int     2fh
    test    al,7fh   ;if not low bits set 
    pop     ax
    jnz reset_mouse_hacks

    cmp al,02                           ; function two we let through
					; twice because mouse is detecting
					; ballpoint mouse every function 0
    je  chain_15
    cmp cs:num_inits,1                  ; and we have already inited once
    ja  skip_hardware_reset             ;
    jmp short chain_15                  ;

skip_hardware_reset:                    ; then don't chain on
    clc                                 ; clear errors
    xor bh,bh                           ;
    mov ah,bh                           ; and return to caller (int 33 function 0)
    iret        
handle_15 endp


Mouse_UnHook33 proc
    push ds
    push cs
    pop ds
    lds dx, chain_int33
    mov  ax, 2533h
    int  21h         ; Restore old int 33 handler
    pop ds
    ret
Mouse_UnHook33 endp

Mouse_UnHook15 proc
    push ds
    push cs
    pop ds
    lds dx, chain_int15
    mov  ax, 2515h
    int  21h         ; Restore old int 15 handler
    pop ds
    ret
Mouse_UnHook15 endp


IF SWITCHER

;;;***
include swtch_re.inc
;;;***
ENDIF
; ----  Program resident data area  ----
;

include resmsg.inc

startup_name_seg dw ?
startup_name_off dw ?

dma_ptr     dd  0
PUBLIC dma_ptr
dopauseflag     db  0
reinitswitchervars db 0         ; set if switcher globals are invalidated

myPSP       dw  ?

myAllocationStrategy dw ?  ;initial results from int 21 ah = 5800
myUmbLinkState       dw ?  ;initial results from int 21 ah = 5803

inShell db      0          ;in shell Ctrl-C startup window?
isBallPoint     db 0          ;detected a ballpoint mouse?

par_blk  dw     0               ; use current environment
cmd_loc  dw     COMMANDLINEOUT  ; command-line address
cmd_seg  dw     0               ; fill in at initialization
	 dw     offset fcb1     ; default FCB #1
fcb1_seg dw     0               ; fill in at initialization
	 dw     offset fcb2     ; default FCB #2
fcb2_seg dw     0               ; fill in at initialization

fcb1     db     0
	 db     11 dup (' ')
	 db     25 dup ( 0 )
fcb2     db     0
	 db     11 dup (' ')
	 db     25 dup ( 0 )

	 dw     25 dup ( 0 )   ; program stack area
stk      dw     0

last equ $
gresSwitchInfo db SIZE Switch_Info DUP(?)
;;last equ $
last_switcher equ $             ; last address used if switcher present

;;;** transient data/code **
WaitBeforeErase db 0
PUBLIC WaitBeforeErase
include transmsg.inc
do_resident:
IF SWITCHER

;
;   Determine if this is a ballpoint mouse.  If so, don't do funky
;   mouse hacks
;   NOTE this detection code came from window's 386 (with modifcations!)
       mov     cs:IsBallPoint,1 

       mov     ax,0c200h         ;check for error if we
       mov     bh,0             ;try to disable the mouse
       int     15h              ;we do this because on some
       jc      not_ballpoint    ;machines c202,bh=0 done twice locks the keyboard
				;if no mouse driver is loaded.  This call
				;will return carry 

       mov     ax, 0C202h
       mov     bh, 0
       Int     15h                             ; Set to 10hz
       jc      not_ballpoint
 
       mov     ax, 0C202h
       mov     bh, 2              ; Set it back to 40hz
       Int     15h
       jc      not_ballpoint
 
       mov     ax, 0C204h
       Int     15h
       jc      not_ballpoint
       cmp     bh, 2
       je      is_ballpoint
not_ballpoint:

       mov     cs:isBallPoint,0
	call Mouse_Hook15
	call Mouse_Hook33
is_ballpoint:
	;;;***
	call    Switcher_Hook
	;;;***
	;;;***
	;;;this is now done at shell launch time 
	;mov ax,4a05h
	;mov si,CINIT_PROGRAM_LIST
	;int 2fh
	;;;***
ENDIF
	mov     ax,cs           ; set up segment registers
	mov     ds,ax
	mov     ss,ax           ; set up stack pointer
	mov     sp,offset cs:stk
	assume cs:cseg,ds:cseg,ss:cseg

	mov     ax,cs:myPSP     ; ES = segment to shrink
	mov     es,ax
	mov     bx,seg_swtch_size     ; BX = new segment size
	mov     ah,4ah          ; AH = modify memory block
	int     21h             ; free excess memory
	mov     ax,cs:myPSP
	mov     cmd_seg,ax      ; setup segments in
	mov     fcb1_seg,ds     ; parameter block for EXEC
	mov     fcb2_seg,ds
	mov     dx,offset Ctrl_C
	mov     ax,2523h        ; AX = set Control-C handler
	int     21h             ; set handler to DS:DX
	mov     dx,offset fail_crit
	mov     ax,2524h        ; AX = set critical error handler
	int     21h             ; set handler to DS:DX
				; Note: DS is equal to CS
	jmp     main_loop

_GET_COMMAND_PTR proc
	mov dx,WORD PTR cs:dma_ptr[2]
	mov ax,WORD PTR cs:dma_ptr[0]
	retf
_GET_COMMAND_PTR endp
PUBLIC _GET_COMMAND_PTR

_GET_ARGS_PTR proc
	mov dx,WORD PTR cs:dma_ptr[2]
	mov ax,WORD PTR cs:dma_ptr[0]
	add ax,40h
	retf
_GET_ARGS_PTR endp

PUBLIC _GET_ARGS_PTR

_GET_WAIT_FLAG proc
	mov ax,WORD PTR cs:WaitBeforeErase
	retf
_GET_WAIT_FLAG endp
PUBLIC _GET_WAIT_FLAG

_GET_STARTUP_NAME proc
	mov dx,WORD PTR cs:startup_name_seg
	mov ax,WORD PTR cs:startup_name_off
	retf
_GET_STARTUP_NAME endp
PUBLIC _GET_STARTUP_NAME

;;; es is segment of psp on entry,
;;; scoots the command line down a bit so we can put our stuff in front
;;; WARNING assumes that COMMANDMAXLENGTH is even
scoot_command proc near
	;;; we do this backwards so we can do it in place
	std
	push ax
	push ds
	push cx
	mov cx,COMMANDMAXLENGTH    ;; we are moving words
	mov ax,es                    ;; from psp to psp
	mov ds,ax
	mov si,COMMANDLINEIN+COMMANDMAXLENGTH
	;;WARNING WARNING we are tromping on an extra 3 bytes!
	mov di,COMMANDLINEIN+129
	rep movsb
	pop cx
	pop ds
	pop ax
	ret
scoot_command endp

;; on entry cs:startup_name points to the start up name.
;; one exit, the startup name is modified to be a .exe instead
;; of a .com
startup_to_exe proc near
    push es
    push di
    mov es,cs:startup_name_seg
    mov di,cs:startup_name_off
    xor ax,ax
    repnz scasb
    ;now es:di points to null terminated "..\dosshell.com"
    sub di,4
    mov BYTE PTR es:[di],'e'
    inc di
    mov BYTE PTR es:[di],'x'
    inc di
    mov BYTE PTR es:[di],'e'
    pop di
    pop es
    ret
startup_to_exe endp

ENVSEGOFFSETINPSP equ 02Ch

;;; es is segment of psp on entry,
;;; sets globals startup_name_seg and startup_name_off
;;; kills ax,cx,di
find_startup_name proc near
	push es
	cld
	;;; we get the segment of the environment...
	mov ax,es:ENVSEGOFFSETINPSP
	mov es,ax
	;;; whose offset is 0...
	xor ax,ax
	mov di,ax
	mov cx,-1
keep_looking:
	;;; and scan to the end of it to find the start-up name
	repnz scasb
	;;;repnz scasw puts di one farther than first non-match
	;;;also a zero here?
	cmp es:[di],al
	jz  found
	loop keep_looking
found:
	add di,3 ;;;1 for zero,2 for initial word in front of name
	;;;es:di points to startup name
	;;;save it
	mov cs:startup_name_seg,es
	mov cs:startup_name_off,di
	pop es
	ret
find_startup_name endp

Mouse_Hook33 proc
    push ds
    push cs
    pop ds
    mov ax,3533h                 ; save interrupt 33
    int 21h
    mov WORD PTR chain_int33,bx
    mov ax,es
    mov WORD PTR chain_int33[2],ax

ifdef JAPAN
	cld
	push    ds
	push    es
	mov     ds,ax
	mov     ax,cs
	mov     es,ax
	mov     si,bx
	sub     si,COPYRIGHT_LEN
	lea     di,ms_copyright
	mov     cx,COPYRIGHT_LEN
	rep     movsb                   ; copy copyright string
	pop     es
	pop     ds
endif

    mov dx,offset handle_33             ;patch interrupt 33 with
    mov ax,2533h                        ;our own routine
    int 21h
    pop ds
    ret
Mouse_Hook33 endp


Mouse_Hook15 proc
    push ds
    push cs
    pop ds
    mov ax,3515h                 ; save interrupt 15
    int 21h
    mov WORD PTR chain_int15,bx
    mov ax,es
    mov WORD PTR chain_int15[2],ax
    mov dx,offset handle_15             ;patch interrupt 15 with
    mov ax,2515h                        ;our own routine
    int 21h
    pop ds
    ret
Mouse_Hook15 endp


;;;***
IF SWITCHER
IFNDEF QLOADER
include swtch_tr.inc
ENDIF
;;;***
ENDIF

print_copyright proc near
	push ax
	push ds
	push dx
	mov     ah, 09h
	push    cs
	pop     ds
	mov     dx, offset Startup_string
	int     21h
	mov     ah, 09h
	push    cs
	pop     ds
	mov     dx, offset Copyright_string
	int     21h
	pop dx
	pop ds
	pop ax
	ret
print_copyright endp

cseg     ends
	 end    start
