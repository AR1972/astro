;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

?WIN = 0                ;Not windows;
?PLM =    1             ;DO use PL/M calling conventions

;This hook is used only when the user is entering a hotkey for the
;switcher.  Thus, int 9 is hooked when the properties dialog is put up,
;and unhooked when the properties dialog is taken down.
;The hook stores the scancode and shift state of the key sequence
;in global (cs) variables. 
;NOTE: this method was suggested by the CW developers on recognition
;that the keydown/keyup documentation is incorrect; this is the only
;way to get the codes they say.

include cmacros.inc
sBegin code

    assumes cs, code
    assumes ds, code

extrn scLatest:FAR 

;;;WARNING gscancode should not initialize to zero! (compares to null hot key)
gscancode       db      0FFh            ; scan code of key just hit
gkeyup          db      1               ; startup is "last key down is up"
gshiftstate     db      0               ; shift state of key just hit
ISR9            dd      ?               ; ptr to old int 9 function

;INPUT none 
;RETURNS scancode in al,shiftstate in ah of last int9 event
;
cProc  GetLastScanCode, PUBLIC,  <si,di,ds,es>
cBegin GetLastScanCode
	mov     al,cs:gscancode
	mov     ah,cs:gshiftstate
	;;;     treat shift as right+left
	test    ah,3 ;; either shift down?
	jz      no_shift
	or      ah,3 ;; set both states as down
no_shift:
cEnd  GetLastScanCode

;INPUT none 
;RETURNS 0 if last key was key down, non zero if last key
;       was key up
;
cProc  GetLastKeyState, PUBLIC,  <si,di,ds,es>
cBegin GetLastKeyState
	mov     al,cs:gkeyup
	xor     ah,ah
cEnd  GetLastKeyState


;INPUT none
;RETURNS none
;FUNCTION places our keyboard hook in the int9 chain
cProc  HookISR9, PUBLIC,  <si,di,ds,es>
cBegin HookISR9
	    push cs
	    pop  ds

	    mov ax,3509h                 ; save interrupt 09 location
	    int 21h

	    mov WORD PTR ISR9,bx
	    mov ax,es
	    mov WORD PTR ISR9[2],ax

	    mov dx,offset cs:int9hook       ;patch interrupt 09 vector with
	    ;;; ds already holds code segment
	    mov ax,2509h                 ;our own routine
	    int 21h
cEnd  HookISR9

;INPUT none
;RETURNS none
;FUNCTION takes our int 9 hook out of the chain
;WARNING must be preceded by a HookISR9!
cProc  UnHookISR9, PUBLIC,  <si,di,ds,es>
cBegin UnHookISR9

	    mov dx,WORD PTR cs:ISR9      ;patch interrupt 09 vector with
	    mov ds,WORD PTR cs:ISR9[2]
	    mov ax,2509h                 ;the old handler
	    int 21h

cEnd  UnHookISR9

;This is the int 9 hardware hook.
;All it does is record the scan code and shift state of the last
;keyboard event.

int9hook        proc    far

	pushf                           ; simulate the real interrupt:
	call    cs:ISR9                 ; call real int 9

	push ax                         ; save registers
	push ds

	;;; We can tell that this is a keyup in two ways, depending
	;;; on the machine (XT,AT)
	;;; Docs say that F0 scan code means that the next action is
	;;; a keyup. This is for XTs. For ATs, the high bit is set
	;;; in the scancode to mean key up.
	;;; so, our algorithm is this:
	;;;     if ((lastscancode == F0h) || (thiscancode & 0x80))
	;;;             its a key up
	;;;     else  its a keydown
	mov     ax,DGROUP
	mov     ds,ax
	assumes ds , DGROUP
	mov     al,byte ptr ds:scLatest
	;;in      al,60h                  ; put scan code into al

	push    bx
	xor     bx,bx
	cmp     cs:gscancode,0F0h       ;gscancode has last int 9 code
	jne     XTlastwasnotbreak       ;if it was an XT style break sequence
	inc     bl                      ;set break bool in bl
XTlastwasnotbreak:
	test    al,080h                 ;this scancode is in al now
	jz      ATlastwasnotbreak       ;if it has an AT style break
	cmp     al,0E0h
	je      ATlastwasnotbreak
	inc     bl                      ;set break bool in bl
ATlastwasnotbreak:      
	mov     cs:gscancode,al         ; save in scancode global
	mov     cs:gkeyup,bl            ; save keyup/down state
	pop     bx      

	push    es
	mov     ax, 40h
	mov     es, ax

	or     byte ptr es:[17h],80h    ; set insert flag as we are always insert
	mov     al, byte ptr es:[17h]   ; bios location of keyboard state and status flags
	pop     es
	mov     cs:gshiftstate,al       ; save keyboard state into global

	pop     ds
	pop     ax
	iret

int9hook        endp


sEnd   code
end




















