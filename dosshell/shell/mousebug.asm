;/* Revision history since 5.0 Golden:
; *
; *  M013 SHK 10/02/91	Zero out BX before we make the call to detect mouse
; *			mouse version numbers.
; */


;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

?WIN = 0                ;Not windows;
?PLM = 1                ;DO use pl/m
include cmacros.inc

DOSARENA STRUC 
	Signature       db ?
	Owner           dw ?
	Paragraphs      dw ?
	Special         db 3 dup(?)
	OwnerName       db 8 dup(?)
DOSARENA ENDS

sBegin data
sEnd data

sBegin code
    assumes cs, code
    assumes ds, data

;;;RETURNS The mouse version ,
;;;major in hibyte, minor in lobyte
cProc  MouseVersion, PUBLIC , <si,di,ds,es>
cBegin MouseVersion
	xor bx,bx	; M013 older mouse versions may not set version, so
			;      zero here so we can detect "unknown" version	
	mov ax,0024h ;Get Mouse Information
	int 33h
	xchg ax,bx
cEnd   MouseVersion

is_system_chain db 0
myUmbLinkState  dw ?

;;; DetectLogitech
;;; FUNCTION 
;;;     detects the presence (in memory!) of a Logitech Mouse driver
;;; RETURNS 
;;;     if Logitech detected (numbers are binary) :
;;;             dh = 0
;;;             dl = major version
;;;             ah = minor version
;;;             al = incremental version
;;;           eg: Logitech version 5.01 -> dx=0005h:ax=0001h
;;;     if Not detected
;;;             dx:ax = 0
;;; EFFECTS
;;;     Temporarily links in UMBs, scans all memory arenas
;;; ALGORITHM
;;;
;;;     Link in UMBS, scan ALL memory arenas more name "MOUSE". Scan
;;;     all "MOUSE" arenas for the sequence "LOGITECH MOUSE DRIVER VX.XX"
;;;     
cProc DetectLogitech , PUBLIC,<si,di,ds,es>
cBegin DetectLogitech

	;;;     We must link in the UMBs for this search, since
	;;;     the mouse driver might be in a UMB!
	
	;;; first save state
	mov ax,5802h                        ; Get link state; output in ax
	int 21h
	mov WORD PTR cs:myUmbLinkState,ax   ; Save link state for restoration

	;;; now set state (umbs linked in)
	mov     ax,5803h         ; link/unlink UMBs bx = 0 is unlink,
	mov     bx,1            ; bx =1 is link
	int     21h

	;;; Get DOS memory arena pointer just above DBP
	mov     AH,52h                  ; "undocumented"
	int     21h                     ; ES:BX --> first DBP
	sub     bx,4                    ; ARENA HEAD POINTER is now in es:bx
	
	les    bx,es:[bx]               ; first arena -> es:bx
	mov    ax,es                    ; ax is temporary segment saver

	;;;    loop through all arenas...
nextarena:
	;;;    We use the same code to walk through regular arenas and
	;;;    system arenas, regular arena chains end with signature 'Z'
	;;;    while system arenas end with 'F'

	mov    cl,es:[bx].Signature     ; current arena the arena list tail?
	cmp    cl,'F'                   ; 'F' means we were in a system list
	je     end_system_chain 

	cmp    cl,'Z'                   ; 'Z' means there are no more arenas
	je     endofarenas1

	cmp    cl, 'M'                  ; sanity check--> valid 
	je     arena_valid              ; regular arenas are id 'M'
	cmp    cl, 'D'                  ; and system arenas are id 'D'
	je     arena_valid
	jmp    short endofarenas1       ; invalid arenas!
arena_valid:
	;;;    check for special system type of arena (UMBs,devices)
	cmp    es:[bx].owner,08h        ; system arenas have owner of 08h
	jne    find_a_regular_mouse_arena 
	cmp    WORD PTR es:[bx].ownername,'DS' ; system arenas have owner name of SD
	jne    find_a_regular_mouse_arena

	;;; here we set up for walking a system arena (UMB,device lists)
	cmp     cs:is_system_chain,0   ; if we are already in a system arena, 
	je      start_system_chain     ; something weird is going on fall through!

	;;; restore setup for walking regular arena chains  
end_system_chain:
	dec     cs:is_system_chain     ; no longer in a system chain
	pop     ax                     ; restore registers saved at chain start
	pop     bx                     ; 
	pop     es                     ; es:bx points to next arena
	jmp     short find_a_regular_mouse_arena ;continue arena walking
endofarenas1:   
	jmp endofarenas ; jump out of range, this is intermediate

start_system_chain:
	;;; here we setup for walking system chain
	inc     cs:is_system_chain    ; remember we are in a special chain
	push    es                    ; remember current arena
	push    bx                    ; is es:bx, as well as segment in ax
	push    ax
	add     bx,16                 ; system arena head starts at first
				      ; segment in this regular arena
      ; fall through to generic search code

find_a_regular_mouse_arena:

	;;;es:bx -> arena header
	cmp    WORD PTR es:[bx].OwnerName[0],'OM' ; is owner "MOUSE"?
	jne    continue_chain                     ; note we are doing
	cmp    WORD PTR es:[bx].OwnerName[2],'SU' ; double byte compares!
	jne    continue_chain
	cmp    BYTE PTR es:[bx].OwnerName[4],'E'
	jne    continue_chain

	;; found a MOUSE ARENA
	mov     di,bx                            ; get pointer to memory block
	mov     CX, es:[bx].paragraphs           ; lengths is in paragraphs
	shl     cx,1                             ; we need bytes
	shl     cx,1                             ; mul by 16
	shl     cx,1
	shl     cx,1
	cld                                      ; don't forget!
	
continue_this_scan:
;;; scanning for "LOGITECH MOUSE DRIVER VX.XX" in this es:di memory block
;;;               012345678901234567890123456
	mov     dx,ax
	mov     AX,'OL'
	repne   scasw
	or      cx,cx
	mov     ax,dx
	jz      continue_chain
	cmp     WORD PTR es:[di],'IG'
	jne     continue_this_scan
	cmp     WORD PTR es:[di+2],'ET'
	jne     continue_this_scan
	cmp     WORD PTR es:[di+4],'HC'
	jne     continue_this_scan
	cmp     WORD PTR es:[di+6],'M '
	jne     continue_this_scan
	;;;     note we don't check for "OUSE DRIVER", just too much
	cmp     WORD PTR es:[di+19],'V '
	jne     continue_this_scan

	;;;     found the signature, now get the version after the "V"
	;;;     WARNING assumes D.DD as version format, but we already 
	;;;     checked "real" version for less than 6.23 so we probably
	;;;     wouldn't get here...

	mov     dl,BYTE PTR es:[di+21] ; ascii major version
	mov     ah,BYTE PTR es:[di+23] ; ascii minor version
	mov     al,BYTE PTR es:[di+24] ; ascii incremental version
	mov     dh,'0'                 ; binarize the numbers ...
	sub     dl,dh
	sub     ah,dh
	sub     al,dh
	xor     dh,dh                  ; note dh = 0 on return!
	jmp short DetectLogictechReturn ; clean up and get out!
continue_chain:
	;;;    this arena has not logitech, go to next
	mov    es,ax                   ; restore arena segment 
	add    ax,es:[bx].paragraphs   ; skip the memory block to next arena
	inc    ax                      ; one paragraph for last header
	mov    es,ax                   ; save arena segment in ax
	jmp    nextarena               ; ready to search again
endofarenas:

	;;; If we get here, we did not find a logitech signature
	;;; in a "MOUSE" owned arena
	xor    ax,ax
	xor    dx,dx
DetectLogictechReturn:
	;;;     if we were in a system chain, we need to clean up
	;;;     the stuff (es,bx,ax = 6 bytes) we saved on the stack
	cmp     cs:is_system_chain,0
	je      no_cleanup
	add     sp,6
no_cleanup:

	;;;     we now need to restore the UMB link state

	push    ax              ; don't toast return value!
	;;; now re-set umb link state
	mov     ax,5803h         ;link/unlink UMBs bx = 0 is unlink,bx =1 is link
	mov     bx,cs:myUmbLinkState
	int     21h
	
	pop     ax      ;; restore return value
cEnd DetectLogictech

sEnd   code
end




















