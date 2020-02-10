;/*				      
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

include bambi.inc

PUBLIC	warning_pop_up
PUBLIC  warning_pop_up_DOS
PUBLIC  shut_down_pop_up

extrn	popup1			:byte
extrn	popup2			:byte
extrn	reboot_when_ok		:byte
extrn	startup_info		:byte

WARNING_DIALOG_ATTRIBUTES equ	7	;white on black (usually!)

ROM_SET_CURSOR 		equ	2
ROM_GET_CURSOR		equ	3
ROM_WRITE_ACHAR		equ	09h
ROM_WRITE_CHAR		equ	0ah
ROM_TELETYPE		equ	0eh
ROM_GET_VIDEO_MODE	equ	0fh

zseg    segment public 'CODE'

	assume  cs:zseg
	assume	ds:nothing

lastscancode	db	0		;scancode of last keypress
popupsaveint9 	dd	?
user_response	db	?

popupint9handler proc near
	push	ax
	in	al,60h			;get the scancode
	mov	byte ptr cs:lastscancode,al

	;;; The following code was taken from the swapper
	;;; it EOIs the int9 effectively "swallowing" the keys

	in      al,61h                  ;read in code
	mov     ah,al                   ;save it
	or      al,80h
	out     61h,al                  
	xchg    ah,al
	out     61h,al
	cli                             
	mov     al,20h                  ;need to output the eoi
	out     20h,al
	sti
	pop     ax                      ;restore

	iret
popupint9handler endp

grab_the_keyboard proc near

	push 	es
	push	bx

	xor	ax,ax
	mov	es,ax

	cli
	mov	ax,es:[4*9]
	mov	bx,es:[4*9+2]

	mov	word ptr popupsaveint9[0],ax
	mov	word ptr popupsaveint9[2],bx
	mov	ax,offset cs:popupint9handler
	mov	es:[4*9],ax
	mov	es:[4*9+2],cs
	sti
	
	pop	bx
	pop	es
	ret
grab_the_keyboard endp

restore_the_keyboard proc near

	push	ax
	push	bx
	push	es
	xor 	ax,ax
	mov	es,ax

	mov	ax,word ptr popupsaveint9[0]
	mov	bx,word ptr popupsaveint9[2]

	cli
	mov	es:[4*9],ax
	mov	es:[4*9+2],bx
	sti

	mov	cs:lastscancode,0
	pop	es
	pop	bx
	pop	ax
	ret

restore_the_keyboard endp


;
; INPUT
;	CS:BP -> dialog template
;	dh:dl -> y,x
;	bh  = display page
;	bl = color attributes
;	al  = drive letter replaceable parameter
; OUTPUT
;	none
; USES 
;	ALL except DS,ES
;	BP is modified!
; NOTES
;	Text wraps at single NULL, ends at '$'
;
save_al			db	0
query_cursor_locx	db 	0
query_cursor_locy	db	0

draw_dialog proc near
	mov	cs:save_al,al
display_loop:

	mov	ah,ROM_SET_CURSOR
 	int	10h			
	inc	dl


	mov	al,cs:[bp]
	cmp	al,'$'
	jz	done_displaying
	cmp	al,'&'
	jne	check_replace
	mov	cs:query_cursor_locx,dl
	mov	cs:query_cursor_locy,dh
	mov	al,' '
	jmp	short continue_line
check_replace:
	cmp	al,'@'
	jne	check_wrap
	mov	al,cs:save_al
	add	al,'A'
	jmp	short continue_line
check_wrap:
	cmp	al,0
	jne	continue_line
	inc	dh   			;wrap to new line
	xor	dl,dl
continue_line:
	mov	cx,1
	mov	ah,ROM_WRITE_ACHAR 	;display a message
	int	10h
	inc	bp
	jmp	short display_loop
		
done_displaying:	
	mov	dl,cs:query_cursor_locx
	mov	dh,cs:query_cursor_locy
	mov	ah,ROM_SET_CURSOR
 	int	10h			

	ret
draw_dialog endp

setup_video_access proc near
	push	ax
	mov	ah,ROM_GET_VIDEO_MODE	; get video page into bh
	int	10h
	mov	ah,ROM_GET_CURSOR	; get cursor location
	int	10h
	mov	cs:save_starting_cursorx,dl
	mov	cs:save_starting_cursory,dh
	pop	ax
	ret
setup_video_access endp

finish_video_access proc near
	mov	dl,cs:save_starting_cursorx ; put the cursor back 
	mov	dh,cs:save_starting_cursory
	mov	ah,ROM_SET_CURSOR
 	int	10h			
	ret
finish_video_access endp


;
; INPUT
;	al = drive letter which has error
; OUTPUT
;	al = ASCIICODE_RETRY => retry the operation
;	al = ASCIICODE_IGNORE => ignore the error
;		
save_starting_cursorx	db	0
save_starting_cursory	db	0

warning_pop_up proc near

;the int2f initialization will point this to warning_pop_up_DOS below,
;vxd load will change this to the vxd's windows handler
	push	es
	push	di
	
	les	di,cs:startup_info.SIS_Reference_Data
	call	DWORD PTR es:[di]
	pop	di
	pop	es
	mov	al,ASCIICODE_RETRY		;retry is only real option
	ret
warning_pop_up endp

warning_pop_up_DOS proc far
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	bp
	push	es

	push	ax
	call	set_windows_focus
	pop	ax

	;;; before we can display to the screen
	;;; we need to get the active display page
	;;; which is retuned in bh from get_video_mode

	call	setup_video_access
	
	xor	dx,dx
	mov	bl,WARNING_DIALOG_ATTRIBUTES ;color of dialog
	mov	bp,offset cs:popup1
	;al still has drive letter
	push	bx			; save display page for later
	call	draw_dialog


	;;; BUG BUG BUG BUG BUG BUG BUG 
	;;; this code continually scans the keyboard port waiting for
	;;; the retry or ignore scan codes. This is evil and satanic, but
	;;; we MUST get user input, and this is the only way that works
	;;; in diverse environments like windows.  It is concievable that
	;;; this could break and cause a hang. But it will DEFINITELY
	;;; cause a hang if we don't do it in many cases. So, we stomp
	;;; on eggs! CODE REVIEW ME
	;;; NOTE that the int 9 handler is still eating the keys,
	;;; and is also polling the port so we won't miss the key
	;;;
	call	grab_the_keyboard
waitforkey:
;	in	al,60h			;get the scancode
; 	cmp	al,SCANCODE_RETRY
;	je	key_valid
;;;bug bug took out check for ignore
       ;	cmp	al,SCANCODE_IGNORE
       ;	je	key_valid
	mov	al,cs:lastscancode
	cmp	al,SCANCODE_RETRY
	jne	waitforkey
       ;	cmp	al,SCANCODE_IGNORE
       ;	jne	waitforkey

key_valid:
	call	restore_the_keyboard
	mov	ah,ROM_WRITE_CHAR
	cmp	al,SCANCODE_RETRY
	jne	was_ignore
	mov	al,ASCIICODE_RETRY
	jmp	short was_retry
was_ignore:
	mov	al,ASCIICODE_IGNORE
was_retry:
	mov	cs:user_response,al
	pop	bx	 		; restore display page
	push    bx			; but still save till later
	mov	cx,1			; just display one character
	int	10h
	
	pop	bx		      	; restore display page
	mov	bl,0			; display out same dialog,
	mov	bp,offset cs:popup1	; only use black-on-black
	xor	dx,dx
	call	draw_dialog

	call	finish_video_access

	pop	es
	pop	bp
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	cmp	cs:reboot_when_ok,0
	je	not_reboot		;hack--if the shut-down popup is up
	call	shut_down_pop_up	;we should put it back up so
					;the user won't be confused if he
					;hits ctrl+alt+delete, then
					;pops out the floppy, gets this 
					;message, replaces the floppy
					;but the message is erased
not_reboot:
	mov	al,cs:user_response
	ret
	
warning_pop_up_DOS endp

shut_down_pop_up proc near

	push	ax
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	bp
	push	es

	call	setup_video_access
	
	xor	dx,dx
	mov	bl,WARNING_DIALOG_ATTRIBUTES ;color of dialog
	mov	bp,offset cs:popup2
	call	draw_dialog
	call	finish_video_access

	pop	es
	pop	bp
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax

	ret

shut_down_pop_up endp

set_windows_focus proc near
if 0
;;;superceded by code from msbio1. This code suggested by ralphl,
;;;msbio code suggested by aaronr

	mov	ax,1600h
	int	2fh		; get win386 version
	cmp	ax,3		; Q: is it 3.00?
	jne	use_31way	; N: Do 3.1 call

	mov	ax,1681h
	int	2fh		; enter a critical section

;
;	point int 24 vector at an IRET instruction
;
	push	ds
	xor	ax,ax
	mov	ds,ax
	push	word ptr ds:[24h*4]
	push	word ptr ds:[24h*4+2]
	mov	word ptr ds:[24h*4],offset iretinstruction
	mov	word ptr ds:[24h*4+2],cs

	int 	24h
	pop	word ptr ds:[24h*4+2]
	pop	word ptr ds:[24h*4]
	pop	ds

	mov	ax,1682h
	int	2fh		;exit critical section

	ret
use_31way:
	
	mov	ax,168Bh
	xor	bx,bx
	int	2fh
endif

	push	di
	push	es
	push	bx
	push	ax

	xor	di,di
	mov	es,di
	mov	bx,0015h	;Device ID of DOSMGR device
	mov	ax,1684h	;Get API entry point
	int	2fh
	mov	ax,es
	or	ax,di		
	jz	Skip
;
;Here, es:di is address of API routine. Set up stack frame to simulate a call
;
	push	cs		;push return segment
	mov	ax,OFFSET Skip
	push	ax		;push return offset
	push	es
	push	di		;API far call address
	mov	ax,1		;SetFocus function number
	retf			;do the call
Skip:
	pop	ax
	pop	bx
	pop	es
	pop	di
	ret

	ret

iretinstruction:
	iret
set_windows_focus endp

zseg ends

end
