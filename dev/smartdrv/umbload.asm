;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;
;OVERVIEW
;	
;	This module actually contains the initial entry point for the
;program.  It detemines if the program has been loaded in a UMB, and
;if not, it re-execs itself in an attempt to load into a UMB. If the new
;load fails, it continues to load the TSR low. If the load succeeds,
;it exits and thus leaves a zero low memory footprint.
;


include bambi.inc

;
;	routines from int2f.asm
;
extrn	call_bambi			:near

extrn	initialize	:near
extrn 	transient_stack :word
extrn	vxd_name	:byte

PUBLIC	myPSP
PUBLIC  savestrategy
PUBLIC	saveumblink
PUBLIC	find_startup_name	;used later too for vxd load

zseg	segment	public 'code'

	assume	cs:zseg,ds:zseg
start   :
;;;     PROGRAM EXECUTION BEGINS HERE
;;;     ES:0 is the PSP

;;;	first set up a stack!
	mov	ax,cs
	mov	ss,ax
	mov	sp,offset cs:transient_stack
	
;;;     Get my PSP to look at command line

	mov	cs:myPSP,es

	push	cs
	pop	ds

	mov     ax,es
	mov     cmd_seg,ax

	mov ax,5800h    ; Get allocation Strategy ;output in ax
	int 21h
	mov 	savestrategy,ax


	mov ax,5802h    	; Get link state; output in al
	int 21h
	xor	ah,ah		;be sure ah = 0!
	mov	saveumblink,ax

	call	check_loadlow_switch	;note es:80h points to command line
	jc	go_with_what_we_have_short
	
	mov	ax,BAMBI_GET_STATS	; get stats also used for detection
	call	call_bambi
	cmp	ax,BAMBI_SIGNATURE	; ax = signature if bambi is loaded
	jne	try_to_load
go_with_what_we_have_short:
	jmp	go_with_what_we_have
	
try_to_load:

	cmp	saveumblink,1
	jne	not_already_high
	jmp	already_high
not_already_high:
	mov     ax,5803h        ;link/unlink UMBs bx = 0 is unlink,bx =1 is link
	mov     bx,1		;enable umb links
	int     21h
	jc	cant_set_link

	mov	ax,5801h
	mov	bx,40h		;first-fit high only
	int	21h

	call find_startup_name

 	mov     ax,cs
	mov     es,ax

	mov     fcb1_seg,ds     ; parameter block for EXEC
	mov     fcb2_seg,ds


	mov     bx,offset zseg:par_blk
	mov	ax,offset dummy_environ
	mov	cl,4
	shr	ax,cl
	mov	dx,cs
	add	ax,dx
	mov	[bx],ax

	push	ds
	mov     dx,startup_name_off
	mov     ds,startup_name_seg
	

	mov     ax,4b00h        ; AX = EXEC/run program
	int     21h             ; carry = EXEC failed
	pop	ds
	jc	go_with_what_we_have

	push	ax		;save return code from exec

	mov     ax,5803h        ;link/unlink UMBs bx = 0 is unlink,bx =1 is link
	mov     bx,cs:saveumblink	;disable umb links
	int     21h

	mov	ax,5801h
	mov	bx,cs:savestrategy	
	int	21h

	pop	ax		;restore exec return code
	cmp	al,1		;error code from bambinit.asm
	je	really_exit

	mov	ax,BAMBI_GET_STATS	; get stats also used for detection
	call	call_bambi
	cmp	ax,BAMBI_SIGNATURE	; ax = signature if bambi is loaded
	jne	go_with_what_we_have
really_exit:

	mov	ax,4c00h		; no error code
	int	21h
	int	3

go_with_what_we_have:
cant_set_link:
	mov     ax,5803h        ;link/unlink UMBs bx = 0 is unlink,bx =1 is link
	mov     bx,cs:saveumblink	;disable umb links
	int     21h

	mov	ax,5801h
	mov	bx,cs:savestrategy	
	int	21h

already_high:
	mov	es,cs:myPSP		;restore input PSP 
	jmp	initialize


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

	push	ds
	push	si
	push	di
	;;; save off the name here since it is need by the resident
	;;; code when it tells win386 what file the sdvxd is in.
	;;;
	push	es
	pop	ds
	mov	si,di
	;;;ds:si-> source string
	push	cs
	pop	es
	mov	di,offset cs:vxd_name
	;;;es:di-> destination string
	mov cx,67	
	rep movsw		

	pop	di
	pop	si
	pop	ds

	pop es
	ret
find_startup_name endp

;
; INPUT
;	es:80h points to command line
; OUTPUT
;	carry set means dont loadhi
;
check_loadlow_switch proc near
	push	es
	push	di
	mov	di,COMMAND_LINE_OFFSET+1 ;skip length byte
	mov	cx,MAX_COMMAND_LINE_LENGTH	
	mov	al,COMMAND_LINE_TERMINATOR
	repnz	scasb
	jnz	not_loadlow
	mov	ax,MAX_COMMAND_LINE_LENGTH
	sub	ax,cx
	xchg	ax,cx			;cx = actual commandline length

	mov	di,COMMAND_LINE_OFFSET	;start back at beginning
	mov	al,COMMAND_SWITCH_CHAR
scanning_for_switch:
	repnz	scasb
	jnz	not_loadlow
	mov	bl,byte ptr es:[di]
	or	bl,32			;lower case it
	cmp	bl,LOAD_LOW_FLAG
	je	load_low
	jmp	short	scanning_for_switch
not_loadlow:
	clc
	jmp	short checkout
load_low:
	stc
checkout:
	pop	di
	pop	es
	ret
check_loadlow_switch endp

ENVSEGOFFSETINPSP equ 02Ch
COMMANDLINEIN equ 80h        ; offset of command line passed in (PSP)

savestrategy	 	dw 	?
saveumblink		dw	?
extrn 	startup_name_seg 	:word
extrn	startup_name_off 	:word
myPSP		 	dw	?

par_blk  dw     0
cmd_loc  dw     COMMANDLINEIN   ; command-line address
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

align 16
dummy_environ	dd	0
zseg	ends
	end	start


