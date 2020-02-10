;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

extrn __astart:FAR

stderr	equ 2		     ; standard error
cr	equ 0dh 	     ; ASCII carriage return
lf	equ 0ah 	     ; ASCII linefeed
VOODOO	equ 02h
cseg	segment para public 'CODE'
;
; ----	Set up DS, ES, and SS:SP to run as .COM  ----
;
	assume	cs:cseg
start	proc	far
;;;	Get my PSP to look at command line

	mov	ax,es
	mov	cs:myPSP,ax
	mov	bx,82h
	mov	al,BYTE PTR es:[bx+4]
	cmp	al,VOODOO
	je	do_cshell
	jmp	do_resident
	;;;; run the shell interface program
	;;;; the parameters indicate where to place the data for the
	;;;; next program to launch--client_name
do_cshell:
	mov	ax,WORD PTR es:[bx]
	mov	WORD PTR cs:dma_ptr[0],ax	    ;; offset
	mov	ax,WORD PTR es:[bx+2]
	mov	WORD PTR cs:dma_ptr[2],ax	     ;; segment
	jmp    __astart
main_loop:

	;;;	setup dma location (offset,segment)
	mov	ax,cs:myPSP
	mov	es,ax
	mov	BYTE PTR es:[0C0h+0],5h
	mov	WORD PTR es:[0C0h+2],80h
	mov	WORD PTR es:[0C0h+4],ax
	mov	BYTE PTR es:[0C0h+6],VOODOO

	mov	ax,cs
	mov	ds,ax
	mov	es,ax
	mov	dx,offset cseg:shell_name
	mov	bx,offset cseg:par_blk
	mov	ax,4b00h	; AX = EXEC/run program
	int	21h		; carry = EXEC failed

	mov	ax,cs:myPSP
	mov	es,ax

	;mov	 BYTE PTR es:[0C0h+0],0h
	;mov	 BYTE PTR es:[0C0H+2],cr
	;mov	 BYTE PTR es:[0C0h+6],not VOODOO

	;;exit
	    mov al, BYTE PTR es:[080h+0]
	    cmp al,0FFh
	    jne dont_go_away
	    mov ax,04C00h
	    int 21h
	;;
 dont_go_away:

	mov	ax,cs
	mov	es,ax
	mov	bx,offset cs:par_blk
	mov	dx,80h
	mov	ax,cs:myPSP
	mov	ds,ax

	mov	ax,4b00h	; AX = EXEC/run program
	int	21h		; carry = EXEC failed
cant_load_second:
cant_load:
	jmp	main_loop	; execute forever
start	endp


; ----	Program resident data area  ----
;
shell_name  db	'.\SHELL.EXE',0
dma_ptr     dd	0
PUBLIC dma_ptr

myPSP	    dw	?

par_blk  dw	0		; use current environment
	 dw	0C0h		 ; command-line address
cmd_seg  dw	0		; fill in at initialization
	 dw	offset fcb1	; default FCB #1
fcb1_seg dw	0		; fill in at initialization
	 dw	offset fcb2	; default FCB #2
fcb2_seg dw	0		; fill in at initialization

fcb1	 db	0
	 db	11 dup (' ')
	 db	25 dup ( 0 )
fcb2	 db	0
	 db	11 dup (' ')
	 db	25 dup ( 0 )

	 dw	25 dup ( 0 )   ; program stack area
stk	 dw	0

last	 equ	$		; last address used

do_resident:
	mov	ax,cs		; set up segment registers
	mov	ds,ax
	mov	ss,ax		; set up stack pointer
	mov	sp,offset cs:stk
	assume cs:cseg,ds:cseg,ss:cseg
seg_size equ (((offset last) - (offset start)) + 10fh)/16
	mov	ax,cs:myPSP	; ES = segment to shrink
	mov	es,ax
	mov	bx,seg_size	; BX = new segment size
	mov	ah,4ah		; AH = modify memory block
	int	21h		; free excess memory
	mov	ax,cs:myPSP
	mov	cmd_seg,ax	; setup segments in
	mov	fcb1_seg,ds	; parameter block for EXEC
	mov	fcb2_seg,ds
	mov	dx,offset main_loop
	mov	ax,2523h	; AX = set Control-C handler
	int	21h		; set handler to DS:DX
	mov	dx,offset main_loop
	mov	ax,2524h	; AX = set critical error handler
	int	21h		; set handler to DS:DX
				; Note: DS is equal to CS

	jmp	main_loop

_GET_COMMAND_PTR proc far
	mov dx,WORD PTR cs:dma_ptr[2]
	mov ax,WORD PTR cs:dma_ptr[0]
	ret
_GET_COMMAND_PTR endp
PUBLIC _GET_COMMAND_PTR

_GET_ARGS_PTR proc far
	mov dx,WORD PTR cs:dma_ptr[2]
	mov ax,WORD PTR cs:dma_ptr[0]
	add ax,40h
	ret
_GET_ARGS_PTR endp
PUBLIC _GET_ARGS_PTR



cseg	 ends
	 end	start
