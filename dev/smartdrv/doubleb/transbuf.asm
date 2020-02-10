;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;extrn	 ismsdos		 :far

vseg segment byte public 'CODE'
	assume cs:vseg,ds:vseg,es:NOTHING


include ..\bambi.inc

PUBLIC 	initialize_double_buffering
PUBLIC 	init_safedsk

extrn	safe_13_handler		:far
extrn	int_2f_handler		:far
extrn	track_buffer		:near
extrn	int_2f_chain		:dword
extrn  	int_13_chain		:dword
extrn  	sectrack		:word
extrn	track_buffer_ptr	:dword
extrn	buffer_size		:word
extrn	check_for_dangerous_disk_partitions:near
extrn	check_dma_services	:byte
extrn	double_buffering_required:byte

extrn	SignOn			:byte  
extrn	SignOff			:byte
extrn	Warning1		:byte
extrn	Warning2		:byte
extrn	Noloadumb		:byte


SWITCH_CHAR equ '/'
BUFFER_CHAR equ 'D'
PLUS_CHAR   equ '+'

MAX_CONFIG_LINE equ 128
MAXDRIVES 	equ 	16

	assume	ds:nothing

dontload	db	0

parse_config_line proc near
	push	es
	push	bx
	push	di
	push	ax

	mov	bx,es:[di].12h
	mov	es,es:[di].14h

	mov	di,bx
	mov	cx,MAX_CONFIG_LINE		;maximum length
	mov	al,0dh
	repne	scasb
	cmp	cx,0
	jne	found_terminator

	mov	di,bx
	mov	cx,MAX_CONFIG_LINE
	mov	al,0ah
	repne	scasb

found_terminator:
	sub	cx,MAX_CONFIG_LINE
	neg	cx

	mov	di,bx
continue_scan:
	mov	al,SWITCH_CHAR
	repne	scasb

	jne	no_switch_found
	;;;es:di points right after the switch
	mov	al,es:[di]
	and	al,not 20h	;mask off shift bit
	cmp	al,BUFFER_CHAR
	jne	continue_scan

	;;;/d switch found

	mov	al,PLUS_CHAR
	repne	scasb
	jne	no_plus_found

	mov	cs:check_dma_services,0

	;;; we have to set table to -1 so smardrv.exe will print proper status
	mov	cx,MAXDRIVES
continue_force_buffering:
	mov	bx,cx
	dec	bx
	mov	cs:double_buffering_required[bx],-1		
	loop	continue_force_buffering

no_plus_found:

	pop	ax	
	pop	di
	pop	bx
	pop	es
	ret
no_switch_found:
;	mov	cs:dontload,1 ;now we load even if no switch was given
	jmp 	short no_plus_found

parse_config_line endp

init_safedsk proc near

	;;; refuse to load above a000 since this is a umb.
	;;;
	mov	ax,cs
	cmp	ax,0A000h
	jb	probablynotumb
	mov	dx,offset cs:Noloadumb
	jmp	dontloadumb
probablynotumb:

	mov	ax,MULT_BAMBI			;device already loaded?
	mov	bx,BAMBI_GET_BUFFER_INFO	;try to get info
	int	2fh				;using int 2f api
	cmp	ax,BAMBI_SIGNATURE		;if we get an answer
	je	dontloadnowarning			;we are already loaded

	xor	ax,ax
;	call	ismsdos	philba says not to do this here
	mov	ax,2000h 
	call	parse_config_line
	cmp	cs:dontload,1
	je	dontloadnowarning
	cmp	ah,0
	je	dont_load
loadanyway:
	
	cmp	cs:check_dma_services,0
	je	dont_check_partition

	push	ds
	push	es
	push	si
	push	di
	push	bx

	call	check_for_dangerous_disk_partitions

	pop	bx
	pop	di
	pop	si
	pop	es
	pop	ds
	jc	dontloadnowarning
dont_check_partition:       	

	mov     word ptr es:[di+3],0100h ;Completed code passed in packet
	mov	cx,2
	call    initialize_double_buffering

	mov     ah,09h                  ;Print out a the sign on message
	mov     dx,offset       SignOn
	int     21h

	mov	dx,cs:buffer_size
	mov	cl,4
	shl	dx,cl
	add	dx,offset cs:track_buffer

	mov     WORD PTR es:[di+0Eh],dx  	;resident up to track buffer
	mov     WORD PTR es:[di+10h],cs        	;the rest is free'd
	stc
	ret
dont_load:

	mov     ah,09h                  ;Print out a warning message
	mov     dx,offset       Warning1
	int     21h

	mov	ah,dontload
	and	ah,7
	add	ah,'0'
	mov	bx,dx
	mov	[bx],ah
	mov	byte ptr [bx+1],'$'
	mov     ah,09h                  
	int     21h

	mov     ah,09h                  
	mov     dx,offset       Warning2
	int     21h
	jmp	loadanyway
dontloadnowarning:
	mov     dx,offset       SignOff
dontloadumb:
	mov     ah,09h                  ;Print out a the sign off message
	int     21h

	clc
	ret
init_safedsk endp


;
;INPUT
;	cx = number of 512 byte sectors in double buffer
;	     if cx = 0, code uses default track size
;
initialize_double_buffering proc near

	push	ds
	push	cs
	pop	ds

assume	ds:vseg
	
	or	cx,cx		;size specified
	jnz	buffer_size_input
    ;
    ; First figure out sec/track of any hardfiles
    ;
	MOV	DL,80H
	MOV	AH,8
	INT	13H
	JC	NO_HARDFILES
	OR	DL,DL
	JZ	NO_HARDFILES
	xor	ch,ch
	AND	CL,00111111B
buffer_size_input:
	MOV	[SECTRACK],CX


	mov	bx,cx
	mov	cl,5
	add	bx,1
	shl	bx,cl

if 0
device drivers cannot alloc memory
	push	bx

	mov     ax,5803h        ;link/unlink UMBs bx = 0 is unlink,bx =1 is link
	mov     bx,0		;enable umb links
	int     21h
	pop	bx

	mov	ah,48h
	push	bx
	int	21h			;segment in AX if no error
	pop	bx
	jnc	allocation_complete
endif

	mov	cs:buffer_size,bx
	mov	word ptr Track_buffer_ptr[2],cs
	mov	word ptr track_buffer_ptr[0],offset cs:track_buffer
	jmp	append_alloc

allocation_complete:
	mov	word ptr Track_buffer_ptr[2],ax
	mov	word ptr track_buffer_ptr[0],0
append_alloc:
    ;
    ; Figure out if we have a DMA boundary problem
    ;
	mov	dx,word ptr Track_buffer_ptr[2]
	shl	DX,1
	shl	DX,1
	shl	DX,1
	shl	DX,1			; Segment converted to absolute address
	add	DX,word ptr TRACK_BUFFER_PTR[0]	; Combine with offset
	add	DX,511			; simulate a one sector transfer
					; And set next divide for round up
;
; If carry is set, then we are within 512 bytes of the end of the DMA segment.
; Adjust TRACK_BUFFER_PTR UP by 512 bytes.
;
	jnc	NotWithin512
	add	word ptr TRACK_BUFFER_PTR[0],512	; adjust


notwithin512:
	call	Hook_ISR_13
	call	Hook_ISR_2f
	clc
FINISHED:
	pop	ds
	ret
NO_HARDFILES:
	pop	ds
	stc
	ret
initialize_double_buffering endp

Hook_ISR_13     proc    near

	    push es
	    push di
	    
	    mov ax,3513h                 ; save interrupt 13 vector
	    int 21h                      ; returns in es:bx

	    mov WORD PTR cs:Int_13_Chain,bx
	    mov ax,es
	    mov WORD PTR cs:Int_13_Chain[2],ax

	    mov dx,offset safe_13_handler    ;patch interrupt 13 vector with
	    mov ax,2513h                 ;our own routine
					 ;assumes ds already holds code segment
	    int 21h

	    pop di
	    pop es
	    ret

Hook_ISR_13     endp

Hook_ISR_2f     proc    near

	    push es
	    push di
	    
	    mov ax,352fh                 ; save interrupt 2f vector
	    int 21h                      ; returns in es:bx

	    mov WORD PTR cs:Int_2f_Chain,bx
	    mov ax,es
	    mov WORD PTR cs:Int_2f_Chain[2],ax

	    mov dx,offset int_2f_handler    ;patch interrupt 2f vector with
	    mov ax,252fh                 	;our own routine
					 ;assumes ds already holds code segment
	    int 21h

	    pop di
	    pop es
	    ret

Hook_ISR_2f     endp



vseg ends

end

