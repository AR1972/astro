;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

include bambi.inc
include msbds.inc

PUBLIC  logical_to_physical
public	log_phys_list
public	compute_logical_to_physical 	

zseg    segment public 'CODE'

	assume  cs:zseg
	assume	ds:zseg

extrn	get_drive_type			:near
extrn	dos_3x				:word
extrn	dosinfo			:word

log_phys_list	db	26	dup(?)

compute_logical_to_physical proc near
;	int 1
	mov	cx,26			;do each drive
next_drive:
	push	cx   			;keep track of count
	mov	dl,cl			;unit goes in dl..
	dec	dl			;zero based from 1 based count
	xor	dh,dh			;turn into index
	mov	bx,dx			;so we can access array 
	push	bx			;later...
	call	logical_to_physical	;map this logical unit to physical
	pop	bx			;get array index
	mov	log_phys_list[bx],dl	;remember logphys mapping
	pop	cx			;get back count
	loop	next_drive		;loop for all possible drives

	ret
compute_logical_to_physical endp
;
;given a logical drive unit, get the int13 drive number
;
;INPUT 
;	dl = logical unit
;OUTPUT
;	dl = physical unit, -1 if no mapping exists
logical_to_physical proc near

	push	ds
	push	di
	push	si

	cmp	cs:dos_3x,0	;bug bug int2f doesnt work on older dos
	jne	old_way		;so use old way on dos 3.x


	xor	di,di
	mov	ds,di

	push	dx
	mov	ax,0803h		;get bds chain
	int	2fh			;output in ds:di
	assume ds:nothing
	pop	dx
	
	mov	si,ds	  		;did function return a pointer?
	or	si,di
	jz	old_way

walk_bds:
	cmp	ds:[di].bds_drivelet,dl
	je	found_it
	cmp	word ptr ds:[di].bds_link,-1
	je	no_mapping
	lds	di,ds:[di].bds_link
	jmp	short walk_bds

logtophysout:
	pop	si
	pop	di	
	pop	ds
	ret

found_it:
	mov	dl,ds:[di].bds_drivenum
	jmp	short logtophysout
no_mapping:
	mov	dl,-1
	jmp	short logtophysout
	
old_way:
	call	old_logical_to_physical
	jmp	short logtophysout
	
logical_to_physical endp



temp13chain 	dd	0
physdrive	db	-1

temp13hook proc far
	mov	cs:physdrive,dl
	jmp	dword ptr cs:[temp13chain]
temp13hook endp

;logbuf	db	512	dup(?)
extrn	logbuf	:byte	;see rdtata.asm

old_logical_to_physical proc near

	push	ds
	push	es
	push	ax
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	bp

	mov	cs:physdrive,-1			;default to no mapping

	push	dx
	call	get_drive_type
	pop	dx
	sub	ax,dosinfo
	cmp	ax,HARDDISK_TYPE
	jne	quick_out


	push	dx
	mov	ax,3513h	
	int	21h
	
	mov	WORD PTR cs:temp13chain,bx
	mov	WORD PTR cs:temp13chain[2],es

	mov	dx,offset cs:temp13hook
	mov	ax,2513h
	int	21h

	mov	WORD PTR cs:temp13chain,bx
	mov	WORD PTR cs:temp13chain[2],es


	pop	dx
	mov	al,dl

	push	AX			
	mov	AH,0dh			; DOS disk reset function
	int	21h
	pop	AX

	
	push	cs
	pop	ds

	mov	CX,1			; read one sector
	mov	DX,1			; read sector one
        mov	bx,offset logbuf	; point to our buffer

	int	25h			; DOS absolute sector read
	pop	BX			; Adjust the stack (see MS Prog Ref)

	mov	dx,word ptr cs:temp13chain
	mov	ds,word ptr cs:temp13chain[2]
	mov	ax,2513h
	int	21h

	pop	bp
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	pop	es
	pop	ds

	mov	dl,cs:physdrive
	ret

quick_out:
	pop	bp
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	pop	es
	pop	ds
	ret
old_logical_to_physical endp



zseg ends

end

