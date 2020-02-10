	page	,132
debug	=	1
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;	This is the module which provides the interface for Queueman
;	to call back into Bambi.asm when it wants to commit dirty
;	cache elements to the disk.  It is very intimately tied to
;	Bambi.asm, as it uses several entry points into Bambi.asm's
;	utility subroutines, and also uses many of its variables.
;
;	The interface from this module to Queueman is much cleaner,
;	consisting of a single call to our public label, dirty_write.
;
;	This module also calls warning_pop_up when it detects a
;	fatal disk problem during a write-behind.


	.xlist
	include	bambi.inc
	include	devsym.inc
	.list

public	dirty_write		; this is our entry point from queueman.asm

zseg	segment	dword public 'code'
	assume	cs:zseg
	assume	ds:nothing

;	define entry points to bambi.asm

	extrn	set_start_sector:near
	extrn	call_dd_common:near
	extrn	read_full_cache_block:near
	extrn	lookup_device:near

;	Define entry points to popup.asm

	extrn	warning_pop_up:near

;	some of our extrn data is actually located in bambi.asm

	extrn	our_count:word
	extrn	our_trans_off:word
	extrn	our_trans_seg:word
	extrn	our_starth:word
	extrn	our_startl:word
	extrn	our_start:word
	extrn	loc_reqblk:dword
	extrn	last_buffer:word
	extrn	lb_seg:word

	extrn	dirty_h:word
	extrn	dirty_l:word

	extrn	max_valid_buffers:word
	extrn	num_valid_buffers:word

	extrn	cache_block_bytes:word
	extrn	rblk_op:byte
	extrn	media_id:byte
	extrn	media_ids:byte
	extrn	packet_size:byte
	extrn	packet_sizes:byte

	extrn	accessing_swap_file_ptr	:dword

;	Here are our own local variables

wrt_dd_header		dd	0
wrt_align_factor	dw	0	; written as byte, read as word
wrt_secsize_bytes	dw	0
wrt_cache_unit		db	0
wrt_unmapped_unit	db	0

;-----------------------------------------------------------------------
;
;	Here's where Queueman calls us back for disk writes
;
;	parameters:
;	  ax == 16 bit dirtyindex
;	  di/si == block number to write
;	  bp == element identifier of block in cache

	if	debug
in_dirty	db 0	
	endif

dirty_write	proc	near
	assume	cs:zseg,ds:nothing,es:nothing

	push	ax
	push	bx
	push	cx
	push	dx
	push	bp
	push	si
	push	di
	push	ds

	push	cs
	pop	ds
	assume	ds:zseg			; assure addressability

	push	es

	if	debug
	inc	in_dirty
	cmp	in_dirty,1
	je	not_bogus

bogus_bogus:
	int	1
	mov	al,'!'-'A'
	call	warning_pop_up
	jmp	bogus_bogus

not_bogus:
	endif

	add	dirty_l,1
	adc	dirty_h,0

;
;	First we've got to load that block out of the cache into
;	  our local buffer, then write it out to disk.
;
;	the mask code in ax tells us exactly which of the blocks
;	  are dirty.


	push	ax			; save dirty mask
	push	di			; save write block number
	push	si

	mov	ax,di
	mov	al,ah

	mov	wrt_unmapped_unit,al	; save for error reporting

	;;; setup the drive unit so enhanced mode windows
	;;; can know the drive letter if it gets an error
	push	es
	push	di
	les	di,cs:accessing_swap_file_ptr
	mov	es:[di].delay_write_drive_unit,al
	pop	di
	pop	es

;	******  Get logical unit code to write to into al

	push	bp			; save handle
	call	lookup_device
	and	al,3fh			; ignore status bits
	mov	wrt_cache_unit,al	; save unit code
	mov	byte ptr wrt_align_factor,ch
	mov	word ptr wrt_dd_header,dx	; save dd header
	mov	word ptr wrt_dd_header[2],bp
	mov	ax,cache_block_bytes	; calculate sector size in bytes
	shr	ax,cl
	mov	wrt_secsize_bytes,ax

	pop	bp			; restore handle

	mov	ax,max_valid_buffers	; is super-cache full?
	cmp	ax,num_valid_buffers
	jnz	no_truncate_super_cache
	dec	num_valid_buffers	; trash last block
no_truncate_super_cache:

	mov	di,last_buffer		; point to last buffer
	mov	es,lb_seg

	push	di
	push	cx			; save cache block shift factor
	call	read_full_cache_block	; read into our local buffer
	pop	cx			; restore cache block shift
	pop	di

;	There's a block at lb_seg:di which needs to get written to
;	  disk at wrtblk.  The cl register (on stack) contains a mask which
;	  says which sectors are actually dirty.

	pop	ax			; get block number low
	pop	dx			; get block number high
	xor	dh,dh			;mask off unit code


	mov	bx,0ffffh	; use bx as scratch register in 32-bit left
;				;  shift by cl.  This is for converting a
;				;  block number to its base sector address.

	rol	ax,cl
	shl	dx,cl
    	shl	bx,cl
    	mov	cx,ax
    	and	ax,bx
	not	bx
	and	cx,bx
	or	dx,cx

	pop	cx			; restore mask word into cx
	jcxz	dirtywrt_done		; this shouldn't happen
;					; but if it did, we'd hang without it

	xor	bx,bx			; zero the count of blocks written
;					;  in a previous I/O.

	sub	ax,wrt_align_factor
	sbb	dx,bx			; **** ASSUME BX == 0 ****

;	Now, we have to skip through our mask register to find what
;	  needs to be written and write it.  Notice that we'll loop
;	  back to here after writing if there are non-adjacent blocks,
;	  so that we may update the transfer pointer and sector number
;	  for the next I/O.  In this case, BX will have the number of
;	  sectors just written, which must be added to the number of
;	  intervening clean sectors.


clean_sector_count_loop:
	inc	bx			; count clean sectors
	shr	cx,1			; and shift the mask right one bit
	jnc	clean_sector_count_loop	; loop until we find a dirty one

	dec	bx			; okay, we went one too far.

	push	ax
	push	dx			; damn!  MUL needs these registers
	mov	ax,wrt_secsize_bytes
	mul	bx
	add	di,ax			; update transfer pointer to next dirty
	pop	dx
	pop	ax
	add	ax,bx		; update sector number
	adc	dx,0

;	SCORE!  We found a dirty sector!  Now decide how many consecutive
;	dirty_bits there are to write.  For every contiguous one-bit we
;	find starting from the low-end of cl, we'll add one to the number
;	of sectors we're going to write out in the next I/O.

	xor	bx,bx			; init count to 0

count_consecutive_dirtys:
	inc	bx
	shr	cx,1			; shift out while bits are dirty
	jc	count_consecutive_dirtys

	shl	cx,1			; put the zero bit back into bit zero

;	Now bx has sector count to write, cx has the remaining mask.

;	  BUGBUG:  An optimization exists here, which would have
;	  to take valid_bits into consideration -- It may be more
;	  efficient to go ahead and write the non-dirty sectors
;	  in between the first and last dirty ones, provided they are
;	  indeed VALID.  This might be desirable if the amount of
;	  overhead in sending write commands to the disk controller
;	  is too large for the subsequent command to be fully issued
;	  during the passage of the intervening sectors.
;
;	  The downside of this optimization is that it actually
;	  causes excess writing to be done, ie:  Sectors are theoretically
;	  written with their existing content.  There is some chance that
;	  a data transfer error, most likely on the disk controller or
;	  in XMS storage, will cause corruption of the hard disk during
;	  any given write operation, so writing data redundantly
;	  exposes the user's data to a small risk.

	push	cx			; retain mask for later
	call	write_sectors		; write bx blocks from sector dx:ax
	pop	cx			; restore mask accumulator

	or	cx,cx
	jnz	clean_sector_count_loop

dirtywrt_done:

	if	debug
	dec	in_dirty
	endif

	pop	es
	pop	ds
	pop	di
	pop	si
	pop	bp
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret

dirty_write	endp

;-----------------------------------------------------------------------
;
;	write bx sectors to disk, starting at sector dx:ax, from
;	  lb_seg:di.  Must preserve bx, ax, dx, and di.

write_sectors	proc	near
	assume	cs:zseg,ds:zseg,es:nothing

;	save parameter registers for retries and restoring upon exit

	push	ax			; save start sector
	push	dx
	push	bx			; save sector count
	push	di			; save transfer address

;	stick parameter registers into request block as appropriate.

	mov	our_trans_off,di	; save for actual disk write
	mov	our_count,bx

	;;;maintain packetsize and mediaid across dirty write!
	mov	bh,packet_size
	mov	bl,media_id
	push	bx

	les	bx,wrt_dd_header	; point to device header
	call	set_start_sector	; set the startint sector from ax:dx

	mov	ax,lb_seg
	mov	our_trans_seg,ax	; set transfer segment

	xor	bh,bh
	mov	bl,wrt_unmapped_unit
	mov	al,byte ptr media_ids[bx]
	mov	byte ptr media_id,al

	mov	al,byte ptr packet_sizes[bx]
	mov	byte ptr packet_size,al

	or	dx,dx			;access above 32 meg must
	jz	ss16done		;have larger packet size,
					;note that packet is 
					;always correct for < 32 meg
					;since we set both fields
	cmp	packet_size,18h
	ja	ss16done
	mov	packet_size,18h	
ss16done:


	les	bx,loc_reqblk
	mov	byte ptr rblk_op,devwrt

	mov	al,wrt_cache_unit	; get remapped cache unit
	mov	es:byte ptr [bx].requnit,al

	call	call_wrt_dd		; call into selected device driver

	pop	bx			;restore mediaid and packet size
	mov	packet_size,bh	
	mov	media_id,bl

	les	bx,loc_reqblk
	test	es:[bx].reqstat,8000h
	jnz	handle_fatal_write_error ; go prompt user if error

;	restore registers and exit

ignore_fatal_write_error:
	pop	di			; restore transfer address
	pop	bx			; restore count
	pop	dx			; restore start sector
	pop	ax
	ret				; and back to caller

;	Shit!  Fatal error on write-behind!  Ask user what to do with
;	  sleazy popup.

handle_fatal_write_error:
	mov	al,wrt_unmapped_unit	; get drive code
	call	warning_pop_up
	cmp	al,ASCIICODE_RETRY
	jnz	ignore_fatal_write_error

	pop	di			; restore transfer address
	pop	bx			; restore count
	pop	dx			; restore start sector
	pop	ax
	jmp	write_sectors		; and back to front of this routine
;					;  to retry


write_sectors	endp

;-----------------------------------------------------------------------
;
;	call through to the write behind device driver

call_wrt_dd	proc	near
	assume	cs:zseg,ds:zseg,es:nothing

	push	ds			; save caller's ds
	lds	si,wrt_dd_header 	; get actual device's header
	assume	ds:nothing
	jmp	call_dd_common		; and join regular dd calling code

call_wrt_dd	endp

;-----------------------------------------------------------------------

zseg	ends
	end
