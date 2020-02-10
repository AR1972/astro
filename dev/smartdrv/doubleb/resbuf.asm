;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

vseg segment byte public 'CODE'
	assume cs:vseg,ds:vseg,es:NOTHING

include ..\bambi.inc

PUBLIC  int_2f_chain
PUBLIC 	int_13_chain
PUBLIC 	sectrack	
PUBLIC 	track_buffer_ptr
PUBLIC	buffer_size
PUBLIC  track_buffer
PUBLIC  int_2f_handler
PUBLIC	safe_13_handler
PUBLIC  check_dma_services
PUBLIC  double_buffering_required

;;; RESIDENT DATA AREA anything here is not freed after initialization

Int_2f_Chain	dd	?
Int_13_Chain    dd      ?

SafeDrv		db	0	; drive (80-FF) for following info
SafeSecTrk	db	?	; sectors per track
SafeHds		db	?	; # heads



BiosDataSeg	dw	40h		; BIOS data area seg addr
DMASERVOFFSET	equ	7Bh		; offset in BIOS data area of byte
					;  containing DMA Services bit
DMASERVBIT	equ	20h		; mask for DMA Services bit

CHECK_DMA_SERVICES DB 1	;1 means check dynamically
VIA_TRACK_BUFFER DB   1 ;1 means double buffer
SECTRACK	DW	?	; Sectors per track (size of track buffer)
TRACK_BUFFER_PTR DD	0
buffer_size	dw	0

MAXDRIVES 	equ 	16

double_buffering_required	db	MAXDRIVES dup(0)

dma_descriptor_structure	struc
	dmasize		dd	?
	dmaoffset 	dd	?
	dmaseg		dw	?
	dmaid		dw	?

	dmaphysaddr	dd	?
dma_descriptor_structure	ends

dma_descriptor_struc db size dma_descriptor_structure dup(0)

mysig	dw	?


;
;return pointer to double buffer table in es:di
;
int_2f_handler proc far
	cmp	ax,MULT_BAMBI
	je	handle_bambi_api
chainint2f:
	jmp	dword ptr cs:int_2f_chain
handle_bambi_api:
	cmp	bx,BAMBI_GET_BUFFER_INFO
	jne	chainint2f
	mov	ax,BAMBI_SIGNATURE
	push	cs
	pop	es
	mov	di,offset cs:double_buffering_required

	iret
int_2f_handler endp


single_check:

	;first, set buffer to unlikely data
	push	ax
	push	dx

	push	ax
	push	di
	push	cx
	mov	cx,256	;256 words per sector
	mov	ax,cs:mysig
	mov	di,bx
	rep	stosw
	pop	cx
	pop	di
	pop	ax

	mov	al,1		;1 sector

	push	ax


	pushf
	cli
	call	dword ptr cs:int_13_chain

	pop	ax

	push	ax
	push	di
	push	cx
	mov	cx,256	;256 words per sector
	mov	ax,cs:mysig
	mov	di,bx
	repe	scasw
	cmp	cx,0	;all same?
	pop	cx
	pop	di
	pop	ax
	je	need_double_buffering

	mov	al,1		;1 sector

	push	ds
	push	si
	push	es
	push	bx
	push	ax
	push	cx

	push	es		;ds:si -> user buffer
	pop	ds
	mov	si,bx

	mov	es,word ptr cs:Track_buffer_ptr[2]
	mov	bx,word ptr cs:Track_Buffer_Ptr[0]	; es:bx = ptr to track buffer

	push	ax
	pushf
	cli
	call	dword ptr cs:int_13_chain
	pop	ax

	mov	di,bx

	mov	cx,256	;256 words per sector
	cld
	repe	cmpsw

	pop	cx
	pop	ax
	pop	bx
	pop	es
	pop	si
	pop	ds
	jnz	need_double_buffering

	clc
	pop	dx
	pop	ax
	retn


need_double_buffering:

	stc
	pop	dx
	pop	ax
	retn

;***	Safe_13_Handler - safe (double-buffered, when required) int 13 handler
;
;	Double-buffer disk i/o through track buffer, when required.

;	Input:	int 13 registers
;
;	Output:	int 13 return regs
;
;	Note:	We are an interrupt handler.
;		We use the track buffer for double-buffering.  Meaning
;		 nobody can count on the contents of the track buffer
;		 *across* a Safe_13 call.  If we use the track buffer,
;		 we will invalidate the contents.  (Valid track buffer
;		 contents are assumed to be in the cache as well, 
;		 which is out of our control.)
;
;	Note:	This code is not reentrant.

	assume	cs:vseg,ds:nothing,es:nothing,ss:nothing

Safe_13_Handler	proc	far

	sti				; allow interrupts

;*	Find out if this is really a hard disk read or write request.

	test	dl,80h
	jz	s13$Pass		; not hard drive - pass to old int 13
	cmp	ah,2
	je	s13$Ours		; read request - pay attention
	cmp	ah,3
	je	s13$Ours		; write request - pay attention

s13$Pass:

;*	Pass request to old int 13 handler.

	jmp	dword ptr int_13_chain

s13$Ours:

;*	Find out if double-buffering is required.

	cmp	Check_DMA_Services,1
	je	s13$Dyn		; no dynamic checking of DMA Services
	jmp	do_double_buffer

s13$Dyn:
;	Check if Lim/Win/EMM386 DMA Services are active.

	push	ax
	push	es
	mov	es,BiosDataSeg		; es = BIOS data area seg addr
	mov	al,es:DMASERVOFFSET	; AL contains DMA Services Active bit
	and	al,DMASERVBIT		; isolate bit
	mov	Via_Track_Buffer,al	; set double-buffering flag
	pop	es
	pop	ax



;	Via_Track_Buffer is nonzero if double-buffering is required.

	cmp	Via_Track_Buffer,0
	je	s13$Pass		; double-buffering not required
if 1

	push	bx
	mov	bx,dx
	xor	bh,bh
	sub	bx,80h
	cmp	double_buffering_required[bx],-1
	pop	bx
	jne	continue_check
	jmp	do_double_buffer
continue_check:

    	push	bx
   	mov	bx,dx
    	xor	bh,bh
    	sub	bx,80h
    	cmp	double_buffering_required[bx],4
    	pop	bx
    	jae	s13$Pass
else
	jmp	do_double_buffer
endif
check_vds:

;
;	use vds to check to see if the destination buffer is ok
;
;	int 1
	push	ax
	push	es
	push	di
	push	dx
	push	cx

if 1
	mov	cx,es	

	push	cs
	pop	es
	mov	di,offset cs:dma_descriptor_struc

	mov	es:[di].dmaseg,cx
	mov	word ptr es:[di].dmaoffset,bx

	xor	ah,ah
	xchg	al,ah
	shl	ax,1		;total multiply by 512 (sector size)
				;note, we do this correctly since this
				;lock will cause win386 to mark the 
				;buffer dirty--the ROM BIOS may not
				;know about VDS and do it itself
	mov	word ptr es:[di].dmasize,ax

	mov	dx,4		;do not attempt auto-remap
	mov	ax,8103h
	int	4bh

	pop	cx
	pop	dx
	pop	di
	pop	es
	pop	ax
	jnc	no_regionerror
	jmp	region_lock_error
no_regionerror:

	push	ax
	push	es
	push	di
	push	dx
	push	cx

	mov	dx,0		;do not copy data
	push	cs
	pop	es
	mov	di,offset cs:dma_descriptor_struc
	mov	ax,8104h
	int	4bh


	push	di
	mov	ax,es:[di].dmaseg
	xor	dx,dx
	mov	cl,4

	mov di,0FFFFh        	;      set up to make a mask
	rol ax,cl	     	;      
    	shl dx,cl	     	;      
    	shl di,cl	     	;      
    	mov cx,ax		;      
    	and ax,di	     	;      
    	not di	     		;      
    	and cx,di	     	;      
    	or  dx,cx	     	;      
	pop	di
	
	add	ax, word ptr es:[di].dmaoffset
	adc	dx,0

	cmp	ax,word ptr es:[di].dmaphysaddr[0]
	jne	phys_no_equ
	cmp	dx,word ptr es:[di].dmaphysaddr[2]
	jne	phys_no_equ

	pop	cx
	pop	dx
	pop	di
	pop	es
	pop	ax

	jmp	s13$Pass		; double-buffering not required

endif 

phys_no_equ:
	pop	cx
	pop	dx
	pop	di
	pop	es
	pop	ax

;
;Only do checking for addresses above a000 ie umbs
;
;
	push	ax
	push	bx
	shr	bx,1
	shr	bx,1
	shr	bx,1
	shr	bx,1
	inc	bx
	mov	ax,es
	add	ax,bx
	cmp	ax,0A000h
	pop	bx
	pop	ax
	jb	do_double_buffer

;	int 1

;;
;; If we get here, we can see if double buffering is really required
;; by attempting to read into the physical (!=linear) address.
;;
;; for now, only check on reads
if 1
	cmp	ah,2
	je	check_read
endif
	jmp	do_double_buffer


check_read:

	push	ax
	push	bx
	push	cx
	push	dx
	push	bp
	push	si
	push 	di
	push	es
	push	ds

	mov	cs:mysig,'SQ'
	call	single_check

	jc	double_buffering_needed
	
	mov	bx,dx
	xor	bh,bh
	sub	bx,80h	;turn drive id into index into our table
        inc	double_buffering_required[bx]

double_buffering_not_needed:
	pop	ds
	pop	es
	pop	di
	pop	si
	pop	bp
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	jmp	s13$Pass		; double-buffering not required

double_buffering_needed:

	mov	bx,dx
	xor	bh,bh
	sub	bx,80h	;turn drive id into index into our table
        mov	double_buffering_required[bx],-1

	
	pop	ds
	pop	es
	pop	di
	pop	si
	pop	bp
	pop	dx
	pop	cx
	pop	bx
	pop	ax

region_lock_error:

do_double_buffer:

;*	Double-buffer the request.

	push	si			; save registers
	push	di
	push	ds

	push	cs
	pop	ds			; ds = vseg seg addr
	assume	ds:vseg

	cmp	al,byte ptr SecTrack	; will xfr fit in track buffer?
	ja	s13$Split		; it won't fit
	call	SafeXfr			; transfer it via track buffer
	jmp	short s13$Done

s13$Split:
	call	SafeSplit		; split transfer to fit in track buf

s13$Done:

	pop	ds			; restore registers
	pop	di
	pop	si
	ret	2

Safe_13_Handler	endp




;***	SafeSplit - split disk transfer up and double-buffer via track buffer
;
;	Input:	int 13 regs
;		ah = 2 or 3
;		al > SecTrack (size of track buffer)
;		ds = vseg seg addr
;
;	Output:	int 13 return
;
;	Used:	si,di
;
;	Note:	That's right, al is greater than, NOT equal to
;		the size of the track buffer in sectors.

SafeSplit	proc

	push	bx			; save registers
	push	cx
	push	dx
	push	bp
	push	ax

ss$Next:
	mov	bp,ax			; bp = saved ax
	mov	al,byte ptr SecTrack	; al = # sectors in track buffer
	call	SafeXfr			; transfer one track buffer's worth
	jc	ss$Done			; return int 13 error
	mov	ax,bp			; ax restored
	call	SafeAdjNext		; adjust reg's to next piece
	jc	ss$Done			; return int 13 error
	cmp	al,byte ptr SecTrack
	jbe	ss$Last			; last xfr coming up
	jmp	ss$Next			; go do next track buffer's worth

ss$Last:
	call	SafeXfr			; xfr last piece

ss$Done:
	pop	bx			; bx = original ax
	mov	al,bl			; restore original sector count
	pop	bp			; restore regs
	pop	dx
	pop	cx
	pop	bx
	ret
	
SafeSplit	endp




;***	SafeAdjNext - adjust int 13 reg's to next xfr piece
;
;	Assuming we just transferred a track buffer's worth,
;	adjust the int 13 registers to point to the rest.
;
;	We'll obtain sec/track and #heads info via int 13,
;	returning any error from that call.  Not relying
;	on SMARTDrive's internal drive tables will help
;	make us bulletproof.
;
;	Input:	int 13 input reg's
;		ah = 2 or 3
;		al > SecTrack (size of track buffer)
;		ds = vseg seg addr
;
;	Output:	int 13 reg's for rest of transfer
;		or possible carry set if drive info int 13 call failed
;		 in which case ah = error code
;
;	Used:	si
;
;	Note:	That's right, al is greater than, NOT equal to
;		the size of the track buffer in sectors.

SafeAdjNext	proc

	push	ax			; save ax
	cmp	dl,SafeDrv		; same drive as last time thru?
	je	sa$Adj			; yes, don't update drive info
	call	SafeDrvInfo		; update drive info
	jnc	sa$Adj			; no error, continue
	pop	si			; discard saved ax
	ret				; return int 13 error
sa$Adj:
	xor	ax,ax
	mov	ah,byte ptr SecTrack	; ax = # words transferred
	shl	ax,1			; ax = # bytes transferred
	add	bx,ax			; bx = new offset in user buffer

	mov	ax,cx			; ax = cyl,sector
	and	ax,00111111b		; ax = sector
	dec	ax			; ax = 0-based sector #
	add	ax,SecTrack		; ax = next sector before adjustment
	div	SafeSecTrk		; al = tracks passed
	inc	ah			; ah = next sector
	and	cl,11000000b		; cx = cyl,
	or	cl,ah			; cx = cyl,next sector
	add	dh,al			; dh = next head before adjustment
sa$Hd:	cmp	dh,SafeHds
	jb	sa$Done			; head is legal
	sub	dh,SafeHds		; dh = head decr'd by # heads
	inc	ch			; bump cylinder
	jnz	sa$Hd
	add	cl,40h			; bump upper two bits of cylinder
	jmp	sa$Hd			; and check head again
sa$Done:
	pop	ax			; restore ax
	sub	al,byte ptr SecTrack	; al = # sectors left in xfr

;	Carry is clear.

	ret	

SafeAdjNext	endp




;***	SafeDrvInfo - get info for drive we're dealing with
;
;	Input:	int 13 regs
;		dl = drv (80-FF)
;
;	Output:	regs unchanged except ax
;		if error returned from int 13 get drive info,
;		  carry = set
;		  ah = error code
;
;	Used:	ax
;
;	Effect:	SafeDrv, SafeSecTrk, SafeHds updated

SafeDrvInfo	proc

	push	bx		; save registers
	push	cx
	push	dx
	push	di
	push	es

	mov	ah,8		; al = Int 13 "Get drive parameters" function

	pushf
	cli
	call	dword ptr cs:int_13_chain

	jc	sd$Ret		; return int 13 error

	and	cl,00111111b	; cl = sectors per track
	mov	SafeSecTrk,cl	; record sectors per track
	inc	dh		; dh = # heads
	mov	SafeHds,dh	; record # heads

;	The above instructions leave CY clear.

sd$Ret:
	pop	es		; restore registers
	pop	di
	pop	dx
	pop	cx
	pop	bx
	jc	@F		; if error, don't update drive number
	mov	SafeDrv,dl	; update drive number for recorded info
@@:	ret

SafeDrvInfo	endp




;***	SafeXfr - double-buffer disk transfer via track buffer
;
;	Input:	int 13 regs
;		ah = 2 or 3
;		al <= SecTrack  (size of track buffer)
;		ds = vseg seg addr
;
;	Output:	int 13 returns
;
;	Used:	si,di

Int13AX		label	word
Int13Sectors	db	?
Int13Function	db	?

SafeXfr	proc

	mov	Int13AX,ax		; save function code, sector count

	cmp	ah,2
	je	sx$Read			; read request

	call	SafeXfrBuf		; write request
	call	SafeXfrDisk
	jmp	short sx$Ret

sx$Read:
	call	SafeXfrDisk
	jc	sx$Ret			; return int 13 error
	call	SafeXfrBuf

sx$Ret:	ret

SafeXfr	endp




;***	SafeXfrDisk - transfer between track buffer and disk for SafeXfr
;
;	Input:	int 13 regs
;		ah = 2 or 3
;		al <= SecTrack (size of track buffer)
;		es:bx = ptr to original buffer, not track buffer
;		ds = vseg seg addr
;
;	Output:	int 13 returns
;		es:bx = still ptr to original buffer (which is untouched)

SafeXfrDisk	proc

	push	es			; save ptr to dest buffer
	push	bx

	mov	es,word ptr Track_buffer_ptr[2]
	mov	bx,word ptr Track_Buffer_Ptr[0]	; es:bx = ptr to track buffer

	pushf
	cli
	call	dword ptr cs:int_13_chain

	pop	bx			; bx = offset of dest buffer
	pop	es			; es = seg addr of dest buffer
	ret

SafeXfrDisk	endp




;***	SafeXfrBuf - transfer between track buf and original buf for SafeXfr
;
;	Input:	int 13 regs (may be before or after call)
;		Int13AX = ax before int 13 call
;		ds = vseg seg addr
;
;	Output:	same regs
;
;	Used:	si,di, but NOT flags

SafeXfrBuf	proc
	assume	ds:nothing

	pushf				; save flags
	push	ds			; save seg regs
	push	es

	mov	ds,word ptr Track_buffer_ptr[2]
	mov	si,word ptr track_buffer_ptr[0]	; ds:si = ptr to track buffer
	mov	di,bx			; es:di = ptr to original buffer

	cmp	si,di
	jne	sb$Move			; track buf & original buf not same
	push	ax
	mov	ax,ds
	mov	bx,es
	cmp	ax,bx
	mov	bx,di			; bx = ptr to original buffer again
	pop	ax
	je	sb$Ret			; buffers are same, we're done
sb$Move:
	cmp	Int13Function,2
	je	sb$Read			; read request, track buf -> orig buf
	xchg	si,di			; write request, orig buf -> track buf
	push	es
	pop	ds			; ds:si = ptr to original buffer
	assume	ds:nothing
	mov	es,word ptr Track_buffer_ptr[2]
sb$Read:
	push	cx			; save cx
	xor	cl,cl
	mov	ch,Int13Sectors		; cx = # words in transfer
	cld

;	Check for odd-boundary segment wrap in buffer.  ;M02

	test	bx,1		   ;M02 ; bx = 1 if buffer offset is odd
	jnz	sb$Odd		   ;M02	; odd offset - check for address wrap

sb$MoveWords:
	rep	movsw			; move data one way or t' other
	pop	cx			; restore cx

sb$Ret:
	pop	es			; restore seg regs
	pop	ds
	popf				; restore flags
	ret

	;M02	begin

;*	Buffer is at an odd address offset.  If the transfer will
;	wrap past the end of the segment address, we'd better do
;	a byte move to avoid GP faulting.

sb$Odd:
	shl	cx,1			; cx = # bytes in transfer
	dec	bx			; bx = buffer address minus 1
	add	bx,cx
	sub	bx,cx			; bx unchanged, but CY set for wrap
	inc	bx
	jc	sb$MoveBytes		; address wrap - move bytes

;	No address wrap, go back and do word move.

	shr	cx,1			; cx = # words in transfer
	jmp	sb$MoveWords

sb$MoveBytes:
	rep	movsb			; move data one way or t' other
	pop	cx			; restore cx
	jmp	sb$Ret

	;M02	end

	assume	ds:vseg

SafeXfrBuf	endp

TRACK_BUFFER:

vseg ends

end 
