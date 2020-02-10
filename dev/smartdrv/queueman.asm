;/*
; *                      Microsoft confidential
; *                      Copyright (c) Microsoft Corporation 1991
; *                      All rights reserved.
; */
 
;	This module handles the main cache data structures.  Its
;	functions include lookup, create element, and commit element.

BEEP = 0
 
public  flush_queue
public  set_dirtyindex
public  fast_lookup
public  queue_element
public  commit_all_dirty
public  commit_lru_dirty_element
public  invalidate_element
 
zseg    segment public 'code'
 
	assume  cs:zseg
	assume  ds:nothing
 
;
;       data from rdata.asm
;
extrn   dirtyindexoffset                :word
extrn	word_masks			:byte
	ifdef	USE_VALID
extrn	validindexoffset		:word
	endif
extrn   elementidentifierlowordoffset   :word
extrn   elementidentifierhiwordoffset   :word
extrn   number_of_dirty_elements        :word
extrn   queueheadptr                    :dword
extrn   queuelength                     :word
;extrn	rbeep_flag			:byte
 
;
;       routines from bambi.asm
;
extrn   dirty_write                     :near
extrn	in_bambi			:byte
;
;	routines from beep.asm
;
if beep
extrn	flush_beep			:near
extrn	invalidate_beep			:near
endif
 
;
;       local data
;
queue_nextoffset dw	?
queue_prevoffset dw	?
PUBLIC queue_nextoffset
PUBLIC queue_prevoffset
queue_head	dw	0
queue_tail	dw	0
 
PUBLIC queue_head
PUBLIC queue_tail
PUBLIC initialize_lru_queue
 
 
;
;INPUT
;	none
;OUTPUT
;       none
;USES
;	ALL except DS,ES
;NOTES
;	Initializes the LRU queue by linking all the links together
;	and setting the head and tail pointers

initialize_lru_queue proc near

	push	es
	push	cs
	pop	es

	mov	di,cs:queue_nextoffset	; init NEXT fields
	xor	ax,ax			; start links at 0
	mov	cs:queue_head,ax	; head starts at zero

	mov	cx,cs:queuelength
	dec	cx			; don't store last element

initialize_queue_next:
	add	ax,2			; pre-increment
	stosw
	loop	initialize_queue_next

	xor	ax,ax
	stosw				; point last one to front

	mov	cx,cs:queuelength
	dec	cx			; get number of last element
	mov	di,cs:queue_prevoffset	; init PREV fields
	mov	ax,cx
	add	ax,ax
	mov	cs:queue_tail,ax	; the number of elements
	stosw				; point first back to last
	xor	ax,ax			; all rest point to themselves - 1

initialize_queue_prev:
	stosw
	add	ax,2
	loop	initialize_queue_prev
 
	pop	es
	ret

initialize_lru_queue endp
 
;
;INPUT
;	BP = element to insert at front of LRU chain
;OUTPUT
;       none
;USES
;	none
;NOTES
;	inserts the BP element at the front of the LRU chain
;WARNING! the BPth element *must* be deleted before it is re-inserted
;	otherwise a loop in the chain will form
 
insert_mru_queue proc near
 
	push	bx				;preserve regs
	push	di
	mov	bx,cs:queue_head		;insert at front of queue
	mov	di,cs:queue_prevoffset		;point next prev to new item
	mov	cs:[bx+di],bp			;set prev pointer
	mov	di,cs:queue_nextoffset		;point new next to former head
	mov	cs:[bp+di],bx			;set next field
	mov	cs:queue_head,bp		;set header pointer
	pop	di				;restore regs
	pop	bx
	ret
 
insert_mru_queue endp
 
 
;
;INPUT
;	none
;OUTPUT
;       BP = index of deleted element
;USES
;	none
;NOTES
;	removes the LRU element from the linked list
;
delete_lru_queue proc near
 
	push	bx				;preserve regs
	push	di
	mov	bp,cs:queue_tail   		;get the tail element
	mov	di,cs:queue_prevoffset		;get tails prev element
	mov	bx,cs:[bp+di]			;get prev
	mov	cs:queue_tail,bx		;set tail to prev
	pop	di
	pop	bx
	ret
 
delete_lru_queue endp
 
;
;INPUT
;	none
;OUTPUT
;       BP = item to bring to the front of the LRU chain
;USES
;	none
;NOTES
;	removes the LRU element from the linked list
;
bring_to_front proc near
 
	cmp	bp,cs:queue_tail		;tail is special case
	je	do_tail				;
	cmp	bp,cs:queue_head		;dont need to do head!
	je	alreadydone
 
	push	si				;preserve regs
	push	di
	mov	si,queue_prevoffset		;get the previous guy
	mov	di,cs:[bp+si]			;into di
	mov	si,queue_nextoffset		;get the next guy
	mov	si,cs:[bp+si]			;into si
 
	push	bp
	mov	bp,cs:queue_nextoffset		;set previous next to next
	mov	cs:[di+bp],si			;set field
	mov	bp,cs:queue_prevoffset		;set next previous to previous
	mov	cs:[si+bp],di			;set filed
	pop	bp
 
	pop	di
	pop	si
	call	insert_mru_queue		;insert just deleted item
 
alreadydone:
	ret
 
do_tail:
	push	bp
	call	delete_lru_queue		;delete tail item
	call	insert_mru_queue		;insert at head
	pop	bp
	ret
 
bring_to_front endp
 
 
;
; input
;       bp      =       elementidentifier index
;       cx      =       new dirty bits
;	bx	=	new valid_bits (if enabled)
;
; used
;	cx, ax
; notes
;	not only does set_dirtyindex set the dirty bits, but
;       it also keeps track of the number of dirty elements in
;	number_of_dirty_elements
;
 
set_dirtyindex proc near
 
	push	bp			;preserve index
	mov	ax,cx			; remember new dirty_bits
 
	cmp	word_masks,0		; using 16-bit masks?
	jz	set_dirty_bytemasks	;  brif not
 
	add	bp,cs:dirtyindexoffset	;get bucket for dirty bits
	xchg	ax,cs:[bp]		;fill bucket with new bits,
					;old dirty bits into ax

	ifdef	USE_VALID
	sub	bp,cs:dirtyindexoffset
	add	bp,cs:validindexoffset
	mov	word ptr cs:[bp],bx
	endif

	jmp	short set_dirty_common
 
set_dirty_bytemasks:
	shr	bp,1			; convert to byte mask
	add	bp,cs:dirtyindexoffset	; get byte dirty array
	xchg	al,cs:[bp]		; store new value, get old
	xor	ah,ah

	ifdef	USE_VALID
	sub	bp,cs:dirtyindexoffset
	add	bp,cs:validindexoffset
	mov	byte ptr cs:[bp],bl	; set valid_bits, too
	endif
 
set_dirty_common:
	sub	cx,1				;set carry if new bits=0
						;if new bits=0 then
	sbb	cs:number_of_dirty_elements,0	;subtract 1 from count
	sub	ax,1				;set carry if old bits=0
						;if old bits=0 then
	adc	cs:number_of_dirty_elements,0	;add 1 to count
 
	pop	bp				;restore index
	ret
 
set_dirtyindex endp
 
 
; invalidates the block's data
; input
;       bp      =       elementidentifier index
; notes
;       sets element identifier to -1,-1
;
invalidate_element proc near
	push	cx
 
	push	bp				;preserve index
	add	bp,cs:elementidentifierlowordoffset;get low bucket
	mov	word ptr cs:[bp],-1		;set low identifier
	sub	bp,cs:elementidentifierlowordoffset;back to word array index
	add	bp,cs:elementidentifierhiwordoffset;get high bucket
	mov	word ptr cs:[bp],-1		;set high identifier
	pop	bp				;restore index
 
	xor	cx,cx				;set not dirty!
	ifdef	USE_VALID
	push	bx
	xor	bx,bx				; set not valid
	endif
	call	set_dirtyindex
	ifdef	USE_VALID
	pop	bx
	endif
	pop	cx
	ret
invalidate_element endp
 
;
; input
;       bp = elementidentifier index
; uses
;	ax,di,si,cx
; notes
;	calls dirty_write function to commit a block to disk.
;
commit_dirty_element proc near
 
	mov	si,cs:dirtyindexoffset
	cmp	word_masks,0
	jnz	commit_dirty_wordmask

	xor	ch,ch
	shr	bp,1				; convert to byte index
	mov	cl,cs:byte ptr [si+bp]		; get byte dirtyindex
	ifdef	USE_VALID
	mov	si,cs:validindexoffset
	mov	al,cs:byte ptr [si+bp]		; fetch valid info, too.
	endif
	shl	bp,1
	jmp	short commit_dirty_common

commit_dirty_wordmask:
	mov	cx,cs:word ptr [si+bp]		; fetch word dirtymask
	ifdef	USE_VALID
	mov	si,cs:validindexoffset
	mov	ax,cs:word ptr [si+bp]
	endif

commit_dirty_common:
	mov	si,cs:elementidentifierlowordoffset
	mov	si,word ptr cs:[bp+si]
	mov	di,cs:elementidentifierhiwordoffset
	mov	di,word ptr cs:[bp+di]
 
	ifdef	USE_VALID
	push	bx		; save original bx now
	push	ax		; save the old valid mask
	endif
 
	;input parameters to dirty_write
	;are di:si = 32-bit elementidentifier
	;    al = 8-bit dirtyindex
	;    bp = cache_element_index
 
	mov	ax,cx				;dirty bits into ax
	push	bp
	call	dirty_write			;call out to write data
	pop	bp				;restore index
 
	ifdef	USE_VALID
	pop	bx				; restore old valid mask
	endif

	push	cx ;bugbug is this necessary
	xor	cx,cx				;zero dirty bits
	call	set_dirtyindex			; retain original valid bits

	pop	cx

	ifdef	USE_VALID
	pop	bx
	endif
	ret
commit_dirty_element endp
 
 
;
;input
;	none
;output
;       bp-> new cache element
;uses
;	cx, (from commit_dirty) ax,si,di
;notes
;	allocates a new element from the queue by discarding the
;	first-in element. if the element contains "dirty" data,
;	the data must first be commited to disk via commit_dirty_element
;
get_new_cache_element proc near
	call	delete_lru_queue
	call	insert_mru_queue
	push	bp					;preserve index
 
	cmp	word_masks,0				; using word masks?
	jz	get_new_cache_el_bytemask

	add	bp,cs:dirtyindexoffset
	mov	cx,word ptr cs:[bp]	; get word dirtymask
	jmp	short get_new_cache_el_common
 
get_new_cache_el_bytemask:
	shr	bp,1				; convert to byte index
	add	bp,cs:dirtyindexoffset		;get bucket for dirty bits
	mov	cl,byte ptr cs:[bp]		;put dirty bits into cl
	xor	ch,ch
 
get_new_cache_el_common:
	pop	bp					;restore index
	or	cx,cx
	jz	lrunotdirty
	call	commit_dirty_element
lrunotdirty:
	ret
 
get_new_cache_element endp
 
;
; INPUT
;       none
; OUTPUT
;	none
; USES
;	ALL except DS,ES
; NOTES
;	this function is called at interrupt time to
;	commit the least recently used==first-in dirty data
;	element to disk
;
commit_lru_dirty_element proc near
 
	cmp	cs:Number_Of_Dirty_Elements,0	;quick out
	je	done_commit_lru
 
	mov	bp,cs:queue_tail
	mov	di,cs:dirtyindexoffset		;get dirty bits
	mov	si,cs:queue_prevoffset
 
	cmp	word_masks,0		; using byte masks?
 	jnz	start_word_walk
	jmp	short	start_byte_walk

walk_for_lru:
	shl	bp,1
	mov	bp,cs:[si+bp]
start_byte_walk:
	
	shr	bp,1			; index byte array
	cmp	byte ptr cs:[bp+di],0
	je	walk_for_lru
 
	shl	bp,1			; convert to word index
	jmp	short word_commit_lru

word_walk_for_lru:
	mov	bp,cs:[si+bp]
start_word_walk:
	cmp	word ptr cs:[bp+di],0
	je	word_walk_for_lru

word_commit_lru:
	call	commit_dirty_element
done_commit_lru:
	ret
 
commit_lru_dirty_element endp
IF 0
;BUGBUG!!!!!!  This routine HAS NOT been adapted for word-index bps.
;old no-lru code
;
; input
;       none
; output
;	none
; uses
;	all except ds,es
; notes
;	this function is called at interrupt time to
;	commit the least recently used==first-in dirty data
;	element to disk
;
commit_lru_dirty_element proc near
	push	es				;preserve segments!
	push	cs				;"data" segment=cs->es
	pop	es
 
						;bug bug redundant?
	cmp	cs:number_of_dirty_elements,0	;quick out
	je	lrudone_commiting
 
	mov	bp,word ptr cs:queueheadptr[0]	;get queue head
	inc	bp				;get first-in-first-out
	cmp	bp,cs:queuelength		;array wrap must goes to zero
	jb	lruno_wrap
 
	xor	bp,bp				;array wrapped, so fifo=zero
lruno_wrap:
	mov	di,cs:dirtyindexoffset		;get dirty bits for fifo
	add	di,bp				;need di for rep scasb
 
	cmp	word_mask,0			; byte masks?
	jz	commit_lru_bytemask
 
	add	di,bp				; point into word array
 
	mov	cx,cs:queuelength		;cx = rep scasb count
	sub	cx,bp				;starting at bp element
	inc	cx	;go one past since we test for cxz bug bug dumb?
	cld					;good programmer
 
w_lrucontinue_commit:
	xor	ax,ax  				;searching for non-zero
	repe	scasw     			;scan until non-zero
	jcxz	w_lrufirst_done			;cx=0 means nothing found bug bug
	jmp	short w_lru_commit_it
 
 
w_lrufirst_done:
	mov	di,cs:dirtyindexoffset		;nothing was found in first
						;half of queue scan, so
						; start at array head to
						;complete scan
 
	mov	cx,word ptr cs:queueheadptr[0]	;count -> cx
	inc	cx 				;one cause cx is a count
	inc	cx 				;one more since we check cxz
 
	xor	ax,ax
	repe	scasw
	jcxz	lrudone_commiting
 
w_lru_commit_it:
	dec	di    				;one past, go back
	dec	di
 
	sub	di,cs:dirtyindexoffset
	shr	di,1
	jmp	short commit_lru_di
 
commit_lru_bytemask:
 
	mov	cx,cs:queuelength		;cx = rep scasb count
	sub	cx,bp				;starting at bp element
	inc	cx	;go one past since we test for cxz bug bug dumb?
	cld					;good programmer
 
lrucontinue_commit:
	xor	al,al  				;searching for non-zero
	repe	scasb     			;scan until non-zero
	jcxz	lrufirst_done			;cx=0 means nothing found bug bug
	jmp	short lru_commit_byte
 
 
lrufirst_done:
	mov	di,cs:dirtyindexoffset		;nothing was found in first
						;half of queue scan, so
						; start at array head to
						;complete scan
 
	mov	cx,word ptr cs:queueheadptr[0]	;count -> cx
	inc	cx 				;one cause cx is a count
	inc	cx 				;one more since we check cxz
 
	xor	al,al
	repe	scasb
	jcxz	lrudone_commiting
 
lru_commit_byte:
	dec	di    				;one past, go back
 
	sub	di,cs:dirtyindexoffset
 
commit_lru_di:
	mov	bp,di
 
	call	commit_dirty_element
 
lrudone_commiting:
	pop	es
	ret
 
commit_lru_dirty_element endp
 
endif
;
; input
; 	none
; output
;       none
; note
;	commits all dirty blocks
; uses
;	none
;
commit_all_dirty proc near
	push	ax				;preserve registers
	push	bx				;trashed by commit_lru_dirty
	push	cx
	push	dx
	push	si
	push	di
	push	bp
if BEEP
	cmp	rbeep_flag,1
	jne	nobeep
	call	flush_beep
nobeep:
endif
 
	inc	cs:in_bambi
continue_commit:
	call	commit_lru_dirty_element	;get all dirty blocks to disk
	cmp	cs:number_of_dirty_elements,0	;until there are no more
	jnz	continue_commit
	dec	cs:in_bambi
 
	pop	bp				;restore registers
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
commit_all_dirty endp
 
;
;input
;       ax:dx = 32-bit cookie
;       cx    = 16-bit dirty index; 0 means not dirty
;	bx     = 16-bit valid mask, if enabled
;output
;       bp = element index
;used
;	cx
;notes
;
;	allocates a new element in the cache; possibly commits
;	dirty block to disk to free slot
;
queue_element proc near
 
	ifdef	USE_VALID
	push	bx
	endif

	push	cx				;preserve input registers
	push	ax
	push	dx
	call	get_new_cache_element		;get handle to element->bp
	pop	dx				;restore input registers
	pop	ax
	pop	cx

	ifdef	USE_VALID
	pop	bx
	endif
 
 
	push	bp				;preserve index
	add	bp,cs:elementidentifierlowordoffset;get low bucket
	mov	word ptr cs:[bp],ax		;set low identifier
	sub	bp,cs:elementidentifierlowordoffset;back to word array index
	add	bp,cs:elementidentifierhiwordoffset;get high bucket
	mov	word ptr cs:[bp],dx		;set high identifier
	pop	bp				;restore index	
 
	jmp	set_dirtyindex			;set dirty bits
 
queue_element endp
 
;
;input
;	dx:ax == 32-bit cookie
;output
;       bp = element index if found, -1 if not
;	cx = dirty_bits for that element
;	ax = valid_bits for that element, (if USE_VALID is defined)
;uses
;	all except ds,si,bx
;WARNING TRASHES ES
;notes
;	looks up the cookie in the cache and returns handle to it
;	if found
;
fast_lookup proc near
	push	cs
	pop	es
 
	mov	di,cs:elementidentifierlowordoffset
	mov	cx,cs:queuelength
 
;	precompute the distance between di after a match and
;	  the corresponding word in the highword array.
 
	mov	bp,cs:elementidentifierhiwordoffset
	sub	bp,di
	sub	bp,2		; di will already be incremented when we use it
	cld			;BUG BUG not really necessary since
			        ;this is done at entry point, but safety
				;is paramount and the cost is low
 
continue_scan:
	repne	scasw
	jnz	no_match
 
	cmp	dx,cs:[bp+di]
	jne	continue_scan
 
	lea	bp,[di].-2		; point bp back to matched loword
 
	sub	bp,cs:elementidentifierlowordoffset
 
;	now bp is a word index of the correct element
 
	mov	di,cs:dirtyindexoffset
 
	cmp	word_masks,0		; is dirtyindex a word array?
	jz	lookup_dirty_is_bytes
 
	mov	cx,word ptr cs:[bp+di]	; get dirtyindex

	ifdef	USE_VALID
	mov	di,cs:validindexoffset
	mov	ax,word ptr cs:[bp+di]	; get validindex
	endif

;	**** Note:  Assume bring_to_front doesn't trash our return registers.

	jmp	bring_to_front
 
lookup_dirty_is_bytes:
	shr	bp,1			; convert to byte index
	mov	cl,byte ptr cs:[bp+di]
	xor	ch,ch

	ifdef	USE_VALID
	mov	di,cs:validindexoffset
	mov	al,byte ptr cs:[bp+di]
	xor	ah,ah
	endif

	shl	bp,1			; back to word index
	jmp	bring_to_front
 
no_match:
	mov	bp,-1
	ret
 
fast_lookup endp
 
;
;input
;	none
;output
;       none
;uses
;	all except ds,es
;notes
;	marks all elements as not allocated, not dirty
;warning
;	dirty elements are *not* commited--this must be done
;	before calls to flush_queue!
;
flush_queue proc near
if BEEP
	cmp	rbeep_flag,1
	jne	nobeep2
	call	invalidate_beep
nobeep2:
endif
 
	mov	word ptr cs:queueheadptr[0],0
 
	push	es
	push	di
	mov	si,cs:queuelength
	push	cs
	pop	es
 
	mov	di,cs:elementidentifierlowordoffset
	mov	cx,si
	mov	ax,-1
	rep	stosw
 
	mov	di,cs:elementidentifierhiwordoffset
	mov	cx,si
	rep	stosw
 
	mov	di,cs:dirtyindexoffset
	mov	cx,si
	xor	ax,ax
	cmp	word_masks,0
	jz	flush_byte_masks
	add	cx,cx			; just double the count
;					;  for word masks
flush_byte_masks:
	rep	stosb
 
	pop	di
	pop	es
	mov	word ptr cs:number_of_dirty_elements,0
	ret
 
flush_queue endp
 
zseg ends
 
end
