	page	,132

;
;	Cache initialization module
;

;tracker	=	0ffffh
tracker = 0

include devsym.inc	; get device driver structure definitions
include dpb.inc
include bambi.inc
PUBLIC	initialize

;extrn	 ismsdos			 :far
zseg	segment	public 'code'

	assume	cs:zseg

;
;	data from umbload.asm
;
extrn	myPSP				:word
extrn	savestrategy			:word
extrn	saveumblink			:word

extrn	warning_pop_up			:near
;
;	data from rdata.asm
;
extrn	dos_3x				:word
extrn	last_buffer			:word
extrn	max_valid_buffers		:word
extrn	block_buffer			:byte
extrn	ending_address			:word
extrn	processor_type			:byte

extrn	real_dd_headers			:dword
extrn	real_cache_units		:byte
extrn	secsize_and_align_info		:word
extrn	cache_block_bytes		:word
extrn	driver_hooks			:byte

extrn	loc_req_seg			:word
extrn	lb_seg				:word

extrn	first_instance			:byte
extrn	number_of_cache_elements	:word
extrn	number_of_cache_elements_win	:word
extrn	drives_to_cache			:byte

extrn	queuelength			:word

extrn	elementidentifierhiwordoffset	:word
extrn	elementidentifierlowordoffset	:word
extrn	dirtyindexoffset		:word
extrn	word_masks			:byte
	ifdef	USE_VALID
extrn	validindexoffset		:word
	endif
;
;	data from text.asm
;
extrn	msg_xms_memory_error		:byte
extrn	msg_no_himem			:byte 
extrn	msg_version_fail		:byte
extrn	msg_installed			:byte
extrn	msg_in_dos_box			:byte
extrn	msg_too_large			:byte
;
;	routines from bambi.asm
;
extrn	our_int				:far
extrn	our_strat			:far
extrn	resident_initialization		:near
;
;	routines from int2f.asm
;
extrn	call_bambi			:near
extrn	initialize_int2f		:near

;
;	routines from cmdline.asm
;
extrn	parse_command_line		:near
extrn	display_status			:near
extrn	be_quiet			:word
extrn	load_low			:byte
;
;	routines from cacheman.asm
;
extrn	initialize_cache		:near
extrn	halt_cache			:near
;
;	routines from hooksini.asm
;
extrn	init_bambi_io			:near
extrn	hook_ints			:near
;
;	routines from drvtype.asm
;
extrn	setup_default_drive_list	:near

;
;	routines from indosbox.asm
;
extrn	in_dos_box			:near
;
;	routines from get_dbp.asm
;
extrn	get_dpb				:near
;
;	routines from xms.asm
;
extrn	query_free_xms_memory		:near
extrn	initialize_xms			:near
;
;	data from queueman.asm
;
extrn	queue_nextoffset		:word
extrn	queue_prevoffset		:word
;
;	routines from pdetect.asm	
;
extrn	detect_processor		:near
;
;	routines from dec_out.asm
;
extrn	set_thousands_separator		:near
;
;	routines from logphys.asm
;
extrn	compute_logical_to_physical 	:near


extrn	msg_dos_access			:byte
extrn	msg_dos_access2			:byte
extrn	shutupmsdosflag			:byte
extrn	warnmsdosmessage		:byte
extrn	display_dec_dword		:near

ENVSEGOFFSETINPSP equ 02Ch

PUBLIC dos_size
PUBLIC win_size

dos_size	dw	512		;default size--here if query fails
win_size	dw	512		;default size--here if query fails

dosinfo		dw	?
actual_buffer_size	dw	?
target_buffer_size	dw	16384		; use 16K buffer by default
PUBLIC target_buffer_size
PUBLIC msg_and_fail
PUBLIC dosinfo
PUBLIC display_message

display_message:
	mov	ah,9
	int	21h
	stc
	ret

initialize:
;	int 3
;	mov	ax,1234h
;	call	call_bambi

	cld
	push	cs
	pop	ds
	assume	ds:zseg

	mov	loc_req_seg,cs		; set up a quick reference constant

;	set up the save_it vector for tracker

	if	tracker

tracker_mux	=	0c500h		; tracker's int2f mux number

	extrn	save_it_off:word,save_it_seg:word

	mov	save_it_seg,cs		; just in case, set up the far return

	push	es
	mov	ax,tracker_mux
	int	2fh
	or	ax,ax
	jnz	no_tracker

	mov	save_it_off,bx
	mov	save_it_seg,es		; save entry point to log routine

no_tracker:
	pop	es

	endif

	xor	ax,ax
;	call	ismsdos dont do this, instead do next line...
	or	ax,2000h	

	push	ax
	push	bx

	mov	ah,30h			; get DOS version
	int	21h
	cmp	al,3			; 3.x is special case, earlier fails
	ja	version_ok

	jb	version_fail
	cmp	ah,10			; must be 3.1 or greater
	jae	version_3_ok

version_fail:
	add	sp,4
	mov	dx,offset cs:msg_version_fail
msg_and_fail:
	call	display_message
	jmp	dont_load_exit

version_3_ok:
	mov	dos_3x,-1		; set flag for DOS 3.x

version_ok:

	mov	ax,dos_3x
	add	dosinfo,ax

	call	set_thousands_separator

	mov	ax,BAMBI_GET_STATS	; get stats also used for detection
	call	call_bambi
	cmp	ax,BAMBI_SIGNATURE	; ax = signature if bambi is loaded
	jne	bambi_already_resident
	mov	cs:first_instance,0	; bambi is already resident

	mov	be_quiet, 0		; don't be quiet if already resident


bambi_already_resident:
	pop	bx
	pop	dosinfo

	call	setup_default_size
	call	setup_default_drive_list ; select default drives to cache
					; list may be modifed by parsing

	call	compute_logical_to_physical 

	call	parse_command_line
	jc	just_exit

warnmsdos:
	cmp	shutupmsdosflag,1
	je	dontwarn
	cmp	warnmsdosmessage,1
	jne	dontwarn

	push	bp
	mov	dx,offset msg_dos_access
	call	display_message
	pop	ax
	xor	dx,dx
	call	display_dec_dword	
	mov	dx,offset msg_dos_access2
	call	display_message

dontwarn:
	
	cmp	cs:first_instance,0
	jne	short_jump_around

just_status:
	call	display_status
just_exit:
	jmp	dont_load_exit
short_jump_around:
	call	in_dos_box			;are we in a dos box (zero flag set)
	jnz	not_in_dos_box
	mov	dx,offset cs:msg_in_dos_box
	call	display_message			
	jmp	dont_load_exit	


not_in_dos_box:

	call	detect_stacker_volume

;	calculate quick reference constants


;	initialize secsize_and_align_info to 'unknown'

	push	cs
	pop	es
	mov	di,offset secsize_and_align_info
	mov	ax,0ffffh		; align factors & secsize unknown
	mov	cx,26
	cld
	rep	stosw


;;;	calculate memory locations of data structures

;	see how big our buffer should be.  We'll use target_buffer_size
;	  as an attempt, but we'll round it up to our block size.

	mov	ax,target_buffer_size
	cmp	ax,cache_block_bytes
	jae	target_ok
	mov	ax,cache_block_bytes	;target buffer must be at least 1 block
target_ok:
	xor	dx,dx
	div	cache_block_bytes	; get number of full buffers
	add	dx,-1			; force carry if any remainder
	adc	ax,0			;  and round up
	mov	max_valid_buffers,ax	; save it
	dec	ax			; get one less for last buffer
	mul	cache_block_bytes	; find start of last buffer
	;add	ax,offset block_buffer
	mov	last_buffer,ax		;  and save pointer for dirty_write

	add	ax,cache_block_bytes	; allocate last buffer, too
	mov	actual_buffer_size,ax

;;;
	mov	dx,offset block_buffer	
;;;
	;mov	dx,ax

;	See if masks are words or bytes and set variable accordingly.

	mov	ax,cache_block_bytes	; get cache block size
	add	ax,-8192		; set carry if >= 8192
	sbb	al,al			; propogate carry across al
	mov	word_masks,al		; set word_masks value

;	mov	word_masks,0		; continue using word_masks=0 for
;					;  now until word masks are better
;					;  tested.

;	now allocate cache data arrays at cs:dx

	mov	cs:dirtyindexoffset,dx

	mov	ax,number_of_cache_elements

	cmp	cs:word_masks,0		; are we using word masks?
	jz	init_with_byte_masks	; brif so
	shl	ax,1			; allocate 1word per array element
init_with_byte_masks:
	add	dx,ax
	jc	too_large_for_memory
	ifdef	USE_VALID
	mov	cs:validindexoffset,dx
	add	dx,ax
	jc	too_large_for_memory
	endif

	cmp	cs:word_masks,0		; if we just allocated byte masks,
;					;  now we've got to adjust our
;					;  array size value for word arrays.
	jnz	init_with_word_masks
	shl	ax,1			; following arrays are words
init_with_word_masks:

	mov	cs:ElementIdentifierHiwordOffset,dx
	add	dx,ax
	jc	too_large_for_memory
	mov	cs:ElementIdentifierLowordOffset,dx
	add	dx,ax
	jc	too_large_for_memory
	mov	cs:queue_nextoffset,dx
	add	dx,ax
	jc	too_large_for_memory
	mov	cs:queue_prevoffset,dx
	add	dx,ax
	jc	too_large_for_memory
	add	dx,0fh
	jc	too_large_for_memory
	mov	cl,4
	shr	dx,cl			; calculate ending address
	mov	cs:ending_address,dx


	mov	cx,cs:number_of_cache_elements

	cmp	cx,2
	jb	allocation_error		;must be at least 2 cache blocks

	mov	cs:queuelength,cx
	mov	bx,cs:cache_block_bytes

	push	dx
	call	initialize_cache
	pop	dx
	jnc	no_xms_error

error_allocating_XMSmemory:
	cmp	ax,ERROR_NO_XMS_DRIVER
	jne	allocation_error
	mov	dx,offset cs:msg_no_himem
	jmp	short xmserrormessage
allocation_error:
	mov	dx,offset cs:msg_XMS_memory_error
xmserrormessage:
	call	display_message
	jmp	short	dont_load_exit
too_large_for_memory:
	mov	dx,offset cs:msg_too_large
	call	display_message
	jmp	short dont_load_exit



	;;; called from below if we could not allocate our buffer
	;;; in a UMB
no_umb:		      	

	;;;	we were unable to allocate our buffer into umb, however
	;;;	it may fit in the code's block. 
	mov	ax,cs
	add	ax,cs:ending_address
	mov	cs:lb_seg,ax
	mov	ax,cs:actual_buffer_size
	mov	cl,4
	shr	ax,cl
	add	cs:ending_address,ax	;add buffersize onto TSR chop off point

	mov	bx,cs:ending_address	;save for use with memory size check
	add	bx,10h			;adjust for PSP
	push	es
	mov     es,cs:myPSP		;es = segment to shrink
	
	mov	ax,cs
	add	bx,ax
	cmp	word ptr es:[2],bx
	

	;mov     ah,4ah          	;AH = modify memory block
	;int     21h             	;free excess memory
	pop	es
	;;;	if carry is set, we dont fit into this memory block
	;;;	with code and data
	;;;	BUG BUG assume that means we are in a too small UMB
	;;;	so exit back to umbload to load code low 
	jc	dont_load_exit
	
	jmp	short allocate_computed

explicitely_disable_umbs:
	;;;	here, we disable umb allocation, but continue with the
	;;; 	allocation.  We do this so the user can use
	;;;	loadhi to get the CODE to load high, but the /L
	;;;	switch will force the buffer low.

	mov     ax,5803h        ;link/unlink UMBs bx = 0 is unlink,bx =1 is link
	mov     bx,0		;enable umb links
	int     21h
	jmp	short	continue_alloc


dont_load_exit:
	call	Halt_Cache
	mov	ax,4c01h		; error code must be 1 for umbload.asm
	int	21h

no_xms_error:

	;;;	now allocate the big buffer
	;;;	first we try to allocate it in a UMB. If this fails,
	;;;	we tack it on to the code and chop off with TSR.
	;;;	The reason we do it like this is so we can be sure
	;;;	our initialization code is not tromped before we
	;;;	are done initializing.

	cmp	load_low,1
	je	explicitely_disable_umbs

        mov     ax,cs
        cmp     ax,0a000h
        ja      no_umb          ; we are already in a umb, so append
                                ; we know there is space since we loaded

	mov     ax,5803h        ;link/unlink UMBs bx = 0 is unlink,bx =1 is link
	mov     bx,1		;enable umb links
	int     21h
	jc	no_umb

      	mov	ax,5801h
      	mov	bl,40h		;(assume bh zero) first-fit high only
      	int	21h

continue_alloc:
	mov	ah,48h
	mov	bx,actual_buffer_size	;size of block buffer
	mov	cx,4
	shr	bx,cl			;paragraphs
	int	21h			;segment in AX if no error
	jc	no_umb
	
	mov	cs:lb_seg,ax
	;;;
allocate_computed:
			       
	call	detect_processor
	mov	processor_type,al

;WARNING--POINT OF NO RETURN--device drivers will be hooked and
;intterrupts too, so after this point TSR is the only way to exit!
;
;	int	3
	call	init_drive_info		

	;;;	drives_to_cache is setup in command line parsing
	mov	cx,26
loop_drive:
	mov	bp,cx
	mov	al,drives_to_cache[bp-1]
	cmp	al,NO_CACHE
	je	dontcachethisdrive
	cmp	al,READ_CACHE
	mov	al,3Fh		;assume read/write
	jne	read_write
	mov	al,7fh		;read only
read_write:
	push	cx
	dec	cx
	call	cache_drive
	pop	cx
dontcachethisdrive:
	loop	loop_drive


	call	initialize_int2f
	call	init_bambi_io

	call	hook_ints


	call	display_status

	cmp	be_quiet,0
	jne	no_display
	mov	dx,offset cs:msg_installed
	call	display_message
no_display:

	call 	hook_drives

	mov     ax,5803h        ;link/unlink UMBs bx = 0 is unlink,bx =1 is link
	mov     bx,saveumblink	;disable umb links
	int     21h

	mov	ax,5801h
	mov	bx,cs:savestrategy	
	int	21h

	;;; free the environment
	mov	es,cs:myPSP
	mov 	es,es:ENVSEGOFFSETINPSP
	mov	ah,49h
	int	21h

	jmp	resident_initialization	;initialize dynamic data, TSR

;
;INPUT
;	cx = drive letter to cache (one based)
;	ah = C0h if no cacheing is done, 0h if caching is done
;OUTPUT
;	none
info_drive proc near
	push	es
	push	ds
	push	bp
	push	bx
	push	dx

	mov	dl,cl
	call	get_DPB
	jc	info_drive_done

	mov	bp,cx
	dec	bp

	;;; save off the unit codes (byte fields)

	mov	al,byte ptr ds:[bx].DPB_UNIT ; get unit code for this logical drive
	or	al,ah			; set high bits (C0h) if no cacheing
	mov	byte ptr cs:real_cache_units[bp],al
	mov	dx,bp
	mov	byte ptr ds:[bx].DPB_UNIT,dl
	
	shl	bp,1
	shl	bp,1	

	add	bx,cs:dos_3x			; adjust for DOS 3.x dpb

	mov	ax,ds:word ptr [bx].DPB_DRIVER_ADDR ; get original driver address
	mov	word ptr cs:real_dd_headers[bp],ax	; and save it
	mov	ax,ds:word ptr [bx].DPB_DRIVER_ADDR+2
	mov	word ptr cs:real_dd_headers[bp+2],ax
	
info_drive_done:
	pop	dx
	pop	bx
	pop	bp
	pop	ds
	pop	es
	ret
info_drive endp

init_drive_info proc near
	mov	cx,26

init_this_drive:
	mov	ah,0C0h  ; initalize all drives to no-cacheing
	call	info_drive
	loop	init_this_drive
	ret
init_drive_info endp

;
;INPUT
; 	cx = drive to cache (0 based)
;	al = read/write mask =
;		3F -> read/write
;		7F -> read only
;OUTPUT
;	none
cache_drive proc near
	push 	bx
	mov  	bx,cx
	and  	byte ptr cs:real_cache_units[bx],al ;mask off high bits
	;;bug bug read/write both set!
	pop  	bx
	ret	
cache_drive endp

hook_drives proc near
	;int 3

	push	ds
	push	es


	;jmp	rope
	push	ds
	push	es
	push	bp
	push	si
	push	di

	mov	ah,52h
	int	21h
	add	bx,34
	;;;	es:bx->device header chain
	push	es
	pop	ds
	mov	bp,bx
	;;;	ds:bp->device header chain

	xor	di,di			; di is index into new header table
next_chain:
	les	bx,es:[bx]
	cmp	bx,-1
	je	done_walking_device_chain
	test	word ptr es:[bx].sdevatt,8000h		;block device?
	jnz	next_chain
	;;;	copy device header into our table

	mov	ax,es:[bx].sdevatt
	mov	cs:driver_hooks[di].sdevatt,ax


	mov	ax,word ptr es:[bx].sdevname[0]
	mov	word ptr cs:driver_hooks[di].sdevname[0],ax
	mov	ax,word ptr es:[bx].sdevname[2]
	mov	word ptr cs:driver_hooks[di].sdevname[2],ax
	mov	ax,word ptr es:[bx].sdevname[4]
	mov	word ptr cs:driver_hooks[di].sdevname[4],ax
	mov	ax,word ptr es:[bx].sdevname[6]
	mov	word ptr cs:driver_hooks[di].sdevname[6],ax

	;;;	point new header to our entry points

	mov	ax,offset cs:our_int
	mov	cs:driver_hooks[di].sdevint,ax
	mov	ax,offset cs:our_strat
	mov	cs:driver_hooks[di].sdevstrat,ax

	
	;;;	hook into front of device chain

	cli     ; better safe than sorry..
	mov	si,word ptr ds:[bp]
	mov	dx,word ptr ds:[bp+2]
	mov	ax,offset cs:driver_hooks
	add	ax,di
	mov	word ptr ds:[bp],ax
	mov	word ptr ds:[bp+2],cs
	mov	word ptr cs:driver_hooks[di].sdevnext[0],si
	mov	word ptr cs:driver_hooks[di].sdevnext[2],dx
	sti

	push	cs
	pop	ds
	mov	bp,ax


	;;;	update any DPB that points to the replaced driver header
	;;;	to point to the new driver header
	;;;	right now, es:bx points to old driver,
	;;;	ds:bp points new new header
	;;;
	call	update_dpbs

	add	di,size sysdev

	jmp	next_chain
done_walking_device_chain:
	
	pop	di
	pop	si
	pop	bp
	pop	es
	pop	ds

rope:

	pop	es
	pop	ds
	clc
	ret

hook_drives endp


STACKER_DD struc
dv_strategy	dd	1 dup(?)	;physical device driver strategy addr
dv_interrupt	dd	1 dup(?)	;physical device driver interrupt addr
dv_att		dw	1 dup(?)	;device driver header attributes
dv_cluster0	dw	1 dup(?)	;first file cluster of
dv_log2		db	1 dup(?)	;LOG base 2 of physical bytes/sector
dv_unit		db	1 dup(?)	;physical device driver unit number
STACKER_DD ends


extrn	detect_stacker		:near
extrn	detect_stacker_volume	:near
extrn	stacker_dd_pointer	:near

;;;	right now, es:bx points to old driver,
;;;	ds:bp points new new header
;;;
update_dpbs proc near
	push	cx
	push	ds
	push	es
	push	bx
	push	si
	push	di
	push	dx
	push	bp

	mov	di,ds
	mov	si,bp

	mov	bp,bx
	mov	cx,es
	;;;	now cx:bp points to old driver, di:si points to new
	mov	dx,1

loop_dpb:
	
	push	ax
	push	bx
	push	cx
	push	dx
	push	bp
	push	es
	push	ds
	push	si
	push	di
;;;
;;; New code added in Astro to detect magic drives 7/30/92 scottq
;;;

        push    bx
        push    dx
        mov     ax,MAGICDRV_2F          ;is magicdrv installed?
        mov     bx,MAGICDRV_DETECT      
        push    cx
        int     2fh
        pop     cx
        cmp     bx,MD_STAMP
        pop     dx
        pop     bx
        jne     not_magicdrive

        push    bx
        mov     ax,MAGICDRV_2F          ;get magicdrv's host unit 
        mov     bx,MAGICDRV_SMART1
        mov     cx,dx
        dec     cx
        int     2fh
        pop     bx
        cmp     cl,0FFh                 ;FF means this is not a magicdrv unit
        je      not_magicdrive

        ;;;right now, cl holds the host driver unit number for the magicdrv
        ;;;es:di->interrupt,es:si->strategy

        ;;;here, we scan smartdrv's list to find the re-mapped unit number
        ;;;this is the unit number magicdrv should use to call the smartdrv
        ;;;entry points for this drive.
        ;;;NOTE smartdrv's unit numbers are always 1 to 1 with the
        ;;;drive letter, but this code does not assume this
        ;;;
        mov     bx,di
        mov     bp,si

	xor	si,si		; start with drive a:
mlup:

	mov	al,cs:real_cache_units[si]	; get our unit code
	and	al,3fh				; get rid of cache enable bits
	cmp	al,cl           		; match magicdrv's unit?
	jnz	mexit_nomatch

	push	si
	add	si,si				; index dword array
	add	si,si
	lds	si,cs:real_dd_headers[si]	; get dd_header
	mov	ax,ds
        push    bx
        mov     bx,es				; check segment
	cmp	ax,bx
        pop     bx
	jnz	mpopsi_nomatch

        cmp     ds:[si].sdevstrat,bp
	jnz	mpopsi_nomatch

        cmp     ds:[si].sdevint,bx
	jz	mpopsi_match

mpopsi_nomatch:
	pop	si

mexit_nomatch:
	inc	si			; next drive
	cmp	si,26
	jb	mlup

	jmp	short not_magicdrive	; if we can't match, just
mpopsi_match:
	pop	ax
        
        ;;;     We have found this drive in smartdrv's list
        ;;;     now tell magicdrv to call smartdrv with the correct
        ;;;     remapped unit number instead of the dos device driver
        ;;;
        mov     cl,dl
        dec     cl
        mov     dl,al                   ; tell magicdrv new unit number
        mov     ax,MAGICDRV_2F
        mov     bx,MAGICDRV_SMART2
	mov	di, offset cs:our_int
	mov	si, offset cs:our_strat
        push    cs
        pop     es

        int     2fh                     ;magicdrv will switch pointers...

        jmp     stacker_not_present     ;magicdrvs cannot be stacker drives!
        
not_magicdrive:
;;;first detect stacker drives...
	push	dx
	call	detect_stacker		;output is stacker version * 100(dec)
					;in ax (1.00 -> 64h)
	pop	dx
	or	ax,ax
	jnz	stacker_maybe_present
stacker_not_present_short:
	jmp	stacker_not_present
stacker_maybe_present:
	cmp	ax,200			;bug bug only handles version 2 
	jb	stacker_not_present_short

	dec	dx			;unit is zero based for check

	push	cx
	push	dx
	call	detect_stacker_volume
	pop	dx
	pop	cx
	cmp	ax,1
	jne	stacker_not_present_short

	push	cx
	call	stacker_dd_pointer
	pop	cx
	mov	bx,ax
	or	bx,dx
	jz	stacker_not_present_short	;error means ignore
	;;; if we get here, dx:ax -> stacker_dd struc for this unit

	mov	es,dx
	mov	bx,ax	;es:bx -> stacker's structure


	;;; now, es:bx -> stacker

;	***Now we have to figure out what SmartDrv remapped unit number
;	   is for the STAC host drive.  We do that by scanning our
;	   device header and unit code tables to find the drive/unit.

	xor	si,si		; start with drive a:
zlup:

	mov	al,cs:real_cache_units[si]	; get our unit code
	and	al,3fh				; get rid of cache enable bits
	cmp	al,es:[bx].dv_unit		; match stacker's unit code?
	jnz	zexit_nomatch

	push	si
	add	si,si				; index dword array
	add	si,si
	lds	si,cs:real_dd_headers[si]	; get dd_header
	mov	ax,ds				; check segment
	cmp	ax,es:word ptr [bx].dv_strategy.2
	jnz	popsi_nomatch

	mov	ax,ds:[si].sdevstrat
	cmp	ax,word ptr es:[bx].dv_strategy
	jnz	popsi_nomatch

	mov	ax,ds:[si].sdevint
	cmp	ax,word ptr es:[bx].dv_interrupt
	jz	popsi_match

popsi_nomatch:
	pop	si

zexit_nomatch:
	inc	si			; next drive
	cmp	si,26
	jb	zlup

	jmp	short not_stacker_volume	; if we can't match, just
;						;  don't cache this one.

popsi_match:
	pop	ax
	mov	es:[bx].dv_unit,al		; this is new unit code

	mov	word ptr es:[bx].dv_interrupt[0],offset cs:our_int
	mov	word ptr es:[bx].dv_interrupt[2],cs
	mov	word ptr es:[bx].dv_strategy[0],offset cs:our_strat
	mov	word ptr es:[bx].dv_strategy[2],cs

not_stacker_volume:		
stacker_not_present:
	pop	di
	pop	si

	pop	ds
	pop	es
	pop	bp
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	call	get_DPB

assume ds:nothing

	jc	done_dpb_update

	add	bx,cs:dos_3x			; adjust pointer for DOS 3.x dpb

	cmp	word ptr ds:[bx].dpb_driver_addr[0],bp
	jne	do_next_dpb
	cmp	word ptr ds:[bx].dpb_driver_addr[2],cx
	jne	do_next_dpb
	;;;	update dbp to point to new driver
	mov	word ptr ds:[bx].dpb_driver_addr[0],si
	mov	word ptr ds:[bx].dpb_driver_addr[2],di	

do_next_dpb:
	inc	dl
	jmp	loop_dpb
done_dpb_update:
	pop	bp
	pop	dx
	pop	di	
	pop	si
	pop	bx
	pop	es
	pop	ds	
	pop	cx		
assume ds:zseg
	ret
update_dpbs endp

setup_default_size proc near
	call	initialize_xms
	jc	near_fail
	call	query_free_xms_memory
	or	ax,ax
	jz	near_fail
	cmp	ax,512
	jbe	all_dos
	cmp	ax,1024
	jbe	one_meg
	cmp	ax,2048
	jbe	two_meg
	cmp	ax,4096
	jbe	four_meg
	cmp	ax,6144
	jbe	six_meg
big_memory:
	mov	dos_size,2048
	mov	win_size,2048
	jmp	short	exit_default_size
near_fail:
	jmp	short error_query_failed
one_meg:
	mov	dos_size,ax
	mov	win_size,0
	jmp	short exit_default_size
two_meg:
	mov	dos_size,1024
	mov	win_size,256
	jmp	short exit_default_size
four_meg:
	mov	dos_size,1024
	mov	win_size,512
	jmp	short exit_default_size

six_meg:
	mov	dos_size,2048
	mov	win_size,1024
	jmp	short exit_default_size

all_dos:
	mov	dos_size,ax
	mov	win_size,0	

exit_default_size:
	mov	ax,1024			;size is in Kilobytes
	mul	win_size		;dx:ax = #K cache size
	div	cache_block_bytes	;ax = number of cache elements needed
	mov	number_of_cache_elements_win,ax

	mov	ax,1024			;size is in Kilobytes
	mul	dos_size		;dx:ax = #K cache size
	div	cache_block_bytes	;ax = number of cache elements needed
	mov	number_of_cache_elements,ax

error_query_failed:

	ret	
setup_default_size endp

zseg	ends
	end	

