	page    58,132
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;debug  =       1       ; enables some debug code
;tracker        =       0ffffh  ; enables special TRACKER events
debug = 0
tracker = 0

;    BAMBI global register conventions:
;
;       The machine registers are generally considered extremely
;       volatile in this program and should not generally be
;       considered to be preserved when calling a subroutine,
;       unless specifically stated otherwise.
;
;       DS: generally points to CS: in order to access the data
;       stored there without overrides.
;
;    BAMBI design assumptions:
;
;       The number of sectors per cache block CANNOT be > 16.
;
;       Write caching of a given drive implies that it is read cached.
;
;    Purpose of this module:
;
;       This is the main resident device driver interface for Bambi.
;       It contains the main caching logic flow and utilizes external
;       modules for keeping track of caching data structure and accessing
;       secondary RAM storage.
;
;       This module provides one main entry point, which is our_strat/
;       our_int.  This is what DOS calls through to access the block
;       device drivers.  It also contains entry points which are used
;       by the dirty_write function in a companion module.

	.xlist
	include msequ.inc       ; get device request packet definitions
	include devsym.inc      ; get device driver structure definitions
	include bambi.inc
	include bootform.inc
	include bpb.inc
	.list

public  our_int                 ; DOS branches here for ALL block
public  our_strat               ;  device driver calls for devices which
				;  are present when we're first initialized

;       We publish a few special entry points and variables for use
;       by the dirtywrt.asm module.

public  lookup_device
public  read_full_cache_block
public  set_start_sector
public  call_dd_common

public  loc_req_seg             ; allow bambinit to initialize our pointers
public  lb_seg

public  our_count
public  our_trans_off
public  our_trans_seg
public  our_starth
public  our_startl
public  our_start

public  loc_reqblk
public  num_valid_buffers
public  rblk_op
public  media_id
public  media_ids 

public  packet_size
public  packet_sizes

public  next_bad_entry_ptr 
public  num_bad_blocks  

zseg    segment public 'code'
	assume  cs:zseg,ds:nothing,es:nothing

;
;       data from rdata.asm
;
extrn   last_buffer             :word
extrn   max_valid_buffers       :word

extrn   dos_3x                  :word
extrn   hit_l                   :word
extrn   hit_h                   :word
extrn   nohit_l                 :word
extrn   nohit_h                 :word
extrn   in_bambi                :byte

extrn   selected_drive          :byte

extrn   real_dd_headers         :dword
extrn   real_cache_units        :byte
extrn   secsize_and_align_info  :word
extrn   cache_block_bytes       :word

extrn   cache_block_words       :word

;
;       routines from queueman.asm
;
extrn   flush_queue             :near
extrn   set_dirtyindex          :near
extrn   fast_lookup             :near
extrn   invalidate_element      :near
extrn   queue_element           :near

;
;       routines from cacheman.asm
;
extrn   cache_to_buffer         :near
extrn   buffer_to_cache         :near
extrn   commit_all_dirty        :near
;
;       data from int2f.asm
;
extrn   accessing_swap_file_ptr :dword
;
;       data from queueman.asm
;
extrn   queuelength             :word
;
;       data from hooks.asm
;
extrn   resident_stack          :word
extrn   temp_res_stack          :word
extrn   save_stack_ss           :word
extrn   save_stack_sp           :word
extrn   commit_all_when_ok      :byte
extrn   write_behind_cache      :near

;
;       routines from popup.asm
;
extrn   warning_pop_up          :near

;
;       data from bambinit.asm
;
extrn   ending_address          :word
extrn   number_of_cache_elements:word   ;WARNING transient!
extrn   initqueue               :near

;       local variables
;
;       Please note:  Many of these variables are only valid for
;                     the operation being performed by the mainline
;                     routine.  dirty_write must handle its own
;                     configuration constants on the fly.
;
;       put our dword pointers first so they're all
;       aligned without wasting space

far_call_address        dd      0
d_trans                 dd      0
dd_header               dd      0
user_reqblk             dd      0
MAXRENTER       equ     5               ;arbitrary number of reentracy allowed
user_save_reqblk        dd      MAXRENTER       dup(0)

;       quick reference points for our block buffer and request block
;          the segments fields are declared external so bambinit.asm
;          can initialize them.

loc_reqblk              label   dword
			dw      our_reqblk
loc_req_seg             dw      0       ; init'd by bambinit, always == cs

local_buf               label   dword
			dw      0
lb_seg                  dw      0       ; this will generally be a
					;  different segment from cs

;       put word variables next

blockids                dw      16 dup (?)
d_count                 dw      0
curblk_l                dw      0
curblk_h                db      0,0     ; may be accessed as word

lastblk_l               dw      0
lastblk_h               db      0,0     ; may be accessed as word

curblk_index            dw      0       ; high byte may be assumed to be zero
cache_element_index     dw      0

num_valid_buffers       dw      0       ; num. valid buffers in 'super-cache'
bufferblk_l             dw      0       ; low word of base block of super-cache
bufferblk_h             dw      0       ; high word + drive number

cache_align_factor      dw      0       ; must be < cache_block_sectors
cache_block_shift       dw      2       ; log2(blocksize/sectorsize)

sector_size_bytes       dw      512
cache_block_sectors     dw      4

dirty_mask              dw      0       ; mask for last looked up element
	ifdef   USE_VALID
valid_mask              dw      0       ; mask for last looked up element
	endif
cache_mask              dw      0       ; this is the full mask
;                                       ; for the selected block/sector size

;       the following data structure is used when we wish to
;       wish to issue a device driver call other than the one
;       originally passed to us from DOS.  It can benefit from
;       word alignment.

our_reqblk      label   byte
packet_size     db      30      ; length
rblk_cache_unit db      2       ; cache unit
rblk_op         db      devrd   ; read command
devstatus       dw      0       ; status
		db      13-5 dup (0) ; other stuff (???)
media_id        db      0f0h    ; media id byte
our_trans_off   dw      0       ; transfer offset
our_trans_seg   dw      0       ; transfer segment
our_count       dw      0       ; count
our_start       dw      0ffffh  ; start (ignored)
		dw      0,0
our_startl      dw      0       ; start low
our_starth      dw      0

in_device_call  dw      -1
media_ids       db      26      dup(0f0h) ;save media_id for dirty writes

packet_sizes    db      26      dup(30h)  ;save packet sizes for dirty writes

MAXBADS         equ     32
next_bad_entry_ptr dw   0       ; first entry will be 0
num_bad_blocks  dw      0
bad_blocks      dw      MAXBADS dup(0)
bad_drives      dw      MAXBADS dup(-1)

;       now the byte variables

original_unit           db      -1
flags_and_unit          db      0
cache_unit              db      0


;       enter a BAMBI record in TRACKER, subtype as argument to macro
;         this macro is defined to be a null macro of TRACKER is set
;         false, so that every single LOG event needn't have IF TRACKER
;         conditionals on it.

log_it  macro   rectype
	if      tracker
	push    ax
	mov     al,rectype
	call    tracker_log
	pop     ax
	endif
	endm

	if      tracker

;       We may want to save some special BAMBI events in
;       the TRACKER log.  If this feature is enabled, we'll
;       call through a FAR variable into TRACKER.

	public  save_it_off     ; allow external initialization for
	public  save_it_seg     ;  TRACKER save entry point

save_it         label   dword
save_it_off     dw      offset far_ret
save_it_seg     dw      0       ; will be init'd to cs if tracker not present

far_ret:
	retf

code_bambi      =       6       ; this is our special TRACKER record type

event           struc           ; this is the TRACKER event structure
rectype         db      ?
level           db      ?
regax           dw      ?
regbx           dw      ?
regcx           dw      ?
regdx           dw      ?
reges           dw      ?
time            dd      ?       ; time stamp
event           ends

xbuf    event   <code_bambi,0>  ; this is our local event structure

;-----------------------------------------------------------------------
;
;       enter a bambi-type tracker log
;
;       entry: al == event subtype
;              ds == cs
;
;       exit:  no registers affected

tracker_log     proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	push    si
	mov     si,offset xbuf
	mov     ds:[si].regax,ax
	call    save_it
	pop     si
	ret

tracker_log     endp

	endif

;-----------------------------------------------------------------------
;
;       call into the device driver using the user's original
;         request packet.
;
;       Entry: ds == cs
;
;       Trashed: es, bx, ax, si, far_call_addr
;                (plus anything affected by bad d.d.'s)

call_dd_ureqblk proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	les     bx,user_reqblk          ;es:bx points to request packet

call_dd_ureqblk endp                    ; note:  fall through to call_dd

;-----------------------------------------------------------------------
;
;       call through to the device driver
;         plug in the mapped unit code to the request block
;
;       Entry: ds == cs, es:[bx] -> request block
;
;       Trashed: ax, si, far_call_addr
;                (plus anything affected by bad d.d.'s)

call_dd proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	mov     al,cache_unit           ; get remapped cache unit
	mov     es:byte ptr [bx].requnit,al
	push    ds                      ; save caller's ds
	lds     si,dd_header            ; get actual device's header
	assume  ds:nothing
					; fall into call_dd_common
call_dd endp

;-----------------------------------------------------------------------
;
;       This is a special entry point used by dirtywrt for calling
;       out to the device driver.  When this routine is JMP'd to,
;       the original ds is already saved on the stack after the
;       actual return address.
;
;       Entry:  ds:[si] -> device driver header
;               es:[bx] -> request packet
;
;       Exit:   ds restored from stack
;
;       Trashed: ax, "far_call_address", and anything munched by
;                 bad device drivers.

call_dd_common  proc    near
	assume  cs:zseg,ds:nothing,es:nothing

	mov     devstatus,0
	mov     ax,word ptr [si].SDEVSTRAT ; get strategy entry
	mov     word ptr far_call_address[2],ds
	mov     word ptr far_call_address[0],ax

	push    es                      ; save packet for unit restore
	push    bx

	call    DWORD PTR[far_call_address]

	mov     ax,word ptr [si].SDEVINT ; get interrupt entry
	mov     word ptr far_call_address[0],ax

	call    DWORD PTR[far_call_address]

	pop     bx                      ; get packet
	pop     es

;       We have to make sure the UNIT code is returned to the
;         caller unchanged if we're calling through on DOS's
;         packet.  In the case of our own packet, original_unit
;         may not have any meaning, but in this case, we're sure
;         we're not going to care about what's left in that
;         field in the packet.
;
;       This is made more complicated by the fact that the SET_LOGICAL
;         device function returns a value in that field, so in this
;         particular case, we can't restore the original field.

	
	cmp     es:[bx].reqfunc,19
	jae     no_restore_unit         ; skip unit restore if so

	mov     al,cs:original_unit
	mov     byte ptr es:[bx].requnit,al

no_restore_unit:
	pop     ds
	ret                             

call_dd_common  endp

;-----------------------------------------------------------------------
;
;       our strategy entry point

our_strat       proc    far
	assume  cs:zseg,ds:nothing,es:nothing

	inc     cs:in_device_call               ;keep track of reentrancy
	jnz     reentering                      ;zero means first entry
finish_strategy:
	mov     cs:word ptr user_reqblk,bx
	mov     cs:word ptr user_reqblk+2,es
	ret

reentering:
	;
	; we need to save off the packet pointer for the re-entered
	; call so it will still be valid when its execution continues.
	; we use our semaphore as an index into a table which will 
	; hold the saved packet
	
	push    di                              ;must preserve all regs
	push    es
	push    bx

	mov     di,cs:in_device_call            ;treat as index into table
	shl     di,1                            ;dword table
	shl     di,1
	les     bx,user_reqblk                  ;get re-entered packed
	mov     word ptr cs:user_save_reqblk[di],bx     ;save it in our table
	mov     word ptr cs:user_save_reqblk[di+2],es

	pop     bx
	pop     es
	pop     di
	jmp     short finish_strategy   

our_strat       endp


handle_reentrancy_exit:
	pop     word ptr far_call_address[2]
	pop     word ptr far_call_address
	pop     word ptr dd_header[2]
	pop     word ptr dd_header
	pop     word ptr cache_unit
	pop     word ptr flags_and_unit
	pop     word ptr original_unit

	;
	; we need to save off the packet pointer for the re-entered
	; call so it will still be valid when its execution continues.
	; we use our semaphore as an index into a table which will 
	; hold the saved packet
	
	push    di                              ;must preserve all regs
	push    es
	push    bx

	mov     di,cs:in_device_call            ;treat as index into table
	shl     di,1                            ;dword table
	shl     di,1

	mov     bx,word ptr user_save_reqblk[di]
	mov     es,word ptr user_save_reqblk[di+2]
	mov     word ptr user_reqblk,bx
	mov     word ptr user_reqblk[2],es

	pop     bx
	pop     es
	pop     di

	jmp     dont_restore_stack

handle_reentrancy_enter:
	push    word ptr original_unit
	push    word ptr flags_and_unit
	push    word ptr cache_unit
	push    word ptr dd_header
	push    word ptr dd_header[2]
	push    word ptr far_call_address
	push    word ptr far_call_address[2]
	jmp     short skip_stack_swap


;-----------------------------------------------------------------------
;
;       This is it!  This is when DOS is trying to call a device
;         driver.  We intercept the call, and decide if it is something
;         that can be:
;
;               A) saved away into the cache
;            or B) gotten from the cache
;            or C) some combination of A & B
;
;       Entry:  our parameter block was saved in user_reqblk by
;               a call to the previous routine
;
;       Exit:   We'll try to satisfy the request and make sure we
;               fill in the status word of the request block before returning.
;
;       Trashed: We should preserve ALL registers, but we trash ax, bx & es
;                BUGBUG -- will ANYBODY ever care about ax, bx & es here?
;                          we leave es:[bx] pointing at the original req pkt.
;                Flags including direction

our_int proc    far
	assume  cs:zseg,ds:nothing,es:nothing

	push    ds              ; save caller's ds
	push    cs
	pop     ds
	assume  ds:zseg         ; make our data quickly addressable

	inc     in_bambi        ; make sure we aren't reentered

;       if there are no hw stacks, we may be the straw that breaks
;       the stack, so we have to switch to our own stack to ensure
;       no stack overflow problems

	cmp     in_device_call,0
	jne     handle_reentrancy_enter

	mov     save_stack_ss,ss        ;save the current stack 
	mov     save_stack_sp,sp        ;context
	
	push    cs                      ;and switch to our own
	pop     ss                      ;internal stack
	mov     sp,offset resident_stack

skip_stack_swap:

	push    bp                      ; we ALWAYS need these registers saved
	push    dx
	push    cx
	push    es
	push    bx
	push    di
	push    si


;       We must find the device header and unit number for the
;       drive number in the request block.  The unit number code
;       also has a couple of flag bits in it which indicate what
;       level of caching is enabled for this drive.

	les     bx,user_reqblk          ;es:bx points to request packet
	mov     al,byte ptr es:[bx].requnit 

	mov     original_unit,al        ; save original unit code
	call    lookup_device           ; remap unit code, get dev header
	xor     al,0c0h                 ; flip the 'active' bits
	mov     flags_and_unit,al       ; save flags and unit
	and     al,3fh                  ; mask off high bits 
	mov     cache_unit,al

	mov     word ptr dd_header,dx   ; save actual device header
	mov     word ptr dd_header[2],bp

;       done setting up packet structure, its off to the races!
						
	cmp     queuelength,0   ;even if caching is enabled for a drive
	je      just_pass_thru  ;the queue size could be zero, so
				;be sure there is a non-zero cache present

	test    flags_and_unit,0c0h     ; any caching enabled?
	jnz     this_drive_is_cached    ; if any caching, enter main module

just_pass_thru:
	push    si
	call    call_dd                 ; call through to device driver
	pop     si

return_to_caller:
	jmp     go_back_to_dos


;       The code below will branch back up here if the operation is
;         a media check for a cached drive.  We must note the return
;         code before returning in this case so that we can invalidate
;         the cache on a media change.

do_media_check:
	push    si
	call    call_dd
	pop     si
	cmp     BYTE PTR es:[bx].14,1
	je      return_to_caller

;;;the following code notifies windows that the media for
;;;this drive has changed.  While mutlitasking dos boxes,
;;;windows has to maintain the CDS state but can get 
;;;confused when the media changes. This notification will
;;;ensure that windows keeps track of disk changes correctly
;;;code courtesy of aaronr
	xor     di,di
	mov     es,di
	mov     bx,15h                  ;dosmgr device id
	mov     ax,1684h                ;get device api entry point
	int     2fh
	mov     ax,es
	or      ax,di
	jz      nobodytonotify
	push    bp
	mov     bp,sp
	push    es
	push    di
	mov     ax,5h                   ;media change detected
	mov     bl,original_unit        ;zero based drive number
	call    dword ptr [bp-4]        ;change detected on a=0,b=1...
	;
	;Carry is clear if media change processed. Carry set if invalid
	;  drive passed in BL (drive is not in use, is out of range, is
	;  a network drive).
	;
	pop     ax                      
	pop     ax
	pop     bp
nobodytonotify:

	call    reset_drive_stuff

	xor     bh,bh                   ;cause the bpb to be re-read next access
	mov     bl,original_unit
	shl     bx,1
	mov     word ptr secsize_and_align_info[bx],0ffffh
	mov     selected_drive,-1

	jmp     go_back_to_dos

handle_get_bpb:
	call    reset_drive_stuff
	jmp     short not_get_bpb       

;       We now know we're operating on a cached drive.  We have to
;       look at the function number to decide what to do next.

this_drive_is_cached:
	cmp     es:[bx].reqfunc,devrd   ; read, write, writev == go do it
	je      cache_this_readwrite
	cmp     es:[bx].reqfunc,devwrt
	je      cache_this_readwrite
	cmp     es:[bx].reqfunc,devwrtv
	je      cache_this_readwrite
	cmp     es:[bx].reqfunc,1
	je      do_media_check          ; watch return code from media check
	cmp     es:[bx].reqfunc,2
	je      handle_get_bpb
not_get_bpb:

	cmp     es:[bx].reqfunc,genioctl ; certain genioctls require
	jnz     just_pass_thru          ; that we invalidate our cache

	mov     ax,word ptr es:[bx].majorfunction
	cmp     ax,6008h                ;dont trap get bpb
	je      just_pass_thru_short
	cmp     ax,6608h                
	jae     just_pass_thru_short
	call    reset_drive_stuff
just_pass_thru_short:
	jmp     just_pass_thru  

;       We've now qualified this operation substantially.  We know that
;       the request was a read, write, or write w/verify on a drive that
;       we are caching.  Furthermore, the cache is not zero-length and
;       it isn't a Win 3.1 swap file access.

cache_this_readwrite:

	mov     al,es:[bx].0dh                  ; use correct media id
	mov     media_id,al

	mov     al,es:[bx]                      ; use correct packet size
	mov     packet_size,al

	cld                     ; allow our internals to assume direction

;       have we changed drives from last time?

	mov     al,original_unit        ; no need to recalculate constants
	cmp     al,selected_drive       ; on repeated accesses to the same
	jz      constants_already_valid ; drive

;       setup the relevant constants for this drive

	mov     selected_drive,al
	call    get_drive_info

	mov     byte ptr cache_align_factor,ch  ; save cache align factor
	mov     byte ptr cache_block_shift,cl   ; save shift factor

	push    bx
	xor     bh,bh
	mov     bl,original_unit
	mov     al,media_id
	mov     byte ptr media_ids[bx],al
	mov     al,packet_size
	mov     byte ptr packet_sizes[bx],al
	pop     bx

	mov     ax,cache_block_bytes            ; get sector size
	shr     ax,cl
	mov     sector_size_bytes,ax

	mov     al,1
	shl     al,cl
	mov     byte ptr cache_block_sectors,al ; get number of secs / cache block

;       Now we have to generate a mask with one bit per sectors/blk

	mov     cl,al
	mov     ax,1
	shl     ax,cl
	dec     ax                      ; get one bit per sector in this block
	mov     cache_mask,ax           ; and save the mask

constants_already_valid:
;
;       When we come here, we know that variables like sector_size_bytes
;       and cache_block_sectors are set correctly to their values for the
;       drive for this operation.  The caller's registers have mostly been
;       saved on the stack.  It is time to go to work.

	mov     ax,es:[bx.count]        ; copy count & transfer address to
	mov     d_count,ax              ; local temporary variables

	mov     ax,es:word ptr [bx.trans]
	mov     word ptr d_trans,ax
	mov     ax,es:word ptr [bx.trans+2]
	mov     word ptr d_trans+2,ax

	mov     ax,es:[bx.start]        ; get 16-bit block number
	xor     dx,dx

;       We must look at our device header (original copy) to see
;         if we should support 32 bit sector numbers.

	push    es
	push    bx
	les     bx,dd_header
	test    es:byte ptr [bx.4],2    ; 32-bit capable device?
	pop     bx
	pop     es
	jz      use_16_bit_blocknum

	cmp     dos_3x,0
	je      notdos3x
	cmp     byte ptr es:[bx],24     ;compaq's 24 byte packet?
	jne     use_16_bit_blocknum

	mov     dx,es:[bx.start.2]      ;get second part of 32-bit offset
	jmp     short use_16_bit_blocknum

notdos3x:

	cmp     ax,-1                   ; for 32 bit block numbers, the
	jnz     use_16_bit_blocknum     ;  low 16 should be -1

	mov     dx,es:[bx.start_h]      ; get high part
	mov     ax,es:[bx.start_l]      ; get 32-bit block number

use_16_bit_blocknum:
;       We now have dx:ax == the starting sector for this I/O.  Let's
;       figure out which cache block contains that sector, and what the
;       sector offset within that cache block is

;       cmp     es:byte ptr [bx.1],1            ; drive b:
;       jnz     no_bkpt
;       cmp     ax,0b10h
;       jb      no_bkpt
;
;       int     3
;
;no_bkpt:
	add     ax,cache_align_factor   ; adjustment factor attempts to align
	adc     dx,0                    ;  cache blocks with FAT clusters

	mov     cx,cache_block_shift
	push    ax                      ; save the low bits

;       The following is a fast 32-bit shift right.     It will be used
;       elsewhere in this program without the timing documentation.
;       WARNING: assumes CL <=16!

	mov     di,0FFFFh ;2 clks 3bytes   set up to make a mask
	ror     dx,cl   ;3,       2bytes   low bits of dx set to final bit position
	shr     ax,cl   ;3,       2bytes   low bits of ax set to final bit position
	shr     di,cl   ;3,       2bytes   the mask should just cover low bits in dx
	mov     cx,dx   ;2,       2bytes   save off dx (we still need high bits)
	and     dx,di   ;2,       2bytes   mask off the high bits of dx--dx is is done
	not     di      ;2,       2bytes   invert mask so we can get high bits for ax
	and     cx,di   ;2,       2bytes   mask just covers the high bits
	or      ax,cx   ;2        2bytes   or the high bits into ax--ax is done
			;= 21 clocks total (386),19 bytes

	mov     curblk_l,ax             ; store the cache block number of
	mov     curblk_h,dl             ;  the 1st sector in a temp. variable

	pop     ax                      ; get low bits (modulo)
	mov     ah,byte ptr cache_block_sectors
	dec     ah                      ; generate a mask for secs/blk
	and     al,ah                   ; extract the partial block size
	mov     byte ptr curblk_index,al ; save index within cache block

	cmp     es:[bx.reqfunc],devrd   ; reading?
	jnz     do_writes               ; assume all other functions are writes

	call    cache_reads             ; use a subroutine for the read loop
	jmp     short common_exit

do_writes:
	call    cache_write_data        ;  write it, cache if appropriate

common_exit:
go_back_to_dos:

	pop     si
	pop     di
	pop     bx
	pop     es
	pop     cx                      ; restore 1st tier of saved regs
	pop     dx
	pop     bp

	cmp     in_device_call,0
	jne     handle_reentrancy_exit_near

	mov     ss,save_stack_ss        ;restore stack
	mov     sp,save_stack_sp

dont_restore_stack:
	dec     in_bambi                ; free device_driver resource

	dec     in_device_call          ;keep track of reentrancy

	cmp     commit_all_when_ok,0    ;now is a good time to determine if
	jne     must_commit_all         ;some asynch event (ctrl+alt+delete)
					;requires us to commit all 
continue_returntodos:

	pop     ds                      ; restore caller's ds
	assume  ds:nothing

	ret                             ; back to DOS (or last hooker)
must_commit_all:
	call    write_behind_cache
	jmp     short continue_returntodos

handle_reentrancy_exit_near:
	jmp     handle_reentrancy_exit
our_int endp

;-----------------------------------------------------------------------
;
;       This routine is used when a media_change or an IOCTL forces
;       us to invalidate our information about a given drive so that
;       we are forced to re-read it.
;
reset_drive_stuff       proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	mov     num_valid_buffers,0     ; invalidate supercache
	push    ax                      ;flush the cache and invalidate
	push    bx
	push    cx
	push    dx
	push    bp
	push    si
	push    di
	push    es

	call    commit_all_dirty
	call    flush_queue

	pop     es
	pop     di
	pop     si
	pop     bp
	pop     dx
	pop     cx
	pop     bx
	pop     ax

	ret

reset_drive_stuff       endp

;-----------------------------------------------------------------------
;
;       Come here when we have a read operation on a cached drive.
;       The block number and index within the first block have already
;       been setup by the common entry code.
;
;       The basic idea is that we break the read down into:
;
;          First partial (non-cache-block aligned sectors on front)
;          Full cache blocks
;          Last partial (cache-block aligned, but we don't need it all)

xno_partial_on_front: jmp no_partial_on_front   ; make short branch reach

cache_reads     proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	cmp     byte ptr curblk_index,0 ; partial block at the front?
	jz      xno_partial_on_front    ;  skip if not

	call    check_supercache_match  ; if we're lucky, the data we want
					;  is already in our RAM buffer and
					;  we won't even have to hit XMS

	jnc     no_supercache_in_partial ; Skip if we weren't lucky

;       the block we need part of is already in the cache, copy to
;         user's buffer.  Note:  si has already been setup (pointing
;         to the target block in the supercache) and must not be trashed.

	call    first_partial_setup     ; get sector count, set es:di for move
	push    ax                      ; save # sectors
	mov     ax,curblk_index         ; get offset within block
	mul     sector_size_bytes
	add     si,ax                   ; point to correct source in supercache
	pop     ax
	mul     sector_size_bytes
	mov     cx,ax
	shr     cx,1                    ; convert to words

	mov     ds,lb_seg
	assume  ds:nothing

;       ds:si -> desired sector in supercache (our local RAM buffer)
;       es:di -> user's transfer address
;       cx    =  length of this transfer in words
;       ax    =  length of this transfer in bytes

	rep     movsw                   ; move to user's buffer
	push    cs
	pop     ds
	assume  ds:zseg                 ; restore addressability

	jmp     short next_read_block   ; update transfer, curblk, and continue

;       We're still trying to satisfy our first partial, but it wasn't
;       in the supercache.  Our next best bet is our REAL cache.

no_supercache_in_partial:

	call    check_hit               ; see if we hit on curblk
	jnz     partial_was_a_hit       ;  skip if that block is present

;       the block we need isn't in cache!  We've got to hit the disk,
;         and while we're at it, we'll stick it into the cache for
;         future use.

	mov     cx,1                    ; just get one block
	call    get_curblks_into_cache  ; get the curblk into local buf
;                                       ;  and cache it!
	jnc     frst_prt_rdok           ; done if no disk read error

;       we couldn't read the whole block.  Now we'd better try to
;         read just the part the user wanted directly into his buffer
;         before we decide to bomb out and report an error.

	call    first_partial_setup     ; get es:di -> dma addr, ax=count

	push    ax                      ; save count
	mov     ah,al                   ; get count into ah
	mov     al,byte ptr curblk_index ; get sector within block
	call    read_part_curblk_from_disk
	pop     dx                      ; restore count into dx to preserve
					; error code in ax

	jc      xgot_fatal_error        ; bomb out if fatal disk error

;       lucky us!  the read error was in another part of the
;       block, or, the retry was sufficient to get the data.

	mov     ax,dx                   ; get count just xferred
	mul     sector_size_bytes       ; get amount to bump xfer addr by
	jmp     short next_read_block

xgot_fatal_pop2:                        ; BUG1 - adjust stack & exit if error
	add     sp,4                    ; BUG1 - pop 2 entries off stack
xgot_fatal_error:       jmp     got_fatal_error ; make short branch reach

;       We've got the first partial read and cached.  Now pass it
;       back to the caller.

frst_prt_rdok:

	call    first_partial_setup     ; setup for partial block

	mul     sector_size_bytes       ; get byte count for transfer
	mov     cx,ax
	shr     cx,1                    ; convert to words

	push    ax                      ; preserve the byte count in ax
	mov     ax,curblk_index         ; get starting index
	mul     sector_size_bytes
	lds     si,local_buf
	assume  ds:nothing
	add     si,ax
	rep     movsw                   ; move the sectors
	pop     ax

	push    cs
	pop     ds
	assume  ds:zseg                 ; restore addressability

	jmp     short next_read_block

x_do_last_partial:      jmp     do_last_partial ; bridge for short jumps

;       the block we need part of is already in the cache, copy to
;         user's buffer.

partial_was_a_hit:
	call    first_partial_setup

	push    ax                      ; save # sectors
	mov     ah,byte ptr curblk_index ; get al blocks, starting here

	ifdef   USE_VALID
	push    ax
	call    make_sure_sectors_are_valid     ; if valid_bits not set,
;                                               ;  this routine will read the
;                                               ;  whole block and make sure
;                                               ;  it's all valid.
	jc      xgot_fatal_pop2                 ; BUG1 exit if error reading
	pop     ax

	endif

	mov     bp,cache_element_index
	les     di,d_trans
	call    read_part_cache_block   ; read from cache into user buffer
	pop     ax                      ; restore sector count
	mul     sector_size_bytes

;//////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\///////////////////////////////

;       This is the main READ loop.  We come back here after successfully
;       completing the first partial or any full blocks.  AX has the number
;       of bytes just transferred, and will be added to the transfer address.
;       curblk is also incremented.

next_read_block:
	add     word ptr d_trans,ax     ; update transfer address
	add     curblk_l,1              ; move to next block
	adc     curblk_h,0

;       Now we're block aligned.  Complete any remaining full blocks first.

no_partial_on_front:
	mov     ax,cache_block_sectors
	cmp     ax,d_count              ; do we need a full block?
	ja      x_do_last_partial

	sub     d_count,ax              ; count it down

	call    check_supercache_match  ; did we match in supercache?
	jnc     no_full_block_supercache_hit

;       copy the block from supercache

	les     di,d_trans              ; get user buffer
	mov     cx,cache_block_words    ; move one full cache block
	mov     ds,lb_seg               ; si set by check_supercache_match
	assume  ds:nothing
	rep     movsw                   ; move from supercache
	push    cs
	pop     ds
	assume  ds:zseg                 ; restore addressability

	mov     ax,cache_block_bytes    ; size for updating pointer
	jmp     short next_read_block

no_full_block_supercache_hit:

;       No supercache match on full cache block.  Our next best bet
;       is the REAL cache.

	call    check_hit               ; is curblk in cache?
	jz      full_nohit              ; brif not

	ifdef   USE_VALID
	mov     ax,cache_mask           ; make sure it is valid
	call    make_sure_sectors_are_valid_mask
	jc      zgot_fatal_error        ; BUG1 -- exit if read error!
	endif

	les     di,d_trans              ; get user buffer
	mov     bp,cache_element_index
	call    read_full_cache_block   ;  read the whole cached block
	mov     ax,cache_block_bytes    ; size for updating pointer
	jmp     next_read_block

zgot_fatal_error:                       ; BUG1 -- error exit bridge
	jmp     got_fatal_error         ; BUG1

;       block not in cache.  Figure out how many contiguous nohits
;         there are in the data we need, and read them all, then
;         transfer them all into the cache at once.

full_nohit:
	mov     ax,curblk_l
	mov     dl,curblk_h             ; get last block number

	xor     cx,cx                   ; init block needed count
	mov     bx,d_count              ; we need this many more sectors

count_nohit_loop:
	inc     cx                      ; count blocks needed
	sub     bx,cache_block_sectors  ;  do we need another full block?
	jb      we_know_how_many_nohits

	add     ax,1                    ; point to next block
	adc     dl,0

	push    ax
	push    bx
	push    cx
	push    dx
	call    check_hit_in_regs       ; do we need to read next block?
	pop     dx
	pop     cx
	pop     bx
	pop     ax
	jz      count_nohit_loop        ; just keep looping while no hits


we_know_how_many_nohits:

;       now cx tells us how many blocks we need to read from disk
;           bx is the number of remaining sectors after the contig nohits

;       Okay, then.  Let's jam directly on the user's buffer.

cant_fit_more_in_localbuf:
	les     di,d_trans              ; get user buffer
	push    cx                      ; save number of blks for caching loop
	mov     ax,cx                   ; get count of blocks
	call    read_curblks_from_disk  ; read that sucker
	pop     cx
	jc      got_fatal_error         ; error out!  We NEED those blocks
;                                       ;  to succeed this call!

;       adjust d_count to reflect multiple blocks transferred

	mov     ax,cx                   ; we know this is at least one
	dec     ax
	mul     cache_block_sectors
	sub     d_count,ax

;       Now we've got to copy all of that shit into the cache

	les     di,d_trans              ; point to buffer

	push    es                              ;check if this is a 
	les     si,accessing_swap_file_ptr      ;swapfile access, if so
	cmp     byte ptr es:[si],0              ;dont cache it
	pop     es
	jne     skip_cache_em

cache_em:
	push    cx
	push    di
	xor     cx,cx                   ; none of it is dirty!!!
	ifdef   USE_VALID
	mov     bx,cache_mask           ; all valid
	endif
	call    create_cache_block      ; **** create the block^
	pop     di
	push    di
	call    write_full_cache_block
	add     curblk_l,1
	adc     curblk_h,0
	pop     di
	pop     cx
	add     di,cache_block_bytes
	loop    cache_em

done_cache_em:
	mov     word ptr d_trans,di     ; update d_trans

;       **** Note:  We're branching all of the way back here so
;            we can test for the case where we've done with all
;            of the full blocks.  A potential speed optimization
;            would be to duplicate the test here, and not bother
;            looking up the next block if there is another one.
;            In that case, we already know it is a hit.  The
;            block number would have to be saved above in this
;            case.

	jmp     no_partial_on_front
skip_cache_em:
	add     curblk_l,1
	adc     curblk_h,0
	add     di,cache_block_bytes
	loop    skip_cache_em
	jmp     short   done_cache_em


;       pass read error condition back to caller.

got_fatal_error:
	les     bx,user_reqblk
	mov     es:[bx.reqstat],ax       ; pass error code back to caller
	mov     es:word ptr [bx.count],0 ; no sectors complete
	ret

;       d_count is now less than a full cache block
;         we are cache block aligned

do_last_partial:
	cmp     d_count,0               ; *IS* there a partial?
	jz      reading_done            ;  skip if not

	call    check_supercache_match
	jc      last_partial_moveit     ; just move it from supercache if so

;       last partial not in supercache.  Try regular cache next.

	call    check_hit               ; is last block in cache?
	jz      need_to_read_last_partial

;       Got it in regular cache!

	ifdef   USE_VALID
	mov     ax,d_count              ; ah=0, al=sectors we need
	call    make_sure_sectors_are_valid ; make sure they're valid
	jc      got_fatal_error         ; BUG1 fatal exit if read error
	endif

	mov     ax,d_count              ; starting with zero, read d_count
	les     di,d_trans
	mov     bp,cache_element_index
	call    read_part_cache_block   ; read it from cache
	jmp     short reading_done      ;  done!  exit okay!

;       We've gotta hit the disk.  Maybe we'll do a readahead too.

need_to_read_last_partial:

	call    readahead_check         ; should we readahead?
	mov     cx,1                    ; just read this one block if we're
;                                       ;  not sequential from last time
	jnz     last_partial_no_readahead

	mov     cx,max_valid_buffers    ; read/cache this many blocks if seq.

last_partial_no_readahead:

	call    get_curblks_into_cache  ; get those suckers into cache
	jnc     last_partial_readok     ; done if no read error

;       read error.  We'd better try reading just exactly what the
;         user requested.

	mov     ah,byte ptr d_count     ; get count
	xor     al,al                   ; start reading from front of block
	les     di,d_trans
	call    read_part_curblk_from_disk
	jnc     reading_done            ; done if read ok!!!
	jmp     got_fatal_error         ; fatal error if disk read error

last_partial_readok:
	mov     si,word ptr local_buf   ; we know the data is in the
					;  front of the local_buf

;       We will enter here when we're moving the data out of the
;       supercache.

last_partial_moveit:

	mov     ax,d_count
	mul     sector_size_bytes
	mov     cx,ax                   ; copy this many
	shr     cx,1                    ; convert to words

	les     di,d_trans
	mov     ds,lb_seg
	assume  ds:nothing
	rep     movsw

	push    cs
	pop     ds                      ; restore addressability
	assume  ds:zseg

reading_done:
	mov     ax,curblk_l
	mov     lastblk_l,ax            ; copy curblk to lastblk
	mov     al,curblk_h
	mov     lastblk_h,al

readwrite_done:
	les     bx,user_reqblk
	mov     es:[bx.reqstat],100h    ; normal completion
	ret                             ; no error checking for now


cache_reads     endp

;-----------------------------------------------------------------------
;
;       subroutine to check to see if curblk is just past lastblk;
;         this condition is used during a read of anything besides
;         a first partial, to trigger readahead.  Zero is set
;         when (curblk-1) == lastblk
;
;       ****Note:  For now, we'll also return zero when curblk==lastblk.
;           Depending on the exact exit path taken, lastblk may or may not
;           be updated to point past the end of the previous read.
;
;       trashes ax, cl

readahead_check proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	mov     ax,curblk_l
	mov     cl,curblk_h
	sub     ax,lastblk_l
	sbb     cl,lastblk_h
	and     al,0feh         ; was result 0 or 1?
	or      al,ah
	or      al,cl           ; set zero flag if so
	ret

readahead_check endp

	ifdef   USE_VALID

;-----------------------------------------------------------------------
;
;       make sure that the currently needed sectors of the current
;         block are indeed valid.  If not, we've gotta read the
;         whole block and mark it valid.
;
;       Entry:  ah=first sector we need, al=number of sectors we need
;       Exit:   carry set if we had a read error
;               ax == error code if carry set   BUG1
;
;       Trashes:  all

make_sure_sectors_are_valid     proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	mov     cx,ax                   ; get our counts into cx
	mov     ax,1
	shl     ax,cl                   ; get one bits for each of ours
	dec     ax
	mov     cl,ch                   ; now shift that for starting place
	shl     ax,cl

make_sure_sectors_are_valid     endp    ; fall into similar routine w/ mask

;-----------------------------------------------------------------------
;
;       make sure that the sectors in the currently looked-up cache
;        block whose mask is in ax are valid.  Otherwise, we've got
;        to force the whole damn cache block to be valid.
;
;       Entry:  ax == mask of sectors required from this block
;       Exit:   carry set if we had a read error
;               ax == error code if carry set BUG1
;
;       Trashes:  all

make_sure_sectors_are_valid_mask        proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	log_it  0               ; tracker:  bambi type zero

	mov     cx,valid_mask           ; here is what's valid
	not     cx
	and     cx,ax                   ; were any of our 1 bits zero bits?
	jnz     make_cache_block_valid

	clc                             ; return with no error
	ret

make_cache_block_valid:

	log_it  1               ; tracker:  bambi type one

	mov     num_valid_buffers,0     ; invalidate supercache

	les     di,local_buf
	mov     ax,1                    ; block count
	push    cx                      ; BUG1 -- save mask of needed sectors
	call    read_curblks_from_disk  ; read it into buffer
	pop     cx                      ; BUG1 -- restore mask of needed secs
	jc      make_cache_readerror    ; skip if read error!!!!

	log_it  2               ; tracker:  time stamp reading done

;       Now we've got to copy only the NON-DIRTY sectors into
;       the cache, so that we don't replace data that hasn't yet
;       made it onto the disk.

	mov     cx,dirty_mask
	not     cx                      ; get NON-DIRTY bits
	and     cx,cache_mask

	xor     ah,ah                   ; ah=0 (sector zero)

;       Here we will assume CX != 0.  If all of the bits in
;       that block were DIRTY, they would also have to be VALID
;       and we'd never be here in the first place.

find_one_to_copy:
	inc     ah                      ; count it
	shr     cx,1
	jnc     find_one_to_copy        ; bypass clean block

	dec     ah                      ; this is the sector number
	xor     al,al                   ; count=1+number of contig ones in cl

find_number_to_copy:
	inc     al
	shr     cx,1
	jc      find_number_to_copy     ; loop while we find more ones

	shl     cx,1                    ; put a zero bit back
	push    cx                      ; save mask

	push    ax                      ; save count
	mov     al,ah                   ; get starting sector
	xor     ah,ah
	mul     sector_size_bytes
	les     di,local_buf            ; find location in disk buffer
	add     di,ax
	pop     ax                      ; restore count

	push    ax                      ; save count again
	mov     bp,cache_element_index
	call    write_part_cache_block  ; write into cache
	pop     ax
	pop     cx                      ; restore continuing mask

	log_it  3               ; tracker: xms written

	add     ah,al                   ; update sector number
	or      cx,cx
	jnz     find_one_to_copy

	mov     bx,cache_mask           ; valid == all
	mov     valid_mask,bx
	mov     cx,dirty_mask
	mov     bp,cache_element_index
	call    set_dirtyindex          ; mark it all valid
	clc
	ret                             ; return with carry set if error

;       BUG1 -- BEGIN NEW CODE!!!!!

;        -- the following error exit was totally messed up
;               in the Win 3.1 version of this code, causing
;               potential data loss in this case of bad adjacent blocks.
;       
;        the block we need is only partially valid, but trying
;        the usual technique of making the whole thing valid
;        won't work because there are bad sectors in the block.
;        We must try to read exactly the missing sectors we
;        need.  If this succeeds, we'll stick them in the
;        cache, otherwise, we'll bomb out with a fatal error.

make_cache_readerror:

;       Here is the old, wrong code.

;BUG1   mov     bp,cache_element_index
;BUG1   call    invalidate_element
;BUG1   stc                             ; return error condition, ax == code
;BUG1   ret

	push    cx                      ; save mask of new sectors to read
	xor     ah,ah                   ; ah=0 (sector zero)

;       Here we will assume cx != 0, because it is the mask of
;       sectors we need which are not valid in the cache.

find_one_to_read:
	inc     ah                      ; count it
	shr     cx,1
	jnc     find_one_to_read        ; bypass unneeded block

	dec     ah                      ; this is the sector number
	xor     al,al                   ; count=1+number of contig ones in cx

find_number_to_read:
	inc     al
	shr     cx,1
	jc      find_number_to_read     ; loop while we find more ones

	shl     cx,1                    ; put a zero bit back
	push    cx                      ; save mask

	push    ax                      ; save count
	mov     al,ah                   ; get starting sector
	xor     ah,ah
	mul     sector_size_bytes
	les     di,local_buf            ; find location in disk buffer
	add     di,ax
	pop     ax                      ; restore count

;       Now we have to read those sectors into the buffer.


	push    ax
	xchg    ah,al                           ; swap count and start
	push    es
	push    di
	call    read_part_curblk_from_disk      ; read the sectors we need
	pop     di
	pop     es
	jc      make_sure_fatal_read_error      ; abort if error
	pop     ax                      ; restore start sector and count

	push    ax                      ; save count again
	mov     bp,cache_element_index
	call    write_part_cache_block  ; write into cache
	pop     ax

	pop     cx                      ; restore continuing mask

	log_it  3                       ; tracker: xms written

	add     ah,al                   ; update sector number
	or      cx,cx
	jnz     find_one_to_read

	pop     ax                      ; get mask of sectors we just cached

	or      valid_mask,ax
	mov     bx,valid_mask
	mov     cx,dirty_mask
	mov     bp,cache_element_index
	call    set_dirtyindex          ; mark new sectors valid
	clc
	ret                             ; return no carry, no error

make_sure_fatal_read_error:
	add     sp,6                    ; get rid of count, running mask, and
;                                       ;  overall mask of missing sectors
	stc                             ; set error condition
	ret                             ; and return with error code in ax

;       BUG1 -- end of new code

make_sure_sectors_are_valid_mask        endp

	endif                           ; ifdef USE_VALID

;-----------------------------------------------------------------------
;
;       Now let's do a write function, caching any data that's
;         appropriate.

cache_write_data        proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	test    flags_and_unit,40h      ; should we delay writes?
	jz      write_without_delay
if 1
	push    es                              ;check if this is a 
	les     si,accessing_swap_file_ptr      ;swapfile access, if so
	cmp     byte ptr es:[si],0              ;dont cache it
	pop     es
	jne     write_without_delay
endif
	jmp     short do_delayed_writes

;       Writes are not delayed.  Just make sure it's all in the
;         cache (except for first partial block with no hit), then
;         pass it through to the driver and return the error.

write_without_delay:
	mov     num_valid_buffers,0     ; invalidate supercache
	cmp     byte ptr curblk_index,0 ; partial block at the front?
	jz      wnodel_no_first_partial

;       count is min (cache_block_sectors - curblk_index, d_count)

	mov     ax,cache_block_sectors
	sub     ax,curblk_index         ; how many left in block?
	cmp     ax,d_count              ; do we want that many?
	jb      wnodel_need_em_all      ; skip if so

	mov     ax,d_count              ; only transfer what we need

wnodel_need_em_all:
	push    ax                      ; save the count
	call    check_hit               ; see if we hit on curblk
	pop     ax                      ; restore count into ax!!!!

	jz      wnodel_first_partial_common

;       the partial block is in the cache.  Better copy in our data.

	push    ax                      ; save sector count
	les     di,d_trans              ; get disk transfer address
	mov     ah,byte ptr curblk_index ; get first sector #

;       Note:  al == transfer count

	call    write_part_cache_block  ; copy partial block into cache

	pop     ax                      ; restore sector count

;       come here to update pointers after dealing with first partial
;          block.  al == number of sectors in first partial

wnodel_first_partial_common:
	sub     d_count,ax              ; keep track of count

	mul     sector_size_bytes       ; bump transfer addr by this much

wnodel_write_next_block:
	add     word ptr d_trans,ax     ; update transfer address
	add     curblk_l,1              ; let's move to next block
	adc     curblk_h,0

;       Now we're block aligned.  Complete any full blocks first.

wnodel_no_first_partial:
	mov     ax,cache_block_sectors
	cmp     ax,d_count              ; do we need a full block?
	ja      wnodel_last_partial     ; skip if no more full blocks

	sub     d_count,ax              ; count it down

	call    check_hit               ; is curblk in cache?
	jz      wnodel_full_common

;       We got a hit!  Must update cache!

	les     di,d_trans              ; get user buffer
	call    write_full_cache_block  ; write the whole block

wnodel_full_common:
	mov     ax,cache_block_bytes    ; update pointer
	jmp     wnodel_write_next_block

;       d_count is now less than a full cache block
;         we are cache block aligned

wnodel_last_partial:
	cmp     d_count,0               ; *IS* there a partial?
	jz      wnodel_done             ;  skip if not

	call    check_hit               ; is last partial in cache?
	jz      wnodel_done             ;  skip if not

	les     di,d_trans

	mov     ax,d_count              ; ah=0, al=count
	call    write_part_cache_block  ;  copy that data into cache

;       Now we've updated the cache to reflect our pending write.
;         Go ahead and do the write, and let the device driver
;         return the error code to the caller.

wnodel_done:
	jmp     call_dd_ureqblk         ;  let dd set return code

;       The writes are being delayed.  Just put everything into
;         the cache with dirty bits set.

do_delayed_writes:
	mov     num_valid_buffers,0     ;data may have been dupped in supercache
					;so invalidate supercache 
					;PARADOX BUG FIX 8/4/92 Scottq

	cmp     byte ptr curblk_index,0 ; partial block at the front?

	ifdef   USE_VALID
	jz      no_partial_to_write
	else
	jnz     must_write_partial
	jmp     no_partial_to_write
	endif

;       we need a mask with curblk_index (known non-zero) low bits
;         clear, and 'count' bits set after that, counting from bit zero.
;         This mask gives the bits which we'll have to set in dirty_bits.

must_write_partial:

;       count is min (cache_block_sectors - curblk_index, d_count)

	mov     cx,cache_block_sectors
	sub     cx,curblk_index         ; how many left in block?
	cmp     cx,d_count              ; do we want that many?
	jb      xbsetup_need_em_all     ; skip if so

	mov     cx,d_count              ; only transfer what we need

xbsetup_need_em_all:

	mov     ax,1
	shl     ax,cl                   ; now put in that many one bits
	dec     ax                      ; convert to mask

	push    cx                      ; save the count
	mov     cl,byte ptr curblk_index
	shl     ax,cl                   ; that many zero bits

	push    ax                      ; save the mask

	call    check_hit               ; see if we hit on curblk

	pop     cx                      ; restore mask into cx!!!!
	pop     ax                      ; restore count into ax!!!!
	push    ax                      ; re-save sector count

;       *****************************************************
;       The following big chunk of code varies greatly between
;       the USE_VALID version and otherwise, so it exists in
;       two separate versions rather than riddling the code
;       with ifdefs
;       *****************************************************

	ifdef   USE_VALID       ; for debugging, use other code
	jnz     wr_partial_was_a_hit    ; brif we hit partial

;                                       ; seed mask is in cx

	mov     bx,cx                   ; only the ones we're writing will
;                                       ;  be valid
	call    create_cache_block
	jmp     short wr_partial_common


wr_partial_was_a_hit:
	or      cx,dirty_mask           ; or with previous mask
	ifdef   USE_VALID
	mov     bx,cx                   ; set all dirty_bits valid
	or      bx,valid_mask
	endif
	call    set_dirtyindex

wr_partial_common:

;       Note!  We've set the dirty_bits before we filled up the
;              buffer.  Good thing we know the commit_cache function
;              can't be running concurrently with this mainline code.

;       copy from cache to local_buf

;       ***  Note:  We really only have to copy the part of the block
;                    that we're not replacing.

	mov     bp,cache_element_index  ;setup cache block index
	les     di,d_trans              ;get pointer to user data
	pop     ax                      ;ax is now actual count
	push    ax                      ;must restore later

	sub     d_count,ax              ;update global sector count
	mov     ah,byte ptr curblk_index;setup for next call

	call    write_part_cache_block  ;write data to xms

	mov     num_valid_buffers,0     ;data may have been dupped in supercache
					;so invalidate supercache
	pop     ax                      ;restore count
	mul     sector_size_bytes       ;adjust to bytes


;       go write next block

	else                    ; ndef USE_VALID

	jnz     wr_partial_was_a_hit    ; brif we hit partial

;                                       ; seed mask is in cx

;       This non-USE_VALID code path may still be used during
;       debugging, so it still has some USE_VALID logic in it

	ifdef   USE_VALID
	mov     bx,cache_mask           ; all valid for now
	endif
	call    create_cache_block      ;  filling up local_buf

;       NOTE:  We really only need to read the first part of the block

	les     di,local_buf
	mov     ax,1
	mov     num_valid_buffers,0     ; invalidate local_buf
	call    read_curblks_from_disk  ; read it into buffer
	jnc     wr_first_partial_common

;       Shit!  read error.  Better invalidate block & bomb!

	call    invalidate_cache_block
	pop     ax                      ; get sector count to write

	push    ax                      ; resave count
	mov     ah,al                   ; count into ah
	mov     al,byte ptr curblk_index        ; get starting sector
	les     di,d_trans
	call    write_part_curblk_to_disk
	pop     dx                      ; restore sector count
	jnc     write_first_partial_recover_ok

	jmp     got_fatal_error

write_first_partial_recover_ok:
	mov     ax,dx                   ; get number of sectors just written
	mul     sector_size_bytes       ; get amount to increase d_trans by
	jmp     short write_next_block

wr_partial_was_a_hit:
	or      cx,dirty_mask           ; or with previous mask
	ifdef   USE_VALID
	mov     bx,cx                   ; set all dirty_bits valid
	or      bx,valid_mask
	endif
	call    set_dirtyindex

;       Note!  We've set the dirty_bits before we filled up the
;              buffer.  Good thing we know the commit_cache function
;              can't be running concurrently with this mainline code.

;       copy from cache to local_buf

;       ***  Note:  We really only have to copy the part of the block
;                    that we're not replacing.

	mov     bp,cache_element_index  ;setup cache block index
	les     di,d_trans              ;get pointer to user data
	pop     ax                      ;ax is now actual count
	push    ax                      ;must restore later

	sub     d_count,ax              ;update global sector count
	mov     ah,byte ptr curblk_index;setup for next call

	call    write_part_cache_block  ;write data to xms

	mov     num_valid_buffers,0     ;data may have been dupped in supercache
					;so invalidate supercache
	pop     ax                      ;restore count
	mul     sector_size_bytes       ;adjust to bytes

	ifdef   USE_VALID
	jmp     write_next_block        ;continue...
	else
	jmp     short write_next_block
	endif

wr_first_partial_common:

	mov     ax,curblk_index         ; get starting index
	mul     sector_size_bytes
	les     di,local_buf
	add     di,ax                   ; point to destination in buffer
	pop     ax                      ; get the actual transfer count
	sub     d_count,ax              ; keep track of count
	mul     sector_size_bytes
	mov     cx,ax
	shr     cx,1                    ; convert count to words

	lds     si,d_trans              ; get disk transfer address
	assume  ds:nothing
	rep     movsw                   ; move the data

	push    cs
	pop     ds
	assume  ds:zseg                 ; restore addressability

	push    ax                      ; save byte count for update

	mov     ax,1                    ; make supercache valid
	call    set_supercache_valid    ;  with one block (curblk)

	les     di,local_buf
	call    write_full_cache_block  ; write that pig thru the cache
	pop     ax                      ; restore byte count

;       go write next block

	endif                           ; ifdef USE_VALID

write_next_block:
	add     word ptr d_trans,ax     ; update transfer address

	add     curblk_l,1              ; let's move to next block
	adc     curblk_h,0

;       Now we're block aligned.  Complete any full blocks first.

no_partial_to_write:
	mov     ax,cache_block_sectors
	cmp     ax,d_count              ; do we need a full block?
	ja      write_last_partial      ; skip if no more full blocks

	sub     d_count,ax              ; count it down

	call    check_hit               ; is curblk in cache?
	jnz     wr_full_was_a_hit       ;  skip if that block is present

	mov     cx,cache_mask           ; fully dirty
	ifdef   USE_VALID
	mov     bx,cx                   ;  and fully valid
	endif
	call    create_cache_block      ;  filling up local_buf
	jmp     short wr_full_common

wr_full_was_a_hit:
	mov     cx,cache_mask           ; mark it fully dirty
	ifdef   USE_VALID
	mov     bx,cx                   ;  and fully valid
	or      bx,valid_mask
	endif
	call    set_dirtyindex

;       okay.  copy that puppy into cache.

wr_full_common:
	les     di,d_trans              ; get user buffer
	call    write_full_cache_block  ; write the whole block
	mov     ax,cache_block_bytes    ; update pointer
	jmp     write_next_block

;       d_count is now less than a full cache block
;         we are cache block aligned

write_last_partial:
	cmp     d_count,0               ; *IS* there a partial?
	jnz     write_last_partial_not_done

	jmp     short writing_done      ;  skip if not

write_last_partial_not_done:

;       figure out our mask bits

	mov     cl,byte ptr d_count
	mov     ax,1
	shl     ax,cl
	dec     ax                      ; convert to mask

	push    ax
	call    check_hit               ; is last partial in cache?
	pop     cx                      ; get new mask bits into cx

;       *********************************************
;       Here is another big block which is different for USE_VALID
;       *********************************************

	ifdef   USE_VALID

	jnz     wr_last_partial_was_a_hit ;  skip if that block is present

	mov     bx,cx                   ; only our writing bits are valid
	call    create_cache_block      ;  filling up local_buf
	jmp     short wr_last_partial_common

wr_last_partial_was_a_hit:
	or      cx,dirty_mask           ; or with previous
	mov     bx,cx                   ; valid=dirty
	or      bx,valid_mask
	call    set_dirtyindex

wr_last_partial_common:

	mov     num_valid_buffers,0     ; invalidate super-cache just in case
	mov     bp,cache_element_index  ;get block index
	les     di,d_trans              ;get pointer to user data

	mov     ax,d_count              ;get final count of last write
	call    write_part_cache_block  ;write data to cache in xms



	else                            ;  ndef USE_VALID

	jnz     wr_last_partial_was_a_hit ;  skip if that block is present

;       ifdef   USE_VALID
;       mov     bx,cache_mask           ; the whole thing is valid
;       endif
	call    create_cache_block      ;  filling up local_buf

;       Note:  We really don't need to read the whole block, only the
;               last part of it.

	les     di,local_buf
	mov     ax,1
	mov     num_valid_buffers,0     ; invalidate supercache
	call    read_curblks_from_disk  ; read it into buffer
	jnc     wr_last_partial_common
	call    invalidate_cache_block

;       Now we should attempt to write the sectors directly to the disk.
;         Only if this fails will we actually report an error.

	les     di,d_trans
	mov     ah,byte ptr d_count     ; count of sectors to write
	xor     al,al                   ; start from front of block
	call    write_part_curblk_to_disk

;       Note:  this may have to be split between the two different
;               places that enter this code path.

	jnc     writing_done            ; done if no error writing

	jmp     got_fatal_error

wr_last_partial_was_a_hit:
	or      cx,dirty_mask           ; or with previous
;       ifdef   USE_VALID
;       mov     bx,cx                   ; valid=dirty
;       or      bx,valid_mask
;       endif
	call    set_dirtyindex

;       copy from cache to local_buf

;           Note:  we really only need to copy the part we aren't filling in.

	mov     bp,cache_element_index  ;get block index
	les     di,d_trans              ;get pointer to user data

	mov     ax,d_count              ;get final count of last write
	;;;     ah=0!
	call    write_part_cache_block  ;write data to cache in xms

	mov     num_valid_buffers,0     ;be sure super-cache is consitent

	ifdef   USE_VALID
	jmp     writing_done            ;outtahere
	else
	jmp     short writing_done      ;outtahere
	endif

wr_last_partial_common:
	les     di,local_buf

	mov     ax,d_count              ; get sectors in last partial
	mul     sector_size_bytes
	mov     cx,ax
	shr     cx,1                    ; convert count to words

	lds     si,d_trans
	assume  ds:nothing
	rep     movsw                   ; move the data
	push    cs
	pop     ds
	assume  ds:zseg                 ; restore addressability

	mov     ax,1
	call    set_supercache_valid    ; mark the supercache as valid

	les     di,local_buf
	call    write_full_cache_block  ; write that pig thru the cache

	endif                           ; ifdef USE_VALID

writing_done:
	jmp     readwrite_done          ; no errors

cache_write_data        endp

;-----------------------------------------------------------------------
;
;       the partial block we need isn't in the cache.  Read the block,
;          and possibly the blocks after it into the cache.  Return
;          as many blocks as can be read successfully (not marked bad,
;          not hits, not read error) in the supercache.  Return carry
;          if we couldn't even read the first block.
;
;       entry:  cx == number of blocks to read & cache.  Those blocks
;               will be left in local_buf on return.
;
;       returns carry set if error on first block
;
;       If count > 1, that means we're doing a readahead.  We haven't
;         actually checked to see if any of that is in the cache.

get_curblks_into_cache  proc    near
	assume  cs:zseg,ds:zseg,es:nothing


	push    cx                      ; save original count
	mov     ax,curblk_l

	mov     dl,curblk_h             ; get last curblk into dl:ax
	mov     dh,original_unit        ; and original_unit into dh
	jmp     short skip_hitcheck_on_1st      ; we already know first block
;                                               ; is a nohit or we couldn't be
;                                               ; here now.


get_curblk_ckhit_loop:
	push    ax
	push    cx
	push    dx                      ; save blocknum & count

	call    check_hit_in_regs       ; is it a hit?

	pop     dx
	pop     cx
	pop     ax
	jnz     get_curblk_ckhit_exit   ; we got a readahead stopper!

skip_hitcheck_on_1st:

	push    cx
	mov     cx,num_bad_blocks
	jcxz    not_bad_block           ; skip if no bads exist

	push    cs
	pop     es      
	mov     di,offset bad_blocks    ; point to list of known baddies
continue_bad_check:
	repnz   scasw                   ; see if this is one.
	jz      might_be_bad            ; OOOPS.  might be bad.  go check rest

;       this block is a candidate for caching.  Loop.

not_bad_block:
	pop     cx
	add     ax,1
	adc     dl,0                    ; adjust block number

	loop    get_curblk_ckhit_loop
	pop     cx                      ; restore the original count

	jmp     short get_curblks_no_rdahead ; go ahead and read the whole thing

;       the low 16 bits matched that of a known bad block.  Let's
;       see if the high 16 do, too.

might_be_bad:
       ;        int 1
	cmp     dx,[di].bad_drives-bad_blocks-2
	jnz     continue_bad_check

	pop     cx                      ; get our loop count from stack

;       fall into the same logic that deals with cache hits.  Stop
;         the read before we get here!

;       we got a hit.  Now we have to figure out how many blocks we
;         can read safely before the hit.

get_curblk_ckhit_exit:
	pop     ax                      ; get original block count
	sub     ax,cx                   ; truncate number past first nohit
	mov     cx,ax
	jcxz    cant_read_any_blocks    ; done if first block marked bad

get_curblks_no_rdahead:
	mov     num_valid_buffers,0     ; invalidate old supercache

	mov     di,offset blockids
	push    cx                      ; save count

	mov     ax,curblk_l
	mov     dl,curblk_h
	mov     dh,original_unit        ; merge unit code into cookie

if 1
	push    es                              ;check if this is a 
	les     si,accessing_swap_file_ptr      ;swapfile access, if so
	cmp     byte ptr es:[si],0              ;dont cache it
	pop     es
	jne     dont_add_to_cache
endif

;       now loop through and create cache blocks for everything we're
;          reading.

get_curblks_00:
	push    cx
	push    dx
	push    ax
	push    di
	xor     cx,cx                   ; create clean cache block before
	ifdef   USE_VALID
	mov     bx,cache_mask           ; all valid, please
	mov     valid_mask,bx           ; set existing valid mask
	endif
	call    queue_element
	pop     di
	pop     ax
	pop     dx
	pop     cx

	add     ax,1
	adc     dl,0                    ; update block number

	mov     word ptr [di],bp        ; save cache block number
	add     di,2
	loop    get_curblks_00


	les     di,local_buf
	pop     ax                      ; get block count
	push    ax                      ;  and re-save on stack
	call    read_curblks_from_disk  ; read them into buffer
	pop     cx                      ; restore block count
	jc      get_curblks_readerror   ; skip if read error!!!!

	mov     ax,cx                   ; get the block count
	call    set_supercache_valid    ;  and mark it valid
	les     di,local_buf
	mov     si,offset blockids
copy_localbuf_into_cache:
	push    si
	push    di
	push    cx
	mov     cx,cache_block_words
	mov     bp,word ptr [si]        ; get cache block number
	xor     si,si                   ; do entire block


	call    buffer_to_cache
	pop     cx
	pop     di
	pop     si

	add     si,2
	add     di,cache_block_bytes    ; update dma pointer
	loop    copy_localbuf_into_cache

	clc
	ret                             ; no errors

cant_read_any_blocks:
	stc                             ; first block was marked bad
	ret

dont_add_to_cache:
	pop     ax
	push    ax
	les     di,local_buf
	call    read_curblks_from_disk  ; read them into buffer
	pop     ax
	jc      mark_curblk_bad

	call    set_supercache_valid    ;  and mark it valid
	clc
	ret                             ; no errors

get_curblks_readerror:
	mov     di,offset blockids

get_curblks_invalidate:
	mov     bp,word ptr [di]
	add     di,2
	push    di
	push    cx
	call    invalidate_element
	pop     cx
	pop     di
	loop    get_curblks_invalidate


;       exit through here if we got a read error.  Actually, we should
;       try to figure out which block was bad, but for now, we'll just
;       assume it was the first one.  Eventually, the problem block
;       will be marked.

mark_curblk_bad:
       ;        int 1
	mov     di,next_bad_entry_ptr
	mov     dx,curblk_l
	mov     bad_blocks[di],dx
	mov     dl,curblk_h
	mov     dh,original_unit
	mov     bad_drives[di],dx

	add     di,2                    ; point to next entry
	cmp     di,MAXBADS*2
	jb      mark_curblk_bad_1
	xor     di,di                   ; next one back to zero
mark_curblk_bad_1:

	mov     next_bad_entry_ptr,di   ; update pointer for next bad sector

	cmp     num_bad_blocks,MAXBADS
	jae     no_incr_numbads
	inc     num_bad_blocks
no_incr_numbads:

	stc                             ; return error condition, ax == code
	ret

get_curblks_into_cache  endp

;-----------------------------------------------------------------------
;
;       this routine is called when we can guarantee that the information
;         in the local_buf is one or more valid cache blocks.  This may
;         save us an XMS transaction if we need that data again soon.
;
;       call with ax == number of valid blocks in local_buf
;                       curblk_l, curblk_h == cache block of first block
;                       original_unit         == the drive it is from

set_supercache_valid    proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	mov     num_valid_buffers,ax
	mov     ax,curblk_l
	mov     bufferblk_l,ax
	mov     al,curblk_h
	mov     ah,original_unit
	mov     bufferblk_h,ax
	ret

set_supercache_valid    endp

;-----------------------------------------------------------------------
;
;       check to see if curblk is in the supercache.  If so, return
;          lb_seg:si pointing to it, ax == number of subsequent blocks present.
;          if not in supercache, return carry false.

check_supercache_match  proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	mov     al,curblk_h
	mov     ah,original_unit
	sub     ax,bufferblk_h
	clc
	jnz     check_supercache_exit   ; exit w/ carry false if no high match

	mov     ax,curblk_l
	sub     ax,bufferblk_l          ; see if we are present
	cmp     ax,num_valid_buffers
	jnc     check_supercache_exit

	push    ax                      ; save number we need to skip
	mul     cache_block_bytes
	mov     si,word ptr local_buf
	add     si,ax                   ; point to curblk in local_buf
	pop     ax
	sub     ax,num_valid_buffers
	neg     ax                      ; get ax == number of blocks present
;                                       ;  at and past curblk
	stc                             ; set carry to indicate success!
check_supercache_exit:
	ret

check_supercache_match  endp

;-----------------------------------------------------------------------
;
;       this routine calculates the number of blocks which can be
;         transferred in the starting partial block.  This count
;         is returned in ax, and is automatically subtracted
;         from d_count.  Also, es:di is pre-loaded to point to
;         the transfer address.
;
;       Preserves:  MUST PRESERVE SI!!

first_partial_setup     proc    near
	assume  cs:zseg,ds:zseg,es:nothing

;       count is min (cache_block_sectors - curblk_index, d_count)

	mov     ax,cache_block_sectors
	sub     ax,curblk_index         ; how many left in block?
	cmp     ax,d_count              ; do we want that many?
	jb      pbsetup_need_em_all     ; skip if so

	mov     ax,d_count              ; only transfer what we need

pbsetup_need_em_all:
	sub     d_count,ax              ; keep track of count
	les     di,d_trans              ; get disk transfer address
	ret

first_partial_setup     endp

;-----------------------------------------------------------------------
;
;       see if the block at curblk is in the cache.
;         alternate entry has block number in dl:ax.
;
;       If found, return:  BP == cache block number
;                          Zero flag clear
;
;       If not found, return: BP == 0ffffh
;                             zero flag true
check_hit       proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	mov     ax,curblk_l
	mov     dl,curblk_h

check_hit_in_regs:
	mov     dh,original_unit        ; merge unit code into cookie

	call    fast_lookup
	mov     dirty_mask,cx           ; save mask of hit element
	ifdef   USE_VALID
	mov     valid_mask,ax           ; save valid mask of hit element
	endif
	cmp     bp,0ffffh
	jz      nohit_count

	add     hit_l,1
	adc     hit_h,0

;       Move that queue entry to head of chain

	;call   bring_queue_entry_to_head
	mov     cache_element_index,bp

	cmp     bp,0ffffh               ; adjust zero flag again
	ret

nohit_count:

	add     nohit_l,1
	adc     nohit_h,0
	mov     cache_element_index,bp
	cmp     bp,0ffffh
	ret

check_hit       endp

;-----------------------------------------------------------------------
;
;       NOTE:  We may get called back at dirty_write here!  This
;               will trash our local_buf!!!!  Be aware!
;
;       Regs:  Input -- CX == initial dirty_bits value
;                       BX == initial valid_bits value (if enabled)

create_cache_block      proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	mov     ax,curblk_l
	mov     dl,curblk_h
	mov     dh,original_unit        ; merge unit code into cookie
	ifdef   USE_VALID
	mov     valid_mask,bx
	endif
	mov     dirty_mask,cx
	call    queue_element
	mov     cache_element_index,bp
	ret

create_cache_block      endp

;-----------------------------------------------------------------------
;
;       this entry is called when we created a cache block and
;         were then unable to fill it completely.

invalidate_cache_block  proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	mov     bp,cache_element_index
	jmp     invalidate_element

invalidate_cache_block  endp

;-----------------------------------------------------------------------
;
;       This puppy reads the cache block (from bp) into memory
;         at es:di.

read_full_cache_block   proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	mov     cx,cache_block_words
	xor     si,si           ; do entire block
	jmp     short read_cache_common

read_full_cache_block   endp

;-----------------------------------------------------------------------
;
;       call with ah=index, al=count

read_part_cache_block   proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	call    mash_for_xms

read_cache_common:
	jmp     cache_to_buffer

read_part_cache_block   endp

;-----------------------------------------------------------------------
;

;       This is similar to read_*_cache_block above except that
;         it uses the block number in cache_element_index instead
;         of bp.

write_full_cache_block  proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	mov     cx,cache_block_words
	xor     si,si           ; do entire block
	jmp     short write_cache_common

write_full_cache_block  endp

;-----------------------------------------------------------------------
;
;       called with al = count to write, ah = starting index

write_part_cache_block  proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	call    mash_for_xms

write_cache_common:
	mov     bp,cache_element_index
	jmp     buffer_to_cache

write_part_cache_block  endp

;-----------------------------------------------------------------------
;
;       call this for partial cache reads/writes.  Calculates
;         cx (word count) and si (index into block) based on
;         ah=sector index, al=sector count

mash_for_xms    proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	push    ax              ; save first sector
	xor     ah,ah
	mul     sector_size_bytes
	mov     cx,ax           ; count for transfer
	shr     cx,1            ; convert to words

	pop     ax              ; restore first sector
	mov     al,ah
	xor     ah,ah
	mul     sector_size_bytes       ; in dx:ax
	mov     si,ax           ; pass to cache manager in dx:si
	ret

mash_for_xms    endp

;-----------------------------------------------------------------------
;
;       this function reads blocks at curblk into memory at
;         es:di.  curblk == 0 is a special case where only
;         a partial read may be needed, depending on the
;         cache_align_factor.
;
;       number of blocks to read is in ax
;
;       returns ax= device driver's return code.
;         carry set if error condition occurred.

read_curblks_from_disk  proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	mul     cache_block_sectors
	mov     our_trans_off,di
	mov     our_trans_seg,es
	mov     our_count,ax

	mov     ax,curblk_l
	mov     dx,word ptr curblk_h
	mov     cx,cache_block_shift

	;The following is a fast 32-bit shift left.  
	;WARNING: assumes CL <=16! 
	;BUG BUG: duped below!

	mov di,0FFFFh           ;      set up to make a mask
	rol ax,cl               ;      
	shl dx,cl               ;      
	shl di,cl               ;      
	mov cx,ax               ;      
	and ax,di               ;      
	not di                  ;      
	and cx,di               ;      
	or  dx,cx               ;      

	sub     ax,cache_align_factor
	sbb     dx,0
	jnb     read_curblk_go

;       we're doing block zero and have to do a partial read

	mov     ax,cache_align_factor
	sub     our_count,ax

	mul     sector_size_bytes
	add     our_trans_off,ax

;       *****  Alternate entry point from lookup_device
;               routine.  Notice that it has already set up the
;               count and transfer address fields.

read_sector_zero:
	xor     ax,ax
	xor     dx,dx
read_curblk_go:

	les     bx,dd_header            ; point to device header
	call    set_start_sector        ; store dx:ax as start sector

	mov     byte ptr rblk_op,devrd
	les     bx,loc_reqblk

	call    call_dd                 ; call device driver

	les     bx,loc_reqblk           ; be sure packet is int es:bx
	mov     ax,es:[bx].reqstat      ; fetch up error code
	rol     ah,1                    ; get ax bit 15 into carry
	ror     ah,1                    ; without trashing ax
	ret

read_curblks_from_disk  endp

;-----------------------------------------------------------------------
;
;       this function reads a fraction of a block at curblk
;         into memory at es:di.  AL=first sector, AH=number of secs.
;         Return carry if disk read error, ax=error code from dev. drvr.

read_part_curblk_from_disk      proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	mov     byte ptr rblk_op,devrd
	jmp     short read_write_part_curblk

read_part_curblk_from_disk      endp

;-----------------------------------------------------------------------
;
;       this function writes a fraction of a block at curblk from
;         memory at es:di.  AL=first sector, AH=number of secs.
;         Return carry if disk write error, ax=error code from dev. drvr.

write_part_curblk_to_disk       proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	mov     byte ptr rblk_op,devwrt

read_write_part_curblk:
	mov     our_trans_off,di
	mov     our_trans_seg,es
	mov     byte ptr our_count,ah
	mov     byte ptr our_count+1,0

	push    ax                      ; save first sector to read

	mov     ax,curblk_l
	mov     dx,word ptr curblk_h
	mov     cx,cache_block_shift

	;The following is a fast 32-bit shift left.  
	;WARNING: assumes CL <=16! 
	;BUG BUG: dupped above!

	mov di,0FFFFh           ;      set up to make a mask
	rol ax,cl               ;      
	shl dx,cl               ;      
	shl di,cl               ;      
	mov cx,ax               ;      
	and ax,di               ;      
	not di                  ;      
	and cx,di               ;      
	or  dx,cx               ;      

	sub     ax,cache_align_factor
	sbb     dx,0

;       We don't actually have to deal with block zero here, because
;         even if our dx:ax sector number goes negative here, the sector
;         within the block will get added back in.  Since that reflects
;         an actual calling program read/write request, we can assume
;         that it will yield a valid sector number (ie: >= 0)

	pop     cx                      ; get the starting sector number
	xor     ch,ch                   ;  within curblk
	add     ax,cx
	adc     dx,0

	les     bx,dd_header            ; point to device header
	call    set_start_sector        ; store dx:ax as starting sector

	les     bx,loc_reqblk
	call    call_dd                 ; call device driver

	les     bx,loc_reqblk           ; be sure packet is int es:bx
	mov     ax,es:[bx].reqstat      ; fetch up error code
	rol     ah,1                    ; get ax bit 15 into carry
	ror     ah,1                    ; without trashing ax
	ret

write_part_curblk_to_disk       endp


;-----------------------------------------------------------------------
;
;       This function stores dx:ax into our local DD request packet
;       as the starting sector.  This is more complicated than it
;       sounds, because we have to take into account the DOS version
;       and whether or not said device supports 32-bit block numbers.

set_start_sector        proc    near
	assume  cs:zseg,ds:zseg,es:nothing

;       ALWAYS store the block into start_l and start_h, in case
;       some errant device driver picks them up there.

	mov     our_startl,ax
	mov     our_starth,dx

	cmp     dos_3x,0        ; are we running on dos 3.x?
	jnz     ss_dos30        ; brif so

;       If we're running DOS 4.0 or later, we'll always try to pass
;       block numbers in 32-bit mode if the driver supports it, since
;       this is (??) the way those versions of DOS work.  Compaq 3.31,
;       on the other hand, will CHOKE if you send it a 32-bit block
;       number for a sector which could otherwise be handled with 16 bits.

	test    es:byte ptr [bx.4],2    ; see if it is 32-bit capable
	jz      ss_use_16

ss_use_32:
	mov     our_start,-1

	ret

ss_dos30:
ss_use_16:
	mov     our_start+2,dx

	mov     our_start,ax

	ret

	
	
set_start_sector        endp

;-----------------------------------------------------------------------
;
;       get actual unit code and physical device header for logical
;         unit in al
;
;       return remapped unit code in al, device header in bp:dx
;         return cx = align factor and sector size code, for use
;          only by dirty_write since it is potentially invalid
;          at mainline entry.

lookup_device   proc    near
	assume  cs:zseg,ds:zseg,es:nothing
	push    bx                      ;note: we use bx instead of
					;bp since bp will need to have
					;a DS override 4 times=8 clocks,4 bytes
					;but push/pop bx = 4 clocks,2 bytes

	xor     ah,ah                   ; extend to 16 bits
	mov     bx,ax                   ; snag the byte field
	mov     al,byte ptr real_cache_units[bx]

	shl     bx,1                    ; index word field
	mov     cx,secsize_and_align_info[bx]
	shl     bx,1                    ; snag dword field
	mov     dx,word ptr real_dd_headers[bx]
	mov     bp,word ptr real_dd_headers[bx+2]       
	pop     bx

	ret

lookup_device   endp

;-----------------------------------------------------------------------
;-----------------------------------------------------------------------
;
;       get the sector size and align factor for the drive in al.
;
;       Must preserve es:bx

get_drive_info  proc    near
	assume  cs:zseg,ds:zseg,es:nothing

	xor     ah,ah                   ; extend to 16 bits
	mov     bp,ax
	shl     bp,1                    ; index word field

	mov     cx,secsize_and_align_info[bp]
	cmp     ch,0ffh
	jz      gotta_set_secsize_and_align

	ret

;       Now we've got to go hit the disk to get sector zero.
;         We can safely assume we're not called from dirty_write
;         because no sector should ever become dirty other than
;         through the main bambi code, which guarantees that said
;         drive has been accessed (logged in).

gotta_set_secsize_and_align:
	push    es
	push    bx

	push    bp                      ; save the word index

;       to calculate the align factor, find the sector address of a
;         cluster -- (123h for my 130Mb hard drive, 21h for 1.44M flop).
;         mask off low bits under cluster mask, subtract from cluster
;         size.  This means 15 for 1.44M, 13 for my 130M drive c:
;
;       My 110M system has a data field starting at sector 1bfh, so the
;         tuning constant is 1.

	mov     num_valid_buffers,0     ; invalidate super-cache
	les     di,local_buf            ; get address of temp buffer
	mov     our_trans_off,di
	mov     our_trans_seg,es
	mov     our_count,1
	
	call    read_sector_zero        ; read bpb
	les     bx,local_buf            ; point back to block we read
	jc      use_defaults

	push    es
	push    bx
	call    verify_bpb
	pop     bx
	pop     es
	jc      use_defaults

continue_info:
	mov     ax,cache_block_bytes
	xor     dx,dx
	div     es:word ptr [bx].0bh    ; get secs/cache block
	push    ax                      ; and save on stack

	mov     al,es:byte ptr [bx].10h ; get nfats
	xor     ah,ah
	mul     word ptr es:[bx].16h    ; times fatsize
	add     ax,es:[bx].0eh          ; add in reserved sectors

;       now we have to add in the number of directory sectors

	mov     bx,es:[bx].11h          ; get number of directory entries
	mov     cl,4                    ; divide by 16 (512 bytepersec/32)
	shr     bx,cl
	add     ax,bx                   ; this is address of first cluster

	neg     ax                      ; get negative of that

	pop     cx                      ; get cache block sectors
	dec     cl                      ; convert count to mask
	and     al,cl                   ; al == align factor
	inc     cl

;       we still need the log2(secs/block) value for the configuration array.

	mov     ah,al
	mov     al,-1

calc_log2_secs_per_block:
	inc     al
	shr     cl,1
	jnz     calc_log2_secs_per_block

	mov     cx,ax

	pop     bx                      ; get index for storing drive config
	mov     secsize_and_align_info[bx],cx

	pop     bx
	pop     es
	ret
use_defaults:
	mov     word ptr es:[di].0bh,200h
	mov     byte ptr es:[di].10h,2
	mov     word ptr es:[di].16,9
	mov     word ptr es:[di].0eh,1
	mov     word ptr es:[di].11h,0
	jmp     short continue_info


get_drive_info  endp

validate_secsize:
	mov     ax,es:word ptr [bx].0bh ; get sector size
	cmp     ax,64                   ; cannot be valid if sector size 
	jb      invalidbootsec          ;  is <64 bytes
vs1:
	shl     ax,1
	jnc     vs1
	jz      check_bpb_mediabyte
	jmp     short   invalidbootsec  

verify_bpb proc near

		; put a sanity check for the boot sector in here to detect
		; boot sectors that do not have valid bpbs. we examine the
		; first two bytes - they must contain a long jump (69h) or a
		; short jump (ebh) followed by a nop (90h), or a short jump
		; (e9h). if this test is passed, we further check by examining
		; the signature at the end of the boot sector for the word
		; aa55h. if the signature is not present, we examine the media
		; descriptor byte to see if it is valid. for dos 3.3, this
		; logic is modified a little bit. we are not going to check
		; signature. instead we are going to sanity check the media
		; byte in bpb regardless of the validity of signature. this is
		; to save the already developed commercial products that have
		; good jump instruction and signature but with the false bpb
		; informations
; that will crash the diskette drive operation. (for example, symphony diskette).

	 cmp    byte ptr es:[bx],0      ; might be bogus Corel and Cronies?
	 je     validate_secsize

	 cmp    byte ptr es:[bx],069h   ; is it a direct jump?
	 je     check_bpb_mediabyte             ; don't need to find a nop
	 cmp    byte ptr es:[bx],0e9h   ; dos 2.0 jump?
	 je     check_bpb_mediabyte             ; no need for nop
	 cmp    byte ptr es:[bx],0ebh   ; how about a short jump.
	 jne    invalidbootsec
	 cmp    byte ptr es:[bx]+2,090h ; is next one a nop?
	 jne    invalidbootsec

; check for non-ibm disks which do not have the signature aa55 at the
; end of the boot sector, but still have a valid boot sector. this is done
; by examining the media descriptor in the boot sector.

check_bpb_mediabyte:

	mov     al,es:[bx].EXT_BOOT_BPB.BPB_MEDIADESCRIPTOR
	 and     al,0f0h
	 cmp     al,0f0h                ; allow for strange media
	 jnz     invalidbootsec

; there were some (apparently a lot of them) diskettes that had been formatted
; under dos 3.1 and earlier versions which have invalid bpbs in their boot
; sectors. these are specifically diskettes that were formatted in drives
; with one head, or whose side 0 was bad. these contain bpbs in the boot
; sector that have the sec/clus field set to 2 instead of 1, as is standard
; in dos. in order to support them, we have to introduce a "hack" that will
; help our build bpb routine to recognise these specific cases, and to
; set up out copy of the bpb accordingly.
; we do this by checking to see if the boot sector is off a diskette that
; is single-sided and is a pre-dos 3.20 diskette. if it is, we set the
; sec/clus field to 1. if not, we carry on as normal.

checksinglesided:
	mov     al,es:[bx].EXT_BOOT_BPB.BPB_MEDIADESCRIPTOR
	cmp     al, 0f0h        ; is it one of the new diskette type?
	je      gooddsk         ; new disks are supported only after 3.2

	test    al,0001h        ; is low bit set? - indicates double sided
	jnz     gooddsk

	cmp     word ptr es:[bx+8],"." shl 8 + "3"
	jnz     mustbeearlier
	cmp     byte ptr es:[bx+10],"2"
	jae     gooddsk

; we must have a pre-3.20 diskette. set the sec/clus field to 1

mustbeearlier:
	mov     es:[bx].EXT_BOOT_BPB.BPB_SECTORSPERCLUSTER,1
	jmp     short gooddsk


	 inc     bx                         ; indicate that boot sector invalid
gooddsk:                                    ; carry already reset
	 clc
	 ret

invalidbootsec:
	 stc
	 ret

verify_bpb endp

if 0
hard_error_hang:
	mov     ax,' '-'A'      ;just leave drive letter blank
	call    warning_pop_up
	jmp short hard_error_hang
endif

PUBLIC resident_initialization
resident_initialization:
	
	push    cs                      ;transient data (stack) gets blown
	pop     ss                      ;away in initqueue, so we have to
;;;hack hack!!
;;;386max's Qcache does an int 13 on TSR, so our resident stack will be
;;;swapped.  We set the temp stack here to the middle of the stack
;;;assuming that the int13 will not use too much stack. We do this
;;;since the stack must be resident, but we dont want to wast space.

	mov     sp,offset cs:temp_res_stack     ;make sure our stack is safe

	mov     cx,cs:number_of_cache_elements
	mov     bx,cs:cache_block_bytes
	mov     dx,cs:ending_address    
	push    dx                      ;save this now, since transient data dies
					;in initqueue
	call    InitQueue
	pop     dx

	add     dx,10h                  ; adjust for PSP size
	mov     ax,3100h                ; terminate, stay resident
	int     21h
	;;;no fall through!


;-----------------------------------------------------------------------

zseg    ends
	end
