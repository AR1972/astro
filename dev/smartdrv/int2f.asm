;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;
;
;Bambi cache interface multiplex:
;
;       MULT_BAMBI(ax)                                  equ     4a10h
;            BAMBI_GET_STATS(bx)                        equ        0
;            BAMBI_COMMIT_ALL(bx)                       equ        1
;            BAMBI_REINITIALIZE(bx)                     equ        2
;            BAMBI_CACHE_DRIVE(bx)                      equ        3
;                  CACHE_DRIVE_GET(dl)                  equ        0
;                  CACHE_DRIVE_READ_ENABLE(dl)          equ        1
;                  CACHE_DRIVE_READ_DISABLE(dl)         equ        2
;                  CACHE_DRIVE_WRITE_ENABLE(dl)         equ        3
;                  CACHE_DRIVE_WRITE_DISABLE(dl)        equ        4
;            BAMBI_GET_INFO(bx)                         equ        4    
;            BAMBI_GET_ORIGINAL_DD_HEADER(bx)           equ     7
;
;       the following is a temporary function for a debug version

	     BAMBI_INTERNAL_POINTERS                    equ        10
;       BAMBI_SIGNATURE                                 equ     BABEh
;
;BAMBI_GET_STATS:
;FUNCTION:
;       Gets cache hit/miss statistics as well as detects presense of cache.
;
;       INPUT:
;               AX=MULT_BAMBI
;               BX=BAMBI_GET_STATS
;       OUTPUT:
;               AX=BAMBI_SIGNATURE
;               DI=cache misses (hiword)
;               SI=cache misses (loword)
;               DX=cache hits (hiword)
;               BX=cache hits (loword)
;               CX=number of uncommitted (dirty) blocks
;               BP=version number in binary (0400h)
;       USED:
;               All except DS,ES
;
;BAMBI_COMMIT_ALL:
;FUNCTION:
;       Commits all uncommitted blocks (dirty blocks) to disk.
;
;       INPUT:
;               AX=MULT_BAMBI
;               BX=BAMBI_COMMIT_ALL
;       OUTPUT:
;               NONE
;       USED:   
;               NONE
;
;BAMBI_REINITIALIZE:
;FUNCTION:
;       Commits all uncommitted blocks to disk and invalidates the cache.
;Re-sizes the cache with input parameters.
;       INPUT:
;               AX=MULT_BAMBI
;               BX=BAMBI_REINITIALIZE
;               CX=number of cache elements
;               DX=size of cache elments
;       OUTPUT:
;               carry set if procedure unable to complete via XMS errors
;       USED:
;               ALL except DS,ES
;
;BAMBI_CACHE_DRIVE:
;FUNCTION:
;       Enables and disables read or write caching for a particular drive unit.
;Returns the cache state of the drive in DL. Get takes no action, but simply
;returns cache state for drive unit in DL.
;       INPUT:
;               AX=MULT_BAMBI
;               BX=BAMBI_CACHE_DRIVE
;               DL=CACHE_DRIVE_<get,read|write enable|disable>
;               BP=unit number of drive 
;       OUTPUT:
;               DL=cache state of unit:
;                       Bit 8 set -> no caching enabled for this unit
;                       Bit 8 not set -> read caching enabled for this unit
;                       Bit 7 set -> write caching not enabled for this unit
;                       Bit 7 not set -> write caching enabled for this unit
;                       -1 -> not a cachable drive
;       USES:
;               ALL except DS,ES
;
;BAMBI_GET_INFO:
;FUNCTION:
;       Get the current size of the cache.
;       INPUT:
;               AX=MULT_BAMBI
;               BX=BAMBI_GET_INFO
;       OUTPUT:
;               CX=size of cache block in bytes
;               AX=number of cache blocks while in dos
;               BX=number of cache blocks in cache
;       USES:
;               ALL except DS,ES
;
;BAMBI_GET_ORIGINAL_DD_HEADER:
;FUNCTION:
;       Returns a pointer to the original device header for a given
;       drive, along with the unit number which represents it on
;       the original device.
;       INPUT:
;               AX=MULT_BAMBI
;               BX=BAMBI_GET_ORIGINAL_DD_HEADER
;               BP=unit number of drive 
;       OUTPUT:
;               DL=original unit number
;            ES:DI=original device header
;       USES:
;               ALL except DS
;
;BAMBI_INTERNAL_POINTERS:
;FUNCTION:
;       return a pointer to a structure which describes the main cache
;       data structures.  This is intended to be used by a program which
;       takes a snapshot of the contents, for analyzing what is contained
;       in the cache at any given moment.
;       INPUT:
;               AX=MULT_BAMBI
;               BX=BAMBI_INTERNAL_POINTERS
;       OUTPUT:
;               ES:BX -> data structure:
;                  *validindexoffset word,
;                  *DirtyIndexOffset word,
;                  *ElementIdentifierLowordOffset word,
;                  *ElementIdentifierHiwordOffset word,
;                  *Queuelength word.

;
include bambi.inc

public  int2fhook

;
;       data from rdata.asm
;
extrn   real_dd_headers         :dword
extrn   real_cache_units        :byte
extrn   in_bambi                :byte
extrn   in_win                  :byte
extrn   nohit_h                 :word
extrn   nohit_l                 :word
extrn   hit_h                   :word
extrn   hit_l                   :word
extrn   number_of_dirty_elements:word
extrn   int2fchain              :dword
extrn   queuelength             :word
extrn   cache_block_bytes       :word
extrn   number_of_cache_elements:word
extrn   number_of_cache_elements_win:word
;
;       routines from queueman.asm
;
extrn   commit_all_dirty        :near
extrn   initqueue               :near
;
;       routines from cacheman.asm
;
extrn   reinitialize_cache:near

extrn   warning_pop_up          :near
extrn   warning_pop_up_DOS      :near
;
;       routine from hooks.asm
;
extrn   safety_flush            :near
extrn   commit_all_when_ok      :byte
extrn   write_behind_cache      :near
extrn   invalidate_when_ok      :byte
;
; from umbload.asm
;
;extrn  startup_name_off        :word
;extrn  startup_name_seg        :word
extrn   vxd_name                :byte

zseg    segment public 'CODE'

	assume  cs:zseg
	assume  ds:nothing

dblmaptable     db      0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17
		db      18,19,20,21,22,23,24,25


;see bambi.inc for data structure definitions
smart_win_data  db size smart_win_access dup(0) ;data struction *must* be zerod
startup_info    db size Win386_Startup_Info_Struc dup(0) ;packet for vxd load
reference_ptr   dd      ?

accessing_swap_file_ptr         dd      ?

public accessing_swap_file_ptr
public smart_win_data
public startup_info
public reference_ptr

chain2fnodec:
	jmp     DWORD PTR cs:int2fchain

swappeddrv  db  ?
far_handle_dblspace:
	cmp     bx,MAGICDRV_SWAP        ;drive swap?
	jne     chain2fnodec
	
; dl = 0-based drive letter
;
	push    ax
	push    bx
	mov     ax,MAGICDRV_2F          
	mov     bx,MAGICDRV_MAP         ;get drive map from magicdrv
	pushf
	cli
	call    DWORD PTR cs:int2fchain ;by call chaining
	and     bl,7Fh
	mov     cs:swappeddrv,bl
	pop     bx
	pop     ax

	pushf                           ;let dblspace do the swap/unswap
	cli
	call    DWORD PTR cs:int2fchain
	or      al,al                   ;error means no swap occured
	jnz     not_swapped
	
	push    ax                      ;save error code (al==0)
	mov     ax,MAGICDRV_2F          
	mov     bx,MAGICDRV_MAP         ;get drive map from magicdrv
	pushf
	cli
	call    DWORD PTR cs:int2fchain ;by call chaining
	push    bp                      ;preserve registers we toast
	push    bx
	and     bx,7fh                  ;mask off high-bit, we want unit
	cmp     bl,dl                   ;if the same, we just did an unswap
	je      unswap

continueswap:
	;;;dl   holds src drive
	;;;bl   holds dest drive
	xor     dh,dh                   ;need register for index
	mov     bp,dx                   ;so use bp
	
	mov     al,cs:dblmaptable[bx]   ;swap table entries
	xchg    al,cs:dblmaptable[bp]
	mov     cs:dblmaptable[bx],al
	
	pop     bx                      ;restore regs
	pop     bp
	pop     ax                      ;and error code 

not_swapped:
	iret                            ;back to caller
unswap:
	mov     bh,cs:dblmaptable[bx]   ;get drive to unswap with
	cmp     bh,bl                   ;do we think its already swapped?
	je      handleunswapbeforeswap
	mov     bl,bh
contunswap:
	xor     bh,bh
	jmp     short continueswap
handleunswapbeforeswap:
	mov     bl,cs:swappeddrv
	jmp     short contunswap


checkidle:
	cmp     bx,18h
	jne     chain2fshort
	cmp     cx,0
	jne     chain2fshort
	cmp     cs:number_of_dirty_elements,0
	je      chain2fshort
	dec     cs:in_bambi
	call    write_behind_cache
	xor     ax,ax
	iret

chain2fshort:
	jmp     chain2f

get_page_file_access:
	cmp     bx,0021h                ;pagefile device id?
	jne     checkidle
	or      cx,cx
	jnz     chain2f

	;;; first call down the chain to see if anyone else has 
	;;; a pointer to the pagefile access byte. If not, we must
	;;; supply one!
	pushf
	cli
	call    dword ptr cs:int2fchain
	or      ax,ax
	jz      byte_in_es_di
	
	xor     ax,ax
	push    cs
	pop     es
	mov     di,offset smart_win_data
byte_in_es_di:
	push    ax
	mov     ax,cs:cache_block_bytes
	mov     es:[di].max_delayed_write_size,ax
	pop     ax
	mov     word ptr cs:accessing_swap_file_ptr[0],di       ;fill in our access pointer
	mov     word ptr cs:accessing_swap_file_ptr[2],es       ;fill in our access pointer
	dec     cs:in_bambi
	iret

handle_apm:
	cmp     bl,2                    ;standby or suspend?
	ja      chain2f                 ;chain on if not
	
;       call    flush_in_int2f          ;flush cache

	dec     cs:in_bambi             ;unset semaphore since
	mov     cs:commit_all_when_ok,1
	call    write_behind_cache      ;write_behind looks at it,
	inc     cs:in_bambi             ;its ok to re-enter here anyway

	cmp     cs:number_of_dirty_elements,0;all dirty data flushed?
	je	chain2f

;	fail the apm request

	mov     bh,80h                  ;fail code for apm
	jmp     short chain2f
		
handle_dblspace:
	jmp    far_handle_dblspace

int2fhook proc far
	cmp     ax,MAGICDRV_2F          ;dblspace ?
	je      handle_dblspace

	inc     cs:in_bambi
	cmp     ax,530Bh                ;APM call? (suspend/resume?)
	je      handle_apm

	cmp     ax,MULT_BAMBI           ;is it the bambi interface multiplex?
	je      handle_bambi_api        
	cmp     ah, 16h                 ;or the windows' 
	jne     short chain2f
	cmp     al, 05h                 ;starting up code
	je      short windows_broadcast
	cmp     al, 06h                 ;or shutting down code?
	je      short windows_broadcast
	cmp     al, 07h                 ;win386 device broadcast?
	jne     chain2f
	jmp     get_page_file_access


chain2f:
	dec     cs:in_bambi
	jmp     DWORD PTR cs:int2fchain ;chain on down if not
windows_broadcast:
	push    ax
	push    bx
	push    cx
	push    dx
	push    si
	push    di
	push    bp
	push    es
	push    ds

	call    flush_in_int2f
	cmp     al,06h
	je      winshutdown
winstartup:     
	mov     cx,cs:number_of_cache_elements_win
	mov     cs:in_win,1             ; set in windows flag


	jmp     short complete_windows_broadcast
handle_bambi_api:
	jmp     handle_bambi_api_toofar
winshutdown:

	mov     cx,cs:number_of_cache_elements
	mov     cs:in_win,0             ; reset in windows flag
complete_windows_broadcast:
	call    reinitialize_cache
	jnc     noxmserror
	call    handle_reallocfailure
noxmserror:
	pop     ds
	pop     es
	pop     bp
	pop     di
	pop     si
	pop     dx
	pop     cx
	pop     bx
	pop     ax
;;;     now handle vxd load
	pushf                                   ;chain down to get packet in es:bx
	cli                                     ;safety first
	call    DWORD PTR cs:int2fchain
	mov     cs:startup_info.SIS_Version_High,3 ;windows 3.0 compatible
	mov     cs:startup_info.SIS_Version_Low,0
	mov     word ptr cs:startup_info.SIS_Next_Dev_Ptr[0],bx ;set packet chain
	mov     word ptr cs:startup_info.SIS_Next_Dev_Ptr[2],es
	mov     word ptr cs:startup_info.SIS_Instance_Data_Ptr[0],0     ;no instance data
	mov     word ptr cs:startup_info.SIS_Instance_Data_Ptr[2],0
	push    cs
	pop     es
	mov     bx,offset cs:vxd_name
;       mov     es,cs:startup_name_seg 
;       mov     bx,cs:startup_name_off
	mov     word ptr cs:startup_info.SIS_Virt_Dev_File_Ptr[0],bx
	mov     word ptr cs:startup_info.SIS_Virt_Dev_File_Ptr[2],es
	push    cs
	pop     es
	mov     bx,offset cs:startup_info
	jmp     exit_no_chain

handle_reallocfailure:
	mov     cx,cs:number_of_cache_elements_win ;guaranteed to be <=
	call    initqueue
	retn

get_original_dd_header:
	dec     bp              ; make unit zero relative
	push    bx
	xor     bh,bh
	mov     bl,cs:dblmaptable[bp]
	mov     dl,cs:real_cache_units[bx]
	and     dl,3fh          ; get rid of extra bits
	add     bx,bx
	add     bx,bx
	les     di,cs:real_dd_headers[bx]
	pop     bx
	jmp     exit_no_chain

handle_bambi_api_toofar:
	cmp     bx,BAMBI_GET_STATS      ;detect bambi/get bambi status?
	je      get_stats
	cmp     bx,BAMBI_COMMIT_ALL     ;commit the cache synchronously?
	je      commit_all
	cmp     bx,BAMBI_REINITIALIZE   ;reset bambi's cache?
	je      reinitialize
	cmp     bx,BAMBI_CACHE_DRIVE
	je      cache_drive
	cmp     bx,BAMBI_GET_INFO       ;cache size,etc
	je      get_info
	cmp     bx,BAMBI_GET_ORIGINAL_DD_HEADER ; get pointer to orig dd, etc.
	je      get_original_dd_header
	if      1                       ;  ***Chuck's debug stuff!!!
	cmp     bx,BAMBI_INTERNAL_POINTERS
	je      return_internals
	endif
	cmp     bx,1234h
	je      display_the_message
	jmp     chain2f

	if      1                       ; *** Chuck's debug stuff!!!!
return_internals:
	push    cs
	pop     es
	mov     bx,offset internals_struct
	jmp     exit_no_chain

	extrn   validindexoffset:word
	extrn   DirtyIndexOffset:word
	extrn   ElementIdentifierLowordOffset:word
	extrn   ElementIdentifierHiwordOffset:word
	extrn   Queuelength:word

internals_struct:
	dw      offset validindexoffset
	dw      offset DirtyIndexOffset
	dw      ElementIdentifierLowordOffset
	dw      ElementIdentifierHiwordOffset
	dw      Queuelength

	endif

	;iret
get_stats:
	mov     ax,BAMBI_SIGNATURE
	mov     di,cs:nohit_h
	mov     si,cs:nohit_l
	mov     dx,cs:hit_h
	mov     bx,cs:hit_l
	mov     cx,cs:number_of_dirty_elements  
	mov     bp,BAMBI_VERSION_BCD
	jmp     exit_no_chain

get_info:
	mov     cx,cs:cache_block_bytes
	mov     bx,cs:queuelength
	mov     dx,cs:number_of_cache_elements_win
	mov     ax,cs:number_of_cache_elements
	jmp     exit_no_chain

commit_all:
	call    flush_in_int2f

	jmp     exit_no_chain

reinitialize:
;QEMM in a dos box will toast the handle, dont know why
;but we don't want to eat the disk, and sincethe resize
;part of this api is removed, we just do the dorky way
;       mov     cx,cs:number_of_cache_elements
;       call    reinitialize_cache
;       jnc     reinitsucceeded
;       call    handle_reallocfailure   
;reinitsucceeded:
	mov     cs:invalidate_when_ok,1
	call    flush_in_int2f
	jmp     exit_no_chain

display_the_message:
	mov     al,3
	call    warning_pop_up
	jmp     exit_no_chain

cache_drive:
	;input 
	;       bp = unit number to examine cache state 
	;       dl = 0 get state of this units cacheing
	;       dl = 1 enable read cacheing for this drive
	;       dl = 2 disable read cacheing for this drive
	;       dl = 3 enable write cacheing for this drive
	;       dl = 4 disable write cacheing for this drive
	;output
	;       dl = un-mapped unit or'd with 80h if not cacheing
	;       dl == -1 indicates this drive does not exist
	;       
	or      dl,dl
	jz      return_cache_state_bridge

;	Now we must verify that this is not a MagicDrv drive.
;	Our main command line will never ask us to do it, but
;	SmartMon might, and it is disastrous.

	push	ax
	push	bx
	push	cx
	push	dx

	mov	ax,MAGICDRV_2F
	xor	bx,bx				; install check, get version
	pushf					; chain down to MagicDrv
	cli                                     ;safety first
	call    DWORD PTR cs:int2fchain
	or	ax,ax				; is MD present, no error?
	jnz	not_magic

	mov	dx,bp
	mov	ax,MAGICDRV_2F
	mov	bx,MAGICDRV_MAP			; see if it is compressed
	pushf
	cli
	call	DWORD PTR cs:int2fchain		; call through
	not	bl				; invert the "compressed" bit
	and	bl,80h				; zero true if compressed

;	When we get here with zero true, the drive is compressed

not_magic:
	pop	dx
	pop	cx
	pop	bx
	pop	ax

	jz	return_cache_state_bridge	; ignore it and return status


	cmp     dl,1
	je      read_cache_this_drive
	cmp     dl,2
	je      disable_read_cacheing_for_this_drive
	cmp     dl,3
	je      write_cache_this_drive
	cmp     dl,4 
	jne	exit_no_chain		; exit if illegal value

;	disable write cacheing for this drive

	push    bp
	call    flush_in_int2f

	pop     bp
	push    bx
	xor     bh,bh
	mov     bl,cs:dblmaptable[bp];
	or      cs:byte ptr cs:real_cache_units[bx],40h
	pop     bx
	jmp     short common_disable
disable_read_cacheing_for_this_drive:
	push    bp
	call    flush_in_int2f

	pop     bp
	
	push    bx
	xor     bh,bh
	mov     bl,cs:dblmaptable[bp];
	or      cs:byte ptr cs:real_cache_units[bx],80h
	pop     bx

common_disable:
	mov     cx,cs:queuelength
	push    bp
	call    initqueue
	pop     bp
return_cache_state_bridge:
	jmp     short return_cache_state
write_cache_this_drive:
	push    bx
	xor     bh,bh
	mov     bl,cs:dblmaptable[bp]
	and     cs:real_cache_units[bx],0bfh
	pop     bx

	jmp     short return_cache_state
read_cache_this_drive:
	push    bx
	xor     bh,bh
	mov     bl,cs:dblmaptable[bp]
	and     cs:real_cache_units[bx],7fh
	pop     bx

return_cache_state:
	push    bx
	xor     bh,bh
	mov     bl,cs:dblmaptable[bp]
	mov     dl,byte ptr cs:real_cache_units[bx]     
	pop     bx

	mov     ax,BAMBI_SIGNATURE
;	jmp     exit_no_chain
	;iret
int2fhook endp

exit_no_chain:
	dec     cs:in_bambi
	iret


flush_in_int2f proc near
	dec     cs:in_bambi             ;unset semaphore since
	mov     cs:commit_all_when_ok,1
	call    safety_flush            ;safety_flush looks at it,
	inc     cs:in_bambi             ;its ok to re-enter here anyway
	ret
flush_in_int2f endp

zseg ends

end
