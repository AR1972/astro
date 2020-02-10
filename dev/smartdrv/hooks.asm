;/*                                   
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;
;OVERVIEW
;
;       This module contains the interrupt hooks for
;int 8 (timer interrupt), int 10 (video BIOS), int13 (disk BIOS)        
;int 19 (reboot vector), int 15 (keyboard intercept),int 9 (keyboard)
;and int 28 (idle interrupt)
;       Also, it contains the main entry point for write behind.
;       See hooksini.asm for the code which inserts these hooks into
;the interrupt chains.
;
;       Int 8 is hooked so write behind can occur.  Every timer tick
;the handler determines if it is appropriate to attempt write behind, and
;if so call the write behind entry point.  Note that previous int 8 
;handlers are allowed to process before write behind since the EOI must
;be done quickly.
;
;       Int 10 is hooked to prevent write behind from occuring during
;video BIOS interaction.  Write behind could mess with video timing and
;cause video problems.  
;
;       Int 13 is hooked to keep external int 13 callers from 
;breaking cache consistency,and to keep those cases from allowing a
;re-entrancy to occur on the int 13 vector.
;
;       Int 19 is hooked to catch those programs which use int 19
;to soft-reboot the machine.  This is done so the cache can be commited
;to disk if necessary.
;
;       Int 15 is hooked so that on those machines which support the
;keyboard intercept we can catch the ctrl+alt+delete sequence. Since
;this does not work on all machines, we also hook int 9.  However, this
;hook is not redunant because some programs use the int 15 intercept to
;reboot the machine.
;
;       Int 9 is hooked to catch ctrl+alt+delete and allow the cache
;to commit dirty buffers before reboot occures.  Also, we prevent 
;write behind during int 9 processing since this is a timing dependant
;interrupt on some machines.
;
;       Int 28 is hooked to allow the cache to write behind during idle
;cycles at an accelerated pace (compared to int 8).
;
;       Note that int 16 is no longer hooked for write behind acceleration
;because the hook encounters problems with windows (esp idle detection)
;the hook was redundant for the most part since most software uses int 28
;for idle.  Those software that do not do int 28s will have a slower
;write-behind since write behind will only occur on timer ticks.
;
;
;       Write_behind_cache is the main entry point for the write behind.
;The code determines if it is OK to access the disk, and if so will
;commit one cache element to disk.  Also, if the reboot hooks have been
;triggered, it will commit all uncommited data and reboot the machine.
	

public  commit_all_when_ok
public  invalidate_when_ok
public  reboot_when_ok
public  write_behind_cache
public  safety_flush

public  int08hook                               ;vector for timer interrupt
public  int09hook                               ;vector for keyboard interrupt
public  int10hook                               ;vector for video bios
public  int13hook                               ;vector for disk bios
public  int19hook                               ;vector for reboot
public  int15hook                               ;vector for reboot keys
;public int16hook                               ;vector for keyboard bios
public  int28hook                               ;vector for software idle
public  int21hook
public  int25hook
public  int26hook
;
;       data from rdata.asm
;
extrn   indosaddr               :dword
extrn   in_win                  :byte           ; in windows flag
extrn   in_08                   :byte           ;re-entry count for int 08
extrn   in_09                   :byte           ;re-entry count for int 09
extrn   in_10                   :byte           ;re-entry flag(?) for int 10
extrn   in_13                   :byte           ;re-entry flag(?) for int 13
extrn   in_16                   :byte           ;re-entry flag(?) for int 16
extrn   in_bambi                :byte           ;re-entry count for bambi
extrn   int08chain              :dword
extrn   int09chain              :dword
extrn   int10chain              :dword
extrn   int13chain              :dword
extrn   int15chain              :dword
extrn   int16chain              :dword
extrn   int19chain              :dword
extrn   int28chain              :dword
extrn   int21chain              :dword
extrn   int2fchain              :dword
extrn   int25chain              :dword
extrn   int26chain              :dword
extrn   processor_type          :byte

extrn   number_of_dirty_elements:word
;
;       routines from queueman.asm
;
extrn   commit_lru_dirty_element:near
extrn   commit_all_dirty:near
;
;       routines from reboot.asm
;
extrn   rebootsystem            :near
;
;       routines from queueman.asm
;
extrn   flush_queue             :near
;
;       routines from popup.asm
;
extrn   shut_down_pop_up        :near
;
;       data from bambi.asm
;
extrn   num_valid_buffers       :word
extrn   next_bad_entry_ptr      :word   ; first entry will be 0
extrn   num_bad_blocks          :word

extrn   accessing_swap_file_ptr :dword  

extrn   commitflag              :byte

;       align to word for stack save variables

zseg    segment word public 'CODE'

	assume  cs:zseg
	assume  ds:nothing

include bambi.inc

;
;       local data
;
save_stack_ss           dw      ?       ;tempory stack pointer saves
save_stack_sp           dw      ?
public save_stack_ss
public save_stack_sp

num_ticks_to_timeout    db      18      ;1 sec, WARNING must be non-zero!
commit_all_when_ok      db      0       ;non zero means commit all dirty
invalidate_when_ok      db      0       ;non zero means invalidate after commit
reboot_when_ok          db      0       ;non zero means reboot after commit

num_ticks_to_int28      db      0
ignore_intenter_checks  db      0       ;somebody else is doing bad things,
					;lets hope they know what they're doing
;=============================================================================
;==  Scan Code
;=============================================================================
SC_DEL          equ     0053h           ; Delete
SC_INS          equ     0052h           ; Insert
;=============================================================================
;==  Keyboard Control Byte Bit Positions (ROM Data Area)
;=============================================================================
KP_ALT          equ     00001000b
KP_CTRL         equ     00000100b
KP_LSHIFT       equ     00000010b
KP_RSHIFT       equ     00000001b

interrupt_status_ok proc near
	push    ax
	mov     ax,00001011b
	out     20h,al
	jmp     short sometime
sometime:
	in      al,20h
	cmp     ah,al
	pop     ax
	ret
interrupt_status_ok endp

;int28hook
;FUNCTION
;       idle interrupt handler--write behind on idle
;USES
;       none
;
int28hook proc far
	pushf                                   ;setup iret fraim
	cli                                     ;safety first
	call    DWORD PTR cs:int28chain         ;call down idle chain

	cmp     cs:num_ticks_to_int28,5         ;bug bug hard coded
	jae     write_behind_on_int28
	push    es
	push    di

	les     di,cs:accessing_swap_file_ptr
	test    byte ptr es:[di].enhanced_mode_flags,ENHANCED_MODE_WIN
	pop     di
	pop     es
	jnz     no_write_behind_on_int28
write_behind_on_int28:
	mov     cs:num_ticks_to_int28,0
	call    write_behind_cache              ;write behind a block
no_write_behind_on_int28:
	iret
int28hook endp

;
;safety flush is consolidate code from i21 and i13 handlers
;for flushing the cache.
;set commit_all_when_ok and/or invalidate_when ok and
;call this function to properly flush the cache
;
safety_flush    proc    near
	jmp     short be_sure_to_invalidate     ;invalidate even if no dirty
continue_write_reset:
	cmp     cs:invalidate_when_ok,0
	jne     be_sure_to_invalidate
	cmp     cs:number_of_dirty_elements,0
	je      done_reset
be_sure_to_invalidate:
	sti                                     ;enable interrupts so
						;concurrent entry to
						;write behind can complete

	;if we get here, we must allow writes to go through
	;even if we are "in" int 10, 9, or int 13.
	;This will only happen is some other app (not smartdrv)
	;is already breaking one of the re-entrancy rules.
	;this should be dangerous, but heck, we just have to
	;hope they know what they're doing

	inc     cs:ignore_intenter_checks
	call    write_behind_cache              ;write behind a block
	dec     cs:ignore_intenter_checks
	jmp     short continue_write_reset
done_reset:
	ret
safety_flush    endp

;int21hook
;FUNCTION
;USES
int21hook proc far
	cmp     ah,3fh                          ; file read?
	je      handle_file_read                ;  check for read of STDIN
	cmp     ah,0dh                          ;disk reset?
	je      handle_reset
	cmp     ax,2513h                        ;see below
	je      flush_for_pctools
	cmp     ah,68h                          ;commit_file?
	je      handle_reset_flag
chainondown21:
	jmp     DWORD PTR cs:int21chain

handle_file_read:
	or      bx,bx
	jnz     chainondown21                   ; only commit if handle == STDIN
	cmp     cs:in_win,1                     ;  and not in windows
	jz      chainondown21
	mov     cs:commit_all_when_ok,1
	call    safety_flush                    ; commit before blocking read
	jmp     chainondown21                   ;  of console makes it
;                                               ;  impossible.


handle_reset:
	pushf                                   ;setup iret fraim
	cli                                     ;safety first
	call    DWORD PTR cs:int21chain         ;call down idle chain
	;;;Set carry flag appropriately in iret frame
	push    ax
	push    bp                              
	mov     bp,sp                           
	lahf
	mov     byte ptr ss:[bp+8],ah           
	pop     bp
	pop     ax                              

	mov     cs:commit_all_when_ok,1
	cmp     ah,0dh
	jne     commit_no_invalidate
	mov     cs:invalidate_when_ok,1
commit_no_invalidate:
	call    safety_flush
	iret

handle_reset_flag:
	cmp     cs:commitflag,1
	je      handle_reset
	jmp     short chainondown21

;
;PCtools hooks int13 and succeeds the call with carry clear, but does
;not reflect down the int 13 chain for the actual read.  They do this
;on int 25s for disk detection.  Here, we detect that someone is hooking
;int 13h and flush the cache so when the int 25 happens,
;the commit will have nothing to do.  It cannot, since the next int13
;will not go through!
flush_for_pctools:   


	cmp     cs:in_bambi,0                   ;if we get call while in_bambi
	jne     chainondown21                   ;we arent in pctools--safetyflush
	mov     cs:commit_all_when_ok,1         ;will hang if in_bambi is set
	call    safety_flush    
	jmp     short chainondown21     
int21hook endp


IF 0
messes with windows idle detection
;int16hook
;FUNCTION
;       software keyboard handler
;       write behind on keyboard poll
;
int16hook proc far
	pushf
	inc     cs:in_16
	cmp     cs:in_16,1
	jne     dont_reenter16
	call    write_behind_cache
dont_reenter16:
	dec     cs:in_16
	popf
	jmp     DWORD PTR cs:int16chain         ;call down int 16 chain
int16hook endp
endif

;
;quick out exit for short jmps in write_behind_cache
;
quick_out_writebehind:
	dec     cs:in_bambi
	push    ds                              ;set the indos flag
	push    di                              ;so TSRS wont try to
	lds     di,cs:indosaddr                 ;pop up while we are
	dec     byte ptr ds:[di]                ;doing write behind
	pop     di                              ;re-enter us via int 21
	pop     ds

	ret

;
;write_behind_cache
;FUNCTION
;       Determine that it is safe to access the disk (via the device drivers)
;to write behind cached data.  If it is, commit the "least recently used"
;block to disk.
;       This function is called at (asynchronous) interrupt time.
;In particular, interrupt 08 (the timer tick).
;
;USES   
;       none
;EFFECTS
;       stack change
;       critical section entered
;
write_behind_cache proc near
write_behind_on_idle:

	push    ds                              ;set the indos flag
	push    di                              ;so TSRS wont try to
	lds     di,cs:indosaddr                 ;pop up while we are
	inc     byte ptr ds:[di]                ;doing write behind
	pop     di                              ;re-enter us via int 21
	pop     ds


	inc     cs:in_bambi
	cmp     cs:in_bambi,1                   ;do not re-enter any part
	jne     quick_out_writebehind           ;of the caching mechanism

	cmp     cs:invalidate_when_ok,0         ;invalidate even if no dirty
	jne     dont_check_dirty_count

	cmp     cs:number_of_dirty_elements,0   ;if there are no dirty blocks
	je      quick_out_writebehind           ;don't do anything
dont_check_dirty_count:
	cmp     cs:ignore_intenter_checks,0     ;see safety_flush above
	jne     dont_do_int_checks

	cmp     cs:in_10,0                      ;do not re-enter the ROM
	jne     short_jne_exit_08_no_crit                       ;BIOS if already in video
						;functions--could lead to
						;crash and odd video (timing,
						;snow) problems. Device drivers
						;called by cache call the
						;ROM BIOS
	
	cmp     cs:in_09,0                      ;do no process during
	jne     short_jne_exit_08_no_crit                       ;keyboard interrupt--timing
						;dependent on some machines
	cmp     cs:in_13,0                      ;do not process (reenter)

	je      noexit
						;during ROM BIOS disk 
						;functions non-dos programs can
						;use int13 directly without
						;going through DOS
short_jne_exit_08_no_crit:
	jmp     exit_08_no_crit
noexit:

dont_do_int_checks:

	push    ax                              ;At this point, we need to
	mov     ax,8001h                        ;enter a critical disk
	int     2ah                             ;section so a context 
	pop     ax                              ;switch will not interrupt
						;(therefore re-enter) the
						;disk drivers (and our own
						;code!)

	cmp     cs:in_bambi,1                   ;we need to re-check this
	jne     critical_section_out            ;within the critical section
						;to verify that window's
						;hasn't swapped to another
						;task since our first check
						;we do it this way to
						;prevent critical sections
						;on every timer tick


	mov     cs:save_stack_ss,ss             ;save the current stack 
	mov     cs:save_stack_sp,sp             ;contexed
	
	push    cs                              ;and switch to our own
	pop     ss                              ;internal stack
	mov     sp,offset cs:resident_stack

	cmp     cs:processor_type,CPU386
	jae     save386regs

	push    ds                              ;we cannot corrupt any
	push    ax                              ;registers on a timer tick!
	push    bx      

	push    cx                              ;finish saving all the 
	push    dx                              ;registers for write-behind
	push    es
	push    bp
	push    si
	push    di

done_save_regs:

	cmp     cs:ignore_intenter_checks,0     ;see safety_flush above
	jne     dont_check_istatus

	call    interrupt_status_ok             ;got this from MSDOS
	jc      commit_done                     ;excyclopedia,ralphl
dont_check_istatus:             

	cld                                     ;clear direction for all
						;the guys in commit!
	cmp     cs:commit_all_when_ok,0
	jnz     commit_all_behind

	les     di,cs:accessing_swap_file_ptr
	test    es:[di].enhanced_mode_flags,WRITE_IS_PENDING
	jnz     commit_done
	or      es:[di].enhanced_mode_flags,THIS_WRITE_IS_LAZY
	call    Commit_LRU_Dirty_Element        ;write out a dirty block!
	les     di,cs:accessing_swap_file_ptr
	and     es:[di].enhanced_mode_flags,NOT THIS_WRITE_IS_LAZY

commit_done:
	cmp     cs:processor_type,CPU386
	jae     restore386regs
	pop     di                              ;restore registers...
	pop     si
	pop     bp
	pop     es
	pop     dx
	pop     cx
	pop     bx
	pop     ax
	pop     ds
done_restore_regs:
	
	mov     ss,cs:save_stack_ss             ;restore stack
	mov     sp,cs:save_stack_sp

critical_section_out:
	push    ax                              ;turn off the disk critical             
	mov     ax,8101h                        ;section--ok to context
	int     2ah                             ;switch now
	pop     ax
exit_08_no_crit:
	dec     cs:in_bambi
	push    ds                              ;set the indos flag
	push    di                              ;so TSRS wont try to
	lds     di,cs:indosaddr                 ;pop up while we are
	dec     byte ptr ds:[di]                ;doing write behind
	pop     di                              ;re-enter us via int 21
	pop     ds

	ret
save386regs:
;WARNING 386 code only runs on a 386!
.386
	pushad
	push    fs
	push    gs
	push    ds
	push    es
	jmp     short done_save_regs
restore386regs:
	pop     es
	pop     ds
	pop     gs
	pop     fs
	popad
	jmp     short done_restore_regs
.8086

commit_all_behind:
	cmp     cs:reboot_when_ok,0
	je      not_reboot
	call    shut_down_pop_up
not_reboot:
	call    commit_all_dirty
	cmp     cs:invalidate_when_ok,0
	je      do_not_invalidate
	mov     cs:num_valid_buffers,0  ; invalidate supercache
	call    flush_queue
	mov     cs:invalidate_when_ok,0 
do_not_invalidate:
	cmp     cs:reboot_when_ok,0
	je     just_commit
	call    rebootsystem
	;;no fall through!
just_commit:
	mov     cs:commit_all_when_ok,0         
						
	jmp     short   commit_done

write_behind_cache endp

;
;int08hook
;FUNCTION
;       hardware timer interrupt 08h handler.  
;       processes the timer interrupt by calling older handlers (one will
;       do the EOI)
;       
;       If a while has passes since the last try, do some write-behind
;USES
;       none
;
int08hook proc far

	pushf                                   ;set up iret frame
	cli                                     ;in case of bad software
	call    DWORD PTR cs:int08chain         ;call int 08 handler chain

	inc     cs:num_ticks_to_int28
	dec     cs:num_ticks_to_timeout         ;'nuf time passed to do
	jz      check_dump                      ;write behind?

no_action:
	iret                                    ;if not, just exit
check_dump:
	cmp     cs:number_of_dirty_elements,0   ;timeout period begins when
	jz      restart_timeout                 ;dirty blocks exist


	;;; we should't re-enter the DOS bios on timer ticks
	;;; so we need to check that here.  We don't put the check
	;;; in write_behind since there are times we will want to
	;;; write behind when indos is set.
	push    ds                              ;set the indos flag
	push    di                              ;so TSRS wont try to
	lds     di,cs:indosaddr                 ;pop up while we are
	cmp     byte ptr ds:[di],0
	pop     di                              ;re-enter us via int 21
	pop     ds
	jne     no_action


	sti                                     ;allow interrupts through
						;during write behind

						;dirty blocks are timed out
	call    write_behind_cache              ;so do a write-behind
	mov     cs:num_ticks_to_timeout,1       ;1 so next tick decs to zero
						;so write-behind will continue
						;until there are no more
						;dirty blocks
	jmp     short no_action                 ;exit
restart_timeout:
						;no dirty blocks so restart
						;timeout period
	mov     cs:num_ticks_to_timeout,5*18    ;BUG BUG hard coded
	jmp     short no_action
int08hook endp
	 

;int10hook
;FUNCTION
;       keep track of entry count on ROM BIOS video functions
;       flag used to prevent write-behind during video bios operations
;USES
;       none
int10hook proc far

	pushf                                   ;set up iret frame
	inc     cs:in_10                        ;entering int10h
	cli                                     ;safetyfirst
	call    cs:int10chain                   ;allow call through chain
	dec     cs:in_10                        ;exiting int10h
	iret                                    ;exit to caller
int10hook endp


;int13hook
;FUNCTION
;       keep track of entry count on ROM BIOS disk functions
;       handle special case where applications do disk IO directly
;       by comming cache before operations
;       via int 13 (quite rare, but case must be handled)
;USES
;       none
;
int13hook proc far

	;;;the next 8 instructions are basically a PUSHF
	;;;that maintains the input flags (interrupt flag) 

	push    ax                              ;reserve area for flags
	push    bp                              ;preserve bp
	push    ax                              ;preserve ax
	mov     bp,sp                           ;get base pointer to stack
	mov     ax,ss:[bp+10]                   ;get user input flags->ax
	mov     ss:[bp+4],ax                    ;put user input flags into
						;reserved area
	pop     ax                              ;restore ax
	pop     bp                              ;restore bp

;       pushf                                   ;iret frame with input flags
	cmp     cs:in_bambi,0                   ;bambi called device driver?
	je      flush_before_13                 ;get all data to disk!
continue_13:
	inc     cs:in_13                        ;normal entry of int13
	cli                                     ;safety first
	call    cs:int13chain                   ;call int13 chain
	pushf                                   ;preserve flags on return!
	dec     cs:in_13                        ;normal exit of int13
	popf                                    ;restore int13 return flags
	retf    2                               ;exit with current flags
	;iret
flush_before_13:
	push    es
	push    di

	les     di,cs:accessing_swap_file_ptr
	cmp     byte ptr es:[di],0
	pop     di
	pop     es
	jne     continue_13

	push    ax
	push    bx
	push    cx
	push    dx
	push    si
	push    di
	push    bp
	push    ds
	push    es
continue_write_int13:
	mov     cs:commit_all_when_ok,1
	mov     cs:invalidate_when_ok,1
	call    safety_flush
	pop     es
	pop     ds
	pop     bp
	pop     di
	pop     si
	pop     dx
	pop     cx
	pop     bx
	pop     ax

	jmp     short continue_13               ;go do the int 13 request
int13hook endp


biosdataoffset  dw      40h

;int09hook
;FUNCTION
;       keyboard interrupt handler keeps track of entry and exit to
;       keyboard handling routines
;
int09hook proc far
	pushf                                   ;iret frame with input flags
	cli
	cmp     cs:number_of_dirty_elements,0
	jne     check_ctrlaltdel
continue_09:
	inc     cs:in_09                        ;entry to int09h
	call    cs:int09chain                   ;do the int09 chaing
	dec     cs:in_09                        ;exit of int09h
	iret
check_ctrlaltdel:
	push    ax
	push    es
	mov     es, word ptr cs:biosdataoffset
	mov     al, byte ptr es:[17h]   ; bios location of keyboard state and status flags
	test    al, KP_ALT 
	jz      not_alted_in9
	test    al, KP_CTRL
not_alted_in9:
	jnz     ctrl_alt_down
popesaxandcontinue_09:
	pop     es
	pop     ax
;;;zero flag not set if ctrl+alt are down
	jmp     short   continue_09
ctrl_alt_down:

	push    ax
	in      al,60h                          ; put scan code into al
	cmp     al,SC_DEL                       ;delete key?
	pop     ax
	jne     popesaxandcontinue_09


	and     byte ptr es:[17h],NOT KP_CTRL

	pop     es
	pop     ax
	inc     cs:in_09                        ;entry to int09h
	call    cs:int09chain                   ;do the int09 chain
	dec     cs:in_09                        ;exit of int09h
	mov     cs:commit_all_when_ok,1
	mov     cs:reboot_when_ok,1

	push    es
	mov     es,word ptr cs:biosdataoffset
	or      byte ptr es:[17h],KP_CTRL
	pop     es

;the following may finish off the write behind and reboot the system,
;however it will not if in_bambi is set 
;the next timer tick will do this too however
	call    write_behind_cache

	iret
	
	
int09hook endp

;int19hook
;FUNCTION
;       flush the cache before rebooting machine!
;
int19hook proc far
	cmp     cs:in_bambi,0
	jne     dont_commit19
	call    commit_all_dirty
dont_commit19:
	jmp     dword ptr cs:int19chain
int19hook endp

;int15hook
;FUNCTION
;       flush the cache before rebooting machine!
;       based on code from emm386
int15hook proc far
	cmp     ah,4fh                  ;keyboard intercept?
	je      check_keys
chain15:
	jmp     dword ptr cs:int15chain
check_keys:
	cmp     al,SC_DEL
	jne     chain15

	push    ax
	push    es
	mov     ax, 40h
	mov     es, ax
	mov     al, byte ptr es:[17h]   ; bios location of keyboard state and status flags
	test    al, KP_ALT 
	jz      not_alted
	test    al, KP_CTRL
not_alted:
	pop     es
	pop     ax
	jz      chain15
	cmp     cs:reboot_when_ok,0
	jne     already_shutting_down           ;don't race when we reboot!
	mov     cs:reboot_when_ok,1
	mov     cs:commit_all_when_ok,1
	call    write_behind_cache
already_shutting_down:
	jmp     short chain15
int15hook endp


int25isstupid   dw      ?
;int25hook
;FUNCTION
;       flush and invalidate the cache before and after absolute
;       disk read.  This is for BONEHEADED Central Point Software's
;       brain-damaged disk detection code from hell.  Also, it
;       makes disk de-fragmenters safe.
;
int25hook proc far

	;;;the next 8 instructions are basically a PUSHF
	;;;that maintains the input flags (interrupt flag) 

	push    ax                              ;reserve area for flags
	push    bp                              ;preserve bp
	push    ax                              ;preserve ax
	mov     bp,sp                           ;get base pointer to stack
	mov     ax,ss:[bp+10]                   ;get user input flags->ax
	mov     ss:[bp+4],ax                    ;put user input flags into
						;reserved area
	pop     ax                              ;restore ax
	pop     bp                              ;restore bp

;       pushf                                   ;iret frame with input flags

	mov     cs:commit_all_when_ok,1
	mov     cs:invalidate_when_ok,1
	call    safety_flush
	cli
	call    dword ptr cs:int25chain
	pop     cs:int25isstupid
	pushf
	jnc     noresetbads
	mov     cs:next_bad_entry_ptr,0         ;nortons will return error correctly
	mov     cs:num_bad_blocks,0             ;but we don't want to think first block
						;is bad when its ok
noresetbads:
	mov     cs:commit_all_when_ok,1
	mov     cs:invalidate_when_ok,1
	call    safety_flush
	popf
	retf    ;no 2 since this is int 25
int25hook endp

;int26hook
;FUNCTION
;       flush and invalidate the cache before and after absolute
;       disk write.  This is for BONEHEADED Central Point Software's
;       brain-damaged disk detection code from hell.  Also, it
;       makes disk de-fragmenters safe.
;
int26hook proc far

	;;;the next 8 instructions are basically a PUSHF
	;;;that maintains the input flags (interrupt flag) 

	push    ax                              ;reserve area for flags
	push    bp                              ;preserve bp
	push    ax                              ;preserve ax
	mov     bp,sp                           ;get base pointer to stack
	mov     ax,ss:[bp+10]                   ;get user input flags->ax
	mov     ss:[bp+4],ax                    ;put user input flags into
						;reserved area
	pop     ax                              ;restore ax
	pop     bp                              ;restore bp

;       pushf                                   ;iret frame with input flags
	mov     cs:commit_all_when_ok,1
	mov     cs:invalidate_when_ok,1
	call    safety_flush
	cli
	call    dword ptr cs:int26chain
	pop     cs:int25isstupid
	pushf
	mov     cs:commit_all_when_ok,1
	mov     cs:invalidate_when_ok,1
	call    safety_flush
	popf
	retf    ;no 2 since this is int 26
int26hook endp

align 2
		dw      64      dup ('!')               ;11-11-92 scottq
		dw      128     dup ('!')               ;stack for write-behind
temp_res_stack  dw      128     dup ('!')
resident_stack  dw      0
public resident_stack
public temp_res_stack
zseg ends

end
