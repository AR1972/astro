;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

include bambi.inc

public  parse_command_line
public  display_status

zseg    segment public 'CODE'

	assume  cs:zseg
;
;       data from text.asm
;
extrn   help_text               :byte
extrn   statistics1             :byte
extrn   statistics2             :byte
extrn   statistics3             :byte
extrn   statistics4             :byte
extrn   statistics5             :byte
extrn   drivestring1            :byte
extrn   drivestring2            :byte
extrn   drivestring3            :byte
extrn   msg_cannot_cache        :byte
extrn   bufferstring1           :byte
extrn   bufferstring2           :byte
extrn   bufferstring3           :byte
extrn   extrastatus1            :byte
extrn   extrastatus2            :byte
extrn   extrastatus3            :byte
extrn   extrastatus4            :byte
extrn   extrastatus5            :byte
extrn   magicdrvstring          :byte
extrn   whatsanasterisk         :byte
;
;       data from rdata.asm
;
extrn   cache_block_bytes       :word
extrn   cache_block_words       :word
extrn   commitflag              :byte
extrn   dosinfo                 :word
;
;       data from tdata.asm
;
extrn   drives_to_cache         :byte
extrn   first_instance          :byte

extrn   number_of_cache_elements:word
extrn   number_of_cache_elements_win:word
;
;       routines from int2fini.asm
;
extrn   call_bambi              :near
extrn   init_bambi_io           :near
;
;       routines from queueman.asm
;
extrn   commit_all_dirty        :near
;
;       routines from bambi.asm
;
;
;       data from bambinit.asm
;
extrn   target_buffer_size      :word
;
;       routine from dec_out.asm
;
extrn   display_dec_dword               :near
;
;       data from bambinit.asm
;
extrn   dos_size                :word
extrn   win_size                :word
;
;       routines from drvtype.asm
;
extrn   IsMagicDrive            :near

extrn   get_drive_type          :near
;
;       routines from logphys.asm
;
extrn   logical_to_physical             :near
extrn   log_phys_list                   :byte
;
;       routines from xms.asm
;
extrn   query_free_xms_memory           :near
extrn   initialize_xms                  :near
;       
;       routines from stacker.asm
;
extrn   detect_stacker          :near
extrn   detect_stacker_volume   :near
extrn   stacker_dd_pointer      :near

	display_asterisk_message db 0
	be_quiet	dw	1	;default to quiet start up
	load_low        db      0
PUBLIC  load_low
PUBLIC  be_quiet
;locals
	dos_size_done   dw      0
	extra_status    db      0

init_command_line_parsing proc near
	mov     bx,COMMAND_LINE_OFFSET  
	clc
	ret
init_command_line_parsing endp

at_eol proc near
	cmp     bx,COMMAND_LINE_OFFSET+MAX_COMMAND_LINE_LENGTH
	jae     is_at_eol
	mov     al,BYTE PTR es:[bx]
	cmp     al,COMMAND_LINE_TERMINATOR
	je      is_at_eol
	clc
	ret
is_at_eol:
	;be sure future calls always return carry
	mov     bx,COMMAND_LINE_OFFSET+MAX_COMMAND_LINE_LENGTH
	stc
	ret     
at_eol endp

get_next_char proc near
	inc     bx
	call    at_eol
	jc      eol
	mov     al,BYTE PTR es:[bx]
	cmp     al,COMMAND_LINE_TERMINATOR
	je      eol
	clc
	ret
eol:
	stc
	ret
get_next_char endp

ascii_to_hex proc near
	cmp     al,'9'
	ja      letter
	cmp     al,'0'
	jb      not_hex_char
	sub     al,'0'
	clc
	ret
letter:
	cmp     al,'F'
	ja      not_hex_char
	cmp     al,'A'
	jb      not_hex_char
   sub  al,'A'-10
	clc
	ret
not_hex_char:
	stc
	ret
ascii_to_hex endp             

ascii_to_dec proc near
	cmp     al,'9'
	ja      not_dec
	cmp     al,'0'
	jb      not_dec
	sub     al,'0'
	xor     ah,ah
	clc
	ret
not_dec:
	stc
	ret
	
ascii_to_dec endp

;
; INPUT
;       al = binary number
; OUTPUT
;       ax = two ascci chars
;
hex_to_ascii proc near
    push cx
    mov ah,al
    and al,0Fh
    add al,90h                
    daa                       
    adc al,40h                
    daa                       
    xchg al,ah
    mov cl,4
    shr al,cl
    add al,90h                
    daa                       
    adc al,40h                
    daa                          
    pop cx
    ret
hex_to_ascii endp

;
; INPUT
;    al = char to output
;
output_char proc near
     push dx
     push ax
     mov dl,al
     mov ah,2
     int 21h
     pop ax
     pop dx
     ret
output_char endp

parse_number    proc near
	xor     cx,cx
	call    get_next_char           ;get next digit in number
	jc      not_a_number    
		
get_number:
	call    ascii_to_dec            ;turn into binary number
	jc      get_number_done         ;error means this is not a hex digit
			; multiply accumlated value by 10
	push    dx
	add     cx,cx
	mov     dx,cx
	add     cx,cx
	add     cx,cx
	add     cx,dx                   
	pop     dx

	add     cx,ax                   ;add in this digit
	call    get_next_char           ;get next digit in number
	jc      get_number_done 
	jmp     short get_number
	
get_number_done:        
	clc
	ret
not_a_number:
	;stc
	ret
parse_number    endp

;
; INPUT
;       DX:BX 
; USES
;       AX
display_hex_dword       proc near
	mov     al,dh
	call    hex_to_ascii
	call    output_char
	xchg    al,ah
	call    output_char     
	mov     al,dl
	call    hex_to_ascii
	call    output_char
	xchg    al,ah
	call    output_char     
	mov     al,bh
	call    hex_to_ascii
	call    output_char
	xchg    al,ah
	call    output_char     
	mov     al,bl
	call    hex_to_ascii
	call    output_char
	xchg    al,ah
	call    output_char     
	ret     
display_hex_dword       endp

display_status:
	mov     ax,BAMBI_GET_STATS
	call    call_bambi
	cmp     ax,BAMBI_SIGNATURE
	je      do_display_status
	jmp     display_help_text ;bug bug
do_display_status:
	cmp     be_quiet,0
	je      not_quiet
	jmp     done_stats
not_quiet:
	

	push    dx
	mov     dx,offset statistics1
	mov     ah,09h
	int     21h
	pop     dx

	cmp     extra_status,0
	je      no_extra_status

	push    dx
	mov     dx,offset extrastatus1
	mov     ah,09h
	int     21h
	pop     dx

	mov     ax,BAMBI_GET_INFO
	call    call_bambi

	push    cx

	xor     dx,dx
	call    display_dec_dword
	
	push    dx
	mov     dx,offset extrastatus2
	mov     ah,09h
	int     21h
	pop     dx

	pop     cx

	mov     bx,cx
	xor     dx,dx
	call    display_dec_dword

	push    dx
	mov     dx,offset extrastatus3
	mov     ah,09h
	int     21h
	pop     dx

	mov     ax,BAMBI_GET_STATS
	call    call_bambi

	push    di
	push    si

	call    display_dec_dword

	mov     dx,offset extrastatus4
	mov     ah,09h
	int     21h

	pop     bx
	pop     dx

	call    display_dec_dword

	push    dx
	mov     dx,offset extrastatus5
	mov     ah,09h
	int     21h
	pop     dx
	
no_extra_status:

	push    dx
	mov     dx,offset statistics2
	mov     ah,09h
	int     21h
	pop     dx

	mov     ax,BAMBI_GET_INFO
	call    call_bambi

	push    cx
	push    dx
	;calculate actual cache size by multiplying number*size
	mov     ax,cx
	mul     bx
	mov     bx,ax
	call    display_dec_dword

	mov     dx,offset statistics3
	mov     ah,09h
	int     21h

	pop     dx
	pop     cx

	mov     ax,cx
	mov     bx,dx
	mul     bx
	mov     bx,ax
	call    display_dec_dword


	push    dx
	mov     dx,offset statistics4
	mov     ah,09h
	int     21h
	pop     dx


	push    bp
	push    bx
	push    dx

	mov     bp,-1                   ; start at unit zero,-1 for next inc
	push    bp                      
drive_loop:
	pop     bp
	inc     bp
	push    bp

	cmp     bp,26                   ;bug bug
	jb      not_done_drives
	jmp     done_drives

not_done_drives:

	push    bp
	mov     dx,bp
	call    IsMagicDrive
	pop     bp
	jz      NotMagicDriveReadout

	and     bx,7Fh  ;mask off all but host drive letter
	push    bx
	mov     dx,bx
	call    isMagicDrive
	pop     ax
	jz      notswappedhost
	and     bx,7Fh
	jmp     short isswappedhost
notswappedhost:
	mov     bx,ax
isswappedhost:
	mov     bp,bx

NotMagicDriveReadout:

	mov     ax,BAMBI_CACHE_DRIVE
	xor     dl,dl                   ; get cache state
	call    call_bambi
	jnc     not_done_drives2             ; bug bug
	jmp     done_drives
not_done_drives2:
	test    dl,80h                  ; read caching enabled?
	jnz     drive_loop

	mov     bx,offset drivestring2  ;yes,no

	test    dl,40h                  ; write caching enabled?
	jnz     read_cache_only         
	
	mov     bx,offset drivestring1  ;yes,yes

read_cache_only:

	pop     dx                      ;get actual drive letter
	push    dx

	mov     byte ptr [bx+04],dl
	add     byte ptr [bx+04],'A'
	mov     byte ptr [bx+06],' '
	cmp     dx,bp                   ;remapped dblspace drive?
	je      noasterisk
	mov     cs:display_asterisk_message,1
	mov     byte ptr [bx+06],'*'
noasterisk:

	push    dx
	mov     dx,bx
	mov     ah,09h
	int     21h
	pop     dx

	push    es
	push    di
	push    dx
	mov     dx,bp
;       call    logical_to_physical 
	push    bx
	mov     bl,dl
	xor     bh,bh   
	mov     dl,log_phys_list[bx]
	pop     bx
	test    dl,80h                  ;harddisk?
	jz      print_no


	mov     ax,MULT_BAMBI
	mov     bx,BAMBI_GET_BUFFER_INFO
	int     2fh                     ;get pointer to safedsk's drive list
	cmp     ax,BAMBI_SIGNATURE
	jne     print_no

	and     dl,NOT 80h              ;mask off high bit to get array index
	xor     bh,bh
	mov     bl,dl

	;;;es:di points to safedsk table
	cmp     byte ptr es:[bx+di],-1
	jne     do_we_know

	push    dx
	mov     dx,offset bufferstring1
	mov     ah,09h
	int     21h
	pop     dx

	jmp     short print_done
do_we_know:
	cmp     byte ptr es:[bx+di],0
	je      print_dont_know
print_no:
	push    dx
	mov     dx,offset bufferstring2
	mov     ah,09h
	int     21h
	pop     dx
	jmp     short print_done

print_dont_know:

	push    dx
	mov     dx,offset bufferstring3
	mov     ah,09h
	int     21h
	pop     dx

print_done:
	pop     dx
	pop     di
	pop     es

	jmp     drive_loop      
done_drives:
	pop     bp      ;pop off extra bp on stack from drive loop entry
	pop     dx
	pop     bx
	pop     bp

	cmp     cs:display_asterisk_message,1
	jne     no_asterisks

	push    dx
	mov     dx,offset whatsanasterisk
	mov     ah,09h
	int     21h
	pop     dx

no_asterisks:
	push    dx
	mov     dx,offset statistics5
	mov     ah,09h
	int     21h
	pop     dx


if 0
	push    dx
	mov     dx,offset statistics2
	mov     ah,09h
	int     21h
	pop     dx

	push    bx
	push    dx
	push    cx
	push    di
	push    si

	mov     ax,BAMBI_GET_INFO
	call    call_bambi

	;display block size
	push    bx
	mov     bx,cx
	xor     dx,dx
	call    display_dec_dword
	pop     bx

	mov     dx,offset number_elements
	mov     ah,09h
	int     21h

	;display queuelength
	xor     dx,dx
	call    display_dec_dword

	mov     dx,offset actual_size
	mov     ah,09h
	int     21h

	;calculate actual cache size by multiplying number*size
	mov     ax,cx
	mul     bx
	mov     bx,ax
	call    display_dec_dword
	

	mov     dx,offset actual_break
	mov     ah,09h
	int     21h

	pop     si
	pop     di
	pop     cx
	pop     dx
	pop     bx
   
	push    dx
	mov     dx,offset cache_hits
	mov     ah,09h
	int     21h
	pop     dx
	;display hits
	call    display_dec_dword

	mov     dx,offset cache_misses
	mov     ah,09h
	int     21h

	;display nohits 

	mov     dx,di
	mov     bx,si
	call    display_dec_dword

	mov     dx,offset dirty_blocks
	mov     ah,09h
	int     21h

	;display dirty
	mov     bx,cx
	xor     dx,dx
	call    display_dec_dword

	mov     dx,offset stats_end
	mov     ah,09h
	int     21h

endif
done_stats:     
	stc
	ret
parse_command_line proc near

	call    init_command_line_parsing

scan_parse:     
	call    get_next_char           ;get next char on command line
	jc      done_scanning_jmp       ;carry set if not more chars
	cmp     al,COMMAND_SWITCH_CHAR  ;is it a switch char?
	jne     not_switch              
	jmp     handle_switch
	
not_switch:

	cmp     al,' '                  ;skip white spaces
	je      scan_parse
	cmp     al,09h;tab              ;...tabs
	je      scan_parse

	;;;     check for number (dossize,winsize)
	cmp     al,'0'
	jae     mightbeanumber
	jmp     not_number
mightbeanumber:
	cmp     al,'9'
	jbe     isanumber
	jmp     not_number
isanumber:

	xor     cx,cx                   ;zero count for get_number!
	call    get_number      ; get value of number->cx
	jc      done_scanning_jmp       ;something went wrong BUG BUG necessary?

	;;; minimum size for cache!
	cmp     cx,MINIMUM_CACHE_SIZE
	ja      user_is_big_enough
	mov     cx,MINIMUM_CACHE_SIZE
user_is_big_enough:

	push    cx
	push    bx
	call    initialize_xms          ;be sure its ok to call xms
	pop     bx
	pop     cx
	jc      forget_it
	push    cx
	push    bx
	call    query_free_xms_memory   ;find out how much xms memory is free
	pop     bx
	pop     cx
	or      ax,ax
	jz      forget_it
	cmp     cx,ax
	jbe     user_is_small_enough
	mov     cx,ax
user_is_small_enough:
forget_it:

	cmp     dos_size_done,0         ;have we parsed the dos size already?
	jne     get_windows_size        ;if so, we are at the window's size
	mov     dos_size_done,1         ;next time through will be for windows

winsize_bigger:                         ;BUG BUG if win size is bigger
					;we set dossize==winsize!
	mov     dos_size,cx             ;save number in global

	mov     ax,1024                 ;size is in Kilobytes
	mul     dos_size                ;dx:ax = #K cache size
	div     cache_block_bytes       ;ax = number of cache elements needed

	mov     number_of_cache_elements,ax     ;remember how many elements
	mov     number_of_cache_elements_win,ax ;win size must be set so
						;defaults won't be used!
	jmp     scan_parse      
done_scanning_jmp:
	jmp     done_scanning

get_windows_size:
	cmp     cx,dos_size
	ja      winsize_bigger


	mov     win_size,cx
	mov     ax,1024                 ;size is in Kilobytes
	mul     win_size                ;dx:ax = #K cache size
	div     cache_block_bytes       ;ax = number of cache elements needed

	mov     number_of_cache_elements_win,ax

	jmp     scan_parse      
magicspecial:
	pop     bx
	pop     ax
	push    dx
	mov     dx,offset magicdrvstring
	mov     ah,09h
	int     21h
	pop     dx
	jmp     short notcached
cannot_cache_drive:

	push    dx
	mov     dx,offset msg_cannot_cache
	mov     ah,09h
	int     21h
	pop     dx
notcached:
	;;; we have detected a drive we cannot cache, but
	;;; if the user added a '+' or '-' after it, we have
	;;; "eat" it since it will be a syntax error when we
	;;; attempt to continue parsing
	call    get_next_char
	jc      nopluslast
	cmp     al,'+'
	je      eatit
	cmp     al,'-'
	je      eatit
	dec     bx                      ;unget the char
eatit:
	clc                             ;dont error out
nopluslast:
	jmp     scan_parse
not_number:

	;;;     check for drive letter and modifier + or -

	and     al,NOT 20h              ;mask off shift bit
	cmp     al,'A'                  ;valid drive letter?
	jb      done_scanning_jmp       ;not recognized
	cmp     al,'Z'
	ja      done_scanning_jmp       ;not recognized
	sub     al,'A'                  ;A=0,B=1,...

	push    ax
	push    bx
	push    cx
	push    dx
	push    ds
	push    es
	xor     dx,dx
	mov     dl,al
	push    dx
	call    detect_stacker
	pop     dx

	or      ax,ax
	jz      no_stacker
	cmp     ax,200                  ;version 2?
	jb      no_stacker              ;version 1 ok to cache
	call    detect_stacker_volume
	cmp     ax,1
	jne     no_stacker
	pop     es
	pop     ds
	pop     dx
	pop     cx
       ;        pop     bx
       ;        pop     ax
	jmp     short dont_cache        
no_stacker:
	pop     es
	pop     ds
	pop     dx
	pop     cx
	pop     bx
	pop     ax
		  
	push    ax                      ;save drive letter      
	push    bx
	mov     dl,al                   ;check if drive is a valid
	xor     dh,dh                   ;cachable drive (not network)
	call    get_drive_type
	sub     ax,dosinfo
	
	cmp     ax,REMOTE_TYPE
	je      dont_cache
	cmp     ax,CDROM_TYPE
	je      dont_cache
	cmp     ax,INVALID_TYPE
	je      dont_cache
	cmp     ax,MAGIC_TYPE
	jne     popbxax
	jmp     magicspecial
popbxax:
dont_cache:                             ;leave 'e' set for test after pops
	pop     bx
	pop     ax                      ;restore drive letter
	je      cannot_cache_drive

	mov     cl,al                   ;al holds drive unit
	mov     dx,bx                   ;save parameter cursor
	call    get_next_char           ;checking for + or -
	jc      read_cache_enable       ;no more to read, no modifier there!
	cmp     al,'+'                  ;+ sign?
	je      write_cache_enable      ;means to enable write cache too
	cmp     al,'-'                  ;- sign?
	je      disable_all             ;meands to disable read/write cache
	mov     bx,dx                   ;restore parameter cursor
read_cache_enable:
	;;;     if resident already, we need to call resident guy
	cmp     cs:first_instance,0
	je      notify_resident_read_cache
	
	push    bx                      ;preserve text ptr offset
	mov     bl,cl                   ;cl is the unit number
	xor     bh,bh                   ;bx is unit number
	mov     drives_to_cache[bx],READ_CACHE ; set read cache only for unit
	pop     bx      
	clc
	jmp     scan_parse

notify_resident_read_cache:
	push    bp
	push    bx
	xor     ch,ch
	mov     bp,cx
	mov     dl,CACHE_DRIVE_READ_ENABLE
	mov     ax,BAMBI_CACHE_DRIVE
	push    ax
	push    bx      
	push    bp
	call    call_bambi
	pop     bp
	pop     bx
	pop     ax
	mov     dl,CACHE_DRIVE_WRITE_DISABLE
	call    call_bambi
	pop     bx 
	pop     bp
	clc
	jmp     scan_parse

write_cache_enable:
		;;;     if resident already, we need to call resident guy
	cmp     cs:first_instance,0
	je      notify_resident_write_cache
	
	push    bx                      ;preserve text ptr offset
	mov     bl,cl                   ;cl is the unit number
	xor     bh,bh                   ;bx is unit number
	mov     drives_to_cache[bx],WRITE_CACHE ; set read cache only for unit
	pop     bx      
	clc
	jmp     scan_parse

notify_resident_write_cache:
	push    bp
	push    bx
	xor     ch,ch
	mov     bp,cx
	mov     dl,CACHE_DRIVE_READ_ENABLE
	mov     ax,BAMBI_CACHE_DRIVE
	push    ax
	push    bx      
	push    bp
	call    call_bambi
	pop     bp
	pop     bx
	pop     ax
	mov     dl,CACHE_DRIVE_WRITE_ENABLE
	call    call_bambi
	pop     bx 
	pop     bp
	clc
	jmp     scan_parse


disable_all:
			;;;     if resident already, we need to call resident guy
	cmp     cs:first_instance,0
	je      notify_resident_no_cache
	
	push    bx                      ;preserve text ptr offset
	mov     bl,cl                   ;cl is the unit number
	xor     bh,bh                   ;bx is unit number
	mov     drives_to_cache[bx],NO_CACHE ; set read cache only for unit
	pop     bx      
	clc
	jmp     scan_parse

notify_resident_no_cache:
	push    bp
	push    bx
	xor     ch,ch
	mov     bp,cx
	mov     dl,CACHE_DRIVE_READ_DISABLE
	mov     ax,BAMBI_CACHE_DRIVE
	push    ax
	push    bx      
	push    bp
	call    call_bambi
	pop     bp
	pop     bx
	pop     ax
	mov     dl,CACHE_DRIVE_WRITE_DISABLE
	call    call_bambi
	pop     bx 
	pop     bp
	clc
	jmp     scan_parse



done_scanning:
	call    at_eol
	cmc
	jnc     nodisp
	jmp     display_help_text
nodisp:
	ret

reset_cache:
	push    bx
	call    init_bambi_io
	mov     ax,BAMBI_REINITIALIZE
	call    call_bambi
	pop     bx

	jmp     scan_parse

handle_switch:
	call    get_next_char
	jc      done_scanning
	or      al,32                   ;lower case
	cmp     al,FLUSH_FLAG
	je      no_colon
	cmp     al,RESET_FLAG
	je      reset_cache
	cmp	al,QUIET_FLAG
	je	set_quiet_flag
	cmp	al,VERBOSE_FLAG
	je	clr_quiet_flag
	cmp     al,LOAD_LOW_FLAG
	je      check_loadlow_flag
	cmp     al,COMMIT_FLAG
	jne     notwarning
	jmp     setcommitflag
notwarning:
	cmp     al,STATUS_FLAG
	jne     cont_swtch
	mov     extra_status,1
	jmp     display_status
check_loadlow_flag:
	mov     load_low,1
	jmp     scan_parse

set_quiet_flag:
	mov	be_quiet,1
	jmp	short cont_quiet_flag
clr_quiet_flag:
	mov	be_quiet, 0
cont_quiet_flag:
	call	get_next_char  ;;no characters allowed after /q or /v
	jc      done_scanning
	cmp	al,' '	       ;;must be a space after /q or /v, if anything
	jne     display_help_text
	jmp     scan_parse
cont_swtch:
	mov     dl,al

	call    get_next_char           ;get the ':' 
	jc      display_help_text
	cmp     al,':'

	mov     al,dl
	cmp     al,ELEMENTS_FLAG        ;is it the #elements flag?
	jne     check_size_flag         ;no--continue parsing

	;
	;       if bambi is already resident, we need
	;       to re-initialize resident bambi
	push    ax
	push    bx
	mov     ax,BAMBI_GET_STATS      ;getstatus also used for detection
	call    call_bambi
	cmp     ax,BAMBI_SIGNATURE
	pop     bx
	pop     ax
	jne     not_reinitialize
	jmp     reinitialize
not_reinitialize:

	call    parse_number
	jc      display_help_text
	mov     number_of_cache_elements,cx
	jmp     scan_parse
no_colon:
	call    init_bambi_io
	mov     ax,BAMBI_COMMIT_ALL
	call    call_bambi
	stc
	ret
display_help_text:
	mov     dx,offset cs:help_text
	mov     ah,9
	int     21h
	stc
	ret
check_size_flag:
	cmp     al,ELEMENT_SIZE_FLAG
	jne     check_buffer_flag
	call    parse_number
	jc      display_help_text

	;;;     we should ensure the element size is
	;;;     1024 <= >= 8192 and is a power of 2
	;;;     ie, 1024,2048,4096,8192
	;;;     
	cmp     cx,1024
	jbe     eis1024
	cmp     cx,2048
	jbe     eis2048
	cmp     cx,4096
	jbe     eis4096
	mov     cx,8192
	jmp     short eisset
eis1024:
	mov     cx,1024
	jmp     short eisset
eis2048:
	mov     cx,2048
	jmp     short eisset
eis4096:
	mov     cx,4096
eisset:
	
	mov     cs:cache_block_bytes,cx
	shr     cx,1
	mov     cs:cache_block_words,cx
	jmp     scan_parse

check_buffer_flag:
	cmp     al,BUFFER_FLAG
	jne     check_drive_flag
	call    parse_number
	jc      display_help_text

	mov     cs:target_buffer_size,cx
	jmp     scan_parse

check_drive_flag:
	cmp     al,DRIVE_FLAG
	jne     display_help_text
	jmp     display_help_text
	call    get_next_char
	jc      display_help_text       
	cmp     al,'a'
	jb      display_help_text ;bug bug correct error message
	cmp     al,'z'
	ja      display_help_text ;bug bug correct error message
	sub     al,'a'          ; a = 0, b=1

	;;;     if resident already, we need to call resident guy
	cmp     cs:first_instance,0
	je      notify_resident
	
	push    bx
	mov     bl,al
	xor     bh,bh
	mov     drives_to_cache[bx],1
	pop     bx      
	clc
	jmp     scan_parse

notify_resident:
	push    bp
	push    bx
	xor     ah,ah
	mov     bp,ax
	mov     dl,1
	mov     ax,BAMBI_CACHE_DRIVE
	call    call_bambi
	pop     bx 
	pop     bp
	clc
	jmp     scan_parse

reinitialize:
		
	call    parse_number
	jnc     number_ok
	jmp     display_help_text
number_ok:
	;;note CX holds new number of cache elements    
	push    cx
	call    init_bambi_io
	mov     ax,BAMBI_COMMIT_ALL
	call    call_bambi
	pop     cx

	mov     dx,0800h        ;BUG BUG this is hardcoded
	mov     ax,BAMBI_REINITIALIZE
	call    call_bambi
	stc
	ret
				
parse_command_line endp

setcommitflag:
	mov     cs:commitflag,0
	jmp     scan_parse

zseg ends

end
