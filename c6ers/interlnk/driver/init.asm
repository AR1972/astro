;***
;* $Workfile:   init.asm  $
;* $Revision:   1.5  $
;*   $Author:   Dave Sewell  $
;*     $Date:   08 Aug 1989 16:44:52  $
;***

                TITLE   Expansion Box Main Device Driver
                PAGE    66, 132


                INCLUDE drivers.mac
                INCLUDE packets.mac
                INCLUDE debug.mac


CORE            SEGMENT WORD PUBLIC 'CODE'
                EXTRN   C int2f_handler:FAR
                EXTRN   C int25_handler:FAR
                EXTRN   C int26_handler:FAR
                EXTRN   header:BYTE
                EXTRN   lpt3_header:BYTE
                EXTRN   lpt2_header:BYTE
                EXTRN   lpt1_header:BYTE
                EXTRN   strat_seg:WORD
                EXTRN   intr0_seg:WORD
                EXTRN   intr1_seg:WORD
                EXTRN   intr2_seg:WORD
                EXTRN   intr3_off:WORD
                EXTRN   intr3_seg:WORD
                EXTRN   end_low_stub:BYTE
                EXTRN   finger_print:BYTE

                EXTRN   setup_ports_parallel:NEAR
                EXTRN   setup_ports_serial:NEAR
                EXTRN   reset_ports_parallel:NEAR
                EXTRN   reset_ports_serial:NEAR

                EXTRN   intr_error:FAR
        IFDEF DEBUG
                EXTRN   debug_init:NEAR
                EXTRN   debug_msg:WORD
                EXTRN   hex_out:WORD
        ENDIF
                EXTRN   gri_proc:NEAR
                EXTRN   send_sync_proc:NEAR
                EXTRN   driver_call:DWORD
                EXTRN   header_seg:WORD
                EXTRN   driver_id:BYTE
                EXTRN   client_id:DWORD
                EXTRN   max_baud:BYTE
                EXTRN   client_max_baud:BYTE
                EXTRN   idle_semaphore:BYTE
                EXTRN   max_secsize:WORD
                EXTRN   dos_version:WORD
                EXTRN   dos_major:BYTE
                EXTRN   rhptr:DWORD
                EXTRN   bios_port_num:BYTE
                EXTRN   is_serial:BYTE
                EXTRN   port_address:WORD
                EXTRN   port_index:WORD
                EXTRN   int2f_ok:BYTE
                EXTRN   old_int2f_vec:DWORD
                EXTRN   old_int25_vec:DWORD
                EXTRN   old_int26_vec:DWORD
                EXTRN   num_ports:BYTE
                EXTRN   num_ser_ports:BYTE
                EXTRN   num_par_ports:BYTE
                EXTRN   serial_ports:BYTE
                EXTRN   parallel_ports:BYTE
                EXTRN   printer_loaded:BYTE
                EXTRN   parallel_start:BYTE
                EXTRN   par_fixup_start:WORD
                EXTRN   par_fixup_end:WORD
                EXTRN   device_high:BYTE
                EXTRN   init_size:WORD
                EXTRN   low_memory:BYTE
                EXTRN   driver_size:WORD
                EXTRN   initialized:BYTE
                EXTRN   display_scan:BYTE
                EXTRN   fx_force_variable:BYTE
                EXTRN   Virt_Dev_File:BYTE
                EXTRN   Win386_Startup_Info:Win386_Startup_Info_Struc
CORE            ENDS

BLOCK           SEGMENT WORD PUBLIC 'CODE'
                EXTRN   intr_vector:WORD
                EXTRN   normal_intr:NEAR
                EXTRN   block_start:BYTE
                EXTRN   block_init_end:BYTE
                EXTRN   bpb_array:WORD
                EXTRN   units:BYTE
                EXTRN   default_units:BYTE
                EXTRN   first_unit:BYTE
                EXTRN   dd_attributes:WORD
                EXTRN   max_devices:BYTE
                EXTRN   dispatch:WORD
                EXTRN   bpb_pnt_array:WORD
                EXTRN   lpt_unused:NEAR
BLOCK           ENDS

SERIAL          SEGMENT WORD PUBLIC 'CODE'
SERIAL          ENDS

PARALLEL        SEGMENT WORD PUBLIC 'CODE'
                EXTRN   par_fixup1:WORD
                EXTRN   par_fixup2:WORD
                EXTRN   par_fixup3:WORD
                EXTRN   par_fixup4:WORD
PARALLEL        ENDS

PRINTER         SEGMENT WORD PUBLIC 'CODE'
                EXTRN   prn_fixup:WORD
PRINTER         ENDS

                EXTRN   serial_start:BYTE
                EXTRN   printer_start:BYTE
                EXTRN   comma_space:BYTE
                EXTRN   invalid_switch:BYTE
                EXTRN   product_name:BYTE
                EXTRN   port_address_msg:BYTE
                EXTRN   port_address_loc:BYTE
                EXTRN   ser_port_name:BYTE
                EXTRN   ser_port_number:BYTE
                EXTRN   par_port_name:BYTE
                EXTRN   par_port_number:BYTE
                EXTRN   baud_msg:BYTE,           baud_loc:BYTE
                EXTRN   irq_msg:BYTE,            irq_loc:BYTE
                EXTRN   cr_lf:BYTE
                EXTRN   signon_default:BYTE,     signon_dfirst:BYTE
                EXTRN   signon_paren:BYTE
                EXTRN   signon_dlast:BYTE,       signon_ndrives:WORD
                EXTRN   printer_default:BYTE,    printer_dfirst:BYTE
                EXTRN   printer_paren:BYTE
                EXTRN   printer_dlast:BYTE,      printer_ndrives:BYTE
                EXTRN   msg_no_ports:BYTE
                EXTRN   msg_no_devices:BYTE
                EXTRN   too_big:BYTE
                EXTRN   m_bad_ser_addr:BYTE
                EXTRN   m_too_many_ser:BYTE
                EXTRN   m_bad_bios_ser:BYTE
                EXTRN   m_bad_par_addr:BYTE
                EXTRN   m_too_many_par:BYTE
                EXTRN   m_bad_bios_par:BYTE
                EXTRN   m_com_bad:BYTE
                EXTRN   m_lpt_bad:BYTE
                EXTRN   m_connect_try:BYTE
                EXTRN   m_connect_ok:BYTE
                EXTRN   m_connect_fail:BYTE
                EXTRN   m_baud_error:BYTE
                EXTRN   m_max_baud:BYTE

MAXCMD          equ 16                  ; Maximum allowed command code
                                        ; 12 for MS-DOS 2.x
                                        ; 16 for MS-DOS 3.0-3.1
                                        ; 24 for MS-DOS 3.2-3.3

TIMER_0         EQU 40H         ;8253 counter 0 port
TIMER_CTL       EQU 43H         ;8253 control port
TIMER_0_LATCH   EQU 00H         ;8253 cmd to save channel 0 current count

DOS30           EQU (3 SHL 8) + 30      ; MS-DOS 3.30 word format version number
DOS331          EQU (3 SHL 8) + 31      ; MS-DOS 3.31 word format version number
DOS40           EQU (4 SHL 8) + 00      ; MS-DOS 4.00 word format version number
DOS50           EQU (5 SHL 8) + 00      ; MS-DOS 5.00 word format version number


INIT            SEGMENT WORD PUBLIC 'CODE'
                PUBLIC  get_dos_version
                PUBLIC  show_crlf
                PUBLIC  init_end
                EXTRN   num_lpt:BYTE
                EXTRN   num_com:BYTE
                EXTRN   auto_str:BYTE
                EXTRN   spaces_str:BYTE
                EXTRN   none_str:BYTE
                EXTRN   printer_init:NEAR
                EXTRN   printer_signon:BYTE
                EXTRN   drives_error:BYTE
                EXTRN   no_drives:BYTE
                EXTRN   no_printers:BYTE
                EXTRN   m_prn_cl_letter:BYTE
                EXTRN   m_prn_se_letter:BYTE
                EXTRN   m_prn_map:BYTE
                EXTRN   m_lost_connect:BYTE
                EXTRN   actual_prn_map:BYTE
                EXTRN   packet_buf:BYTE
                EXTRN   m_map_header:BYTE
                EXTRN   m_drive_map:BYTE
                EXTRN   m_client_letter:BYTE
                EXTRN   m_server_letter:BYTE
                EXTRN   m_no_driver:BYTE
                EXTRN   send_pack:WORD
                EXTRN   recv_pack:WORD
                EXTRN   drive_mapping:BYTE

                ORG     0

                DB      "Copyright (C) 1989-1991 by Sewell Development Corp."

force_low       db      0               ; Set non-zero if forcing low memory
bpb_start       dw      ?
noscan          db      0
auto            db      0               ; TRUE if load only on connect
umb_allocated   db      0               ; Set TRUE if high mem block allocated

initialize      PROC    NEAR            ; function 0 = initialize driver
                PUBLIC  initialize

                mov     ax, cs
                mov     WORD PTR driver_call + 2, ax
                mov     header_seg, ax
        IFDEF DEBUG
                call    debug_init
        ENDIF
                call    signon

get_version:    call    get_dos_version ; Get DOS version number
                call    make_client_id  ; Get an arbitrary client ID
                les     di, rhptr
                call    get_equipment
                les     si, es:[di].init_req.init_cmd    ; es:si -> command line
                call    parse_args      ; Parse and process command line args
                jc      init_no_install

                call    get_max_secsize ; Get the maximum sector size on MASTER
                                        ; Also get first unit number

                mov     al, 'Z' - 'A' + 1   ; Calculate max devices
                sub     al, first_unit
                mov     max_devices, al
                mov     al, default_units
                cmp     al, max_devices ; See if we can use all of the default
                jbe     save_units      ; devices

                mov     al, max_devices ; If not enough device letters, use only
                                        ; those that are available

save_units:     mov     units, al       ; Save the number of units

; Force NON_IBM format.  This will avoid the initial read of the first
; sector of the FAT before a build BPB call.  The slave system will perform
; this read if necessary.
;
; Force OCRM, HUGE, & GEN_IOCTL.  We maintain count of the open files even
; if the
; slave driver doesn't.  We will always use HUGE addressing, the read and
; write I/O routines, convert the 32-bit sector numbers into the appropriate
; format for the slave system.  We must force HUGE because the other side
; may use HUGE addressing and we don't know about it (because communications
; couldn't be established above).  If we didn't, we might start addressing
; a HUGE disk on the slave system with small sectors -- which would destroy
; the disk!!!  On DOS versions before 3.31, this bit should be ignored!

                mov     header.device_header.attribute, ATT_NON_IBM OR ATT_OCRM OR ATT_HUGE OR ATT_GEN_IOCTL

;--- Load only those segments of code into memory which are really needed ---

                push    ds
                pop     es
                mov     di, DVR:block_init_end
                mov     ax, DVR:normal_intr
                sub     ax, DVR:intr_vector + 2
                mov     intr_vector, ax
                mov     di, DVR:serial_start
                cmp     num_ser_ports, 0
                je      copy_parallel

                mov     di, DVR:parallel_start

copy_parallel:  cmp     num_par_ports, 0
                je      copy_printer

                mov     si, DVR:parallel_start
                mov     cx, DVR:printer_start
                call    check_copy_code
                jc      copy_printer              ; Carry set if no copy needed

                sub     par_fixup1, ax
                sub     par_fixup2, ax
                sub     par_fixup3, ax
                sub     par_fixup4, ax
            rep movsw
                mov     si, DVR:par_fixup_start
                mov     cx, DVR:par_fixup_end
                call    fixup_table

copy_printer:   cmp     printer_loaded, 0
                je      fixup_bpbs

                mov     si, DVR:printer_start
                mov     cx, DVR:bpb_array
                call    check_copy_code
                jc      fixup_bpbs

            rep movsw
                sub     WORD PTR prn_fixup, ax

fixup_bpbs:     mov     bpb_start, di
                cmp     units, 0
                je      check_size

                mov     si, DVR:bpb_array
                cmp     si, di
                je      check_size

                mov     cx, (MAX_DEVICES * TYPE bios_parameter_block) / 2
            rep movsw

                mov     ax, bpb_start
                mov     cx, MAX_DEVICES
                mov     di, DVR:bpb_pnt_array

fix_bpb_pnt:    stosw
                add     ax, TYPE bios_parameter_block
                loop    fix_bpb_pnt

check_size:     mov     al, units       ; Now calculate length of BPB array
                mov     cl, TYPE bios_parameter_block
                mul     cl

                add     ax, bpb_start   ; Calculate Driver length
                mov     cx, ax          ; cx = break address offset

                add     ax, 000FH       ; Include all of last paragraph in
                and     ax, 0FFF0H      ; required space message

                cmp     dos_version, DOS50
                jl      save_size

                ; have determined that we are dos 5.0 or greater, so
                ; check end address to make sure we will not go past
                ; it with bpb array.

                mov     dx, cs
                mov     cl, 4
                shr     ax, cl
                add     dx, ax         
                        ; compare max address seg. to break seg (both
                        ; normalized) to see if we will fit
                les     di, rhptr
                mov     bx, WORD PTR es:[di].init_ans.init_end
                shr     bx, cl
                add     bx, WORD PTR es:[di].init_ans.init_end + 2
                cmp     dx, bx
                jbe     load_size_ok

                pmsg    too_big
                jmp     init_no_install ; Not enough room!

load_size_ok:   shl     ax, cl          ; restore ax to num bytes

save_size:      mov     driver_size, ax ; Save driver size in bytes
                mov     init_size, ax
                cmp     num_ports, 0
                jne     init_show_port

                pmsg    msg_no_ports
                jmp     init_no_install

init_show_port: cmp     units, 0
                jne     @F

                cmp     printer_loaded, 0
                jne     @F

                pmsg    msg_no_devices
                jmp     init_no_install

@@:             call    show_max_baud

                mov     bx, cs
                cmp     bx, 0A000H
                jb      in_low

                inc     device_high
                dec     low_memory      ; Set low_memory to 0
                jmp     short connect_scan

in_low:         cmp     force_low, 0
                jne     connect_scan

                cmp     dos_version, DOS50
                jb      connect_scan

                mov     bx, driver_size ; Get size (already rounded up)
                mov     cl, 4
                shr     bx, cl          ; Convert size to paragraphs
                mov     ah, 48H         ; Try to allocate UMB memory block
                int     21H
                jc      connect_scan    ; Failed - must be none available

                inc     umb_allocated
                mov     strat_seg, ax
                mov     intr0_seg, ax
                mov     intr1_seg, ax
                mov     intr2_seg, ax
                mov     intr3_seg, ax
                mov     WORD PTR driver_call + 2, ax
                dec     low_memory
                mov     init_size, (DVR:end_low_stub + 15)
                and     init_size, 0FFF0H
                dec     ax                      ; Point to MCB just in front
                mov     es, ax
                inc     ax                      ; Restore our segment
                mov     si, DVR:finger_print
                mov     di, 8
                mov     es:[di - 7], ax         ; Point "owner" word to us
                mov     cx, 4
            rep movsw
                mov     es, ax
                xor     si, si
                xor     di, di
                mov     cx, driver_size
                shr     cx, 1                   ; Change to word count
            rep movsw                           ; ===> Move the driver! <===
                mov     ds, ax                  ; Update DS with new driver seg

connect_scan:   call    setup_ports_parallel    ; Parallel setup
                call    setup_ports_serial      ; Serial setup
                cmp     cs:noscan, 0
                jne     init_show

                call    connect
                jnc     init_show               ; Connection succeeded

                cmp     cs:auto, 0              ; Connection failed
                je      init_show

                call    reset_ports_parallel
                call    reset_ports_serial
                jmp     init_no_install

init_show:      call    printer_init            ; Printer redirection setup
                call    show_units
                call    show_printers
                cmp     initialized, 0
                je      init_continue

                call    show_drive_map
                jc      init_continue

                call    show_prn_map

init_continue:  call    hook_int2f
                call    hook_int25_26

init_done:      mov     idle_semaphore, MINIMUM_TICKS + 1
                les     di, rhptr

;--- Unlink LPT drivers that aren't necessary.

                mov     ax, DVR:header
                cmp     units, 0
                jne     check_num_lpt

                mov     ax, -1              ; Cause block driver to be unlinked

check_num_lpt:  cmp     printer_loaded, 0
                je      load_only_one

                cmp     num_lpt, 0
                je      zero_lpts

                cmp     num_lpt, 1
                je      one_lpt

load_only_one:  mov     cs:lpt3_header.device_header.next_offset, ax
                cmp     printer_loaded, 0
                je      make_null

                cmp     num_lpt, 2
                je      init_ret_size

make_null:      mov     WORD PTR cs:lpt3_header.device_header.name_num, "UN"
                mov     WORD PTR cs:lpt3_header.device_header.name_num[2], "2L"
                mov     cs:intr3_off, DVR:lpt_unused
                jmp     short init_ret_size

zero_lpts:      mov     cs:lpt1_header.device_header.next_offset, ax
                jmp     short init_ret_size

one_lpt:        mov     cs:lpt2_header.device_header.next_offset, ax

init_ret_size:  mov     ax, init_size
                mov     WORD PTR es:[di].static_rhp.rhp_status, STATUS_DONE
                mov     WORD PTR es:[di].init_ans.init_end, ax
                mov     WORD PTR es:[di].init_ans.init_end + 2, cs   ; Break address
                cmp     init_size, 0
                jne     init_ret

;--- We are not going to load, so unlink the other drivers.

                mov     cs:lpt3_header.device_header.next_offset, -1
                cmp     cs:umb_allocated, 0
                je      init_ret

                mov     es, strat_seg
                mov     ah, 49H
                int     21H             ; Return allocated block in UMB

init_ret:       ret

init_no_install:mov     units, 0        ; and do not install driver
                mov     printer_loaded, 0
                mov     init_size, 0
                pmsg    m_no_driver
                jmp     init_done

initialize      ENDP

IFDEF DEBUG

dump_break      PROC    NEAR

                mov     ax, es
                DBG     ' '
                DBG     'r'
                DBG     'h'
                DBG     '='
                HEX     ah
                HEX     al
                DBG     ':'
                mov     ax, di
                HEX     ah
                HEX     al
                DBG     ' '
                DBG     'b'
                DBG     'a'
                DBG     '='
                mov     ax, WORD PTR es:[di].init_ans.init_end + 2
                HEX     ah
                HEX     al
                DBG     ':'
                mov     ax, WORD PTR es:[di].init_ans.init_end
                HEX     ah
                HEX     al
                ret

dump_break      ENDP
ENDIF

hook_int2f      PROC    NEAR

                mov     ax, 352FH
                int     21H
                mov     ax, es
                or      ax, bx
                jz      change_int2f

                inc     int2f_ok        ; Vector is not zero
                mov     WORD PTR old_int2f_vec, bx
                mov     WORD PTR old_int2f_vec + 2, es
                mov     ax, (INTERLNK_MULTIPLEX_ID SHL 8) OR 0
                mov     bl, 0
                mov     dx, 0FFFFH
                int     2FH
                cmp     al, 0FFH
                je      @F

                xor     bl, bl

@@:             inc     bl
                mov     driver_id, bl

change_int2f:   mov     dx, DVR:int2f_handler       ;DS:DX -> int2f_handler
                mov     ax, 252FH
                int     21H
                mov     WORD PTR Win386_Startup_Info.SIS_Virt_Dev_File_Ptr + 2, ds
                ret

hook_int2f      ENDP

hook_int25_26   PROC    NEAR

                mov     ax, 3525H
                int     21H
                mov     WORD PTR old_int25_vec, bx
                mov     WORD PTR old_int25_vec + 2, es
                mov     dx, DVR:int25_handler
                mov     ax, 2525H
                int     21H

                mov     ax, 3526H
                int     21H
                mov     WORD PTR old_int26_vec, bx
                mov     WORD PTR old_int26_vec + 2, es
                mov     dx, DVR:int26_handler
                mov     ax, 2526H
                int     21H

                ret

hook_int25_26   ENDP

signon          PROC    NEAR
                PUBLIC  signon

                pmsg    product_name
                ret

signon          ENDP


show_units      PROC    NEAR
                PUBLIC  show_units

                mov     al, units
                aam
                add     ax, "00"
                xchg    ah, al
                cmp     al, '0'
                jne     stuff_ndrives

                mov     al, ' '

stuff_ndrives:  mov     cs:signon_ndrives, ax
                cmp     units, 0
                jne     show_drives

                pmsg    no_drives
                jmp     short show_units_ret

show_drives:    mov     bl, first_unit  ; Setup default drive mapping message
                add     bl, 'A'
                mov     cs:signon_dfirst, bl
                add     bl, units
                dec     bl
                mov     cs:signon_dlast, bl
                cmp     units, 1
                jne     show

                mov     WORD PTR cs:signon_paren, 0D29H         ; ")\r"
                mov     WORD PTR cs:signon_paren + 2, 240AH     ; "\n$"

show:           pmsg    signon_default

show_units_ret: ret

show_units      ENDP


show_printers   PROC    NEAR
                PUBLIC  show_printers

                cmp     printer_loaded, 0
                jne     have_printers

                pmsg    no_printers
                jmp     short show_pr_ret

have_printers:  mov     al, 3
                sub     al, num_lpt
                mov     ah, al              ; Save number of printers in AH
                add     al, '0'
                mov     cs:printer_ndrives, al
                mov     bl, 3
                sub     bl, ah
                add     bl, '1'
                mov     cs:printer_dfirst, bl
                add     bl, ah
                dec     bl
                mov     cs:printer_dlast, bl
                cmp     ah, 1
                jne     show_pr

                mov     WORD PTR cs:printer_paren, 0D29H        ; ")\r"
                mov     WORD PTR cs: printer_paren + 2, 240AH   ; "\n$"

show_pr:        pmsg    printer_default

show_pr_ret:    ret

show_printers   ENDP


connect         PROC    NEAR
                PUBLIC  connect

                pmsg    m_connect_try
                inc     display_scan
                mov     ax, DVR:send_sync_proc
                call    driver_call
                dec     display_scan    ;"dec" DOES NOT AFFECT CARRY FLAG
                jc      connect_fail

                mov     initialized, 0
                mov     ax, DVR:gri_proc
                call    driver_call
                jc      connect_fail

show_ok:        pmsg    m_connect_ok
                mov     bx, port_index
                cmp     is_serial, 0
                jne     connect_ser

                call    show_port_par
                jmp     short connect_ok

connect_ser:    call    show_port_ser

connect_ok:     call    show_crlf
                mov     idle_semaphore, 0       ; Turn off idler for now
                clc
                jmp     short connect_done

connect_fail:   pmsg    m_connect_fail
                stc

connect_done:   ret

connect         ENDP


print_asciiz    PROC    NEAR

                push    dx

print_loop:     lodsb
                or      al, al
                jz      blank_fill

                mov     ah, 2
                mov     dl, al
                int     21H
                loop    print_loop

blank_fill:     jcxz    print_done
                
blank_loop:     mov     ah, 2
                mov     dl, ' '
                int     21H
                loop    blank_loop

print_done:     pop     dx
                ret

print_asciiz    ENDP

print_msg       PROC    NEAR
                PUBLIC  print_msg

                push    ds
                push    cs
                pop     ds
                mov     ah, 9
                int     21H
                pop     ds
                ret

print_msg       ENDP

show_drive_map  PROC    NEAR
                PUBLIC  show_drive_map

                pmsg    m_map_header
                mov     cl, units
                xor     ch, ch
                or      cx, cx
                jz      show_map_ret

                xor     bx, bx

show_unit_loop: mov     al, bl
                add     al, first_unit
                add     al, 'A'
                mov     cs:m_client_letter, al
                mov     al, drive_mapping[bx]
                cmp     al, MAX_DEVICES
                jae       next_unit

                add     al, 'A'
                mov     cs:m_server_letter, al
                pmsg    m_drive_map

                push    bx
                push    cx
                mov     packet_buf.common_packet.packet_type, DRIVE_INFO_REQ
                mov     al, drive_mapping[bx]
                mov     packet_buf.drive_info_r.dir_unit, al
                mov     cx, TYPE drive_info_r
                mov     si, DVR:packet_buf
                push    ds
                pop     es
                mov     ax, send_pack   ; Verify that send_pack preserves ES
                call    driver_call
                jc      show_map_err

                mov     di, DVR:packet_buf
                mov     cx, MAX_PACKET
                mov     ax, recv_pack
                call    driver_call
                jc      show_map_err

                mov     si, DVR:packet_buf.drive_info_a.dia_size
                mov     cx, 8
                call    print_asciiz

                mov     si, DVR:packet_buf.drive_info_a.dia_vol_label
                mov     cx, 12
                call    print_asciiz
                call    show_crlf
                pop     cx
                pop     bx

next_unit:      inc     bx
                dec     cx
                jcxz    show_map_ret

                jmp     show_unit_loop
                
show_map_ret:   clc
                ret

show_map_err:   pop     cx
                pop     bx
                pmsg    m_lost_connect
                stc
                ret

show_drive_map  ENDP


show_prn_map    PROC    NEAR
                PUBLIC  show_prn_map
    
                cmp     printer_loaded, 0
                je      show_prn_ret

                mov     cx, 3
                mov     bx, 0

prn_loop:       mov     al, actual_prn_map[bx]
                cmp     al, 0FFH
                je      prn_next

                add     al, '1'
                mov     cs:m_prn_se_letter, al
                mov     al, bl
                add     al, '1'
                mov     cs:m_prn_cl_letter, al
                pmsg    m_prn_map

prn_next:       inc     bx
                loop    prn_loop

show_prn_ret:   ret

show_prn_map    ENDP


fixup_table     PROC    NEAR

; Enter with AX = fixup value, SI = start of table, CX = end of table
; Destroys BX, SI

                xor     bx, bx

table_loop:     cmp     [si], bx
                je      advance

                sub     [si], ax

advance:        add     si, 2
                cmp     si, cx
                jb      table_loop

                ret

fixup_table     ENDP

check_copy_code PROC    NEAR

                sub     cx, si          ; CX = number of bytes to move
                mov     ax, si
                sub     ax, di          ; Leave fixup value in AX
                jz      no_copy

                shr     cx, 1
                clc                     ; Indicate copy needs to be done
                jmp     short copy_done

no_copy:        add     di, cx
                stc

copy_done:      ret

check_copy_code ENDP

get_equipment   PROC    NEAR

                int     11H
                push    ax
                rol     ax, 1
                rol     ax, 1
                and     al, 3
                mov     num_lpt, al
                cmp     al, 3
                jne     @F

                mov     printer_loaded, 0

@@:             pop     ax
                mov     cl, 9
                shr     ax, cl
                and     al, 7
                cmp     al, 4
                jbe     ser_count_ok

                mov     al, 4

ser_count_ok:   mov     num_com, al
                ret

get_equipment   ENDP

baud_buff       db      "115200"
baud_buff_tail  EQU     $ - 1
                db      CR, LF, '$'

show_max_baud   PROC    NEAR
                PUBLIC  show_max_baud

                mov     bl, client_max_baud
                cmp     bl, BAUD_115200
                jb      show_max

                mov     client_max_baud, BAUD_115200
                jmp     short show_max_ret

show_max:       pmsg    m_max_baud
                xor     bh, bh
                mov     dl, cs:baud_table_high[bx]
                xor     dh, dh
                shl     bx, 1
                mov     ax, cs:baud_table_low[bx]
                mov     di, DVR:baud_buff_tail

            	mov     bx, ax      ;DX:BX = value
                mov     cx, 10      ;Get radix to CX

next_digit:     xchg    ax,dx       ;Divide hi
	            xor     dx,dx
	            div     cx          ;DX = rem, AX = hi div
                xchg    ax,bx       ;AX = lo,  BX = hi div
	            div     cx          ;DX = rem, BX:AX = div
	            xchg    ax,dx       ;AX = rem, BX:DX = div
	            xchg    dx,bx       ;AX = rem, DX:BX = div
	            add     al,'0'
                mov     cs:[di], al
                dec     di
	            mov     ax,dx
	            or      ax,bx
	            jnz     next_digit

                inc     di
                mov     dx, di
                call    print_msg

show_max_ret:   ret

show_max_baud   ENDP


get_dos_version PROC    NEAR            ; Gets dos version numbers

                mov     ax, 3000H       ; Get the dos version number
                int     21H

                xchg    al, ah          ; Put version number in WORD format

                cmp     ax, DOS30       ; Make special check for Zenith DOS 3.30+
                jne     @F

                cmp     bh, 5           ; 5 is Zeniths OEM number
                jne     @F

                mov     ax, DOS331      ; Pretend Zenith-DOS 3.30+ is DOS 3.31

@@:             mov     dos_version, ax
                ret

get_dos_version ENDP

toupper         PROC    NEAR
                PUBLIC  toupper

                cmp     al, 'a'
                jb      toupper_ret

                cmp     al, 'z'
                ja      toupper_ret

                sub     al, 'a' - 'A'

toupper_ret:    ret

toupper         ENDP

arg_table       db      3, 'COM'
                dw      DVR:set_com_port
                db      3, 'LPT'
                dw      DVR:set_lpt_port
                db      3, 'LOW'
                dw      DVR:set_force_low
                db      6, 'DRIVES'
                dw      DVR:set_num_drives
                db      9, 'NOPRINTER'
                dw      DVR:set_noprinter
                db      6, 'NOSCAN'
                dw      DVR:set_noscan
                db      4, 'AUTO'
                dw      DVR:set_auto
                db      4, 'BAUD'
                dw      DVR:set_baud
                db      1, 'V'
                dw      DVR:set_variable
                db      0

parse_line      PROC    NEAR
                PUBLIC  parse_line

                push    si

convert_upper:  mov     al, es:[si]
                cmp     al, ' '
                jb      convert_done

                call    toupper
                mov     es:[si], al
                inc     si
                jmp     convert_upper

convert_done:   pop     si

grab_char:      mov     al, es:[si]
                inc     si
                cmp     al, '/'
                je      found_arg

                cmp     al, '-'
                je      found_arg

                cmp     al, ' '
                je      grab_char

                jb      parse_exit

                mov     di, si
                jmp     short bad_option

found_arg:      mov     di, si
                mov     si, DVR:arg_table
                xor     ch, ch

arg_loop:       lodsb   
                or      al, al
                jz      bad_option

                mov     cl, al
                push    di
        repe    cmpsb
                mov     bl, es:[di]         ; Grab next character
                pop     di
                jne     no_match

                cmp     bl, 'A'
                jb      arg_match

                cmp     bl, 'Z'
                ja      arg_match

no_match:       add     si, cx
                add     si, 2
                jmp     arg_loop

arg_match:      mov     cl, al
                add     di, cx              ; Point past the arg
                lodsw
                mov     si, di
                call    ax
                jmp     grab_char

bad_option:     pmsg    invalid_switch

                mov     dl, es:[di - 1]
                mov     ah, 2
                int     21H

unrecog_loop:   mov     al, es:[di]
                cmp     al, '0'
                jb      unrecog_done

                mov     dl, al
                mov     ah, 2
                int     21H
                inc     di
                jmp     unrecog_loop

unrecog_done:   call    show_crlf
                stc
                jmp     short parse_ret

parse_exit:     clc

parse_ret:      ret

parse_line      ENDP

default_cl      DB      '/LPT* /COM*', CR

get_exe_name    PROC    NEAR

                push    di
                mov     di, DVR:Virt_Dev_File

;--- Skip any leading white space

skip_white:     mov     al, es:[si]
                cmp     al, ' '
                ja      skip_done

                or      al, al
                jz      get_exe_done

                inc     si
                jmp     skip_white

skip_done:      cmp     BYTE PTR es:[si + 1], ':'
                jne     unknown_drive

                mov     ah, es:[si + 1]
                mov     [di], ax
                add     di, 2
                add     si, 2
                jmp     check_root

unknown_drive:  
;                mov     WORD PTR [di], ":?"
                mov     ah, 19H             ;===> Can we really do this? <===
                int     21H
                add     al, 'A'
                mov     ah, ':'
                mov     WORD PTR [di], ax
                add     di, 2                

check_root:     cmp     BYTE PTR es:[si], '\'
                je      copy_exe_name

                mov     BYTE PTR [di], '\'
                inc     di

copy_exe_name:  mov     al, es:[si]
                cmp     al, '/'
                je      get_exe_done

                cmp     al, ' '
                jbe     get_exe_done

                mov     [di], al
                inc     di
                inc     si
                jmp     copy_exe_name

get_exe_done:   pop     di
                ret

get_exe_name    ENDP

parse_args      PROC    NEAR

                call    get_exe_name

IFDEF DEBUG
                push    si
                mov     si, DVR:Virt_Dev_File
                DBG     '<'
                DBG     '<'
                DBG     '<'

show_loop:      lodsb
                or      al, al
                jz      show_done

                DBG     al
                jmp     show_loop

show_done:
                DBG     '>'
                DBG     '>'
                DBG     '>'
                pop     si
ENDIF

;--- Skip any leading white space after .EXE file name

skip_white:     mov     al, es:[si]
                cmp     al, ' '
                jne     skip_non_white

                inc     si
                jmp     skip_white

skip_non_white: 
IFDEF OLD_CODE
                mov     al, es:[si]
                cmp     al, ' '
                jb      parse_default

                je      do_parse

                inc     si
                jmp     skip_non_white
ENDIF

do_parse:       call    parse_line
                jc      parse_fail

                cmp     num_ports, 0
                jne     parse_ok

parse_default:  push    cs
                pop     es
                mov     si, DVR:default_cl
                call    parse_line
                jmp     short parse_done

parse_ok:       clc
                jmp     short parse_done

parse_fail:     stc

parse_done:     ret

parse_args      ENDP

show_port_ser   PROC    NEAR

; Enter with BX = index into serial_ports[]

                mov     al, serial_ports[bx].SERIAL_PORT_DEF.sp_biosnum
                or      al, al
                jz      not_ser_bios

                add     al, '0'
                mov     cs:ser_port_number, al
                pmsg    ser_port_name
                jmp     short show_sp_done

not_ser_bios:   mov     cs:ser_port_number, '$'
                pmsg    ser_port_name
                mov     ax, serial_ports[bx].SERIAL_PORT_DEF.sp_address
                call    show_address

show_sp_done:   ret

show_port_ser   ENDP


show_port_par   PROC    NEAR

                mov     al, parallel_ports[bx].PARALLEL_PORT_DEF.pp_biosnum
                or      al, al
                jz      not_par_bios

                add     al, '0'
                mov     cs:par_port_number, al
                pmsg    par_port_name
                jmp     short show_pp_done

not_par_bios:   mov     cs:par_port_number, '$'
                pmsg    par_port_name
                mov     ax, parallel_ports[bx].PARALLEL_PORT_DEF.pp_address
                call    show_address

show_pp_done:   ret

show_port_par   ENDP


is_alpha        PROC    NEAR
                PUBLIC  is_alpha

                cmp     al, 'A'
                jb      out_of_range

                cmp     al, 'Z'
                jbe     in_range

                cmp     al, 'z'
                ja      out_of_range

                cmp     al, 'a'
                jb      out_of_range

in_range:       clc
                jmp     short alpha_ret

out_of_range:   stc

alpha_ret:      ret

is_alpha        ENDP

find_arg        PROC    NEAR

; Skip the optional colon (or equal sign) and point at the numeric argument
; (if present).
; Error if next character is alphabetic.
; Error if end of command line (< ' ') or '/' seen before argument.

                mov     al, es:[si]
                cmp     al, ':'
                je      colon_or_equal

                cmp     al, '='
                je      colon_or_equal

                jmp     short grab_first

colon_or_equal: inc     si

grab_first:     mov     al, es:[si]
                call    is_alpha
                jnc     arg_error

                clc
                jmp     short arg_ret

arg_error:      stc
                
arg_ret:        ret

find_arg        ENDP

find_port_arg   PROC    NEAR

                mov     al, es:[si]
                cmp     al, ' '
                jbe     fpa_ok                  ; /COM or /LPT is OK

                call    find_arg
                jc      fpa_fail

                mov     al, es:[si]
                cmp     al, '*'
                je      fpa_incr

                cmp     al, '0'
                jb      fpa_fail

                cmp     al, '9'
                jbe     fpa_ok

fpa_fail:       stc
                jmp     short fpa_ret

fpa_incr:       inc     si

fpa_ok:         clc

fpa_ret:        ret

find_port_arg   ENDP

set_force_low   PROC    NEAR

                mov     force_low, 1
                ret

set_force_low   ENDP

set_num_drives  PROC    NEAR

                mov     ax, es:[si]
                cmp     al, ':'
                je      colon_equal

                cmp     al, '='
                je      colon_equal

                jmp     short set_nd_error

colon_equal:    cmp     ah, '0'
                jb      set_nd_error

                cmp     ah, '9'
                ja      set_nd_error

                inc     si
                call    decimal_number
                jc      set_nd_error

                cmp     ax, MAX_DEVICES     ; #### Can we really do this many?
                ja      set_nd_error

                mov     default_units, al
                jmp     short snd_ret

set_nd_error:   pmsg    drives_error

snd_ret:        ret

set_num_drives  ENDP

baud_table_low  dw      1200                ;   1200 baud   (index 0)     
                dw      2400                ;   2400 baud   (index 1)
                dw      4800                ;   4800 baud   (index 2)
                dw      9600                ;   9600 baud   (index 3)
                dw      19200               ;  19200 baud   (index 4)
                dw      38400               ;  38400 baud   (index 5)
                dw      57600               ;  57600 baud   (index 6)
                dw      49664               ; 115200 baud   (index 7)
NUM_BAUD_VALUES EQU     (($ - baud_table_low) / 2)

baud_table_high db      0                   ;   1200 baud   (index 0) 
                db      0                   ;   2400 baud   (index 1)
                db      0                   ;   4800 baud   (index 2)
                db      0                   ;   9600 baud   (index 3)
                db      0                   ;  19200 baud   (index 4)
                db      0                   ;  38400 baud   (index 5)
                db      0                   ;  57600 baud   (index 6)
                db      1                   ; 115200 baud   (index 7)

set_baud        PROC    NEAR
                PUBLIC  set_baud

                mov     ax, es:[si]
                inc     si
                cmp     al, ':'
                je      sb_colon_equal

                cmp     al, '='
                jne     set_baud_error

sb_colon_equal: cmp     ah, '0'
                jb      set_baud_error

                cmp     ah, '9'
                ja      set_baud_error

                call    decimal_number
                jc      set_baud_error

                push    es
                push    cs
                pop     es
                mov     di, DVR:baud_table_low + (2 * 3)
                mov     cx, NUM_BAUD_VALUES
        repne   scasw
                pop     es
                jne     set_baud_error

                sub     di, DVR:baud_table_low + 2
                mov     bx, di
                shr     bx, 1
                cmp     dl, cs:baud_table_high[bx]
                jne     set_baud_error

                mov     client_max_baud, bl
                mov     max_baud, bl
                jmp     short set_baud_ret

set_baud_error: pmsg    m_baud_error

set_baud_ret:   ret

set_baud        ENDP

set_noscan      PROC    NEAR

                inc     noscan
                ret

set_noscan      ENDP


set_auto        PROC    NEAR

                inc     auto
                ret

set_auto        ENDP


set_variable    PROC    NEAR

                inc     fx_force_variable
                ret

set_variable    ENDP


set_noprinter   PROC    NEAR

                mov     printer_loaded, 0
                ret

set_noprinter   ENDP


set_com_port    PROC    NEAR

                call    find_port_arg
                jc      com_bad

                cmp     al, ' '
                jbe     com_wild

                cmp     al, '*'
                jne     com_not_wild

com_wild:       mov     cl, num_com
                xor     ch, ch
                jcxz    c_get_port_ret

                mov     bios_port_num, 1

com_wild_loop:  mov     al, bios_port_num
                xor     ah, ah
                call    set_port_ser
                inc     bios_port_num
                loop    com_wild_loop

                jmp     short c_get_port_ret

com_not_wild:   call    hex_number
                call    set_port_ser
                jmp     short c_get_port_ret

com_bad:        pmsg    m_com_bad

c_get_port_ret: ret

set_com_port    ENDP


set_lpt_port    PROC    NEAR

                call    find_port_arg
                jc      lpt_bad

                cmp     al, ' '
                jbe     lpt_wild

                cmp     al, '*'
                jne     lpt_not_wild

lpt_wild:       mov     cl, num_lpt
                xor     ch, ch
                jcxz    l_get_port_ret

                mov     bios_port_num, 1

lpt_wild_loop:  mov     al, bios_port_num
                xor     ah, ah
                call    set_port_par
                inc     bios_port_num
                loop    lpt_wild_loop

                jmp     short l_get_port_ret

lpt_not_wild:   call    hex_number
                call    set_port_par
                jmp     short l_get_port_ret

lpt_bad:        pmsg    m_lpt_bad

l_get_port_ret: ret

set_lpt_port    ENDP


hex_number      PROC    NEAR

; Input:
;   ES:SI pointing at hexadecimal string
;
; Output:
;   AX = binary value
;

                xor     ax, ax          ; Clear accumulator
                mov     cl, 4

hex_loop:       mov     bl, es:[si]     ; Get the next digit
                cmp     bl, '0'         ; Check range
                jb      hex_done

                cmp     bl, 'a'
                jb      check_hex

                sub     bl, 'a' - 'A'   ; Convert to upper case

check_hex:      cmp     bl, '9'
                jbe     is_digit

                cmp     bl, 'F'
                ja      hex_done

                cmp     bl, 'A'
                jb      hex_done

                sub     bl, '7'
                jmp     short add_digit

is_digit:       sub     bl, '0'

add_digit:      shl     ax, cl
                or      al, bl

                inc     si
                jmp     hex_loop

hex_done:       ret

hex_number      ENDP


decimal_number  PROC    NEAR

; Input:
;   ES:SI pointing at decimal string
;
; Output:
;   DX:AX = decimal value
;
; NOTE:  This routine stops scanning after it overflows into DX.  It can
; handle numbers as big as 655350 before it stops scanning.
;

                xor     ax, ax          ; Clear accumulator
                xor     bx, bx
                xor     dx, dx
                mov     di, 10          ; Base 10 multiplier

decimal_loop:   mov     bl, es:[si]     ; Get the next digit
                cmp     bl, '0'         ; Check range
                jb      decimal_ret

                cmp     bl, '9'
                ja      decimal_ret

                sub     bl, '0'         ; It's a digit, so normalize it
                or      dx, dx
                jnz     dec_overflow

                mul     di              ; Multiply previous value by 10
                add     ax, bx          ; Add in new 'ones' value
                adc     dx, 0
                inc     si
                jmp     decimal_loop

decimal_ret:    clc
                ret

dec_overflow:   stc
                ret

decimal_number  ENDP


validate_port   PROC    NEAR

                test    ax, 3
                jnz     bad_port

                cmp     ax, 200H
                jb      bad_port

                cmp     ax, 8000H
                jae     bad_port

                clc
                jmp     short vp_ret

bad_port:       stc

vp_ret:         ret

validate_port   ENDP

set_port_ser    PROC    NEAR

                push    cx
                or      ax, ax
                jz      bad_ser_addr

                cmp     ax, 4
                ja      ser_do_set

                cmp     al, num_com
                ja      bad_bios_ser

                mov     bios_port_num, al
                push    es
                mov     bx, 40H
                mov     es, bx
                dec     ax
                mov     di, ax
                shl     di, 1               ; Serial table at 40:0
                mov     ax, es:[di]
                pop     es

ser_do_set:     call    validate_port
                jc      bad_ser_addr

                xor     bx, bx
                mov     cl, 0

walk_ser:       cmp     cl, num_ser_ports
                jae     new_ser

                cmp     ax, serial_ports[bx].SERIAL_PORT_DEF.sp_address
                je      set_port_ret                ; Ignore duplicate address

                inc     cl
                add     bx, TYPE SERIAL_PORT_DEF
                jmp     walk_ser

new_ser:        cmp     num_ser_ports, MAX_SERIAL_PORTS
                jae     too_many_ser

                mov     serial_ports[bx].SERIAL_PORT_DEF.sp_address, ax
                push    es
                push    di
                xor     di, di
                mov     cx, 40H
                mov     es, cx
                mov     cl, num_com
                xor     ch, ch
        repne   scasw
                mov     ax, di
                pop     di
                pop     es
                je      save_ser_bios

                xor     ax, ax

save_ser_bios:  shr     al, 1
                mov     serial_ports[bx].SERIAL_PORT_DEF.sp_biosnum, al
                inc     num_ser_ports
                inc     num_ports
                jmp     short set_port_ret

bad_ser_addr:   push    ax
                pmsg    m_bad_ser_addr
                jmp     short show_hex_addr

bad_bios_ser:   push    ax
                pmsg    m_bad_bios_ser
                jmp     short show_hex_addr

too_many_ser:   push    ax
                pmsg    m_too_many_ser

show_hex_addr:  pop     ax
                call    show_hex
                call    show_crlf

set_port_ret:   pop     cx
                ret

set_port_ser    ENDP

set_port_par    PROC    NEAR

                push    cx
                or      ax, ax
                jz      bad_par_addr

                cmp     ax, 3
                ja      par_do_set

                cmp     al, num_lpt
                ja      bad_bios_par

                mov     bios_port_num, al
                push    es
                mov     bx, 40H
                mov     es, bx
                dec     ax
                mov     di, ax
                shl     di, 1               ; Serial table at 40:0
                add     di, 8               ; Parallel table at 40:8
                mov     ax, es:[di]
                pop     es

par_do_set:     call    validate_port
                jc      bad_par_addr

                xor     bx, bx
                mov     cl, 0

walk_par:       cmp     cl, num_par_ports
                jae     new_par

                cmp     ax, parallel_ports[bx].PARALLEL_PORT_DEF.pp_address
                je      par_sp_ret          ; Ignore duplicate address

                inc     cl
                add     bx, TYPE PARALLEL_PORT_DEF
                jmp     walk_par

new_par:        cmp     num_par_ports, MAX_PARALLEL_PORTS
                jae     too_many_par

                mov     parallel_ports[bx].PARALLEL_PORT_DEF.pp_address, ax
                push    es
                push    di
                mov     di, 8
                mov     cx, 40H
                mov     es, cx
                mov     cl, num_lpt
                xor     ch, ch
        repne   scasw
                mov     ax, di
                pop     di
                pop     es
                je      save_par_bios

                mov     al, 8

save_par_bios:  sub     al, 8
                shr     al, 1
                mov     parallel_ports[bx].PARALLEL_PORT_DEF.pp_biosnum, al
                inc     num_par_ports
                inc     num_ports
                jmp     short par_sp_ret

bad_par_addr:   push    ax
                pmsg    m_bad_par_addr
                jmp     short show_hex_par

bad_bios_par:   push    ax
                pmsg    m_bad_bios_par
                jmp     short show_hex_par

too_many_par:   push    ax
                pmsg    m_too_many_par

show_hex_par:   pop     ax
                call    show_hex
                call    show_crlf

par_sp_ret:     pop     cx
                ret

set_port_par    ENDP

show_crlf       PROC    NEAR

                pmsg    cr_lf
                ret

show_crlf       ENDP

show_hex        PROC    NEAR

                push    bx
                push    cx
                mov     bx, 3
                mov     cl, 4

skip_zero_loop: test    ah, 0F0H
                jnz     show_digits

                rol     ax, cl
                dec     bx
                jnz     skip_zero_loop

show_digits:    inc     bx

show_dig_loop:  rol     ax, cl
                mov     dl, al
                and     dl, 0FH
                add     dl, '0'
                cmp     dl, '9'
                jbe     show_dig

                add     dl, 7

show_dig:       call    show_char
                dec     bx
                jnz     show_dig_loop

                pop     cx
                pop     bx
                ret

show_hex        ENDP

show_address    PROC    NEAR

                mov     dl, '('
                call    show_char
                call    show_hex
                mov     dl, ')'
                call    show_char
                ret

show_address    ENDP

show_char       PROC    NEAR
                PUBLIC  show_char

                push    ax
                mov     ah, 2
                int     21H
                pop     ax
                ret

show_char       ENDP

IFDEF   DEBUG
wait_message    db      LF, "Hit any key to continue. . .$"

wait_for_key    PROC    NEAR

                pmsg    wait_message

                mov     ah, 08H         ; Wait for any key
                int     21H

                call    show_crlf

                ret

wait_for_key    ENDP
ENDIF

; get_max_secsize finds the largest allowable sector size on the MASTER
; system.  Any device that has a sector size larger that max_secsize on the
; slave system cannot be used!  This subroutine also determines the first
; unit number!
; 
get_max_secsize PROC    NEAR

                mov     ah, 52H         ; Get DOS's list of lists
                int     21H

                ; Use DOS 3.0+ offsets

                mov     ax, es:[bx + 10H]   ; Max sector size
                mov     cl, es:[bx + 20H]   ; First unit

                mov     max_secsize, ax
                mov     first_unit, cl

                ret

get_max_secsize ENDP

read_timer      PROC    NEAR

                pushf
                cli
                mov     al, TIMER_0_LATCH
                out     TIMER_CTL, al   ;Latch current count in 8253
                jmp     short $+2       ;Insure 5 clocks for 8253 recovery time
                in      al, TIMER_0     ;Get low order byte
                mov     ah, al          ;Save it in AH
                jmp     short $+2       ;Insure 5 clocks for 8253 recovery time
                in      al, TIMER_0     ;Get high order byte
                popf
                ret

read_timer      ENDP

; make_client_id gets an arbitrary non-zero number and places it in
; client_id.  This value is used to identify this image of the device driver
; to the server system.  If the ID's don't match in a SERVER_INFO exchange,
; the initialized flag will be cleared to force a re-initialization.
;
make_client_id  PROC    NEAR

                push    bx
                push    cx
                push    dx
                push    si
                push    es
                mov     si, 40H
                mov     es, si
                xor     si, si
                mov     cx, 64

; Build a 32-bit machine ID by XORing the first 64 DWORDS in the BIOS data
; area into DX:BX.  Let DX:BX start with whatever happens to be in them at
; the time.

build_loop:     lodsw
                xor     bx, ax
                lodsw
                xor     dx, ax
                loop    build_loop

                call    read_timer      ; Now XOR timer value into high word
                xor     bx, ax
                call    read_timer      ; and again into low word
                xor     dx, ax

                or      bx, dx
                jnz     @F

                inc     bx              ; insure != 0

@@:             mov     word ptr client_id, bx
                mov     word ptr client_id + 2, dx
                pop     es
                pop     si
                pop     dx
                pop     cx
                pop     bx
                ret

make_client_id ENDP

init_end        LABEL   BYTE

INIT            ENDS

                END
