;***
;* $Workfile:   core.asm  $
;* $Revision:   1.5  $
;*   $Author:   Dave Sewell  $
;*     $Date:   08 Aug 1989 16:44:52  $
;***

                INCLUDE drivers.mac
                INCLUDE packets.mac
                INCLUDE debug.mac

DRIVER_ATTRIB   = ATT_BLOCK     ; Device driver type
LPT_ATTRIB      = ATT_CHARACTER OR ATT_OCRM

PARALLEL        SEGMENT WORD PUBLIC 'CODE'
                EXTRN   send_pack_parallel:NEAR
                EXTRN   recv_pack_parallel:NEAR
                EXTRN   send_sync_parallel:NEAR
                EXTRN   save_parallel:NEAR
                EXTRN   restore_parallel:NEAR
                EXTRN   timer_passthru_par:FAR
                EXTRN   ss_timeout:NEAR
                EXTRN   ssp1:NEAR
                EXTRN   ssp2:NEAR
                EXTRN   ssp3:NEAR
                EXTRN   sw_timeout:NEAR
                EXTRN   swp1:NEAR
                EXTRN   swp2:NEAR
                EXTRN   sp_timeout:NEAR
                EXTRN   spp2:NEAR
                EXTRN   spp3:NEAR
                EXTRN   spp4:NEAR
                EXTRN   spn_timeout:NEAR
                EXTRN   spn2:NEAR
                EXTRN   spn3:NEAR
                EXTRN   spn4:NEAR
                EXTRN   rw_timeout:NEAR
                EXTRN   rwp1:NEAR
                EXTRN   rwp2:NEAR
                EXTRN   rp_timeout:NEAR
                EXTRN   rpp1:NEAR
                EXTRN   rpp2:NEAR
                EXTRN   rpp3:NEAR
                EXTRN   rpn_timeout:NEAR
                EXTRN   rpn1:NEAR
                EXTRN   rpn2:NEAR
                EXTRN   rpn3:NEAR
                EXTRN   wsa1:NEAR
                EXTRN   wsa_timeout:NEAR
PARALLEL        ENDS

SERIAL          SEGMENT WORD PUBLIC 'CODE'
                EXTRN   send_pack_serial:NEAR
                EXTRN   recv_pack_serial:NEAR
                EXTRN   send_sync_serial:NEAR
                EXTRN   init_port_serial:NEAR
                EXTRN   reset_port_serial:NEAR
SERIAL          ENDS

BLOCK           SEGMENT WORD PUBLIC 'CODE'
                EXTRN   intr:FAR
                EXTRN   unmapped_unit:WORD
                EXTRN   units:BYTE
BLOCK           ENDS

CORE            SEGMENT WORD PUBLIC 'CODE'
                PUBLIC  start_packet
                PUBLIC  end_packet
                PUBLIC  header
                PUBLIC  lpt3_header
                PUBLIC  lpt2_header
                PUBLIC  lpt1_header
                PUBLIC  finger_print
                PUBLIC  major_version
                PUBLIC  minor_version
                PUBLIC  strat_seg
                PUBLIC  intr1_seg
                PUBLIC  intr2_seg
                PUBLIC  intr3_off
                PUBLIC  intr3_seg
                PUBLIC  intr0_seg
                PUBLIC  intr_error
                PUBLIC  device_id
                PUBLIC  header_seg
                PUBLIC  driver_id
                PUBLIC  end_low_stub
                PUBLIC  end_core_data
                PUBLIC  driver_call
                PUBLIC  send_pack
                PUBLIC  recv_pack
                PUBLIC  send_sync
                PUBLIC  get_remote_info
        IFDEF DEBUG
                PUBLIC  debug_msg
                PUBLIC  hex_out
        ENDIF
                PUBLIC  client_id
                PUBLIC  crc_errors
                PUBLIC  seq_num
                PUBLIC  busy_semaphore
                PUBLIC  idle_semaphore
                PUBLIC  port_address
                PUBLIC  port_index
                PUBLIC  bios_port_num
                PUBLIC  max_baud
                PUBLIC  client_max_baud
                PUBLIC  max_serial_block
                PUBLIC  is_serial
                PUBLIC  int2f_ok
                PUBLIC  old_int2f_vec
                PUBLIC  old_int25_vec
                PUBLIC  old_int26_vec
                PUBLIC  C int2f_handler
                PUBLIC  C int25_handler
                PUBLIC  C int26_handler
                PUBLIC  win386_enh_mode
                PUBLIC  win386_std_mode
                PUBLIC  Virt_Dev_File
                PUBLIC  Win386_Startup_Info
                PUBLIC  win_386_api_ok
                PUBLIC  win_386_api
                PUBLIC  crctab
                PUBLIC  rhptr
                PUBLIC  dos_version
                PUBLIC  dos_major
                PUBLIC  dos_minor
                PUBLIC  slave_dos_version
                PUBLIC  slave_dos_minor
                PUBLIC  slave_dos_major
                PUBLIC  server_major
                PUBLIC  server_minor
                PUBLIC  packet_buf
                PUBLIC  num_ports
                PUBLIC  num_ser_ports
                PUBLIC  num_par_ports
                PUBLIC  serial_ports
                PUBLIC  parallel_ports
                PUBLIC  default_units
                PUBLIC  first_unit
                PUBLIC  printer_loaded
                PUBLIC  actual_prn_map
                PUBLIC  slave_units
                PUBLIC  initialized
                PUBLIC  drive_mapping
                PUBLIC  default_mapping
                PUBLIC  invalid
                PUBLIC  dd_attributes
                PUBLIC  save_area
                PUBLIC  timeout
                PUBLIC  code_list_ptr
                PUBLIC  code_save
                PUBLIC  ticks_remaining
                PUBLIC  set_alarm_time
                PUBLIC  alarm_vector
                PUBLIC  par_timer_save
                PUBLIC  send_sync_list
                PUBLIC  send_word_list
                PUBLIC  send_pack_list2
                PUBLIC  sp_normal_list
                PUBLIC  recv_word_list
                PUBLIC  recv_pack_list
                PUBLIC  rp_normal_list
                PUBLIC  wait_send_list
                PUBLIC  timer_jmp
                PUBLIC  par_fixup_start
                PUBLIC  par_fixup_end
                PUBLIC  request        
                PUBLIC  port           
                PUBLIC  old_int17
                PUBLIC  device_high
                PUBLIC  init_size
                PUBLIC  low_memory
                PUBLIC  driver_size
                PUBLIC  num_lpt
                PUBLIC  num_com
                PUBLIC  display_scan
                PUBLIC  fx_force_variable

                ORG     0       ; The First device header must be at loc 0!

lpt3_header     device_header <DVR:lpt2_header, -1, LPT_ATTRIB, DVR:strat_jmp, DVR:intr3_jmp, 'LPT3    '>
lpt2_header     device_header <DVR:lpt1_header, -1, LPT_ATTRIB, DVR:strat_jmp, DVR:intr2_jmp, 'LPT2    '>
lpt1_header     device_header <DVR:header,      -1, LPT_ATTRIB, DVR:strat_jmp, DVR:intr1_jmp, 'LPT1    '>
header          device_header <-1, -1, DRIVER_ATTRIB, DVR:strat_jmp, DVR:intr0_jmp, 'C'>
                                        ; Device header

finger_print    db      'INTERLNK'
major_version   db      MAJOR_VER
minor_version   db      MINOR_VER

strat_jmp       LABEL   NEAR

                jmp     far ptr strat

strat_seg       EQU     WORD PTR strat_jmp + 3

intr1_jmp       LABEL   NEAR

                push    ax
                mov     al, 1
                jmp     far ptr intr

intr1_seg       EQU     WORD PTR $ - 2

intr2_jmp       LABEL   NEAR

                push    ax
                mov     al, 2
                jmp     far ptr intr

intr2_seg       EQU     WORD PTR $ - 2

intr3_jmp       LABEL   NEAR

                push    ax
                mov     al, 3
                jmp     far ptr intr

intr3_off       EQU     WORD PTR $ - 4
intr3_seg       EQU     WORD PTR $ - 2

intr0_jmp       LABEL   NEAR

                push    ax
                mov     al, 0
                jmp     far ptr intr

intr0_seg       EQU     WORD PTR $ - 2

end_low_stub    LABEL   NEAR

                EVEN

;NOTE: port_address gets set to zero if send_pack or recv_pack fails.
;This indicates that the connection has been lost and that send_sync must
;be called again to re-establish the connection before calling send_pack
;or recv_pack again.

port_address    dw      0       ; Set to address of serial or parallel port

; initialized   This value is non-zero if the init_packet sequence has been
;               successfully completed (get_remote_info).
;
initialized     db      0                   ; Driver currently Initialized flag

device_id       db      0                   ; 0 = block, 1 - 3 = LPT1 - LPT3
header_seg      dw      0

driver_id       db      1                   ; Logical Interlnk driver number

is_serial       db      0
send_pack       DW      DVR:error_proc
recv_pack       DW      DVR:error_proc

driver_call     DW      DVR:far_call
                DW      ?                   ; Must be updated with correct CS

send_sync       DW      DVR:send_sync_proc
get_remote_info DW      DVR:gri_proc
start_packet    DW      DVR:start_vxd
end_packet      DW      DVR:end_vxd

IFDEF DEBUG
debug_msg       DW      DVR:debug_msg_proc
hex_out         DW      DVR:hex_out_proc
ENDIF

;NOTE: the two DWORDs below must be in this order so that they can be moved
;into the SERVER_INFO_REQ packet with a block move, and compared against the
;SERVER_INFO_ANS packet with a block compare.

client_id       dd      0
last_server_id  dd      0

; crc_errors    This is the number of CRC errors that have been detected
;               since the driver was installed.  This value is maintained
;               by the protocol routines and is for informational purposes
;               only.
;
crc_errors      dw  0                   ; Number of CRC errors recieved

; busy_semaphore    This byte is non-zero if communications are currently in
;                   progress.  All auxiliary drivers, TSRs as well as the main
;                   driver update this byte when they start and end a com-
;                   munication session.  If the semaphore is set, then somebody
;                   else is using the port.  It is mandatory that this byte be
;                   checked and updated in a non-interruptable manner (either
;                   by using sti/cli or xchg/test).
;
busy_semaphore  db      0

idle_semaphore  db      0       ; Set non-zero if background idling is OK

port_index      dw      0       ; Indexes into serial_ports or parallel_ports

bios_port_num   db      0       ; 0 if not BIOS, 1 - 4 for COM1 - 4, LPT1 - 4

max_baud        db      BAUD_115200     ; Maximum actual baud rate
client_max_baud db      BAUD_115200     ; Maximum desired baud rate
max_serial_block    dw  MAX_SERIAL_BLOCK

win386_enh_mode db      0
win386_std_mode db      0
int2f_ok        db      0
old_int2f_vec   dd      0
old_int25_vec   dd      0
old_int26_vec   dd      0
win_386_api_ok  db      0
win_386_api     dd      ?

Virt_Dev_File   db      67 DUP (0)

Win386_Startup_Info Win386_Startup_Info_Struc <>

; default_units Number of units to be installed
;
default_units   db  3                   ; Default max number of devices

; first_unit    This is the actual drive number (0=A:, 1=B:, etc.) of the first
;               drive supported by this driver.
;
first_unit      db  ?                   ; First drive number

printer_loaded  db  1                   ; 0 if printer code not loaded

; slave_units   This is the number of logical block devices available on the
;               slave system.
;
slave_units     db      ?               ; Number of Block devices on slave


;-----------------------------------------------------------------------------
;--- IMPORTANT NOTE: actual_prn_map must immediately precede drive_mapping,
;--- because a block move is used to copy values in and out of the init packet
actual_prn_map  db  3 dup (DONT_CARE)
; drive_mapping This array is the drive mapping translation table.  This array
;               is addressed by the unit number in the RHP.  The contents of
;               the location is the actual unit number on the slave device.
;               The most significant bit of the value is the write protect bit.
;               If the bit is set, write operations are not allowed.  If it is
;               clear, write operations are allowed.  The lower 7 bits are the
;               actual unit number on the slave devices.
;
drive_mapping   LABEL   BYTE            ; Drive mapping translation table
        db      MAX_DEVICES DUP(DONT_CARE)

default_mapping db      MAX_DEVICES DUP(DONT_CARE)
;-----------------------------------------------------------------------------

; invalid   This array (like the drive_mapping array) is addressed by the unit
;           number in the RHP.  In general if invalid[unit] is non-zero,    
;           the BPB for the unit must be updated.
;               In media_check, if invalid[unit] is set, the media_check
;           will be forced to return MEDIA_CHANGED.  The on-line drive mapping
;           sets this value.  This forces DOS to rebuild the BPB for the device
;           the next time it is accessed.
;
invalid         db  MAX_DEVICES DUP (0FFH)  ; Rebuild-BPB flags

                db      90H                 ;$$$

; dd_attributes are the device driver attributes for each logical device on
; the slave system.  This array is addressed by the mapped unit number (the
; device number on the slave system)--NOT the unit number in the RHP
;
dd_attributes   dw      MAX_DEVICES DUP (0)


crctab  DW 00000H, 01021H, 02042H, 03063H, 04084H, 050a5H, 060c6H, 070e7H
        DW 08108H, 09129H, 0a14aH, 0b16bH, 0c18cH, 0d1adH, 0e1ceH, 0f1efH
        DW 01231H, 00210H, 03273H, 02252H, 052b5H, 04294H, 072f7H, 062d6H
        DW 09339H, 08318H, 0b37bH, 0a35aH, 0d3bdH, 0c39cH, 0f3ffH, 0e3deH
        DW 02462H, 03443H, 00420H, 01401H, 064e6H, 074c7H, 044a4H, 05485H
        DW 0a56aH, 0b54bH, 08528H, 09509H, 0e5eeH, 0f5cfH, 0c5acH, 0d58dH
        DW 03653H, 02672H, 01611H, 00630H, 076d7H, 066f6H, 05695H, 046b4H
        DW 0b75bH, 0a77aH, 09719H, 08738H, 0f7dfH, 0e7feH, 0d79dH, 0c7bcH
        DW 048c4H, 058e5H, 06886H, 078a7H, 00840H, 01861H, 02802H, 03823H
        DW 0c9ccH, 0d9edH, 0e98eH, 0f9afH, 08948H, 09969H, 0a90aH, 0b92bH
        DW 05af5H, 04ad4H, 07ab7H, 06a96H, 01a71H, 00a50H, 03a33H, 02a12H
        DW 0dbfdH, 0cbdcH, 0fbbfH, 0eb9eH, 09b79H, 08b58H, 0bb3bH, 0ab1aH
        DW 06ca6H, 07c87H, 04ce4H, 05cc5H, 02c22H, 03c03H, 00c60H, 01c41H
        DW 0edaeH, 0fd8fH, 0cdecH, 0ddcdH, 0ad2aH, 0bd0bH, 08d68H, 09d49H
        DW 07e97H, 06eb6H, 05ed5H, 04ef4H, 03e13H, 02e32H, 01e51H, 00e70H
        DW 0ff9fH, 0efbeH, 0dfddH, 0cffcH, 0bf1bH, 0af3aH, 09f59H, 08f78H
        DW 09188H, 081a9H, 0b1caH, 0a1ebH, 0d10cH, 0c12dH, 0f14eH, 0e16fH
        DW 01080H, 000a1H, 030c2H, 020e3H, 05004H, 04025H, 07046H, 06067H
        DW 083b9H, 09398H, 0a3fbH, 0b3daH, 0c33dH, 0d31cH, 0e37fH, 0f35eH
        DW 002b1H, 01290H, 022f3H, 032d2H, 04235H, 05214H, 06277H, 07256H
        DW 0b5eaH, 0a5cbH, 095a8H, 08589H, 0f56eH, 0e54fH, 0d52cH, 0c50dH
        DW 034e2H, 024c3H, 014a0H, 00481H, 07466H, 06447H, 05424H, 04405H
        DW 0a7dbH, 0b7faH, 08799H, 097b8H, 0e75fH, 0f77eH, 0c71dH, 0d73cH
        DW 026d3H, 036f2H, 00691H, 016b0H, 06657H, 07676H, 04615H, 05634H
        DW 0d94cH, 0c96dH, 0f90eH, 0e92fH, 099c8H, 089e9H, 0b98aH, 0a9abH
        DW 05844H, 04865H, 07806H, 06827H, 018c0H, 008e1H, 03882H, 028a3H
        DW 0cb7dH, 0db5cH, 0eb3fH, 0fb1eH, 08bf9H, 09bd8H, 0abbbH, 0bb9aH
        DW 04a75H, 05a54H, 06a37H, 07a16H, 00af1H, 01ad0H, 02ab3H, 03a92H
        DW 0fd2eH, 0ed0fH, 0dd6cH, 0cd4dH, 0bdaaH, 0ad8bH, 09de8H, 08dc9H
        DW 07c26H, 06c07H, 05c64H, 04c45H, 03ca2H, 02c83H, 01ce0H, 00cc1H
        DW 0ef1fH, 0ff3eH, 0cf5dH, 0df7cH, 0af9bH, 0bfbaH, 08fd9H, 09ff8H
        DW 06e17H, 07e36H, 04e55H, 05e74H, 02e93H, 03eb2H, 00ed1H, 01ef0H

rhptr           dd      ?               ; Pointer to request header, passed
                                        ; by MS-DOS kernal to Strategy routine

dos_version     LABEL   WORD            ; DOS's version number in WORD format
dos_minor       db      ?               ; DOS's minor version number
dos_major       db      ?               ; DOS's major version number

slave_dos_version   LABEL   WORD        ; DOS version of slave in WORD format
slave_dos_minor db      ?               ; DOS minor number of slave
slave_dos_major db      ?               ; DOS major number of slave

server_major    db      ?               ; Server major version number
server_minor    db      ?               ; Server minor version number

server_multitasker  db  ?

seq_num         db      ?
num_ports       db      0
num_ser_ports   db      0
num_par_ports   db      0

serial_ports    db      TYPE SERIAL_PORT_DEF * MAX_SERIAL_PORTS DUP (?)
parallel_ports  db      TYPE PARALLEL_PORT_DEF * MAX_PARALLEL_PORTS DUP (?)

save_area       db      5 DUP (?)       ; Save area for port state info

timeout         db      0
code_list_ptr   dw      ?
code_save       dw      (2 + MAX_FIXUPS) dup (?)
ticks_remaining dw      ?               ; Number of ticks until timeout occurs
alarm_vector    dw      ?
set_alarm_time  dw      ?
bios_tab_ptr    dw      ?
bios_port_save  dw      ?

                EVEN

par_timer_save  dd      0               ; Old timer vector


;--- The serial_id byte must immediately precede the serial jump vectors ---

                db      ?
serial_id       db      1               ; Gets copied into is_serial

;========================= Offsets into serial segment ====================
;=================== Must be fixed up if serial code moved ================

send_pack_svec      DW      DVR:send_pack_serial
recv_pack_svec      DW      DVR:recv_pack_serial

;--- The parallel_id byte must immediately precede the parallel jump vectors ---

                db      ?
parallel_id     db      0               ; Gets copied into is_serial

par_fixup_start     LABEL   WORD
;========================= Offsets into parallel segment ====================
;=================== Must be fixed up if parallel code moved ================

send_pack_pvec      DW      DVR:send_pack_parallel
recv_pack_pvec      DW      DVR:recv_pack_parallel
send_sync_pvec      DW      DVR:send_sync_parallel

save_par_vec    DW      DVR:save_parallel
restore_par_vec DW      DVR:restore_parallel

;******************************************************************************
;* IMPORTANT NOTE:  There must not be more than MAX_FIXUPS entries in any of
;* the lists below (not counting the initial fail address and trailing null).
;*
;* Also, each jump must be within short jump distance (128 bytes) of the fail
;* address, or of the the next jump that follows it.
;******************************************************************************
send_sync_list  dw      DVR:ss_timeout, DVR:ssp1, DVR:ssp2, DVR:ssp3, 0
send_word_list  dw      DVR:sw_timeout, DVR:swp1, DVR:swp2, 0
send_pack_list2 dw      DVR:sp_timeout, DVR:spp2, DVR:spp3, DVR:spp4, 0
sp_normal_list  dw      DVR:spn_timeout, DVR:spn2, DVR:spn3, DVR:spn4, 0
recv_word_list  dw      DVR:rw_timeout, DVR:rwp1, DVR:rwp2, 0
recv_pack_list  dw      DVR:rp_timeout, DVR:rpp1, DVR:rpp2, DVR:rpp3, 0
rp_normal_list  dw      DVR:rpn_timeout, DVR:rpn1, DVR:rpn2, DVR:rpn3, 0
wait_send_list  dw      DVR:wsa_timeout, DVR:wsa1, 0

par_fixup_end       LABEL   WORD

timer_jmp       dw      ?
;----------------------------------------------------------------------------

;--- Data for printer code -------------------------------------------------

request         dw      ?               ; request (save area for al)
port            db      ?               ; port number

old_int17       dd      ?               ; Old interrupt 17H vector
;----------------------------------------------------------------------------

device_high     db      0               ; Set to 1 if DEVICEHIGH was used
init_size       dw      ?               ; Size to tell INIT
low_memory      db      1               ; TRUE if loaded in low memory
driver_size     dw      ?               ; Device driver size in bytes
num_com         db      ?
num_lpt         db      ?
display_scan    db      0
fx_force_variable   db  0

packet_buf      db      MAX_PACKET DUP (?)  ; buffer for packets

end_core_data   LABEL   BYTE

                PUBLIC  strat
strat           PROC    FAR             ; device driver "Strategy" routine
                                        ; called from MS-DOS driver with
                                        ; ES:BX = address of request header
                mov     WORD PTR cs:rhptr, bx
                mov     WORD PTR cs:rhptr + 2, es
                ret                     ; Save pointer and return to MS-DOS

strat           ENDP

intr_error      PROC    FAR

                push    es
                push    bx
                les     bx, cs:rhptr
                mov     es:[bx].static_rhp.rhp_status, STATUS_ERROR OR STATUS_DONE OR \
                                            ERR_UNK_COMMAND
                pop     bx
                pop     es
                ret

intr_error      ENDP

far_call        PROC    FAR

                call    ax
                ret

far_call        ENDP

port_num        db      90H

error_proc      LABEL   NEAR

                stc                     ; Indicate error

null_proc       LABEL   NEAR

                ret

show_dot        PROC    NEAR

                cmp     display_scan, 0
                je      show_dot_ret

                push    ax
                push    dx
                mov     dl, '.'
                mov     ah, 2
                int     21H
                pop     dx
                pop     ax

show_dot_ret:   ret

show_dot        ENDP

start_vxd       PROC    NEAR

                pushf
                cmp     win_386_api_ok, 0
                je      start_done

                push    ax
                mov     ax, SET_VMSTAT_HIGH_PRI_BACK
                call    dword ptr win_386_api
                pop     ax

start_done:     popf
                ret

start_vxd       ENDP

end_vxd         PROC    NEAR

                pushf
                cmp     win_386_api_ok, 0
                je      done

                push    ax
                mov     ax, RESET_VMSTAT_HIGH_PRI_BACK
                call    dword ptr win_386_api
                pop     ax

done:           popf
                ret

end_vxd         ENDP

zero_bios_table PROC    NEAR

                mov     bios_tab_ptr, 0FFFFH
                cmp     port_address, 0
                je      zero_done

                cld
                mov     ax, 40H
                mov     es, ax
                mov     ax, port_address
                mov     bios_port_save, ax
                mov     cx, 4
                xor     di, di
                cmp     is_serial, 0
                jne     scan_table

                mov     cx, 3
                mov     di, 8

scan_table:     repne   scasw
                jne     zero_done

                sub     di, 2
                mov     bios_tab_ptr, di
                mov     word ptr es:[di], 0

zero_done:      ret

zero_bios_table ENDP


;************************************************************************
;* Interlnk 2FH interface:
;*
;* Input:
;*      AH = 56H (for all functions)
;*      AL = function number
;*           (0 = install check, 1 = drive check, 2 = port address check)
;*      BL = logical Interlnk driver number (0 = any, 1 = 1st, 2 = 2nd, etc.)
;*      BH = drive number (subfunction 1 only, 0 = drive A, 1 = B, etc.)
;*      CX = port address (used only for subfunction 2)
;*      DX = FFFF
;* Return values:
;*      AL = FF on success, 0 on failure (or unchanged if no responding driver)
;*      BL = logical Interlnk driver number of responding driver
;*      CL = major version number of responding Interlnk driver
;*      CH = minor version number of responding Interlnk driver
;*      DX = segment address of responding Interlnk driver
;************************************************************************

int2f_handler   PROC    FAR

                cmp     ah, INTERLNK_MULTIPLEX_ID
                jne     check_win

                cmp     dx, 0FFFFH
                jne     int2f_done

                cmp     bl, 0
                je      check_function

                cmp     bl, cs:driver_id
                jne     int2f_done

check_function: cmp     al, 0
                je      ok

                cmp     al, 1              ; Check drive letter
                je      check_drive

                cmp     al, 2
                jne     int2f_done

                cmp     cx, cs:port_address
                jne     fail

                jmp     ok

check_drive:    cmp     bh, 25
                ja      int2f_done          ; Pass on if bad drive number

                cmp     bh, cs:first_unit
                jb      fail

                push    ax
                mov     ah, cs:first_unit
                add     ah, cs:units
                cmp     bh, ah
                pop     ax
                jae     fail

                jmp     short ok

fail:           or      bl, bl              ; If any driver, pass it on
                jz      int2f_done

                mov     al, 0
                jmp     short answer

ok:             mov     al, 0FFH            ; Return AL = 0FFH for installed

answer:         mov     bl, cs:driver_id    ; Return BL = logical driver number
                mov     cx, WORD PTR cs:major_version   ; CL = major, CH = minor version #
                mov     dx, cs              ; Return DX = segment of driver
                iret                    

check_win:      cmp     ah, 16h
                jne     int2f_done          ; Not windows API interrupt

                cmp     al, 5
                je      windows_init

                cmp     al, 6
                je      windows_exit

                cmp     al, 8
                je      win_init_done

                cmp     al, 9
                je      win_begin_exit

                jmp     int2f_done

windows_init:   test    dl, 1
                jnz     win_std_init        ; Non-zero = standard mode init


.386
                pusha
                push    ds
                push    es
                mov     ax, cs
                mov     ds, ax
                mov     win386_enh_mode, 1
                call    zero_bios_table
                pop     es
                pop     ds
                popa
.8086
                pushf
                call    dword ptr cs:old_int2f_vec
                mov     word ptr cs:Win386_Startup_Info.SIS_Next_Dev_Ptr, bx
                mov     word ptr cs:Win386_Startup_Info.SIS_Next_Dev_Ptr[2], es
                push    cs
                pop     es
                mov     bx, DVR:Win386_Startup_Info
                jmp     short int2f_ret

win_std_init:   mov     cs:win386_std_mode, 1
                jmp     short int2f_done

windows_exit:   test    dl, 1
                jnz     win_std_exit        ; Non-zero = standard mode exit

                mov     cs:win386_enh_mode, 0
                jmp     short int2f_done

win_std_exit:   mov     cs:win386_std_mode, 0
                jmp     short int2f_done

win_init_done:  push    ax
                push    bx
                push    di
                push    es

;--- Restore the port we zeroed in the BIOS table (if there was one).

                cmp     cs:bios_tab_ptr, 0FFFFH
                je      restore_done

                mov     ax, 40H
                mov     es, ax
                mov     ax, cs:bios_port_save
                mov     di, cs:bios_tab_ptr
                mov     word ptr es:[di], ax

restore_done:   xor     di, di
                mov     es, di                          ; Zero ES:DI
                mov     ax, 1684h
                mov     bx, VFXD_Device_ID
                int     2Fh

                mov     word ptr cs:win_386_api, di
                mov     ax, es
                mov     word ptr cs:win_386_api[2], ax
                or      ax, di
                pop     es
                pop     di
                pop     bx
                pop     ax
                jz      no_api


                push    ax
                push    ds
                push    di

                mov     di, cs
                mov     ds, di
                lea     di, win_386_api_ok
                mov     al, -1 
                call    dword ptr win_386_api

                pop     di
                pop     ds
                pop     ax
                        
no_api:         jmp     short int2f_done

win_begin_exit: mov     cs:win_386_api_ok, 0
;###            jmp     short int2f_done

;--- Pass on the INT 2F to the previous handler

int2f_done:     cmp     cs:int2f_ok, 0
                jne     pass_int2f

int2f_ret:      iret

pass_int2f:     jmp     dword ptr cs:old_int2f_vec


int2f_handler   ENDP

int25_handler   PROC    FAR

                cmp     al, cs:first_unit
                jb      int25_pass

                push    ax
                mov     ah, cs:first_unit
                add     ah, cs:units
                cmp     al, ah
                pop     ax
                jae     int25_pass

IFDEF DEBUG
                push    ds
                push    cs
                pop     ds
                DBG     '<'
                DBG     'I'
                DBG     'N'
                DBG     'T'
                DBG     '2'
                DBG     '5'
                DBG     '>'
                pop     ds
ENDIF

                mov     ax, 0102H
                stc
                retf

int25_pass:     jmp     dword ptr cs:old_int25_vec

int25_handler   ENDP

int26_handler   PROC    FAR

                cmp     al, cs:first_unit
                jb      int26_pass

                push    ax
                mov     ah, cs:first_unit
                add     ah, cs:units
                cmp     al, ah
                pop     ax
                jae     int26_pass

IFDEF DEBUG
                push    ds
                push    cs
                pop     ds
                DBG     '<'
                DBG     'I'
                DBG     'N'
                DBG     'T'
                DBG     '2'
                DBG     '6'
                DBG     '>'
                pop     ds
ENDIF

                mov     ax, 0102H
                stc
                retf

int26_pass:     jmp     dword ptr cs:old_int26_vec

int26_handler   ENDP

send_sync_proc  PROC    NEAR
                PUBLIC  send_sync_proc

                mov     idle_semaphore, 0   ; Insure idler off during sync
                cmp     port_address, 0
                jne     sync_ok             ; Skip sync if connected already

                xor     bx, bx
                mov     port_num, bl

parallel_loop:  mov     al, port_num
                cmp     al, num_par_ports
                jae     scan_serial

                mov     ax, parallel_ports[bx].PARALLEL_PORT_DEF.pp_address
                mov     port_address, ax
                call    save_par_vec
                call    show_dot
                call    send_sync_pvec
                jc      par_reset

                mov     is_serial, 0
                mov     ax, send_pack_pvec
                mov     send_pack, ax
                mov     ax, recv_pack_pvec
                mov     recv_pack, ax
                jmp     short save_index

par_reset:      call    restore_par_vec
                inc     port_num
                add     bx, TYPE PARALLEL_PORT_DEF
                jmp     parallel_loop

scan_serial:    xor     bx, bx
                mov     port_num, bl

serial_loop:    mov     al, port_num
                cmp     al, num_ser_ports
                jae     scan_fail

                mov     ax, serial_ports[bx].SERIAL_PORT_DEF.sp_address
                mov     port_address, ax
                call    init_port_serial
                call    show_dot
                call    send_sync_serial
                jc      ser_reset

                mov     is_serial, 1
                mov     send_pack, DVR:send_pack_serial
                mov     recv_pack, DVR:recv_pack_serial

save_index:     mov     port_index, bx          ; Save index into port array
                mov     idle_semaphore, MINIMUM_TICKS + 1

sync_ok:        clc
                jmp     short send_sync_ret
                
ser_reset:      call    reset_port_serial
                inc     port_num
                add     bx, TYPE SERIAL_PORT_DEF
                jmp     serial_loop

scan_fail:      mov     port_address, 0
                stc

send_sync_ret:  ret

send_sync_proc  ENDP

reset_drives    PROC    NEAR

                push    ds
                pop     es
                mov     di, DVR:drive_mapping
                mov     si, DVR:default_mapping
                mov     cx, MAX_DEVICES
            rep movsb
                ret

reset_drives    ENDP

reset_printers  PROC    NEAR

                push    ds
                pop     es
                mov     al, UNASSIGNED
                mov     di, DVR:actual_prn_map
                xor     ch, ch
                cmp     printer_loaded, 0
                mov     cl, 3
                je      @F

                mov     cl, num_lpt
            rep stosb
                mov     cl, 3
                sub     cl, num_lpt
                mov     al, DONT_CARE

@@:         rep stosb

                ret

reset_printers  ENDP

; get_remote_info sends an INIT_PACKET to the remote.  The remote should
; respond with an array of BPBs for all devices on the remote system.
;
; Inputs:
;   max_devices         Maximum number of devices driver can support
;   dos_version         The word format of the dos versin number
;   max_secsize         Maximum sector size this DOS version can support
;   first_unit          Device number of the first unit in this driver
;
; Outputs:
;   CF                  Set if successful initialization transaction
;                       Clear if some error occured
;
;   dd_attributes       Attribute word array (one word/device)
;   slave_dos_version   DOS version in WORD format of slave system
;   server_major Server program version number
;   initialized         Set if successful communication
;
;   If a communication error occures, the above variables will not be modified.
;
; All registers are destroyed.
;
gri_proc        PROC    NEAR
                PUBLIC  gri_proc

;--- Send server info request (a zero word) ---
                mov     word ptr packet_buf.common_packet.packet_type, 0
                push    ds
                pop     es
                mov     si, DVR:packet_buf
                mov     cx, 2
                call    send_pack
                jc      gr_fail

                mov     di, DVR:packet_buf
                mov     cx, ((TYPE server_info_r + 1) SHR 1)
                xor     ax, ax
        rep     stosw                       ; Zero unused fields
                mov     packet_buf.server_info_r.sir_os_type, OS_MSDOS
                mov     packet_buf.server_info_r.sir_developer, DEV_SEWELL
                mov     packet_buf.server_info_r.sir_product, PRODUCT_DOSLINK
                mov     packet_buf.server_info_r.sir_version, PRODUCT_VERSION
                mov     packet_buf.server_info_r.sir_device_driver, 1
                mov     packet_buf.server_info_r.sir_want_supported, 0
                mov     packet_buf.server_info_r.sir_checksum, 0
                mov     packet_buf.server_info_r.sir_crc, 1
                mov     packet_buf.server_info_r.sir_max_ser_block, MAX_SERIAL_BLOCK
                mov     si, DVR:client_id
                mov     di, DVR:packet_buf.server_info_r.sir_client_id
                mov     cx, 4
        rep     movsw
                mov     si, DVR:packet_buf
                mov     cx, TYPE server_info_r
                call    send_pack
                jc      gr_fail

                mov     di, DVR:packet_buf
                mov     cx, MAX_PACKET
                call    recv_pack
                jc      gr_fail

                mov     ax, packet_buf.server_info_a.sia_max_ser_block
                mov     max_serial_block, ax
                mov     di, DVR:client_id
                mov     si, DVR:packet_buf.server_info_a.sia_last_client_id
                mov     cx, 4
        repe    cmpsw
                je      check_support

                mov     initialized, 0
                call    reset_drives
                mov     si, DVR:packet_buf.server_info_a.sia_server_id
                mov     di, DVR:last_server_id
                movsw               ; Update last_server_id
                movsw

check_support:  cmp     packet_buf.server_info_a.sia_device_server, 0
                je      gr_fail

                cmp     initialized, 0
                jnz     gr_success

                call    reset_printers
                mov     packet_buf.common_packet.packet_type, INIT_PACKET_REQ
                mov     al, default_units   ; Send default units for max dev.
                mov     packet_buf.init_packet_r.ipr_max_devices, al
                mov     ax, dos_version
                mov     packet_buf.init_packet_r.ipr_dos_version, ax
                mov     WORD PTR packet_buf.init_packet_r.ipr_major_version, (MAJOR_VER OR (MINOR_VER SHL 8))
                mov     al, first_unit
                mov     packet_buf.init_packet_r.ipr_first_unit, al
                mov     si, DVR:actual_prn_map
                mov     di, DVR:packet_buf.init_packet_r.ipr_prn_map
                push    ds
                pop     es
                mov     cx, 3 + MAX_DEVICES
            rep movsb

                mov     cx, TYPE init_packet_r
                mov     si, DVR:packet_buf
                push    ds
                pop     es

                call    send_pack       ; Send init packet
                jc      gr_fail

                push    ds
                pop     es
                mov     di, DVR:packet_buf
                mov     cx, MAX_PACKET  ; es:di, cx -> receive buffer

                call    recv_pack       ; get init response
                jc      gr_fail

                mov     ax, packet_buf.init_packet_a.ipa_dos_version  ; Save version numbers
                mov     slave_dos_version, ax
                mov     ax, WORD PTR packet_buf.init_packet_a.ipa_major_version
                mov     WORD PTR server_major, ax
                mov     al, packet_buf.init_packet_a.ipa_multitasker
                mov     server_multitasker, al

                mov     cx, 3 + MAX_DEVICES
                mov     si, DVR:packet_buf.init_packet_a.ipa_prn_map
                mov     di, DVR:actual_prn_map
                push    ds
                pop     es
            rep movsb

                mov     cl, packet_buf.init_packet_a.ipa_devices
                xor     ch, ch          ; cx = number of devices
                mov     slave_units, cl ; Save slave units
                mov     si, DVR:packet_buf.init_packet_a.ipa_attributes
                mov     di, DVR:dd_attributes
            rep movsw                   ; Copy device driver attributes

                mov     al, 0FFH
                mov     initialized, al ; Set initialized flag
                mov     cx, MAX_DEVICES ; Set media invalid flag for all drives
                mov     di, DVR:invalid ; NOTE: ES and AL set up above
            rep stosb

gr_success:     clc                     ; Return success flag
                ret

gr_fail:        stc                     ; Return fail flag
                ret

gri_proc        ENDP


IFDEF DEBUG
MIN_OFFSET      EQU     3 * 2 * 80
MAX_OFFSET      EQU     20 * 2 * 80
screen_offset   dw      (MIN_OFFSET + MAX_OFFSET) / 2
screen_seg      dw      0B800H

debug_init      PROC    NEAR
                PUBLIC  debug_init

                mov     ah, 15
                int     10H
                cmp     al, 7
                jne     color_seg

                mov     screen_seg, 0B000H
                jmp     short init_done

color_seg:      mov     screen_seg, 0B800H

init_done:      ret

debug_init      ENDP

debug_ret       dw      ?

debug_msg_proc  PROC    NEAR

; AX has been pushed on the stack before calling this routine
; Output the character in AL
;   ALL REGISTERS AND FLAGS PRESERVED

                pop     debug_ret
                pushf
                push    es
                push    di
                mov     di, cs:screen_seg
                mov     es, di
                mov     ah, 70H
                mov     di, cs:screen_offset
                stosw
                cmp     di, MAX_OFFSET
                jb      @F

                mov     di, MIN_OFFSET

@@:             mov     ax, 720H
                mov     es:[di], ax
                mov     cs:screen_offset, di
                pop     di
                pop     es
                popf
                pop     ax
                jmp     debug_ret

debug_msg_proc  ENDP

hex_nibble      PROC    NEAR

                and     al, 0FH
                add     al, '0'
                cmp     al, '9'
                jbe     @F

                add     al, 7

@@:             ret

hex_nibble      ENDP


hex_out_proc    PROC    NEAR

; Output value in AL as hex digits
;   ALL REGISTERS AND FLAGS PRESERVED

                pushf
                push    ax
                mov     ah, al
                shr     al, 1
                shr     al, 1
                shr     al, 1
                shr     al, 1
                call    hex_nibble          ; Prepare original high nibble
                call    debug_msg_proc
                push    ax
                call    hex_nibble          ; Prepare original low nibble
                call    debug_msg_proc
                popf
                ret

hex_out_proc    ENDP
ENDIF

CORE            ENDS

                END
