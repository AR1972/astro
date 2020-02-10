;***
;* $Workfile:   block.asm  $
;* $Revision:   1.5  $
;*   $Author:   Dave Sewell  $
;*     $Date:   08 Aug 1989 16:44:52  $
;***

                TITLE   Permanent device driver code and data
                PAGE    66, 132
                
                INCLUDE drivers.mac
                INCLUDE packets.mac
                INCLUDE debug.mac

                SUBTTL  MS-DOS device driver definitions
                PAGE

DOS30           EQU (3 SHL 8) + 30      ; MS-DOS 3.30 word format version number
DOS331          EQU (3 SHL 8) + 31      ; MS-DOS 3.31 word format version number
DOS40           EQU (4 SHL 8) + 00      ; MS-DOS 4.00 word format version number
DOS50           EQU (5 SHL 8) + 00      ; MS-DOS 5.00 word format version number


CORE            SEGMENT WORD PUBLIC 'CODE'
                EXTRN   header:BYTE
                EXTRN   init_size:WORD
                EXTRN   strat_seg:WORD
                EXTRN   header_seg:WORD
                EXTRN   crctab:WORD
                EXTRN   dos_version:WORD
                EXTRN   dos_minor:BYTE
                EXTRN   dos_major:BYTE
                EXTRN   slave_dos_version:WORD
                EXTRN   slave_dos_minor:BYTE
                EXTRN   slave_dos_major:BYTE
                EXTRN   busy_semaphore:BYTE
                EXTRN   idle_semaphore:BYTE
                EXTRN   is_serial:BYTE
                EXTRN   win386_enh_mode:BYTE
                EXTRN   int2f_ok:BYTE
                EXTRN   send_pack:WORD
                EXTRN   recv_pack:WORD
                EXTRN   send_sync:WORD
                EXTRN   get_remote_info:WORD
                EXTRN   rhptr:DWORD
                EXTRN   packet_buf:BYTE
                EXTRN   default_units:BYTE
                EXTRN   first_unit:BYTE
                EXTRN   slave_units:BYTE
                EXTRN   initialized:BYTE
                EXTRN   port_address:WORD
                EXTRN   drive_mapping:BYTE
                EXTRN   dd_attributes:WORD
                EXTRN   invalid:BYTE
                EXTRN   device_id:BYTE
                EXTRN   actual_prn_map:BYTE
        IFDEF DEBUG
                EXTRN   hex_out:WORD
                EXTRN   debug_msg:WORD
        ENDIF
CORE            ENDS

INIT            SEGMENT WORD PUBLIC 'CODE'
                EXTRN   initialize:NEAR
                EXTRN   init_end:BYTE
INIT            ENDS


BLOCK           SEGMENT WORD PUBLIC 'CODE'
                PUBLIC  intr
                PUBLIC  block_start
                PUBLIC  intr_vector
                PUBLIC  block_init_end
                PUBLIC  max_devices
                PUBLIC  units
                PUBLIC  max_secsize
                PUBLIC  dispatch
                PUBLIC  bpb_array
                PUBLIC  bpb_pnt_array
                PUBLIC  unmapped_unit
                PUBLIC  lpt_dispatch
                PUBLIC  lpt_unused

                ORG     0

block_start     LABEL   BYTE

MAX_LPT_BUFF    EQU     64

save_sp         dw      ?               ; save area for DOS stack
save_ss         dw      ?

; Device driver stack.  If debugging, the device driver stack has words
; 1234H and 4321H on the top and bottom.  These are used in debugging to
; check for stack overflows and under flows.  They have no other purpose.
; The stack must be aligned on an even boundry.

                EVEN

IFDEF   DEBUG
                dw      1234H           ; Start of stack has a 1234H
ENDIF
                db      STACK_SIZE dup (0)
stack_end:
IFDEF   DEBUG
                dw      4321H           ; End of stack always 4321H
ENDIF

lpt_buff        db      MAX_LPT_BUFF dup(0)
lpt_count       dw      0
lpt_write_count dw      0
lpt_buffering   db      0


intr            PROC    FAR             ; device driver "Interrupt" routine

                pushf                   ; Save flags
                cli                     ; Disable until switch to larger stack
                cld                     ; Set direction flag to UP
                push    ds              ; Save all registers
                push    es
                push    bx
                mov     bx, cs          ; make local data addressable
                mov     ds, bx
                mov     bl, 0FFH
                xchg    busy_semaphore, bl
                or      bl, bl
                jnz     intr_busy
 
                push    cx
                push    dx
                push    di
                push    si
                push    bp
                mov     device_id, al   ; Save device ID passed in AL
                mov     save_sp, sp     ; save old stack location
                mov     save_ss, ss
                mov     bx, cs
                mov     ss, bx          ; Interrupts must be disabled--in case
                mov     sp, DVR:stack_end   ; we're on an old 8088/8086!
                sti                     ; ok to turn interrupts back on now

                call    initialize      ; Call address get modified!!
        intr_vector     EQU     $ - 2

                mov     busy_semaphore, 0
                cli                     ; Just in case an old 8088/8086
                mov     ss, save_ss     ; restore the old stack
                mov     sp, save_sp
                pop     bp
                pop     si
                pop     di
                pop     dx
                pop     cx
                jmp     short intr_restore

intr_busy:      les     bx, rhptr
                mov     es:[bx].static_rhp.rhp_status, STATUS_ERROR OR STATUS_DONE OR ERR_NOT_READY

intr_restore:   pop     bx
                pop     es
                pop     ds
                popf
                pop     ax
                ret                     ; return (far) to MS-DOS

intr            ENDP

; units         This is the number of logical devices supported by the driver.
;
units           db  0                   ; Number of devices

;==== Above here is the core portion of BLOCK which is always loaded =====

block_init_end  LABEL   BYTE

; max_devices   This is the maximum number of devices ever allowed by the
;               the driver, reguardless of the default units.
;
max_devices     db  MAX_DEVICES         ; Maximum number of devices allowed

max_secsize     dw      ?               ; Maximum sector size for driver

; bpb_pnt_array is the array of offsets to the BPBs addressed by the unit
; number in the RHP.
;
bpb_pnt_array   LABEL   WORD            ; Array of offsets to the BPBs
        CNT = 0
        REPT    MAX_DEVICES
                dw      DVR:bpb_array + CNT
            CNT = CNT + TYPE bios_parameter_block
        ENDM

mapped_unit     dw  ?                       ; Device number on slave system

unmapped_unit   dw  ?                       ; Unit on master system

vol_ser_buff    db      MAX_VOLUME DUP (0)  ; Safe area for volume label
                dd      0                   ; and serial number

comm_start_time dw      ?
handler_index   dw      ?

handler_value   LABEL   BYTE
                db      'i'                 ;  0 = initialize driver
                db      'm'                 ;  1 = media check on block device
                db      'b'                 ;  2 = build BIOS parameter Block
                db      'o'                 ;  3 = I/O control read
                db      'r'                 ;  4 = read input from device
                db      'n'                 ;  5 = non-destructive read
                db      '6'                 ;  6 = return current input status
                db      'f'                 ;  7 = flush device input buffers
                db      'w'                 ;  8 = write (output) to device
                db      'v'                 ;  9 = write with verify
                db      'O'                 ; 10 = return current output status
                db      'B'                 ; 11 = flush output buffers
                db      'W'                 ; 12 = I/O control write
                db      'd'                 ; 13 = device open
                db      'D'                 ; 14 = device close
                db      'r'                 ; 15 = removable media
                db      'u'                 ; 16 = output until busy
                db      '7'                 ; 17 = not used
                db      '8'                 ; 18 = not used
                db      'g'                 ; 19 = generic IOCTL
                db      '0'                 ; 20 = not used
                db      '1'                 ; 21 = not used
                db      '2'                 ; 22 = not used
                db      'l'                 ; 23 = get logical device
                db      'L'                 ; 24 = set logical device
                db      '?'                 ; Invalid commands


;*** The table below corresponds to the dispatch table below it.  A non-zero
;*** value in this table means the handler needs to communicate with the
;*** remote system.  It also contains the size of the request header packet
;*** which should be saved and restored when retries are necessary.  Doing
;*** this allows the lower level routines to modify the request header fields
;*** as necessary without worrying about messing up retries.

;*** IMPORTANT NOTE:  The buffer below MUST be at least as large as the largest
;*** number in the table below it.

rhp_size        LABEL   BYTE
                db      0                   ;  0 = initialize driver
                db      14                  ;  1 = media check on block device
                db      16                  ;  2 = build BIOS parameter Block
                db      0                   ;  3 = I/O control read
                db      28                  ;  4 = read input from device
                db      0                   ;  5 = non-destructive read
                db      0                   ;  6 = return current input status
                db      0                   ;  7 = flush device input buffers
                db      22                  ;  8 = write (output) to device
                db      22                  ;  9 = write with verify
                db      0                   ; 10 = return current output status
                db      0                   ; 11 = flush output buffers
                db      0                   ; 12 = I/O control write
                db      3                   ; 13 = device open
                db      3                   ; 14 = device close
                db      0                   ; 15 = removable media
                db      0                   ; 16 = output until busy
                db      0                   ; 17 = not used
                db      0                   ; 18 = not used
                db      23                  ; 19 = generic IOCTL
                db      0                   ; 20 = not used
                db      0                   ; 21 = not used
                db      0                   ; 22 = not used
                db      0                   ; 23 = get logical device
                db      0                   ; 24 = set logical device
                db      0                   ; Invalid commands

                EVEN

dispatch        LABEL   WORD                ; Interrupt routine command-code
                                            ; dispatch table
                dw      DVR:init_block      ;  0 = inititalize driver
                dw      DVR:media_check     ;  1 = media check on block device
                dw      DVR:build_bpb       ;  2 = build BIOS parameter Block
                dw      DVR:unused          ;  3 = I/O control read
                dw      DVR:read            ;  4 = read input from device
                dw      DVR:unused          ;  5 = non-destructive read
                dw      DVR:unused          ;  6 = return current input status
                dw      DVR:unused          ;  7 = flush device input buffers
                dw      DVR:write           ;  8 = write (output) to device
                dw      DVR:write_verify    ;  9 = write with verify
                dw      DVR:unused          ; 10 = return current output status
                dw      DVR:unused          ; 11 = flush output buffers
                dw      DVR:unused          ; 12 = I/O control write
                dw      DVR:device_open     ; 13 = device open
                dw      DVR:device_close    ; 14 = device close
                dw      DVR:removable_media ; 15 = removable media
                dw      DVR:unused          ; 16 = output until busy

                                            ; commands below are for MS-DOS 3.2+
                dw      0                   ; 17 = not used
                dw      0                   ; 18 = not used
                dw      DVR:generic_ioctl   ; 19 = generic IOCTL
                dw      0                   ; 20 = not used
                dw      0                   ; 21 = not used
                dw      0                   ; 22 = not used
                dw      0                   ; 23 = get logical device
                dw      0                   ; 24 = set logical device

lpt_dispatch    LABEL   WORD                ; Interrupt routine command-code
                                            ; dispatch table
                dw      DVR:init_char       ;  0 = inititalize driver
                dw      0                   ;  1 = media check on block device
                dw      0                   ;  2 = build BIOS parameter Block
                dw      0                   ;  3 = I/O control read
                dw      0                   ;  4 = read input from device
                dw      0                   ;  5 = non-destructive read
                dw      0                   ;  6 = return current input status
                dw      0                   ;  7 = flush device input buffers
                dw      DVR:lpt_write       ;  8 = write (output) to device
                dw      DVR:lpt_write       ;  9 = write with verify
                dw      DVR:output_status   ; 10 = return current output status
                dw      DVR:flush_output    ; 11 = flush output buffers
                dw      0                   ; 12 = I/O control write
                dw      DVR:lpt_dev_open    ; 13 = device open
                dw      DVR:lpt_dev_close   ; 14 = device close
                dw      0                   ; 15 = removable media
                dw      0                   ; 16 = output until busy

IFDEF DEBUG
print_sector    PROC    NEAR

                push    ax
                push    bx
                push    dx
                mov     bx, handler_index
                mov     al, handler_value[bx]
                cmp     al, 'r'
                je      print

                cmp     al, 'w'
                jne     print_done

print:          xor     dx, dx
                mov     ax, es:[di].io_req.io_start
                cmp     ax, 0FFFFH
                jne     print_high

                mov     ax, word ptr es:[di].io_req.io_huge_start
                mov     dx, word ptr es:[di].io_req.io_huge_start + 2

print_high:     or      dx, dx
                jz      print_low

                HEX     dh
                HEX     dl

print_low:      or      dh, dl
                or      dh, ah
                jz      @F

                HEX     ah

@@:             HEX     al
                mov     ax, es:[di].io_req.io_requested
                cmp     ax, 1
                je      print_done

                DBG     ','
                or      ah, ah
                jz      low_length

                HEX     ah

low_length:     HEX     al

print_done:     pop     dx
                pop     bx
                pop     ax
                ret

print_sector    ENDP
ENDIF

normal_intr     PROC    NEAR
                PUBLIC  normal_intr

                les     di, rhptr           ; es:di = request header ptr
                mov     bl, es:[di].static_rhp.rhp_command
                xor     bh, bh              ; bx = driver subfunction

                cmp     device_id, 0
                jne     lpt_driver

                mov     al, handler_value[bx]
                DBG     al
                cmp     bl, 24
                ja      invalid_cmd

                shl     bx, 1
                cmp     dispatch[bx], 0
                je      invalid_cmd

                shr     bx, 1
                call    handler
                jmp     short handler_done

lpt_driver:     cmp     bl, 16
                ja      invalid_cmd

                mov     al, handler_value[bx]
                DBG     al
                shl     bx, 1
                cmp     lpt_dispatch[bx], 0
                je      invalid_cmd

                shr     bx, 1
                call    lpt_handler
                jmp     short handler_done

invalid_cmd:    mov     ax, STATUS_ERROR OR ERR_UNK_COMMAND

handler_done:   or      ax, STATUS_DONE     ; set the done bit
                mov     idle_semaphore, MINIMUM_TICKS + 1
                les     di, rhptr
                mov     es:[di].static_rhp.rhp_status, ax
                ret

normal_intr     ENDP

handler         PROC    NEAR

                mov     handler_index, bx
                cmp     bl, 19          ; Generic IOCTL?
                jne     @F

;--- For generic IOCTL, bypass connection attempt unless function == 46H or 66H

                les     di, rhptr
                mov     al, es:[di].gen_ioctl_req.gen_function
                cmp     al, 46H
                je      @F

                cmp     al, 66H
                jne     call_handler

@@:             cmp     rhp_size[bx], 0
                je      call_handler
    IFDEF DEBUG
                call    print_sector
    ENDIF

                cmp     port_address, 0
                jne     call_handler

                call    send_sync       ; Insure we are connected
                jc      not_ready

                call    get_remote_info ; Insure we are initialized.
                jc      not_ready

call_handler:   mov     bx, handler_index
                shl     bx, 1           ; form index to dispatch table
                les     di, rhptr
                call    dispatch[bx]    ; Call the handler
                cmp     port_address, 0
                jne     handler_ret

                mov     bx, handler_index
                cmp     rhp_size[bx], 0
                je      handler_ret

not_ready:      mov     ax, STATUS_ERROR OR ERR_NOT_READY

;--- NOTE: If a write, or write-verify request fails due to a
;--- communications problem, we must insure the transfer count is zeroed.

                mov     bx, handler_index
                cmp     bl, 4
                je      rd_wr_fail

                cmp     bl, 8
                je      rd_wr_fail

                cmp     bl, 9
                jne     handler_ret

rd_wr_fail:     les     di, rhptr
                mov     es:[di].io_ans.io_transfered, 0

handler_ret:    ret

handler         ENDP


lpt_handler     PROC    NEAR

                mov     handler_index, bx
                cmp     rhp_size[bx], 0
                je      call_handler

                cmp     port_address, 0
                jne     call_handler

                call    send_sync           ; Insure we are connected
                jc      not_ready

                call    get_remote_info     ; Insure we are initialized.
                jc      not_ready

call_handler:   mov     bx, handler_index
                shl     bx, 1               ; form index to dispatch table
                les     di, rhptr
                call    lpt_dispatch[bx]    ; Call the handler
                cmp     port_address, 0
                jne     handler_ret

                mov     bx, handler_index
                cmp     rhp_size[bx], 0
                je      handler_ret

not_ready:      mov     ax, STATUS_ERROR OR ERR_NOT_READY
                mov     bx, handler_index

;--- NOTE: Always return success for device_open and device_close to get 
;--- around problem with CHCP command.

                cmp     bl, 13
                je      open_close_ok

                cmp     bl, 14
                je      open_close_ok

;--- NOTE: If a read, write, or write-verify request fails due to a
;--- communications problem, we must insure the transfer count is zeroed.
                
                cmp     bl, 8
                je      wr_fail

                cmp     bl, 9
                jne     handler_ret

wr_fail:        les     di, rhptr
                mov     es:[di].io_ans.io_transfered, 0
                jmp     short handler_ret

open_close_ok:  xor     ax, ax

handler_ret:    ret

lpt_handler     ENDP


; error_packet -- This routine sends an error packet to the remote system.
; Any communication errors are ignored.
;
; Inputs:
;   ah              error code
;   mapped_unit     Mapped unit number
;   unmapped_unit   Unit number passed in from DOS
;
; Outputs: none
;
; Destroys all registers
;
error_packet    PROC    NEAR

                mov     packet_buf.common_packet.packet_type, ERROR_REQ
                                        ; Set up media check request packet
                mov     packet_buf.error_r.err_code, ah
                mov     packet_buf.error_r.err_block_dvr, 0FFH

                mov     al, BYTE PTR unmapped_unit
                mov     packet_buf.error_r.err_unit, al
                mov     al, BYTE PTR mapped_unit
                mov     packet_buf.error_r.err_data, al

                mov     cx, TYPE error_r ; Set up send pack parameters
                mov     si, DVR:packet_buf
                push    ds
                pop     es              ; es:si, cx = packet

                call    send_pack       ; Ignore errors

                ret

error_packet    ENDP



; map_unit  converts the masters unit number into the slaves device number
;           checking for invalid units
;
; Inputs:
;   es:di           pointer to request header packet (with valid rhp_unit)
;   units           number of units on master system
;   slave_units     number of block devices on slave system
;   drive_mapping   translation table
;
; Outputs:
;   CF              Clear if unit is OK
;                   Set if invalid unit number 
;                   If error, AX is also set to STATUS_ERROR OR ERR_UNK_UNIT
;   mapped_unit     The translated unit number (WORD)
;   unmapped_unit   The masters unit number (WORD)
;   ax              same as mapped_unit
;   bx              same as unmapped_unit
;
; Destroys registers: ax, bx
;
map_unit        PROC    NEAR

                mov     bl, es:[di].static_rhp.rhp_unit
                cmp     bl, units       ; See if valid unit number
                jae     mu_unknown

                xor     bh, bh          ; bx = master unit number
                mov     al, drive_mapping[bx]
                and     al, 7FH         ; And off write protect
                cmp     al, slave_units ; See if valid slave unit number
                jae     mu_unknown

                xor     ah, ah
                mov     mapped_unit, ax ; keep track of unit numbers
                mov     unmapped_unit, bx

                clc
                ret

mu_unknown:     stc
                mov     ax, STATUS_ERROR OR ERR_UNK_UNIT
                ret

map_unit        ENDP

; convert_start_sector  Convert requested sector number into appropriate format
;                       for slave system.
;   Inputs:
;       es:di       Pointer to current request header packet
;       mapped_unit Unit number on the slave system
;
;   Outputs:
;       packet_buf.io_r.ior_start       16 bit start packet value
;       packet_buf.io_r.ior_huge_start  32 bit start packet value
;
;   Uses registers: ax, bx, cx, dx
;   
convert_start_sector    PROC    NEAR

                ; Now get the 16 bit start sector into cx and the 32 bit start
                ; sector into dx,ax

                cmp     dos_version, DOS331
                ja      css_dos40
                je      css_dos331

                mov     cx, es:[di].io_req.io_start
                mov     ax, cx
                xor     dx, dx
                jmp     SHORT css_convert

css_dos331:     mov     cx, es:[di].io_req.io_start
                mov     ax, cx
                mov     dx, es:[di].io_req.io_start_high
                jmp     SHORT css_convert

css_dos40:      mov     ax, WORD PTR es:[di].io_req.io_huge_start
                mov     dx, WORD PTR es:[di].io_req.io_huge_start + 2
                mov     cx, ax

css_convert:    ; Now find out if we need to stuff a FFFF in for the 16 bit
                ; sector number (DOS 4.0 HUGE media only)

                cmp     slave_dos_version, DOS331
                jbe     load_start

                mov     bx, mapped_unit ; Get device driver attributes
                shl     bx, 1
                test    dd_attributes[bx], ATT_HUGE
                jz      load_start

                mov     cx, 0FFFFH

                ; Now put the values into the ior packet

load_start:     mov     packet_buf.io_r.ior_start, cx
                mov     WORD PTR packet_buf.io_r.ior_huge_start, ax
                mov     WORD PTR packet_buf.io_r.ior_huge_start + 2, dx

                ret

convert_start_sector    ENDP

; check_unit -- Check for valid unit number
;   Inputs:
;       initialized         Current initialization state
;       rhptr               Seg:Off of reequest header packet
;
;   Outputs:
;       CF                  Set if unit number ok
;                           Clear if invalid unit number
;       AX                  If invalid unit or not initialized,
;                               STATUS_ERROR OR <appropriate critical error>
;                           If valid unit, ax = undefined
;
;       packet_buf.master_id Set to master code if unit is ok (most routines
;                           need to set this anyway, so it saves a little code
;                           to set it here).
;
;       initialized         Set if communications are established
;
;   If driver has not been initialized (communications established with remote)
;   get_remote_info will be called.  So all of the inputs of get_remote_info
;   need to be set before calling this routine (they should have been
;   calculated in initialize).  Also, the outputs of get_remote_info will be
;   set (overwriting the default information).
;
;   Registers destroyed: ALL
;
;   This function first checks to see if communications have been initialized.
;
;   Once communcations have been initialized, the unit for the request is
;   verified.  First, it must be a valid unit number (x < units) and the
;   CHARACTER bit must be clear in the device attribute word.  build_bpb sets
;   ATT_CHARACTER if the master system is going to have a problem reading this
;   device (the device has HUGE sectors and the master's DOS version is before
;   4.00 or 16-bit fat table entries and the master's DOS version is before
;   3.00, or the devices sector size is to big).
;
;
check_unit      PROC    NEAR            ; Check unit number

    ; Check the unit for which I/O is being requested.

                les     di, rhptr
                call    map_unit        ; ax = mapped unit, bx = unmapped unit
                jc      cu_error

                shl     ax, 1           ; ax = offset in attribute list
                mov     bx, ax
                mov     bx, dd_attributes[bx] ; bx = device's driver attribute
                test    bx, ATT_CHARACTER
                jz      stuff_code

                mov     ah, ER_BAD_MEDIA
                call    error_packet    ; Send a bad media packet

                mov     ax, STATUS_ERROR OR ERR_UNK_MEDIA
                jmp     SHORT cu_error

stuff_code:     clc                     ; Set success flag
                ret

cu_error:       stc                     ; Set fail flag
                ret

check_unit      ENDP


; copy_label    Copies a volume label from a packet to a safe place in the
;               driver
; Inputs:
;
; Outputs: none
;
; Uses registers si, es:di, cx
;

copy_label      PROC    NEAR

                mov     cx, MAX_VOLUME + 4  ; add 4 for vol id number
                push    ds
                pop     es              ; es:di = safe place to store label
                mov     di, DVR:vol_ser_buff
            rep movsb                   ; copy the label
                les     di, rhptr
                ret

copy_label      ENDP

                SUBTTL  Subfunction handlers
                PAGE

COMMENT @
    Command code subfunctions follow:
        Inputs:     es:di   - Points to the request header packet
        Outputs:    ax      - 0 if function completed succesfully
                            - STATUS_ERROR OR (error code) if error condition

        Registers need not be preserved.
@

media_check     PROC    NEAR            ; function 1 = Media Check

                call    check_unit      ; Check for errors & send sync
                jc      mc_exit

                les     di, rhptr       ; es:di -> request header packet
                mov     packet_buf.common_packet.packet_type, MEDIA_CHECK_REQ
                                        ; Set up media check request packet
                mov     al, BYTE PTR mapped_unit
                mov     packet_buf.media_check_r.mcr_unit, al

                mov     al, es:[di].media_check_req.media_id
                mov     packet_buf.media_check_r.mcr_media_id, al

                mov     cx, TYPE media_check_r  ; Set up send pack parameters
                mov     si, DVR:packet_buf
                push    ds
                pop     es              ; es:si, cx = packet
                call    send_pack
                jc      mc_error

                mov     di, DVR:packet_buf
                mov     cx, MAX_PACKET
                call    recv_pack       ; receive media check answer
                jc      mc_error

                les     di, rhptr       ; Now setup return packet
                mov     al, packet_buf.media_check_a.mca_changed
                mov     si, DVR:packet_buf.media_check_a.mca_volume
                call    copy_label              ; Copy label from server

                mov     bx, unmapped_unit
                test    invalid[bx], 0FFH
                jz      mc_store_mc     ; See if invalid flag set

                mov     al, MEDIA_CHANGED ; If unit invalid, force media changed

mc_store_mc:    mov     es:[di].media_check_ans.media_changed, al
                cmp     al, MEDIA_CHANGED
                jne     mc_status

                mov     WORD PTR es:[di].media_check_ans.media_label, DVR:vol_ser_buff
                mov     WORD PTR es:[di].media_check_ans.media_label + 2, ds

mc_status:      mov     ax, packet_buf.media_check_a.mca_status
                jmp     short mc_exit

mc_error:       mov     ax, STATUS_ERROR OR ERR_NOT_READY

mc_exit:        ret

media_check     ENDP


build_bpb       PROC    NEAR            ; function 2 = Build BIOS param block

                call    check_unit
                jc      bb_exit

                les     di, rhptr       ; es:di -> request header packet
                mov     packet_buf.common_packet.packet_type, BUILD_BPB_REQ
                mov     al, BYTE PTR mapped_unit
                mov     packet_buf.build_bpb_r.bbr_unit, al
                mov     al, es:[di].build_bpb_req.media_id
                mov     packet_buf.build_bpb_r.bbr_media_id, al
                mov     cx, TYPE build_bpb_r    ; set up send pack parameters
                mov     si, DVR:packet_buf
                push    ds
                pop     es                  ; es:si, cx = packet
                call    send_pack
                jc      bb_error

                mov     di, DVR:packet_buf
                mov     cx, MAX_PACKET  ; es:di, cx -> recieve area
                call    recv_pack
                jnc     bb_ok

bb_error:       mov     ax, STATUS_ERROR OR ERR_NOT_READY
                jmp     short bb_exit

bb_ok:          les     di, rhptr                   ; Now setup return packet
                mov     bx, unmapped_unit
                shl     bx, 1                       ; bx = unit * 2
                mov     bx, bpb_pnt_array[bx]       ; bx = offset of units bpb
                mov     WORD PTR es:[di].build_bpb_ans.bpb_bpb, bx
                mov     WORD PTR es:[di].build_bpb_ans.bpb_bpb + 2, ds
                test    packet_buf.build_bpb_a.bba_status, STATUS_ERROR
                jnz     bb_status                   ; On error, skip copying BPB

                push    ds                          ; Get ready for block move
                pop     es                          ; es:di = dest. for BPB
                mov     di, bx
                mov     si, DVR:packet_buf.build_bpb_a.bba_bpb  ; ds:si = bpb address in packet
                mov     cx, TYPE bios_parameter_block
            rep movsb
                mov     bx, unmapped_unit
                mov     invalid[bx], 0              ; Clear invalid flag

    ; Now check the media for unusable types.
    ; bad media if  32-bit sectors and not DOS 3.31+, 
    ; or devices sector size greater than masters maximum sector size

                mov     si, packet_buf.build_bpb_a.bba_bpb.bytes_per_sector
                cmp     si, max_secsize ; devices sector size is larger than
                ja      bb_bad_media    ; maximum sector size -> bad media

                mov     bx, packet_buf.build_bpb_a.bba_bpb.total_sectors
                or      bx, bx          ; Check for 32 bit sector addressing
                jnz     bb_status       ; No - we're OK then

                cmp     dos_version, DOS331 ; Yes - require DOS 3.31 or later
                jb      bb_bad_media    ; dos_version < MS-DOS 3.31 -> bad media

    ; Note: MS-DOS 3.31 can address 32-bit sectors, but the format of the
    ; RHP is different than DOS 4.0.

                cmp     slave_dos_version, DOS331
                jae     bb_status

bb_bad_media:   mov     bx, mapped_unit
                shl     bx, 1           ; si = offset of attribute word
                or      dd_attributes[bx], ATT_CHARACTER
                                        ; Flag device as having incompatable media
                mov     ax, STATUS_ERROR OR ERR_UNK_MEDIA
                ret

bb_status:      mov     ax, packet_buf.build_bpb_a.bba_status

bb_exit:        ret

build_bpb       ENDP

check_data_len  PROC    NEAR

; Enter with:
;       AX = # of sectors to read or write
; Return:
;    DX:AX = # of bytes to read or write
;       CX = copy of final AX value
;       BX = bytes per sector from BPB
;       NC - success if DX:CX != 00000H and DX:CX <= 10000H
;        C - failure (bad count)

                mov     bx, unmapped_unit
                shl     bx, 1           ; bx = unit * 2
                mov     bx, bpb_pnt_array[bx]       ; bx = offset of units bpb
                mov     bx, [bx].bios_parameter_block.bytes_per_sector   ; bx = length of one sector
                mul     bx              ; dx:ax = bytes to transfer
                mov     cx, ax
                jcxz    check_64k

                or      dx, dx
                jnz     bad_length

                jmp     short check_ok

check_64k:      cmp     dx, 1           ; If CX == 0, DX must equal 1
                je      check_ok

bad_length:     stc
                jmp     short check_ret

check_ok:       clc

check_ret:      ret

check_data_len  ENDP


read_count      dw      ?

read            PROC    NEAR            ; function 4 = read (input)

                call    check_unit
                jc      rd_err_exit

                les     di, rhptr       ; es:di -> request header packet
                mov     packet_buf.common_packet.packet_type, READ_REQ
                mov     al, BYTE PTR mapped_unit
                mov     packet_buf.io_r.ior_unit, al
                mov     al, es:[di].io_req.media_id
                mov     packet_buf.io_r.ior_media_id, al
                mov     ax, es:[di].io_req.io_requested
                or      ax, ax
                jz      rd_bad_length

                mov     packet_buf.io_r.ior_requested, ax
                call    convert_start_sector    ; Convert sector numbers
                mov     cx, TYPE io_r   ; set up send pack parameters
                mov     si, DVR:packet_buf
                push    ds
                pop     es            ; es:si, cx = packet
                call    send_pack
                jc      rd_error

                mov     di, DVR:packet_buf
                mov     cx, MAX_PACKET
                call    recv_pack
                jc      rd_error

                les     di, rhptr
                mov     ax, packet_buf.io_a.ioa_status
                cmp     al, ERR_DISK_CHANGE
                jne     check_count

                mov     si, DVR:packet_buf.io_a.ioa_volume
                call    copy_label      ; Get the label into a safe place
                mov     WORD PTR es:[di].io_ans.io_label, DVR:vol_ser_buff
                mov     WORD PTR es:[di].io_ans.io_label + 2, ds

check_count:    mov     ax, packet_buf.io_a.ioa_transfered
                mov     read_count, ax
                or      ax, ax          ; see if anything transfered
                jz      rd_done

                cmp     ax, es:[di].io_req.io_requested
                ja      rd_bad_length

                call    check_data_len
                jc      rd_bad_length

                les     di, es:[di].io_req.io_data
                call    recv_pack
                jc      rd_error

rd_done:        les     di, rhptr
                mov     ax, read_count
                mov     es:[di].io_ans.io_transfered, ax
                mov     ax, packet_buf.io_a.ioa_status
                ret

rd_bad_length:  mov     ax, STATUS_ERROR OR ERR_READ_FAULT
                jmp     short rd_err_exit

rd_error:       mov     ax, STATUS_ERROR OR ERR_NOT_READY

rd_err_exit:    les     di, rhptr       ; Error in transfer
                mov     es:[di].io_ans.io_transfered, 0    ; set NO data transfered

rd_exit:        ret

read            ENDP


write           PROC    NEAR            ; function 8 = Write (output)
write_verify    LABEL   PROC            ; function 9 = Write with verify

                mov     bl, es:[di].static_rhp.rhp_unit
                xor     bh, bh          ; See if device is write protected!
                test    drive_mapping[bx], 80H
                jz      not_wr_prot

                mov     ax, STATUS_ERROR OR ERR_WRITE_PROT
                jmp     wr_err_exit

not_wr_prot:    call    check_unit
                jc      wr_err_exit

                les     di, rhptr       ; es:di -> request header packet
                mov     packet_buf.common_packet.packet_type, WRITE_REQ
                cmp     es:[di].static_rhp.rhp_command, WRITE
                je      @F

                mov     packet_buf.common_packet.packet_type, WRITE_VER_REQ

@@:             mov     al, BYTE PTR mapped_unit
                mov     packet_buf.io_r.ior_unit, al
                mov     al, es:[di].io_req.media_id
                mov     packet_buf.io_r.ior_media_id, al
                mov     ax, es:[di].io_req.io_requested
                mov     packet_buf.io_r.ior_requested, ax
                call    convert_start_sector
                mov     cx, TYPE io_r   ; set up send pack parameters
                mov     si, DVR:packet_buf
                push    ds
                pop     es              ; es:si, cx = packet
                call    send_pack
                jc      wr_error

                les     di, rhptr       ; Now get ready to send data
                mov     ax, es:[di].io_req.io_requested
                call    check_data_len
                jc      wr_bad_length

                les     si, es:[di].io_req.io_data ; es:si, cx -> buffer
                call    send_pack       ; Send the data
                jc      wr_error

                push    ds
                pop     es              ; receive write answer
                mov     di, DVR:packet_buf
                mov     cx, MAX_PACKET
                call    recv_pack
                jc      wr_error

                les     di, rhptr
                mov     ax, packet_buf.io_a.ioa_status
                cmp     al, ERR_DISK_CHANGE
                jne     copy_count

                mov     si, DVR:packet_buf.io_a.ioa_volume
                call    copy_label      ; Get the label into a safe place
                mov     WORD PTR es:[di].io_ans.io_label, DVR:vol_ser_buff
                mov     WORD PTR es:[di].io_ans.io_label + 2, ds

copy_count:     mov     bx, packet_buf.io_a.ioa_transfered
                mov     es:[di].io_ans.io_transfered, bx
;NOTE: AX still has status in it from before the copy_label call
                ret

wr_bad_length:  mov     ax, STATUS_ERROR OR ERR_WRITE_FAULT
                jmp     short wr_err_exit

wr_error:       mov     ax, STATUS_ERROR OR ERR_NOT_READY

wr_err_exit:    les     di, rhptr       ; Error in transmission
                mov     es:[di].io_ans.io_transfered, 0    ; Return no data transfered

wr_exit:        ret

write           ENDP

lpt_write_block PROC    NEAR

; Enter with:
;   EX:SI - points to block of characters to print
;      CX - count

                push    bx
                mov     packet_buf.common_packet.packet_type, AUX_WRITE_REQ
                mov     bl, device_id
                xor     bh, bh
                dec     bx
                mov     al, actual_prn_map[bx]
                cmp     al, 3
                jae     unknown_lpt

                mov     packet_buf.lpt_o_r.lpt_id, al
                mov     packet_buf.lpt_o_r.print_count, cx
                mov     al, es:[si]
                mov     packet_buf.lpt_o_r.print_data, al
                push    cx
                push    si
                push    es
                mov     cx, TYPE lpt_o_r    ; set up send pack parameters
                mov     si, DVR:packet_buf
                push    ds
                pop     es              ; es:si, cx = packet
                call    send_pack
                pop     es
                pop     si
                pop     cx
                jc      wr_error

                cmp     packet_buf.lpt_o_r.print_count, 1
                je      get_reply

                call    send_pack       ; Send the data
                jc      wr_error

get_reply:      push    ds
                pop     es              ; receive write answer
                mov     di, DVR:packet_buf
                mov     cx, MAX_PACKET
                call    recv_pack
                jc      wr_error

                les     di, rhptr
                mov     bx, packet_buf.lpt_o_a.lpt_transferred
                mov     es:[di].io_ans.io_transfered, bx
                mov     ax, packet_buf.lpt_o_a.lpt_status
                jmp     short wr_exit

unknown_lpt:    mov     ax, STATUS_ERROR OR ERR_UNK_UNIT
                jmp     short wr_zero_trans

wr_error:       mov     ax, STATUS_ERROR OR ERR_NOT_READY
                les     di, rhptr       ; Error in transmission

wr_zero_trans:  mov     es:[di].io_ans.io_transfered, 0    ; Return no data transfered

wr_exit:        pop     bx
                ret

lpt_write_block ENDP

lpt_write       PROC    NEAR            ; function 8 = Write (output)

                call    get_lpt_id
                jae     unknown_lpt

                les     bx, rhptr
                mov     cx, es:[bx].io_req.io_requested
                mov     lpt_write_count, cx

check_count:    mov     ax, lpt_count
                add     ax, cx
                jc      flush

                cmp     ax, MAX_LPT_BUFF
                ja      flush

insert:         cmp     lpt_buffering, 0
                je      write_data

                push    cx
                push    ds                          ; Save driver DS
                push    es
                mov     di, lpt_count
                add     di, DVR:lpt_buff
                push    ds
                lds     si, es:[bx].io_req.io_data
                pop     es                          ; Driver DS to ES
            rep movsb
                pop     es
                pop     ds
                pop     cx
                add     lpt_count, cx
                xor     cx, cx
                mov     ax, lpt_count
                cmp     ax, MAX_LPT_BUFF
                jb      flush_done

flush:          call    lpt_flush
                test    ax, STATUS_ERROR
                jnz     write_done

flush_done:     jcxz    write_ok

                cmp     cx, MAX_LPT_BUFF
                jb      insert

write_data:     les     di, rhptr
                les     si, es:[di].io_req.io_data
                call    lpt_write_block
                jmp     short write_done

write_error:    les     di, rhptr
                mov     es:[di].io_ans.io_transfered, 0
                jmp     short write_done

write_ok:       les     di, rhptr
                mov     cx, lpt_write_count
                mov     es:[di].io_ans.io_transfered, cx
                mov     ax, STATUS_DONE
                jmp     short write_done

unknown_lpt:    mov     ax, STATUS_ERROR OR ERR_UNK_UNIT

write_done:     ret

lpt_write       ENDP

lpt_flush       PROC    NEAR

                push    cx
                push    si
                push    es
                xor     ax, ax              ; Preset for no error
                mov     cx, lpt_count
                jcxz    flush_done

                mov     si, DVR:lpt_buff
                push    ds
                pop     es
                call    lpt_write_block
                test    ax, STATUS_ERROR
                jnz     flush_done

                mov     lpt_count, 0

flush_done:     test    ax, STATUS_ERROR    ; Set flags (NZ = error)
                pop     es
                pop     si
                pop     cx
                ret

lpt_flush       ENDP

get_lpt_id      PROC    NEAR

; Returns: 
;       AL = lpt_id
;        B if lpt_id is valid (0 - 2)
;       AE if lpt_id is invalid

                mov     bl, device_id
                xor     bh, bh
                dec     bx
                mov     al, actual_prn_map[bx]
                cmp     al, 3
                ret

get_lpt_id      ENDP

output_status   LABEL   NEAR

                mov     packet_buf.common_packet.packet_type, OUTPUT_STATUS_REQ
                jmp     short lpt_dev_common

flush_output    LABEL   NEAR

                call    lpt_flush
                jnz     dev_exit

                mov     packet_buf.common_packet.packet_type, FLUSH_OUTPUT_REQ
                jmp     short lpt_dev_common

lpt_dev_open    LABEL   NEAR

                call    get_lpt_id
                jb      @F

                mov     ax, STATUS_DONE
                jmp     short dev_exit

@@:             call    lpt_flush
                jnz     dev_exit

                mov     lpt_buffering, 1
                mov     packet_buf.common_packet.packet_type, AUX_DEV_OPEN_REQ
                jmp     short lpt_dev_common

lpt_dev_close   PROC    NEAR

                call    get_lpt_id
                jb      @F

                mov     ax, STATUS_DONE
                jmp     short dev_exit

@@:             call    lpt_flush
                jnz     dev_exit

                mov     lpt_buffering, 0
                mov     packet_buf.common_packet.packet_type, AUX_DEV_CLOSE_REQ

lpt_dev_common::call    get_lpt_id
                jae     unknown_lpt

                mov     packet_buf.lpt_cmd_r.lpt_id, al
                mov     cx, TYPE lpt_cmd_r    ; set up send pack parameters
                mov     si, DVR:packet_buf
                push    ds
                pop     es              ; es:si, cx = packet
                call    send_pack
                jc      dev_error

                mov     di, DVR:packet_buf
                mov     cx, MAX_PACKET
                call    recv_pack
                jc      dev_error

                mov     ax, packet_buf.lpt_cmd_a.lca_status
                jmp     short dev_exit

unknown_lpt:    mov     ax, STATUS_ERROR OR ERR_UNK_UNIT
                jmp     short dev_exit

dev_error:      mov     ax, STATUS_ERROR OR ERR_NOT_READY

dev_exit::      ret

lpt_dev_close   ENDP

removable_media PROC    NEAR            ; function 15 = Removable media

; NOTE: We always say the media is removeable without asking the server.

                call    map_unit
                jc      rm_exit

                mov     ax, STATUS_DONE ; Return busy bit == 0 for removable

rm_exit:        ret

removable_media ENDP


device_open     PROC    NEAR            ; function 13 = Device Open
device_close    LABEL   PROC            ; function 14 = Device Close

                call    map_unit        ; ax = mapped unit, bx = unmapped unit
                jc      do_exit

                mov     bx, ax          ; See if slave driver supports ATT_OCRM
                mov     ax, STATUS_DONE ; Set return status in case slave
                                        ; doesn't support ATT_OCRM
                shl     bx, 1
                test    dd_attributes[bx], ATT_OCRM
                jz      do_exit

                call    check_unit      ; Unit supports it, so go ahead and
                jc      do_exit         ; send the packet

                les     di, rhptr
                mov     al, DEV_OPEN_REQ
                cmp     BYTE PTR es:[di].static_rhp.rhp_command, DEVICE_OPEN
                je      do_send

                mov     al, DEV_CLOSE_REQ

do_send:        mov     packet_buf.common_packet.packet_type, al
                mov     al, BYTE PTR mapped_unit
                mov     packet_buf.ocrm_r.ocr_unit, al
                
                mov     cx, TYPE ocrm_r
                mov     si, DVR:packet_buf
                push    ds
                pop     es              ; es:si, cx = packet
                call    send_pack       ; Send request
                jc      do_error

                mov     di, DVR:packet_buf
                mov     cx, MAX_PACKET
                call    recv_pack
                jc      do_error

                mov     ax, packet_buf.ocrm_a.oca_status

do_exit:        ret

do_error:       mov     ax, STATUS_ERROR OR ERR_NOT_READY
                ret

device_open     ENDP

gen_data_save   dd      ?

generic_ioctl   PROC    NEAR            ; function 19 = Generic IOCTL

                call    map_unit        ; ax = mapped unit, bx = unmapped unit
                jc      gi_exit

                mov     bx, ax          ; See if slave driver supports ATT_GEN_IOCTL
                shl     bx, 1
                test    dd_attributes[bx], ATT_GEN_IOCTL
                jz      gi_unsupported

                les     di, rhptr
                cmp     es:[di].gen_ioctl_req.gen_category, 8
                jne     gi_unsupported

                mov     al, es:[di].gen_ioctl_req.gen_function
                mov     packet_buf.gen_ioctl_r.gir_function, al ; Save function code
                HEX     al
                les     si, es:[di].gen_ioctl_req.gen_data      ; Get data ptr
                cmp     al, 46H
                je      supported

                cmp     al, 66H
                jne     gi_unsupported

supported:      mov     WORD PTR gen_data_save, si
                mov     WORD PTR gen_data_save[2], es
                mov     packet_buf.common_packet.packet_type, GEN_IOCTL_REQ
                mov     al, BYTE PTR mapped_unit
                mov     packet_buf.gen_ioctl_r.gir_unit, al
                mov     packet_buf.gen_ioctl_r.gir_category, 8
                mov     cx, TYPE gen_ioctl_r
                mov     si, DVR:packet_buf
                push    ds
                pop     es
                call    send_pack
                jc      gi_error

                les     si, gen_data_save
                cmp     packet_buf.gen_ioctl_r.gir_function, 66H
                je      gi_recv

                mov     cx, TYPE media_id_buffer
                call    send_pack
                jc      gi_error

gi_recv:        push    ds
                pop     es              ; receive response
                mov     di, DVR:packet_buf
                mov     cx, MAX_PACKET
                call    recv_pack
                jc      gi_error

                test    packet_buf.gen_ioctl_a.gia_status, STATUS_ERROR
                jnz     gi_done

                les     di, rhptr
                cmp     es:[di].gen_ioctl_req.gen_function, 46H
                je      gi_done

                mov     cx, TYPE media_id_buffer
                les     di, gen_data_save
                call    recv_pack
                jc      gi_error

gi_done:        mov     ax, packet_buf.gen_ioctl_a.gia_status
                jmp     short gi_exit

gi_unsupported: mov     ax, STATUS_ERROR OR ERR_UNK_COMMAND
                jmp     short gi_exit

gi_error:       mov     ax, STATUS_ERROR OR ERR_NOT_READY

gi_exit:        ret

generic_ioctl   ENDP


lpt_unused      PROC    FAR

                push    es
                push    di
                les     di, cs:rhptr
                mov     WORD PTR es:[di].static_rhp.rhp_status, STATUS_DONE
                pop     di
                pop     es
                pop     ax
                ret

lpt_unused      ENDP


unused          PROC    NEAR

                xor     ax, ax
                ret

unused          ENDP



                PAGE

init_char       PROC    NEAR

                mov     ax, init_size
                mov     WORD PTR es:[di].init_ans.init_end, ax
                mov     ax, header_seg
                mov     WORD PTR es:[di].init_ans.init_end + 2, ax
                xor     ax, ax
                ret

init_char       ENDP

init_block      PROC    NEAR

                mov     ax, init_size
                mov     WORD PTR es:[di].init_ans.init_end, ax
                mov     ax, header_seg
                mov     WORD PTR es:[di].init_ans.init_end + 2, ax
                mov     WORD PTR es:[di].init_ans.init_bpb, DVR:bpb_pnt_array
                mov     WORD PTR es:[di].init_ans.init_bpb + 2, ds   ; BPB offset array
                mov     al, units
                mov     es:[di].init_ans.init_units, al
                mov     cs:header.device_header.name_num[0], al  ; Number of units
                xor     ax, ax
                ret

init_block      ENDP


BLOCK           ENDS


                SUBTTL  Resident variable length data
                PAGE
VARLEN          SEGMENT WORD PUBLIC 'CODE'

                EVEN

bpb_array       LABEL   bios_parameter_block
        bios_parameter_block MAX_DEVICES DUP (<200H, 1, 1, 2, 64, 128, 0, 1>)
                                        ; Allocation for BPB array

VARLEN          ENDS


                END
