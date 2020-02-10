;***
;* $Workfile:   main.asm  $
;* $Revision:   1.5  $
;*   $Author:   Dave Sewell  $
;*     $Date:   08 Aug 1989 16:44:52  $
;***

                TITLE   Main program for driver when run as a .EXE file
                PAGE    66, 132

                INCLUDE drivers.mac
                INCLUDE packets.mac
                INCLUDE debug.mac

INIT            SEGMENT WORD PUBLIC 'CODE'
                EXTRN   print_msg:NEAR
                EXTRN   toupper:NEAR
                EXTRN   is_alpha:NEAR
                EXTRN   show_char:NEAR
                EXTRN   show_units:NEAR
                EXTRN   show_printers:NEAR
                EXTRN   signon:NEAR
                EXTRN   get_dos_version:NEAR
                EXTRN   show_crlf:NEAR
                EXTRN   connect:NEAR
                EXTRN   requires_dos3:BYTE
                EXTRN   m_not_installed:BYTE
                EXTRN   invalid_switch:BYTE
                EXTRN   doslink_help:BYTE
                EXTRN   m_drive_syntax:BYTE
                EXTRN   m_drive_map:BYTE
                EXTRN   m_client_letter:BYTE
                EXTRN   m_server_letter:BYTE
                EXTRN   m_not_connected:BYTE
                EXTRN   m_dl_ver_mismatch:BYTE
                EXTRN   show_drive_map:NEAR
                EXTRN   show_prn_map:NEAR
                EXTRN   m_lost_connect:BYTE
                EXTRN   device_high:BYTE
                EXTRN   init_size:WORD
                EXTRN   low_memory:BYTE
                EXTRN   driver_size:WORD
INIT            ENDS

BLOCK           SEGMENT WORD PUBLIC 'CODE'
                EXTRN   block_start:BYTE
                EXTRN   block_init_end:BYTE
BLOCK           ENDS

CORE            SEGMENT WORD PUBLIC 'CODE'
                EXTRN   end_low_stub:BYTE
                EXTRN   end_core_data:BYTE
                EXTRN   finger_print:BYTE
                EXTRN   major_version:WORD
                EXTRN   dos_major:BYTE
                EXTRN   strat_seg:WORD
                EXTRN   driver_call:DWORD
                EXTRN   drive_mapping:BYTE
                EXTRN   default_mapping:BYTE
                EXTRN   printer_loaded:BYTE
                EXTRN   first_unit:BYTE
                EXTRN   units:BYTE
                EXTRN   slave_units:BYTE
                EXTRN   is_serial:BYTE
                EXTRN   idle_semaphore:BYTE
                EXTRN   gri_proc:NEAR
CORE            ENDS

STACK           SEGMENT WORD STACK 'STACK'
                db      512 dup(?)
STACK           ENDS

MAIN            SEGMENT WORD PUBLIC 'CODE'

psp             dw      ?
help            db      0               ; Set to TRUE if help   display desired

startup         PROC    FAR

                cld
                push    cs
                pop     ds
                call    get_dos_version
                cmp     dos_major, 2
                ja      get_psp

                pmsg    requires_dos3
                jmp     short error_exit

get_psp:        mov     ah, 62H
                int     21H
                mov     cs:psp, bx      ; Save PSP and pass to show_help in BX
                call    show_help       ; Show help if requested
                jc      normal_exit     ; and exit if help was shown

                call    find_driver
                jc      error_exit

                call    parse_mappings
                jc      drive_syntax

                call    show_crlf
                call    connect         ; NOTE: turns off idling
                pushf
                call    reset_map       ; Reset default map.
                popf
                jnc     process_cmd     ; If connected, process command line

                pmsg    m_not_connected
                jmp     short error_exit

process_cmd:    call    show_drive_map
                jc      error_exit

                call    show_prn_map

normal_exit:    mov     idle_semaphore, MINIMUM_TICKS + 1   ; Turn on idling
                mov     ax, 4C00H       ; Terminate with code 0
                int     21H

drive_syntax:   pmsg    m_drive_syntax

error_exit:     mov     ax, 4C01H       ; Terminate with code 1
                int     21H

lost_connect:   pmsg    m_lost_connect
                jmp     error_exit

startup         ENDP

reset_map       PROC    NEAR

                push    ds
                pop     es
                mov     al, DONT_CARE
                mov     di, DVR:default_mapping
                mov     cx, MAX_DEVICES
            rep stosb
                ret

reset_map       ENDP

show_help       PROC    NEAR

; Enter with:
;   BX = PSP segment
; Return:
;   NC - Help was not requested (with "/?" on command line).
;   C  - Help was requested and displayed.

                mov     es, bx
                mov     si, 81H         ; Point ES:SI to cmd line in PSP

sh_loop:        mov     ax, es:[si]
                cmp     al, ' '
                jb      sh_no_help

                cmp     ax, "?/"
                jne     sh_next

                pmsg    doslink_help
                stc
                jmp     short sh_ret

sh_next:        inc     si
                jmp     sh_loop

sh_no_help:     clc

sh_ret:         ret

show_help       ENDP

local_drive     db      ?

parse_mappings  PROC    NEAR

                mov     es, cs:psp
                mov     si, 81H         ; Point ES:SI to cmd line in PSP

pm_grab_char:   mov     al, es:[si]
                call    toupper
                inc     si
                cmp     al, '/'
                je      skip_arg

                cmp     al, ','
                je      pm_grab_char

                cmp     al, ':'
                je      pm_grab_char

                cmp     al, ' '
                je      pm_grab_char

                jb      parse_ok

                call    is_alpha
                jc      parse_error

                sub     al, 'A'
                cmp     al, MAX_DEVICES
                jae     parse_error

                sub     al, first_unit
                mov     cs:local_drive, al

look_equal:     mov     al, es:[si]
                call    toupper
                cmp     al, ' '
                jb      parse_error

                inc     si

                cmp     al, ':'
                je      look_equal

                cmp     al, '='
                jne     parse_error

look_server:    mov     al, es:[si]
                call    toupper
                xor     bh, bh
                mov     bl, cs:local_drive
                cmp     al, ' '
                jbe     deassign

                cmp     al, ':'
                je      swallow_colon

                cmp     bl, units
                jae     parse_error

                call    is_alpha
                jc      deassign

                inc     si
                cmp     al, 'Z'
                ja      parse_error

                sub     al, 'A'

                mov     ah, UNASSIGNED
                mov     dx, ds
                call    delete_server
                jmp     short store_mapping

deassign:       mov     al, UNASSIGNED

store_mapping:  mov     drive_mapping[bx], al
                mov     default_mapping[bx], al
                jmp     pm_grab_char

swallow_colon:  inc     si
                jmp     look_server

skip_arg:       mov     al, es:[si]
                cmp     al, ' '
                je      pm_grab_char

                jb      parse_ok

                inc     si
                jmp     skip_arg

parse_error:    stc
                jmp     short pm_parse_exit

parse_ok:       clc

pm_parse_exit:  ret

parse_mappings  ENDP

delete_server   PROC    NEAR

; Enter with:
;   AL = server drive letter
;   AH = replacement value if found (DONT_CARE or UNASSIGNED)
;   DX = segment of map table

                push    cx
                push    di
                push    es
                mov     es, dx
                mov     di, DVR:drive_mapping
                mov     cx, MAX_DEVICES

        repne   scasb
                jne     del_done

                mov     BYTE PTR es:[di - 1], ah

del_done:       pop     es
                pop     di
                pop     cx
                ret

delete_server   ENDP


find_driver     PROC    NEAR

                mov     ax, 352FH
                int     21H
                mov     ax, es
                or      ax, bx
                jz      not_found

                mov     ax, (INTERLNK_MULTIPLEX_ID SHL 8) OR 0
                mov     bl, 0
                mov     dx, 0FFFFH
                int     2FH
                cmp     al, 0FFH
                jne     not_found

                cmp     dx, 0FFFFH
                je      not_found

                mov     es, dx
                mov     si, DVR:finger_print
                mov     di, si
                mov     cx, 8
        repe    cmpsb
                je      found

not_found:      pmsg    m_not_installed

fd_show_prog:   call    print_prog_name
                call    show_crlf
                stc
                jmp     short find_ret

found:          cmp     WORD PTR es:major_version, (MAJOR_VER OR (MINOR_VER SHL 8))
                je      same_version

                pmsg    m_dl_ver_mismatch
                jmp     fd_show_prog

same_version:   mov     ds, es:strat_seg
                clc

find_ret:       ret

find_driver     ENDP

IFDEF DEBUG
show_hex_byte   PROC    NEAR

                push    ax
                push    cx
                push    dx
                mov     cl, 4
                shl     ax, cl
                shr     al, cl
                and     ax, 0F0FH
                mov     dl, ah
                call    show_hex_nibble
                mov     dl, al
                call    show_hex_nibble
                mov     dl, ' '
                call    show_char
                pop     dx
                pop     cx
                pop     ax
                ret

show_hex_byte   ENDP

show_hex_nibble PROC    NEAR

                add     dl, '0'
                cmp     dl, '9'
                jbe     show_nibble

                add     dl, 7

show_nibble:    call    show_char
                ret

show_hex_nibble ENDP
ENDIF

print_prog_name PROC    NEAR

                mov     es, cs:psp
                mov     es, es:[2CH]    ;Get segment of environment
                xor     di, di
                xor     ax, ax
                mov     cx, 8000H

scan_end: repne scasb
                scasb
                jne     scan_end

                inc     di
                inc     di

print_prog:     mov     dl, es:[di]
                mov     ah, 6
                int     21H
                inc     di
                or      al, al
                jnz     print_prog

                ret

print_prog_name ENDP

MAIN            ENDS

                END     startup
