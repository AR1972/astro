;***
;* $Workfile:   printer.asm  $
;* $Revision:   1.1  $
;*   $Author:   Dave Sewell  $
;*     $Date:   30 Jun 1989 18:27:00  $
;***

TIMEOUT         equ     01H             ; Timeout bit
ERROR           equ     08H             ; Error bit
SELECTED        equ     10H             ; Selected bit
OUT_OF_PAPER    equ     20H             ; Out of paper bit
ACKNOWLEDGE     equ     40H             ; Acknowledge bit
NOT_BUSY        equ     80H             ; Not busy bit

BUSY            equ     SELECTED        ; Printer ok but busy

PRINTER_WRITE   equ     0               ; Printer write request
PRINTER_INIT    equ     1               ; Printer initialize request
PRINTER_STATUS  equ     2               ; Printer status request


                INCLUDE drivers.mac
                INCLUDE packets.mac
                INCLUDE debug.mac

CORE            SEGMENT WORD PUBLIC 'CODE'
                EXTRN   send_pack:WORD
                EXTRN   recv_pack:WORD
                EXTRN   send_sync:WORD
                EXTRN   get_remote_info:WORD
                EXTRN   busy_semaphore:BYTE
                EXTRN   initialized:BYTE
                EXTRN   actual_prn_map:BYTE
                EXTRN   printer_loaded:BYTE
                EXTRN   packet_buf:BYTE
                EXTRN   request:WORD
                EXTRN   port:BYTE
                EXTRN   old_int17:DWORD
                EXTRN   port_address:WORD
                EXTRN   idle_semaphore:BYTE
        IFDEF DEBUG
                EXTRN   hex_out:WORD
                EXTRN   debug_msg:WORD
        ENDIF
CORE            ENDS

PRINTER         SEGMENT WORD PUBLIC 'CODE'
                PUBLIC  printer_start
                PUBLIC  prn_fixup

                ORG     0

printer_start   LABEL   BYTE


int17_trap      PROC    FAR             ; INT 17H entry point

                cmp     dl, 3           ; Check for illegal LPT value
                jae     int17_passthru  ; If illegal, let previous guy handle it

                push    bx
                mov     bl, dl
                xor     bh,bh
                cmp     cs:actual_prn_map[bx], 3
                jb      remote          ; Value falls with 0 - 2

                pop     bx

int17_passthru: 
                jmp     cs:old_int17    ; Pass request to previous INT 17H handler

remote:         push    cx
                push    dx
                push    di
                push    si
                push    bp
                push    ds
                push    es
                mov     bx, cs
                mov     ds, bx          ; Local data now addressable
                mov     bl, 0FFH
                xchg    bl, busy_semaphore
                or      bl, bl
                jz      not_busy

                mov     ah, BUSY        ; Return printer busy if main driver
                jmp     SHORT it_exit   ; is busy

not_busy:       mov     request, ax
                mov     port, dl
                cld                     ; Insure direction flag is UP
                sti                     ; Insure interrupts enabled for transfer

                cmp     port_address, 0
                jne     send_remote

                call    send_sync       ; Insure we are connected
                jc      it_error

                call    get_remote_info ; Insure we are initialized
                jc      it_error
                
send_remote:    call    bios_remote

                mov     al, BYTE PTR request
                jmp     short it_done

it_error:       mov     ah, SELECTED OR ERROR OR TIMEOUT

it_done:        mov     idle_semaphore, MINIMUM_TICKS + 1
                mov     busy_semaphore, 0       ; Clear communications busy flag

it_exit:        pop     es              ; Restore remaining registers
                pop     ds
                pop     bp
                pop     si
                pop     di
                pop     dx
                pop     cx
                pop     bx
                iret

int17_trap      ENDP

; bios_remote   Sends a BIOS request packet to the remote system.
;
; Inputs:
;   request original request value in ax
;   port    original port number in dx
;
; Outputs:
;   AH = printer status (ERROR OR TIMEOUT on communication error)
;
bios_remote     PROC    NEAR

                mov     bl, port
                xor     bh, bh
                mov     al, actual_prn_map[bx]  ; Get server LPT (0 - 2)
                mov     packet_buf.bios_r.bsr_bios_port, al
                mov     ax, request
                cmp     ah, PRINTER_WRITE
                jne     @F

                DBG     'p'
                mov     packet_buf.common_packet.packet_type, BIOS_PRINT_REQ
                mov     packet_buf.bios_r.bsr_char, al
                mov     cx, TYPE bios_r
                jmp     SHORT br_go

@@:             mov     cx, TYPE bios_r - 1
                mov     packet_buf.common_packet.packet_type, BIOS_INIT_REQ
                cmp     ah, PRINTER_INIT; Initialize printer request?
                je      br_go

                DBG     's'
                mov     packet_buf.common_packet.packet_type, BIOS_STATUS_REQ

br_go:          push    ds                  ; send the packet
                pop     es
                mov     si, DVR:packet_buf  ; es:si, cx = packet
                call    send_pack
                jc      br_error

                mov     di, DVR:packet_buf
                mov     cx, TYPE packet_buf
                call    recv_pack
                jnc     br_endio

br_error:       mov     packet_buf.bios_a.bsa_status, ERROR OR TIMEOUT

br_endio:       mov     ah, packet_buf.bios_a.bsa_status
                ret

bios_remote     ENDP

printer_end     LABEL   BYTE            ; End of printer resident code

PRINTER         ENDS

INIT            SEGMENT WORD PUBLIC 'CODE'

printer_init    PROC    NEAR
                PUBLIC  printer_init

                cmp     printer_loaded, 0   ; Any printers to redirect?
                je      hook_done

                mov     ax, 3517H       ; Get current INT 17H vector
                int     21H

                mov     WORD PTR old_int17, bx
                mov     WORD PTR old_int17 + 2, es

                mov     ax, 2517H       ; Set new INT 17H vector
                mov     dx, DVR:int17_trap
prn_fixup       EQU     $ - 2
                int     21H

hook_done:      ret

printer_init    ENDP

INIT            ENDS

                END
