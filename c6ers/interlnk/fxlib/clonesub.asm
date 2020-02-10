
INCLUDE         model.inc
INCLUDE         fastlynx.inc

DATA_PORT       EQU     0
INT_ENABLE      EQU     1
INT_ID          EQU     2
LINE_CONTROL    EQU     3
MODEM_CONTROL   EQU     4
LINE_STATUS     EQU     5
MODEM_STATUS    EQU     6

DATABITS8       EQU     3
STOPBITS1       EQU     0
NOPARITY        EQU     0

PICSTAT1        EQU     20H
PICSTAT2        EQU     21H
IRQ3_BIT        EQU     08H
IRQ4_BIT        EQU     10H
SPECIFIC_EOI    EQU     60H

PROMPT_WAIT     EQU     (5 * 18 + 1)

CTRL_C          EQU     3

INTQ_SIZE       EQU     64

                .DATA
                EXTRN   serial_port:WORD
                EXTRN   seq_num:BYTE
                EXTRN   last_operation:BYTE

irq_vec_save    dd      ?
irq_num         db      ?       ; COM1 & COM3 = IRQ4 (4), COM2 & COM4 = IRQ3 (3)
irq_vec_num     db      ?       ; COM1 & COM3 = vector 12, COM2 & COM4 = 11
irq_bit         db      ?

clone_port_idx  dw      ?

prompt_reset    db      ?

                EVEN

intq_buff       db      INTQ_SIZE DUP(?)
intq_num        dw      ?
intq_front      dw      ?
intq_rear       dw      ?


        		.CODE	text
                EXTRN   GetPortInfo:NEAR
                EXTRN   InitPort:NEAR
                EXTRN   GetTicks:NEAR
                EXTRN   SetBaud:NEAR

_fxc_bios_ticks	PROC    NEAR    PASCAL
                
;* NAME
;*	bios_ticks -- Return current BIOS tick count.
;*
;* SYNOPSIS
;*	time = bios_ticks();
;*
;*	unsigned time;	 Current number of ticks (approximately 18.2 ticks/sec).
;*
;*	extern	unsigned _cdecl bios_ticks(void);

                push    bp
                push    bx
                push    cx
                push    dx
                mov     ah, 0
                int     1AH
                mov     ax, dx      ; Low  portion of tick count to AX
                pop     dx
                pop     cx
                pop     bx
                pop     bp
                ret

_fxc_bios_ticks	ENDP

irq_handler     PROC    FAR

                push    ax
                push    bx
                push    dx
                push    ds
                mov     ax, DGROUP
                mov     ds, ax
                mov     dx, serial_port
                add     dl, INT_ID
                in      al, dx
                and     al, 6
                cmp     al, 4
                jne     send_eoi

                cmp     intq_num, INTQ_SIZE
                jb      @F

                call    clear_buffer

@@:             inc     intq_num
                mov     bx, intq_rear
                sub     dl, INT_ID
                in      al, dx              ;Get the character
                mov     intq_buff[bx], al
                inc     intq_rear
                cmp     intq_rear, INTQ_SIZE
                jb      send_eoi

                mov     intq_rear, 0

send_eoi:       mov     al, irq_num
                or      al, SPECIFIC_EOI
                out     PICSTAT1, al
                pop     ds
                pop     dx
                pop     bx
                pop     ax
                iret

irq_handler     ENDP

@_fxc_send_byte PROC    NEAR USES CX

                mov     dx, serial_port
                mov     bl, al
                add     dl, LINE_STATUS
                call    GetTicks
                mov     cx, ax

send_wait:      in      al, dx
                test    al, 20H
                jnz     output

                call    GetTicks
                sub     ax, cx
                cmp     ax, 2
                jb      send_wait

output:         sub     dl, LINE_STATUS
                mov     al, bl
                out     dx, al
                ret

@_fxc_send_byte ENDP

@_fxc_recv_byte PROC    NEAR USES CX

                mov     bx, ax
                mov     dx, serial_port
                call    GetTicks
                mov     cx, ax

point_status:   add     dl, LINE_STATUS

recv_wait:      in      al, dx
                shr     al, 1
                jc      got_byte

                call    GetTicks
                sub     ax, cx
                cmp     ax, bx
                jb      recv_wait

                mov     ax, -1
                jmp     short recv_ret

got_byte:       sub     dl, LINE_STATUS
                test    al, 7               ; Check for overrun, parity, framing
                in      al, dx
                jnz     point_status

                xor     ah, ah

recv_ret:       ret

@_fxc_recv_byte ENDP

@_fxc_output    PROC    NEAR  USES CX ES

                mov     bl, al
                mov     dx, serial_port
                add     dl, MODEM_CONTROL
                mov     al, 0BH
                out     dx, al                  ; Raise DTR and RTS
                recover

                mov     ax, 40H
                mov     es, ax
                mov     cx, es:[6CH]
                inc     dx                      ; Point to LINE_STATUS

wait_ready:     in      al, dx
                test    al, 20H
                jnz     do_send

                mov     ax, es:[6CH]
                sub     ax, cx
                cmp     ax, 2
                jb      wait_ready

do_send:        sub     dl, LINE_STATUS
                mov     al, bl
                out     dx, al
                ret

@_fxc_output    ENDP

_fxc_read_serial    PROC    NEAR PASCAL   USES SI

                cmp     intq_num, 0
                je      no_chars

                mov     si, intq_front
                mov     al, intq_buff[si]
                inc     si
                xor     ah, ah
                dec     intq_num
                cmp     si, INTQ_SIZE
                jb      update_front

                xor     si, si

update_front:   mov     intq_front, si
                jmp     short read_ser_ret

no_chars:       xor     ax, ax
                dec     ax

read_ser_ret:   ret

_fxc_read_serial    ENDP

@_fxc_init_port PROC    NEAR USES ES

                LOCAL   biosnum:BYTE

                mov     bx, ax              ; Get port index to BX
                shl     bx, 1               ; Make it a word port index
                mov     clone_port_idx, bx
                call    GetPortInfo
                mov     biosnum, ah
                mov     serial_port, dx
                add     dl, LINE_CONTROL
                mov     al, 80H
                out     dx, al              ; Set DLAB = 1
                recover

                sub     dl, (LINE_CONTROL - 1)  ; Point to base + 1
                xor     al, al              ; High byte of 2400 baud divisor
                out     dx, al
                recover

                dec     dx
                mov     al, 48              ; Low byte of 2400 baud divisor
                out     dx, al
                recover

                add     dl, LINE_CONTROL
                mov     al, NOPARITY OR DATABITS8 OR STOPBITS1
                out     dx, al
                recover

                sub     dl, (LINE_CONTROL - INT_ENABLE)
                mov     al, 1
                out     dx, al              ; Data available interrupt
                recover

                add     dl, MODEM_CONTROL - INT_ENABLE
                mov     al, 9
                out     dx, al
                call    clear_buffer
                
                mov     ah, biosnum
                dec     ah
                and     ah, 1
                mov     al, 12
                sub     al, ah
                mov     irq_vec_num, al
                mov     ah, 35H
                int     21H
                mov     word ptr irq_vec_save, bx
                mov     word ptr irq_vec_save[2], es
                push    ds
                mov     ah, 25H
                mov     al, irq_vec_num
                push    cs
                pop     ds
                mov     dx, OFFSET irq_handler
                int     21H
                pop     ds
                mov     ah, biosnum
                dec     ah
                and     ah, 1
                mov     al, 4
                sub     al, ah
                mov     irq_num, al
                or      al, SPECIFIC_EOI
                out     PICSTAT1, al
                mov     dx, serial_port
                in      al, dx              ; Read to clear possible stray char
                mov     ah, IRQ3_BIT
                test    biosnum, 1
                jz      unmask

                mov     ah, IRQ4_BIT

unmask:         mov     irq_bit, ah
                in      al, PICSTAT2
                recover

                not     ah
                and     al, ah
                out     PICSTAT2, al

                call    _fxc_drain
                ret

@_fxc_init_port ENDP

_fxc_reset_port PROC    NEAR    PASCAL

                in      al, PICSTAT2
                recover

                or      al, irq_bit
                out     PICSTAT2, al
                push    ds
                mov     ah, 25H
                mov     al, irq_vec_num
                lds     dx, irq_vec_save
                int     21H
                pop     ds
                mov     bx, clone_port_idx
                call    GetPortInfo
                call    InitPort
                mov     al, BAUD_38400
                call    SetBaud             ; Set baud rate to 38400 for clone
                mov     last_operation, SEND_OPERATION
                mov     seq_num, 0
                ret

_fxc_reset_port ENDP

_fxc_drain      PROC    NEAR PASCAL USES CX DI SI ES

                mov     ax, 40H
                mov     es, ax
                mov     cx, es:[6CH]        ; Save original time in CX
                mov     di, cx              ; DI = last_ticks

clear_loop:     call    _fxc_read_serial
                mov     si, es:[6CH]        ; SI = new_ticks
                or      ax, ax
                js      check_time

                mov     di, si

check_time:     mov     ax, si
                sub     ax, di
                cmp     ax, 3
                ja      clear_done          ; 3 ticks elapsed with no new chars

                mov     ax, si
                sub     ax, cx
                cmp     ax, 18
                jbe     clear_loop

clear_done:     ret

_fxc_drain      ENDP

_fxc_delay      PROC    NEAR

                push    cx
                mov     ax, 40H
                mov     es, ax
                mov     cx, es:[6CH]

wait_loop:      mov     ax, es:[6CH];
                sub     ax, cx
                cmp     ax, bx
                jb      wait_loop

                pop     cx
                ret

_fxc_delay      ENDP

clear_buffer    PROC    NEAR

                xor     ax, ax
                mov     intq_num, ax
                mov     intq_front, ax
                mov     intq_rear, ax
                ret

clear_buffer    ENDP

                END
