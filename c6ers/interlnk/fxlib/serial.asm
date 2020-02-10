INCLUDE         model.inc
INCLUDE         fastlynx.inc

;### NO_TIMEOUT      EQU    1
;### DEBUG           EQU    1

INT_MASK        EQU     0FEH            ; All but timer disabled
TIMER_INTERRUPT EQU     08H             ; Timer interrupt vector

; Note on timings:  timer_handler counts how many normal clock ticks pass
; each tick occurs every 55 milli-seconds.  However, from the time that the
; alarm is set to the first tick can be from zero seconds up to 55 milli-
; seconds.  So maximum times should be calculated using the number of ticks
; minus one.  Also, for the same reason, MINIMUM_TICKS must be >= 2.
;
; MINIMUM_TICKS is the minimum time allowed on the timer.  This must be no less
; that 2.  If it is 1, it is possible for the timeout to occur immediately.
;
; For a data segment of 65535 bytes (maximum number of bytes possible) the
; timeout would be 129 ticks (using shift factor 9, minimum 2) or 7.04 seconds.
;
; SEND_PACK_TICKS is the default timeout value used in send_pack_ser.
; 19 ticks allows the receiver .99 seconds to respond to the send request.
;
;

CONNECT_BAUD    EQU     BAUD_9600

INITIAL_CRC     EQU     0FFFFH          ; Initial CRC accumulator
MAGIC_CRC       EQU     1D0FH           ; Magic CRC number

OVERRUN_ERROR   EQU     00000010B
PARITY_ERROR    EQU     00000100B
FRAMING_ERROR   EQU     00001000B
PARITY_OR_FRAMING_ERROR EQU 0CH

; Serial port flags
BAD_PORT        EQU     80H

DATA_PORT       EQU     0               ; Serial port definitions
INT_ENABLE      EQU     1
INT_ID          EQU     2
LINE_CONTROL    EQU     3
MODEM_CONTROL   EQU     4
LINE_STATUS     EQU     5
MODEM_STATUS    EQU     6

DATABITS8       EQU     3
STOPBITS1       EQU     0
NOPARITY        EQU     0


                PUBLIC  SendSerialBlock
                PUBLIC  SendSerial
                PUBLIC  RecvSerial
                PUBLIC  SetBaud
                PUBLIC  @FxSetBaud
                PUBLIC  FxShowBaud
                PUBLIC  serial_port
                PUBLIC  NearShowBaud

                EXTRN   __crctab:WORD
                EXTRN   __crc_errors:WORD

                .DATA
                EXTRN   last_operation:BYTE
                EXTRN   SyncTimeout:WORD
        IFDEF DEBUG
                EXTRN   _display_segment:WORD
                PUBLIC  screen_offset
        ENDIF

ShowBaud        dw      OFFSET  far_null_proc
                dw      SEG far_null_proc

MAX_BLOCK       EQU     8192
                

client          db      ?
server          db      ?
first_send      db      ?
polite_timer    db      ?    
interrupt_mask  db      ?
recv_type       db      ?               ; type of packet received
recv_buff_ptr   dw      ?               ; Save area of recv_buff_ptr
recv_byte_error db      ?
serial_port     dw      ?               ; I/O port base address
win386_enh_mode db      ?
hp_95lx         db      0
need_send_echo  db      0
send_remaining  dw      ?           ; Number of bytes remaining in send

recv_table      dw      OFFSET recv_3_norm
                dw      OFFSET recv_3_echo
                dw      OFFSET recv_3_norm
                dw      OFFSET recv_3_echo
                dw      OFFSET recv_3_norm
                dw      OFFSET recv_3_echo
                dw      OFFSET recv_3_norm
                dw      OFFSET recv_3_echo

send_table      dw      OFFSET send_3_norm
                dw      OFFSET send_3_echo
                dw      OFFSET send_3_norm
                dw      OFFSET send_3_echo
                dw      OFFSET send_3_norm
                dw      OFFSET send_3_echo
                dw      OFFSET send_3_norm
                dw      OFFSET send_3_echo

baud_table      db      96          ;   1200 baud   (index 0)
                db      48          ;   2400 baud   (index 1)
                db      24          ;   4800 baud   (index 2)
                db      12          ;   9600 baud   (index 3)
                db       6          ;  19200 baud   (index 4)
                db       3          ;  38400 baud   (index 5)
                db       2          ;  57600 baud   (index 6)
                db       1          ; 115200 baud   (index 7)

packet_ok        db      ?
prior_baud       db      ?

MAX_SEND_FAILS  EQU     2

echo_byte       db      ?
echo_ret        dw      ?
ax_save         dw      ?
send_fails      db      0


baud_list1      db      ?, 0FFH, 000H, 05AH, 055H, 0AAH, 0F0H, 00FH, 0E7H, 07EH, 0C3H, 03CH, 081H, 018H, 000H, 0FFH
BAUD_LIST_SIZE  EQU     $ - baud_list1
baud_list2      db      ?, 000H, 0FFH, 0A5H, 0AAH, 055H, 0F0H, 00FH, 0FFH, 000H, 018H, 081H, 03CH, 0C3H, 07EH, 0E7H

        		.CODE   text
                EXTRN   ResetListenTime:NEAR
                EXTRN   GetTicks:NEAR
                EXTRN   GetPortInfo:NEAR
                EXTRN   GetPortIndex:NEAR
                EXTRN   SyncStart:NEAR
                EXTRN   SyncDone:NEAR
                EXTRN   CheckAbort:NEAR
                PUBLIC  SerialInit
                PUBLIC  setup_recv_byte

; These variable must be addressable relative to the CS register.

                EVEN

is_serial       db      0               ; 1 if connected on a serial port
alarm_vector    dw      ?               ; Return offset when timeout occurs
timer_save      dd      0               ; Old timer vector
ticks_remaining dw      ?               ; Number of ticks until timeout occurs
lost_ticks      dw      ?               ; Tick count while timer set
set_alarm_time  dw      ?

MAX_FIXUPS      EQU     5

null_code_list  dw      0, 0
code_list_ptr   dw      OFFSET null_code_list
code_save       dw      (2 + MAX_FIXUPS) dup (?)
timeout         db      ?


DBG     MACRO   char
    IFDEF DEBUG
        push    ax
        mov     al, char
        call    debug_msg_proc     ; debug_msg pops off AX and restores it
    ENDIF
        ENDM

HEX     MACRO   char
    IFDEF DEBUG
        push    ax
        mov     al, char
        call    hex_out_proc
        pop     ax
    ENDIF
        ENDM

IFDEF DEBUG
MIN_OFFSET      EQU     2 * 2 * 80
MAX_OFFSET      EQU     8 * 2 * 80
screen_offset   dw      MIN_OFFSET
debug_ret       dw      ?

@debug_char     PROC    FAR
                PUBLIC  @debug_char

                push    ax
                call    debug_msg_proc
                ret

@debug_char     ENDP

@debug_hex      PROC    FAR
                PUBLIC  @debug_hex

                call    hex_out_proc
                ret

@debug_hex      ENDP

debug_msg_proc  PROC    NEAR

; AX has been pushed on the stack before calling this routine
; Output the character in AL
;   ALL REGISTERS AND FLAGS PRESERVED

                pop     cs:debug_ret
                pushf
                push    es
                push    di
                mov     di, _display_segment
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
                jmp     cs:debug_ret

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

;----------------------------------------------------------------------------
timer_handler   PROC   FAR

;*** The two bytes below are modified as follows:
;***    impolite handler set:   1st 2 bytes of:
;***                                            push    ax
;***                                            mov     al, 20H
;***    alarm not set:          jmp short timer_passthru
;***    polite alarm set:       jmp short polite_handler
;***
;*** IMPORTANT: handlers below MUST be within short jump distance from here!!!

                push    ax
                mov     al, 20H         ; Non-Specific EOI
                out     20H, al
                inc     cs:lost_ticks
                dec     cs:ticks_remaining
                jz      @F              ; Jump if time has expired

th_ret:         pop     ax
                iret

@@:             call    modify_code
                jmp     SHORT th_ret

polite_handler  LABEL   FAR

                push    ax
                push    bx
                call    GetTicks
                mov     bx, ax
                sub     ax, cs:set_alarm_time
                mov     cs:set_alarm_time, bx
                sub     cs:ticks_remaining, ax
                pop     bx
                pop     ax
                jc      timed_out

                jnz     @F

timed_out:      call    modify_code

@@:             jmp     DWORD PTR cs:timer_save


; timer_passthru -- Interrupt handler when alarm is not set!
;
timer_passthru  LABEL   FAR         ; Interrupt handler while alarm not on!


passthru:       jmp     DWORD PTR cs:timer_save   ; chain through old timer vector

timer_handler   ENDP

far_null_proc   PROC    FAR

                ret

far_null_proc   ENDP

check_windows   PROC    NEAR

                push    bx
                push    cx
                push    dx
                push    di
                push    si
                push    bp
                push    es
                mov     win386_enh_mode, 0
                mov     hp_95lx, 0
                mov     ax, 352FH
                int     21H
                mov     ax, es
                or      ax, bx
                jz      check_done

                mov     ax, 1600H
                int     2FH
                test    al, 7FH
                jz      check_done

                cmp     al, 1
                je      check_done

                cmp     al, 0FFH
                je      check_done

                inc     win386_enh_mode

check_done:     xor     bx, bx
                mov     ax, 4DD4H
                int     15H
                cmp     bx, "HP"            ; Look for BH = 'H', BL = 'P'
                jne     @F

                inc     hp_95lx

@@:             pop     es
                pop     bp
                pop     si
                pop     di
                pop     dx
                pop     cx
                pop     bx
                ret

check_windows   ENDP


WaitIdle        PROC    NEAR    USES CX

;* Enter with:
;*      DX = port base address
;*      BX = port vector index
;* Uses:
;*      AX
;* Returns:
;*      CARRY = Timed out

                call    GetTicks
                mov     cx, ax

wait_loop:      add     dl, LINE_STATUS
                in      al, dx
                sub     dl, LINE_STATUS
                cmp     al, 60H
                je      wi_done

                rcr     al, 1
                jnc     check_time

                in      al, dx

check_time:     call    GetTicks
                sub     ax, cx
                cmp     ax, MINIMUM_TICKS
                jbe     wait_loop

                stc
                jmp     short return

wi_done:        clc

return:         ret

WaitIdle        ENDP

SetBaud         PROC    NEAR USES SI DX
;* Enter with:
;*      AL = desired baud_table index
;*      DX = port base address
;* Uses:
;*      AX
;* Returns:
;*      NC - set baud succeeded
;*      C  - set baud failed

                mov     fx_baud, al
                xor     ah, ah
                mov     si, ax
                call    WaitIdle
                jc      sb_ret              ; Return with carry set

                add     dl, LINE_CONTROL
                mov     al, 80H OR NOPARITY OR STOPBITS1 OR DATABITS8
                out     dx, al
                recover

                sub     dl, 2               ; dx -> Baud Rate (MSB)
                xor     al, al              
                out     dx, al
                recover

                dec     dx                  ; dx -> Baud Rate (LSB)
                mov     al, baud_table[si]
                out     dx, al
                recover

                add     dl, LINE_CONTROL
                mov     al, NOPARITY OR STOPBITS1 OR DATABITS8
                out     dx, al
                recover

                sub     dl, LINE_CONTROL
                call    WaitIdle
                jc      sb_ret              ; Return with carry set

                cmp     fx_port, -1
                je      sb_ok

                call    NearShowBaud

sb_ok:          clc

sb_ret:         ret

SetBaud         ENDP

@FxSetBaud      PROC    FAR

                call    SetBaud
                mov     ax, 0
                jc      set_baud_ret

                inc     ax

set_baud_ret:   ret

@FxSetBaud      ENDP

InitPort        PROC    NEAR    USES SI

;* Enter with:
;*      DX = port base address
;*      BX = port vector index
;* Uses:
;* Performs:

                call    WaitIdle
                jc      bad_port            ; could not obtain idle

                add     dl, INT_ENABLE      ; dx -> INT_ENABLE
                xor     al, al              ; Turn off all interrupts
                out     dx, al
                recover

                add     dl, MODEM_CONTROL - INT_ENABLE  ; dx -> MODEM_CONTROL
                out     dx, al
                recover

                sub     dl, MODEM_CONTROL

                mov     al, CONNECT_BAUD
                call    SetBaud
                jc      bad_port

                call    GetPortIndex
                and     FxPortInfo[si].FxPortInfoDef.pi_flags, BAD_PORT XOR 0FFH
                ret

bad_port:       call    GetPortIndex
                or      FxPortInfo[si].FxPortInfoDef.pi_flags, BAD_PORT
                ret

InitPort        ENDP
                
SerialInit      PROC    NEAR

;* Enter with:
;* Uses:
;*      BX
;* Performs:    Cycles through all of the ports defined to fastlynx and for
;*              each port that is serial initializes the UART and sets the
;*              baud rate to CONNECT_BAUD.

                xor     bx, bx

init_loop:      cmp     bx, fx_tail
                jae     steal_vector

                call    GetPortInfo
                cmp     al, SERIAL_PORT
                jne     skip

                call    InitPort

skip:           inc     bx
                inc     bx
                jmp     init_loop

steal_vector:   push    dx
                push    ds
                mov     ax, 3500H + TIMER_INTERRUPT
                int     21H             ; Current timer vector -> timer_save
                mov     WORD PTR cs:timer_save, bx
                mov     WORD PTR cs:timer_save + 2, es
;###            mov     WORD PTR cs:timer_handler, 0EBH OR ((OFFSET timer_passthru - (OFFSET timer_handler + 2)) SHL 8)
                mov     WORD PTR cs:timer_handler, 0EBH OR (3Ah SHL 8)
.ERRE (OFFSET timer_passthru - (OFFSET timer_handler + 2)) EQ 3Ah
                mov     dx, OFFSET timer_handler
                push    cs
                pop     ds              ; ds:dx -> timer_handler
                mov     ax, 2500H + TIMER_INTERRUPT
                int     21H             ; Set timer vector to timer handler
                pop     ds
                pop     dx
                ret

SerialInit      ENDP

SaveSerial      PROC    NEAR

;* Enter with:
;*      DX    = port address
;*      ES:DI = ptr to 5 byte save area
;* 5 bytes of port info is saved for each port as follows:
;*    Interrupt Enable Register
;*    Modem Control Register
;*    Line Control Register
;*    Baud Rate Divisor high byte
;*    Baud Rate Divisor low  byte

                inc     dx
                cli                             ;Disable interrupts
                in      al, dx                  ;Grab INT ENABLE byte
                recover
                stosb
                add     dx, 3                   ;Point to MODEM CONTROL
                in      al, dx
                recover
                stosb
                dec     dx                      ;Point back to LINE CONTROL
                in      al, dx
                recover
                stosb
                sub     dx, LINE_CONTROL        ;Point back to DATA port
                call    GetBaudRate           ;Interrupts re-enabled in here
                xchg    al, ah                  ;Store high byte, then low
                stosw
                ret

SaveSerial      ENDP

RestoreSerial   PROC    NEAR

;* Enter with:
;*      DX    = port address
;*      DS:SI = ptr to 5 byte save area
;*      AH    = bios port number of COM port (0 if none)
;* 5 bytes of port info is restored for each port as follows:
;*    Interrupt Enable Register
;*    Modem Control Register
;*    Line Control Register
;*    Baud Rate Divisor high byte
;*    Baud Rate Divisor low  byte

                inc     dx
                lodsb
                cli                             ;Disable interrupts
                out     dx, al                  ;Update INT ENABLE
                recover
                add     dx, 3                   ;Point to MODEM CONTROL
                lodsb
                out     dx, al
                recover
                dec     dx                      ;Point back to LINE CONTROL
                lodsb
                or      al, 80H                 ;Set DLAB
                out     dx, al
                recover
                dec     dx
                dec     dx
                lodsb
                out     dx, al                  ;Update high byte of divisor
                recover
                lodsb
                dec     dx
                out     dx, al                  ;Update low byte of divisor
                recover
                add     dx, 3                   ;Point back to LINE CONTROL
                mov     al, [si - 3]            ;Get back original line control
                out     dx, al                  ;Update line control
                recover
                sub     dx, 3                   ;Point back to data port
                in      al, dx                  ;Read to prime interrupt pump
                or      ah, ah                  ;BIOS port?
                jz      @F                      ;Not BIOS port - don't reset IRQ

                mov     al, 64H
                dec     ah                      ;Convert 1 - 4 to 0 - 3
                and     ah, 1                   ;Isolate low bit
                sub     al, ah                  ;COM1 & 3: IRQ4, COM2 & 4: IRQ3
                out     20H, al                 ;Specific EOI for IRQ

@@:             sti                             ;Re-enable interrupts
                ret

RestoreSerial   ENDP

SerialExit      PROC    NEAR

                push    ds
                push    dx
                lds     dx, cs:timer_save
                mov     ax, 2500H + TIMER_INTERRUPT
                int     21H
                pop     dx
                pop     ds
                ret

SerialExit      ENDP

GetBaudRate     PROC    NEAR USES BX

;Enter with:
;   DX = serial port base address
;Return:
;   AX = current baud rate divisor

                cli                             ;Disable interrupts
                add     dl, LINE_CONTROL
                in      al, dx
                push    ax                      ;Save original LINE CONTROL
                recover
                or      al, 80H
                out     dx, al                  ;Set DLAB
                sub     dl, 2
                in      al, dx                  ;Grab high byte of divisor
                mov     bh, al
                recover
                dec     dx
                in      al, dx                  ;Grab low byte of divisor
                mov     bl, al
                recover
                add     dl, 3                   ;Point back to LINE CONTROL
                pop     ax                      ;Restore original LINE CONTROL
                and     al, 7FH                 ;Mask off DLAB just in case
                out     dx, al                  ;Reset DLAB
                mov     ax, bx
                sti                             ;Re-enable interrupts
                ret

GetBaudRate     ENDP

GetByte             PROC    NEAR

;* Inputs:
;*      DX = port base address
;* Returns:
;*      JC = no byte available on port
;*      AL = byte received if no carry
;* Uses:
;*      NONE

                add     dl, LINE_STATUS
                in      al, dx
                sub     dl, LINE_STATUS
                test    al, 1
                jz      fail

                in      al, dx
                clc
                ret

fail:           stc
                ret

GetByte             ENDP

SendByte            PROC    NEAR    USES BX

;* Enter with:
;*      AL = byte to send
;*      DX = port base address
;* Returns:
;*      JC = byte could not be sent
;*      JNC= byte sent
;* Uses:
;*      NONE
                
                mov     bl, al
                add     dl, LINE_STATUS
                in      al, dx
                sub     dl, LINE_STATUS
                test    al, 20H
                jz      fail

                mov     al, bl
                out     dx, al
                clc
                ret

fail:           stc
                ret

SendByte            ENDP


SerialListenInit    PROC    NEAR    USES CX SI

;* Enter with:
;*      BX = port vector index
;* Uses:
;*      AX

                mov     fx_listen_vector[bx], OFFSET SerialListenInit
                call    GetPortIndex                    ; index si to FxPortInfo
                test    FxPortInfo[si].FxPortInfoDef.pi_flags, BAD_PORT
                jnz     return

                mov     dx, FxPortInfo[si].FxPortInfoDef.pi_address   ; get port base address
                call    InitPort                        ; initialize port, set baud rate
                mov     word ptr ShowBaud, OFFSET far_null_proc
                mov     word ptr ShowBaud[2], cs
                mov     cs:is_serial, 0
                test    FxPortInfo[si].FxPortInfoDef.pi_flags, BAD_PORT
                jnz     return

                mov     fx_listen_vector[bx], OFFSET ListenGetAA

return:         stc
                ret

SerialListenInit    ENDP


ListenGetAA         PROC    NEAR
            
;* Enter with:
;*      DX = port base address
;*      BX = port vector index
;* Uses:
;*      AX
;* Performs:    Waits to receive AA.  If AA is received, sets vector to Send00.

                call    GetByte
                jc      fail
                               
                cmp     al, 0AAH
                jne     fail


next_state:     call    ResetListenTime
                mov     fx_listen_vector[bx], OFFSET ListenSend00

fail:           stc
                ret

ListenGetAA         ENDP


ListenSend00        PROC    NEAR

;* Enter with:
;*      DX = port base address
;*      BX = port vector index
;* Uses:
;*      AX
;* Performs:    Sends 00.  Sets vector to Get55.

                xor     al, al
                call    SendByte
                jc      fail

                call    ResetListenTime
                mov     fx_listen_vector[bx], OFFSET ListenGet55

fail:           stc
                ret

ListenSend00        ENDP


ListenGet55         PROC    NEAR

;* Enter with:
;*      DX = port base address
;*      BX = port vector index
;* Uses:
;*      AX
;* Performs:    Waits to receive 55.  If 55 is received, sets vector to SendFF.

                call    GetByte
                jc      fail

                cmp     al, 055H
                jne     fail


next_state:     call    ResetListenTime
                mov     fx_listen_vector[bx], OFFSET ListenSendFF

fail:           stc
                ret

ListenGet55         ENDP


ListenSendFF        PROC    NEAR

;* Enter with:
;*      DX = port base address
;*      BX = port vector index
;* Uses:
;*      AX
;* Performs:    Sends 00.  Sets vector to Get5A.

                mov     al, 0FFH
                call    SendByte
                jc      fail

                mov     fx_listen_vector[bx], OFFSET ListenGet5A

fail:           stc
                ret

ListenSendFF        ENDP


ListenGet5A         PROC    NEAR

;* Enter with:
;*      DX = port base address
;*      BX = port vector index
;* Uses:
;*      AX
;* Performs:    Waits to receive 5A.  If 5A is received, sets vector to Send11.

                call    GetByte
                jc      fail

                cmp     al, 05AH
                jne     fail


next_state:     call    ResetListenTime
                mov     fx_listen_vector[bx], OFFSET ListenSend11

fail:           stc
                ret

ListenGet5A         ENDP


ListenSend11        PROC    NEAR

;* Enter with:
;*      DX = port base address
;*      BX = port vector index
;* Uses:
;*      AX
;* Performs:    Sends 11.  Resets vector to SerialListenInit on success.

                mov     al, 011H
                call    SendByte
                jc      fail

                mov     fx_listen_vector[bx], OFFSET SerialListenInit
                mov     al, FxSettings.FxSettingsDef.fxs_max_serial_baud
                mov     fx_max_serial_baud, al
                mov     ax, FxSettings.FxSettingsDef.fxs_max_serial_block
                mov     fx_max_serial_block, ax   ; start at maximum block size
                mov     first_send, 1
                mov     server, 1           ; listen means I'm the server
                mov     client, 0
                mov     need_send_echo, 0
                mov     fx_send_variable, 0
                mov     fx_recv_variable, 0
                mov     fx_errno, 0
                mov     cs:is_serial, 1
                call    check_windows
                clc
                ret

fail:           stc
                ret

ListenSend11        ENDP


modify_code     PROC    NEAR

; Destroys NOTHING

                push    ax
                push    bx
                push    di
                push    si

                cmp     cs:timeout, 0
                jne     fixup_return

                mov     cs:timeout, 1
                mov     si, OFFSET sbs1
                mov     ax, 0EBH OR ((OFFSET send_byte_fail - (OFFSET sbs1 + 2)) SHL 8)
                xchg    ax, cs:[si]
                mov     cs:code_save[0], ax
                mov     si, OFFSET rbs1
                mov     ax, 0EBH OR ((OFFSET recv_byte_fail - (OFFSET rbs1 + 2)) SHL 8)
                xchg    ax, cs:[si]
                mov     cs:code_save[2], ax
                mov     di, cs:code_list_ptr
                xor     bx, bx

save_code_loop: mov     si, cs:[di + bx + 2]
                or      si, si
                jz      fixup_return

                mov     ax, cs:[di]     ; Get vector to fail address
                sub     ax, si          ; Get forward jump delta to AX
                sub     ax, 2           ; Adjust size of short jump
                cmp     ax, 127
                jbe     in_range

                mov     ax, cs:[di + bx + 4]    ; Grab following jump address
                sub     ax, si
                sub     ax, 2

in_range:       mov     ah, al
                mov     al, 0EBH        ; Jump short directly to fail location.
                xchg    ax, cs:[si]
                mov     cs:code_save[bx + 4], ax
                add     bx, 2
                jmp     save_code_loop

fixup_return:   pop     si
                pop     di
                pop     bx
                pop     ax
                ret

modify_code     ENDP


; clear_alarm_ser -- Reset the timer interrupt handler to timer_passthru
;
; Destroys registers: None, FLAGS ARE PRESERVED ALSO
;
clear_alarm_ser PROC    NEAR

                pushf
;###            mov     WORD PTR cs:timer_handler, 0EBH OR ((OFFSET timer_passthru - (OFFSET timer_handler + 2)) SHL 8)
                mov     WORD PTR cs:timer_handler, 0EBH OR (3AH SHL 8)
.ERRE (OFFSET timer_passthru - (OFFSET timer_handler + 2)) EQ 3AH
                push    ax              ; Preserve Register
                push    cx

                cmp     polite_timer, 0
                jne     cascade_done

                mov     al, interrupt_mask
                out     21H, al
                mov     cx, cs:lost_ticks
                jcxz    cascade_done

                cli

ca_cascade:     pushf                   ; make up lost timer ticks
                call    cs:timer_save
                loop    ca_cascade

                sti
                mov     cs:lost_ticks, 0

cascade_done:   cmp     cs:timeout, 0
                je      clear_done

                push    bx
                push    di
                push    si
                mov     si, OFFSET sbs1
                mov     ax, cs:code_save[0]
                mov     cs:[si], ax
                mov     si, OFFSET rbs1
                mov     ax, cs:code_save[2]
                mov     cs:[si], ax
                mov     di, cs:code_list_ptr
                xor     bx, bx

restore_code:   mov     si, cs:[di + bx + 2]
                or      si, si
                jz      restore_done

                mov     ax, cs:code_save[bx + 4]
                mov     cs:[si], ax
                add     bx, 2
                jmp     restore_code

restore_done:   pop     si
                pop     di
                pop     bx

clear_done:     pop     cx
                pop     ax
                popf
                ret

clear_alarm_ser ENDP


; set_alarm_ser -- Starts the alarm counting down ticks.
;
; Destroys registers: None
;
; set_alarm_ser changes the timer interrupt vector from timer_passthru to
; timer_handler_ser.  Before calling this routine alarm_vector should be set 
; to the offset of the timeout code and ticks_remaining should be set to the 
; number of ticks before a timeout should occur.  Since the granularity of the 
; alarm is ñ 1 tick, ticks_remaining should always be set to 2 or more (if set 
; to 1, a timeout could occur immediately after the return from this routine).
;
set_alarm_ser   PROC    NEAR

; Destroys NOTHING

                push    ax
                mov     cs:timeout, 0
                mov     cs:lost_ticks, 0    ; Clear the lost ticks count
                mov     al, win386_enh_mode ; Always polite if Windows 386 Enh
                or      al, hp_95lx         ; Always polite if HP 95LX
                or      al, fx_force_variable   ; Always polite if forced variable
                or      polite_timer, al
                cmp     polite_timer, 0
                jnz     set_polite

                cmp     fx_baud, BAUD_9600
                ja      mask_ints

                mov     polite_timer, 1 ; 9600 baud or slower - always polite
                jmp     short set_polite

mask_ints:      in      al, 21H         ; Get current interrupt mask
                mov     interrupt_mask, al

                mov     al, INT_MASK    ; Set our interrupt mask
                out     21H, al

                mov     WORD PTR cs:timer_handler, 0B050H   ; <=== modifies instruction
                jmp     short set_alarm_ret

set_polite:     call    GetTicks
                mov     cs:set_alarm_time, ax
                mov     WORD PTR cs:timer_handler, 0EBH OR (16H SHL 8)
.ERRE (OFFSET polite_handler - (OFFSET timer_handler + 2)) EQ 16H

set_alarm_ret:  pop     ax
                ret

set_alarm_ser   ENDP

; send_byte_ser Send byte on serial port.
;               Updates CRC (doesn't support checksum)
;
; The timeout routines must already be executing before this routine is called.
;
; Inputs:
;   ax      Current CRC
;   bl      Byte to be sent
;   dx      Base I/O port
;
; Outputs:
;   ax      Updated CRC
;
;   If control returns inline, the character has been sent.  The only other
;   way out of this routine is through a timeout.

send_byte_fail  LABEL   NEAR

                pop     ax              ; restore stack frame
                sub     dl, LINE_STATUS ; Point back to data output register
                add     sp, 2           ; Pop off the return address
                jmp     WORD PTR cs:alarm_vector

send_byte_ser   PROC    NEAR

                push    ax              ; preserve current CRC
                add     dl, LINE_STATUS ; set dx to status port

@@:             in      al, dx          ; Wait for transmitter holding to empty
                test    al, 20H
sbs1::          jz      @B

                sub     dl, LINE_STATUS ; Point back to data output register
                mov     al, bl          ; Get the byte to output.
                out     dx, al          ; and output it.

                pop     ax              ; restore current CRC value
                push    bx
                xchg    ah, al
                xor     al, bl
                xor     bx, bx
                xchg    bl, al
                shl     bx, 1
                xor     ax, __crctab[bx]
                pop     bx
                ret

send_byte_ser   ENDP

send_byte_echo  PROC    NEAR

                cmp     fx_send_variable, 0
                je      send_byte_ser

                pop     echo_ret            ; Pop off return address
                mov     echo_byte, bl
                call    send_byte_ser
                mov     ax_save, ax

wait_echo:      call    bp
                cmp     bl, echo_byte
                jne     wait_echo

                mov     ax, ax_save
                jmp     echo_ret

send_byte_echo  ENDP

recv_byte_echo  PROC    NEAR

                pop     echo_ret
                call    bp
                push    ax
                mov     al, bl
                call    SendByte
                pop     ax
                jmp     echo_ret

recv_byte_echo  ENDP

drain           PROC    NEAR

                push    ax
@@:             add     dl, LINE_STATUS
                in      al, dx
                sub     dl, LINE_STATUS
                test    al, 1
                jz      empty

                in      al, dx
                jmp     @b

empty:          pop     ax
                ret

drain           ENDP

; recv_byte_ser Get a byte from serial port.  Updates CRC
;
;   NOTE:   This does not support CheckSum and is only used in 3 wire mode.
;
; The timeout routines must already be executing before this routine is called.
;
; Inputs:
;   ax      Current CRC
;   dx      Base I/O port
;
; Outputs:
;   ax      Updated CRC
;   bl      Read character (May be garbled if an error occured)
;
;   If control returns in line, the character was successfully read
;

recv_byte_fail  LABEL   NEAR

                sub     dl, LINE_STATUS ; Point DX back where it started
                add     sp, 2           ; Pop off return address from stack
                jmp     WORD PTR cs:alarm_vector

recv_byte_ser   PROC    NEAR

                mov     bl, al          ; Save AL in BL

next_byte:      add     dl, LINE_STATUS

@@:             in      al, dx          ; Wait for received data ready
                shr     al, 1
rbs1::          jnc     @b

                sub     dl, LINE_STATUS
                test    al, ((FRAMING_ERROR OR PARITY_ERROR OR OVERRUN_ERROR) SHR 1)
                jnz     record_error    ; Record error

rb_grab::       in      al, dx          ; grab the character

                xchg    bl, al          ; return byte recvd in bl, restore CRC
                push    bx
                xchg    ah, al
                xor     al, bl
                xor     bx, bx
                xchg    bl, al
                shl     bx, 1
                xor     ax, __crctab[bx]
                pop     bx
                ret

record_error:   mov     recv_byte_error, al
                in      al, dx
                jmp     next_byte

recv_byte_ser   ENDP

recv_byte_win   PROC    NEAR

                mov     bl, al          ; Save AL in BL

recv_next:      add     dl, LINE_STATUS 

recv_look:      in      al, dx          ; Wait for transmitter holding to empty
                shr     al, 1           ; Data Ready?
                jnc     recv_chek_time

                sub     dl, LINE_STATUS
                test    al, ((FRAMING_ERROR OR PARITY_ERROR OR OVERRUN_ERROR) SHR 1)
                jz      rb_grab         ; ====> Jump to code in above PROC

                mov     recv_byte_error, al
                in      al, dx          ; Throw away possible garbage character
                jmp     recv_next

recv_chek_time: push    ax
                call    GetTicks
                sub     ax, cs:set_alarm_time
                cmp     ax, cs:ticks_remaining
                pop     ax
                jl      recv_look

                jmp     recv_byte_fail

recv_byte_win   ENDP

send_baud_rate  PROC    NEAR  USES BX CX DI
;   Inputs:
;       cl          desired baud rate index
;   Outputs:        JC: Set baud attempt failed, fx_errno = -4
;                   JNC: Baud rate changed

                mov     al, fx_baud
                mov     prior_baud, al

                mov     bl, seq_num
                not     bl
                DBG     '{'
                call    send_sync_ser
                jc      sync_fail

                DBG     '}'
                mov     cs:alarm_vector, OFFSET request_fail
                mov     cs:code_list_ptr, OFFSET null_code_list
                mov     cs:ticks_remaining, MINIMUM_TICKS
                mov     polite_timer, 1
                call    set_alarm_ser

                mov     bl, cl
                call    send_byte_ser       ; send back desired baud

                mov     al, bl
                call    clear_alarm_ser
                call    SetBaud
                jc      request_fail

                mov     cs:alarm_vector, OFFSET change_fail
                mov     cs:ticks_remaining, 2 * MINIMUM_TICKS
                mov     polite_timer, 1
                call    set_alarm_ser

                mov     ch, cl              ; save baud rate in ch
                shl     cl, 1               ; shift baud rate to high nibble
                shl     cl, 1
                shl     cl, 1
                shl     cl, 1
                or      cl, seq_num         ; put seq_num in low nibble

get_ack:        call    bp                  ; get new baud 
                cmp     bl, cl
                jne     get_ack

                mov     baud_list1[0], cl
                not     cl
                mov     baud_list2[0], cl
                xor     di, di

sbr_send_loop:  mov     bl, baud_list1[di]
                call    send_byte_ser       ; send new baud rate again
sbr_recv_loop:  call    bp
                cmp     bl, baud_list2[di]
                jne     sbr_recv_loop

                inc     di
                cmp     di, BAUD_LIST_SIZE
                jb      sbr_send_loop

return_ok:      call    clear_alarm_ser
                mov     fx_errno, 0
                clc
                ret

sync_fail:      mov     fx_errno, ax
                ret

request_fail:   call    clear_alarm_ser
                DBG     '*'
                mov     fx_errno, -4
                xor     ax, ax
                stc
                ret

change_fail:    call    clear_alarm_ser
                cmp     ch, prior_baud
                jb      request_fail        ; change error on step down is fatal
                
                DBG     '#'
                mov     al, prior_baud
                mov     fx_max_serial_baud, al
                call    SetBaud
                jc      request_fail

                jmp     return_ok

time_out:       xor     ax, ax
                mov     fx_errno, 4
                stc
                ret

send_baud_rate  ENDP

goto_max_baud   PROC    NEAR
                
                cmp     first_send, 1
                jne     goto_max_ret

                push    cx
                mov     cl, fx_baud

max_loop:       cmp     cl, fx_max_serial_baud
                je      done                ; done if at max already

                jb      advance

                mov     cl, fx_max_serial_baud
                jmp     short change_rate

advance:        inc     cl
                DBG     ''

change_rate:    call    send_baud_rate
                mov     cl, fx_baud         ; Get actual new baud rate
                jnc     max_loop

done:           pop     cx
                mov     first_send, 0

goto_max_ret:   ret

goto_max_baud   ENDP

SendSerial      PROC    NEAR
;   Inputs:
;       cx          Length of buffer to be sent
;       es:si       Pointer to buffer to be sent
;
;   Uses:   ax, bx, cx, si

                mov     bl, fx_error_checking_mode
                mov     dx, serial_port     ; set up port in DX
                mov     send_remaining, cx
                call    setup_recv_byte
                call    goto_max_baud       ; connected with connect, crank baud

next_block:     mov     cx, send_remaining
                jcxz    split

                cmp     cx, fx_max_serial_block     ; see if packet > block size
                jbe     no_split

split:          or      bl, CONTINUED_PACKET        ; indicate packet to follow
                mov     cx, fx_max_serial_block
                jmp     short send_it

no_split:       and     bl, NOT CONTINUED_PACKET

send_it:        and     bl, NOT ECHOPLEX    ; setup echoplex flag
                or      bl, need_send_echo
                call    SendSerialBlock     ; attempt transmission
                cmp     fx_errno, -1        ; check for failure
                jl      packet_fail

                cmp     fx_errno, 0
                jne     send_ser_ret

                test    bl, CONTINUED_PACKET
                jz      send_ser_ret

                mov     ax, fx_max_serial_block
                sub     send_remaining, ax
                add     si, ax
                mov     send_fails, 0
                jmp     next_block     ; send the rest

packet_fail:    cmp     fx_send_variable, 0 ; first recourse on failure is to
                jne     @F                  ; send echo_plex

                mov     send_fails, 0
                mov     need_send_echo, ECHOPLEX
                mov     fx_send_variable, ECHOPLEX
                call    NearShowBaud
                jmp     next_block                  ; Try again in echoplex


@@:             inc     send_fails
                cmp     send_fails, MAX_SEND_FAILS
                jb      next_block

                mov     cl, fx_baud
                cmp     cl, CONNECT_BAUD
                ja      shift_down

                mov     fx_errno, -1
                mov     fx_port, -1
                xor     ax, ax
                jmp     send_ser_ret

shift_down:     dec     cl                  ; drop down one baud rate
                DBG     ''
                call    send_baud_rate
                jc      send_ser_ret

                mov     send_fails, 0
                mov     fx_send_variable, 0
                mov     need_send_echo, 0
                call    NearShowBaud
                jmp     next_block          ; try again

send_ser_ret:   ret
                
SendSerial      ENDP

NearShowBaud    PROC    NEAR USES ES AX BX CX DX

                call    dword ptr ShowBaud          ; call to user's ShowBaud
                ret

NearShowBaud    ENDP

send_sync_ser   PROC    NEAR    USES BP BX CX
;   Inputs:     
;            BL  synchronization byte, status flags w/ sequence number
;            DX  serial port base address
;   Returns: JC  sync failure
;            fx_send_variable set to receiver's request
;   Uses:
;           AX

                call    SyncStart
                mov     cx, ax              ; Save original tick count in CX
                cmp     last_operation, RECV_OPERATION
                jne     sent_last

                mov     bp, OFFSET wait_turn

wait_turn:      call    GetByte
                jc      check_abort

                HEX     al
                DBG     ','
                cmp     al, seq_num
                jne     check_abort

                mov     last_operation, SEND_OPERATION

sent_last:      mov     bp, OFFSET send_sync
                call    drain               ; Drain away any leftover bytes

send_sync:      mov     al, bl              ; get synchronization byte
                call    SendByte
                jc      check_abort

                HEX     al
                DBG     '|'
                not     bl                  ; sync_ans is inverse of sync
                and     bl, NOT ECHOPLEX
                mov     bp, OFFSET get_ans

get_ans:        call    GetByte
                jc      check_abort

                HEX     al
                mov     bh, al
                and     bh, ECHOPLEX
                and     al, NOT ECHOPLEX
                cmp     al, bl
                jne     check_abort

                cmp     bh, fx_send_variable
                je      @F

                mov     fx_send_variable, bh
                call    NearShowBaud

@@:             clc                         ; carry clear on success
                jmp     short return

check_abort:    call    GetTicks
                sub     ax, cx
                cmp     ax, SyncTimeout
                ja      time_out

                call    CheckAbort
                jc      error_exit

                jmp     bp

time_out:       mov     ax, FX_ERR_TIMEOUT

error_exit:     mov     fx_errno, ax

                xor     ax, ax
                stc

return:         call    SyncDone
                ret

send_sync_ser   ENDP

; SendSerialBlock
;   Inputs:
;       bl          Will have status bits for sync exchange set
;       cx          Length of buffer to be sent
;       es:si       Pointer to buffer to be sent
SendSerialBlock PROC    NEAR   USES BX SI

                DBG     ''
                or      ch, ch              ; see if length fits in byte
                jnz     word_length         ; clear bit if not
                
                or      bl, BYTE_LENGTH     ; set byte length bit
                jmp     short byte_length

word_length:    and     bl, NOT BYTE_LENGTH ; clear byte length bit

byte_length:    or      bl, seq_num         ; add sequence to status bits
                mov     packet_ok, bl       ; packet_ok is original sync
                DBG     '['
                call    send_sync_ser
                jc      ssb_sync_fail
                           
                DBG     ']'
                push    bx
                xor     bx, bx              ; zero al and or in send states
                or      bl, fx_send_variable
                or      bl, fx_serial_7_wire
                or      bl, fx_error_checking_mode
                shr     bl, 1               ; convert to table vector
                mov     ax, send_table[bx]
                pop     bx
                call    ax
                jc      ssb_fail

                inc     seq_num
                and     seq_num, SEQUENCE_BITS
                mov     ax, 1
                mov     fx_errno, 0

ssb_return:     ret

ssb_sync_fail:  DBG     '*'
                jmp     ssb_return

ssb_fail:       mov     fx_errno, FX_ERR_FAIL
                xor     ax, ax
                jmp     ssb_return

SendSerialBlock ENDP

send_3_echo     PROC    NEAR

;*  USES:   AX, BX, CX, SI

                DBG     '{'
                DBG     '3'
                DBG     'E'
                mov     cs:alarm_vector, OFFSET sp_fail
                mov     cs:ticks_remaining, 2 * MINIMUM_TICKS
                mov     cs:code_list_ptr, OFFSET null_code_list
                mov     polite_timer, 1
                call    set_alarm_ser

                mov     ax, INITIAL_CRC     ; start CRC calculations in AX
                mov     bl, cl              ; send length (LSB)
                call    send_byte_echo

                or      ch, ch              ; see if length was < 256
                jz      send_data

                mov     bl, ch              ; send length (MSB)
                call    send_byte_echo

send_data:      mov     cs:ticks_remaining, 2 * MINIMUM_TICKS
                mov     bl, es:[si]
                inc     si
                call    send_byte_echo
                loop    send_data

                DBG     '}'
                mov     bx, ax              ; get CRC
                not     bx
                mov     cs:ticks_remaining, 2 * MINIMUM_TICKS
                xchg    bl, bh              ; send high byte
                call    send_byte_echo
                mov     bl, bh              ; send low byte
                call    send_byte_ser

                mov     cs:ticks_remaining, 2 * MINIMUM_TICKS
                call    bp
                cmp     bl, packet_ok
                je      send_3_ok

                DBG     '@'
                HEX     bl

sp_fail:        stc
                DBG     '#'       
                jmp     short return

send_3_ok:      clc

return:         call    clear_alarm_ser
                ret

send_3_echo     ENDP

send_3_norm     PROC    NEAR

;*  USES:   AX, BX, CX, SI

                DBG     '{'
                DBG     '3'
                DBG     'N'
                DBG     '('
                HEX     ch
                HEX     cl
                DBG     ')'
                mov     cs:alarm_vector, OFFSET sp_fail
                mov     cs:ticks_remaining, 2 * MINIMUM_TICKS
                mov     cs:code_list_ptr, OFFSET null_code_list
                mov     polite_timer, 1
                call    set_alarm_ser

                mov     ax, INITIAL_CRC     ; start CRC calculations in AX
                mov     bl, cl              ; send length (LSB)
                call    send_byte_ser

                or      ch, ch              ; see if length was < 256
                jz      send_data_loop

                mov     bl, ch              ; send length (MSB)
                call    send_byte_ser

send_data_loop: mov     cs:ticks_remaining, 2 * MINIMUM_TICKS
                mov     bl, es:[si]
                inc     si
                call    send_byte_ser
                loop    send_data_loop

                DBG     '}'
                mov     cs:ticks_remaining, 2 * MINIMUM_TICKS
                mov     bx, ax              ; get CRC
                not     bx
                xchg    bl, bh              ; send high byte
                call    send_byte_ser
                mov     bl, bh              ; send low byte
                call    send_byte_ser

                mov     cs:ticks_remaining, 2 * MINIMUM_TICKS
                call    bp
                cmp     bl, packet_ok
                jne     sp_fail

                clc

return:         call    clear_alarm_ser
                ret

sp_fail:        stc
                DBG     '#'       
                DBG     '('
                HEX     ch
                HEX     cl
                DBG     ')'
                jmp     short return

send_3_norm     ENDP

recv_baud_rate  PROC    NEAR USES BX CX DI SI BP

                call    drain
                mov     cs:alarm_vector, OFFSET request_fail
                mov     cs:code_list_ptr, OFFSET null_code_list
                mov     cs:ticks_remaining, MINIMUM_TICKS
                mov     polite_timer, 1
                call    set_alarm_ser

                HEX     bl
                DBG     ']'
                call    send_byte_ser       ; send inverse of received byte
                DBG     '<'
                call    bp                  ; expect back desired baud
                HEX     bl
                call    clear_alarm_ser
                cmp     bl, fx_baud
                jb      recv_change         ; Going down - don't look at max

                cmp     bl, fx_max_serial_baud
                ja      request_ok          ; Don't go above max_baud

recv_change:    DBG     '>'
                mov     al, bl
                mov     bl, fx_baud
                mov     prior_baud, bl
                call    SetBaud
                jc      fatal_fail

                mov     bh, fx_baud
                mov     cl, 4
                shl     bh, cl
                or      bh, seq_num

                mov     al, bh
                not     al
                mov     baud_list2[0], al
                xor     di, di

rbr_save_time:  call    GetTicks
                mov     cx, ax              ; Save original tick count in CX
                mov     si, ax              ; and a copy in SI

send_loop1:     mov     bp, OFFSET send_loop1
                mov     al, bh
                call    SendByte
                jc      check_time

                call    GetTicks
                mov     si, ax
                mov     bp, OFFSET get_byte_loop

get_byte_loop:  call    GetByte
                jnc     got_byte

                call    GetTicks
                push    ax
                sub     ax, si
                pop     si
                jnz     send_loop1

                jmp     short check_time

got_byte:       cmp     al, bh
                jne     check_time

send_loop:      mov     bp, OFFSET send_loop
                mov     al, baud_list2[di]
                call    SendByte
                jc      check_time

                mov     bp, OFFSET recv_loop
                inc     di
                cmp     di, BAUD_LIST_SIZE
                jae     request_ok

recv_loop:      call    GetByte
                jc      check_time

                cmp     al, baud_list1[di]
                je      send_loop

check_time:     call    GetTicks
                sub     ax, cx
                cmp     ax, MINIMUM_TICKS
                ja      change_fail

                jmp     bp

request_fail:   call    clear_alarm_ser
request_ok:     clc                         ; assume false request, stay in 
                ret                         ; sync loop

change_fail:    mov     al, prior_baud
                DBG     '#'
                cmp     al, fx_baud
                jae     fatal_fail          ; change error on step down is fatal

                mov     fx_max_serial_baud, al
                call    SetBaud
                jnc     request_ok

fatal_fail:     mov     ax, 4
                DBG     '~'
                stc
                ret

recv_baud_rate  ENDP

RecvSerial      PROC    NEAR    USES BP
;   Inputs:
;       cx          Length of buffer to be received
;    es:di          Pointer to buffer to be received

                call    setup_recv_byte
                xor     bx, bx              ; count received

next_block:     push    cx                  ; preserve length remaining
                push    di                  ; preserve buffer pointer
                push    bx                  ; preserve current count
                call    RecvSerialBlock
                jc      recv_error

                test    bl, CONTINUED_PACKET; see if split block received
                jz      return              ; if not we're done

                pop     bx                  ; get prior length
                add     bx, ax              ; add received length
                pop     di                  ; restore buffer pointer
                pop     cx                  ; restore length remaining
                sub     cx, ax              ; subtract received length
                add     di, ax              ; move up buffer pointer
                jmp     next_block

recv_error:     cmp     fx_errno, -1        ; no retry if we didn't sync
                je      return_error

                cmp     fx_errno, 0
                jg      return_error

                pop     bx
                pop     di
                pop     cx
                jmp     next_block          ; try again

return:         pop     bx                  ; get prior length
                add     bx, ax              ; add received length
                pop     di
                pop     cx
                mov     ax, bx
                jmp     short rs_ret

return_error:   pop     bx
                pop     di
                pop     cx
                xor     ax, ax              ; return 0 for error

rs_ret:         ret

RecvSerial      ENDP

recv_sync_ser   PROC    NEAR    USES CX
;   Inputs:
;           DX  serial port base address
;   Returns:
;           BL  sync byte received
;   Uses:
;           NONE

                push    bp
                call    SyncStart           ; Returns current tick count in AX
                mov     cx, ax              ; Save original tick count in CX
                DBG     '['

                cmp     last_operation, SEND_OPERATION
                jne     recvd_last

                mov     al, seq_num
                mov     bp, OFFSET send_seq

send_seq:       call    SendByte
                jc      check_abort

                HEX     al
                DBG     ','
                mov     last_operation, RECV_OPERATION

recvd_last:     mov     bp, OFFSET wait_sync

wait_sync:      call    GetByte
                jc      check_abort

                mov     ah, seq_num
                xor     ah, 1               ; Toggle low order sequence bit
                or      ah, IDLE_BITS
                cmp     al, ah
                jne     not_idle

                mov     bp, OFFSET send_idle

send_idle:      call    SendByte            ; Echo back what we received
                jc      check_abort

recv_restart:   call    GetTicks
                mov     cx, ax
                mov     bp, OFFSET wait_sync
                jmp     short check_abort

not_idle:       HEX     al
                DBG     '|'
                mov     bl, al
                and     al, SEQUENCE_BITS   ; check for sequence
                cmp     al, seq_num
                je      send_reply

                not     al                  ; check for inverse of sequence
                and     al, SEQUENCE_BITS
                cmp     al, seq_num
                jne     check_abort

                not     bl
                pop     bp
                call    recv_baud_rate
                push    bp
                jc      error_exit

                jmp     recv_restart

send_reply:     call    drain
                mov     packet_ok, bl
                clc                         ; carry clear on success
                jmp     short return

check_abort:    call    GetTicks
                sub     ax, cx
                cmp     ax, SyncTimeout
                ja      time_out

                call    CheckAbort
                jc      error_exit

                jmp     bp

time_out:       mov     ax, FX_ERR_TIMEOUT
                DBG     '$'

error_exit:     mov     fx_errno, ax
                xor     ax, ax
                stc

return:         call    SyncDone
                pop     bp
                ret

recv_sync_ser   ENDP

; recv_pack_ser     Recieve a packet
;   Inputs:
;       es:di       pointer to buffer
;       cx          max size of buffer
;
;   Outputs:
;       bl          sync byte received
;       CF          Set if packet recieved
;                   Clear if timeout or some other error
;       cx          If no error, number of bytes recieved.  Otherwise, unknown.
;
;   Uses registers: ax bx cx dx si di bp
;
; Slave definition:
;   Inputs:
;       buffer      Far pointer to buffer for data
;       count       size of buffer
;
;   Globals:
;       serial_port base register of port to use
;
;   Returns:
;       integer     number of bytes recieved
;                   zero if some error occured
;
; extern unsigned int recv_pack_ser(void far *buffer, unsigned int count);
;
RecvSerialBlock      PROC    NEAR USES si di

                DBG     ''
                mov     recv_buff_ptr, di   ; Save original buff ptr
                mov     fx_errno, 0
                mov     dx, serial_port
                call    recv_sync_ser
                jc      rp_ret              ; Return with carry set for error

                xor     al, al              ; al to be mode change indicator
                mov     ah, bl
                and     ah, ECHOPLEX        ; isolate echoplex bit
                cmp     fx_force_variable, 0
                jne     force_echoplex

                cmp     win386_enh_mode, 0
                jne     force_echoplex      ; Force echoplex if 386 Enhanced Mode

                cmp     hp_95lx, 0          ; Force echoplex if on HP 95LX
                je      @F

force_echoplex: or      ah, ECHOPLEX

@@:             cmp     ah, fx_recv_variable   
                je      @F                     
                                               
                mov     fx_recv_variable, ah
                inc     ax

@@:             cmp     al, 0
                je      dispatch

                call    NearShowBaud

dispatch:       mov     recv_byte_error, 0
                push    bx
                xor     bx, bx
                or      bl, fx_serial_7_wire
                or      bl, fx_recv_variable
                or      bl, fx_error_checking_mode
                shr     bl, 1
                mov     ax, recv_table[bx]
                pop     bx
                call    ax

rp_ret:         ret

RecvSerialBlock      ENDP

recv_3_norm     PROC    NEAR

;*  INPUTS:
;*          BL = sync byte received from sender.
;*          CX = receive buffer length.
;*          DX = serial port.
;*          BP = recv_byte_ser or recv_byte_win.
;*       ES:DI = pointer to receive buffer.
;*  OUTPUTS:
;*          AX = length received.
;*          BL = sync byte received from sender.
;*
;*  USES:   BH

                mov     cs:alarm_vector, OFFSET r3n_fail
                mov     cs:ticks_remaining, MINIMUM_TICKS
                mov     cs:code_list_ptr, OFFSET null_code_list
                mov     polite_timer, 0
                call    set_alarm_ser
                mov     si, bx          ; Save sync flags in low byte of SI
                not     bl              ; Complement what we got in recv_sync
                and     bl, NOT ECHOPLEX
                xor     bh, bh          ; zero high byte of length
                HEX     bl
                DBG     ']'
                call    send_byte_ser   ; Send back reply
                mov     ax, INITIAL_CRC ; start CRC calculation
                call    bp              ; Length (LSB)
                test    si, BYTE_LENGTH ; Is length only 1 byte?
                jnz     skip_len_byte

                mov     bh, bl          ; save LSB in bh
                call    bp              ; Length (MSB)
                xchg    bh, bl          ; flip bytes to correct order

skip_len_byte:  dec     bx
                dec     cx
                cmp     bx, cx          ; See if packet is to big for buffer
                jbe     @F

                call    dump_block

@@:             inc     bx
                mov     cx, bx          ; Count to CX
                DBG     '{'
                DBG     '3'
                DBG     'N'

read_data_loop: call    bp
                mov     cs:ticks_remaining, MINIMUM_TICKS
                mov     es:[di], bl
                inc     di
                loop    read_data_loop

                DBG     '}'
                call    bp              ; recv CRC bytes
                call    bp
                cmp     recv_byte_error, 0
                je      check_crc

                DBG     '%'
                call    dump_block

check_crc:      cmp     ax, MAGIC_CRC   ; Is it a match?
                je      send_ok

                DBG     '~' 
                call    dump_block

send_ok:        mov     bx, si          ; return sync byte received to sender
                mov     cs:ticks_remaining, MINIMUM_TICKS
                call    send_byte_ser

                call    clear_alarm_ser
                inc     seq_num
                and     seq_num, 3
                mov     ax, di
                sub     ax, recv_buff_ptr
                mov     fx_errno, 0
                clc

rp_ret:         mov     bx, si          ; return split flag for RecvSerial
                ret

r3n_fail:       call    clear_alarm_ser ; Clear alarm clock
                DBG     '#'
                mov     fx_errno, FX_ERR_FAIL
                xor     ax, ax
                stc
                jmp     rp_ret

recv_3_norm     ENDP

recv_3_echo     PROC    NEAR

                mov     cs:alarm_vector, OFFSET r3e_fail
                mov     cs:ticks_remaining, MINIMUM_TICKS
                mov     cs:code_list_ptr, OFFSET null_code_list
                mov     polite_timer, 1 ; Polite timer for echo mode!
                call    set_alarm_ser
                mov     si, bx
                not     bl
                or      bl, ECHOPLEX
                xor     bh, bh
                HEX     bl
                DBG     ']'
                call    send_byte_ser
                mov     ax, INITIAL_CRC ; start CRC calculation
                call    recv_byte_echo  ; Length (LSB)
                test    si, BYTE_LENGTH ; Is length only 1 byte?
                jnz     skip_len_byte

                mov     bh, bl          ; save LSB in bh
                call    recv_byte_echo  ; Length (MSB)
                xchg    bh, bl          ; flip bytes to correct order

skip_len_byte:  dec     bx
                dec     cx
                cmp     bx, cx
                jbe     @F

                call    dump_block

@@:             inc     bx
                mov     cx, bx          ; Count to CX
                DBG     '{'
                DBG     '3'
                DBG     'E'

read_data:      call    recv_byte_echo
                mov     cs:ticks_remaining, MINIMUM_TICKS
                mov     es:[di], bl
                inc     di
                loop    read_data

                DBG     '}'
                call    recv_byte_echo  ; recv CRC bytes
                call    bp
                cmp     recv_byte_error, 0
                je      check_crc

                DBG     '%'
                call    dump_block

check_crc:      cmp     ax, MAGIC_CRC   ; Is it a match?
                je      send_ok

                DBG     '~' 
                call    dump_block

send_ok:        mov     bx, si          ; return sync byte received to sender
                mov     cs:ticks_remaining, MINIMUM_TICKS
                call    send_byte_ser

                call    clear_alarm_ser
                inc     seq_num
                and     seq_num, SEQUENCE_BITS
                mov     ax, di
                sub     ax, recv_buff_ptr   ; Calculate # of received bytes
                mov     fx_errno, 0
                clc

rp_ret:         mov     bx, si          ; return split flag for RecvSerial
                ret

r3e_fail:       call    clear_alarm_ser ; Clear alarm clock
                DBG     '#'
                mov     fx_errno, FX_ERR_FAIL
                xor     ax, ax
                stc
                jmp     rp_ret

recv_3_echo     ENDP

dump_block      PROC    NEAR

;*  INPUTS:
;*          DX = serial port.
;*          BP = recv_byte_ser or recv_byte_win.
;*  NOTE:
;*          dump_block only returns to the fail vector.  Alarms must be set
;*          by the caller.
;*
;*  USES:   BL

                add     sp, 2           ; prepare stack for return via alarm

@@:             mov     cs:ticks_remaining, MINIMUM_TICKS
                call    bp
                jmp     @B

dump_block      ENDP


FxShowBaud      PROC    PASCAL USES ES, func:FAR PTR

                les     ax, func
                mov     word ptr ShowBaud, ax
                mov     word ptr ShowBaud[2], es
                ret

FxShowBaud      ENDP




setup_recv_byte PROC    NEAR

                mov     bp, OFFSET recv_byte_ser
                cmp     win386_enh_mode, 0
                je      setup_ret

                mov     bp, OFFSET recv_byte_win
                mov     fx_serial_7_wire, 0 ; disable seven wire in windows

setup_ret:      ret

setup_recv_byte ENDP


	            END
