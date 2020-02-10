;***
;* $Workfile:   serial.asm  $
;* $Revision:   1.3  $
;*   $Author:   Dave Sewell  $
;*     $Date:   25 Jul 1990 12:05:02  $
;***

                TITLE   Serial communications subroutines
                PAGE    66, 132

;NO_TIMEOUT     EQU     1

                INCLUDE drivers.mac
                INCLUDE debug.mac

                PUBLIC  send_pack_serial
                PUBLIC  recv_pack_serial

                PUBLIC  init_port_serial
                PUBLIC  reset_port_serial

                PUBLIC  send_sync_serial

                SUBTTL  Definitions
                PAGE

TIMER_INTERRUPT equ 08H                 ; Timer interrupt vector

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

INITIAL_CRC     equ     0FFFFH          ; Initial CRC accumulator
MAGIC_CRC       equ     1D0FH           ; Magic CRC number

OVERRUN_ERROR   EQU     00000010B
PARITY_ERROR    EQU     00000100B
FRAMING_ERROR   EQU     00001000B

DATA_PORT       EQU     0               ; Serial port definitions
INT_ENABLE      EQU     1
INT_ID          EQU     2
LINE_CONTROL    EQU     3
MODEM_CONTROL   EQU     4
LINE_STATUS     EQU     5
MODEM_STATUS    EQU     6

PICSTAT1        EQU     20H
PICSTAT2        EQU     21H
IRQ3_BIT        EQU     08H
IRQ4_BIT        EQU     10H
SPECIFIC_EOI    EQU     60H

DATABITS8       EQU     03H
DATABITS7       EQU     02H

STOPBITS1       EQU     00H
STOPBITS2       EQU     04H

NOPARITY        EQU     00H
ODDPARITY       EQU     08H
EVENPARITY      EQU     18H

PCKTS_TO_UPBAUD EQU     64

INT_MASK        EQU     0FEH        ; All but timer disabled

FX_ERR_FAIL         EQU     -3

CORE            SEGMENT WORD PUBLIC 'CODE'
                PUBLIC  last_operation
                EXTRN   serial_ports:BYTE
                EXTRN   num_ser_ports:BYTE
                EXTRN   crctab:WORD
                EXTRN   crc_errors:WORD
                EXTRN   seq_num:BYTE
                EXTRN   is_serial:BYTE
                EXTRN   busy_semaphore:BYTE
                EXTRN   idle_semaphore:BYTE
                EXTRN   win386_enh_mode:BYTE
                EXTRN   win386_std_mode:BYTE
                EXTRN   win_386_api:DWORD
                EXTRN   win_386_api_ok:BYTE
                EXTRN   port_address:WORD
                EXTRN   save_area:BYTE
                EXTRN   timeout:BYTE
                EXTRN   code_save:WORD
                EXTRN   ticks_remaining:WORD
                EXTRN   set_alarm_time:WORD
                EXTRN   alarm_vector:WORD
                EXTRN   client_max_baud:BYTE
                EXTRN   max_baud:BYTE
                EXTRN   max_serial_block:WORD
                EXTRN   start_packet:WORD
                EXTRN   end_packet:WORD
                EXTRN   fx_force_variable:BYTE
        IFDEF DEBUG
                EXTRN   debug_msg:WORD
                EXTRN   hex_out:WORD
        ENDIF

BAUD_1200       EQU     0
BAUD_2400       EQU     1
BAUD_4800       EQU     2
BAUD_9600       EQU     3
BAUD_19200      EQU     4
BAUD_38400      EQU     5
BAUD_57600      EQU     6
BAUD_115200     EQU     7
                
CONNECT_BAUD    EQU     BAUD_9600

baud_table      db      96          ;   1200 baud   (index 0)
                db      48          ;   2400 baud   (index 1)
                db      24          ;   4800 baud   (index 2)
                db      12          ;   9600 baud   (index 3)
                db       6          ;  19200 baud   (index 4)
                db       3          ;  38400 baud   (index 5)
                db       2          ;  57600 baud   (index 6)
                db       1          ; 115200 baud   (index 7)
NUM_BAUD_VALUES EQU     ($ - baud_table)

desired_baud        db  ?

polite_timer        db  ?

bios_count          db  ?               ; Count of Actual # of BIOS COM ports

interrupt_mask      db  ?               ; Save area for previous interrupt mask

hp_95lx         db      0


                EVEN

timer_save      dd      0               ; Old timer vector
lost_ticks      dw      ?               ; Tick count while timer set

SEND_OPERATION      EQU     0
RECEIVE_OPERATION   EQU     1

MAX_SEND_FAILS  EQU     2

last_operation  db      ?
packet_ok       db      ?
fx_errno        dw      0
fx_baud         db      CONNECT_BAUD
prior_baud      db      ?
recv_buff_ptr   dw      ?               ; Save area for recv buffer ptr
idle_recv_wait  db      0
first_send      db      ?
fx_send_echoplex    db      0
fx_recv_echoplex    db      0
need_send_echo  db      0
echo_byte       db      ?
echo_ret        dw      ?
ax_save         dw      ?
send_fails      db      0

baud_list1      db      ?, 0FFH, 000H, 05AH, 055H, 0AAH, 0F0H, 00FH, 0E7H, 07EH, 0C3H, 03CH, 081H, 018H, 000H, 0FFH
BAUD_LIST_SIZE  EQU     $ - baud_list1
baud_list2      db      ?, 000H, 0FFH, 0A5H, 0AAH, 055H, 0F0H, 00FH, 0FFH, 000H, 018H, 081H, 03CH, 0C3H, 07EH, 0E7H

send_remaining  dw      ?           ; Number of bytes remaining in send

recv_sync_ticks dw      ?

recv_byte_error db      ?

CORE                ENDS


SERIAL              SEGMENT WORD PUBLIC 'CODE'
                    PUBLIC  serial_start

                    ORG     0

serial_start        LABEL   BYTE

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
                mov     cs:set_alarm_time, bx       ; Update set_alarm_time
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

                cmp     cs:busy_semaphore, 0
                jne     pass_it

                cmp     cs:port_address, 0
                je      pass_it

                cmp     cs:is_serial, 0
                je      pass_it

                cmp     cs:idle_semaphore, 0
                je      pass_it

                cmp     cs:idle_semaphore, 1
                je      do_idle

                dec     cs:idle_semaphore
                mov     cs:idle_recv_wait, 0
                jmp     short pass_it

do_idle:        push    ax
                call    GetTicks
                cmp     ax, cs:set_alarm_time
                mov     cs:set_alarm_time, ax       ; Update set_alarm_time
                pop     ax
                je      pass_it

                call    idle_handler

pass_it:        jmp     cs:timer_save   ; chain through old timer vector

timer_handler   ENDP

made_connection PROC    NEAR

                push    ax
                mov     al, client_max_baud
                mov     max_baud, al            ; Reload max_baud
                DBG     '{'
                HEX     al
                DBG     '}'
                pop     ax
                ret

made_connection ENDP

lost_connection PROC    NEAR

                mov     port_address, 0     ; Connection lost
                mov     idle_semaphore, 0
                ret

lost_connection ENDP


idle_handler    PROC    NEAR

                push    ax
                push    bx
                push    dx
                push    ds
                push    cs
                pop     ds              ; Load DS with our segment
                mov     last_operation, SEND_OPERATION
                mov     dx, port_address
                mov     al, seq_num
                xor     al, 1               ; Toggle low order sequence bit
                or      al, IDLE_BITS
                mov     bx, ax              ; Save copy of idle byte in BL
                cmp     win_386_api_ok, 0
                jne     send_by_vfxd

                call    SendByte            ; Send idle if status is ready
                call    GetByte
                jmp     short check_response

send_by_vfxd:   mov     ax, SERIAL_IDLER        ; function to send idle
                call    dword ptr win_386_api   ; Note: idle passed in BL

check_response: jc      no_recv

                cmp     al, bl
                jne     no_recv

                mov     idle_recv_wait, 0
                jmp     short idle_ret

no_recv:        inc     idle_recv_wait
                cmp     idle_recv_wait, 2 * MINIMUM_TICKS
                jbe     idle_ret

drop_connect:   call    lost_connection

idle_ret:       pop     ds
                pop     dx
                pop     bx
                pop     ax
                ret

idle_handler    ENDP


;----------------------------------------------------------------------------

modify_code     PROC    NEAR

; Destroys NOTHING

                cmp     cs:timeout, 1
                je      modify_ret

                push    ax
                mov     cs:timeout, 1
                mov     ax, 0EBH OR ((OFFSET send_byte_fail - (OFFSET sbs1 + 2)) SHL 8)
                xchg    ax, WORD PTR cs:sbs1
                mov     cs:code_save[0], ax
                mov     ax, 0EBH OR ((OFFSET recv_byte_fail - (OFFSET rbs1 + 2)) SHL 8)
                xchg    ax, WORD PTR cs:rbs1
                mov     cs:code_save[2], ax
                pop     ax

modify_ret:     ret

modify_code     ENDP


; set_alarm_ser -- Starts the alarm counting down ticks.
;
; Destroys registers: None
;
; set_alarm_ser changes the timer interrupt vector from timer_passthru to
; timer_handler_ser.  Before calling this routine start_transaction must have
; been called.  Also, alarm_vector should be set to the offset of the timeout
; code and ticks_remaining should be set to the number of ticks before a
; timeout trap should occur.  Since the granularity of the alarm is ñ tick,
; ticks_remaining should always be set to 2 or more (if set to 1, a timeout
; could occur immediately after the return from this routine).
;
set_alarm_ser   PROC    NEAR

; Destroys NOTHING

                push    ax
                mov     cs:timeout, 0
                mov     cs:lost_ticks, 0    ; Clear the lost ticks count
    IFDEF   NO_TIMEOUT
                mov     WORD PTR cs:timer_handler, 0EBH OR ((OFFSET timer_passthru - (OFFSET timer_handler + 2)) SHL 8)
    ELSE
                mov     al, win386_enh_mode ; Always polite if 386 Enhanced Mode
                or      al, win386_std_mode ; or if standard mode
                or      al, hp_95lx         ; or if running on HP 95LX
                or      al, fx_force_variable   ; or if forced variable mode
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
    ENDIF

set_polite:     call    GetTicks
                mov     set_alarm_time, ax
                mov     WORD PTR cs:timer_handler, 0EBH OR (16H SHL 8)
.ERRE (OFFSET polite_handler - (OFFSET timer_handler + 2)) EQ 16H

set_alarm_ret:  pop     ax
                ret

set_alarm_ser   ENDP


; clear_alarm_ser -- Reset the timer interrupt handler to timer_passthru
;
; Destroys registers: None
;
clear_alarm_ser PROC    NEAR

                mov     WORD PTR cs:timer_handler, 0EBH OR (3AH SHL 8)
.ERRE (OFFSET timer_passthru - (OFFSET timer_handler + 2)) EQ 3AH
                push    ax              ; Preserve Register
                push    cx

    IFNDEF  NO_TIMEOUT
                cmp     polite_timer, 0
                jne     cascade_done

                mov     al, interrupt_mask
                out     21H, al
    ENDIF
                mov     cx, cs:lost_ticks
                jcxz    cascade_done

                cli

ca_cascade:     pushf                   ; make up lost timer ticks
                call    cs:timer_save
                loop    ca_cascade

                sti
                mov     cs:lost_ticks, 0

cascade_done:   cmp     cs:timeout, 0
                je      no_timeout

                mov     ax, cs:code_save[0]
                mov     WORD PTR cs:sbs1, ax
                mov     ax, cs:code_save[2]
                mov     WORD PTR cs:rbs1, ax
                stc
                jmp     short clear_done

no_timeout:     clc

clear_done:     pop     cx
                pop     ax
                ret

clear_alarm_ser ENDP

; send_byte_ser Send byte on serial port
;
; The timeout routines must already be executing before this routine is called.
;
; Inputs:
;   bl      Byte to be sent
;   dx      Base I/O port
;   cx      return address
;
; Outputs: none
;
;   If control returns inline, the character has been sent.  The only other
;   way out of this routine is through a timeout.
;
; Destroys AX only
;

send_byte_fail  LABEL   NEAR

                pop     ax
                sub     dl, LINE_STATUS ; Point back to data output register
                add     sp, 2           ; Pop off the return address
                jmp     WORD PTR cs:alarm_vector

send_byte_ser   PROC    NEAR

; Inputs:
;   dx      Base I/O port
;
; Destroys:
;   AX only - CRC is updated

                push    ax              ; Save current CRC value

                add     dl, LINE_STATUS

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
                xor     ax, crctab[bx]
                pop     bx
                ret

send_byte_ser   ENDP

send_byte_echo  PROC    NEAR

                cmp     fx_send_echoplex, 0
                je      send_byte_ser

                pop     echo_ret         ; Pop off return address
                mov     echo_byte, bl
                call    send_byte_ser
                mov     ax_save, ax

wait_echo:      call    bp
                cmp     bl, echo_byte
                jne     wait_echo

                mov     ax, ax_save
                jmp     echo_ret

send_byte_echo  ENDP

last_ticks      dw      0
current_ticks   dw      0

GetTicks        PROC    NEAR

;* Enter with:
;* Uses:
;*      AX = logical tick count returned

                pushf
                cli
                push    es
                push    bx
                mov     ax, 040H
                mov     es, ax
                mov     ax, es:[06CH]
                mov     bx, ax
                sub     ax, cs:last_ticks
                mov     cs:last_ticks, bx
                cmp     ax, 2 * 18
                jb      @F

;*** NOTE: Elapsed time greater than or equal to 2 seconds means either:
;***
;***    1)  Clock rolled over at midnight, so we adjust by only 1 tick.
;***    2)  User is starting a new timing sequence, so it doesn't matter.

                mov     ax, 1

@@:             add     cs:current_ticks, ax
                mov     ax, cs:current_ticks
                pop     bx
                pop     es
                popf
                ret

GetTicks        ENDP

check_hp_95lx   PROC    NEAR

                push    bx
                push    cx
                push    dx
                push    di
                push    si
                push    bp
                push    es
                mov     hp_95lx, 0
                xor     bx, bx
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

check_hp_95lx   ENDP


WaitIdle        PROC    NEAR

;* Uses:
;*      AX
;* Returns:
;*      DX    = port address
;*      CARRY = Timed out

                push    cx
                mov     dx, port_address
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
                cmp     ax, 2
                jbe     wait_loop

                stc
                jmp     short wi_ret

wi_done:        clc

wi_ret:         pop     cx
                ret

WaitIdle        ENDP

SetBaud         PROC    NEAR
;* Enter with:
;*      AL = desired baud_table index
;* Uses:
;*      AX
;* Returns:
;*      DX = port base address

                push    cx
                push    si
                push    es
                mov     fx_baud, al
                xor     ah, ah
                mov     si, ax
                call    WaitIdle

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

                pop     es
                pop     si
                pop     cx
                ret

SetBaud         ENDP

; send_sync -- Send synchronization byte
;
;   Inputs: None
;
;   Outputs:
;       CF      Set if synchronization was OK.  Clear otherwise.
;       seq_num Set to zero if Sync was OK.
;
;   Registers used: FLAGS
;
;   The complement of this routine is recv_sync.
;
;   extern int send_sync_ser(void);
;
send_sync_serial        PROC    NEAR

; Destroys AX only

                push    bx
                push    cx
                push    dx
                push    di
                push    si
                push    bp

                mov     si, DVR:ConnectSendAA
                call    GetTicks
                mov     bp, ax              ; Grab current logical tick count
                mov     dx, port_address

sync_loop:      call    si
                jnc     sync_ret            ; Return with NC on success

                call    GetTicks
                sub     ax, bp
                cmp     ax, CONNECT_TICKS
                jb      sync_loop

                stc

sync_ret:       pop     bp
                pop     si
                pop     di
                pop     dx
                pop     cx
                pop     bx
                ret

send_sync_serial        ENDP

GetByte         PROC    NEAR

;* Enter with:
;*      DX = port base address
;* Uses:
;*      AX
;* Performs:    Returns carry set if no byte is available.  If a byte is
;*              read it is returned in AL.

                add     dl, LINE_STATUS
                in      al, dx
                sub     dl, LINE_STATUS
                test    al, 1
                jz      return_error

                in      al, dx
                clc
                ret

return_error:   stc
                ret

GetByte         ENDP

SendByte        PROC    NEAR

;* Enter with:
;*      AL = byte to send
;*      DX = port base address
;* Returns:
;*      JC = byte could not be sent
;*      JNC= byte sent
;* Uses:
;*      ALL REGISTERS PRESERVED
                
                push    ax
                add     dl, LINE_STATUS
                in      al, dx
                sub     dl, LINE_STATUS
                test    al, 20H
                pop     ax
                jz      SendByteFail

                out     dx, al
                clc
                ret

SendByteFail:   stc
                ret

SendByte        ENDP


ConnectSendAA   PROC    NEAR

;* Enter with:
;*      DX = port base address
;*      BX = port vector index
;* Uses:
;*      AX
;* Performs:    Sends AA.  Sets vector to Get00

                mov     al, 0AAH
                call    SendByte
                jc      SendAA_Fail

                DBG     '1'
;--- Reset time here in case Windows did a Device Conflict dialogue

                call    GetTicks
                mov     bp, ax
                mov     si, DVR:ConnectGet00

SendAA_Fail:    stc
                ret

ConnectSendAA   ENDP

ConnectGet00    PROC    NEAR

;* Enter with:
;*      DX = port base address
;*      BX = port vector index
;* Uses:
;*      AX
;* Performs:    Waits to receive 00.  If 00 is received, sets vector to Send55.

                call    GetByte
                jc      Get00_Fail

                DBG     '2'
                DBG     '('
                HEX     al
                DBG     ')'
                cmp     al, 0
                jne     Get00_Fail

                call    GetTicks
                mov     bp, ax                      ; Reset timeout
                mov     si, DVR:ConnectSend55

Get00_Fail:     stc
                ret

ConnectGet00    ENDP

ConnectSend55   PROC    NEAR

;* Enter with:
;*      DX = port base address
;*      BX = port vector index
;* Uses:
;*      AX
;* Performs:    Sends 55.  Sets vector to GetFF

                mov     al, 055H
                call    SendByte
                jc      Send55_Fail

                DBG     '3'
                mov     si, DVR:ConnectGetFF

Send55_Fail:    stc
                ret

ConnectSend55   ENDP


ConnectGetFF    PROC    NEAR

;* Enter with:
;*      DX = port base address
;*      BX = port vector index
;* Uses:
;*      AX
;* Performs:    Waits to receive FF.  If FF is received, sets vector to Send5A

                call    GetByte
                jc      GetFF_Fail

                DBG     '4'
                DBG     '('
                HEX     al
                DBG     ')'
                cmp     al, 0FFH
                jne     GetFF_Fail

                call    GetTicks
                mov     bp, ax                      ; Reset timeout
                mov     si, DVR:ConnectSend5A

GetFF_Fail:     stc
                ret

ConnectGetFF    ENDP

ConnectSend5A   PROC    NEAR

;* Enter with:
;*      DX = port base address
;*      BX = port vector index
;* Uses:
;*      AX
;* Performs:    Sends 5A.  Sets vector to Get11

                mov     al, 05AH
                call    SendByte
                jc      Send5A_Fail

                DBG     '5'
                mov     si, DVR:ConnectGet11

Send5A_Fail:    stc
                ret

ConnectSend5A   ENDP


ConnectGet11    PROC    NEAR

;* Enter with:
;*      DX = port base address
;*      BX = port vector index
;* Uses:
;*      AX
;* Performs:    Waits to receive 11.  If 11 is received, returns success.

                call    GetByte
                jc      Get11_Fail

                DBG     '6'
                DBG     '('
                HEX     al
                DBG     ')'
                cmp     al, 011H
                jne     Get11_Fail

                call    made_connection
                mov     last_operation, SEND_OPERATION
                mov     seq_num, 0      ; reset sequence number
                mov     need_send_echo, 0
                mov     fx_send_echoplex, 0
                mov     fx_recv_echoplex, 0
                mov     first_send, 1
                clc
                ret

Get11_Fail:     stc
                ret

ConnectGet11    ENDP

send_baud_rate  PROC    NEAR
;   Inputs:
;       cl          desired baud rate index
;   Outputs:        JC: Set baud attempt failed, fx_errno = -4
;                   JNC: No loss of connection
;                        NOTE: baud rate may not actually have changed

                push    bx
                push    cx
                push    di
                mov     al, fx_baud
                mov     prior_baud, al
                mov     cs:ticks_remaining, SYNC_TICKS
                mov     cs:alarm_vector, DVR:time_out
                mov     polite_timer, 1
                DBG     '{'
                call    set_alarm_ser

                cmp     last_operation, RECEIVE_OPERATION
                jne     sent_last           ; if the last operation was a receive
                                            ; then we just sent a packet_ok, and
wait_turn:      call    bp                  ; need to receive the channel turn-
                HEX     bl
                DBG     ','
                cmp     bl, seq_num         ; around byte
                jne     wait_turn

                mov     last_operation, SEND_OPERATION

sent_last:      mov     bl, seq_num
                not     bl
                mov     bh, bl              ; Save byte we will expect in BH
                not     bh
                HEX     bl
                call    send_byte_ser       ; send baud rate change request
                DBG     '|'

wait_sync:      call    bp                  ; receive acknowledgement
                HEX     bl
                cmp     bl, bh              ; check for current sequence
                jne     wait_sync

                DBG     '}'
                call    clear_alarm_ser
                mov     cs:alarm_vector, DVR:request_fail
                mov     cs:ticks_remaining, MINIMUM_TICKS
                mov     polite_timer, 1
                call    set_alarm_ser

                mov     bl, cl
                call    send_byte_ser       ; send back desired baud

                mov     al, bl
                call    clear_alarm_ser
                call    SetBaud

                mov     cs:alarm_vector, DVR:change_fail
                mov     cs:ticks_remaining, 2 * MINIMUM_TICKS
                mov     polite_timer, 1
                call    set_alarm_ser

                shl     cl, 1
                shl     cl, 1
                shl     cl, 1
                shl     cl, 1               ; Get baud value to high nibble
                or      cl, seq_num

get_answer:     call    bp                  ; Receive answer
                cmp     bl, cl
                jne     get_answer

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
                jmp     short sbr_ret

request_fail:   call    clear_alarm_ser
                DBG     '@'
                mov     fx_errno, -4
                stc
                jmp     short sbr_ret

change_fail:    call    clear_alarm_ser
                cmp     cl, prior_baud
                jb      request_fail        ; change error on step down is fatal
                
                DBG     '#'
                mov     al, prior_baud
                mov     max_baud, al
                DBG     '{'
                HEX     al
                DBG     '}'
                call    SetBaud
                jmp     return_ok

time_out:       call    clear_alarm_ser
                DBG     '*'
                mov     fx_errno, 4
                stc

sbr_ret:        pop     di
                pop     cx
                pop     bx
                ret

send_baud_rate  ENDP

goto_max_baud   PROC    NEAR
                
                cmp     first_send, 1
                jne     goto_max_ret

                push    cx
                mov     cl, fx_baud

max_loop:       cmp     cl, max_baud
                je      done                ; done if at max already

                jb      advance

                mov     cl, max_baud
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

setup_recv_byte PROC    NEAR

                mov     bp, DVR:recv_byte_ser
                cmp     win386_enh_mode, 0
                je      setup_ret

                mov     bp, DVR:recv_byte_win

setup_ret:      ret

setup_recv_byte ENDP


; send_pack_serial  Send a packet
;   Inputs:
;       cx      Length of buffer to be sent
;       es:si   Pointer to buffer to be sent
;       seq_num Current packet sequence number
;
;   Outputs:
;       CF      Set if packet succesfully sent
;       seq_num Incremented if packet successfully sent
;
;   Registers Destroyed: all
;
; extern int send_pack_ser(void far *buffer, unsigned int count);
;
send_pack_serial        PROC    NEAR

                mov     send_fails, 0
                call    start_packet
                push    bp
                call    setup_recv_byte
                mov     dx, port_address
                or      dx, dx
                jz      sps_err_ret

                mov     send_remaining, cx
                xor     bl, bl              ; Clear all flags
                call    goto_max_baud       ; Go to max baud if first time

next_block:     mov     cx, send_remaining
                jcxz    split               ; Full 64K packet - must split

                cmp     cx, max_serial_block; remainder > max_serial_block?
                jbe     no_split            ; if not don't split

split:          or      bl, CONTINUED_PACKET; indicate packet to follow
                mov     cx, max_serial_block
                jmp     short send_it

no_split:       and     bl, NOT CONTINUED_PACKET

send_it:        and     bl, NOT ECHOPLEX
                or      bl, need_send_echo
                call    SendSerialBlock     ; attempt transmission
                cmp     fx_errno, -1        ; check for failure
                jl      packet_fail

                cmp     fx_errno, 0
                jne     return

                test    bl, CONTINUED_PACKET
                jz      sps_ok

                mov     ax, max_serial_block
                sub     send_remaining, ax
                add     si, ax              ; advance pointer in buffer
                mov     send_fails, 0
                jmp     next_block          ; send the rest

packet_fail:    cmp     fx_send_echoplex, 0
                jne     @F

                mov     send_fails, 0
                mov     need_send_echo, ECHOPLEX
                jmp     next_block          ; Try again in echoplex

@@:             inc     send_fails
                cmp     send_fails, MAX_SEND_FAILS
                jb      next_block

                mov     cl, fx_baud
                cmp     cl, CONNECT_BAUD
                ja      shift_down

                mov     fx_errno, -1
                xor     ax, ax
                jmp     short sps_fail

shift_down:     dec     cl                  ; drop down one baud rate
                DBG     ''
                call    send_baud_rate      ; if not, then step down baud
                jc      return              ; return the error
                
                mov     send_fails, 0
                mov     need_send_echo, 0
                jmp     next_block

return:         cmp     fx_errno, 0
                jne     sps_fail

sps_ok:         clc
                jmp     short sps_ret

sps_fail:       call    lost_connection

sps_err_ret:    stc

sps_ret:        pop     bp
                call    end_packet
                ret
                
send_pack_serial        ENDP

; SendSerialBlock
;   Inputs:
;       bl          Will have status bits for sync exchange set
;       cx          Length of buffer to be sent
;       es:si       Pointer to buffer to be sent
SendSerialBlock      PROC    NEAR
  
                DBG     ''
                or      ch, ch              ; see if length fits in byte
                jnz     word_length         ; assume clear bit if not
                
                or      bl, BYTE_LENGTH     ; set byte length bit
                jmp     short save_flags

word_length:    and     bl, NOT BYTE_LENGTH ; clear byte length bit

save_flags:     mov     bh, bl
                push    bx
                push    cx
                push    si
                mov     cs:ticks_remaining, SYNC_TICKS
                mov     cs:alarm_vector, DVR:sp_total_fail
                mov     polite_timer, 1
                DBG     '['
                call    set_alarm_ser

                cmp     last_operation, RECEIVE_OPERATION
                jne     ssb_sent_last       ; if the last operation was a receive
                                            ; then we just sent a packet_ok, and
ssb_wait_turn:  call    bp                  ; need to receive the channel turn-
                HEX     bl
                DBG     ','
                cmp     bl, seq_num         ; around byte
                jne     ssb_wait_turn

                mov     last_operation, SEND_OPERATION

ssb_sent_last:  call    drain               ; Drain away leftovers
                mov     bl, bh
                or      bl, seq_num         ; Add sequence to status bits
                mov     packet_ok, bl       ; packet_ok is original sync with
                HEX     bl
                call    send_byte_ser
                DBG     '|'

                not     bl                  ; sync_ans is inverse of sync
                mov     bh, bl
                and     bh, NOT ECHOPLEX

ssb_wait_sync:  call    bp
                HEX     bl
                mov     al, bl
                and     bl, NOT ECHOPLEX
                cmp     bl, bh              ; expect back inverse
                jne     ssb_wait_sync

                and     al, ECHOPLEX
                mov     fx_send_echoplex, al
                DBG     ']'
                call    clear_alarm_ser
                mov     cs:alarm_vector, DVR:sp_fail
                mov     cs:ticks_remaining, 2 * MINIMUM_TICKS
                mov     polite_timer, 1     ; Polite timer while sending
                call    set_alarm_ser

                mov     ax, INITIAL_CRC     ; start CRC calculations in AX
                mov     bl, cl              ; send length (LSB)
                call    send_byte_echo

                or      ch, ch              ; see if length was < 256
                jz      send_data_loop

                mov     bl, ch              ; send length (MSB)
                call    send_byte_echo

send_data_loop: mov     cs:ticks_remaining, 2 * MINIMUM_TICKS
                mov     bl, es:[si]
                inc     si
                call    send_byte_echo
                loop    send_data_loop

                mov     bx, ax              ; get CRC
                not     bx
                mov     cs:ticks_remaining, 2 * MINIMUM_TICKS
                xchg    bl, bh              ; send high byte
                call    send_byte_echo
                mov     bl, bh              ; send low byte
                call    send_byte_ser

get_reply:      mov     cs:ticks_remaining, 2 * MINIMUM_TICKS
                call    bp
                cmp     bl, packet_ok
                jne     sp_bad_reply

                call    clear_alarm_ser
                inc     seq_num
                and     seq_num, 3
                mov     ax, 1
                jmp     short sp_exit

sp_total_fail:  call    clear_alarm_ser
                DBG     '*'
                mov     ax, -1
                jmp     short sp_err_exit

sp_bad_reply:   DBG     '$'

sp_fail:        call    clear_alarm_ser
                DBG     '#'
                mov     ax, FX_ERR_FAIL

sp_err_exit:    mov     fx_errno, ax
                xor     ax, ax
                jmp     short sp_ret

sp_exit:        mov     fx_errno, 0

sp_ret:         pop     si
                pop     cx
                pop     bx
                ret

SendSerialBlock      ENDP


; recv_byte_ser Get a byte from serial port
;
; The timeout routines must already be executing before this routine is called.
;
;
; Inputs:
;   dx                  Base I/O port
;   cx                  return address
;
; Outputs:
;   ah      Line status
;   al      Read character (May be garbled if an error occured)
;
;   If control returns in line, the character was successfully read
;
; Registers used: ax
;

recv_byte_fail  LABEL   NEAR

                sub     dl, LINE_STATUS ; Point DX back where it started
                add     sp, 2           ; Pop off return address from stack
                jmp     cs:alarm_vector

recv_byte_ser   PROC    NEAR

                mov     bl, al          ; Save AL in BL

next_byte:      add     dl, LINE_STATUS 

@@:             in      al, dx          ; Wait for transmitter holding to empty
                shr     al, 1           ; Data Ready?
rbs1::          jnc     @b

                sub     dl, LINE_STATUS
                test    al, ((FRAMING_ERROR OR PARITY_ERROR OR OVERRUN_ERROR) SHR 1)
                jnz     record_error    ; Record error

rb_grab::       in      al, dx          ; grab the character

recv_got_byte:  xchg    bl, al          ; return byte recvd in bl, restore CRC
                push    bx
                xchg    ah, al
                xor     al, bl
                xor     bx, bx
                xchg    bl, al
                shl     bx, 1
                xor     ax, crctab[bx]
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
                sub     ax, set_alarm_time
                cmp     ax, ticks_remaining
                pop     ax
                jl      recv_look

                jmp     recv_byte_fail

recv_byte_win   ENDP

recv_byte_echo  PROC    NEAR

                pop     echo_ret
                call    bp
                push    ax
                mov     al, bl
                call    SendByte
                pop     ax
                jmp     echo_ret

recv_byte_echo  ENDP

recv_baud_rate  PROC    NEAR
            
                push    bx
                push    cx
                push    di
                push    si
                push    bp
                call    drain
                mov     cs:alarm_vector, DVR:rbr_req_fail
                mov     cs:ticks_remaining, MINIMUM_TICKS
                mov     polite_timer, 1
                call    set_alarm_ser

                mov     bl, bh
                not     bl
                HEX     bl
                DBG     ']'
                call    send_byte_ser       ; send inverse of received byte
                DBG     '<'
                call    bp                  ; expect back desired baud
                HEX     bl
                call    clear_alarm_ser
                cmp     bl, fx_baud
                jb      recv_change         ; Going down - don't look at max

                cmp     bl, max_baud        ; Don't go above max baud
                ja      rbr_ok

recv_change:    DBG     '>'
                mov     al, bl
                mov     bl, fx_baud
                mov     prior_baud, bl
                call    SetBaud

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
                mov     si, ax

send_loop1:     mov     bp, DVR:send_loop1
                mov     al, bh
                call    SendByte
                jc      rbr_check_time

                call    GetTicks
                mov     si, ax
                mov     bp, DVR:get_byte_loop

get_byte_loop:  call    GetByte
                jnc     got_byte

                call    GetTicks
                push    ax
                sub     ax, si
                pop     si
                jnz     send_loop1

                jmp     short rbr_check_time

got_byte:       cmp     al, bh
                jne     rbr_check_time

send_loop:      mov     bp, DVR:send_loop
                mov     al, baud_list2[di]
                call    SendByte
                jc      rbr_check_time

                mov     bp, DVR:recv_loop
                inc     di
                cmp     di, BAUD_LIST_SIZE
                jae     rbr_ok

recv_loop:      call    GetByte
                jc      rbr_check_time

                cmp     al, baud_list1[di]
                je      send_loop

rbr_check_time: call    GetTicks
                sub     ax, cx
                cmp     ax, MINIMUM_TICKS
                ja      rbr_change_fail

                jmp     bp

rbr_req_fail:   call    clear_alarm_ser

rbr_ok:         clc                         ; assume false request, stay in sync
                jmp     short rbr_ret

rbr_change_fail:mov     al, prior_baud
                DBG     '#'
                cmp     al, fx_baud
                jae     rbr_fatal_fail      ; change error on step down is fatal

                mov     max_baud, al
                DBG     '{'
                HEX     al
                DBG     '}'
                call    SetBaud
                clc
                jmp     short rbr_ret

rbr_fatal_fail: mov     ax, 4
                DBG     '~'
                stc

rbr_ret:        pop     bp
                pop     si
                pop     di
                pop     cx
                pop     bx
                ret

recv_baud_rate  ENDP

; recv_pack_serial    Recieve a packet
;   Inputs:
;       es:di       pointer to buffer
;       cx          max size of buffer
;
;   Outputs:
;       CF          Set if packet recieved
;                   Clear if timeout or some other error
;       cx          If no error, number of bytes recieved.  Otherwise, unknown.
;
;   Uses registers: ax bx cx dx si di bp
;
; extern unsigned int recv_pack_serial(void far *buffer, unsigned int count);
;

recv_pack_serial    PROC    NEAR
;   Inputs:
;       cx          Length of buffer to be received
;    es:di          Pointer to buffer to be received

                call    start_packet
                call    check_hp_95lx
                push    bp
                call    setup_recv_byte
                mov     ax, RECV_PACK_TICKS
                mov     recv_sync_ticks, ax
                cmp     port_address, 0
                je      rps_fail

                mov     bx, di
                add     bx, cx
                jc      rps_fail            ; Buff ptr + max count = seg wrap

                xor     bx, bx              ; count received

recv_next_block:
                push    bx                  ; preserve current count
                call    RecvSerialBlock
                mov     recv_sync_ticks, SYNC_TICKS
                jc      recv_error

                test    bl, CONTINUED_PACKET; see if split block received
                jz      rps_return          ; if not we're done

                pop     bx                  ; get prior length
                add     bx, ax              ; add received length
                sub     cx, ax              ; subtract received length
                add     di, ax              ; move up buffer pointer
                jmp     recv_next_block

recv_error:     cmp     fx_errno, -1        ; no retry if we didn't sync
                je      rps_error

                cmp     fx_errno, 0
                jg      rps_error

                pop     bx
                jmp     recv_next_block     ; try again

rps_return:     pop     bx                  ; get prior length
                add     bx, ax              ; add received length
                mov     ax, bx
                clc                         ; Carry clear on success
                jmp     short rps_ret

rps_error:      pop     bx
                call    lost_connection

rps_fail:       xor     ax, ax              ; return 0 for error
                stc                         ; and set carry flag

rps_ret:        pop     bp
                call    end_packet
                ret

recv_pack_serial        ENDP

drain           PROC    NEAR

@@:             add     dl, LINE_STATUS
                in      al, dx
                sub     dl, LINE_STATUS
                test    al, 1
                jz      empty

                in      al, dx
                jmp     @b

empty:          ret

drain           ENDP

IFDEF DEBUG
bell            PROC    NEAR

                mov     ah, 14
                mov     al, 7
		        INT	    10H
                ret

bell            ENDP

display_block   PROC    NEAR

                DBG     '<'
                DBG     '<'
                push    si
                mov     si, recv_buff_ptr

disp_loop:      cmp     si, di
                jae     disp_done

                cmp     si, recv_buff_ptr
                je      @F

                DBG     ','

@@:             mov     al, es:[si]
                HEX     al
                inc     si
                jmp     disp_loop

disp_done:      pop     si
                DBG     '>'
                DBG     '>'
                ret

display_block   ENDP

ENDIF

; RecvSerialBlock       Recieve a packet
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
RecvSerialBlock      PROC    NEAR

                DBG     ''
                push    cx
                push    di
                push    si
                mov     recv_buff_ptr, di
                mov     fx_errno, 0
                mov     dx, port_address
                DBG     '['
                cmp     last_operation, SEND_OPERATION  ; if the last operation was a 
                jne     rsb_sync_retry          ; send, then this side just
                                                ; received a packet_ok, and 
                mov     al, seq_num             ; needs to send a byte to turn
                HEX     al
                DBG     ','
                call    SendByte                ; around the channel
                mov     last_operation, RECEIVE_OPERATION

rsb_sync_retry: mov     ax, recv_sync_ticks
                mov     cs:ticks_remaining, ax
                mov     dx, port_address
                mov     cs:alarm_vector, DVR:rp_total_fail
                mov     polite_timer, 1
                call    set_alarm_ser

rsb_wait_sync:  call    bp
                HEX     bl
                DBG     '|'
                mov     bh, bl              ; save copy in high byte
                and     bl, SEQUENCE_BITS   ; check for sequence
                cmp     bl, seq_num
                je      short send_reply

                not     bl                  ; check for inverse of sequence
                and     bl, SEQUENCE_BITS
                cmp     bl, seq_num
                je      @F

                jmp     rsb_wait_sync

@@:             call    clear_alarm_ser
                call    recv_baud_rate
                jnc     rsb_sync_retry

                DBG     '@'
                jmp     rp_err_ret

send_reply:     call    clear_alarm_ser
                call    drain
                mov     packet_ok, bh
                mov     ah, bh
                and     ah, ECHOPLEX
                cmp     fx_force_variable, 0
                jne     force_echoplex

                cmp     win386_enh_mode, 0
                jne     force_echoplex  ; Force echoplex if 386 Enhanced Mode

                cmp     hp_95lx, 0      ; Force echoplex if on HP 95LX
                je      @F

force_echoplex: or      ah, ECHOPLEX

@@:             mov     fx_recv_echoplex, ah
                mov     bl, bh
                xor     bh, bh          ; Zero out BH for later
                not     bl              ; send inverse
                and     bl, NOT ECHOPLEX
                or      bl, ah          ; BL has reply byte to send back
                HEX     bl
                mov     cs:alarm_vector, DVR:rp_fail
                mov     cs:ticks_remaining, MINIMUM_TICKS
                mov     al, fx_recv_echoplex
                mov     polite_timer, al    ; Use polite timer if echoplex
                call    set_alarm_ser
                mov     recv_byte_error, 0
                test    fx_recv_echoplex, ECHOPLEX
                jnz     echo_recv

                call    send_byte_ser   ; Send reply
                DBG     ']'
                mov     ax, INITIAL_CRC ; start CRC calculation
                call    bp              ; Length (LSB)
                test    packet_ok, BYTE_LENGTH
                jnz     skip_len_byte

                mov     bh, bl          ; save LSB in bh
                call    bp              ; Length (MSB)
                xchg    bh, bl          ; flip bytes to correct order

skip_len_byte:  dec     bx
                dec     cx
                cmp     bx, cx          ; See if packet is to big for buffer
                ja      len_too_big

                inc     bx
                mov     cx, bx          ; Count to CX

read_data_loop: call    bp
                mov     cs:ticks_remaining, MINIMUM_TICKS
                mov     es:[di], bl
                inc     di
                loop    read_data_loop

                call    bp
                call    bp
                jmp     recv_common

;--- Echoed version

echo_recv:      call    send_byte_ser   ; Send reply
                DBG     ']'
                mov     ax, INITIAL_CRC ; start CRC calculation
                call    recv_byte_echo  ; Length (LSB)
                test    packet_ok, BYTE_LENGTH
                jnz     skip_len2

                mov     bh, bl          ; save LSB in bh
                call    recv_byte_echo              ; Length (MSB)
                xchg    bh, bl          ; flip bytes to correct order

skip_len2:      dec     bx
                dec     cx
                cmp     bx, cx          ; See if packet is to big for buffer
                ja      len_too_big

                inc     bx
                mov     cx, bx          ; Count to CX

read_data2:     call    recv_byte_echo
                mov     cs:ticks_remaining, MINIMUM_TICKS
                mov     es:[di], bl
                inc     di
                loop    read_data2

                call    recv_byte_echo
                call    bp

recv_common:    cmp     recv_byte_error, 0
                jne     rb_error

                cmp     ax, MAGIC_CRC   ; Is it a match?
                jne     bad_crc

                ; Send ACK and seq_num back to sender

send_ok:        mov     bl, packet_ok
                mov     cs:ticks_remaining, MINIMUM_TICKS
                call    send_byte_ser

                call    clear_alarm_ser
                inc     seq_num
                and     seq_num, 3
                mov     ax, di
                sub     ax, recv_buff_ptr
                clc
                jmp     rp_ret

rb_error:       DBG     '%'
                DBG     '%'
IFDEF DEBUG
                test    recv_byte_error, (FRAMING_ERROR SHR 1)
                jz      @F

                DBG     'f'

@@:             test    recv_byte_error, (PARITY_ERROR SHR 1)
                jz      @F

                DBG     'p'

@@:             test    recv_byte_error, (OVERRUN_ERROR SHR 1)
                jz      @F

                DBG     'o'
@@:
ENDIF
                jmp     short dump_block

bad_crc:        DBG     '~'
                DBG     '~'
                jmp     short dump_block

;--- Bad length - we dropped something already, so drain the rest.

len_too_big:    DBG     'g'
                DBG     '('
                HEX     bh
                HEX     bl
                DBG     ','
                HEX     ch
                HEX     cl
                DBG     ')'
IFDEF DEBUG
                cmp     recv_byte_error, 0
                jne     rb_error
ENDIF
                jmp     short dump_block

len_zero:       DBG     'z'

dump_block:     mov     cs:ticks_remaining, MINIMUM_TICKS
                call    bp
                jmp     dump_block

rp_total_fail:  call    clear_alarm_ser
                DBG     '*'
                mov     ax, -1
                jmp     short rp_err_ret                

rp_fail:        call    clear_alarm_ser ; Clear alarm clock
                DBG     '#'
                mov     ax, FX_ERR_FAIL

rp_err_ret:     mov     fx_errno, ax
                xor     ax, ax
                stc

rp_ret:         mov     bl, packet_ok       ; return split flag for RecvSerial
                pop     si
                pop     di
                pop     cx
                ret

RecvSerialBlock      ENDP

init_polling    PROC    NEAR

; Destroys AX only
     
                push    dx
                mov     dx, port_address
                inc     dx              ; dx -> Interrupt Enable
                xor     al, al
                out     dx, al          ; Turn off all interrupts
                recover

                add     dl, MODEM_CONTROL - INT_ENABLE  ; dx -> Modem Control
                xor     al, al          ; Disable OUT2 & insure not in loopback
                out     dx, al
                recover

                mov     al, fx_baud
                call    SetBaud
                pop     dx
                ret

init_polling    ENDP


; init_port_serial  Initialize I/O port
;
;   Sets up all initial values needed for communications on the port, caputures
;   any needed interrupt vectors and saves necessary values for a subsequent
;   reset_port_serial call.
;
;   Inputs:
;       AL      Bios number for port
;
;
;   Outputs: CF     Set if port successfully initialized, clear otherwise
;
;   Registers used: All
;
;   extern int init_port_ser(unsigned int port_num, unsigned char far *save_area);
;
;
init_port_serial        PROC    NEAR

                mov     fx_baud, CONNECT_BAUD
                mov     dx, port_address
                push    ds
                pop     es
                mov     di, DVR:save_area
                call    WaitIdle        ; Returns with DX = port_address

                inc     dx              ; dx -> Interrupt Enable
                in      al, dx
                recover
                stosb                   ; save_area[0] = Interrupt Enable

                add     dl, MODEM_CONTROL - INT_ENABLE  ; dx -> Modem Control
                in      al, dx
                recover
                stosb                   ; save_area[1] = Modem Control

                dec     dx              ; dx -> Line Control
                in      al, dx
                recover
                stosb                   ; save_area[2] = Line Control

                or      al, 80H
                out     dx, al          ; Set DLAB = 1
                recover

                sub     dl, LINE_CONTROL - INT_ENABLE   ; dx -> Baud Rate (MSB)
                in      al, dx
                recover
                stosb                   ; save_area[3] = Baud Rate (MSB)

                dec     dx              ; dx -> Baud Rate (LSB)
                in      al, dx
                recover
                stosb                   ; save_area[4] = Baud Rate (LSB)

                add     dl, LINE_CONTROL ; Line Control register
                mov     al, NOPARITY OR STOPBITS1 OR DATABITS8
                out     dx, al          ; Set DLAB = 0
                call    init_polling
                ret

init_port_serial        ENDP


; reset_port_serial  Reset the I/O port
;
;   Restores the port registers to there original values, restores any interrupt
;   vectors, and performs any other cleanup needed to release the current
;   communication port.  Essentially this routine undoes what init_port does.
;
;   Inputs:  None
;
;   Outputs: None
;
;   Destroys registers: All   
;
;   NOTE: in the slave version for most of this routine, DS does not point to
;   the data segment!  So if any variables need to be referenced, ds will need
;   to be restored before doing so!
;
;   extern void reset_port_ser(unsigned int base_port, unsigned char far *save_area);
;
reset_port_serial       PROC    NEAR

                push    ax
                mov     dx, port_address
                mov     si, DVR:save_area
                inc     dx              ; dx -> Interrupt Enable
                lodsb
                out     dx, al
                recover

                add     dl, MODEM_CONTROL - INT_ENABLE  ; dx -> Modem Control
                lodsb
                out     dx, al
                recover

                dec     dx              ; dx -> Line Control
                lodsb
                or      al, 80H         ; Set DLAB
                out     dx, al
                recover

                sub     dl, LINE_CONTROL - INT_ENABLE   ; dx -> Baud Rate (MSB)
                lodsb
                out     dx, al                  ;Update high byte of divisor
                recover

                dec     dx              ; dx -> Baud Rate (LSB)
                lodsb
                out     dx, al
                recover

                add     dl, LINE_CONTROL    ; dx -> Line Control
                mov     al, [si - 3]    ; Get back original line control
                out     dx, al
                recover

                sub     dl, LINE_CONTROL    ; dx -> Receive data/Base register
                in      al, dx          ; Read to prime interrupt pump
                pop     ax
                ret

reset_port_serial       ENDP


SERIAL          ENDS

INIT            SEGMENT WORD PUBLIC 'CODE'

                PUBLIC  setup_ports_serial
                PUBLIC  reset_ports_serial

setup_ports_serial      PROC    NEAR

                cmp     num_ser_ports, 0
                je      setup_done

                mov     ax, 3500H + TIMER_INTERRUPT
                int     21H             ; Current timer vector -> timer_save
                mov     WORD PTR timer_save, bx
                mov     WORD PTR timer_save + 2, es
                mov     WORD PTR timer_handler, 0EBH OR (3AH SHL 8)
.ERRE (OFFSET timer_passthru - (OFFSET timer_handler + 2)) EQ 3AH
                mov     dx, DVR:timer_handler
                mov     ax, 2500H + TIMER_INTERRUPT
                int     21H             ; Set timer vector to timer handler

setup_done:     ret

setup_ports_serial      ENDP


reset_ports_serial      PROC    NEAR

                cmp     num_ser_ports, 0
                je      reset_done

                push    ds
                mov     ax, 2500H + TIMER_INTERRUPT
                lds     dx, timer_save
                int     21H
                pop     ds

reset_done:     ret

reset_ports_serial      ENDP

INIT            ENDS

                END
