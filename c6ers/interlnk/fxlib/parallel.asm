INCLUDE         model.inc
INCLUDE         fastlynx.inc

;### NOIDLE     EQU     1
;### DEBUG      EQU     1
;### NO_TIMEOUT EQU     1

                PUBLIC  SendParallel
                PUBLIC  RecvParallel
                PUBLIC  parallel_port

                EXTRN   __crctab:WORD
                EXTRN   __crc_errors:WORD


                SUBTTL  Definitions
                PAGE

TIMER_INTERRUPT equ 1CH                 ; User timer interrupt vector

; Read and Write ACK/NAK values:
; The read values are the same as the write values execpt that they are shifted
; three to the left and the most significant bit is complemented.

WACK            equ     00000110B       ; WACK = 06H    NOT WACK = F9H
WNAK            equ     00000011B       ; WNAK = 03H    NOT WNAK = FCH
WIDLE           equ     00001010B

RACK            equ     ((WACK SHL 3) AND 7FH) OR ((NOT (WACK SHL 3)) AND 80H)
                                        ; RACK = B0H    NOT RACK = 4FH
RNAK            equ     ((WNAK SHL 3) AND 7FH) OR ((NOT (WNAK SHL 3)) AND 80H)
                                        ; RNAK = 98H    NOT RNAK = 67H

RIDLE           equ     ((WIDLE SHL 3) AND 7FH) OR ((NOT (WIDLE SHL 3)) AND 80H)

WSYNC           equ     00010101B       ; WSYNC = 15H   NOT WSYNC = EAH
RSYNC           equ     ((WSYNC SHL 3) AND 7FH) OR ((NOT (WSYNC SHL 3)) AND 80H)
                                        ; RSYNC = A8H   NOT RSYNC = 57H

INITIAL_CRC     equ     0FFFFH          ; Initial CRC accumulator
MAGIC_CRC       equ     1D0FH           ; Magic CRC number

DIRECTION_BIT   EQU     00100000B


                SUBTTL  protocol Specific Data
                PAGE

                .DATA
                EXTRN   SyncTimeout:WORD
                EXTRN   last_operation:BYTE

parallel_port       dw  ?               ; I/O port base address
save_length         dw  ?               ; Save area for received byte count
recv_ptr            dw  ?               ; Save area for buffer pointer
send_ptr            dw  ?               ; Save area for send buffer pointer
send_count          dw  ?               ; Save area for send count
ticks_remaining     dw  ?               ; Number of ticks until timeout occurs
last_nib_sent       db  ?
last_nib_received   db  ?



        		.CODE	text
                EXTRN   SyncStart:NEAR
                EXTRN   SyncDone:NEAR
                EXTRN   CheckAbort:NEAR
                EXTRN   GetTicks:NEAR
                EXTRN   NearShowBaud:NEAR
                
; These variables must be addressable relative to the CS register.

                EVEN

data_segment    dw      DGROUP
timer_save      dd      0               ; Old timer vector
alarm_vector    dw      ?

;******************************************************************************
;* IMPORTANT NOTE:  There must not be more than MAX_FIXUPS entries in any of
;* the lists below (not counting the initial fail address and trailing null).
;*
;* Also, each jump must be within short jump distance (128 bytes) of the fail
;* address, or of the the next jump that follows it.
;******************************************************************************

MAX_FIXUPS      EQU     5

send_word_list  dw      OFFSET sw_timeout, OFFSET swp1, OFFSET swp2, 0
send_pack_list2 dw      OFFSET sp_timeout, OFFSET spp2, OFFSET spp3, OFFSET spp4, 0
sp_normal_list  dw      OFFSET spn_timeout, OFFSET spn2, OFFSET spn3, OFFSET spn4, 0
recv_word_list  dw      OFFSET rw_timeout, OFFSET rwp1, OFFSET rwp2, 0
recv_pack_list  dw      OFFSET rp_timeout, OFFSET rpp1, OFFSET rpp2, OFFSET rpp3, 0
rp_normal_list  dw      OFFSET rpn_timeout, OFFSET rpn1, OFFSET rpn2, OFFSET rpn3, 0
wait_send_list  dw      OFFSET wsa_timeout, OFFSET wsa1, 0

code_list_ptr   dw      ?
code_save       dw      MAX_FIXUPS dup (?)

timer_jmp       dw      ?

timeout         db      ?
set_alarm_time  dw      ?

DBG     MACRO   char
        ENDM

HEX     MACRO   char
        ENDM


delay           PROC    NEAR

                push    ax
                push    bx
                push    cx
                mov     bx, ax

                call    GetTicks
                mov     cx, ax

delay_loop:     call    GetTicks
                sub     ax, cx
                cmp     ax, bx
                jb      delay_loop

                pop     cx
                pop     bx
                pop     ax
                ret

delay           ENDP


readStatus      PROC    NEAR

;* Performs:    Reads a byte from the status port five times, and determines
;*              if the defined bits are equal all five times.
;* Enter with:
;*      DX  = port base address
;* Returns:
;*      C   - Bytes were NOT equal
;*      NC  - Bytes were all equal, in which case sign bit is set as follows:
;*            S  : High bit of AL is set
;*            NS : High bit of AL is not set
;* Uses:
;*      AX

                inc     dx
                in      al, dx
                mov     ah, al
                and     ah, 0F8H

        REPT    4
                in      al, dx
                and     al, 0F8H
                cmp     al, ah
                jne     read_fail
        ENDM

                dec     dx              ; Restore DX
                or      al, al
                clc
                ret

read_fail:      dec     dx              ; Restore DX
                stc

read_ret:       ret

readStatus      ENDP


SaveParallel      PROC    NEAR

;* Enter with:
;*      DX    = port address
;*      ES:DI = ptr to 2 byte save area, as follows:
;*              data    register
;*              control register

                push    bx
                in      al, dx          ; Grab data register
                stosb
                inc     dx
                inc     dx
                in      al, dx          ; Grab control register (BASE + 2)

                mov     ah, al          ; AH = original port value
                xor     al, al
                out     dx, al          ; Write a 00 to control port
                recover
                recover
                in      al, dx
                mov     bl, al          ; Bits read to BL
                mov     al, 0FH
                out     dx, al          ; Write a 0F to control port
                recover
                recover
                in      al, dx
                xor     bl, al          ; BL = bits that changed
                or      bl, 0F0H
                and     bl, NOT DIRECTION_BIT
                mov     bh, 0CH

; BL = change bits.  If bit = 1, copy value from original port value in AH.
; If bit = 0, copy value from standard value in BH.

bit_loop:       shr     bl, 1
                jc      copy_old

                shr     ah, 1
                shr     bh, 1
                jmp     short copy

copy_old:       shr     bh, 1
                shr     ah, 1

copy:           rcr     al, 1
                or      bl, bl
                jnz     bit_loop

                stosb
                pop     bx
                ret

SaveParallel    ENDP

RestoreParallel PROC    NEAR

;* Enter with:
;*      DX    = port address
;*      DS:SI = ptr to 2 byte save area, as follows:
;*              data    register
;*              control register

                lodsb
                out     dx, al          ; Restore data register
                lodsb
                inc     dx
                inc     dx
                out     dx, al          ; Restore control register
                ret

RestoreParallel ENDP

; Listen should send a synchronization signal, and then wait for an answer.
; Upon failure to receive an answer, it should back up to sending the prior
; signal.

ParallelListenInit      PROC    NEAR

;* Enter with:
;*      BX    = vector index

                mov     fx_listen_vector[bx], OFFSET ListenSendWSYNC_2
                stc
                ret

ParallelListenInit      ENDP

ListenSendWSYNC_2       PROC    NEAR

;* Enter with:
;*      DX    = port base address
;*      BX    = port vector index
;* Uses:
;*      AX

                mov     al, NOT WSYNC
                out     dx, al
                mov     fx_listen_vector[bx], OFFSET ListenGetRSYNC
                stc
                ret

ListenSendWSYNC_2       ENDP

ListenGetRSYNC          PROC    NEAR

;* Enter with:
;*      DX    = port base address
;*      BX    = port vector index
;* Uses:
;*      AX

                call    readStatus
                jc      return

                cmp     al, RSYNC
                jne     return

                mov     al, WSYNC
                out     dx, al
                mov     fx_listen_vector[bx], OFFSET ListenGetRSYNC_2
return:         stc
                ret

ListenGetRSYNC          ENDP


ListenGetRSYNC_2        PROC    NEAR

;* Enter with:
;*      DX    = port base address
;*      BX    = port vector index
;* Uses:
;*      AX
;* Performs:    Tries to receive an RSYNC_2, upon failure will set the state
;*              back to waiting for RSYNC.

                call    readStatus
                jc      return

                cmp     al, (NOT RSYNC) AND 0F8H
                jne     test_2

                mov     al, WNAK
                out     dx, al
                mov     fx_listen_vector[bx], OFFSET ListenSendWSYNC_2
                mov     fx_parallel_speed, PARALLEL_TURBO
                clc
                ret

test_2:         cmp     al, (NOT RNAK) AND 0F8H
                jne     return

                mov     fx_listen_vector[bx], OFFSET ListenSendWSYNC_2
return:         stc
                ret

ListenGetRSYNC_2        ENDP


driver_timer_handler    LABEL   FAR

                jmp     WORD PTR cs:timer_jmp

; timer_handler_par -- Interrupt handler for timing while alarm is running
;
;   timer_handler_par is the interrupt handler which counts down the ticks in a
;   running alarm.  When the remaining ticks reaches zero, the interrupt return
;   will be modified to return to the timeout address.
;
;   In all cases, this routine appropriately chains to the previous interrupt
;   handler.
;
timer_handler_par   PROC    FAR

                push    ax
                push    bx
                push    ds
                mov     ds, cs:data_segment
                call    GetTicks
                mov     bx, ax
                sub     ax, cs:set_alarm_time
                mov     cs:set_alarm_time, bx       ; Update set_alarm_time
                sub     ticks_remaining, ax
                pop     ds
                pop     bx
                pop     ax
                jc      timed_out

                jnz     passthru        ; Jump if any time left

timed_out:      push    ax              ; Preserve used registers
                push    bx
                push    di
                push    si

                mov     cs:timeout, 1   ; Flag occurrence of timeout
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
                mov     cs:code_save[bx], ax
                add     bx, 2
                jmp     save_code_loop

fixup_return:   pop     si
                pop     di
                pop     bx
                pop     ax

timer_passthru_par  LABEL   FAR         ; Interrupt handler while alarm not on!


passthru:       jmp     cs:timer_save   ; chain through old timer vector

timer_handler_par   ENDP




; set_alarm_par -- Starts the alarm counting down ticks.
;
; Destroys registers: None
;
; set_alarm_par changes the timer interrupt vector from timer_passthru_par to
; timer_handler_par.  Before calling this routine ticks_remaining should be set
; to the number of ticks before a timeout trap should occur and code_list_ptr
; should be set to point to the list of code locations to change to NOPs.
; Since the granularity of the alarm is ñ tick,
; ticks_remaining should always be set to 2 or more (if set to 1, a timeout
; could occur immediately after the return from this routine).
;
set_alarm_par   PROC    NEAR

                mov     cs:timeout, 0
                push    ax
                call    GetTicks
                mov     cs:set_alarm_time, ax
                pop     ax
                mov     WORD PTR cs:timer_jmp, OFFSET timer_handler_par
                ret

set_alarm_par   ENDP


; clear_alarm_par -- Reset the timer interrupt handler to timer_passthru_par
;
; Destroys registers: None
;
; Outputs:
;   CF set if OK.  Clear if timeout occurred.
;
clear_alarm_par PROC    NEAR

                mov     WORD PTR cs:timer_jmp, OFFSET timer_passthru_par
                cmp     cs:timeout, 0
                je      no_timeout

                push    ax
                push    bx
                push    di
                push    si
                mov     di, cs:code_list_ptr
                xor     bx, bx

restore_code:   mov     si, cs:[di + bx + 2]
                or      si, si
                jz      restore_done

                mov     ax, cs:code_save[bx]
                mov     cs:[si], ax
                add     bx, 2
                jmp     restore_code

restore_done:   pop     si
                pop     di
                pop     bx
                pop     ax
                stc                     ; Set carry flag for timeout failure
                ret

no_timeout:     clc                     ; No carry if OK
                ret

clear_alarm_par ENDP


update_crc      PROC    NEAR

; Update CRC/checksum value in CX based on byte value in BL
; Destroys no registers


                push    bx
                xchg    ch, cl
                xor     cl, bl
                xor     bx, bx
                xchg    bl, cl
                shl     bx, 1
                xor     cx, __crctab[bx]
                pop     bx
                ret


update_crc      ENDP

; send_byte
;   The complement of this routine is recv_byte (and recv_byte_noack).
;
;   Inputs:
;       AL          Ticks to wait
;       BL          Byte to be sent
;       DX          Port base address
;
;   Outputs:
;       NC          Carry clear if byte successfully sent
;       C           Carry set if error in send
;       AL          If no error, final acknowledgment value
;                   Otherwise, unknown
;       DX          If no error occured, port base address--otherwise unknown
;
;   Registers destroyed: AX, BL
;
send_byte       PROC    NEAR

                xor     ah, ah
                mov     ticks_remaining, ax
                call    update_crc
                mov     cs:code_list_ptr, OFFSET send_word_list
                call    set_alarm_par   ; And we're off . . .
                mov     al, bl          ; Send low byte first
                or      al, 10H         ; Set transition bit
                out     dx, al          ; Send lower nibble
                shr     bl, 1           ; Prepare high nibble
                shr     bl, 1
                shr     bl, 1
                shr     bl, 1

@@:             call    readStatus      ; Wait for transition to occur
swp1::          jc      @B
                js      @B

                mov     al, bl          ; Get ready to send high nibble
                out     dx, al          ; Output high nibble

@@:             call    readStatus
swp2::          jc      @B
                jns     @B

sw_timeout::    call    clear_alarm_par

                ret

send_byte       ENDP

make_seq_flags  PROC    NEAR

                mov     bl, seq_num
                ret

make_seq_flags  ENDP


send_byte_sync  PROC    NEAR USES SI

                call    update_crc
                call    SyncStart
                mov     si, ax              ; Save starting time in SI
                mov     al, bl
                or      al, 10H
                out     dx, al
                shr     bl, 1
                shr     bl, 1
                shr     bl, 1
                shr     bl, 1
                mov     bp, OFFSET sbs_wait1

sbs_wait1:      call    readStatus
                jc      sbs_check
                js      sbs_check

                mov     al, bl          ; Get ready to send high nibble
                out     dx, al          ; Output high nibble
                mov     bp, OFFSET sbs_wait2

sbs_wait2:      call    readStatus
                jc      sbs_check
                jns     sbs_check

                clc
                jmp     short sbs_ret

sbs_check:      call    GetTicks
                sub     ax, si
                cmp     ax, SyncTimeout
                ja      sbs_fail

                call    CheckAbort
                jc      sbs_fail

                jmp     bp

sbs_fail:       stc

sbs_ret:        call    SyncDone
                ret

send_byte_sync  ENDP

; SendParallel
;   Inputs:
;       cx          Length of buffer to be sent
;       es:si       Pointer to buffer to be sent
;
;   Outputs:
;       CF          Set if packet successfully sent
;
;   Registers Destroyed: AX, BX, CX, DX, SI, DI, BP
;
; extern int SendParallel(void far *buffer, unsigned int count);
;

SendParallel    PROC    NEAR

                DBG     ''
                mov     send_ptr, si
                mov     send_count, cx
                mov     dx, parallel_port   ; dx = base (data) register
                cmp     last_operation, SEND_OPERATION
                je      sp_proceed

;--- Wait for line turnaround

                mov     bl, last_nib_received
                call    wait_send_ack
                mov     last_operation, SEND_OPERATION

sp_proceed:     mov     di, send_count
                mov     si, send_ptr
                mov     cx, INITIAL_CRC     ; Initialize CRC accumulator
                call    make_seq_flags      ; Prepares sequence & flags in BL
                call    send_byte_sync      ; Expects param in AH
                jc      sp_error

                cmp     al, RACK
                jne     sp_pack_err         ; Seq num error - unrecoverable

                mov     bx, di
                mov     al, MINIMUM_TICKS
                call    send_byte
                jc      sp_pack_err

                mov     bl, bh              ; Send MSB of count
                mov     al, MINIMUM_TICKS
                call    send_byte
                jc      sp_pack_err

                cmp     fx_parallel_speed, PARALLEL_NORMAL
                je      sp_normal

                mov     ticks_remaining, MINIMUM_TICKS

                mov     cs:code_list_ptr, OFFSET send_pack_list2
                call    set_alarm_par   ; Start the race. . .

sp_loop:        mov     ticks_remaining, MINIMUM_TICKS
                mov     al, es:[si]     ; al = byte to send
                mov     bl, al          ; Keep a copy for CRC calculation
                inc     si              ; Point to next byte
                mov     ah, al          ; Preserve High nibble
                or      al, 10H         ; Or in transition bit
                out     dx, al          ; ==> Output lower nibble
                shr     ah, 1           ; ##### Replace w/ shr ah, 4 on 286
                shr     ah, 1
                shr     ah, 1
                shr     ah, 1
                inc     dx              ; Point to status register

@@:             in      al, dx          ; Wait for receivers transition
                or      al, al
spp2::          js      @B

                dec     dx              ; Point to data register
                mov     al, ah          ; al = high nibble
                out     dx, al          ; ==> output high nibble
                xchg    ch, cl
                xor     cl, bl
                xor     bx, bx
                xchg    bl, cl
                shl     bx, 1
                xor     cx, __crctab[bx]
                inc     dx              ; Point to status register

@@:             in      al, dx          ; Wait for receivers transition
                or      al, al
spp3::          jns     @B

                dec     dx              ; Point to data register
                dec     di              ; Decrement count
spp4::          jnz     sp_loop         ; Repeat until no bytes left

                call    clear_alarm_par ; Turn off alarm
                jc      sp_timeout

sp_common:      not     cx              ; Complement CRC accumulator
                mov     bx, cx
                xchg    bh, bl
                mov     al, MINIMUM_TICKS
                call    send_byte       ; Send complemented CRC accumulator MSB
                jc      sp_retry_err

                mov     bl, bh
                mov     al, bl
                mov     cl, 4
                shr     al, cl
                mov     last_nib_sent, al
                mov     al, MINIMUM_TICKS
                call    send_byte       ; Send complemented CRC accumulator LSB
                jc      sp_retry_err

                cmp     al, RACK        ; Did CRC match?
                jne     crc_error

sp_ok:          inc     seq_num
                and     seq_num, SEQUENCE_BITS
                mov     ax, 1           ; Return AX == 1 on success
                jmp     SHORT sp_ret

crc_error:      inc     __crc_errors    ; Increment CRC error counts
                mov     ax, FX_ERR_CRC
                jmp     short sp_retry_err  ; Get out if to many

sp_timeout::    DBG     '*'
                call    clear_alarm_par

sp_retry_err:   mov     ax, FX_ERR_FAIL

sp_check_speed: cmp     fx_parallel_speed, PARALLEL_NORMAL
                je      sp_error

;--- Got an error at TURBO speed so retry at NORMAL speed.

                mov     fx_parallel_speed, PARALLEL_NORMAL
                call    NearShowBaud
                mov     dx, parallel_port
                mov     al, WNAK
                out     dx, al
                mov     ax, 4 * MINIMUM_TICKS
                call    delay
                jmp     sp_proceed

sp_pack_err:    mov     ax, FX_ERR_FAIL

sp_error:       mov     fx_errno, ax
                mov     dx, parallel_port
                mov     al, WNAK
                out     dx, al
                xor     ax, ax          ; Return 0 on failure

sp_ret:         ret

;----------------------- normal (slow) code ----------------------------------

sp_normal:      mov     ticks_remaining, MINIMUM_TICKS
                mov     cs:code_list_ptr, OFFSET sp_normal_list
                call    set_alarm_par   ; Start the race. . .

spn_loop:       mov     ticks_remaining, MINIMUM_TICKS
                mov     al, es:[si]     ; al = byte to send
                mov     bl, al          ; Keep a copy for CRC calculation
                inc     si              ; Point to next byte
                mov     bh, al          ; Preserve High nibble
                or      al, 10H         ; Or in transition bit
                out     dx, al          ; ==> Output lower nibble
                shr     bh, 1           ; ##### Replace w/ shr ah, 4 on 286
                shr     bh, 1
                shr     bh, 1
                shr     bh, 1

@@:             call    readStatus
spn2::          jc      @B
                js      @B

                mov     al, bh          ; al = high nibble
                out     dx, al          ; ==> output high nibble
                xchg    ch, cl
                xor     cl, bl
                xor     bx, bx
                xchg    bl, cl
                shl     bx, 1
                xor     cx, __crctab[bx]

@@:             call    readStatus
spn3::          jc      @B
                jns     @B

                dec     di              ; Decrement count
spn4::          jnz     spn_loop        ; Repeat until no bytes left

                call    clear_alarm_par ; Turn off alarm
                jnc     sp_common

                jmp     sp_pack_err

spn_timeout::   DBG     '*'
                jmp     sp_timeout


SendParallel   ENDP



; recv_byte_noack
;   Receives a byte on port DX
;   This routine assumes that the sender posted an ACK on the port.
;
;   The final acknowledgement is not sent by this routine, but must be sent by
;   the caller of this routine.  This allows either an ACK or a NAK to be sent
;   back to the sender.
;
;   The complement of this routine is send_byte
;
;
;   Inputs:
;       AX          Ticks to wait for word
;       DX          Port base address
;
;   Outputs:
;       BL          byte received (Unknown if error)
;       CF          Clear if byte successfully recieved
;                   Set if error in recieve
;       DX          If no error occurs, port base address.  Otherwise unknown.
;
;   Registers destroyed: AX
;
;   If the last nibble recieved is RNAK, the alternate ready flag will be set.
;   Othersize it will be cleared
;
recv_byte_noack PROC    NEAR

                xor     ah, ah
                mov     ticks_remaining, ax
                mov     cs:code_list_ptr, OFFSET recv_word_list
                call    set_alarm_par   ; And we're off . . .

@@:             call    readStatus
rwp1::          jc      @B
                js      @B

                mov     bl, al          ; Save low nibble
                mov     al, NOT WNAK    ; Set Transition bit
                out     dx, al          ; And output it
                shr     bl, 1
                shr     bl, 1
                shr     bl, 1           ; place low nibble, high bit already 0

@@:             call    readStatus
rwp2::          jc      @B
                jns     @B

                shl     al, 1           ; Fix up nibble
                or      bl, al          ; Whole byte now in BL
                call    update_crc
                
rw_timeout::    call    clear_alarm_par

rw_ret:         ret 

recv_byte_noack ENDP

wait_send_ack   PROC    NEAR

;* Wait for sender to acknowledge having seen our last ACK/NAK.
;* The last nibble sent will be complemented when sender acknowledges.
;*
;* Enter with:
;*      DX = port base address
;*      BL = last nibble received
;* On return:
;*      AX, BL clobbered
;*      NC - Carry clear if send acknowledge seen
;*       C - Carry set if no sender acknowledge seen

                mov     ticks_remaining, MINIMUM_TICKS
                mov     cs:code_list_ptr, OFFSET wait_send_list
                call    set_alarm_par

                not     bl                      ; Complement the high nibble
                and     bl, 0F0H                ; Isolate turnaround bits

;*** Now wait for sender to toggle his high nibble so we know that he saw
;*** our acknowledgment.

@@:             call    readStatus
wsa1::          jc      @B

                shl     al, 1
                cmp     al, bl
                jne     @B

wsa_timeout::   call    clear_alarm_par
                ret

wait_send_ack   ENDP

recv_byte       PROC    NEAR

                call    recv_byte_noack
                jc      recv_byte_ret

                mov     al, WNAK
                out     dx, al
                
recv_byte_ret:  ret

recv_byte       ENDP

recv_byte_sync  PROC    NEAR USES SI DI BP

; Enter with:
;   DX = port_address
; On return:
;   BL = character read, NC, C flag set if error
;   BH, AX  clobbered

                call    SyncStart
                mov     di, ax          ; Save original time
                mov     bh, 0FFH        ; Put impossible input into BH
                mov     bp, OFFSET rbs1

reset_time:     call    GetTicks
                mov     si, ax          ; Save starting time in SI

rbs1:           call    readStatus
                jc      rbs_check
                jns     rbs_high_nib

                cmp     al, RIDLE       ; 11010000B
                je      valid_idle

                cmp     al, 10101000B
                jne     rbs_check

valid_idle:     push    ax
                call    GetTicks
                sub     ax, di
                cmp     ax, 3
                pop     ax
                jb      rbs_check

                mov     ah, al          ; Save copy in AH
                shr     al, 1
                shr     al, 1
                shr     al, 1
                and     al, 0FH
                out     dx, al
                cmp     ah, bh
                mov     bh, ah
                jne     reset_time

                jmp     short rbs_check

rbs_high_nib:   mov     bl, al          ; Save low nibble
                mov     al, NOT WNAK    ; Set Transition bit
                out     dx, al          ; And output it
                shr     bl, 1
                shr     bl, 1
                shr     bl, 1           ; place low nibble, high bits already 0
                mov     bp, OFFSET rbs2

rbs2:           call    readStatus
                jc      rbs_check
                jns     rbs_check

                shl     al, 1           ; Fix up nibble
                or      bl, al          ; Whole byte now in BL
                clc
                jmp     short rbs_ret

rbs_check:      call    GetTicks
                sub     ax, si
                cmp     ax, SyncTimeout
                ja      rbs_fail

                call    CheckAbort
                jc      rbs_fail

                jmp     bp

rbs_fail:       stc

rbs_ret:        call    SyncDone
                ret

recv_byte_sync  ENDP

; RecvParallel     Recieve a packet
;   Inputs:
;       es:di       pointer to buffer
;       cx          max size of buffer
;
;   Outputs:
;       CF          Set if packet recieved
;                   Clear if timeout or some other error
;       cx          If no error, number of bytes recieved.  Otherwise, unknown.
;
;   Uses registers: ax bx cx dx si di
;
; extern unsigned int RecvParallel(void far *buffer, unsigned int count);
;
RecvParallel    PROC    NEAR

                DBG     ''
                mov     fx_errno, 0
                mov     bp, cx              ; save buffer length
                mov     recv_ptr, di
                mov     dx, parallel_port   ; Point to base (data) register
                cmp     last_operation, RECV_OPERATION
                je      rp_proceed

                mov     al, last_nib_sent
                not     al
                and     al, 0FH
                out     dx, al
                mov     last_operation, RECV_OPERATION

rp_proceed:     mov     di, recv_ptr
                call    recv_byte_sync
                mov     ax, FX_ERR_TIMEOUT
                jc      rp_error


                mov     cx, INITIAL_CRC     ; Initialize CRC accumulator
                call    update_crc
                and     bl, SEQUENCE_BITS
                cmp     bl, seq_num
                je      get_cnt

                mov     ax, FX_ERR_BAD_SEQUENCE
                jmp     rp_error

get_cnt:        mov     al, WACK
                out     dx, al          ; ==> Acknowledge sequence number
                mov     al, MINIMUM_TICKS
                call    recv_byte
                jc      rp_fail

                mov     bh, bl          ; Save LSB of length in BH
                mov     al, MINIMUM_TICKS
                call    recv_byte
                jc      rp_fail

                xchg    bh, bl
                dec     bx              ; NOTE: 0 = 64K, so dec before cmp
                dec     bp
                cmp     bx, bp          ; Room in buffer?
                ja      rp_fail

                inc     bx
                inc     bp
                mov     si, bx          ; SI = count
                mov     save_length, bx ; Save number of bytes to be read
                cmp     fx_parallel_speed, PARALLEL_NORMAL
                je      recv_normal

                mov     ticks_remaining, MINIMUM_TICKS

                mov     cs:code_list_ptr, OFFSET recv_pack_list
                call    set_alarm_par   ; And there off. . .

rp_loop:        inc     dx              ; Point to status register

@@:             in      al, dx          ; Wait for transition
                or      al, al
rpp1::          js      @B

                in      al, dx          ; re-read after line has settled a bit
                mov     bl, al          ; save low nibble
                dec     dx              ; Point to data register
                mov     al, NOT WNAK    ; set transition bit
                out     dx, al          ; output transition
                shr     bl, 1           ; get lower nibble into its position
                shr     bl, 1           ; #### Replace with shr bl, 3 on 286
                shr     bl, 1
                inc     dx
                mov     ticks_remaining, MINIMUM_TICKS

@@:             in      al, dx          ; Wait for transition
                or      al, al
rpp2::          jns     @B

                in      al, dx          ; Let line settle a bit
                mov     bh, al          ; save high nibble
                mov     al, WNAK        ; clear transition bit
                dec     dx              ; point to data register
                out     dx, al          ; Output transition
                shl     bh, 1           ; Position high nibble
                and     bh, 0F0H        ; clear out garbage nibble
                or      bl, bh          ; or nibbles together
                mov     al, bl          ; save byte in the buffer
                stosb
                xchg    ch, cl
                xor     cl, bl
                xor     bx, bx
                xchg    bl, cl
                shl     bx, 1
                xor     cx, __crctab[bx]

                dec     si              ; Any bytes left to recieve?
rpp3::          jnz     rp_loop

                call    clear_alarm_par ; Turn off the alarm clock
                jc      rp_timeout

recv_common:    mov     al, MINIMUM_TICKS
                call    recv_byte       ; Receive CRC MSB
                jc      rp_timeout

                mov     al, MINIMUM_TICKS
                call    recv_byte_noack ; Receive CRC LSB
                jc      rp_timeout

                cmp     cx, MAGIC_CRC   ; Check for CRC error
                jne     crc_error

rp_ok:          mov     al, WACK        ; Acknowledge packet recieved
                out     dx, al
                mov     last_nib_received, bl   ; Save copy of last nibble
                inc     seq_num
                and     seq_num, SEQUENCE_BITS
                mov     ax, save_length
                jmp     SHORT rp_exit

crc_error:      inc     __crc_errors      ; Increment CRC error counts
                DBG     '~'
                mov     ax, FX_ERR_CRC
                jmp     short rp_check_speed

rp_timeout::    call    clear_alarm_par
                DBG     '*'
                mov     ax, FX_ERR_FAIL

rp_check_speed: cmp     fx_parallel_speed, PARALLEL_NORMAL
                je      rp_error

;--- Got an error at TURBO speed so retry at NORMAL speed.

                mov     fx_parallel_speed, PARALLEL_NORMAL
                call    NearShowBaud
                mov     dx, parallel_port
                mov     al, WNAK
                out     dx, al
                mov     ax, 2 * MINIMUM_TICKS
                call    delay
                jmp     rp_proceed

rp_fail:        mov     ax, FX_ERR_FAIL

rp_error:       mov     fx_errno, ax
                mov     dx, parallel_port
                mov     al, WNAK
                out     dx, al
                xor     ax, ax

rp_exit:        ret

;--------------------- normal (slow) receive code ---------------------------

recv_normal:    mov     ticks_remaining, MINIMUM_TICKS
                mov     cs:code_list_ptr, OFFSET rp_normal_list
                call    set_alarm_par   ; And there off. . .

rpn_loop:       call    readStatus
rpn1::          jc      rpn_loop
                js      rpn_loop

                mov     bl, al          ; save low nibble
                mov     al, NOT WNAK    ; set transition bit
                out     dx, al          ; output transition
                shr     bl, 1           ; get lower nibble into its position
                shr     bl, 1           ; #### Replace with shr bl, 3 on 286
                shr     bl, 1
                mov     ticks_remaining, MINIMUM_TICKS

@@:             call    readStatus
rpn2::          jc      @B
                jns     @B

                mov     bh, al          ; save high nibble
                mov     al, WNAK        ; clear transition bit
                out     dx, al          ; Output transition
                shl     bh, 1           ; Position high nibble
                or      bl, bh          ; or nibbles together
                mov     al, bl          ; save byte in the buffer
                stosb
                xchg    ch, cl
                xor     cl, bl
                xor     bx, bx
                xchg    bl, cl
                shl     bx, 1
                xor     cx, __crctab[bx]

                dec     si              ; Any bytes left to recieve?
rpn3::          jnz     rpn_loop

                call    clear_alarm_par ; Turn off the alarm clock
                jc      rp_fail

                jmp     recv_common

rpn_timeout::   DBG     '*'
                jmp     rp_timeout


RecvParallel    ENDP


ParallelInit    PROC    NEAR

                mov     ax, 3500H + TIMER_INTERRUPT
                int     21H             ; Current timer vector -> timer_save
                mov     WORD PTR cs:timer_save, bx
                mov     WORD PTR cs:timer_save + 2, es

                mov     WORD PTR cs:timer_jmp, OFFSET timer_passthru_par
                mov     dx, OFFSET driver_timer_handler
                push    ds              ; Save DS
                push    cs
                pop     ds              ; ds:dx -> driver_timer_handler
                mov     ax, 2500H + TIMER_INTERRUPT
                int     21H             ; Set timer vector to timer handler
                pop     ds              ; restore DS
                ret

ParallelInit    ENDP

ParallelExit    PROC    NEAR

                push    ds
                lds     dx, cs:timer_save
                mov     ax, 2500H + TIMER_INTERRUPT
                int     21H
                pop     ds
                ret

ParallelExit    ENDP


                END
