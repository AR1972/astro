;***
;* $Workfile:   parallel.asm  $
;* $Revision:   1.3  $
;*   $Author:   Dave Sewell  $
;*     $Date:   04 May 1990  9:12:18  $
;***

                TITLE   Parallel communications subroutines
                PAGE    66, 132


;### PREVENT_TIMEOUT EQU     1

                INCLUDE drivers.mac
                INCLUDE debug.mac

                SUBTTL  Definitions
                PAGE

TIMER_INTERRUPT equ 1CH                 ; User timer interrupt vector

SEND_OPERATION   EQU     0
RECV_OPERATION   EQU     1

PARALLEL_NORMAL         EQU     0
PARALLEL_TURBO          EQU     1

DIRECTION_BIT   EQU     00100000B

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

WSYNC           equ     00010101B       ; WSYNC = 15H   NOT WSYNC = EAH
RSYNC           equ     ((WSYNC SHL 3) AND 7FH) OR ((NOT (WSYNC SHL 3)) AND 80H)
                                        ; RSYNC = A8H   NOT RSYNC = 57H

INITIAL_CRC     equ     0FFFFH          ; Initial CRC accumulator
MAGIC_CRC       equ     1D0FH           ; Magic CRC number

                PUBLIC  send_pack_parallel
                PUBLIC  recv_pack_parallel
                PUBLIC  send_sync_parallel
                PUBLIC  save_parallel
                PUBLIC  restore_parallel
                PUBLIC  ss_timeout
                PUBLIC  ssp1
                PUBLIC  ssp2
                PUBLIC  ssp3
                PUBLIC  sw_timeout
                PUBLIC  swp1
                PUBLIC  swp2
                PUBLIC  sp_timeout
                PUBLIC  spp2
                PUBLIC  spp3
                PUBLIC  spp4
                PUBLIC  spn_timeout
                PUBLIC  spn2
                PUBLIC  spn3
                PUBLIC  spn4
                PUBLIC  rw_timeout
                PUBLIC  rwp1
                PUBLIC  rwp2
                PUBLIC  rp_timeout
                PUBLIC  rpp1
                PUBLIC  rpp2
                PUBLIC  rpp3
                PUBLIC  rpn_timeout
                PUBLIC  rpn1
                PUBLIC  rpn2
                PUBLIC  rpn3
                PUBLIC  wsa1
                PUBLIC  wsa_timeout
                PUBLIC  par_fixup1
                PUBLIC  par_fixup2
                PUBLIC  par_fixup3
                PUBLIC  par_fixup4
                PUBLIC  timer_passthru_par


CORE            SEGMENT WORD PUBLIC 'CODE'
                EXTRN   win386_enh_mode:BYTE
                EXTRN   win_386_api:DWORD
                EXTRN   win_386_api_ok:BYTE
                EXTRN   last_operation:BYTE
                EXTRN   num_par_ports:BYTE
                EXTRN   crctab:WORD
                EXTRN   crc_errors:WORD
                EXTRN   seq_num:BYTE
                EXTRN   port_address:WORD
                EXTRN   save_area:BYTE
                EXTRN   timeout:BYTE
                EXTRN   code_list_ptr:WORD
                EXTRN   code_save:WORD
                EXTRN   ticks_remaining:WORD
                EXTRN   set_alarm_time:WORD
                EXTRN   alarm_vector:WORD
                EXTRN   par_timer_save:DWORD
                EXTRN   send_sync_list:WORD
                EXTRN   send_word_list:WORD
                EXTRN   sp_normal_list:WORD
                EXTRN   send_pack_list2:WORD
                EXTRN   recv_word_list:WORD
                EXTRN   recv_pack_list:WORD
                EXTRN   rp_normal_list:WORD
                EXTRN   wait_send_list:WORD
                EXTRN   timer_jmp:WORD
                EXTRN   is_serial:BYTE
                EXTRN   busy_semaphore:BYTE
                EXTRN   idle_semaphore:BYTE
                EXTRN   start_packet:WORD
                EXTRN   end_packet:WORD
        IFDEF DEBUG
                EXTRN   debug_msg:WORD
        ENDIF

idle_ack        db      WIDLE
idle_recv_wait  db      0

last_nib_sent       db  ?
last_nib_received   db  ?

recv_ptr            dw  ?               ; Save area for buffer pointer
send_ptr            dw  ?               ; Save area for send buffer pointer
send_count          dw  ?               ; Save area for send count

fx_parallel_speed   db  ?

last_ticks      dw      0
current_ticks   dw      0

CORE            ENDS


PARALLEL            SEGMENT WORD PUBLIC 'CODE'
                    PUBLIC  parallel_start

                    ORG     0

parallel_start      LABEL   BYTE

driver_timer_handler    LABEL   FAR

                jmp     cs:timer_jmp

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
                call    GetTicks
                mov     bx, ax
                sub     ax, cs:set_alarm_time
                mov     cs:set_alarm_time, bx       ; Update set_alarm_time
                sub     cs:ticks_remaining, ax
                pop     bx
                pop     ax
                jc      timed_out

                jnz     timer_passthru_par

timed_out:      cmp     cs:timeout, 1
                je      timer_passthru_par

                push    ax              ; Preserve used registers
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
                jmp     cs:par_timer_save   ; chain through old timer vector

timer_passthru_par  LABEL   FAR         ; Interrupt handler while alarm not on!

                cmp     cs:busy_semaphore, 0
                jne     pass_it

                cmp     cs:port_address, 0
                je      pass_it

                cmp     cs:is_serial, 0
                jne     pass_it

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

pass_it:        jmp     cs:par_timer_save   ; chain through old timer vector

timer_handler_par   ENDP

GetTicks        PROC    NEAR

;* Enter with:
;* Uses:
;*      AX = logical tick count returned

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
                ret

GetTicks        ENDP


made_connection PROC    NEAR

                mov     fx_parallel_speed, PARALLEL_TURBO
                ret

made_connection ENDP

lost_connection PROC    NEAR

                mov     port_address, 0     ; Connection lost
                mov     idle_semaphore, 0
                ret

lost_connection ENDP

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

idle_handler    PROC    NEAR

                push    ax
                push    bx
                push    dx
                push    ds
                push    cs
                pop     ds              ; Load DS with our segment
                mov     last_operation, SEND_OPERATION
                mov     dx, port_address
                cmp     win_386_api_ok, 0
                jne     send_by_vfxd

                mov     al, idle_ack
                out     dx, al
                inc     dx
                in      al, dx          ; See if we got what we were expecting
                jmp     short check_response

send_by_vfxd:   mov     ax, PARALLEL_IDLER
                mov     bl, idle_ack
                call    dword ptr win_386_api

check_response: xor     al, 80H
                shr     al, 1
                shr     al, 1
                shr     al, 1
                cmp     al, idle_ack
                jne     no_recv

                not     idle_ack
                and     idle_ack, 0FH       ; Complement idle ack
                mov     idle_recv_wait, 0   ; and reset timeout
                jmp     short idle_ret

no_recv:        inc     idle_recv_wait
                cmp     idle_recv_wait, 2 * MINIMUM_TICKS
                jbe     idle_ret

                call    lost_connection

idle_ret:       pop     ds
                pop     dx
                pop     bx
                pop     ax
                ret

idle_handler    ENDP

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

                mov     timeout, 0
                push    ax
                call    GetTicks
                mov     set_alarm_time, ax
                pop     ax
    IFDEF   PREVENT_TIMEOUT
                mov     timer_jmp, DVR:timer_passthru_par
    ELSE
                mov     timer_jmp, DVR:timer_handler_par
    ENDIF
par_fixup1      EQU     $ - 2
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

                mov     timer_jmp, DVR:timer_passthru_par
par_fixup2      EQU     $ - 2
                cmp     timeout, 0
                je      no_timeout

                push    ax
                push    bx
                push    di
                push    si
                mov     di, code_list_ptr
                xor     bx, bx

restore_code:   mov     si, [di + bx + 2]
                or      si, si
                jz      restore_done

                mov     ax, code_save[bx]
                mov     [si], ax
                add     bx, 2
                jmp     restore_code

restore_done:   pop     si
                pop     di
                pop     bx
                pop     ax
                stc                     ; Carry for timeout failure
                ret

no_timeout:     clc                     ; No carry if no timeout
                ret

clear_alarm_par ENDP

readStatus      PROC    NEAR

;* Performs:    Reads a byte from the status port four times, and determines
;*              if the defined bits are equal all four times.
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


; send_sync -- Send synchonization byte
;
;   Inputs: None
;
;   Outputs:
;       CF  Set if synchronization was OK.  Clear otherwise.
;
;   Registers used: ALL
;
;   The complement of this routine is recv_sync.
;
;   Note: The send_sync_par and recv_sync_par subroutines rely on all other
;   I/O routines leaving something on the port other than the sync nibble.
;       If the sync nibble is on the port, the send_sync_par routine could
;   interpret that as the response from the other system.  But the other system
;   might never see the first sync nibble--thus the sync routines would time
;   out (the problem being described here is analogous to the problem on the
;   Toshiba laptops in Fastwire II).
;       If send_sync_par succeeds, the output line will be set to NOT WSYNC.
;   If it fails, output will be set to WNAK.
;
;   extern int send_sync_par(void);
;
send_sync_parallel      PROC    NEAR

                push    bx              ; Insure BX preserved
                mov     dx, port_address
                mov     al, NOT WNAK
                out     dx, al
                mov     ticks_remaining, CONNECT_TICKS
                mov     code_list_ptr, DVR:send_sync_list
                call    set_alarm_par   ; Start timing the sync

@@:             call    readStatus
ssp1::          jc      @B

                cmp     al, (NOT RSYNC) AND 0F8H
                jne     @B

                mov     al, WSYNC       ; Write out first sync nibble
                out     dx, al

@@:             call    readStatus
ssp2::          jc      @B

                cmp     al, RSYNC
                jne     @B
            
                mov     al, NOT WSYNC   ; Write out second sync nibble
                out     dx, al

@@:             call    readStatus
ssp3::          jc      @B

                cmp     al, RNAK
                jne     @B

                mov     seq_num, 0      ; reset sequence number
                call    made_connection
                jmp     SHORT ss_exit

ss_timeout::    mov     al, WNAK        ; Insure known state on output
                out     dx, al          ; dx is pointing to data register!

ss_exit:        call    clear_alarm_par
                pop     bx
                mov     last_operation, SEND_OPERATION
                ret

send_sync_parallel      ENDP


update_crc      PROC    NEAR    ; Update CRC in CX based on value in BL

                push    bx
                xchg    ch, cl
                xor     cl, bl
                xor     bx, bx
                xchg    bl, cl
                shl     bx, 1
                xor     cx, crctab[bx]
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

                mov     ticks_remaining, ax
                call    update_crc
                mov     code_list_ptr, DVR:send_word_list
                call    set_alarm_par   ; And we're off . . .
                mov     al, bl          ; Send low byte first

sw_loop:        or      al, 10H         ; Set transition bit
                out     dx, al          ; Send lower nibble
                shr     bl, 1           ; Prepare high nibble
                shr     bl, 1
                shr     bl, 1
                shr     bl, 1

@@:             call    readStatus
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

; send_pack_parallel  Send a packet
;   Inputs:
;       cx          Length of buffer to be sent
;       es:si       Pointer to buffer to be sent
;
;   Outputs:
;       CF          Set if packet successfully sent
;
;   Registers Destroyed: AX, BX, CX, DX, SI, DI, BP
;

send_pack_parallel      PROC    NEAR

                DBG     ''
                call    start_packet
                mov     send_ptr, si
                mov     send_count, cx
                dec     cx
                add     si, cx
                jc      sp_fail             ; Buff ptr + cnt = seg wrap

                mov     dx, port_address; dx = base (data) register
                or      dx, dx
                jz      sp_fail

                cmp     last_operation, SEND_OPERATION
                je      sp_proceed

;--- Wait for line turnaround

                mov     bl, last_nib_received
                call    wait_send_ack
                mov     last_operation, SEND_OPERATION

sp_proceed:     mov     di, send_count
                mov     si, send_ptr
                mov     cx, INITIAL_CRC     ; Initialize CRC accumulator
                mov     ax, SYNC_TICKS
                mov     bl, seq_num
                call    send_byte
                jc      sp_pack_err

                cmp     al, RACK
                jne     sp_error

                mov     bx, di
                mov     ax, MINIMUM_TICKS
                call    send_byte       ; Send LSB of count
                jc      sp_pack_err

                mov     bl, bh  
                mov     ax, MINIMUM_TICKS
                call    send_byte       ; Send MSB of count
                jc      sp_pack_err

                cmp     fx_parallel_speed, PARALLEL_NORMAL
                je      sp_normal

                mov     ticks_remaining, MINIMUM_TICKS
                mov     code_list_ptr, DVR:send_pack_list2
                call    set_alarm_par   ; Start the race. . .

sp_loop:        mov     ticks_remaining, MINIMUM_TICKS
                mov     al, es:[si]     ; al = byte to send
                mov     bl, al          ; Keep a copy for CRC calculation
                inc     si              ; Point to next byte
                mov     ah, al          ; Preserve High nibble
                or      al, 10H         ; Or in transition bit
                out     dx, al          ; Output lower nibble
                shr     ah, 1
                shr     ah, 1
                shr     ah, 1
                shr     ah, 1
                inc     dx              ; Point to status register

@@:             in      al, dx          ; Wait for receivers transition
                or      al, al
spp2::          js      @B

                dec     dx              ; Point to data register
                mov     al, ah          ; al = high nibble
                out     dx, al          ; output high nibble
                xchg    ch, cl
                xor     cl, bl
                xor     bx, bx
                xchg    bl, cl
                shl     bx, 1
                xor     cx, crctab[bx]
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
                mov     ax, MINIMUM_TICKS
                call    send_byte       ; Send complemented CRC accumulator MSB
                jc      sp_retry_err

                mov     bl, bh
                mov     al, bl
                mov     cl, 4
                shr     al, cl
                mov     last_nib_sent, al
                mov     ax, MINIMUM_TICKS
                call    send_byte
                jc      sp_retry_err

                cmp     al, RACK        ; Did CRC match?
                jne     sp_crc_err

                inc     seq_num
                and     seq_num, SEQUENCE_BITS
                clc                     ; Set success flag
                jmp     SHORT sp_exit

sp_crc_err:     inc     crc_errors      ; Increment CRC error counts
                DBG     '~'
                jmp     short sp_retry_err

sp_timeout::    call    clear_alarm_par
                DBG     '@'

sp_retry_err:   cmp     fx_parallel_speed, PARALLEL_NORMAL
                je      sp_error

;--- Got an error at TURBO speed so retry at NORMAL speed.

                DBG     ''
                mov     fx_parallel_speed, PARALLEL_NORMAL
                mov     dx, port_address
                mov     al, WNAK
                out     dx, al
                mov     ax, 4 * MINIMUM_TICKS
                call    delay
                jmp     sp_proceed

sp_pack_err:

sp_error:       DBG     '*'
                mov     dx, port_address
                mov     al, WNAK        ; Set transition bit to a known state
                out     dx, al
                call    lost_connection

sp_fail:        stc

sp_exit:        call    end_packet
                ret

sp_normal:      mov     ticks_remaining, MINIMUM_TICKS
                mov     code_list_ptr, DVR:sp_normal_list
                call    set_alarm_par   ; Start the race. . .

spn_loop:       mov     ticks_remaining, MINIMUM_TICKS
                mov     al, es:[si]     ; al = byte to send
                mov     bl, al          ; Keep a copy for CRC calculation
                inc     si              ; Point to next byte
                mov     bh, al          ; Preserve High nibble
                or      al, 10H         ; Or in transition bit
                out     dx, al          ; ==> Output lower nibble
                shr     bh, 1           ; ##### Replace w/ shr bh, 4 on 286
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
                xor     cx, crctab[bx]

@@:             call    readStatus
spn3::          jc      @B
                jns     @B

                dec     di              ; Decrement count
spn4::          jnz     spn_loop        ; Repeat until no bytes left

                call    clear_alarm_par ; Turn off alarm
                jnc     sp_common

                jmp     sp_pack_err

spn_timeout::   jmp     sp_timeout


send_pack_parallel      ENDP


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
recv_byte_noack PROC    NEAR

                mov     ticks_remaining, ax
                mov     code_list_ptr, DVR:recv_word_list
                call    set_alarm_par   ; And we're off . . .

@@:             call    readStatus
rwp1::          jc      @B
                js      @B

                mov     bl, al          ; Save low nibble
                mov     al, NOT WNAK    ; Set Transition bit
                out     dx, al          ; And output it
                shr     bl, 1
                shr     bl, 1
                shr     bl, 1           ; place low nibble, high bits already 0

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
;*      AX clobbered
;*      BL clobbered
;*      NC - Carry clear if send acknowledge seen
;*       C - Carry set if no sender acknowledge seen

                mov     ticks_remaining, MINIMUM_TICKS
                mov     code_list_ptr, DVR:wait_send_list
                call    set_alarm_par

                not     bl
                and     bl, 0F0H        ; Now AH has complemented high nibble

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



; recv_pack_parallel    Recieve a packet
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
; extern unsigned int recv_pack_par(void far *buffer, unsigned int count);
;
; Note: The last nibble written out by send_pack_par will be either an WACK
; or a WNAK.  The last operation will either be SEND or UNKNOWN.
;
recv_pack_parallel      PROC    NEAR

                DBG     ''
                call    start_packet
                mov     dx, port_address   ; Point to base (data) register
                or      dx, dx
                jz      rp_fail

                mov     bp, cx          ; save buffer length
                mov     recv_ptr, di
                dec     cx
                add     cx, di
                jc      rp_fail         ; Buff + max cnt = seg wrap

                cmp     last_operation, RECV_OPERATION
                je      rp_proceed

                mov     al, last_nib_sent
                not     al
                and     al, 0FH
                out     dx, al
                mov     last_operation, RECV_OPERATION

rp_proceed:     mov     di, recv_ptr
                mov     cx, INITIAL_CRC
                mov     ax, RECV_PACK_TICKS
                call    recv_byte_noack
                jc      rp_error

                and     bl, SEQUENCE_BITS
                cmp     bl, seq_num
                jne     rp_error

                mov     al, WACK
                out     dx, al
                mov     ax, MINIMUM_TICKS
                call    recv_byte
                jc      rp_error

                mov     bh, bl          ; Save LSB of length in BH
                mov     ax, MINIMUM_TICKS
                call    recv_byte
                jc      rp_error

                xchg    bh, bl
                dec     bx              ; NOTE: 0 = 64K, so dec before cmp
                dec     bp
                cmp     bx, bp          ; Room in buffer?
                ja      rp_error

                inc     bx
                inc     bp
                mov     si, bx          ; SI = count

                cmp     fx_parallel_speed, PARALLEL_NORMAL
                je      recv_normal

                mov     ticks_remaining, MINIMUM_TICKS
                mov     code_list_ptr, DVR:recv_pack_list
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
                shr     bl, 1
                shr     bl, 1
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
                xor     cx, crctab[bx]

                dec     si              ; Any bytes left to recieve?
rpp3::          jnz     rp_loop

                call    clear_alarm_par ; Turn off the alarm clock
                jc      rp_timeout

recv_common:    mov     ax, MINIMUM_TICKS
                call    recv_byte       ; Get CRC MSB
                jc      rp_timeout

                mov     ax, MINIMUM_TICKS
                call    recv_byte_noack ; Get CRC LSB
                jc      rp_timeout

                cmp     cx, MAGIC_CRC   ; Check for CRC error
                jne     rp_crc_err

                mov     al, WACK        ; Acknowledge packet recieved
                out     dx, al
                mov     last_nib_received, bl
                inc     seq_num
                and     seq_num, SEQUENCE_BITS
                clc                     ; set success flag
                jmp     SHORT rp_exit

rp_crc_err:     inc     crc_errors      ; Increment CRC error counts
                DBG     '~'
                jmp     short rp_check_speed

rp_timeout::    call    clear_alarm_par
                DBG     '*'

rp_check_speed: cmp     fx_parallel_speed, PARALLEL_NORMAL
                je      rp_error

;--- Got an error at TURBO speed so retry at NORMAL speed.

                mov     fx_parallel_speed, PARALLEL_NORMAL
                DBG     ''
                mov     dx, port_address
                mov     al, WNAK
                out     dx, al
                mov     ax, 2 * MINIMUM_TICKS
                call    delay
                jmp     rp_proceed

rp_error:       mov     dx, port_address
                mov     al, WNAK        ; Set transition bit to a known state
                out     dx, al
                call    lost_connection

rp_fail:        stc

rp_exit:        call    end_packet
                ret

recv_normal:    mov     ticks_remaining, MINIMUM_TICKS
                mov     cs:code_list_ptr, DVR:rp_normal_list
                call    set_alarm_par   ; And there off. . .

rpn_loop:       

@@:             call    readStatus
rpn1::          jc      @B
                js      @B

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
                xor     cx, crctab[bx]

                dec     si              ; Any bytes left to recieve?
rpn3::          jnz     rpn_loop

                call    clear_alarm_par ; Turn off the alarm clock
                jnc     recv_common

                jmp     rp_check_speed

rpn_timeout::   jmp     rp_timeout

recv_pack_parallel      ENDP


; save_parallel
;
save_parallel   PROC    NEAR

                push    bx
                mov     dx, port_address
                inc     dx
                inc     dx              ; Point to control register
                in      al, dx
                mov     ah, al          ; AH = original control port value
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

                mov     ah, al          ; Save desired control port value
                mov     al, 0CH         ; No Interrupts
                out     dx, al
                dec     dx
                dec     dx              ; Point to data register
                in      al, dx
                mov     WORD PTR save_area, ax   ; keep copy of original data
                pop     bx
                ret

save_parallel   ENDP



; reset_port_par  Reset the I/O port
;
;   Restores the port registers to there original values, restores any interrupt
;   vectors, and performs any other cleanup needed to release the current
;   communication port.  Essentially this routine undoes what init_port does.
;
;   Inputs:  None
;
;   Outputs: None
;
;   Destroys registers: AL, DX
;
;   extern void reset_port_par(void);
;
restore_parallel    PROC    NEAR

                mov     dx, port_address
                mov     ax, WORD PTR save_area
                out     dx, al          ; Restore original data
                add     dl, 2
                mov     al, ah
                out     dx, al          ; Restore original control
                ret

restore_parallel    ENDP


PARALLEL        ENDS


INIT            SEGMENT WORD PUBLIC 'CODE'


                PUBLIC  setup_ports_parallel
                PUBLIC  reset_ports_parallel

setup_ports_parallel    PROC    NEAR

                cmp     num_par_ports, 0
                je      setup_done

                mov     ax, 3500H + TIMER_INTERRUPT
                int     21H             ; Current timer vector -> par_timer_save
                mov     WORD PTR par_timer_save, bx
                mov     WORD PTR par_timer_save + 2, es
                mov     timer_jmp, DVR:timer_passthru_par
par_fixup3      EQU     $ - 2
                mov     dx, DVR:driver_timer_handler
par_fixup4      EQU     $ - 2
                mov     ax, 2500H + TIMER_INTERRUPT
                int     21H             ; Set timer vector to timer handler

setup_done:     ret

setup_ports_parallel    ENDP

reset_ports_parallel    PROC    NEAR

                cmp     num_par_ports, 0
                je      reset_done

                push    ds
                mov     ax, 2500H + TIMER_INTERRUPT
                lds     dx, par_timer_save
                int     21H
                pop     ds

reset_done:     ret

reset_ports_parallel    ENDP


INIT            ENDS

                END
