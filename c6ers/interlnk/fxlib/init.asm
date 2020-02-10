; FxInit is used to save the current state of all ports, and to set the
; port listen and connect vectors to their initial state.

FX_MAIN         EQU     1

INCLUDE         model.inc
INCLUDE         fastlynx.inc

                .DATA
                PUBLIC  seq_num
                PUBLIC  fx_tail
                PUBLIC  fx_index
                PUBLIC  fx_port_save_area
                PUBLIC  fx_connect_vector
                PUBLIC  fx_listen_vector
                PUBLIC  last_operation
                PUBLIC  C FxSettings
                EXTRN   FirstListen:BYTE

seq_num             db      ?
fx_port_save_area   db      (3 * 2) * MAX_PORTS DUP(?)
fx_tail             dw      ?
fx_index            dw      ?
fx_connect_vector   dw      MAX_PORTS DUP(?)
fx_listen_vector    dw      MAX_PORTS DUP(?)
last_operation      db      ?
FxSettings          FxSettingsDef <>


                .CODE   text
                EXTRN   FxSaveAllPorts:NEAR
                EXTRN   FxRestoreAllPorts:NEAR
                EXTRN   SerialListenInit:NEAR
                EXTRN   ParallelListenInit:NEAR
                EXTRN   RestoreSerial:NEAR
                EXTRN   RestoreParallel:NEAR
                EXTRN   SerialInit:NEAR
                EXTRN   SerialExit:NEAR
                EXTRN   ParallelInit:NEAR
                EXTRN   ParallelExit:NEAR
                EXTRN   GetPortInfo:NEAR
                PUBLIC  GetTicks

RestorePort     dw      OFFSET RestoreSerial
                dw      OFFSET RestoreParallel

ListenInit      dw      OFFSET SerialListenInit
                dw      OFFSET ParallelListenInit

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


FxInitPorts     PROC    NEAR    USES AX BX SI

; This sets the listen and connect vectors for each port to it's initial state.

                mov     FirstListen, 1
                xor     ax, ax
                dec     ax
                mov     fx_port, ax         ; fx_port  := -1
                mov     fx_index, ax        ; fx_index := -1
                xor     bx, bx

init_loop:      cmp     bx, fx_tail
                jae     finished

                call    GetPortInfo         ; Returns port type in AL
                xor     ah, ah              ;
                mov     si, ax
                shl     si, 1
                call    ListenInit[si]
                inc     bx
                inc     bx
                jmp     init_loop

finished:       ret

FxInitPorts     ENDP


FxInit          PROC    PASCAL

;* extern _pascal FxInit(void);

                mov     ax, fx_num_ports
                shl     ax, 1
                mov     fx_tail, ax         ; fx_tail = fx_num_ports * 2
                call    FxSaveAllPorts
                call    SerialInit
                call    ParallelInit
                call    FxInitPorts

                ret

FxInit          ENDP

FxExit          PROC    PASCAL PUBLIC

                call    FxRestoreAllPorts
                call    SerialExit
                call    ParallelExit
                ret

FxExit          ENDP


                END
