INCLUDE         model.inc
INCLUDE         fastlynx.inc

                .DATA
                EXTRN   last_operation:BYTE
                PUBLIC  FirstListen
FirstListen     db      1
StartTicks      dw      ?
                
        		.CODE	text
                PUBLIC  ResetListenTime
                EXTRN   FxRestorePorts:NEAR
                EXTRN   FxInitPorts:NEAR
                EXTRN   GetTicks:NEAR
                EXTRN   SetPortInfo:NEAR
                EXTRN   GetPortInfo:NEAR

ResetListenTime PROC    NEAR

                call    GetTicks
                mov     StartTicks, ax
                ret

ResetListenTime ENDP

FxListen        PROC    FAR PASCAL

;* extern int _pascal FxListen(void);

                cmp     FirstListen, 1
                jne     no_reset

                call    ResetListenTime
                mov     FirstListen, 0

no_reset:       xor     bx, bx

listen_loop:    cmp     bx, fx_tail
                jae     fail

                call    GetPortInfo
                call    fx_listen_vector[bx]
                jnc     success

                inc     bx
                inc     bx
                jmp     listen_loop

fail:           call    GetTicks
                sub     ax, StartTicks
                cmp     ax, FxSettings.FxSettingsDef.fxs_time_out
                jl      no_timeout

                call    FxInitPorts

no_timeout:     xor     ax, ax
                ret

success:        call    SetPortInfo
                call    FxRestorePorts
                mov     last_operation, RECV_OPERATION
                mov     ax, 1
                ret

FxListen        ENDP

	            END
