;***
;* $Workfile:   sendrecv.asm  $
;* $Revision:   1.0  $
;*   $Author:   Dave Sewell  $
;*     $Date:   05 May 1989 16:34:22  $
;***

INCLUDE         model.inc
INCLUDE         fastlynx.inc


                .DATA
                PUBLIC  SendVector
                PUBLIC  SyncTimeout
                PUBLIC  RecvVector
                PUBLIC  SyncStart
                PUBLIC  SyncDone
                PUBLIC  CheckAbort
                EXTRN   parallel_port:WORD
                EXTRN   serial_port:WORD
                EXTRN   seq_num:BYTE            ; Serial sequence number


SendVector      dw      SendParallel
RecvVector      dw      RecvParallel

CheckAbortVec   dw      OFFSET DefaultCheckAbort
                dw      SEG DefaultCheckAbort

SyncTimeout     dw      2 * ONE_SECOND          ; Default sync timeout

SyncStartTime   dw      ?

        		.CODE	text
                EXTRN   FxInitPorts:NEAR
                EXTRN   FxRestorePorts:NEAR
                EXTRN   SendSerial:NEAR
                EXTRN   SendSerialBlock:NEAR
                EXTRN   RecvSerial:NEAR
                EXTRN   SendParallel:NEAR
                EXTRN   RecvParallel:NEAR
                EXTRN   setup_recv_byte:NEAR
                EXTRN   GetTicks:NEAR

SetPortInfo     PROC    NEAR USES DX            ; Enter with BX = port index

                mov     fx_index, bx            ; Save index as word value
                shr     bx, 1                   ; Convert to byte index
                mov     fx_port, bx
                shl     bx, 1                   ; Back to word index
                call    GetPortInfo
                cmp     al, SERIAL_PORT
                je      set_serial

                mov     SendVector, OFFSET SendParallel
                mov     RecvVector, OFFSET RecvParallel
                mov     parallel_port, dx
                jmp     short set_done

set_serial:     mov     SendVector, OFFSET SendSerial
                mov     RecvVector, OFFSET RecvSerial
                mov     serial_port, dx

set_done:       mov     seq_num, 0
                ret

SetPortInfo     ENDP

GetPortIndex    PROC    NEAR USES DX

                shr     bx, 1                   ; Convert to byte index
                mov     ax, TYPE FxPortInfoDef
                mul     bx
                shl     bx, 1                   ; Back to word index
                mov     si, ax
                ret

GetPortIndex    ENDP

GetPortInfo     PROC    NEAR    USES SI

; Enter with BX = word port index
; Returns    DX = port address
;            AL = port type
;            AH = biosnum

                call    GetPortIndex
                mov     dx, FxPortInfo[si].FxPortInfoDef.pi_address
                mov     al, FxPortInfo[si].FxPortInfoDef.pi_type
                mov     ah, FxPortInfo[si].FxPortInfoDef.pi_biosnum
                ret

GetPortInfo     ENDP


@FxSendSerialBlock  LABEL   FAR
                    PUBLIC  @FxSendSerialBlock

@FXSENDSERIALBLOCK  PROC    FAR PASCAL PRIVATE USES CX DI SI ES, buf:FAR PTR

                mov     cx, ax          ; CS    := length
                les     si, buf         ; ES:SI := buf ptr
                xor     bl, bl          ; clear all sending flags
                mov     dx, serial_port
                call    setup_recv_byte ; Note: BP is clobbered in here
                call    SendSerialBlock
                ret

@FXSENDSERIALBLOCK  ENDP

@FxSend         LABEL   FAR
                PUBLIC  @FxSend

@FXSEND         PROC    FAR PASCAL PRIVATE USES CX DI SI ES, buf:FAR PTR

;* int _far _fastcall FxSend(byte _far *buf, word length);

                mov     cx, ax          ; CX    := length
                les     si, buf         ; ES:SI := buff ptr
                dec     ax
                add     ax, si
                jnc     count_ok

                mov     fx_errno, FX_ERR_BAD_COUNT
                xor     ax, ax
                jmp     short send_fail

count_ok:       call    SendVector
                or      ax, ax
                jnz     FxSendRet       ; Returns non-zero value if success

send_fail:      call    FxInitPorts     ; zero if failure

FxSendRet:      ret

@FXSEND         ENDP


@FxReceive      LABEL   FAR
                PUBLIC  @FxReceive

@FXRECEIVE      PROC    FAR PASCAL PRIVATE USES CX DI SI ES, buf:FAR PTR

;* word _far _fastcall FxReceive(byte _far *buf, word max_len);

                mov     cx, ax          ; CX    := max length
                les     di, buf         ; ES:DI := buff ptr
                dec     ax
                add     ax, di
                jnc     count_ok

                mov     fx_errno, FX_ERR_BAD_COUNT
                xor     ax, ax
                jmp     short recv_fail

count_ok:       call    RecvVector
                or      ax, ax
                jnz     FxRecvRet       ; Returns count received on success

recv_fail:      call    FxInitPorts     ; zero if failure

FxRecvRet:      ret

@FXRECEIVE      ENDP

DefaultCheckAbort   PROC    FAR                 ; Default CheckAbort procedure

                xor     ax, ax
                ret

DefaultCheckAbort   ENDP

DoCheckAbort    PROC    NEAR USES BX CX ES

                call    GetTicks
                sub     ax, SyncStartTime
                call    dword ptr CheckAbortVec
                ret

DoCheckAbort    ENDP

SyncStart       PROC    NEAR

                call    GetTicks
                mov     SyncStartTime, ax
                ret

SyncStart       ENDP

SyncDone        PROC    NEAR USES AX DX

                pushf
                mov     dl, CHECK_ABORT_DONE
                call    DoCheckAbort
                popf
                ret

SyncDone        ENDP

CheckAbort      PROC    NEAR USES DX

; Enter with:
;   AX = elapsed tick count
; Return with:
;   AX = 0 to continue, else error code to be placed in fx_errno
;   ALL OTHER REGISTERS PRESERVED

                mov     dl, CHECK_ABORT_WAITING
                call    DoCheckAbort
                or      ax, ax
                jz      CheckAbortRet           ; Return with NC if AX == 0

                mov     fx_errno, ax
                stc

CheckAbortRet:  ret

CheckAbort      ENDP

@FxSyncTimeout  PROC

                xchg    ax, SyncTimeout
                ret

@FxSyncTimeout  ENDP

@FxSetCheckAbort    LABEL   FAR
                    PUBLIC  @FxSetCheckAbort

@FXSETCHECKABORT    PROC    FAR PASCAL PRIVATE USES ES, func:FAR PTR

                les     ax, func
                mov     dx, es                  ; DX:AX := func ptr
                mov     bx, dx
                or      bx, ax
                jnz     set_func_ptr

                mov     ax, OFFSET DefaultCheckAbort
                push    cs
                pop     dx

set_func_ptr:   mov     word ptr CheckAbortVec, ax
                mov     word ptr CheckAbortVec[2], dx
                ret

@FXSETCHECKABORT    ENDP

                END
