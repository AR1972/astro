;***
;* $Workfile:   saveport.asm  $
;* $Revision:   1.0  $
;*   $Author:   Dave Sewell  $
;*     $Date:   05 May 1989 16:34:22  $
;***

INCLUDE         model.inc
INCLUDE         fastlynx.inc

                .DATA
    
        		.CODE	text
                
                EXTRN   SaveSerial:NEAR
                EXTRN   SaveParallel:NEAR
                EXTRN   RestoreSerial:NEAR
                EXTRN   RestoreParallel:NEAR
                EXTRN   GetPortInfo:NEAR
                PUBLIC  FxRestorePorts

FxSaveAllPorts  PROC    NEAR

                xor     bx, bx

save_loop:      cmp     bx, fx_tail
                jae     save_done

                call    FxSavePort

                inc     bx
                inc     bx
                jmp     save_loop

save_done:      ret

FxSaveAllPorts  ENDP

FxSavePort      PROC    NEAR    USES BX DI SI ES

                mov     al, 3
                mul     bl
                mov     di, ax
                push    ds
                pop     es                          ; ES:DI = ptr to save area
                add     di, OFFSET DGROUP:fx_port_save_area
                call    GetPortInfo
                cmp     al, SERIAL_PORT
                je      save_serial

                call    SaveParallel
                jmp     short save_ret

save_serial:    call    SaveSerial

save_ret:       ret

FxSavePort      ENDP

FxRestorePort   PROC    NEAR    USES BX SI

;*  Enter with:
;*      BX = Port to be restored

                mov     al, 3
                mul     bl
                mov     si, ax
                add     si, OFFSET DGROUP:fx_port_save_area
                call    GetPortInfo
                cmp     al, SERIAL_PORT
                je      restore_serial

                call    RestoreParallel
                jmp     short restore_ret

restore_serial: call    RestoreSerial

restore_ret:    ret

FxRestorePort   ENDP


FxRestorePorts  PROC    NEAR    USES SI

                xor     bx, bx

restore_loop:   cmp     bx, fx_tail
                jae     done

                cmp     bx, fx_index
                je      restore_next

                call    FxRestorePort

restore_next:   inc     bx
                inc     bx
                jmp     restore_loop

done:           ret

FxRestorePorts  ENDP

FxRestoreAllPorts   PROC    NEAR

                xor     bx, bx

restore_loop:   cmp     bx, fx_tail
                jae     restore_done

                call    FxRestorePort

                inc     bx
                inc     bx
                jmp     restore_loop

restore_done:   ret

FxRestoreAllPorts   ENDP

	            END
