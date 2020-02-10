;***
;* $Workfile:   switcher.asm  $
;* $Revision:   1.1  $
;*   $Author:   Dave Sewell  $
;*     $Date:   27 Jun 1989 14:55:26  $
;***


%               .MODEL  memmodel, language

                .DATA
                EXTRN   C dos5_switcher:BYTE
                EXTRN   _display_segment:WORD

savepwr_flag    db      0               ; Set non-zero if SAVEPWR.EXE installed
savepwr_status  db      0               ; Original SAVEPWR.EXE status

                .CODE


call_back_info  LABEL   BYTE

SCBI_Next       dd      0
SCBI_Entry_Pt   dd      client_call_out
SCBI_Reserved   dd      0
SCBI_API_Ptr    dd      0

ts_callout      dd      0

old_int2f       dd      ?

client_call_out PROC    FAR

                retf

client_call_out ENDP

int2f_handler   PROC    FAR

                cmp     ax, 4B01H
                jne     pass_thru

                pushf
                call    dword ptr cs:old_int2f
                mov     word ptr cs:SCBI_Next, bx
                mov     word ptr cs:SCBI_Next + 2, es
                mov     bx, OFFSET call_back_info
                push    cs
                pop     es

int2f_iret::    iret

pass_thru:      jmp     dword ptr cs:old_int2f


int2f_handler   ENDP

hook_int2f      PROC    

                push    ds
                push    di
                push    si
                push    bp

;--- Save the old INT 2FH handler address

                mov     ax, 352FH
                int     21H
                mov     ax, es
                or      ax, bx
                jnz     save_old_int2f

                mov     bx, OFFSET int2f_iret
                push    cs
                pop     es

save_old_int2f: mov     WORD PTR cs:old_int2f, bx
                mov     WORD PTR cs:old_int2f + 2, es

;--- Insert our handler into the chain

                mov     ax, 252FH
                mov     dx, OFFSET int2f_handler
                push    cs
                pop     ds
                int     21H

; --- Use the Detect Switcher call to see if a task switcher is running

                mov     ax, 4B02H
                xor     bx, bx
                xor     di, di
                mov     es, di
                int     2FH
                mov     ax, es
                or      ax, di
                jz      hook_done       ; No task switcher loaded

                push    ds
                mov     ax, DGROUP
                mov     ds, ax
                mov     dos5_switcher, 1
                pop     ds
                mov     word ptr cs:ts_callout, di
                mov     word ptr cs:ts_callout + 2, es
                push    cs              ; Push return segment
                mov     ax, OFFSET hook_done
                push    ax              ; Push return offset
                push    es
                push    di
                mov     ax, 4
                mov     di, OFFSET call_back_info
                push    cs
                pop     es
                retf                    ; Simulate far call

hook_done:      mov     ax, DGROUP
                mov     ds, ax          ; Restore DS

; --- Now check for presence of SAVEPWR.EXE device driver ---

                mov     ax, 5400H
                xor     bx, bx
                int     2FH
                cmp     bx, "PM"
                jne     hook_ret

                inc     savepwr_flag
                mov     ax, 5401H
                mov     bx, 0100H
                int     2FH
                mov     savepwr_status, bh

hook_ret:       pop     bp
                pop     si
                pop     di
                pop     ds
                ret

hook_int2f      ENDP

unhook_int2f    PROC    NEAR


                push    ds
                push    di
                push    si
                push    bp
                cmp     savepwr_flag, 0
                je      @F

                mov     ax, 5401H
                mov     bh, 1
                mov     bl, savepwr_status
                int     2FH

@@:             mov     ax, word ptr cs:ts_callout
                or      ax, word ptr cs:ts_callout + 2
                jz      restore_int2f

                mov     ax, 5           ; Unhook call-out
                mov     di, OFFSET call_back_info
                push    cs
                pop     es
                call    dword ptr cs:ts_callout

restore_int2f:  mov     ax, 252FH
                lds     dx, cs:old_int2f
                int     21H
                pop     bp
                pop     si
                pop     di
                pop     ds
                ret

unhook_int2f    ENDP

                END
