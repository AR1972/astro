;***
;* $Workfile:   percent.asm  $
;* $Revision:   1.0  $
;*   $Author:   Dave Sewell  $
;*     $Date:   13 Oct 1989 11:28:00  $
;*
;*****************************************************************************

%               .MODEL memmodel

INTERLNK_MULTIPLEX_ID   EQU     056H

                .CODE

;extern int _fastcall is_il_drive(unsigned drive_num);
;

@is_il_drive    PROC
                PUBLIC  @is_il_drive

                push    cx
                mov     bh, al                      ; Driver number to BH
                mov     al, 1                       ; drive check
                push    di
                push    si
                push    bp
                push    ds
                push    es
                mov     ah, INTERLNK_MULTIPLEX_ID
                mov     bl, 0
                mov     dx, 0FFFFH
                int     2FH
                cmp     al, 0FFH
                mov     ax, 1
                je      done

                xor     ax, ax

done:           pop     es
                pop     ds
                pop     bp
                pop     si
                pop     di
                pop     cx
                ret

@is_il_drive    ENDP

            END
