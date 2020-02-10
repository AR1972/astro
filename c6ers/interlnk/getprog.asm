;***
;* $Workfile:   getenv.asm  $
;* $Revision:   1.0  $
;*   $Author:   Dave Sewell  $
;*     $Date:   13 Oct 1989 11:28:00  $
;*
;*****************************************************************************

%               .MODEL memmodel

                .DATA
                EXTRN   C _psp:WORD

                .CODE

;extern void _fastcall getprog(char near *progname);
;
; BX = progname

@getprog        PROC
                PUBLIC  @getprog

                push    di
                mov     es, _psp
                mov     es, es:[2CH]     ;Get segment of environment
                xor     di, di
                xor     ax, ax
                mov     cx, 8000H

scan_end: repne scasb
                scasb
                jne     scan_end

                inc     di
                inc     di

copy_prog:      mov     al, es:[di]
                inc     di
                mov     [bx], al
                inc     bx
                or      al, al
                jnz     copy_prog

                pop     di
                ret

@getprog        ENDP

            END
