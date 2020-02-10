;***
;* $Workfile:   setcerr.asm  $
;* $Revision:   1.1  $
;*   $Author:   Dave Sewell  $
;*     $Date:   27 Jun 1989 14:55:10  $
;***

PAGE 65, 132
;***
;* File:   setcerr.asm
;* Author: Dave Sewell
;*
;* Routine to set INT 24 critical error handler.
;***

%               .MODEL memmodel, language

                EXTRN    pascal criterr:PROC

                .CODE

critical_error  PROC    FAR

                push    bx              ;Save registers
                push    cx
                push    dx
                push    si
                push    di
                push    bp
                push    ds
                push    es

                push    ax              ;Preserve AH

                mov     bx, DGROUP
                mov     ds, bx

                push    ax              ;Push Device number & allowed action
                                        ;code
                mov     bx, di
                xor     bh, bh
                push    bx              ;Push error code

                push    bp              ;Push driver header segment
                push    si              ;Push driver header offset

                call    criterr         ;Returns Abort/Ignore/Fail code in AL

                pop     bx              ;restore AH
                mov     bh, ah

                pop     es              ;restore other registers
                pop     ds
                pop     bp
                pop     di
                pop     si
                pop     dx
                pop     cx
                pop     bx

                iret

critical_error  ENDP

;*
;* Calling sequence: set_critical_error();
;*      extern void set_critical_error(void);

set_critical_error  PROC    USES DS DI SI

                mov     dx, OFFSET critical_error
                push    cs
                pop     ds
                mov     ax, 2524H       ;Set INT 24 vector
                int     21H

                ret

set_critical_error  ENDP

                END
