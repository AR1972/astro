;***
;* $Workfile:   calcreq.asm  $
;* $Revision:   1.0  $
;*   $Author:   Dave Sewell  $
;*     $Date:   13 Oct 1989 11:28:00  $
;*
;*****************************************************************************

%               .MODEL memmodel

                .CODE

;int _fastcall fc_toupper(int value);

@fc_toupper     PROC
                PUBLIC  @fc_toupper

                cmp     al, 'a'
                jb      upper_done

                cmp     ax, 'z'
                ja      upper_done

                sub     al, 'a' - 'A'

upper_done:     ret

@fc_toupper        ENDP


@isdigit        PROC
                PUBLIC  @isdigit

                cmp     al, '0'
                jb      false

                cmp     al, '9'
                ja      false

true:           mov     ax, 1
                ret

false:          xor     ax, ax
                ret

@isdigit        ENDP

            
                END
