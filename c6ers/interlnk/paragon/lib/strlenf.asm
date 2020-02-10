;***
;* $Workfile:   strlenf.asm  $
;* $Revision:   1.1  $
;*   $Author:   Dave Sewell  $
;*     $Date:   09 Oct 1989 11:56:14  $
;*
;*  strlenf.asm      Alan Butt   April 26, 1988
;*
;*  String length routine for far pointers.
;*

%               .MODEL  memmodel, PASCAL

            IF  @CodeSize
                .CODE   PARAGON_TEXT
            ELSE
                .CODE
            ENDIF


;   extern unsigned int pascal strlenf(char far *str);
;*
;*  string length of far string
;*
strlenf         PROC    USES DI, str:FAR PTR BYTE

                les     di, str
                xor     al, al
                mov     cx, -1

                repne   scasb

                mov     ax, cx
                not     ax
                dec     ax

                ret

strlenf         ENDP

                END
