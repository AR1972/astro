;***
;* $Workfile:   pstrlen.asm  $
;* $Revision:   1.2  $
;*   $Author:   Dave Sewell  $
;*     $Date:   06 Sep 1990 14:49:40  $
;*
;*  pstrlen.asm     Alan Butt	May 12, 1988
;*
;*  String length function using pascal calling conventions
;*

%               .MODEL memmodel

        IF  @CodeSize
                .CODE   PARAGON_TEXT
        ELSE
                .CODE
        ENDIF
            
@pstrlen        PROC
                PUBLIC  @pstrlen

;   extern unsigned int _fastcall pstrlen(char near *str);
;*
;*  string length with _fastcall calling conventions
;*

                push    di
	            push    ds
	            pop     es
	            mov     di, bx

	            xor     ax, ax
	            mov     cx, -1
	            repne   scasb
	            mov     ax, cx
	            not     ax
	            dec     ax

                pop     di
	            ret

@pstrlen        ENDP

        	    END
