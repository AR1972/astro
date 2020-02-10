;***
;* $Workfile:   nullproc.asm  $
;* $Revision:   1.1  $
;*   $Author:   Dave Sewell  $
;*     $Date:   09 Oct 1989 11:56:16  $
;*
;* Null routine used for short delay.  All it does is a far return.
;***

%		.MODEL memmodel, language

        IF  @CodeSize
            .CODE   PARAGON_TEXT
        ELSE
            .CODE
        ENDIF

null_proc	PROC	FAR

    		ret

null_proc	ENDP

		END
