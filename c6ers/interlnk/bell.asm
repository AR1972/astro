;***
;* File:    bell.asm
;* Author:  Dave Sewell
;* Date:    7-20-88
;* Description:
;*	Sound the bell using a DOS call below 12 so this can be called from
;*	the critical error handler.
;***

%		.MODEL memmodel, language

                IF  @CodeSize
                    .CODE   CWORTHY_TEXT
                ELSE
                    .CODE
                ENDIF

Bell		    PROC
;* extern void Bell(void);

    	    	mov	    dl, 7
        		mov	    ah, 6
		        INT	    21H
        		ret

Bell	    	ENDP
                
        		END
