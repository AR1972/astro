;***
;* $Workfile:   biostick.asm  $
;* $Revision:   1.0  $
;*   $Author:   Dave Sewell  $
;*     $Date:   05 May 1989 16:34:22  $
;*
;* Routine to read BIOS tick count.
;***

%		.MODEL memmodel, C

    IF  @CodeSize
		.CODE	_TEXT
    ELSE
        .CODE
    ENDIF

bios_ticks	    PROC
                
;* NAME
;*	bios_ticks -- Return current BIOS tick count.
;*
;* SYNOPSIS
;*	time = bios_ticks();
;*
;*	unsigned time;	 Current number of ticks (approximately 18.2 ticks/sec).
;*
;*	extern	unsigned _cdecl bios_ticks(void);

                push    bp
                push    bx
                push    cx
                push    dx
                mov     ah, 0
                int     1AH
                mov     ax, dx      ; Low  portion of tick count to AX
                pop     dx
                pop     cx
                pop     bx
                pop     bp
                ret

bios_ticks	    ENDP

	            END
