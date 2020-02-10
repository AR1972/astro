;***
;* $Workfile:   b_keybrd.asm  $
;* $Revision:   1.1  $
;*   $Author:   Dave Sewell  $
;*     $Date:   09 Oct 1989 11:56:28  $
;*
;* Fixed version of MSC "_bios_keybrd" routine.
;
;   Their routine messed up on Ctrl-Break.  This routine maps that key to
;   ESC.
;*******************************************************************************

%		.MODEL memmodel, language


        IF  @CodeSize
            .CODE   PARAGON_TEXT
        ELSE
            .CODE
        ENDIF

;***
;extern unsigned pascal paragon_bios_keybrd(unsigned service);
;
;Purpose:
;	The function "_bios_keybrd" performs
;	keyboard services using interrupt 16H.
;
;Entry:
;	unsigned service - specifies which keyboard service is being requested
;
;Exit:
;	For service 0, AL = Character code, AH = Scan Code
;	For service 1, AX = 0 if no key waiting, otherwise same as service 0
;	For service 2, AL = shift status byte
;
;Uses:
;	BX, CX, DX
;
;Exceptions:
;
;*******************************************************************************

paragon_bios_keybrd PROC    service:BYTE

		mov	ah, service
		int	16H		; request keyboard service
		jnz	check_cbrk

		cmp	service, 1
		jne	check_cbrk

		xor	ax,ax		; return 0 to indicate no key ready
		jmp	short done

check_cbrk:	or	ax, ax
		jnz	done

		mov	ax, 011BH	;Map Ctrl-Break to ESC

done:		ret

paragon_bios_keybrd ENDP

		    END
