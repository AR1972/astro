;***
;* $Workfile:   htoi.asm  $
;* $Revision:   1.0  $
;*   $Author:   Dave Sewell  $
;*     $Date:   28 Apr 1989 16:50:42  $
;*
;* Hex string to integer, assembly language version
;**/

INCLUDE dos.mac

	TEXTSEG

; extern int pascal htoi(char *pnt);

PASCALDEF   htoi

	    push    bp
	    mov     bp, sp
	    push    si
	IF  LDATA
	    push    ds
	    lds     si, [BP + X]
	ELSE
	    mov     si, [BP + X]
	ENDIF
	    xor     dx, dx
	    mov     cl, 4
	    xor     ah, ah

hloop:	    lodsb
	    cmp     al, 'f'
	    ja	    done

	    cmp     al, 'a'
	    jb	    chkupper

	    sub     al, '0' + ('a' - 'A') + 7
	    jmp     short nextdig

chkupper:   cmp     al, 'F'
	    ja	    done
	    cmp     al, 'A'
	    jb	    chkdigit

	    sub     al, '0' + 7
	    jmp     short nextdig

chkdigit:   sub     al, '0'
	    jc	    done

	    cmp     al, 9
	    ja	    done

nextdig:    shl     dx, cl
	    add     dx, ax
	    jmp     hloop

done:	    mov     ax, dx
	IF  LDATA
	    pop     ds
	ENDIF
	    pop     si
	    pop     bp
	    ret     I

PASCALEND   htoi

	    TEXTEND
	    END
