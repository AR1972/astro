;***
;* $Workfile:   find.asm  $
;* $Revision:   1.2  $
;*   $Author:   Dave Sewell  $
;*     $Date:   02 May 1990  8:45:38  $
;*
;* Routine to read a directory and place the resulting directory entries in
;* a buffer.
;*****************************************************************************

%               .MODEL memmodel, language

                .CODE

@long_to_ascii  PROC    USES DI SI
                PUBLIC  @long_to_ascii

; extern void _fastcall long_to_ascii(long value, char near *buff);
;
; _fastcall: DX:AX = value, BX = buff

                push    ds
                pop     es
                mov     di, bx      ;Point ES:DI to buffer
            	mov     bx, ax      ;DX:BX = value
            	mov     si, di      ;Save start of buffer
                mov     cx, 10      ;Get radix to CX

next_digit:     xchg    ax,dx       ;Divide hi
	            xor     dx,dx
	            div     cx          ;DX = rem, AX = hi div
                xchg    ax,bx       ;AX = lo,  BX = hi div
	            div     cx          ;DX = rem, BX:AX = div
	            xchg    ax,dx       ;AX = rem, BX:DX = div
	            xchg    dx,bx       ;AX = rem, DX:BX = div
	            add     al,'0'
	            stosb
	            mov     ax,dx
	            or      ax,bx
	            jnz     next_digit

            	mov     [di],al     ;Store trailing null

;       reverse digits

revloop:        dec     di          ; point back to last char
	            lodsb               ; exchange bytes
	            xchg    [di],al
	            mov     [si-1],al
; The following is equivalent to "cmp si,(di-1)"
; but avoids segment wrap in case DI == 0
	            lea	ax,[si+1]
	            cmp     ax,di       ; are we halfway?
	            jb	revloop         ;   no

                ret

@long_to_ascii  ENDP


            	END
