;            name    XMSDriver
.model  small,c
.code
            public  XMSDriver
XMSDriver   proc    XMSPtr: far ptr
            mov     ah,00h
            call    [XMSPtr]
            mov     ax,bx
            ret
XMSDriver   endp
;            end
;
;            name    XMSVersion
.model  small,c
.code
            public  XMSVersion
XMSVersion  proc    XMSPtr: far ptr
            mov     ah,00h
            call    [XMSPtr]
            ret
XMSVersion  endp


;
; This code snitched from xmsinfo.exe
;

HimemHandleTable proc uses es si, pHandleCount:NEAR PTR, pHandleSize:NEAR PTR

	mov	ax, 4309h	; 'secret' handle table info call
	int	2fh
	cmp	al, 43h
	jne	not_himem

	cmp	word ptr es:[bx], 0A01h ; info version 1 & Handle size 10?
	jne	not_himem

	mov	al, es:[bx+1]		; handle size
	cbw
	mov	si, [pHandleSize]
	mov	[si], ax

	mov	ax, es:[bx+2]		; # handles
	mov	si, [pHandleCount]
	mov	[si], ax

	mov	ax, es:[bx+4]		; offset to handle table
	mov	dx, es:[bx+6]		; segment of handle table
	jmp	short hht_ret

not_himem:

	xor	ax,ax			; doesn't look like himem,
	mov	dx,ax			; return NULL ptr

hht_ret:

	ret

HimemHandleTable endp


;
; Machines before the 80286 will set high the top four bits in the flags
; register as soon as you try to clear them.  So put zero in AX, move that
; to the flags register and back into AX ... if AX has the top four bits
; set, it's a pre-286.
;

IsPre286 proc			; Returns 0 if 286 or higher, !0 if <= 286

        xor    AX,AX
        push   AX
        popf
        pushf
        pop    AX

        and    AX, 0F000h	; Returns F000 if <= 286
	cmp    AX, 0F000h
	jz     ip286_ret

	xor    AX, AX

ip286_ret:

        ret

IsPre286 endp

	end
