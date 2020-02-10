;
;       Microsoft Confidential
;       Copyright (C) Microsoft Corporation 1991
;       All Rights Reserved.
;


vseg segment para public 'CODE'
	assume cs:vseg,ds:vseg,es:NOTHING

	org     0

PUBLIC  Interrupt
PUBLIC  Strategy

extrn   init_safedsk:near
extrn   buffer_size:word
extrn   track_buffer:byte

Header:
	dd      -1                      ;device chain link--filled in by dos
DevAttr dw      0C840h                  ;character device attribute word
	dw      Strategy                ;Strategy entry point
	dw      Interrupt               ;Interrupt entry point
	db      '_doubleB'              ;logical device name

RHPtr   dd      ?                       ;Request Header pointer filled in by 
					;Strategy routing

Strategy        proc    far

	mov     WORD PTR cs:[RHPtr],bx
	mov     WORD PTR cs:[RHPTR+2],es
	ret

Strategy        endp

Interrupt       proc    far
	push    ds
	push    es
	push    di
	push    bx
	push    dx

	push    cs
	pop     ds

	les     di,DWORD PTR cs:[RHPtr] ;load Request Header pointer into es:di
	mov     bl,es:[di+2]            ;get command code from RHPtr into bl

	or      bl,bl                   ;We only handle command zero (Init)
	jnz     error_out

	push    es
	push    di
	call    init_safedsk
	pop     di
	pop     es
	jnc     short dont_load
	

Return_To_Dos:
	pop     dx
	pop     bx
	pop     di
	pop     es
	pop     ds
	ret

dont_load:
	mov     WORD PTR es:[di+0Eh],offset Header ;return pointer to header
	mov     WORD PTR es:[di+10h],cs            ;to indicate don't load
	mov     word ptr es:[di+3],8100h           ;inform kernel of error

	mov     ax,7FFFh                           ;also clear high bit
	and     WORD PTR cs:DevAttr,ax             ;of attribute word
						   ;so next driver will load
						   ;on top of us

	xor     ax,ax                              ;return no error
	jmp     short return_to_dos

error_out:
	mov     word ptr es:[di+3],8003h ;bad command code in request header
	jmp     short return_to_dos

Interrupt       endp


vseg ends


end 

endif
