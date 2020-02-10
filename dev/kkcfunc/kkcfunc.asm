;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;
;	***** INT 2Fh function 4Dh for Kana Kanji Converters *****
;

include		versiona.inc
include		syscall.inc
include		kkcfunc.inc

code	segment byte public
	assume	cs:code,ds:code,es:code

	org	0

start	label	byte

dev_header	label	byte
	dd	-1
	dw	CHAR_DEV
	dw	strategy
	dw	interrupt
	db	"KKCFUNC$"

req_header	dd	?

org_int		label	word
	dd	?			; int 23h
	dd	?			; int 24h

org_int2f	dd	?
indos_flag	dd	?
org_pdp		dw	?

dos_func_flag	dw	?

dos_func_err	dw	?

crit_err_no	dw	0

kkc_ver_tbl	label	word
	dw	MAX_KKC_NUM
	dw	MAX_KKC_NUM dup (0)

kkc_func_tbl	label	word
	dw	get_func_adrs
	dw	get_err_no
	dw	regist_kkc
	dw	get_table_adrs
KKC_FUNC_MAX	equ	($ - kkc_func_tbl) / 2

kkc_dos_tbl	label	byte
	db	19h,-1			; get current disk
	db	1ah,-1			; set DTA address
	db	2fh,-1			; get DTA address
	db	3dh,-1			; open file
	db	3eh,-1			; close file
	db	3fh,-1			; read file or device
	db	40h,-1			; write file or device
	db	42h,-1			; set file pointer
	db	44h,02h			; read control data
	db	44h,0eh			; get logical drive map
	db	47h,-1			; get current directory
	db	4eh,-1			; find first file
KKC_DOS_LEN	equ	($ - kkc_dos_tbl) / 2

dos_save_buf	db	DOS_SAVE_LEN dup (0)

;------------------------------------------------------------------------------

strategy	proc	far
	mov	word ptr cs:[req_header],bx
	mov	word ptr cs:[req_header+2],es
	ret
strategy	endp

interrupt	proc	far
	push	bx
	push	es
	les	bx,cs:[req_header]
	cmp	es:[bx.command],DEV_INIT
	jz	intr_init		; if device initialize
	cmp	es:[bx.command],DEV_REST
	jz	intr_rest		; if device restore
	mov	ah,DEV_ERROR or DEV_DONE
	mov	al,DEV_INVALID
	jmp	short intr_ret
intr_init:
	call	inst_kkc
	jc	intr_ret
	not	word ptr es:[bx.bpb_adrs]	; device restore is available
	not	word ptr es:[bx.bpb_adrs+2]
	mov	word ptr es:[bx.break_adrs], offset end_off ; device end offset
	mov	word ptr es:[bx.break_adrs+2],cs
	jmp	short intr_ret
intr_rest:
	call	rest_kkc
	mov	ah,DEV_DONE
	xor	al,al
intr_ret:
	mov	es:[bx.status],ax
	pop	es
	pop	bx
	ret
interrupt	endp

inst_kkc	proc	near
	cld
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	ds
	push	es
	push	cs
	pop	ds
	mov	ah,Get_Version
	int	21h
	cmp	ax,expected_version
	jnz	ver_err			; invalid version
	mov	ah,Get_Interrupt_Vector
	mov	al,2Fh
	int	21h
	mov	word ptr org_int2f,bx	; save original address
	mov	word ptr org_int2f+2,es
	mov	ah,Get_InDOS_Flag
	int	21h
	mov	word ptr indos_flag,bx	; save indos flag address
	mov	word ptr indos_flag+2,es
	push	ds
	mov	ah,Get_Current_PDB
	int	21h
	mov	ds,bx
	mov	si,0
	lea	di,kkc_pdp
	push	cs
	pop	es
	mov	cx,256 / 2
	rep	movsw			; copy pdp
	pop	ds
	mov	ah,Set_Interrupt_Vector
	mov	al,2Fh
	lea	dx,int2f_kkc		; install KKCFUNC
	int	21h
	mov	ah,DEV_DONE
	xor	al,al
	clc
	jmp	short inst_ret
ver_err:
	xor	ax,ax
	stc
inst_ret:
	pop	es
	pop	ds
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	ret
inst_kkc	endp

;------------------------------------------------------------------------------

rest_kkc	proc	near
	cld
	push	ax
	push	si
	push	di
	push	ds
	push	es
	mov	ax,cs
	mov	ds,ax
	mov	ax,0			; int vect seg
	mov	es,ax
	lea	si,org_int2f
	mov	di,2fh*4
	cmp	word ptr es:[di],offset int2f_kkc
	jnz	@f			; if int2f has changed
	mov	ax,cs
	cmp	es:[di+2],ax
	jnz	@f
	movsw				; restore original int 2fh
	movsw
@@:
	pop	es
	pop	ds
	pop	di
	pop	si
	pop	ax
	ret
rest_kkc	endp

;------------------------------------------------------------------------------

int2f_kkc	proc	far
	cmp	ah,4dh
	jnz	not_kkc			; if not KKCFUNC
	cmp	al,KKC_FUNC_MAX
	jae	not_kkc			; if invalid sub function
	push	si
	mov	si,ax
	and	si,0ffh			; strip high byte
	shl	si,1
	call	cs:[kkc_func_tbl+si]	; execute subfunction
	pop	si
	iret
not_kkc:
	jmp	cs:org_int2f
int2f_kkc	endp

;------------------------------------------------------------------------------

get_func_adrs	proc	near
	lea	dx,kkc_dos_func
	mov	cx,cs
	mov	ax,SUCCESS
	ret
get_func_adrs	endp

get_err_no	proc	near
	mov	ax,cs:[dos_func_err]
	mov	dx,cs:[crit_err_no]
	ret
get_err_no	endp

chk_busy	proc	near
	mov	dx,cs:dos_func_flag
	mov	ax,SUCCESS
	ret
chk_busy	endp

regist_kkc	proc	near
	cld
	push	cx
	push	di
	push	es
	push	cs
	pop	es
	cmp	cx,1
	jz	rk_rel			; if release
	lea	di,kkc_ver_tbl+2
	mov	cx,es:[di-2]
	mov	ax,0
	repnz	scasw			; find null handle
	mov	ax,OVER_ERR
	jnz	rk_ret			; if full
	sub	di,2
	mov	es:[di],dx		; regist kkc version
	mov	bx,di
	sub	bx,offset kkc_ver_tbl+2
	shr	bx,1			; get handle no.
	inc	bx
	mov	ax,SUCCESS
	jmp	short rk_ret
rk_rel:
	mov	di,bx
	cmp	di,cs:[kkc_ver_tbl]
	mov	ax,HANDLE_ERR
	ja	rk_ret			; if invalid handle
	shl	di,1
	cmp	cs:[kkc_ver_tbl+di],0
	jz	rk_ret			; if not registrated
	mov	cs:[kkc_ver_tbl+di],0	; release
	mov	ax,SUCCESS
rk_ret:
	pop	es
	pop	di
	pop	cx
	ret
regist_kkc	endp

get_table_adrs	proc	near
	lea	dx,kkc_ver_tbl
	mov	cx,cs
	mov	ax,SUCCESS
	ret
get_table_adrs	endp

;------------------------------------------------------------------------------

kkc_dos_func	proc	far
	mov	cs:[dos_func_err],0
	mov	cs:[crit_err_no],-1

	call	chk_func_no
	jc	kdf_inv			; if invalid function

	cmp	cs:dos_func_flag,BUSY
	jz	kdf_busy		; if busy

	cli
	mov	cs:dos_func_flag,BUSY

	call	save_dos_data
	call	break_off
	call	set_pdp
	call	set_int

	int	21h
	pushf

	cli
	call	rest_int
	call	rest_pdp
	call	rest_dos_data

	mov	cs:dos_func_flag,NOT_BUSY

	cmp	cs:[dos_func_err],CRITICAL_ERR
	jz	kdf_crit
	popf
	sti
	jmp	short kdf_ret
kdf_crit:
	popf
	sti
	jmp	short kdf_err
kdf_inv:
	mov	cs:[dos_func_err],INVALID_FUNC
	jmp	short kdf_err
kdf_busy:
	mov	cs:[dos_func_err],FUNC_BUSY
kdf_err:
	mov	ax,ERR
	stc
kdf_ret:
	ret
kkc_dos_func	endp

chk_func_no	proc	near
	cld
	push	ax
	push	cx
	push	di
	push	es
	push	cs
	pop	es
	lea	di,kkc_dos_tbl
	mov	cx,KKC_DOS_LEN
	xchg	ah,al
cfn_loop:
	scasb				; find function no.
	jnz	cfn_next		; if not found
	cmp	byte ptr es:[di],-1
	jz	@f			; if no sub funcion no. to check
	cmp	es:[di],ah
	jnz	cfn_next		; if subfunction not found
@@:
	clc
	jmp	short cfn_ret
cfn_next:
	inc	di
	loop	cfn_loop
	stc
cfn_ret:
	pop	es
	pop	di
	pop	cx
	pop	ax
	ret
chk_func_no	endp

save_dos_data	proc	near
	cld
	push	cx
	push	si
	push	di
	push	ds
	push	es
	lds	si,cs:indos_flag
	cmp	byte ptr [si],0
	jz	sdd_ret			; if not in dos
	mov	si,DOS_SAVE_ST
	mov	cx,DOS_SEG
	mov	ds,cx
	lea	di,dos_save_buf
	push	cs
	pop	es
	mov	cx,DOS_SAVE_LEN
	rep	movsb			; save dos data
sdd_ret:
	pop	es
	pop	ds
	pop	di
	pop	si
	pop	cx
	ret
save_dos_data	endp

rest_dos_data	proc	near
	cld
	push	cx
	push	si
	push	di
	push	ds
	push	es
	lds	si,cs:indos_flag
	cmp	byte ptr [si],0
	jz	rdd_ret			; if not in dos
	lea	si,dos_save_buf
	push	cs
	pop	ds
	mov	di,DOS_SAVE_ST
	mov	cx,DOS_SEG
	mov	es,cx
	mov	cx,DOS_SAVE_LEN
	rep	movsb			; restore dos data
rdd_ret:
	pop	es
	pop	ds
	pop	di
	pop	si
	pop	cx
	ret
rest_dos_data	endp

break_off	proc	near
	push	ax
	push	dx
	mov	ah,Set_CTRL_C_Trapping
	mov	al,1			; set break
	mov	dl,0			; off
	int	21h
	pop	dx
	pop	ax
	ret
break_off	endp

set_pdp		proc	near
	push	ax
	push	bx
	mov	ah,Get_Current_PDB
	int	21h
	mov	cs:org_pdp,bx		; save original pdp addrs
	mov	bx,cs
	add	bx,KKC_PDP_SEG
	mov	ah,Set_Current_PDB
	int	21h
	pop	bx
	pop	ax
	ret
set_pdp		endp

rest_pdp	proc	near
	push	ax
	push	bx
	mov	ah,Set_Current_PDB
	mov	bx,cs:org_pdp		; original pdp addrs
	int	21h
	pop	bx
	pop	ax
	ret
rest_pdp	endp

set_int		proc	near
	cld
	push	ax
	push	cx
	push	si
	push	di
	push	ds
	push	es
	mov	ax,0			; int vector seg
	mov	ds,ax
	mov	ax,cs
	mov	es,ax
	mov	si,23h*4		; int 23h
	lea	di,org_int
	mov	cx,4
	rep	movsw			; save original int 23 & 24
	mov	ax,0
	mov	es,ax
	mov	di,23h*4		; int 23h
	lea	ax,int_23
	stosw				; set new int 23h
	mov	ax,cs
	stosw
	lea	ax,int_24
	stosw				; set new int 24h
	mov	ax,cs
	stosw
	pop	es
	pop	ds
	pop	di
	pop	si
	pop	cx
	pop	ax
	ret
set_int		endp

rest_int	proc	near
	cld
	push	ax
	push	cx
	push	si
	push	di
	push	ds
	push	es
	mov	ax,cs
	mov	ds,ax
	mov	ax,0			; int vector seg
	mov	es,ax
	lea	si,org_int
	mov	di,23h*4		; int 23h
	mov	cx,4
	rep	movsw			; restore original int 23 & 24
	pop	es
	pop	ds
	pop	di
	pop	si
	pop	cx
	pop	ax
	ret
rest_int	endp

;------------------------------------------------------------------------------

int_23		proc	far
	iret
int_23		endp

int_24		proc	far
	mov	cs:[dos_func_err],CRITICAL_ERR
	mov	cs:[crit_err_no],di
	xor	al,al
	iret
int_24		endp

;------------------------------------------------------------------------------

	db	16 - (($ - start) mod 16) dup (0)

KKC_PDP_SEG	=	($ - start) / 16

kkc_pdp		db	256 dup (0)

end_off		label	byte

code	ends

	end
