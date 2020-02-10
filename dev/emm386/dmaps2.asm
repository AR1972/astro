.386p
page	58,132
;******************************************************************************
	title	DMAPS2
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1989-1991
;   (C) Copyright COMPAQ Computer Corp. 1989-1991
;
;   Title:    EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;   Module:   DMAPS2 - Routines to virtualize the PS2 DMA ports
;
;   Version:  1.0
;
;   Date:     Sep. 19,1989
;
;   Author:   Harish K. Naidu
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   09/19/89  Original
;
;******************************************************************************
;
;   Functional Description:
;
;
;******************************************************************************

	include vdmseg.inc
	include dma.inc
	include	emmdata.inc
	include emm386.inc

;******************************************************************************
;	P U B L I C S
;******************************************************************************

	public	DMA_FuncReg
	public	DMA_ExecFunc

;******************************************************************************
;	D E F I N E S
;******************************************************************************


;******************************************************************************
;	E X T E R N A L   R E F E R E N C E S
;******************************************************************************

	extrn	DMASinMsk1:near
	extrn	DMASinMsk2:near
	extrn	DMAMask1:near
	extrn	DMAMask2:near
	extrn	DMACheckTC:near

	extrn	DMARegSav:byte

;******************************************************************************
;	C O D E   S E G M E N T
;******************************************************************************


_TEXT	segment
	assume cs:_TEXT, ds:_DATA, es:_DATA, gs:R_CODE, ss:STACK

;############################################################################
;
;	Procedure name	: DMA_FuncReg
;
;	ENTRY		: Protected mode
;			  AL = byte to output to port
;			  DS = _DATA
;		 	  BX = port address
;			  DX =  0 => Emulate Input
;			  DX <> 0 => Emulate Output
;
;	EXIT		: if input AL = byte
;			  else output virtulaized
;
;	REGs MOD	:
;
;	written		: HKN 9/19/89
;
;	Description:
;
;############################################################################

DMA_FuncReg:

	jmp	short DMA_proc

FuncHandler	label word
	dw	OFFSET PS2_DMA_Set_IO_Adr	; 0
	dw	OFFSET PS2_DMA_Bad_Func		; 1
	dw	OFFSET PS2_DMA_Set_Mem_Adr	; 2
	dw	OFFSET PS2_DMA_Get_Mem_Adr	; 3
	dw	OFFSET PS2_DMA_Set_Count	; 4
	dw	OFFSET PS2_DMA_Get_Count	; 5
	dw	OFFSET PS2_DMA_Get_Status	; 6
	dw	OFFSET PS2_DMA_Set_Mode	  	; 7
	dw	OFFSET PS2_DMA_Set_Arbus	; 8
	dw	OFFSET PS2_DMA_Set_Chn_Mask	; 9
	dw	OFFSET PS2_DMA_Reset_Chn_Mask   ; A
	dw	OFFSET PS2_DMA_Bad_Func	  	; B
	dw	OFFSET PS2_DMA_Bad_Func	  	; C
	dw	OFFSET PS2_DMA_Master_Clear	; D
	dw	OFFSET PS2_DMA_Bad_Func	  	; E
	dw	OFFSET PS2_DMA_Bad_Func	  	; F

DMA_proc:

	push	eax
	push	di
	push	ebx
	xor	di,di

	call	DMACheckTC

ifdef	ROMIDMCA
	cmp	[ROMID],ROMIDISA	;Q: ISA/EISA system?
	je	short DFRNoEm		; Y: don't emulate this port?
endif
	test	gs:[GenFlags], fMCA	; Q: MCA system
	jz	short DFRNoEm		; N: don't emulate this port?

	or	dx,dx			;Q: Read?
	jz	short DFRNoEm		; Y: don't emulate

	mov	bl, al			; save original cmd byte
	and	eax, PS2_CHANNEL_MASK	; eax = channel #
	cmp	al, 4			; Q; 2nd controller
	jb	short first_ctl
	mov	di, 1			; 2nd controller index

first_ctl:
	mov	al, bl			; al = original cmd byte
	and	ebx, PS2_FUNCTION_MASK
	shr	ebx, 3			; make function # into offset into
					; the FuncHandler table
	call	word ptr cs:[FuncHandler+ebx]

	clc
	pop	ebx
	pop	di
	pop	eax
	ret

PS2_DMA_Bad_Func:
	
	; invalid function code. just let it go thru.

DFRNoEm:
	stc
	pop	ebx
	pop	di
	pop	eax
	ret


;############################################################################
;
;	Procedure name	: DMA_ExecFunc
;
;	ENTRY		: Protected mode
;			  AL = byte to output to port
;			  DS = _DATA
;		 	  BX = port address
;			  DX =  0 => Emulate Input
;			  DX <> 0 => Emulate Output
;
;	EXIT		: if input AL = byte
;			  else output virtulaized
;
;	REGs MOD	:
;
;	Description:
;
;############################################################################

DMA_ExecFunc:

	push	eax
	push	esi
	push	ebx
	push	edx
	push	edi
	push	ebp
	mov	ebp, esp

	call	DMACheckTC

ifdef	ROMIDMCA
	cmp	[ROMID],ROMIDISA	;Q: ISA/EISA system?
	je	DEFNoEm			; Y: don't emulate this port?
endif
	test	gs:[GenFlags], fMCA	; Q: MCA system
	jz	DEFNoEm			; N: don't emulate this port?

	lea	esi, [DMARegSav]	; ds:si points to DMA save area
	or	dx,dx			; Q: reading
	jnz	short PS2_Put_DMA_Byte	; N: goto  write

;
; Let the user read the port even though he has'nt set up for it.
;
;	cmp	[esi.DMA_writing], FALSE; Q: is it a read operation
;	jne	DEFNoEm			; N: bad read. attempting to read
;					;    port 1A while executing a write
					;    command.


	movzx	ebx, [esi.DMA_byteptr]
	mov	al, byte ptr [ebx+esi.DMA_data]
	mov	byte ptr [ebp+20], al	; put al on the stack.
	jmp	PS2_Inc_Byte_ptr



PS2_Put_DMA_Byte:
	cmp	[esi.DMA_writing], TRUE	; Q: is it a write operation
	jne	bad_write		; N: attempting to write to port 1A
					;    without properly programming the
					;    function register.

	movzx	edx, [esi.DMA_PS2_cmd]
	and	edx, PS2_FUNCTION_MASK
	cmp	edx, Set_Mem_Adr	; Q: is it a mem. addrd write
	je	PS2_New_Adr		; Y: go write a byte
	cmp	edx, Set_Count		; Q: is it a count write
	je	short PS2_New_Count	; Y: go write a byte
	cmp 	edx, Set_Mode	   	; Q: writing extended mode
	je	short PS2_New_Mode	; Y: go write a byte
	cmp	edx, Set_Arbus		; Q: writing Arbus
	je	PS2_Set_Arbus		; Y: go write Arbus
	cmp	edx,Set_IO_Adr		; Q: writing to set IO address
	jne	bad_write		; N: illegal command

PS2_IO_Addr:
	call	PS2_Get_data_Ptr	; edi-> channel info within dma save
					;       area
	movzx	ebx, [esi.DMA_bytePtr]	; obtain the virtual byte ptr
	out	1ah, al			; no need to virtulaize IO addr reg.
	jmp	PS2_Inc_Byte_Ptr

PS2_Set_Arbus:
	out	1ah, al
	jmp	PS2_write_done

PS2_New_Mode:
	call	PS2_Get_data_Ptr	; edi -> channel within dma save area

; translate the extended mode info into that which is required in
; DMAChnFlgs by the bufferred DMA code in dmatrap.asm

	and	[esi][edi].DMAChnFlgs,not (fReadOp+fWriteOp+fWordTx)
	test	al, Transfer_data	; Q: verify op?
	jz	short no_transfer	; Y:
	test	al, Write_Mem		; Q: write memory?
	jnz	short PS2_write		; Y:
	or	[esi][edi].DMAChnFlgs,freadop ; N:
	jmp	short PS2_read

PS2_write:
	or	[esi][edi].DMAChnFlgs,fwriteop

PS2_read:
	test	al, _16_bit_xfer	; Q: is this a 16 bit transfer
	jz	short byte_transfer	; N:
	or	[esi][edi].DMAChnFlgs,fWordTx

byte_transfer:
no_transfer:
					; indicate channel is programmed
					; using extended PI
	or	[esi+edi.DMAChnFlgs],fExtPI
	jmp	short PS2_prog_port



PS2_New_Count:
	call	PS2_Get_data_Ptr	; edi-> channel within dma save area
	movzx	ebx, [esi.DMA_bytePtr]	; obtain virtual byte ptr in ebx
	lea	edi, [esi+edi]
					; store count byte
	mov	byte ptr [ebx+edi.DMACount], al
	mov	byte ptr [ebx+esi.DMA_data], al
					; indicate channel is programmed
					; using extended PI
	or	[edi.DMAChnFlgs],fExtPI+fDirtyCount
	jmp	short PS2_Inc_Byte_Ptr


PS2_New_Adr:
	call	PS2_Get_data_Ptr	; edi-> channel info within dma save
					;       area
	movzx	ebx, [esi.DMA_bytePtr]	; obtain the virtual byte ptr

	lea	edi, [esi+edi]
					; store the appropriate byte
	mov	byte ptr [ebx+edi.DMALinAdr], al
	mov	byte ptr [ebx+esi.DMA_data], al
					; indicate channel is programmed
					; using extended PI
	or	[edi.DMAChnFlgs],fExtPI+fDirtyAddr

;
; Increment the bytr ptr for the controller

PS2_Inc_Byte_Ptr:
	inc	ebx
	cmp	bl, [esi.DMA_dataBytes]
	jb	short next_ok
	xor	ebx, ebx
next_ok:
	mov	[esi.DMA_bytePtr], bl
	jmp	short PS2_write_done


PS2_write_done:
	clc			; IO emulated
	pop	ebp
	pop	edi
	pop	edx
	pop	ebx
	pop	esi
	pop	eax
	ret

bad_write:

PS2_prog_port:
DEFNoEm:
	stc
	pop	ebp
	pop	edi
	pop	edx
	pop	ebx
	pop	esi
	pop	eax
	ret

;############################################################################
;
;	Procedure name	: PS2_DMA_Set_IO_Adr
;
;	ENTRY		: al = original cmd byte
;			: ebx = function # * 2
;			: di = controller index
;
;	EXIT		: DMARegSav structure updated with information
;
;	REGs MOD	: NONE
;
;############################################################################

PS2_DMA_Set_IO_Adr:

	push	esi

	lea	esi, [DMARegSav]	; ds:si DMA save area
	mov	[esi.DMA_PS2_cmd], al	; save cmd byte
	mov	[esi.DMA_bytePtr], 0	; reset internal byte ptr
	mov	[esi.DMA_databytes],2	; allow for writing 2 bytes
	mov	[esi.DMA_writing], TRUE
	out	18h,al

	pop	esi
	ret



;############################################################################
;
;	Procedure name	: PS2_DMA_Set_Mem_Adr
;
;	ENTRY		: al = original cmd byte
;			: ebx = function # * 2
;			: di = controller index
;
;	EXIT		: DMARegSav structure updated with information
;
;	REGs MOD	: NONE
;
;############################################################################

PS2_DMA_Set_Mem_Adr:

	push	esi

	lea	esi, [DMARegSav]	; ds:si DMA save area
	mov	[esi.DMA_PS2_cmd], al	; save cmd byte
	mov	[esi.DMA_bytePtr], 0	; reset internal byte ptr
	mov	[esi.DMA_databytes],3	; allow for writing 3 bytes
	mov	[esi.DMA_writing], TRUE

	pop	esi
	ret


;############################################################################
;
;	Procedure name	: PS2_DMA_Set_Count
;
;	ENTRY		: al = original cmd byte
;			: ebx = function # * 2
;			: di = controller index
;
;	EXIT		: DMARegSav structure updated with information
;
;	REGs MOD	: NONE
;
;############################################################################

PS2_DMA_Set_Count:

	push	esi

	lea	esi, [DMARegSav]	; ds:si DMA save area
	mov	[esi.DMA_PS2_cmd], al	; save cmd byte
	mov	[esi.DMA_bytePtr], 0	; reset internal byte ptr
	mov	[esi.DMA_databytes],2	; allow for writing 2 bytes
	mov	[esi.DMA_writing], TRUE

	pop	esi
	ret



;############################################################################
;
;	Procedure name	: PS2_DMA_Set_Mode & PS2_DMA_Set_Arbus:
;
;	ENTRY		: al = original cmd byte
;			: ebx = function # * 2
;			: di = controller index
;
;	EXIT		: DMARegSav structure updated with information
;
;	REGs MOD	: NONE
;
;############################################################################

PS2_DMA_Set_Arbus:
PS2_DMA_Set_Mode:

	push	esi

	lea	esi, [DMARegSav]	; ds:si DMA save area
	mov	[esi.DMA_PS2_cmd], al	; save cmd byte
	mov	[esi.DMA_bytePtr], 0	; reset internal byte ptr
	mov	[esi.DMA_databytes],1	; allow for writing 1 byte
	mov	[esi.DMA_writing], TRUE
	out	DMA_XFN,al		; actually do IO	;LEO

	pop	esi
	ret



;############################################################################
;
;	Procedure name	: PS2_DMA_Get_Mem_Adr
;
;	ENTRY		: al = original cmd byte
;			: ebx = function # * 2
;			: di = controller index
;
;	REGs MOD	: NONE
;
;	Description:
;		When port 18H is programmed for reading the adddr., the
;	appropriate addr is obtained and placed in the DMA save area
;	by calling PS2_Setup_Read. The next in to port 1AH will result
;	in the appropriate byte being read from the DMA save area (see
;	DMA_ExecFunc for reads).
;
;
;############################################################################

PS2_DMA_Get_Mem_Adr:

	push	esi
	push	eax
	push	edi
	push	ecx

	lea	esi, [DMARegSav]	; ds:si DMA save area
	and 	eax, PS2_CHANNEL_MASK
	mov	cx, SIZE DMARegREc
	mul	cx			; eax -> channel within dma
					; save area
	mov	edi,eax
	lea	edi, [esi+edi]
	mov	eax, [edi.DMALinAdr]
	mov	ecx, 3
	call	PS2_Setup_Read

	pop	ecx
	pop	edi
	pop	eax
	pop	esi
	ret


;############################################################################
;
;	Procedure name	: PS2_DMA_Get_Count
;
;	ENTRY		: al = original cmd byte
;			: ebx = function # * 2
;			: di = controller index
;
;	REGs MOD	: NONE
;
;	Description:
;		When port 18H is programmed for reading the count, the
;	appropriate count is obtained and placed in the DMA save area
;	by calling PS2_Setup_Read. The next in to port 1AH will result
;	in the appropriate byte being read from the DMA save area (see
;	DMA_ExecFunc for reads).
;
;
;############################################################################

PS2_DMA_Get_Count:

	push	esi
	push	edi
	push	ecx
	push	eax

	lea	esi, [DMARegSav]		; ds:si DMA save area
	and 	eax, PS2_CHANNEL_MASK
	mov	cx, SIZE DMARegRec
	mul	cx				; eax -> channel within dma
						; save area
	mov	edi,eax
	lea	edi, [esi+edi]

	test	[edi.DMAChnFlgs], fDirtyCount	; Q: count virtualized?
	jz	short get_phy_cnt		;   N: get physical

	mov	eax, [edi.DMACount]		;   Y: virtual count
ret_cnt:
	mov	ecx, 2
	call	PS2_Setup_Read

	pop	eax
	pop	ecx
	pop	edi
	pop	esi
	ret

get_phy_cnt:
	pop	eax			; recover orig cmd byte in al
	push	eax

	out	DMA_XFN, al		; issue get count cmd for channel
	jmp	$+2
	jmp	$+2

	xor	eax, eax		; clear high word of count

	in	al, DMA_EXE		; get low byte of count
	jmp	$+2
	jmp	$+2
	mov	ah, al			; save low byte in ah

	in	al, DMA_EXE		; get high byte of count
	jmp	$+2
	jmp	$+2
	xchg	ah, al			; ah = high, al = low
	jmp	short ret_cnt


;############################################################################
;
;	Procedure name	: PS2_DMA_Get_Status
;
;	ENTRY		: al = original cmd byte
;			: ebx = function # * 2
;			: di = controller index
;
;	REGs MOD	: NONE
;
;	Description:
;		When port 18H is programmed for reading the status, the
;	appropriate status is obtained and placed in the DMA save area
;	by calling PS2_Setup_Read. The next in to port 1AH will result
;	in the appropriate byte being read from the DMA save area (see
;	DMA_ExecFunc for reads).
;
;
;############################################################################

PS2_DMA_Get_Status:

	push	esi
	push	eax
	push	edi
	push	ecx

	lea	esi, [DMARegSav]		; ds:si DMA save area
						; save area

	xor	al, al
	xchg	al, [DMARegSav][di].DMAStatus	; status register must be
						; cleared after a read
	mov	ecx, 1
	call	PS2_Setup_Read

	pop	ecx
	pop	edi
	pop	eax
	pop	esi
	ret


;############################################################################
;
;	Procedure name	: PS2_DMA_Set_Chn_Mask
;
;	ENTRY		: al = original cmd byte
;			: ebx = function # * 2
;			: di = controller index
;
;
;	REGs MOD	: NONE
;
;############################################################################

PS2_DMA_Set_Chn_Mask:

	and	al, PS2_CHANNEL_MASK
	cmp	al, 4
	pushf				; save result of comparison
	and	al, 00000011b		; get channel # within controller
					; modulo 4
	or	al, 00000100b		; set bit in order to set mask
	popf				; restore result of original compare
	jb	short msk_cont1		; less than 4 then controller 1
	call	DMASinMsk2		; else controller 2
	jmp	short setmskexit

msk_cont1:
	call	DMASinMsk1

setmskexit:
	ret


;############################################################################
;
;	Procedure name	: PS2_DMA_Reset_Chn_Mask
;
;	ENTRY		: al = original cmd byte
;			: ebx = function # * 2
;			: di = controller index
;
;
;	REGs MOD	: NONE
;
;############################################################################

PS2_DMA_Reset_Chn_Mask:

	and	al, PS2_CHANNEL_MASK
	cmp	al, 4
	pushf				; save result of comparison
	and	al, 00000011b		; get channel # within controller
					; modulo 4 and also reset bit 2 for
					; unmask operation.
	popf				; restore result of original compare
	jb	short unmsk_cont1	; less than 4 then controller 1
	call	DMASinMsk2		; else controller 2
	jmp	short resetmskexit

unmsk_cont1:
	call	DMASinMsk1

resetmskexit:
	ret




;############################################################################
;
;	Procedure name	: PS2_DMA_Master_Clear
;
;	ENTRY		: al = original cmd byte
;			: ebx = function # * 2
;			: di = controller index
;
;
;	REGs MOD	: NONE
;
;############################################################################

PS2_DMA_Master_Clear:

	push	esi
	push	eax

	lea	esi, [DMARegSav]
	xor	eax, eax
	mov	[esi.DMAStatus][0], al
	mov	[esi.DMAStatus][1], al
	mov	[esi.DMAFF][0], al
	mov	[esi.DMAFF][1], al
	mov	[esi.DMA_PS2_cmd], al
	mov	[esi.DMA_writing], al
	mov	[esi.DMA_bytePtr], al
	mov	[esi.DMA_dataBytes], al
	mov	[esi.DMA_data], eax

	mov	al, 0fh
	call	DMAMask1

	mov	al, 0fh
	call	DMAMask2

	pop	eax
	pop	esi

	ret




;############################################################################
;
;	Procedure name	: PS2_Get_data_ptr
;
;	ENTRY		: esi = dma save area
;
;	EXIT		: edi -> the appropriate channel data within dma
;			         save area
;
;
;############################################################################

PS2_Get_data_Ptr	proc	near

	push	eax
	movzx	eax, [esi.DMA_PS2_cmd]
	and 	al, PS2_CHANNEL_MASK

	mov	di, SIZE DMARegRec
	mul	di
	mov	edi, eax

	pop	eax

	ret

PS2_Get_data_Ptr	endp


;############################################################################
;
;	Procedure name	: PS2_Setup_Read
;
;	ENTRY		: EAX is data (1, 2 or 3 bytes)
;			: ECX is # of bytes
;			: ESI -> dma save area
;
;	REGs MOD	: NONE
;
;############################################################################

PS2_Setup_Read	proc	near

	mov	[esi.DMA_writing], FALSE
	mov	[esi.DMA_byteptr], 0
	mov	[esi.DMA_databytes], cl
	mov	[esi.DMA_data], eax
	ret

PS2_Setup_Read	endp

_TEXT	ends

	end

