;	TITLE	RWDOT - Read and write dot for MSHERC.
;***
;RWDOT
;
;	Copyright <C> 1987, 1988, Microsoft Corporation
;
;Purpose:
;	Read and write individual pixels.
;
;******************************************************************************

	include	hgcdefs.inc

code            segment para public 'code'
                assume  cs:code,ds:code

Public	ReadDot		;Read the Graphics Mode Pixel Value
Public	WriteDot	;Write the Graphics Mode Pixel

Extrn	MapGraphXYToVideoOffset:near ;Map Graphics coords to a video offset
Extrn	Bit0:byte		;Bit Mask Table

;-------------------------------------------------------------------------------
; C  WRITE DOT
;	 BH => Display Page
;	 DX => Row
;	 CX => Column
;	 AL => Color, bit 7 causes use of XOR operation for write
;-------------------------------------------------------------------------------
WriteDot	proc	near
	mov	di,Scr0_Buf_Seg 		;Assume Graphics Page 0
	test	bh,1				;Graphics Page 1 on?
	jz	SetBufSeg			;No, point es to page 0
	mov	di,Scr1_Buf_Seg 		;Yes, point es to page 1
SetBufSeg:
	mov	es,di				;Point es to graphics segment

;------ Calculate the video buffer offset -----
	Call	MapGraphXYToVideoOffset		;Result in cx and dx
	mov	di,cx				;di = ptr to video offset
	mov	bx,dx				;Save bit position in bx
	mov	bl,Bit0[bx]			;Fetch bit mask

	or	al,al			;Clear the pixel?
	jnz	SetPix			;No, set the pixel

	not	bl			;Invert the bit mask
	and	es:[di],bl		;Clear the pixel
	jmp	FiniWriteDot		;Exit procedure

SetPix:
	test	al,80H			;use XOR?
	jz	UseOr			;go if not
	xor	es:[di],bl		;XOR the pixel
	jmp	FiniWriteDot		;Exit procedure
UseOr:
	or	es:[di],bl		;Set the pixel

FiniWriteDot:
	Ret	;Finished the Write Dot Procedure
WriteDot	endp

;-------------------------------------------------------------------------------
; D  READ DOT
;	 BH => Display Page
;	 DX => Row
;	 CX => Column
;	 AL <= Color
;-------------------------------------------------------------------------------
ReadDot	proc	near
	mov	di,Scr0_Buf_Seg 		;Assume Graphics Page 0
	test	bh,1				;Graphics Page 1 on?
	jz	RSetBufSeg			;No, point es to page 0
	mov	di,Scr1_Buf_Seg 		;Yes, point es to page 1
RSetBufSeg:
	mov	es,di				;Point es to graphics segment

;------ Calculate the video buffer offset -----
	Call	MapGraphXYToVideoOffset		;Result in cx and dx
	mov	di,cx				;di = ptr to video offset
	mov	bx,dx				;Save bit position in bx
	mov	bl,Bit0[bx]			;Fetch bit mask
	mov	al,es:[di]	;Read the video byte
	and	al,bl		;Isolate the desired pixel
	neg	al		;set carry iff non-0
	sbb	al,al		;AL = (AL==0) ? 0 : -1
	neg	al		;AL = (AL==0) ? 0 :  1
	mov	byte ptr FunData.FunAX,al	;Save pixel color
	Ret	;Finished Pixel Color Read Procedure
ReadDot	endp

code	ends
	end
