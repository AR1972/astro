;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */

ifdef JAPAN	; ### if JAPAN ###

;----------------------------------------------------------------------------;
; This module contains routines realated to saving and restoring of the KKC  ;
; (Kana-Kanji Converter) context.    					     ;
;									     ;
; History:								     ;
;									     ;
;        Mon Feb-18-1991.  	-by-  Norihiko Maruko [norim]		     ;
;        Created for DOS Shell. (Added the History legend) 		     ;
;----------------------------------------------------------------------------;

	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	include	njmp.mac
	include macros.mac
	.list

	.8086

	public	KkcBufferSeg
	public	KkcBufferSize
	public	KkcDataSize

sBegin	StubSeg

;----------------------------------------------------------------------------;
; define the external function calls.		          		     ;
;----------------------------------------------------------------------------;

KkcBufferSize		dw	0	; size of a buffer to save/restore
KkcBufferSeg		dw	0	; segment of a buffer to save/restore

	assumes	cs,StubSeg

;----------------------------------------------------------------------------;
; KkcBusyCheck:							             ;
;								             ;
; Busy check to save/restore KKC state                                       ;
;									     ;
; OUT:									     ;
;	CF - 0 Not Busy							     ;
;	     1 Busy							     ;
;----------------------------------------------------------------------------;

cProc	KkcBusyCheck,<FAR,PUBLIC,PASCAL>,<ax,bx,dx,es>

cBegin

	mov	ax,_DATA		;
	mov	es,ax			;
	mov	bx,es:KkcHandleNo	;
	mov	ax,BusyCheck		;
	xor	dx,dx			;
	int	2fh			; busy check
	or	ax,ax			; not support function 4eh
	jnz	@f			; yes
	or	dx,dx			; KKC busy ?
	jnz	CheckError		; yes
@@:
	clc				; not busy
	jmp	short CheckEnd		;
CheckError:
	stc				; busy
CheckEnd:

cEnd
;---------------------------------------------------------------------------
sEnd

sBegin	Data

;----------------------------------------------------------------------------;
; define the KKC state related variables.				     ;
;----------------------------------------------------------------------------;

KkcHandleNo		dw	?	; KKC handle No.
KkcDataSize		dd	0	; data size of KKC

sEnd	Data

sBegin	Code

	assumes	cs,Code
	assumes	ds,Data

	;-------------------------------------------------------;
	; external OLDAPP procedures.			        ;
	;-------------------------------------------------------;


		externNP	ReadFile		;(WOAFILE.ASM)
		externNP	WriteFile		;(WOAFILE.ASM)
		externNP	LseekFile		;(WOAFILE.ASM)

;----------------------------------------------------------------------------;
; declare the variables which are defined elsewhere.			     ;
;----------------------------------------------------------------------------;


;----------------------------------------------------------------------------;
; define constants related to various KKC.				     ;
;----------------------------------------------------------------------------;

; Int 2fh function
GetVersionTable		equ	4d03h	; get address of a version table
DisableEnable		equ	4e00h	; disable/enable KKC
BusyCheck		equ	4e01h	; check busy for KKC
GetSize			equ	4e02h	; get size of KKC state
SaveKKC			equ	4e03h	; save KKC state
RestoreKKC		equ	4e04h	; restore KKC state

MaxKkcCount		equ	1	; max KKC count
DefaultHandleNo		equ	1	; default handle no.
KkcBufSize		equ	2048	; buffer's size to save/restore
FileAttribute		equ	0	; file attribute to create
AccessMode		equ	2	; file open mode

;----------------------------------------------------------------------------;

;----------------------------------------------------------------------------;
; EnableKkc:								     ;
;									     ;
; This routine enables the KKC.		  				     ;
;									     ;
;----------------------------------------------------------------------------;

cProc	EnableKkc,<NEAR,PUBLIC,PASCAL>,<ax,bx,dx>

cBegin

	mov	ax,DisableEnable	;
	mov	bx,KkcHandleNo		;
	mov	dl,1			; set enable request
	int	2fh			; set KKC enable
	or	ax,ax			; not support function 4dh ?
	jnz	EnableError		; yes
	clc				;
	jmp	short @f		;
EnableError:
	stc				;
@@:

cEnd
;----------------------------------------------------------------------------;
; DisableKkc:								     ;
;									     ;
; This routine disables the KKC. 					     ;
;----------------------------------------------------------------------------;

cProc	DisableKkc,<NEAR,PUBLIC,PASCAL>,<ax,bx,dx>

cBegin

	mov	ax,DisableEnable	;
	mov	bx,KkcHandleNo		;
	mov	dl,0			; set disable request
	int	2fh			; set KKC disable
	or	ax,ax			; not support function 4dh ?
	jnz	DisableError		; yes
	clc				;
	jmp	short @f		;
DisableError:
	stc				;
@@:

cEnd
;----------------------------------------------------------------------------;
; SaveKkcState:								     ;
; Save the state of the KKC			       			     ;
;----------------------------------------------------------------------------;

cProc	SaveKkcState, <NEAR,PUBLIC,PASCAL>, <ax,bx,cx,dx,di,ds,es>

	parmW	FileHandle
	localD	FileOffset
	localW	DataEmpty
cBegin	     

	xor	ax,ax			;
	mov	bx,2			;
	cCall	LseekFile,<FileHandle,ax,ax,bx> ; seek end of file
	jc	SaveErrorEnd		; seek error
	mov	seg_FileOffset,dx	; save file offset
	mov	off_FileOffset,ax	;

	lea	ax,KkcDataSize		; write total size of KKC data
	mov	cx,4			; need to write 4 bytes
	xor	dx,dx			;
	cCall	WriteFile,<FileHandle,ds,ax,dx,cx>
	jc	SaveErrorEnd		; write error

	mov	ax,_WOARLMSEG		; get StubSeg
	mov	es,ax			;
	mov	ax,es:KkcBufferSeg	; get a segment address to save
	mov	es,ax			;
SaveLoop:
	mov	ax,SaveKkc		; get KKC save function
	mov	bx,KkcHandleNo		; get KKC handle No.
	mov	cx,KkcBufSize		; get size of the saving buffer
	xor	di,di			;
	push	es			; save
	int	2fh			; get KKC state
	pop	es			; restore
	cmp	ax,SaveKkc		; not support this function ?
	jz	SaveErrorEnd		; yes
	mov	DataEmpty,ax		;

	xor	di,di			;
	xor	ax,ax			;
	push	es			; save
	cCall	WriteFile,<FileHandle,es,di,ax,cx>
	pop	es			; restore
	jc	SaveErrorEnd		; write wrror

	cmp	DataEmpty,0		; data empty ?
	jnz	SaveLoop		; no

WriteOffset:
	xor	ax,ax			;
	mov	bx,4			;
	cCall	LseekFile,<FileHandle,ax,bx,ax>
	jc	SaveErrorEnd		; seek error
	lea	ax,FileOffset		; ss:ax points to data to write
	mov	cx,4			; need to write 4 bytes
	xor	dx,dx			;
	cCall	WriteFile,<FileHandle,ss,ax,dx,cx>
	jnc	SaveEnd			; no error

SaveErrorEnd:
	stc				; set error flag

SaveEnd:

cEnd
;----------------------------------------------------------------------------;
; RestoreKkcState:							     ;
;									     ;
; Restores the state of the KKC.					     ;
;----------------------------------------------------------------------------;

cProc	RestoreKkcState, <NEAR,PUBLIC,PASCAL>, <ax,bx,cx,dx,di,ds,es>

	parmW	FileHandle
	localD	FileOffset
	localD	EndPointer
	localD	ReadSize
	localW	RestoreCount
cBegin

	xor	ax,ax			;
	mov	bx,4			;
	cCall	LseekFile,<FileHandle,ax,bx,ax>
	njc	RestoreErrorEnd		; seek error
	lea	ax,FileOffset		; ss:ax points to data to read
	mov	cx,4			; need to read 4 bytes
	xor	dx,dx			;
	cCall	ReadFile,<FileHandle,ss,ax,dx,cx>
	jc	RestoreErrorEnd		; read error

	xor	ax,ax			;
	cCall	LseekFile,<FileHandle,FileOffset,ax>
	jc	RestoreErrorEnd		; seek error

	lea	ax,ReadSize		; read total size of KKC's data
	mov	cx,4			; need to read 4 bytes
	xor	dx,dx			;
	cCall	ReadFile,<FileHandle,ss,ax,dx,cx>
	jc	RestoreErrorEnd		; read error

	mov	ax,_WOARLMSEG		; get StubSeg
	mov	es,ax			;
	mov	ax,es:KkcBufferSeg	; get buffer address to restore
	mov	es,ax			;

RestoreLoop:
	mov	dx,KkcBufSize		;
	cmp	seg_ReadSize,0		; more than 64KB ?
	jnz	@f			; yes
	cmp	off_ReadSize,dx		; reading file size > buffer size ?
	jae	@f			; yes
	mov	dx,off_ReadSize		; get a size to read
@@:
	mov	RestoreCount,dx		; save a counter
	xor	di,di			;
	xor	cx,cx			; get read counter
	push	es			; save
	cCall	ReadFile,<FileHandle,es,di,cx,dx>
	pop	es			; restore
	jc	RestoreErrorEnd		; read error

	mov	ax,cx			; get counter
	mov	ax,RestoreKkc		; get KKC function to restore
	mov	bx,KkcHandleNo		; get KKC handle No.
	xor	di,di			;
	push	es			; save
	int	2fh			; restore KKC state
	pop	es			; restore
	or	ax,ax			; not support this function ?
	jnz	RestoreErrorEnd		; yes

	mov	ax,RestoreCount		;
	sub	off_ReadSize,ax		; renew count to read
	sbb	seg_ReadSize,0		;
	mov	ax,off_ReadSize		;
	or	ax,seg_ReadSize		; finish to restore ?
	jnz	RestoreLoop		; no
	clc				;
	jmp	short RestoreEnd	;

RestoreErrorEnd:
	stc				; set error flag

RestoreEnd:

cEnd
;----------------------------------------------------------------------------;
; GetKkcStateSize:							     ;
;								             ;
; Gets the size of the buffer needed to save the KKC state if there is a     ;
; KKC installed.						             ;
;									     ;
;----------------------------------------------------------------------------;

cProc	GetKkcStateSize,<NEAR,PUBLIC,PASCAL>,<ax,bx,cx,es>


cBegin

	xor	ax,ax			;
	mov	bx,_WOARLMSEG		; get StubSeg address
	mov	es,bx			;
	mov	es:KkcBufferSize,ax	; initialize
	mov	wptr KkcDataSize,ax	; initialize
	mov	wptr KkcDataSize+2,ax	;

	cCall	GetKkcHandleNo		; get KKC handle No.
	jc	GetSizeError		; not installed
	mov	ax,KkcBufSize/16	; set buffer size
	mov	es:KkcBufferSize,ax	; need buffer size

	mov	ax,GetSize		;
	mov	bx,KkcHandleNo		;
	int	2fh			; get total data size of KKC
	or	ax,ax			;
	jnz	GetSizeError		;
	mov	wptr KkcDataSize+2,cx	; set total size
	mov	wptr KkcDataSize,dx	;
	clc
GetSizeError:
	stc				;
GetEnd:
cEnd
;----------------------------------------------------------------------------;
; GetKkcHandleNo:							     ;
;									     ;
; This routine gets the handle number of KKC.				     ;
;----------------------------------------------------------------------------;

cProc	GetKkcHandleNo,<NEAR,PUBLIC,PASCAL>,<ax,bx,cx,dx,ds,es>


cBegin

	mov	ax,GetVersionTable	;
	xor	cx,cx			;
	xor	dx,dx			;
	int	2fh			; get an address of the version table
	or	ax,ax			; no get ?
	jnz	GetHandleError		; yes

	mov	ds,cx			;
	mov	bx,dx			;
	mov	ax,ds:[bx]		; get a counter installing KKC
	or	ax,ax			; KKC installed ?
	jz	GetHandleError		; no
	mov	ax,ds:[bx+2]		; get a version No. of KKC
	or	ax,ax			; not installed ?
	jz	GetHandleError		; yes
	mov	es:KkcHandleNo,DefaultHandleNo
	clc
	jmp	short @f		;
GetHandleError:
	stc				;
@@:
cEnd
;----------------------------------------------------------------------------;
sEnd	Code

endif		; ### end if JAPAN ###

end
