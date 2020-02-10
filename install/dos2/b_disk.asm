	page	,132
	title	b_disk
;***
;b_disk.asm - contains the _bios_disk() function
;
;	Copyright (c) 1987-1990, Microsoft Corporation. All rights reserved.
;
;Purpose:
;	This module contains the _bios_disk() function.
;
;*******************************************************************************

include model.inc
include dpmi.inc

   .286
	.xlist
	.list

extrn lpInt13Buf:DWORD
extrn lpfnInt13Func:DWORD

diskinfo_t struc
_drive	dw	?		; 0-3 or 0x80-0x81
_head	dw	?		; 0-?
_track	dw	?		; 0-1023
_sector dw	?		; 0-16
_numsec dw	?		; 0-128
_buffer dd	?
diskinfo_t ends

.data

RealModeCallFrame	RealModeCallStruc	<>

.CODE

page
;***
;unsigned _bios_disk(service, p_diskinfo) - BIOS disk services
;
;Purpose:
;	The function "_bios_disk" allows direct access to the BIOS
;	services to access floppy diskettes and hard disks.
;	These BIOS services are called via INT 13h.
;
;
;Entry:
;	unsigned service - the diskette/disk service requested
;	struct diskinfo_t {
;		unsigned drive;
;		unsigned head;
;		unsigned track;
;		unsigned sector;
;		unsigned nsectors;
;		void far *buffer;
;	} *p_diskinfo	-	contains diskette/disk parameters
;
;Exit:
;	AL = disk status byte (0 means success, otherwise error)
;
;Uses:
;	BX, CX, DX
;
;Exceptions:
;
;*******************************************************************************


_bios_disk	proc  uses ds si di, service:word, p_diskinfo:word

	mov	ah, byte ptr service
	mov 	bx, p_diskinfo

   cmp        ah, 5
   je         special_format_code

	mov	dl,byte ptr [bx._drive]
	cmp	ah,2
	jb	do_int13	; do not load other regs for services 0 and 1

	mov	dh,byte ptr [bx._head]
	mov	cx,[bx._track]
	xchg	ch,cl
	ror	cl,1		   ; bits 8 and 9 of track are stored
	ror	cl,1		   ; in the high two bits of CL
	and	cl,0C0H 	              ; clear off unused lower bits
	or	cl,byte ptr [bx._sector]
	mov	al,byte ptr [bx._numsec]
	les	bx,[bx._buffer]

do_int13:
	int	13H		   ; request diskette/disk service

	ret

special_format_code:
;
; We have some special code here that will handle int 13h func 5 (format track.)
; This code is necessary to work around a bug in some older Compaq BIOS's. The bug
; will crop up if you have emm386 loaded and you use int 13 to format a floppy disk
; track from standard mode. (pmode).
;
; The fix for this is to execute the int 13 in real mode via the DPMI provided
; "Call Real Mode Procedure with Far return". DOS 1 CUI has left us a bit of code in
; his resident stub that will to the int 13h followed by a far ret. We get the seg:off
; of the int 13h stub in DOS 1 CUI via the vinfo structure.

;
; Convert real mode segment into protected mode selector
;

   push  bx
   mov   ax,SEG_TO_DESC           ; Function to convert segment to descriptor.
   mov   bx,word ptr lpInt13Buf+2 ; bx = real mode segment.
   int   DPMI                	  ; ax = protect mode selector.
   pop   bx
   jc    err_no_mem

   ;
   ; Now, copy _buffer into the real mode addressable memory
   ;
   mov	es,ax
   mov	di,word ptr lpInt13Buf	; es:di -> destination buffer
   push  ds
   lds   si,[bx._buffer]        ; Source pointer loaded into ds:si
   mov   cx,256d                ; Move 256 words (512 bytes.)
   cld                          ; Assure that we increment si,di
   rep   movsw
   pop   ds
   ;
   ; Now, we need to prepare the real mode call data structure so that we can
   ; ask DPMI to call our real mode int 13h stub.
   ;
	lea   di, [RealModeCallFrame]
	mov   byte ptr [di].RegEAX+1, 05        ; ah = 5  Int 13h / func 5
   mov   al, byte ptr [bx._numsec]
	mov   byte ptr [di].RegEAX,al           ; al = Num sectors.
   mov   al, byte ptr [bx._head]
   mov   byte ptr [di].RegEDX+1, al        ; dh = Head number.
   mov   al, byte ptr [bx._drive]
   mov   byte ptr [di].RegEDX, al          ; dl = Drive number.

   mov   ax, word ptr lpInt13Buf
   mov   word ptr [di].RegEBX, ax
   mov   ax, word ptr lpInt13Buf+2
   mov   [di].RegES, AX                    ; es:bx = ptr to real mode buffer

	mov   cx,[bx._track]
	xchg  ch,cl
	ror   cl,1		                  ; bits 8 and 9 of track are stored
	ror   cl,1	           	       ; in the high two bits of CL
	and   cl,0C0H 	                  ; clear off unused lower bits
   mov   word ptr [di].RegECX, cx          ; cx = Track.
   mov   [di].RegSS,0                      ; ss:sp 0:0 to force DPMI to
   mov   [di].RegSP,0                      ; give us a 30 word stack.
	pushf
	pop   ax                                ; flags.
	mov   [di].RegFlg,ax
   mov   ax, word ptr lpfnInt13Func
   mov   [di].RegIP, ax                    ; cs:ip = pointer to real mode
   mov   ax, word ptr lpfnInt13Func+2
   mov   [di].RegCS, AX                    ; call address, seg:off
	;
	; Call real mode procedure with far return.
	;
	mov   ax, ds
	mov   es, ax	      ; es:di = RealModeCallStruc
   mov   bh, 0                 ; should be zero to avoid A20 state change.
   xor   cx,cx                 ; Number or words to copy from pmode to real mode stack.
   mov   ax,CALL_REALMODE_PROC ; DPMI Func 301h
	int   DPMI

   ;
   ; Lastly, we get the real mode int 13h AX and flag returns from the
   ; RealModeCallFrame so we can determine the outcome of our int 13h call.
   ;
	lea   di, [RealModeCallFrame]
   mov   ax, word ptr [di].RegEAX ; Get AX into ax.
   ret

err_no_mem:
   mov   ax,-1
   ret

_bios_disk	endp
	end

