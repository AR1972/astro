;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;
; Initialize routine for EXE high-loading
;
;	Created by yoshim	May 27, 1987
;

;	for DOS 4.01 by yukini	Feb 20, 1989

PDB_BLOCK_LEN	equ	02h
PDB_PARENT_PID	equ	16h
PDP_ENVIRON	equ	2Ch


prog		group	code, data

code		segment byte public 'code'
code		ends
data		segment para public 'data'
data		ends

code		segment byte public 'code'
	assume	cs:code, ds:nothing, es:nothing, ss:nothing

iSP		dw	0
iSS		dw	0
iIP		dw	0
iCS		dw	0
rel		dw	0

stack		db	(128-10) dup(0)

		; cs points code segment
		; ds and es point psp segment

		mov	ax, cs			; get my code segment
		mov	bx, cs:[rel]
		sub	bx, 16
		add	ax, bx			; psp area(256 bytes)
		mov	ds, ax			; set new psp to DS
	
		mov	si, word ptr es:[PDB_BLOCK_LEN] ; set memory size
		mov	dx, ds			; set new psp
		mov	ah, 055h		; dup psp
		int	21h

		mov	ax, word ptr es:[PDB_PARENT_PID] ; copy parent pid
		mov	word ptr ds:[PDB_PARENT_PID], ax

		mov	bx, es			; go back old psp
		mov	ah, 50h			; set psp
		int	21h

		mov	cx, 4
close_loop:	mov	bx, cx
		mov	ah, 3Eh			; close
		int	21h
		loop	close_loop

		mov	bx, ds			; go to new psp
		mov	ah, 50h			; set psp
		int	21h

		mov	bx, ds			; bx = newpsp - oldpsp - 1
		mov	ax, es
		sub	bx, ax
		dec	bx
		mov	ah, 04Ah		; modify memory
		int	21h	

		push	es
		mov	ax, ds
		mov	es, ax
		mov	bx, 0FFFFh
		mov	ah, 04Ah		; modify memory. but, error!
		int	21h			; return possible size in BX
		mov	ah, 04Ah		; modify memory
		int	21h
		pop	es
; ------------------------------------------------<MSKK01>----------------------
		push	ds			;
		push	es			;

		mov	ax,es			;
		dec	ax			; points OLD PSP mem arena block
		mov	bx,ds			;
		dec	bx			; points NEW PSP mem arena block
		mov	si,8			;
		mov	di,si			;
		mov	cx,8			; transfer arena name to new one
		cld				;
		mov	ds,ax			;
		mov	es,bx			;
		rep	movsb			;
		pop	es			;
		pop	ds			;
; ------------------------------------------------<MSKK01>----------------------

		; free old psp
		; es already point old psp
		mov	ah, 049H		; free memory
		int	21h

		mov	ax, es:[PDP_ENVIRON]
		dec	ax
		mov	es, ax
		mov	es:[1], ds
	
		mov	ax, cs
		add	ax, cs:[rel]		; relocation factor to AX
;
;
; At this point, we are done loading, and it is time to pass
; control to the the expanded program.  Thus, we must set SS:SP
; appropriately, make ES and DS point at the PSP, and jump to
; the proper location.
;
		mov	di, cs:[iSP]		; Get initial SP value
		mov	si, cs:[iSS]		; Get initial SS value
		add	si, ax			; Relocate
		add	cs:[iCS], ax		; Relocate initial CS
		sub	ax, 10H			; Subtract PSP increment
		mov	ds, ax			; DS points to PSP
		mov	es, ax			; ES points to PSP
		mov	bx, offset prog:iIP	; Get offset of jump vector
		cli				; Disable interrupts
		mov	ss, si			; Initialize SS
		mov	sp, di			; Initialize SP
		sti				; Enable interrupts
		jmp	dword ptr cs:[bx]	; Jump to starting point

code		ends

data		segment
newmem		db	16 dup(0)
newpsp		db	256 dup(0)
data		ends
end
