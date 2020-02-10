;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

include	bambi.inc

public	hook_ints
public	init_bambi_io

;
;	data from rdata.asm
;
extrn	indosaddr		:dword
extrn	in_08			:byte
extrn	in_09			:byte
extrn	in_10			:byte
extrn	in_13			:byte
extrn	in_16			:byte
extrn	int08chain		:dword
extrn	int10chain		:dword
extrn	int09chain		:dword
extrn	int13chain		:dword
extrn	int28chain		:dword
extrn	int16chain		:dword
extrn	int19chain		:dword
extrn	int2fchain		:dword
extrn	int15chain		:dword
extrn	int21chain		:dword
extrn	int25chain		:dword
extrn	int26chain		:dword
;
;	routines from hooks.asm
;
extrn	int08hook		:far
extrn	int09hook		:far
extrn	int10hook		:far
extrn	int13hook		:far
;extrn	int16hook		:far
extrn	int28hook		:far
extrn	int19hook		:far
extrn	int15hook		:far
extrn	int21hook		:far
extrn	int25hook		:far
extrn	int26hook		:far
;
;	routines from int2f.asm
;
extrn	int2fhook		:far

zseg    segment public 'CODE'

	assume  cs:zseg
	assume	ds:nothing



init_bambi_io proc near

	push	es
	mov	ah,34h	
	int	21h	;es:bx->indos
	mov	WORD PTR cs:indosaddr[0],bx
	mov	WORD PTR cs:indosaddr[2],es
	pop	es	
	clc
no_bambi:
	ret
init_bambi_io endp


hook_ints proc near
	push	es
	push	ds
	push	cs
	pop	ds
;;;	int 15 reboot vector

	mov	ax,3515h	
	int	21h
	
	mov	WORD PTR cs:int15chain,bx
	mov	WORD PTR cs:int15chain[2],es

	mov	dx,offset cs:int15hook
	mov	ax,2515h
	int	21h

;;;	int 19 reboot vector

	mov	ax,3519h	
	int	21h
	
	mov	WORD PTR cs:int19chain,bx
	mov	WORD PTR cs:int19chain[2],es

	mov	dx,offset cs:int19hook
	mov	ax,2519h
	int	21h

;;; int 10
	mov	ax,3510h	
	int	21h
	
	mov	WORD PTR cs:int10chain,bx
	mov	WORD PTR cs:int10chain[2],es

	mov	dx,offset cs:int10hook
	mov	ax,2510h
	int	21h

;;; int 9
	mov	ax,3509h	
	int	21h
	
	mov	WORD PTR cs:int09chain,bx
	mov	WORD PTR cs:int09chain[2],es

	mov	dx,offset cs:int09hook
	mov	ax,2509h
	int	21h

;;; int 13
	mov	ax,3513h	
	int	21h
	
	mov	WORD PTR cs:int13chain,bx
	mov	WORD PTR cs:int13chain[2],es

	mov	dx,offset cs:int13hook
	mov	ax,2513h
	int	21h

;;; int 2f
	mov	ax,352fh	
	int	21h
	
	mov	WORD PTR cs:int2fchain,bx
	mov	WORD PTR cs:int2fchain[2],es

	mov	dx,offset cs:int2fhook
	mov	ax,252fh
	int	21h

;;;	int 21 dos 

	mov	ax,3521h	
	int	21h
	
	mov	WORD PTR cs:int21chain,bx
	mov	WORD PTR cs:int21chain[2],es

	mov	dx,offset cs:int21hook
	mov	ax,2521h
	int	21h

;;;	int 25 absolute disk read 

	mov	ax,3525h	
	int	21h
	
	mov	WORD PTR cs:int25chain,bx
	mov	WORD PTR cs:int25chain[2],es

	mov	dx,offset cs:int25hook
	mov	ax,2525h
	int	21h

;;;	int 26 absolute disk write

	mov	ax,3526h
	int	21h
	
	mov	WORD PTR cs:int26chain,bx
	mov	WORD PTR cs:int26chain[2],es

	mov	dx,offset cs:int26hook
	mov	ax,2526h
	int	21h


;;;	int 28 idle 

	mov	ax,3528h	
	int	21h
	
	mov	WORD PTR cs:int28chain,bx
	mov	WORD PTR cs:int28chain[2],es

	mov	dx,offset cs:int28hook
	mov	ax,2528h
	int	21h

;;;	int 16 keyboard poll/etc

;	mov	ax,3516h	
;	int	21h
;	
;	mov	WORD PTR cs:int16chain,bx
;	mov	WORD PTR cs:int16chain[2],es
;
;	mov	dx,offset cs:int16hook
;	mov	ax,2516h
;	int	21h
;;;	hook timer last
	mov	ax,3508h	
	int	21h
	
	mov	WORD PTR cs:int08chain,bx
	mov	WORD PTR cs:int08chain[2],es

	mov	dx,offset cs:int08hook
	mov	ax,2508h
	int	21h

	pop	ds
	pop	es
	ret
hook_ints endp



zseg ends

end
