;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

include bambi.inc

public	call_bambi
public	initialize_int2f
extrn	accessing_swap_file_ptr	:dword
extrn	smart_win_data		:byte
extrn	warning_pop_up		:near
extrn	warning_pop_up_DOS	:near
extrn	find_startup_name	:near
extrn	startup_info		:byte
extrn	myPSP			:word
extrn	reference_ptr		:dword

zseg    segment public 'CODE'

	assume  cs:zseg
	assume	ds:nothing

;
; INPUT
;	AX = offset of bambi call
;
call_bambi proc near
	mov	bx,MULT_BAMBI
	xchg	ax,bx
	int	2fh
	ret
call_bambi endp

initialize_int2f proc near
;be sure swap file point is pointing to
;valid (0) byte!
	push	es
	mov	word ptr cs:accessing_swap_file_ptr[2],cs
	mov	word ptr cs:accessing_swap_file_ptr[0],offset cs:smart_win_data

	mov	word ptr cs:reference_ptr[0],offset cs:warning_pop_up_DOS
	mov	word ptr cs:reference_ptr[2],cs

	mov	word ptr cs:startup_info.SIS_Reference_Data[0],offset cs:reference_ptr
	mov	word ptr cs:startup_info.SIS_Reference_Data[2],cs

	mov	es,cs:myPSP	
	call	find_startup_name    ;ensure that global name ptr is accurate
	pop	es

	ret
initialize_int2f endp
zseg ends

end

