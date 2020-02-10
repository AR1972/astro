;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

?WIN = 0                ;Not windows;
?PLM = 1		;DO use pl/m
include cmacros.inc


; 5 byte packet for int 2f to print a file.
sBegin data
packet	label BYTE
	version			db	0 		; the first byte of the packet should have 0
	file_offset		dw	?		; offset of file name (to be printed)
	file_segment	dw	?		; segment of file name (to be printed)
sEnd data

sBegin code
    assumes cs, code
    assumes ds, DGROUP

;
; int pascal check_spooler() ;
;	returns 
;	        0 if print spooler installed.
;			1 if print spooler not installed  
;
cProc  check_spooler, PUBLIC,  <si,di,ds,es>
cBegin	check_spooler
	mov	ax, 0100h		; ah=01, al=00 Get printer installed state.
	int	2fh
	jc	short CS_Error	; function unsuccesful

	; function call was succesful
	; AL gives us the spooler state
	; = 00H if not installed, OK to install
	; = 01H if not installed, NOT OK to install
	; = FFH if installed

	xor ah, ah
	inc	al
	jz	CS_End

	; We get here if function unsuccesful or if spooler not installed.
	; al was not FFH at function return.
CS_Error:
	mov	ax, 1

CS_End:
	; ax is 0 at this point if we jumped to this label!!

cEnd  check_spooler

;
; int pascal a_print_file(char *name) ;
;	input: full path name of file to be printed.
;	returns 
;	        0 if file submitted for printing succesfully
;			1 if file could not be submitted.
;
cProc  a_print_file, PUBLIC,  <si,di,ds,es>
parmW string_segment
parmW string_offset
cBegin	a_print_file

	lea  dx, packet		; DS:DX = ptr to print packet

	; the 4 bytes after the first one in packet should have the 
	; segment:offset of the ASCIIZ filename.
	mov	ax, string_segment
	mov	file_segment, ax

	mov	ax, string_offset
	mov	file_offset, ax

	mov	ax, 0101h		; ah=01, al=01 File to be submitted for printing

	int	2fh

	; Carry set implies error and ax = error code.
	jc	err_exit

	xor	ax, ax		; Mark return status specifying file submitted succesfully

err_exit:

cEnd  a_print_file



sEnd   code


end
