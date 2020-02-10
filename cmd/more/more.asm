;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;******************************************************************************
;
; MODULE:   more.asm
;
; Modification History:
;
;  Version    Author	       date	   comment
;  -------    ------	       ----	   -------
;  V4.0       RussW			   ;AN000; initial extended attr. support
;
;  V4.0       Bill L	      9/17/87	   ;AN001; DCR 201 - extended attr. enhancement
;					   ;AN002; DCR 191
;					   ;AN003; PTM 3860 - add CR-LF to make DOS3.3 compat.
;******************************************************************************

FALSE	EQU	0
TRUE	EQU	NOT FALSE

IBMJAPVER   EQU FALSE

STDOUT	EQU	1			 ;AN003;

BREAK	MACRO	subtitle
	SUBTTL	subtitle
	PAGE
ENDM

	include	version.inc
	INCLUDE SYSCALL.INC

	INCLUDE MORE.INC		 ;MORE strucs and equates
	.XLIST				 ;
	INCLUDE STRUC.INC		 ;Structured macros
	INCLUDE SYSMSG.INC		 ;Message retriever code
	include rombios.inc		 ;ROM BIOS data definition
	.LIST				 ;

MSG_UTILNAME <MORE>			 ;

CODE	SEGMENT PUBLIC
	ORG	100H
ASSUME	CS:CODE,DS:CODE,ES:CODE,SS:CODE


START:
	CALL	SYSLOADMSG
	jnc	chk_opt
	CALL	SYSDISPMSG
	MOV	AH,EXIT
	INT	21H

	; Check for /? on the command line.
chk_opt:
	CALL	CHECK_OPTIONS
	jnc	get_dev_char
	MOV	AH,EXIT
	INT	21H

	; get number of lines on the screen
get_dev_char:

ifdef JAPAN

	mov	MAXROW,LINESPERPAGE 	; assume 24 rows
	mov	MAXCOL,LINELENGTH	; assume 80 columns

else

	push	ds
	mov	ax,ROMBIOS_DATA 	  ;Get ROM Data segment
	mov	ds,ax
	Assume	DS:ROMBIOS_DATA

	mov	al,CRT_Rows		  ;Get max rows
	pop	ds
	Assume	DS:Code

	or	al,al			  ;If zero specified
	jnz	@F
	mov	al,LINESPERPAGE 	  ;assume 24 rows

@@:
	mov	maxrow,al		  ;set lines per page from ROM BIOS
endif

;
; Check if ANSI is loaded, and if so, use it to find screen dimensions
;

	MOV	AX,ANSI_GET		  ;prepare for device characteristics..
	MOV	BX,STDERR		  ;request.
	MOV	CX,GET_SUBFUNC		  ;get subfunction..
	LEA	DX,ANSI_BUF		  ;point to buffer.
	INT	21H
	jc	no_ansi

;
; ANSI is around and get tell us the screen dimensions
;

	LEA	DI,ANSI_BUF
	cmp	[di].d_mode, TEXT_MODE	  ;if we are in a text mode then..
	jne	no_ansi 		  ;default already initialized
	MOV	AX,[DI].SCR_ROWS	  ;store the screen length...else..
	MOV	MAXROW,AL		  ;default (25) is assumed.

ifdef JAPAN
	mov	ax,[di].SCR_COLS
	mov	MAXCOL,al
no_ansi:

else

no_ansi:
	MOV	AH,0FH
	INT	10H
	MOV	MAXCOL,AH

endif

	XOR	BX,BX			; DUP FILE HANDLE 0
	MOV	AH,XDUP
	INT	21H
	MOV	BP,AX			; Place new handle in BP

	MOV	AH,CLOSE		; CLOSE STANDARD IN
	INT	21H

	MOV	BX,2			; DUP STD ERR TO STANDARD IN
	MOV	AH,XDUP
	INT	21H

	MOV	CX,CRLF_LEN		;AN003; ;display a newline
	MOV	DX,OFFSET CRLF		;AN003;
	MOV	BX,STDOUT		;AN003;
	MOV	AH,WRITE		;AN003;
	INT	21H			;AN003;
ALOOP:
	CLD
	MOV	DX,OFFSET BUFFER
	MOV	CX,4096
	MOV	BX,BP
	MOV	AH,READ
	INT	21H
	OR	AX,AX
	JNZ	SETCX
DONE:	INT	20H
SETCX:	MOV	CX,AX
	MOV	SI,DX

TLOOP:
	LODSB
	CMP	AL,1AH
	JZ	DONE

ifdef DBCS
	cmp	dbcs_flag,1
	jz	set_dbcs		; if last char was lead byte
	cmp	dbcs_flag,2
	jnz	@f			; if last char was not tail byte
	mov	dbcs_flag,0
@@:
	call	IsDBCSLeadByte
	jnz	not_lead		; if this is not lead byte
@@:
	mov	ah,CURCOL
	inc	ah
	cmp	ah,MAXCOL
	jbe	set_dbcs		; if this is not at last column
	dec	si			; put back
	inc	cx
	inc	CURROW			; go to next row
	mov	CURCOL,1
	mov	al,' '
	jmp	short ISCNTRL
set_dbcs:
	inc	dbcs_flag
not_lead:
endif

	CMP	AL,13
	JNZ	NOTCR
	MOV	BYTE PTR CURCOL,1
	JMP	SHORT ISCNTRL

NOTCR:	CMP	AL,10
	JNZ	NOTLF
	INC	BYTE PTR CURROW
	JMP	SHORT ISCNTRL

NOTLF:	CMP	AL,8
	JNZ	NOTBP
	CMP	BYTE PTR CURCOL,1
	JZ	ISCNTRL
	DEC	BYTE PTR CURCOL
	JMP	SHORT ISCNTRL

NOTBP:	CMP	AL,9
	JNZ	NOTTB
	MOV	AH,CURCOL
	ADD	AH,7
	AND	AH,11111000B
	INC	AH
	MOV	CURCOL,AH
	JMP	SHORT ISCNTRL

NOTTB:
	CMP	AL,7
	JZ	ISCNTRL

	INC	BYTE PTR CURCOL
	MOV	AH,CURCOL
	CMP	AH,MAXCOL
	JBE	ISCNTRL
	INC	BYTE PTR CURROW
	MOV	BYTE PTR CURCOL,1

ISCNTRL:
	MOV	DL,AL
	MOV	AH,STD_CON_OUTPUT
	INT	21H
	MOV	AH,CURROW
	CMP	AH,MAXROW
	JB	CHARLOOP

ASKMORE:
	PUSH	BP			;AN000; ;save file handle
	PUSH	SI			;AN000; ;save pointer
	PUSH	CX			;AN000; ;save count
	MOV	AX,MORE_MSG		;AN000; ;use message retriever..
	MOV	BX,STDERR		;AN000; ;to issue..
	XOR	CX,CX			;AN000; ;-- More --
	MOV	DL,NO_INPUT		;AN000;
	MOV	DH,UTILITY_MSG_CLASS	;AN000;
	CALL	SYSDISPMSG		;AN000;

	MOV	AH,STD_CON_INPUT_FLUSH	 ;WAIT FOR A KEY, NO ECHO
	MOV	AL,STD_CON_INPUT_NO_ECHO ;AC000; ;no echo
	INT	21H

	CMP	AL,EXTENDED		;AN000; ;Check for extended key?
	JNE	NOT_EXTENDED		;AN000; ;continue
	MOV	AH,STD_CON_INPUT_NO_ECHO ;AN000; ;clear extended key
	INT	21H			;AN000; ;

NOT_EXTENDED:
	MOV	CX,CRLF2_LEN		;AC003; ;place cursor..
	MOV	DX,OFFSET CRLF2 	;AC003; ;..on new line.
	MOV	BX,STDERR		;AN000;
	MOV	AH,WRITE		;AN000;
	INT	21H			;AN000;
	POP	CX			;AN000; ;restore count
	POP	SI			;AN000; ;restore pointer
	POP	BP			;AN000; ;restore file handle

	MOV	BYTE PTR CURCOL,1
	MOV	BYTE PTR CURROW,1

CHARLOOP:
	DEC	CX
	JZ	GOBIG
	JMP	TLOOP
GOBIG:	JMP	ALOOP


;----------------------------------------------------------------
;
; CHECK_OPTIONS
;
; on entry:
;   no value passed
;   DS:80 has the command tail
;
; on exit:
;   if /? option found:
;     options help message displayed
;     carry set
;   else
;     nothing displayed
;     carry clear
;   all non-segment registers used.
;
; function:
;   Check the command tail to see if the user specified /?.
;   If so, display the options help message, and set the
;   carry flag so the program will end and they can try again.
;
;----------------------------------------------------------------

	PUBLIC	CHECK_OPTIONS
CHECK_OPTIONS	PROC	NEAR

	; Pick up a pointer to the command tail.

	mov	si, CommandTail		; len and tail

	; See if the user put "/?" on the command line somewhere.
	; If so, that's our cue to display the options help.

	mov	cl, [si]		; get the command tail length
	xor	ch, ch			;  as a word
	jcxz	CO_Done			; done if no tail
	inc	si			; point to start of tail
	xor	ax,ax			; ah = 0 -> no error
CO_FindSwitchLoop:
	lodsb				; get next char
	cmp	al, SWITCHAR		; is it '/?'?
	je	CO_Switch		; yes, look for ?
	cmp	al,SPACE		
	je 	CO_ForNxtChar
	inc	ah			; some redundant arg given
CO_ForNxtChar:	
	loop	CO_FindSwitchLoop
	jmp	short CO_invcmdline

	; Switch found.
	; See if it's one we know about.

CO_Switch:
	jcxz	CO_dsperr		; if no switch after switch char,error
	cmp	byte ptr[si], OPTNCHAR	; is it '?'?
	je	CO_Display		;  jump if so
CO_SwitchInvalid:
	inc	si			; pass it by
	inc	ah			; invalid switch error
	loop	CO_FindSwitchLoop	; go for next char

; some arg has been given ; since more does not have any args except /?,
; display error message and quit

CO_invcmdline:
	or	ah,ah			; was there an error ?
	jz	CO_Done			; no, just quit
CO_dsperr:
	mov	ax, MSG_INV_ARG		; message number
	mov	bx, STDERR		; stderr handle
	xor	cx, cx			; substitution count
	mov	dl, NO_INPUT		; no input needed
	mov	dh, UTILITY_MSG_CLASS	; message class
	call	SYSDISPMSG		; display it
	mov	ax,1			; error code
	stc
	jmp	short CO_Exit


	; Display each line of the options help message.
	; Assumes that AX, BX, CX, and DX are not modified
	; by SYSDISPMSG.

CO_Display:
	mov	ax, MSG_OPTIONS_FIRST	; message number
	mov	bx, STDOUT		; output handle
	xor	cx, cx			; substitution count
	mov	dl, NO_INPUT		; no input needed
	mov	dh, UTILITY_MSG_CLASS	; message class
CO_Display_Loop:
	call	SYSDISPMSG		; display it
	cmp	ax, MSG_OPTIONS_LAST	; see if last message
	je	CO_Display_Done		;  done if so
	inc	ax			; else bump message number
	jmp	short CO_Display_Loop	;  and go do next one

	; Exits

CO_Display_Done:
	xor	al, al			; return code
	stc				; flag to exit program
	jmp	short CO_Exit		;  and leave
CO_Done:
	clc				; flag no /? found
CO_Exit:
	ret


CHECK_OPTIONS	ENDP

ifdef DBCS
;
;	Test if the character is DBCS Lead Byte
;
;	input:	AL = character to check
;	outpit:	ZF = 1 if DBCS Lead Byte
;

DBCSLeadByteTable	dd	0

IsDBCSLeadByte		proc	near
	push	ax
	push	si
	push	ds
	lds	si,cs:DBCSLeadByteTable
	cmp	word ptr cs:DBCSLeadByteTable+2,0
	jnz	idlb_check		; if table is already set
	push	ax
	mov	ax,6300h
	int	21h			; get DBCS lead byte table
	pop	ax
	mov	word ptr cs:DBCSLeadByteTable,si
	mov	word ptr cs:DBCSLeadByteTable+2,ds
idlb_check:
	cmp	word ptr [si],0
	jz	idlb_not		; if end of table
	cmp	al,[si]
	jb	idlb_next		; if below low value
	cmp	al,[si+1]
	jbe	idlb_yes		; if below high value
idlb_next:
	add	si,2			; do next
	jmp	short idlb_check
idlb_not:
	or	al,1			; reset ZF
	jmp	short idlb_end
idlb_yes:
	and	al,0			; set ZF
idlb_end:
	pop	ds
	pop	si
	pop	ax
	ret
IsDBCSLeadByte		endp
endif


MAXROW	DB	25
MAXCOL	DB	80
CURROW	DB	1
CURCOL	DB	1
ifdef DBCS
dbcs_flag	db	0		; 0=single, 1=lead byte, 2=tail byte
endif

ANSI_BUF ANSI_STR <>			;AN000; ;buffer for IOCTL call

.XLIST					;AN000;
MSG_SERVICES <MSGDATA>			;AN000; ;message retriever code
MSG_SERVICES <LOADmsg,DISPLAYmsg,NOCHECKSTDIN>	;AN002;
MSG_SERVICES <MORE.CL1> 		;AN000;
MSG_SERVICES <MORE.CL2> 		;AN000;
MSG_SERVICES <MORE.CLA> 		;AN000;
.LIST					;AN000;

CRLF	    DB	   13,10		   ;AC000;
CRLF_LEN    DW	   $ - CRLF		   ;AC000;
CRLF2	    DB	   13,10,13,10		   ;AN003;
CRLF2_LEN   DW	   $ - CRLF2		   ;AN003;

BUFFER	LABEL BYTE

CODE	ENDS
	END	START

