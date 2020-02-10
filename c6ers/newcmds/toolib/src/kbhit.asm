;
; kbhit () return 0 if character waiting, non-0 otherwise
;     Differs from C runtime version in that ^C is not checked for and kbhit
; returns FALSE instead of TRUE if a keystroke is waiting.
;
;   09-Dec-1986 bw  Added DOS 5 support
;   28-Aug-1987 dl  Remove getch
;   30-Oct-1987 bw  Changed 'DOS5' to 'OS2'


.xlist
include ..\h\cmacros.inc
ifdef OS2
include ..\h\subcalls.inc
if1
%out  ! OS2 module
endif
endif
.list

sBegin	data
assumes ds,data

staticB input,?
staticB fInput,0

ifdef  OS2
staticB Extend, 0
endif

sEnd

sBegin	code
assumes cs,code

cProc	kbhit,<PUBLIC>
ifdef OS2
localV	PeekBuff, %(size KeyData)
endif
cBegin

ifdef OS2
	mov	ax, 0
	cmp	Extend, 0   ; If last getch got byte 0 of extend char ...
	jne	Done

	push	ss
	lea	ax, PeekBuff
	push	ax
;	 mov	 ax, 1	     ; Don't wait for keystroke
;	 push	 ax
	xor	ax, ax
	push	ax	    ; KBD handle
	CALL	KBDPEEK
	cmp	PeekBuff.status, 0   ; Status non-0 means char waiting or error.
	jne	Done
	mov	ax, 1		     ; 0, so no char
Done:

else
;
; If character pre-read, then say OK.
;
	MOV	AL,fInput
	OR	AL,AL
	JNZ	Done
;
; Use call 6 to interrogate/read the input characters.
;
	mov	ah,6
	mov	dl,0FFh
	int	21h
	jz	Done		    ; convert the 0 in AL to no-char
	mov	Input,al
	mov	AL,0FFH
	mov	fInput,al
Done:
	CBW
	NOT	AX

endif

cEnd


cProc   zgetch,<PUBLIC>
ifdef OS2
localV	CharBuff, %(size KeyData)
endif
cBegin

ifdef OS2
	cmp	Extend, 0
	je	Ask
	mov	al, Extend
	mov	Extend, 0
	jmp	short gotit

Ask:
	push	ss
	lea	ax, CharBuff
	push	ax
	xor	ax, ax
	push	ax		; Wait for input
	push	ax		; KBD handle
	call	KBDCHARIN
	mov	ax, word ptr CharBuff.char_code   ; ALSO picks up scan_code
	test	al, 0FFH
	jne	gotit
	mov	Extend, ah
gotit:	xor	ah, ah

else

	cmp	fInput,0
	jz	Ask
	mov	fInput,0
	mov	al,Input
	jmp	short finish
Ask:
	MOV	AH,7
	INT	21h
finish:
	XOR	AH,AH

endif

cEnd

sEnd

end
