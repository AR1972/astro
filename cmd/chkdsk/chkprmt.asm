;******************************************************************************
;
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;  Change Log:
;
;    Date    Who   #			  Description
;  --------  ---  ---  ------------------------------------------------------
;  03/21/90  EGH  C06  Problem fixed - if CHKDSK output was redirected to a
;		       file, the user response to a y/n prompt would appear
;		       in the file rather than on the screen.  Fix is to
;		       display user input via STDERR rather than STDOUT.
;  03/23/90  EGH  C07  Problem fixed - if the response to a y/n prompt was
;		       ENTER, the screen would scroll but the prompt would
;		       not be redisplayed.  Fix is to save the pointer to the
;		       message and restore it when needed.
;
;******************************************************************************
TITLE	CHKPRMT - Procedures called from chkdsk which prompt			;an000;bgb
page	,132					;				;an000;bgb
										;an000;bgb
	.xlist									;an000;bgb
	include chkseg.inc							;an000;bgb
	INCLUDE CHKCHNG.INC							;an000;bgb
	INCLUDE SYSCALL.INC							;an000;bgb
	INCLUDE CHKEQU.INC							;an000;bgb
	INCLUDE CHKMACRO.INC							;an000;bgb
	include pathmac.inc							;an000;bgb
	.list									;an000;bgb
										;an000;bgb
										;an000;bgb
CONST	SEGMENT PUBLIC PARA 'DATA'						;an000;bgb
	EXTRN	HECODE:byte,CONBUF:byte 					;an000;bgb
	EXTRN	crlf2_arg:word						   ;C06   ;an000;bgb
CONST	ENDS									;an000;bgb
										;an000;bgb
										;an000;bgb
CODE	SEGMENT PUBLIC PARA 'CODE'						;an000;bgb
ASSUME	CS:DG,DS:DG,ES:DG,SS:DG 						;an000;bgb
	EXTRN	PRINTF_CRLF:NEAR,DOCRLF:NEAR					;an000;bgb
										;an000;bgb
	pathlabl chkprmt							;an000;bgb
;*****************************************************************************	;an000;bgb
;Routine name:PromptYN								;an000;bgb
;*****************************************************************************	;an000;bgb
;										;an000;bgb
;description: Validate that input is valid Y/N for the country dependent info	;an000;bgb
;	     Return Z flag if 'Y' entered					;an000;bgb
;Called Procedures: Message (macro)						;an000;bgb
;		    User_String 						;an000;bgb
;										;an000;bgb
;Change History: Created	5/10/87 	MT				;an000;bgb
;										;an000;bgb
;Input: DX = offset to message							;an000;bgb
;										;an000;bgb
;Output: Z flag if 'Y' entered							;an000;bgb
;										;an000;bgb
;Psuedocode									;an000;bgb
;----------									;an000;bgb
;										;an000;bgb
;	DO									;an000;bgb
;	   Display prompt and input character					;an000;bgb
;	   IF got character							;an000;bgb
;	      Check for country dependent Y/N (INT 21h, AX=6523h Get Ext Country;an000;bgb)
;	      IF NC (Yes or No) 						;an000;bgb
;		 Set Z if Yes, NZ if No 					;an000;bgb
;	      ENDIF								;an000;bgb
;	   ELSE  (nothing entered)						;an000;bgb
;	      stc								;an000;bgb
;	   ENDIF								;an000;bgb
;	ENDDO NC								;an000;bgb
;	ret									;an000;bgb
;*****************************************************************************	;an000;bgb
Procedure PromptYN				;				;an000;bgb;AN000;
	push	si				;Save reg			;an000;bgb
;	$DO					;				;an000;bgb;AC000;
$$DO1:
	   push    dx				;save ptr to message	   ;C07
	   Call    Display_Interface		;Display the message		;an000;bgb;AC000;
	   MOV	   DX,OFFSET DG:CONBUF		;Point at input buffer		;an000;bgb
;C06	   DOS_Call Std_Con_String_Input	;Get input			;an000;bgb;AC000;
;C06	   CALL    DOCRLF			;				;an000;bgb
	   call    get_input			;Get input		   ;C06
	   mov	   dx,offset dg:crlf2_arg	;Point at CR LF 	   ;C06
	   call    display_interface		;display it		   ;C06
	   MOV	   SI,OFFSET DG:CONBUF+2	;Point at contents of buffer	;an000;bgb
	   CMP	   BYTE PTR [SI-1],0		;Was there input?		;an000;bgb
;	   $IF	   NE				;Yep				;an000;bgb;AC000;
	   JE $$IF2
	      mov     al,23h			;See if it is Y/N		;an000;bgb;AN000;
	      mov     dl,[si]			;Get character			;an000;bgb;AN000;
	      DOS_Call GetExtCntry		;Get country info call		;an000;bgb;AN000;
;	      $IF     NC			;Yes or No entered		;an000;bgb;AN000;
	      JC $$IF3
		 cmp	 ax,Yes_Found		;Set Z if Yes, NZ if No 	;an000;bgb;AN000;
		 clc				;CY=0 means Y/N found		;an000;bgb
;	      $ENDIF				;CY set if neither		;an000;bgb;AN000;
$$IF3:
;	   $ELSE				;No characters input		;an000;bgb
	   JMP SHORT $$EN2
$$IF2:
	      stc				;CY means not Y/N		;an000;bgb
;	   $ENDIF				;				;an000;bgb
$$EN2:
	   pop	   dx				;restore ptr to message    ;C07
;	$ENDDO	NC				;				;an000;bgb;AN000;
	JC $$DO1
	pop	si				;				;an000;bgb
	ret					;				;an000;bgb
PromptYN endp					;				;an000;bgb;AN000;
	pathlabl chkprmt							;an000;bgb
										;an000;bgb
procedure get_input
	push	ax			;save ax
	push	bx			;save bx
	push	cx			;save cx
	push	dx			;save dx
	push	si			;save si
	push	di			;save di
	mov	si,dx			;si=ptr to start of buffer
	mov	byte ptr [si+1],0	;initialize # chars to zero
	cmp	byte ptr [si],0 	;Q: is buffer size = 0?
	je	return			; Y: then exit
	add	dx,2			;
	mov	di,dx			;di=ptr to first free buf entry
get_key:
	mov	ah,08h			;console input without echo
	int	21h			;get key
	mov	[di],al 		;save character
	cmp	al,0Dh			;Q: CR?
	je	return			; Y: exit loop
	cmp	al,00h			;Q: double byte character?
	je	get_second		; Y: get second byte
	cmp	al,08h			;Q: backspace character?
	je	backspace		; Y: handle backspace
	cmp	al,09h			;Q: tab character?
	je	get_key 		; Y: ignore it
	mov	ah,byte ptr [si]	;size of buffer
	dec	ah			;save room for CR
	cmp	ah,byte ptr [si+1]	;Q: room for character?
	je	no_room 		; N: ring bell
	mov	ah,40h			;write to file/device
	mov	bx,0002h		;use STDERR!
	mov	cx,1			;one character
	mov	dx,di			;ptr to character
	int	21h			;display character
	inc	di			;next free buf entry
	inc	byte ptr [si+1] 	;inc # of characters
	jmp	short get_key		;get next key
backspace:
	cmp	byte ptr [si+1],0	;Q: any characters?
	je	get_key 		; N: get next key
	mov	ah,40h			;write to file/device
	mov	bx,0002h		;use STDERR!
	mov	cx,1			;one character
	mov	dx,di			;ptr to character
	int	21h			;display character
	mov	ah,40h			;write to file/device
	mov	byte ptr [di],20h	;
	int	21h			;display character
	mov	ah,40h			;write to file/device
	mov	byte ptr [di],08h	;
	int	21h			;display character
	dec	di			;next free buf entry
	dec	byte ptr [si+1] 	;inc # of characters
	jmp	short get_key		;get next key
get_second:
	mov	ah,08h			;console input without echo
	int	21h			;get second byte
	jmp	short get_key		;get next key
no_room:
	mov	ah,02h			;display output
	mov	dl,07h			;bell character
	int	21h			;ring bell
	jmp	short get_key		;keep looking for CR
return:
	pop	di			;restore di
	pop	si			;restore si
	pop	dx			;restore dx
	pop	cx			;restore cx
	pop	bx			;restore bx
	pop	ax			;restore ax
	ret
get_input endp

CODE	ENDS									;an000;bgb
	END									;an000;bgb
