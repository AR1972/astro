	page	,132
	TITLE	uiword.asm - Low level code for delineating words
;*** 
;uiword.asm
;
;	Copyright <C> 1985-1988, Microsoft Corporation
;
;Purpose:
;	Interface between CW and and QB for determining characters
;	delineating words.  Contains tables and code that define
;	character set allowed in generic words, and BASIC labels.
;
;
;*******************************************************************************


	.xlist
	include	version.inc
	.list

	UIWORD_ASM = ON

	IncludeOnce uiint


assumes DS,DATA
assumes ES,DATA
assumes SS,DATA

	subttl	DATA segment definitions.
	page


sBegin UI
assumes CS,UI

	extrn	GetEditWord:near	

	; Verify that all the GEW constants have realistic values
	; These must match with the word at wordDot.

	.erre	GEW_DFLTMASK   EQ 5E03H
	.erre	GEW_DFLTMASK   EQ GEW_NODOTMASK OR 0002H
	.erre	GEW_DFLTMASK   EQ GEW_HELPMASK	OR 4002H
	.erre	GEW_VARMASK    EQ 0000H
	.erre	GEW_VARDOTMASK EQ GEW_VARMASK	OR 0002H


wordCharBitMask	dw	00000H	; 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
				
				; N S S E E E A B B H L V F C S S
				; U O T T O N C E S T F T F R O I
				; L H X X T Q K L

		dw	00000H	; 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
				
				; D D D D D S E C E S E F G R U S
				; L C C C C Y T A M U S S S S S P
				; E 1 2 3 4 N B N   B C

wordDot 	dw	05e03H	; 0 1 0 1 1 1 1 0 0 0 0 0 0 0 1 1
				;   ! " # $ % & ' ( ) * + , - . /

		dw	0ffC0H	; 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0
				; 0 1 2 3 4 5 6 7 8 9 : ; < = > ?

		dw	07fffH	; 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
				; @ A B C D E F G H I J K L M N O

		dw	0ffe0H	; 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0
				; P Q R S T U V W X Y Z [ \ ] ^ _

		dw	07fffH	; 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
				; ` a b c d e f g h i j k l m n o

		dw	0ffe0H	; 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0
				; p q r s t u v w x y z { | } ~ D
				;                               E
				;                               L

		dw	0ffffH	; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
				
				; Accented letters (IBM char set)

		dw	0ffe0H	; 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0
				
				; Accented letters (IBM char set)

		dw	0ff00H	; 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0
				
				; Accented letters (IBM char set)

LabelBitMask	dw	00040H	; 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0
				
				; N S S E E E A B B H L V F C S S
				; U O T T O N C E S T F T F R O I
				; L H X X T Q K L

		dw	00000H	; 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
				
				; D D D D D S E C E S E F G R U S
				; L C C C C Y T A M U S S S S S P
				; E 1 2 3 4 N B N   B C

		dw	08002H	; 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0
				;   ! " # $ % & ' ( ) * + , - . /

		dw	0ffc0H	; 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0
				; 0 1 2 3 4 5 6 7 8 9 : ; < = > ?

		dw	07fffH	; 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
				; @ A B C D E F G H I J K L M N O

		dw	0ffe0H	; 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0
				; P Q R S T U V W X Y Z [ \ ] ^ _

		dw	07fffH	; 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
				; ` a b c d e f g h i j k l m n o

		dw	0ffe0H	; 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0
				; p q r s t u v w x y z { | } ~ D
				;                               E
				;                               L

		dw	0ffffH	; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
				
				; Accented letters (IBM char set)

		dw	0ffe0H	; 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0
				
				; Accented letters (IBM char set)

		dw	0ff00H	; 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0
				
				; Accented letters (IBM char set)

	subttl	Low level CW interface for chars in defining words/labels
	page

;***
;IsInTable - checks specified table for presence of character
;Purpose:
;  Uses mod arithematic to check of a bit in a mask table
;  is on for a character. This is used to classify character
;Entry:
;  al - character
;  si - pointer to mask table
;Exit:
;  returns - 1 if in table
;	     0 if not
;Uses:
;  bx, cx
;***
cProc	IsInTable,<NEAR>
cBegin
	cmp	al, 0b0H
	jae	NotInTable

; See if bit # char is set in the array of bits WordCharBitMask
	push	ax
	and	ax, 00f0H
	mov	cl, 3
	shr	al, cl
	mov	bx, ax
	mov	bx, cs:[si+bx]		; table is FAR to save DGROUP
	pop	ax
	and	al, 0fH
	inc	al
	mov	cl, al
	xor	ax, ax
	stc
	rcr	ax, cl
	and	ax, bx
	jz	NotInTable

	mov	ax, 1
	jmp	short InTableDone

NotInTable:
	xor	ax, ax

InTableDone:
cEnd


;***
;IsWordChar, IsLabelChar - checks if character can be in a word or BASIC label
;Purpose:
;  Sets up word table and calls IsInTable to see if specified character
;  is a valid character in a word/BASIC label.
;Entry:
;  parmB char - character to test
;Exit:
;  returns - 1 if char can be part of a word
;	     0 if not
;Uses:
;  Per Convention
;*****************************************************************************
cProc	IsWordChar,<PUBLIC,FAR>,<SI>
	parmB	char
cBegin
	mov	si, UIOFFSET WordCharBitMask
	mov	al, char
	cCall	IsInTable
cEnd

cProc	IsLabelChar,<PUBLIC,NEAR>,<SI>
	parmB	char
cBegin
	mov	si, UIOFFSET LabelBitMask
	mov	al, char
	cCall	IsInTable
cEnd


;***
;GetEditWordMask - Calls GetEditWord, disallowing certain standard characters
;
;Purpose:
;	Added with revision [5].
;	Toggles the bits inthe table that are specified by the mask, then
;	calls GetEditWord.
;
;Entry:
;	pBuf - ptr to buffer to receive word	(same as GetEditWord)
;	cbMax - maximum size of buffer		(same as GetEditWord)
;	wMask  - GEW constant defining what values to mask out for this
;		GetEditWord search.
;
;Exit:
;	none
;
;Uses:
;	Per Convention
;*****************************************************************************
cProc	GetEditWordMask,<PUBLIC,NEAR>
parmW	pBuf
parmW	cbMax
parmW	wMask
cBegin
	mov	ax,wMask		; get the new set of legal chars
	mov	cs:[wordDot],ax		; disable characters in words
	cCall	GetEditWord,<pBuf,cbMax> ;get word under cursor
	or	cs:[wordDot],GEW_DFLTMASK ; re-enable proper chars in words
cEnd

sEnd	UI
	end
