;********** CrisPFont **********
;*	entry:	cx = points (box height)
;*		al = character 
;*		ah = color attribute
;*		dl = ffont attributes
;*		dh = ffontExtraMask
;*		ds:bx = current INST
;*		ds:si = char bit map
;*	exit:	ds:si = ChrisP char bit map
;*	trash:	ax,cx,dx
;*	Fonts: 8x8, 8x14, 8x16

cProc	ChrisPFont, <NEAR>
   localB fExtraMask
   localW dyChar
cBegin	ChrisPFont

	mov	dyChar,cx
	test	dl,ffontOrCharacter
	jz	@F
	shr	dh,1
	shr	dh,1
	shr	dh,1
	shr	dh,1
	mov	fExtraMask,dh		;take upper 4-bits of ffont word
@@:
	cmp	cx,8
	je	ffont8x8
	jmp	not8x8font

ffont8x8:	

;*************************************************
;*	8 X 8
;*

	XOR	AX,AX

	TEST	DL,ffontSuperscript			; Test for superscripted
	JZ	NotSuper
	TEST	DL,ffontSubscript			; If bits 10H and 20H are set, then
	JZ	GotSuper		; we have mini-caps.
	MOV	CX,[SI + 3]
	MOV	[SI + 4],CX
	MOV	CL,[SI]
	MOV	CH,[SI + 2]
	MOV	[SI + 2],CX
	MOV	[SI],AX
	JMP	SHORT NotSub

GotSuper:
	MOV	CX,[SI + 2]		; If superscript, scrunch up
					; the character
	MOV	[SI + 1],CX
	MOV	CL,[SI + 4]
	MOV	[SI + 3],CL
	MOV	CX,[SI + 6]
	MOV	[SI + 4],CX
	MOV	[SI + 6],AX

NotSuper:
	TEST	DL,ffontSubscript			; If subscripted character, scrunch the
	JZ	NotSub			; character down
	MOV	CL,[SI + 6]
	MOV	[SI + 7],CL
	MOV	CX,[SI + 3]
	MOV	[SI + 5],CX
	MOV	CL,[SI]
	MOV	CH,[SI + 2]
	MOV	[SI + 3],CX
	MOV	[SI],AX
	MOV	[SI + 2],AL

NotSub:
	TEST	DL,ffontBold 		; If we want a bold character
	JZ	NotBold8x8 		; then get the old font, shift it over
	MOV	CX,[SI] 		; one pixel, and OR in the result with
	SHR	CL,1			; the original character.
	SHR	CH,1
	OR	[SI],CX
	MOV	CX,[SI + 2]
	SHR	CL,1
	SHR	CH,1
	OR	[SI + 2],CX
	MOV	CX,[SI + 4]
	SHR	CL,1
	SHR	CH,1
	OR	[SI + 4],CX
	MOV	CX,[SI + 6]
	SHR	CL,1
	SHR	CH,1
	OR	[SI + 6],CX

NotBold8x8:
	test	dl,ffontItalic			; If printing italic character
	Jz	NotItalics		; shift the top part 3 pixels,
	SHR	BYTE PTR [SI],1
	SHR	BYTE PTR [SI],1
	SHR	BYTE PTR [SI],1
	SHR	BYTE PTR [SI + 1],1	; The middle part 2 pixels
	SHR	BYTE PTR [SI + 1],1
	SHR	BYTE PTR [SI + 2],1
	SHR	BYTE PTR [SI + 2],1
	SHR	BYTE PTR [SI + 3],1	; And near-the-bottom part one pixel
	SHR	BYTE PTR [SI + 4],1

NotItalics:
	TEST	DL,ffontUnderline			; If an underscore or double underscore,
	JZ	NotUnderScore		; fill in the bottom line
	TEST	DL,ffontDoubleUnderline			; If both underscore and double
	JZ	Underscore		; underscore, make bottom line
					; dotted (hidden)
	OR	BYTE PTR [SI + 7],088H
	JMP	SHORT NotDoubleScore

NotUnderScore:
	TEST	DL,ffontDoubleUnderline			; If a double underscore, fill in
	JZ	NotDoubleScore		; 2nd from bottom line
	MOV	BYTE PTR [SI + 6],-1

Underscore:
	MOV	BYTE PTR [SI + 7],-1

NotDoubleScore:
	TEST	DL,ffontStrikeThrough			; If a strike through, fill in middle
	JZ	NotStrike		; line of character
	MOV	BYTE PTR [SI + 3],-1

NotStrike:
	jmp	exitChrisP

not8x8font:

IFDEF CPF814

;*************************************************
;*	8 X 14
;*

	cmp	cx,14
	je	ffont8x14
	jmp	not8x14font
	
ffont8x14:
	mov	ah,dl
	xor	dx,dx

	TEST	AH,ffontSuperscript			; Test for superscripted
	JZ	OCENoSuper
	TEST	AH,ffontSubscript			; If bits 10H and 20H are set, then
	JZ	OCESuper		; we have mini-caps.
	MOV	AL,[SI + 8]
	MOV	[SI + 9],AL
	MOV	AL,[SI + 7]
	MOV	[SI + 8],AL
	MOV	AL,[SI + 6]
	MOV	[SI + 7],AL
	MOV	AL,[SI + 5]
	MOV	[SI + 6],AL
	MOV	AL,[SI + 4]
	MOV	[SI + 5],AL
	MOV	AL,[SI + 2]
	MOV	[SI + 4],AL
	MOV	AL,[SI + 1]
	MOV	[SI + 3],AL
	MOV	[SI + 2],DL
	MOV	[SI + 1],DL
	MOV	[SI + 0],DL
	JMP	OCENoSub

OCESuper:				; If superscript, scrunch up
					; the character
	MOV	AL,[SI + 1]
	MOV	[SI + 0],AL
	MOV	AL,[SI + 2]
	MOV	[SI + 1],AL
	MOV	AL,[SI + 4]
	MOV	[SI + 2],AL
	MOV	AL,[SI + 5]
	MOV	[SI + 3],AL
	MOV	AL,[SI + 6]
	MOV	[SI + 4],AL
	MOV	AL,[SI + 7]
	MOV	[SI + 5],AL
	MOV	AL,[SI + 8]
	MOV	[SI + 6],AL
	MOV	AL,[SI + 10]
	MOV	[SI + 7],AL
	MOV	AL,[SI + 12]
	MOV	[SI + 8],AL
	MOV	[SI + 9],DL
	MOV	[SI + 10],DL
	MOV	[SI + 11],DL
	MOV	[SI + 12],DL

OCENoSuper:
	TEST	AH,ffontSubscript			; If subscripted character, scrunch the
	JZ	OCENoSub		; character down
	MOV	AL,[SI + 12]
	MOV	[SI + 13],AL
	MOV	AL,[SI + 10]
	MOV	[SI + 12],AL
	MOV	AL,[SI + 8]
	MOV	[SI + 11],AL
	MOV	AL,[SI + 7]
	MOV	[SI + 10],AL
	MOV	AL,[SI + 6]
	MOV	[SI + 9],AL
	MOV	AL,[SI + 5]
	MOV	[SI + 8],AL
	MOV	AL,[SI + 4]
	MOV	[SI + 7],AL
	MOV	AL,[SI + 2]
	MOV	[SI + 6],AL
	MOV	AL,[SI + 1]
	MOV	[SI + 5],AL
	MOV	AL,[SI + 0]
	MOV	[SI + 4],AL
	MOV	[SI + 3],DL
	MOV	[SI + 2],DL
	MOV	[SI + 1],DL
	MOV	[SI + 0],DL

OCENoSub:
	TEST	AH,ffontBold	 	; If we want a bold character
	JZ	OCENoBold		; Then get the old font, shift it over
					; one pixel, and OR in the result with
	mov	dx,si
OCEBoldLp:				; the original character
	mov	al,[si]
	SHR	AL,1
	OR	[SI],AL
	inc	si
	LOOP	OCEBoldLp
	mov	si,dx

OCENoBold:
	test	AH,ffontItalic			; If printing italic character,
	jz	OCENoItalic		; shift the top part 2 pixels
	MOV	CL,2
	SHR	BYTE PTR [SI],CL
	SHR	BYTE PTR [SI + 1],CL
	SHR	BYTE PTR [SI + 2],CL
	SHR	BYTE PTR [SI + 3],CL
	SHR	BYTE PTR [SI + 4],CL
	SHR	BYTE PTR [SI + 5],1	; Middle part 1 pixel
	SHR	BYTE PTR [SI + 6],1
	SHR	BYTE PTR [SI + 7],1
	SHL	BYTE PTR [SI + 12],1	; Bottom line 1 pixel other way

OCENoItalic:
	TEST	AH,ffontUnderline			; If an underscore or double underscore,
	JZ	OCENoUnder		; fill in the bottom line
	TEST	AH,ffontDoubleUnderline			; If both underscore and double
	JZ	OCEUnder		; underscore, make bottom line
					; dotted (hidden)
	OR	BYTE PTR [SI + 13],088H
	JMP	SHORT OCENoDouble

OCENoUnder:
	TEST	AH,ffontDoubleUnderline			; If a double underscore, fill in
	JZ	OCENoDouble		; 2nd from bottom line
	MOV	BYTE PTR [SI + 12],-1

OCEUnder:
	MOV	BYTE PTR [SI + 13],-1

OCENoDouble:
	TEST	AH,ffontStrikeThrough			; If a strike through, fill in
	JZ	OCENoStrike		; middle line of character
	MOV	BYTE PTR [SI + 6],-1

OCENoStrike:
	mov	dl,ah			;restore ffont attribute
	jmp	exitChrisP

not8x14font:

ENDIF
IFDEF CPF816

;*************************************************
;*	8 X 16
;*

	cmp	cx,16
	je	ffont8x16
	jmp	exitChrisP

ffont8x16:
	mov	ch,dl
	xor	dx,dx

	test	ch,ffontSuperscript			; Test for superscripted
	JNZ	OSHIsMiniCaps
	JMP	SHORT OSHNotSuper

OSHIsMiniCaps:
	TEST	CH,ffontSubscript			; IF bits 10H and 20H are set, THEN
	JZ	OSHGotSuper		; we have mini-caps.
	MOV	AX,[SI + 6]
	MOV	[SI + 7],AX
	MOV	AL,[SI + 5]
	MOV	[SI + 6],AL
	MOV	AX,[SI + 2]
	MOV	[SI + 4],AX
	MOV	[SI + 1],DL
	MOV	[SI + 2],DX
	JMP	SHORT OSHNotSub

OSHGotSuper:
	MOV	AX,[SI + 2]		; IF superscript, scrunch up the
					; character
	MOV	[SI],AX
	MOV	AX,[SI + 5]
	MOV	[SI + 2],AX
	MOV	AX,[SI + 7]
	MOV	[SI + 4],AX
	MOV	AL,[SI + 10]
	MOV	[SI + 6],AL
	MOV	AX,[SI + 11]
	MOV	[SI + 7],AX
	MOV	AL,[SI + 14]
	MOV	[SI + 9],AL
	MOV	[SI + 10],DX
	MOV	[SI + 12],DX
	MOV	[SI + 14],DX

OSHNotSuper:
	TEST	CH,ffontSubscript			; IF subscripted character, scrunch the
	JZ	OSHNotSub		; character down
	MOV	AX,[SI + 13]
	MOV	[SI + 14],AX
	MOV	AX,[SI + 10]
	MOV	[SI + 12],AX
	MOV	AX,[SI + 7]
	MOV	[SI + 10],AX
	MOV	AX,[SI + 5]
	MOV	[SI + 8],AX
	MOV	AX,[SI + 2]
	MOV	[SI + 6],AX
	MOV	AX,[SI]
	MOV	[SI + 4],AX
	MOV	[SI],DX
	MOV	[SI + 2],DX

OSHNotSub:
	TEST	CH,ffontBold	  	; IF we want a bold character
	JZ	OSHNotBold		; THEN get the old font, shift it over
	mov	ah,ch			;save ffont
	MOV	CX,16			; one pixel, and OR in the result with
	mov	dx,si			; the original character.

OSHboldL:
	MOV	AL,[SI]
	SHR	AL,1
	OR	[SI],AL
	INC	SI
	LOOP	OSHboldL
	mov	si,dx
	mov	ch,ah

OSHNotBold:
	test	ch,ffontItalic			; IF printing italic character
	Jz	OSHNotItalics
	SHR	BYTE PTR [SI + 0],1
	SHR	BYTE PTR [SI + 1],1
	SHR	BYTE PTR [SI + 2],1
	SHR	BYTE PTR [SI + 3],1
	SHR	BYTE PTR [SI + 4],1
	SHL	BYTE PTR [SI + 10],1
	SHL	BYTE PTR [SI + 11],1	; And near-the-bottom part two pixels
	SHL	BYTE PTR [SI + 12],1
	SHL	BYTE PTR [SI + 13],1
	SHL	BYTE PTR [SI + 14],1

OSHNotItalics:
	TEST	CH,ffontUnderline			; IF an underscore or double underscore,
	JZ	OSHNotUnderScore	; fill in the bottom line
	TEST	CH,ffontDoubleUnderline		; IF both under and double underscore
	JZ	OCSHUnder		; make bottom dotted ( hidden )
	OR	BYTE PTR [SI + 15],088H
	JMP	SHORT OSHNotDoubleScore

OSHNotUnderScore:
	TEST	CH,ffontDoubleUnderline			; IF double underscore, fill in 2nd from
	JZ	OSHNotDoubleScore	; bottom line
	MOV	BYTE PTR [SI + 14],-1

OCSHUnder:
	MOV	BYTE PTR [SI + 12],-1

OSHNotDoubleScore:
	TEST	CH,ffontStrikeThrough			; IF strike through, fill in middle line
	JZ	No16Strike				; of character
	MOV	BYTE PTR [SI + 7],-1
No16Strike:
	mov	dl,ch			;restore ffont attribute
ENDIF

exitChrisP:
	test	dl,ffontOrCharacter
	jz	@F

	mov	cx,dyChar
	cmp	fExtraMask,0
	jne	NotOr0
	mov	al,00011000b		;* or in normal vertical line
	jmp short OrCharBit
NotOr0:	
	cmp	fExtraMask,1
	jne	@F
	mov	al,00011100b		;* or in bold vertical bar
OrCharBit:
	mov	dx,si	
LoopOr:
	or	[si],al
	inc	si
	loop	LoopOr
	mov	si,dx
@@:
	
cEnd 	ChrisPFont
