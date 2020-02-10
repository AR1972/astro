;*
;*	CW : Character Oriented Windows
;*
;*	screen.inc : screen specific stuff (DOS version general)

;*	* Assembler version of SA structure (see color.c for C version) *
SA	STRUC
	bFiller	DB	?		;* filler
	caSa	DB	?		;* character attribute
SA	ENDS


;*	* UNIONS :
caColor		EQU BYTE PTR caSa		;* color initial value
caMono		EQU BYTE PTR bFiller		;* b&w initial value
rgcaFill	EQU WORD PTR bFiller		;* near pointer to array

dmNormal	EQU	0
dmText		EQU	1
dmForeBack	EQU	2
dmFore		EQU	3
dmBack		EQU	4
dmTextFore	EQU	5
dmTextBack	EQU	6

dmSpecialMin	EQU	7
dmTextMapB	EQU	7
dmTextMapF	EQU	8
dmMapB		EQU	9
dmMapF		EQU	10

;*	* Special values (for mouse drawing)
ayNil		EQU	253		;* set ayDrawing to ayNil if not drawing
		;* (can't be 254/255 since we allow 2 char for mouse height).

externFPublic <FEnableMouse>


;********** CalcCoord **********
;*	entry : axPos = ax position
;*		ayPos = ay position
;*	exit : AX = address of character in video segment
;*	* ONLY USE THIS MACRO IF YOU KNOW THE MOUSE IF OFF !!!
;*	* TRASHES AX,BX

CalcCoord MACRO axPos,ayPos
	mov	al,ayPos
	mul	axMac
	mov	bl,axPos
	xor	bh,bh
	add	ax,bx
	shl	ax,1		; ax = (ayPos * axMac + ayPos) * 2
ENDM

;********** CheckWnd **********
;*	entry:	di => offset in video segment
;*		skip = label to jump to
;*	* check if pwndCur is NULL or drawable window
;*	*  if not, add 2 to DI and jump to skip.
;*	exit:	only di changed (if jump taken)
IFDEF LATER
	... re-adjust DoDraw do that BP is free
ENDIF

IFDEF WINDOW_OVERLAP
externW <psOverlap>

CheckWnd MACRO skip
	LOCAL	do_it
	push	cx
	mov	cx,pwndCur
	jcxz	do_it
        push    di
        and     di,0fffeh
	push	ds
	mov	ds,psOverlap
	cmp	cx,[di]
	pop	ds
        pop     di
	je	do_it
	pop	cx
	lea	di,[di+2]		;* skip to next char
	jmp	short skip
do_it:
	pop	cx
ENDM	;CheckWnd

ELSE ; no overlap

CheckWnd MACRO skip
ENDM	;CheckWnd

ENDIF ;WINDOW_OVERLAP


IFDEF BUILTIN_SNOW

;;* CGA-old only
;;* QC/QB only
;;* For Twin compatibility sake

;********** WaitRetraceDefinite **********
;* MACRO
;*	entry : DX = portVideoStatus
;*	* waits for the start of a horizontal retrace
;*	* so video memory can be accessed flicker-free.
;*	exit : INTERRUPT DISABLED
;*		TRASHES AX, DX retained

WaitRetraceDefinite MACRO
	local	enable_int_window, wait_for_noretrace, wait_for_retrace, do_it

enable_int_window:
	sti
	mov	ah,20			;* magic number of iterations
	cli				;* clear interrupts for magic loop
wait_for_noretrace:
	dec	ah
	jz	enable_int_window
	in	al,dx			; fetch status
	test	al,8
	jnz	do_it
	test	al,1			; horizontal retracing?
	jnz	wait_for_noretrace	; yes, wait until no retrace
wait_for_retrace:
	in	al,dx			; fetch status
	test	al,9			; horizontal/vertical retracing?
	jz	wait_for_retrace	; no, wait...
do_it:
ENDM



;********** StartDrawCrit **********
;* MACRO
;*	* like WaitRetraceDefinite, but test if we have to wait first
;*	* next critical regions with StartDrawCrit/EndDrawCrit

StartDrawCrit MACRO
	local	no_wait
	cmp	fWaitRetrace,0
	je	no_wait

	WaitRetraceDefinite
no_wait:
ENDM


;********** EndDrawCrit **********
;* MACRO
;*	* after StartDrawCrit and the critical operation, enable interrupts

EndDrawCrit MACRO
	sti
ENDM

ENDIF 	;BUILTIN_SNOW
