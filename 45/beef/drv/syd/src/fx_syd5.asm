;*
;*	CW : Character Windows Drivers
;*
;*	fx_syd3.asm : linked in DOS 3 SYD file
;*****************************************************************************

	include	syd_head.inc

	include	fxdrv.inc

	include	syd_data.inc

;*	* OS2 global info
	include glis.inc

;*****************************************************************************

	include	fx_data.asm

;*****************************************************************************

sBegin	DRV
    assumes CS,DRV
    assumes ds,NOTHING
    assumes ss,NOTHING

IF cbDataSyd NE 0
;*	* There is no low memory structure for the linked driver
OFF_lpwDataSyd	DW	dataOffset rgwDataSyd
ENDIF

;*****************************************************************************

NonStandard	DoSoundSyd
NonStandard	LGetTimeSyd

;*****************************************************************************

;********** DoSoundSyd **********
;*	* SYD entry point (see documentation for interface)

cProc	DoSoundSyd, <FAR, PUBLIC, ATOMIC>
    parmW  req
cBegin	DoSoundSyd

	mov	cx,req			;* 0=> beep, 1=> click
	jcxz	do_beep
	dec	cx
	jnz	end_sound
;*	* do click (440Hz, 10ms)
	mov	ax,440
	mov	cx,10
	jmp	short do_sound

;*	* do beep (880Hz, 50ms)
do_beep:
	mov	ax,440*2
	mov	cx,50
do_sound:
	mov	bx,pinos
	cCall	[bx].lpfnDosBeepInos,<ax, cx>

end_sound:

cEnd	DoSoundSyd



;*****************************************************************************

;*****************************************************************************
;********** LGetTimeSyd **********
;*	* SYD entry point (see documentation for interface)

cProc	LGetTimeSyd, <FAR, PUBLIC, ATOMIC>
cBegin	LGetTimeSyd

	mov	bx,pinos
	mov	es,[bx].sdGlisInos
	xor	bx,bx
reget_loop:
	mov	cx,word ptr es:[bx].msGlis		;* low word
	mov	ax,word ptr es:[bx].msGlis + 2		;* high word
	cmp	cx,word ptr es:[bx].msGlis		;* tick changed ?
	jnz	reget_loop
;*	* ax:cx = ms tick resolution (convert to 1/18th sec)
	mov	bx,55					;* divide value
	xor	dx,dx
	div	bx					;* ax = quotient,
							;*  dx = remainder
	push	ax					;* high word
	mov	ax,dx					;* remainder
	mul	bx					;* dx:ax = remainder*55
	add	ax,cx
	adc	dx,0
	div	bx					;* ax = remainder
	pop	dx
;*	* dx:ax = system time / 55

cEnd	LGetTimeSyd


;*****************************************************************************

	include	syd_std.asm		;* standard init/term

	include	syd_tail.asm		;* tail file

;*****************************************************************************

	END

