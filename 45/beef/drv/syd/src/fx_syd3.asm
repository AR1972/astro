;*
;*	CW : Character Windows Drivers
;*
;*	fx_syd3.asm : linked in DOS 3 SYD file
;*****************************************************************************

	include	syd_head.inc
	include	fxdrv.inc

	include	syd_data.inc

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
;*	* do click
	mov	cx,200		;* ?????
	cCall	Tone
	jmp	short end_sound

;*	* do beep
do_beep:
	mov	cx,1356		;* A5
	cCall	Tone
	mov	cx,1708		;* F5
	cCall	Tone
end_sound:

cEnd	DoSoundSyd


cProc	Tone,<NEAR,ATOMIC>
cBegin	Tone
	mov	al,182
	out	43h,al
	mov	al,cl
	out	42h,al
	mov	al,ch
	out	42h,al

	in	al,61h
	or	al,3		; turn speaker on
	out	61h,al
	
;*	* delay 60 ms.
	mov	cx,60
delay1:
	push	cx
	mov	cx,260
delay2:
	loop	delay2
	pop	cx
	loop	delay1

	in	al,61h
	and	al,0fch		; turn speaker off
	out	61h,al
cEnd	Tone



;*****************************************************************************

;*****************************************************************************
;********** LGetTimeSyd **********
;*	* SYD entry point (see documentation for interface)

cProc	LGetTimeSyd, <FAR, PUBLIC, ATOMIC>
cBegin	LGetTimeSyd

TIC_COUNT	EQU	46ch
	xor	ax,ax
	mov	es,ax
	les	ax,es:[TIC_COUNT]	; ES:AX current timer count from bios
	mov	dx,es			; DX:AX count 
					; DH is always 0, so don't have to make
					; it signed.  Biggest DL gets is 18h
;
; Shouldn't do the INT 1ah, since it clears and returns the "date rolled over"
; flag.
;	mov	ah,0
;	int	1aH
;	mov	ax,dx
;	mov	dx,cx
;	and	dx,7fffH			;* make signed

cEnd	LGetTimeSyd


;*****************************************************************************

	include	syd_std.asm		;* standard init/term

	include	syd_tail.asm		;* tail file

;*****************************************************************************

	END

