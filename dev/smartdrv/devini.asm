;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;
;OVERVIEW
;
;	This module contains the strategy and interrupt entry points for
;the cache's dummy device driver.  This copies the safedsk device driver
;on top of the cache segment so the safedsk.sys will be loaded.
;

vseg segment para public 'CODE'
vseg ends

zseg    segment public 'CODE'

	assume  cs:zseg
	assume	ds:nothing

extrn	header			:near
extrn	devattr			:word
extrn	last_safe_byte		:byte
extrn	interrupt		:near
extrn	strategy		:near

PUBLIC	tempstrategy
PUBLIC	tempinterrupt
PUBLIC	RHPtr

RHPtr   dd      ?                       ;Request Header pointer filled in by 
					;Strategy routing



tempstrategy        proc    far

	mov     WORD PTR cs:[RHPtr],bx
	mov     WORD PTR cs:[RHPTR+2],es
	ret

tempstrategy        endp

tempinterrupt       proc    far

	push	es
	push	ds
	push	cx
	push	di
	push	si

	;;;copy the safedsk on top of the current device header.
	;;;We do this so the device driver will not be part of the
	;;;TSR when it is loaded.
	;;;We have put the safedsk into its own 0 org'd segment
	;;; named "vseg" so we can just copy right on top of the 
	;;;0 org'd temp driver.
	;;;
	;;;Once loaded, we need to call the safedsk interrupt routine
	;;;since init won't be called by DOS since we are in the
	;;;init call right now!
	;;;
	push	cs			
	pop	es			;destinatain = cs:0
	xor	di,di

	mov	ax,vseg		;src = vseg:0
	mov	ds,ax
	xor	si,si

	mov	cx,offset vseg:last_safe_byte 	;get offet of last label in safedsk
	inc	cx			      	;round up
	shr	cx,1			      	;movsw does two bytes at a time
	rep	movsw	

	pop	si
	pop	di
	pop	cx
	pop	ds
	pop	es

	push	cs				;simulate far call to safedsk 
	call	strategy			;strategy point
	push	cs				;simulate far call to safedsk
	call	interrupt			;interrupt (init call)

	ret					;return to dos--future calls
						;will go to the safedsk
						;interrupt point because we
						;overwrote the device header

tempinterrupt       endp


zseg ends

end

