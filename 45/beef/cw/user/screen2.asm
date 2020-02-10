;*
;*	COW : Character Oriented Windows
;*
;*	screen2.asm : more of installable screen drivers

	title	screen2 - More screen control code for CW

.xlist
	include	user.inc
	include uisa.inc		;* for isa's
	include	screen.inc		;* screen stuff

	include	inscr.inc
	include	indrv.inc

ifdef	KANJI
	include kanji.inc
endif	;KANJI

.list

;----------------------------------------------------------------------------

sBegin	DATA

	assumes DS,DGROUP

externB insj

ifdef	BUILTIN_SNOW
externB fWaitRetrace		;* snow protect for twin compatibility,QC only
endif	;BUILTIN_SNOW

sEnd	DATA

;----------------------------------------------------------------------------

sBegin	BSS

	assumes DS,DGROUP

externB	instCur			; data for installable drivers

externW	fRestoreDbcs		; => restore DBCS characters
				; stays 0 for non-KANJI

ifdef	SCREEN_FFONT
externW	fFontAvailable		; => ffont support allowed
endif	;SCREEN_FFONT

externW	offDrawFirst		; offset where drawing started
externB	axMac			; Maximum column number
externW	axMacTimes2		; for row modulus - WORD !!!

sEnd	BSS

;----------------------------------------------------------------------------

sBegin	SCREEN

	assumes CS,SCREEN
	assumes DS,DGROUP
	assumes SS,DGROUP
	assumes	ES,nothing

;********** SaveArc **********
;*	entry : axLeft, ayTop, axRight, ayBottom : rectangle
;*		lpb = buffer to save data in (assumed to be large enough).
;*	* Save ARC - NOTE: no interface to screen driver !!
;*	exit : n/a

	assumes DS,DATA

cPrivate SaveArc,<ATOMIC>,<SI,DI>

	parmB axLeft
	parmB ayTop
	parmB axRight
	parmB ayBottom			;* also day
	parmD lpb

ifdef	SCREEN_FFONT
	localW dbSec			;* secondary - primary (in lpb)
endif	;SCREEN_FFONT

	localW fMouseOn				;* FEnableMouse old state

cBegin	SaveArc

	AssertUp

	xor	ax,ax
	cCall	FEnableMouse,<ax>		;* turn mouse off
	mov	fMouseOn,ax

	mov	al,axMac
	cmp	axRight,al
	jbe	@F
	mov	axRight,al
@@:

ifdef	SCREEN_FFONT
;*	* calculate the dbSec (ayBottom-ayTop)*(axRight-axLeft)*2
	mov	al,ayBottom
	sub	al,ayTop
	mov	ah,axRight
	sub	ah,axLeft
	mul	ah			;* ax = cch
	shl	ax,1
	mov	dbSec,ax
endif	;SCREEN_FFONT

	CalcCoord axLeft,ayTop
	mov	si,ax			; setup video as source
	les	di,lpb			; setup far destination

	mov	cl,axRight		; load cx with #columns
	sub	cl,axLeft
	xor	ch,ch

	mov	al,ayTop
	sub	ayBottom,al		;* day
;	cld				-- already cleared
	push	ds
	mov	ds,instCur.psPrimInst
	assumes ds,nothing

savearc_loop:
	push	si
	push	cx

ifdef	SCREEN_FFONT
;*	* extra copy for FFONT
	cmp	fFontAvailable,0
	je	@F
	push	ds				;* ds=> prim buffer
	AssertNE instCur.psSecInst,0
	mov	ds,instCur.psSecInst
	push	si				;* si is correct
	push	di				;* must adjust to 2nd part
	add	di,dbSec
	push	cx				;* save count
	rep	movsw
	pop	cx
	pop	di
	pop	si
	pop	ds
@@:
endif	;SCREEN_FFONT

ifdef	BUILTIN_SNOW
	push	dx
	mov	dx,3DAh			;* CGA video status port
SnowL2:
	StartDrawCrit
	movsw
	EndDrawCrit
	loop	SnowL2
	pop	dx
ELSE	;NOT DEFINED BUILTIN_SNOW
	rep	movsw
endif	;BUILTIN_SNOW

	pop	cx
	pop	si
	add	si,axMacTimes2		; point to next row
	dec	ayBottom		;* day
	jnz	savearc_loop

	pop	ds
	assumes ds,dgroup

	cCall	FEnableMouse,<fMouseOn>	;* restore mouse state

cEnd	SaveArc



;********** RestoreArc **********
;*	entry : axLeft, ayTop, axRight, ayBottom : rectangle
;*		lpb => buffer to restore data from (assumed to be right size)
;*	exit : n/a

cPrivate RestoreArc,<ATOMIC>,<SI,DI>

	parmB axLeft
	parmB ayTop				;* also used to save day
	parmB axRight
	parmB ayBottom
	parmD lpb

ifdef	SCREEN_FFONT
	localW dbSec			;* secondary - primary (in lpb)
endif	;SCREEN_FFONT

	localW fMouseOn				;* FEnableMouse old state
	localB dayT
cBegin	RestoreArc

	AssertUp

	mov	al,axMac
	cmp	axRight,al
	jbe	@F
	mov	axRight,al
@@:

ifdef	SCREEN_FFONT
;*	* calculate the dbSec (ayBottom-ayTop)*(axRight-axLeft)*2
	mov	al,ayBottom
	sub	al,ayTop
	mov	ah,axRight
	sub	ah,axLeft
	mul	ah			;* ax = cch
	shl	ax,1
	mov	dbSec,ax
endif	;SCREEN_FFONT

	xor	ax,ax
	cCall	FEnableMouse,<ax>		;* turn mouse off
	mov	fMouseOn,ax

	CalcCoord axLeft,ayTop
	mov	di,ax
	mov	offDrawFirst,ax
	mov	es,instCur.psPrimInst
	push	ds
	lds	si,lpb			; setup pb as source
	assumes ds,nothing
	
	mov	cl,axRight		; load cx with #columns
	sub	cl,axLeft
	xor	ch,ch

	mov	al,ayTop
	sub	ayBottom,al		;* ayBottom == day
	mov	al,ayBottom
	mov	dayT,al			;* save for second use
;	cld				-- already cleared

restorearc_loop:
	push	cx
	push	di

ifdef	SCREEN_FFONT
;*	* extra copy for FFONT
	cmp	fFontAvailable,0
	je	@F
	push	es				;* es=> prim buffer
	AssertNE instCur.psSecInst,0
	mov	es,instCur.psSecInst
	push	di				;* di is correct
	push	si				;* must adjust to 2nd part
	add	si,dbSec
	push	cx				;* save count
	rep	movsw
	pop	cx
	pop	si
	pop	di
	pop	es
@@:
endif	;SCREEN_FFONT

ifdef	BUILTIN_SNOW
	push	dx
	mov	dx,3DAh			;* CGA video status port
SnowL3:
	StartDrawCrit
	movsw
	EndDrawCrit
	loop	SnowL3
	pop	dx
ELSE	;NOT DEFINED BUILTIN_SNOW
	rep	movsw
endif	;BUILTIN_SNOW

	pop	di
	pop	cx
	add	di,axMacTimes2		;* point to next row
	dec	ayBottom		;* day
	jnz	restorearc_loop
	
	pop	ds			;* restore DS since part of far pointer

;*	* inform the screen driver that we have invalidated a lot
;*	* cx = dax
	mov	si,cx
	mov	di,offDrawFirst
loop_inform_restore:
	cCall	insj.lpfnDoUpdateCsdInsj, <ayTop, axLeft, si, di, fRestoreDbcs>
	add	di,axMacTimes2		;* point to next row
	inc	ayTop
	dec	dayT
	jnz	loop_inform_restore

;*	* all done, refresh
	cCall	insj.lpfnDoneUpdateCsdInsj		;* ()

	cCall	FEnableMouse,<fMouseOn>	;* restore mouse state

cEnd	RestoreArc

sEnd	SCREEN

;----------------------------------------------------------------------------

	END

