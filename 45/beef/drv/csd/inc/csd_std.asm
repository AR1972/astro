;*
;*	CW : Character Windows
;*
;*	csd_std.asm : standard defaults
;*	(not machine specific)

ifndef ImodeGuessCurrentCsd_NonDefault
;*****************************************************************************
;********** ImodeGuessCurrentCsd **********
;*	* CSD entry point (see documentation for interface)

cProc	ImodeGuessCurrentCsd, <FAR, PUBLIC, ATOMIC>, <SI,DI>
cBegin	ImodeGuessCurrentCsd

	mov	di,OFF_lpwDataCsd

	cCall	FvmGetCur		;* get fvm

	push	ax
	cCall	ModeGetCur		;* al = mode, ah = ayMac (0=>unknown)
	pop	bx			;* bx = fvm

;*	* Search for current mode and fvm in rgdm

	mov	si,drvOffset rgdm
	mov	cx,cdmMax
	xor	dx,dx

;*	* al = current mode, ah = ayMac (or 0=>unknown)
;*	* bx = fvm
;*	* si = pdm
;*	* dx = idm = imode
;*	* cx = loop count


imgc_next:
	cmp	al,cs:[si].modeDm
	jne	@F
	test	bl,cs:[si].fvmReqAdapDm
	jz	@F				
	test	bh,cs:[si].fvmReqDispDm
	jz	@F			;* not available
	or	ah,ah
	jz	imgc_end		;* height unknown => use this one
	cmp	ah,cs:[si].ayMacDm
	jz	imgc_end		;* same height => use this one

@@:
	add	si,size DM
	inc	dx
	loop	imgc_next
	mov	dx,-1			;* unknown
imgc_end:	;* dx = imode
	mov	ax,dx			;* guess this mode

cEnd	ImodeGuessCurrentCsd
;*****************************************************************************
endif	;* ImodeGuessCurrentCsd_NonDefault


ifndef FQueryInstCsd_NonDefault
;*****************************************************************************
;********** FQueryInstCsd **********
;*	* CSD entry point (see documentation for interface)

cProc	FQueryInstCsd, <FAR, PUBLIC, ATOMIC>, <si, di>
    parmDP pinst
    parmW imode
    localW fvm
cBegin	FQueryInstCsd

	mov	di,OFF_lpwDataCsd
	cCall	FvmGetCur	;* find out what we got ...
				;* will query hardware at first call
	mov	fvm,ax

	mov	si,imode
	cmp	si,cdmMax
	jb	got_imode
fail_query:
	xor	ax,ax			;* failure
	jmp	end_qmode

got_imode: ;* si = imode
	mov	ax,SIZE DM
	mul	si
	mov	si,ax
	add	si,drvOffset rgdm		;* CS:SI => INST info

;*	* copy DM info into INST
	mov	di,pinst			;* ds:di => dest
;*	* clear out the INST structure
	push	di
	push	ds
	pop	es
	mov	cx,cbInstMin / 2
	xor	ax,ax
	rep stosw
	pop	di

;*	* move information from DM to INST
	;* finst
	mov	ax,fvm			;* al=adapter, ah=monitor
	mov	dx,cs:[si].finstDm
	test	al,cs:[si].fvmReqAdapDm
	jz	@F
	test	ah,cs:[si].fvmReqDispDm
	jz	@F
	or	dx,finstAvailable	;* this mode is currently available
@@: ;* dx = finst
IFDEF BUILTIN_SNOW
;*	* KLUDGE: set finstQuestionable for a builtin CGA.
	test	al,fvmCGA
	jz	@F
	or	dx,finstQuestionable
@@:
ENDIF ;BUILTIN_SNOW
	mov	ds:[di].finstInst,dx

IFDEF EARLIER
	Assert	<ayMacDm EQ axMacDm+1>
	Assert	<ayMacInst EQ axMacInst+1>
	;* axMac, ayMac
	mov	dx,word ptr cs:[si].axMacDm
ELSE
	mov	dl,cs:[si].axMacDm
	mov	dh,cs:[si].ayMacDm
ENDIF
	mov	wo ds:[di].axMacInst,dx		;* move both axMac and ayMac

	;* mode index
	mov	dx,imode
	mov	ds:[di].imodeInst,dx

	;* coMac, covMac, coiMac
	mov	dl,cs:[si].coMacDm
	mov	ds:[di].coMacInst,dl
	cCall	CoiCovFromFvm			;* al = fvm
	mov	ds:[di].covMacInst,ah
	mov	ds:[di].coiMacInst,dx

	;* INFT information
	Assert	<dyCharDm EQ dxCharDm+1>
	Assert	<dyCharInft EQ dxCharInft+1>
	mov	dx,word ptr cs:[si].dxCharDm
	mov	word ptr ds:[di].inftInst.dxCharInft,dx
					;* move both dxChar and dyChar
	mov	dl,cs:[si].dyBaseDm
	mov	ds:[di].inftInst.dyBaseLineInft,dl
	mov	dl,cs:[si].ifontDm
	mov	ds:[di].inftInst.ifontInft,dl

	mov	dx,0FFFFh			;default ffont values
	mov	ds:[di].ffontSupportedInst,dx

	;* Buffer info
	mov	ax,cs:[si].psVideoDm
	mov	ds:[di].psPrimInst,ax
	AssertEQ ds:[di].psSecInst,0
	AssertEQ ds:[di].cwExtraInst,0

;*	* set private info (store pointer to DM in the INST structure)
	mov	[di].pdmInst,si

	mov	ax,sp				;* ok
end_qmode:

cEnd	FQueryInstCsd


;*****************************************************************************
endif	;* FQueryInstCsd_NonDefault


ifndef TermCsd_NonDefault
;*****************************************************************************
;********** TermCsd **********
;*	* CSD entry point (see documentation for interface)
;*	* normally a no-op

cProc	TermCsd, <FAR, PUBLIC, ATOMIC>
cBegin	TermCsd
ifdef	KANJI
	cCall	EnableBlinkBit
endif	; KANJI
cEnd	TermCsd
;*****************************************************************************
endif	;* TermCsd_NonDefault
