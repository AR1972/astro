;*
;*	CW : Character Windows
;*
;*	csd_ibm.asm : contains default routines "IBM and compatible" routines
;*

;*****************************************************************************


ifndef ModeGetCur_NonDefault
;********** ModeGetCur *********
;*	entry:	n/a
;*	* get current machine mode
;*	exit:	al = mode, ah = ayMac (or 0 if unknown)

cProc	ModeGetCur, <NEAR, PUBLIC, ATOMIC>, <ES>
cBegin	ModeGetCur

	AssertEQ di,OFF_lpwDataCsd
	mov	dl,25			;default 25 rows for CGA
	cmp	[di].fvmCurAdap,fvmCGA	;single card case
	je	@F
	mov	ax,40H
	mov	es,ax
	mov	dl,es:[0084H]		;* read BIOS rows
	inc	dl			;* dl = screen height
@@:
	push	bx
	mov	ah,0fh
	int	10h			;* get current state, return al = mode
	pop	bx
	mov	ah,dl

cEnd	ModeGetCur
endif	;* ModeGetCur_NonDefault



ifndef FvmGetCur_NonDefault
;*****************************************************************************
;********** FvmGetCur **********
;*	entry:	DS:DI => driver data
;*	* if first time called, identify the current screen and set fvmCur
;*	*  subsequent calls will return fvmCur
;*	*  After this call: fvmCur is initialized
;*	exit:	AL == adapter (0 => no supported screen found)
;*		AH == monitor
cProc	FvmGetCur, <NEAR, PUBLIC, ATOMIC>
cBegin	FvmGetCur

	AssertEQ di,OFF_lpwDataCsd
	mov	al,[di].fvmCurAdap
	mov	ah,[di].fvmCurDisp
	or	ax,ax
	jz	init_get
	jmp	end_fvm_get
init_get:	;* initial get

	push	bp			; Int10's on some machines crunch bp.

	mov	ax,1A00h		; Test for MCGA and VGA.
	int	10h
	cmp	al,1Ah
	jne	notPS2			; Jump if not PS/2 (MCGA or VGA)

	mov	ax,fvmVGA
	cmp	bl,7
;	je	@F
	je	found_EGA
	cmp	bl,8
	je	found_EGA
;	je	@F
	mov	ax,fvmMCGA
	cmp	bl,0Bh
	je	@F
	cmp	bl,0Ch
	je	@F
	xor	ax,ax			;it's PS2, but currently inactive
	jmp 	found_EGA		;go check for MDA/CGA
@@:
	jmp	found_fvm

notPS2:
	mov	ah,12h			;Test for EGA
	mov	bl,10h
	int	10h
	xor	dx,dx			; see below (CGA detection)
	cmp	bl,10h
	je	notEGA
;
; Now, figure out if the EGA is the active display.
;
	int	11H			;Get the BIOS equipment flags
	and	al, 00110000B		;
	add	al, 11010000B		;Carry flag is set only if
					;   a mono monitor is active.
	sbb	bh,0			;Result is zero only if the
					;   ega is the active adapter
	jnz	notEGA			

	mov	ax,fvmEGAM		; Test for a monochrome EGA display
	cmp	cl,0BH	 
	je	found_EGA
	cmp	cl,0AH
	je	found_EGA
	cmp	cl,04H
	je	found_EGA
	cmp	cl,05H
	je	found_EGA

	mov	ax,fvmEGA		; It's got more than 64K of RAM
	cmp	cl,3			; HiRes display for settings 9 & 3
	je	found_EGA
	cmp	cl,9
	je	found_EGA
	mov	ax,fvm64KEGA		;UNDONE - Is this correct

found_EGA:
	or	bl,bl			; Test for 64K of EGA RAM
	jnz	@F
	or	ax,fvm64KEGA
@@:
	mov	dx,ax			; allow co-exist of EGA and CGA/MDA
notEGA:					
; Test for MDA
	push	bx
	mov	ah,0fh			
	int	10h
	pop	bx
	cmp	al,7			; 7 == monochrome text mode
	jne	@F
	or	dx,fvmMDA		; assume it's an MDA	
	mov	ax,dx
	jmp	found_fvm
@@:
; Test for CGA (Note that Hercules will pass the CGA 6845 test so 
; it is put after the MDA test.)
	test	dx,fvmEGAM		
	jz	@F
; this test used in dual card config
	cmp	al,2			;al from previous int10 call
	je	jCGA
	cmp	al,3
	je	jCGA
	cmp	al,6
	je	jCGA
	mov	al,dl
	jmp	found_fvm
@@:
	push	dx
	mov	dx,3D4h			;CGA's 6845 CRTC port 3D4h
	mov	al,0Fh			
	out	dx,al			;register 0Fh (Cursor Low)
	inc	dx
	in	al,dx
	mov	ah,al			;preserve Cursor Low value in AH
	mov	al,66h			;send aributrary value
	out	dx,al
	mov	cx,7FFFh
L102:	loop	L102			;delay
	in	al,dx
	xchg	ah,al
	out	dx,al			;restore Cursor Low 
	pop	dx
	xor	ah,al			;AH=AL if CGA present
	jnz	@F
jCGA:
	or	dx,fvmCGA
	mov	ax,dx
	jmp	found_fvm
@@:
	mov	ax,dx
	or	ax,ax			; ax!=0 == card identified
	jnz	found_fvm		

; If no display card identified then use default CGA	
	mov	ax,fvmCGA		

found_fvm:	;* al = fvm
	AssertEQ [di].fvmCurAdap,0
	mov	dl,al
	mov	ah,0FAh 		; Find out if there is an EGA.SYS
	xor	bx,bx			; installed
	int	10h
	or	bx,bx
	jz	@F			; Jump if there isn't.
	or	dl,fvmMouse		; EGA.SYS is there (mirror registers)
@@:
	mov	al,dl
	mov	[di].fvmCurAdap,al
	mov	ah,0FFh			; assume no monitor chk
	test	al,fvmEGAM or fvmVGA or fvmMCGA	or fvm64KEGA or fvmEGA
	jz	NoDispCheck

	push	bx    			;* check monochrome
	test	al,fvmMCGA
	jz	@F

	push	ax			;save al (adapter info)
	mov	ax,1A00h		;read display combination code
	int	10h
	cmp	al,1Ah	
	pop	ax
	jne	ColorMode
	
	mov	ah,fvmECD or fvmCD
	cmp	bl,0Ch			;analog color ?
	je	ColorMode
	mov	ah,fvmMD		;analog mono (0Bh) 
	jmp	ColorMode
@@:	
;* EGA/VGA
	test	al,fvmCGA or fvmMDA	;MDA/EGA or CGA/EGA co-exist ?
	jnz	ColorMode		;if dual-card then enble all monitors
	push	ax			;save al (adapter info)
	mov	ah,12h
	mov	bl,10h
	int	10h	
	pop	ax
	mov	ah,fvmMD
	or	bh,bh			;0 = color, 1 = mono
	jnz	ColorMode
	mov	ah,fvmCD
	and	cl,00000110b		;switches 2 and 3 
	jnz	ColorMode		;off?
	mov	ah,fvmECD		;on.
ColorMode:
	pop	bx		
NoDispCheck:	
	mov	[di].fvmCurDisp,ah		;
	pop	bp

end_fvm_get: ;* ax = fvm

cEnd	FvmGetCur

;*****************************************************************************
endif	;* FvmGetCur_NonDefault


ifndef CoiCovFromFvm_NonDefault
;*****************************************************************************
;********** CoiCovFromFvm **********
;*	entry:	AL = fvm
;*	* Produce coiMac, covMac for fvm (AL)
;*	exit:	AL = fvm, AH = covMac, DX = coiMac

cProc	CoiCovFromFvm, <NEAR, PUBLIC, ATOMIC>
cBegin	CoiCovFromFvm

	xor	ah,ah
	xor	dx,dx
	test	al,fvmMDA		;* monochrome coiMac = 0, covMac = 0
	jnz	@F
	test	al,fvmCGA		;* CGA coiMac = 0, covMac = 0
	jnz	@F
	mov	ah,64
	test	al,fvmVGA		;* EGA coiMac = 0, covMac = 64
	jz	@F
	push	ax
	mov	ax,101ah		;* read color page state
	int	10h
	pop	ax
	mov	dx,64			;* VGA coiMac = 64, covMac = 64?
	or	bl,bl
	jz	@F
	mov	ah,16			;* VGA coiMac = 64, covMac = 16
@@:

cEnd	CoiCovFromFvm
;*****************************************************************************
endif	;* CoiCovFromFvm_NonDefault



ifndef GetPFonts_NonDefault
;*****************************************************************************
;********** GetPFonts **********
;*	entry:	AL = ifont
;*	* get font information
;*	exit:	DX:AX = lpbFont for first 128 characters
;*		CX:BX = lpbFont for last 128 characters

cProc	GetPFonts, <NEAR, PUBLIC, ATOMIC>
cBegin	GetPFonts


cEnd	GetPFonts
;*****************************************************************************
endif	;* GetPFonts_NonDefault


ifndef FInitCsd_NonDefault
;*****************************************************************************
;********** FInitCsd **********
;*	* CSD entry point (see documentation for interface)
;*	* Initialize the screen to the given mode
;*	exit:	AX != 0 if ok

cProc	FInitCsd, <FAR, PUBLIC, ATOMIC>, <DI>
    parmDP pinst
    parmDP pinch
cBegin	FInitCsd

	mov	di,OFF_lpwDataCsd		;* Data in data segment

;*	* set mode
	mov	bx,pinst
	mov	[di].pinstDrv,bx
	mov	bx,ds:[bx].pdmInst		;* CS:BX => DM info

;*	* copy mode info into driver globals
	mov	ax,cs:[bx].vparmCursOnDm
	mov	[di].vparmCursOn,ax		;initialize underline cursor
	mov	[di].vparmCursSize,ax
	mov	ax,cs:[bx].wExtraDm
	mov	[di].wExtra,ax

	mov	ah,0fh
	int	10h				;* return al = mode
	and	al,7Fh				;* mask off clr vid buff bit.
	mov	cl,al				;* current mode

	xor	ah,ah				;* set mode
	mov	al,cs:[bx].modeDm
	cmp	al,cl
	je	@F				;* don't reset
	int	10h				;* set mode
@@:
	mov	ax,1111h			;default 8x14 font
	push	bx
	cmp	cs:[bx].ayMacDm,25		;80x25 text ?
	mov	bx,0E00h
	je	InitFont

	inc	al				;load 8x8 char into RAM
	mov	bh,08

InitFont:
	int	10h				
	pop	bx

;*	* the INCH array already contains the standard Code Page 437
;*	*  character set, so it usually can be left the same.

;*	* Do other diddling
	cCall	DiddleBlinkBit

	mov	ax,sp				;* success
cEnd	FInitCsd
;*****************************************************************************
endif	;* FInitCsd_NonDefault



ifndef DiddleBlinkBit_NonDefault
;*****************************************************************************
;********** DiddleBlinkBit **********
cProc	DiddleBlinkBit, <NEAR, ATOMIC, PUBLIC>, <DS>
cBegin	DiddleBlinkBit

;*	* Diddle blink bit via BIOS call (or diddle CGA bit)

	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	test	[bx].fvmCurAdap, fvmCGA or fvmMDA
	jz	DBB_NotCGA

	xor	ax,ax
	mov	ds,ax
	mov	dx,ds:[BIOS_addr_6845]
	add	dx,PORT_mode
;*	* diddle the mode set flag
	and	ds:[BIOS_crt_mode_set],NOT 20H
	mov	al,ds:[BIOS_crt_mode_set]
	out	dx,al				;* send to port

;Fall through because fvmEGAM is also marked as fvmMDA
;   and the above OUT would not work

;*	* EGA etc has a BIOS call for this
DBB_NotCGA:
	mov	ax,1003H			;* set intensify
	xor	bx,bx				;* intensify
	int	10h
cEnd	DiddleBlinkBit
;*****************************************************************************
endif	;* DiddleBlinkBit_NonDefault



ifndef MoveHwCursCsd_NonDefault
;*****************************************************************************

;********** MoveHwCursCsd **********
;*	* CSD entry point (see documentation for interface)
;*	* Standard BIOS call for hardware cursor
;*	* save new position in "posCurs"

cProc	MoveHwCursCsd,<FAR, PUBLIC, ATOMIC>, <DI>
    parmB axCurs
    parmB ayCurs
    parmW fOn
    localB axCursor
    localB ayCursor
cBegin	MoveHwCursCsd

	mov	di,OFF_lpwDataCsd		;* Data in data segment

;*	* move cursor position
	xor	bh,bh
	mov	dh,ayCurs
	mov	dl,axCurs

	mov	ah,2
	int	10h				;* SetCursorPosition

	mov	cx,2000H			;* assume off
	mov	byte ptr [di].fCurs,cl		;* graphics cursor 
	cmp	fOn,0
	je	SetCursorOnOff			;* turn off

	mov	dx,[di].vparmCursSize
	cmp	fOn,2
	jne	@F
	xor	dh,dh				;(fOn = 2) Set block cursor
@@:
	mov	[di].vparmCursOn,dx		;* update cursor size
	mov	bx,[di].pinstDrv
	test	ss:[bx].finstInst,finstGraphics	;graphics text 8x14
	jz	DrawCursDone

	mov	dx,[di].posCurs		;dl = axFirst
	mov	ayCursor,dh
	mov	axCursor,dl
	xor	cx,cx			;fRestoreDbcs
	mov	ah,cl
	mov	al,ss:[bx].axMacInst
	mul	dh
	mov	dh,cl
	add	ax,dx			
	shl	ax,1			;ax = offFirst
	mov	dx,1
	cCall	<Near ptr DoUpdateCsd>, <ayCursor,axCursor,dx,ax,cx,cs>	;erase cursor
	xor	cx,cx
	mov	dh,ayCurs
	mov	dl,axCurs
	mov	[di].posCurs,dx		;update position
	mov	ah,cl
	mov	bx,[di].pinstDrv
	mov	al,ss:[bx].axMacInst
	mul	dh
	mov	dh,cl
	add	ax,dx
	shl	ax,1			;ax = offFirst
	mov	dx,1
	mov	byte ptr [di].fCurs,1		;* graphics cursor on
	cCall	<Near ptr DoUpdateCsd>, <ayCurs,axCurs,dx,ax,cx,cs>		;draw cursor

DrawCursDone:
	mov	cx,[di].vparmCursOn		;* turn on
		
SetCursorOnOff:
	mov	ah,1
	int	10h				;* SetCursorType

cEnd	MoveHwCursCsd

;*****************************************************************************
endif	;* MoveHwCursCsd_NonDefault


ifndef FGetColorPaletteCsd_NonDefault
;*****************************************************************************
;********** FGetColorPaletteCsd **********
;*	* CSD entry point (see documentation for interface)

cProc	FGetColorPaletteCsd, <FAR, PUBLIC, ATOMIC>,<DI,SI>
    parmW  coGet
    parmDP pcovGet
    parmDP pcoiGet
cBegin	FGetColorPaletteCsd

	mov	di,OFF_lpwDataCsd

	cCall	FvmGetCur
	mov	di,[di].pinstDrv
	mov	bx,coGet
	cmp	bl,[di].coMacInst
	jae	fgcpc_fail		;* color number does not exist
	mov	cx,pcoiGet
	test	al,fvmVGA
	jz	fgcpc_notVGA

;*	* VGA

	mov	ax,1007h		;* palette, read palette value
	int	10h

;*	* save cov value

	xchg	bl,bh
	xor	bh,bh
	mov	si,pcovGet
	mov	[si],bx

	jcxz	fgcpc_success		;* not interested in RGB info

;*	* get RGB info

	cCall	IcrFromCov
	mov	ax,1015h		;* palette, read color register
	int	10h
	push	ss
	pop	es
	xor	ah,ah
	mov	di,pcoiGet
	mov	al,dh
	stosw
	mov	al,ch
	stosw
	mov	al,cl
	stosw
	jmp	short fgcpc_success

fgcpc_notVGA:
	jcxz	fgcpc_getcov

fgcpc_fail:
	xor	ax,ax
	jmp	short fgcpc_done

fgcpc_getcov:
	test	al,fvmMouse		;* check for ega.sys
	jz	fgcpc_fail		;* not available

;*	* EGA.SYS

	mov	ah,0f0h			;* read one register
	mov	dx,18h
	int	10h
	mov	si,pcovGet
	mov	[si],bx

fgcpc_success:
	mov	ax,sp

fgcpc_done:

cEnd	FGetColorPaletteCsd


;********** IcrFromCov **********
;*	entry:	bl = cov
;*	Determine color register number for cov.
;*	exit:	bx = icr (color register index)
;*	TRASHES: ax,cx,dx

cProc	IcrFromCov, <NEAR, PUBLIC, ATOMIC>
cBegin	IcrFromCov

	push	bx
	mov	ax,101ah		;* palette, read color page mode
	int	10h
	pop	dx
	xchg	bl,bh
	mov	cl,6
	or	bh,bh			;* check page mode for 6 or 4 bit
	jz	ifc_makeicr

	dec	cl
	dec	cl
	jmp	short ifc_makeicr

ifc_makeicr:

	shl	bl,cl
	or	bl,dl
	xor	bh,bh

cEnd	IcrFromCov
;*****************************************************************************
endif	;* FGetColorPaletteCsd_NonDefault


ifndef SetColorPaletteCsd_NonDefault
;*****************************************************************************
;********** SetColorPaletteCsd **********
;*	* CSD entry point (see documentation for interface)

cProc	SetColorPaletteCsd, <FAR, PUBLIC, ATOMIC>,<DI,SI>
    parmW  coSet
    parmW  covSet
    parmDP pcoiSet
cBegin	SetColorPaletteCsd

	mov	di,OFF_lpwDataCsd

	cCall	FvmGetCur
	mov	di,[di].pinstDrv
	mov	bx,coSet
	cmp	bl,[di].coMacInst
	jae	scpc_done		;* color number does not exist

	;* EGA.SYS f1 and EGA 1000h calls use bl == co, bh == cov

	mov	cx,covSet
	mov	bh,cl
	AssertCmp bh,L,[di].covMacInst	;* check range

	test	al,fvmVGA		;* check for VGA
	jnz	scpc_RGB

scpc_cov:
IFNDEF NEWEGASYS
;* This code is required until we get the new EGA.SYS
;*  Taken from QINTER\mcload.asm
	test	al,fvmMouse		;* check for EGA.SYS
	jz	scpc_notEGASYS
	mov	dx,3dah
	in	al,dx
	mov	ah,0f1h
	mov	dx,18h
	int	10h
	mov	dx,3c0h
	mov	al,20h
	out	dx,al
	jmp	short scpc_done
scpc_notEGASYS:
ENDIF ;!NEWEGASYS
	mov	ax,1000h		;* set palette registers, individual
	jmp	short scpc_int10

;*	* RGB color palette

scpc_RGB:
	mov	cx,[di].coiMacInst
	jcxz	scpc_cov		;* not an RGB system
	mov	cx,pcoiSet
	jcxz	scpc_cov		;* not interested in RGB
	push	cx
	mov	ax,1007h		;* palette, read palette value
	int	10h
	mov	bl,bh
	cCall	IcrFromCov
	pop	si			;* si = pcoiSet
IFDEF DEBUG
	mov	ax,[si]
	AssertCmp ax,L,[di].coiMacInst	;* check ranges
	mov	ax,[si+2]
	AssertCmp ax,L,[di].coiMacInst
	mov	ax,[si+4]
	AssertCmp ax,L,[di].coiMacInst
ENDIF ;DEBUG
	mov	ax,1010h		;* set color register
	mov	dh,[si]
	mov	ch,[si+2]
	mov	cl,[si+4]

scpc_int10:
	int	10h

scpc_done:

cEnd	SetColorPaletteCsd

;*****************************************************************************
endif	;* SetColorPaletteCsd_NonDefault


ifndef FQueryInftCsd_NonDefault
;*****************************************************************************
;********** FQueryInftCsd **********	(assume called after FQueryInst)
;*	entry:	pinft, ifont
;*	* get font info
;*	exit:	AX = 0 => no more fonts
;*		ax != 0	Success,filled INFT
;*		trash bx,dx

cProc	FQueryInftCsd, <FAR, PUBLIC, ATOMIC>, <si, di>
    parmDP pinft
    parmW  ifont
cBegin	FQueryInftCsd

	xor	ax,ax
	mov	si,ifont
	cmp	si,cdmMax
	jae	ExitFQueryInft			; no more fonts

	mov	ax,SIZE DM
	mul	si
	mov	si,ax
	add	si,drvOffset rgdm		;* CS:SI => INST info
	mov	di,pinft			;* ds:di => INFT
	Assert	<dyCharDm EQ dxCharDm+1>
	Assert	<dyCharInft EQ dxCharInft+1>
	mov	dx,word ptr cs:[si].dxCharDm
	mov	wo ds:[di].dxCharInft,dx
					;* move both dxChar and dyChar
	mov	dl,cs:[si].dyBaseDm
	mov	ds:[di].dyBaseLineInft,dl
	
	mov	ax,ifont
	mov	ds:[di].ifontInft,al		;font index

	mov	ax,sp

ExitFQueryInft:
cEnd	FQueryInftCsd

;*****************************************************************************
endif	;* FQueryInftCsd_NonDefault



ifndef GetCharMapCsd_NonDefault
;*****************************************************************************
;********** GetCharMapCsd **********
;*	entry:	pinft, ch, pbitmap
;*	* get character bit map

cProc	GetCharMapCsd, <FAR, ATOMIC, PUBLIC>
    parmDP pinft
    parmB  char
    parmDP pbitmap    
cBegin	GetCharMapCsd

cEnd	GetCharMapCsd
;*****************************************************************************
endif	;* GetCharMapCsd_NonDefault
