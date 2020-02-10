
;*
;*	CW : Character Windows
;*
;*	ega.asm : DOS3 TWIN compatible screen driver
;*
;*****************************************************************************

;*	* Special screen save
NonStandard	CbSizeVidsCsd
NonStandard	FSaveVidsCsd
NonStandard	FRestoreVidsCsd
NonStandard	SetVideoMode
NonStandard	SaveVidDataCsd
NonStandard	RestoreVidDataCsd
NonStandard	EnableVidsMonitorCsd

;********** CbSizeVidsCsd **********
;*	* CSD entry point (see documentation for interface)

cProc	CbSizeVidsCsd, <FAR, PUBLIC, ATOMIC>
cBegin	CbSizeVidsCsd

	mov	ax,SIZE EGA_VIDS
	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	test	[bx].fvmCurAdap, fvmEGA or fvmVGA or fvm64KEGA or fvmEGAM or fvmMCGA
	jnz	notCGAVids

	sub	ax,24				;for EGA palette settings
	jmp	short CbSizeExit
notCGAVids:
	test	[bx].fvmCurAdap, fvmMCGA or fvmVGA
	jz	CbSizeExit

	add	ax,255*3			;for PS2 palette settings
CbSizeExit:
	inc	ax				;get even cb
	and	ax,0FFFEh      
cEnd	CbSizeVidsCsd


;********** FSaveVidsCsd ********
;*	entry:	pvidsSave = near pointer to VIDS structure
;*		pinst = near pointer to INST for new mode
;*	* fill *pvidsSave with state of current screen mode (not screen data)
;*	exit:	AX != 0 if ok, == 0 if error

cProc	FSaveVidsCsd, <FAR, PUBLIC, ATOMIC>, <si, di>
    parmDP pvidsSave
    parmDP pinst
cBegin	FSaveVidsCsd

	mov	di,pvidsSave

	mov	ah,0Fh
	int	10h				;* GetMode and page
	and 	al,07Fh				; clear msb
	mov	[di].modeVids,al
	mov	[di].pageVids,bh

	mov	[di].fClearRegenVids,00H
				      
	mov	ax,SIZE EGA_VIDS - cbVidsMin	;default EGA
	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	test	[bx].fvmCurAdap, fvmEGA or fvmVGA or fvm64KEGA or fvmEGAM or fvmMCGA
	jnz	notFSCGA
	
	sub	ax,24			;EGA palette regs
	jmp	short notFSPS2
notFSCGA:
	test	[bx].fvmCurAdap, fvmMCGA or fvmVGA
	jz	notFSPS2

	add	ax,255*3		;DAC color regs
notFSPS2:
	inc	ax			;get even cb
	and	ax,0FFFEh      
	shr	ax,1
	mov	[di].cwExtraVids,ax

	mov	ah,3
	int	10h				;* GetCursorPos
	mov	[di].vparmCursorVids,cx

	mov	ax,ds
	mov	es,ax
	mov	dx,di
	lea	di,[di].rgwCursorPosVids	;* save cursor positions here

	push	ds
	xor	ax,ax
	mov	ds,ax
	mov	si,BIOS_cursor_posn		;* 8 Cursor positions in BIOS

	mov	cx,8
	rep movsw
	pop	ds
	mov	di,dx			; restore di

public saveCGAPal			;UNDONE - remove these two lines
saveCGAPal:				;UNDONE
	;
	; Assumes bCGABg is immediately before bCGAPal
	; and bCGABgVids is immediately before bCGAPalVids
	;
	.errnz	(bCGABgVids+1) - bCGAPalVids
	.errnz	(bCGABg+1) - bCGAPal
	mov	ax, word ptr cs:[bCGABg]
	mov	word ptr [di].bCGABgVids, ax

	mov	dl, [di].modeVids
	cmp	dl,40H			;Olivetti
	jz	@F

	test	[bx].fvmCurAdap, fvmCGA
	jz	skipCGA
@@:

;* for CGA only
	xor	ax,ax
	mov	es,ax
	mov	al, byte ptr es:[466H]
	mov	[di].ayOverScanVids, al
	mov	ax, 80*25	; Maybe we can get by just saving what we use (4k b).
	cmp	dl, 2
	je	save_not_graphics
	cmp	dl, 3
	je	save_not_graphics
	mov	ax, 8*1024	; Must mode switch (4,5,6) so must save all of regen(16k b)
	cmp	dl, 40H		; Is this an Olivetti graphics screen mode?
	jne	@F
	add	ax, ax		; Yes, there is twice as much memory to save
@@:

save_graphics:
	and	[di].fvidsVids,not fvidsChAttr	;graphics modes 
save_not_graphics:	;* ax = # of words to swap
	mov	[di].cwSwapVids,ax
	jmp	EgaSS_Exit

skipCGA:
	test	[bx].fvmCurAdap, fvmEGA or fvmVGA or fvm64KEGA or fvmEGAM or fvmMCGA
	jz	@F
	cCall	EgaSavePalette		;save palette regs
@@:	
	push	bp
	mov	ax,1130h		; get info
	xor	bx,bx
	xor	cx,cx
	int	10h
	pop	bp

	inc	dl			; dl = scan lines
	mov	[di].ayMacVids,dl	; rows
	mov	[di].ayPointsVids,cl	; cl = number of lines in char

	cmp	[di].modeVids,4		;check simple graphics
	jb	notmode4to6

	cmp	[di].modeVids,6
	ja	notmode4to6

	mov	ax,8*1024	; Must mode switch (4,5,6) so must save all of regen(16k b)
	mov	[di].cwSwapVids,ax
	and	[di].fvidsVids,not fvidsChAttr	;graphics modes 
	jmp	EgaSS_Exit

notmode4to6:
	mov	ax,80			;default 80 columns
	mov	si,pinst
	cmp	dl,[si].ayMacInst	;rows
	ja	AboveNewAy

	mov	dl,[si].ayMacInst	;assume going into text mode
AboveNewAy:
	mul	dl			;columns * rows
	mov	cl,[di].modeVids
	cmp	cl, 08H
	ja	EgaSS_Hard
	cmp	cl, 07H
	jb	NotHerc

; We are in mode 7 or 8 so we might have a Hercules card.
	push	ax			;Save cwSwap
	mov	ah,0efH
	mov	dl, -1
	int	10H
	pop	ax			;restore cwSwap
	inc	dl
	jz	NotHerc			;DL not changed by INT 10 unless herc.
	mov	ax,8*1024		;Must save 8k words for herc
					;We would only have to save 4K words
					;If it weren't for Compaq's stupid
					;BIOS which always clears 16K bytes
					;when switching into mode 7.
	mov	[di].fClearRegenVids,80H
NotHerc:
	mov	[di].cwSwapVids,ax	
	mov	[di].fvidsVids,fvidsChAttr	;Text modes 
	jmp	EgaSS_Exit

; *** Graphics Text ***
; Ok, we are in one of the difficult graphics modes.
; We must save cLines*80 words from bit plane 0 and the same from bit plane 1
; and we must save 4k words from bit plane 2 (because the character
; generator gets loaded into this area when we switch to alpha mode).

EgaSS_Hard:
	mov	[di].cwSwapVids,ax	
	and	[di].fvidsVids,not fvidsChAttr	;graphics modes 
	mov	ax, 2*1024			; MCGA uses 2k words
	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	test	[bx].fvmCurAdap, fvmMCGA
	jnz	EgaSS_Alloc

	mov	ax,[di].cwSwapVids		; ax == cLines*cColumns words
	shl	ax,1				; for bit plane 0 and 1
EgaSS_Alloc:
	add	ax, 4*1024		; Save 4k words of bit plane 2
	test	[bx].fvmCurAdap, fvm64KEGA
	jz	EgaSS_Exit

; More memory gets trashed by the BIOS doing a mode reset on a 64k Ega.
; Note: On a 64k Ega, the 4k words starting at 16k of bit plane 2 also get
;       trashed by the BIOS for some reason (so we gotta save that too).

	add	ax,4*1024

EgaSS_Exit:
	mov	[di].cwVidDataVids,ax
	mov	ax,sp				;* success

cEnd	FSaveVidsCsd


;********** EgaSavePalette ********
;*
;*	entry: ds:[di] => Vids

cProc EgaSavePalette,<NEAR,PUBLIC>,<SI,DI,ES>
cBegin EgaSavePalette

	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	mov	ax,ds
	mov	es,ax
	test	[bx].fvmCurAdap, fvmMCGA or fvmVGA 	;PS2 ?
	jz	@F

	lea	dx, [di].rgbPS2PaletteVids		
	mov	ax, 1017H			; Read block of Palette regs
	xor	bx,bx				;    start at reg 0
	mov	cx, 256				;    read 256 regs
	int	10H
@@:
	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	test	[bx].fvmCurAdap, fvmEGA or fvmVGA or fvm64KEGA or fvmEGAM
	jz	@F

	push	ds
	pop	es
	lea	di, [di].rgbEgaPaletteVids	;es:[di] -> buffer
	push	cs
	pop	ds
	mov	si, DrvOffset EGAPaletteMirror
	mov	cx,17				;16 palette + overscan regs
	rep	movsb
	push	ss
	pop	ds				;restore ds
@@:
cEnd EgaSavePalette


;********** FRestoreVidsCsd ********
;*	entry:	pvidsRestore = near pointer to VIDS structure
;*	* restore video state with data in *pvidsRestore (not screen data)
;*	exit:	AX != 0 if ok, == 0 if error

cProc	FRestoreVidsCsd, <FAR, PUBLIC, ATOMIC>, <si, di>
    parmDP pvidsRestore
cBegin	FRestoreVidsCsd

	mov	di,pvidsRestore
	mov	al, [di].modeVids
	cmp	al, 2
	je	EgaFRS_Text

	cmp	al, 3
	je	EgaFRS_Text

	cmp	al, 40H				;[==] Olivetti
	je	EgaFRS_SimpleGraphics		;[==]

	cmp	al, 8
	ja	EgaFRs_HardGraphics
	cmp	al, 7
	jae	EgaFRS_Text

EgaFRS_SimpleGraphics:			;mode 4,5 & 6
	cCall	SetVideoMode
	jmp	EgaFRS_Common

EgaFRS_Text:	; mode 2,3 and 7
ifndef	KANJI
	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	cmp	[bx].fBlinkEnable,0
	je	@F				;trash ax,dx

	mov	[bx].fBlinkEnable,0		;disable Blink
	cCall	DiddleBlinkbit
	jmp short RestoreBlink
@@:
	mov	[bx].fBlinkEnable,1		;enable Blink
	cCall	EnableBlinkbit
RestoreBlink:
endif	; !KANJI
	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	test	[bx].fvmCurAdap, fvmVGA 	;VGA ?
	jz	EgaFRs_SetScanLines_done

	mov	al,2			;default 400 lines
	cmp	[di].ayMacVids,43     	; 43 line mode
	jne	useDefaultLine

	dec	al			;set 350 scan line

useDefaultLine:
	mov	ah,12h			;set vertical scan line
	mov	bl,30h
	int	10h			;takes effect on next mode set

EgaFRs_SetScanLines_done:
	cCall	SetVideoMode
	or	ax,ax			;mode set ?
	jz	EgaFRS_Common
	cmp	[di].ayPointsVids,8	;8x8 font ?
	jne	EgaFRS_Common

	mov	ax,1112h
	mov	bh,8
	xor	bl,bl
	int	10h
	jmp	EgaFRS_Common

EgaFRS_HardGraphics:
	cCall	ClearRegen
	cCall	SetVideoMode
	cmp	[di].ayPointsVids, 8
	jne	EgaFRs_NoLoad8

	mov	ax,1123h
	xor	bh,bh
	mov	bl,[di].modeVids
	sub	bl,0dh
	mov	dl,cs:[GraphicsModeRows+bx]
	xor	bl,bl
	int	10h

EgaFRs_NoLoad8:

EgaFRs_Common:
	mov	al,[di].pageVids
	mov	ah,5
	int	10h				;* SetPage

	mov	dx,di
	xor	ax,ax
	mov	es,ax
	lea	si,[di].rgwCursorPosVids
	mov	di,BIOS_cursor_posn
	mov	cx,8
	rep movsw
	mov	di,dx

	mov	bl,[di].pageVids
	xor	bh,bh
	shl	bx,1
	mov	dx,[di+bx].rgwCursorPosVids	;* get cursor pos for this page
	shr	bx,1
	mov	bh,bl
	mov	ah,2
	int	10h				;* SetCursorPos

	mov	cx,[di].vparmCursorVids
	mov	ah,1
	int	10h				;* SetCursor

	cmp	[di].modeVids,40H		;Don't EgaRestore palette for
	je	@F				; Olivetti
	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	test	[bx].fvmCurAdap, fvmEGA or fvmVGA or fvm64KEGA or fvmEGAM or fvmMCGA
	jz	@F

	cCall	EgaRestorePalette
@@:
public restoreCGAPal				;UNDONE - remove
restoreCGAPal:					;UNDONE - remove

	push	bx
	mov	bl, [di].bCGAPalVids
	or	bl,bl
	js	@F				;* skip if negative
	mov	bh,1
	mov	ah,0BH
	int	10H				;* set color palette
@@:
	mov	bl, [di].bCGABgVids
	or	bl,bl
	js	@F				;* skip if negative
	xor	bh,bh
	mov	ah,0BH
	int	10H				;* set background color
@@:
	pop	bx

	test	[bx].fvmCurAdap, fvmCGA 		;CGA ?
	jz	@F

	xor	ax,ax
	mov	es,ax
	mov	al, [di].ayOverScanVids
	mov	byte ptr es:[466H],al
	mov	dx, 3d9H
	out	dx, al
@@:
	mov	ax,sp				;* success

cEnd	FRestoreVidsCsd


;********** EgaRestorePalette ********
;*
;*
cProc	EgaRestorePalette,<NEAR,PUBLIC>,<SI,DI,ES>
cBegin  EgaRestorePalette

	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	mov	ax,ds
	mov	es,ax
	test	[bx].fvmCurAdap, fvmMCGA or fvmVGA 	;PS2 ?
	jz	@F

	lea	dx, [di].rgbPS2PaletteVids		
	mov	ax, 1012H			; write block of Palette regs
	xor	bx,bx				;    start at reg 0
	mov	cx, 256				;    read 256 regs
	int	10H

@@:
	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	test	[bx].fvmCurAdap, fvmEGA or fvmVGA or fvm64KEGA or fvmEGAM
	jz	@F

	cmp	cs:[lpbBIOSINT10_Offset],0	;save check
	je	@F
	
	mov	ax,1002h
	lea	dx, [di].rgbEGAPaletteVids	;es:[dx] -> buffer
	int	10h
@@:	
cEnd	EgaRestorePalette


;***********************
cProc	EnableBlinkBit, <NEAR, ATOMIC, PUBLIC>, <DS>
cBegin	EnableBlinkBit

;*	* Diddle blink bit via BIOS call (or diddle CGA bit)

	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	test	[bx].fvmCurAdap, fvmCGA or fvmMDA
	jz	EBB_NotCGA

	xor	ax,ax
	mov	ds,ax
	mov	dx,ds:[BIOS_addr_6845]
	add	dx,PORT_mode
;*	* set the mode set flag
	or	byte ptr ds:[BIOS_crt_mode_set],20H
	mov	al,ds:[BIOS_crt_mode_set]
	out	dx,al				;* send to port

;*	* EGA etc has a BIOS call for this
EBB_NotCGA:
	mov	ax,1003H			;* set intensify/blink
	mov	bl,1				;* enable blink
	int	10h

cEnd	EnableBlinkBit


;********** SetVideoMode **********
;*	entry:	al = video mode
;*	* Set specified video mode
;*	exit:	trash ax,cx,es
;*	ax = 0 same mode,-1 mode set

cProc	SetVideoMode,<NEAR>,<es>
cBegin	SetVideoMode
	mov	ah,0Fh
	int	10h				;* GetMode
	and	al,7fh
	cmp	al,[di].modeVids
	jne	SVM_SetMode	    		;reset

;same mode, check rows		
	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	test	ss:[bx].fvmCurAdap, fvmCGA	;CGA only support 25 rows
	jnz	mode_already_set
	push	bp
	mov	ax,1130h		; get info
	xor	bx,bx
	xor	cx,cx
	int	10h
	pop	bp
	inc	dl			; dl = scan lines
	cmp	dl,[di].ayMacVids	; rows
	jne	SVM_SetMode
	jmp	mode_already_set

SVM_SetMode:
	mov	al,[di].modeVids
	or	al,[di].fClearRegenVids
	
	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	test	ss:[bx].fvmCurAdap, fvmEGA or fvmVGA or fvm64KEGA or fvmMCGA or fvmEGAM
	jz	TrashRegen	;CGA doesn't support the function below
	cmp	al, 40H		;Olivetti doesn't support it either.
	je	TrashRegen

	or	al,80h				;* don't clear RGEN

TrashRegen:
	xor	ah,ah
	push	ax				;save mode
	int	10h				;* set mode
	pop	ax				;restore
;*	* Delay so that we won't start writting to the display before it has
;*	* settled down.
	mov	cx,500
delay_me:
	loop	delay_me

;
; When the olivetti is in mode 40H the value at BIOS_info is a pointer to
; the character set (NOT flags as it is in EGA modes)
; So let's not trash the pointer by clearing the "don't clear regen" bit.
;
	cmp	al, 40H
	je	@F

	test	ss:[bx].fvmCurAdap, fvmEGA or fvmVGA or fvm64KEGA or fvmMCGA or fvmEGAM
	jz	@F

;* Clear the "don't clear regen" bit in the BIOS image
	xor	ax,ax
	mov	es,ax
	and	byte ptr es:[BIOS_info],7fH	;487H
	mov	ax,40h
	mov	es,ax
	mov	al,[di].ayMacVids		;
	dec	al				; rows - 1
	mov	byte ptr es:[0084H],al		;* update BIOS rows
@@:
;
;	The following is provided only to overcome a bug in the EGA BIOS
;	routines which support the graphics "compatibility mode" (BIOS 4)
;	so that the two calls related to PALETTE (INT10 AH = 0BH and
;	INT10H AH = 10H) work correctly.  If we use the first call once
;	when user invokes SCREEN 1 to set the background color, then the
;	BIOS will subsequently reference the correct (low-intensity)
;	color values for the 4 palette attributes whenever the call using
;	INT10H, AH = 0BH is used to toggle the palette, and whenever the
;	call INT10H, AH = 10H is used to set an individual palette regis-
;	ter.  In the absence of this initialization, the high-intensity
;	color values for both palettes are referenced.
;
	test	ss:[bx].fvmCurAdap, fvmEGA or fvmVGA or fvm64KEGA or fvmEGAM
	jz	SkipKludge
	mov	al, [di].modeVids
	cmp	al,4
	je	@F
	cmp	al,5
	jne	SkipKludge
@@:
	push	bx
	xor	bx,bx
	mov	ah, 0BH
	int	10H		;set background color
	pop	bx
SkipKludge:
	mov	ax,-1		;mode set
	jmp short @F
mode_already_set:
	xor	ax,ax		;same mode
@@:
cEnd	SetVideoMode


;********** SaveVidDataCsd ********
;*	entry:	pvidsSaveData = near pointer to VIDS structure
;*		lpwBuffer = buffer to save data
;*	* save screen data into buffer
;*	exit:	n/a

cProc	SaveVidDataCsd, <FAR, PUBLIC, ATOMIC>, <DS, SI, DI>
    parmDP pvidsSaveData
    parmD  lpwBuffer
cBegin	SaveVidDataCsd

	mov	di,pvidsSaveData
	mov	si,di
	mov	dl,[di].modeVids
	mov	cx,0B800h
	cmp	dl,7			
	jb	@F
	cmp	dl,8			
	ja	@F
	mov	cx,0B000h			;video segment for mono mode
@@:
	mov	ax,[di].cwSwapVids
	les	di,lpwBuffer			;es:[di] -> buffer
	cmp	dl,40h				;Olivetti ?
	je	@F
	cmp	dl,8			
	ja	save_graphics_text
@@:						;Text modes or mode 6
	cCall	SaveRegen
	mov	di,si
	cCall 	ClearRegen
	jmp	SaveVidsDataExit

save_graphics_text:
	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	mov	ax, 0a000H			; Address of bit planes
	mov	ds, ax				;ds:[si] -> video buffer
	test	ss:[bx].fvmCurAdap, fvmMCGA
	jz	EgaSS_NotMCGA

	mov	si, 8000H
	mov	cx, 2*1024
EgaSS_MCGA1:
	xor	ax,ax
	xchg	ax, [si]
	inc	si
	inc	si
	stosw
	loop	EgaSS_MCGA1
	
	jmp	short EgaSS_SaveBitPlane2

EgaSS_NotMCGA:
	mov	cl, 0		   	;bit plane 0
	mov	bx,pvidsSaveData

	mov	ax, ss:[bx].cwSwapVids
	cCall	SaveBitPlane

	mov	cl, 1			;bit plane 1
	mov	bx,pvidsSaveData
	mov	ax, ss:[bx].cwSwapVids
	cCall	SaveBitPlane

EgaSS_SaveBitPlane2:
	mov	cl, 2			;bit plane 2
	mov	ax, 4*1024
	cCall	SaveBitPlane

;
; The 8k bytes starting at 16k only gets trashed on 64k Ega cards
;
	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	test	ss:[bx].fvmCurAdap, fvm64KEGA
	jz	EgaSs_64plus

	mov	cl, 2
	mov	ax, 4*1024
	mov	si, 16*1024
	cCall	SaveBitPlane2

EgaSs_64plus:
	push	ss				; Restore ds
	pop	ds
	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	test	[bx].fvmCurAdap, fvmMCGA
	jnz	EgaSS_NotMCGA2

	mov	di,pvidsSaveData
	cCall	ClearBitPlanes

EgaSS_NotMCGA2:

SaveVidsDataExit:

cEnd	SaveVidsDataCsd

;***
;
;  SaveRegen - Saves the video regen buffer to global memory
;
;  Inputs:	[ax] - Number of words to save
;		[di] - Points to a VideoState buffer where handle to
;		       global memory used to save the regen buffer is
;		       store (along with the size of the saved buffer)
;		[es] - Segment of video regen buffer
;		
;
;  Outputs:	none.
;
;  Uses:	es, si
;
;****
cProc	SaveRegen,<NEAR>,<DI,SI>
cBegin	SaveRegen
	push	ds
	mov	ds,cx
	mov	cx,ax
	xor	si,si
;	cCall	VideoOff		; (CGA) Only affects ax and dx
	rep movsw
	pop	ds

;	cCall	VideoOn
NoRegenSave:
cEnd	SaveRegen


cProc	VideoOff,<NEAR,PUBLIC>,<ES>	;trash ax,dx
cBegin	VideoOff
	mov	dx,CGA_6845_STATUS
WaitVerticalRetrace:
	in	al,dx 
	test	al,08H
	jz	WaitVerticalRetrace

	xor	ax,ax
	mov	es,ax 
	mov	ax,es:[0465H]		; Get Current CRT mode
	and	ax, NOT 0008H		; turn off video signal bit
	mov	dx,CGA_6845_MODE
	out	dx,al 
cEnd	VideoOff


cProc	VideoOn,<NEAR,PUBLIC>,<ES>	;trash ax,dx
cBegin	VideoOn
	xor	ax,ax
	mov	es,ax 
	mov	ax,es:[0465H] 		; Get Current CRT mode
	or	ax,0008H		; Turn on video signal bit
	mov	dx,CGA_6845_MODE
	out	dx,al 
cEnd	VideoOn


;***
;
;  ClearRegen - Clears the part of the regen buffer used by TWIN.
;
;  Inputs:	[di] - Points to a VideoState buffer containing the
;		       video state to be restored.
;		[videoseg] - Segment of video regen buffer
;
;  Outputs:	The global memory used to save the regen buffer is freed.
;		i.e. don't call RestoreRegen again with the save video state.
;
;  Uses:	es,di
;
;****
cProc	ClearRegen,<NEAR>,<di,es>
cBegin	ClearRegen
	mov	ax, 0720H		; Assume text mode (clear to spaces)
	mov	cx, 0B800h		; EGA
	mov	bl, [di].modeVids
	cmp	bl, 3
	jbe	CR_1
	cmp	bl,7
	jb	@F
	cmp	bl,8
	ja	@F
	mov	cx,0B000h
	jmp short CR_1
@@:
	xor	ax,ax			; Clear to Null for graphics modes.
CR_1:
	mov	es, cx			; Clear the part of the regen buffer
	mov	cx, [di].cwSwapVids	; Save for later
	xor	di,di			; that we used.
	rep stosw
cEnd	ClearRegen


;***
;
;  RestoreRegen - Restores the video regen buffer from pVideoState
;
;  Inputs:	[di] - Points to a VideoState buffer containing the
;		       video state to be restored.
;		[videoseg] - Segment of video regen buffer
;		cx = video segment
;
;  Outputs:	NZ - if restore ok
;		Z - if not restored
;
;               The global memory used to save the regen buffer is freed.
;		i.e. don't call RestoreRegen again with the save video state.
;
;  Uses:	es, si
;
;****

cProc	RestoreRegen,<NEAR>,<di>
cBegin	RestoreRegen
	mov	es,cx
	mov	cx,ax
	xor	di,di
	rep movsw
cEnd	RestoreRegen



; SaveBitPlane - Saves a given number of words from a given bit plane.
;
; INPUT:
;    ax - Number of words to save
;    cl - Number of bit plane
;    ds - is the segment address of the bit plane.
;    es:di - where to save.
;              
; OUTPUT:
;    ES:DI - Points 1 word past the last saved word
;
; USES:
;    SI
;
cProc	SaveBitPlane,<NEAR>
cBegin	SaveBitPlane
	xor	si,si

LabelNP	<SaveBitPlane2>
	push	ax			; Save count of words
	cCall	MapBitPlane
	pop	cx			; Restore
	rep movsw
cEnd	SaveBitPlane

;
; RestoreBitPlane - Restores a given number of words to a given bit plane.
;
; INPUT:
;    ax - Number of words to restore
;    cl - Number of bit plane
;    es - is the segment address of the bit plane.
;    ds:si - where to restore from.
;              
; OUTPUT:
;    ds:si - Points 1 word past the last restored word
;
; USES:
;    DI
;
cProc	RestoreBitPlane,<NEAR>
cBegin	RestoreBitPlane
	xor	di,di
LabelNP <RestoreBitPlane2>
	push	ax			; Save count of words
	cCall	MapBitPlane
	pop	cx			; Restore
	rep movsw
cEnd	RestoreBitPlane


;
; MapBitPlane - Maps the specified bit plane in for read/write
;
; INPUT:
;    CL - the bit plane.
;
cProc	MapBitPlane,<NEAR>
cBegin	MapBitPlane
	mov	al, 2			; Set Map Mask register
	mov	ah, 1
	shl	ah, cl
	cCall	SetEgaSequencer		; Only trashes DX,AX

	mov	al, 4			; Read Map Select
	mov	ah,cl
	mov	dx, 3ceH		; Graphics Controler Address Port
	cCall	OutWord
cEnd	MapBitPlane

cProc	ClearBitPlanes,<NEAR>,<DI,SI>
cBegin	ClearBitPlanes
	mov	si,di				; ds:si -> saved video state

	mov	ax, 0a000H
	mov	es,ax

	mov	cl, 0				; Map in bit plane 0
	cCall	MapBitPlane

	xor	di,di
	mov	cx, [si].cwSwapVids
	xor	ax,ax
	rep stosw

	mov	cl, 1				; Map in bit plane 1
	cCall	MapBitPlane

	xor	di,di
	mov	cx, [si].cwSwapVids
	xor	ax,ax
	rep stosw

	mov	cl, 2				; Map in bit plane 2
	cCall	MapBitPlane
	xor	di,di
	mov	cx, 8*1024/2
	xor	ax,ax
	rep stosw

	push	ss
	pop	ds				; Restore ds
cEnd	ClearBitPlanes



; SetEgaSequencer - Sends data to the Ega sequencer
;
; INPUT:
;    AL - Sequencer register number
;         00 Reset
;         01 Clocking Mode
;         02 Map Mask
;         03 Character Map Select
;         04 Memory Mode
;    AH - Data for the register
;
cProc	SetEgaSequencer,<NEAR>
cBegin	SetEgaSequencer
	mov	dx, 3c4H		; Ega Sequencer Address Port
	cCall	OutWord
cEnd	SetEgaSequencer

;
; This is just an `out dx,ax' kludge to ensure that it works on
; AT&T 6300
;
cProc	OutWord,<NEAR>
cBegin	OutWord
	out	dx, al
	jmp	short OUTWORD_1		; I/O delay
OUTWORD_1:
	inc	dx
	mov	al,ah
	out	dx, al
cEnd	OutWord


;********** RestoreVidDataCsd ********
;*	entry:	pvidsRestoreData = near pointer to VIDS structure
;*		lpwBuffer = buffer to save data (NULL => just clear screen)
;*	* restore screen data from buffer
;*	exit:	n/a

cProc	RestoreVidDataCsd, <FAR, PUBLIC, ATOMIC>, <DS, SI, DI>
    parmDP pvidsRestoreData
    parmD  lpwBuffer
cBegin	RestoreVidDataCsd

	mov	di,pvidsRestoreData
	mov	al, [di].modeVids

	cmp	al, 3
	jb	EgaRS_Text

	cmp	al, 7
	je	EgaRS_Text
	jb	EgaRS_SimpleGraphics
	cmp	al,8
	je	EgaRs_Text
	cmp	al,40h				;Olivetti ?
	je	EgaRS_SimpleGraphics
	jmp	short EgaRs_HardGraphics

EgaRS_SimpleGraphics:			;mode 6
	push	ax
	cCall	ClearRegen
	pop	ax
EgaRS_Text:	; mode 2,3 and 7,8
	mov	cx,0B800h
	cmp	al, 7
	jb	@F
	cmp	al,8			
	ja	@F
	mov	cx,0B000h
@@:
	mov	ax,[di].cwSwapVids
	lds	si,lpwBuffer
	cCall	RestoreRegen
	jmp	RestoreVidsCsd_exit

EgaRS_HardGraphics:
	cCall	ClearRegen
	mov	ax, 0a000H			; Address of bit planes
	mov	es, ax
	lds	si,lpwBuffer			;ds:[si] -> buffer

	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	test	ss:[bx].fvmCurAdap, fvmMCGA
	jz	EgaRs_NotMCGA

	mov	di, 8000H
	mov	cx, 2*1024
	rep movsw
	jmp	short EgaRs_RestoreBitPlane2

EgaRs_NotMCGA:
	mov	cl, 0
	mov	di,pvidsRestoreData
	mov	ax, ss:[di].cwSwapVids
	cCall	RestoreBitPlane

	mov	cl, 1
	mov	di,pvidsRestoreData
	mov	ax, ss:[di].cwSwapVids
	cCall	RestoreBitPlane

EgaRs_RestoreBitPlane2:
	mov	cl, 2
	mov	ax, 4*1024
	cCall	RestoreBitPlane

;
; The 8k bytes starting at 16k only gets trashed on 64k Ega cards
;
	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	test	ss:[bx].fvmCurAdap, fvm64KEGA
	jz	EgaRs_64plus

	mov	cl, 2
	mov	ax, 4*1024
	mov	di, 16*1024
	cCall	RestoreBitPlane2

EgaRs_64Plus:
	mov	ax,ss				; Restore ds
	mov	ds,ax

;
; Set Map Mask register back to what BASIC expects
;
	mov	ax, 0f02H			; Set map mask to 0fH
	cCall	SetEgaSequencer
	jmp	RestoreVidsCsd_exit

EgaRs_HardNoRegen:
	cCall	ClearBitPlanes

RestoreVidsCsd_exit:

cEnd	RestoreVidDataCsd

;
; INT10Handler
;    The BIOS INT10 (Video Sevices) is hooked to mirror the palette
;    registers (so they can be restored).
;


	RegPtr	cs_bx, cs, bx
	RegPtr	es_bx, es, bx


;********** EnableVidsMonitorCsd ********
;*	entry:	fMonitorOn => monitor should be on
;*	* enable/disable INT 10 monitor
;*	exit:	n/a

cProc	EnableVidsMonitorCsd, <FAR, PUBLIC, ATOMIC>,<di>
    parmW  fMonitorOn
cBegin	EnableVidsMonitorCsd
	mov	di,OFF_lpwDataCsd		;* Data in data segment

	cmp	fMonitorOn,0
	je	Unhook

; Hook int10 trap
	test	ss:[di].fvmCurAdap, fvmEGA or fvmVGA or fvmMCGA
	jz	@F
	mov	[ColourPaletteYellow], 010100B 	;brown
@@:
	mov	dx,drvOffset ColourPalette   	; Default Enhanced colour
	test	ss:[di].fvmCurAdap, fvmEGAM
	jz	@F
	mov	dx,drvOffset MonoPalette   	; Default Monochrome palette
@@:	
	push	cs
	pop	ds				;now ds:dx points to source
	cCall	CopyPalette			; Initialise the user palette
	push	ss
	pop	ds				;restore ds
	

	mov	ax, 10H
	mov	bx, drvOffset INT10Handler
	cCall	HookVector,<ax, cs_bx>
	mov	cs:[lpbBIOSINT10_Offset], ax
	mov	cs:[lpbBIOSINT10_Segment], dx
	jmp	EVM_Exit

Unhook:
; Unhook int10 trap
	les	bx, cs:[BIOSINT10]
	mov	ax, 10H
	cCall	HookVector,<ax,es_bx>
	xor	ax,ax			     	; clear handler
	mov	cs:[lpbBIOSINT10_Offset], ax
	mov	cs:[lpbBIOSINT10_Segment], ax
EVM_Exit:	
cEnd	EnableVidsMonitorCsd


;***
;
;  HookVector(vecNum, vector)
;
;  Sets an interrupt vector and returns the old interrupt vector
;
;  Inputs:	vecNum - Which vector to set
;		vector - address of the new vector
;
;  Outputs:	DX:AX == old interrupt vector
;
;****

cProc	HookVector,<NEAR,PUBLIC>,<DS>
	parmB	vecNum
	parmD	vector
cBegin	HookVector

	mov	ah, 35H
	mov	al, [vecNum]
	int	21H			; Get old vector
	push	bx			; save for return
	push	es			; save for return

	mov	ah, 25H
	mov	al, [vecNum]
	lds	dx, [vector]
	int	21H			; Set new vector

	pop	dx			; return the old vector
	pop	ax			; return the old vector
cEnd	HookVector

;-------------------------------------------------------------------;

INT10Handler:
	or	ah,ah
	jz	INT10SetMode
	cmp	ah, 10h			;set palette regs ?
	je	INT10SetPalette
	cmp	ah, 0Bh
	je	INT10SetCGABgPal
INT10_Chain:
	jmp	cs:[BIOSINT10]

INT10SetMode:
	mov	cs:[bCGABg],-1		; bCGABg is now invalid.
	mov	cs:[bCGAPal],-1		; bCGAPal is now invalid.
	jmp	short INT10_Chain

public INT10SetCGABgPal			;UNDONE - Remove.

INT10SetCGABgPal:
	or	bh,bh			;Set Background colour?
	jnz	@F			;brif no.
;*	* assume BH == 1 for setting palette (if not CGA who cares anyway)
	mov	cs:[bCGABg],bl
	jmp	short INT10_Chain
@@:
	mov	cs:[bCGAPal],bl		;Must be setting CGA Palette
	jmp	short INT10_Chain

INT10SetPalette:
	push	ax			; Save the regs we use
	or	al,al
	jz	INT10_SetIndividualPaletteReg	;ax = 1000h
	dec	al
	jz	INT10_SetOverscanReg		;     1001h
	dec	al
	jz	INT10_SetAllPaletteRegs		;     1002h

INT10_Exit:
	pop	ax
	jmp	short INT10_Chain

;
; INT10_SetIndividualPaletteReg
;    Sets the specified palette register
;
; BL - Is the register number (0-15)
; BH - Is the value
;
INT10_SetIndividualPaletteReg:
	push	bx
	xchg	al, bh			; BH <-- 0, AL <-- value
	mov	cs:[EgaPaletteMirror+bx], al
	pop	bx
	jmp	short INT10_SetPaletteCommon

;
; INT10_SetOverscanReg
;    Sets the overscan register
;
; BH - Is the value.
;
INT10_SetOverscanReg:
	mov	cs:[OverScanMirror], bh
	jmp	short INT10_Exit

;
; INT10_SetAllPaletteRegs
;    Sets all the palette registers and the overscan register.
;
; ES:DX - points to table of values for the 16 palette registers and
;         the overscan register
;
INT10_SetAllPaletteRegs:
	push	ds

	push	es
	pop	ds		; now ds:dx points to source
	call	CopyPalette

	pop	ds
	mov	cs:[bCGAPal],-1		; bCGAPal is now invalid.
INT10_SetPaletteCommon:
	mov	cs:[bCGABg],-1		; bCGABg is now invalid.
	jmp	short INT10_Exit

;
; Copy DS:DX to CS:EgaPaletteMirror
;
cProc CopyPalette,<NEAR>,<SI,DI,CX,ES>
cBegin CopyPalette
	push	cs
	pop	es
	mov	si, dx
	mov	di, DrvOffset EgaPaletteMirror
	mov	cx, 17
	rep movsb
cEnd CopyPalette

ifndef	KANJI
;***** UNDONE - HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK
;***** UNDONE	This piece of code is called directly by QB 4.5
;***** UNDONE	It must not be called by QBJ
;***** UNDONE	Remove this as soon as QB 4.5 ships.
cProc SetBlinkBit,<FAR,PUBLIC>
	parmB	fOn
cBegin
	cmp	[fOn],0
	je	@F
	cCall	EnableBlinkBit
	jmp	short SBB_Exit
@@:
	cCall	DiddleBlinkBit
SBB_Exit:
cEnd
endif	; !KANJI
