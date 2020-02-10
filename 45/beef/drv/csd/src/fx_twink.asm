;*
;*	CW : Character Windows Drivers
;*
;*	fx_ega.asm : CSD for IBM version with screen saving
;*		     (code taken from twin.asm for QC/QB)
;*	* DOES NOT INCLUDE "csd_code"
;*************************************************************************

BUILTIN_SNOW = 1		;* builtin snow control
TWINCSD = 1
KANJI = 1
	include	csd_head.inc
	include fxdrv.inc
	include	csd_data.inc

;*****************************************************************************

	include fx_data.asm

;*****************************************************************************

sBegin	DRV
    assumes CS,DRV
    assumes ds,NOTHING
    assumes ss,NOTHING

;*****************************************************************************
;*	* There is no low memory structure for the linked driver
OFF_lpwDataCsd	DW	dataOffset rgwDataCsd

	include	saveega.inc			;* screen save specifics

;*****************************************************************************

	MDACSD = 1		;enable all drivers
	CGACSD = 1
	EGACSD = 1
	MCGACSD = 1
	VGACSD = 1

	include genmodes.asm			;* modes table

;*****************************************************************************

NonStandard	FInitCsd


;********** FInitCsd **********
;*	entry:
;*		pinch = near pointer to INCH structure to fill
;*	* Initialize the screen to the given mode
;*	exit:	AX != 0 if ok

cProc	FInitCsd, <FAR, PUBLIC, ATOMIC>, <ds,di>
    parmDP pinst
    parmDP pinch
    localB modeCur
    localB fscanset
cBegin	FInitCsd

	mov	di,OFF_lpwDataCsd		;* Data in data segment

;*	* save old mode
	cCall	ModeGetCur			;* al = mode, ah = ayMac
	mov	modeCur,al			;* current mode

	mov	bx,pinst
	cmp	ds:[bx].ayMacInst,ah
	je	@F				;* same resolution
	mov	modeCur,0			;* cause mode reset
@@:

;*	* set mode
	mov	[di].pinstDrv,bx
	mov	bx,ds:[bx].pdmInst		;* CS:BX => DM info

;*	* copy mode info into driver globals
	mov	ax,cs:[bx].vparmCursOnDm
	mov	[di].vparmCursOn,ax		;initialize underline cursor
	mov	[di].vparmCursSize,ax
	mov	ax,cs:[bx].wExtraDm
	mov	[di].wExtra,ax

	mov	al,cs:[bx].modeDm
	cmp	al,modeCur
	jne	@F				
	jmp	finitdone			;same mode, no reset
@@:
	mov	fscanset,0
	test	[di].fvmCurAdap,fvmVGA 		;Test for VGA
	jz	initvideomode			

	mov	al,2				;default 400 scan lines
	cmp	cs:[bx].modeDm,7
	je	@F
	cmp	cs:[bx].ayMacDm,43		;80x43 text ?
	jne	not43line
@@:	
	dec	al				;350 lines (EGA)
	mov	fscanset,al
not43line:
	mov	ah,12h				;set vertical scan line for VGA
	push	bx
	mov	bl,30h
	int	10h
	pop	bx

initvideomode:	
	xor	ah,ah				;* set mode
	mov	al,cs:[bx].modeDm
	or	al,80h				;* don't clear REGEN
	int	10h

	test	ss:[bx].fvmCurAdap, fvmEGA or fvmVGA or fvm64KEGA or fvmMCGA or fvmEGAM
	jz	@F

;* Clear the "don't clear regen" bit in the BIOS image
	xor	ax,ax
	mov	es,ax
	and	byte ptr es:[BIOS_info],7fH	;487H
@@:
	mov	ax,40H
	mov	es,ax
	mov	al,cs:[bx].ayMacDm
	dec	al				; rows - 1
	mov	byte ptr es:[0084H],al		;* update BIOS rows

	test	[di].fvmCurAdap,fvmCGA		 
	jnz	finitdone

	mov	al,12h				;8x8 fonts
	mov	ah,8
	cmp	cs:[bx].ayMacDm,25		
	jne	@F
	
	test	[di].fvmCurAdap,fvmEGA or fvmEGAM or fvm64KEGA		 
	jnz	finitdone
		
	mov	al,14h				;8x16
	mov	ah,16

	cmp	fscanset,1
	jne	@F

	mov	al,11h				;8x14 (VGA 350 scan mode)
	mov	ah,14
@@:
	push	bx
	xor	bl,bl
	mov	bh,ah
	mov	ah,11h				;load char set
	int	10h				
	pop	bx

	cmp	cs:[bx].ayMacDm,50		;* 50 line mode ?
	jne	finitdone
	
	test	[di].fvmCurAdap,fvmMCGA		 
	jz	finitdone	
					;extra works for MCGA 50 line mode
	mov	ax,1103h		;load font page
	xor	bx,bx
	int	10h			

	mov	dx,3D4h			;program CRT
	mov	ax,309h
	out	dx,ax
	mov	al,0Ah
	out	dx,ax
	mov	al,0Bh
	out	dx,ax

	push	ds
	mov	ax,40h				;update video Bios data segment
	mov	ds,ax
	mov	word ptr ds:[4Ch],1F40h		;80x50x2
	mov	byte ptr ds:[84h],31h		;50-1 rows
	mov	word ptr ds:[85h],8		;points	
	pop	ds

finitdone:

;*	* normally the INCH array would be copied (but since it is already
;*	*  setup in DATA just leave it alone).

;*	* Do other diddling
	cCall	DiddleBlinkBit

	mov	dx,drvOffset ColourPalette   	; Default Enhanced colour
	test	[di].fvmCurAdap, fvmEGAM
	jz	@F
	mov	dx,drvOffset MonoPalette   	; Default Monochrome palette
@@:	
	push	cs
	pop	es				; es:dx = color array
	mov	ax,1002H
	int	10H				; Set default palette

        mov     ax, 0500H
	int	10H				; Set default page to 0

	mov	ax,sp				;* success
cEnd	FInitCsd


;*****************************************************************************

	include saveega.asm			;* save stuff

;*****************************************************************************

	include	csd_std.asm		;* standard init/term
	include	csd_ibm.asm		;* IBM specific routines

;*****************************************************************************

	include kanji.inc
	include csd_oax.asm		;* OAX procs for direct video I/O
	include	csd_save.asm		;* default screen save (none)

;*****************************************************************************

	include	csd_tail.asm		;* tail file

;*****************************************************************************

	END

