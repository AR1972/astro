;*
;*	CW : Character Windows Drivers
;*
;*	Vectra.asm : HP Vectra CSD
;*
;*****************************************************************************

	include	csd_head.inc
	include	csd_data.inc

;*****************************************************************************

	include	csd_code.asm			;* first part of code

;*	* Display modes table
rgdm:

;* #0 - standard color mode
	DB	0ffh				;* any
	DB	0ffh				;* any
	DB	3				;* mode
	DW	finstText			;* flags
	DB	80, 25				;* screen size
	DB	16				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0B800H				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved
	Assert	<($-rgdm) EQ SIZE DM>

;* #1 - 25 line Graphics text mode (mono)
	DB	0ffh				;* any
	DB	0ffh				;* any
	DB	5				;* mode
	DW	finstGraphics OR finstFont OR finstMonoChrome or finstFastScroll ;* flags
	DB	80, 25				;* screen size
	DB	2				;* coMac
	DB	8, 16, 0, 0			;* INFT
	DW	0				;* video address
	DW	0E0FH				;* cursor
	DW	0				;* reserved

;* #2 - 50 line Graphics text mode (mono)
	DB	0ffh				;* any
	DB	0ffh				;* any
	DB	5				;* mode
	DW	finstGraphics OR finstFont OR finstMonoChrome or finstFastScroll ;* flags
	DB	80, 50				;* screen size
	DB	2				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved

cdmMax	equ	($ - rgdm) / (size DM)		;* # of modes

;*****************************************************************************
;*	* Special routines

NonStandard	ModeGetCur
NonStandard	FInitCsd

;*****************************************************************************

;********** ModeGetCur *********
;*	entry:	n/a
;*	* get current machine mode
;*	exit:	al = mode, ah = ayMac (or 0 if unknown)

cProc	ModeGetCur, <NEAR, PUBLIC, ATOMIC>, <ES>
cBegin	ModeGetCur

	mov	ax,40H
	mov	es,ax
	mov	dl,es:[0084H]		;* read BIOS rows
	inc	dl			;* dl = screen height
	cmp	dl,25			;do this since some clones don't
	je	@F			;update BIOS data
	cmp	dl,50
	je	@F
	mov	dl,25			;* default to 25 rows
@@:	
	push	bx
	mov	ah,0fh
	int	10h			;* get current state, return al = mode
	and	al,7Fh			;* mask off clear video buffer bit.
	pop	bx
	cmp	al,3			;text mode ?
	je	@F
	mov	al,5			;set graphics mode
@@:
	mov	ah,dl

cEnd	ModeGetCur




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
	mov	[di].vparmCursOn,ax
	mov	[di].vparmCursSize,ax
	mov	ax,cs:[bx].wExtraDm
	mov	[di].wExtra,ax

	cCall	ModeGetCur			;* al = mode, ah = ayMac

	mov	cx,40H
	mov	es,cx
	mov	cl,cs:[bx].ayMacDm
	dec	cl				; rows - 1
	mov	byte ptr es:[0084H],cl		;* update BIOS rows

	test	cs:[bx].finstDm,finstGraphics
	jz	InitText

	xor	ch,ch	
	mov	cl,cs:[bx].dyCharDm
	mov	[di].ayBox,cx			;* points
	cmp	cx,8
	je	@F

	mov	[di].SEG_lpbFont,cs				;8x16 font
	mov	[di].OFF_lpbFont,drvOffset rgbVectFont8x16
	jmp short font1
@@:
	mov	[di].SEG_lpbFont,0F000h		;8x8 font (first 128)
	mov	[di].OFF_lpbFont,0FA6Eh
font1:	
	mov	ax,6F05h			;* set mode
	mov	bl,0Dh
	int	10h
	jmp	short InitDone

InitText:	
	cmp	al,cs:[bx].modeDm
	je	@F				;* don't reset

	mov	al,cs:[bx].modeDm
	xor	ah,ah				;* set mode
	int	10h				;* set mode
@@:

;*	* the INCH array already contains the standard Code Page 437
;*	*  character set, so it usually can be left the same.

;*	* Do other diddling
	cCall	DiddleBlinkBit

InitDone:
	mov	ax,sp				;* success
cEnd	FInitCsd

;*****************************************************************************

	VECTRACSD = 1
	include update2.asm
	include	vect8x16.inc		;* hard code font table

;*****************************************************************************

	include	csd_std.asm		;* standard init/term
	include	csd_ibm.asm		;* IBM specific routines

;*****************************************************************************

	include	csd_vram.asm		;* default procs for direct video I/O
	include	csd_save.asm		;* default screen save (none)

;*****************************************************************************

	include	csd_tail.asm		;* tail file

;*****************************************************************************


	END

