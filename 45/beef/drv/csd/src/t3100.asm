;*
;*	CW : Character Windows Drivers
;*
;*	t3100.asm : Toshiba 3100 (LCD) CSD
;*
;*****************************************************************************

	include	csd_head.inc
	include	csd_data.inc

;*****************************************************************************

	include	csd_code.asm			;* first part of code

;*	* Display modes table
rgdm:

;* #0 - standard mono mode
	DB	0ffh				;* any
	DB	0ffh				;* any
	DB	3				;* mode
	DW	finstText or finstMonoChrome 	;* flags
	DB	80, 25				;* screen size
	DB	2				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0B800H				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved
	Assert	<($-rgdm) EQ SIZE DM>

;* #1 - 25 line Graphics text mode (mono)
	DB	0ffh				;* any
	DB	0ffh				;* any
	DB	74h				;* mode
	DW	finstGraphics OR finstFont OR finstMonoChrome or finstFastScroll	;* flags
	DB	80, 25				;* screen size
	DB	2				;* coMac
	DB	8, 16, 0, 0			;* INFT
	DW	0				;* video address
	DW	0E0FH				;* cursor
	DW	0				;* reserved

;* #2 - 50 line Graphics text mode (mono)
	DB	0ffh				;* any
	DB	0ffh				;* any
	DB	74h				;* mode
	DW	finstGraphics OR finstFont OR finstMonoChrome or finstFastScroll	;* flags
	DB	80, 50				;* screen size
	DB	2				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved

cdmMax	equ	($ - rgdm) / (size DM)		;* # of modes

segROM	equ	0F000H
T3100_Font_Off	EQU	0CA00H		; Offset in BIOS for 640X400 fonts

;*****************************************************************************
;*	* Special routines

NonStandard	FInitCsd

;*****************************************************************************


;********** FInitCsd **********
;*	* CSD entry point (see documentation for interface)
;*	* Initialize the screen to the given mode
;*	exit:	AX != 0 if ok

cProc	FInitCsd, <FAR, PUBLIC, ATOMIC>, <DI>
    parmDP pinst
    parmDP pinch
    localB modeCur
cBegin	FInitCsd

	mov	di,OFF_lpwDataCsd		;* Data in data segment

;*	* save old mode
	cCall	ModeGetCur			;* al = mode, ah = ayMac
	mov	modeCur,al			;* current mode

	mov	bx,pinst
	cmp	ds:[bx].ayMacInst,ah		;* new = current rows ?
	je	@F				;* same resolution
	mov	modeCur,0			;* cause mode reset
@@:

;*	* set mode
	mov	bx,pinst
	mov	[di].pinstDrv,bx
	mov	bx,ds:[bx].pdmInst		;* CS:BX => DM info

;*	* copy mode info into driver globals
	mov	ax,cs:[bx].wExtraDm
	mov	[di].wExtra,ax
	mov	ax,cs:[bx].vparmCursOnDm
	mov	[di].vparmCursSize,ax
	mov	[di].vparmCursOn,ax

	xor	ah,ah				;* set mode
	mov	al,cs:[bx].modeDm
	cmp	al,modeCur
	je	@F				;* don't reset
	int	10h				;* set mode
@@:
	mov	ax,40H
	mov	es,ax
	mov	al,cs:[bx].ayMacDm
	dec	al				; rows - 1
	mov	byte ptr es:[0084H],al		;* update BIOS rows

	test	cs:[bx].finstDm,finstGraphics
	jz	InitText

	xor	ah,ah	
	mov	al,cs:[bx].dyCharDm
	mov	[di].ayBox,ax			;* points
	cmp	ax,8
	je	@F

	mov	[di].SEG_lpbFont,segROM		;8x16 font
	mov	[di].OFF_lpbFont,T3100_Font_Off
	jmp short font1

@@: 
	mov	[di].SEG_lpbFont,segROM		;8x8 font (lower 128)
	mov	[di].OFF_lpbFont,0FA6Eh
font1:	
	jmp	short InitDone

InitText:	
;*	* the INCH array already contains the standard Code Page 437
;*	*  character set, so it usually can be left the same.

;*	* Do other diddling
	cCall	DiddleBlinkBit

InitDone:
	mov	ax,sp				;* success
cEnd	FInitCsd

;*****************************************************************************

	T3100CSD = 1
	include	update2.asm

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