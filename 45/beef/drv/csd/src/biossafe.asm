;*
;*	CW : Character Windows Drivers
;*
;*	biossafe.asm : BIOS safe CSD 
;* 	
;*	## This version is built for MP only
;*****************************************************************************

	include	csd_head.inc
	include	csd_data.inc

;*****************************************************************************

	include	csd_code.asm			;* first part of code

;*	* Display modes table
rgdm:
;* #0 - standard color mode
	DB	fvmCGA				;* fake 
	DB	fvmCD or fvmECD or fvmMD	;* fake 
	DB	3				;* mode 
	DW	finstText or finstMonochrome 	;* flags
	DB	80, 25				;* screen size
	DB	2				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0B800H				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved
	Assert	<($-rgdm) EQ SIZE DM>

cdmMax	equ	($ - rgdm) / (size DM)		;* # of modes

;*****************************************************************************

Nonstandard	FInitCsd
Nonstandard	ModeGetCur
Nonstandard	FvmGetCur

;*****************************************************************************

;********** FvmGetCur **********
;*	entry:	DS:DI => driver data
;*	* if first time called, identify the current screen and set fvmCur
;*	*  subsequent calls will return fvmCur
;*	*  After this call: fvmCur, codepageBase, codepageAlt are initialized
;*	exit:	AL == fvm for current screen (0 => no supported screen found)

cProc	FvmGetCur, <NEAR, PUBLIC, ATOMIC>
cBegin	FvmGetCur
	mov	al,fvmCGA	;fake CGA for running within window
	mov	ah,0FFh
cEnd	FvmGetCur



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
	mov	al,3			;fake mode 7 for running within window
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
	mov	ax,cs:[bx].wExtraDm
	mov	[di].wExtra,ax

	;*	No mode set here

;*	* the INCH array already contains the standard Code Page 437
;*	*  character set, so it usually can be left the same.

;*	* Do other diddling
	cCall	DiddleBlinkBit

	mov	ax,sp				;* success
cEnd	FInitCsd


;*****************************************************************************


	include	csd_std.asm		;* standard init/term

	include	csd_ibm.asm		;* IBM specific routines

;*****************************************************************************

	include	csd_bios.asm		;* default procs for BIOS video I/O

	include	csd_save.asm		;* default screen save (none)

;*****************************************************************************

	include	csd_tail.asm		;* tail file

;*****************************************************************************

	END

