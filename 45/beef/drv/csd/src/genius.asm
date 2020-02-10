;*
;*	CW : Character Windows Drivers
;*
;*	genius.asm : MDS (Micro Display System) Genius CSD
;*****************************************************************************

	include	csd_head.inc
	include	csd_data.inc

;*****************************************************************************

	include	csd_code.asm			;* first part of code

;*	* Display modes table
rgdm:
;;;* mode = 7(IBM), 8(MDS)
;* #0 - IBM text mode
	DB	0ffh				;* any
	DB	0ffh				;* any
	DB	7				;* mode
	DW	finstText or finstMonoChrome	;* flags
	DB	80,25				;* screen size
	DB	2				;* coMac
	DB	0, 0, 0, 0			;* INFT
	DW	0B000H				;* video address
	DW	0C0DH				;* cursor
	DW	0800H				;* port 3B8/3D8H
	Assert	<($-rgdm) EQ SIZE DM>

;* #1 - MDS text mode (80 col)
	DB	0ffh				;* any
	DB	0ffh				;* any
	DB	8				;* mode
	DW	finstText or finstMonoChrome	;* flags
	DB	80,66				;* screen size
	DB	2				;* coMac
	DB	0, 0, 0, 0			;* INFT
	DW	0B000H				;* video address
	DW	0D0EH				;* cursor
	DW	0800H				;* port 3B8/3D8H

;* #2 - MDS graphics text mode (63 row)
	DB	0ffh				;* any
	DB	0ffh				;* any
	DB	8				;* mode
	DW	finstGraphics or finstMonoChrome OR finstFont	;* flags
	DB	91,63				;* screen size
	DB	2				;* coMac
	DB	8, 16, 0, 0			;* INFT
	DW	0				;* video address
	DW	0E0FH				;* cursor
	DW	0008H				;* port 3B8/3D8H

cdmMax	equ	($ - rgdm) / (size DM)		;* # of modes

Genius_Mode	EQU	8		; Special Genius mode for full screen
CRTPort2	EQU	3B0H		; Genius CRT Control Port # 2
MDSMode_Mask	EQU	00000001B	; 1 = MDS Mode, 0 = IBM Mode
Upper_Screen	EQU	0A000H
Lower_Screen	EQU	0B800H
BWModeCtrlReg	EQU	3B8H
ColorModeCtrlReg EQU	3D8H

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

	mov	dx,3B0h
	in	al,dx
	and 	al,1
	jnz	@F
	mov	ax,1907h 		;ibm 80x25 mono
	jmp short exitMGCur
@@:
	mov	ax,40H
	mov	es,ax
	mov	ah,es:[0084H]		;* read BIOS rows
	inc	ah			;* screen height
	cmp	ah,66		       	;MDS text
	je	@F
	cmp	ah,63			;MDS graphics
	je	@F
	mov	ah,66			;* default to text mode
@@:	
	mov	al,8			;* MDS mode 8
exitMGCur:
cEnd	ModeGetCur


;********** FInitCsd **********
;*	entry:
;*		pinch = near pointer to INCH structure to fill
;*	* Initialize the screen to the given mode
;*	exit:	AX != 0 if ok

cProc	FInitCsd, <FAR, PUBLIC, ATOMIC>, <ds,di>
    parmDP pinst
    parmDP pinch
    localB modeCur
    localB fInverse
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
	mov	[di].vparmCursOn,ax
	mov	[di].vparmCursSize,ax
	mov	ax,cs:[bx].wExtraDm
	mov	[di].wExtra,ax

	mov	al,cs:[bx].modeDm
	cmp	al,modeCur
	jne	@F				
	jmp	initdone			;* don't reset
@@:
	mov	dx,3B0h
	in	al,dx
	and	al,00100000b			;get inverse bit
	mov	fInverse,al

	mov	al,cs:[bx].modeDm
	xor	ah,ah				;* set mode
	int	10h

	in	al,dx
	and	al,11011111b
	or	al,fInverse
	cmp	cs:[bx].modeDm,7
	jne	@F
	cmp	fInverse,0
	je	@F
	or	al,10h				;clear bottom graphics screen
@@:	
	out	dx,al

	mov	ax,40H
	mov	es,ax
	mov	al,cs:[bx].modeDm
	mov	byte ptr es:[0049H],al		;* modes
	mov	al,cs:[bx].ayMacDm
	dec	al				; rows - 1
	mov	byte ptr es:[0084H],al		;* update BIOS rows

	mov	ax,cs:[bx].wExtraDm
	mov	dx,3D8h			
	out	dx,al
	mov	al,ah
	mov	dx,3B8h
	out	dx,al

	xor	al,al
	cmp	cs:[bx].modeDm,7		;IBM 80x25 text ?
	jne	@F
	mov	al,90h				;set double height character
@@:
	mov	dx,3B1h
	out	dx,al

	test	cs:[bx].finstDm,finstGraphics
	jz	finittext

; KLUDGE ALERT!!  Genius designed themselves into a box, so we need to
;	communicate with the mouse through the cursor position for
;	page 3 (an arbitrarily chosen location, hopefully safe).
	mov	ax,40H				;IBM segment
	mov	es,ax
	mov	al,cs:[bx].modeDm
	mov	byte ptr es:[50h + (3 * 2)],al	;CURSOR_POSN
initdone:
	xor	ah,ah	
	mov	al,cs:[bx].dyCharDm
	mov	[di].ayBox,ax			;* points
	mov	[di].SEG_lpbFont,cs		;* 8x16 font
	mov	[di].OFF_lpbFont,drvOffset rgbVectFont8x16
finitdone:
;*	* normally the INCH array would be copied (but since it is already
;*	*  setup in DATA just leave it alone).

;*	* Do other diddling
	test	cs:[bx].finstDm,finstText
	jz	@F
finittext:
	cCall	DiddleBlinkBit
@@:
	mov	ax,sp				;* success
cEnd	FInitCsd

;*****************************************************************************

	GENIUSCSD = 1
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