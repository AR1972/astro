;*
;*	CW : Character Windows
;*
;*	egasave.inc : Screen Save IBM specific info


;*****************************************************************************


EGA_VIDS	STRUC		;* VIDS structure + private info
	vidsPrefix	DB	cbVidsMin DUP (?)
	vparmCursorVids	DW	?			;* cursor size
	rgwCursorPosVids DW	8 DUP (?)		;* cursor positions
							;* for 8 pages
	fClearRegenVids	db	?			;* 80H for no regen clr
	ayPointsVids	DB	?			;* char points
	ayMacVids	DB	?			;* rows
	ayOverScanVids	DB	?
	cwSwapVids	DW	?			;* lines*columes
	bCGABgVids	DB	?			;* CGA Background color
	bCGAPalVids	DB	?			;* 4-color palette
	rgbEGAPaletteVids	DB	21 DUP (?)	;* attr ctrl regs
	rgbPS2PaletteVids	DB	3 DUP (?)	;* reserved for DAC color regs
EGA_VIDS	ENDS


;*****************************************************************************

GRChipOffset	EQU	10H
CRTCChipOffset	EQU	00H
SEQChipOffset	EQU	08H
AttrChipOffset	EQU	18H

CGA_6845_MODE	EQU	3D8H
CGA_6845_STATUS	EQU	3DAH

; Number of lines when 8x8 character set is used in the following graphics
; modes:            0d  0e  0f  10  11  12  13
;                   |   |   |   |   |   |   |
;                   v   v   v   v   v   v   v
GraphicsModeRows DB 25, 25, 43, 43, 60, 60, 25

		
;
; ColourPalette - used to reset the EGA palette for 16-color
;		  modes with a Color Display.
;
labelB	<ColourPalette>
	;	RGBrgb
	DB	000000B 	;black
	DB	000001B 	;blue
	DB	000010B 	;green
	DB	000011B 	;cyan
	DB	000100B 	;red
	DB	000101B 	;magenta
labelB <ColourPaletteYellow>
	DB	000110B 	;yellow
	DB	000111B 	;white
	DB	111000B 	;gray
	DB	111001B 	;light blue
	DB	111010B 	;light green
	DB	111011B 	;light cyan
	DB	111100B 	;light red
	DB	111101B 	;light magenta
	DB	111110B 	;light yellow
	DB	111111B 	;bright white
	db	0		;Overscan
;
; MonoPalette - used to reset the EGA palette for 16-color
;		  modes with a monochrome Display.
;
labelB	<MonoPalette>
	;	 IV   
	DB	000000B 	;black
	DB	001000B 	;white
	DB	001000B 	;white
	DB	001000B 	;white
	DB	001000B 	;white
	DB	001000B 	;white
	DB	001000B 	;white
	DB	001000B 	;white
	DB	010000B 	;gray
	DB	011000B 	;bright white
	DB	011000B 	;bright white
	DB	011000B 	;bright white
	DB	011000B 	;bright white
	DB	011000B 	;bright white
	DB	011000B 	;bright white
	DB	011000B 	;bright white
	db	0		;Overscan

BIOSINT10	label	dword
lpbBIOSINT10_Offset	dw	0
lpbBIOSINT10_Segment	dw	0

EgaPaletteMirror db	0,1,2,3,4,5,14H,7,38H,39H,3aH,3bH,3cH,3dH,3eH,3fH
OverScanMirror	db	0

;
; bCGABg - CGA background colour
;          Set by INT 10 with AH=0B, BH=0, BL=bCGABg
;
; bCGAPal - CGA 320x200 4-colour palette
;	    Set by INT 10 with AH=0B, BH=1, BL=bCGAPal
;
; -1 indicates value not set.
;
bCGABg		db	-1
bCGAPal		db	-1
