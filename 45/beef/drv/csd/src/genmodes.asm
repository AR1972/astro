;*	Standard Screen Modes
;*	included in fx_twin, ibmtext, and fx_csd3

;*	* Display modes table
rgdm:

IF (MDACSD OR EGACSD OR VGACSD)
;* #0 - standard 25 line monochrome mode 
	DB	fvmMDA or fvmEGAM or fvmVGA
	DB	fvmMD
	DB	7				;* mode
  	DW	finstText OR finstMonochrome
	DB	80, 25
	DB	2				;* coMac
	DB	0, 0, 0, 0			;* INFT (unknown 9x14 or 9x16)
	DW	0B000H				;* video address
	DW	0C0DH				;* cursor
	DW	0				;* reserved
	Assert	<($-rgdm) EQ SIZE DM>
ENDIF	;(MDACSD OR EGACSD)

IF MCGACSD
;* #0 - standard 25 line monochrome mode 
	DB	fvmMCGA
	DB	fvmMD
	DB	2				;* mode
  	DW	finstText OR finstMonochrome
	DB	80, 25
	DB	2				;* coMac
	DB	8, 16, 0, 0			;* INFT
	DW	0B800H				;* video address
	DW	0E0FH				;* cursor
	DW	0				;* reserved
ENDIF	;MCGACSD

IF (EGACSD OR VGACSD)
;* #1 - standard 43 line monochrome mode 
	DB	fvmEGAM or fvmVGA
	DB	fvmMD
	DB	7				;* mode
	DW	finstText OR finstMonochrome
	DB	80, 43
	DB	2				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0B000H				;* video address
	DW	0406H				;* cursor
	DW	0				;* reserved
ENDIF	;(EGACSD OR VGACSD)

IF VGACSD
;* #0 - standard 50 line monochrome mode 
	DB	fvmVGA
	DB	fvmMD
	DB	7				;* mode
  	DW	finstText OR finstMonochrome
	DB	80, 50
	DB	2				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0B000H				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved
ENDIF	;VGACSD

IFNDEF	CGASNOW
IF CGACSD
;* #2 - non-color burst mode (composit)
	DB	fvmCGA
	DB	fvmMD
	DB	2				;* mode
	DW	finstText or finstMonochrome
	DB	80, 25
	DB	16				;* coMac (gray level in this mode)
	DB	0, 0, 0, 0			;* INFT 
	DW	0B800H				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved
ENDIF	;CGACSD

IF (CGACSD OR MCGACSD OR EGACSD OR VGACSD)
;* #3 - standard color mode (CGA/EGA/MCGA/VGA)
	DB	fvmCGA or fvmEGA or fvm64KEGA or fvmMCGA or fvmVGA
	DB	fvmECD or fvmCD
	DB	3				;* mode
	DW	finstText
	DB	80, 25
	DB	16				;* coMac
	DB	0, 0, 0, 0			;* INFT (unknown)
	DW	0B800H				;* video address
	DW	0607H				;* cursor !!REVIEW
	DW	0				;* reserved
ENDIF 	;(CGACSD OR MCGACSD OR EGACSD OR VGACSD)
ENDIF	;NOT DEFINED CGASNOW

IFDEF	CGASNOW
;* #0 - non-color burst mode
	DB	fvmCGA OR fvm64KEGA OR fvmEGA OR fvmEGAM OR fvmMCGA OR fvmVGA
	DB	fvmMD 
	DB	2				;* mode
	DW	finstText or finstCgaSnow	;* flags
	DB	80, 25				;* screen size
	DB	16				;* coMac (gray level)
	DB	8, 8, 0, 0			;* INFT
	DW	0h				;* video address (Null, so allocate buffer)
	DW	0607H				;* cursor
	DW	0				;* reserved

;* #1 - standard color mode
	DB	fvmCGA OR fvm64KEGA OR fvmEGA OR fvmEGAM OR fvmMCGA OR fvmVGA
	DB	fvmCD or fvmECD 
	DB	3				;* mode
	DW	finstText or finstCgaSnow	;* flags
	DB	80, 25				;* screen size
	DB	16				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0h				;* video address (Null, so allocate buffer)
	DW	0607H				;* cursor
	DW	0				;* reserved
ENDIF	;CGASNOW

IF (EGACSD OR VGACSD)
;* #4 - 43 line mode (EGA/VGA 640x350)
	DB	fvmEGA or fvm64KEGA or fvmVGA
	DB	fvmECD
	DB	3				;* mode
	DW	finstText
	DB	80, 43
	DB	16				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0B800H				;* video address
	DW	0406H				;* cursor
	DW	0				;* reserved
ENDIF	;(EGACSD OR VGACSD)

IF VGACSD
;* #5 - 50 line (VGA 640x400)
	DB	fvmVGA
	DB	fvmECD
	DB	3				;* mode
	DW	finstText
	DB	80, 50
	DB	16				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0B800H				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved
ENDIF	;(MCGACSD OR VGACSD)

IFDEF GRAPHICSTEXT

;''''''''''''''
; Graphics Text
;,,,,,,,,,,,,,,

IF CGACSD
;* #2 - mono graphics text (CGA)
	DB	fvmCGA
	DB	fvmECD or fvmCD			;
	DB	6				;* mode
	DW	finstGraphics or finstFont or finstMonochrome or finstFastScroll
	DB	80, 25				;* screen size
	DB	2				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved

;* #3 - 4-color graphics text (8x8 fonts, CGA)
	DB	fvmCGA 
	DB	fvmECD or fvmCD
	DB	4				;* mode
	DW	finstGraphics or finstFont
	DB	40,25				;* 40,25 REVIEW!!!
	DB	4				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved
ENDIF	;CGACSD

IF MCGACSD
;* #3 - 256 Color graphics text (40X25)
	DB	fvmMCGA 
	DB	fvmECD or fvmCD
	DB	13h				;* mode
	DW	finstGraphics or finstFont
	DB	40,25				;* 40,25 REVIEW!!!
	DB	16				;* coMac (see MCGA.GSD)
	DB	8, 8, 0, 0			;* INFT
	DW	0				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved
ENDIF	;MCGACSD

IF EGACSD
;* #4a - mono graphics text (8x14 fonts, EGA[64K])
	DB	fvm64KEGA or fvmEGAM
	DB	fvmMD
	DB	0Fh				;* mode
	DW	finstGraphics or finstFont OR finstMonochrome or finstFastScroll
	DB	80, 25
	DB	2				;* coMac
	DB	8, 14, 0, 0			;* INFT
	DW	0				;* video address
	DW	0D0EH				;* cursor
	DW	0				;* reserved

;* #5a - mono 43 line graphics text (8x8 fonts, EGA[64K])
	DB	fvm64KEGA or fvmEGAM
	DB	fvmMD
	DB	0FH				;* mode
	DW	finstGraphics or finstFont OR finstMonochrome or finstFastScroll
	DB	80, 43
	DB	2				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved

;* #4b - mono graphics text (8x14 fonts, EGA[64K])
	DB	fvm64KEGA
	DB	fvmECD
	DB	10h				;* mode
	DW	finstGraphics or finstFont OR finstMonochrome or finstFastScroll
	DB	80, 25
	DB	2				;* coMac
	DB	8, 14, 0, 0			;* INFT
	DW	0				;* video address
	DW	0D0EH				;* cursor
	DW	0				;* reserved

;* #5b - mono 43 line graphics text (8x8 fonts, EGA[64K])
	DB	fvm64KEGA 
	DB	fvmECD
	DB	10H				;* mode
	DW	finstGraphics or finstFont OR finstMonochrome or finstFastScroll
	DB	80, 43
	DB	2				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved

;* #6 - color graphics text (8x8 fonts,640x200 EGA[64K])
	DB	fvm64KEGA
	DB	fvmECD or fvmCD
	DB	0EH				;* mode
	DW	finstGraphics or finstFont or finstFastScroll 
	DB	80, 25
	DB	16				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved
ENDIF 	;EGACSD

IF (EGACSD OR VGACSD)
;* #7 - color graphics text (8x14 fonts, EGA[>64K]/VGA)
	DB	fvmEGA or fvmVGA 
	DB	fvmECD
	DB	10H				;* mode
	DW	finstGraphics or finstFont or finstFastScroll
	DB	80, 25				;* screen size
	DB	16				;* coMac
	DB	8, 14, 0, 0			;* INFT
	DW	0				;* video address
	DW	0D0EH
	DW	0				;* reserved

;* #8 - color 43 line graphics text (8x8 fonts, EGA[>64K]/VGA)
	DB	fvmEGA or fvmVGA
	DB	fvmECD
	DB	10H				;* mode
	DW	finstGraphics or finstFont or finstFastScroll
	DB	80, 43
	DB	16				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved
ENDIF 	;EGACSD OR VGACSD

IF MCGACSD
;* #9 - mono 30 line graphics text (8x16 fonts, MCGA/VGA)
	DB	fvmMCGA 
	DB	fvmMD or fvmECD
	DB	11H				;* mode
	DW	finstGraphics or finstFont OR finstMonochrome or finstFastScroll
	DB	80, 30
	DB	2				;* coMac
	DB	8, 16, 0, 0			;* INFT
	DW	0				;* video address
	DW	0F10H				;* cursor
	DW	0				;* reserved

;* #10 - mono 60 line graphics text (8x8 fonts, MCGA/VGA)
	DB	fvmMCGA 
	DB	fvmMD or fvmECD
	DB	11H				;* mode
	DW	finstGraphics or finstFont OR finstMonochrome or finstFastScroll
	DB	80, 60
	DB	2				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved
ENDIF 	;MCGACSD

IF VGACSD
;* #9 - mono 30 line graphics text (8x16 fonts, MCGA/VGA)
	DB	fvmVGA 
	DB	fvmMD 
	DB	11H				;* mode
	DW	finstGraphics or finstFont OR finstMonochrome or finstFastScroll
	DB	80, 30
	DB	2				;* coMac
	DB	8, 16, 0, 0			;* INFT
	DW	0				;* video address
	DW	0F10H				;* cursor
	DW	0				;* reserved

;* #10 - mono 60 line graphics text (8x8 fonts, MCGA/VGA)
	DB	fvmVGA 
	DB	fvmMD
	DB	11H				;* mode
	DW	finstGraphics or finstFont OR finstMonochrome or finstFastScroll
	DB	80, 60
	DB	2				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved

;* #11 - color 30 line graphics text (8x16 fonts, 16 color, VGA)
	DB	fvmVGA
	DB	fvmECD
	DB	12H				;* mode
	DW	finstGraphics or finstFont or finstFastScroll
	DB	80, 30
	DB	16				;* coMac
	DB	8, 16, 0, 0			;* INFT
	DW	0				;* video address
	DW	0F10H				;* cursor
	DW	0				;* reserved

;* #12 - color 34 line graphics text (8x14 fonts, 16 color, VGA)
	DB	fvmVGA
	DB	fvmECD
	DB	12H				;* mode
	DW	finstGraphics or finstFont or finstFastScroll
	DB	80, 34
	DB	16				;* coMac
	DB	8, 14, 0, 0			;* INFT
	DW	0				;* video address
	DW	0D0EH				;* cursor
	DW	0				;* reserved

;* #13 - color 60 line graphics text (8x8 fonts, VGA)
	DB	fvmVGA
	DB	fvmECD
	DB	12H				;* mode
	DW	finstGraphics or finstFont or finstFastScroll
	DB	80, 60
	DB	16				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved
ENDIF	;VGACSD
ENDIF 	;GRAPHICSTEXT

cdmMax	equ	($ - rgdm) / (size DM)		;* # of modes

