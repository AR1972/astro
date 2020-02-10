;*
;*	CW : Character Windows Drivers
;*
;*	fx_data.asm : Fixed driver data (linked in)
;*****************************************************************************


sBegin	DATA
    assumes DS,DGROUP
    assumes CS,DGROUP

IFDEF DUALOS2

;*	* default characters (tracks INCH strucure)
;*	* NOT NEEDED FOR DUALOS2 - SUPPLIED BY FX_CSD3

ELSE ;!DUALOS2

;*	* default characters (tracks INCH strucure)
labelB	<PUBLIC, inch>

IFNDEF KANJI
;*	* IBM characters
	DB	218	;* chTopLeftCorner1
	DB	191	;* chTopRightCorner1
	DB	192	;* chBottomLeftCorner1
	DB	217	;* chBottomRightCorner1
	DB	196	;* chTopSide1
	DB	196	;* chBottomSide1
	DB	179	;* chLeftSide1
	DB	179	;* chRightSide1
	DB	195	;* chMiddleLeft1
	DB	180	;* chMiddleRight1
	DB	201	;* chTopLeftCorner2
	DB	187	;* chTopRightCorner2
	DB	200	;* chBottomLeftCorner2
	DB	188	;* chBottomRightCorner2
	DB	205	;* chTopSide2
	DB	205	;* chBottomSide2
	DB	186	;* chLeftSide2
	DB	186	;* chRightSide2
	DB	24	;* chUpArrow
	DB	25	;* chDownArrow
	DB	27	;* chLeftArrow
	DB	26	;* chRightArrow
	DB	7	;* chBullet
	DB	0FAH	;* chMiddleDot
	DB	0B0H	;* chScrollbar
	DB	' '	;* chElevator
	DB	0B1H	;* chShadowInit
	DB	009H	;* chClose
	DB	01FH	;* chZoomIn
	DB	01EH	;* chZoomOut
	DB	012H	;* chUpDownArrow
	DB	01DH	;* chLeftRightArrow
ELSE
;*	* Kanji characters (for Sanyo OAX card)
	DB	00AH	;* chTopLeftCorner1
	DB	00BH	;* chTopRightCorner1
	DB	00DH	;* chBottomLeftCorner1
	DB	00CH	;* chBottomRightCorner1
	DB	008H	;* chTopSide1
	DB	008H	;* chBottomSide1
	DB	009H	;* chLeftSide1
	DB	009H	;* chRightSide1
	DB	00EH	;* chMiddleLeft1
	DB	010H	;* chMiddleRight1
	DB	015H	;* chTopLeftCorner2
	DB	016H	;* chTopRightCorner2
	DB	018H	;* chBottomLeftCorner2
	DB	017H	;* chBottomRightCorner2
	DB	013H	;* chTopSide2
	DB	013H	;* chBottomSide2
	DB	014H	;* chLeftSide2
	DB	014H	;* chRightSide2
	DB	004H	;* chUpArrow
	DB	005H	;* chDownArrow
	DB	007H	;* chLeftArrow
	DB	006H	;* chRightArrow
	DB	'>'	;* chBullet
	DB	'*'	;* chMiddleDot
	DB	07FH	;* chScrollbar
	DB	0DBH	;* chElevator
	DB	' '	;* chShadowInit or 7Fh
	DB	'C'	;* chClose
	DB	'?'	;* chZoomIn
	DB	'?'	;* chZoomOut
	DB	002H	;* chUpDownArrow
	DB	003H	;* chLeftRightArrow
ENDIF ;KANJI
;*	* filler for "inch" structure
	DW	16 DUP (?)	;* reserved
	Assert <($-inch) EQ cbInchMin>
ENDIF ;!DUALOS2

;*****************************************************************************

labelB	<PUBLIC, insj>			;* jump table (see header for INSJ)
	DD	ImodeGuessCurrentCsd
	DD	FQueryInstCsd
	DD	FInitCsd
	DD	TermCsd	
	DD	MoveHwCursCsd
	DD	FQueryInftCsd
	DD	FGetColorPaletteCsd
	DD	SetColorPaletteCsd
	DD	PrepUpdateCsd
	DD	DoUpdateCsd
	DD	DoneUpdateCsd
	DD	SpecialUpdateCsd

;*	* screen save
	DD	CbSizeVidsCsd
	DD	FSaveVidsCsd
	DD	FRestoreVidsCsd
	DD	SaveVidDataCsd
	DD	RestoreVidDataCsd
	DD	EnableVidsMonitorCsd

	DD	BltArcCsd
	DD	GetCharMapCsd
	Assert <(($ - insj) / 4) EQ cpfnCsdMin>

sEnd	DATA

;*****************************************************************************

sBegin	BSS
    assumes DS,DGROUP

IFDEF DUALDOS3
externW	rgwDataCsd
;*	* NOT NEEDED FOR DUALOS2 - SUPPLIED BY FX_CSD3
;*	* NOTE: Assertion cbDataCsd(fxdcsd3) <= cbDataCsd(fxdcsd5)
ELSE ;!DUALDOS3
globalW	rgwDataCsd, <(cbDataCsd / 2) DUP (?)>	;* Screen driver data
ENDIF ;!DUALDOS3

sEnd	BSS

;*****************************************************************************
