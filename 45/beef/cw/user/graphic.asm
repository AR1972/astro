;*
;*	CW : Character Windows
;*
;*	graphic.asm : GPD/GSD interface layer

	include user.inc
	include	indrv.inc
	include	ingxd.inc

;*****************************************************************************

externFP	<RerrLoadDrv,FreeIndv>

;*****************************************************************************

sBegin	BSS
    assumes DS,DGROUP

	Assert <cpfnGsdMin EQ cpfnGpdMin>

;*	* INDJ structure
labelB	<PUBLIC, indjGsd>
	DD	cpfnGsdMin DUP (0)

labelB	<PUBLIC, indjGpd>
	DD	cpfnGpdMin DUP (0)

sEnd	BSS

sBegin	DATA
    assumes DS,DGROUP

globalW	fPrinting, 0
globalW	pindjCur, <dataOffset indjGsd>

labelB	<PUBLIC, indvGsd>		;* Graphic Screen
	DB	indtGraphicScreen,0
	DW	dataOffset indjGsd
	DW	cpfnGsdMin
	DW	cpfnGsdMin
	DW	0
	DW	0

labelB	<PUBLIC, indvGpd>		;* Graphic Printer
	DB	indtGraphicPrinter,0
	DW	dataOffset indjGpd
	DW	cpfnGpdMin
	DW	cpfnGpdMin
	DW	0
	DW	0

sEnd	DATA

;*****************************************************************************
;* Load GSD file


sBegin	INIT
    assumes CS,INIT
    assumes DS,DGROUP
    assumes SS,DGROUP


;********** FreeGsd/Gpd **********
;*	entry:	bx = pindv for which driver
;*	* Free a GSD/GPD driver
;*	exit:	n/a

;* GPD free
LabelFP	<PUBLIC, FreeGpd>
	mov	bx,dataOffset indvGpd
	jmp	short FreeGsdGpd

;* GSD free
LabelFP	<PUBLIC, FreeGsd>
	mov	bx,dataOffset indvGsd

cPublic	FreeGsdGpd
cBegin	FreeGsdGpd
;* enter with BX => indv
	cCall	FreeIndv, <bx>
	
cEnd	FreeGsdGpd


;********** FLoadGsd/Gpd **********
;*	entry:	szFile = file name
;*		bx = pindv for which driver
;*	* Try and load a GSD/GPD driver
;*	exit:	AX != 0 if loaded successfully

;* GPD load
LabelFP	<PUBLIC, FLoadGpd>
	mov	bx,dataOffset indvGpd
	jmp	short FLoadGsdGpd

;* GSD load
LabelFP	<PUBLIC, FLoadGsd>
	mov	bx,dataOffset indvGsd

cPublic	FLoadGsdGpd
    parmDP szFile
cBegin	FLoadGsdGpd
;* enter with BX => indv
	mov	cx,1
	Save	<bx>
	cCall	RerrLoadDrv, <szFile, bx, cx>
	or	ax,ax
	mov	ax,0
	jnz	end_fload			;* load error
	mov	cx,[bx].psLoadedIndv
	jcxz	end_fload			;* not loaded
	inc	ax				;* success
end_fload:
	
cEnd	FLoadGsdGpd


sEnd	INIT

;*****************************************************************************

sBegin	SCREEN
    assumes CS,SCREEN
    assumes DS,DGROUP
    assumes SS,DGROUP

;********** SetPrinting **********
;*	entry: fEnablePrinting => whether we want printing or not
;*	* set up globals for subsequent graphic driver calls

cPublic	SetPrinting
    parmW  fEnablePrinting
cBegin	SetPrinting

	mov	cx,fEnablePrinting
	mov	ax,dataOffset indjGsd
	jcxz	@F				;* back to screen
	mov	ax,dataOffset indjGpd
@@:
	mov	pindjCur,ax
	mov	fPrinting,cx

cEnd	SetPrinting


;*****************************************************************************
;* Driver Stubs (see Documentation for interface details)

GD_ENTRY	MACRO label
labelFP <PUBLIC, label>
	mov	bx,pindjCur
	AssertNE <word ptr [bx].pfn&label&Indj>,0	;* trap empty vector
	jmp	[bx].pfn&label&Indj
ENDM

	GD_ENTRY	FInitGraphics
	GD_ENTRY	TermGraphics
	GD_ENTRY	Move
	GD_ENTRY	Draw
	GD_ENTRY	SetAreaPat
	GD_ENTRY	SetLinePat
	GD_ENTRY	SetLineWeight
	GD_ENTRY	SetColor
	GD_ENTRY	Text
	GD_ENTRY	Rectangle
	GD_ENTRY	Arc
	GD_ENTRY	Polygon
	GD_ENTRY	BitBlt

;*****************************************************************************

sEnd	SCREEN

	END
