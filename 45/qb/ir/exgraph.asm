page	49,132
	TITLE	exgraph.asm - graphics function and statement executors
;***
;exgraph.asm - graphics function and statement executors for QBI
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;
;****************************************************************************

	.xlist
	include 	version.inc
EXGRAPH_ASM = ON
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	opintrsc
	IncludeOnce	opstmt
	IncludeOnce	context
	IncludeOnce	architec
	IncludeOnce	extort
	IncludeOnce	rttemp
	IncludeOnce	array
	.list

assumes es, NOTHING
assumes ss, DATA

sBegin	DATA
	staticW	fGetPutPalette,-1
sEnd	DATA

sBegin	CODE
assumes cs, CODE

;==============================================================================
;		Intrinsic Graphics Function Executors
;==============================================================================
MakeExe exFnPmap,opFnPmap		;(R4, I2)
;Added with [3]
	pop	ax			;Clear I2 off stack
	sub	sp,4			;Make room for R4
	mov	bx,sp
	fstp	dword ptr DGROUP:[bx]	;Transfer R4 to local stack
	push	ax			;I2 follows it
	fwait
;End of [3]
	CALLRT	B$PMAP,DispR4

MakeExe exFnPoint1,opFnPoint1
	CALLRT	B$PNT1,DispR4

MakeExe exFnPoint2I2,opFnPoint2
	CallRt	B$PNI2,DispAx

MakeExe exFnPoint2R4,opFnPoint2		
;Added with [3]
	sub	sp,8			;Make room for two R4s
	mov	bx,sp
	fstp	dword ptr DGROUP:[bx]	;Transfer R4 to local stack
	fstp	dword ptr DGROUP:[bx+4]
	fwait
;End of [3]
	CallRt	B$PNR4,DispAx

;==============================================================================
;		Graphics Statement Executors
;==============================================================================
MakeExe exCoordI2,opCoord
	CALLRT	B$N1I2,Disp

MakeExe exCoordStepI2,opCoordStep
	CALLRT	B$S1I2,Disp

MakeExe exCoordSecondI2,opCoordSecond
	CALLRT	B$N2I2,Disp

MakeExe exCoordStepSecondI2,opCoordStepSecond
	CALLRT	B$S2I2,Disp

MakeExe exCoordR4,opCoord
;Added with [3]
	sub	sp,8			;Make room for two R4s
	mov	bx,sp
	fstp	dword ptr DGROUP:[bx]	;Transfer R4 to local stack
	fstp	dword ptr DGROUP:[bx+4]
	fwait
;End of [3]
	CALLRT	B$N1R4,Disp

MakeExe exCoordStepR4,opCoordStep
;Added with [3]
	sub	sp,8			;Make room for two R4s
	mov	bx,sp
	fstp	dword ptr DGROUP:[bx]	;Transfer R4 to local stack
	fstp	dword ptr DGROUP:[bx+4]
	fwait
;End of [3]
	CALLRT	B$S1R4,Disp

MakeExe exCoordSecondR4,opCoordSecond
;Added with [3]
	sub	sp,8			;Make room for two R4s
	mov	bx,sp
	fstp	dword ptr DGROUP:[bx]	;Transfer R4 to local stack
	fstp	dword ptr DGROUP:[bx+4]
	fwait
;End of [3]
	CALLRT	B$N2R4,Disp

MakeExe exCoordStepSecondR4,opCoordStepSecond
;Added with [3]
	sub	sp,8			;Make room for two R4s
	mov	bx,sp
	fstp	dword ptr DGROUP:[bx]	;Transfer R4 to local stack
	fstp	dword ptr DGROUP:[bx+4]
	fwait
;End of [3]
	CALLRT	B$S2R4,Disp


MakeExe exCircleStart,opCircleStart
;Added with [3]
	sub	sp,4			;Make room for R4
	mov	bx,sp
	fstp	dword ptr DGROUP:[bx]	;Transfer R4 to local stack
	fwait
;End of [3]
	CALLRT	B$CSTT,Disp

MakeExe exCircleEnd,opCircleEnd
;Added with [3]
	sub	sp,4			;Make room for R4
	mov	bx,sp
	fstp	dword ptr DGROUP:[bx]	;Transfer R4 to local stack
	fwait
;End of [3]
	CALLRT	B$CSTO,Disp

MakeExe exCircleAspect,opCircleAspect
;Added with [3]
	sub	sp,4			;Make room for R4
	mov	bx,sp
	fstp	dword ptr DGROUP:[bx]	;Transfer R4 to local stack
	fwait
;End of [3]
	CALLRT	B$CASP,Disp

MakeExe exStCircle,opStCircle
	PUSHI	ax,-1			;default color attribute
	SkipExHeader
MakeExe exStCircleColor,opStCircleColor
;Added with [3]
	pop	ax			;Clear I2 off stack
	sub	sp,4			;Make room for R4
	mov	bx,sp
	fstp	dword ptr DGROUP:[bx]	;Transfer R4 to local stack
	push	ax			;I2 follows it
	fwait
;End of [3]
	CALLRT	B$CIRC,Disp

MakeExe exStColor,opStColor
	LODSWTX 			;fetch count of parms already on stack
	push	ax
	CALLRT	B$COLR,Disp

MakeExe exStDraw,opStDraw
	CALLRT	B$DRAW,Disp

MakeExe exStGraphicsGet,opStGraphicsGet
PaletteUsing:
	inc	[fGetPutPalette]	;tell shared code this is a GET
	SkipExHeader
MakeExe exStGraphicsPut,opStGraphicsPut
	;This executes may be preceeded by either a exAdRf or an exAIdRf.
	;The exAdRf is used when cDims == 0 and the exAIdRf is used otherwise.

	cmp	byte ptr es:[si-6],0	; Is cDims == 0
	je	NoDims			; Brif yes

	;The preceeding executor was an exAIdRf which consumed the indices
	;and left the far pointer to the data on the stack.  We need to push
	;the pointer to the array descriptor.

	mov	bx,PTRTX[si-4]		; BX = oVar
	call	OVarToPAd		; BX = pAd
	push	bx			

GetPutUsingCom:
	mov	cx,-1
	xchg	cx,[fGetPutPalette]	;fetch flag, and reset to default
	jcxz	GraphicsGet		;brif we want to do a GET here

	inc	cx			;cx = 0 if doing a PUT
	jcxz	GraphicsPut

	CALLRT	B$PALU,DispMov 	;not GET or PUT, must be PALETTE USING

NoDims:
	;The preceeding executor was an exAdRf which left pAd on the stack.
	;We need to push the far address of the data and then the pAd.

	pop	bx			; BX = pAD
	push	[bx.AD_fhd.FHD_hData]	;Segment of first data item
        push    [bx.AD_fhd.FHD_oData]   ;Offset to first data item
        push    bx                      ;pAD
        jmp     short GetPutUsingCom    ;Back to common code

GraphicsPut:
	LODSWTX 			;fetch 'function' operand
	or	ah,ah
	jns	Not_Defaulted		;brif an action code was specified

	mov	ax,4			;'XOR', the default action code
Not_Defaulted:
	push	ax
	CALLRT	B$GPUT,DispMov

GraphicsGet:
	CALLRT	B$GGET,DispMov

MakeExe exStLineStyle,opStLineStyle
	pop	ax
	PUSHI	bx,-1			;default for the color parm
	jmp	short Line_Common	;push style parm back on and go

MakeExe exStLine,opStLine
	PUSHI	ax,-1			;default for first (color) parm
	SkipExHeader
MakeExe exStLineColor,opStLineColor
	mov	ax,-1			;default for second (style) parm
Line_Common:
	push	ax			;push second parm and go
	SkipExHeader
MakeExe exStLineStyleColor,opStLineStyleColor
	LODSWTX 			;fetch BorF parameter and push it
	push	ax
	CALLRT	B$LINE,Disp

MakeExe exStPaint2,opStPaint2
	CALLRT	B$PAIN,Disp

MakeExe exStPaint2Tile,opStPaint2
	PUSHI	ax,-1			;Push p to null descriptor
	SkipExHeader
MakeExe exStPaint3,opStPaint3
	CALLRT	B$PNTC,Disp

MakeExe exStPaletteUsing,opStPaletteUsing
	inc	[fGetPutPalette]	;tell shared code this is PALETTE USING
	jmp	PaletteUsing

MakeExe exStPalette0,opStPalette0
	CALLRT	B$PAL0,Disp

MakeExe exStPalette2,opStPalette2
	CALLRT	B$PAL2,Disp

MakeExe	exStPCopy,opStPCopy
	CALLRT	B$PCPY,Disp

MakeExe exStPreset,opStPreset
	CALLRT	B$PRST,Disp

MakeExe exStPresetColor,opStPresetColor
	SkipExHeader
MakeExe exStPsetColor,opStPsetColor
	CALLRT	B$PSTC,Disp

MakeExe exStPset,opStPset
	CALLRT	B$PSET,Disp

MakeExe exStView0,opStView0
	CALLRT	B$VEW0,Disp

MakeExe exStView,opStView
	xor	ax,ax			;set SCREEN flag to 0 (FALSE)
	SkipExHeader
MakeExe exStViewScreen,opStViewScreen	;NOTE: depending on ax != zero here ...
	DbAssertRel	ax,nz,0,CODE,<exStViewScreen: ax == 0 on entry>
View_Common:
	push	ax			;push flag stating whether SCREEN was
					;  given
	CALLRT	B$VIEW,Disp

MakeExe exStWindow0,opStWindow0
	CALLRT	B$WIN0,Disp

MakeExe exStWindow,opStWindow
	xor	ax,ax			;set SCREEN flag to 0 (FALSE)
	SkipExHeader
MakeExe exStWindowScreen,opStWindowScreen ;NOTE: depending on ax != zero here ..
	DbAssertRel	ax,nz,0,CODE,<exStWindowScreen: ax == 0 on entry>
Window_Common:
;Added with [3]
;Pop 4 R4s from 8087 stack onto 8086 stack
	sub	sp,16
	mov	bx,sp
	fstp	dword ptr DGROUP:[bx]	;Top of 8087 stack to top of 8086 stack
	fstp	dword ptr DGROUP:[bx+4]
	fstp	dword ptr DGROUP:[bx+8]
	fstp	dword ptr DGROUP:[bx+12]
	fwait
;End of [3]
	push	ax			;push flag stating whether SCREEN was
					;  given
	CALLRT	B$WIND,Disp

sEnd	CODE
end
