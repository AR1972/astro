	TITLE	LLEGASUP - LowLevel EGA support (shared routines)
;***
; LLEGASUP - LowLevel EGA support
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	This module contains support routines extracted from LLEGA.ASM
;	which are shared by EGA and VGA functions.
;
;******************************************************************************

	INCLUDE switch.inc	;feature switches
	INCLUDE rmacros.inc

	USESEG	_BSS
	USESEG	GR_TEXT

	INCLUDE seg.inc
	INCLUDE ibmunv.inc
	INCLUDE baslibma.inc
	INCLUDE llgrp.inc

sBegin	_BSS
;
;
; ***************************************************************************
; External variables
; ***************************************************************************
;
externB b$EgaPalSup
externD b$InitPalette
externB b$MaxColor
externB b$Monitor
externW B$LEOFST
externW B$REOFST
externW B$VTOFST
externW B$VBOFST
externW B$VLOFST
externW B$VROFST
externB B$VLMASK
externB B$VRMASK
externD b$AddrC
externB b$AttrC
externW b$BytesPerRow
externB b$DivShift
externB b$EgaWrMd
externW b$Incr1
externW b$Incr2
externW b$IncrY
externB b$MaskC
externB b$MaxAttr
externW b$ModMask
externW b$OffC
externB b$PaintBorder
externW b$PenC
externB b$Planes
externW b$SegC
externB b$Tiling
externW b$SaveCa
externB b$SaveCm
externB b$PlaneMask
;
; ***************************************************************************
; External function vectors
; ***************************************************************************
;
externW b$PutVector
externW b$ReadC
externW b$PalTrans
externW b$PalPut
;
; ***************************************************************************
; Local variables
; ***************************************************************************
staticB AttrTile,,4		;tile attribute pattern table
staticB ColorBits,,8		;table of rotated tiling patterns
staticW ByteCount,,1
staticB EgaPalette,,17
;

sEnd	_BSS

assumes CS,GR_TEXT

sBegin	GR_TEXT

externNP B$BumpDS
externNP B$BumpES		
externNP B$DecDS		
externNP B$OutWord
externNP B$ResetEGA

;***
; B$EgaMapXYC_D
;
;Purpose:
;	Map given X and Y coordinates to the graphics cursor for Screen 7.
;Entry:
;	CX = X coordinate
;	DX = Y coordinate
;Exit:
;	b$OffC, b$MaskC updated
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaMapXYC_D,<PUBLIC,NEAR>
cBegin
	mov	bx,dx		;multiply y by 40 to compute row displacement
	shl	dx,1		;dx=2*Y
	shl	dx,1		;dx=4*Y
	add	dx,bx		;dx=5*Y
	jmp	short MapCommon
cEnd	<nogen>

;***
; B$EgaMapXYC
;
;Purpose:
;	Map given X and Y coordinates to the graphics cursor for EGA modes
;Entry:
;	CX = X coordinate
;	DX = Y coordinate
;Exit:
;	b$OffC, b$MaskC updated
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaMapXYC,<PUBLIC,NEAR>
cBegin
	mov	bx,dx		;multiply y by 80 to compute row displacement
	shl	dx,1		;dx=2*Y
	shl	dx,1		;dx=4*Y
	add	dx,bx		;dx=5*Y
	shl	dx,1		;dx=10*Y
MapCommon:
	shl	dx,1		;dx=20*Y
	shl	dx,1		;dx=40*Y
	shl	dx,1		;dx=80*Y   (40*Y for mode 0DH)
	mov	ax,cx		;save x
	shr	ax,1		;div by PixelsPerByte (8)
	shr	ax,1		;  to get byte index
	shr	ax,1
	add	dx,ax		;add x byte offset to y row address
	mov	b$OffC,dx	;save byte offset
	and	cl,7		;mask in x bit addr
	mov	ch,10000000B	;leftmost pixel on in shift mask
	shr	ch,cl		;move over to get mask
	mov	b$MaskC,ch	;store cursor mask
cEnd

;***
; B$EgaLeftC/B$EgaLeftC_13
;
;Purpose:
;	Move graphics cursor left 1 pixel for EGA modes or Screen 13,
;	depending on entry point.  No test is made for screen boundaries.
;Entry:
;	None
;Exit:
;	b$MaskC, b$OffC updated
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaLeftC,<PUBLIC,NEAR>
cBegin
	rol	b$MaskC,1
	jc	LeftCnext	;go if not in same byte
	ret
labelNP <PUBLIC,B$EgaLeftC_13>
LeftCnext:
	dec	b$OffC 	;move byte pointer
cEnd

;***
; B$EgaChkUpC
;
;Purpose:
;	Move graphics cursor up 1 pixel for EGA modes.	A test is made for
;	boundaries.  If it is a boundary then PSW.C is set upon return and
;	no move is made.
;
;Entry:
;	None
;Exit:
;	b$MaskC, b$OffC updated
;	PSW.C = set if original graphics cursor was on screen top edge.
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaChkUpC,<PUBLIC,NEAR>
cBegin
	MOV	AX,b$OffC	;[AX] = cursor offset
	CMP	AX,B$VTOFST	;already on top of viewport ?
	JAE	B$EgaUpC	;less than VTOFST means on top of
				;viewport
	STC			;STC to indicate that cursor already
cEnd				;on top of viewport

;***
; B$EgaUpC
;
;Purpose:
;	Move graphics cursor up 1 pixel for EGA modes.	No test is made for
;	screen boundaries.
;Entry:
;	None
;Exit:
;	b$MaskC, b$OffC updated
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaUpC,<PUBLIC,NEAR>
cBegin
	mov	ax,b$BytesPerRow 
	sub	b$OffC,ax	;up one row
	clc			;(no error for ChkUpC)
cEnd

;***
; B$EgaChkDownC
;
;Purpose:
;	Move graphics cursor down 1 pixel for EGA modes.  If beyond the
;	bottom edge, PSW.C is set upon return and no move is made.
;Entry:
;	None
;Exit:
;	PSW.C = set if original cursor was on screen edge.
;	b$MaskC, b$OffC Updated otherwise
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaChkDownC,<PUBLIC,NEAR>
cBegin
	MOV	AX,b$OffC	;[AX] = cursor offset
	CMP	AX,B$VBOFST	;already at the bottom of viewport?
	JB	B$EgaDownC	;Brif not
	STC			;STC to indicate that cursor is
cEnd				;on bottom of viewport

;***
; B$EgaDownC
;
;Purpose:
;	Move graphics cursor down 1 pixel for EGA modes.  No test is made for
;	screen boundaries.
;Entry:
;	None
;Exit:
;	b$MaskC, b$OffC Updated
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaDownC,<PUBLIC,NEAR>
cBegin
	mov	ax,b$BytesPerRow 
	add	b$OffC,ax	;down one row
	clc			;(no error for ChkDownC)
cEnd

;***
; B$EgaSetAttr
;
;Purpose:
;	Set b$AttrC to user-supplied attribute for EGA modes.	If
;	user-supplied attribute is outside legal range, use maximum legal
;	attribute.
;Entry:
;	AL = attribute
;Exit:
;	b$AttrC set to new attribute
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaSetAttr,<PUBLIC,NEAR>,<AX>
cBegin
	cmp	al,b$MaxAttr	;test against maximum attribute
	jbe	SetAttrOk	;Brif legal
	mov	al,b$MaxAttr	;limit to max
SetAttrOk:
	mov	b$AttrC,al
	clc			;exit no error
cEnd

;***
; B$EgaReadC
;
;Purpose:
;	For EGA modes, return the attribute of the current pixel as
;	specified by b$MaskC and b$OffC.
;
;Entry:
;	b$MaskC and b$OffC specify pixel to read
;Exit:
;	AL = attribute of pixel
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaReadC,<PUBLIC,NEAR>,<SI,ES>
cBegin
	les	si,b$AddrC
	mov	bx,0103H	;start at plane 3 and dec by 1 for 4 planes
ReadCX1:
	EGAINT10CLI		; disable interrupts if using EGAINT10
	MOV	DX,GRPADD	;address of graphics index register
	MOV	AX,RWMREG	;r/w mode index in AL, 0 in AH
	OutWord 		;set read mode to 0 so that we read byte
				;specified as 0/1 for one plane at a time
	MOV	AL,RMPREG	;AL=index to read map reg
	OUT	DX,AL		;indicate next data for read map register
	INC	DX		;data port address
	MOV	al,bl		;al = plane number
	xor	ah,ah		;ah = color accumulator
	MOV	cl,b$MaskC	;bit position
ReadC1:
	OUT	DX,AL		;indicate plane to read
	MOV	ch,ES:[si]	;get 8 bits for plane
	AND	ch,cl		;isolate bit to read
	neg	ch		;carry = (ch==0)?0:1
	rcl	ah,1		;shift bit into color accumulator
	sub	al,bh		;reference next color plane to read
	jae	ReadC1		;do next plane
	XCHG	AH,AL		;color attribute returned in AL
	CALL	B$ResetEGA	;set up EGA for next BIOS write
cEnd

;***
; B$EgaReadC_F
;
;Purpose:
;	For Screen 10, return the attribute of the current pixel as
;	specified by b$MaskC and b$OffC.
;
;Entry:
;	b$MaskC and b$OffC specify pixel to read
;Exit:
;	AL = attribute of pixel
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaReadC_F,<PUBLIC,NEAR>,<SI,ES>
cBegin
	les	si,b$AddrC
	mov	bx,0202H	;start at plane 2 and dec by 2 for 2 planes
				;  (0&2)
	jmp	short ReadCX1	;continue in common code
cEnd	<nogen>

;***
; B$EgaReadC_64K
;
;Purpose:
;	For Screen 9/64K, return the attribute of the current pixel as
;	specified by b$MaskC and b$OffC.
;
;Entry:
;	b$MaskC and b$OffC specify pixel to read
;Exit:
;	AL = attribute of pixel
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaReadC_64K,<PUBLIC,NEAR>,<SI,ES>
cBegin
	les	si,b$AddrC
	mov	bx,si		;copy video address
	mov	bh,2		;bh = dec by 2 between planes
	and	bl,1		;1 if odd, 0 if even
	add	bl,bh		;bl = plane (use maps 3 and 1 if odd address)
				;	    ( or maps 2 and 0 if even address)
	jmp	short ReadCX1	;continue in common code
cEnd	<nogen>

;***
; B$EgaSetC
;
;Purpose:
;	Set the pixel defined by the current graphics cursor to
;	the current attribute for EGA modes.
;Entry:
;	b$PenC  = cursor mask and attribute
;	b$AddrC = address of pixel byte
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaSetC,<PUBLIC,NEAR>,<ES>	
cBegin
	EGAINT10CLI		; disable interrupts if using EGAINT10
	MOV	DX,GRPADD	;address of graphics index port
	MOV	AL,RWMREG	;index to the graphics controller mode reg
	OUT	DX,AL
	MOV	AL,b$EgaWrMd	;write mode 2; odd/even or sequential addr.
	INC	DX		;address data port
	OUT	DX,AL
	MOV	AL,BMKREG	;index to the bit mask register
	DEC	DX
	OUT	DX,AL
	mov	ax,b$PenC	;[al] = cursor mask, [ah] = attribute
	INC	DX
	OUT	DX,AL		;set bit mask
	les	bx,b$AddrC	;[BX] = cursor offset, [DS] = segment
	XCHG	ah,es:[BX]	;latch screen contents and do a color write
	;reset EGA regs to those expected by the BIOS
	MOV	AL,0FFH 	;set all mask bits
	OUT	DX,AL
	MOV	AL,RWMREG	;index to the graphics controller mode reg
	DEC	DX
	OUT	DX,AL
	MOV	AL,b$EgaWrMd	;odd/even or sequential addr.
	and	al,10H		;set write mode 0 for bios
	INC	DX
	OUT	DX,AL
	EGAINT10STI		; reenable interrupts if using EGAINT10
cEnd

;***
; B$EgaSetPixC
;
;Purpose:
;	Set the pixel defined by the current graphics cursor to
;	the current attribute for EGA modes.  This is identical
;	to B$EgaSetC with the initialization and termination code
;	extracted to make it faster when multiple pixels are being
;	set in a graphics functin (ie: CIRCLE).  A call to
;	B$EgaSetPixFirstC should preceed the first call to
;	B$EgaSetPixC.	A call to B$ResetEGA should follow
;	the last.  B$EgaSetPixFirstC sets up ES which should be
;	preserved for all B$SetPixC calls.
;Entry:
;	ES	= video segment (set up by B$EgaSetPix)
;	b$PenC = cursor mask and attribute
;	b$OffC = address of pixel
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaSetPixC,<PUBLIC,NEAR>
cBegin
	MOV	DX,GRPADD+1	;address graphics data port
	mov	ax,b$PenC	;[al] = cursor mask, [ah] = attribute
	OUT	DX,AL
	mov	bx,b$OffC	;[BX] = cursor offset
				;[ES] = setup by SetPixFirstC
	XCHG	ah,ES:[BX]	;latch screen contents and do a color write
cEnd

;***
; B$EgaSetPixFirstC
;
;Purpose:
;	Set up ES to the video segment and set up EGA write mode so
;	repeated calls can be made to B$EgaSetPixC without having
;	to reinitialize.
;Entry:
;	b$SegC    = video segment
;	b$EgaWrMd = EGA write mode to set up
;Exit:
;	ES = video segment
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaSetPixFirstC,<PUBLIC,NEAR>
cBegin
	mov	es,b$SegC	;ES = video segment for B$SetPix
	MOV	DX,GRPADD	;address of graphics index port
	MOV	AL,RWMREG	;index the graphics controller mode reg
	OUT	DX,AL
	MOV	AL,b$EgaWrMd	;write mode 2; odd/even or sequential addr.
	INC	DX		;address data port
	OUT	DX,AL
	MOV	AL,BMKREG	;index to the bit mask register
	DEC	DX		;leave bit mask register addressed so all
	OUT	DX,AL		;  we have to do is output the mask
cEnd

;***
; B$EgaLineX
;
;Purpose:
;	Draw an X-major line for EGA modes.
;Entry:
;	AH    = color (b$AttrC)
;	AL    = bit accumulator
;	BH    = bit mask
;	BL	unused
;	CX    = point count
;	DX    = EGA graphics controller data port
;	SI    = delta decision value
;	DI    = line style
;	BP    = video offset
;	ES    = video segment
;	Incr1 = major axis delta update value
;	Incr2 = minor axis delta update value
;	IncrY = BP change for Y movement
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaLineX,<PUBLIC,NEAR>
cBegin
	MOV	DX,GRPADD+1	;graphics controller data port for bit mask

LineEXloop:

	ROL	DI,1		;next line style bit
	JNC	LineEX2 	;go if bit is 0 not to plot

	OR	AL,BH		;OR this bit into byte mask
LineEX2:
	OR	SI,SI		;time to move in Y (+ or 0 delta)?
	JNS	LineEX4 	;go if so
	ADD	SI,b$Incr1	;update delta for X movement
	ROR	BH,1		;move to next X
	JC	LineEX3 	;go if not still in same byte
	LOOP	LineEXloop
	JMP	SHORT LineEX7
LineEX3:
	OUT	DX,AL		;output mask from accumulated pixels
	MOV	AL,AH		;color for write
	XCHG	AL,ES:[BP]	;read loads latches, write color thru mask
	XOR	AL,AL		;clear pixel accumulator
	INC	BP		;go to next byte
	LOOP	LineEXloop
	JMP	SHORT LineEX7
LineEX4:
	ADD	SI,b$Incr2	;update delta for Y movement
	OUT	DX,AL		;output mask from accumulated pixels
	MOV	AL,AH		;color for write
	XCHG	AL,ES:[BP]	;read loads latches, write color thru mask
	XOR	AL,AL		;clear pixel accumulator
	ROR	BH,1		;move to next X
	ADC	BP,b$IncrY	;move to next Y (+1 if next X byte)
	LOOP	LineEXloop
LineEX7:			;flush accumulated pixels
	OUT	DX,AL		;output mask from accumulated pixels
	XCHG	AH,ES:[BP]	;read loads latches, write color thru mask
cEnd

;***
; B$EgaLineY
;
;Purpose:
;	Draw a Y-major line for EGA modes.
;Entry:
;	AH    = color (b$AttrC)
;	AL    = bit mask
;	BH    = trash for XCHG
;	BL	unused
;	CX    = point count
;	DX    = EGA graphics controller data port
;	SI    = delta decision value
;	DI    = line style
;	BP    = video offset
;	ES    = video segment
;	Incr1 = major axis delta update value
;	Incr2 = minor axis delta update value
;	IncrY = BP change for Y movement
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaLineY,<PUBLIC,NEAR>
cBegin
	MOV	DX,GRPADD+1	;graphics controller data port for bit mask

LineEYloop:

	ROL	DI,1		;next line style bit
	JNC	LineEY2 	;go if bit is 0 not to plot

	OUT	DX,AL		;output bit mask
	MOV	BH,AH		;color for write
	XCHG	BH,ES:[BP]	;read loads latches, write color thru mask
LineEY2:
	OR	SI,SI		;time to move in X (+ or 0 delta)?
	JNS	LineEY3 	;go if so
	ADD	SI,b$Incr1	;update delta for Y movement
	ADD	BP,b$IncrY	;move to next Y
	LOOP	LineEYloop
	ret
LineEY3:
	ADD	SI,b$Incr2	;update delta for X movement
	ROR	AL,1		;move to next X
	ADC	BP,b$IncrY	;move to next Y (+1 if next X byte)
	LOOP	LineEYloop
cEnd

;***
; B$EgaLineV
;
;Purpose:
;	Draw a vertical line for EGA modes.
;Entry:
;	AH = color (b$AttrC)
;	AL = bit mask
;	BX = IncrY = BP change for Y movement
;	CX = point count
;	DI = line style
;	BP = video offset
;	ES = video segment
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaLineV,<PUBLIC,NEAR>
cBegin
	MOV	DX,GRPADD+1	;graphics controller data port for bit mask
	OUT	DX,AL		;output mask
	MOV	BX,b$IncrY	;to register here
LineEVloop:

	ROL	DI,1		;next line style bit
	JNC	LineEV2 	;go if bit is 0 not to plot

	MOV	AL,AH		;color for write
	XCHG	AL,ES:[BP]	;read loads latches, write color thru mask
LineEV2:
	ADD	BP,BX		;to next Y
	LOOP	LineEVloop
cEnd

;***
; PutPreset/PutOther
;
;Purpose:
;	Support routine for EGA PUT.  Write to the specified screen byte
;	the specified attribute (after NOTing it if entry point is Preset).
;	Write will apply bit-wise logic as defined by the contents of
;	DTRREG, which is set up in PUT action routines which then branch
;	to PutOther.
;Entry:
;	AH    = attribute
;	ES:DI = screen address
;Exit:
;	DI incremented to next screen address
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	PutPreset,<NEAR>
cBegin
	not	ah		;NEGATE DATA FOR PRESET
labelNP PutOther
	xchg	ah,es:[di]	;read loads latches, write sets data according
	inc	di		;  to EGA function set in DTRREG
cEnd

labelW	PutTable		;EGA write function according to put action
	DB	10000B		;Or
	DB	01000B		;And
	DB	00000B		;Preset (data unmodified, but NOT before)
	DB	00000B		;Pset	(data unmodified)
	DB	11000B		;Xor

;***
; B$EgaPutAction_64K
;
;Purpose:
;	Set b$PutVector to appropriate PUT action routine for Screen 9/64K.
;	Set up DTRREG according to PUT action table values so that next
;	screen write will apply the requested PUT action to the data.
;Entry:
;	AL = PUT action [0..4] representing (OR, AND, PRESET, PSET, XOR)
;Exit:
;	b$PutVector set to entry point of appropriate PUT action routine
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaPutAction_64K,<PUBLIC,NEAR>
cBegin
	mov	bl,11001100B	;setup for first plane pair (after two shifts)
	SKIP	2		;fall thru to normal PutAction
cEnd	<nogen>

;***
; B$EgaPutAction_F
;
;Purpose:
;	Set b$PutVector to appropriate PUT action routine for Screen 10.
;	Set up DTRREG according to PUT action table values so that next
;	screen write will apply the requested PUT action to the data.
;Entry:
;	AL = PUT action [0..4] representing (OR, AND, PRESET, PSET, XOR)
;Exit:
;	b$PutVector set to entry point of appropriate PUT action routine
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaPutAction_F,<PUBLIC,NEAR>
cBegin				;(NOTE: SKIP above!!)
	mov	bl,01000100B	;setup for first plane (after two shifts)
	SKIP	2		;fall thru to normal PutAction
cEnd	<nogen>

;***
; B$EgaPutAction
;
;Purpose:
;	Set b$PutVector to appropriate PUT action routine for EGA modes.
;	Set up DTRREG according to PUT action table values so that next
;	screen write will apply the requested PUT action to the data.
;Entry:
;	AL = PUT action [0..4] representing (OR, AND, PRESET, PSET, XOR)
;Exit:
;	b$PutVector set to entry point of appropriate PUT action routine
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaPutAction,<PUBLIC,NEAR>
cBegin
				;(NOTE: SKIPs above!!)
	mov	bl,10001000B	;setup for first plane (after 1 shift)
	mov	b$PlaneMask,bl
	;NOTE:	the plane mask is replicated in both nibbles for wrap-around
	;	shift, but will be masked to the low nibble for map-enabling
	mov	bx,GR_TEXTOFFSET PutPreset  ;assume preset
	cmp	al,2			    ;is it?
	je	IsPreset		    ;go if so
	mov	bx,GR_TEXTOFFSET PutOther   ;all others use same code and
IsPreset:				    ;  rely on EGA for operation
	mov	b$PutVector,bx 	    ;save vector
	mov	bx,GR_TEXTOFFSET PutTable   ;EGA function table
	xlat	cs:[bx] 		    ;convert put action to EGA func
	mov	ah,al
	MOV	DX,GRPADD	;address graphics controller
	mov	al,DTRREG	;data rotate/function register
	OutWord 		;select EGA logical operation
cEnd

	ASSUME	DS:NOTHING

;***
; B$EgaNReadL_F
;
;Purpose:
;	Read a line of pixels from the screen to an array for Screen 10.
;Entry:
;	ES:DI	= screen address
;	DS:SI	= array address
;	CL	= array align shift count
;	BP	= count of bits (not pixels) to read
;	BH	= plane to read from
;Exit:
;	DS:SI	= updated to array byte past point filled
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaNReadL_F,<PUBLIC,NEAR>
cBegin
	shl	bh,1		;logical plane 0 = physical plane 0
				;logical plane 1 = physical plane 2
cEnd	<nogen> 		;fall thru to regular NReadL

;***
; B$EgaNReadL
;
;Purpose:
;	Read a line of pixels from the screen to an array for EGA modes.
;Entry:
;	DS:SI	= screen address
;	ES:DI	= array address
;	CL	= array align shift count
;	CH	= mask for last partial byte
;	BP	= count of bits (not pixels) to read
;	BH	= plane to read from
;Exit:
;	ES:DI	= updated to array byte past point filled
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaNReadL,<PUBLIC,NEAR>
cBegin
	MOV	DX,GRPADD	;address graphics controller
	MOV	al,RMPREG	;read map select register
	out	dx,al		
	mov	al,bh		
	inc	dx		
	out	dx,al		
	mov	ah,[si] 	;preload hi byte
	inc	si		
NRdLoop:
	lodsb			;fill ax word with video bytes
	mov	bh,al		;this lo byte will become next hi byte
	rol	ax,cl		;align to array
	sub	bp,8		;8 bits done
	jbe	NRdLast 	;go if bit count exhausted
	mov	es:[di],ah	;save full byte
	inc	di		
	mov	ah,bh		;move lo byte (BH) to hi byte (AH)
	jnz	NRdLoop 	;loop if no offset overflow
	call	B$BumpES	;move array pointer over segment boundary
	jmp	short NRdLoop	;go do another
NRdLast:
	and	ah,ch		;strip unused bits from last byte
	mov	es:[di],ah	;save last byte
	inc	di		
	jnz	NRdDone 	
	call	B$BumpES	;move array pointer over segment boundary
NRdDone:
cEnd

;***
; B$EgaNWriteL_F
;
;Purpose:
;	Write a line of pixels from an array to the screen for Screen 10.
;Entry:
;	ES:DI	= screen address
;	DS:SI	= array address
;	CL	= array align shift count
;	BP	= count of bits (not pixels) to write
;	DL	= last partial byte mask
;	DH	= first partial byte mask
;Exit:
;	DS:SI	= updated to array byte past point used
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaNWriteL_F,<PUBLIC,NEAR>
cBegin
	rol	b$PlaneMask,1	;logical plane 0 = physical plane 0
				;logical plane 1 = physical plane 2
				;must shift plane mask twice (1 more below)
cEnd	<nogen> 		;fall thru to regular NWriteL

;***
; B$EgaNWriteL
;
;Purpose:
;	Write a line of pixels from an array to the screen for EGA modes.
;Entry:
;	ES:DI	= screen address
;	DS:SI	= array address
;	CL	= array align shift count
;	BP	= count of bits (not pixels) to write
;	DL	= last partial byte mask
;	DH	= first partial byte mask
;	BH	= plane
;Exit:
;	DS:SI	= updated to array byte past point used
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaNWriteL,<PUBLIC,NEAR>
cBegin
	rol	b$PlaneMask,1	;shift plane mask by one plane
	push	dx
	MOV	BL,DH		;first byte bit mask
	MOV	DX,GRPADD	;address graphics controller
	mov	al,BMKREG	;  bit mask register
	OUT	DX,AL		;set index register
	XCHG	AX,BX		;set up data in AL, plane moves to AH
	INC	DX		
	OUT	DX,AL		
	DEC	DX		
	MOV	AL,RWMREG	;set r/w mode 0
	OUT	DX,AL		
	XOR	AL,AL		
	INC	DX		
	OUT	DX,AL		
	DEC	DX		
	MOV	al,RMPREG	;read map select register
	OUT	DX,AL		
	MOV	AL,AH		;[al] = plane number
	INC	DX		
	OUT	DX,AL		
	MOV	DX,SEQADD	;address the sequencer
	MOV	AL,MMREG	;  map mask register
	OUT	DX,AL		
	mov	al,b$PlaneMask ;get plane mask bit
	and	al,0FH		;strip to nibble
	INC	DX		
	OUT	DX,AL		
	pop	dx

	mov	ah,[si] 	;preload byte from array
	inc	si
	jnz	NWrOvfl1	
	call	B$BumpDS	;move array pointer over segment boundary
NWrOvfl1:
.erre	ID_SSEQDS		;assumes ss = ds
	mov	bx,ss:[b$PutVector]	;preload PUT action vector
	ror	ax,cl		;align to video
	add	bp,cx
	sub	bp,8		;account for first partial byte
	jbe	NWrLast 	;go if last byte
	call	bx		;put the byte
	mov	dh,0FFH 	;mask for whole bytes in the middle
	push	ax
	push	dx
	mov	ah,dh		;middle byte bit mask
	MOV	DX,GRPADD	;address graphics controller
	mov	al,BMKREG	;  bit mask register
	OUT	DX,AL		
	XCHG	AL,AH		;bit mask to AL and output
	INC	DX		
	OUT	DX,AL		
	EGAINT10STI		;reenable ints if using EGAINT10
	pop	dx
	pop	ax
	jmp	short NWrLoop2
NWrLoop:
	call	bx		;put the byte via PUT action vector
NWrLoop2:
	rol	ax,cl		;re-align to array
	lodsb			;fill ax word with array bytes
	or	si,si		
	jz	NWrOvfl3	;go if address overflow
NWrOvfl2:
	xchg	ah,al		
	ror	ax,cl		;align to video
	sub	bp,8		;8 bits done
	ja	NWrLoop 	;go if bit count not exhausted
	add	bp,8		;restore BP to #bits in last byte
	cmp	cx,bp		;did we use any of the second byte?
	jb	NWrLast 	;go if so
	or	si,si		;at start of segment?
	jnz	NWrUnfl 	;go if not
	call	B$DecDS	;backup to previous segment
NWrUnfl:			
	dec	si		;move ptr back
NWrLast:
	push	ax
	and	dh,dl		;combine first|middle mask with end mask
	mov	ah,dh		;last byte bit mask
	MOV	DX,GRPADD	;address graphics controller
	mov	al,BMKREG	;  bit mask register
	EGAINT10CLI		;disable ints for direct EGA manipulation
	OUT	DX,AL		
	XCHG	AL,AH		
	INC	DX		
	OUT	DX,AL		;output bit mask
	DEC	DX		
	pop	ax
.erre	ID_SSEQDS		;assumes ss = ds
	call	bx		;put the last byte
	EGAINT10STI		;reenable ints if using EGAINT10
cEnd

NWrOvfl3:			
	call	B$BumpDS	;move array pointer over segment boundary
	jmp	short NWrOvfl2	;back to loop

	ASSUME	DS:DGROUP

;***
; B$EgaNSetC
;
;Purpose:
;	Set a horizontal line of pixels to the current attribute for EGA
;	modes.	The line starts at the current cursor position and moves right.
;
;	This becomes a bit of a mess with EGAINT10 and QCG support.  Here's
;	the current approach:
;	    No EGAINT10 (QCG or not, doesn't matter) --
;		No problem, just execute straight code, modifying the EGA
;		hardware directly where needed.
;	    EGAINT10 and no QCG --
;		Disable ints at the beginning and reenable them at the end.
;		This will work because in the worst case the entire routine
;		takes approx. 1500 clock cycles, and we are using a ceiling
;		of 1800 clock cycles between CLI/STI.
;	    EGAINT10 and QCG --
;		Now things get ugly.  We can't just disable ints over the
;		whole routine because it would exceed the 1800 clock max.
;		Instead, we use a combination of writing regs through the
;		EGAINT10 interface (like setting RWMReg, because it's needed
;		for the entire routine), and modifying regs directly within
;		small CLI/STI brackets (like setting BMKReg for a byte and
;		then restoring it before reenabling ints).
;Entry:
;	b$AddrC specifies start pixel
;	b$PenC = cursor mask and attribute
;	BX	= number of pixels to set
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaNSetC,<PUBLIC,NEAR>,<ES,DI>
cBegin
	EGAINT10CLI		;disable ints for entire routine if EGAINT10
	MOV	DX,GRPADD	;address of graphics index register
	MOV	AL,RWMREG	;r/w mode register
	OUT	DX,AL		;index the mode register
	MOV	AL,b$EgaWrMd	;write mode 2; odd/even or sequential addr.
	INC	DX		;to data port
	OUT	DX,AL		
	DEC	DX		;back to index port
	MOV	AL,BMKREG	
	OUT	DX,AL		;index the mask register
	INC	DX		;to data port
	les	di,b$AddrC	;graphics cursor address
	mov	cx,b$PenC	;ch = desired attribute
				;cl = cursor mask
	or	cl,cl		;left aligned in byte?
	js	NSet2a		;go if so, skip single-bit start
NSet1:
	xor	ah,ah		;zero out new mask accumulator
NSet2:
	or	ah,cl		;include this pixel in mask
	dec	bx		;decrement pixel count
	jz	NSet4		;treat as last byte if bit count exhausted
	ror	cl,1		;move 1 pixel right
	jnb	NSet2		;continue if not right-most bit
	mov	al,ah		;mask to AL for OUT
	out	dx,al		;set bit mask
	mov	al,ch		;copy desired attribute
	xchg	al,es:[di]	;set the byte
	inc	di		;bump cursor byte pointer
NSet2a:
	push	cx
	mov	al,0FFH 	;set bit mask for full bytes
	out	dx,al
	mov	ax,bx		;remaining bit count
	mov	cl,b$DivShift	;pixels/byte divisor shift
	shr	ax,cl		;compute full byte count
	jz	NSet3		;go do remaining bits if no full bytes
	xchg	ax,cx		;byte count to cx
	mov	al,ah		;attribute byte
	rep	stosb		;block write full bytes
NSet3:
	pop	cx
	and	bx,b$ModMask	;mask in remaining bit count
	jnz	NSet1		;go do remaining bits
	jmp	short NSet5	;no bits remaining - exit
NSet4:				;update last byte
	mov	al,ah		;mask to AL for OUT
	out	dx,al		;set bit mask
	xchg	ch,es:[di]	;set the byte
NSet5:
	EGAINT10STI		;reenable ints if using EGAINT10
	CALL	B$ResetEGA	;call routine to reset write mode for BIOS
cEnd


;***
; B$EgaPaintBound_11
;
;Purpose:
;	Called by PAINT before painting each scan line for
;	Screen 11 to facilitate fast viewport edge detection.
;	Set VIEW left and right cursor addresses and masks.
;Entry:
;	b$OffC = address of current pixel
;Exit:
;	B$LEOFST = left edge offset
;	B$REOFST = right edge offset
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaPaintBound_11,<PUBLIC,NEAR>
cBegin
	mov	AX,b$OffC	;video offset
	MOV	CX,80		;divisor = 80
	XOR	DX,DX
	DIV	CX		;quotient in AX = row # (INT(b$OffC/80))
	SHL	AX,1		;row * 2
	JMP	SHORT PntBndCommon ;finish the math and store the results
cEnd	<nogen>

;***
; B$EgaPaintBound_D
;
;Purpose:
;	Called by PAINT before painting each scan line for
;	Screen 7 to facilitate fast viewport edge detection.
;	Set VIEW left and right cursor addresses and masks.
;Entry:
;	b$OffC = address of current pixel
;Exit:
;	B$LEOFST = left edge offset
;	B$REOFST = right edge offset
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaPaintBound_D,<PUBLIC,NEAR>
cBegin
	mov	ax,b$OffC	;video offset
	MOV	CX,40		;divisor = 40
	XOR	DX,DX
	DIV	CX		;quotient in AX = INT(CLOC/40) -- this is row
	CALL	PntBndCommon	;finish the math and store the results
	JMP	SHORT SetColorBits ;set up ColorBits array and exit
cEnd	<nogen>

;***
; B$EgaPaintBound
;
;Purpose:
;	Called by PAINT before painting each scan line for
;	EGA modes to facilitate fast viewport edge detection.
;	Set VIEW left and right cursor addresses and masks.
;Entry:
;	b$OffC = address of current pixel
;Exit:
;	B$LEOFST = left edge offset
;	B$REOFST = right edge offset
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaPaintBound,<PUBLIC,NEAR>
cBegin
	mov	ax,b$OffC	;video offset
	MOV	CX,80		;divisor = 80
	XOR	DX,DX
	DIV	CX		;quotient in AX = INT(CLOC/80) -- this is row
	SHL	AX,1		;row * 2
	CALL	PntBndCommon	;finish the math and store the results

;	In order for SCANL and SCANR to be able to check whether any tiles
;	will actually change, we need bit-specific information about the
;	tile pattern for the current byte.  This is collected as binary
;	representations of the colors of each bit, and stored in the array
;	ColorBits.
;
;	At this point, the SetTile routine has set up the AttrTile array
;	with the tile pattern for this row. Now calculate the ColorBits
;	array.

SetColorBits:
	PUSH	DI
	PUSH	SI
	PUSH	BX
	MOV	SI,7		;offset to color array
	MOV	DI,3		;offset into AttrTile
SHFTBT:
	XOR	BH,BH		;clear byte for isolating one color
GETCOL:
	ROR	AttrTile[DI],1	;plane 3 bit
	RCL	BH,1		;rotate carry into color accumulator
	DEC	DI		;next plane
	TEST	b$Planes,2	;if 2-plane graphics --
	JZ	GETCL1		;we need to pass up next plane down --
	DEC	DI		;to find color bit for odd/even mode
	ROR	AttrTile[DI],1	;get bit for low plane
	RCL	BH,1		;rotate carry
	JMP	SHORT STTIL3
GETCL1:
	ROR	AttrTile[DI],1	;plane 2 bit
	RCL	BH,1		;rotate carry
	DEC	DI		;for 4-plane, repeat down through 0
	ROR	AttrTile[DI],1	;plane 1 bit
	RCL	BH,1
	DEC	DI
	ROR	AttrTile[DI],1	;plane 0 bit
	RCL	BH,1
STTIL3:
	MOV	ColorBits[SI],BH ;store color in array element
	MOV	DI,3		;go back to plane 3 pattern
	DEC	SI		;next lower offset in color array
	JNS	SHFTBT		;and shift out next color
	POP	BX
	POP	SI
	POP	DI
cEnd

cProc	PntBndCommon,<NEAR>
cBegin
	SHL	AX,1		;row * 2 (or row * 4)
	SHL	AX,1		;    * 4 (or row * 8)
	SHL	AX,1		;    * 8 (or row * 16)
	MOV	DX,AX		;save (row * 8) or (row * 16)
	SHL	AX,1		;row * 16 (or row * 32)
	SHL	AX,1		;row * 32 (or row * 64)
	ADD	AX,DX		;(row*32) + (row*8) = row*40 = byte address
				;(or (row*64)+(row*16)=row*80 = byte address)
				;left margin of screen at this row
	MOV	DX,AX		;two copies of this value
	ADD	AX,B$VLOFST	;byte address at (x=0) + offset to viewport
	MOV	B$LEOFST,AX	;left margin = byte address of left margin
	ADD	DX,B$VROFST	;byte address at (x=0) + offset to viewport
	MOV	B$REOFST,DX	;right margin = byte address of right margin
cEnd

;***
; B$EgaSetTile
;
;Purpose:
;	This routine stores the internal form of the current tile attribute.
;	This routine is called each time a row is to be painted.
;Entry:
;	BL = internal form of the tile attribute
;	BH = which plane the attribute is for
;Exit:
;	AttrTile[plane] = attribute
;	b$AttrC	= attribute
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaSetTile,<PUBLIC,NEAR>,<AX,DI>
cBegin
	MOV	AL,BH
	XOR	AH,AH
	MOV	DI,AX		;DI = 1 based plane number
	CMP	b$Planes,2	;2 plane system?
	JA	FOUR_PLANE
	SHL	DI,1		;plane now 2 or 4
	DEC	DI
	MOV	AttrTile[DI],BL
FOUR_PLANE:
	DEC	DI		;0 based
	MOV	AttrTile[DI],BL
	MOV	b$AttrC,BL	;set attribute to the tile attribute
cEnd

;***
; B$EgaScanInit
;
;Purpose:
;	This routine does some initialization for both ScanL and ScanR
;	for EGA modes.	Set up EGA Read/Write Mode register for color
;	compare read and EGA Color Compare register with b$PaintBorder.
;Entry:
;	None
;Exit:
;	ES:SI = Video segment address (b$AddrC)
;	CH = cursor mask	      (b$MaskC)
;	CL = flag for pixels changed  (0)
;	BX = count of pixels changed  (0)
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaScanInit,<PUBLIC,NEAR>,<DX>
cBegin
	les	si,b$AddrC	;di=cursor offset, es=video seg
	mov	ch,b$MaskC	;ch=cursor mask
	MOV	DX,GRPADD	;address of graphics index register
	MOV	AL,RWMREG
	OUT	DX,AL		;index the mode register
	MOV	AL,8H		;set read mode 1 for color compare
	INC	DX		;to data port
	OUT	DX,AL		
	DEC	DX		;back to index port
	MOV	AL,LOW CLCREG
	OUT	DX,AL		;index the color compare register
	MOV	AL,b$PaintBorder ;set up color compare with border attribute
	INC	DX		;to data port
	OUT	DX,AL
	XOR	CL,CL		;flag for pixels changed
	XOR	BX,BX		;count of pixels changed initialized to 0
cEnd

;***
; B$EgaPAINPX
;Purpose:
;	Paint first byte (and) last byte (and) whole bytes in between
;	Fast write is left to right or v.v. depending on whether SCANR, SCANL.
;	Call has reset or set direction flag.
;
;	This becomes a bit of a mess with EGAINT10 and QCG support.  Here's
;	the current approach:
;	    No EGAINT10 (QCG or not, doesn't matter) --
;		No problem, just execute straight code, modifying the EGA
;		hardware directly where needed.
;	    EGAINT10 and no QCG --
;		Disable ints at the beginning and reenable them at the end.
;		This will work because in the worst case the entire routine
;		takes approx. 1500 clock cycles, and we are using a ceiling
;		of 1800 clock cycles between CLI/STI.  TILING is an exception:
;		If tiling is used then the routine takes too long, but there
;		is an opportune point in the middle to reenable ints and then
;		disable them again without too much of a speed hit.
;	    EGAINT10 and QCG --
;		Now things get ugly.  We can't just disable ints over the
;		whole routine because it would exceed the 1800 clock max.
;		Instead, we use a combination of writing regs through the
;		EGAINT10 interface (like setting RWMReg, because it's needed
;		for the entire routine), and modifying regs directly within
;		small CLI/STI brackets (like setting BMKReg for a byte and
;		then restoring it before reenabling ints).
;Entry:
;	DI = first byte
;	SI = last byte,
;	BL = bit mask for first byte
;	BH = bit mask for last byte
;	BP = count of whole bytes to paint in between first and last
;	CH = 00 if SCANR (for INC DI before REP) or
;	   = FF if SCANL (for DEC DI before REP)
;Exit:
;	CL = non-zero to flag pixels changed
;Uses:
;Exceptions:
;******************************************************************************

;	Now to write the whole line from [DI], first pixel, to [SI],
;	final pixel first set up for write mode 2, if solid color PAINT,
;	or 0, if tile PAINT.

cProc	B$EgaPAINPX,<PUBLIC,NEAR>,<DX>
cBegin
	MOV	DX,GRPADD
	MOV	AH,b$EgaWrMd
	CMP	b$Tiling,0
	JZ	PAINT1
	AND	AH,10H		;if TILE write, use write mode 0
PAINT1:
	MOV	AL,LOW RWMREG
	EGAINT10CLI		;disable interrupts if using EGAINT10
	OUT	DX,AL		;index the mode register
	XCHG	AL,AH		;set write mode
	INC	DX		;to data port
	OUT	DX,AL		
	DEC	DX		;back to index port

;	Set up bit mask.

	MOV	AL,LOW BMKREG
	OUT	DX,AL		;index the bit mask register
	MOV	AL,BL		
	INC	DX		;to data port
	OUT	DX,AL		
	DEC	DX		;back to index port

;	Latch

	MOV	AL,ES:[DI]
	CMP	b$Tiling,0
	JZ	NOTIL1		;tiling not on, write solid color b$AttrC
	CALL	WRTTIL		;else call routine for writing partial tile
	JMP	SHORT CHKLST	;proceed to check for a last byte
NOTIL1:

;	Write first byte.

	MOV	BL,b$AttrC
	MOV	ES:[DI],BL
CHKLST:
	OR	BH,BH		;see if a last byte to write
	JNZ	MORWRT
	CALL	CLRMSK		;all done -- clear bit mask
	JMP	SHORT WHLBYT
MORWRT:

;	Set up bit mask for last byte.

	MOV	AL,LOW BMKREG
	OUT	DX,AL		;index the bit mask register
	MOV	AL,BH		;set mask
	INC	DX		;to data port
	OUT	DX,AL		
	DEC	DX		;back to index port

;	Latch

	MOV	AL,ES:[SI]
	CMP	b$Tiling,0
	JZ	NOTIL2
	XCHG	DI,SI		;WRTTIL addresses screen with DI
	CALL	WRTTIL		;if tiling is on, set [partial] last byte
	XCHG	DI,SI		;restore DI for whole-byte write
	JMP	SHORT CLRBT1
NOTIL2:
	MOV	ES:[SI],BL	;write last byte
CLRBT1:
	CALL	CLRMSK
WHLBYT:
	OR	BP,BP		;check if intermediate whole bytes
	JZ	CLRBT2

;	Set up to write all whole bytes.

	INC	DI		;if SCANR, go right
	OR	CH,CH
	JZ	BYTE2
	DEC	DI
	DEC	DI		;else start to left of first byte
BYTE2:
	MOV	BH,CH		;may need INC/DEC flag for 2-plane write
	MOV	CX,BP		;whole-byte counter in CX
	MOV	AL,BL		;attribute in AL for no-tile speed routine
	CMP	b$Tiling,0	;see whether tiling is on
	JZ	WRITE4
	CALL	TILSET		;set up for copy of off-screen tile

;	NOTE: This call has the effect of immunizing all bits from
;	      processor writes therefore, we must clear the bit mask
;	      following the write if tiling is on.

;	QCG tiling always does slow loop, so 2-plane hack isn't needed?

	TEST	b$Planes,4	;check for four-plane graphics mode
	JNZ	WRITE4		;if 2 planes, need byte-by-byte write routine
; QCG: would need to fix bkg mask here if using this code
	OR	BH,BH		;see whether SCANR or SCANL
	JNZ	CALLFT		;non-zero flag indicates move left
	CALL	WRTRGT		;zero indicates move right
	JMP	SHORT CLRBT2
CALLFT:
	CALL	WRTLFT
	JMP	SHORT CLRBT2

WRITE4:
	REP	STOSB
CLRBT2:
	EGAINT10STI		; reenable interrupts at common exit
	MOV	CL,1		;single pixels counted in B$EgaCHKBTR calls
				;non-zero as pixels changed flag
cEnd

;***
; WRTTIL
;Purpose:
;	Used for writing first and last bytes, possibly partial bytes,
;	to the screen when tiling is on for EGA modes.
;	At entry, the bit mask has been set up for the byte to write, and the
;	contents have been latched.
;	This routine writes the relevant bits to each plane one at a time,
;	using the map mask register to enable the planes from 3 to 0 and
;	writing from AttrTile from offset 3 down to 0.
;
;Entry:
;	ES:DI	 = screen address
;	AttrTile = tile value to write
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;*******************************************************************************
WRTTIL:
	PUSH	DX
	PUSH	BX
	MOV	DX,SEQADD	;index port for sequencer
	MOV	AL,LOW MMREG	;index to Map Mask Register
	OUT	DX,AL
	INC	DX
	MOV	AL,8H		;start with plane 3
	MOV	BX,3
WRTPLN:
	OUT	DX,AL		;set up for one plane
	MOV	AH,AttrTile[BX] ;get tile pattern for that plane
	MOV	ES:[DI],AH	;write to one plane
	MOV	AH,ES:[DI]	;latch that write
	DEC	BX
	SHR	AL,1		;next plane down the list
	JNB	WRTPLN		;repeat 3,2,1,0
	DEC	DX		;back to SEQADD
	MOV	AL,MMREG	
	OUT	DX,AL		;index the register
	XCHG	AL,AH		
	MOV	AL,0FH		;reeanable all planes
	INC	DX		;to data port
	OUT	DX,AL		
	POP	BX
	POP	DX
	RET

;***
; TILSET
;Purpose:
;	Prepares for write of whole tile bytes to the screen in the following
;	fashion :
;
;	i.  uses the routine WRTTIL to write the tile pattern, plane by
;	    plane, at the first offscreen address (6D60H); and, if we are
;	    in odd/even mode, writes it at the second offscreen address also
;	ii. sends 0 to Bit Mask Register to indicate all bits immune
;	iii.reenables write to all planes
;
;	This sets up the card so that it will always write not from processor
;	data but from the latches. If more than 64K, mode 10H, we can perform
;	one latch read at the off-screen address and then write on-screen bytes
;	in the tile pattern simply by changing the address back on-screen and
;	asking for writes to the screen.
;	If we are in odd/even mode, we must perform a latch for each byte
;	written, at the even address for even bytes, at the odd address for odd
;	bytes.	These reads occur within the routines WRTRGT and WRTLFT, which
;	then write to the screen from the latches, byte by byte.
;Entry:
;	ES:DI = screen address
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************

TILSET: 			;this routine altered for Modes D,E support
	TEST	b$Planes,4	;see if 4-plane tile
	JNZ	TILST4		;if so, just write first whole byte,latch,exit
	PUSH	DI		;for odd/even mode, save on-screen byte address
	MOV	DI,OFFSCN	;and substitute offscreen address (fancy
	CALL	WRTTIL		;monitor only)
	INC	DI		;if 2-plane, move to odd offscreen address
	CALL	WRTTIL		;and write to the 2 odd planes
	POP	DI		;restore on-screen address
	JMP	SHORT TILSEX	;no need to latch for 2-plane tile
TILST4:
	CALL	WRTTIL
	MOV	AL,ES:[DI]	;latch
TILSEX:
	MOV	AL,BMKREG	;send 0 to Bit Mask Register
	OUT	DX,AL		;index the bit mask register
	XOR	AL,AL		;disable all bits
	INC	DX		;to data port
	OUT	DX,AL		
	DEC	DX		;back to index port
	RET

;***
; B$EgaPIXCNT
;Purpose:
;	Whether or not we actually change any pixels, we have to send
;	the number "painted".
;
;Entry:
;	BP = number of whole bytes painted
;	DX = number of single pixels painted
;Exit:
;	BX = total number of pixels painted
;Uses:
;	BP, DX
;Exceptions:
;***************************************************************************
cProc	B$EgaPIXCNT,<PUBLIC,NEAR>
cBegin
	SHL	BP,1		;no. whole bytes * 2
	SHL	BP,1		;		 * 4
	SHL	BP,1		;		 * 8
SCNEX1:
	ADD	DX,BP		;pixel count
	MOV	BX,DX
cEnd

;***
; B$EgaSETCMP
;Purpose:
;	Set color compare register to current paint attribute.
;
;	This routine os called to set up the color compare register for
;	PIXLF2/PIXLF3/PIXRT2/PIXRT3.  Since these routines can take more
;	time than is allowed between CLI/STI, this routine must use an
;	EGAINT10 call to set the reg.
;Entry:
;	b$AttrC = attribute
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;*****************************************************************************
cProc	B$EgaSETCMP,<PUBLIC,NEAR>,<DX>
cBegin
	MOV	DX,GRPADD
	MOV	AL,LOW CLCREG
	OUT	DX,AL		;index the color compare register
	MOV	AL,b$AttrC	
	INC	DX		;to data port
	OUT	DX,AL
cEnd

;***
; CLRMSK
;Purpose:
;	Clear bit mask so full bytes will be written.
;Entry:
;	None
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;****************************************************************************

CLRMSK:
	MOV	AX,0FFH SHL 8 + BMKREG ;AH=FF,AL=8
	OUT	DX,AL		;index the bit mask register
	XCHG	AL,AH		
	INC	DX		;to data port
	OUT	DX,AL		
	DEC	DX		;back to index port
	RET

;***
; B$EgaCHKBTR
;Purpose:
;	Check byte right.
;	Look through byte setting up bit mask.
;	BX is used for bit masks, left and right bytes
;
;Entry:
;	AL = border bits set
;	AH = viewport edge mask if in this byte
;	CH = bit mask
;	DX = count of pixels painted so far
;Exit:
;	BH = final bit mask
;	CH = updated b$MaskC
;	DX = updated to reflect pixels painted
;Uses:
;	per conv.
;Exceptions:
;****************************************************************************
cProc	B$EgaCHKBTR,<PUBLIC,NEAR>
cBegin
	MOV	BH,CH		;initial bit mask

;if viewport edge coincides with a border pixel, don't count as painted
;	TEST	BH,AH		;is this viewport edge pixel?
;	JNZ	BYTEX1		;yes, set pixels (BH) and exit with
				; current b$MaskC
BYTLP:
	TEST	CH,AL		;check for border pixel
	JNZ	BYTEXT
	OR	BH,CH		;add pixel to set
	TEST	BH,AH		;have we just OR'd in viewport edge?
	JNZ	BYTEX1		;if so, exit with CH current and
				; BH=bit mask
	INC	DX		;count pixels painted
	ROR	CH,1		;move over one
	JNB	BYTLP		;repeat if not end of byte
	ROL	CH,1		;if off edge, rotate bit in at right
BYTEXT:

;	Final bit mask returned in BH.

	NOT	AL		;0 if border pixel(s) in AL
	AND	BH,AL		;cross out border pixel in bit mask
	RET

;	This exit only if viewport edge was encountered.

BYTEX1:
	INC	DX		;indicate one (more) pixel set
cEnd

;***
; B$EgaCHKBTL
;Purpose:
;	Check byte left.
;	Look through byte setting up bit mask.
;	BX is used for bit masks, left and right bytes
;
;Entry:
;	AL = border bits set
;	AH = viewport edge mask if in this byte
;	CH = bit mask
;	DX = count of pixels painted so far
;Exit:
;	BH = final bit mask
;	CH = value for updating b$MaskC
;	DX = updated to reflect pixels painted
;Uses:
;	per conv.
;Exceptions:
;**************************************************************************
cProc	B$EgaCHKBTL,<PUBLIC,NEAR>
cBegin
;	First check whether least significant bit is border (thinking
;	right to left).  If so, we have moved one byte too far to the
;	left for b$OffC, and want to set nothing in this byte.

	TEST	AL,CH
	JZ	BYTLF0
	INC	DI		;increment for b$OffC
	JMP	SHORT BYTLFX
BYTLF0:
	MOV	BH,CH		;initial bit mask
	TEST	BH,AH		;is this viewport edge pixel?
	JNZ	BYTLF1		;yes, set pixels (BH) and exit with
				; preexisting b$MaskC
BYTLFT:
	TEST	CH,AL		;check for border pixel
	JNZ	BYTLFX
	OR	BH,CH		;add pixel to set
	TEST	BH,AH		;test for viewport edge bit encountered
	JNZ	BYTLF1		;if found, exit with CH at that bit
	INC	DX		;count pixels painted
	ROL	CH,1		;move over one
	JNB	BYTLFT		;repeat if not end of byte
BYTLFX:
	ROR	CH,1		;else set up as rightmost pixel
	NOT	AL
	AND	BH,AL		; final bit mask returned in BH
	RET
BYTLF1: 			;this exit only if viewport edge bit encountered
	INC	DX		;count one (more) pixel painted
cEnd

;***
; PIXRT2
;Purpose:
;	Check for non-paint pixels right for 4 plane EGA modes.
;	Look through entire range of non-border pixels left to right
;	to determine whether any will actually change color.
;	AL is used for Color Don't Care mask
;
;Entry:
;	SI = byte address of leftmost byte in range
;	BL = bit mask for leftmost byte
;	BH = bit mask for rightmost byte
;	DI = total number of whole bytes -1
;	ES = video segment
;Exit:
;	CL = 0 if no pixels found to change, non-zero if pixels to change
;Uses:
;	per conv.
;Exceptions:
;****
cProc	PIXRT2,<NEAR>
cBegin
	XOR	CL,CL		;maybe no bits change

;	read first byte

	MOV	AL,ES:[SI]	;bit pattern of first byte with 0's where
				;color not paint attribute
	AND	AL,BL		;AND now produces difference if non-paint
				;bit in significant position
	XOR	AL,BL		;for significant bits in our first byte (bit
				;set in BH), then non-paint will be one
	JNZ	BITFDR		;found a bit to change
	OR	DI,DI		;any more bytes?
	JZ	PIXLAST 	;Brif not, test the last byte

;	Look at whole bytes within viewport range until non-paint color found.

LKPNTR:
	INC	SI
	MOV	AL,ES:[SI]
	NOT	AL		;check if all bits set (all paint color)
	OR	AL,AL		;NOT does not affect flags
	JNZ	BITFDR
	DEC	DI
	JNZ	LKPNTR		;keep looking until search of complete
				;bytes is exhausted
;	On last byte now, mask in BH.

PIXLAST:			
	INC	SI
	MOV	AL,ES:[SI]	;do last compare
	AND	AL,BH		;significant bit = 0 if not paint color
	XOR	AL,BH		;if different, not paint
	JZ	NOBITR
BITFDR:
	MOV	CL,AL		;set change bits flag to non-zero
NOBITR:
cEnd

;***
; B$EgaTILRGT
;Purpose:
;	Check for non-paint pixels right if tiling is on.
;	Use READC to do bit-wise color compares with tile colors stored in
;	color array ColorBits (colors of bits 0 to 7 configured left to right
;	are represented in the array as elements offset 0 to 7 from base
;	address).
;
;Entry:
;	BL    = bit mask for leftmost partial byte
;	BH    = bit mask for rightmost partial byte
;	DI    = count of whole bytes
;	ES:SI = screen address
;Exit:
;	CL = 0 iff no pixels to change
;Uses:
;	per conv.
;Exceptions:
;****
cProc	B$EgaTILRGT,<PUBLIC,NEAR>,<DX,BP>
cBegin
	PUSH	b$OffC
	PUSH	b$PenC
	PUSH	BX		;copy for BH check
	MOV	ByteCount,DI	;whole byte counter
	MOV	AL,BL		;copy bit mask from BL
	OR	AL,AL		;make sure at least 1 bit is set
	JZ	TILRT5
	CALL	LOWCHK		;check first partial byte (uses mask in AL)
	JNZ	TILRT1		;if no carry, non-match was found
TILRT5:
	CMP	ByteCount,0	;see whether whole bytes to check
	JZ	TILRT4
TILRT2:
	INC	SI
	MOV	b$OffC,SI
	MOV	b$MaskC,80H
	XOR	BP,BP		;offset to color array
TILRT3:
	CALL	[b$ReadC]
	CMP	AL,ColorBits[BP] ;check for non-match
	JNZ	TILRT1
	INC	BP
	SHR	b$MaskC,1	;move to next bit
	JNB	TILRT3
	DEC	ByteCount
	JNZ	TILRT2		;keep looking until all whole bytes done
TILRT4:
	POP	BX		;get bit mask in BH
	OR	BH,BH
	PUSH	BX
	JZ	TILRT6		;BH=0 indicates no "last byte"
	MOV	AL,BH		;get last partial byte
	INC	SI
	CALL	HICHK
	JNZ	TILRT1		;exit if non-match found
TILRT6:
	MOV	CL,-1
TILRT1:
	INC	CL		;non-zero if pixels changed
	POP	BX
	POP	b$PenC
	POP	b$OffC
cEnd

;***
; PIXLF2
;Purpose:
;	Check for non-paint pixels left for 4 plane EGA modes.
;	Look through entire range of non-border pixels right to left
;	to determine whether any will actually change color.
;	BH/BL = bit masks for leftmost and rightmost bytes, respectively
;	DI = total whole bytes between first and last
;
;Entry:
;	SI = byte address of rightmost byte in range
;	BL = bit mask for rightmost byte
;	BH = bit mask for leftmost byte
;	DI = total number of whole bytes
;	ES = video segment
;Exit:
;	CL = 0 if no pixels found to change, non-zero if pixels to change
;Uses:
;	per conv.
;Exceptions:
;****
cProc	PIXLF2,<NEAR>
cBegin
	XOR	CL,CL		;maybe no bits change

;	read first byte

	MOV	AL,ES:[SI]	;bit pattern of first byte with 0's where
				;color not paint attribute
	AND	AL,BL		;AND now produces difference if non-paint
				;bit in significant position
	XOR	AL,BL		;for significant bits in our first byte (bit
				;set in BH), then non-paint will be one
	JNZ	BITFDL		;found a bit to change
	OR	BH,BH		;see if only one byte
	JZ	NOBITL		;nothing to paint
	OR	DI,DI		;see if only a "last byte"
	JZ	LSTBYT

;	Look at whole bytes within viewport range until non-paint color found.

LKPNTL:
	DEC	SI
	MOV	AL,ES:[SI]
	NOT	AL		;check if all bits set (all paint color)
	OR	AL,AL		;NOT does not affect flags
	JNZ	BITFDL
	DEC	DI
	JNZ	LKPNTL		;keep looking until search of complete
				;bytes is exhausted

;	On last byte now, mask in BH.

LSTBYT:
	DEC	SI
	MOV	AL,ES:[SI]	;do last compare
	AND	AL,BH		;significant bit = 0 if not paint color
	XOR	AL,BH		;if different, not paint
	JZ	NOBITL
BITFDL:
	MOV	CL,AL		;set change bits flag to non-zero
NOBITL:
cEnd

;***
; B$EgaTILLFT
;Purpose:
;	Check for non-paint pixels left if tiling is on.
;	Use READC to do bit-wise color compares with tile colors stored in
;	color array ColorBits (colors of bits 0 to 7 configured left to right
;	are represented in the array as elements offset 0 to 7 from base
;	address).
;
;Entry:
;	BL    = bit mask for rightmost partial byte
;	BH    = bit mask for leftmost partial byte
;	DI    = count of whole bytes
;	ES:SI = screen address
;Exit:
;	CL = 0 iff no pixels to change
;Uses:
;	per conv.
;Exceptions:
;****
cProc	B$EgaTILLFT,<PUBLIC,NEAR>,<DX,BP>
cBegin
	PUSH	b$OffC
	PUSH	b$PenC
	PUSH	BX
	MOV	AL,BL		;copy of bit mask for rightmost byte
	MOV	ByteCount,DI	;store whole byte counter
	OR	AL,AL		;make sure at least 1 bit set
	JZ	TILLF5
	CALL	HICHK		;check first [partial] byte using mask in [AL]
	JNZ	TILLF1		;exit if non-match found
TILLF5:
	CMP	ByteCount,0	;check for intermediate whole bytes
	JZ	TILLF4		;if not, proceed to check for last byte
TILLF2:
	DEC	SI		;move left on line
	MOV	b$OffC,SI	; store in b$OffC for READC
	MOV	b$MaskC,1	;start at rightmost bit
	MOV	BP,7		;rightmost element of color array
TILLF3:
	CALL	[b$ReadC]	;determine color of pixel (returned in AL)
	CMP	AL,ColorBits[BP] ;look for non-match
	JNZ	TILLF1		;when we find one, we can exit
	DEC	BP
	SHL	b$MaskC,1	;move to next bit left
	JNB	TILLF3		;continue 'til we shift out of byte left
	DEC	ByteCount	;prepare to check next byte left
	JNZ	TILLF2
TILLF4:
	POP	BX
	OR	BH,BH		;see whether a "last byte"
	PUSH	BX
	JZ	TILLF6		;if not, there was only 1 byte
	MOV	AL,BH		;get last byte's bit mask
	DEC	SI
	CALL	LOWCHK		;look through last [partial] byte for match
	JNZ	TILLF1
TILLF6:
	MOV	CL,-1
TILLF1:
	INC	CL		;non-zero if pixels changed
	POP	BX
	POP	b$PenC
	POP	b$OffC
cEnd

;***
; HICHK
;Purpose:
;	Check partial byte for tile pattern match, where byte may be
;	partial in that some low bits may be immunized from PAINT and
;	therefore must be skipped during the tile pattern check.
;
;Entry:
;	AL = screen byte to check
;	ColorBits array contains tile pattern to check against
;Exit:
;	PSW.C set iff no match found
;Uses:
;	per conv.
;Exceptions:
;****
cProc	HICHK,<NEAR>
cBegin
	MOV	CH,10000000B	;rotate will start mask at 0000/0001
	MOV	BP,8		;start counter at 8
HICHK1:
	ROL	CH,1
	DEC	BP		;offset to first bit to check for color
	ROR	AL,1		;shift immune bits out right
	JNB	HICHK1		;if we got a 1, we found a relevant bit
	MOV	b$MaskC,CH
HICHK2:
	PUSH	AX		;store state of shift mask
	MOV	b$OffC,SI
	CALL	b$ReadC	;get color for this pixel
	CMP	ColorBits[BP],AL ;see if match
	POP	AX		;restore shift mask
	JNZ	HICHK3		;our goal is just one non-matching pixel
	DEC	BP
	ROR	AL,1		;look for additional 0's to left of first 1
	JNB	HICHK4
	SHL	b$MaskC,1
	JNB	HICHK2
HICHK4:
	XOR	BP,BP		;return ZF set if no match found
HICHK3:
cEnd

;***
; LOWCHK
;Purpose:
;	Check partial byte for tile pattern match, where byte may be
;	partial in that some high bits may be immunized from PAINT and
;	therefore must be skipped during the tile pattern check.
;
;Entry:
;	AL = screen byte to check
;	ColorBits array contains tile pattern to check against
;Exit:
;	PSW.C set iff no match found
;Uses:
;	per conv.
;Exceptions:
;****
cProc	LOWCHK,<NEAR>
cBegin
	MOV	CH,00000001B	;rotate will start mask at 1000/0000
	XOR	BP,BP
	NOT	BP		;counter at -1
LWCHK1:
	ROR	CH,1
	INC	BP		;offset to first bit to check for color
	ROL	AL,1		;shift immune bits out left
	JNB	LWCHK1		;if we got a 1, we found a relevant bit
	MOV	b$OffC,SI
	MOV	b$MaskC,CH
LWCHK2:
	PUSH	AX		;store state of shift mask
	CALL	b$ReadC	;get color for this pixel
	CMP	ColorBits[BP],AL ;see if match
	POP	AX		;restore state of shift mask
	JNZ	LWCHK3		;our goal is just one non-matching pixel
	ROL	AL,1		;look for any trailing 0's indicating immune
	JNB	LWCHK4
	INC	BP
	SHR	b$MaskC,1
	JNB	LWCHK2
LWCHK4:
	XOR	BP,BP		;set ZF
LWCHK3: 			;non-zero indicates non-match
cEnd

;***
; WRTLFT
;Purpose:
;	Used for tiling if in odd/even mode, during SCANL.
;	Latches off-screen tile pattern at even address when DI even, and
;	at odd address when DI odd, then "writes" this pattern at DI.
;
;Entry:
;	ES:DI = screen address of start byte
;	CX    = count of bytes to write
;Exit:
;	ES:DI = screen address of stop byte
;Uses:
;	SI
;Exceptions:
;****
cProc	WRTLFT,<NEAR>
cBegin
	MOV	SI,OFFSCN	;get address of off-screen location
	TEST	DI,1		;see whether first byte odd
	JNZ	INCBYL
	INC	SI		;even byte, so set up for DEC loop
DECBYL:
	DEC	SI		;SI gets even offscreen byte address
	MOV	AL,ES:[SI]	;latch even byte pattern
	MOV	ES:[DI],AL	;dummy write puts latched byte out
	DEC	DI		;move left on write line
	DEC	CX		;decrement byte counter
	JCXZ	LFTEX
INCBYL:
	INC	SI		;point to odd offscreen byte
	MOV	AL,ES:[SI]	;latch odd byte pattern
	MOV	ES:[DI],AL	;dummy write of latched pattern
	DEC	DI		;move left
	LOOP	DECBYL
LFTEX:
cEnd

;***
; WRTRGT
;Purpose:
;	Used for tiling if in odd/even mode, during SCANR.
;	Latches off-screen tile pattern at even address when DI even, and
;	at odd address when DI odd, then "writes" this pattern at DI.
;
;Entry:
;	ES:DI = screen address of start byte
;	CX    = count of bytes to write
;Exit:
;	ES:DI = screen address of stop byte
;Uses:
;	SI
;Exceptions:
;****
cProc	WRTRGT,<NEAR>
cBegin
	MOV	SI,OFFSCN	;get address of off-screen location
	TEST	DI,1		;see whether first byte odd
	JNZ	INCBYR
	INC	SI		;even byte, so set up for DEC loop
DECBYR:
	DEC	SI		;SI gets even offscreen byte address
	MOV	AL,ES:[SI]	;latch even byte pattern
	MOV	ES:[DI],AL	;dummy write puts latched byte out
	INC	DI		;move right on write line
	DEC	CX		;decrement byte counter
	JCXZ	RGTEX
INCBYR:
	INC	SI		;point to odd offscreen byte
	MOV	AL,ES:[SI]	;latch odd byte pattern
	MOV	ES:[DI],AL	;dummy write of latched pattern
	INC	DI		;move right
	LOOP	DECBYR
RGTEX:
cEnd

;***
; ScanL
;Purpose:
;	To scan left beginning one pixel to the left of the current
;	graphics cursor setting pixels to the paint attribute until
;	either edge of viewport or border is found; to return
;	certain information to the calling routine.
;
;	Algorithm for SCANL2 is as follows: starting next left to b$OffC
;	  b$MaskC:
;
;	  i.  While (not border) AND (not viewport edge) AND (not left
;	      edge of byte) move left in byte.
;	  ii. If (left edge of byte) then
;		 decrement b$OffC, set up b$MaskC, go to i.
;	      Else
;		 test if on border pixel
;		    x.	if true, exit unchanged
;		    xx. else
;			   xx1. set up bit mask for first byte
;			   xx2. calculate # whole bytes if any
;			   xx3. set up bit mask for last byte if any
;			   xx4. see whether any pixels in range will
;				change color
;				xx41. if not, exit to pixel count routine
;				xx42. if so, paint all pixels
;			   xx5. calculate number of pixels "painted"
;			   xx6. exit with appropriate information
;Entry:
;	b$AttrC       = attribute to paint
;	b$PaintBorder = border attribute which ends paint
;	b$OffC, b$MaskC specify pixel one to right of first to examine
;Exit:
;	BX = number of nonborder pixels tested
;	CL = 0 ifF no pixels changed color
;	b$OffC, b$MaskC specify location of last pixel painted, or
;	       unchanged if only border pixels found
;Uses:
;	per conv.
;Exceptions:
;****************************************************************************
cProc	B$EgaScanL,<PUBLIC,NEAR>,<ES>
cBegin
	CALL	B$EgaScanInit
	ROL	CH,1		;see if cursor is left edge of byte
	JNB	VWPCK1
	SUB	SI,1		;if so, start next byte left
	JNC	VWPCK1		;(SUB used since DEC doesn't set carry)
	JMP	BRDEX1		;if negative, hit corner of screen
VWPCK1: 			;to check first for viewport edge byte
	CMP	SI,B$LEOFST	;see if on edge of viewport, or off to left
	JNB	VWPCK2		;if viewport byte or to right,
	JMP	BRDEX1		; continue, else off to left; do nothing, exit
VWPCK2: 			;to check for edge bit if in edge byte
	JNZ	NOTEDG		;if not viewport edge byte, skip
	CMP	CH,B$VLMASK	;bit check else compare first pixel left with
				;viewport edge bit -- if farther left, we are
;	JA	BRDEX1		;are left of viewport edge and must
	JNA	NOTEDG		;exit
	JMP	BRDEX1
NOTEDG: 			;exit
	MOV	DI,SI		;extra copy of first byte address
	MOV	CL,CH		;extra copy of initial bit mask
	MOV	BP,-1		;this will be count of whole bytes
	XOR	DX,DX		;this will be #pixels painted
	XOR	AH,AH		;initialize this byte's viewport mask to 0

;	read first byte off the screen

	MOV	AL,ES:[DI]
	TEST	AL,CH		;see whether initial pixel is border
	JZ	SRCHLF		;if not, start search left
	XOR	CL,CL		;else set pixels-changed flag back to 0
	JMP	SHORT BRDEX1	;and exit gracefully
SRCHLF:

;	look for border or viewport in first byte

	CMP	DI,B$LEOFST	;is this in fact viewport edge byte?
	JNZ	NOTVWL
	MOV	AH,B$VLMASK	;if so, set up viewport mask in AH
NOTVWL:

;	while not border

	TEST	AL,CH
	JNZ	HAVPIX

;	and not viewport edge

	TEST	AH,CH
	JNZ	HAVPIX

;	and not off the edge of the byte

	ROL	CH,1
	JNB	NOTVWL

;	keep moving left - edge of first byte

	DEC	DI		;next byte address left
	INC	BP		;count of intermediate bytes
	MOV	AL,ES:[DI]	;read next byte left
	JMP	SHORT SRCHLF	;check next byte

HAVPIX:

;	Here when border or viewport edge found.
;	Set up bit mask for first (possibly only) byte.
;	SI = rightmost byte
;	DI = leftmost byte (possibly same byte)
;	CL = mask for rightmost bit in rightmost byte

;	If viewport edge was found, AH will contain the viewport bit
;	mask, and DI is the viewport edge byte. If SI=DI=viewport edge
;	byte, we need to retain the viewport mask in AH.  Otherwise
;	clear AH and fetch the mask again later if needed for DI.

	CMP	SI,B$LEOFST	;see if rightmost byte is LEFT viewport
	JZ	SINOTV		;if so, don't clear viewport mask
				; register
	XOR	AH,AH		;else clear AH for B$EgaCHKBTL on
SINOTV: 			;  rightmost byte
	MOV	CH,CL		;initial bit position in CH
	MOV	AL,ES:[SI]	;get border bits if any
	CALL	B$EgaCHKBTL	;set up bit mask for first byte
	MOV	BL,BH		;store in BL
	XOR	BH,BH		;there may be only one byte

;	see if more than 1 byte to paint

	PUSH	SI		;save a copy of rightmost address
	INC	BP		;see if still -1
	JZ	ONEBYT
	DEC	BP		;if not, recover real value
	MOV	CH,1		;set up mask for final byte
	MOV	AL,ES:[DI]	;get border bits if any
	CMP	DI,B$LEOFST	;was this viewport byte?
	JNZ	DINOTV		;no -- don't need viewport mask
	MOV	AH,B$VLMASK	;yes -- get viewport mask for CHKTBL
DINOTV:
	CALL	B$EgaCHKBTL	;set up leftmost byte bit mask in BH
ONEBYT:
	MOV	b$OffC,DI
	MOV	b$MaskC,CH	;update cursor
	PUSH	DI		;save a copy of leftmost address
	MOV	DI,BP		;store whole byte count for PIXLF2
	CMP	b$Tiling,0
	JZ	COLCM5
	CALL	B$EgaTILLFT
	JMP	SHORT COLCM6
COLCM5:
	CALL	B$EgaSETCMP	;set color compare register to paint attribute
	CALL	PIXLF2		;see whether any pixels in range will change
COLCM6:
	POP	SI		;restore leftmost address to SI
	POP	DI		;restore rightmost address to DI
	OR	CL,CL		;returns CL non-zero if changes needed
	JZ	BRDEXT

;	We found at least 1 pixel to change, so set entire range
;	set pixels-changed flag, set up write mode 2

	XOR	CH,CH
	NOT	CH		;set to FF as decrement flag
	STD			;for SCANL, decrement from DI
	CALL	B$EgaPAINPX
	CLD
BRDEXT:
	CALL	B$EgaPIXCNT	;returns # pixels "painted" in BX
BRDEX1:
	CALL	B$ResetEGA
cEnd

;***
; ScanR
;Purpose:
;	To scan right beginning with the graphics cursor at entry
;	setting certain pixels to the current graphics attribute
;	(color or tile pattern); to return certain information to
;	the calling routine .
;
;	ScanR algorithm is as follows :
;	 i.  Search right until
;	     a.  DX pixels have been tested without encountering non-
;		 border,
;		 OR
;	     b.  viewport edge is encountered without encountering non-
;		 border,
;		 OR
;	     c. a non-border pixel is found.
;	 ii. If a. or b., then exit with
;		[BX] = number of pixels painted = 0
;		[CL] = pixels modified flag = 0
;		[DX] = border pixels skipped during successful search for
;		       non-border pixel = 0
;		graphics cursor preserved
;		CSAVE (cursor values returned by previous SCANs) preserved
;
;	 iii.If c., then continue searching right painting non-border
;	     pixels until
;	     a. a border pixel is found
;		OR
;	     b. the edge of the viewport is encountered
;	     then exit with
;		[BX] = count of pixels painted (even if no color change
;		[CL] = pixels modified flag (0 if no pixels changed color)
;		[DX] = entry [DX] - count of border pixels searched before
;		       encountering non-border
;		b$OffC, b$MaskC = last pixel examined (border or viewport
;		      edge, painted if viewport edge
;		b$SaveCa,b$SaveCm = cursor values for first non-border pixel
;
;	This routine, which is specific to the EGA graphics modes, takes
;	advantage of the following special properties of the EGA card :
;	      a.  The ability to return 8-bit color compare to a specified
;		  color with respect to all 4 planes via a single MOV
;	      b.  The ability to write to all four planes via a single MOV
;
;	The algorithm is as follows : starting with b$OffC, b$MaskC
;
;	i.  While (border) and (not viewport edge) and (not DX=0) and
;	    (not right edge of byte) do
;		move right in byte
;		decrement DX
;	ii. If (right edge of byte) then
;		increment b$OffC, set up b$MaskC
;		go to i.
;	    Else
;		test if on border pixel
;		x.  if true, exit with appropriate information
;		xx. else
;		   xx1. save pertinent information including bit mask for
;		       first byte
;		   xx2. calculate number of bytes between b$OffC and
;		       viewport edge
;		   xx3. read all bytes until edge or border pixel
;		   xx4. analyze last byte
;		   xx5. set CL = 0
;			starting at CSAVE, while (pixel = paint
;			attribute) and (not end of range) read right
;			if (pixel = non-attribute) then
;			   CL = non-zero
;			   write first byte, whole bytes, last byte
;		   xx6. exit with appropriate information.
;
;Entry:
;	DX		   = count of border pixels which may be skipped
;	b$AddrC, b$MaskC = starting pixel
;	b$PaintBorder	   = attribute of paint region border
;	b$AttrC	   = attribute to paint
;	B$REOFST, B$VRMASK   = right viewport edge
;Exit:
;	BX		   = number of pixels painted
;				(whether or not they changed color)
;	CL		   = 0 iff no pixels changed color
;	DX		   = remaining border pixel count
;	b$OffC, b$MaskC  = the last non-border pixel examined/painted
;	SI, AL		   = the first non-border pixel encountered
;Uses:
;	per conv.
;Exceptions:
;*****************************************************************************
cProc	B$EgaScanR,<PUBLIC,NEAR>,<ES>
cBegin
;	set up EGA registers for color compare read
;	point ES:[SI] to screen memory, b$MaskC in CH
;	CL = 0 (pixels changed flag)

	CALL	B$EgaScanInit	;setup

;	perform color compare on first byte

	MOV	AL,ES:[SI]	;bits set where border found

;	starting at entry cursor, search right looking for non-border,
;	viewport edge, or end-of-byte as long as DX does not decrement to 0

	XOR	AH,AH		;initialize viewport mask to 0
SRCHRT:
	CMP	SI,B$REOFST	;check whether we are in viewport edge byte
	JNZ	NOTVPR
	MOV	AH,B$VRMASK	;if so, get viewport edge mask
NOTVPR:

;	While border...

	TEST	AL,CH		;compare color compare mask with b$MaskC
	JZ	ENDRT		;if pixel not border, exit loop

;	and not viewport edge...

	TEST	AH,CH		;compare viewport edge mask with b$MaskC
	JNZ	ENDRT		;if edge found, exit

;	and DX is greater than 0...

	DEC	DX		;contains # pixels which can be skipped
	JZ	ENDRT		;in search for non-border pixel

;	and not off the edge of the byte...

	ROR	CH,1		;shift bit mask right

;	repeat the search

	JNB	NOTVPR

;	end of first byte.

	INC	SI		;next byte address
	MOV	AL,ES:[SI]
	MOV	CH,80H		;mask now 1000/0000 for next search
	JMP	SHORT SRCHRT

;	either (not border) OR (viewport edge) OR (DX = 0)

ENDRT:
	TEST	AL,CH		;border?
	JZ	NOTBRD		;if so, we are either at viewport edge
	XOR	DX,DX		;or have skipped DX pixels and therefore
	JMP	SHORT SCNEX2	;should exit with info as initialized

;	look for viewport edge to determine how many bytes to
;	look through for border pixel

NOTBRD:
	PUSH	DX		;store skipcount for later
	XOR	DX,DX		;use to count pixels painted
	MOV	b$SaveCa,SI	;we have a new CSAVE
	PUSH	SI		;store copy of first byte address
	MOV	b$SaveCm,CH
	CALL	B$EgaCHKBTR	;set up byte for write, and count some pixels
				;(AH = viewport edge mask if any)
	MOV	BL,BH		;store first bit mask in BL
	XOR	BH,BH		;zero BH until last byte bit mask if any
	XOR	BP,BP		;start whole byte count at 0
	MOV	DI,B$REOFST
	SUB	DI,SI		;viewport edge address - first byte address
	TEST	BL,1		;if last bit not set, we found border for sure
	JZ	WRTPIX		;if just one byte, we're done
	OR	DI,DI		;check also if we hit viewport edge
	JZ	WRTPIX		;if so, also done

;	else look through DI bytes for border (this includes
;	viewport edge byte)

	DEC	BP		;start increment at -1
	MOV	CH,80H		;start each byte at left edge
SCANEM:
	INC	BP		;whole byte count
	INC	SI		;point to byte
	MOV	AL,ES:[SI]	;read each byte for color compare
	OR	AL,AL		;check for occurrence of border pixel(s)
	JNZ	BRDPIX		;set up last byte
	DEC	DI		;decrement to 0 to include last byte
	JNZ	SCANEM		;go check out this byte
;[alice]MOV	AH,B$VRMASK	;if edge of viewport, get viewport mask
				;and proceed to set up byte for write
BRDPIX:
	CMP	SI,B$REOFST	;if viewport edge not reached, skip
	JNZ	BRDRFD		; since it was border with no
				; viewport edge
				;this instruction reached if
				; viewport edge byte
				;this byte may contain border also
	MOV	AH,B$VRMASK	;if edge of viewport, get viewport mask
BRDRFD:
	CALL	B$EgaCHKBTR	;set up byte for write

;	most recent call to B$EgaCHKBTR has generated new cursor
;	location and mask

WRTPIX:
	MOV	b$OffC,SI
	MOV	b$MaskC,CH
	POP	DI		;restore leftmost byte address
	PUSH	DI		;save a copy for leftmost add. for painting
	PUSH	SI		;save copy of rightmost address also
	MOV	SI,DI		;leftmost byte address in SI for PIXRT2
	MOV	DI,BP		;PIXRGT will use DI to count whole bytes
	CMP	b$Tiling,0	;see if tiling is on
	JZ	COLCM1
	CALL	B$EgaTILRGT	;need routine for reading individual pixels
	JMP	SHORT COLCM2	;for position-sensitive compare
COLCM1:
	CALL	B$EgaSETCMP	;set color compare register to paint attribute
	CALL	PIXRT2		;routine to determine whether any pixels change
COLCM2:
	POP	SI		;restore rightmost
	POP	DI		;and leftmost byte addresses
	OR	CL,CL		;non-zero indicates at least one must change
	JZ	NOPNTR
	XOR	CH,CH		;zero as increment flag
	CLD			;for SCANR, paint routine should increment REP
	CALL	B$EgaPAINPX	;set line
NOPNTR:
	CALL	B$EgaPIXCNT	;return # pixels "painted" in BX
	POP	DX		;skipcount in DX
SCNEX2:
	CALL	B$ResetEGA	;reset EGA registers for BIOS write mode 0
	MOV	SI,b$SaveCa	;return CSAVE
	MOV	AL,b$SaveCm
cEnd

sEnd	GR_TEXT

	END
