	TITLE	PALETTE - BASCOM palette support
;***
; PALETTE - BASCOM palette support
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - PALETTE Statement:
;
;      PALETTE
;	  |
;	B$PAL0
;
;
; - PALETTE Statement:
;
;      PALETTE color number, display color
;	  |
;	B$PAL2
;
;
; - PALETTE USING Statement:
;
;      PALETTE USING array(array index)
;	  |
;	B$PALU
;
;******************************************************************************
	INCLUDE rmacros.inc	; Runtime Macro Defintions
	INCLUDE switch.inc

	useSeg	_BSS
	useSeg	GR_TEXT 	

	INCLUDE seg.inc 	
	INCLUDE array.inc

sBegin	_BSS
externW b$PalPut
externW b$PalSet
externW b$PalReset
sEnd	_BSS

sBegin	GR_TEXT 		
assumes CS,GR_TEXT		

	SUBTTL	PALETTE, PALETTE USING support


	externNP B$ERR_FC	
	externNP B$SCINIT	; Performs screen initialization
	externNP B$ComputeSpace

;***
; B$PAL0 - PALETTE (no arguments)
;
; Purpose:
;	This routine supports the PALETTE statement with no arguments which
;	resets the palette to the modes initial values.
; Input:
;	NONE
; Output:
;	NONE
; Modifies:
;	NONE
; Exceptions:
;	Can jump to B$ERR_FC
;****
cProc	B$PAL0,<FAR,PUBLIC,FORCEFRAME> ; set up frame for error recovery
cBegin

	CALL	B$SCINIT	; initialize screen if not already done
	CALL	[b$PalReset]	;no args - initialize palette
	JB	PALERR		;branch if error occurred
cEnd

;***
; B$PAL2 - PALETTE #logical color,#actual color
;
; Purpose:
;	This routine supports the PALETTE statement with arguments to define
;	the Color mapping for a specific Attribute (palette index).
; Input:
;	Attribute = logical color
;	Color	  = actual  color
; Output:
;	NONE
; Modifies:
;	NONE
; Exceptions:
;	Can jump to B$ERR_FC
;****
cProc	B$PAL2,<FAR,PUBLIC>
parmW	Attribute
parmD	Color
cBegin

	CALL	B$SCINIT	; initialize screen if not already done
	MOV	BX,Attribute
	MOV	AX,OFF_Color	;lo order word of color
	MOV	DX,SEG_Color	;hi order word of color
	CALL	[b$PalPut]
	JB	PALERR

PALRET: 			
cEnd

PALERR: JMP	B$ERR_FC

;***
; B$PALU - PALETTE USING array name(array index)
;
;Purpose:
; This routine supports the PALETTE USING statement which defines the actual
; colors to use for Basic's logical colors, from an integer array starting at a
; particular index. The compiler insures the array is I2 or I4. The array
; descriptor is used by the low-levels to insure there are sufficient entries
; remaining to the end of the array for palette loading and to determine if the
; elements are I2 or I4.
;
; NOTE: This routine can take a far pointer to a movable item in a heap. This
; routine cannot directly or indirectly cause heap movement.
;
;Input:
; fpData = far ptr to start of array data
; pAd	 = pointer to array descriptor of interest
;
;Output:
; NONE
;
;Modifies:
; NONE
;
;Exceptions:
;	Can jump to B$ERR_FC
;****

cProc	B$PALU,<FAR,PUBLIC>,<ES,SI>
parmD	fpData			;far ptr to start of array data
parmW	pAd			;pointer to array descriptor of interest
cBegin
	CALL	B$SCINIT	; initialize screen if not already done
	cCall	B$ComputeSpace,<fpData,pAd>
				;[DX:AX] = space from address to end of array
				;[ES:SI] = starting address
	mov	bx,pAd
	mov	cx,[bx].AD_cbElement ;get the element size
	shr	dx,1		;divide size by 2 for count of I2 elements
	rcr	ax,1
	cmp	cx,4
	jne	PalI2		;skip next divide if I2 elements
	shr	dx,1		;divide size by 4 for count of I4 elements
	rcr	ax,1
PalI2:				;[DX:AX] = element count
	or	dx,dx		;hi order word of element count 0?
	jz	SmallCnt	;go if not, otherwise max out the count
	mov	ax,65535	;[AX]	 = element count	[7]
SmallCnt:			;[ES:SI] = starting address	[7]
				;[CX]	 = element size
	CALL	[b$PalSet]	;set the palette
	JB	PALERR
cEnd


sEnd	GR_TEXT

	END
