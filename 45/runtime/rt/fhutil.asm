	TITLE	FHUTIL - Far Heap and Array Utility routines
	PAGE	56,132
;***
;FHUTIL - Far Heap and Array Utility routines
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
; Contains utility routines used in Far Heap and general array accessing.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	;Runtime Macro Defintions

	useSeg	_DATA
	useSeg	FH_TEXT

	INCLUDE seg.inc
	INCLUDE array.inc	;far heap and array descriptor structures
	INCLUDE idmac.inc

sBegin	FH_TEXT
assumes CS,FH_TEXT

	SUBTTL	B$ADArraySize - compute array size from descriptor
	PAGE
;***
;B$ADArraySize - compute array size from descriptor
;
;Purpose:
;	To compute the array size in bytes from its descriptor.
;	This value is the product of the element size and the
;	size of each dimension.
;
;	For FV_LONGPTR versions:
;	    return value is not defined if array is not dimensioned
;
;Entry:
;	ds:bx = offset of array descriptor.
;Exit:
;	CY = set if overflow, else cleared.
;	DX:AX = 32-bit size of the array in bytes (invalid if overflow).
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************

cProc	B$ADArraySize,<NEAR,PUBLIC>,<BX,CX,SI,DI>
cBegin

;	Get the number of dimensions from the descriptor.

	MOV	CL,[BX].AD_cDims ;move the dimension number
	XOR	CH,CH		;make a word value

DbAssertRel	CX,NE,0,FH_TEXT,<Array with zero dimensions in B$ADArraySize>

;	Initialize the running product in SI:DI to the element size.

	MOV	DI,[BX].AD_cbElement ;get the element size
	XOR	SI,SI		;make a doubleword value

;	For each dimension, multiply SI:DI by the element count.

ADArraySizeLoop:
	MOV	AX,DI		;get low-order word of running product
	MUL	[BX].AD_tDM.DM_cElements ;get first product of loop
	MOV	DI,AX		;move low-order word of first product
	MOV	AX,SI		;get high-order word of running product
	MOV	SI,DX		;running product now has first product
	MUL	[BX].AD_tDM.DM_cElements ;get second product of loop
	JC	ADArraySizeOflo ;if high-order word nonzero, then overflow
	ADD	SI,AX		;add low-order word to high-order running prod
	JC	ADArraySizeOflo ;if carry, then overflow
	ADD	BX,SIZE DM	;add size of array dim info - clears carry
	LOOP	ADArraySizeLoop ;loop until done

;	Move final result to DX:AX.

	MOV	AX,DI		;move low-order result and..
	MOV	DX,SI		;the high-order result

;	Exit with carry set if overflow in computation (> 32 bits),
;	else with carry clear.

NotDimmed:			
ADArraySizeOflo:

cEnd

sEnd
	END
