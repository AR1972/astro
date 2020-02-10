page	49,132
TITLE	exaryutl - Array Utilities

;***
;exaryutl.asm - interpreter specific array support.
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   This module includes support routines for array related executors:
;   - ResolveArray - a procedure that resolves indices and an array
;     descriptor to a memory address.
;
;
;
;****************************************************************************

	.xlist
	include 	version.inc
EXARYUTL_ASM	= ON
	IncludeOnce	architec
	IncludeOnce	array
	IncludeOnce	context
	IncludeOnce	exint
	IncludeOnce	qbimsgs
	IncludeOnce	rtinterp
	IncludeOnce	variable
	.list


assumes cs, CODE
assumes es, NOTHING
assumes ss, DATA

	    assumes ds,DATA

sBegin	CODE
	page
;***
;oVarToPAd - Find/Make array descriptor for a variable table entry
;
;Purpose:
;
;   This routine returns address of the array descriptor.
;   The space for the array descriptor will always be allocated.
;
;Input:
;
;   bx = oVar of array
;
;Output:
;
;   ax = SB of array descriptor if SizeD
;   ds:bx = address of array descriptor.
;   cDims and fFeatures (FADF_STATIC only) fields set from variable table
;
;Preserves:
;
;   cx,dx
;
;***************************************************************************
	public	oVarToPAd
oVarToPAd:
	push	cx
	DbChk	OVar,bx 		;Verify that this is a variable

	mov	cl,byte ptr [pVarBx-VAR_value].VAR_fStat; Get FV_STATIC flag
	mov	ch,[pVarBx].ASTAT_cDims

	    mov     ax,[pVarBx-VAR_value].VAR_Flags ;Get flags
	    TestX   ax,FVVALUESTORED	;Static?
	    jnz     StaticDescr
	    TestX   ax,FVFORMAL 	;Parameter?
	    jnz     ParamDescr
	    TestX   ax,FVCOMMON 	;Common?
	    jnz     CommonDescr

	;Array descriptor is local variable

	mov	bx,[pVarBx].AFRAME_oFrame; oBP of array descriptor
	add	bx,bp			;Point at array descriptor in stack
	jmp	short SetupAD


StaticDescr:

	lea	bx,[pVarBx].ASTAT_ad	; Get address of array descriptor

SetupAD:
	;ch = dim count

	or	ch,ch			; cDims unknown?
	jz	SetStatic		
	mov	[bx].AD_cDims,ch	;Set the count of dimensions in AD
SetStatic:				
	test	cl,FV_STATIC		;Is this a $STATIC array?
	jz	ArrayDescr		;Not $STATIC
	or	[bx].AD_fFeatures,FADF_STATIC	;Indicate $STATIC array
ArrayDescr:
	pop	cx
	ret

ParamDescr:
	;Array descriptor already set up - just point to it

	    mov     bx,[pVarBx].AFORMAL_oFrame	;oBP of pAd
	    add     bx,bp		;BX = Pointer to pAd
	mov	bx,[pFrame]		;ds:bx = pAD

;Added with [10]
	cmp	[bx].AD_cDims,ch	;cDims correct?
	jz	ArrayDescr
	or	ch,ch			;Unknown cDims?
	jz	ArrayDescr
	jmp	RangeErr		; cDims mismatch
;End of [10]


CommonDescr:
	mov	ax,[pVarBx].ACOM_oValue ;Offset into common block
	mov	bx,[pVarBx].ACOM_oCommon;oCommon
	test	cl,FV_STATIC		;Is the array $STATIC?
	jz	GetComDesc		;  brif not

	;$STATIC array descr. is in COMMON type table

	add	bx,COM_bdType - COM_bdValue	;Adjust to point to type table
GetComDesc:
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,ax				;Offset in block
	jmp	SetupAD



	subttl ResolveArray
	page
;***
;ResolveArray - Resolve array reference to address
;
;Purpose:
;
;   Resolve an array reference to an element address.
;
; NOTE:
;
;   This algorithm requires that:
;      1. No more than 255 indices are allowed.
;      2. The number of indices is correct.  This has been verified by SsScan.
;
;Input:
;
;   ds:bx = address of array descriptor
;      cx = index count
;   stack contains cx ET_I2 index arguments
;
;Output:
;
;   dx:bx = element address
;      ax = address of array descriptor
;      ds = base of module variable table if SizeD
;   index parameters cleaned from stack
;
;Preserves:
;
;   none
;
;Exceptions:
;
;   $ERR_BS - error for bad subscript
;
;*************************************************************************

	public	ResolveArray
ResolveArray:
	cmp	[bx].AD_fhd.FHD_hData,0	;this field is 0 when array not DIMed
	jz	UnDimmedErr		;Brif not allocated.  Error.
	dec	cx			;Special case single dimension array
	jnz	ResolveMulDims		;Go resolve multiple dimension case
	pop	cx			;Return address
	pop	ax			;Get index
	push	cx			;Return address back
	sub	ax,[bx].AD_tDM.DM_iLbound	;Subtract lower bound
	jl	RangeErr
	cmp	ax,[bx].AD_tDM.DM_cElements	;Test for range
	jge	RangeErr
	xor	dx,dx			;Index in dx:ax
	mov	cx,[bx].AD_cbElement	;Size of each element
;Need to multiply index in dx:ax by size of element in cx
	cmp	cx,2			;I2?
	jz	TwoByte
	cmp	cx,4			;I4,R4,SD?
	jnz	OddSize
;Handle 4-byte element
	shl	ax,1
TwoByte:
	shl	ax,1
	rcl	dx,1

HugeArray:
	;dx:ax has 32-bit offset
	;bx = pAD

	add	ax,[bx].AD_fhd.FHD_oData;Add base offset

	    adc     dx,0		;Ripple carry
	    jz	    GetArraySeg

	    ;Multiply dx by 64K/16 = 1000H to get relative segment

	    ror     dx,1
	    ror     dx,1
	    ror     dx,1
	    ror     dx,1

GetArraySeg:
	    add     dx,[bx].AD_fhd.FHD_hData;Load segment from AD

	xchg	bx,ax			;Offset to bx, pAD to ax
	ret

OddSize:
	mul	cx
	jmp	HugeArray

MulRangeErr:
	pop	cx
	pop	di
	pop	si			;Restore location of error

UnDimmedErr:
RangeErr:
	mov	al,ER_SOR		;Subscript out of range
	call	RtErrorCODE		;generate error, don't return


	page
;ResolveMulDims
;
;Purpose:
;
;   Handle the multiple dimension, R8 or user defined record type varients
;   of array access.  These varients can not be optimized in the same way as
;   single dimension I2/I4/SD array resolution.
;
;Input:
;
;   bx = address of AD
;   cx = cDims minus one

ResolveMulDims:
	push	si
	mov	si,sp
	add	si,4			;Point to first index
	push	di
	lea	di,[bx].AD_tDM		;Point to first dimension in descriptor
	push	cx
	xor	cx,cx			;Initialize byte offset value
	    mov     dx,cx
	jmp	short IndexLoopEntry	;Start element offset comp with add

	;Within this loop:
	;ax    = count of remaining indices
	;dx:cx = current offset value (dx is high word). EB uses CX only!
	;bx    = pAD
	;si points to next index
	;di points into array descriptor


IndexLoop:
	push	ax
	add	di,SIZE DM		;Move to next descriptor field

	;[di] * dx:cx multiplication (current offset by dimension in array descriptor)

	    mov     ax,dx
	    mul     [di].DM_cElements
	xchg	cx,ax			;High word has to be zero (DIM worked)
	mul	[di].DM_cElements
	    add     dx,cx		;Can't be carry (DIM worked)
	mov	cx,ax

IndexLoopEntry:
	lods	word ptr DGROUP:[si]	; get index argument
	sub	ax,[di].DM_iLbound	;Account for TO or OPTION BASE
	jl	MulRangeErr 		;Index error
	cmp	ax,[di].DM_cElements	;test for index out of bounds
	jge	MulRangeErr 		;error - index too high
	add	cx,ax			;add index to offset value
	    adc     dx,0		; with carry into high word
					; Can't be overflow - DIM worked
	pop	ax
	dec	ax			;Decrement argument count
	jns	IndexLoop		;Loop to process next argument

	;DX:AX = DX:CX * cbElement.  For EB: AX = CX * cbElement.

	    mov     ax,dx		;Prepare for mult
	    mul     [bx].AD_cbElement	;cbElement * high word

	xchg	cx,ax			;Save low word (high word is zero, since
					; there was no error during DIM)
	mul	[bx].AD_cbElement	;cbElement * low word
	    add     dx,cx		;Add in result of cbType * high word
	dec	si
	dec	si			;New top of stack
	pop	di
	pop	cx			;Old si
	pop	DGROUP:[si]		; Move return address to stack top
	mov	sp,si			;Clean off arguments
	mov	si,cx
	jmp	HugeArray	 	;Exit through single dim code
					; with:
					;   ax = data offset low word
					;   bx = AD address
					;   dx = data offset high word
	page

sEnd	CODE
end
