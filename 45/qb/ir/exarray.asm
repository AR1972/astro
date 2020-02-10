page	49,132
TITLE	exarray - Array statement executors
;***
;exarray.asm - interpreter specific array support.
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   This module includes:
;   - DIM, REDIM, and OPTION BASE executors.
;
;
;****************************************************************************

	.xlist
	include 	version.inc
EXARRAY_ASM	= ON
	IncludeOnce	architec
	IncludeOnce	context
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	extort
	IncludeOnce	opid
	IncludeOnce	opintrsc
	IncludeOnce	opstmt
	IncludeOnce	pcode
	IncludeOnce	scanner
	IncludeOnce	ui
	IncludeOnce	variable

	.list

assumes cs, CODE
assumes es, NOTHING
assumes ss, DATA

	extrn	ScanExExit:far
	extrn	B$ERAS:far

	    extrn   B$ADIM:far



sBegin	DATA
	extrn	SsScanExSrc:word

	public	DimAtScanType		
DimAtScanType	db	SSDIM_EXECUTE	; Type of Dim initiated by scanner

sEnd	DATA


sBegin	CODE

;***
;exStOptionBase<0|1>
;Purpose:
;	Handled at scan time so it doesn't need to be executed
;
;***************************************************************************
MakeExe exStOptionBase0,opStOptionBase0
	SkipExHeader
MakeExe exStOptionBase1,opStOptionBase1
	jmp	short Disp1

;***
;exDimOptionBase - executor to push current option base
;Purpose:
;	Emit the current OPTION BASE setting.
;	One use for this executor is as follows:
;	Array dimension clauses in DIM and REMDIM may or may not use the
;	TO keyword.  This executor is used in cases where TO is not used
;	so that DIM and REDIM executors always get both the lower and upper
;	bound for each dimension.
;Input:
;	none
;Output:
;	stack contains current OPTION BASE (ET_I2).
;Modifies:
;	none
;***************************************************************************
MakeExe exDimOptionBase,opDimOptionBase
	GETRS_SEG es,bx,<SIZE,LOAD>	
	mov	bx,[grs.GRS_oMrsCur]	
	RS_BASE add,bx			
	xor	ax,ax
	test	BPTRRS[bx.MRS_flags],FM_OptionBase1  
	je	OptBase0		;brif Current OPTION BASE setting is 0
	inc	ax			;else it is 1
OptBase0:
	push	ax			;Return result on stack
Disp1:
	jmp	DispMov 		; refresh es and dispatch

	subttl	Value Table Reference Ids
	page
;***
;exAVtRf - variable table reference executor
;
;Purpose:
;
;   This executor is used in the Dim, COMMON, SHARED, STATIC, Auto,
;   and Public statements.  The only executable statement in the
;   group is Dim and this is only executable with FV_QB4LANG.
;
;   There are two possible situations for these executors to be
;   executed.  The first is during scan time, these can be dispatched
;   to allocate the storage for the array.  Currently, this occurs for
;   $Static Common arrays only and then during the Dim statement processing.
;   The second situation is during normal execution with FV_QB4LANG.  In this
;   case, the array is checked if $Static or $Dynamic.	If $Static, no
;   action is taken if already allocated.  If $Dynamic, allocation occurs.
;
;Input:
;Output:
;Modifies:
;***************************************************************************

MakeExe exAVtRfSD,opAVtRf,ET_SD
	SkipExHeader
MakeExe exAVtRfR8,opAVtRf,ET_R8
	SkipExHeader
MakeExe exAVtRfI4,opAVtRf,ET_I4
	SkipExHeader
MakeExe exAVtRfR4,opAVtRf,ET_R4
	SkipExHeader
MakeExe exAVtRfI2,opAVtRf,ET_I2
	SkipExHeader
MakeExe exAVtRfImp,opAVtRf,ET_Imp
	inc	si			; Ignore argument count
	inc	si			
	LODSWTX
	xchg	ax,bx			;BX = oVar
DoDim:					
	xor	cx,cx			;Indicate Dim
	    jmp     short DimReDim	;Jump into shared code



	public	DimImplicit
DimImplicit:

	GETRS_SEG es
	    mov     bx,[grs.GRS_oMrsCur]
	    RS_BASE add,bx
	mov	al,BPTRRS[bx.MRS_flags] ;Low bound is option base
	.erre	FM_OptionBase1 EQ 1
	and	ax,FM_OptionBase1	;AX = option base (0 or 1)
	mov	dx,10			;Upper bound is 10

	call	GetEsDi 		;Setup to access pcode
	mov	cx,PTRTX[si]		;CX = cDims
	mov	bx,PTRTX[si+2]		;BX = oVar
@@:
	push	ax			;Push low bound
	push	dx			;Push upper bound
	loop	@B			;Brif more dimensions

	mov	[DimAtScanType],SSDIM_STATIC
	jmp	short DoDim



	page
;***
;exStReDimTo - REDIM executors.
;
;Purpose:
;
;   For DIM:
;   =======
;      Syntax: DIM <id>(x TO y,...) or DIM <id>(x,...)
;      Runtime Entry Point for DIM for arrays.
;      DIM Statement for dynamic arrays.  If the array is
;      already defined, an error is returned.
;
;   For REDIM:
;   =========
;      Syntax: REDIM <id>(x TO y,...) or REDIM <id>(x,...)
;      Runtime Entry Point for REDIM for arrays.
;      This algorithm depends on:
;      1. VarMgr setting up an array template, even for dynamic or common
;	  variables.
;      2. Scanner verifying correctness of index argument count
;
;Input:
;
;   Stack contains:
;	Variable Table Offset
;	Index count
;	count index arguments, consisting of lower and upper bounds
;
;Output:
;
;   none
;
;Modifies:
;
;*************************************************************************

MakeExe exStReDimTo,opStReDimTo
	pop	cx			; cx = pAD.  This is never 0!!!
	DbAssertRel cx,ne,0,CODE,<exStReDimTo: pAD == 0>
	mov	bx,PTRTX[si-4]		; Get Offset to Variable table
DimReDim:

	DbChk	oVar,bx 		;Verify that this is a variable
	mov	dx,[pVarBx-VAR_value].VAR_flags 
	mov	ax,dx
	and	ax,FV_TYP_MASK
	jz	RecArray		
	.erre	ET_MAX LT 100h		; Assure we can use AL
	cmp	al,ET_FS		;[9]
	jb	HavOTyp 		
	    .erre   ET_FS EQ ET_MaxStr	;[9]
	    if	    ET_MaxStr NE ET_MAX 
		ja	HavOTyp 	
	    endif			; ET_MaxStr NE ET_MAX
	    push    ax			; Save oTyp
	push	[pVarBx-VAR_Value].VAR_cbFixed	; Push length
	jmp	short HaveSize		

RecArray:
	mov	ax,[pVarBx-VAR_value].VAR_oTyp	; Get type while we have pVt

HavOTyp:
	    push    ax			; Save oTyp
	call	OTypCbTyp		;ax = bytes in oType passed in ax
	push	ax			;push cbElement

HaveSize:				

	    ;Look for $STATIC array in COMMON

	    cmp     [DimAtScanType],SSDIM_COMMON
	    jne     NotStaticCommon	; Brif not Dim'ing $Static common

	    pop     cx			; cbElement
	    pop     ax			; oTyp
	    push    cx			; Restore cbElement
	    mov     dl,[pVarBx].ACOM_cDims  
	    mov     dh,FADF_STATIC+FADF_NEAR
	    cmp     ax,ET_SD		;See if string
	    jne     @F
	    or	    dh,FADF_SD		;Tell runtime this is a string array
@@:
	    push    dx			;Push flags/cDims
	    push    [SsScanExSrc]	;Push pAd

	    ;Compute size of $Static array and set up array descriptor

	    call    B$ADIM		;Compute array size, don't allocate
	    mov     [SsScanExSrc],ax	;Save return value
	    jmp     short DimXds	;Return to scanner

NotStaticCommon:

	jcxz	@F			; Brif Dim
	mov	bx,cx			; DI:BX = sbAd:oAd
	mov	cx,1			; Needed below
	jmp	short GotPAd		

@@:
	call	oVarToPAd		;on exit bx = pAd
					; sets FADF_STATIC & cDims in array desc

GotPAd: 				
	;It's OK to execute a single $STATIC DIM more than once.  Multiple DIMs
	;are caught at scan time.  However, DIM of a $STATIC array passed as a
	;parameter is illegal.	In EB this test is not necessary because Dim
	;statements are not executable.  Therefore, a $Static array will
	;never be allocated more than once.

	    TestX   dx,FVFORMAL 	;Passed as parameter?
	    pop     dx			; cbElement
	    pop     ax			;AX = oTyp
	    push    dx			; Restore cbElement

	mov	dx,word ptr [bx].AD_cDims ;get flags byte & cDimensions (set up
					  ;  by oVarToPAd)

	    jnz     @F			;Brif parameter, always attempt Dim

	    ;Allow multiple DIM of $STATIC arrays

	    test    dh,FADF_STATIC	;$STATIC array?
	    jz	    @F			;If not, always DIM it

	    cmp     [bx].FHD_hData,0	;Space allocated to $STATIC array?
	    jnz     CleanUp		;If so, don't DIM again, no error
@@:

	;ax = oTyp
	;ds:bx = pAD
	;cx = 0 for DIM, 1 for REDIM
	;dh = Feature flags
	;dl = cDims
	;Stack has cbElement followed by bounds

	cmp	ax,ET_SD
	je	SetSD

	    or	    dh,FADF_FAR 	;Assume array is far not huge
	    test    dh,FADF_STATIC	;$STATIC array?
	    jnz     @F			;They can never be huge
	    test    [cmdSwitches],CMD_SW_HAR
	    jz	    @F			;Brif /AH switch not specified
	    or	    dh,FADF_HUGE	;Set Huge indicator for runtime
@@:

Flags_Set:
	push	dx			;flags/cDims
	push	bx			;pAD
SizeSet:
	jcxz	Dim_The_Array		;Brif Dim

	CALLRT	B$RDIM,Mov		;ReDim array via runtime code
	jmp	short After_Dim


SetSD:

	    or	    dh,FADF_SD OR FADF_NEAR ;Tell runtime this is a string array
	    jmp     short Flags_Set



CleanUp:
	    mov     dh,0		;dx=cDims
	    shl     dx,1		;Two words/dimension
	    inc     dx			; Plus one word for cbElement
	    shl     dx,1		;Two bytes/index
	    add     sp,dx		;Clear indices off stack
	    jmp     short DimX

Dim_The_Array:
	CALLRT	B$DDIM,Mov		;Dimension array via runtime code

After_Dim:

DimXds:
	;Determine how to return.

	mov	al,SSDIM_EXECUTE
	xchg	al,[DimAtScanType]	;Get Dim type and reset
	cmp	al,SSDIM_EXECUTE	;Is this execute scan time Dim?
	jne	DimAtScanExit		;Brif not

	;Exit for case that DIM executed as part of normal pcode execution

DimX:
	DispMac

	;Exit for a Dim that was executed at scan time for a $Static array

DimAtScanExit:
	jmp	ScanExExit		; Exit


	subttl	exStErase
	page
;***
;exStErase - erase one or more arrays
;
;Purpose:
;
;   Support for ERASE statement.
;
;Input:
;
;   es:si = pcode address of argument count
;   count pAD arguments on the stack
;
;Output:
;
;   none
;
;************************************************************************

MakeExe exStErase,opStErase
	LODSWTX 			;Load argument count
	mov	di,ax			;Arg count to di
EraseNext:
	call	B$ERAS			;erase this array descriptor

	;Note: this CAN cause heap movement

	dec	di
	jnz	EraseNext		;Go erase next array
	jmp	DispMov


	subttl	UnlinkArray
	page
;***
;UnlinkArray
;
;Purpose:
;
;   This routine unlinks Auto non-string arrays from the owners frame
;
;Input:
;
;   sbAd:pAd on stack
;
;Output:
;
;   none
;
;Preserves:
;
;   DI
;
;************************************************************************


;***
;exFn<U|L>bound<1|2>
;
;Purpose:
;
;   Support for LBOUND function
;
;Input:
;
;   pAD on the stack
;   iDim on stack   (exFnLbound2 only)
;
;Output:
;
;   none
;
;************************************************************************
;
MakeExe ExFnLbound1,opFnLbound1
	PushI	ax,1			
	SkipExHeader			
MakeExe ExFnLbound2,opFnLbound2
	CALLRT	B$LBND,DispAx


MakeExe ExFnUbound1,opFnUbound1
	PushI	ax,1			
	SkipExHeader			
MakeExe ExFnUbound2,opFnUbound2
	CALLRT	B$UBND,DispAx


;=============================================================================
	subttl	Utilities
	page
;***
;OTypCbTyp
;Purpose:
;	This routine returns the number of bytes of data required for
;	the input type.
;	Significantly rewritten for revision [7].
;
;Input:
;	ax = oTyp
;Output:
;	ax = cbTyp
;Modifies:
;	none
;Preserves:
;	all
;***************************************************************************

mpCbTyp equ	$-1

	.erre	ET_I2 EQ ($-mpCbTyp)
	DB	2			;ET_I2
	.erre	ET_I4 EQ ($-mpCbTyp)
	DB	4			;ET_I4

	.erre	ET_R4 EQ ($-mpCbTyp)
	DB	4			;ET_R4

	.erre	ET_R8 EQ ($-mpCbTyp)
	DB	8			;ET_R8


	.erre	ET_SD EQ ($-mpCbTyp)
	    db	    SIZE SD



OTypCbTyp:
	push	bx
	DbChk	oTyp,ax 		;sanity check on input oTyp
	cmp	ax,ET_MAX		;Is it a fundamental type?
	ja	NotPredefinedType	;  brif not - user defined

	    DbAssertRel ax,be,ET_SD,CODE,<OTypCbTyp: Invalid oTyp>

	mov	bx,offset cs:mpCbTyp	;base of lookup table in CS
	xlat	byte ptr cs:[bx]	;al == desired size

OTypCbTyp_Exit:
	pop	bx
	ret

NotPredefinedType:
	push	cx
	push	dx
	push	es

	cCall	CbTypFar,<ax>
	pop	es
	pop	dx
	pop	cx
	jmp	OTypCbTyp_Exit

sEnd	CODE
end
