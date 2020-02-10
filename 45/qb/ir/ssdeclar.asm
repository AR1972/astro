page	49,132
	TITLE	ssdeclare - scan support for declarative statement opcodes
;***
;ssdeclare - scan support for declarative statement opcodes
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   These routines scan DIM, COMMON, VtRf, AVtRf, and provide additional
;   external support for COMMON.
;
;   COMMON utilizes a value table, where the actual values are stored,
;   and a type table, which describes what's in the value table.  (In
;   the case of arrays, the array descriptor is in the value table.)
;
;   Each entry in the value table is rounded up to the next whole word
;   in size if it needs an odd number of bytes.  This can only happen
;   when fixed-length strings are involved, either as simples or in
;   records.
;
;   The basic entry in the type table is the oType of its corresponding
;   element in the value table.  Types and values are linked only by their
;   order in the tables.  For arrays, the oType is preceded by a word
;   with bit 14 set, and the count of dimensions in its low byte.  For
;   records, the oType is followed by the oMRS that defines the type.
;
;   Arrays are always assumed to have 8 dimensions for purposes of
;   allocating space in the value table for the array descriptor.  The
;   actual number of dimensions is kept for type checking, not space
;   allocation.
;
;   When chaining, information about user types is lost.  To still
;   provide some type checking, the type table entry is modified.
;   The oMRS field for the record type is changed to contain the
;   record length.  Bit 13 of the oType is set to indicate this was
;   done.  Type checking consists of verifying the records are of
;   of the same length.  The oType itself is no longer used.
;
;
;****************************************************************************

	.xlist
	include		version.inc
SSDECLARE_ASM = ON
	IncludeOnce	context
	IncludeOnce	executor	
	IncludeOnce	optables
	IncludeOnce	pcode		
	IncludeOnce	qbimsgs
	IncludeOnce	ssint
	IncludeOnce	txtmgr
	IncludeOnce	variable
	.list


extrn	B$ISdUpd:far
extrn	B$IAdUpd:far
extrn	B$STDL:far
extrn	B$IErase:far

assumes	es,nothing
assumes	ds,DATA
assumes	ss,DATA
assumes cs,SCAN


sBegin	DATA

	globalW	pSsCOMcur,0		;normally zero; when we're growing
					;  COMMON bdTyp and bdValue tables, this
					;  contains a pointer to where those
					;  tables can be found on the stack.
					;  This is necessary for bkptr updating
					;  in case the value table moves.
sEnd	DATA

sBegin	SCAN

;These flags are used in the high bits of oTyp in a COMMON block type table
cDimsFlag=	40H	;This word has cDims in low byte, not oTyp
LengthFlag=	20H	;Next word is length of record
StaticFlag=     1       ;$STATIC array in COMMON

;***
;Ss_StCommon - Scan the COMMON statement
;
;Purpose:
;
;   Creates a stack frame with COMMON information.  This frame is removed
;   by SsBos.
;
;   The frame includes the owners of the COMMON block's type and value
;   tables, as well as indexes into those tables.  The change of owners
;   is needed so they don't move as the the tables grow.
;
;***********************************************************************

SsProc	StCommon,rude
	mov	[SsOTxPatchBos],di	; Patch this with next Bos address
	STOSWTX 			;Emit executor
	test	[SsExecFlag],OPA_fExecute ;Already seen executable stmt?
	jz	@F
	mov	ax,MSG_COM		;COMMON must precede executable stmts
	call	SsError
@@:
	MOVSWTX 			;Skip over oTx operand
	LODSWTX				;Get oNam
	STOSWTX 			;Emit it
	push	ax
	call	MakeCommon
	inc	ax			;Out of memory?
	jz	OME
	dec	ax
	or	[SsBosFlags],SSBOSF_StCommon	;Set flag for VtRf

	;Make stack frame with COMMON info

	push	bp
	push	ax			; Place holder for COM_cbFixed
	push	ax			;Save oCommon
	    add     ax,[grs.GRS_bdtComBlk.BD_pb]    ;oCommon --> pCommon
	add	ax,SsCom+SsComSize	;Get to end of structure
	xchg	bx,ax			;pCommon.bdType to bx
	mov	cx,SsComSize/2		;Word size of structure
@@:
	dec	bx			
	dec	bx
	push	[bx]			;Copy word to stack
	loop	@B			;Repeat
	mov	bp,sp			;bp is low byte of COM structure
	    mov     [pSsCOMcur],sp	;see module header for explanation
;Assign owners
.errnz	SsCom - COM_bdType
	push	bx			;Current owner
	push	bp			;New owner
;If COMMON in user library, Value field is not an owner
	    add     bx,COM_bdValue - COM_bdType
	    cmp     [bx].BD_cbPhysical,UNDEFINED    ;User Library?
	    jz	    CopyTypOwner		    ;yes, skip value field
	push	bx			;Current owner
	lea	bx,[bp-COM_bdType].COM_bdValue
	push	bx			;New owner
	call	BdChgOwner		;Copy BD to stack
CopyTypOwner:
	call	BdChgOwner
	jmp	CommonX

OME:
	mov	ax,ER_OM
	call	SsError
	or	[SsBosFlags],SSBOSF_StStatic	;Set flag for no work in VtRf
	jmp	CommonX


subttl	Ss_AVtRf and Ss_VtRf
page
;***
;Ss_AVtRf - Scan AVtRf variants array Id opcodes
;
;Purpose:
;
;   Scan the id variants opAVtRf<type>.
;
;   The statements STATIC, SHARED, COMMON, and DIM all use AVtRf opcodes.
;
;   Arrays are $STATIC or $DYNAMIC based on the first reference to
;   the array.	Variables in determining the array type are:
;   - Statement.  The first reference may be in any of the following
;     statements: STATIC, SHARED, COMMON, DIM, REDIM, ERASE, PUT, GET
;     or <implicit ref> (indicating any other legal location for an array
;     reference).
;   - $STATIC and $DYNAMIC metacommand.  The default is $STATIC.  This
;     default may be changed by using the $STATIC and $DYNAMIC metacommands.
;
;   The table below shows what kind of array ($STATIC/$DYNAMIC) is created
;   or what error is reported by the BASCOM 2.0 compiler.  The <implicit>
;   case has been added for completeness - it does not use an AVtRf opcode.
;
;   Statement of First Ref  $STATIC		    $DYNAMIC
;   -----------------------------------------------------------
;   STATIC/COMMON/REDIM     $DYNAMIC		    $DYNAMIC
;   DIM (constant indices)  $STATIC		    $DYNAMIC
;   DIM (expression  index) $DYNAMIC		    $DYNAMIC
;   <implicit>		    $STATIC		    $STATIC
;   ERASE/PUT/GET/SHARED    Syntax error	    Syntax error
;
;   In the case of statements where the opcode follows the opAVtRf
;   the AVtRf scanner pushes the oVT and a flag indicating the
;   existence of an expression as an index.  The statement scanners
;   use this information to determine whether the array is $STATIC or
;   $DYNAMIC.  The declarative statement scanners are given the
;   number of AVtRf arguments by methods described in the scan routines
;   for those statements.
;
;   In the case of statements where the statement opcode preceeds the
;   opAVtRf the opAVtRf scanner sees that a flag is set, indicating
;   which executor was seen.  This flag is cleared at BOS.  The AVtRf
;   scanner completes the scan task for the statement indicated by
;   this flag.
;
;   Functions are referenced using the same opcodes as variables.
;   The VtRf variants may reference a function.  However, if they do
;   it is an error.
;
;   Tasks:
;	1. bind to executor.
;	2. complete the scan task for STATIC/COMMON/SHARED.
;	3. calculate whether any index contains an expression (as opposed
;	   to a literal).
;	4. make a scan stack entry for arrays for the case that the statement
;	   executor follows the opAVtRf.
;	5. Coerce all index expressions to integer.  This ensures that the
;	   executor for this statement can clean the stack.
;
;Algorithm:
;
;   Load and emit executor
;   Copy operand(s)
;   Ensure that the variable is not a function.
;   Coerce arguments, calculating whether any argument is not a literal.
;   If COMMON, SHARED, STATIC
;      Perform scan work for these statements.
;   ELSE (must be ERASE, PUT, GET, DIM, REDIM)
;      Push stack entry
;	  oVar
;	  flag TRUE if an index was an expression.
;	  index count
;      Scan routines for these opcodes must verify that the number of
;	  dimensions matches the number of indices.  opStDimTo must have
;	  twice the indices as opStGet, and ERASE takes no indices.
;   Return to scnner main loop
;
;Input:
;
;   ax	  = opcode
;   bx	  = 2 * opcode
;   es:di = pcode emission address
;   es:si = pcode source address
;
;Output:
;
;   si updated
;
;Modifies:
;Exceptions:
;	Ss_Error
;
;******************************************************************

.errnz	FALSE		;This algorithm depends on F_Static and F_StaticCalc
	page

AVtRfToFun:
	jmp	VtRfToFun

AVtRfRedirect:
	mov	ax,[bx].VAR_value	;Get new oVar
	mov	PTRTX[di-2],ax		;Patch into pcode
	jmp	short AVtRfRetry

StaticArray:
	;If NOT first ref, it's an error

	TestX	dx,FV_STATICSET 	;First reference?
	mov	ax,ER_DD		;Duplicate definition if not
	jnz	AVtRfError
	call	SetArrayTypeNoDim	;Set fStatic for this array
AVtRfX:
	jmp	[ScanRet]

SharedArray:
	;Make sure it's referenced at the module level

	TestX	dx,FV_STATICSET 	;First reference?
	jnz	AVtRfX			;Better not be
	mov	ax,ER_UA		;Array not defined
AVtRfError:
	call	SsError
	jmp	short AVtRfX

FRAME=	FVCOMMON+FVSTATIC+FVSHARED+FVFORMAL+FVVALUESTORED+FVREDIRECT

ComDimCnt	=	8		;No. of dims allowed in COMMON
ComArraySize	=	(size AD - 1) + ComDimCnt * (size DM)

CommonArrayJ:				
	jmp	CommonArray		


SsProc	AVtRf,Rude
	xchg	ax,bx			; BX = executor map address
	mov	al,byte ptr es:[si-1]	; High byte of opcode
	.erre	OPCODE_MASK EQ 03ffh	
	and	ax,HIGH (NOT OPCODE_MASK)
	shr	ax,1			; Convert to word offset
	add	bx,ax			; Index into map
	mov	ax,cs:[bx]		; Load executor
	STOSWTX 			;Emit the executor
	LODSWTX 			;Load argument count
	STOSWTX 			;And emit the arg count
	xchg	cx,ax			;Preserve for processing indices
	LODSWTX 			;Load oVar
	STOSWTX 			;Emit oVar
AVtRfRetry:
	    add     ax,[MrsCur.MRS_bdVar.BD_pb] ;oVar --> pVar
	xchg	ax,bx
	DbChk	pVar,bx 		;Verify that this is a variable
	mov	ax,[bx].VAR_Flags	;[5]

	;Check for AVtRf to a function error.

	TestX	ax,FVFUN		;Is this a ref to a function?
	jnz	AVtRfToFun		;Error - AVtRf to a function.

	    ;Check for AVtRf to redirected variable.

	    TestX   ax,FVREDIRECT	;Is the variable redirected?
	    jnz     AVtRfRedirect	;Redirected variable.

	DbAssertTst ax,nz,FVARRAY,SCAN,<Ss_AVtRf: Non-array>

	;Allocate oFrame.

	    TestX   ax,FRAME		;Is it a frame var?
	    jnz     @F

	call	SsAllocOFrame		;Allocate an oFrame for this var
@@:

	    xchg    dx,ax		;Keep var flags in dx
	    mov     [f_StaticCalc],FALSE;If first ref, assume dynamic array
	    mov     al,[SsBosFlags]
	    test    al,SSBOSF_StCommon	;Is it a COMMON statement?
	    jnz     CommonArrayJ
	    test    al,SSBOSF_StStatic
	    jnz     StaticArray
	    test    al,SSBOSF_StShared
	    jnz     SharedArray

	;DIM case handling - the statement opcode hasn't been seen.

	;Initialize Index Seen flag for $STATIC array calculation.
	;Flag is initialized to current default array type.
	;Needed only for DIM

	    mov     al,[f_Static]	;TRUE if $STATIC in effect
	    mov     [f_StaticCalc],al	;Move to temporary for calc

	mov	ax,ET_I2		;Target type
	call	SsCoerceN		;Coerce cx indices to type ax
					;f_StaticCalc set FALSE if any nonlits

	cmp	[f_StaticCalc],FALSE	; Were any expressions found?
	jne	@F			; Brif no expression found

	    or	    [SsExecFlag],OPA_fExecute	; This is executable

@@:					
	mov	dx,[bx].VAR_Flags	

	;Test for second DIM of array error.
	;In QB multiple Dims of $Dynamic arrays are allowed.
	;In EB multiple Dims are prevented by the variable manager.


	    TestX   dx,FV_STATICSET	;Test if array type has been set
	    jnz     @F			; Brif second Dim.
	    TestX   dx,FVCOMMON 	; Is this common array
	    jz	    NotSecondDimErr	; Brif not common.	Set type

	    ;This is first reference to a Common array.  The array must be
	    ;$Static since the Common statement would have set FV_STATICSET.

	    mov     ax,[SsOTxStart]	;Load oTx for this Dim clause.
	    mov     [bx].VAR_value.ACOM_oValue,ax   ;Save oTx of Dim statement
@@:
	    test    byte ptr [bx].VAR_fStat,FV_STATIC ;Is the array $STATIC?
	    jnz     SecondDimError	;Brif second dim of $Static array
NotSecondDimErr:

	call	SetArrayType		;Set BX=pVtArray to type in f_StaticCalc
	mov	cx,PTRTX[di-4]		;AX = cArgs
	shr	cx,1			;Two indices per dimension in DIM TO
					; Parser ensures pairs of indices.
	cmp	cl,[bx].VAR_value.ASTAT_cDims	;Is index count = dims
	jne	WrongCDimError		;Brif cDims is incorrect
@@:

	mov	ax,[SsOTxStart] 	;Load oTx for this Dim clause.
	mov	[SsScanExStart],ax	;Save in case needed below


AllocateArray:
	test	byte ptr [bx].VAR_fStat,FV_STATIC ;Is the array $Static?
	jz	DimExit 		; Brif $Dynamic array

	    TestX   dx,FVCOMMON 	; Is this common array
	    jnz     DimExit		; Brif common. Don't allocate now.

	cmp	[bx].ASTAT_ad.FHD_hData,0   ;Allocated already?
	jne	DimExit 		;Brif yes.  Bypass allocation.


	cmp	[SsErr],0		;Any errors?
	jne	DimExit2		;Brif yes.  Bypass allocation.

	mov	[DimAtScanType],SSDIM_STATIC
	push	ax			;Dummy parameter
	push	ax			;Dummy parameter
	call	ExecuteFromScan 	;Allocate array.  AX = RT error code.
	jnz	DimError		;Brif error code <> 0

DimExit:				

DimExit2:
	mov	[SsOTxStart],di 	;Update pointer for next Dim clause
	jmp	[ScanRet]		;Scan next opcode



SecondDimError: 			
	mov	ax,MSG_OBA		;Array already dimensioned
	jmp	short DimError		



WrongCDimError: 			
	mov	ax,MSG_SubCnt		;Wrong number of subscripts
DimError:
	call	SsError
	jmp	short DimExit		



NewArTypeJ:
	jmp	NewArType

StaticCommonJ:				
	jmp	StaticCommon		

CommonArray:
	call	SetArrayTypeNoDim	;Set fStatic for this array
					; to type in f_StaticCalc
					;Input:
					; bx = pVtArray

	;Set oCommon and oValue in variable table

        cmp     [SsErr],0                       ;Any errors so far?
        jnz     CommonX                         ;Don't risk it if so
	mov	ax,[bx].VAR_cbFixed		; Get length of FS
	mov	[bp-SsCom].COM_cbFixed,ax	; Save
	mov	ax,[bp-SsCom].COM_oCom		;Get oCommon
	mov	[bx].VAR_value.ACOM_oCommon,ax
	mov	cl,[bx].VAR_value.ACOM_cDims	;Get cDims
	GetOtyp dx,[bx] 			;Get oTyp of element
	test	byte ptr [bx].VAR_fStat,FV_STATIC ;$STATIC array in COMMON?
	jnz	StaticCommonJ
	mov	ch,cDimsFlag

	mov	ax,[bp-SsCom].COM_oValCur	;Get oValue
	mov	[bx].VAR_value.ACOM_oValue,ax

	;See if this stuff fits

	add	ax,ComArraySize			;Size of AD in COMMON
	call	ChkComSize			;bx = oTypCur
	jc	CommonX 			;Quit if no room

	;See if there's a type in table

	cmp	bx,[bp-SsCom].COM_bdType.BD_cbLogical	;Have entry in table?
	jae	NewArTypeJ

	;Compare with existing type

	add	bx,[bp-SsCom].COM_bdType.BD_pb	;Point into type table

	;First check no. of dimensions

	cmp	ch,[bx+1]			;Make sure both are arrays
	jnz	TypTabErrNz
	or	cl,cl				;cDims not set in Var Table?
	jz	CompArElem			;Ignore count if not known
	cmp	cl,[bx]				;cDims match with type table?
	jz	CompArElem
	cmp	byte ptr [bx],0			;cDims not set in type table?
	mov	ax,MSG_SubCnt
	jnz	ComErr				;Index count error
	mov	[bx],cl				;Set cDims in type table
CompArElem:
	;Compare element type
	inc	bx
	inc	bx				;Point to element type
CompType:
	cmp	dx,ET_MAX			;Record type?
	ja	CompRec 			; Must compare across modules
	cmp	dx,[bx]				;ET types match?
TypTabErrNz:
	jne	TypTabErr
	.erre	ET_MAX LE 100h			; ET_FS in single byte
	cmp	dl,ET_FS			
	jb	SkipOTyp			; brif not fixed string
	inc	bx				
	inc	bx				; Point to length
	mov	ax,[bp-SsCom].COM_cbFixed	; Get length to Var table
	cmp	ax,word ptr [bx]		; Compare to common length
	jne	TypTabErr			

SkipOTyp:
	inc	bx
	inc	bx
	sub	bx,[bp-SsCom].COM_bdType.BD_pb	;pTypCur --> oTypCur
	mov	[bp-SsCom].COM_oTypCur,bx	;Update position in type table
CommonX:
	GetSegTxtCur				
	jmp	[ScanRet]

CompRec:
	mov	cx,[bx]				;Get oType
	cmp	cx,ET_MAX			;Is it a record?
	jbe	TypTabErr			; brif not record
	cmp	ch,LengthFlag			;Reduced to just a length?
	jz	CompLength

	mov	ax,[bx+2]			;Get oRS of this oTyp
	push	bx
	mov	bx,[grs.GRS_oRsCur]
	cCall	CompareTyps,<ax,bx,cx,dx>	
	REFRESH_ES				
	pop	bx
	or	ax,ax				
CompRecResults:
	jnz	TypTabErr
	inc	bx
	inc	bx
	jmp	SkipOTyp

CompLength:
	xchg	ax,dx				;oTyp to ax
	call	CbTypOTypSCAN			; Get its length
	cmp	ax,[bx+2]			;Match type table?
	jmp	CompRecResults

TypTabErr:
	mov	ax,ER_TM
ComErr:
	call	SsError
	jmp	CommonX
	
StaticCommon:
	;See if there's a type in table
	;cl = cDims
	;dx = oTyp
	;ds:bx = pVar

        mov     ch,cDimsFlag+StaticFlag
	push	[bx].VAR_value.ACOM_oValue	; Push oTxDim
	mov	ax,[bp-SsCom].COM_oTypCur	;Current type table offset
	inc	ax
	inc	ax				;Skip cDims
	mov	[bx].VAR_value.ACOM_oValue,ax	;Value is offset to AD
	dec	ax
	dec	ax
	xchg	bx,ax				;oTypCur to bx
	cmp	bx,[bp-SsCom].COM_bdType.BD_cbLogical	;Have entry in table?
	jae	NewStatic

	;Compare with existing type

	add	bx,[bp-SsCom].COM_bdType.BD_pb	;Point into type table

	;First check no. of dimensions

	cmp	cx,[bx]				;Make sure both are arrays
	xchg	ax,cx				;cDims to al
	pop	cx				;Get oTxDim
	jnz	TypTabErr
	cbw					;Zero ah
.errnz  size DM - 4
        shl     ax,1
        shl     ax,1
        add     ax,size AD-1		;ax = size of AD
        sub     sp,ax
	mov	bx,sp			;bx = pAD
	call	ExecDim

	;AX = Size of array in bytes

	push	si
	push	di
	mov	si,sp
	add	si,(size AD-1)+4	;Point to start of DM fields
	mov	di,bx			;pTypCur
	mov	cl,[di]			;Get cDims again
	add	di,(size AD-1)+2	;Skip cDims and AD header
	xor	ch,ch
	shl	cx,1			;2 words/dimension
	mov	ax,cx
	push	ds
	pop	es
	rep	cmpsw			;Compare dimensions
	mov	bx,di			;Pointer to element type
	pop	di
	pop	si
	call	TMErrorNz
	shl	ax,1			;cb of dimensions
	add	ax,size AD-1
	add	sp,ax			;Remove AD from stack
	jmp	CompType

NewStatic:
	;cl = cDims, ch = $STATIC array flags
	;dx = oTyp
	;bx = oTypCur
	;[sp] = oTxDim

	mov	ax,cx
	cbw				;Zero ah
        shl     ax,1
        shl     ax,1
	add	ax,(size AD-1)+2	;cDims, size, and AD header
	push	bx			;oTypCur
	add	bx,ax			;Make room for dimensions
	call	ExtendType
	pop	ax
	jc	ShrinkType		;Didn't fit
	xchg	bx,ax			;oTypCur to bx, ax points after AD
	add	bx,[bp-SsCom].COM_bdType.BD_pb	;Point into type table
	mov	[bx],cx			;Set array type, cDims
	pop	cx			;Get oTxDim
	push	ax			;points after AD
	inc	bx
	inc	bx			;bx = pAD
	call	ExecDim
	jc	ShrinkType		;Remove this entry from type table
	mov	[bx+2].AD_fhd.FHD_hData,DGROUPSEG   ;Allocated in DGROUP
	mov	[bx+2].AD_fhd.FHD_cPara,ax ;Use size that's been rounded even
	neg	ax
	add	ax,[bp-SsCom].COM_oValCur ;Array starts at oValCur
	add	ax,[bp-SsCom].COM_bdValue.BD_pb
	mov	[bx+2].AD_fhd.FHD_oData,ax
	pop	bx			;Offset to element type
	add	bx,[bp-SsCom].COM_bdType.BD_pb
	jmp	short SetOTyp

ShrinkType:
	pop	dx			;Clean off stack
	mov	bx,[bp-SsCom].COM_oTypCur
	mov	[bp-SsCom].COM_bdType.BD_cbLogical,bx
CommonXj:
	jmp	CommonX

ComErrJ:
	jmp	ComErr

NewArType:
	inc	bx
	inc	bx				;Skip over cDims word
NewType:
	call	ExtendType
	jc	CommonXj
	add	bx,[bp-SsCom].COM_bdType.BD_pb	;Point into type table
	cmp	ch,cDimsFlag			;Have an array?
	jnz	SetOTyp
	mov	[bx-2],cx			;Set cDims
	cmp	cl,ComDimCnt			;Max allowed dimensions
	mov	ax,MSG_SubCnt			;Wrong no. of dimensions
	ja	ComErrJ
SetOTyp:
	mov	[bx],dx 			;Set oTyp
	cmp	dx,ET_FS			; Fixed? Record?
	jb	SkipOTypJ			; brif numeric, SD, or TX
	    .erre   ET_FS EQ ET_MAX		
	    je	    SetLength			
	mov	ax,[grs.GRS_oRsCur]
SetExtension:					
	inc	bx
	inc	bx
	mov	[bx],ax				;Add oRS for records
SkipOTypJ:
	jmp	SkipOTyp
SetLength:					
	mov	ax,[bp-SsCom].COM_cbFixed	; Length of FS
	jmp	SetExtension			

VtRfCommon:
        cmp     [SsErr],0                       ;Any errors so far?
	jnz	CommonXj			;Don't risk it if so

	;Set oCommon and oValue in variable table

	mov	ax,[bp-SsCom].COM_oCom		;Get oCommon
	mov	[bx].VAR_value.COMREF_oCommon,ax
	mov	cx,[bp-SsCom].COM_oValCur	;Get oValue
	mov	[bx].VAR_value.COMREF_oValue,cx
	GetOtyp ax,[bx] 			; Get oTyp of element
	mov	dx,ax				; Save
	call	CbTypOTypSCAN			; Get size of this type
	jnz	Check_Size			; Brif not fixed length
	mov	ax,[bx].VAR_cbFixed		; Get length of FS
	mov	[bp-SsCom].COM_cbFixed,ax	; Save

	;See if this stuff fits

Check_Size:					
	add	ax,cx				;New allocation
	inc	ax
	and	al,not 1			;Round up to even
	call	ChkComSize			;bx = oTypCur
	jc	CommonXj			;Quit if no room in value table

	;See if there's a type in table

	xor	cx,cx				;Ensure cDimsFlag is clear
	cmp	bx,[bp-SsCom].COM_bdType.BD_cbLogical	;Have entry in table?
	jae	NewType

	;Compare with existing type

	add	bx,[bp-SsCom].COM_bdType.BD_pb	;Point into type table
	jmp	CompType

VtRfCommonJ:
	jmp	SHORT VtRfCommon


;***
;Ss_VtRf - scan simple VtRf opcodes
;
;Purpose:
;
;   Functions are referenced using the same opcodes as variables.
;   The VtRf variants may reference a function.  However, if they do
;   it is an error.
;
;   Tasks:
;	1. bind to executor.
;	2. handle redirection.
;	3. handle references to functions (errors).
;	4. complete the scan task for COMMON.
;	5. if not COMMON, STATIC or SHARED then assume DIM of a
;	   simple variable.
;
;Algorithm:
;
;   Load and emit executor
;   Copy operand
;   Ensure that the variable is not a function.
;   If COMMON
;      Complete COMMON work
;   If not COMMON, SHARED or STATIC, assume DIM
;   Return to scnner main loop
;
;Input:
;
;   ax	  = opcode
;   bx	  = 2 * opcode
;   es:di = pcode emission address
;   es:si = pcode source address
;
;Output:
;
;   si updated
;
;Modifies:
;Exceptions:
;	Ss_Error
;
;******************************************************************
page

VtRfRedirect:
	mov	ax,[bx].VAR_value	;Get new oVar
	mov	PTRTX[di-2],ax		;Patch into pcode
	jmp	short VtRfRetry

SsProc	VtRf,Rude
	xchg	ax,bx			; BX = executor map address
	mov	al,byte ptr es:[si-1]	; High byte of opcode
	.erre	OPCODE_MASK EQ 03ffh	
	and	ax,HIGH (NOT OPCODE_MASK)
	shr	ax,1			; Convert to word offset
	add	bx,ax			; Index into map
	mov	ax,cs:[bx]		; Load executor
	STOSWTX 			;Emit the executor
	LODSWTX 			;Load operand
	STOSWTX 			;Emit the operand
VtRfRetry:
	    add     ax,[MrsCur.MRS_bdVar.BD_pb] ;oVar --> pVar
	xchg	bx,ax
	DbChk	pVar,bx 		;Verify that this is a variable
	mov	ax,[bx].VAR_Flags	;[5]
	    ;Check for VtRf to redirected variable.

	    TestX   ax,FVREDIRECT	;Is the variable redirected?
	    jnz     VtRfRedirect	;Brif Redirected variable.

	;Check for VtRf to a function error.

	TestX	ax,FVFUN		;Is this a ref to a function?
	jnz	VtRfToFun		;Error - VtRf to a function.

	mov	dx,ax			; Preserve var flags in dx
	    TestX   ax,FRAME		;Is it a frame var?
	    jnz     @F			;Brif not

	call	SsAllocOFrame		;Allocate an oFrame for this var
@@:

	    mov     al,[SsBosFlags]
	    test    al,SSBOSF_StCommon	;Is it a COMMON statement?
	    jnz     VtRfCommonJ		;Not a COMMON array - done
	    test    al,SSBOSF_StShared	;Is it SHARED?
	    jnz     VtRfX		;No work for SHARED

	;If NOT first ref, it's an error

	TestX	dx,FV_STATICSET 	; First reference?
	mov	ax,ER_DD		; Duplicate definition if not
	jnz	VtRfError		; Brif not first reference


VtRfX:
	;The oTx of the next emitted executor must be saved so that the
	;subsequent declaration can evaluate the array bounds by starting
	;execution at the saved address.

	mov	[SsOTxStart],di 	;Update pointer for next Dim clause
	jmp	[ScanRet]

VtRfToFun:
	call	TMError
	jmp	VtRfX


VtRfError:
	call	SsError 		
	jmp	VtRfX			

	public	mpAVtRfOpExe			
mpAVtRfOpExe	label	word			
	DWEXT	exAVtRfImp			
	DWEXT	exAVtRfI2			
	DWEXT	exAVtRfI4			
	DWEXT	exAVtRfR4
	DWEXT	exAVtRfR8			
	DWEXT	exAVtRfSD			


	public	mpVtRfOpExe			
mpVtRfOpExe	label	word			
	DWEXT	exVtRfImp			
	DWEXT	exVtRfI2			
	DWEXT	exVtRfI4			
	DWEXT	exVtRfR4
	DWEXT	exVtRfR8			
	DWEXT	exVtRfSD			

page
;***
;Subroutines for COMMON

ChkComSize:
;See if COMMON block is big enough, grow if needed (and possible)
;
;Input:
;       ax = New total length needed
;Output:
;       bx = oTypCur
;       CY set if unable to fit
;cx,dx preserved

	mov	[bp-SsCom].COM_oValCur,ax	;Update position
 	mov	bx,[bp-SsCom].COM_bdValue.BD_cbLogical	
 	sub	ax,bx				;Fit within present size?
	jz	BigEnough
	cmc					;Success if CY clear
	jnc	BigEnough
;COMMON block growing - unless it's in user library
	    cmp     [bp-SsCom].COM_bdValue.BD_cbPhysical,UNDEFINED ;UL COMMON?
	    jz	    NoGrowULCommon
	push	cx
	push	dx
	push	bx				
	lea	bx,[bp-SsCom].COM_bdValue
	push	ax				;Remember how much space
	push	bx				;Owner to grow
	push	ax				;additional space needed
	call	BdGrowVar			;Extend COMMON block value table
	pop	cx				;Amount of new space
	pop	bx				;Position in COMMON
	call	OMEcheck			;See if it worked
	jc	NoZero				;If alloc failed, don't init
;Zero out new COMMON block space
	push	di				;Save emit oTx
	mov	di,bx				;Position in COMMON
	push	ds
	pop	es				;es = ds
	add	di,[bp-SsCom].COM_bdValue.BD_pb	;Point to new COMMON block space
	xor	ax,ax
rep	stosb					;Zero out COMMON block
	pop	di				;Restore emit oTx
NoZero:
	pop	dx
	pop	cx
BigEnough:
	mov	bx,[bp-SsCom].COM_oTypCur	;Current type table offset
	ret

NoGrowULCommon:
	mov	ax,MSG_ULCom
	call	CyError
	jmp	BigEnough

OMECheck:
	or	ax,ax
	jnz	OkRet
OMError:
	mov	ax,ER_OM
CyError:
	call	SsError
	stc				;Unable to grow COMMON
OkRet:	ret


ExecDim:
;Execute the DIM statement for a $STATIC array in COMMON
;The array space is allocated in the COMMON value table if possible,
;or the error is reported.
;
;Inputs:
;	bx = pAD
;	cx = oTxDim
;Outputs:
;	CY set if failed (error reported)
;	ax = size of array, rounded up to whole words
;	bx = pTypCur
;Preserves:
;	dx

	mov	[SsScanExSrc],bx	;Pass pAD to DIM
	DbAssertRel cx,nz,NULL,SCAN,<No DIM for $STATIC COMMON array>
        mov     [SsScanExStart],cx
	mov	[bx].AD_fhd.FHD_hData,0	;Flag it as not allocated
	push	dx
	mov	[DimAtScanType],SSDIM_COMMON
	push	ax			; ExecuteFromScan requires
	push	ax			; two garbage parameters
        call    ExecuteFromScan
	pop	dx
        jnz	CyError			;Error reported by runtime (in ax)?
	mov	ax,[SsScanExSrc]	;Size of array returned by DIM
	inc	ax
	jz	OMError
	and	ax,not 1		;Round up to next word
;Make sure it's not too big
	mov	cx,ax
	add	ax,[bp-SsCom].COM_oValCur	;Get oValue
        jc	OMError
	call	ChkComSize
 	jc	OkRet				;Return with CY set if error
	add	bx,[bp-SsCom].COM_bdType.BD_pb	;bx = pTypCur
	xchg	ax,cx
	ret

ExtendType:
;Grow the COMMON type table
;
;Inputs:
;	bx = new length
;	dx = oTyp
;Outputs:
;	CY set if failed (error reported)
;Preserves:
;	bx,cx,dx

	push	dx
	push	cx
	push	bx
	inc	bx
	inc	bx				;Need at least one more word
	cmp	dx,ET_FS			; Record? Fixed?
	.erre	ET_FS EQ ET_MAX 		
	jb	SmallTyp			
	inc	bx				; FS have cbFixed
	inc	bx				;Records have oRS too
SmallTyp:
.errnz	COM_bdType - SsCom
	push	bp				;Owner
	push	bx				;New size
	call	BdRealloc			;Extend COMMON type table
	call	OMEcheck			;See if it worked
	pop	bx				;oTypCur
	pop	cx
	pop	dx
	ret

	page
;***
;Ss_ReDim - Scan opReDim
;
;Purpose:
;
;   Scan opReDim.
;
;   The opReDim opcode comes after a AIdLd opcode.  This scan
;   routine receives a scan stack entry describing the referenced
;   variable as follows:
;
;		oTx	    Always immediately preceeding point
;		oType	    <-- (Top of stack)
;
;   Tasks include:
;
;	1. Bind to executor
;	2. Coerce remaining arguments to integers
;	3. if argument is the first reference to an array
;	   then set whether the array is $STATIC or $DYNAMIC
;	4. Test for REDIMing a $STATIC array error.
;
;Input:
;
;   ax	  = executor
;   bx	  = 2 * opcode
;   es:si = pcode source address
;   es:di = pcode emission address
;
;Output:
;
;   si, di updated
;
;Modifies:
;
;Exceptions:
;
;   Ss_Error
;
;******************************************************************

	page

SsProc	ReDim
	STOSWTX 			;Emit the executor
	dec	si
	dec	si			;Report errors on preceding AIdLd

	pop	ax			;Discard oType of array
	DbAssertTst ax,z,ST_Array?,SCAN,<Ss_ReDim: Non-array>
	pop	bx			;BX = oTx after exAIdLd
	call	MakeArrayRef		;Convert to exAdRf

	    mov     bx,PTRTX[si-2]		;BX = oVar
	    add     bx,[mrsCur.MRS_bdVar.BD_pb] ;BX = pVar

	test	byte ptr [bx].VAR_fStat,FV_STATIC ;Is this a static array?
	jnz	ReDimStatic		;REDIM stmt. $Static arrays not allowed.
ReDimX:
	inc	si
	inc	si
	jmp	[ScanRet]		;Scan next opcode

ReDimStatic:
	mov	ax,MSG_OBA		;Array already dimensioned
	call	SsError
	jmp	ReDimX

	page
;SetArrayType
;
;Purpose:
;
;   Routine to set FV_STATIC and FV_STATICSET in an array var entry.
;
;   This routine sets FV_STATIC to the value in f_StaticCalc iff
;   FV_STATICSET is FALSE.  It then ensures that FV_STATICSET is TRUE.
;
;Input:
;
;   f_StaticCalc = TRUE if type is $STATIC, else FALSE
;   bx		 = pVariable for an array variable
;
;Outputs:
;
;   CY set if first reference
;
;Preserves:
;
;   all


	public	SetArrayType
SetArrayType:
	or	[SsFlags],SSF_HaveDimmed	;Had a DIM: no OPTION BASE now
SetArrayTypeNoDim:
	DbChk	pVar,bx 			;Verify that this is a variable
SetArrayTypePublic:
	TestM	[bx].VAR_flags,FV_STATICSET	
						;Determine if the $STATIC flag
						; is already set for this array.
	jnz	SetArrayTypeX			;Already set - nothing to do.

	;Note:	In EB, all declarations at the module
	;level are shared and all at the the procedure level
	;are not therefore there need not be two checks.

	    ;Don't set flags for shared variables while scanning proc

	    TestM   [bx].VAR_flags,FVSHARED	; Shared variable?
	    jz	    SetStatic			;If not, set bit
	    test    byte ptr [grs.GRS_oRsCur+1],80H ;Scanning module level?
	    jnz     SetArrayTypeX		;If not, don't set bit
SetStatic:

	.errnz	FALSE
	cmp	[f_StaticCalc],FALSE
	je	@F				;$DYNAMIC, leave FV_STATIC clear

	; Array is dynamic if it lives on the procedure frame

	    TestM   [bx].VAR_flags,FRAME	; In the stack?
	    jz	    @F				; Brif array is on frame
	or	byte ptr [bx].VAR_fStat,FV_STATIC	;Set $STATIC
@@:
	or	WORD PTR [bx].VAR_Flags,FV_STATICSET	; Indicate FV_STATIC is set.
	stc
SetArrayTypeX:
	ret



sEnd	SCAN


sBegin	CP

assumes cs, CP

;***
;ChainCommon - prepare blank common for chaining
;
;Purpose:
;
;   Module type tables will be destroyed by chaining, so the
;   COMMON type table must stop referring to it.  Instead, user
;   type entries are replaced with a flag and the size of the type.
;   Type checking against the chained-to program will only compare
;   record sizes.
;
;Preserves:
;
;   dx
;
;Modifies:
;
;   si
;
;***********************************************************************
;
cProc	ChainCommon,<PUBLIC,FAR,NODATA>,<SI>
cBegin
	mov	si,[grs.GRS_bdtComBlk.BD_pb]	;pBlankCommon
	DbAssertRel [si].COM_ogNam,z,NULL,CP,<ChainCommon: 1st block in bdtComBlk is not for blank COMMON> 
	mov	cx,[si].COM_bdType.BD_cbLogical	;Size of type table
	mov	si,[si].COM_bdType.BD_pb	;pTypeTable
	add	cx,si			;Ending address
LookForRecord:
	cmp	cx,si			;Reach end?
	jbe	CommonReady
	lodsw
	cmp	ah,cDimsFlag+StaticFlag	;Static array?
	jz	SkipStatic
	cmp	ah,cDimsFlag		;Array count?
	jnz	Element
GetElement:
	lodsw				;Skip count, do element
Element:
	cmp	ah,LengthFlag		;Already converted?
	jz	SkipOver
.errnz	ET_MAX - ET_FS			; ensure FS is max type
	cmp	ax,ET_MAX		;Record type or fixed length string?
	jb	LookForRecord		; If not, look at next
;Have a record or fixed length string
	xchg	bx,ax			;Save oTyp in bx
	lodsw				;Get oRS
	je	LookForRecord		; brif fixed length string
	xchg	bx,ax			;oTyp to ax, oRS to bx
	call	CbTypOTypOMrs		;Get its size
	mov	byte ptr [si-3],LengthFlag	;Flag as size-only entry
	mov	[si-2],ax
	jmp	LookForRecord

SkipStatic:
	cbw				;ah=0
	shl	ax,1
	shl	ax,1			;Size of dimensions
	add	ax,(size AD-1)		;Skip AD
	add	si,ax			;Point to element
	jmp	GetElement

SkipOver:
	lodsw				;Skip over length
	jmp	LookForRecord

CommonReady:
cEnd

;***
;SsTrimCommon
;
;Purpose:
;
;   After chaining, blank COMMON could be larger than actually used.
;   This routine trims it back, releasing any owners.  The value table
;   is not actually shortened (it could be in a UL), but the type table
;   is, and that's what counts.
;
;   THIS ROUTINE MUST NOT BE CALLED IF SCAN ERROR OCCURED
;
;Inputs:
;
;   [oTypComMax] = max size of type table
;   [oValComMax] = max size of value table
;
;***********************************************************************
	public	SsTrimCommon

SsTrimCommon:
	push	si
	push	di
	mov	si,[grs.GRS_bdtComBlk.BD_pb]	;pBlankCommon
	cmp	[si].COM_bdValue.BD_cbPhysical,UNDEFINED ; QuickLib common?
	jz	SharedQLB			; brif so -- don't delete
						; or zero-fill common
	mov	cx,[si].COM_bdType.BD_cbLogical
	mov	bx,[si].COM_bdValue.BD_pb	
	add	bx,[oValComMax]
	mov	si,[si].COM_bdType.BD_pb	;Pointer to type table
	add	cx,si				;Point last+1 of type table
	add	si,[oTypComMax]
	call	DelCommon
	mov	si,[grs.GRS_bdtComBlk.BD_pb]	;pBlankCommon
;Zero fill trimmed part of value table
	mov	di,[oValComMax]
	push	ds
	pop	es				;Make sure es=ds
	mov	cx,[si].COM_bdValue.BD_cbLogical
	sub	cx,di				;Amount to zero fill


	add	di,[si].COM_bdValue.BD_pb
	shr	cx,1
	xor	ax,ax
rep	stosw

SharedQLB:					
	mov	ax,[oTypComMax] 		
	mov	[si].COM_bdType.BD_cbLogical,ax ; Trim the type table

	pop	di
	pop	si
	ret


;***
;SsAdjustCommon
;
;Purpose:
;
;   Called whenever a QBI-specific COMMON value table is moved (by heap
;   management code).  Updates the backpointers to all SD's and string
;   array descriptors found in the value table for the given COMMON block.
;
;Inputs:
;
;   bx = pointer to COM_bdType field
;   di = adjustment factor to be passed on runtime
;	    if di=0, delete all SD's and arrays
;
;***********************************************************************

DelCommon:
;si = starting point in type table
;bx = starting point in value table
;cx = ending point in type table
	xor	di,di
	push	si
	jmp	short TestEndCommon

	public	SsAdjustCommon
SsAdjustCommon:
	push	si
	mov	cx,[bx].BD_cbLogical
	mov	si,[bx].BD_pb		;Pointer to type table
	mov	bx,[bx-COM_bdType].COM_bdValue.BD_pb
	add	cx,si			;Point last+1 of type table
TestEndCommon:
	cmp	si,cx			;End of type table?
	jae	AdjustX
	lodsw				;Get type table entry
	cmp	ah,cDimsFlag+StaticFlag	;Static array?
	jz	AdjustStatic
	cmp	ah,cDimsFlag		;Array?
	jz	AdjustArray
	cmp	ax,ET_MAX		; Record ?
	ja	AdjustRecord		; Brif yes
	.erre	ET_MAX LE 0100h 	; Assure we can use AL
	cmp	al,ET_SD		; String type?
	jb	NotRecord		; Brif not
	    .erre   ET_FS EQ ET_SD+1	; Assure JA is sufficient
	    .erre   ET_FS EQ ET_MAX	; Assure no new types added
	    ja	    RecWithLen		; Brif fixed string

	call	AdjustOneSD
	jmp	TestEndCommon

AdjustRecord:				
	test	ah,LengthFlag		;Has record been crunched to length?
	jnz	RecWithLen
	push	bx
	mov	bx,[si]			;Get oMRS
	inc	si
	inc	si
	call	CbTypOTypOMrs
	pop	bx
	jmp	short AddCbTyp

AdjustX:
	pop	si
	ret

NotRecord:
	call	CbTypOTyp		;Get length of item
AddCbTyp:
	inc	ax			
	and	al,not 1		;Round even
	add	bx,ax
	jmp	TestEndCommon

RecWithLen:
	lodsw				;Get length of record or string
	jmp	AddCbTyp

AdjustStatic:
	cmp	[si].AD_fhd.FHD_hData,0	;Any space in array?
	jz	AdjustX			;In the middle of building an entry


	add	[si].AD_fhd.FHD_oData,di;Adjust pointer to array data
	cbw				;ah=0
	shl	ax,1
	shl	ax,1			;Size of dimensions
	add	ax,(size AD-1)		;Skip header
	xchg	dx,ax			;Save in dx
	mov	ax,[si].AD_fhd.FHD_cPara;Get size of array
	add	si,dx			;Point to element
	cmp	word ptr [si],ET_SD	;String array?
	jz	AdjSdStatic
	add	bx,ax
EatOTyp:
	lodsw				;Get oTyp
	cmp	ax,ET_FS		
	    .erre   ET_FS EQ ET_MAX	
	jb	TestEndCommon		
	lodsw				;Skip over oMRS/length
	jmp	TestEndCommon

AdjustArray:
	push	cx
	push	bx
	or	di,di			;Delete the array?
	jz	DelArray
	lodsw				;Get oTyp of array
	cmp	ax,ET_SD		;Array of strings?
	jne	NoAdjArray
UpdStrings:				
	push	bx			;First arg for runtime
	push	di			;2nd arg for runtime
	call	B$IAdUpd		;Have runtime do update
AddCbArray:
	pop	bx
	pop	cx
	add	bx,ComArraySize
	jmp	TestEndCommon

DelArray:
	push	bx			;First arg for runtime
	call	B$IErase		;ERASE array (no heap movement varient)
	lodsw				;Get oTyp
NoAdjArray:				
	cmp	ax,ET_FS		
	    .erre   ET_FS EQ ET_MAX	
	jb	AddCbArray		
	lodsw				;Skip over oMRS/length
	jmp	AddCbArray

AdjSdStatic:
;ax has size of array
	shr	ax,1
	shr	ax,1			;ax = no. of 4-byte SD's
	push	cx
	xchg	cx,ax			;No. of elements to cx
AdjustEachSD:
	call	AdjustOneSD
	loop	AdjustEachSD
	pop	cx
	jmp	EatOTyp

AdjustOneSD:
	push	cx
	push	bx
	push	bx			;First arg for runtime
	or	di,di
	jz	DelSD
	push	di			;2nd arg for runtime
	call	B$ISdUpd
AddCbSD:
	pop	bx
	pop	cx
	add	bx,4			;Size of SD in value table
	ret

DelSD:
	call	B$STDL			;Delete the SD
	jmp	AddCbSD

sEnd	CP

end
