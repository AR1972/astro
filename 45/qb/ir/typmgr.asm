	TITLE	TYPMGR - Type Table Management Code for QBI

;***
;TypMgr.asm - Type Table Management Code for QBI
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Create and search for User defined types and elements of types
;Assumptions:
;	The "module type table" was at one point an actual table. With QB5/EB,
;	it was moved into the module variable table - - - both VAR and
;	TYP/ELEM structures have no particular order requirements, being
;	chained together, and both are created and discarded at the same
;	time. We can therefore have VAR, TYP, and ELEM structs interleaved 
;	in the one physical table, but continue to think of them as unique 
;	logical tables.
;
;	Each module type table consists of a single chain of TYP structures
;	the offset to the first TYP in this chain is at offset oOFIRST_TYP 
;	in the table.
;
;	The first TYP struct in each module type table must start at an offset
;	greater than ET_MAX so an oTyp for a user-defined record is always 
;	greater than any predefined type constant. Besides the offset to the 
;	first TYP, the number of currently defined types is found in the table
;	following the offset to the first TYP at offset oCTYPES.
;
;	Each user-defined type has an associated chain of elements. The offset
;	to the first element in a type is contained in the TYP structure. The
;	elements are always chained in the same order as they are found in
;	the text; this is ensured by the fact that the entire type table
;	is discarded (and the module put in SS_RUDE) whenever a type is
;	removed or an element is inserted out of order (i.e., not sequentially
;	after the last element inserted in the type).
;
;	Note that TYP and ELEM structures are intermingled in the table
;	The physical order doesn't matter, only the chain order (we can
;	never walk each physical entry in the table, only walk the TYP
;	chain and each ELEM chain).
;
;	The TYP and ELEM structures are similar; we take advantage of the
;	fact that the oNam field is in the same location for both.
;
;*******************************************************************************

	.xlist
	include version.inc
	TYPMGR_ASM = ON		;don't include EXTRNs defined in this file
	includeOnce	architec
	includeOnce	context
	includeOnce	heap
	includeOnce	names
	includeOnce	qbimsgs
	includeOnce	rtps
	includeOnce	scanner
	includeOnce	txtmgr
	includeOnce	variable
	.list

;	.sall

assumes DS,DATA
assumes ES,DATA
assumes SS,DATA
assumes CS,CP

sBegin	DATA
staticW	oElemLast,0		;used to allow DefineElem to leverage off of
				;some code in RefElem
externB b$ErrInfo
sEnd	DATA



sBegin	CP
assumes	cs,CP

;***
;DefineTyp(oNam) - create a new entry in the type table
;Purpose:
;  Called when the parser encounters a TYPE statement, to create a new
;  entry in the module type table for the type. 
;
;  Note that the scanner uses bits 13, 14, & 15 of an oTyp, so type tables
;  can be no larger than 8k.
;
;  Note: EB has a special use for this where the 'oNam' is really something
;	 else. The high bit set on input indicates that this is not really
;	 an oNam - - - the high bit is maintained in the oNam field of the typ.
;
;Entry:
;  oNam - module name table offset for the name of the type.
;
;Exit:
;  returns an offset into mrsCur.bdVar for the new type entry, or an
;  error code, depending on bit 15 of the return value. If bit 15 is
;  clear, an offset is returned, if set, the return value with bit 15
;  masked off is a standard basic error code.
;
;Exceptions:
;  none.
;  
;
;******************************************************************************
cProc	DefineTyp,<FAR,PUBLIC,NODATA>,<SI,DI>	
	parmW	oNam
	localW	oTypLast
cBegin	DefineTyp
;	register si = pTypBase
;	register di = pTyp
;	dx = cbOld
	DbChk	MrsCur
	mov	si,[mrsCur.MRS_bdVar.BD_pb]	
	mov	di,[si+oOFIRST_TYP]		
	mov	dx,[mrsCur.MRS_bdVar.BD_cbLogical] ; dx = cbOld
	jmp	SHORT DefineTyp_Loop_Check
DefineTyp_Loop:
	add	di,si		;pTyp = pTyp + pTypBase
	mov	ax,PTRVAR[di.TYP_oNam]
	cmp	[oNam],ax
	jnz	DefineTyp_No_Error

	mov	ax,08000H OR ER_DD
	jmp	short DefineTyp_Exit
DefineTyp_No_Error:
	mov	ax,PTRVAR[di.TYP_oTypNext]
	and	ah,07FH 	; mask off flag bit
	sub	di,si
	mov	[oTypLast],di	;oTypLast = pTyp - pTypBase
	xchg	ax,di		;di = new oTyp
DefineTyp_Loop_Check:
	or	di,di		;end of TYP chain?
	jnz	DefineTyp_Loop	;  brif not

	cmp	WORD PTR PTRVAR[si+oCTYPES],CTYPESMAX	;[3]
	je	DefineTyp_OM_ER1 ;brif this would cause table to have more than 
				;the legal max. number of types
	push	dx		;save cbOld across call
	PUSHI	ax,<dataOFFSET mrsCur.MRS_bdVar>	
	PUSHI	ax,<SIZE TYP>
	call	BdGrowVar	; grow type table enough for new type
	; we can't just blindly call BdGrowVar at a point where the user CAN
	;  continue - - - but txtmgr guarantees that any edit of a TYPE 
	;  statement or of an element in a TYPE block causes module rude edit
	DbAssertRel  grs.GRS_otxCONT,z,UNDEFINED,CP,<DefineTyp: CAN Continue>
	pop	dx		;restore cbOld
	or	ax,ax
	jne	DefineTyp_Cont	; brif attempt to grow table succeeded
DefineTyp_OM_ER1:		
	jmp	short DefineTyp_OM_ER		

DefineTyp_Cont:			
	mov	si,[mrsCur.MRS_bdVar.BD_pb]
				;update pTypBase in case of heap movement
	inc	PTRVAR[si+oCTYPES]		
				; increment count of types in table
	mov	di,dx		;cbOld
	add	di,si		;di = pTyp
	mov	ax,[oNam]
	mov	PTRVAR[di.TYP_oNam],ax		
	sub	ax,ax
	mov	PTRVAR[di.TYP_cbData],ax	
	mov	PTRVAR[di.TYP_oElementFirst],ax	
	mov	PTRVAR[di.TYP_oTypNext],ax	
	mov	bx,[oTypLast]
	cmp	PTRVAR[si+oOFIRST_TYP],ax	
				;special case of start of type table?
	jne	ChainTyp	;  brif not

	mov	bx,oOFIRST_TYP - TYP_oTypNext	
				; this will make the following instruction
				; put this oTyp in the table, to start the 
				; TYP chain
ChainTyp:
	or	PTRVAR[bx.TYP_oTypNext][si],dx	
	xchg	ax,dx		;cbOld = oTypNew = retval
DefineTyp_Exit:
cEnd	DefineTyp

DefineTyp_OM_ER:
	mov	ax,ER_OM OR 08000H
	jmp	SHORT DefineTyp_Exit

;***
;RefTyp(oNam, oTxRef) - Return oTyp for a type described by oNam
;
;Purpose:
;  Given an oNam and the text offset at which it was found, return the 
;  offset into the type table for the type of this name.
;  If the text offset at which the type was defined is larger than oTxRef
;  (i.e., if this amounts to a forward reference), return an error code,
;  MSG_UndType.
;
;Entry:
;  oNam - offset into the module name table.
;  oTxRef - offset into active text table where type reference was found
;
;Exit:
;  If bit 15 is clear, return value is an offset into the module type table 
;  for the desired type entry; if bit 15 is set, the return value is an 
;  error code; this error code with bit 15 masked off is a standard basic 
;  error code.
;  Only one error code is defined: the case where no type entry is found
;  with the input oNam.
;  If this error occurs, the parser will emit an opReParse - - - it is NOT
;  safe to create an empty type entry in this case, because the reference
;  might create a static variable entry, which would end up with a size
;  of zero for the value field ... a type reference prior to definition
;  must trigger an error.
;
;Exceptions:
;  none.
;Preserves:
;  ES
;******************************************************************************
cProc	RefTyp,<PUBLIC,FAR,NODATA>,<ES>   
	parmW	oNam
	parmW	oTxRef
cBegin	RefTyp
	DbChk	MrsCur

	;Check to see that this is not a forward reference
	cCall	OtxTypDefined,<oNam>
RefTyp_Cont:				
	mov	bx,[mrsCur.MRS_bdVar.BD_pb] 
	mov	dx,PTRVAR[bx+oOFIRST_TYP]; dx = offset to first typ in chain
	xchg	bx,dx
	cmp	ax,[oTxRef]		;returned from OtxTypDefined
	jb	RefLoop_Start
RefTyp_Err_Exit:
	mov	ax,MSG_UndType OR 08000H
	jmp	SHORT RefTyp_Exit
RefTyp_Loop:
	mov	bx,PTRVAR[bx.TYP_oTypNext] ; offset to next type (oTypCur)
	and	bh,07FH 		; mask off flag bit
RefLoop_Start:
	or	bx,bx
	je	RefTyp_Err_Exit		;brif no more entries - not found

	add	bx,dx			;bx = pTypCur
	mov	ax,PTRVAR[bx.TYP_oNam]	
	cmp	[oNam],ax
	jne	RefTyp_Loop		;brif names don't match
	or	BPTRVAR[bx.TYP_fReferenced],080H	
					;set fReferenced bit
	xchg	ax,bx
	sub	ax,dx			;subtract off table base for retval
RefTyp_Exit:
cEnd	RefTyp

;***
;DefineElem(oNam, oTyp, oTypElem) - Add an element to a type
;DefineElemFixed(cbFixed, oNam, oTyp, oTypElem) - alternate entry point.
;
;Purpose:
;  Given an oNam for a new element, the oTyp for the type entry the element
;  is to be a part of, and the oTyp for the type of the new element,
;  add the element to the chain of elements for that type.
;  DefineElemExp converted to DefineElemFixed as part of revision [7].
;Entry:
;  oNam - offset into mrsCur.bdlNam for the element being defined
;  oTyp - offset into mrsCur.bdVar for the type entry it will belong to
;  oTypElem - oTyp for the element that's being defined, i.e., the type
;             of the new element (can be some user defined type).
;  For DefineElemExp, oTypElem will be ET_FS or ET_FT, and this word
;	      parameter will also have its high bit set (per pcode) if
;	      the cbFixed parameter is really an oNam of a constant which
;	      contains the length of the fixed-length string/text.
;Exit:
;  A new element entry is allocated, completely filled in, and linked in to
;     the end of the element chain for the given type.
;  Return value is a standard BASIC error code, OR'd with 0x8000 for 
;     consistency with other TypeMgr functions. Possible error codes are:
;
;     ER_DD - already exists an element of this type of that oNam
;     ER_OM - Out of Memory
;     MSG_UndType - recursive definition, i.e., element not allowed to
;                       be of the same oTyp as its parent type.
;     MSG_InvConst - DefineElemExp called with the oNam for a CONSTant, and
;			some error occured in finding a matching CONSTant.
;     If all bits are clear (i.e., 0 is returned), no error.
;
;  NOTE: As a side effect of this routine, mkVar.MKVAR_oTyp can be modified.
;        This is wierd, but it saves some code in RefElem and callers.
;
;Exceptions:
;  none.
;
;******************************************************************************
cProc	DefineElemFixed,<PUBLIC,NEAR,NODATA>
	parmW	cbFixed
	parmW	oNam
	parmW	oTyp
	parmW	oTypElem
cBegin
	mov	cx,[cbFixed]
	cCall	DefineElemCommon,<oNam,oTyp,oTypElem>
cEnd

cProc	DefineElem,<PUBLIC,NEAR,NODATA>
cBegin	<nogen>
	xor	cx,cx
cEnd	<nogen>

cProc	DefineElemCommon,<NEAR,NODATA>,<SI,DI>
	parmW	oNam
	parmW	oTyp
	parmW	oTypElem
	localW	cbOld
	localW	pTyp
	localW	cbFixed 		
cBegin
	mov	[cbFixed],cx		; remember if fixed-length string/text
					;   element or not
	jcxz	DefineElem_Shared	;brif DefineElem was called
	mov	ax,[oTypElem]
	or	ah,ah			
	jns	DefineElem_Shared	; brif cbFixed actually is a byte count

	and	ah,07FH 		; mask off high bit
	mov	[oTypElem],ax		; restore as an actual ET_ type
	xchg	ax,cx			
	;ax = oNam of a CONSTant
	mov	[mkVar.MKVAR_oNam],ax
	mov	[mkVar.MKVAR_oTyp],ET_I2
	mov	[mkVar.MKVAR_flags],0	;only want to find ET_I2 match
	or	[mkVar.MKVAR_flags2],MV_fDontCreate
	call	MakeVariable
	xchg	ax,bx			;put retval in bx
	mov	ax,MSG_InvConst	OR 08000H ;in case of error return
	or	bx,bx
	js	DefineElem_Exit2	;brif some error finding the CONSTant

	add	bx,[mrsCur.MRS_bdVar.BD_pb]
	mov	ax,PTRVAR[bx.VAR_value] ;get the I2 CONSTant value
	mov	[cbFixed],ax
DefineElem_Shared:			;code common to both entry points from
					;  here on
	DbChk	oNam,oNam
	DbChk	oTyp,oTyp
	DbChk	oTyp,oTypElem
	mov	ax,[mrsCur.MRS_bdVar.BD_cbLogical] 
	mov	[cbOld],ax
	mov	ax,[oTypElem]
	cmp	[oTyp],ax
	mov	ax,MSG_UndType OR 08000H ;give this error for self recursion.
					 ;note that that parser won't allow
					 ;indirect recursion case to occur
	jz	DefineElem_Exit2	;brif self recursion - error

	mov	[oElemLast],0		

	push	[oNam]			; parm to RefElem
	PUSHI	ax,ET_IMP		; error if match regardless of type
	mov	ax,[oTyp]		
	mov	[mkVar.MKVAR_oTyp],ax	
	call	far ptr RefElem 	; ax = retval - better be an error
					;  NOTE: Updates oElemLast
	cmp	ax,MSG_UndElem OR 08000H ; RefElem shouldn't have found elem
	mov	ax,ER_DD OR 08000H	;ER_DD if it did
	jz	DefineElem_Cont0
DefineElem_Exit2:
	jmp	DefineElem_Exit1	
DefineElem_Cont0:
	mov	cx,[cbFixed]		
	jcxz	Grow_tElem		; brif not fixed-length string/text elem
					
	mov	cx,2			; size of ELEM_cbFixed field
Grow_tElem:				
	add	cx,SIZE ELEM
	PUSHI	ax,<dataOFFSET mrsCur.MRS_bdVar>	
	push	cx			
	call	BdGrowVar		; grow type table enough for new type
	or	ax,ax
	je	DefineElem_OM_Error_1

	mov	di,[mrsCur.MRS_bdVar.BD_pb] ;di points to base of type table
	mov	ax,[oTyp]
	add	ax,di
	mov	[pTyp],ax
	mov	si,[cbOld]
	add	si,di			;si points to element being defined
	mov	ax,[oNam]
	mov	PTRVAR[si.ELEM_oNam],ax	
	mov	PTRVAR[si.ELEM_oElementNext],0 ;[3]
	mov	ax,[oTypElem]
	mov	PTRVAR[si.ELEM_oTyp],ax	
	cCall	CbTyp,<ax>
	jnz	Got_cbTyp		; brif type wasn't ET_FS or ET_FT
					
	mov	ax,[cbFixed]		
	mov	PTRVAR[si.ELEM_cbFixed],ax 
Got_cbTyp:
	mov	bx,[pTyp]
	mov	cx,PTRVAR[bx.TYP_cbData]
	mov	PTRVAR[si.ELEM_oVar],cx	
	add	PTRVAR[bx.TYP_cbData],ax
	jc	DefineElem_OM_Error	; brif wrap beyond 64k

	mov	ax,[oTypElem]
	cmp	ax,ET_MAX
	jbe	@F			; brif new elem not of user-def. type

	or	BPTRVAR[bx.TYP_fReferenced],080H    
					;set fReferenced bit
@@:
	cmp	ax,ET_SD		; dynamic string?
	jnz	@F			; brif not
	or	BPTRVAR[bx.TYP_flags],F_NOBLKCPYTYP 
					; remember that in order to assign a
					; var of this type to another such
					; var, we can't simply block copy
@@:					
	mov	bx,[oElemLast]
	mov	ax,[cbOld]
	or	bx,bx			
	jne	Not_1st_Elem		;brif this is not 1st elem in typ
	mov	bx,[pTyp]		;special start of elem chain code
	mov	PTRVAR[bx.TYP_oElementFirst],ax	
	jmp	SHORT DefineElem_Cont2
DefineElem_OM_Error_1:			
DefineElem_OM_Error:
	mov	ax,ER_OM OR 08000H
DefineElem_Exit1:
	jmp	short DefineElem_Exit
Not_1st_Elem:				;link new element in @ end of chain
	mov	PTRVAR[bx.ELEM_oElementNext][di],ax	
DefineElem_Cont2:
	sub	ax,ax
DefineElem_Exit:
cEnd	DefineElem

;***
;RefElem - return offset to specified element in tTyp
;
;Purpose:
;  Given the oNam for an element in type mkVar.MKVAR_oTyp, return the offset 
;  into mrsCur.bdVar for the element, and place the type of the element into
;  mkVar.MKVAR_oTyp.
;
;Entry:
;  oNam - name of the element to be found
;  oTypElem - oTyp of element; ET_IMP if caller doesn't know type.
;		This is used so we can give ER_TM if user puts an (incorrect)
;		explicit type char on an element reference.
;  The oTyp of the parent type is in mkVar.MKVAR_oTyp.
;Exit:
;  return value is an offset into mrsCur.bdVar for the element if bit
;     15 is clear, or is a standard BASIC error code OR'd with bit 15 if set.
;  If successful, mkVar.MKVAR_oTyp is changed to the oTyp of the found element.
;  If the found element is a fixed-length string/text, mkVar.MKVAR_fsLength
;	is changed to the length of the string.
;  Static oElemLast:
;	if the element contains no types of the found element is the first
;		in the chain, oElemLast will be unchanged.
;	else if the element is found, oElemLast will be an offset to the
;		previous element in the chain.
;	else (element not found in non-empty element chain), oElemLast is
;		an offset to the last element in the chain.
;
;Exceptions:
;  none.
;
;******************************************************************************
cProc	RefElem,<PUBLIC,FAR>,<si,di>	
	parmW	oNamElem		
	parmW	oTypElem		
cBegin					
	mov	si,[mrsCur.MRS_bdVar.BD_pb] 
	mov	bx,[mkVar.MKVAR_oTyp]	
	DbChk	UserTyp,bx		;Ensure the oTyp we're looking in is 
					;  valid, and a user defined oTyp
	mov	ax,PTRVAR[bx.TYP_oElementFirst][si]	
	and	ah,07FH			;mask off fReferenced bit
	xchg	di,ax			;di = table offset to first elem in typ
	mov	dx,[oNamElem]		
	DbChk	oNam,dx 		
	jmp	short RefElem_LoopStart 

RefElem_MSG_Undefined:
	mov	ax,MSG_UndElem OR 08000H
	jmp	SHORT RefElem_Exit

RefElem_Loop:
	mov	di,PTRVAR[di.ELEM_oElementNext]	
RefElem_LoopStart:
	or	di,di			
	jz	RefElem_MSG_Undefined	;brif end of elem chain - elem not found
	
	mov	[oElemLast],di		;always keep oElem of last elem here
	add	di,si			;di = pElem
	cmp	PTRVAR[di.ELEM_oNam],dx	
	jnz	RefElem_Loop		;brif no match

	mov	ax,PTRVAR[di.ELEM_oTyp]	
	mov	[mkVar.MKVAR_oTyp],ax	;set oTyp of found element in location
					;  provided by caller
	mov	bx,ax			
	cmp	ax,ET_FS		
	jnz	RefElem_Exit1		

	mov	bx,ET_SD		; explicit type char for ET_SD ($)
					; legally matches element of ET_FS
	mov	ax,PTRVAR[di.ELEM_cbFixed] 
	mov	[mkVar.MKVAR_fsLength],ax  
RefElem_Exit1:
	xchg	ax,di			;ax = pElemFound
	sub	ax,si			;ax = retval = oElemFound

	.errnz	ET_IMP - 0		
	mov	cx,[oTypElem]		
	jcxz	RefElem_Exit		; brif caller doesn't want to
					; check type of element
	cmp	cx,bx			; does element match given oTyp?
	jz	RefElem_Exit		; brif so

	mov	ax,ER_TM OR 08000H	; type mismatch error
RefElem_Exit:				
cEnd					


;***
;CompareTypsRecurse - compare 2 types to see if they're the same
;
;Purpose:
;	Given two oTyps, compare them (recursively element-by-element)
;	to see if they're the same. The given oTyp's do not have to be
;	for user-defined types - - - any valid oTyp's are okay.
;
;Entry:
;  ax = oTyp1 - first type
;  bx = oTyp2 - first type
;  if SizeD,
;		ds is set to seg of type table for oTyp1,
;		es is set to seg of type table for oTyp2.
;  else
;		si points to base of type table for oTyp1
;		di points to base of type table for oTyp2
;Exit:
;  PSW.Z set if the two types match, reset if not.
;	if PSW.Z set, CX = 0 indicates no further comparison need be made.
;	if CX != 0, however, the oTyp's are either ET_FS or ET_FT, and
;	the lengths must be compared by the caller (who presumeably has
;	access to these lengths).
;  If PSW.Z reset, CX = 0 if routine succeeded, ER_OM if insufficient
;	stack space for required recursion.
;Exceptions:
;  none.
;
;******************************************************************************
cProc	CompareTypsRecurse,<NEAR,NODATA>
cBegin	CompareTypsRecurse
	cmp	bx,ET_MAX		;is oTyp2 user-defined?
	jbe	Compare_Cmp_Exit	; brif not

	cmp	ax,ET_MAX		;is oTyp1 user-defined?
	jbe	Compare_Cmp_Exit	; brif not
	add	ax,si			;ax = pTyp1
	add	bx,di			;bx = pTyp2
	mov	bx,PTRVAR[bx.TYP_oElementFirst]	
	and	bh,07FH			;mask off fReferenced bit
	xchg	ax,bx
	mov	bx,[bx.TYP_oElementFirst]
	and	bh,07FH			;mask off fReferenced bit
Elem_Compare_Loop:
	;bx = oElem1, ax = oElem2
	or	bx,bx			
	jz	Compare_Cmp_Exit	;end of chain 1 - set exit code based on
					; whether both chains end
	or	ax,ax			;end of chain 2?
	jz	Compare_Cmp_Exit	;brif end of chain 2  - reset PSW.Z,exit
					;not end of either chain - - continue

	add	bx,si			;bx = pElem1
	push	bx			;save across recursive call
	mov	bx,[bx.ELEM_oTyp]
	xchg	ax,bx			;ax = oTyp of element 1, bx = oElem2
	add	bx,di			;bx = pElem2
	push	bx			;save across recursive call
	mov	bx,PTRVAR[bx.ELEM_oTyp]	; bx = oTyp of element2
	mov	cx,sp
	sub	cx,6			;CompareTypsRecurse requires 6 bytes
					; of stack space per invocation
	cmp	cx,[b$pend]
	ja	Compare_Cont		;brif sufficient stack space to recurse

	mov	cx,ER_OM		;abnormal termination - not enough stack
	mov	[b$ErrInfo],OMErr_STK;note this is really Out of Stack space
	pop	ax			; clean stack
	pop	ax			; clean stack
	or	sp,sp			;reset PSW.Z to indicate failure
	jmp	short CompareTypsRec_Exit1
Compare_Cont:
	call	CompareTypsRecurse	;compare these types
	pop	bx			;pElem2Old
	pop	ax			;pElem1Old
	jnz	CompareTypsRec_Exit	;if any element match fails, whole
					; process terminates
	jcxz	Compare_Cont_1

	;ET_FS or ET_FT - - - oTyp's compare, but must also check string
	;lengths - - -
	xchg	ax,bx			
	mov	cx,[bx.ELEM_cbFixed]	; cx = size of element1
	xchg	ax,bx			
	cmp	cx,PTRVAR[bx.ELEM_cbFixed] 
	jnz	CompareTypsRec_Exit	; if lengths are different, no match
Compare_Cont_1:
	mov	bx,PTRVAR[bx.ELEM_oElementNext]	; fetch new oElem2
	xchg	ax,bx			;ax = oElem2, bx = pElem1Old
	mov	bx,[bx.ELEM_oElementNext]
	jmp	short Elem_Compare_Loop	;continue until end of both chains
					; found, or an element pair is found
					; that doesn't match
Compare_Cmp_Exit:
	cmp	ax,ET_FS		; special comparison required?
	jnz	CompareTypsRec_Cmp	; brif no special compare step
	;ax is either ET_FS or ET_FT	
	cmp	ax,bx			; set condition codes for retval
	mov	cx,sp			; caller must check string lengths
	jmp	short CompareTypsRec_Exit1 
CompareTypsRec_Cmp:			
	cmp	ax,bx			;sets condition codes for retval
CompareTypsRec_Exit:
	mov	cx,0			;routine terminated normally
CompareTypsRec_Exit1:
cEnd	CompareTypsRecurse

;***
;CompareTyps - compare 2 types to see if they're the same
;
;Purpose:
;	Given two oTyps, compare them (recursively element-by-element)
;	to see if they're the same. The given oTyp's do not have to be
;	for user-defined types - - - any valid oTyp's are okay.
;
;	This routine does the start-up work, and uses
;	CompareTypsRecurse to do the actual comparison.
;
;	Interface modified as revision [15].
;
;Entry:
;  ax = oRs1  - oRs of first type
;  bx = oRs2  - oRs of first type
;  cx = oTyp1 - first type
;  dx = oTyp2 - first type
;
;  parm1 = oRs1 = oRs of 1st type
;  parm2 = oRs2 = oRs of 2nd type
;  parm3 = oTyp1 = oTyp of 1st type
;  parm4 = oTyp2 = oTyp of 2nd type
;Exit:
;  PSW.Z set if the two types match, reset if not.
;  If PSW.Z reset, CX = 0 if routine succeeded, ER_OM if insufficient
;	stack space for required recursion.
;
;  AX = 0 if two types match
;  If AX != 0, DX = 0 if routine succeeded, ER_OM if insufficient
;	stack space for required recursion.
;Preserves:
;  ES - scanner depends on this (in non-windows versions)
;Exceptions:
;  none.
;
;******************************************************************************
cProc	CompareTyps,<PUBLIC,FAR,NODATA>,<SI,DI,ES>	
	parmW	oRs1			
	parmW	oRs2			
	parmW	oTyp1			
	parmW	oTyp2			
cBegin	CompareTyps
assumes ds,DATA 			
	mov	ax,[oRs1]		; parm to OMrsORs
	call	OMrsORs 		;get oMrs of type1
	mov	si,[oRs2]		
	xchg	si,ax			;si = oMrs1, ax = oRs2
	call	OMrsORs			;get oMrs of type2
	xchg	ax,di			;di = oMrs2, ax = garbage
	mov	cx,[oTyp1]		
	mov	dx,[oTyp2]		
	cmp	si,di			;oTyp's in different modules?
	jnz	Diff_Module		; brif so

	cmp	cx,dx			;return PSW.Z set appropriately
	mov	cx,0			;CompareTyps terminated normally
	jmp	short CompareTyps_Exit
Diff_Module:
	push	[grs.GRS_oRsCur]
	push	cx			;preserve oTyp's across call
	push	dx
	call	MrsDeActivate		;so both mrs's are in mrs table
	RS_BASE add,si			; si = pMrs1
	RS_BASE add,di			; di = pMrs2
	GETRS_SEG es,bx,<SIZE,LOAD>	;[5] es == Rs table seg, trashes bx
	mov	si,PTRRS[si.MRS_bdVar.BD_pb] ;[2] si = base pointer to type table 1
	mov	di,PTRRS[di.MRS_bdVar.BD_pb] ;[2] di = base pointer to type table 2
	pop	bx			;bx = oTyp2
	pop	ax			;ax = oTyp1
	call	CompareTypsRecurse
	pop	ax			;oRsCur on entry
	pushf				;save retval flags
	cCall	RsActivateCP,<ax>	;restore oRsCur to entry value
	popf
CompareTyps_Exit:
	mov	dx,cx			; per new interface
	mov	ax,sp			; non-zero
	jnz	CompareTyps_Exit_1	; brif types don't match

	sub	ax,ax			
CompareTyps_Exit_1:			
cEnd	CompareTyps

;***
;ONamOElem, ONamOTyp - Return the oNam for the name of a given element or type
;Purpose: 
;  Used for descanning. Given an offset into mrsCur.bdVar to an element or type
;  entry, returns the oNam for the name of the element.
;
;Entry:
;  oElem or oTyp - offset into mrsCur.bdVar for the desired element or type
;
;Exit:
;  return value is an offset into the module name table for the name of the
;	element or type.
;
;Exceptions:
;  none.
;
;Preserves:
;  All but AX and BX (for callers in CP. Callers from outside CP cannot
;  assume this).
;
;******************************************************************************
PUBLIC	ONamOTyp
ONamOTyp PROC FAR
	.errnz	TYP_oNam - ELEM_oNam
	;fall into ONamOElem, taking advantage of the fact that the oNam
	;  field is in the same position in the ELEM and TYP structures.
ONamOTyp ENDP
cProc	ONamOElem,<PUBLIC,FAR,NODATA>
	parmW	oStruc			
cBegin	ONamOElem
	mov	bx,[oStruc]		
	add	bx,[mrsCur.MRS_bdVar.BD_pb]
	mov	ax,[bx.ELEM_oNam]
cEnd	ONamOElem

;===============================================================================

;***
;ForEachPrimElem - recursively walk each primitive element in a TYPE
;
;Purpose:
;  Recursively visit each primitive element in a TYPE. By "primitive element"
;  we mean an element that is not itself of some user-defined type.
;  For each primitive element, call the near routine pointed to by SI.
;  In the special case where SI == 0, just increment CX instead, i.e., this
;  routine then simply counts all primitive elements.
;
;Entry:
;  an oTyp in ax.
;  SI == 0 to count, or is a near pointer to a helper routine.
;
;Exit:
;  if SI == 0, cx = count of primitive elements on exit,
;  otherwise cx is not touched, and can be used as a return value by the
;	helper routine.
;  Does not use dx - - - caller & helper routine can also use dx as desired.
;
;Exceptions:
;  In non-RELEASE case, DebHalt may be called if input not a valid 
;     user-defined oTyp.
;
;******************************************************************************
DbPub	ForEachPrimElem 		
cProc	ForEachPrimElem,<NEAR,NODATA>,<DI>
cBegin
	DbChk	oTyp,ax
	mov	bx,[mrsCur.MRS_bdVar.BD_pb] 
	mov	di,ax
	mov	di,PTRVAR[di.TYP_oElementFirst][bx]	
	and	di,07FFFH		;mask off fReferenced bit
	add	di,bx			;di = pElem
	add	ax,bx
	cmp	ax,di
	jz	ForEachPrimElem_Exit

ForEachPrimElem_Loop_Start:
	lea	ax,[bx+0]		
	cmp	ax,di
	jz	ForEachPrimElem_Exit	;brif end of chain

	mov	ax,PTRVAR[di.ELEM_oTyp] 
	cmp	ax,ET_MAX
	ja	@F			; brif user-defined type

	or	si,si			; special case?
	jnz	CallHelper		; brif not - - call helper

	inc	cx			; increment count of prim elements
	jmp	short ForEachPrimElem_Continue 
CallHelper:				
	;ax == oTyp of primitive element
	;if SizeD, es == segment of type table
	;di == pElem for primitive element
	call	si			; call helper for this prim element
	jmp	short ForEachPrimElem_Continue 
@@:					
	call	ForEachPrimElem 	; recurse to handle elements in
					;	this user-defined type
ForEachPrimElem_Continue:		
	mov	di,PTRVAR[di.ELEM_oElementNext]
	add	di,bx			;add table base to get next element
	jmp	short ForEachPrimElem_Loop_Start
ForEachPrimElem_Exit:
cEnd


;***
;CPrimElemFar(oTyp) - return the number of primitive elements in a type
;
;Purpose:
;  Recursively count the total number of primitive elements owned by a 
;  given user-defined type. 
;
;Entry:
;  an oTyp.
;
;Exit:
;  a count of the number of actual elements (i.e., of type ET_I2, ET_I4,
;     ET_R4, ET_R8, fixed-length string, etc) in the type.
;
;Exceptions:
;  In non-RELEASE case, DebHalt may be called if input not a valid 
;     user-defined oTyp.
;
;******************************************************************************
cProc	CPrimElemFar,<PUBLIC,FAR,NODATA>,<SI>
	parmW	oTyp
cBegin	CPrimElemFar
	mov	ax,[oTyp]
	sub	si,si			; just inc cx for each prim element
	sub	cx,cx			; initialize count
	call	ForEachPrimElem 	; cx == count of primitive elemtns
	xchg	ax,cx			; ax == retval
cEnd	CPrimElemFar

;===============================================================================


sEnd	CP


;===============================================================================


	end
