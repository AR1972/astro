	TITLE	context.asm - context manager

;***
;context.asm - context manager for the Quick Basic Interpreter
;
;	Copyright <C> 1986, 1987 Microsoft Corporation
;
;Purpose:
;   -  Creation and Deletion of entries in the Module and Procedure Register Set
;       tables.
;   -  Swapping module and procedure register sets between the global table
;	of Rs's and the static current working register sets 'mrsCur' and
;       'prsCur'.
;
;   Note: static handling of Procedure Register Sets is performed by this
;		module; frame (per invocation) handling is performed by
;		the Procedure Manager.
;
;
;******************************************************************************

	.xlist

	include version.inc
	CONTEXT_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	conint
	includeOnce	heap
	includeOnce	names
	includeOnce	pcode
	includeOnce	qbimsgs
	includeOnce	rtinterp
	includeOnce	parser
	includeOnce	sb			
	includeOnce	scanner
	includeOnce	txtmgr
	includeOnce	ui
	includeOnce	util
	includeOnce	variable
	includeOnce edit			

	.list

	assumes CS,CP
	assumes DS,DATA
	assumes SS,DATA
	assumes ES,NOTHING


sBegin	DATA
	globalB	conFlags,F_CON_StaticStructs	;static bit flags
	staticB	fCouldBeBogusPrs,FALSE		;TRUE if there's a possiblity of
	staticW	oRsTest,UNDEFINED		;For use in ValidORs
	globalW	oRsDupErr,0			;MrsMake and PrsMake tell
						; caller duplicate oRs
	globalW	oPrsFirst,UNDEFINED		; head of prs chain in tRs
	staticW	oFreePrsFirst,UNDEFINED		; head of free prs chain
	staticW	oFreeMrsFirst,UNDEFINED		; head of free mrs chain

sEnd	DATA

	extrn	B$ClearRange:FAR		;clears all owners in given rg



sBegin CP

;==============================================================================
;Notes On mrs and prs structures:
;	There is a single global table which contains both mrs and prs
;	structures. The first entry in this table MUST be the global mrs
;	(code exists which assumes the oMrs for the global mrs is OMRS_GLOBAL).
;	We also assume that, if there is an empty unnamed mrs, it is the 
;	very next struct in the table after the global mrs.
;
;	All mrs valid mrs entries are chained from this entry via the
;	oMrsNext entry; UNDEFINED marks the end of the chain.
;	Prs's are similarly chained together, with the global 'oPrsFirst'
;	pointing to the first prs.   It is NOT safe to assume that these
;	chains relate to table position; the only safe way to walk the
;	Rs table in any way is via the appropriate chain.
;
;	Table entries are fixed in place; oRs/oMrs/oPrs's are known and
;	saved outside of this component, so an mrs or prs can never move
;	within this structure. If an entry is discarded, it is added to
;	a free chain, for later re-use. Since mrs and prs structs are of
;	different size, there are two free chains; oFreePrsFirst and
;	oFreeMrsFirst are the head pointers for these chains.
;
;	Note that when an mrs or prs is 'active' (i.e., copied to mrsCur/
;	prsCur) the copy in the table should not be referenced or modified
;	but that entry remains linked in place. Care should be taken when
;	updating an Rs chain to consider the active prs and/or mrs, i.e.,
;	mrsCur and prsCur are not and cannot be linked in an rs chain.
;	
;	The actual first entries in the mrs & prs structs are the count of
;	frame temps and frame vars allocated for each instance of that
;	module or procedure. There are definite dependancies on the fact
;	that these are in the same spot in the mrs & prs structs.
;
;	For all prs's, there is guaranteed to be a name table entry for the 
;	prs name. This eliminates some OM_ERR checking when obtaining the oNam 
;	of a prs (generally via FieldsOfPrs).
;
;	At execution time, the heap-update & block copy time required by the
;	normal context switching code is too slow for CALL speed. For this
;	reason, whenever program execution is started, DisStaticStructs is
;	called to set a static flag and deactivate mrsCur, prsCur, and txdCur.
;	At execution time (only), a couple of grs fields are used to allow
;	for quickly fetching the segment address of the current text table.
;	Note that whenever this flag (F_CON_StaticStructs) is reset (FALSE), 
;	the mrsCur and prsCur structures contain garbage; only the oMrsCur, 
;	oPrsCur, and oRsCur fields in grs can be used to access current 
;	context information.
;
.errnz	SIZE MRS AND 1
.errnz	SIZE PRS AND 1
;	The two assertions above are based on the procedure manager depending
;	on an oRs always being an even number (so the low bit can be used
;	as a boolean in a special case). For oRs's to always be even, mrs
;	and prs structs must in turn both be even.
;	
;==============================================================================


;##############################################################################
;#									      #
;#			  Initialization Functions			      #
;#									      #
;##############################################################################

sEnd	CP				;initialization code goes in RARE seg
sBegin	RARE
	assumes CS,RARE

;***
;InitContext()
;
;Purpose:
;    This function is called once during BASIC initialization to initialize:
;     - the global register set (grs)
;     - the Rs table (grs.bd[l]Rs), mrsCur, and the global mrs via MrsMake
;Entry:
;	none.
;Exit:
;	grs, mrsCur initialized.
;	ax = 0 if no error, else		[18]
;	ax = standard error code.		[18]
;	PSW flags set up based on an OR AX,AX	[18]
;*******************************************************************************
cProc	InitContext,<NEAR,PUBLIC,NODATA>	
cBegin	InitContext
	mov	ax,dataOFFSET grs	;ax == pointer to global 'grs' struct
	mov	bx,SIZE GRSTYPE		;bx == size of 'grs' struct
	mov	cx,GRS_CB_ZERO_INIT	;cx == cb @ start of grs to 0-fill
	push	ax			; parm to ZeroFill
	push	cx			; parm to ZeroFill

	shr	bx,1			; convert cbStruct to cwStruct
	cCall	FillUndef,<ax,bx>	; fill whole struct with UNDEFINED
	cCall	ZeroFill		; fill first 'cbZeroes' with zeroes

	PUSHI	ax,<dataOFFSET grs.GRS_bdlDirect>
	PUSHI	ax,CB_PCODE_MIN 	;it must never be < CB_PCODE_MIN bytes
	PUSHBDL_TYPE  pgtypEBPcode,ax	; pass sb type for EB version
	call	BdlAlloc		;allocate direct mode buffer (far heap)
	or	ax,ax
	DJMP	jz	OM_Err_In_Init	
	PUSHI	ax,<dataOFFSET grs.GRS_bdRs>
	PUSHI	ax,0
	PUSHI	ax,IT_MRS
	call	BdAlloc 		;allocate table of register sets
	or	ax,ax
	jz	OM_Err_In_Init

	PUSHI	ax,OGNAM_GMRS		; make global module
	PUSHI	ax,<100h * FM2_File>

	call	far ptr MrsMake		; make initial (untitled) mrs 
	or	ax,ax			
	jnz	InitContext_Exit	; return error code to caller

	call	far ptr MakeInitMrs	;make initial mrs, ax=errCode
	; just pass retval to caller for non FV_QB4LANG case
	or	ax,ax			
	jnz	InitContext_Exit	

	PUSHI	ax,<dataOFFSET grs.GRS_bdtComBlk>
	PUSHI	ax,0
	PUSHI	ax,IT_COMMON_BLOCK
	call	BdAlloc			;allocate table of common blocks
	or	ax,ax
	jz	OM_Err_In_Init

	PUSHI	ax,UNDEFINED
	call	MakeCommon		;allocate table(s) for blank common
	inc	ax			;UNDEFINED returned if OM error
	.errnz	UNDEFINED - 0FFFFH
	jz	OM_Err_In_Init

	DbAssertRel ax,z,1,RARE,<InitContext: Unnamed block not at offset 0>
	mov	[grs.GRS_fDirect],al	;so UserInterface() thinks opEot was
					;from direct mode buffer
	sub	ax,ax			; retval; 0 == 'no errors'
InitContext_Exit:			
	or	ax,ax			
cEnd	InitContext

OM_Err_In_Init:
	mov	ax,ER_OM		
	jmp	short InitContext_Exit	

sEnd	RARE
sBegin	CP
	assumes CS,CP



;##############################################################################
;#									      #
;#	  	Context Manager Functions Common to mrs's and prs's	      #
;#									      #
;##############################################################################

;***
;InitStruct
;
;Purpose:
;	This function is designed to initialize a structure by filling
;	it partly with UNDEFINED words, and partly with zeroes. The input
;	pointer is to the start of the structure, and the input count
;	is for the number of zeroes to fill in at the beginning of the
;	structure, after first filling the entire structure with UNDEFINED.
;	The result is a structure with the first cbZeroes bytes initialized
;	to zero, the remainder filled with UNDEFINED.
;
;	Made NEAR in CP (moved from RARE) as part of revision [17].
;Entry:
;	ax = pStruct -	pointer to the start of the structure.
;	bx = cbStruct -	count of bytes in the structure
;	cx = cbZeroes -	count of bytes to fill (@ start of struct) w/0
;Exit:
;	none.
;Uses:
;	none.
;Exceptions:
;	none.
;*******************************************************************************
InitStruct PROC	NEAR
	push	ax			;parm to ZeroFill
	push	cx			;parm to ZeroFill

	shr	bx,1			;convert cbStruct to cwStruct
	cCall	FillUndef,<ax,bx>	;fill whole struct with UNDEFINED
	cCall	ZeroFill		;fill first 'cbZeroes' with zeroes
	ret
InitStruct ENDP

;***
;MrsTableEmpty - see if there are any mrs entries
;
;Purpose:
;	This function is called to check to see if there are any active 
;	mrs entries in tRs.
;	We ignore any text mrs's, i.e., if the only mrs's in the table are for 
;	text objects (rather than for modules), say that the table is empty. 
;	Also ignore the global mrs.
;
;	NOTE: it is assumed that this routine will only be called when there
;		is no current entry, i.e., grs.oMrsCur is UNDEFINED. 
;
;Entry:
;	es set to rs table segment
;Exit:
;	PSW.Z set if table empty, clear if it has one or more active entries.
;	if table not empty, bx = oMrs for active entry.
;Uses:
;	none.
;Exceptions:
;	none.
;*******************************************************************************
cProc	MrsTableEmpty,<NEAR>,<ES>
cBegin	MrsTableEmpty
	GETRS_SEG   es,bx,<SIZE,LOAD>	
	mov	bx,OMRS_GLOBAL		; es:bx points to the global mrs
					;  (ignore the global mrs)
	RS_BASE  add,bx 		;add base of tRs to bx
Walk_Loop:
	mov	bx,PTRRS[bx.MRS_oMrsNext] ;advance to next mrs
	inc	bx			;end of chain?
	.errnz	UNDEFINED - 0FFFFH
	jz	Exit_Table_Walk		;  brif so - - - no module mrs's found
	dec	bx

	RS_BASE  add,bx 		;add base of tRs to bx
	test	BPTRRS[bx.MRS_flags2],FM2_Include
	jnz	Walk_Loop		;brif found an include file - ignore
	test	BPTRRS[bx.MRS_flags2],FM2_NoPcode
	jnz	Walk_Loop		;brif active mrs found for a non-file 
					;  mrs (ignore it)
	RS_BASE  sub,bx 		;subtract base of tRs from bx
	or	sp,sp			;clear PSW.Z
Exit_Table_Walk:
cEnd	MrsTableEmpty

;***
;RsTableSearch - search the Rs table
;
;Purpose:
;	This function is shared by mrs-specific and prs-specific code to
;	search for a matching structure entry. 'procType' (ax input value) 
;	indicates whether we're to search the mrs or prs chain; in addition, 
;	it cues us in on additional search logic needed in the case we're to 
;	search for a DEF FN (must then also match oMrs and oTyp).
;	
;	[10] This shared routine is only possible so long as the ogNam fields
;	[10] are in the same place relative to the start of prs & mrs structs,
;	[10] and the ogNam field of an valid prs or mrs entry can never be 
;	[10] UNDEFINED (which is the signal that an entry is active,
;	[10] and is thus a 'hole' in the table).
;	[10] Also, note that a special case (mrs chain only) exists where
;	[10] an "untitled" entry can exist, whose ogNam field will be 
;	[13] OGNAM_UNNAMED.
;
;	[10] Note that the global mrs has an ogNam that can't possibly be a
;	[10] valid ogNam, yet is not UNDEFINED, so no tMrs search should ever 
;	[10] 'find' the global mrs unless it's specifically being looked for
;	[10] but it shouldn't be, since it ALWAYS exists, and is always at 
;	[10] offset 0.
;
;	NOTE: This routine assumes that names are unique - - - a simple
;	NOTE: comparison is done to see if they match, so any differences
;	NOTE: w.r.t. file paths will cause no match to occur. Case Sensitivity
;	NOTE: is, however, ignored in the comparison (i.e., 'a' == 'A' for
;	NOTE: the search).
;
;Entry:
;       al == procType - If this is PT_NOT_PROC, then the mrs chain is searched
;			otherwise, it must be PT_SUB, PT_FUNCTION, or PT_DEFFN.
;			PT_SUB and PT_FUNCTION use the same search logic as
;			PT_NOT_PROC, but for PT_DEFFN, additional matching logic
;			is used.
;       dl == oTyp, IF ax == PT_DEFFN; only used in the case we're searching
;			the prs table for a DEF FN. If ax is not PT_DEFFN,
;			then dl is undefined, and not used in search.
;			Only used for FV_QB4LANG versions.
;	es set to rs table segment (if FV_FAR_RSTBL)
;	ogNam -	Name of entry to search for (ogNam field).
;			If ogNam == OGNAM_UNNAMED, then we know we're searching
;			for an entry (in the mrs chain) whose ogNam entry is 
;			also OGNAM_UNNAMED.
;	NOTE: it is assumed on entry that there is no 'current' entry for
;		the table being searched, i.e., if the prs chain is to be 
;		searched, it is assumed that grs.oPrsCur == UNDEFINED - - - 
;		therefore, callers of this routine should call 
;		Mrs/PrsDeActivate first.
;	NOTE: it is assumed by at least one caller of this routine that it
;		cannot trigger any heap movement.
;Exit:
;	AX == offset into the appropriate table if the search is successful,
;		or UNDEFINED if it is not.
;	BX == if AX is UNDEFINED, then this is an offset into the 
;		appropriate table to the last 'hole', or UNDEFINED if there 
;		are no holes in the table.
;		If, however, AX is a table offset, BX is a pointer to the
;		found entry.
;	ES will be set to the tRs seg (if FV_FAR_RSTBL)
;Uses:
;	none.
;Exceptions:
;	none. 
;*******************************************************************************
cProc	RsTableSearch,<NEAR,NODATA>,<SI,DI>
	parmW	ogNam			
	LocalB	oTyp
	LocalB	procType
	LocalW	oRsFree
cBegin	RsTableSearch
	mov	[oTyp],0
	mov	[procType],al
	mov	di,[ogNam]		
	mov	bx,[oFreePrsFirst]	;assume we're searching prs chain
	cmp	al,PT_NOT_PROC		;Should we search the prs chain?
	jnz	Search_tPRS		;  brif so

	DbAssertRel grs.GRS_oMrsCur,e,UNDEFINED,CP,<grs.oMrsCur not UNDEFINED in RsTableSearch in context.asm>
	mov	si,OMRS_GLOBAL		; offset to the global mrs
	mov	bx,[oFreeMrsFirst]
	jmp	short	Search_Table

Search_tPRS:
	DbAssertRel grs.GRS_oPrsCur,e,UNDEFINED,CP,<grs.oPrsCur not UNDEFINED in RsTableSearch context.asm>
	mov	si,[oPrsFirst]
	cmp	al,PT_DEFFN		;are we searching for a DEF FN?
	jnz	Search_Table		;brif not

	mov	[oTyp],dl		;save type of DEF FN (else oTyp stays 0)
	DbAssertRelB ET_MAX,ae,dl,CP,<RsTableSearch input oTyp not predefined>

Search_Table:
	mov	[oRsFree],bx		;offset to first free struct
	GETRS_SEG   es,bx,<SIZE,LOAD>	
;At this point, [oTyp]=0 for MRS, SUB, FUNCTION, non-zero for DEF FN
;		si is the oRs for the first struct to examine
;	        [procType] = PT_xxx of entry being searched for
;		di = input ogNam
Search_Loop:
	inc	si
	.errnz	UNDEFINED - 0FFFFH
	jz	Quit_Search		;brif end of chain found

	dec	si
	RS_BASE add,si			;add base of table
	DbAssertRel PTRRS[si.MRS_ogNam],nz,UNDEFINED,CP,<RsTableSearch: err 1>
	
	mov	ax,PTRRS[si.MRS_ogNam]
	.errnz	OGNAM_UNNAMED - 0	
	or	di,di			; special case where ogNam = 0?
	jz	Untitled_Mrs		;brif so

	cmp	di,ax
	jnz	Not_A_Match		;brif names don't match
	mov	al,[oTyp]
	or	al,al			;searching for a DEF FN?
	jz	Match_Found		;  brif not

	mov	dx,[grs.GRS_oMrsCur]	;oMrs of the DEF must be oMrsCur ...
	cmp	dx,PTRRS[si.PRS_oMrs]
	jnz	Not_A_Match		;  brif they're not the same
	
	mov	ah,BPTRRS[si.PRS_oType] 
	and	ah,M_PT_OTYPE		; mask out any flags in this byte
	cmp	al,ah			; oTyp's must match if it's a DEF
	jz	Match_Found
Not_A_Match:
	cmp	[procType],PT_NOT_PROC
	jz	MrsSearch		;brif we're walking the mrs chain

	mov	si,PTRRS[si.PRS_oPrsNext] ;move to next entry
	jmp	Search_Loop		;loop to consider next entry
MrsSearch:
	mov	si,PTRRS[si.MRS_oMrsNext] ;move to next entry
	jmp	Search_Loop		;loop to consider next entry

Untitled_Mrs:
	or	ax,ax			;is this the special 'untitled' entry?
	.errnz	OGNAM_UNNAMED - 0	
	jnz	Not_A_Match		;brif not
Match_Found:
	xchg	ax,si			;pointer to found entry
	RS_BASE sub,ax			;pRs --> oRs
	jmp	short Exit_Table_Search	;found entry - exit routine

Quit_Search:
	mov	ax,UNDEFINED		;entry not found
	mov	bx,[oRsFree]		;first free struct of appropriate
					;  size
Exit_Table_Search:
cEnd	RsTableSearch

;***
;PrsFind - find a prs in the tRs
;Purpose:
;	Given the name of a prs, return the oPrs for it, or UNDEFINED if it's
;	not found.
;	Preserve the Mrs & Prs state of the caller.
;
;	NOTE: This should never be called to search for a DEF FN's, only
;	NOTE: a SUB or a FUNCTION.
;
;	For complete details of the matching algorithm, see RsTableSearch.
;Entry:
;	ogNam - name of the prs to search for
;Exit:
;	AX = UNDEFINED if no match found, otherwise it's the oPrs for the
;		desired prs.
;Exceptions:
;	none.
;****
;***
;MrsFind - find an mrs in tRs
;Purpose:
;	Given the name of an mrs, return the oMrs for it, or UNDEFINED if it's
;	not found.
;	Preserve the Mrs & Prs state of the caller.
;
;	For complete details of the matching algorithm, see RsTableSearch.
;Entry:
;	ogNam - name of the mrs to search for
;Exit:
;	AX = UNDEFINED if no match found, otherwise it's the oMrs for the
;		desired mrs.
;Exceptions:
;	none.
;****
cProc	PrsFind,<PUBLIC,FAR,NODATA>
cBegin	<nogen>
	mov	al,PT_SUB		;search for prs's
	SKIP2_PSW			;skip MrsFind, fall into RsFind
cEnd	<nogen>

cProc	MrsFind,<PUBLIC,FAR,NODATA>
cBegin	<nogen>
	mov	al,PT_NOT_PROC		;search for mrs's
cEnd	<nogen>

?DFP = DFP_NONE 			; don't smash regs on entry
cProc	RsFind,<FAR,NODATA>,<si>
	parmW	ogNam			
cBegin
?DFP = DFP_CP				; restore switch
	DbChk	ogNam,ogNam		
	push	ax
	mov	si,[grs.GRS_oRsCur]	;save, so we can restore at exit 
	cCall	MrsDeActivate
	pop	ax
	cCall	RsTableSearch,<ogNam>	
	push	ax			;save retval
	cCall	RsActivateCP,<si>	;reactivate original RsCur
	pop	ax			;restore retval
cEnd

;***
;MoveTheMrs
;
;Purpose:
;	This code is intended to be used by the Mrs[De]Activate routines,
;	to copy a structure from static data into the table or vice versa.
;
;	Changed significantly and renamed (was CopyNchg2Bds) as part of
;	revision [10].
;Entry:
;	si == pointer to the start of the source mrs
;	bx == pointer to the start of the dest. mrs
;	if FV_FAR_RSTBL, ds is source seg, es is dest. seg
;	NOTE: if FV_FAR_RSTBL, one CANNOT assume ds == ss!
;Exit:
;	none.
;Uses:
;	si
;Exceptions:
;	None.
;****
MoveTheMrs	PROC	NEAR	
	push	di
	mov	di,bx				;so pDst is saved across calls

	mov	cx,SIZE MRS			;    cx == cbMrs
	mov	dx,MRS_txd.TXD_bdlText
	call	CopyNchgMrs			;Common code for mrs's and prs's
	mov	ax,[si.MRS_bdlnam.FHD_pNext]	;special case: in case call to
	mov	[di.MRS_bdlnam.FHD_pNext],ax	;  BdlChgOwner in CopyNchgMrs
						;  changed the pNext field
						;  in source bdl
	mov	ax,MRS_bdlnam
	mov	bx,di
	add	bx,ax				;bx = destination pBdlNam 
	add	ax,si				;ax = source pBdlNam
	cCall	BdlChgOwner,<ax,bx>		;NOTE: We MUST call this AFTER
						;  the block copy
	mov	ax,MRS_bdVar

	add	si,ax
	add	di,ax
	cCall	BdChgOwner_NoCopy,<si,di>	;change owner of 2nd bd
						;  copies the mrs
	pop	di
	ret
MoveTheMrs	ENDP

;***
;CopyNchgMrs, CopyNchgPrs
;
;Purpose:
;	This code is intended to be used by the Mrs/Prs[De]Activate routines,
;	for copying a structure from static data into a table or vice versa.
;	In addition, the owner to the bdlText is also updated (via bdmgr)
;	for CopyNchgMrs.
;Entry:
;		si == pointer to the start of the source struct
;		bx == pointer to the start of the dest. struct	
;	For Mrs callers:
;		cx == count of bytes in the structure to copy
;		dx == offset into structure to prs/mrs text bdl
;	For Prs callers:
;	if FV_FAR_RSTBL, ds is source seg, es is dest. seg,
;	    and NOTE that in this case, one CANNOT assume ss == ds
;Exit:
;	none.
;Exceptions:
;	None.
;****
CopyNchgPrs PROC NEAR
	mov	ax,sp
	mov	cx,SIZE PRS - SIZE TXD
	.errnz (SIZE PRS - SIZE TXD) - PRS_txd	;we're counting on the txd
						;  being the last element in
						;  the prs structure
	SKIP2_PSW
CopyNChgMrs:
	sub	ax,ax
	push	di
	push	ss
	pop	es
	xchg	di,ax				;save flag in di

	mov	ax,dx
	add	ax,bx				;points to dest. bdlText
	add	dx,si				;points to source bdlText
	push	dx				;pass to BdlChgOwner
	push	ax				;pass to BdlChgOwner

	push	si				;pass pbSrc to CopyBlk
	push	bx				;pass bpDst to CopyBlk
	push	cx				;pass cbStruct to CopyBlk
	call	CopyBlk
	
	or	di,di
	jnz	CopyNchg_Cont			;brif don't want to move bdl
						;  owner from the table
	cCall	BdlChgOwner			;must do this AFTER block copy
	SKIP2_AX				
CopyNchg_Cont:
	pop	ax				;disgard parms to BdlChgOwner
	pop	ax
	mov	[si.MRS_ogNam],UNDEFINED	; indicate struct not active
	pop	di
	ret
CopyNchgPrs ENDP

;***
;SetMrsUndef, SetMrsAx
;Purpose:
;	Set grs.oMrsCur and grs.oRsCur to [ax] or UNDEFINED
;Entry:
;	SetMrsAx: ax = new grs.oMrsCur
;Preserves:
;	Caller's assume that no registers other than ax are modified
;
;***************************************************************************
SetMrsUndef PROC NEAR
	mov	ax,UNDEFINED
SetMrsUndef ENDP
	;fall into SetMrsAx
SetMrsAx PROC NEAR
	mov	[grs.GRS_oMrsCur],ax
	mov	[grs.GRS_oRsCur],ax
	ret
SetMrsAx ENDP

;***
;SetPrsUndef, SetPrsAx
;Purpose:
;	Set grs.oPrsCur to [ax] or UNDEFINED, keeping grs.oRsCur consistent.
;Entry:
;	SetPrsAx: ax = new grs.oPrsCur
;Preserves:
;	Caller's assume that no registers other than ax are modified
;
;***************************************************************************
SetPrsUndef PROC NEAR
	mov	ax,UNDEFINED
SetPrsUndef ENDP
	;fall into SetPrsAx
SetPrsAx PROC NEAR
	mov	[grs.GRS_oPrsCur],ax
	inc	ax			;test for UNDEFINED
	jz	NoPrs			;brif no prs is active
	dec	ax
	or	ah,80h			;high bit indicates prs active
	mov	[grs.GRS_oRsCur],ax
	ret
NoPrs:
	mov	ax,[grs.GRS_oMrsCur]
	mov	[grs.GRS_oRsCur],ax	;oRsCur = oMrsCur (no prs active)
	ret
SetPrsAx ENDP


;##############################################################################
;#									      #
;#		  Module related Context Manager Functions		      #
;#									      #
;##############################################################################

;***
;MrsDiscard()
;
;Purpose:
;     Release all resources associated with current module. This includes the
;     module's type table, static value table, and text table.
;     This routine calls DiscardPrs for every procedure in this module.
;
;     TxtDiscard is also called (prior to setting grs.oMrsCur to UNDEFINED).
;
;Entry:
;     static bit flag F_CON_LeaveTMrsEmpty in conFlags - - - if we're 
;		discarding the last mrs and this bit flag is set (TRUE), just 
;		leave no mrs in tRs, otherwise, call MrsMake to create a
;		new unnamed module.
;Exit:
;     AX = non-zero
;	   Special case: AX = UNDEFINED if MakeInitMrs is called. [47]
;     grs.oMrsCur = grs.oPrsCur = UNDEFINED
;Uses:
;	none.
;Exceptions:
;	if the module has been modified and user cancels, triggers a runtime
;	error.
;*******************************************************************************
cProc	MrsDiscard,<PUBLIC,FAR,NODATA>
cBegin	MrsDiscard
	DbAssertRel  mrsCur.MRS_ogNam,nz,OGNAM_GMRS,CP,<MrsDiscard: global mrs!>
	DbAssertRel  mrsCur.MRS_ogNam,nz,OGNAM_CLIPBOARD,CP,<MrsDiscard: err 1>
	DbAssertRel  mrsCur.MRS_ogNam,nz,OGNAM_IMMEDIATE,CP,<MrsDiscard: err 2>

	cCall	PrsDeActivate
	call	AlphaORsFree			;release table of sorted oRs's
	call	VarDealloc			;deallocate any owners in
						; the module variable table

	cCall	DiscardHistoryORs,<grs.GRS_oMrsCur> ; throw away help
						; history for mrs.

	;Note that the below scheme depends on the fact that NextPrsInMrs
	;finds the first prs in the table if grs.GRS_oPrsCur is UNDEFINED, and
	;then subsequent prs's based on grs.GRS_oPrsCur.  It is not safe to
	;call ForEachCP to do this, because that scheme depends on walking
	;the prs chain, and PrsDiscard1 might discard the current entry.
	;In essense we are starting from the top of the prs chain each time
	;through the loop below.
	; To further complicate things, PrsDiscard1 might or might not
	; free the prs; if it does not, the below still works, but we
	; walk from prs to prs, not freeing them, but marking them as
	; undefined proc.s and freeing associated resources.
PrsDiscard_Loop:
	call	far ptr NextPrsInMrs		;activate next prs in this mrs
	inc	ax				;no more prs's in this module?
	jz	MrsDiscard_Cont 		;  brif so
	call	PrsDiscard1			;discard active prs it
	jmp	short PrsDiscard_Loop
MrsDiscard_Cont:
	cCall	PrsDeActivate			; ensure TxtDeleteAll
						; deletes txt in MODULE
						; text table, so any
						; declares to proc.s in
						; this module are removed
						; prior to calling
						; ChkAllUndefPrsSaveRs

	call	TxtDeleteAll			;delete all text in txt table,
						; but don't free bdl yet
	call	ChkAllUndefPrsSaveRs		;search for new defining
						;references for Prs entries
						;which had their "defining"
						;reference deleted.
	cCall	TxtDiscard			;discard current text table

	PUSHI	ax,<dataOFFSET mrsCur.MRS_bdVar>
	cCall	BdFree

	PUSHI	ax,<dataOFFSET mrsCur.MRS_bdlNam>
	call	BdlFree				;free module name table

	test	[mrsCur.mrs_flags2],FM2_NoPcode ; is a document mrs?
	jz	NoDocBuffer			; brif not
	push	[mrsCur.MRS_pDocumentBuf]	; document table to delete
	call	FreeBuf				; Free the buffer
NoDocBuffer:					

	;don't need to modify entry in table - - - without an active mrs,
	;  it will automatically look like a 'hole'.

	mov	ax,[grs.GRS_oMrsCur]
	cmp	ax,[grs.GRS_oMrsMain]		;discarding Main module?
	jnz	Not_Main_Module			;brif not

	mov	[grs.GRS_oMrsMain],UNDEFINED	;now there is no current Main
	jmp	SHORT UndefCur

Not_Main_Module:
	call	MainModified			;mark main module as modified
						; if current mrs is pcode
						; mrs (so we will be sure
						; to save updated xxx.MAK)
						; (preserves ax)
UndefCur:
	push	ax
	call	SetMrsUndef			;grs.oMrsCur=oRsCur=UNDEFINED
	pop	ax				;restore ax = oMrsCur

	;now unlink this entry from the mrs chain and link it into the free
	;  mrs chain
	GETRS_SEG   es,bx,<SIZE,LOAD>		
	mov	bx,ax				;ax = bx = oMrs we're unlinking
	RS_BASE add,bx				;PTRRS:bx = pMrs we're unlinking
	mov	cx,ax
	xchg	cx,[oFreeMrsFirst]		;cx = offset to first free mrs,
						;  this mrs is new first free
	xchg	PTRRS[bx.MRS_oMrsNext],cx	;mrs now linked into free list
	;now, cx is the offset to the next mrs in the active chain; use
	;  that to unlink this mrs from the active chain
	mov	bx,OMRS_GLOBAL			; start from global mrs
UnLink_Loop:
	RS_BASE add,bx
	mov	dx,PTRRS[bx.MRS_oMrsNext]	;offset to next mrs in chain
	cmp	dx,ax				;found previous chain entry?
	jz	UnLinkIt			;  brif so

	mov	bx,dx				;advance to next entry
	jmp	UnLink_Loop

UnLinkIt:
	mov	PTRRS[bx.MRS_oMrsNext],cx	;unlink the discarded mrs

	push	sp				; Default return value
						; Assumes:
						;	 sp != 0
						;	 sp != UNDEFINED

	test	[conFlags],F_CON_LeaveTMrsEmpty ;should we just leave tMrs
						;  empty if this was only mrs?
	jnz	MrsDiscard_Exit			;brif so

Mrs_No_Trim:
	push	ax				;pass oMrsCur to WnReAssign

	call	MrsTableEmpty			;PSW.Z clear if no mrs's
						; bx = remaining oMrs (if any)
	push	bx				;pass oMrsNew to WnReAssign
	jnz	MrsDiscard_Exit1		;brif mrs table not empty

	call	far ptr MakeInitMrs		;make initial mrs
						;  (certain it won't run out
						;   of memory here ...)
	DbAssertRel  ax,z,0,CP,<MrsDiscard: MakeInitMrs returned an error code>	
	pop	bx				;discard invalid oMrsNew
	pop	dx				; Get oMrsOld
	pop	ax				; discard default ret val
	pushi	ax, UNDEFINED			; return UNDEFINED to
						; indicate MakeInitMrs called
	push	dx				; Put oMrsOld back
	push	[grs.GRS_oMrsCur]		;pass oMrsNew to WnReAssign

	cCall	MrsDeActivate			;don't want it active
	
MrsDiscard_Exit1:
	PUSHI	ax,0				;not just renaming an oRs
	call	WnReAssign			;parms already pushed

MrsDiscard_Exit:
	pop	ax				; Get return value
MrsDiscard_Exit2:				
cEnd	MrsDiscard

;***
;UnlinkFreeMrs
;Purpose:
;	Given an oMrs, search the free mrs chain for the oMrs and unlink it
;	from the chain if found.
;Input:
;	ax = oMrs to be unlinked from the free list
;Exit:
;	none
;******************************************************************************
UnlinkFreeMrs  PROC    NEAR
	GETRS_SEG   es,bx,<SIZE,LOAD>		; prepare for link work
	mov	dx,[oFreeMrsFirst]
	inc	dx
	jz	UnlinkEmpty_Done		;free chain is empty

	dec	dx
	cmp	ax,dx
	jnz	Unlink_Unnamed_Entry		;brif unnamed hole not first free

	mov	bx,dx
	RS_BASE add,bx
	mov	dx,PTRRS[bx.MRS_oMrsNext]
	mov	[oFreeMrsFirst],dx		;unlink complete
	jmp	short UnlinkEmpty_Done
Unlink_Unnamed_Entry:
	mov	bx,dx
	RS_BASE add,bx
	mov	dx,PTRRS[bx.MRS_oMrsNext]
	inc	dx
	jz	UnlinkEmpty_Done		;unnamed mrs hole not in free list

	dec	dx
	cmp	ax,dx
	jnz	Unlink_Unnamed_Entry
	xchg	dx,bx
	RS_BASE add,bx
	mov	cx,PTRRS[bx.MRS_oMrsNext]
	xchg	dx,bx
	mov	PTRRS[bx.MRS_oMrsNext],cx	;unlink complete
UnlinkEmpty_Done:
	ret
UnlinkFreeMrs  ENDP

;***
;MrsMake(ogNam, flags)
;
;Purpose:
;     The user-interface is the only QBI component which deals in any way with
;     module names. A module's name is the same as the path name from which it
;     was loaded. This function searches for a module by the name 'ogNam'. If
;     found, an error code is returned (ER_DD).  If not found, it creates
;     a new Mrs entry and names it 'ogNam'.  Calls the TextMgr routine
;     TxtCurInit() to initialize the text table. Calls the Variable Manager's
;     MakeMrsTVar() if this mrs is not for a text object.
;
;	Note that if we're making a named mrs and there exists an empty unnamed
;	mrs (which can only be at a specific offset from the start of the 
;	table), we'll just discard the empty unnamed one, and use that as a 
;	hole to put the new mrs in. 
;
;	Note also that, if there is no MAIN module on successful conclusion of
;	this routine, the new current module will be made the MAIN one.
;
;Entry:
;     ogNam:
;	 If module's name is to be 'Untitled',
;	    ogNam = OGNAM_UNNAMED
;	 else if module is the global module (created only once)
;           ogNam = OGNAM_GMRS
;	 else if module is for some other special purpose
;		ogNam <= OGNAM_PSEUDO_MAX
;        else
;	    ogNam is an offset into the global name table for this module name
;     flags:
;	low byte = initial value of MRS_flags
;	high byte = initial value of MRS_flags2
;	  Command Window:  FM2_NoPcode
;	  ClipBoard:       FM2_NoPcode
;	  Module:          FM2_File
;	  Include File:    FM2_File + FM2_Include
;	  Document File:   FM2_File + FM2_NoPcode
;	  If either FM2_Include or FM2_NoPcode are set:
;	    - we must NOT replace the existing first module even if it's empty
;	    - this mrs can never be made the MAIN module
;	    - it can never have oMrs of 0
;	    - this mrs can never have a variable table (or be scanned to
;	      SS_PARSE or SS_EXECUTE)
;Exit:
;        if no error 
;	    AX = 0
;	    grs.oMrsCur = new module's oMrs
;	    grs.oPrsCur = UNDEFINED
;	    all fields in mrsCur are setup
;	    all fields in txdCur are setup
;	    rsNew = module's oMrs (tell's user interface to show list window)
;	 else
;	    caller can count on the input mrsCur being still active
;	    if out of memory error, AX = ER_OM
;	    if a matching module is found, AX = ER_DD and [oRsDupErr] =
;	       oRs of existing module with same name.
;Uses:
;	none.
;Exceptions:
;	none.
;*******************************************************************************
cProc	MrsMake,<PUBLIC,FAR,NODATA>,<SI,DI>
	parmW	ogNam				
	parmW	flags
	localB	fUsingUnNamed_Mrs
	localB	fGrewRsTable			
cBegin	MrsMake
	mov	word ptr [fGrewRsTable],FALSE	; initialize both
						; fUsingUnNamed_Mrs and
						; fGrewRsTable with one mov
	mov	di,[grs.GRS_oMrsCur]		; save grs.oMrsCur
	cmp	[ogNam],OGNAM_GMRS		; making global mrs?
	DJMP	jz  Make_gMrs			;	brif so - special case
	call	AlphaORsFree			;release table of sorted oRs's
	cCall	MrsDeActivate			;save existing mrs, empty mrsCur

	mov	al,PT_NOT_PROC			;signal to search tMrs, not tPrs
	cCall	RsTableSearch,<ogNam>		; returns oMrs if entry 
						;   found, or
	mov	dx,UNDEFINED
	cmp	ax,dx				;   UNDEFINED if not found
						;   also puts oHole in bx
	jnz	Make_Mrs_DD_ER			;brif entry was found

	inc	dx				;now dx == 0
	.errnz	OGNAM_UNNAMED - 0
	mov	cx,[ogNam]			
	cmp	cx,dx				; making an unnamed mrs?
	jz	Make_Unnamed_Mrs		;  brif so

	cmp	cx,OGNAM_PSEUDO_MAX		; some other special ogNam?
	jbe	Make_Mrs			;   brif so

	DbChk	ogNam,ogNam			
	test	byte ptr ([flags+1]),FM2_NoPcode OR FM2_Include	
						;is this mrs for a text object?
	jnz	Make_Mrs			;  brif so; keep unnamed mrs

	call	EmptyMrs			;see if there's an empty
						;  unnamed mrs
	jcxz	Make_Mrs			; brif there isn't one
	jmp	short Use_Empty_Mrs		

Make_Mrs_DD_ER:
	mov	[oRsDupErr],ax			;tell caller oRs of conflict
	mov	ax,ER_DD
	jmp	MrsMake_Err_Exit1		;reactivate input mrsCur & exit

Use_Empty_Mrs:
	mov	si,SIZE MRS + OMRS_GLOBAL	; oMrs for unnamed entry
	push	si				;parm to MrsActivateCP
	call	MrsActivateCP

	or	[conFlags],F_CON_LeaveTMrsEmpty ;so MrsDiscard won't create a
						;  new module after discarding!
	cCall	MrsDiscard			;discard empty unnamed mrs
	and	[conFlags],NOT F_CON_LeaveTMrsEmpty ;reset to default
	mov	[fUsingUnNamed_Mrs],TRUE	;in case of later OM error
	mov	ax,si				;oMrs to unlink from free list
	call	UnlinkFreeMrs			;unlink the entry from the
						;  free list, (MrsDiscard
						;  linked it in)
	jmp	SHORT Init_New_Mrs1		

Make_Text_Mrs:
	or	bx,bx
	jnz	Make_Mrs

	dec	bx				;if making a text mrs, CANNOT
						;  allow it to go in @ offset 0
	jmp	short Make_Mrs

Make_Unnamed_Mrs:
	mov	ax,SIZE MRS + OMRS_GLOBAL	;offset that empty unnamed mrs
						;  MUST go at (parm to UnlinkFree Mrs)
	mov	si,ax
	call	UnlinkFreeMrs			;unlink the entry from the
	mov	bx,si				; offset that empty unnamed
						;   mrs MUST go at
	cmp	bx,[grs.GRS_bdRs.BD_cbLogical]	;is the table empty?
	jz	Make_Mrs_Grow
Init_New_Mrs1:					
	jmp	short Init_New_Mrs
Make_gMrs:
	mov	si,OMRS_GLOBAL			; new oMrs
	PUSHI	ax,<dataOFFSET grs.GRS_bdRs>	
	PUSHI	ax,<SIZE MRS + OMRS_GLOBAL>	
	call	BdGrow				
	jmp	short Make_Mrs_Grow_1		
Make_Mrs:
	mov	si,bx				;save hole location
	cmp	si,UNDEFINED			;is there a hole in table?
	jz	Make_Mrs_Grow			;  brif not

	DbAssertRel bx,z,oFreeMrsFirst,CP,<MrsMake: given hole not first free>
	RS_BASE add,bx
	mov	bx,PTRRS[bx.MRS_oMrsNext]
	mov	[oFreeMrsFirst],bx		;free entry now unlinked from
						;  free mrs chain
	GETRS_SEG   es,bx,<SIZE,LOAD>		
	jmp	short Init_New_Mrs
Make_Mrs_Grow:
	mov	si,[grs.GRS_bdRs.BD_cbLogical]	;new oMrs
	or	si,si				; Rs table > 32k?
	js	J_MrsMake_OM_Err		; brif so ... give OM error

	PUSHI	ax,<dataOFFSET grs.GRS_bdRs>
	PUSHI	ax,<SIZE MRS>
	call	BdGrow
Make_Mrs_Grow_1:				
	or	ax,ax
	jnz	@F				; brif successful
J_MrsMake_OM_Err:				
	jmp	MrsMake_OM_Err
@@:						
	mov	[fGrewRsTable],TRUE		; remember we grew in case
						; later error
Init_New_Mrs:
	mov	ax,dataOFFSET mrsCur
	mov	bx,SIZE MRS
	mov	cx,MRS_CB_ZERO_INIT
	call	InitStruct
	mov	[mrsCur.MRS_cbFrameVars],-FR_FirstVar	;	
						; reset to init. value. This
						; value is 2 to account for
						; the fact that b$curframe 
						; is always pushed on the
						; stack after bp, so we
						; treat this word as a frame
						; var for ref'ing the real
						; frame vars off of bp
	mov	ax,[flags]
	mov	[mrsCur.MRS_flags],al
	mov	[mrsCur.MRS_flags2],ah
	mov	ax,si
	call	SetMrsAx			;grs.oMrsCur = oRsCur = ax
	GETRS_SEG   es,bx,<SIZE,LOAD>		
	mov	bx,si				;oMrsNew
	RS_BASE add,bx				;add base of tRs to bx
	mov	PTRRS[bx.MRS_ogNam],UNDEFINED	;mark entry as inactive, since
						;  mrsCur has actual mrs
	mov	PTRRS[bx.MRS_oMrsNext],UNDEFINED ;in case someone tries to walk
						 ;  the mrs chain before this
						 ;  mrs gets deactivated
	cCall	TxtCurInit			;init txdCur
	jnz	no_error
	jmp	MrsMake_TxtInit_Err
no_error:

	test	byte ptr ([flags+1]),FM2_NoPcode; is a document mrs?
	jz	MakeVarTables			; brif not, create tables
	call	NewBuf				; allocate document buffer
	mov	WORD PTR [mrsCur.MRS_pDocumentBuf],ax ; set ptr to buffer
	or	ax,ax				; was it successful?
	mov	al,ER_OM			; assume not, set error
	jz	MrsMake_OM_2_Err		; brif out of memory
	jmp	SHORT MrsMake_Name		; finish creating txt tbl

MakeVarTables:

	cCall	MakeMrsTVar			; set up module var table
	or	ax,ax
	jz	MrsMake_OM_2_Err

	mov	ax,SbGNam			; assume global mrs
	cmp	[ogNam],OGNAM_GMRS		; making global mrs?
	jz	MrsMake_TNam			;	brif so

	sub	ax,ax				; don't use special sb
MrsMake_TNam:					
	cCall	TNamInit,<ax>			; set up module name table
	or	ax,ax
	jz	MrsMake_OM_3_Err

MrsMake_Name:
	mov	ax,[ogNam]			
	mov	[mrsCur.MRS_ogNam],ax		; set module name
	mov	[mrsCur.MRS_oMrsNext],UNDEFINED 
	.errnz	1 - OGNAM_GMRS			
	dec	ax				; is this the global module?
	jz	MrsMake_Exit			;  brif so - exit (this
						;      mrs doesn't have to be
						;      linked, as it's known
						;      to be at offset 0)
	;After this point, no errors can occur, so its ok to tell
	;user interface the oRs of the new module
	
	xchg	ax,dx
	mov	ax,[grs.GRS_oMrsCur]
	mov	[rsNew],ax			;tell user interface to
						; show new mrs in list window
Link_Mrs:
	;Now, link this module into mrs chain
	GETRS_SEG   es,bx,<SIZE,LOAD>		
	mov	bx,OMRS_GLOBAL			;start with global mrs
MrsLink_Loop:
	RS_BASE add,bx
	mov	dx,PTRRS[bx.MRS_oMrsNext]
	inc	dx
	.errnz	UNDEFINED - 0FFFFH
	jz	GotChainEnd			;brif bx points to last current
						;  entry in the mrs chain
	dec	dx
	mov	bx,dx
	jmp	MrsLink_Loop
GotChainEnd:
	mov	PTRRS[bx.MRS_oMrsNext],ax	;link new entry at end of chain
	;Note that the oMrsNext field in the new entry will automatically be
	;UNDEFINED in the new entry, and we set it to UNDEFINED earlier in
	;this routine

	xor	ax,ax
	test	byte ptr ([flags+1]),FM2_NoPcode OR FM2_Include	
						;is this mrs for a text or
						;  INCLUDE object?
	jnz	Main_Set2			;  brif so - IT can't be MAIN

	dec	ax
	.errnz	UNDEFINED - 0FFFFh
	cmp	[grs.GRS_oMrsMain],ax		;is there already a MAIN module?
	jz	Main_Set0			;  brif not
	call	MainModified			;mark main module as modified
						; if current mrs is pcode
						; mrs (so we will be sure
						; to save updated xxx.MAK)
	jmp	SHORT Main_Set1

Main_Set0:
	mov	dx,[grs.GRS_oMrsCur]
	mov	[grs.GRS_oMrsMain],dx		;this mrs gets set as MAIN
Main_Set1:
	inc	ax				;inc ax back to 0
Main_Set2:
	.errnz	FALSE
MrsMake_Exit:
cEnd	MrsMake

MrsMake_OM_3_Err:
	mov	bx,dataOFFSET mrsCur.MRS_bdVar	
	cCall	BdFree,<bx>			
	mov	bx,dataOFFSET mrsCur.MRS_bdlNam 
	cCall	BdlFree,<bx>			
MrsMake_OM_2_Err:
	call	TxtDiscard
MrsMake_TxtInit_Err:
	cmp	[fGrewRsTable],FALSE		; did we grow Rs table?
	jz	@F				; brif not
	sub	[grs.GRS_bdRs.BD_cbLogical],SIZE MRS
@@:						
MrsMake_OM_Err: ;Tried to grow the Rs table but failed
	call	SetMrsUndef			;grs.oMrsCur=oRsCur=UNDEFINED
	cmp	[fUsingUnNamed_Mrs],FALSE	;special case where we tried
						;  to reuse unnamed mrs?
	jz	Not_Unnamed			;  brif not

	cmp	di,SIZE MRS + OMRS_GLOBAL	; was the unnamed one the
						;  active one on entry?
	jnz	Not_Unnamed			;  brif not

	;can't reactivate the one that was active on input, as we've 
	;  tried to reuse that one but failed. For this special case, we
	;  must recurse to restore the unnamed empty mrs
	call	far ptr MakeInitMrs		;remake empty unnamed mrs
						;  (certain it won't run out
						;   of memory here ...)
	DbAssertRel  ax,z,0,CP,<MrsMake: MakeInitMrs returned an error code>	
	DbAssertRel  di,z,grs.GRS_oMrsCur,CP,<MrsMake: di not equal to oMrsCur>
Not_Unnamed:
	mov	ax,ER_OM
MrsMake_Err_Exit1:
	push	ax				;save error code
	push	di				;input oMrsCur 
	call	MrsActivateCP			;re-activate original mrs
	pop	ax				;restore error code for retval
	jmp	MrsMake_Exit

;***
; MakeInitMrs
; Purpose:
;	Make an unnamed module mrs.
; Entry:
;	none
; Exit:
;	ax = error code
;
;*******************************************************************************
cProc	MakeInitMrs,<FAR,NODATA>
cBegin	MakeInitMrs
	xor	ax,ax			; set to OGNAM_UNNAMED
	.errnz	OGNAM_UNNAMED - 0	
	push	ax			;make untitled module
	PUSHI	ax,<100h * FM2_File>	;initial flags for module mrs
	call	MrsMake			;make initial (untitled) mrs 
cEnd	MakeInitMrs
;***
; MainModified
; Purpose:
;	Mark main module as modified if current mrs is pcode mrs.
;	This is done so we will be sure to save updated xxx.MAK)
; Preserves:
;	ax (caller's assume this)
;	es is either preserved, or explicitly set to seg of tRs
;
;*******************************************************************************
PUBLIC	MainModified	;for debugging only
MainModified PROC NEAR
DbAssertRel [grs.GRS_oMrsMain],ne,[grs.GRS_oMrsCur],CP,<MainModified: err1>
	mov	bx,[grs.GRS_oMrsMain]
	inc	bx			;test for UNDEFINED
	jz	MmExit			;brif no main module
					; (can happen when we're terminating
					;  and main module discarded before
					;  command window's mrs)
	test	[mrsCur.MRS_flags2],FM2_NoPcode OR FM2_Include
	jnz	MmExit			;brif current mrs is not a pcode mrs

	dec	bx
	RS_BASE add,bx			;bx now points to main mrs
					; (we know mrs oMrsMain is in table,
					;  i.e. not in mrsCur struct)
	or	BPTRRS[bx.MRS_flags2],FM2_Modified
	GETRS_SEG   es,bx,<SIZE,LOAD>	
MmExit:
	ret
MainModified ENDP

;***
;MrsDeActivate()
;
;Purpose:
;     Save the	current module's register set (mrsCur) back into the tRs
;     entry. This includes a call to TxtDeActivate prior to actually
;     deactivating the mrs, as well as deactivating the current procedure 
;     (via PrsDeActivate).
;
;     NOTE: This routine is guaranteed to cause no heap movement.
;
;Entry:
;	 grs.oMrsCur = current module to be deactivated
;	   UNDEFINED if there is no current module
;Exit:
;	 grs.oMrsCur = UNDEFINED
;	 grs.oPrsCur = UNDEFINED
;Uses:
;	none.
;Exceptions:
;	none.
;*******************************************************************************
cProc	MrsDeActivate,<PUBLIC,NEAR,NODATA>,<SI>
cBegin	MrsDeActivate
	cmp	[grs.GRS_oMrsCur],UNDEFINED	;is there an active mrs?
	jz	MrsDeAct_Exit			;just return if not

	DbHeapMoveOff				;assert no heap movement here
	DbChk	ConStatStructs
	cCall	PrsDeActivate			;deactivate current prs, if any

	cCall	TxtDeActivate			;deactivate current txd, if any

	mov	bx,[grs.GRS_oMrsCur]
	RS_BASE add,bx
	GETRS_SEG   es,si,<SIZE,LOAD>		; es set to mrs dst. seg
	mov	si,dataOFFSET mrsCur		;now ds:si == mrs src. address
	call	MoveTheMrs			; move mrsCur into tRs

	call	SetMrsUndef			;grs.oMrsCur=oRsCur=UNDEFINED
	DbHeapMoveOn
MrsDeAct_Exit:
cEnd	MrsDeActivate

;***
;MrsActivate(oMrsNew)
;
;Purpose:
;     Make the module indicated by oMrsNew the current active module. The
;     current module's register set (if any) will be saved via MrsDeActivate, 
;     and TxtActivate will be called at the end.
;
;     NOTE: This routine is guaranteed to cause no heap movement.
;
;Entry:
;	 'oMrsNew' is an offset into the tRs for module to be made current.
;	Note that if oMrsNew is UNDEFINED or if the entry at offset oMrsNew is
;		a discarded entry, the current mrs (if any) will be
;		deactivated, and no new mrs will be activated.
;Exit:
;	 grs.oMrsCur = oMrsNew
;	 grs.oPrsCur = UNDEFINED
;	 if oMrsNew != UNDEFINED,
;		all fields in mrsCur are setup
;		all fields in txdCur are setup
;Uses:
;	none.
;Exceptions:
;	none.
;     
;*******************************************************************************
cProc	MrsActivate,<PUBLIC,FAR,NODATA>
	parmW	oMrsNew
cBegin	MrsActivate
	cCall	MrsActivateCP,<oMrsNew>
cEnd	MrsActivate

cProc	MrsActivateCP,<PUBLIC,NEAR,NODATA>,<SI>
	parmW	oMrsNew
cBegin	MrsActivateCP

	DbHeapMoveOff				;assert no heap movement here
	call	PrsDeActivate
	mov	ax,[oMrsNew]
	cmp	ax,[grs.GRS_oMrsCur]
	jz	MrsActivate_Exit

	push	ax				;save oMrsNew
	cCall	MrsDeActivate			;deactivate current mrs, if any
	pop	ax				;ax = oMrsNew
	inc	ax				;test for UNDEFINED
	jz	MrsActivate_Exit		;brif no mrs is to be activated
	dec	ax				;restore ax = oMrsNew

	DbChk	oMrs,ax 			;ife RELEASE - ensure validity
	xchg	ax,si				;si = oMrs to activate
	RS_BASE add,si				;si = pMrs to activate
	GETRS_SEG   ds,bx,<SIZE,LOAD>		; ds set to mrs src. seg
	assumes DS,NOTHING
	mov	bx,dataOFFSET mrsCur		;bx == mrs dest. address
	SETSEG_EQ_SS	es			;es:bx = mrs dest. address
	call	MoveTheMrs			; move mrsCur into tRs
	SETSEG_EQ_SS	ds			;restore ds = ss
	assumes DS,DATA

	mov	ax,[oMrsNew]			;note new oMrsCur
	call	SetMrsAx			;grs.oMrsCur = oRsCur = ax

	cCall	TxtActivate			;activate txdCur from this mrs
MrsActivate_Exit:
	DbHeapMoveOn
cEnd	MrsActivate

;***
;EmptyMrs
;Purpose:
;	This boolean function is used to determine whether there is an
;	empty unnamed module.
;	[52] NOTE: more than one caller of this routine assumes that if
;	[52]	   there IS an empty unnamed mrs, then there is no other
;	[52]	   (pcode) mrs loaded.
;Entry:
;	none.
;Exit:
;	CX = 0 if there is not an empty unnamed module
;	else CX is the offset in the Rs table to the empty unnamed module
;Uses:
;	none.
;Preserves:
;	BX
;Exceptions:
;	none.
;*******************************************************************************
PUBLIC	EmptyMrs
EmptyMrs	PROC	NEAR
	push	bx
	;---------------------------------------------------------------------
	;NOTE: an inherent assumption is that, if there exists an unnamed mrs,
	;	it will be at offset SIZE MRS + OMRS_GLOBAL in the table (the
	;	global mrs is at offset OMRSGLOBAL in the table). The user
	;	cannot create an unnamed mrs, so this seems a safe assumption.
	;---------------------------------------------------------------------
	mov	cx,SIZE MRS + OMRS_GLOBAL   ;[60]
	mov	bx,OFFSET DGROUP:mrsCur	;assume this is the current module
	mov	ax,[txdCur.TXD_bdlText_cbLogical]
	SETSEG_EQ_SS	es		;seg of mrsCur is always same as ss
	cmp	[grs.GRS_oMrsCur],cx	;is this the current module?
	jz	EmptyMrs_Cont		;  brif so

	GETRS_SEG   es,bx,<SIZE,LOAD>	
	mov	bx,cx			;bx = oMrs
	RS_BASE add,bx			;bx = pMrs
	mov	ax,PTRRS[bx.MRS_txd.TXD_bdlText_cbLogical]
EmptyMrs_Cont:
	cmp	PTRRS[bx.MRS_ogNam],OGNAM_UNNAMED  ;[10] is MAIN mrs unnamed?
	jnz	EmptyMrs_FalseExit	;  brif not

	cmp	ax,CB_EMPTY_TEXT	;is it's text table empty?
	ja	EmptyMrs_FalseExit	;  brif not

	;This block checks to see if module has any procedures
	mov	ax,UNDEFINED
	mov	bx,cx			;bx == oMrs of interest for call below
	DbAssertRel grs.GRS_oPrsCur,z,UNDEFINED,CP,<EmptyMrs: prsCur active>
	call	GetNextPrsInMrs		;ax = oPrs of 1st prs in module
	js	EmptyMrs_Exit		; brif module contains no prs's

EmptyMrs_FalseExit:
	xor	cx,cx			; indicate no empty unnamed mrs
EmptyMrs_Exit:
	pop	bx
	ret
EmptyMrs	ENDP


;##############################################################################
;#									      #
;#		  Procedure related Context Manager Functions		      #
;#									      #
;##############################################################################

;------------------------------------------------------------------------------
;
;                           Prs Transition States
;
; Due to the user interface, prs handling is more complicated than mrs
; handling. For a Module Register Set, there are just 2 states: no mrs, or
; an mrs, with two arcs connecting the states: MrsMake, and MrsDiscard.
;
; Procedure Register Sets have the following states and transitions:
;
;       S0 - No prs entry
;       S1 - SUB has been referenced, but not declared or defined
;            This only applies to SUBs, not FUNCTIONS or DEFFNs.
;            (i.e. FUNCTIONS and DEF FNs never enter this state)
;            If a prs entry is still in this state at runtime,
;            We bind to a compiled SUB on first execution.
;       S2 - procedure has no text tbl, has been DECLAREd but not defined.
;            If a prs entry is still in this state at runtime,
;            We bind to a compiled SUB/FUNCTION on first execution.
;            The procedure may or may not have declarations.
;            (DEF FNs never enter this state)
;       S3 - procedure text tbl & window allocated, but no definition.
;            The procedure may or may not have declarations.
;            (DEF FNs never enter this state)
;       S4 - procedure has a definition (i.e. SUB/FUNCTION/DEF FN stmt)
;
; These states can be uniquely determined by these fields in the prs structure:
;		(But note: txd.bdlText.status should only be tested while the
;		 prs is not active, i.e., in the tPrs. To see if prsCur has
;		 a text table, test txdCur.TXD_flags - - if FTX_mrs is set,
;		 prsCur has no text table)
;       S0:  bdName.pb == NULL
;       S1:  bdName.pb != NULL, txd.bdlText.status == NOT_OWNER,
;            FP_DECLARED and FP_DEFINED are NOT set,
;            (mrs,prs,otx refer to the CALL reference)
;       S2:  bdName.pb != NULL, txd.bdlText.status == NOT_OWNER
;            FP_DECLARED IS set, FP_DEFINED is NOT set,
;            (mrs,prs,otx refer to DECLARE stmt's pcode),
;       S3:  bdName.pb != NULL, txd.bdlText.status != NOT_OWNER,
;            FP_DEFINED is NOT set,
;       S4:  bdName.pb != NULL, txd.bdlText.status != NOT_OWNER for SUB/FUNC
;            but is UNDEFINED for DEF FNs, FP_DEFINED is set, The setting of
;            FP_DECLARED is not important in this state.
;            (mrs,prs,otx refer to SUB/FUNCTION/ DEF FN statement's pcode)
;
; With these states defined, and arcs connecting them being functions
; defined below, the following table shows how the arcs connect the states:
; In this table, PrsDefine refers to when PrsDefine is called with fNotDefine=0,
; i.e. for a SUB/FUNCTION/DEF FN statement.
; PrsDeclare refers to when PrsDefine is called with fNotDefine != 0,
; i.e. for a DECLARE SUB/FUNCTION/DEF FN statement.
;
;	Initial State	  Arc       Final State
;	-------------	  ---       -----------
;	    S0		PrsRef(FUNC/DEF)S0
;	    S0		PrsRef(SUB)	S1
;           S0          PrsDeclare      S2
;	    S0		PrsMake		S3
;	    S0		PrsDefine	S4
;
;           S1          <txt mgr sees defining reference deleted and:
;                        finds another SUB reference -> S1
;                        finds no other references -> PrsFree -> S0
;	    S1		PrsRef(SUB)	S1
;           S1          PrsDeclare      S2
;	    S1		PrsMake		S3
;	    S1		PrsDefine	S4
;
;           S2          <txt mgr sees defining DECLARE stmt deleted and:
;                        finds another DECLARE -> S2
;                        finds another SUB reference -> S1
;                        finds no other declares or references -> PrsFree -> S0
;	    S2		PrsRef          S2
;	    S2          PrsDeclare      S2
;	    S2          PrsMake		S3
;	    S2          PrsDefine       S4
;
;	    S3		PrsRef		S3
;	    S3		PrsMake		S3
;	    S3		PrsDeclare	S3
;	    S3		PrsDefine	S4
;           S3		<txt mgr sees text table and window removed and:
;			 finds a SUB reference -> S1
;			 finds no SUB reference -> PrsFree -> S0
;
;	    S4		PrsRef		S4
;	    S4		PrsMake		S4
;           S4          PrsDeclare      S4
;           S4          <txt mgr sees defining SUB/FUNC/DEF stmt deleted and:
;			 finds text table and window still active -> S3
;                        finds a DECLARE -> S2
;                        finds a SUB reference -> S1
;                        finds no declares or references -> PrsFree -> S0
;
;
; Note that PrsDiscard cannot directly eliminate the Prs Entry if there
; are references to the prs in other text tables.  It can only
; eliminate the text table associated with the entry by calling TxdDiscard,
; which will cause the text manager to see the definition be deleted.
; If the text manager is unable to find a DECLARE to replace as the new
; definition of the prs, the text manager calls PrsFree to completely
; eliminate the Prs entry.
;
; The above table describes a state-transition diagram, which was not
; attempted in this source file due to the number of transitions.
;------------------------------------------------------------------------------

;***
;PrsRef(oNam,procType,oTyp)
;
;Purpose:
;     This function searches for a procedure in the tRs by the name oNam.
;     If a match is found to a prs entry, the oPrs is returned, otherwise, 
;     UNDEFINED.
;     This function will not normally affect prsCur; if, however, procType
;     is PT_SUB and the entry does not already exist, instead of returning
;     UNDEFINED, it will create the entry and return the oPrs.
;
;     The procType field is primarily used to indicate whether the procedure
;     is a DEF FN or not; if not, then the oTyp input is not used. If it
;     is a DEF FN, however, the search logic is different - - - rather than
;     just matching on the name, the search algorithm must match on the oTyp
;     and oMrs as well.
;
;     This routine is called by the parser when a procedure reference is seen,
;     and probably by the scanner when it sees a function entry in a variable
;     table.
;
;Entry:
;	oNam is an offset into current module's name table for proc name
;       procType is either PT_SUB, PT_FUNCTION, or PT_DEFFN.
;	oTyp is only valid if procType == PT_DEFFN, in which case it is
;		the oTyp of the DEF; otherwise, this input is undefined.
;Exit:
;	AX == oPrs if found, UNDEFINED otherwise.
;	In the event that the input procType is PT_SUB and no matching prs
;		is found, the prs will be created; if an error occurs in
;		creating the prs, the same standard error code will be returned,
;		(as is returned by PrsDefine) OR'd with 0x8000. If no error
;		occurs, the oPrs of the new prs will be returned.
;	In all cases, prsCur will be the same on exit as on entry.
;Uses:
;	none.
;Exceptions:
;	none.
;*******************************************************************************
cProc	PrsRef,<PUBLIC,NEAR,NODATA>,<SI>
	parmW	oNam
	parmB	procType
	parmB	oTyp
cBegin	PrsRef
	DbChk	oNam,oNam			;sanity check on input oNam
	mov	si,[grs.GRS_oPrsCur]		;save for later restoring
	cCall	PrsDeActivate			;must deactivate for search

	cCall	OgNamOfONam,<oNam>		; given oNam, fetch ogNam
	xchg	ax,cx				; cx = ogNam or 0
	mov	ax,08000H OR ER_OM		;[10] in case of OM err ret
	jcxz	PrsRef_Exit			;[10]  brif OM error

	mov	al,[procType]			;tell RsTableSearch to use tPrs
	mov	dl,[oTyp]
	cCall	RsTableSearch,<cx>		; search tRs for entry

	cmp	ax,UNDEFINED
	jnz	PrsRef_Exit			;brif found

PrsRef_No_Match:
	cmp	[procType],PT_SUB		;looking for a SUB?
	jnz	PrsRef_Exit			;  brif not - exit

	;special case; reference to a SUB which was not found. Create the
	;	entry, return the oPrs or error code
	push	[oNam]
	push	Word Ptr [procType]
	push	ax				;push garbage for oTyp parm
	mov	al,1
	push	ax				;push a non-zero word (true)
	call	far ptr PrsDefine		; create the entry
PrsRef_Err_Check:
	or	ah,080h				;set high bit in case of error
	cmp	ax,08000h			;FALSE return if no error
	jnz	PrsRef_Exit			;  brif error; pass code back

	mov	ax,[grs.GRS_oPrsCur]		;retval

PrsRef_Exit:
	push	ax				;save retval
	push	si				;parm to PrsActivateCP
	call	PrsActivateCP			;reactivate original prs
	pop	ax				;retval
cEnd	PrsRef

;***
;PrsMake(ogNam, procType)
;
;Purpose:
;     This function searches for a procedure by the name ogNam.
;     If found, it gives an ER_DD if the found procedure has
;     a text table; otherwise, it just activates the found procedure and gives
;     it a text table.  If a new entry is created, it calls the TextMgr
;     routine TxtCurInit() to initialize the text table,
;     and calls the Variable Manager's MakePrsTVar().
;Entry:
;	 ogNam for the proc name
;	 procType - PT_SUB or PT_FUNCTION (should never get PT_DEFFN here)
;Exit:
;        AX == FALSE if no error, ER_OM if out of memory, MSG_IdTooLong
;		if the input name is too long, and ER_DD if a matching
;		entry is found that already has a text table, or if a
;		match is found to a compiled module name.
;	 if ER_DD, [oRsDupErr] = oRs of existing proc with same name.
;        Note that is an error code is returned, there will be no active prs
;	 grs.oMrsCur = procedure's oMrs (unchanged if new proc)
;	 grs.oPrsCur = procedure's oPrs
;	 all fields in prsCur are setup if we're making an existing prs active
;		if we're creating a new prs, then prsCur will have the correct 
;		ogNam, oVarHash, oMrs, and oPrs fields; other fields must be 
;		set up by caller as appropriate.
;	 all fields in txdCur are setup
;Uses:
;	none.
;Exceptions:
;	none.
;*******************************************************************************
cProc	PrsMake,<PUBLIC,FAR,NODATA>,<SI>
	parmW	ogNam			
	parmB	procType
	LocalW	oPrsSave
	LocalW	oMrsSave
	LocalW	searchResult
cBegin	PrsMake
	mov	ax,[grs.GRS_oPrsCur]
	mov	[oPrsSave],ax		;save original oPrs
	mov	ax,[grs.GRS_oMrsCur]
	mov	[oMrsSave],ax		;save original oMrs
	cCall	PrsDeActivate
	mov	si,[ogNam]		
	cCall	ONamOfOgNam,<si>	; given ogNam, fetch oNam
	jnz	PrsMake_Cont		;  Catch possible OM error
					;  now, so later callers of FieldsOfPrs
					;  don't have to consider it.
	jmp	PrsMake_ER_OM1		;brif ONamOfOgNam returned Out of Memory
PrsMake_Cont:
	;---------------------------------------------------------------------
	cmp	[procType],PT_SUB
	jnz	PrsMake_Namespace_OK	;brif not a SUB - leave name space alone

	push	ax			;save oNam across call
	mov	dl,NMSP_Sub
	cCall	SetONamSpace,<ax,dx>	;set namespace bit to remember that this
					;  is a procedure name
	pop	ax
	jnz	PrsMake_ER_DD_1		;brif namespace conflict	

PrsMake_Namespace_OK:
	cCall	GetVarNamChar,<ax>	;ah non-zero if name starts with FN
	or	ah,ah
	jz	No_FN_Name_Error

	cmp	[procType],PT_DEFFN
	jz	No_FN_Name_Error

	mov	ax,MSG_FNstart		;"Can't start with 'FN'"
	jmp	PrsMake_ER_Exit
No_FN_Name_Error:
	DbAssertRelB procType,nz,PT_DEFFN,CP,<PrsMake got called with PT_DEFFN>
	mov	al,[procType]
	cCall	RsTableSearch,<ogNam>	; if err code in CX on retn, ignore
	mov	[searchResult],ax	;save in case of OM error
	inc	ax			;matching entry found?
	.errnz	UNDEFINED - 0FFFFh
	jz	No_Match_Found		;  brif not

	dec	ax			;restore search result
	push	ax			;parm to PrsActivate
	call	PrsActivateCP		;activate this oPrs
	test	[txdCur.TXD_flags],FTX_mrs
	jnz	PrsMake_EnsureTxd	;no error
	;txdCur is for found prs - - error

PrsMake_ER_DD_1:
	jmp	short PrsMake_ER_DD

No_Match_Found:
	call	MakeNewPrs		;make a new entry in prsCur
	jz	PrsMake_ER_OM		;  brif so

	mov	[prsCur.PRS_ogNam],si	; set ogNam in new entry to input
PrsMake_EnsureTxd:
 	mov	al,[procType]		;if we found a match on a FUNCTION or a
 	mov	[prsCur.PRS_procType],al;  DEF FN, FORCE it's proctype to the
	cCall	EnsurePrsTVar		;ax = prs's oVarHash
	jz	PrsMake_ER_OM

	call	AlphaORsFree		;release table of sorted oRs's
	cCall	TxtDeActivate		;save the txd for mrsCur
					; table. leave txdCur as mrs

	;This assignment is necessary for case where prs ref preceded prs
	;definition.  When prs was created for reference, prs's oMrs field
	;was set to mrs with reference.  Now that definition is here, the
	;oMrs field must be set to the mrs which 'owns' the prs.
	
	mov	ax,[oMrsSave]		;ax = caller's oMrs
	mov	[prsCur.PRS_oMrs],ax
;
; At this point mrsCur may not be the same as prsCur.PRS_oMrs, so let's
; ensure that it is.
;
	cmp	[grs.GRS_oMrsCur], ax	
	je	@F			;[477
	push	[grs.GRS_oPrsCur]	; For PrsActivate
	call	PrsDeactivate		
	call	far ptr PrsActivate	
@@:					

	cCall	TxtCurInit		;init txdCur for this new prs
	jz	PrsMake_ER_OM_Txd

	.errnz	FALSE
	xor	ax,ax			;return FALSE
PrsMake_Exit:
cEnd	PrsMake

PrsMake_ER_DD:
	mov	ax,[grs.GRS_oRsCur]
	mov	[oRsDupErr],ax			;tell caller oRs of conflict
	mov	ax,ER_DD
	jmp	short PrsMake_ER_Exit

PrsMake_ER_OM_Txd:
	call	TxtActivate		;reactivate txd for mrsCur
	call	PrsDeActivate		; Set oPrsCur to UNDEFINED,
					;so TxtActivate will activate txd for
					;mrsCur, not this aborted prs
					; Also, in case prsCur on entry was
					; the given prs, must deactivate it
					; here so shared code below will
					; succeed in activating oPrsSave
PrsMake_ER_OM:
	mov	ax,[searchResult]
	inc	ax
	.errnz	UNDEFINED - 0FFFFh
	jnz	PrsMake_ER_OM1		;brif we did not create this prs

	;O.K. - we created it, so we must remove it from the prs table
	mov	ax,dataOFFSET prsCur
	push	ax
	add	ax,SIZE PRS
	dec	ax			;pointer to last byte in range to free
	push	ax
	call	B$ClearRange		;free any owners in prsCur
	call	SetPrsUndef		;grs.oPrsCur=UNDEFINED, oRsCur=oMrsCur
PrsMake_ER_OM1:
	mov	ax,ER_OM
PrsMake_ER_Exit:
	push	ax
	push	oPrsSave		;parm to PrsActivate
	call	PrsActivateCP		;reactivate prsCur
	pop	ax
	jmp	short PrsMake_Exit


;***
;MakeNewPrs
;
;Purpose:
;	Common code to PrsDefine and PrsMake. Sets up prsCur as a new prs.
;Entry:
;	bx = offset to hole in prs table, or UNDEFINED if there is no hole
;	This function assumes that no prs is active (i.e. txdCur reflects
;	   the module's text table)
;Exit:
;	PSW.Z set if out-of-memory
;	grs.oPrsCur is set, meaning prsCur now contains valid info
;Uses:
;	none.
;Exceptions:
;	none.
;****
MakeNewPrs PROC	NEAR
	push	di

	inc	bx				;was there a hole in the table?
	.errnz	UNDEFINED - 0FFFFh
	jz	Prs_GrowTbl			;brif not

	;Unlink the hole from the free list
	GETRS_SEG   es,di,<SIZE,LOAD>		
	dec	bx
	mov	di,bx				;put oHole in di
	DbAssertRel bx,z,oFreePrsFirst,CP,<MakeNewPrs: hole not 1st in free list>
	RS_BASE add,bx
	mov	ax,PTRRS[bx.PRS_oPrsNext]
	mov	[oFreePrsFirst],ax		;free entry now unlinked
	jmp	short PRS_Hole_Found

Prs_GrowTbl:
	sub	ax,ax				
	mov	di,[grs.GRS_bdRs.BD_cbLogical]	;offset to new prs in table
	or	di,di				; Rs table > 32k?
	js	@F				; brif so - OM error

	PUSHI	ax,<dataOFFSET grs.GRS_bdRs>
	PUSHI	ax,<SIZE PRS>
	call	BdGrow
@@:						
	or	ax,ax
	jz	MakeNewPrs_Ret
	
PRS_Hole_Found:
	mov	ax,di
						;new entry put in hole location
	call	SetPrsAx			;grs.oPrsCur = ax
						;grs.oRsCur = 8000h + ax
	GETRS_SEG   es,bx,<SIZE,LOAD>		
	mov	bx,di				;bx = offset to prs in table
	RS_BASE add,bx				;bx = pointer to prs in table
	mov	PTRRS[bx.PRS_ogNam],UNDEFINED	; mark tbl entry as inactve,
						;  since prs is in prsCur
	;Now, link this new prs entry into the prs chain
	mov	PTRRS[bx.PRS_oPrsNext],UNDEFINED ; terminate chain
	mov	dx,bx				; save pPrsNew

	mov	ax,[oPrsFirst]			
	inc	ax				; is this the first prs?
	jnz	LastPrs_Loop			; brif not

	mov	[oPrsFirst],di			; point to new entry
	jmp	short After_Link		; link complete - continue

LastPrs_Loop:					
	dec	ax				; bx == oPrs
	xchg	ax,bx				; advance to next prs
	RS_BASE add,bx				; bx = ptr to prs in table
	mov	ax,PTRRS[bx.PRS_oPrsNext]	; fetch pointer to next prs
	inc	ax				; no more prs's?
	jz	@F				; brif so

	jmp	short LastPrs_Loop		; loop until end of chain
@@:
	mov	PTRRS[bx.PRS_oPrsNext],di	; link new prs in @ end
After_Link:					
	mov	ax,dataOFFSET prsCur		;initialize prsCur, filling it
	mov	bx,SIZE PRS			;  with UNDEFINED, and zeroes 
	mov	cx,PRS_CB_ZERO_INIT		;  as appropriate
	call	InitStruct			; note that this sets
						; the PRS_oPrsNext field to
						; UNDEFINED
	mov	[prsCur.PRS_cbFrameVars],-FR_FirstVar	;	
						; reset to init. value. This
						; value is 2 to account for
						; the fact that b$curframe 
						; is always pushed on the
						; stack after bp, so we
						; treat this word as a frame
						; var for ref'ing the real
						; frame vars off of bp

	mov	ax,[grs.GRS_oMrsCur]		;set oMrs field
	mov	[prsCur.PRS_oMrs],ax
	mov	[fCouldBeBogusPrs],TRUE		;note that there's now a chance
						;  of an unref'd prs
	or	sp,sp				;normal return - reset PSW.Z
MakeNewPrs_Ret:
	pop	di				;restore caller's di
	ret
MakeNewPrs ENDP

;***
;EnsurePrsTVar
;Purpose:
;	Create a local variable table (i.e. allocate oVarHash in
;	module's variable table for local variables) iff:
;	  (1) prs doesn't already have one AND
;	  (2) prs's scan state != SS_RUDE
;Entry:
;	none
;Exit:
;	If didn't need to make local variable table, because prs already
;	   had one, or procedure's scan state == SS_RUDE, ax = UNDEFINED
;	Else if out-of-memory,
;	   ax = 0
;	Else
;	   ax = prs's newly created oVarHash
;	PSW is set based on value in ax
;Exceptions:
;	none
;
;*******************************************************************************
EnsurePrsTVar PROC NEAR
	mov	ax,UNDEFINED
	cmp	[txdCur.TXD_scanState],SS_RUDE	;test table's scan state
	jae	NoTVar			; don't make hash table if there
					; is no variable table
	cmp	[prsCur.PRS_oVarHash],ax
	jnz	NoTVar			;brif prs already has oVarHash
	cCall	MakePrsTVar		;ax = prs's oVarHash, (0 if no memory)
NoTVar:
	or	ax,ax			;set PSW for caller
	ret
EnsurePrsTVar ENDP

;***
;PrsDefine(oNam, procType, oTyp, fNotDefine)
;
;Purpose:
;	Declare or Define a procedure. grs.oPrsCur will be UNDEFINED if we 
;	must first create a new prs entry, or prsCur will be set up to the 
;	appropriate prs entry otherwise; in this latter case, caller
;	wants to rename an existing prs so, we'll create 
;	a new prs for the input name if different from the name of prsCur, 
;	leaving the existing prs (without a text table) in case there are 
;	references (oPrs's) to it.
;
;	This routine is only called when an actual procedure definition
;	is found or when a declaration of a procedure is found.
;
;Entry:
;	oNam -		offset into the name table for mrsCur to the proc name.
;	procType -	PT_SUB, PT_FUNCTION, or PT_DEFFN.
;	oTyp -		only defined if procType == PT_DEFFN; used in searching
;			the Rs table for a matching DEF. Note that this is
;			accepted as a Byte - - we know it must be a predefined
;			type, and the oTyp in the PRS is just stored as a byte.
;	fNotDefine -	non zero if called for DECLARE SUB/FUNCTION
;	                If a prs by this name already exists, it is activated,
;                       otherwise, a new one is created with no text table
;                       allocated to it.
;
;Exit:
;        AX == FALSE if no error, ER_OM if out of memory, ER_DD if a matching
;		entry belongs in another module, and ER_CN if grs.otxCONT is
;		not UNDEFINED (i.e. CONT is possible) and the module variable
;		table would have to grow for this to succeed.
;        Note that if an error code is returned, there may or may not be a
;		current prs.
;	 if a matching module is found, AX = ER_DD and [oRsDupErr] =
;	    oRs of existing module with same name.
;	 grs.oMrsCur will be unchanged.
;	 grs.oPrsCur = procedure's oPrs
;	 prsCur will have the correct bdName, procType, fDefined, oTyp,
;		oVarHash, oMrs, and oPrs fields; other 
;		fields must be set up by caller as appropriate.
;	 all fields in txdCur are setup (will be for mrsCur if PT_DEFFN or
;		input fNotDefine is TRUE)
;        NOTE: In the special case where we're changing the name of an
;		existing prs entry (prsCur on input), we'll create a new
;		entry, copy the old one to the new one, but with the new
;		name, and mark the old one as having no text table or
;		definition. This is because references (oPrs's) may still
;		exist to the old prs name. The new entry will be prsCur, set
;		up as described above.
;	 PSW set based on value in ax
;Uses:
;	none.
;Exceptions:
;	none.
;     
;*******************************************************************************
cProc	PrsDefine,<PUBLIC,FAR,NODATA>,<SI,DI>	
	parmW	oNam
	parmB	procType
	parmB	oTyp
	parmB	fNotDefine
	LocalV	bdName,%(SIZE BD)
	LocalW	oPrsSave
	LocalW	oMrsSave
	LocalW	SearchResult
cBegin	PrsDefine
	DbChk	oNam,oNam			;sanity check on input oNam
	mov	ax,[grs.GRS_oPrsCur]
	mov	[oPrsSave],ax			;save original oPrs
	xchg	ax,di				;in case of error exit
	mov	ax,[grs.GRS_oMrsCur]
	mov	[oMrsSave],ax			;save original oMrs
	cCall	PrsDeActivate			;for table search, below

	cCall	OgNamOfONam,<oNam>		; given oNam, fetch ogNam
	xchg	ax,si				; si = ogNam or 0
	mov	ax,ER_OM
	jz	PrsDefine_Exit1			;brif OM error return

	mov	bx,[oNam]
	mov	al,NMSP_SUB

	;--------------------------------------------------------------------
	cmp	[procType],PT_SUB
	jnz	PrsDefine_Reset_Namespace	;brif not a SUB - reset SUB
						;  bit in name entry in case
						;  we're converting a SUB to
						;  a FUNCTION
	
	cCall	SetONamSpace,<bx,ax>		
	jnz	JNZ_PrsDefine_DD_1		;[69]brif namespace already
						;	 set to something else
	jmp	short PrsDefine_Namespace_OK
PrsDefine_Reset_Namespace:
	call	ResetONamMask			;reset the NMSP_SUB bit	
PrsDefine_Namespace_OK:
	mov	ax,ER_CN
	cmp	[grs.GRS_otxCONT],UNDEFINED	;is user able to CONTinue?
	jnz	PrsDefine_Exit1			;  brif so - - - don't want
						;  to risk moving var tab
	mov	dl,[oTyp]
	mov	al,[procType]			;tell RsTableSearch to use tPrs
	cmp	al,PT_SUB
	jnz	PrsDefine_NotSub		;brif not a sub

	mov	[oTyp],0			; scanner depends on 0 in
						; oTyp field of SUB prs's
PrsDefine_NotSub:
	cCall	RsTableSearch,<si>		;search Rs table for entry
						;bx=offset to hole if not found
	mov	dx,[oPrsSave]			;for later reference
	mov	[SearchResult],ax		;save in case of OM error
	inc	ax				;was a matching entry found?
	jz	PrsDefine_No_Match		;  brif not

	dec	ax
	jmp	SHORT PrsDefine_Match_OK	;brif no match error in search

PrsDefine_Exit1:
	jmp	PrsDefine_ER_Exit		;exit with error code

PrsDefine_Match_OK:
	inc	dx				;was prsCur active on input?
	jz	Change_Entry			;  brif not

	dec	dx
	cmp	ax,dx				;was match made w/ input prsCur?
	jz	Change_Entry			;  brif so - change that entry

	xchg	di,ax				;di = oPrsFound.

	cCall	FieldsOfPrs,<di>		;dh = prs flags,
	GETRS_SEG   es,ax,<SIZE,LOAD>		; es:bx = pPrsFound
	test	dh,FP_DEFINED
JNZ_PrsDefine_DD_1:				
	jnz	PrsDefine_DD_1			;Error - caller trying to change
						;    name to prsCur to the name
						;    of an already existing 
						;    defined prs
	cmp	PTRRS[bx.PRS_txd.TXD_bdlText_status],NOT_OWNER
	jnz	PrsDefine_DD_1			;Error - found prs has text
						;    table.
	mov	ax,PTRRS[bx.PRS_oPrsNext]	; save link field so we
	mov	[prsCur.PRS_oPrsNext],ax	; can restore after blk cpy
	DJMP	jmp SHORT PrsDefine_Cont1	;jmp to copy input prsCur to 
						;    this one - - found prs
						;    not defined, so we'll just
						;    copy prsCur to it, and make
						;    it defined, leaving the
						;    input current prs around,
						;    but w/out a text table
						;    (in case of ref.s to it)

PrsDefine_No_Match:
	cmp	dx,UNDEFINED			;was a prs active?
	jnz	Change_Entry_1			;  brif so - change the entry

	call	MakeNewPrs			;make a new entry in prsCur
	jz	PrsDefine_OM_1			;  brif OM error return
	jmp	PrsDefine_Cont

Change_Entry_1:
	xchg	ax,dx				;ax = oPrsSave, dx = garbage
	mov	[searchResult],ax		;in case of OM error when
						;  changing the name
Change_Entry:
	xchg	di,ax				;di = oPrsFound
	push	di				;parm to PrsActivate: oPrsFound
	call	PrsActivateCP			;activate entry to change
	cmp	[fNotDefine],FALSE
	jnz	PrsDefine_No_Err		;brif called for DECLARE

	test	[prsCur.PRS_flags],FP_DEFINED
	jz	PrsDefine_No_Err1

;error - prs [di] was already defined
PrsDefine_DD_1:
	xchg	ax,di				;ax = existing duplicate oPrs
	or	ah,80h				;ax = existing duplicate oRs
	mov	[oRsDupErr],ax			;tell caller oRs of conflict
	mov	ax,ER_DD
	jmp	PrsDefine_ER_Exit

PrsDefine_No_Err1:
	test	[txdCur.TXD_flags],FTX_Mrs	;does this prs have a text table
	jnz	PrsDefine_No_Err		;  brif not

	cmp	[oPrsSave],UNDEFINED		;was a prs active at entry?
	je	PrsDefine_DD_1			;brif not - must be entering
						; a SUB at module level, when
						; the SUB line has been deleted,
						; or commented out. If we don't
						; do this, a Prs could change
						; from one Mrs to another
						; unexpectedly.
PrsDefine_No_Err:
	cmp	[prsCur.PRS_ogNam],si		; change name of current prs
						;  if it was different
	jz	Names_Match			; brif no change

	;changing name of an existing prs. Must create a new prs for the new
	;	name, and leave the old one around (minus its text table) in
	;	case there are references to it hanging around (oPrs's) in
	;	text or variable tables
	
	DbAssertRelB [fNotDefine],z,FALSE,CP,<PrsDefine got Change_Entry error>
	cCall	PrsDeActivate

	mov	di,[grs.GRS_bdRs.BD_cbLogical]	;offset to new prs in table
	mov	bx,UNDEFINED			; just grow the table
	call	MakeNewPrs			
	jnz	PrsDefine_Cont1 		
PrsDefine_OM_1:
	jmp PrsDefine_ER_OM			

;di = oPrsNew
PrsDefine_Cont1:
	mov	bx,[grs.GRS_bdRs.BD_pb] 	;base pointer to prs table
	mov	cx,bx
	add	cx,di				;cx = pPrsNew
	add	bx,[oPrsSave]			;bx = pPrsOld
	push	bx				;save pPrsOld
	push	cx				;save pPrsNew

	push	bx				;pass pPrsOld to CopyBlk
	push	cx				;pass pPrsNew to CopyBlk
	PUSHI	ax,<SIZE PRS>
	cCall	CopyBlk 			;copy prs

	;After duplicating the entry, we need to initialize a few fields
	;in both the old and new prs entries.  Note that prs.oVarHash
	;is now meaningless in both entries since the only way we could
	;be here is after the user has edited the name of a procedure,
	;which the text manager guarentees is a Rude Edit (causing
	;the module's variable table to be discarded).
	
	call	SetPrsUndef			; in case MakeNewPrs set
	mov	ax,[oPrsSave]
	push	[prsCur.PRS_oPrsNext]		; preserve across call
	call	UndefPrs			;old prs is no longer defined
						;It is up to text mgr
						;ChkAllUndefPrs
						;function to redefine it if
						;any other refs exist
	GETRS_SEG   es,bx,<SIZE,LOAD>		; in case modified by call
	pop	ax				; link field for new entry
	pop	bx				;es:bx = pPrsNew
	mov	PTRRS[bx.PRS_oPrsNext],ax	; so link field is correct
	mov	PTRRS[bx.PRS_ogNam],NULL	; new prs entry has no name

	xchg	ax,bx				;  yet hang onto pPrsNew for
						;  a moment
	pop	bx				;bx = pPrsOld
	add	bx,PRS_txd.TXD_bdlText		;make this a pBdl for old prs
	add	ax,PRS_txd.TXD_bdlText		;make this a pBdl for new prs
	cCall	BdlChgOwner,<bx,ax>
	call	AlphaORsFree			;release table of sorted oRs's
	
	push	di				;oPrsNew	
	call	PrsActivateCP			;reactivate prsCur

Names_Match:

PrsDefine_Cont:
	;If this PRS has the defined bit set, then we have a more powerful
	;reference, don't arbitrarily change the proc type.  If we did this,
	;changing a DECLARE from SUB to FUNCTION for a defined SUB would
	;cause us to list the SUB with an END FUNCTION.  If we are editting,
	;the SUB/FUNCTION definition line, then the txtmgr will have already
	;turned off the FP_DEFINED bit for this prs.
	
	test	[prsCur.PRS_flags],FP_DEFINED	;Is this prs already defined?
	jnz	PrsAlreadyDefined		;brif so
	mov	al,[procType]
	mov	[prsCur.PRS_procType],al
PrsAlreadyDefined:
	cmp	[fNotDefine],FALSE
	jnz	PrsDefine_Declare		;brif called for DECLARE/REF

	mov	ax,[oMrsSave]			;ax = caller's oMrs
	mov	[prsCur.PRS_oMrs],ax

	call	EnsurePrsTVar			;make local var table if
						; none already
	jz	PrsDefine_TVar_Err

PrsDefine_Declare:
	mov	al,[oTyp]
	and	[prsCur.PRS_oType],NOT M_PT_OTYPE   
	or	[prsCur.PRS_oType],al		
	mov	[prsCur.PRS_ogNam],si		; set name of prs
	xor	ax,ax				; retval
PrsDefine_Exit:
	or	ax,ax				;set PSW for caller
cEnd	PrsDefine

;NOTE: The error handling code is fairly large due to the constraint that,
;NOTE: on any error, the error code is returned, and the context is left just
;NOTE: as it was prior to the call.

PrsDefine_TVar_Err:
	cmp	[SearchResult],UNDEFINED	;did prs already exist?
	jnz	PrsDefine_ER_OM_2		;  brif so - leave prs entry
PrsDefine_ER_OM:
	mov	ax,dataOFFSET prsCur
	push	ax
	add	ax,SIZE PRS
	dec	ax				;pointer to last byte in range
	push	ax
	call	B$ClearRange			;free any owners in prsCur
	call	SetPrsUndef			;grs.oPrsCur=UNDEFINED,
						;oRsCur=oMrsCur
PrsDefine_ER_OM_2:
	mov	ax,ER_OM
PrsDefine_ER_Exit:
	push	ax
	push	oMrsSave			;parm to MrsActivateCP
	call	MrsActivateCP			;reactivate prsCur
	push	oPrsSave			;parm to PrsActivate
	call	PrsActivateCP			;reactivate prsCur
	pop	ax				
	jmp	short PrsDefine_Exit

;***
;OTypeOfTypeChar
;Purpose:
;	Returns the oType associated with the specified type character.
;
;	Split from oRsOfHstProc and rewritten in revision [90]
;
;Entry:
;	AL - The character.
;Exit:
;	AX - otype of the specified type character
;	     ET_IMP if not a valid type character.
;	     Z flag is set if ax == ET_IMP.
;
;*******************************************************************************
tcEt LABEL BYTE
cEt_SD: db	'$'
cEt_R8:
	db	'#'
cEt_R4: db	'!'
cEt_I4: db	'&'
cEt_I2: db	'%'
tcEtEnd LABEL BYTE

	.erre	ET_R4 EQ (tcEtEnd - cEt_R4)
	.erre	ET_SD EQ (tcEtEnd - cEt_SD)
	.erre	ET_R8 EQ (tcEtEnd - cEt_R8)
	.erre	ET_I4 EQ (tcEtEnd - cEt_I4)
	.erre	ET_I2 EQ (tcEtEnd - cEt_I2)


DbPub OTypeOfTypeChar
cProc OTypeOfTypeChar,<NEAR>,<DI>
cBegin
	push	cs
	pop	es
	mov	di,CPoffset tcEt
	mov	cx,tcEtEnd - tcEt
	repnz	scasb			;NZ and cx == 0 if not found
					; Z and cx == oType-1 if found
	jnz	@F
	inc	cx			;cx == oType
@@:
	.errnz	ET_IMP
	mov	ax, cx			;ax == ET_IMP if not found
					;   == ET_ type if found
	or	ax,ax
cEnd

cProc OTypeOfTypeCharFar,<FAR,PUBLIC>
	parmB	TypeChar
cBegin
	mov	al,[TypeChar]
	call	OTypeOfTypeChar
cEnd


;***
;PrsDeActivate, PrsDeAct_NoTxd
;
;Purpose:
;     Save the	current procedure's  register set  (prsCur) back into the tRs.
;     TxtDeActivate is called to handle the procedure text table.
;
;     PrsDeAct_NoTxd is the same as PrsDeActivate, except that it doesn't
;     activate mrsCur's txd - - - i.e., there is no txd active on exit, unless
;     the routine is called without an active prs - - - in this latter case,
;     no action is taken.
;
;     NOTE: This routine is guaranteed to cause no heap movement.
;
;Entry:
;	 grs.oPrsCur = current procedure to be DeActivated
;	   UNDEFINED if there is no current procedure
;Exit: 
;	 grs.oPrsCur = UNDEFINED
;        For PrsDeActivate, txdCur is setup for mrsCur 
;		(unless grs.oMrsCur == UNDEFINED on input)
;Uses:
;	none.
;Preserves:
;	ES
;Exceptions:
;	none.
;     
;*******************************************************************************
PUBLIC	PrsDeActivate
PrsDeActivate:
	mov	ax,sp
	SKIP2_PSW
PrsDeAct_NoTxd:
	xor	ax,ax
cProc	PrsDeAct,<PUBLIC,NEAR,NODATA>
cBegin	PrsDeAct
	DbHeapMoveOff				;assert no heap movement here
	cmp	[grs.GRS_oPrsCur],UNDEFINED
	jz	PrsDeAct_Exit

	push	si
	push	di
	push	es
	xchg	ax,di				;save flag in di
	DbChk	ConStatStructs
	mov	si,[grs.GRS_oPrsCur]		;si = pointer to prs entry
	RS_BASE add,si
	GETRS_SEG   es,bx,<SIZE,LOAD>		
	mov	PTRRS[si.PRS_txd.TXD_bdlText_status],NOT_OWNER
	mov	BPTRRS[si.PRS_txd.TXD_flags],FTX_mrs  ;ensure txd looks invalid in
						;  table, in case this prs has
						;  no text table
	cCall	TxtDeActivate			;copies txd into prs table or
						;  mrs table, as appropriate
	GETRS_SEG   es,bx,<SIZE,LOAD>		
	mov	bx,si				;bx = pointer to prs entry
	mov	si,dataOFFSET prsCur		;si = pointer to prsCur
	call	CopyNchgPrs			;copy prs & move bdName owner

	call	SetPrsUndef			;grs.oPrsCur=UNDEFINED,
						;oRsCur=oMrsCur
	or	di,di
	jz	PrsDeAct_NoTxd_Exit		;brif want no txd active on exit

	cCall	TxtActivate			;activate txdCur for mrsCur
PrsDeAct_NoTxd_Exit:
	pop	es
	pop	di
	pop	si
PrsDeAct_Exit:
	DbHeapMoveOn
cEnd	PrsDeAct

;***
;PrsDeActivateFar()
;
;Purpose:
;	Same as PrsDeActivate, but accessible from other code segments.
;
;Entry:
;	 grs.oPrsCur = current procedure to be DeActivated
;	   UNDEFINED if there is no current procedure
;Exit: 
;	 grs.oPrsCur = UNDEFINED
;        txdCur is setup for mrsCur (unless grs.oMrsCur == UNDEFINED on input)
;Uses:
;	none.
;Exceptions:
;	none.
;     
;*******************************************************************************
cProc	PrsDeActivateFar,<PUBLIC,FAR,NODATA>
cBegin	PrsDeActivateFar
	cCall	PrsDeActivate
cEnd	PrsDeActivateFar

;***
;PrsActivate(oPrsNew)
;
;Purpose:
;     Make the procedure indicated by oPrsNew the current active procedure. The
;     current procedure's register set is saved via PrsDeActivate. If oPrsNew's
;     module is different from grs.oMrsCur and the procedure has an associated
;     text table, MrsActivate() is called to make the procedure's module 
;     current. TxtActivate is called at the end.
;
;     NOTE: This routine is guaranteed to cause no heap movement.
;
;Entry:
;	 'oPrsNew' is an offset into the Rs table
;	   for procedure to be made current.
;	 Note that, if oPrsNew == UNDEFINED or if the entry at offset oPrsNew
;		is a discarded entry, the current procedure (if any)
;		will be deactivated, and no new prs will be activated.
;Exit:
;	 grs.oPrsCur = oPrsNew
;	 if oPrsNew != UNDEFINED
;		all fields in prsCur are setup
;		all fields in txdCur are setup
;        If prs @ offset oPrsNew has a text table, then mrsCur is activated
;		for prsNew.oMrs.
;Uses:
;	none.
;Preserves:
;	ES
;Exceptions:
;	none.
;     
;*******************************************************************************
cProc	PrsActivate,<PUBLIC,FAR,NODATA>
	parmW	oPrsNew
cBegin	PrsActivate
	cCall	PrsActivateCP,<oPrsNew>
cEnd	PrsActivate

cProc	PrsActivateCP,<PUBLIC,NEAR,NODATA>,<SI,ES>
	parmW	oPrsNew
cBegin	PrsActivateCP
	DbHeapMoveOff				;assert no heap movement here
	mov	ax,[oPrsNew]
	mov	bx,[grs.GRS_oPrsCur]
	cmp	ax,bx
	DJMP	jz  PrsActivate_Exit		;brif desired prs is already
						;  active
	mov	si,ax				;si = ax = oPrsNew
	inc	ax
	jz	PrsActivate_Cont		;brif oPrsNew == UNDEFINED

	DbChk	oPrs,si				;ife RELEASE - ensure validity

	;we're actually activating a prs. If we currently have a prs active,
	;then we can avoid activating and then just deactivating the txd for
	;mrsCur if both prs's are in the same module  - - - that's a 
	;considerable speed savings.
	RS_BASE add,si				;si = pPrs
	GETRS_SEG   es,cx,<SIZE,LOAD>		
	mov	cx,PTRRS[si.PRS_oMrs]		;is oMrs for this prs the same
	cmp	[grs.GRS_oMrsCur],cx		;  as oMrsCur?
	jnz	PrsActivate_Cont1		;  brif not -

	inc	bx
	jz	NoPrs_Active			;brif no prs active

	call	PrsDeAct_NoTxd			;deactivate existing prs,
						;  don't activate txd for mrsCur
	jmp	short MrsCur_Is_Correct
NoPrs_Active:
	;no prs currently active, and we're activating a prs in mrsCur
	cCall	TxtDeActivate			;deactivate txd for mrsCur
	jmp	short MrsCur_Is_Correct
PrsActivate_Cont1:
	dec	ax
	xchg	ax,si				;si = oPrsNew
PrsActivate_Cont:
	cCall	PrsDeActivate			;deactivate current prs, if any

	inc	si				;test for UNDEFINED
	jz	PrsActivate_Exit		;brif not
	dec	si				;restore si = oPrsNew

	RS_BASE add,si				;si = pPrs
	GETRS_SEG   es,bx,<SIZE,LOAD>		

	mov	ax,PTRRS[si.PRS_oMrs]		;is oMrs for this prs the same
	mov	dx,[grs.GRS_oMrsCur]		
	cmp	dx,ax				;	as oMrsCur?
	jz	MrsCur_Is_Correct1		;  brif so

	;When activating a PRS, we must activate the PRS's MRS iff:
	; - the procedure has a Text Table or
	; - the procedure has a Definition (i.e. SUB/FUNCTION stmt)
	;A PRS could have a text table and no Definition after PrsMake
	; (i.e. after user does a File/New/Sub)
	;A DEF FN PRS could have a Definition and no text table
	;A SUB/FUNCTION PRS could have a Definition and no text table
	; after a SUB/FUNCTION statement is seen by Parser during
	; ASCII Load, but before the TxtMgr calls PrsMake to give the
	; prs a text table.
	
	cmp	PTRRS[si.PRS_txd.TXD_bdlText_status],NOT_OWNER
	jnz	PrsHasText			;brif prs has a text tbl
						; (if so, its mrs must also
						;  be activated)
	inc	dx				; is there an active mrs?
	jz	PrsHasText			; brif not - make sure that
						; an mrs is active

	test	BPTRRS[si.PRS_flags],FP_DEFINED
	jz	MrsCur_Is_Correct1		;brif prs is DECLAREd but not
						;defined
PrsHasText:
	cCall	MrsActivateCP,<ax>		;activate the mrs for this prs	
MrsCur_Is_Correct1:
	cCall	TxtDeActivate			;deactivate txdCur for mrsCur
MrsCur_Is_Correct:
	DbChk	MrsCur				;in case mrsCur not active on
						;  input, and FP_DEFINED not set
	GETRS_SEG   ds,bx,<SIZE,LOAD>		
	assumes ds,nothing
	mov	bx,dataOFFSET prsCur		;now bx == prsCur dest address
	SETSEG_EQ_SS	es			;es:bx == dest address
	call	CopyNchgPrs			;copy prs & move bdName owner
	SETSEG_EQ_SS	ds
	assumes ds,DATA

	mov	ax,[oPrsNew]			;note new oPrsCur
	call	SetPrsAx			;grs.oPrsCur = ax
						;grs.oRsCur = 8000h + ax

	cCall	TxtActivate			;activate txdCur for this prsCur
						;  from the Rs table
PrsActivate_Exit:
	DbHeapMoveOn
cEnd	PrsActivateCP

;***
;PrsFree()
;
;Purpose:
;     This is called to remove prsCur from the Rs table. It releases all
;     resources associated with current procedure including its dynamic value 
;     table and text table (the latter via TxtDiscard). Note that it also 
;     causes ALL variables to be cleared! Note that it also resets the
;     flags byte in the name table entry.
;     Note also that this function should only be called after PrsDiscard
;     has been called for the procedure.
;
;     NOTE: some callers of this routine expect no heap movement to occur.
;
;Entry:
;     grs.oPrsCur assumed != UNDEFINED, and prsCur is a valid, active prs.
;     It is assumed that the txd for prsCur (i.e., txdCur) has already been
;     discarded via a call to PrsDiscard.
;Exit:
;     AX != FALSE (for use with ForEach...)
;Uses:
;     none.
;Exceptions:
;     none.
;	 
;*******************************************************************************
cProc	PrsFree,<PUBLIC,FAR,NODATA>
cBegin	PrsFree
	DbHeapMoveOff				;assert no heap movement here

	DbChk	ogNam,prsCur.PRS_ogNam		
	mov	ax,[prsCur.PRS_oMrs]		
	cmp	ax,[grs.GRS_oMrsCur]		; 'defining' mrs active?
	jz	@F				; brif so

	push	[grs.GRS_oPrsCur]		; parm to PrsActivateCP
	cCall	MrsActivateCP,<ax>		; activate mrs for this prs
	call	PrsActivateCP			; reactivate prsCur
@@:						
	cCall	ONamOfOgNam,<prsCur.PRS_ogNam>	; get oNam for this prs
	DbAssertRel ax,nz,0,CP,<PrsFree: Out of memory return from ONamOfOgNam>
	xchg	ax,bx				;bx=oNam, parm to ResetONamMask
	mov	al,NMSP_SUB OR NM_fShared	;reset these bit flags, i.e.,
	call	ResetONamMask			;  free the module namespace
						;  for this name

	cCall	DiscardHistoryORs,<grs.GRS_oRsCur> ; throw away help
						; history for prs.

	;Now, unlink from the prs chain, and link into the prs free chain
	GETRS_SEG   es,bx,<SIZE,LOAD>		
	mov	ax,[grs.GRS_oPrsCur]
	mov	cx,ax
	xchg	[oFreePrsFirst],ax		;link at head of prs free chain
	mov	bx,[oPrsFirst]
	cmp	bx,cx				;was this prs at head of chain?
	jnz	UnlinkPrs_Loop			;  brif not

	RS_BASE add,bx
	xchg	ax,PTRRS[bx.PRS_oPrsNext]	;finish free chain link,
						;  set ax for unlink
	mov	[oPrsFirst],ax
	jmp	short PrsFree_LinksOk

UnlinkPrs_Loop:
	RS_BASE add,bx
	mov	dx,PTRRS[bx.PRS_oPrsNext]
	cmp	dx,cx				;is this the previous entry?
	jz	GotPrevEntry			;  brif so

	mov	bx,dx
	jmp	UnlinkPrs_Loop

GotPrevEntry:
	xchg	bx,dx
	RS_BASE add,bx
	xchg	ax,PTRRS[bx.PRS_oPrsNext]	;finish free chain link,
						;  set ax for unlink
	xchg	bx,dx
	mov	PTRRS[bx.PRS_oPrsNext],ax	;link now complete
PrsFree_LinksOk:
	call	SetPrsUndef			;grs.oPrsCur=UNDEFINED,
						;oRsCur=oMrsCur
	mov	ax,sp				;non-zero retval
	DbHeapMoveOn
cEnd	PrsFree

;***
;PrsDiscard()
;
;Purpose:
;     This is called by the user-interface in response to the Discard Procedure
;     menu selection. It releases all resources associated with current
;     procedure including its dynamic value table and text table (the latter
;     via TxtDiscard). Note that it also causes ALL variables to be cleared!
;     This is also called by MrsDiscard for all prs's that have a text table.
;
;     PrsDiscard1 is identical to PrsDiscard, except that it doesn't try
;     to find new "defining" references for prs entries which lost their
;     defining references.  This is a speed opt for cases which need to
;     call PrsDiscard multiple times.  After all calls to PrsDiscard1 have
;     been made, the caller should call ChkAllUndefPrs, to update the
;     prs entries.
;
;Entry:
;     grs.oPrsCur assumed != UNDEFINED, and prsCur is a valid, active prs.
;     mrsCur assumed set up correctly as well.
;     grs.oMrsCur assumed == prsCur.oMrs
;     txdCur assumed to be set up correctly for prsCur if the prs has a
;	 text table.
;Exit:
;     AX == TRUE (non-zero)
;Uses:
;	none.
;Exceptions:
;	none.
;	 
;*******************************************************************************
cProc	PrsDiscard,<PUBLIC,FAR,NODATA>
cBegin	PrsDiscard
	call	PrsDiscard1
	call	ChkAllUndefPrs			;search for new defining
						;references for Prs entries
						;which had their "defining"
						;reference deleted.
PrsDiscardX:
	mov	ax,sp				;reval = true
cEnd	PrsDiscard

cProc	PrsDiscard1,<PUBLIC,NEAR,NODATA>
cBegin	PrsDiscard1
	DbChk	ConStatStructs

	or	[mrsCur.MRS_flags],FM_VARNEW	;tell CLEAR code we want to
						; deallocate $static arrays,
						; not just zero-fill them (in
						; this module only)
	call	ClearTheWorld
	and	[mrsCur.MRS_flags],NOT FM_VARNEW ;reset bit in mrs flags
	test	[txdCur.TXD_flags],FTX_mrs
	jnz	PrsHasNoTxtTbl			;brif DECLARE or DEF FN prs

	call	AlphaORsFree			;release table of sorted oRs's
	push	[grs.GRS_oRsCur]		;force module level code to
	push	[grs.GRS_oMrsCur]		; be visible in any windows
						; that show this prs (if any)
	PUSHI	ax,0				;not just renaming an oRs
	call	WnReAssign		

	cCall	DiscardHistoryORs,<grs.GRS_oRsCur> ; throw away help
						; history for prs.

	cCall	TxtDiscard			;discard prs's text table
	cCall	TxtActivate			;activate txdCur for mrsCur
PrsHasNoTxtTbl:
	test	[grs.GRS_flagsDir],FDIR_new
	jz	PrsMayHaveRefs			;brif NewStmt isn't active
	call	PrsFree 			;delete entry from table
PrsMayHaveRefs:
	mov	ax,sp				;retval = true
cEnd	PrsDiscard1

;***************************************************************************
; GetNextPrsInMrs, GetNextPrs
; Purpose:
;	For GetNextPrs, find next prs.
;	For GetNextPrsInMrs, find next prs in given module.
;
;	GetNextPrs added as part of revision [40].
; Note:
;	[13] This function now handles the case where the next prs in
;	[13] the Rs table (or the first prs, if ax = UNDEFINED on entry)
;	[13] is a hole for prsCur.  Note, therefore, that the oPrs returned
;	[13] could be for prsCur, i.e., do not assume that this can be
;	[13] used to access the prs in the Rs table. Suggestion: Use
;	[13] PPrsOPrs or FieldsOfPrs to access prs information.
; Entry:
;	ax = oPrs of current prs (UNDEFINED if caller wants oPrs of
;	     1st prs [in module].
;	bx = module of interest (only used for GetNextPrsInMrs)
; Exit:
;	ax = UNDEFINED if there are no more prs's that meet the requirement
;	else it is the offset into the prs table for the current prs.
;	Since no oPrs can exceed 7FFFH (for global reasons), it
;	is safe for the caller to do:
;		js	NoPrs
;	es = seg of Rs table in far Rs table versions.
;Uses:
;	none.
;Preserves:
;	cx
;Exceptions:
;	none.
;
;***************************************************************************
cProc	GetNextPrs,<PUBLIC,NEAR,NODATA> 	
cBegin	<nogen> 				
	sub	dx,dx				
	jmp	short GetNext_Prs		
cEnd	<nogen> 				

cProc	GetNextPrsInMrs,<PUBLIC,NEAR,NODATA>	
cBegin	<nogen> 				
	mov	dx,sp				
cEnd	<nogen> 				

?DFP = DFP_NONE 				; don't smash regs on entry
cProc	GetNext_Prs,<NEAR,NODATA>,<DI,CX>	
cBegin
?DFP = DFP_CP					; restore switch
	mov	cx,dx				; cx == 0 ==> ignore oMrs
	mov	dx,bx				;dx = oMrs
	GETRS_SEG   es,bx,<SIZE,LOAD>		
	mov	bx,ax				;bx = UNDEFINED/oPrs
	inc	ax
	jnz	NextPrs_Loop_Start		;brif a prs is active

	mov	bx,[oPrsFirst]			;bx == oPrsCur
	jmp	short NextPrs_Start
NextPrs_Loop_Start:
	RS_BASE add,bx
NextPrs_Loop:
	mov	bx,PTRRS[bx.PRS_oPrsNext]
NextPrs_Start:
	mov	ax,UNDEFINED
	inc	bx
	.errnz	UNDEFINED - 0FFFFH
	jz	NextPrs_Exit			;brif no more prs's in table

	dec	bx
	mov	ax,bx				;potential return value
	RS_BASE add,bx
	cmp	PTRRS[bx.PRS_ogNam],UNDEFINED	; is current entry valid?
	jnz	Got_Valid_Entry			;   brif so

	DbAssertRel ax,z,grs.GRS_oPrsCur,CP,<NextPrs err>
						;UNDEFINED ogNam, must be active
	mov	bx,dataOFFSET prsCur		
	SETSEG_EQ_SS	es
Got_Valid_Entry:				
	jcxz	NextPrs_Exit			; brif we want all prs's,
						; not just those for a
						; given module
	cmp	dx,PTRRS[bx.PRS_oMrs]		;is this prs in current module?
	jnz	NextPrs_Loop			;  brif not, loop to next entry

NextPrs_Exit:
	or	ax,ax				;set condition codes for caller
cEnd

;***************************************************************************
; NextMrsFile
; Purpose:
;	Find and activate the next mrs for which has FM2_File set.
; Entry:
;	grs.oMrsCur = oMrs of current mrs (UNDEFINED if caller wants first
;	     module in the Rs table.
;	Note that this does NOT find or activate the global module.
; Exit:
;	next mrs is loaded into mrsCur,
;	ax = grs.oMrsCur = UNDEFINED if there are no more mrs's in the
;		Rs table to be activated,
;	else it is actually oMrsCur.
;	Note that if ax = UNDEFINED on exit, no mrs will be active.
;Uses:
;	none.
;Exceptions:
;	none.
;
;***************************************************************************
cProc	NextMrsFile_All,<PUBLIC,FAR,NODATA>	
cBegin	<nogen> 				
	sub	ax,ax				; do find empty unnamed
	jmp	short NextMrsFile_Common	
cEnd	<nogen> 				

cProc	NextMrsFile,<PUBLIC,FAR,NODATA> 	
cBegin	<nogen> 				
	mov	ax,sp				; don't find empty unnamed
cEnd	<nogen> 				

?DFP = DFP_NONE 				; don't smash regs on entry
cProc	NextMrsFile_Common,<PUBLIC,FAR,NODATA>	
cBegin
?DFP = DFP_CP					; restore switch
	; NOTE: shares exit with PrevMrsFile
	push	ax				; preserve flag
	push	[grs.GRS_oMrsCur]
	cCall	MrsDeActivate
	GETRS_SEG   es,bx,<SIZE,LOAD>		
	pop	bx
	.errnz	UNDEFINED - 0FFFFh
	inc	bx				;want first module?
	jnz	Got_oMrs			;brif not

	mov	bx,OMRS_GLOBAL + 1		; start looking w/1st entry
						;   after the global mrs
Got_oMrs:
	dec	bx				;bx = oMrs for global mrs
	RS_BASE add,bx
NextMrs_Loop:
	mov	bx,PTRRS[bx.MRS_oMrsNext]
	mov	ax,bx				;retval in case of no more mrs's
	inc	bx
	.errnz	UNDEFINED - 0FFFFH
	jz	NextMrs_Exit			;brif no more mrs's in table

	dec	bx

	RS_BASE add,bx
	test	BPTRRS[bx.MRS_flags2],FM2_File
	jz	NextMrs_Loop			;brif not a File mrs

	cmp	PTRRS[bx.MRS_ogNam],OGNAM_UNNAMED ; UNNAMED mrs?
	jnz	@F				; brif not

	pop	cx				; fetch flag
	push	cx				; restore to stack for
						;	next time thru loop
	jcxz	@F				; brif want ALL file mrs's

	call	EmptyMrs			; cx = 0 if no empty unnamed
	GETRS_SEG   es,ax,<SIZE,LOAD>		; refresh ES
	jcxz	@F				; brif unnamed mrs not empty

	jmp	short NextMrs_Loop		; skip empty unnamed mrs
@@:						

	RS_BASE sub,bx				;pMrs --> oMrs
	push	bx				; parm to MrsActivateCP
Skip_Empty_Unnamed:				; shared exit point
	cCall	MrsActivateCP			
NextMrs_Exit:
	pop	cx				; clean flag off stack
	mov	ax,[grs.GRS_oMrsCur]
cEnd


;***************************************************************************
; NextTextPrsInMrs
; Purpose:
;	Find and activate the next prs in current module for which there
;	is a text table.
; Entry:
;	grs.oPrsCur = oPrs of current prs (UNDEFINED if caller wants oPrs of
;	     1st prs in module.
;	grs.oMrsCur identifies current module
; Exit:
;	next prs is loaded into prsCur,
;	ax = grs.oPrsCur = UNDEFINED if there are no more prs's in this mrs
;		with text tables
;	else it is the offset into the Rs table for the current prs.
;Uses:
;	none.
;Exceptions:
;	none.
;
;***************************************************************************
cProc	NextTextPrsInMrs,<PUBLIC,FAR,NODATA>
cBegin
	mov	cx,sp			;want only prs's w/text tables
	jmp	short NextPrs_Shared
cEnd	<nogen>

;***************************************************************************
; NextPrsInMrs
; Purpose:
;	Find and activate the next prs in current module
; Entry:
;	grs.oPrsCur = oPrs of current prs (UNDEFINED if caller wants oPrs of
;	     1st prs in module.
;	grs.oMrsCur identifies current module
; Exit:
;	next prs is loaded into prsCur,
;	ax = grs.oPrsCur = UNDEFINED if there are no more prs's in this mrs.
;	else it is the offset into the Rs table for the current prs.
;Uses:
;	none.
;Exceptions:
;	none.
;
;***************************************************************************
cProc	NextPrsInMrs,<PUBLIC,FAR,NODATA>
cBegin
	xor	cx,cx			;want all valid prs's
NextPrs_Shared:
	mov	ax,[grs.GRS_oPrsCur]
	mov	bx,[grs.GRS_oMrsCur]
	push	cx			;save fText
	call	GetNextPrsInMrs		;ax = next prs
					;or UNDEFINED if no more
	cCall	PrsActivateCP,<ax>	;activate current entry
					;or deactivate prs if ax=UNDEFINED
	pop	cx
	jcxz	NextPrs_IgnoreText	;brif don't care about text tbl

	cmp	[grs.GRS_oPrsCur],UNDEFINED
	jz	NextPrs_IgnoreText	;brif beyond last prs in mrs

	test	[txdCur.TXD_flags],FTX_mrs
	jnz	NextPrs_Shared		;brif prs has no text table

NextPrs_IgnoreText:
	mov	ax,[grs.GRS_oPrsCur]
cEnd

;***
;FreeAllUnrefdPrs
;
;Purpose:
;	This frees all prs entries which have no more references to them.
;	Calls PrsFree for each prs that doesn't have PRS_otxDef = UNDEFINED.
;	This is done in case the user does something which causes a prs
;	entry to be made for which there is no reference in a text table
;	(such as simply typing 'foo' in direct mode - - - don't want a
;	prs for SUB foo hanging around ...).
;Entry:
;	none.
;Exit:
;	Since it is called from Executors, we preserve grs.fDirect
;	and grs.oRsCur.
;Uses:
;       none.
;Exceptions:
;       none.
;
;*******************************************************************************
cProc	FreeAllUnrefdPrs,<PUBLIC,FAR,NODATA>
cBegin
	xor	cx,cx
	xchg	cl,[fCouldBeBogusPrs]	;reset, and get existing setting to test
	jcxz	FreeAllUnrefdPrs_Exit	;brif no chance of bogus prs's around
					;  (this is a speed optimization)
	push	WORD PTR [grs.GRS_fDirect]
	push	[grs.GRS_oRsCur]
	call	FreeAllUndefinedPrs	;call common code to do the work
	call	RsActivateCP
	PopfDirect ax			;set fDirect to pushed value
FreeAllUnrefdPrs_Exit:
cEnd

;***
;PPrsOPrs
;Purpose:
;	Given an oPrs, return a pointer to the actual prs entry.
;Entry:
;	ax = oPrs
;Exit:
;	bx = pPrs
;	if FV_FAR_RSTBL
;	    es is set to the seg of the prs (i.e., retval is es:bx)
;	ax = oPrs (same as input)
;Exceptions:
;	none.
;Preserves:
;	All but bx
;
;***************************************************************************
PPrsOPrs	PROC	NEAR
public	PPrsOPrs
	assumes ds,NOTHING
	DbChk	oPrs,ax 		;ife RELEASE - ensure oPrs is valid
	test	conFlags,F_CON_StaticStructs
	jz	PrsInTable		;brif static structs are disabled

	mov	bx,dataOFFSET prsCur	;assume oPrs is for prsCur
	SETSEG_EQ_SS	es		;es:bx points to desired prs
	cmp	ax,[grs.GRS_oPrsCur]
	jz	Got_pPrs		;brif oPrs is for prsCur

PrsInTable:
	GETRS_SEG   es,bx,<SIZE,LOAD>	
	mov	bx,ax
	RS_BASE add,bx
Got_pPrs:
	ret
	assumes ds,DATA
PPrsOPrs	ENDP

sEnd	CP				
sBegin	SCAN				
	assumes CS,SCAN 		

;***
;PPrsOPrsSCAN
;Purpose:
;	Given an oPrs, return a pointer to the actual prs entry.
;	Added as revision [46].
;	[69] For FV_SBSWAP versions, sets sbRsScan to the sb of the
;	[69] segment containing the prs, to facilitate refreshing this
;	[69] this segment value.
;Entry:
;	ax = oPrs
;Exit:
;	bx = pPrs
;	if FV_FAR_RSTBL
;	    es is set to the seg of the prs (i.e., retval is es:bx)
;	ax = oPrs (same as input)
;Exceptions:
;	none.
;Preserves:
;	All but bx
;
;***************************************************************************
PPrsOPrsSCAN	PROC	NEAR
public	PPrsOPrsSCAN
	assumes ds,NOTHING
	DbChk	oPrs,ax 		;ife RELEASE - ensure oPrs is valid
	DbChk	ConStatStructs		

	mov	bx,dataOFFSET prsCur	;assume oPrs is for prsCur
	SETSEG_EQ_SS	es		;es:bx points to desired prs
	cmp	ax,[grs.GRS_oPrsCur]
	jz	@F			;brif oPrs is for prsCur

	GETRS_SEG   es,bx,<SIZE,LOAD>	
	mov	bx,ax
	RS_BASE add,bx
@@:
	ret
	assumes ds,DATA
PPrsOPrsSCAN	ENDP

sEnd	SCAN				
sBegin	CP				
	assumes CS,CP			

;***
;FFreePrs
;Purpose:
;	Given an oPrs, return zero in AX if it is a free prs
;	This is useful when a routine wants to activate a caller's prs
;	if it has not been freed during the course of the routine.
;Entry:
;	ax = oPrs
;Exit:
;	bx = pPrs
;	ax = zero if invalid prs, non-zero if valid prs
;	condition codes set based on value in AX
;Exceptions:
;	none.
;Preserves:
;	All but bx
;***************************************************************************
FFreePrs PROC	NEAR
	DbChk	ConStatStructs
	mov	bx,dataOFFSET prsCur	;assume oPrs is for prsCur
	SETSEG_EQ_SS	es
	cmp	ax,[grs.GRS_oPrsCur]
	jz	Got_pPrs1		;brif oPrs is for prsCur

	GETRS_SEG   es,bx,<SIZE,LOAD>	
	xchg	ax,bx
	RS_BASE add,bx			;bx points to prs in the Rs table
Got_pPrs1:
	mov	ax,PTRRS[bx.PRS_ogNam]	; ax = UNDEFINED if free prs
	inc	ax			;ax (and condition codes) = zero if free
	ret
FFreePrs ENDP

;***
;RsActivateIfNotFree
;Purpose:
;	Activate a register set if it is not a free prs
;	This is useful when a routine wants to activate a caller's prs
;	if it has not been freed during the course of the routine.
;	If oRs is a free prs, the current oRs is left active.
;Entry:
;	ax = oRs
;Exit:
;	none.
;Exceptions:
;	none.
;
;***************************************************************************
PUBLIC	RsActivateIfNotFree
RsActivateIfNotFree PROC NEAR
	or	ax,ax
	jns	NotAPrs			;brif activating an MRS
	push	ax
	and	ah,7Fh			;ax = oPrs (instead of an oRs)
	call	FFreePrs		;see if it is a free prs
	pop	ax			;restore ax = oRs
	jz	ItsFree			;brif prs is not active
NotAPrs:
	cCall	RsActivateCP,<ax>
ItsFree:
	ret
RsActivateIfNotFree ENDP

;***
;FieldsOfPrs
;Purpose:
;	Given an oPrs, get its oNam, Fields, and flags without changing 
;	the current prs.
;
;	Note: it costs nothing for this routine to return a pointer to the
;	prs in bx, BUT: caller beware! This represents a pointer either to
;	prsCur, OR into the global table of prs's. If the latter, then
;	the knowledge of what information is valid when in the table and
;	how to use it is best left internal to the contextmgr.
;Entry:
;	oPrs
;Exit:
;	ax = oNam (offset into the MODULE name table)
;	bx = pointer to the prs
;	cx = oPrs (same as input parm)
;	dl = procType
;	dh = proc's flags (only useful to masm callers)
;	es = seg of the prs (far Rs table versions only)
;
;	NOTE: The code that creates a new prs explicitly calls the namemgr
;		to create a name entry, even though it doesn't need the oNam.
;		This ensures that FieldsOfPrs doesn't need an out-of-memory
;		error return.
;Exceptions:
;	none.
;Preserves:
;	es
;***************************************************************************
cProc	FieldsOfPrs,<PUBLIC,NEAR,NODATA>
	parmW	oPrs
cBegin	FieldsOfPrs
	mov	ax,[oPrs]
	push	ax			;save for retval
	push	es			; preserve
	call	PPrsOPrs		;bx = pPrs
	DbHeapMoveOff			;assert no heap movement here

	push	bx			;save pPrs across call
	push	es			;save seg of Rs table across call
	push	PTRRS[bx.PRS_ogNam]	
	call	ONamOfOgNam		; get oNam for this prs

	DbHeapMoveOn
	DbAssertRel ax,nz,0,CP,<Out of memory error in FieldsOfPrs>
	pop	es
	pop	bx
	mov	dl,BPTRRS[bx.PRS_procType]
	mov	dh,BPTRRS[bx.PRS_flags]
	pop	es
	pop	cx			;return input oPrs
cEnd	FieldsOfPrs

cProc	FieldsOfPrsFar,<PUBLIC,FAR,NODATA>
	parmW	oPrs
cBegin	FieldsOfPrsFar
	cCall	FieldsOfPrs,<[oPrs]>
cEnd	FieldsOfPrsFar

;***
;SetPrsField
;Purpose:
;	Given an oPrs, an offset into the prs and a 16-bit value, set
;	the field at the given offset in the prs to that value.
;	This function is provided to eliminate the requirement to
;	activate and then deactivate a prs to set one field.
;
;	NOTE: This routine should not be used to set fields in the txd,
;	unless the caller first ensures that the prs (and thus, the txd)
;	is not active.
;
;Entry:
;	oPrs, oField, wValue
;Exit:
;	ax = oPrs (same as input)
;Exceptions:
;	none.
;Preserves:
;	es
;***************************************************************************
cProc	SetPrsField,<PUBLIC,NEAR,NODATA>,<es>
	parmW	oPrs
	parmW	oField
	parmW	wValue
cBegin	SetPrsField
	mov	ax,[oPrs]
	call	PPrsOPrs		;bx = pPrs, ax = oPrs
	add	bx,[oField]		;bx now points to desired field
	mov	cx,[wValue]		;fetch given value
	mov	PTRRS[bx],cx		;set the field with given value
cEnd	SetPrsField

;***
;OMrsORs
;Purpose:
;	Given an oRs, return the oMrs for the associated module.
;Entry:
;	ax = oRs
;Exit:
;	ax = oMrs for the module of the given oRs
;Exceptions:
;	none.
;Preserves:
;	all but bx and es
;***************************************************************************
	PUBLIC OMrsORs
OMrsORs	PROC NEAR
	test	ah,080H			;is this already an oMrs?
	jz	OMrsORs_Exit		;  brif so
	and	ah,07FH			;ax = an oPrs
	DbChk	oPrs,ax
	call	PPrsOPrs		;bx = pPrs
	mov	ax,PTRRS[bx.PRS_oMrs]
OMrsORs_Exit:
	DbChk	oMrs,ax
	ret
OMrsORs	ENDP
	

;***
;EnStaticStructs
;Purpose:
;	Called to ensure that mrsCur, prsCur (if a prs is active), and
;	txdCur are set up correctly for the current context. Used in
;	conjunction with DisStaticStructs.
;Entry:
;	grs.GRS_oRsCur set correctly for current context.
;	if conFlags bit flag F_CON_StaticStructs is reset (FALSE) on
;		entry, it is assumed that mrsCur, prsCur, and txdCur
;		contain no heap owners.
;Exit:
;	conFlags bit flag F_CON_StaticStructs is set (TRUE).
;	mrsCur and txdCur are activated.
;	if high bit of oRsCur is set, oPrsCur is activated.
;	ax = 0 if no action taken, non-zero otherwise. This allows the
;		caller an easy determination as to whether he should call
;		DisStaticStructs at some later point or not.
;***************************************************************************
cProc	EnStaticStructs,<PUBLIC,FAR,NODATA>
cBegin	EnStaticStructs
	xor	ax,ax			; assume no work to do
	test	[conFlags],F_CON_StaticStructs
	jnz	EnStatic_Exit		;brif things are already set correctly

	or	[conFlags],F_CON_StaticStructs
	mov	ax,UNDEFINED
	mov	[grs.GRS_oMrsCur],ax	;so MrsDeActivate doesn't get confused
	mov	[grs.GRS_oPrsCur],ax	;so PrsDeActivate doesn't get confused
	xchg	[grs.GRS_oRsCur],ax
	push	ax			;ax = oRsCur
	call	RsActivateCP		;reactivate mrsCur, prsCur, and txdCur
	mov	ax,sp			; non-zero indicates we changed the
					;  state of things
EnStatic_Exit:
cEnd	EnStaticStructs

;***
;DisStaticStructs
;Purpose:
;	Called to deactivate mrsCur, prsCur, and txdCur, and set a flag
;	such that RsActivateCP will do just the minimal amount of work, 
;	realizing that the actual register sets are in the tables, rather
;	than in their static structures.
;	This is the state of things whenever execution is begun, so procedure
;	call/return speed will be reasonable. GetEsDi fetches ES from the
;	appropriate table based on GRS fields set in this routine.
;Entry:
;	if conFlags bit flag F_CON_StaticStructs is set (TRUE) on
;		entry, it is assumed that mrsCur, prsCur, and txdCur
;		are set up.
;Exit:
;	conFlags bit flag F_CON_StaticStructs is reset (set to FALSE).
;	prsCur, mrsCur, and txdCur are deactivated, but the oRsCur and
;	oMrsCur & oPrsCur fields in grs are left correctly set.
;
;	In non-RELEASE code, mrsCur, prsCur, and txdCur get filled with
;	0FFFFH to help catch cases where code is depending on values in
;	these static structures when deactivated.
;***************************************************************************
cProc	DisStaticStructs,<PUBLIC,FAR,NODATA>
cBegin	DisStaticStructs
	test	[conFlags],F_CON_StaticStructs
	jz	DisStatic_Exit		;brif things are already set correctly

	push	[grs.GRS_oRsCur]	;preserve across MrsDeActivate call
	call	MrsDeActivate		;deactivate mrsCur, prsCur, and txdCur
	and	[conFlags],NOT F_CON_StaticStructs
	call	RsActivateCP		;oRsCur is still on the stack

DisStatic_Exit:
cEnd	DisStaticStructs

;***
; RsActivate, RsActivateCP
; Purpose:
;	Activate a given Module's or Procedure's register set.
;
;	NOTE: This function does one of two very different things based
;		on the current setting of the F_CON_StaticStructs bit flag
;		in 'conFlags'. If static structures are currently being used
;		to maintain mrsCur, prsCur & txdCur, then this just calls
;		MrsActivate or PrsActivate; if not, then the only action
;		is to update some grs fields.
; Entry:
;	parm1: ushort oRsNew - If high bit is set, low 15 bits = oPrs
;	                       else low 15 bits = oMrs to activate
; Exit: Preserves input AX value, and sets PSW based on an OR AX,AX on exit
;
; Preserves:
;	AX
;
;***************************************************************************
cProc	RsActivateCP,<PUBLIC,NEAR,NODATA>
	parmW	oRsNew
cBegin	RsActivateCP
	push	ax			;preserve input value
	mov	ax,[oRsNew]

	test	[conFlags],F_CON_StaticStructs
	jz	No_StaticStr		;brif no static structures

	inc	ax
	js	GotAPrs			;brif not mrs or UNDEFINED
	dec	ax			;ax = oMrs or UNDEFINED
	push	ax
	call	MrsActivateCP
	jmp	SHORT RsActCPExit
GotAPrs:
	dec	ax			;ax = 8000h + oPrs
	and	ah,7Fh			;mask off high bit, ax = oPrs
	push	ax
	call	PrsActivateCP
	jmp	short RsActCPExit

No_StaticStr:				;no static structures; mrsCur and
					;  prsCur are to be found in the Rs tbl
	push	ax
	call	far ptr RsActivateCODEFar
RsActCPExit:
	pop	ax			; restore input value
	or	ax,ax			; set PSW based on input ax, as a
					;   service to some callers
cEnd	RsActivateCP

cProc	RsActivate,<PUBLIC,FAR,NODATA>
	parmW	oRsNew
cBegin	RsActivate
	push	oRsNew
	call	RsActivateCP
cEnd	RsActivate

;NOTE: For EB, the ForEach... varients which take a far pointer cannot be
;NOTE: supported, because code segments could move. The only callers which
;NOTE: use FE_FarCall (below) are the QB User Interface, and a couple of
;NOTE: non-Release routines; these latter are okay for EB, as we'll make the
;NOTE:
;***
;ForEachMrs(pFunction)
;
;Purpose:
;	 Invoke *pFunction for each module, after first making that module the
;	 current module.
;	 If pFunction ever returns FALSE,
;	    ForEachMrs() returns FALSE without examining
;	    any further modules
;	 Else
;	    returns TRUE (non-zero)
;
;     NOTE: This routine is guaranteed to cause no heap movement in and of
;		itself; the function if CALLS may cause heap movement however.
;
;Usage:
;	 boolean ClearModule()
;	 if (ForEachMrs(ClearModule)) {...
;Entry:
;	 pFunction is the FAR address of a the function to be
;	    invoked for each module.
;        the static fMrsTextMask is a flags mask; if we're to search for ALL
;		mrs's (i.e., those for user text objects as well as for modules)
;		then this will be 0, otherwise, it will be set so we can filter
;		out mrs's for which the fText flag is set, and call the given
;		function only for each module. The default is to call it for
;		modules only.
;		NOTE: Even though the flags byte in the mrs structure is a byte,
;			this static mask is a word, so we can keep it in SI - -
;			all the high bits are zero in the mask all the time, so
;			we can safely TEST it against the flags byte as if it
;			were a word.
;Exit:
;	 returns TRUE (non-zero) iff pFunction returned TRUE for each module.
;        ALWAYS restores initial module on exit.
;Uses:
;	none.
;Exceptions:
;	none.
;*******************************************************************************
cProc	ForEachMrs,<PUBLIC,FAR,NODATA>
	parmD	pFunction
cBegin	ForEachMrs
	mov	al,FE_PcodeMrs+FE_CallMrs+FE_FarCall+FE_SaveRs
	les	bx,[pFunction]
	call	ForEachCP
cEnd	ForEachMrs

;***
;ForEachPrsInMrs(pFunction)
;
;Purpose:
;	 Invoke *pFunction for each procedure in the current module after first
;	 making that procedure current.
;	 If pFunction ever returns FALSE,
;	    ForEachPrsInMrs() returns FALSE without examining
;	    any further procedures
;	 Else
;	    returns TRUE (non-zero)
;
;     NOTE: This routine is guaranteed to cause no heap movement in and of
;		itself; the function if CALLS may cause heap movement however.
;
;     NOTE: This routine assumes that, if a prs is active on entry, it can
;		safely be reactivated on exit.
;Usage:
;	 boolean ClearPrs()
;	 if (ForEachPrsInMrs(ClearPrs) {...
;Entry:
;	 pFunction is the FAR address of the function to be
;	    invoked for each procedure in the module.
;Exit:
;	 returns TRUE (non-zero) iff pFunction returned TRUE for each prs.
;	 Always restores initial Mrs and Prs on exit.
;Uses:
;	none.
;Exceptions:
;	none.
;*******************************************************************************
cProc	ForEachPrsInMrs,<PUBLIC,FAR,NODATA>,<SI,DI>
	parmD	pFunction
cBegin	ForEachPrsInMrs
	mov	al,FE_PcodePrs+FE_NoPcodePrs+FE_FarCall+FE_SaveRs
	les	bx,[pFunction]
	call	ForEachCP
cEnd	ForEachPrsInMrs

;*******************************************************************************
;ForEach(flags, pFunction)
;Purpose:
;	Same as ForEachCP
;Entry:
;	flags identifies what is to be done (FE_xxx)
;	pFunction = adr of FAR function to call
;Exit:
;	Same as ForEach
;Uses:
;	none.
;Exceptions:
;	none.
;
;*******************************************************************************
cProc	ForEach,<PUBLIC,FAR>
	parmB	flags
	parmD	pFunction
cBegin
	mov	al,[flags]
	les	bx,[pFunction]
	call	ForEachCP
cEnd

;*******************************************************************************
;ForEachTxtTblInMrs
;Purpose:
;	Same as ForEachCP, except pFunction is called for the
;	mrs's text table, and for every SUB and FUNCTION's text table
;Entry:
;	bx = offset into CP segment to function to be called
;
;*******************************************************************************
cProc	ForEachTxtTblInMrs,<PUBLIC,NEAR>
cBegin	<nogen> 			
	mov	al,FE_CallMrs+FE_PcodePrs+FE_SaveRs
	jmp	SHORT ForEachCP
cEnd	<nogen> 			

;*******************************************************************************
;ForEachTxtTbl
;Purpose:
;	Same as ForEachCP, except pFunction is called for each
;	mrs's text table, and for every SUB and FUNCTION's text table
;Entry:
;	bx = offset into CP segment to function to be called
;
;*******************************************************************************

;*******************************************************************************
;ForEachCP
;Purpose:
;	If pFunction ever returns FALSE,
;	   ForEachCP() returns FALSE without examining any further register sets
;	Else
;	   returns TRUE (non-zero)
;	This function can be called recursively
;NOTE:
;	This routine is guaranteed to cause no heap movement in and of
;	itself; the function it CALLS may cause heap movement however.
;NOTE:
;	None of the ForEach routines invoke the given function with the 
;	global mrs active EVER, regardless of the passed flag combinations.
;Entry:
;	if near (CP) callee, bx = adr of function to call
;	else es:bx = adr of function to call
;	flags identifies what is to be done (FE_xxx)
;	 These flags indicate for what contexts pFunction is to be called:
;	  FE_PcodeMrs	TRUE if mrs's containing pcode are to be visited
;			(NOTE: This does not include INCLUDE mrs's, even
;			 though they have the FM2_NoPcode bit set to 0)
;	  FE_TextMrs	TRUE if FM2_NoPcode mrs's are to be visited (i.e.
;			INCLUDE files, command window's mrs, document files)
;	  FE_CallMrs	TRUE if pFunc is to be called for mrs's text table
;	  FE_PcodePrs	TRUE if prs's with text tables (SUBs/FUNCTIONs)
;			are to be visited
;	  FE_NoPcodePrs	TRUE if DEF FN and DECLARE prs's are to be visited
;	 These flags indicate how pFunction is to be called:
;	  FE_FarCall	TRUE if function to be called is FAR
;			(NOTE: only available in non-Release and non-windows
;				code)
;	  FE_SaveRs	TRUE if ForEach is to restore caller's oRsCur on exit
;
;For Example, (ignoring FE_FarCall and FE_SaveRs):
;	if pFunction is to be called for File (pcode or document):
;	   al=FE_PcodeMrs+FE_TextMrs+FE_CallMrs
;	if pFunction is to be called for every pcode text table in the system
;          and it is to be called for the MRS BEFORE all prs's in that module:
;	   al=FE_PcodeMrs+FE_CallMrs+FE_PcodePrs
;	if pFunction is to be called for every pcode text table in the system
;          and it is to be called for the MRS AFTER all prs's in that module:
;	   al=FE_PcodeMrs+FE_CallMrsAfter+FE_PcodePrs
;	if pFunction is to be called for every pcode text table in the system
;          and it is to be called for the MRS BEFORE AND AFTER all prs's in
;	   that module:
;	   al=FE_PcodeMrs+FE_CallMrs+FE_CallMrsAfter+FE_PcodePrs
;	if pFunction is to be called for every prs in the system:
;	   al=FE_PcodeMrs+FE_PcodePrs+FE_NoPcodePrs
;	if pFunction is to be called for every prs in current module:
;	   al=FE_PcodePrs+FE_NoPcodePrs
;	if pFunction is to be called for every prs in the system which
;	   has no text table (DEF FNs and DECLAREs):
;	   al=FE_PcodeMrs+FE_NoPcodePrs
;Exit:
;	Returns TRUE (non-zero) iff pFunction returned TRUE for each module.
;       If FALSE is returned and caller didn't have FE_SaveRs set in the
;		input flags byte, the active prs and/or mrs is/are for
;		the context from which pFunction returned FALSE.
;Uses:
;	none.
;Exceptions:
;	none.
;
;*******************************************************************************

cProc	ForEachTxtTbl,<PUBLIC,NEAR>
cBegin	<nogen> 			
	mov	al,FE_PcodeMrs+FE_CallMrs+FE_PcodePrs+FE_SaveRs
cEnd	<nogen> 			
	;fall into ForEachCP
;Register usage:
;	si = current oMrs
;	di = current oPrs
;
cProc	ForEachCP,<PUBLIC,NEAR>,<si,di>
	localD	pFunction
	localW	oMrsSave
	localW	oPrsSave
	localB	flags
	localB	fDoneWithPrss
cBegin
	DbChk	ConStatStructs
	mov	[flags],al
	mov	[OFF_pFunction],bx
	mov	[SEG_pFunction],es
	mov	ax,[grs.GRS_oMrsCur]	;save caller's register set
	mov	[oMrsSave],ax
	mov	ax,[grs.GRS_oPrsCur]	;save caller's register set
	mov	[oPrsSave],ax

	mov	di,UNDEFINED		;start with 1st prs in mrs
	test	[flags],FE_PcodeMrs OR FE_TextMrs
	jz	ThisMrsOnly		;brif not visiting all mrs's
	mov	si,OMRS_GLOBAL		; start with 1st mrs, skip gMrs
MrsLoop:
	GETRS_SEG   es,bx,<SIZE,LOAD>	
	RS_BASE add,si
	mov	si,PTRRS[si.MRS_oMrsNext]
	inc	si			;Any more mrs's left?
	jnz	NotDone 		;  brif not
	jmp	FeExitTrue		;exit if beyond end of table

NotDone:
	dec	si
	mov	bx,si
	RS_BASE add,bx			; swapped these ...
	cmp	[grs.GRS_oMrsCur],si	; ... two instructions
	jnz	MrsNotActive		;brif desired mrs is not mrsCur

	mov	bx,dataOFFSET mrsCur
	SETSEG_EQ_SS	es		;mrsCur is in DGROUP, not rs table seg
MrsNotActive:
	mov	al,FE_PcodeMrs
	test	BPTRRS[bx.MRS_flags2],FM2_NoPcode OR FM2_Include
	jz	GotPcodeMrs		;brif got a REAL pcode mrs (not ascii
					; text or INCLUDE file)
	shl	al,1			;al = FE_TextMrs
.errnz	FE_TextMrs - (FE_PcodeMrs + FE_PcodeMrs)
GotPcodeMrs:
	test	[flags],al
	jz	MrsLoop			;brif this mrs is not to be visited

	cCall	MrsActivateCP,<si>	;activate next entry
;We're interested in this mrs.  Call pFunction with one or more of:
; - the mrs's active,
; - each prs which has a text table active,
; - each prs which has no text table active.
;
ThisMrsOnly:
	mov	[fDoneWithPrss],0
	call	PrsDeactivate		;make module's text table active
	test	[flags],FE_CallMrs
	jnz	CallFunc		;brif pFunction is to be called for mrs

PrsLoop:
	xchg	ax,di			;ax = current oPrs
	mov	bx,[grs.GRS_oMrsCur]	;cx = module of interest
	call	GetNextPrsInMrs		;ax = next prs
	xchg	di,ax			;save oPrs in di
	or	di,di
	js	PrsDone			;brif no more prs's
	mov	al,FE_PcodePrs
	GETRS_SEG   es,bx,<SIZE,LOAD>	
	mov	bx,di			;bx = oPrs
	RS_BASE add,bx			;bx = pPrs of prs we propose activating
	;NOTE: [13] we can safely add the table base here because we explicitly
	;NOTE: [13] called PrsDeactivate, above
	DbAssertRel di,nz,grs.GRS_oPrsCur,CP,<ForEachCP: active prs in PrsLoop>
	cmp	PTRRS[bx.PRS_txd.TXD_bdlText_status],NOT_OWNER
	jnz	GotPcodePrs		;brif got a prs with pcode-text-table
					; i.e. a SUB or FUNCTION
	shl	al,1			;al = FE_NoPcodePrs
.errnz	FE_NoPcodePrs - (FE_PcodePrs + FE_PcodePrs)
GotPcodePrs:
	test	[flags],al
	jz	PrsLoop			;brif this prs is not to be visited
CallFunc:
	cCall	PrsActivateCP,<di>	;activate next prs (or activate module
					; if di==UNDEFINED
	test	[flags],FE_FarCall
	jnz	CallFarFunc
	mov	bx,[OFF_pFunction]
	call	bx			;call given NEAR function
	jmp	SHORT CallRet

CallFarFunc:
	cCall	pFunction		;call given FAR function
CallRet:
	or	ax,ax			;is retval FALSE?
	jz	FeExit			;  brif so
	cmp	[fDoneWithPrss],0
	jne	DoneWithThisMrs		;brif just called for MRS after PRSs
	test	[flags],FE_PcodePrs OR FE_NoPcodePrs
	jnz	PrsLoop			;brif visiting prs's
PrsDone:
	call	PrsDeactivate		;make module's text table active
	mov	[fDoneWithPrss],1	;remember that we're done with this
					;module's prss
	test	[flags],FE_CallMrsAfter
	jnz	CallFunc		;brif pFunction is to be called for mrs
DoneWithThisMrs:
	test	[flags],FE_PcodeMrs OR FE_TextMrs
	jz	FeExitTrue
	jmp	MrsLoop			;brif visiting mrs's
					;ax is still = pFunction's return value
FeExitTrue:
	mov	ax,sp			;return TRUE (non-zero)
FeExit:
	xchg	ax,si			;save ret value in si

	test	[flags],FE_SaveRs
	jz	FeNoRestore		;brif caller wants to restore oRs
	push	[oMrsSave]
	cCall	MrsActivateCP		;activate caller's mrs
	push	[oPrsSave]
	cCall	PrsActivateCP		;activate caller's prs

	;Note that saving grs.oRsCur on entry and calling RsActivate
	;on exit is not sufficient, because if the caller's prs was
	;not defined, RsActivate would not activate the caller's mrs.

FeNoRestore:
	xchg	ax,si			;ax = return value
	or	ax,ax			;set condition codes for caller
cEnd

;***
;ForEachPrsInPlaceCP, ForEachPrsInPlaceCPSav
;
;Purpose:
;	This function walks through each prs, and passes a POINTER to the
;	current PRS to the specified function.	This function is guaranteed
;	not to cause ANY heap movement of and by itself.  The called function
;	is responsible for any adjustment to the entry pPrs to account for
;	heap movement that it may cause.
;	[63] If the called function returns AX == 0 then
;	     the ForEach is aborted. Note: called function
;	     must return with CC reflecting contents of AX.
;
;	This routine exists primarily to speed up text editting.  It can
;	be used when an innocuous action needs to be taken for each PRS
;	in the system (eg - checking PRS flags for particular conditions).
;	This routine is SIGNIFICANTLY faster than the generalized ForEach
;	construct since it just walks the table of Prs's instead of activating
;	each PRS individually.
;
;	ForEachPrsInPlaceCPSav - restores callers oRs on Exit
;
;Entry:
;	bx = CP adr of function to call
;Exit:
;	ax = last value return by the called function
;	Condition Codes reflect contents of ax
;Uses:
;	none.
;Exceptions:
;	none.
;*******************************************************************************
cProc	ForEachPrsInPlaceCPSav,<NEAR,PUBLIC,NODATA>
cBegin	ForEachPrsInPlaceCPSav
	push	[grs.GRS_oRsCur]	;remember current oRs for RsActivate
	call	ForEachPrsInPlaceCP
	pop	bx			
	push	ax			
	push	bx			
	call	RsActivateCP		;oRsCur already on stack
	pop	ax			
	or	ax, ax			
cEnd

cProc	ForEachPrsInPlaceCP,<NEAR,PUBLIC,NODATA>,<SI,DI>
cBegin	ForEachPrsInPlaceCP
	xchg	bx,di			;di = function address
	call	PrsDeactivate		;deactivate prs
	mov	si,[oPrsFirst]		;first prs in Rs table
	GETRS_SEG   es,bx,<SIZE,LOAD>	

	DbHeapMoveOff			;assert no heap movement in this section

	;------------------------------------------------------
	;Now,	si == offset to current entry
	;	di == CP addr of ForEach function
	;------------------------------------------------------
	push	sp			; TRUE return value (when no prs's)
PrsInPlaceLoop:
	inc	si			;any entries remaining in table?
	.errnz	UNDEFINED - 0FFFFH
	jz	PrsInPlaceExit		;end of table

	dec	si
	RS_BASE add,si
	pop	ax			; toss last return value
	call	di			;call function
	push	ax			; save return value
	jz	PrsInPlaceExit		; Exit and return Z
	GETRS_SEG   es,bx,<SIZE,LOAD>	

	mov	si,PTRRS[si.PRS_oPrsNext]
	jmp	PrsInPlaceLoop		;loop to consider next entry

PrsInPlaceExit:

	DbHeapMoveOn			;heap movement allowed again

	pop	ax			; Return value
	or	ax,ax			

cEnd	ForEachPrsInPlaceCP

;***
; ValidORs_Check
; Purpose:
;	Given what might be an oRs, return FALSE if it matches oMrsCur or
;	oPrsCur.
;	Used in conjunction with ValidORs.
; Entry:
;	grs.GRS_oRsCur
;	
; Exit:
;	AX = FALSE if oRs looks valid, otherwise TRUE.
;
;***************************************************************************
cProc	ValidORs_Check,<NEAR,NODATA>
cBegin	ValidORs_Check
	mov	ax,sp			;non-zero
	mov	bx,[oRsTest]
	cmp	bx,[grs.GRS_oRsCur]
	jnz	ValidORs_Check_Exit	;brif oRs's don't match

	or	bx,bx
	jns	ValidORs_Check_Done	;brif oRs not an oPrs - - - success
	cmp	[prsCur.PRS_procType],PT_DEFFN
	jz	ValidORs_Check_Done	;brif oRs for an active DEF FN - okay
	test	[txdCur.TXD_flags],FTX_mrs
	jnz	ValidORs_Check_Exit	;brif oRsCur is for prsCur, but the
					;  active text table for mrsCur.
ValidORs_Check_Done:
	xor	ax,ax			;success
ValidORs_Check_Exit:
cEnd	ValidORs_Check

;***
; ValidORs
; Purpose:
;	Given what might be an oRs and an oTx, tell caller if they look
;	valid or not.
;	If they look valid, activate the oRs.
;	[10] NOTE: given the oRs for the global mrs (i.e., OMRS_GLOBAL),
;	[10]	ValidOrs will say that it looks Invalid.
; Entry:
;	[di+4] = possible oRs
;	[di+2] = possible oTx
; Exit:
;	AX != 0 if oRs and oTx don't look valid; otherwise,
;		AX = 0, and the given oRs is activated.
;
;***************************************************************************
cProc	ValidORs,<PUBLIC,NEAR,NODATA>
cBegin	ValidORs
	mov	ax,[di+4]
	mov	[oRsTest],ax
	mov	al,FE_PcodeMrs+FE_CallMrs+FE_PcodePrs+FE_NoPcodePrs
	push	cs
	pop	es
	mov	bx,OFFSET CP:ValidORs_Check
	call	ForEachCP		;returns AX = 0 if oRs valid
cEnd	ValidORs

sEnd CP

sBegin CODE
	assumes CS,CODE


;***
; RsActivateCODE
; Purpose:
;	Activate a given Module's or Procedure's register set.
;
;	NOTE: This function is only callable when static structures are
;		disabled.
; Entry:
;	ax = oRsNew - If high bit is set, low 15 bits = oPrs
;	              else low 15 bits = oMrs to activate
; Exit:
;	Updates the oRsCur, oMrsCur, oPrsCur and offsetTxdSeg fields in the 
;	grs structure as appropriate, based on the input oRs.
;	if FV_FAR_RSTBL, es is set to the seg of the Rs table on exit.
;	if SizeD, ds is set to the seg of the variable table for mrsCur.
;***************************************************************************
PUBLIC RsActivateCODE
RsActivateCODE	PROC	NEAR
	DbChk	oRs,ax
	mov	[grs.GRS_oRsCur],ax
	GETRS_SEG   es,bx,<SIZE,LOAD>	

	or	ax,ax
	js	Activate_oPrs		;brif oRsNew is an oPrs

	mov	[grs.GRS_oMrsCur],ax
	mov	bx,ax
	RS_BASE add,bx			;bx = pMrsCur
	mov	[grs.GRS_pMrsCur],bx	;set up to save code when accessing
	mov	[grs.GRS_pRsCur],bx	;set up to save code when accessing
	add	ax,MRS_txd.TXD_bdlText_seg
					;ax = offset in tRs to the segment
					;     address of the current text table
	mov	[grs.GRS_oPrsCur],UNDEFINED

	mov	[grs.GRS_offsetTxdSeg],ax

	DbChk	ConNoStatStructs
;start of [20]
;end of [20]
	ret

Activate_oPrs:
	and	ah,7Fh			;mask off high bit, ax = oPrs
	mov	[grs.GRS_oPrsCur],ax
	mov	bx,ax			;bx = oPrsNew
	RS_BASE add,bx			;bx = pPrsNew
	mov	cx,PTRRS[bx.PRS_oMrs]
	mov	[grs.GRS_oMrsCur],cx
	mov	[grs.GRS_pRsCur],bx
	add	cx,[grs.GRS_bdRs.BD_pb]
	mov	[grs.GRS_pMrsCur],cx
	DbChk	ConNoStatStructs
	cmp	PTRRS[bx.PRS_txd.TXD_bdlText_status],NOT_OWNER
	jz	Activate_oPrs_Def	;brif oPrsNew is for a DEF FN/DECLARE

	add	ax,PRS_txd.TXD_bdlText_seg
					;ax = offset in Rs table to the segment
					;     address of the current text table
	mov	[grs.GRS_offsetTxdSeg],ax
;start of [20]
;end of [20]
	ret

Activate_oPrs_Def:
	sub	cx,[grs.GRS_bdRs.BD_pb]
	add	cx,MRS_txd.TXD_bdlText_seg
	mov	[grs.GRS_offsetTxdSeg],cx
	ret
RsActivateCODE	ENDP

;***
; RsActivateCODEFar
; Purpose:
;	Activate a given Module's or Procedure's register set.
;	Far interface to RsActivateCODE
;
;	NOTE: This function is only callable when static structures are
;		disabled.
; Entry:
;	ax = oRsNew - If high bit is set, low 15 bits = oPrs
;	              else low 15 bits = oMrs to activate
;
;***************************************************************************
cProc	RsActivateCODEFar,<PUBLIC,FAR,NODATA>
	parmW	oRsNew
cBegin	RsActivateCODEFar
assumes ds,NOTHING
	mov	ax,[oRsNew]
	call	RsActivateCODE
assumes ds,DATA
cEnd	RsActivateCODEFar

sEnd	CODE


	end
