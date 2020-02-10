   TITLE VarUtil.asm - Variable Management utilities in native code

;***
;VarUtil.asm - Variable Management utilities in native code
;
;	Copyright (C) 1986-1989, Microsoft Corporation
;
;Purpose:
;	Provide utility functions for the variable manager in native code
;
;
;*******************************************************************************

	.xlist
	include version.inc
	VarUtil_ASM = ON	;don't include EXTRNs defined in this file
	includeOnce	architec
	includeOnce	conint
	includeOnce	context
	includeOnce	executor
	includeOnce	heap
	includeOnce	names
	includeOnce	qbimsgs
	includeOnce	rtinterp
	includeOnce	scanner
	includeOnce	util
	includeOnce	variable
	.list

;	.sall

;These assertions are made because MakeVariable returns two sets of error
;codes on the assumptions that the ER_ and MSG_ messages it uses will all
;fit in a single byte.
.erre	255d GT MSG_COM
.erre	255d GT MSG_BadElemRef 		
.erre	255d GT MSG_UndElem
.erre	255d GT MSG_ASRqd1st

assumes DS,DATA
assumes ES,DATA
assumes SS,DATA
assumes CS,CP

sBegin	DATA
	globalW oValComMax,0		;Max oVal for all BLANK Common
					; declarations
	globalW oTypComMax,0		;Max oTyp for all BLANK Common
					; declarations

	staticW	oVarPrev,0		;oVar returned by last call to
					; FirstVar or NextVar (for NextVar)
	staticW	oVarTHash,UNDEFINED	;offset to start of appropriate
					; hash table          (for NextVar)
	staticW	iHashCur,0		;index in appropriate hash table to
					; current hash chain  (for NextVar)
	staticW	iHashMax,0		;max valid index hash index based
					; on current context  (for NextVar)
	;These two flags are used to share COMMON support code
	staticB	fResetCommon,FALSE	
	staticB	fCreateCommon,TRUE	;FALSE if we just want MakeCommon to
					;  FIND a COMMON block, not make one
	externW	pSsCOMcur		;used by AdjustCommon
	staticB fQlbCommon,0		; set if common in quicklibs

	externW	vm_fVarFound		;these are used by StdSearch
	externW	vm_fPVCur
	externW vm_pVarCur
	externW vm_oVarCur
	externW vm_oVarTmp
	staticW	oTypNew,0		;here instead of localW so StdSearch
					;  doesn't have to push a frame
	staticW	oVarShared,0		;here instead of localW so StdSearch
					;  doesn't have to push a frame
NMALLOC label	BYTE
	DB	"NMALLOC",0		;used by B$GetNMALLOC
	CB_NMALLOC EQU 7		;size of NMALLOC string

;	Create a public sd to NMALLOC for FindNMalloc to use.
	globalW sdNMALLOC,CB_NMALLOC	
	staticW ,<dataOFFSET NMALLOC>	
sEnd	DATA



	externFP B$ULGetCommon 	;finds user library common blocks
	externFP B$IRTCLR		; release all compiled AD's & SD's,
					; and zero QuickLib vars
	externFP B$IERASE		;QB-specific array ERASE routine
	externFP B$STDL 		;releases a given string (sd)

sBegin	CP

;##############################################################################
;#							  		      #
;#			 	Table Allocation       			      #
;#							  		      #
;##############################################################################

;***
;MakeMrsTVar() - Create module variable table & set up tMV hash table
;Purpose:
;	Allocates a buffer of size CBINITMVHASH, with mrsCur.bdVar as the owner.
;	Sets up the first CBINITMVHASH bytes as a hash table for the tMV; the
;	hash table entries are all initialized to 0.
;
;	Also allocates space for typmgr structures, i.e., information regarding
;	user-defined types and elements. This space is managed by the typmgr
;	component, which, like the varmgr, can grow the vartable and add type
;	and element structures and never needs to move or remove these 
;	structures.
;
;	The tMV hash table comes first, followed by CBINIT_TTYP bytes of table
;	overhead required by the typmgr. This overhead consistes of an offset
;	to the first TYP structure, and a count of TYP structures in the table
;	(both of these are initialized to zero).  VAR, TYP, and ELEM struct 
;	space is then allocated by the varmgr and typmgr as required.
;
;Entry:
;	mrsCur is set up; it is assumed that the bdVar field does NOT currently
;	contain a heap owner.
;
;Exit:
;	FALSE is returned if there is insufficient memory,
;	else table is successfully allocated.
;
;Exceptions:
;	none.
;
;******************************************************************************
cProc	MakeMrsTVar,<NEAR,PUBLIC,NODATA>
cBegin	MakeMrsTVar
	mov	bx,dataOFFSET mrsCur.MRS_bdVar
	DbChk	BdNotOwner,bx		;ensure that given bd isn't an owner now
	push	bx
	PUSHI	ax,CBINITMVHASH+CBINIT_TTYP
	PUSHI	ax,IT_VAR		;alloc a table large enough for tMV
	call	BdAllocVar		;  hash table plus typmgr requirements
	or	ax,ax
	jz	MakeMrsTVar_Exit	;brif allocation failed

	.erre	(CBINITMVHASH+CBINIT_TTYP) GT ET_MAX
					;ensure offset to first TYP struct
					;  is larger than biggest predefined
					;  oTyp value
	mov	ax,CBINITMVHASH+CBINIT_TTYP
	mov	[mrsCur.MRS_oPastLastVar],ax
					;initialize to same as cbLogical
	push	[mrsCur.MRS_bdVar.BD_pb] ;start of hash table
	push	ax			;CBINITMVHASH+CBINIT_TTYP
	call	ZeroFill		;initialize module-level hash table
					;  and typmgr data to zeroes
	mov	ax,sp			;non-zero == TRUE == success
MakeMrsTVar_Exit:
cEnd	MakeMrsTVar

;***
;MakePrsTVar - Put a procedure hash table into mrsCur.bdVar for prsCur
;Purpose:
;	To be called when a new prs is created, adds and initializes a hash
;	table of size CBINITPVHASH at the end of mrsCur.bdVar, placing the 
;	offset into bdVar to this hash table into prsCur in the oVarHash field.
;
;	NOTE: We only create a prs hash table for those prs's with text tables,
;	NOTE: plus DEF FN's (i.e., we DON'T create prs hash tables for 
;	NOTE: DECLARE's). This saves DGROUP space for user library DECLARE's. 
;
;Entry:
;	mrsCur is set up; it is assumed that bdVar is a heap owner.
;	prsCur is set up; its oVarHash field is assumed to contain garbage
;
;Exit:
;	FALSE is returned if there is insufficient memory,
;	else table is successfully allocated.
;
;Exceptions:
;	none.
;
;******************************************************************************
cProc	MakePrsTVar,<FAR,PUBLIC,NODATA>,<SI>	
cBegin	MakePrsTVar
	mov	bx,dataOFFSET mrsCur.MRS_bdVar
	DbChk	BdOwner,bx		;ensure that given bd is an owner

	.errnz	MRS_bdVar - MRS_bdVar.BD_cbLogical
	mov	si,[bx]
	
	DbAssertRel	grs.GRS_oPrsCur,nz,UNDEFINED,CP,<MakePrsTVar: err 1>
	DbAssertRel	prsCur.PRS_oVarHash,z,UNDEFINED,CP,<MakePrsTVar: err 2>

	xor	ax,ax			;in case of error return
	cmp	si,08000H - CBINITPVHASH
	jae	MakePrsTVar_Exit	;brif 32k limit on module var table
					;  exceeded

	push	bx			;pointer to mrsCur.MRS_bdVar
	PUSHI	ax,CBINITPVHASH
	call	BdGrowVar		;allocate space for proc. hash table
	or	ax,ax
	je	MakePrsTVar_Exit	;brif error return

	mov	[prsCur.PRS_oVarHash],si ;input cbLogical, i.e., offset to start
					 ;  of procedure hash table
	add	si,[mrsCur.MRS_bdVar.BD_pb]
	push	si
	PUSHI	ax,CBINITPVHASH
	call	ZeroFill
	mov	ax,sp			;non-zero == TRUE, i.e., success
MakePrsTVar_Exit:
cEnd	MakePrsTVar

;***
;FirstVar - return the pVar and oVar of the first var in the current table
;Purpose:
;	This routine is used in conjunction with NextVar (below). It 
;	returns the oVar and pVar to the first variable in the current
;	tPV or tMV, and sets up some static variables for subsequent calls
;	to NextVar.
;
;Input:
;	none.
;Ouptut:
;	if AX = 0, no (more) variables in current table
;	else AX = oVar, BX = pVar.
;Modifies:
;	none
;***************************************************************************
	PUBLIC FirstVar
FirstVar	PROC NEAR
	mov	[iHashMax],CBINITMVHASH	;assume no procedure active
	xor	ax,ax			;assume no procedure active
	cmp	[grs.GRS_oPrsCur],UNDEFINED
	jz	No_Prs_Active

	mov	ax,[prsCur.PRS_oVarHash]
	mov	[iHashMax],CBINITPVHASH
	
No_Prs_Active:
	mov	[oVarTHash],ax		;module hash table starts at offset 0
	mov	[iHashCur],-2		;so shared code will inc to 0
	mov	dx,[mrsCur.MRS_bdVar.BD_pb]
	jmp	short End_Of_Hash_Chain	;share code with NextVar, below
FirstVar	ENDP

;***
;NextVar - return the pVar and oVar of the next var in the current table
;Purpose:
;	This routine is called repetitively to access each variable in
;	the current procedure or module variable table (tPV or tMV).
;
;	Note: This code is written to account for the fact that heap 
;		movement can occur between calls.
;Input:
;	none.
;Ouptut:
;	if AX = 0, no (more) variables in current table
;	else AX = oVar, BX = pVar.
;Modifies:
;	none
;***************************************************************************
	PUBLIC NextVar
NextVar	PROC NEAR
	DbChk	ConStatStructs		;static structs must be active
	mov	dx,[mrsCur.MRS_bdVar.BD_pb]
NextVar_Clear:
	mov	bx,[oVarPrev]
	DbAssertRel bx,nz,0,CP,<NextVar called w/o FirstVar call first>
	add	bx,dx			;bx = pVarPrev
	mov	ax,[bx.VAR_oHashLink]	;ax = oVarNext
	and	ax,0FFFEH		;mask off low bit
	jz	End_Of_Hash_Chain	;brif end of this hash chain

	mov	bx,ax
NextVar_Exit:
	add	bx,dx			;ax = oVar, bx = pVar
	mov	[oVarPrev],ax		;update static
	ret

End_Of_Hash_Chain:			; callable entry point
					; preserves dx, es
	;Search the hash table for the start of the next non-empty chain
	mov	cx,[iHashCur]
	mov	ax,[oVarTHash]
	add	ax,dx			;ax points to base of hash table
TryNextChain:
	mov	bx,ax
	inc	cx
	inc	cx			;increment index
	cmp	cx,[iHashMax]		;have we checked all chains?
	jz	Next_Var_Done		; brif so - - 

	DbAssertRel cx,b,iHashMax,CP,<NextVar - iHashCur or iHashMax hosed>
	add	bx,cx			;bx points to start of next hash chain
	mov	bx,[bx]			;bx = oVar for first var in chain or
					;  0 if chain is empty
	or	bx,bx	
	jz	TryNextChain		;current chain is empty - try the next

	mov	[iHashCur],cx
	mov	ax,bx			;ax = bx = oVar
	jmp	short NextVar_Exit

	
	ret
Next_Var_Done:
	xor	ax,ax
	ret
NextVar	ENDP

;***
;ClearPV, ClearMV - CLEAR all variables in the given procedure
;
;ClearPV:
;	Purpose:
;		Called via ForEachPrsInPlaceCPSav.
;		CLEAR all static variables in the tPV for the given prs.
;	Entry:
;		SI = pPrs to be cleared (which is NOT prsCur - - - it's in 
;		the table).
;ClearMV:
;	Purpose:
;		Called via ForEachCP
;		CLEAR all static variables in the tMV for mrsCur
;	Entry:
;		none.
;
;Exit:
;	AX != 0 (needed for ForEachCP)
;******************************************************************************
cProc	ClearMV,<NEAR,PUBLIC,NODATA>,<di> 
	localW	oMrsCur
cBegin	
	mov	[iHashMax],CBINITMVHASH
	xor	cx,cx
	jmp	short ClearPVorMV	;jump into ClearPV; share exit
cEnd	<nogen>

cProc	ClearPV,<NEAR,PUBLIC,NODATA>,<di> 
	localW	oMrsCur
cBegin	ClearPV
	GETRS_SEG es			
	mov	cx,PTRRS[si.PRS_oVarHash]   
	inc	cx
	.errnz	UNDEFINED - 0FFFFH
	jz	ClearPV_Exit		;brif no hash table for this prs

	dec	cx
	mov	[iHashMax],CBINITPVHASH

	mov	ax,PTRRS[si.PRS_oMrs]	
	mov	[oMrsCur],ax		;save for use by CbTypOTypOMrs
	mov	bx,ax
	RS_BASE add,ax			; ax = pMrs of given prs
	GETRS_SEG es			
	mov	di,sp			; assume prs not in current mrs
	cmp	bx,[grs.GRS_oMrsCur]	;prs in current mrs?
	jnz	Clear_GotPMrs		;  brif not

ClearPVorMV:
	mov	dx,[grs.GRS_oMrsCur]
	mov	[oMrsCur],dx		;save for use by CbTypOTypOMrs
	mov	ax,dataOFFSET mrsCur
	SETSEG_EQ_SS es 		; es = ss if far Rs tables
	sub	di,di			; remember to refresh es == ss
					; after any calls that hose es
Clear_GotPMrs:
	push	si			;preserve for caller
	xchg	ax,si
	mov	[oVarTHash],cx		;module hash table starts at offset 0
	mov	[iHashCur],-2		;so shared code will inc to 0
	mov	dx,PTRRS[si.MRS_bdVar.BD_pb] ;dx points to base of var table
	DbAssertRel dx,nz,0,CP,<ClearMV/ClearPV: no var table in mrsCur (NULL)>
	DbAssertRel dx,nz,UNDEFINED,CP,<ClearMV/ClearPV: no var table in mrsCur>
	call	End_Of_Hash_Chain	;get first var, ax, = oVar, bx = pVar

Clear_Table_Loop:
	SETSEG_EQ_SS es 		; assume we want to refresh es == ss
	or	di,di			; is that assumption correct?
	jz	Clear_Table_Got_Es	;   brif so

	GETRS_SEG es			
Clear_Table_Got_Es:
	or	ax,ax
	jz	Clear_Table_Exit	;brif no more vars in table

	mov	ax,[bx.VAR_flags]	;cache for multiple tests below
	test	al,FVFUN
	jnz	Clear_Table_Next	;brif Function or Def FN entry

	test	ah,FVVALUESTORED SHR 8
	jz	Clear_Table_Next	;brif value not stored in entry

	test	ah,FVCONST SHR 8
	jz	Clear_Table_Cont	;brif not a CONST

	test	BPTRRS[si.MRS_flags],FM_VARNEW	
	jz	Clear_Table_Next	;brif we're not discarding this var 
					;  table (don't clear CONST values)
Clear_Table_Cont:
	test	ah,FVARRAY SHR 8
	jz	Clear_Not_Array		;brif not an array entry

	test	BPTRRS[si.MRS_flags],FM_VARNEW	
	jz	@F			;brif we're not discarding var table
	and	[bx.VAR_value.ASTAT_ad.AD_fFeatures],NOT FADF_STATIC
					;tell runtime to deallocate this
					;array, not just erase it 
@@:
	lea	ax,[bx.VAR_value.ASTAT_ad]
	push	dx			; save across call
	DbHeapMoveOff			;assert no heap movement here
	cCall	B$IErase,<ax>		;erase the array
	DbHeapMoveOn
	pop	dx			
	jmp	short Clear_Table_Next

ClearPV_Exit:
	jmp	short Clear_Exit

Clear_Not_Array:
	lea	cx,[bx.VAR_value]
	GetOtyp	ax,[bx]			;ax = oTyp for this variable

	cmp	ax,ET_SD
	jnz	Clear_Not_SD		;brif not clearing an SD

	push	ax			;preserve across call
	push	cx			; preserve across call
	push	dx			; preserve across call
	cCall	B$STDL,<cx>		;release the SD
	pop	dx			
	pop	cx			
	pop	ax
Clear_Not_SD:
	push	bx			; save pVar
	mov	bx,[oMrsCur]		;in case we're clearing a prs in some
					;  module other than mrsCur
	call	CbTypOTypOMrs		; returns ax = cb for given type
	pop	bx			; restore pVar
	jnz	Clear_ZeroFill		

	; fixed-length string - - - get count of bytes to fill from var entry
	mov	ax,[bx.VAR_cbFixed]	
Clear_ZeroFill:
	push	dx			; preserve across call
	cCall	ZeroFill,<cx,ax>	
	pop	dx			
Clear_Table_Next:
	call	NextVar_Clear		;ax = oVar or UNDEFINED, bx = pVar
	jmp	Clear_Table_Loop
	
Clear_Table_Exit:
	pop	si
Clear_Exit:
	or	ax,sp			;non-zero exit, per interface
cEnd	ClearPV

;***
;CbTyp(oTyp) - Return the size of a value of given type
;Purpose:
;	This routine returns the number of bytes of data required for
;	the input type. Note that this will work for both predefined and
;	user-defined types.
;Input:
;	oTyp
;Output:
;	ax = cbTyp - i.e., the size of a value of the given type.
;Modifies:
;	none
;***************************************************************************
cProc	CbTyp,<NEAR,PUBLIC,NODATA>
	parmW	oTyp
cBegin	CbTyp
	mov	ax,[oTyp]
	call	CbTypOTyp		;returns with result in AX
cEnd	CbTyp

;***
;CbTypOTyp, CbTypOTypOMrs
;Purpose:
;	This routine returns the number of bytes of data required for
;	the input type. Note that this is called directly from the scanner
;	use CbTyp (above) for a C interface to this routine.
;
;	CbTypOTyp assumes that if the oTyp is a user-defined type, it is
;			an offset into mrsCur.MRS_bdVar.
;	CbTypOTypOMrs uses the MRS_bdVar table in the mrs whose oMrs is
;			given in bx.
;Input:
;	ax = oTyp
;	for CbTypOTypOMrs, bx = oMrs of type table
;Output:
;	ax = cbTyp
;	for input oTyp of ET_FS, ax = zero.
;	PSW flags set based on an OR AX,AX on exit
;Prserves:
;	all (even bx)
;***************************************************************************
;The CbTyp code below was significantly reworked throughout for revision [9]
mpCbTyp label byte
	DB	0		;ET_IMP hole
	.errnz	ET_IMP - 0
	DB	2		;ET_I2
	.errnz	ET_I2  - 1
	DB	4		;ET_I4
	.errnz	ET_I4  - 2
	DB	4		;ET_R4
	.errnz	ET_R4  - 3
	DB	8		;ET_R8
	.errnz	ET_R8  - 4
	DB	SIZE SD 	;ET_SD
	DB	0		;ET_FS - - - can't tell size from ET_ type

	.errnz	ET_SD - 5
	.errnz	ET_FS - 6

	.errnz	ET_MAX - ET_FS	;Ensure this is found if someone adds a type.

	PUBLIC	CbTypOTypOMrs
	PUBLIC	CbTypOTyp
CbTypOTypOMrs	PROC	NEAR
	push	bx
	jmp	short CbTypOTyp_Cont
CbTypOTyp:
	push	bx
	mov	bx,[grs.GRS_oMrsCur]	
	DbChk	oTyp,ax 		;sanity check on input oTyp
CbTypOTyp_Cont:
	cmp	ax,ET_MAX		;Is it a fundamental type?
	ja	NotPredefinedType	;  brif not - user defined

	mov	bx,offset mpCbTyp	;base of lookup table in CS
	xlat	byte ptr cs:[bx]	;al == desired size
	pop	bx
	or	ax,ax			;set PSW flags
	ret

NotPredefinedType:
	test	[conFlags],F_CON_StaticStructs
	jz	Mrs_In_Table		;brif mrsCur not set up

	cmp	bx,[grs.GRS_oMrsCur]
	jz	Want_MrsCur		;brif passed oMrs is for mrsCur

Mrs_In_Table:
	RS_BASE add,bx			; bx points into Rs table
	GETRS_SEG es			
	jmp	short Got_pMrs

Want_MrsCur:				;ax is an offset into type table
	lea	bx,mrsCur		;  found in the current mrs
	SETSEG_EQ_SS es 		
Got_pMrs:
	add	ax,PTRRS[bx.MRS_bdVar.BD_pb] ;[2] ax = pTyp
	xchg	bx,ax			;bx = oTyp, ax = garbage
	mov	ax,[bx].TYP_cbData	;ax = cbData from type table entry
	pop	bx
	or	ax,ax			;set PSW flags
	ret
CbTypOTypOMrs	ENDP


;***
;StdSearch() - search the appropriate hash table with standard algorithm
;Purpose:
;	Search the appropriate table (tPV or tMV) in the typical case.
;Entry:
;	vm_fPVCur - module static flag, TRUE if we're to search tPV, FALSE if 
;			tMV.
;	mrsCur.bdVar assumed set up, and if vm_fPVCur is TRUE, prsCur is 
;		assumed	to be set up, and the oVarHash field is either 
;		UNDEFINED (in which case we just return), or contains an 
;		offset into mrsCur.bdVar to the tPV hash table.
;	mkVar set up as per MakeVariable (below).
;Exit:
;	FALSE = no error
;	otherwise, the same error code is returned as described for MakeVariable
;
;	If no error is returned, then the static vm_fVarFound indicates 
;		success or failure. 
;	If vm_fVarFound == TRUE, vm_oVarCur is set to the offset into 
;		mrsCur.bdVar to the found variable entry, and vm_pVarCur 
;		points to the entry.
;Exceptions:
;	none.
;******************************************************************************
	PUBLIC	StdSearch
StdSearch	PROC	NEAR
	mov	[vm_fVarFound],FALSE		;initialize
	mov	bx,[mkVar.MKVAR_oNam]
	and	bx,HASH_MV_NAMMASK
	.errnz	(HASH_MV_NAMMASK AND HASH_PV_NAMMASK) - HASH_PV_NAMMASK
	cmp	[vm_fPVCur],FALSE
	jz	StdSearch_Cont1			;brif no prs active

	and	bx,HASH_PV_NAMMASK
	add	bx,[prsCur.PRS_oVarHash]
StdSearch_Cont1:
	push	si
	mov	si,[mrsCur.MRS_bdVar.BD_pb]	;ptr to base of var table
	mov	ax,[bx][si]
	or	ax,ax				;empty hash chain?
	jz	StdSearch_Exit_2		;brif so; ax already 0 for retvl

	push	di
	mov	di,ax
	mov	WORD PTR [oTypNew],UNDEFINED	;if something else on exit and
						;  search succeeded, set
						;  mkVar.MKVAR_oTyp to oTypNew
	add	di,si
	mov	cx,[mkVar.MKVAR_oNam]
StdSearch_Loop:
	cmp	si,di				;end of hash chain?
	je	False_Exit			;  brif so

	cmp	[di.VAR_oNam],cx
	je	Got_oNam			;brif oNam's match
Next_Var_Entry1:
	mov	di,[di.VAR_oHashLink]		;offset to next entry in chain
	and	di,0FFFEH			;mask off low bit
	add	di,si				;offset ==> pointer
	jmp	short StdSearch_Loop

StdSearch_Exit_2:
	jmp	StdSearch_Exit_1		;ax already 0 for retval

False_Exit:
	sub	ax,ax
	jmp	StdSearch_Exit

Shared_Or_Const:
	TESTX	cx,<FVSTATIC OR FVFORMAL OR FVCONST>	
	jnz	StdSearch_DD_Err		; Duplicate definition
	jmp	short StdSearch_Cont2

Got_oNam:
	;register usage:
	;	BX = oTyp  for current entry
	;	CX = di.VAR_flags 	i.e., flags for current entry
	;	DX = mkVar.MKVAR_flags	i.e., flags from input
	
	mov	dx,[mkVar.MKVAR_flags]
	mov	cx,[di.VAR_flags]
	TESTX	dx,<FVSHARED OR FVCONST>	
	jnz	Shared_Or_Const			;brif input is nmodule shared

StdSearch_Cont2:
	GetOtyp	bx,[di]				;bx = oTyp for this variable
	mov	ax,[mkVar.MKVAR_oTyp]
	cmp	bx,ax				;do types match?
	jnz	StdSearch_Cont2a		; brif not

	cmp	ax,ET_FS			
	jnz	StdSearch_Cont3a		; brif not fixed-length string

	push	ax				
	mov	ax,[di.VAR_cbFixed]		
	cmp	ax,[mkVar.MKVAR_fsLength]	
	pop	ax
	jz	StdSearch_Cont3a		; branch if lengths match

StdSearch_Cont2a:
	TESTX	dx,FVASCLAUSE			
	jnz	StdSearch_DD_Err		; brif input AS bit set

	TESTX	cx,<FVDECLDVAR OR FVCONST>	
	jnz	Has_Name_Space			;brif CONST or declared in an
						;  AS clause or FUNCTION name
	TESTX	dx,FVCONST			
	jnz	RP_DD_Err
	jmp	short Next_Var_Entry		;types don't match


StdSearch_DD_Err:
	mov	ax,PRS_ER_RE OR ER_DD
StdSearch_Err_Exit:
	jmp	StdSearch_Exit

StdSearch_Cont3a:
	TESTX	dx,FVASCLAUSE			
	jz	StdSearch_Cont3 		; brif not AS clause

	TESTX	cx,<FVFUN OR FVCONST>		; brif entry flags have a bit
	jne	StdSearch_DD_Err		;  inconsistent w/AS clause

	mov	ax,PRS_ER_RE OR MSG_ASRqd1st	;assume AS clause NOT in 1st ref
	TESTX	cx,FVDECLDVAR			
	jz	StdSearch_Err_Exit		;brif error
	jmp	short OTyp_Matches

Has_Name_Space:
	TESTX	dx,FVIMPLICIT			
	jnz	Has_Name_Space_1		;brif input implicity typed

	cmp	ax,ET_SD			;is input oTyp ET_SD?
	jnz	RP_DD_Err			;brif not - error

	cmp	bx,ET_FS			;is entry oTyp a Fixed Length
	jnz	RP_DD_Err			;  string? brif not - error
Has_Name_Space_1:
	inc	ax
	.errnz	UNDEFINED - 0FFFFH
	jnz	Has_Name_Space_3		;brif input oTyp != UNDEFINED

	cmp	bx,ET_MAX
	ja	Has_Name_Space_2		;brif entry type not predefined

	TESTX	cx,FVCONST			
	jz	RP_DD_Err			;brif entry not a CONST - error
Has_Name_Space_2:
	cmp	bx,ET_FS			
	jnz	Has_Name_Space_3		; brif entry oTyp not F.L. String
RP_DD_Err:
	mov	ax,PRS_ER_RP OR ER_DD
	jmp	StdSearch_Exit

Owns_Name_Space:
	TESTX	dx,<FVCOMMON OR FVSHARED OR FVDIM OR FVSTATIC>	
	jz	OTyp_Matches

	test	cl,FVFUN
	jnz	StdSearch_DD_Err

	test	dl,FVSTATIC
	jnz	StdSearch_DD_Err

	test	dl,FVCOMMON
	jz	AS_Rqd_Error

	TESTX	cx,FVARRAY			
	jz	StdSearch_DD_Err
AS_Rqd_Error:
	mov	ax,PRS_ER_RE OR MSG_ASRqd
	jmp	StdSearch_Exit
Has_Name_Space_3:
	mov	[oTypNew],bx			;give actual type back to user

StdSearch_Cont3:
	TESTX	cx,FVDECLDVAR			
	jnz	Owns_Name_Space			;brif entry owns name space

;At this point, we know that the oNam and oTyp matches - - - now check flags
OTyp_Matches:
	TESTX	dx,FVFORCEARRAY			
	jnz	Inp_Definite_Array		;brif input is a definite array

StdSearch_Cont4:
	TESTX	dx,FVINDEXED			
	jz	Not_Indexed			;brif input var not indexed

	TESTX	cx,<FVARRAY OR FVFUN>		
	jnz	StdSearch_Cont5

Next_Var_Entry:
	mov	cx,[mkVar.MKVAR_oNam]		;ditto
	jmp	Next_Var_Entry1

Inp_Definite_Array:
	or	dx,FVINDEXED
	mov	[mkVar.MKVAR_flags],dx		;ensure FVINDEXED flag is set

	TESTX	cx,FVFUN			; is entry a FUNCTION?
	jnz	StdSearch_DD_Err1		;brif so - error

	TESTX	cx,FVARRAY			; is entry an array?
	jz	Next_Var_Entry
;[J1]	Check if current entry is VALUESTORED
;[J1]		if NOT VALUE STRORED then it is either a $DYNAMIC or an Auto
;[J1]			array in which case adding a DIM is okay.
;[J1]		if VALUESTORED that means that space for the var has already
;[J1]			been allocated and another check must be made to see
;[J1]			if it is a STATIC or DYNAMIC.
;[J1]	NOTE:  We must perform the this test first before we make the
;[J1]	       next check that tests if current entry is a static array
;[J1]	       because it is not valid if not FVVALUESTORED.
	TESTX	cx,FVVALUESTORED		;[J1] Is there an array desc.?
	jz	StdSearch_Cont5			;[J1] brif no array desc.

;[J1]	There is an array descriptor present so now we have to see if it is
;[J1]	static or dynamic.  If it static then we have to make sure that this
;[J1]	is not a DIM because then this would be a double definition.  If
;[J1]	dynamic then multiple DIMs are okay.
	test	BYTE PTR [di.VAR_value.ASTAT_ad.AD_fFeatures],FADF_STATIC
	jnz	Check_For_Dim_Err		;[J1] Check if this is a DIM
	jmp	short StdSearch_Cont5		;[J1] accept it.

StdSearch_DD_Err1:
	jmp	StdSearch_DD_Err

RP_DD_Err1:
	jmp	RP_DD_Err

Not_Indexed:
	TESTX	dx,FVFORCEARRAY			; is input a definite array?
	jnz	Check_For_Dim_Err		;  brif so

	TESTX	cx,FVARRAY			; is entry an array?
	jnz	Next_Var_Entry			;  brif so - no match

Check_For_Dim_Err:
	TESTX	dx,FVDIM			; input found in a DIM 
						;   statement?
	jnz	StdSearch_DD_Err1		;  brif so - DD error

StdSearch_Cont5:
	TESTX	cx,FVCONST			; is entry a CONST?
	jnz	Entry_Is_Const			;  brif so

	;now ensure that a proc ref. doesn't match a retval:
	cmp	[vm_fPVCur],FALSE
	jz	StdSearch_Cont6			;brif no prs active

	TESTX	cx,FVFUN			; is entry a FUNCTION or 
						;  DEF FN?
	jz	StdSearch_Cont6			;brif not

	TESTX	dx,FVLVAL			; input seen on left side 
						;  of eq.?
	jnz	StdSearch_Cont6			;brif so - - this is a retval
	jmp	Next_Var_Entry			;don't allow a ref. to match
						;  a retval
StdSearch_Cont6:
	TESTX	cx,FVARRAY			; is entry an array?
	jnz	StdSearch_Array_Checks		;brif so
	jmp	StdSearch_Cont7

Entry_Is_Const:
	FVTEMP EQU FVCOMMON OR FVSTATIC OR FVSHARED OR FVFORMAL OR FVFNNAME
						;shorthand, so all will fit on
						;  on line!
	TESTX	dx,<FVTEMP OR FVFUNCTION OR FVLVAL OR FVDIM OR FVASCLAUSE> 
	jnz	RP_DD_Err1			;brif any of the above flags set

	or	[mkVar.MKVAR_flags2],MV_fConstFound
						;remember we've seen a CONST
	jmp	short StdSearch_Cont6

StdSearch_Array_Checks:
	;ensure the existing array entry was built assuming at least as many
	;dimensions as actually exist. In addition, consider that we might
	;have found a module shared entry here, in which case we must make our
	;checks with the actual module level entry

	;new register use: bx = pVarEntry, i.e., ptr to the actual array entry
	;                  cx is updated if bx changes
	;                  dx = 0 if bx doesn't change, oVar of SHARED entry
	;                         if so

	xor	dx,dx				;assume pVar is correct (flag)
	mov	bx,di				;assume pVar is correct (pVar)
	cmp	[vm_fPVCur],FALSE
	jz	Not_PVCur			;brif procedure not active

	TESTX	cx,FVSHARED			; is this a SHARED entry?
	jz	Not_PVCur			;  brif not

	mov	dx,di
	sub	dx,si				;dx = oVar of proc. SHARED entry
	mov	bx,[di.VAR_value]		;ax = oVar
	add	bx,si				;ax = pVar of module entry
	mov	cx,[bx.VAR_flags]		;update for this entry	
Not_PVCur:
	mov	[oVarShared],dx			;0 in typical case
	mov	al,[bx.VAR_value.ASTAT_cDims]
	mov	ah,[mkVar.MKVAR_cDimensions]
	cmp	ah,al
	jz	StdSearch_Cont7a1		;brif entry cDims matches input

	or	ah,ah
	jz	StdSearch_Cont7a1		;brif input cDims == 0 (i.e.,
						;  if we don't want to change
						;  existing entry)
	TESTX	cx,FVVALUESTORED		; is entry a static variable?
	jz	StdSearch_Cont7a1		;  brif not - - no problem,
						;  var entry size is okay
	or	al,al				;entry cDims == 0?
	jnz	Wrong_Num_Subscripts		;  brif not - - - error

	cmp	ah,1				;input cDims == 1?
	jnz	Check_Var_Size			;  brif not

	mov	BYTE PTR [bx.VAR_value.ASTAT_cDims],1
						;already enough space, just
						;update cDims in var entry
StdSearch_Cont7a1:
	jmp	StdSearch_Cont7a
Check_Var_Size:
	test	cl,FVSTATIC			;was var declared STATIC?
	jz	Redirect_Array_Var		;  brif not

	cmp	ah,8
	jbe	Redirect_Array_Var		;brif okay - redirect entry to
						;  one that's big enough for
						;  new cDims count
Wrong_Num_Subscripts:
	mov	ax,PRS_ER_RE OR MSG_SubCnt	;'Wrong number of subscripts'
	jmp	StdSearch_Exit

Redirect_Array_Var:
	and	[bx.VAR_value.ASTAT_ad.AD_fFeatures],NOT FADF_STATIC
						;so B$IErase will deallocate

	mov	ax,bx
	sub	ax,si
	push	ax				;save oVar across call

	push	cx				;save entry flags across call	

	add	bx,VAR_value.ASTAT_ad		;bx points to array descriptor
	push	bx
	call	B$IERASE			;deallocate existing array

	pop	cx				;restore entry flags

	;Now create the new larger entry
	mov	ax,UNDEFINED			;assume module level
	cmp	[oVarShared],0			;special case of SHARED in prs?
	jnz	Create_The_Var			;  brif so

	cmp	[vm_fPVCur],FALSE		;procedure active?
	jz	Create_The_Var			;  brif not

	mov	ax,[prsCur.PRS_oVarHash]
Create_The_Var:
	push	[mkVar.MKVAR_oTyp]		;preserve in case of error
	mov	dx,[oTypNew]
	inc	dx
	.errnz	UNDEFINED - 0FFFFH
	jz	Create_The_Var1			;brif oTyp not changed

	dec	dx
	mov	[mkVar.MKVAR_oTyp],dx		;in case entry oTyp different
						;  from assumed one
Create_The_Var1:
	push	ax				;oVarHash
	push	cx				;entry flags
	call	CreateVar			;create larger array var entry
	pop	[mkVar.MKVAR_oTyp]		;restore in case of error
	pop	bx				;oVar saved from before erase
	or	ax,ax
	jnz	StdSearch_Exit			;brif some error

	mov	ax,[vm_oVarCur]
	mov	[vm_oVarTmp],ax			;oVar for new entry

	mov	[vm_oVarCur],bx			;existing entry
	push	[vm_fPVCur]
	cmp	[oVarShared],0			;found proc. shared entry, but
						;  need to ReDirect the module
						;  entry it points to?
	jz	Redirect_The_Var		;brif not

	mov	[vm_fPVCur],FALSE		;reference tMV if so
Redirect_The_Var:
	call	ReDirect			;redirect old entry
	pop	[vm_fPVCur]			;restore to entry value

	mov	si,[mrsCur.MRS_bdVar.BD_pb]	;in case of heap movement

	mov	bx,[vm_oVarTmp]			;oVar of newly created entry
	mov	di,[oVarShared]
	or	di,di
	jz	Get_oVarEntry			;brif typical case
	;di contains the oVar for the proc. SHARED entry we were searching for
	;  replace the value field in this entry with the oVar of the new
	;  entry and continue
	add	di,si				;pVar = proc SHARED entry
	mov	[di.VAR_value],bx		;replace oVar in SHARED entry
						;  with new value (old entry
						;  got ReDirected)
	add	bx,si				;pVarDims = new entry
	jmp	short Got_oVarEntry
Get_oVarEntry:
	add	bx,si				;pVarDims = new entry
	mov	di,bx				;pVar = new entry
Got_oVarEntry:

StdSearch_Cont7a:
	mov	al,[mkVar.MKVAR_cDimensions]
	or	al,al
	jz	StdSearch_Cont7			;brif input cDims == 0



	mov	[bx.VAR_value.ASTAT_cDims],al
StdSearch_Cont7:
	
	;note: we must do the following check AFTER we check for array vs.
	;      non-array, or we would be flagging some bogus DD errors

	TESTM	mkVar.MKVAR_flags,<FVSHARED OR FVCONST>	
	jnz	StdSearch_Shared_Or_Const	;brif either flag bit is set

StdSearch_Cont8:
	mov	[vm_fVarFound],TRUE
	mov	ax,[oTypNew]
	inc	ax
	.errnz	UNDEFINED - 0FFFFH
	jz	StdSearch_Cont9			;brif oTyp not changed

	mov	ax,[oTypNew]
	mov	[mkVar.MKVAR_oTyp],ax		;in case entry oTyp different
						;  from assumed one
StdSearch_Cont9:
	mov	[vm_pVarCur],di			;a return value
	sub	di,si
	mov	[vm_oVarCur],di

	sub	ax,ax				;return FALSE - no error
StdSearch_Exit:
	pop	di
StdSearch_Exit_1:
	pop	si
	ret

StdSearch_Shared_Or_Const:
	TESTM	di.VAR_flags,<FVFUN OR FVSTATIC OR FVFORMAL OR FVCONST>	
	jz	StdSearch_Cont8			;brif none of the above are set
	jmp	StdSearch_DD_Err1

StdSearch	ENDP

;***
;MakeVariableFar - Same as MakeVariable, but a far entry point
;Purpose:
;	Added as part of revision [12].
;	See MakeVariable.
;Entry:
;	See MakeVariable.
;Exit:
;	See MakeVariable.
;******************************************************************************/
cProc	MakeVariableFar,<FAR,PUBLIC,NODATA>
cBegin	MakeVariableFar
	call	MakeVariable
cEnd	MakeVariableFar


;***
;OVarOfRetVal
;Purpose:
;	In certain cases, the parser will emit the wrong pcode for return
;	values to FUNCTIONs and DEF FNs, with the result that the pcode will
;	be bound instead to the function reference instead of the return
;	value oVar.  The execute scanner will detect this case, and call this
;	routine for such cases to get the oVar for the return value.
;
;	Note: Guaranteed to cause no heap movement
;Entry:
;	AX = oVar is given for the reference (that was erroneously placed in
;		the pcode).
;Exit:
;	Returns AX = oVar for the return value, or AX has high bit set.
;******************************************************************************/
cProc	OVarOfRetVal,<PUBLIC,FAR>,<DI>		
cBegin
	DbChk	ConStatStructs
	DbHeapMoveOff				;depending on no heap movement
						;  in this routine
	xchg	ax,di				;di = oVar 
	mov	ax,[grs.GRS_oPrsCur]
	inc	ax
	.errnz	UNDEFINED - 0FFFFH
	jz	OVarOfRetVal_Err_Exit		;brif no active procedure
	dec	ax
	cCall	FieldsOfPrs,<ax>		;ax = oNam of prs
						;bx = pPrs (== prsCur)
						;dl = procType of prs
	cmp	dl,PT_SUB
	jz	OVarOfRetVal_Err_Exit		;brif active proc is a SUB

	add	di,[mrsCur.MRS_bdVar.BD_pb]	;di = pVar
	cmp	ax,[di.VAR_oNam]		;was given oVar for a ref. to
						;  prsCur?
	jnz	OVarOfRetVal_Err_Exit		;brif not

	mov	[mkVar.MKVAR_oNam],ax		;setup for MakeVariable call
	mov	al,[prsCur.PRS_oType]
	and	ax,M_PT_OTYPE			
	mov	[mkVar.MKVAR_oTyp],ax		;setup for MakeVariable call
	mov	[mkVar.MKVAR_flags],FVLVAL
	call	MakeVariable			;MUST succeed, because we always
						;  create the retval when we
						;  create a function ref. entry
	DbAssertTst  ah,z,080H,CP,<OVarOfRetVal: MakeVariable returned an error>
	jmp	short OVarOfRetVal_Exit

OVarOfRetVal_Err_Exit:
	or	ah,080H				;signal error return
OVarOfRetVal_Exit:
	DbHeapMoveOn				;heap movement allowed again
cEnd

;***
;AdjustVarTable
;Purpose:
;	This routine is called when a variable table is about to be moved.
;	Due to the overhead that would be required for the runtime to update
;	backpointers to AD's and SD's in static variable tables, we do this
;	work here.
;Input:
;	SI = ptr to the MRS_bdVar.BD_pb field in some mrs
;	DI = adjustment factor
;Output:
;	none
;Modifies:
;	SI
;***************************************************************************
cProc	AdjustVarTable,<FAR,PUBLIC,NODATA>
cBegin	
 	;Calculate the oMrs for this variable table, so we can get
	;at the hash tables for all the prs's
	mov	bx,[grs.GRS_oMrsCur]	;assume tVar is for mrsCur
	mov	ax,si
	mov	si,[si]			;si = pVarTable
	sub	ax,[MRS_bdVar.BD_pb]	;ax = pMrs
	cmp	ax,dataOFFSET mrsCur	;is tVar for mrsCur?
	jz	Got_oMrsBx		;  brif so

	sub	ax,[grs.GRS_bdRs.BD_pb] 
	xchg	ax,bx
Got_oMrsBx:				;bx = oMrs
	mov	ax,UNDEFINED		;start w/first prs in module
PrsAdjustLoop:
	xor	cx,cx			;cx == 0 --> include prs's for DEF FN's
	push	bx			;save oMrs
	call	GetNextPrsInMrs		;ax = an oPrs in module
	js	AdjustMrs		;brif no (more) prs's in this module

	push	ax			;save oPrs
	call	PPrsOPrs		;bx = pPrs
	mov	ax,PTRRS[bx.PRS_oVarHash] 
	inc	ax
	.errnz	UNDEFINED - 0FFFFH
	jz	AdjustNextPrs		;brif this prs has no hash table
	dec	ax

	push	si			;ptr to variable table
	push	ax			;offset to tPV hash table
	push	di			;adjustment factor
	call	AdjustPrsVarTable
AdjustNextPrs:
	pop	ax			;current oPrs - use to fetch next oPrs
	pop	bx			;oMrs of vartable
	jmp	short PrsAdjustLoop

AdjustMrs:
	pop	bx			;clean stack
	push	si			;ptr to variable table
	push	di			;adjustment factor
	call	AdjustMrsVarTable
cEnd	

;***
;oTypOfONamDefault
;Purpose:
;	Given an oNam, return the default oTyp of that name.
;	Note that 'logical first char' implies the third char of a
;	name which starts with 'FN' and the first char of any other name.
;Entry:
;	oNam
;	ps.tEtCur is filled with default types for 26 letters
;Exit:
;	ax = oTyp
;Exceptions:
;	none.
;Preserves:
;	ES
;
;******************************************************************************/
cProc	oTypOfONamDefault,<PUBLIC,FAR>,<ES>	
	parmW	oNam			
cBegin
	push	[oNam]			; pass oNam
	call	GetVarNamChar		;al = 1st logical char of name
	push	ax
	call	GetDefaultType		;al = default oTyp
cEnd

;***
;VarRudeReset - reset module Variable/Type table for Rude Edit
;Purpose:
;	This routine is called as part of descanning to SS_RUDE. The module
;	variable and type tables are reset, i.e., existing variables and types
;	are thrown out, and the table reinitialized.
;Entry:
;	mrsCur.bdVar is currently a heap owner.
;Exit:
;	None. Since the table is at least the same size on entry as it will
;	be on exit, Out of Memory is not possible.
;	The table is set to the same state it was at module creation time.
;Exceptions:
;	none.
;******************************************************************************/
cProc	VarRudeReset,<PUBLIC,NEAR>
cBegin
	PUSHI	ax,<dataOFFSET mrsCur.MRS_bdVar>
	cCall	BdFree

	cCall	MakeMrsTVar
	DbAssertRel  ax,nz,0,CP,<VarRudeReset: MakeMrsTVar returned OM error>
cEnd	VarRudeReset


;###############################################################################
;#                                                                             #
;#                            COMMON Support                                   #
;#                                                                             #
;###############################################################################
; For implementation details, see ..\id\common.doc

;***
;ClearCommon() - CLEAR all variables in all COMMON Blocks
;Purpose:
;	CLEAR all COMMON variables; this includes zeroing numeric vars, 
;	releasing all strings, and ERASing all arrays.
;	Note that this also removes all common blocks (except for blank
;	common) when fResetCommon is TRUE.
;	Note that this skips clearing blank common when fChaining is TRUE.
;Key Assumptions:
;	- Blank (a.k.a. unnamed) COMMON is always present, and is always the
;		first COM structure in bdtComBlk
;Entry:
;	grs.bdtComBlk is assumed to be set up.
;	fChaining flag
;	fResetCommon flag
;	fQlbCommon flag false
;Exit:
;	none.
;Exceptions:
;	none.
;*******************************************************************************
cProc	ClearCommon,<PUBLIC,NEAR,NODATA>,<SI,DI>
cBegin
	;Register Use:	SI points to current COM entry in bdtComBlk
	;		DI points just past last allocated COM entry
	mov	si,[grs.GRS_bdtComBlk.BD_pb]
	mov	di,si
	add	di,[grs.GRS_bdtComBlk.BD_cbLogical]

	cmp	[fChaining],FALSE	;Want to clear blank common too?
	jz	ClearCommon_Cont	;  brif so
ClearCommon_Loop:
	add	si,SIZE COM		;skip to next entry	
ClearCommon_Cont:
	cmp	si,di
	jae	ClearCommon_Exit	;brif no more COM entries

	push	di
	xor	di,di			;tell SsAdjustCommon to release owners
					;  rather than to adjust them
	mov	bx,si
	add	bx,COM_bdType		; point to bdType for this block
	call	SsAdjustCommon
	pop	di

	xor	ax,ax
	test	[mrsCur.MRS_flags],FM_VARNEW	; called from VarDealloc?
	jz	NoBashTypeTable 		; brif not
	mov	[si.COM_bdType.BD_cbLogical],ax ; type table no longer valid
NoBashTypeTable:				

	cmp	[fResetCommon],al	;want to release COMMON block tables?
	jz	Zero_Common		;  brif not - - just set values to zero

	;reset tables to size zero in case this is blank common
	mov	[si.COM_bdType.BD_cbLogical],ax
	cmp	[si.COM_bdValue.BD_cbPhysical],UNDEFINED
	jz	ClearCommon_Reset_Cont	;brif bdValue table is for a U.L. block
					; - - - leave U.L. block bdValue alone
	mov	[si.COM_bdValue.BD_cbLogical],ax
ClearCommon_Reset_Cont:

	cmp	si,[grs.GRS_bdtComBlk.BD_pb]
	jz	ClearCommon_Loop	;don't ever release blank COMMON tables

	lea	ax,[si.COM_bdType]
	cCall	BdFree,<ax>		;free the table of oTyps for this block
	cmp	[si.COM_bdValue.BD_cbPhysical],UNDEFINED
	jz	ClearCommon_Loop	;brif bdValue is not an owner (but is
					;  instead used to hold info on U.L. 
					;  block)
	lea	ax,[si.COM_bdValue]
	cCall	BdFree,<ax>		;free the value table for this block
	jmp	short ClearCommon_Loop	;done

Zero_Common:
	cmp	[si.COM_bdValue.BD_cbPhysical],UNDEFINED ; QuickLib common?
	jne	NotQlb			; brif not -- clear it now
	mov	fQlbCommon,TRUE 	; call B$IRTCLR later to clear it
	jmp	short ClearCommon_Loop	; go back for more

NotQlb: 				; clear the non-QLB common block
	push	[si.COM_bdValue.BD_pb]
	push	[si.COM_bdValue.BD_cbLogical]
	call	ZeroFill
	jmp	short ClearCommon_Loop

ClearCommon_Exit:
	xor	cx,cx			; prepare to clear & test fQlbCommon
	xchg	cl,[fQlbCommon] 	; COMMON block that wasn't deleted?
	jcxz	NoZeroVars		; brif not - don't set vars to zero
	cmp	[fChaining],FALSE	; are we chaining?
	jnz	NoZeroVars		; brif so -- B$CHNINI will do the
					; work for us
	call	B$IRTCLR		
NoZeroVars:				
	DbAssertRel si,z,di,CP,<ClearCommon: End of table not where expected>
cEnd

;***
;ResetCommon() - Deallocate all common blocks
;Purpose:
;	Calls ClearCommon to release all strings and arrays, throws out value
;	and type tables for each common block as well as the common block entry
;	itself, and shrinks the global common block table. If 'fChaining' is
;	FALSE, this is done to the unnamed common block as well, otherwise,
;	that block is excluded. Note that, even in the case where the unnamed
;	block is to be reset, it is still not deallocated; it's buffers are just
;	trimmed to zero. This (along with initialization code) allows us to 
;	always assume that the unnamed block exists, and it's buffers allocated,
;	although perhaps to size zero.
;
;	Note that the value field in COMMON entries in existing variable tables
;	will now be garbage; this is okay, because we know that the scanner will
;	explicitly put the correct information in when it is next invoked - - -
;	the variable tables do NOT need to be accessed by this routine.
;Entry:
;	global flag fChaining set appropriately
;Exit:
;	none.
;Exceptions:
;	none.
;*******************************************************************************
cProc	ResetCommon,<PUBLIC,NEAR,NODATA>
cBegin
	mov	[fResetCommon],TRUE
	call	ClearCommon
	xor	ax,ax
	mov	[fResetCommon],al
	mov	[oValComMax],ax 		;reset max oVar of blank common
	mov	[oTypComMax],ax 		;reset max oTyp of blank common
	mov	[grs.GRS_bdtComBlk.BD_cbLogical],SIZE COM
						;trim global table back so it
						;  just contains blank COMMON
cEnd

;***
;MakeCommon(oNam) - Create COMMON Block if req'd, rtn offset to block
;Purpose:
;	Given an oNam for a COMMON Block (UNDEFINED for the unnamed block),
;	creates the COMMON Block if it does not already exist, and returns an
;	offset into the global table of COMMON Blocks for the specified block.
;	If the block is created, this routine also calls the heap manager to
;	allocate buffers of minimal size for the value and type tables.
;
;Entry:
;	oNam - offset into the module name table for the Block name
;		Note: it is assumed that oNam is valid, i.e., represents
;			an actual name in mrsCur.bdlNam. In many cases, things
;			will work out o.k. even if it is not, but not always.
;	It is assumed that the global table of COMMON Blocks (grs.bdtComBlk)
;		is already a valid heap item. 
;
;Exit:
;	ax = offset into grs.bdtComBlk to the specified block, or UNDEFINED
;		if out of memory.
;
;	Special case: if the static flag 'fCreateCommon' is FALSE, then we
;		don't want to create a new entry, but just find if a given
;		entry is present or not. In this case, ax = offset to specified
;		block or UNDEFINED if the block is not found.
;	
;Exceptions:
;	None. If Out-of-Memory occurs, the logical size of grs.bdtComBlk will be
;	unchanged, and no new buffers will have been allocated.
;*******************************************************************************
cProc	MakeCommon,<PUBLIC,FAR,NODATA>,<SI,DI>
	parmW	oNam
	localW	ogNam			
	localV	bdName,%(SIZE BD)
	localV	bdType,%(SIZE BD)
	localV	bdValue,%(SIZE BD)
cBegin	MakeCommon
	mov	ax,[oNam]		
	inc	ax			; UNDEFINED?
	jz	@F			; brif so - - - set ogNam == 0

	dec	ax			
	cCall	OgNamOfONam,<ax>	
	jnz	@F			; brif no error
	jmp	MakeCommon_Exit 	; OM error adding name to
					;	global name table
@@:					
	mov	[ogNam],ax		
	;Register Use:	SI points to current COM entry in bdtComBlk
	;		DI points just past last allocated COM entry
	mov	si,[grs.GRS_bdtComBlk.BD_pb]
	mov	di,si
	add	di,[grs.GRS_bdtComBlk.BD_cbLogical]

	;First, search table to see if a matching entry exists
	sub	si,SIZE COM		;special 1st-time-thru-loop value
MakeCommon_Loop:
	add	si,SIZE COM
	cmp	si,di
	jae	MakeCommon_Grow		;no more entries - - - no match

	mov	ax,[si.COM_ogNam]	
	cmp	ax,[ogNam]		
	jnz	MakeCommon_Loop		;brif match not found
	jmp	MakeCommon_SI_Exit	;match found

MakeCommon_Grow:
	DbAssertRel	si,z,di,CP,<MakeCommon: SI ne DI @ end of table search>
	cmp	[fCreateCommon],FALSE	;Want a new entry?
	jz	MakeCommon_Err_1Exit1	;  brif not - indicate search failure

	mov	di,[grs.GRS_bdtComBlk.BD_cbLogical]
					;save offset of new entry in si (because
					;  bdtComBlk might move during BdGrow)
	PUSHI	ax,<dataOFFSET grs.GRS_bdtComBlk>
	PUSHI	ax,<SIZE COM>
	call	BdCheckFree		;grow common block table for new entry
					;NOTE: we don't increase cbLogical until
					;NOTE: the entry is built; this way,
					;NOTE: heap movement of this table
					;NOTE: doesn't try to treat random
					;NOTE: garbage now in this new entry
					;NOTE: as heap owners
	or	ax,ax
	jnz	MakeCommon_Grow_OK	;brif BdGrow successful
MakeCommon_Err_1Exit1:
	jmp	MakeCommon_Err_Exit1	;BdGrow failed
MakeCommon_Grow_OK:
	mov	[bdName.BD_pb],NULL
					;in case this is for blank COMMON
	mov	ax,[oNam]
	inc	ax
	.errnz	UNDEFINED - 0FFFFH
	jz	MakeCommon_Got_Name	;brif it is blank COMMON - bdName is set

	dec	ax
	lea	bx,[bdName]
	cCall	CopyONamBd,<ax,bx>	;returns ax = 0 if OM error
	or	ax,ax
	jnz	MakeCommon_Got_Name	;brif no error
	jmp	MakeCommon_Err_Exit2
MakeCommon_Got_Name:
	lea	ax,[bdType]
	push	ax
	PUSHI	ax,0
	PUSHI	ax,IT_NO_OWNERS
	call	BdAlloc			;allocate type table
	or	ax,ax
	jz	MakeCommon_OM_Free1	;if OM error, free all bd's

	mov	si,di			;si = di = offset to new COM entry
	;now call runtime to determine if we should create our own value
	;  table for this block, or if we should use an existing U.L. block
	call	RtPushHandler
	mov	ax,CPOFFSET MakeCommon_RT_Return
	call	RtSetTrap
	lea	ax,[bdName]
	cCall	B$ULGetCommon,<ax>	;always returns ax = 0, plus
					;  bx = 0 if no match, or 
					;  bx = pbBlock, dx = cbBlock
MakeCommon_RT_Return:	
	call	RtPopHandler		;note this preserves ax, bx, & dx
	or	ax,ax			
	jnz	MakeCommon_OM_Free1	;brif runtime error

	or	bx,bx
	jz	Alloc_Value_Table	;brif no match - - allocate our own

	add	si,[grs.GRS_bdtComBlk.BD_pb]
	mov	[si.COM_bdValue.BD_pb],bx
	mov	[si.COM_bdValue.BD_cbPhysical],UNDEFINED
					;cbPhysical == UNDEFINED is what
					; indicates this is a U.L. block
	mov	[si.COM_bdValue.BD_cbLogical],dx
	jmp	short Move_Name_n_bdType
Alloc_Value_Table:
	lea	ax,[bdValue]
	push	ax
	PUSHI	ax,0
	PUSHI	ax,IT_COMMON_VALUE
	call	BdAllocVar		;allocate COMMON Value table in var heap
	or	ax,ax
	jnz	MakeCommon_Cont		;brif no OM error
MakeCommon_OM_Free1:
	jmp	short MakeCommon_OM_Free

MakeCommon_Cont:
	;Success. Now just move the owners, and exit
	add	si,[grs.GRS_bdtComBlk.BD_pb]
	lea	ax,[bdValue]
	lea	bx,[si.COM_bdValue]
	cCall	BdChgOwner,<ax,bx>	;move bdValue into bdtComBlk
Move_Name_n_bdType:
	add	[grs.GRS_bdtComBlk.BD_cbLogical],SIZE COM
					;MakeCommon succeeded - reflect new
					;  table size based on addition of
					;  this new entry
					;NOTE: do this BEFORE these next two
					;NOTE: BdChgOwner calls so non-RELEASE
					;NOTE: code won't complain about finding
					;NOTE: owners past cbLogical in the
					;NOTE: grs.GRS_bdtComBlk table
	mov	ax,[ogNam]		
	mov	[si.COM_ogNam],ax	
	lea	ax,[bdName]		
	cCall	BdFree,<ax>		
	lea	ax,[bdType]
	lea	bx,[si.COM_bdType]
	cCall	BdChgOwner,<ax,bx>	;move bdType into bdtComBlk
	xor	ax,ax
	mov	[si.COM_oTypCur],ax	;initialize
	mov	[si.COM_oValCur],ax
MakeCommon_SI_Exit:
	xchg	ax,si			;ax points to COM struct
	sub	ax,[grs.GRS_bdtComBlk.BD_pb] ;retval is offset into bdtComBlk
MakeCommon_Exit:
cEnd	MakeCommon

MakeCommon_OM_Free:
	lea	ax,[bdName]
	cCall	BdFree,<ax>
	lea	ax,[bdType]
	cCall	BdFree,<ax>
MakeCommon_Err_Exit2:
	sub	[grs.GRS_bdtComBlk.BD_cbLogical],SIZE COM
MakeCommon_Err_Exit1:
	mov	ax,UNDEFINED
	jmp	short MakeCommon_Exit


;***
;B$GetNMALLOC
;Purpose:
;	Called by the runtime to find a QB-specific COMMON block named
;	NMALLOC. If found, we return a pointer to the start of the value
;	table and its size.
;Input:
;	none
;Output:
;	AX = 0 if block not found, or is CB (size of block in bytes)
;	DX = PB (DGROUP-relative pointer to start of block) if AX <> 0
;***************************************************************************
cProc	B$GetNMALLOC,<PUBLIC,FAR,NODATA>
cBegin	B$GetNMALLOC	
	call	EnStaticStructs 	
	push	ax			;remember whether we should disable
					;  Static Structs on exit or not
	mov	ax,dataOFFSET NMALLOC	;points to string 'NMALLOC'
	mov	cx,CB_NMALLOC		;length of string
	call	ONamOfPbCb		;ax = oNam for 'NMALLOC' or 0
	jz	GetNMALLOC_Exit		;if ONamOfPbCb returns out of memory
					;  error code, we know there's no
					;  existing nammgr entry by this name,
					;  because nammgr only needs to grow
					;  name table to add a new entry.
	mov	[fCreateCommon],FALSE	;Tell MakeCommon to just search, not
					;  create
	cCall	MakeCommon,<ax>		;returns ax = offset to found common 
					;  block, or UNDEFINED if not found
	mov	[fCreateCommon],TRUE	;Reset static flag to default
	inc	ax			;was given block found?
	.errnz UNDEFINED - 0FFFFH	
	jz	GetNMALLOC_Exit		;  brif not - - - report failure

	dec	ax	
	add	ax,[grs.GRS_bdtComBlk.BD_pb]
	xchg	ax,bx			;bx now points to COM entry
	mov	dx,[bx.COM_bdValue.BD_pb]
	mov	ax,[bx.COM_bdValue.BD_cbLogical]
GetNMALLOC_Exit:
	pop	cx
	jcxz	GetNMALLOC_Exit1	;brif static structs were already
					;  active on entry
	push	ax			;save return values...
	push	dx			;...across DisStaticStructs
	call	DisStaticStructs
	pop	dx
	pop	ax
GetNMALLOC_Exit1:
cEnd	B$GetNMALLOC	

;***
;AdjustCommon
;Purpose:
;	This routine is called when a common value table is about to be moved.
;	Due to the overhead that would be required for the runtime to update
;	backpointers to AD's and SD's in static variable tables, we do this
;	work here.
;Input:
;	SI = ptr to the COM_bdValue.BD_pb field for an entry in 
;		grs.GRS_bdtComBlk
;	DI = adjustment factor
;Output:
;	none
;Modifies:
;	SI
;***************************************************************************
cProc	AdjustCommon,<PUBLIC,FAR,NODATA>
cBegin	AdjustCommon

	mov	ax,[pSsCOMcur]
	mov	bx,ax
	add	ax,8			;ax = ptr to BdValue.BD_bp on stack
	cmp	ax,si
	jz	AdjustIt		;brif this is the block that's moving

	;nope - - - owner of table MUST therefore be in bdtComBlk
Not_Scanning_Common:
	mov	bx,[grs.GRS_bdtComBlk.BD_pb]

	;First, search table to see if a matching entry exists
	sub	bx,SIZE COM		;special 1st-time-thru-loop value
AdjustCommon_Loop:
	add	bx,SIZE COM
	DbAssertRel bx,b,dx,CP,<AdjustCommon: given value table not found>

	lea	ax,[bx.COM_bdValue.BD_pb]
	cmp	si,ax
	jnz	AdjustCommon_Loop	;brif this isn't the right COMMON block

	add	bx,COM_bdType		; point to bdType for this block
AdjustIt:
	call	SsAdjustCommon		;actually adjust the back pointers
					;  to any SD's and AD's in table
cEnd	AdjustCommon



;###############################################################################
;#                                                                             #
;#                            non-RELEASE Code                                 #
;#                                                                             #
;###############################################################################

;***
;CbTypFar
;Purpose:
;	Far interface to CbTyp
;	Added as part of revision [5].
;Entry:
;	parmW = oTyp
;Exit:
;	size of type
;******************************************************************************/
cProc	CbTypFar,<PUBLIC,FAR>
	parmW	oTyp
cBegin
	cCall	CbTyp,<oTyp>
cEnd

sEnd	CP


	end
