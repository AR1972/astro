        TITLE   conmisc.asm - miscellaneous context-related routines
;***
;conmisc.asm - miscellaneous context-related routines for QBI
;
;       Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;       - CLEAR support
;       - NEW support
;       - Runtime Initialization
;       - Context-sensitive heap management support (updating backpointers ...)
;       - other miscellaneous context-related support
;
;
;*******************************************************************************

        .xlist

        include version.inc 
        CONMISC_ASM = ON 
	includeOnce	architec
	includeOnce	conint
	includeOnce	context
	includeOnce	executor
	includeOnce	heap
	includeOnce	names
	includeOnce	parser
	includeOnce	qbimsgs
	includeOnce	rtinterp
	includeOnce	rtps
	includeOnce	scanner 	
	includeOnce	txtmgr
	includeOnce	ui
	includeOnce	variable

        .list

        assumes CS,CP
        assumes DS,DATA
        assumes SS,DATA
        assumes ES,NOTHING


sBegin  DATA
	globalB	fChaining,FALSE		;TRUE while reinit code is being used
					;  while loading a new program we're
					;  CHAINing to
	staticW	oMrsDesired,0		;used to find an oMrs of specified type
	externB	fInitialized
	externB b$CtrlFlags		;a byte of various runtime bit flags
	externB b$ScreenRestored	;flag set by B$RUNINI
sEnd    DATA

	EXTRN	B$RUNINI:FAR
	EXTRN	B$CHNINI:FAR


sBegin	RT				;so RTOFFSET is defined
sEnd	RT


sBegin CP

;***
;AdjustRsTable
;Purpose:
;	Adjust owners in the global Rs table when it moves.
;	This amounts to walking the mrs chain, adjust the var table
;	backpointers.
;Entry:
;	SI = pbTable = pointer to the start of the appropriate table
;       DI = an adjustment factor to be passed via BdAdjust to update the
;               backpointer for each bd in grs.bdtComBlk.
;Exit:
;       none.
;Exceptions:
;       none.
;
;***************************************************************************
cProc	AdjustRsTable,<PUBLIC,NEAR,NODATA>
cBegin
	mov	si,OMRS_GLOBAL		
	test	[conFlags],F_CON_StaticStructs
	jnz	RsAdjust_Cont		  ;brif static structs - pMrsCur not used

	add	[grs.GRS_pMrsCur],di	;update pointer to mrsCur for use
					;  at execution
	TESTM	grs.GRS_oRsCur,08000H	; is active Rs an mrs?
	jnz	RsAdjust_Cont		  ;  brif so
	
	add	[grs.GRS_pRsCur],di	;update pointer to current Rs for use
					;  at execution
RsAdjust_Cont:
        ;--------------------------------------------------------------------
	; now,	SI = offset to current mrs
	;	DI = adjustment factor for the backpointer into this table
        ;--------------------------------------------------------------------
RsAdjust_Entry:
	cmp	si,[grs.GRS_oMrsCur]
	jnz	RsAdjust_It

	test	[conFlags],F_CON_StaticStructs
	jz	RsAdjust_It		;brif not static structs

	mov	si,dataOFFSET mrsCur
	jmp	short RsAdjust_Next	;brif this is mrsCur - - - static struct
					;  won't move
RsAdjust_It:
	add	si,[grs.GRS_bdRs.BD_pb] ;si points to an mrs in table
	mov	ax,si
	add	ax,MRS_bdVar
	cCall	BdAdjust,<ax>		;adjust bdVar in current mrs
RsAdjust_Next:
	mov	si,[si.MRS_oMrsNext]
	inc	si
	jz	RsAdjust_Done
	dec	si
	jmp	short RsAdjust_Entry
RsAdjust_Done:
cEnd

;***
;AdjustITable
;Purpose:
;       Given a pointer to either grs.bdtMrs or grs.bdtComBlk
;       and the constant IT_MRS or IT_COMMON_BLOCK to indicate which
;       one, walk through through the table, calling the Heap Manager (via
;       BdAdjust) to adjust the backpointer for each of the bd structures in
;       each entry by the constant given in DI.
;
;       This routine is called when the heap manager finds it necessary to
;       move one of these tables, in order to update backpointers to owners
;       in the table as quickly as possible.
;
;       Note that this routine depends on the fact that each of these tables
;	contain entries which have one or two bd structures, depending
;	on the table type. We also assume that the bd structures are contiguous.
;Entry:
;       pbTable = pointer to the start of the appropriate table
;       oPastLast is the cbLogical of the appropriate table. This amounts
;               to an offset to where the NEXT entry would go into the table,
;               and is thus a measure of where the table ends (cbLogical is
;               kept to the actual size of the table).
;       heapType is one of IT_MRS or IT_COMMON_BLOCK.
;       DI = an adjustment factor to be passed via BdAdjust to update the
;               backpointer for each bd in grs.bdtComBlk.
;Exit:
;       none.
;Exceptions:
;       none.
;
;***************************************************************************
cProc   AdjustITable,<PUBLIC,FAR,NODATA>,<SI>
        parmW   pbTable
        parmW   oPastLast
        parmB   heapType
        LocalW  cbAdvance               ;cb to advance to next entry
        LocalW  pbInvalid               ;pointer to 1st byte past last entry
cBegin  AdjustITable
        mov     cx,[oPastLast]		
	jcxz	Adjust_Done		;Stop right now if empty table
        mov     al,[heapType]		
        mov     si,[pbTable]
	cmp	al,IT_COMMON_BLOCK	;is it grs.bdtComBlk?
        jz      Adjust_Cont             ;  brif so

	DbAssertRelB  al,z,IT_MRS,CP,<AdjustITable: unknown heapType>
	call	AdjustRsTable
	jmp	short Adjust_Done

Adjust_Cont:
        add     cx,si                   ;now cx equals end-of-table pointer
        mov     [pbInvalid],cx		

	add	si,COM_bdType		
	mov	[cbAdvance],(SIZE COM - SIZE BD)  
        ;--------------------------------------------------------------------
        ; now,  SI = pointer to current bd to be adjusted
        ;       DI = adjustment factor for each backpointer into this table
	;	cbAdvance = count of bytes from start of last bd to first
	;		    bd in next entry
        ;       pbInvalid = pointer to first byte past last entry in table
        ;--------------------------------------------------------------------
Adjust_Entry:
        cmp     si,[pbInvalid]          ;any entries left?
        jae     Adjust_Done             ;  brif not

        cCall   BdAdjust,<si>           ;adjust first bd in current entry

        add     si,SIZE BD              ;move to next bd
	cmp	[si.BD_cbPhysical],UNDEFINED
					;is this bd really used to store info.
					;  about a U.L. COMMON block?
	jz	Adjust_Advance		;  brif so
        cCall   BdAdjust,<si>           ;adjust it
Adjust_Advance:
	add	si,[cbAdvance]		;advance to next entry in table
	jnc	Adjust_Entry		; brif not special case: wrap
					;	past FFFF
Adjust_Done:
cEnd    AdjustITable



;***
;ClearTheWorld()
;
;Purpose:
;       Clear all common variables and clear all module and procedure static 
;	variables. Does NOT clear (release) frame variables.
;Entry:
;       none.
;Exit:
;       none.
;*******************************************************************************
cProc   ClearTheWorld,<PUBLIC,NEAR,NODATA>
cBegin  ClearTheWorld
	DbChk	ConStatStructs			;ensure static structures
	test	[grs.GRS_flagsDir],FDIR_cleared	;is the world already clear?
	jnz	ClearWorld_Exit			;  brif so - no work to do

Clear_Common:
        cCall  ClearCommon
	mov	bx,OFFSET CP:ClearPV
	call	ForEachPrsInPlaceCPSav		;clear all prs var tables

	mov	al,FE_PcodeMrs+FE_CallMrs+FE_SaveRs
        mov	bx,OFFSET CP:ClearMV
        call    ForEachCP			;clear all mrs tables
						;retval guaranteed TRUE
	or	[grs.GRS_flagsDir],FDIR_cleared	;note that world is now clear

	and	[mrsCur.MRS_flags],NOT FM_VARNEW ;reset bit in mrs flags, in
						 ; case we were called as
						 ; VarDealloc
ClearWorld_Exit:
cEnd	ClearTheWorld


;***
;VarDealloc()
;
;Purpose:
;       Use ClearTheWorld to deallocate all variables in the current module
;
;	Note: shares exit with ClearTheWorld.
;
;	Note: For QB, this clears all variables in the system, whereas for
;	      EB, just the variables for the current module (and all proc-
;	      level statics for procedures in the module) are cleared.
;Entry:
;	none in FV_QB4LANG versions.
;	in other versions, fClearPV TRUE means we should clear proc-level
;		(static) vars as well as module-level vars for the module
;Exit:
;       none.
;*******************************************************************************
cProc	VarDealloc,<PUBLIC,NEAR,NODATA>
cBegin  VarDealloc
	DbChk	ConStatStructs			;ensure static structures
	or	[mrsCur.MRS_flags],FM_VARNEW	;tell CLEAR code we want to
						; deallocate $static arrays,
						; not just zero-fill them (in
						; this module only)
	jmp	short Clear_Common		;share exit with ClearTheWorld
cEnd	<nogen>

						
						
;***
;NewStmt()
;
;Purpose:
;     This is  called by  the NEW,  CHAIN, LOAD  statement executers  to do  the
;     following:
;     - ForEachMrs, discard the mrs
;     - Calls RunInit()
;     - Trims the mrs table so that it includes only the mrs for module Untitled
;     - releases all prs table entries
;     - reinitializes the procedure table
;     - Calls  the variable  manager's ResetCommon()  function  to  discard  all
;       COMMON value and type blocks and reset the grs.bdComBlk block to empty.
;     - Perform the equivalent of a TROFF
;     - Calls ParseNewInit
;     - Collapse the heap (garbage collect)
;
;     Before doing anything, this function calls the user-interface function
;     NotSaved(), which lets the user save any unsaved modules, or CANCEL the
;     action.  If the user selected the CANCEL button (in response to NotSaved),
;     a Runtime Error occurs, which allows us to get back to UserInterface and
;     cancel the current operation.  The user-interface does not report this 
;     error to the user.
;     
;Entry:
;       none.
;Exit:
;       none.
;Uses:
;       DI (preserves ES since this is used by exStNew).
;Exceptions:
;       If a module was modified and not saved, NotSaved will allow user to
;       cancel - - - if he does so, a runtime error is generated, and this
;       routine does not return.
;       Does not return in the case of a runtime error.
;
;*******************************************************************************
	extrn  EmptyMrs:near
cProc   NewStmt,<PUBLIC,FAR,NODATA>,<ES,SI>
cBegin  NewStmt
	call	EnStaticStructs			;ensure static mrsCur and 
						;  prsCur are activated
	cCall	NotSaved			;notify user, get his response.
						; if CANCEL, ax=MSG_GoDirect
						; if user said NO, ax=FFFF
						; if I/O error, ax = error code
						; if files saved ok, ax=0
	DJMP	jg	J1_RtError		;brif runtime error while
						; saving files, or CANCEL

	or	[grs.GRS_flagsDir],FDIR_new	;note that NewStmt is active
						; (speed optimization)
        mov     [grs.GRS_otxCONT],UNDEFINED	;NOTE must be done before any
						;text tables are discarded
						;so we don't tell user "This
						;will prevent CONT"
	call	CmdWatchDelAll			;eliminate all Watch expressions

        cCall   ResetCommon			;note: must call this BEFORE
						;NewDiscard; otherwise, if there
						;exists some common value of
						;user defined type, and we've
						;tossed the type table, then
						;access of that variable will
						;cause an error
	;Note that the below scheme depends on the fact that NextMrsFile
	;finds the first mrs in the table if grs.GRS_oMrsCur is UNDEFINED, and
	;that MrsDiscard will set that field to UNDEFINED.  It is not safe to
	;call ForEachCP to do this, because that scheme depends on walking
	;the mrs chain, and MrsDiscard discards the current entry. In essense
	;we are starting from the top of the mrs chain each time through the
	;loop below.
	call	MrsDeactivate			;required so NextMrsFile
						;starts from the beginning
MrsDiscard_Loop:
	call	far ptr NextMrsFile		;activate next file mrs
	inc	ax				; no more file mrs's?
	jz	MrsDiscard_Cont 		; brif so - exit loop

	call	MrsDiscard			;discard active mrs
	jmp	short MrsDiscard_Loop
MrsDiscard_Cont:
	mov	[fTraceOn],0			; do the equivalent of TROFF
	PUSHI	ax,OMRS_GLOBAL			
	cCall	MrsActivateCP			;[16] activate the global mrs
	PUSHI	ax,SbGNam			; parm to TNamInit
	call	TNamInit			; reinit global name table
	call	VarRudeReset			

	mov	ax,SIZE MRS + OMRS_GLOBAL	; we know new empty unnamed
						;  mrs will be at this offset
	cCall	MrsActivateCP,<ax>		;activate the empty unnamed mrs
	call	ParseNewInit			;parser reinit stuff
	DbAssertRel ax,nz,0,CP,<NewStmt: ParseNewInit returned an error code>


       	or	[b$CtrlFlags],NoSTACKINIT	;speed optimization for LOAD
						;  don't bother to reinit the
						;  stack
	call    far ptr RunInit                 ;note that this alters si & di
       	and	[b$CtrlFlags],NOT NoSTACKINIT	;reset to default value
	cmp	[fChaining],FALSE
	jnz	NewStmt_Exit			;brif we're CHAINing
	test	[conFlags], F_CON_RunFile
	jnz	NewStmt_Cont			;brif we're clearing the decks
						;  to load and run a file -
						;  don't want to show debug 
						;  screen yet in this case
	call	EnsShowDebugScrFar
NewStmt_Cont:
	or	[conFlags],F_CON_ResetStkSize	;reset the stack to default
						;  size on next BOS or BOL
NewStmt_Exit:
	and	[grs.GRS_flagsDir],NOT FDIR_new	;NewStmt no longer active

cEnd    NewStmt

J1_RtError:
	call	RtError				;error(ax), never returns

;***
;TrimBdlDirect - Trim the direct mode buffer
;
;Purpose:
;	We trim the direct mode buffer to give back most of the space to
;	the user when we reinitialize basic (NewStmt) or prior to running
;	a program, but we must never trim it to less than CB_PCODE_MIN - - -
;	this is to ensure that the user can ALWAYS do a CLEAR, SETMEM,
;	SYSTEM, etc., even when essentially out of memory.
;Entry:
;       none.
;Exit:
;       none.
;Uses:
;       none.
;Exceptions:
;       none.
;*******************************************************************************
cProc	TrimBdlDirect,<NEAR,NODATA>
cBegin
	PUSHI	ax,<dataOFFSET grs.GRS_bdlDirect>
	mov	ax,[grs.GRS_bdlDirect_cbLogical]
	cmp	ax,CB_PCODE_MIN
	ja	Trim_It				;brif cbLogical > CB_PCODE_MIN

	mov	ax,CB_PCODE_MIN
Trim_It:
	push	ax				;size to realloc to
	call	BdlRealloc			;trim Direct mode buffer to
						;  minimum safe size
cEnd

;***
;ContReinit, ContReinitStat
;
;Purpose:
;	This routine does a subset of the work that RunInit does, and is
;	called when a user continues.
;Entry:
;       none.
;Exit:
;       none.
;	ContReinit Disables static structs if they weren't already disabled
;	ContReinitStat always leaves static structs active.
;Uses:
;       none.
;Exceptions:
;       none.
;*******************************************************************************
	PUBLIC ContReinit
	PUBLIC ContReinitStat
ContReinit:
	xor	ax,ax
	SKIP2_PSW				;fall into ContReinitGen
ContReinitStat:
	mov	ax,sp
?DFP = DFP_NONE 				; don't smash regs ...
cProc   ContReinitGen,<FAR,NODATA>,<SI>
cBegin	ContReinit
?DFP = DFP_CP					; restore switch
	xchg	ax,si				;save input flag
	call	EnStaticStructs			;activate static structs for
						;  call to ForEachCP
	mov	al,FE_PcodeMRS+FE_CallMRS+FE_SaveRs
	mov	bx,OFFSET CP:CompressTNam
	call	ForEachCP			;crunch all module name tables
						;  down to cbLogical

	cCall   BdCompressAll                   ;reduce all entries to cbLogical
						;  size, compress heap
	cCall	TrimBdlDirect
	or	si,si
	jnz	ContReinit_Exit			;brif want static structs active

	call	DisStaticStructs		;deactivate static structs
ContReinit_Exit:
cEnd	ContReinitGen

;***
;CompressTNam
;
;Purpose:
;	Realloc mrsCur.MRS_bdlNam down to it's cbLogical size (to free up as
;	much heap space as possible for execution).
;Entry:
;       mrsCur assumed set up.
;Exit:
;       always returns TRUE
;Uses:
;       none.
;Exceptions:
;       none.
;*******************************************************************************
cProc   CompressTNam,<NEAR,NODATA>
cBegin	CompressTNam
	PUSHI	ax,<dataOFFSET mrsCur.MRS_bdlNam>
	call	BdlTrim				;crunch module name table down
						;  to free all unused space 
						;  for execution
	mov	ax,sp				;return TRUE
cEnd	CompressTNam

;***
;FindAnMrs
;Purpose:
;	Used to find the first mrs in the mrs table with some special
;	characteristics. Called by any of the ForEachXXX routines, just
;	saves grs.GRS_oMrsCur in the static 'oMrsDesired' and returns FALSE,
;	to terminate the ForEachXXX caller.
;Entry:
;       mrsCur is a pcode mrs.
;Exit:
;       returns FALSE (ax = 0).
;Exceptions:
;       none.
;*******************************************************************************
cProc	FindAnMrs,<NEAR,NODATA>
cBegin	FindAnMrs
	mov	ax,[grs.GRS_oMrsCur]
	mov	[oMrsDesired],ax
	xor	ax,ax				;terminate ForEachXXX caller.
cEnd	FindAnMrs

;***
;RunInit()
;
;Purpose:
;    This is called by NewStmt(), and the RUN statement executer to do the
;    following:
;        - Call ClearStmt
;        - reset event handlers and error traps
;        - deactivate current prs, if any
;        - Set grs.oPrsCur = UNDEFINED,
;        - Call RT entry point B$RUNINI to do runtime reinit.
;
;Entry:
;       none.
;Exit:
;       ax = 0 if no error, else contains a standard error message.
;Uses:
;       SI and DI
;Exceptions:
;       Does not return in the case of a runtime error.
;
;*******************************************************************************
cProc   RunInit,<PUBLIC,FAR,NODATA>
cBegin  RunInit
	and	[grs.GRS_flags],NOT FG_RetDir	;remember there's no ret adr to
						; direct mode buffer on stack.
	call	EnStaticStructs			;activate static mrsCur and 
						;  prsCur for this routine
	push	ax				; save returned flag
	test	[mrsCur.MRS_flags2],FM2_NoPcode
	jz	RunInit_Cont			;brif active module can be
						; counted on to have a var tbl,
						; a name table, etc.
	;search the mrs table and activate the first one encountered that
	;  is a pcode mrs (MUST be one ...) - - - this is so we don't try to
	;  CLEAR a non-existent var table, for example, in, say, the mrs for
	;  Immediate mode (B$RUNINI can also make a call-back that uses the
	;  nammgr ...)
	mov	al,FE_PcodeMrs+FE_CallMrs
        mov	bx,OFFSET CP:FindAnMrs
	cCall	ForEachCP
	push	[oMrsDesired]
	cCall	MrsActivateCP			;activate pcode mrs
RunInit_Cont:
        call    far ptr Clear_RunInit           ;retval must be TRUE, since
                                                ; we're not changing memory size
        mov     [grs.GRS_otxCONT],UNDEFINED
	cCall	DebugReset			;release WATCH str descriptors,
						;  reset history buffer

	cCall	BdCompressAll			;reduce all entries to cbLogical
						;  size, compress heap
	cCall	TrimBdlDirect			;trim direct mode buffer	
	pop	cx
	jcxz	Static_Structs_OK		;brif static structs were 
						;  active on entry

	call	DisStaticStructs		;deactivate static structs
						;  (as they were on input)
Static_Structs_OK:
	push	[grs.GRS_oMrsCur]
        cCall   RsActivateCP			;ensure no procedure is active
						; note we can't just call
						; PrsDeActivate here, as static
						; structures might not be active
	cmp	[fChaining],FALSE
	jnz	Chaining			;brif this is for CHAIN, not RUN

	push	WORD PTR ([fDebugScr])
	call	EnsMouseOff			;otherwise, B$RUNINI leaves a
						; ghost of mouse cursor
	
	PUSHI	ax,<RTOFFSET B$RUNINI>
	call	CallRtTrap_RT			;ax = 0 if no error, error code
						;runtime reinit. for RUN, NEW
	pop	cx				;cl = old value of fDebugScr
	push	ax				;error return from B$RUNINI
	or	cl,cl
	je	RunInit_Exit1			;brif debug screen wasn't active

	cmp	[b$ScreenRestored],0
	je	NotRestored			;brif B$RUNINI didn't change
						; screen modes
	call	TossOutputScreen		;output screen is activ
NotRestored:
	call	EnsShowDebugScrFar		;reactivate debug screen
RunInit_Exit1:
	pop	ax				;error return from B$RUNINI
	jmp	short RunInit_Exit
Chaining:
	mov	bx,[grs.GRS_bdtComBlk.BD_pb]
	;NOTE: we're depending on there always being an entry for blank COMMON,
	;NOTE: and that it's always the entry at offset zero in this table
	add	bx,COM_bdValue			;point to bdValue for blank
						;  COMMON
	xor	ax,ax				;assume no QBI blank common,
						;  or that it's empty
	cmp	[bx.BD_cbLogical],ax		;is blank COMMON empty?
	jz	Chaining_Cont			;   brif so

	TESTM	[bx.BD_pb],1			; if pb is odd, then this is
						;  not an owner, but is for
						;  U.L. blank COMMON
	jnz	Chaining_Cont			;brif not QBI blank COMMON

	xchg	ax,bx				;pass pBdValue to B$CHNINI
						;  so it won't release owners
						;  in this block
Chaining_Cont:
	xchg	ax,bx				;bx is a parm to B$CHNINI
	EXTRN	seg_rt:abs
	PUSHI	ax,<SEG seg_rt>
	PUSHI	ax,<RTOFFSET B$CHNINI>		;runtime reinit. for CHAIN
	call	CallRtTrap_Parm			;ax = 0 if no error, error code
RunInit_Exit:
cEnd    RunInit

;***
;ResetData() - reset the data pointer for mrsCur
;
;Purpose:
;       Called to reset the data pointer for a given module
;	Called as part of CLEAR, NEW, RUN, CHAIN, and whenever scanning
;	a text table and Cant CONT. This latter case ensures that DATA
;	statements get set correctly for READ's in Direct Mode.
;Entry:
;       mrsCur is set up.
;Exit:
;       Returns AX != 0
;Preserves:
;	es
;*******************************************************************************

	PUBLIC	ResetData
ResetData	 PROC       NEAR
        mov     ax,[mrsCur.MRS_data_otxFirst]	;oTx of link field of first DATA
						;  statement
	cmp	ax,UNDEFINED
	jz	ResetData_Exit			;brif no DATA, in case there's a
						;  READ statement (so we give
						;  the proper error @ runtime)
        mov     [mrsCur.MRS_data_oLineCur],6	;offset into current DATA stmt
       	sub	ax,4				;oTx of first exStData pcode
ResetData_Exit:
	mov     [mrsCur.MRS_data_otxCur],ax	;otxFirst - 4  or UNDEFINED
	mov	ax,sp
        ret
ResetData	ENDP


;***
;Clear_Mod
;Purpose:
;	Tasks that must be performed for each module, lumped together here
;	to reduce context switching (for speed)
;Entry:
;	none.
;Exit:
;	always returns AX != 0
;*******************************************************************************
Clear_Mod	 PROC	    NEAR
	mov	[mrsCur.MRS_otxHandler],UNDEFINED
						;reset module error trap
	call	ResetData			;reset DATA statements
	;Reclaim space in module name table. Only really needs to be done for
	;RUN init, but who cares about the extra time for CLEAR ...
	call	CompressTNam			;returns AX != 0
        ret
Clear_Mod	ENDP

;***
;ClearStmt, Clear_RunInit
;
;Purpose:
;
;     This is  called by  CLEAR and by RunInit() for  RUN, LOAD, NEW, CHAIN etc.
;     statements to do the following:
;
;        - Call the Variable Mgr functions ClearCommon,
;          ClearMV() for each module, and ClearPVStat() for
;          each procedure in each module, and ClearPVFrame()
;          for each procedure on the stack to:
;          - Set all numeric static variables to zero
;          - Release all string variables
;          - Do the equivalent of an ERASE on all arrays
;        - Remember that we can't CONT
;        - Reset the module data pointer for each module
;        - Reset the module error trap for each module
;        - Reclaim free space in each module name table
;        - Close all user files (not system files like the
;          file being loaded etc.)
;        - Perform special version-specific reinitialization
;          (graphics, sound, pen, strig, etc.)
;        - Reinitialize the random number seed
;    	 - Sets bosFlags FBOSRESETSTK bit to cause stack to be reset @ next 
;		BOS/BOL
;
;     Since CLEAR  causes the  stack to get reset, we have chosen not to allow a
;     program to  execute CLEAR  from within  a procedure  or function; the text
;     manager will issue an error message to prevent this.
;
;     Clear_RunInit is just like ClearStmt, except that it is assumed that
;     static structs are already set up on input, and that this routine need
;     not reset the DATA statements in each module; this is a speed optimization
;     to prevent needless context switching.
;Entry:
;     none.
;Exit:
;     none.
;Uses:
;       none.
;Exceptions:
;       none.
;
;*******************************************************************************
cProc   Clear_RunInit,<FAR,NODATA>		
cBegin  Clear_RunInit				
        push    es                              ;save for executor
	xor	ax,ax				
	jmp	short Clear_Spt_Common		
cEnd	Clear_RunInit,<nogen>			

cProc   ClearStmt,<PUBLIC,FAR,NODATA>		
cBegin  ClearStmt				
        push    es                              ;save for executor

	call	EnStaticStructs			;activate static mrsCur and 
						;  prsCur for this routine
Clear_Spt_Common:
	push	ax				; save flag
	
	mov	al,FE_PcodeMrs+FE_CallMrs+FE_SaveRs
        mov	bx,OFFSET CP:Clear_Mod
        call    ForEachCP			;retval guaranteed to be TRUE

        call    ClearTheWorld

	or	BosFlags,FBOSRESETSTK		;cause stack to be reset @ next
						;  BOS/BOL
	mov	[pGosubLast],0			;reset - - - can't RETURN now


	and	[grs.GRS_flagsDir],NOT FDIR_cleared ;reset flag, so next call to
						    ;    ClearTheWorld doesn't
						    ;    assume world is clear
	pop	cx
	jcxz	StaticStructs_OK		;brif context state is okay

	call	DisStaticStructs		;deactivate static prsCur & 
						;  mrsCur again (as they were
						;  on input)
StaticStructs_OK:
	pop     es
cEnd						

;***
;CantCont()
;
;Purpose:
;       Sets grs.otxCONT to UNDEFINED, i.e., makes it so the user
;       can no longer CONTINUE.
;Note:
;	When UserInterface() is entered, if we were not executing out of the
;	direct mode buffer, grs.otxCONT is set to the current
;	text pointer (grs.otxCur).
;Entry:
;       none.
;Exit:
;       none.
;Uses:
;       none.
;Exceptions:
;       none.
;
;*******************************************************************************
cProc   CantCont,<PUBLIC,FAR,NODATA>
cBegin  CantCont
	mov	cx,UNDEFINED
        xchg    [grs.GRS_otxCONT],cx	;ensure this gets set to UNDEFINED
	inc	cx
	.errnz	0FFFFH - UNDEFINED
	jcxz	CantCont_Exit		;brif already couldn't CONTinue
					; (because RunInit closes files)
	test	[conFlags], F_CON_RunFile
	jnz	CantCont_Exit		;brif we're clearing the decks
					;  to load and run a file -
					;  don't want to show debug screen
	push	[grs.GRS_oRsCur]	;pass caller's oRs to RsActivateCP below
					; RunInit deactivates current prs
	call	RunInit
	call	RsActivateCP		;parm pushed above
	call	EnsShowDebugScrFar
CantCont_Exit:
cEnd	CantCont



sEnd	CP				
sBegin	SCAN				
assumes cs,SCAN 			

;***
;FCanCont
;
;Purpose:
;       tests grs.otxCONT for UNDEFINED, i.e., sees if the user
;       can CONTINUE.
;Entry:
;       none.
;Exit:
;       ax = 0 and psw.Z is set if the user cannot continue
;Uses:
;       none.
;Exceptions:
;       none.
;
;*******************************************************************************
cProc	FCanCont,<PUBLIC,NEAR,NODATA>	
cBegin
	mov	ax,[grs.GRS_otxCONT]
	inc	ax			;ax = 0 if can't continue
cEnd

sEnd	SCAN				
sBegin	CP				
assumes cs,CP				


;***
;NextQBI_Frame
;Purpose:
;	Given a frame pointer, return the next higher frame pointer if it's
;	a QBI frame. Indicate whether a QBI frame was found or not, and if
;	the end of frame chain or broken chain or non-QBI frame was found.
;Input: 
;	di = pointer to a pointer to a frame
;Output:
;	PSW.C set if invalid frame found (broken chain, or non-QBI frame found)
;	if PSW.C clear,
;	    PSW.Z set if end of BP chain found ,i.e., there exist no more frames
;	    If both of the above flags are clear, di on exit points to the next
;	    higher QBI frame on the stack, and the oRs for this frame is 
;	    activated
;	ax also contains PSW flags
;*******************************************************************************
cProc	NextQBI_Frame,<PUBLIC,FAR>
cBegin
	mov	ax,[di]
	mov	bx,[b$mainframe]
	DbAssertRel  [bx],z,0,CP,<NextQBI_Frame: BP chain not zero terminated>
	cmp	bx,ax				;end of frame chain?
	jc	Flags_Set_Exit			;  brif new ptr above stack
	jz	No_Carry_Exit			;  brif so

	cmp	ax,di				;is new ptr > old?
	jc	Flags_Set_Exit			;  brif not - broken chain

	mov	di,ax

	shr	al,1				;is the new frame ptr odd?
	jc	Flags_Set_Exit			;  brif so - broken chain

	call	ValidORs			;ax = 0 if valid QBI frame
	or	ax,ax
	jnz	Set_Carry_Exit			;brif QBI frame not found

	;success - ensure PSW.Z and PSW.C are reset and exit
	inc	ax				;reset PSW.Z
	SKIP2_AX				;skip to No_Carry_Exit
Set_Carry_Exit:
	stc
	SKIP1_AL				;skip the clc, leave PSW intact
No_Carry_Exit:
	clc					;success or end of BP chain
Flags_Set_Exit:
	pushf					
	pop	ax				
cEnd	NextQBI_Frame


;***
;ActiveORs_Frame
;Purpose:
;	Given a pointer to a pointer to a frame and grs.GRS_oRsCur, walk
;	the stack checking each QBI frame on the stack until we find
;	one that was pushed from the context of the given oRs, or until
;	we find the end of the chain, a broken chain, or a non-QBI frame.
;Input:
;	ppFrame == pointer to a pointer to a frame (ex: mov bx,dataOFFSET
;		   b$curframe)
;	grs.GRS_oRsCur is set for the register set we're looking for on stack.
;Output: 
;	PSW.C set if given oRs definitely has no active frame on the stack.
;	PSW.C clear if given oRs IS found, OR if it MIGHT have an active 
;		frame on the stack.
;
;	ax == 0 if given oRs definitely has no active frame on the stack.
;	ax != 0 if given oRs IS found, OR if it MIGHT have an active
;		frame on the stack.
;Note:
;	Note that this routine could THINK a frame is a QBI frame
;	when it really isn't. It only checks the oRs part of the frame.
;*******************************************************************************
cProc	ActiveORs_Frame,<PUBLIC,FAR>,<SI,DI>	 
	parmW	ppFrame 		
cBegin
	mov	bx,[ppFrame]
	mov	si,[grs.GRS_oRsCur]	;oRs we're looking for
	cmp	word ptr [bx],0
	jz	ORsFrame_Exit		;brif special case of no frames to
					;  search
	mov	di,bx
ORsFrame_Loop:
	call	NextQBI_Frame
	push	ax			
	popf				
	jc	ORsFrame_Success	;brif broken chain or non-QBI frame
	jz	ORsFrame_Exit		;brif end of frame chain found

	cmp	si,[grs.GRS_oRsCur]
	jnz	ORsFrame_Loop		;brif this frame for a different oRs

	;success - - - given oRs frame IS on stack
ORsFrame_Success:
	clc
	SKIP1_AL			;skip stc and exit
ORsFrame_Exit:
	stc				;indicate failure
	pushf
	cCall	RsActivate,<si> 	; reactivate input oRsCur
	mov	bx,di			;retval if success
	sub	ax,ax			; assume no active frame found
	popf
	jc	@F			; brif no active frame found
	mov	ax,sp			
@@:					

cEnd

;***
;FindORsFrame
;Purpose:
;	Given b$curframe and grs.GRS_oRsCur, walk the stack checking each 
;	QBI frame on the stack until we find one that was pushed from the 
;	context of the given oRs, or until we find the end of the chain, a 
;	broken chain, or a non-QBI frame.
;
;	NOTE: This does NOT look at the active frame, only previously pushed
;		frames. If you want to examine the active frame, use oRsCont.
;Input:
;	b$curframe is the first frame to examine.
;	grs.GRS_oRsCur is set for the register set we're looking for on stack.
;Output: 
;	AX = 0 if the given oRs has no active frame on the stack.
;	AX != 0 means that there is PROBABLY an active frame for this oRs.
;*******************************************************************************
cProc	FindORsFrame,<PUBLIC,NEAR>
cBegin
	mov	bx,dataOFFSET b$curframe	;ptr to ptr to first frame
	cCall	ActiveORs_Frame,<bx>		
cEnd

sEnd	CP


        end
