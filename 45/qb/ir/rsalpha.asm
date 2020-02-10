	TITLE	RsAlpha.asm - Alphabetical Register Set Functions

;==========================================================================
;
;Module:  RsAlpha.asm - Alphabetical Register Set Functions
;System:  Quick BASIC Interpreter
;SubSystem:  Register Set Manager
;
;=========================================================================*/

	include		version.inc
	RSALPHA_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	heap
	includeOnce	names			
	includeOnce	util

	assumes ds,DATA
	assumes ss,DATA
	assumes es,NOTHING

sBegin	DATA	
	globalB bdAlphaRs,NULL,<SIZE BD>
sEnd	DATA

sBegin	CP
assumes cs,CP

;**************************************************************************
; void InsertAlpha
; Purpose:
;	Inserts grs.oRsCur into rgRs in the appropriate spot
;	(based on alphabetical order) between iFirst and iLast.
;
; Entry:
;	grs.oRsCur is oRs to be inserted into table
;	si = 1st index to compare with
;	si + di = last index to compare with + 1
;
; Exit:
;	di is bumped by 1
;
;**************************************************************************
cProc	InsertAlpha,<NEAR>
cBegin
	push	si			;save caller's si,di
	push	di

	add	di,si			;di = stopping index
	dec	si
IaLoop1:
	inc	si
	cmp	si,di
	jae	IaDone1			;brif end of range of oRs's to compare
	push	si			;save current index

	xchg	ax,si			;ax = index of oRs to compare with
	call	ORsOfAlphaCP		;ax = oRs to compare with oRsCur
	GETRS_SEG es,bx,<SIZE,LOAD>	;[3]
	mov	bx,DATAOFFSET mrsCur.MRS_ogNam 
	sub	si,si			
	RS_BASE mov,si			; es:si points to base of Rs table
	or	ax,ax
	jns	GotAnMrs
	and	ah,7Fh			;ax = oPrs
	mov	bx,DATAOFFSET prsCur.PRS_ogNam 

;bx points to ogNam for oRsCur 		
;es:si points to base of Rs table
;ax = oMrs or oPrs of oRs to compare with oRsCur
;
GotAnMrs:
	.errnz	MRS_ogNam - PRS_ogNam	
	add	si,ax			;si = pMrs or pPrs for oRs in table
;***** Begin revision [08]
	mov	cx, [bx]		;get ogNam1
	mov	dx,PTRRS[si.MRS_ogNam]	;get ogNam2
	pop	si			;restore si = index of oRs in table

	.errnz	OGNAM_UNNAMED		
	jcxz	IaDone1			;if ogNam1 == OGNAM_UNNAMED then
					;   ogNam1 < OGNAM_UNNAMED

	xchg	dx,cx			;dx = ogNam1, cx = ogNam2
	jcxz	IaLoop1			;if ogNam2 == OGNAM_UNNAMED then
					;   ogNam1 > ogNam2

	push	dx			; ogNam1
	push	cx			;[1] ogNam2
;***** End revision [08]
	call	CmpOgNamIns		; returns:
	;				;     ax = 0 if substrings match,
					;     ax = FFFF if str1 < str2
					;	 ax = 1 if str1 > str2
					;	 dx > 0 if str1 longer than str2
					;	 dx < 0 if str1 shrter than str2
					;	 dx = 0 if they're same length
					;     PSW set via an OR AX,AX

	jne	CmpDiff			; brif strings differ in their subset
	xchg	ax,dx			;ax identifies which string is longer
CmpDiff:
	or	ax,ax
	jg	IaLoop1			;brif have not found insertion point
					; i.e. oRsCur is alphabetically
					; greater than the oRs in the table @ si

;si = index of slot to use for new oRs.
; copy all existing entries from si to the end of the table up 2 bytes
;
IaDone1:
	mov	di,[bdAlphaRs.BD_cbLogical]
	dec	di
	mov	cx,di
	shl	si,1			;convert index count to byte count
	sub	cx,si			;cx = # bytes to be moved
	add	di,[bdAlphaRs.BD_pb]	;di points to last byte to transfer
	mov	si,di
	dec	si
	dec	si			;si points to 1st byte to transfer
	push	ss			
	pop	es
	std
	rep movsb
	cld
	mov	ax,[grs.GRS_oRsCur]
	stosw				;insert new oRs in table
	pop	di
	pop	si
	inc	di			;bump caller's di
cEnd

;**************************************************************************
; ushort AlphaBuildORs()
; Purpose:
;	Build an alphabetically sorted list of register sets if one
;	does not already exist.  This is called by any functions that
;	need to reference ORsOfAlpha or AlphaOrORs.
;	Name changed from AlphaORsBuild as revision [9].
; Exit:
;	returns number of entries in the table.
;	returns 0 if out-of-memory
;
;**************************************************************************
cProc	AlphaBuildORs,<PUBLIC,FAR>,<si,di>
	localW	cMrs
cBegin
	cmp	[bdAlphaRs.BD_pb],NULL
	je	BldAlloc
	jmp	BldExists		;brif table already created
BldAlloc:
	mov	ax,[grs.GRS_bdRs.BD_cbLogical]	
	sub	dx,dx			;dx:ax = size of Rs table
	mov	cx,SIZE PRS		;cx = min. size of 1 entry in Rs table
	.erre	(SIZE MRS) GT (SIZE PRS); use size of smaller struct to be
					;  conservative (i.e., to allow for
					;  max. possible Rs table entries)
	div	cx			;ax = dx:ax / cx
					;   = max number of Rs entries
	shl	ax,1			;ax = max size of table (in bytes)

	PUSHI	dx,<DATAOFFSET bdAlphaRs>
	push	ax
	PUSHI	ax,IT_NO_OWNERS
	call	BdAlloc
	or	ax,ax
	jne	GotBuf
	jmp	BldExit			;brif out-of-memory

;First, put grs.oMrsMain (if any) in bdAlphaRs
;Next, fill bdAlphaRs with oRs for all pcoded mrs's
;Next, append to bdAlphaRs all oRs's for non-pcoded mrs's (documents/includes)
;
GotBuf:
	push	[grs.GRS_oPrsCur]	;preserve caller's register sets
	push	[grs.GRS_oMrsCur]	; Note it isn't enough to save oRsCur
					; since it may be for a DECLARE,
					; in which case the current module
					; wouldn't be restored.

	sub	si,si			;insertion point = 0

	mov	ax,[grs.GRS_oMrsMain]
	inc	ax
	je	BldNoMain		;brif no main module
	dec	ax
	cCall	MrsActivateCP,<ax>
	sub	di,di			;cMrsTemp = 0
	call	InsertAlpha		;insert oRsCur between si and si+di
	inc	si			;new insertion point = 1
BldNoMain:
	mov	cx,1			;1st, save pcoded mrs's
BldLoop0:
	sub	di,di			;cMrsTemp = 0
	push	cx			;remember if we're saving pcode/no-pcode
	call	MrsDeactivate
BldLoop1:
	call	NextMrsFile_All
	inc	ax
	je	BldDone1		;brif done with mrs's
	dec	ax			;ax = grs.oMrsCur
	cmp	ax,[grs.GRS_oMrsMain]
	je	BldLoop1		;main has already been inserted
	pop	cx
	push	cx
	mov	al,[mrsCur.MRS_flags2]
	and	al,FM2_Include OR FM2_NoPcode
	jcxz	BldNotPcoded		;brif saving non-pcoded mrs's
	je	BldInsert		;brif this is pcoded mrs
	jmp	SHORT BldLoop1
BldNotPcoded:
	je	BldLoop1		;brif this is a pcoded mrs
BldInsert:
	call	InsertAlpha		;insert oRsCur between si and si+di
					; and bump di
	jmp	SHORT BldLoop1

BldDone1:
	add	si,di			;si = number of mrs's inserted so far
					; insert all non-pcoded mrs's after
					; the pcoded mrs's
	pop	cx
	dec	cx
	jcxz	BldLoop0		;brif its time to save non-pcoded mrs's

;si = number of mrs's inserted in table bdAlphaRs
	mov	cx,si			;cx = cMrs
	sub	si,si			;iMrs = 0
BldLoop2:
	push	cx			;save cMrs
	mov	ax,si			;ax = index of oRs
	call	ORsOfAlphaCP		;ax = oMrs = oRs
	cCall	MrsActivateCP,<ax>
	inc	si			;iMrs++

;Now, insert all procedures within current module into bdAlphaRs
; between current module and the next module
;
	sub	di,di			;cPrs = 0
BldLoop3:
	call	NextTextPrsInMrs
	inc	ax
	je	BldDone3
	call	InsertAlpha		;insert oRsCur between si and si+di
					; and bump di
	jmp	SHORT BldLoop3

BldDone3:
	add	si,di			;si = number of oRs's in table so far
	pop	cx			;cx = cMrs
	loop	BldLoop2
	shl	si,1			;si = real # bytes of oRs's in table
	DbAssertRel si,be,[bdAlphaRs.BD_cbLogical],CP,<AlphaBuildORs err1>
	mov	[bdAlphaRs.BD_cbLogical],si

	call	MrsActivateCP		;parm pushed on entry
	call	PrsActivateCP		;parm pushed on entry

BldExists:
	mov	ax,[bdAlphaRs.BD_cbLogical] ;ax = size of table
	shr	ax,1			;convert byte count to word count
BldExit:
cEnd

;**************************************************************************
; ushort AlphaORsFree()
; Purpose:
;	Releases the table built by AlphaBuildORs.  It is called whenever
;	a new register set is inserted, or an existing one is deleted.
;
;**************************************************************************
cProc	AlphaORsFree,<PUBLIC,FAR>
cBegin
	PUSHI	ax,<DATAOFFSET bdAlphaRs>
	call	BdFree
cEnd

;**************************************************************************
; ushort ORsOfAlpha(index)
; Purpose:
;	Given an alphabetical index, map it to an oRs
; Entry:
;	parm1 = index
; Exit:
;	If index is beyond end of table, it returns 7FFFh
;	   (high bit is off so callers that are looking for
;	    the end of a module's prs's need take no special action).
;	else ax = equivalent oRs
;
;**************************************************************************
cProc	ORsOfAlpha,<PUBLIC,FAR>
	parmW	index
cBegin
	mov	ax,[index]
	call	ORsOfAlphaCP
cEnd

;**************************************************************************
; ushort ORsOfAlphaCP(ax=index)
; Purpose:
;	Given an alphabetical index, map it to an oRs
; Entry:
;	ax = index
; Exit:
;	If index is beyond end of table, it returns 7FFFh
;	   (high bit is off so callers that are looking for
;	    the end of a module's prs's need take no special action).
;	else ax = equivalent oRs
;
;**************************************************************************
cProc	ORsOfAlphaCP,<PUBLIC,NEAR>
cBegin
	DbAssertRel [bdAlphaRs.BD_pb],ne,NULL,CP,<ORsOfAlphaCP err1>
	mov	dx,7FFFh		;prepare to return End-Of-Table result
	shl	ax,1			;convert word offset to byte offset
	cmp	ax,[bdAlphaRs.BD_cbLogical]
	jae	ORsExit			;brif beyond end of table
	mov	bx,[bdAlphaRs.BD_pb]	;bx -> base of table
	add	bx,ax
	mov	dx,[bx]			;ax = oRs
	DbChk	oRs,dx			;make sure it is a valid oRs
ORsExit:
	xchg	ax,dx			;return result in dx
cEnd

;**************************************************************************
; ushort AlphaOfORs(oRs)
; Purpose:
;	Given an oRs, map it to an alphabetical index
;
;**************************************************************************
	PUBLIC	AlphaOfORs		
AlphaOfORs:				
cProc	AlphaOfORsFar,<PUBLIC,FAR>
	parmW	oRs
cBegin
	cCall	AlphaOfORsNear,<[oRs]>	
cEnd

cProc	AlphaOfORsNear,<NEAR>,<di>	
	parmW	oRs
cBegin
	DbAssertRel [bdAlphaRs.BD_pb],ne,NULL,CP,<AlphaOfORs err1>
	mov	ax,[oRs]		;ax = word to search for
	DbChk	oRs,ax			
	mov	di,[bdAlphaRs.BD_pb]	;di -> 1st word in table to search
	mov	cx,0FFFFh
	push	cx
	push	ds
	pop	es
	repne scasw			;search for opcode in table pointed
					; to by di (uses es)
	pop	ax
	sub	ax,cx			;ax = index (1..n+1)
	dec	ax			;ax = index (0..n)
cEnd

;**************************************************************************
; NextAlphaPrs
; Purpose:
;	Return the alphabetically next prs within current mrs.
;	Assumes AlphaBuildORs() has been called.
; Entry:
;	grs.oRsCur identifies current register set (can be module's)
; Exit:
;	If not at end of module's procedure list,
;	   next prs is activated, ax and PSW are non-zero
;	else
;	   module's text table is activated (no prs), ax and PSW are zero
;
;**************************************************************************
cProc	NextAlphaPrs,<PUBLIC,FAR>
cBegin
	push	[grs.GRS_oRsCur]
	call	AlphaOfORsNear		; ax = index into list of oRs for module
	inc	ax			;ax = index for next oRs
	cCall	ORsOfAlphaCP		;ax = oRs for this oRs (or 7FFF if
					; end of table)
	push	ax
	call	PrsDeactivate
	pop	ax
	or	ax,ax
	jns	NextDone		;brif done with all prs's in this module
	cCall	RsActivateCP,<ax>
NextDone:
	mov	ax,[grs.GRS_oPrsCur]
	inc	ax			;return 0 if done with list
cEnd

sEnd	CP

end
