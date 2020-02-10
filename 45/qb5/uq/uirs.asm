	TITLE	uirs.asm - User Interface Register Set Management.
;*** 
;uirs.asm - User interface register set management.
;
;	Copyright <C> 1985-1988, Microsoft Corporation
;
;Purpose:
;	This module attempts to localize the interface between the rest of
;	the user-interface and the contest manager.
;
;
;*******************************************************************************

	.xlist
	include		version.inc
	include 	..\uq\intl.inc       ;IPG - holds definition for CCHUNTITLED
	UIRS_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	heap
	includeOnce	names		
	includeOnce	qbimsgs
	includeOnce	txtmgr
	includeOnce	ui
	includeOnce	uiint
	.list

assumes	DS,DATA
assumes	ES,DATA
assumes	SS,DATA

sBegin	DATA
	externB szUntitled		
sEnd	DATA


sBegin	CP
assumes	cs,CP

; Added with [7]
;****
;fogNamUntitled - is this ogNam reference to an untitiled text table
;
;Purpose:
;	For QEDIT, there can be two untitled text tables: the untitled
;	text table (ogNam = OGNAM_UNNAMED) and the text table with the title
;	"UNTITLED".  Figure out if this ogNam references either of them.
;
;Entry:
;	ogNam - ogNam to check
;
;Exit:
;	TRUE - ogNam is UNTITLED or for the UNTITILED window
;
;Uses:
;	AX, BX, CX, DX
;****
cProc	fogNamUntitled,<FAR,PUBLIC>,<SI,DI>
parmw	ogNam
cBegin
	mov	ax,ogNam
	cmp	ax,OGNAM_UNNAMED	;is it the untitled text table?
	je	IsUntitled
	xor	cx,cx			;(assume special, set length)
	cmp	ax,OGNAM_PSEUDO_MAX	;is it a special one?
	jbe	IsTitled		;yes, it has a title
	push	ax
	call	FpNamOfOgNam		;es:bx = ptr to name, cx = count

IsTitled:
	xor	ax,ax			;prepare to return FALSE (not untitled)
	cmp	cx,CCHUNTITLED ;IPG			;is it 8 = len(UNTITLED)
	jne	fogExit 		;brif not, return FALSE
	mov	di,bx
	mov	si,dataOffset szUntitled
	repz	cmpsb			;is it untitled?
	jnz	fogExit 		;brif not, return FALSE

IsUntitled:
	mov	ax,sp			;return TRUE

fogExit:
cEnd

sEnd	CP

sBegin	UI
assumes	cs,UI

;**************************************************************************
;fCanContUI
;
;Purpose:
;       tests grs.otxCONT for UNDEFINED, i.e., sees if the user
;       can CONTINUE.
;Entry:
;       none.
;Exit:
;       ax = 0 and psw.Z is set if the user cannot continue
;
;*******************************************************************************
cProc	fCanContUI,<PUBLIC,NEAR,NODATA>
cBegin
	mov	ax,[grs.GRS_otxCONT]
	inc	ax			;ax = 0 if can't continue
cEnd

;**************************************************************************
; ushort GetRsName(oRs, flags, cbMax)
; Purpose:
;	Copy the ASCII Name of a given register set to the global buffer
;	'bufStdMsg'.  This may be a module name or procedure name.
;
; Entry:
;	ushort oRs = register set (as returned by RsMake)
;	uchar flags says to display name in the form:
;	  RSN_fFullName:   <module name>:<procedure name>
;		where procedure name is only included if
;		  grs.oPrsCur != UNDEFINED
;	  RSN_fIndent:     precede name with 2 spaces
;	uchar cbMax = maximum number of characters to display.  If name won't
;		fit, "..." is inserted in middle of procedure name.
;		Assumes cbMax > length of MSG_Untitled
;		Assumes cbMax > 14
;		Assumes cbMax < 256
;		If cbMax > CB_MSG_MAX (defined in qbimsgs.inc) then
;		   cbMax is forced to CB_MSG_MAX
;	mrsCur.ogNam describes module's name (assumed < 256 chars long)
;	prsCur.ogNam describes procedure's name (assumed < 256 chars long)
;
; Exit:
;	returns # bytes in name
;	bufStdMsg contains a 0-byte terminated name
;	preserves active register set on exit
;
; Algorithm:
;       if (oRs == UNDEFINED) {
;          strcpy( &bufStdMsg, "" )
;          return(2)
;          }
;	if (fFullName or grs.oPrsCur != UNDEFINED)
;	   if (((grs.oPrsCur == UNDEFINED) || fFullName) &&
;	       (mrsCur.ogNam == NULL))
;	         cbMax -= (EmitMsg('<Untitled>'))
;	   else
;	      cbMax -= Emit(pFilePart(sdMrsName.pb, cbMax))
;	   if (grs.oPrsCur != UNDEFINED)
;	      emit ':'
;	      cbMax -= 1
;	if (grs.oPrsCur != UNDEFINED)
;	   EmitName(&sdPrsName, cbMax)
;	
;**************************************************************************
cProc	GetRsName,<PUBLIC,NEAR,NODATA>,<si,di>
	parmW	oRs
	parmB	flags
	parmB	cbMax
	localD	sdName,4		
	localV	rsName,255D		
 cBegin	GetRsName
;       if (oRs == UNDEFINED) {
;          strcpy( &bufStdMsg, "" )
;          return(2)
;          }
	mov	di,dataOFFSET bufStdMsg	;di points to destination buffer
	mov	ax,[oRs]
	inc	ax			; Is oRs UNDEFINED?
	jne	oRsOk
	mov	byte ptr [di],0		;store 0-byte terminator
	jmp	GetRsNameEnd		;return ax = length = 0

oRsOk:
	; make a fake sd for use with ogNam of the Rs
	lea	si,rsName		
	mov	[SEG_sdName],si		; set up pointer part of fake sd

	dec	ax			;restore ax = oRs parm
	push	[grs.GRS_oRsCur]	;preserve current context
					; (will be restored on exit)
	push	ax
	call	UiRsActivate		;activate register set of interest
	push	ds
	pop	es			;es = DGROUP for stosb, movsb
	mov	dx,[grs.GRS_oPrsCur]
	and	dh,dl			;dh = FF only if oPrsCur == UNDEFINED
	mov	dl,[cbMax]		;dl = cbMax
	cmp	dl,CB_MSG_MAX
	jbe	CbMaxOk			;brif cbMax <= CB_MSG_MAX
	mov	dl,CB_MSG_MAX		;force cbMax within range
CbMaxOk:
	test	[flags],RSN_fIndent
	je	NoIndent1
	dec	dl			;cbMax -= 2
	dec	dl
NoIndent1:
	inc	dh			;dh = 0 if oPrsCur == UNDEFINED
	je	ListModName		;brif no active procedure
	test	[flags],RSN_fFullName
	je	ListProcName		;brif caller doesn't want mod name
ListModName:
	push	dx			; preserve DX
	push	[mrsCur.MRS_ogNam]	
	call	fogNamUntitled		
	pop	dx			; restore DX
	or	ax,ax			; is it an untitled module?
	mov	bx,MSG_Untitled		; assume so
	jne	SpecialOgNam		;brif module is untitled
	mov	bx,MSG_Immediate	
	mov	ax,[mrsCur.MRS_ogNam]	
	cmp	ax,OGNAM_IMMEDIATE	
	jne	NotSpecialOgNam		;brif module is Immediate window
SpecialOgNam:
	push	dx			;preserve dx
	push	bx			;pass MSG_xxx
	call	ListStdMsg		;copy "<Untitled>" to buffer,
					; ax = #bytes (international <> 8)
	pop	dx			;restore dx
	add	di,ax			;di points beyond "<Untitled>"
	inc	ax			;add 1 for following space if proc
	sub	dl,al			;cbMax = #bytes left in buffer
	jmp	SHORT TryProcName

NotSpecialOgNam:
	push	dx			; preserve across call
	cCall	CopyOgNamPb,<si,ax>	; copy name into fake sd on stack
	pop	dx			
	mov	[OFF_sdName],ax		; save cbName in cb part of fake sd
	xchg	ax,bx			; bx = length of module name
	cCall	pFilePart		; Get pointer to file part of file spec.
					; pointer in si; count in bx
					; all other regs preserved.
	sub	dl,bl			; cbMax -= length of module's name
	mov	cx,bx			; Get byte count of file spec.
	rep movsb			; copy filename to destination

;	if (grs.oPrsCur != UNDEFINED)
;	   if (anything emitted yet)
;	      emit ':'
;	   EmitName(&sdPrsName, cbMax)
;
; dh = 0 if no prs is active
; di points into bufStdMsg
; dl = max bytes left in output buffer
;
TryProcName:
	or	dh,dh
	je	ListZeroTerm		;brif no procedure is active
	mov	al,':'
	stosb				;emit ':' between module and proc name
	dec	dl			;cbMax -= 1
ListProcName:
	push	dx			; preserve across call
	lea	si,rsName		
	cCall	CopyOgNamPb,<si,prsCur.prs_ogNam> 
	pop	dx			
	lea	si,sdName		
	mov	[si.SD_cb],ax		
	call	EmitName		;EmitName(si, di, dl)

;di points beyond last byte of name
ListZeroTerm:
	test	[flags],RSN_fIndent
	je	NoIndent2		;brif no space to be inserted at front
	mov	si,di
	dec	si			;si points to last byte of name
	inc	di
	push	di			;save ptr beyond last byte
	mov	cx,di
	sub	cx,dataOFFSET bufStdMsg	;cx = byte count of name
	std
	rep movsb			;shift name right 2 bytes
	cld
	mov	ax,' '
	stosb				;put '  ' in front of name
	stosb
	pop	di			;di points to last byte of name
	inc	di			;di points beyond last byte of name
NoIndent2:
	pop	ax			;ax = caller's context
	inc	ax			;test for UNDEFINED
	je	NoRsWasActive
	dec	ax			;restore ax = oRs
	push	ax
	call	UiRsActivate		;re-activate saved register set
NoRsWasActive:
	mov	byte ptr [di],0		;store 0-byte terminator
	xchg	ax,di			;ax points beyond end of name
	sub	ax,dataOFFSET bufStdMsg	;ax = actual size (return value)
;ax = #bytes in name
GetRsNameEnd:
	DbAssertRelB	[cbMax],ae,al,UI,<GetRsName: name too long>	
	; If the pathname code doesn't truncate names/extensions,
	; GetRsName can return > cbMax chars, hosing DGROUP, etc.
cEnd	GetRsName

;**************************************************************************
; EmitName(si = psdSrc, di = pbDst, dl = cbMax)
; Purpose:
;	List as much of a name as possible, storing "..." in middle
;	if insufficient room.
;
; Entry:
;	si = psdSrc = pointer to source name's string descriptor
;	di = pbDst = pointer to destination buffer
;	dl = size of destination buffer (assumed > 15)
;
; Algorithm:
;  think of name as [cbLeft-cbSkip-cbRight]
;  if entire name won't fit in destination, [cbLeft...cbRight] is listed
;  cbLeft = cbMax / 2
;  cbRight = cbMax - cbLeft
;  cbSkip = cbName - cbLeft - cbRight = cbName - cbMax
;
; Exit:
;	di points beyond emitted name
;
; Alters:
;	al, bx, cx, si (ALL others are preserved)
;
;**************************************************************************
EmitName:
	mov	cx,[si.SD_cb]		;cx = length of name
					; we can assume ch == 0
	mov	si,[si.SD_pb]		;si points to 1st byte of name
	sub	al,al			;al = cbRight = 0 (assume name fits)
	cmp	cl,dl			;compare cbName with cbMax
	jbe	NameFits		;brif plenty of room for entire name
	sub	dl,3			;account for the `...'
	mov	bx,cx			;bx = bl = cbName
	mov	cl,dl			;cx = cl = cbMax
	shr	cl,1			;cx = cl = cbLeft = cbMax / 2
	mov	al,dl			;al = cbMax
	sub	al,cl			;al = cbRight = cbMax - cbLeft
	sub	bl,dl			;bx = bl = cbSkip = cbName - cbMax
NameFits:
	rep movsb			;copy left part of name
	mov	cl,al			;cx = cl = cbRight
	jcxz	EmitNameExit		;brif no right half to emit
	mov	al,'.'
	stosb				;emit '...'
	stosb
	stosb
	add	si,bx			;si points to right half of name
	rep movsb			;copy right part of name
EmitNameExit:
	ret

;**************************************************************************
; pFilePart(si = pointer to file spec, bx = byte count of file spec)
; Purpose:
;  Advance file spec pointer so that it points to the filename part.
;  Adjust byte count accordingly
;
;  KANJI variant added with [6]
;
; Entry:
;    SI - Pointer to file spec.
;    BX - Count of bytes in file spec.
;
; Exit:
;    SI - Pointer to file part of file spec.
;    BX - Count of bytes of file part in file spec.
;
; Alters: si, bx
;
;**************************************************************************

DbPub pFilePart
cProc pFilePart,<NEAR>,<AX,CX,DX,DI,ES>
cBegin

	push	ds
	pop	es

	mov	di,si
	mov	al,'\'
	mov	cx,bx
ScanForBackSlash:
	repne scasb
	jne	NoMoreBackSlash
	inc	si			; Point after the '\'
	mov	bx,cx			; Save away the count and the pointer
	mov	si,di
	jmp	SHORT ScanForBackSlash

NoMoreBackSlash:

	cmp	bx,2
	jbe	pFilePartEnd
	cmp	byte ptr [si+2],':'	; Is there a drive specification.
	jne	pFilePartEnd
	inc	si			; Throw away drive specification.
	inc	si
	dec	bx
	dec	bx
pFilePartEnd:
cEnd


;************************************************************************
; boolean ContContext()
; Purpose:
;	Make the current MRS and PRS the one where BASIC's program counter
;	is pointing.  If CONTinue is not possible, make main module active.
;
; Exit:
;	If unable to CONT and no main module
;	   returns FALSE
;	else
;	   returns TRUE after setting up grs.oRsCur, grs.otxCur
;
;************************************************************************
cProc	ContContext,<PUBLIC,NEAR>
cBegin
	mov	ax,[grs.GRS_otxCONT]
	mov	[grs.GRS_otxCur],ax
	inc	ax			;test for UNDEFINED
	mov	ax,[grs.GRS_oRsCONT]
	jne	CanCont

	;we can't continue, use 'main' module if there is one
	mov	ax,[grs.GRS_oMrsMain]
	inc	ax
	je	CcExit			;brif no main module
	mov	[grs.GRS_otxCur],0	;grs.otxCur = 0
	dec	ax
;ax = oRs containing next stmt to be executed
CanCont:
	cCall	UiRsActivate,<ax>
	mov	ax,sp			;return TRUE
CcExit:
	or	ax,ax			;set condition codes for caller
cEnd

;************************************************************************
; boolean NeedContContext()
; Purpose:
;	Make the current MRS and PRS the one where BASIC's program counter
;	is pointing.  If CONTinue is not possible, make main module active.
;
; Exit:
;	If unable to CONT and no main module
;	   [uierr] = MSG_NoMainProg
;	   txtErr.oRs = UNDEFINED (so cursor won't be positioned by ReportError)
;	   returns FALSE
;	else
;	   returns TRUE after setting up grs.oRsCur, grs.otxCur
;
;************************************************************************
cProc	NeedContContext,<PUBLIC,NEAR>
cBegin
	call	ContContext		;activate CONT program counter
	jne	CcNoErr			;brif can CONT
	PUSHI	ax,MSG_NoMainProg
	call	SetUiErr
	xor	ax,ax			; set ax and condition codes
CcNoErr:
cEnd

sEnd	CP

end
