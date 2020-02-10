	TITLE	NamMgr.asm - Name Table manager
;****************************************************************************
;	Copyright <C> 1985, Microsoft Corporation
;Purpose:
;	This module contains the routines necessary to create and manage the
;	symbol name table.  These routines are:
;
;  CopyONamPb
;		  Copies the ASCII chars of a symbol from mrsCur.tNam.oNam.name
;		  to [pb].  The length of the string is returned.
;
;  CopyONamBd
;		  Allocates a heap entry for the uninitialized Bd and then
;		  does a CopyONamPb to Bd.pb.
;
;  GetVarNamChar
;		  Returns a first logical char of name and a flag which
;                 indicates whether the name starts with 'FN'.
;
;  FlagOfONam
;		  Returns the raw symbol mask of mrsCur.tNam.oNam.  This value
;		  can then be used to determine the various flag settings.
;
;  LnOfONam
;		  If oNam is a line number offset
;		    THEN AX=mrsCur.tNam.oNam.LnW
;		    ELSE AX=UNDEFINED
;
;  ONamOfPbCb
;		  Returns the offset of a symbol in mrsCur.tNam.  If the symbol
;		  doesn't exist, then it is added.  If a symbol match is found,
;		  but the case is different, then the new case is used to
;		  replace the old symbol.
;
;  ONamOfPsd
;		  Returns the offset of a symbol in mrsCur.tNam.  If the symbol
;		  doesn't exist, then it is added.  If a symbol match is found,
;		  but the case is different, then the new case is used to
;		  replace the old symbol.
;
;  ONamOfLn
;		  Returns the offset of a line number in mrsCur.tNam.  If the
;		  line number doesn't exist, then it is added.
;
;  ResetTNamMask
;		  Resets the all but the selected flag bits in all symbols of
;		  mrsCur.tNam (the symbol's flags are ANDed with the given
;		  byte mask).
;
;  ResetONamMask
;		  Resets the maskW selected flag bits of mrsCur.tNam.oNam
;		  (The symbol's flags are ANDed with the NOT of maskW).
;
;  SetONamMask
;		  Sets the maskW selected flag bits of mrsCur.tNam.oNam
;		  (The symbol's flags are ORed with maskW).
;
;  SetONamSpace
;                 Sets the 2 "name space" bits of the flags byte to some
;                 NMSP_ value. Returns 0 if no error, or ER_DD if either of
;		  those bits were already set.
;
;  CheckONamSpace
;		  Same error checking as SetONamSpace, but doesn't actually
;		  modify any namespace bits.
;                 
;  TNamInit
;		  Initializes the symbol table (mrsCur.tNam).
;
;  OgNamOfPsd
;                 Same as ONamOfPsd, but returns an ogNam.
;  OgNamOfPbCb
;                 Same as OgNamOfPbCb, but returns an ogNam.
;  CopyOgNamPb
;                 Same as CopyONamPb, but takes an ogNam instead of an oNam.
;  CopyOgNamPbNear
;                 Same as CopyOgNamPb, but near rather than far interface.
;  FpNamOfOgNam
;                 Given an ogNam, returns a far pointer to the name (in the
;                 actual table) and the length of the name.
;  OgNamOfONam
;                 Given an oNam for the current module, returns the ogNam
;                 for this name.
;  ONamOfOgNam
;                 Given an ogNam, returns the oNam for the current module
;                 for this name.
;  CharOfOgNam
;                 Given an ogNam, returns the first char in the name.
;  CbOfOgNam
;                 Given an ogNam, returns the length of the name.
;  BdAppendOgNam
;                 Given an ogNam and a pBd, appends the name to the end of
;                 of the bd.
;  CmpOgNamIns
;                 Given two ogNam's, performs a case-insensitive comparison
;                 of the two names.
;  ogNamOfHst
;                 Same as OgNamOfPbCb, but accepts a hst as input instead of
;                 a pb and cb.
;  hstNamOfOgNam
;                 Given an ogNam, returns a far pointer to a hst for the name.
;
;  IsOgNamOfPbCb
;		  Given an string, return OgNam of string if it exists, but
;		  don't add the string if not found in the name table.
;  IsONamOfPbCb
;		  Given an string, return ONam of string if it exists, but
;		  don't add the string if not found in the name table.
;
;    The symbol table is refered to as tNam.  It is a large far heap entry (and
;    therefore restricted to 64K).  The first part of tNam is a table of chain
;    pointers that are used to select the chain to be searched. This is actually
;    two such tables: the first (and larger one) is for all names except for
;    line numbers, while the second is just for line numbers. A hashing function
;    is applied to inputs to hash them into a given chain.  Unknown symbols are
;    added to the end of the appropriate chain.
;
;  tNam structure:
;
;	|-----------------------|
;	|  Chain Header Tables	|
;	|-----------------------|
;	|   Local variables	|
;	|-----------------------|
;	|     symbol chains	|
;	|-----------------------|
;	|	 unused 	|
;	|-----------------------|
;
;    Chain Header Tables: (pointers are NULL if no entries in chain)
;	pNameChain0 - first name chain
;	pNameChain1
;	pNameChain2
;	     .
;	     .
;	pNameChain31 - last name chain
;	pNameChain32 - first line number chain
;	     .
;	     .
;	pNameChain39 - last line number chain
;
;    Local Variables:
;	CurONamHdr - current tNam 1st char index - used by GetNextONam
;	cbUsed - # of bytes allocated to tables & entries
;
;    Each element of a symbol chain has the following format:
;
;	NextW - ptr to next symbol on chain (NULL if end of chain)
;	FlagsB - symbol flag byte
;	cbB    - # of bytes in the 'name' field
;	name   - ASCII chars of symbol name (or binary value for line numbers)
;
;	Each chain is terminated by a NULL Next.  New symbols are added to the
;	end of the appropriate chain.  A symbol can never be deleted or moved.
;	A symbol can't have more than CB_MAX_NAMENTRY chars.  Each char in the
;	name part of the symbol can be any valid ASCII (0-127) except the
;	following:
;	    <Control Char> < ' > < { > < | > < } > < ~ > <DEL>
;
;    Each module name table is limited to 32k. This limitation was imposed
;	based on the pcode generated for opAsTypeExp; as of June '87 that's
;	the only known reason for the limitation. In the (unlikely) event
;	that the 32k limit is ever a problem, this pcode restriction could
;	doubtless be removed.
;
;
;****************************************************************************

	.xlist


	NAMMGR_ASM = 1
	include 	version.inc
	includeOnce	architec
	includeOnce	context
	includeOnce	heap
	includeOnce	names
	includeOnce	qbimsgs
	includeOnce	ui
	includeOnce	util		
	.list

assumes DS,DATA
assumes SS,DATA
assumes ES,NOTHING

sBegin	DATA				
	globalW oRsNam,0		;[10] used to access appropriate tNam
sEnd	DATA				

sBegin	CP
assumes CS,CP

;***
;Use_GMrs
;Purpose:
;	Added as part of revision [10].
;
;	Set oRsNam to point to the global module so generic routines can be
;	used to support the global name table.
;
;	NOTE: Not all of the nammgr routines check for a non-zero offset in
;	NOTE: oRsNam - - - only those that are currently used in conjunction
;	NOTE: with a call to this routine. If you want to use this indirection
;	NOTE: mechanism for some nammgr support routine, ensure that it will
;	NOTE: access the name table via oRsNam.
;Entry:
;	None.
;Exit:
;	static oRsNam is an offset to the global name table in the Rs table.
;Preserves:
;	AX, BX, DX
;Exceptions:
;	none
;***************************************************************************
cProc	Use_GMrs,<NEAR,NODATA>
cBegin	Use_GMrs
	DbAssertRel  grs.GRS_oMrsCur,nz,OMRS_GLOBAL,CP,<Use_GMrs: global mrs is active>
	sub	cx,cx			; assume global mrs is active
	cmp	[grs.GRS_oMrsCur],OMRS_GLOBAL ;[23] is global mrs active?
	jz	Activate_Exit		;  brif so

	mov	cx,MRS_bdlNam + OMRS_GLOBAL ;[23] offset in the global mrs
					    ;[23] to the name table.
Activate_Exit:
	mov	[oRsNam],cx
cEnd	Use_GMrs

;***
;FetchPNam
;	Fetch a pointer to desired name table.
;	If the static oRsNam is non-zero, it is assumed to contain an
;	offset in the Rs table to the desired name table; otherwise, the
;	desired name table is assumed to reside in mrsCur.
;
;	Added as part of revision [10] to share/consolidate code, and
;	to limit access to the mrs to a specific point.
;Purpose:
;Entry:
;	none
;Exit:
;	es:bx is a pointer to the appropriate bdlNam
;Uses:
;	none
;***************************************************************************
cProc	FetchPNam,<NEAR,NODATA>
cBegin
	GETRS_SEG es,bx,<SIZE,LOAD>	;[25] NOTE: can trash bx?
	mov	bx,[oRsNam]
	or	bx,bx			
	jz	Get_PMrs		

	RS_BASE add,bx			
	jmp	short Got_PMrs		
Get_PMrs:				
	SETSEG_EQ_SS es 		
	mov	bx,dataOFFSET mrsCur.MRS_bdlNam 
Got_PMrs:
cEnd

;***
;LnOfONam		LnW = LnOfONam
;Purpose:
;		  If given oNam is a line number offset
;		    THEN return AX=mrsCur.tNam.oNam.LnW
;		    ELSE return AX=UNDEFINED
;Register conventions:
;	DS - Line# segment
;	ES - tNam segment
;Entry:
;     oNam on stack
;     The following globals are referenced:
;	mrsCur.bdlNam => current module's name table
;
;Exit:
;	if oNam represents a binary line number,
;	   AX = line number
;	else (oNam represents an alpha name)
;	   AX = UNDEFINED, DL = 1st letter of name
;	
;Uses:
;	none
;***************************************************************************
cProc	LnOfONam,<PUBLIC,FAR,NODATA>
	parmW	oNam			
cBegin
	DbChk	ConStatStructs
	DbChk	tNam
	DbChk	oNam,oNam		; sanity check on input oNam
	GETSEG	ES,[mrsCur.MRS_bdlNam_seg],,<LOAD,SIZE> ;[25]
	mov	bx,[oNam]		
	mov	ax,ES:NM_NAME[bx]	;return the 16 bit line number
	mov	dx,ax			;dl = 1st letter of alpha name
	test	ES:NM_FLAGS[bx],byte ptr NM_fLineNum ;is it a line #
	jne	LnExit			;brif a line# - AX=line number
	mov	ax,UNDEFINED		;return UNDEFINED for alpha name
LnExit:
cEnd

;***
;ONamOfLn		oNamW = ONamOfLn
;Purpose:
;	To return the Name Table offset of a line number.  If the line number
;	isn't already in the Name Table, then it is added.  Line numbers are
;	kept in their own seperate chains in tNam.  A '0' is returned in AX to
;	indicate an error condition (out of memory, etc).
;
;	NOTE: Shares a common exit with ONamOfPbCb, so these must have identical
;		entries.
;Entry:
;       AX = 16 bit line number
;     The following globals are referenced
;	mrsCur.bdlNam => current module's name table
;
;Exit:
;	AX - Name table offset of LnW
;	     0 is returned for error conditions (out of memory)
;	DL = tNam.oNam.flags if AX != 0
;	PSW flags set via an OR AX,AX on exit
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	ONamOfLn,<PUBLIC,NEAR,NODATA>,<DI>
	LocalW	pbName			; These locals match those in
	LocalW	cbName			;	ONamOfPbCb because we share a
	LocalW	pNamCur 		;	common exit
	LocalB	fGrowTNam		
	LocalB	fDontReplaceNam 	
	LocalW	lineNum 		
	LocalV	bdlTmp,<SIZE BDL>	
cBegin	
	mov	[lineNum],ax
	lea	ax,[lineNum]		;parm to ONamOfPbCb (ptr to 'name')
	mov	cx,2			;parm to ONamOfPbCb (size of 'name')
	mov	di,LineNumHdr1st	;ptr to first line number hash chain
	mov	dl,7			;only 8 chains for line numbers
	mov	bx,0FFFFH		;ensure we match only the correct number
	jmp	short ONamOf_Common
cEnd	<nogen>

;***
;ONamOfPsd		oNamW = ONamOfPsd(psdW)
;Purpose:
;	To return the Name Table offset of a symbol.  If the symbol isn't
;	already in the Name Table, then it is added.  The symbol's case is
;	preserved in tNam , but it is ignored while searching for a match
;	(FoO is the same as fOo). If the cases don't match exactly, then the
;	new symbol will replace the old one in tNam.  A 0 is returned in AX
;	to indicate an error condition (out of memory, etc).
;
;	NOTE: shares code with and exits via ONamOfPbCb.
;Assumptions:
;	)symbols can never be removed from tNam
;	)Each char in the name part of the symbol can be any valid
;	 ASCII (0-127) except the following:
;	 <Control Char>  < ' >  < { >  < | >  < } >  < ~ >  <DEL>
;	)0 < cb < CB_MAX_NAMENTRY
;Entry:
;     The stack contains the following parameters (PLM calling convention)
;	ParmW - ptr to a string descriptor
;     The following globals are referenced
;	mrsCur.bdlNam => current module's name table
;
;Exit:
;	AX - Name table offset of name in psd
;	     0 is returned for error conditions (out of memory)
;	DL = tNam.oNam.flags if AX != 0
;	PSW flags set via an OR AX,AX on exit
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	ONamOfPsd,<PUBLIC,NEAR,NODATA>
cBegin	<nogen>
	pop	dx			;return address to caller
	pop	bx			;psd parm passed by caller
	mov	ax,[bx.BD_pb]		;ptr to string contents
	mov	cx,[bx.BD_cbLogical]	;byte count
	DbAssertRel  cx,be,CB_MAX_NAMENTRY,CP,<ONamOfPsd: psd.cb too large>
	push	dx			;put retval back on stack
cEnd	<nogen>				;fall into ONamOfPbCb

;***
;ONamOfPbCb		oNamW = ONamOfPbCb
;Purpose:
;	To return the Name Table offset of a symbol.  If the symbol isn't
;	already in the Name Table, then it is added.  The symbol's case is
;	preserved in tNam , but it is ignored while searching for a match
;	(FoO is the same as fOo). If the cases don't match exactly, then the
;	new symbol will replace the old one in tNam by default; this replacement
;	is suppressed if the Cb input word has it's high bit set. A 0 is 
;	returned in AX to indicate an error condition (out of memory, etc).
;Assumptions:
;	)symbols can never be removed from tNam
;	)Each char in the name part of the symbol can be any valid
;	 ASCII (0-127) except the following:
;	 <Control Char>  < ' >  < { >  < | >  < } >  < ~ >  <DEL>
;	)0 < cb < CB_MAX_NAMENTRY
;Register conventions:
;	DS - pbName segment
;	ES - tNam segment
;	SI - pbName pointer
;	DI - tNam pointer
;Entry:
;	AX = ptr to ASCII string
;	CX = length of ASCII string
;		If the high bit is set and the name is found, the existing
;		name is NOT to be replaced in the table.
;     The following globals are referenced
;	mrsCur.bdlNam => current module's name table
;
;Exit:
;	AX - Name table offset of PbCb input string
;	     0 is returned for error conditions (out of memory).
;	     Special: when fDontCreateName != 0, ax = 0 indicates that the
;	     name wasn't found (and was NOT added to the table).
;	DL = tNam.oNam.flags if AX != 0
;	PSW flags set via an OR AX,AX on exit
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	ONamOfPbCb,<PUBLIC,NEAR,NODATA>,<DI> ;NOTE: matches entry to ONamOfLn
	LocalW	pbName			;ptr to string containing symbol
	LocalW	cbName			;# of bytes in *pbName
	LocalW	pNamCur
	LocalB	fGrowTNam		;set if tNam needs to be grown
					;before exiting
	LocalB	fDontReplaceNam		;FALSE if we should replace name in tbl
	LocalW	lineNum			;line number here, if entry via ONamOfLn
	LocalV	bdlTmp,<SIZE BDL>	
cBegin
	mov	di,tNam 		; not using line # hash table
	mov	bx,0DFDFH-1		;upper case mask for NextChar loop
	mov	dl,31d			;32 hash chains for names (not line #'s)
ONamOf_Common:				;entry from ONamOfLn
	DbChk	tNam
	push	si
	xor	dh,dh
	mov	[fGrowTNam],dh		;assume that tNam doesn't need to grow
	or	ch,ch
	jns	SetReplaceNam		;brif assumption correct

	and	ch,07FH			;reset high bit
	inc	dh			;Don't replace the name in the name
					;  table if entry found, i.e., leave
					;  case of existing name as-is.
SetReplaceNam:
	mov	[fDontReplaceNam],dh
	DbChk	ConStatStructs
	mov	[pbName],ax		;save input
	mov	[cbName],cx		;save input
	xchg	ax,si			;ptr to Name string in SI
	push	bx			; preserve mask
	call	FetchPNam		; bx points to desired mrs
	GETSEG	ES,PTRRS[bx.BDL_seg],,<SIZE,LOAD> ;[10][6][25]
	pop	bx			; restore mask

	DbAssertRel  cx,be,CB_MAX_NAMENTRY,CP,<ONamOfPbCb: cbName too large>
	DbAssertRel  cx,nz,0,CP,<ONamOfPbCb: cbName is 0>
	lodsb				;al = 1st byte of symbol to search for
	dec	cx
	jcxz	Mask_Hash

	dec	cx
	jcxz	Grab_Last		;brif just 2 chars
	
	xor	al,[si]			;hash in second char of name
Grab_Last:				;grab last char for use in hash
	add	si,cx			;point to last char in name
	xor	al,[si]			;use last char in name for hash too
Mask_Hash:
	inc	bx
	jnz	Mask_Hash2		;brif not a numeric - - (must not shift)
					;NOTE: as long as we just use the low
					;	5 bits of this for the hash
					;	index, we don't need to worry
					;	about upper vs. lower case
					;	letters.
	shr	al,1			;Line numbers tend to end in 0; don't
					;  use least significant bit to provide
					;  a more even hash distribution
	dec	bx			
Mask_Hash2:
	;at this point, BX = 0FFFF for Line Number, 0DFDF otherwise
	xor	dh,dh
	and	ax,dx	 		;get the 1st char hash value by doing
					;doing a MOD 32 (or MOD 8 for line #'s).
					;This value is the byte index into
					;tNam which is used to obtain the
					;ptr to the appropriate symbol chain
	shl	ax,1			;convert to word index
	add	di,ax			;DI = offset to 1st char chain ptr,
					;NOTE - this works because tNam
					;is a far heap ( ES:0 => tNam )
	mov	[pNamCur],di		;pass to AddName (if a null chain)
	mov	dx,ES:[di]		;offset of first symbol in chain
					;or 0 if no symbols on chain
NextName:
	mov	cx,[cbName]		;CX=# of bytes in pbName

NextNameCX:				;this label allows the above 'mov cx'
					;to be removed from the inner loop of
					;NextName
	mov	di,dx
	or	di,di			;test ptr to next symbol
	je	AddName 		;brif end of chain encountered

	mov	dx,ES:[di]		;dx=ptr to next symbol in the chain
	mov	[pNamCur],di		;pass to AddName or MapExit
	cmp	ES:NM_SIZE[di],cl	;are both strings the same size?
	jne	NextNameCX		;brif different sizes

	mov	si,[pbName]		;pb of callers name
	add	di,NM_NAME		;DI = ptr to 1st ASCII char of symbol
NextChar:
	lodsb				;AL=next char from pbName
	mov	ah,ES:[di]		;AH=next char from tNam
	inc	di			;bump tNam index
	and	ax,bx			;BX contains 0DFDFH, so this converts
					;AH & AL to upper case by resetting
					;their respective bit 5's. This
					;mapping works because there are
					;no control or special chars in the
					;symbol names.
	cmp	ah,al			;is pbName[SI]=tNam[DI]
	jne	NextName		;brif chars not the same
	loop	NextChar		;loop till mismatch or all chars tested
;
;We found a case insensitive match - if the names are exactly the same, then
;just quit (typical case?), otherwise, tell the UI that it needs to redraw
;the debug screen, and replace the name in the table

	cmp	[fDontReplaceNam],FALSE	;want to replace the name in the table?
	jz	DoReplaceName		;  brif so
MapExit1:				
	jmp	MapExit 		;  nope - just exit

DoReplaceName:
	mov	di,[pNamCur]		;addr of symbol record that matched
	mov	si,[pbName]		;pb of symbol to add
	mov	cx,[cbName]		;# of bytes in pbName
	add	di,NM_NAME		;offset to 1st byte of new entry name
	repz	cmpsb			;are names exactly the same?
	jz	MapExit1		;  brif so - exit

	push	es			;preserve across call
	call	DrawDebugScrFar		;inform UI
	pop	es
	mov	di,[pNamCur]		;addr of symbol record that matched
	jmp	short CopyString	;copy DS:[pbName] to ES:[DI+NM_NAME]
;
; Add pbName to the end of the current chain

AddName:
	xor	ax,ax			;0 for initializing new fields
	mov	[fGrowTNam],TRUE	;tNam needs to grow
;
; make the last entry point to the next empty symbol
;
	mov	di,ES:[cbUsed]		;# of allocated bytes in tNam
					;this is also the addr of the next
					;available byte since cbLogical is
					;greater than cbUsed
	mov	ES:NM_NEXT[di],ax	;NULL ptr for new record
	inc	bx			;is this name for a Line Number?
	jnz	Not_Ln			;  brif not

	mov	al,NM_fLineNum		;set flag to recall that this is a Line#
Not_Ln:
	mov	ES:[di+NM_FLAGS],al	;initialize flags to OFF state
;
; copy the pb string to [ES:DI.name]

CopyString:
	mov	si,[pbName]		;pb of symbol to add
	mov	cx,[cbName]		;# of bytes in pbName
	mov	ES:NM_SIZE[di],cl	;update length field of symbol
					;NOTE - this may be redundant
	add	di,NM_NAME		;offset to 1st byte of new entry name
	rep	movsb			;copy from DS:pbName[SI] to ES:tNam[DI]
	cmp	[fGrowTNam],FALSE	;do we need to grow tNam
	je	MapExit 		;brif symbol wasn't added - don't need
					;to grow tNam
;
; Make sure there will be enough room to add another symbol the next time thru.
; TNam can now be safely grown since the string has been copied and heap
; movement is no longer a problem.
;

	mov	cx,[oRsNam]		;[10]
	jcxz	MrsCur_Okay		; brif want to grow bdl in mrsCur

	; We want to grow the bdlNam for the global mrs, but it's not
	; active. It must be active because (1) cannot safely grow a bdl
	; when it's in a local heap table, and (2) we need to know where
	; the owner of the bdl is in case of heap movement, to refresh es.
	; Therefore, we active and then deactivate the global mrs.
	; NOTE: This assumes that the only reason pMrsNam is non-zero is
	; for global mrs support.
	; Note: we save both oPrsCur AND oMrsCur here instead of oRsCur
	;	    to protect against the special case where a declared/ref'd
	;	    but not defined prs is active here ... in this special case,
	;	    RsActivate of oRsCur would leave the global mrs active ...
	push	[grs.GRS_oPrsCur]	; save for later restore
	push	[grs.GRS_oMrsCur]	; save for later restore
	PUSHI	ax,OMRS_GLOBAL		;[23] oMrs of global mrs
	call	MrsActivateCP		; activate the global mrs

MrsCur_Okay:				
	mov	bx,dataOFFSET mrsCur.MRS_bdlNam	; pointer to tNam bdl
	mov	ax,CB_MAX_NAMENTRY+NM_NAME+NM_NAME 
					; max # of bytes needed to add
					;the next symbol plus the
				 	;overhead needed by the
					;current symbol.
	add	ax,[cbName]		;plus the _NAME field bytes used by
					;the current symbol
	mov	di,[pNamCur]		;addr of last searched name entry

	push	bx			; pass pbdlNam to BdlCheckFree
	push	ax			;pass byte count to BdlCheckFree
	call	BdlCheckFree		;make sure there is enough room in tNam
					;to add another symbol
	xchg	ax,si			; save return code across poss. call
	mov	cx,[oRsNam]		;[10]
	jcxz	MrsCur_Okay1		; brif mrsCur is to be left alone

	; The oRs of the context that was active on input is on the top
	; of the stack. Activate this again, and then refresh pMrsNam in
	; case the mrs table moved and pMrsNam will be used again.
	mov	[oRsNam],0		; reset so global mrs access in
					; MrsActivateCP will be correct
	call	MrsActivateCP		; reactivate input oMrsCur
	call	PrsActivateCP		; reactivate input oMrsCur
	call	Use_GMrs		

MrsCur_Okay1:				
	sub	ax,ax			; in case of error
	or	si,si			; BdlCheckFree return code
	je	ONamOfPbCb_Exit		;brif not enough memory - the current
					;name isn't added to tNam - even
					;though the _NAME field has been
					;updated.
	mov	si,[cbName]		;# of bytes in new symbol
	add	si,NM_NAME		;compensate for header bytes
					;SI = # of bytes in new entry
	call	FetchPNam		; bx points to desired mrs
	mov	dx,PTRRS[bx.BDL_cbLogical] ;[10] # of bytes in Name Table
	add	dx,si			   ; + padding for next symbol
	js	ONamOfPbCb_Exit		;brif table has grown > 32k
					;  see module header for reason(s) for
					;  this restriction
	mov	PTRRS[bx.BDL_cbLogical],dx  ;[10]
	
	GETSEG	ES,PTRRS[bx.BDL_seg],,<SIZE,LOAD> ;[14][10][6]
					 ;	  may have been changed by the
					 ;	  call to BdlCheckFree

					;make the last entry point to the 
					; next empty symbol
	mov	ax,ES:cbUsed		;# of tNam bytes already used
					;this is also the addr of the next
					;available byte since cbLogical is
					;greater than cbUsed
	mov	ES:[di],ax		;make the last entry point to new entry
	mov	di,ax			;return new oNam to caller
	add	ax,si			;cbUsed + size of new entry
	mov	ES:cbUsed,ax		;# of bytes used by the Name Table
	mov	[pNamCur],di		;return new oNam to caller
MapExit:
	mov	bx,[pNamCur]		;return name offset to caller in AX
	mov	dl,ES:NM_FLAGS[bx] 	;get the flag byte
	xchg	ax,bx
ONamOfPbCb_Exit:
	pop	si
	or	ax,ax			;set flags for callers
cEnd



;***
;TNamInit		TNamInit()
;Purpose:
;	To initialize the Name Table structure and any other data used by
;	the Name Table Manager.  A zero is returned in AX if there was not
;	enough memory.
;Entry:
;	sbToUse - 0 means use any sb for this table. If non-zero, then this
;			sb value is to be used.
;	mrsCur.bdlNam = current module's name table
;Exit:
;	AX = 0 if not enough memory was available
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	TNamInit,<PUBLIC,NEAR,NODATA>,<DI>
	parmW	sbToUse 		
cBegin
	mov	ax,dataOFFSET mrsCur.MRS_bdlNam  ; ptr to tNam bdl
	push	ax			; parm to BdlAlloc
	cCall	BdlFree,<ax>		; in case name table is an owner
	mov	ax,NM_INIT_TABLE_SZ	;# of bytes to allocate
	push	ax			;pass low word of cb to BdlAlloc
	PUSHBDL_TYPE  pgtypEBNameTable,ax ; pass sb type for EB version
	mov	cx,[sbToUse]		
	jcxz	Use_Any_Sb

	push	cx			
	call	BdlAllocSb		
	jmp	short TNamInit_Cont	
Use_Any_Sb:				
	call	BdlAlloc		;allocate a large far heap for NamTab
TNamInit_Cont:
	or	ax,ax			;check return code
	je	TNIExit 		;brif heap wasn't allocated
;
;we now own a far heap entry - initialize it
;
	GETSEG	ES,[mrsCur.MRS_bdlNam_seg],,<SIZE,LOAD> ;[6]

	mov	cx,mrsCur.MRS_bdlNam_cPhysical ;# of paragraphs in Name Table
	shl	cx,1			;convert cPara to cBytes
	shl	cx,1			;  (under DOS 5 this already is cBytes)
	shl	cx,1
	shl	cx,1
	mov	di,tNam 		;di=base addr of tNam
					;NOTE - tNam is a far heap
	sub	cx,di			; zero-fill all but initial bytes
	sub	ax,ax			;AX=0
	REP	STOSB			;Initialize the Name table
	mov	ax,oNamFirst		;offset to 1st usable entry
	mov	ES:cbUsed,ax		;# of bytes currently being used
					;by tNam
	add	ax,NM_NAME+CB_MAX_NAMENTRY 
					; # of bytes currently being used
					;by tNam + enough padding to add
					;another symbol
	mov	mrsCur.MRS_bdlNam_cbLogical,ax ;total size of tNam
TNIExit:
cEnd

;***
;CopyONamPb		cbW = CopyONamPb
;Purpose:
;	To copy the ASCII chars in a name table entry to the callers buffer.
;	The # of ASCII characters copied is returned.
;Assumptions:
;	pbW contains room for at least CB_MAX_NAMENTRY chars
;Register conventions:
;	DS - tNam segment
;	ES - pb segment
;	SI - tNam pointer
;	DI - pb pointer
;Entry:
;	pbBuf - ptr to a buffer
;	oNam  - symbol's name table offset into mrsCur.tNam
;     The following data is referenced:
;	if oRsNam is <> 0, then we use that to fetch name table segment,
;	else mrsCur.bdlNam contains current module's name table segment.
;Exit:
;	AX - # of ASCII chars in mrsCur.tNam.oNam.name
;		These chars are copied into the given buffer.
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	CopyONamPb,<PUBLIC,FAR,NODATA>,<SI,DI,DS>
	parmW	pbBuf			
	parmW	oNam			
cBegin
	DbChk	ConStatStructs
	DbChk	tNam
	DbChk	oNam,oNam		; sanity check on input oNam
	mov	si,[oNam] 		; SI = tNam.oNam
					;NOTE - this works because tNam
					;is a far heap ( ES:0 => tNam )
	call	FetchPNam		; bx points to desired mrs
	GETSEG	DS,PTRRS[bx.BDL_seg],,<SIZE,LOAD> ;[14][10][6]
	push	SS			;callers pb segment
	pop	ES			;ES = callers pb segment
	mov	di,[pbBuf]		
	sub	cx,cx			;CX=0
	mov	cl,NM_SIZE[si]		;# of ASCII bytes in oNam
	mov	ax,cx			;return cbW to caller
	add	si,NM_NAME		;SI = tNam.oNam.name
	rep	movsb			;copy tNam.oNam.nam to pb
cEnd

;***
;CopyONamBd		cbW = CopyONamBd(oNamW,pBdW)
;Purpose:
;	Use oNam to determine the symbol's size and then use that size to
;	allocate a heap entry for pBd.	The ASCII chars in the symbol name
;	are then copied to the heap entry.  pBd must point to an un-initialized
;	Bd (ie - Bd doesn't currently own a heap entry)!
;Entry:
;     The stack contains the following parameters (PLM calling convention)
;	ParmW - symbol's name table offset
;	ParmW - ptr to uninitialized Bd
;Exit:
;	AX =  0 if Bd allocation was unsucessful
;	   =  mrsCur.tNam.oNam.cb if Bd allocation was sucessful
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	CopyONamBd,<PUBLIC,NEAR,NODATA>,<SI,DI>
	parmW	oNam		;symbol's offset into tNam
	parmW	pBd		;ptr to uninitialized Bd
cBegin
	DbChk	ConStatStructs
	DbChk	tNam
	DbChk	oNam,[oNam]		;sanity check on input oNam
	mov	si,[oNam] 		;SI = tNam.oNam
					;NOTE - this works because tNam
					;is a far heap ( ES:0 => tNam )
	sub	cx,cx			;CX=0
	GETSEG	ES,[mrsCur.MRS_bdlNam_seg],,<SIZE,LOAD> ;[6]
	mov	cl,ES:NM_SIZE[si]	;# of ASCII bytes in oNam
	push	cx			;save & pass to BdAlloc

	mov	ax,IT_NO_OWNERS 	;pass bdType to BdAlloc
	cCall	BdAlloc,<pBd,cx,ax>	;allocate a Bd to copy oNam to
	pop	cx
	or	ax,ax			;was the allocation sucessful
	je	CopyONamBdExit		;brif unsucessful
	push	DS			;ES = DGROUP (for the 'rep movsb')
	pop	ES
	GETSEG	DS,[mrsCur.MRS_bdlNam_seg],,<SIZE,LOAD> ;[6]
					;refresh - BdAlloc could have moved it
assumes DS,NOTHING
	mov	ax,cx			;return mrsCur.tNam.oNam.cbW to caller
	add	si,NM_NAME		;SI = tNam.oNam.name (source ptr)
	mov	di,[pBd]		;ptr to Bd to copy to
	mov	di,ES:[di.BD_pb]	;ptr to buf to copy oNam to (dest ptr)
	rep	movsb			;copy tNam.oNam.nam to pb
	push	ES			;DS = DGROUP (for the 'rep movsb')
	pop	DS
assumes DS,DATA
CopyONamBdExit:
cEnd

;***
;GetVarNamChar		ushort = GetVarNamChar(oNamW)
;Purpose:
;	Given an oNam, the logical first char of the name for use in 
;	determining the default type of a variable by that name (i.e.,
;	bypass a leading 'FN' in the name).
;	For versions supporting DEF FN's, also return a flag indicating 
;	whether the name starts with 'FN' or not.
;Entry:
;	oNamW - symbol's name table offset into mrsCur.tNam
;Exit:
;	AL - logical first char of variable name, forced to upper case
;	ife FV_QB4LANG
;		AH - 0 if name doesn't start with 'FN', non-zero if it does
;	endif
;Uses:
;	none.
;Preserves:
;	DX, ES
;Exceptions:
;	none.
;****************************************************************************
cProc	GetVarNamChar,<PUBLIC,NEAR,NODATA>,<ES>
	parmW	oNam
cBegin
	DbChk	oNam,[oNam]		;sanity check on input oNam
	DbChk	tNam
	DbChk	ConStatStructs
	GETSEG	es,[mrsCur.MRS_bdlNam_seg],,<SIZE,LOAD> ;[6]
	mov	bx,[oNam]
	add	bx,NM_NAME		;add table start ptr to offset
	mov	al,es:[bx]		;fetch first char of name
	and	al,0DFh			;force to upper case
	xor	ah,ah
	cmp	al,'F'
	jnz	GetVarChar_Exit		;brif doesn't start with 'FN'

	mov	cl,es:[bx-(NM_NAME - NM_SIZE)] ; fetch length of name
	cmp	cl,2			; long enough to be a DEF FN name?
	jbe	GetVarChar_Exit 	; brif not

	mov	cl,es:[bx+1]		;fetch second char of name
	and	cl,0DFh			;force to upper case
	cmp	cl,'N'
	jnz	GetVarChar_Exit		;brif doesn't start with 'FN'

	mov	al,es:[bx+2]		;fetch third char of name
	and	al,0DFh			;force to upper case
	inc	ah			;set "found 'FN'" flag
GetVarChar_Exit:
cEnd


;***
;FlagOfONam		FlagW = FlagOfONam(oNamW)
;Purpose:
;	To get a symbols flag byte from the symbol table.
;Entry:
;     The stack contains the following parameters (PLM calling convention)
;	ParmW - symbol's name table offset
;     The following globals are referenced
;	mrsCur.bdlNam => current module's name table
;Exit:
;	AX = tNam.oNam.flags
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	FlagOfONam,<PUBLIC,NEAR,NODATA>,<DS>
	parmW	oNam		;symbol's offset into tNam
cBegin
	DbChk	oNam,[oNam]		;sanity check on input oNam
	DbChk	tNam
	DbChk	ConStatStructs
	GETSEG	ds,[mrsCur.MRS_bdlNam_seg],,<SIZE,LOAD> ;[6]
	mov	bx,oNam 		;bx = tNam.oNam
					;NOTE - this works because tNam
					;is a far heap ( DS:0 => tNam )
	sub	ax,ax			;AH=0 - for returning a ushort
	mov	al,NM_FLAGS[bx] 	;get the flag byte
cEnd

;***
;SetONamMask, SetONamSpace, CheckONamSpace
;Purpose:
;	To selectively set individual bits in a symbols FLAG byte.
;	For SetONamSpace, 
;		sets the 2 "name space" bits of the flags byte to some
;      		NMSP_ value. Returns 0 if no error, or ER_DD if either of
;		those bits were already set.
;	For CheckONamSpace,
;		catches the error where a given bit is already set, but
;		doesn't actually set the namespace.
;Entry:
;     The stack contains the following parameters (PLM calling convention)
;	ParmW - symbol's name table offset
;	ParmB - mask to be ORed with symbols FLAG byte
;     The following globals are referenced
;	mrsCur.bdlNam => current module's name table
;Exit:
;	tNam.oNam.flags = tNam.oNam.flags OR maskW
;	For SetONamSpace, AX = 0 if no error, else AX = ER_DD.
;			  PSW.Z set if no error 
;	dl = old value of oNams flags before the new bit was set.
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
PUBLIC	CheckONamSpace		
CheckONamSpace: 		
	mov	ch,0FFH 	; don't set namespace bits
	SKIP2_PSW		
PUBLIC	SetONamSpace
SetONamSpace:
	mov	ch,0		; do set namespace bits
	mov	cl,0FFH 	; do check for errors
	SKIP2_PSW
PUBLIC	SetONamMask
SetONamMask:
	xor	cx,cx
cProc	SetONamMaskGen,<NEAR,NODATA>,<DS>
	parmW	oNam		;symbol's offset into tNam
	parmB	orMask		;bit mask - value to be ORed with NM_FLAGS
cBegin
	DbChk	ConStatStructs
	DbChk	tNam
	GETSEG	ds,[mrsCur.MRS_bdlNam_seg],,<SIZE,LOAD> ;[6]
	mov	bx,[oNam] 		;bx = tNam.oNam
					;NOTE - this works because tNam
					;is a far heap ( ES:0 => tNam )
	DbChk	oNam,bx			;sanity check on input oNam
	mov	dh,[orMask]
	jcxz	NMSP_Okay

	xor	ax,ax			;assume no error for SetONamSpace retval
	mov	dl,NM_FLAGS[bx]		;fetch flags as they were on entry
	and	dl,NMSP_MASK		;had one of the NMSP_ bits been set?
	jz	NMSP_Okay		;  brif not

	cmp	dl,dh			;were those bits same as input?
	jz	NMSP_Okay		;  brif so

	mov	al,ER_DD
	xor	dh,dh			;don't alter table if error
NMSP_Okay:
	or	ch,ch			; want to actually modify flag bits?
	jnz	@F			; brif not - - - exit

	mov	dl,NM_FLAGS[bx] 	;return old value in dl
	or	NM_FLAGS[bx],dh 	;set all MASKed bits
@@:					
	or	ax,ax			;set PSW flags
cEnd

;***
;ResetONamMask		ResetONamMask
;Purpose:
;	To selectively reset individual bits in a symbols FLAG byte.
;Entry:
;	AL = mask to be NANDed with symbols FLAG byte
;	BX = symbol's name table offset
;     The following globals are referenced
;	mrsCur.bdlNam => current module's name table
;Exit:
;	tNam.oNam.flags = tNam.oNam.flags OR maskW
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	ResetONamMask,<PUBLIC,NEAR,NODATA>,<DS>
cBegin
	DbChk	ConStatStructs
	DbChk	tNam
	DbChk	oNam,bx			;sanity check on input oNam
	GETSEG	ds,[mrsCur.MRS_bdlNam_seg],,<SIZE,LOAD> ;[6]
	not	al			;invert the mask
	or	al,NM_fLineNum		;don't let anyone reset this flag
	and	NM_FLAGS[bx],al 	;reset all MASKed bits
cEnd

;***
;ResetONamMaskTmp
;Purpose:
;	C code access to ResetONamMask
;Entry:
;     The stack contains the following parameters (PLM calling convention)
;	mask to be NANDed with symbols FLAG byte
;	symbol's name table offset
;     The following globals are referenced
;	mrsCur.bdlNam => current module's name table
;Exit:
;	tNam.oNam.flags = tNam.oNam.flags OR maskW
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	ResetONamMaskTmp,<PUBLIC,NEAR,NODATA>
	parmW	oNam
	parmB	nandMask
cBegin
	mov	al,[nandMask]
	mov	bx,[oNam]
	call	ResetONamMask
cEnd

;***
;ResetTNamMask		ResetTNamMask(maskW)
;Purpose:
;	Resets the all but the selected flag bits in all symbols of 
;	mrsCur.tNam (the symbol's flags are ANDed with the given byte mask).
;
;	Note that we handle the NMSP_ enumerated bits specially to ensure
;	that the NMSP_SUB constant is never changed.
;Entry:
;	AL - mask to be ANDed with all symbol FLAG bytes
;     The following globals are referenced
;	mrsCur.bdlNam => current module's name table
;Exit:
;	none.
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	ResetTNamMask,<PUBLIC,NEAR,NODATA>,<SI,DS>
cBegin
	DbChk	ConStatStructs
	DbChk	tNam
	GETSEG	ds,[mrsCur.MRS_bdlNam_seg],,<SIZE,LOAD> ;[6]
	xor	si,si			;SI=0 - make GetNextONam return
					;	FirstONam
	or	al,NM_fLineNum+NM_fLineNumLabel ;don't let anyone reset these

	;if NMSP_SUB is set for a name, leave it alone, otherwise, allow
	;caller to reset either both NMSP_ bits or neither
	mov	ah,al
	mov	cl,NMSP_SUB
NextTNamMask:
	call	GetNextONam		;SI = pNextONam
	je	ResetTNamMaskExit	;brif no more oNams
	test	BYTE PTR NM_FLAGS[si],080H
	jnz	NextTNam_Cont		;brif NMSP_Variable or _Constant set
					;  (need this because NMSP_'s are
					;   enumerated - - see .errnz's above)
	or	al,cl			;don't mask off NMSP_SUB flag
NextTNam_Cont:
	and	NM_FLAGS[si],al 	;reset all MASKed bits
	mov	al,ah
	jmp	NextTNamMask		;loop till all oNam's visited
ResetTNamMaskExit:
cEnd

;***
;GetNextoNam
;Purpose:
;	To get the next oNam in tNam
;Entry:
;	DS - tNam segment
;	SI - current oNam - if 0 then return 1st oNam
;	DS:CurONamHdr - current tNam 1st char index
;
;Exit:
;	PSW - IF zero flag set then no more valid oNam's exist
;	SI - next oNam
;	NextONamHdr  - next tNam 1st char index
;Uses:
;	dx
;***************************************************************************
	PUBLIC	GetNextONam
GetNextONam:
	or	si,si			;should we initialize
	jne	NextoNam		;brif initialization not desired
	mov	word ptr DS:CurONamHdr,tNam - 2  
					;offset of 1st valid chain header-2
NextChain:
	mov	dx,DS:CurONamHdr	;offset of current tNam chain header
	inc	dx			;word ptr to next chain header
	inc	dx
	cmp	dx,LineNumHdrLast	;check for the end of tNam header tbl
	mov	DS:CurONamHdr,dx	;update
	ja	GotNextoNam		;brif all chain headers visited

	mov	si,dx			;start with 1st symbol in cur chain
NextoNam:
	mov	si,[si] 		;ptr to next symbol in cur hdr chain
	or	si,si			;test for end of cur chain
	je	NextChain		;brif no more symbols in cur hdr chain
GotNextoNam:
	or	si,si			;set cond codes for caller
	ret

;=============================================================================
;
;	   Global Name Table Support (ogNam support routines)
;
;=============================================================================

;***
;OgNamOfPsd
;Purpose:
;	Same as ONamOfPsd, but returns an ogNam, not an oNam
;
;	Added as part of revision [4]
;Entry:
;	a psd on the stack
;Exit:
;	ax = ogNam or 0
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	OgNamOfPsd,<PUBLIC,FAR,NODATA>
	parmW	psdName
cBegin
	call	Use_GMrs		; point oRsNam to global name table
	cCall	ONamOfPsd,<psdName>	;returns ax = ogNam or 0 for error
	mov	[oRsNam],0		;[10] reset pointer to default
cEnd

;***
;OgNamOfPbCb
;Purpose:
;	Same as ONamOfPbCb, but returns an ogNam, not an oNam
;
;	Added as part of revision [4]
;Entry:
;	pbName and cbName on stack
;Exit:
;	ax = ogNam or 0
;	PSW flags set based on an OR AX,AX on exit
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	OgNamOfPbCb,<PUBLIC,NEAR,NODATA>
	parmW	pbName
	parmW	cbName
cBegin
	call	Use_GMrs		; point oRsNam to global name table
	mov	ax,[pbName]
	mov	cx,[cbName]
	cCall	ONamOfPbCb		;returns ax = ogNam or 0 for error
	mov	[oRsNam],0		;[10] reset pointer to default
cEnd

;***
;CopyOgNamPb, CopyOgNamPbNear
;Purpose:
;	Given an ogNam and a short pointer to a buffer, copy the name to the
;	buffer and return the length of the name
;
;	Added as part of revision [4]
;Entry:
;	CopyOgNamPbNear:
;		ax = pDest
;		bx = ogNam
;	CopyOgNamPb: parameters are on the stack
;		pDest
;		ogNam
;Exit:
;	ax - # of chars in the name, that were copied into the given buffer.
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	CopyOgNamPbNear,<PUBLIC,NEAR,NODATA>
cBegin
	DbChk	ogNam,bx		
	call	Use_GMrs		; point oRsNam to global name table
	cCall	CopyONamPb,<ax,bx>	; perform copy, return ax = cbName
	mov	[oRsNam],0		;[10] reset pointer to default
cEnd

cProc	CopyOgNamPb,<PUBLIC,FAR,NODATA>
	parmW	pDest
	parmW	ogNam
cBegin
	mov	ax,[pDest]
	mov	bx,[ogNam]
	cCall	CopyOgNamPbNear		;perform copy, return ax = cbName
cEnd

;***
;FpNamOfOgNam
;Purpose:
;	Given an ogNam, return a far pointer to, and the length of, the name.
;
;	Added as part of revision [4]
;Entry:
;	ogNam on stack
;Exit:
;	es:bx points to name
;	es:dx also points to name (i.e., dx = bx)
;	cx = cbName
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	FpNamOfOgNam,<PUBLIC,NEAR,NODATA>
	parmW	ogNam
cBegin
	DbChk	ogNam,ogNam		
	lea	bx,[mrsCur.MRS_bdlNam]	 ; assume global mrs active
	cmp	[grs.GRS_oMrsCur],OMRS_GLOBAL ; is global mrs active?
	jz	@F			;  brif so

	GETRS_SEG es,bx,<SIZE,LOAD>	
	mov	bx,MRS_bdlNam_seg + OMRS_GLOBAL ; offset in the global mrs
					    ; to the name table.
	RS_BASE add,bx			
@@:					
	GETSEG es,PTRRS[bx],,<SIZE,LOAD>  
	mov	bx,[ogNam]
	sub	cx,cx
	mov	cl,es:[bx.NM_SIZE]
	add	bx,NM_NAME		;es:bx points to name
	mov	dx,bx			;es:dx also points to name
cEnd

;***
;OgNamOfONam
;Purpose:
;	Given an oNam, return the ogNam for this name. 
;
;	Added as part of revision [4]
;Entry:
;	an oNam for the current module on the stack
;Exit:
;	ax = ogNam or 0
;	PSW flags set based on an OR AX,AX on exit
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	OgNamOfONam,<PUBLIC,NEAR,NODATA>
	parmW	oNam
	localV	namTmp,CB_MAX_NAMENTRY	; alloc space for max. name on stack
cBegin
	lea	ax,namTmp
	push	ax			;parm1 to OgNamOfPbCb
	push	ax			; parm1 to CopyONamPb
	push	[oNam]			; parm2 to CopyONamPb
	DbChk	oNam,oNam		;[11]
	call	CopyONamPb		;necessary, as OgNamOfPbCb expects
					;as DGROUP-relative pointer
	push	ax			;parm2 to OgNamOfPbCb
	call	OgNamOfPbCb
cEnd

;***
;ONamOfOgNam
;Purpose:
;	Given an ogNam, return the oNam for this name. 
;
;	Added as part of revision [4]
;Entry:
;	an ogNam
;Exit:
;	ax = ogNam or 0
;	PSW flags set based on an OR AX,AX on exit
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	ONamOfOgNam,<PUBLIC,NEAR,NODATA>
	parmW	ogNam
	localV	namTmp,CB_MAX_NAMENTRY	; alloc space for max. name on stack
cBegin
	lea	ax,namTmp
	push	ax			;save across call
	mov	bx,[ogNam]
	DbChk	ogNam,bx		
	call	CopyOgNamPbNear		;necessary, as ONamOfPbCb expects
					;a DGROUP-relative pointer
	xchg	ax,cx			;cx = cbName
	pop	ax			;ax = pbName
	call	ONamOfPbCb
cEnd

cProc	ONamOfOgNamFar,<PUBLIC,FAR,NODATA>  
	parmW	ogNam			
cBegin					
	cCall	ONamOfOgNam,<ogNam>	
cEnd					

;***
;CharOfOgNam
;Purpose:
;	Given an ogNam, return the first char in the name in al
;
;	Added as part of revision [4]
;Entry:
;	an ogNam
;	It is assumed that the global mrs is NOT active on entry
;Exit:
;	ax = first char in name
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	CharOfOgNam,<PUBLIC,FAR,NODATA>
	parmW	ogNam
cBegin
	cCall	FpNamOfOgNam,<ogNam>		;returns es:dx pointing to name
						;  and cx = cbName
	mov	bx,dx
	mov	al,es:[bx]
cEnd

;***
;CbOfOgNam
;Purpose:
;	Given an ogNam, return the length of the name in ax
;
;	Added as part of revision [4]
;Entry:
;	an ogNam
;	It is assumed that the global mrs is NOT active on entry
;Exit:
;	ax = length of the name
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	CbOfOgNam,<PUBLIC,FAR,NODATA>
	parmW	ogNam
cBegin
	cCall	FpNamOfOgNam,<ogNam>		;returns es:dx pointing to name
						;  and cx = cbName
	xchg	ax,cx
cEnd

;***
;BdAppendOgNam
;Purpose:
;	Given a pbd and an ogNam, allocate the bd if necessary, and append the 
;	name to the end of the bd.
;
;	Added as part of revision [4]
;Entry:
;	pBd
;	ogNam
;Exit:
;	If not enough memory can be obtained, AX = FALSE
;	else pbd->cbLogical is updated, AX = TRUE (non-zero)
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	BdAppendOgNam,<PUBLIC,FAR,NODATA>,<SI>
	parmW	pBd
	parmW	ogNam
cBegin
	cCall	CbOfOgNam,<ogNam>	;ax = cbName
	mov	si,[pBd]
	push	si			;parm1 to BdCheckFree
	push	ax			;parm2 to BdCheckFree
	cmp	[si.BD_pb],NULL		;is the bd currently an owner?
	jnz	GrowTheBd		; brif not - grow, don't allocate

	PUSHI	ax,IT_NO_OWNERS
	call	BdAlloc			;allocate buffer of sufficient size
	mov	[si.BD_cbLogical],0	;to match algorithm in shared code
	jmp	short BdCheck_OM
GrowTheBd:
	call	BdCheckFree
BdCheck_OM:
	or	ax,ax
	jz	BdAppendOgNam_Exit	;brif error exit

	mov	ax,[si.BD_pb]
	add	ax,[si.BD_cbLogical]	;pointer to where name is to be copied
	mov	bx,[ogNam]
	cCall	CopyOgNamPbNear		;returns ax = cbName
	add	[si.BD_cbLogical],ax	;update logical length of buffer
	;The below assertion is to ensure we return a non-zero result
	DbAssertRel  ax,ne,0,CP,<BdAppendOgNam: 0-length name appended?!>
BdAppendOgNam_Exit:
cEnd

;***
;CmpOgNamIns
;Purpose:
;	Given two ogNam's, perform a case-insensitive comparison on the names.
;	NOTE: This routine could easily be generalized to work with any oNam's.
;
;	Added as part of revision [4]
;Entry:
;	ogNam1
;	ogNam2
;	It is assumed that the global mrs is NOT the current mrs on entry
;Exit:
;	if the two ogNam's are the same then	ax = 0
;	else if name1 < name2 then 		ax = FFFF
;	else name1 > name2 and 			ax = 1
;
;	if name1 longer than name2 then		dx > 0
;	else if name1 shorter than name2 then	dx < 0
;	else name1 same length as name2 and	dx = 0
;
;	PSW set based on an OR AX,AX on exit
;Uses:
;	none
;Exceptions:
;	none
;***************************************************************************
cProc	CmpOgNamIns,<PUBLIC,NEAR,NODATA>
	parmW	ogNam1
	parmW	ogNam2
cBegin
	DbChk	ogNam,ogNam1		
	DbChk	ogNam,ogNam2		
	push	si
	lea	bx,[mrsCur.MRS_bdlNam]	 ; assume global mrs active
	cmp	[grs.GRS_oMrsCur],OMRS_GLOBAL ; is global mrs active?
	jz	@F			;  brif so

	GETRS_SEG es,bx,<SIZE,LOAD>	
	mov	bx,MRS_bdlNam_seg + OMRS_GLOBAL ; offset in the global mrs
					    ; to the name table.
	RS_BASE add,bx			
@@:					
	GETSEG ds,PTRRS[bx],,<SIZE,LOAD>  
assumes DS,NOTHING
	mov	bx,[ogNam1]		;ds:bx = pNam1
	mov	si,[ogNam2]		;ds:si = pNam2

	sub	ax,ax
	mov	al,NM_SIZE[bx]		;ax = length of string1
	mov	cx,ax			; assume string1 is the shorter one
	sub	dx,dx
	mov	dl,NM_SIZE[si]		; dx = length of string2
	sub	ax,dx			;ax > 0 if string1 longer than string2
	jle	DoCmp			;brif string1 is not longer than string2
	mov	cx,dx			;cx = length of shorter string (string2)
DoCmp:
	add	bx,NM_NAME		;ds:bx points to 1st byte of string1
	add	si,NM_NAME		;ds:si points to 1st byte of string2
	push	ax			;preserve across call
	call	CmpStrIns		;call shared code to do the work
	pop	dx			;dx > 0 if string1 longer than string2
	pop	si
	push	ss			
	pop	ds
assumes DS,DATA
cEnd



sEnd	CP

	END
