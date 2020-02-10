	TITLE	util.asm - general purpose utilities (not provided by runtime)
;***
;util.asm - general purpose utilities for the interpreter
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Any general purpose (i.e., code that is deemed to be not tied to
;	any single function area) utility routine in the interpreter
;	will be placed here. In some cases, speed-critical routines may
;	be present which duplicate runtime code, but for which we don't
;	want to pay the size and speed penalty of a long call, and/or we 
;	don't want to pay for the C calling convention.
;
;
;****************************************************************************

	.xlist
	UTIL_ASM = 1		;We need to do this before including version.inc
				;since version.inc ultimately includes
				;debug.inc, which depends on UTIL_ASM
	include version.inc
	includeOnce	heap
	includeOnce	context
	.list

sBegin	DATA


sEnd	DATA

sBegin	RT
	EXTRN	B$SCAT:FAR
	EXTRN	B$FLEN:FAR
	EXTRN	B$END:FAR
sEnd	RT

;	.sall

assumes CS,CP
assumes DS,DATA
assumes ES,DATA
assumes SS,DATA

	page

sBegin	CP

	subttl	Zero fill routine.
	page

;***
;ZeroFill(pDst, cbFill) - Zero fills a block of size cbFill, starting at pDst
;
;Entry:	two values on the stack (C calling convention):
;		- a pointer to the string.
;		- the number of bytes to fill.
;	NOTE: This routine shares code with FillUndef; the interface to these
;		two routines must match, as they share a common exit.
;
;Exit:	
;	On exit, [ax] == 0
;Uses:
;	none.
;****
cProc	ZeroFill,<PUBLIC,FAR>,<DI>
	parmW	pDst		
	parmW	cbFill
cBegin
	xor	ax,ax
	mov	cx,cbFill
FillBytes:			;start of shared code with FillUndef (below)
	jcxz	ZeroEnd

	push	ds		;require ES = DS for rep movs
	pop	es
	cld
	mov	di,pDst

	shr	cx,1		;convert to a word count
	repz	stosw
	jnc	ZeroEnd		;no carry if byte count was even

	stosb			;move the last (odd) byte
	DbChk	Heaps		;check in case we hosed the heap
ZeroEnd:
cEnd
	
	subttl	Routine to fill a block of words with UNDEFINED
	page

;***
;FillUndef(pDst, cwFill) - fills cwFill words w/UNDEFINED, starting at pDst
;
;Entry:	two values on the stack (C calling convention):
;		- a pointer to the start of the block to fill.
;		- the number of words to fill.
;	NOTE: This routine shares code with ZeroFill; the interface to these
;		two routines must match, as they share a common exit.
;
;Exit:	
;	On exit, [ax] == UNDEFINED
;Uses:
;	none.
;****
cProc	FillUndef,<PUBLIC,FAR>,<DI>
	parmW	pDst		
	parmW	cwFill

cBegin
	mov	cx,cwFill
	shl	cx,1		;convert word count to byte count for fill
	mov	ax,UNDEFINED	;NOTE: assuming here that low byte and high
				;	byte of UNDEFINED are the same
	jmp	short FillBytes	;share code with ZeroFill, above
cEnd	nogen

;***
;VOID CopyBlk(pbSrc, pbDst, cb) - do a simple block copy within DGROUP
;
;Purpose:
;	Copy a block of bytes.  This function should not be used
;	if the source block intersects with the destination block,
;	and begins at a lower address than the destination block,
;	otherwise, it will overwrite part of the destination before
;	it gets to it.  Use function CopyBlkHighLow (as yet unwritten)
;	for this case.
;Entry:
;	3 values on the stack (C calling convention):
;	- a pointer to the lowest addressed source byte,
;	- a pointer to the lowest addressed destination byte,
;	- the number of bytes to copy.
;Exit:
;	none
;
;****
cProc	CopyBlk,<PUBLIC,FAR>,<si,di>
	parmW pbSrc
	parmW pbDst
	parmW cb
cBegin
	mov	si,pbSrc		;si = source offset
	mov	di,pbDst		;di = destination offset
	mov	cx,cb
	push	ds			;movsb requires es = ds
	pop	es
	rep movsb			;copy block
	DbChk	Heaps			;check in case we hosed the heap
cEnd


;********************************************************************
; short CmpPsdIns(psd1, psd2, cbMax)
; Purpose:
;	Perform a CASE insensitive comparison of two strings
;
; Entry:
;	parm1: ax = psd1 - points to 1st string descriptor
;	parm2: bx = psd2 - points to 2nd string descriptor
;	parm3: dx = cbMax cmp - max. number of chars to use in comparison
;
; Exit:
;	If strings are the same, returns ax=0
;	If psd1 <> psd2, return ax not 0
;	(it is much faster for this function to not differentiate
;	 between psd1 < psd2 or psd1 > psd2, and callers just care
;	 about equality AND SPEED)
;	Also sets PSW flags via or ax,ax just prior to exit.
;
;********************************************************************
cProc	CmpPsdIns,<PUBLIC,NEAR>,<si>
cBegin
	xchg	ax,si			;si -> sd 1
	lodsw				;ax = byte count for string 1
	mov	cx,ax			;cx = byte count for string 1
	cmp	dx,cx
	jae	Got_Cb1			;brif count is <= max cout to be used

	mov	cx,dx
Got_Cb1:
	lodsw				;ax -> string 1
	mov	si,bx			;si -> sd 2
	mov	bx,ax			;bx -> string 1

	lodsw				;ax = byte count for string 2
	cmp	dx,ax
	jae	Got_Cb2
	xchg	ax,dx
Got_Cb2:
	cmp	ax,cx
	mov	ax,UNDEFINED		;prepare for Not-Equal return
	jne	CmpPsdExit		;brif strings are of different lengths
	lodsw				;ax -> string 2
	xchg	ax,si			;si -> string 2

	;At this point,
	;   bx points to 1st byte of string 1,
	;   si points to 1st byte of string 2
	;   cx = # bytes to compare
	
	call	CmpStrIns		;compare strings, ax=0 if equal
CmpPsdExit:				;return ax
	or	ax,ax			;set flags
cEnd

;********************************************************************
; CmpStrIns
; Purpose:
;	Compare 2 strings, ignoreing case sensitivity
;
; Entry:
;	bx points to 1st byte of string 1,
;	si points to 1st byte of string 2
;	cx = # bytes to compare
;
; Exit:
;	If strings are the same, returns ax=0
;	If string1 < string2, return ax=FFFFh
;	if string1 > string2, return ax=0001h
;	Also sets PSW flags via or ax,ax just prior to exit.
;
;********************************************************************
cProc	CmpStrIns,<PUBLIC,NEAR>,<si>
cBegin
	mov	dx,0DFDFH		; mask for converting lower->upper
CmpLoop:
	lodsb				;al = next byte from string 2
	mov	ah,[bx]			;ah = next byte from string 1
	inc	bx
	and	ax,dx			;force al and ah to upper case
	cmp	al,ah
	loope	CmpLoop			;branch if equal so far, and cx<>0
	mov	ax,1
	jb	CmpStrExit		;branch if string1 > string2
	je	CmpStrEqual		;branch if string1 = string2
	dec	ax			;else string1 < string2
CmpStrEqual:
	dec	ax
CmpStrExit:
	or	ax,ax			;set PSW flags for callers
cEnd

;*********************************************************************
; MapCpW, MapEsW
; Purpose:
;	Map a value from one table to another.
;	If value not found in search table, word beyond end of
;	map table is returned.
; Entry:
;	ax = value to search for
;	cx = number of values in search table
;	dx = ptr to table of values to search for
;	bx = ptr to cooresponding table of map (result) values
; Exit:
;	ax = value from map table
;
; MapCpW Alters ES
;
;*********************************************************************
PUBLIC	MapCpW
MapCpW	PROC NEAR			;maps using CP segment
	push	cs
	pop	es			;es = code segment
MapCpW	ENDP
	;fall into MapEsW
PUBLIC	MapEsW
MapEsW	PROC NEAR
	push	di			;save caller's di
	push	cx			;save number of bytes in table
	mov	di,dx			;di -> 1st word in table to search
	repne scasw			;search for value in table pointed
					; to by di (uses es)
	pop	ax			;ax = number of bytes in table
	sub	ax,cx			;ax = offset into table
	add	bx,ax			;bx = pointer to result value
	mov	ax,es:[bx]
	pop	di			;restore caller's di
	ret
MapEsW	ENDP

;*************************************************************************
; ushort CbSz(sz)
;
; Purpose:
;	Return the number of bytes in a 0-byte terminated string.
;	Does not include terminator in count.
;
; Entry:
;	parm1: char *sz - ptr to source string
;	  
; Exit:
;	[AX] = byte count
;
;*************************************************************************
cProc	CbSz,<PUBLIC,FAR>,<di>
	parmW   sz			
cBegin
	push	ds
	pop	es			;es = DGROUP
	mov	di,sz			;di points to start of string
	mov	cx,0FFFFH		;ax = max byte count
	sub	ax,ax			;al = byte to search for
	repne	scasb			;while cx != 0 and es:[di] != al, di++
	sub	ax,cx
	dec	ax
	dec	ax			;ax = length of string (0..n)
cEnd

;*************************************************************
;RelTempSd_CP
;Purpose:
;	release temp string descriptor
;Entry:
;	stack parm points to temp string descriptor to be released
;
;*************************************************************
PUBLIC	RelTempSd_CP
RelTempSd_CP PROC NEAR
	pop	bx			;bx = return adr
	pop	ax			;ax = parm
	push	bx			;push back return adr
	push	ax			;pass ptr to string desc
	call	B$FLEN			;release temp descriptor
					; (side effect of LEN function)
	ret
RelTempSd_CP ENDP

	
sEnd	CP


	end
