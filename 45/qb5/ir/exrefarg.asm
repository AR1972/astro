page	49,132
	TITLE	exRefArg - excutors that provide pointers to arguments
;***
;exRefArg - excutors that provide pointers to arguments
;
;	Copyright <C> 1986, Microsoft Corporation
;
;
;****************************************************************************

	.xlist
	include		version.inc
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	extort
	IncludeOnce	opstmt
	IncludeOnce	rtinterp
	IncludeOnce	variable
	IncludeOnce	context
	IncludeOnce	pointers	
	.list

assumes cs, CODE
assumes es, NOTHING
assumes ss, DATA

sBegin	CODE

extrn	GetTempAddr:near

MakeExe	exPopPopPush,opNoList0
	pop	ax			;Thing to keep
	pop	dx			;Thing to blow off
	push	ax			;Keep it
	DispMac


	;The exPushSeg executor is forced to an odd address so it can not
	;be confused with an oVar or an oElem.	This is used in SsCoerceReg
	;to determine the oTyp of a record from the scan stack pointer.  The
	;exPushSeg may have been inserted if the evaluation of LHS of an
	;assignment to an element of an array of records could cause heap
	;movement.


	even				; Force even address
	db	1 dup(?)		; Force odd address

MakeExe	exPushSeg,opNoList0
	pop	ax			;Get offset
	push	ds			;Push segment
	push	ax			;   in front of offset
	DispMac

MakeExe	exCopyTmpAr,opNoList3		;oTemp,cb,oVar
;Copy array element to stack temp with info for copy back
;Construction of this temp:
;
;	dw	oVar
;	dd	relative position
;	dw	cb
;	db	<item>

	call	GetTempAddr		;Get addr of temp, adjusted for locals
	pop	bx			;Offset of source
	pop	ds			;Segment of source
	xchg	ax,dx			;Save destination in dx
	LODSWTX				;Get count of bytes
	xchg	cx,ax
	LODSWTX				;Get oVar
	push	es			;Save text segment
	push	si			;Save pc
	push	ss
	pop	es			;es = ss
	push	ds			;Save segment of source
	push	ss
	pop	ds			;ds = ss
	xchg	bx,ax			;oVar to bx, source offset to ax
	xchg	si,ax			;Set source
	push	bx
	call	oVarToPAd		;Get pAD in bx
	pop	ax
	xchg	di,dx			;Set dest, save di in dx
	stosw				;Save oVar in temp
	mov	ax,si			;Source offset
	sub	ax,[bx].AD_fhd.FHD_oData
	stosw				;Save relative offset in temp
	pop	ds
assumes	ds,NOTHING
	mov	ax,ds
	jnc	SubSeg
	sub	ah,10H			;Ripple carry into segment
SubSeg:
	sub	ax,ss:[bx].AD_fhd.FHD_hData ;sub phys seg from FHD
	stosw				;Save relative segment in temp
	mov	ax,cx
	stosw				;Save byte count
	mov	bx,di			;Address of value field
	rep	movsb			;Copy array element
	pop	si
	pop	es
	push	bx			;Put near address of value on stack
	mov	di,dx
	push	ss
	pop	ds			;Restore ds = ss
assumes	ds,DATA
	DispMac

MakeExe	exRestoreTmpAr,opNoList1	;oTemp
;Copy back array element that was copied to stack temp
	call	GetTempAddr		;Get addr of temp, adjusted for locals
	push	si
	call	RestoreAddr
	jc	NoArRestore
	push	es
	mov	es,ax
	xchg	dx,di
	rep	movsb			;Copy array element back
	mov	di,dx
	pop	es
NoArRestore:
	pop	si
	DispMac

RestoreAddr:
;Compute address to which to restore an array element passed as parameter
;
;Input:
;	si = address of temp
;Output:
;	CY flag set if array too small
;	ELSE
;	ax:dx = address of array element
;	cx = size of element
;	si = si + 8 (points after temp info)
;
;Temp construction must be:
;	dw	oVar
;	dd	relative position
;	dw	count

	xchg	ax,si			;Temp address to si
	lodsw				;Get oVar from temp
	xchg	ax,bx			;oVar to bx
	call	oVarToPAd
	lodsw				;Relative offset of element
	mov	dx,ax			;Offset to di
	mov	cl,4
	shr	ax,cl			;Convert offset to paragraph count
	xchg	cx,ax
	lodsw				;Relative segment
	add	cx,ax			;Position in paragraphs
	cmp	cx,[bx].AD_fhd.FHD_cPara;Space allocated for it?
	jae	ArrayTooSmall
	add	dx,[bx].AD_fhd.FHD_oData
	jnc	SegOff
	add	ah,10H			;Ripple carry into segment
SegOff:
	add	ax,[bx].AD_fhd.FHD_hData    ;add phys seg to FHD
	xchg	cx,ax
	lodsw				;Get count of bytes
	xchg	cx,ax
	ret

ArrayTooSmall:
	stc
	ret


MakeExe	exCopyTmpArSD,opNoList2		;oTemp,oVar
;Assign string array element to stack temp, with info for copy back
;Construction of this temp
;
;	dd	SD
;	dw	oVar
;	dw	relative offset
;
	call	GetTempAddr		;Get addr of temp, adjusted for locals
	pop	cx			;Get source offset
	push	ax			;Result - pSD
	push	cx			;Source for B$SASS
	push	ax			;Destination for B$SASS
	xchg	ax,bx			;Destination to bx
	xor	ax,ax
	mov	[bx],ax
	mov	[bx+2],ax		;Make sure temp is a null string
	LODSWTX				;Get oVar
	mov	[bx+4],ax
	xchg	bx,ax			;oVar to bx
	xchg	dx,ax			;Destination to dx
	call	oVarToPAd		;Get pAD in bx
	sub	cx,[bx].AD_fhd.FHD_oData;Compute relative offset
	mov	bx,dx
	mov	[bx+6],cx
	CALLRT	B$SASS,DispMov		;Assign string to stack temp

MakeExe	exRestoreTmpArSD,opNoList1	;oTemp
;Assign string array stack temp back to origin
	call	GetTempAddr		;Get addr of temp, adjusted for locals
	xchg	ax,bx			;Source to bx
	push	bx			;This copy for assign (source)
	mov	dx,[bx+6]		;Get relative offset
	mov	bx,[bx+4]		;Get oVar
	call	oVarToPAd		;Get pAD in bx
	cmp	dx,[bx].AD_fhd.FHD_cPara;Within current bounds?  NOTE: cPara 
					;   has byte count for near arrays
	jae	NoSdRestore		;Don't restore if out of range
	add	dx,[bx].AD_fhd.FHD_oData
	push	dx
	push	dx			;Destination
	CALLRT	B$STDL			;De-allocate current string
	pop	ax
	pop	bx			;Source SD
	mov	cx,[bx]
	mov	bx,[bx+2]		;Get SD
	xchg	ax,bx
	mov	[bx],cx
	mov	[bx+2],ax		;Assign SD
	jcxz	NoBack			;If nul, don't adjust backpointer
	xchg	bx,ax			;bx = pointer to string data
	mov	[bx-2],ax		;Set back pointer
NoBack:
	DispMac

NoSdRestore:
;pSD already on stack
	CALLRT	B$STDL			;De-allocate temp string
	jmp	NoBack

MakeExe	exCopyTmpArFS,opNoList2		;oTemp,oVar
;Copy FS array element to SD stack temp with info for copy back
;Construction of this temp:
;
;	dd	SD
;	dw	oVar
;	dd	relative position
;	dw	cb

	call	GetTempAddr		;Get addr of temp, adjusted for locals
	pop	dx			;cb
	pop	bx			;Source offset
	pop	cx			;Source segment
	push	ax			;Result pSD
	add	ax,4			;Point past SD
	push	cx
	push	bx
	push	dx
	xchg	dx,ax			;Save destination in dx
	LODSWTX				;Get oVar
	xchg	bx,ax			;oVar to bx
	push	bx
	call	oVarToPAd		;Get pAD in bx
	pop	ax
	mov	di,dx			;Set dest
	push	ss
	pop	es			;es = ss
	stosw				;Save oVar in temp
	pop	dx			;cb
	pop	ax			;Source offset
	push	ax
	sub	ax,[bx].AD_fhd.FHD_oData
	stosw				;Save relative offset in temp
	xchg	ax,cx
	jnc	SubSegs
	sub	ah,10H			;Ripple carry into segment
SubSegs:
	sub	ax,ss:[bx].AD_fhd.FHD_hData ;sub phys seg from FHD
	stosw				;Save relative segment in temp
	xchg	ax,dx
	stosw				;Save byte count
	sub	di,12			;Point to SD again
	jmp	short CopyFStoSD

MakeExe	exRestoreTmpArFS,opNoList1	;oTemp
;Copy back FS that was copied to SD stack temp
	call	GetTempAddr		;Get addr of temp, adjusted for locals
	push	ax			;pSD for deallocation
	push	ds
	push	ax			;Far address of SD
	xor	cx,cx
	push	cx			;cb=0 means SD
	add	ax,4			;Skip over SD
	push	si
	call	RestoreAddr
	pop	si
	jc	NoFsRestore
	push	ax			;Segment of FS
	push	dx			;Offset of FS
	push	cx			;Count of bytes
	jmp	short RestoreFS

NoFsRestore:
	add	sp,8			;Clean off arguments
	DispMac

MakeExe	exCopyTmpFS,opNoList1		;oTemp
;Copy FS to SD stack temp with info for copy back
;Construction of this temp:
;
;	dd	far address
;	dw	cb
;	dd	SD

	call	GetTempAddr		;Get addr of temp, adjusted for locals
	xchg	ax,di
	pop	dx			;cb of FS
	pop	ax			;offset of FS
	pop	cx			;Segment of FS
	push	ds
	pop	es			;es = ds
	stosw				;Save offset in temp
	xchg	ax,cx
	stosw				;Save segment in temp
	xchg	ax,dx
	stosw				;Save cb in temp
	push	di			;Pointer to SD result
	push	dx			;Segment
	push	cx			;Offset
CopyFStoSD:
	push	ax			;cb
	push	ds
	push	di			;Far pSD
	xor	ax,ax
	stosw
	stosw				;Make sure temp is a null SD
	push	ax			;Flag as SD - cb=0
	CALLRT	B$ASSN,DispMov

MakeExe	exRestoreTmpFS,opNoList1	;oTemp
	call	GetTempAddr		;Get addr of temp, adjusted for locals
	mov	bx,ax
	add	ax,6			;Point to SD
	push	ax			;Save pSD for de-allocation
	push	ds
	push	ax
	xor	cx,cx
	push	cx			;cb=0 means SD
	push	[bx+2]			;Segment
	push	[bx]			;Offset
	push	[bx+4]			;cb
RestoreFS:
	CALLRT	B$ASSN			;Assign to FS - no errors possible
	CALLRT	B$STDL,Disp		;De-allocate string space

MakeExe	exStringTemp,opNoList0
	CALLRT	B$SCPY,DispMovAx

sEnd	CODE
	end
