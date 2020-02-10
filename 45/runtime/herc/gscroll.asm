;	TITLE	GSCROLL - Scrolling routines for MSHERC.
;***
;GSCROLL
;
;	Copyright <C> 1987, 1988, Microsoft Corporation
;
;Purpose:
;	Scroll Up and Down routines for MSHERC.
;
;******************************************************************************

	include	hgcdefs.inc

code            segment para public 'code'
                assume  cs:code,ds:code

Public	GScrollUp
Public	GScrollDown

Extrn	ZERO:word
Extrn	DeltaCols:word		;Number of columns in scroll window
Extrn	DeltaRows:word		;Number of rows in scroll window
Extrn	LEdge:byte,REdge:byte	;Tables containing left and right edge masks
Extrn	MapGraphXYToVideoOffset:near
Extrn	DstEdgeMasks:word
Extrn	CharHeight:byte
Extrn	ScrollLines:word

;-------------------------------------------------------------------------------
; 6  SCROLL ACTIVE PAGE UP
;	 AL => # lines (0 means blank window)
;	 CH,CL => Row, Column of upper left corner
;	 DH,DL => Row, Column of lower right corner
;	 BH => attribute for blanking lines
;-------------------------------------------------------------------------------
GScrollUp	proc	near

	mov	bl,bh
	push	bx			;save blanking attribute

;------Calculate Scroll Area Parameters-------
	sub	dh,ch			;Calculate the delta row count
	inc	dh
	sub	dh,al			;Adjust # of Rows to scroll
	mov	byte ptr DeltaRows,dh	;Save row count
	xor	ah,ah			;Clear ah
	mul	CharHeight		;Calculate # scans to scroll
	mov	ScrollLines,ax
	sub	dl,cl			;Calculate the delta column count
	inc	dl
	xor	dh,dh			;dx = DeltaColumns
	mov	byte ptr DeltaRows[1],dh ;Clear msb of delta rows

	mov	ax,14			;Fetch current font char height
	mov	bx,ax			;Save height value
	mul	byte ptr DeltaRows	;Char Height*Rows = DeltaY
	mov	DeltaRows,ax		;Save window height

	mov	ax,bx			;Retrieve char height
	mul	ch			;Char height*start row = start y
	push	ax			;Save starting y coordinate

;-----Char width is 9, continue calculating scroll parameters-----
	mov	ax,dx			;Save delta cols
	shl	dx,1			;cols*2
	shl	dx,1			;cols*4
	shl	dx,1			;cols*8
	add	dx,ax			;cols*9 = delta x
	mov	DeltaCols,dx		;Save delta x

;-----Calculate starting x,y graphics coordinates-------
	xor	ch,ch			;Clear ch
	mov	ax,cx			;Save starting col
	shl	cx,1			;Start col*2
	shl	cx,1			;Start col*4
	shl	cx,1			;Start col*8
	add	cx,ax			;Start col*9 = starting x coordinate
	mov	ax,cx			;Save starting x coordinate
	pop	dx			;Fetch starting y coordinate
	mov	bx,dx			;Save starting y coordinate

;------Calculate Display Buffer Offset for beginning of scroll----
	Call	MapGraphXYToVideoOffset	;Result: cx = offset dl = bit mask
	mov	di,cx			;di = dest ptr to screen

	mov	dx,bx			;Retrieve starting y coordinate
	add	dx,ScrollLines		;start y + # scans to scroll
					; = y source for scroll
	mov	cx,ax			;Retrieve starting x coordinate
	Call	MapGraphXYToVideoOffset	;Result: cx = offset, dl = bit mask
	mov	si,cx			;si = src ptr to screen
	push	ax			;Save start x

;------Calculate left and right edge masks for scroll------
	xor	bh,bh			;Clear bh
	mov	bl,dl			;bx = bit mask
	mov	cl,LEdge[bx]		;Fetch the left edge mask
	add	ax,DeltaCols		;starting x + deltax
	dec	ax			;- 1 = right edge x coordinate
	mov	dx,ax			;Save right edge x coordinate
	and	ax,7			;X MOD 8
	neg	ax			;- (X MOD 8)
	add	ax,7			;7 - (X MOD 8)
	mov	bx,ax			;bx = index to bit mask
	mov	ch,REdge[bx]		;Fetch the right edge mask
	mov	DstEdgeMasks,cx 	;Save the scrolling dest edge masks

;------Calculate # bytes in inner loop count------
	shr	dx,1
	shr	dx,1
	shr	dx,1			;byte containing right edge x
	pop	ax			;fetch start x
	shr	ax,1
	shr	ax,1
	shr	ax,1			;byte containing start x
	sub	dx,ax
	inc	dx
	or	cl,cl
	jz	NoLeftEdge
	dec	dx
NoLeftEdge:
	or	ch,ch
	jz	NoRightEdge
	dec	dx
NoRightEdge:
	mov	DeltaCols,dx	;Save inner loop cnt
	
;------Finish setting up for Scroll------
	mov	bx,DstEdgeMasks ;Fetch the left and right edge masks
	mov	cx,Scr1_Buf_Seg
	test	byte ptr es:BIOSPAGE,1
	jnz	SUpPg
	mov	cx,Scr0_Buf_Seg
SupPg:
	mov	es,cx		;es = Display Buffer Segment
	mov	ds,cx		;ds = Display Buffer Segment

	assume	ds:nothing

;------Check to see if the entire window is to be blanked-----
	xor	cx,cx		;Clear cx
	add	cx,ScrollLines	;Scroll Count+cx to set flags
	jnz	SGoUp		;Start normal scroll up

;--------Blank the window-----------
	mov	cx,cs:DeltaRows ;Fetch # of rows to blank
	jmp	BlankUp1	;Blank the window

;------Execute the Scroll---------
SGoUp:
	mov	cx,cs:DeltaRows ;Fetch # of Rows to copy
GoUp:
	push	cx		;Save row count
	push	si		;Save src
	push	di		;Save dst

;------Handle the left edge-----
	or	bl,bl		;Is there a left edge to handle?
	jz	GoUpInner	;No, scroll the inner loop

	mov	ah,es:[di]	;Fetch the destination, and load the latches
	and	ah,bl		;Mask out the portion to be modified

	lodsb			;Load the latches with source data
	not	bl		;Create the source edge mask
	and	al,bl		;Mask out the unwanted source data
	not	bl		;Restore the destination edge mask
	or	al,ah		;Combine the source and destination
	stosb			;Save the desired result

;-----Scroll up the inner portion-----
GoUpInner:
	mov	cx,cs:DeltaCols ;Fetch inner loop count
rep	movsb			;Copy one row

;-----Handle the right edge-----
	or	bh,bh		;Is there a right edge to handle?
	jz	AdvUpPtrs	;No, Update the screen pointers

	mov	ah,es:[di]	;Fetch the destination, and load the latches
	and	ah,bh		;Mask out the portion to be modified

	lodsb			;Load the latches with source data
	not	bh		;Create the source edge mask
	and	al,bh		;Mask out the unwanted source data
	not	bh		;Restore the destination edge mask
	or	al,ah		;Combine the source and destination
	stosb			;Save the desired result

;------Update the Screen Pointers-----
AdvUpPtrs:
	pop	di
	pop	si
	add	si,GrNextScan		;Point to next source row
	jns	PntNxtUpDst		;Update dest ptr
	sub	si,NextScanAdjust	;Adjust Src Ptr to screen

PntNxtUpDst:
	add	di,GrNextScan		;Point to next dest row
	jns	DstPtrOnScr		;Dest Ptr updated
	sub	di,NextScanAdjust	;Adjust Dst Ptr to screen

DstPtrOnScr:
	pop	cx		;Retrieve row count
	loop	GoUp		;Copy next row

	mov	cx,ScrollLines	;# of rows to blank
BlankUp1:
	pop	si		;Blanking Character
BlankUp:
	push	cx				;Save row count
	push	di

;------Handle the left edge-----
	or	bl,bl		;Is there a left edge to handle?
	jz	BlkUpInner	;No, blank the inner loop

	mov	ah,es:[di]	;Fetch the destination, and load the latches
	and	ah,bl		;Mask out the portion to be modified

	mov	dx,si		;dl = blanking value
	not	bl		;Convert to Src Edge Mask
	and	dl,bl		;Mask out unwanted portion of blanking byte
	not	bl		;Convert back to Dst Edge Mask

	or	ah,dl		;Combine the source and destination
	xchg	ah,al
	stosb			;Save the desired result

;-----Blank the inner portion-----
BlkUpInner:
	mov	cx,cs:DeltaCols ;Fetch column count
	mov	ax,si		;Fetch blanking value
rep	stosb			;Blank one row

;-----Handle the right edge-----
	or	bh,bh		;Is there a right edge to handle?
	jz	AdvBlkUpPtr	;No, Update the screen pointers

	mov	ah,es:[di]	;Fetch the destination, and load the latches
	and	ah,bh		;Mask out the portion to be modified

	mov	dx,si		;dl = blanking value
	not	bh		;Create the source edge mask
	and	dl,bh		;Mask out the unwanted source data
	not	bh		;Restore the destination edge mask

	or	ah,dl		;Combine the source and destination
	xchg	ah,al
	stosb			;Save the desired result

;------Update the Screen Pointers-----
AdvBlkUpPtr:
	pop	di
	add	di,GrNextScan		;Point to next dest row
	jns	BDstPtrOnScr		;Dest Ptr updated
	sub	di,NextScanAdjust	;Adjust Dst Ptr to screen

BDstPtrOnScr:
	pop	cx		;Retrieve row count
	loop	BlankUp		;Copy next row

	push	cs		;restore DS & ES
	pop	ds

	assume	ds:code

	mov	es,ZERO
	Ret	;Finished Scroll Up
GScrollUp	endp

;-------------------------------------------------------------------------------
; 7  SCROLL ACTIVE PAGE DOWN
;	 AL => # lines (0 means blank window)
;	 CH,CL => Row, Column of upper left corner
;	 DH,DL => Row, Column of lower right corner
;	 BH => attribute for blanking lines
;-------------------------------------------------------------------------------
GScrollDown	proc	near

	mov	bl,bh
	push	bx			;save blanking attribute

;------Calculate Scroll Area Parameters-------
	sub	dh,ch			;Calculate the delta row count
	inc	dh
	add	ch,dh			;Adjust start row to window bottom
	dec	ch
	sub	dh,al			;Adjust # of Rows to scroll
	mov	byte ptr DeltaRows,dh	;Save row count
	xor	ah,ah			;Clear ah
	mul	CharHeight		;Calculate # scans to scroll
	mov	ScrollLines,ax
	sub	dl,cl			;Calculate the delta column count
	inc	dl
	xor	dh,dh			;dx = DeltaColumns
	mov	byte ptr DeltaRows[1],dh ;Clear msb of delta rows

	mov	ax,14			;Fetch current font char height
	mov	bx,ax			;Save height value
	mul	byte ptr DeltaRows	;Char Height*Rows = DeltaY
	mov	DeltaRows,ax		;Save window height

	mov	ax,bx			;Retrieve char height
	mul	ch			;Char height*start row = start y
	add	ax,13			;(start is at bottom of last text row)
	push	ax			;Save starting y coordinate

;-----Char width is 9, continue calculating scroll parameters-----
	mov	ax,dx			;Save delta cols
	shl	dx,1			;cols*2
	shl	dx,1			;cols*4
	shl	dx,1			;cols*8
	add	dx,ax			;cols*9 = delta x
	mov	DeltaCols,dx		;Save delta x

;-----Calculate starting x,y graphics coordinates-------
	xor	ch,ch			;Clear ch
	mov	ax,cx			;Save starting col
	shl	cx,1			;Start col*2
	shl	cx,1			;Start col*4
	shl	cx,1			;Start col*8
	add	cx,ax			;Start col*9 = starting x coordinate
	mov	ax,cx			;Save starting x coordinate
	pop	dx			;Fetch starting y coordinate
	mov	bx,dx			;Save starting y coordinate

;------Calculate Display Buffer Offset for beginning of scroll----
	Call	MapGraphXYToVideoOffset	;Result: cx = offset dl = bit mask
	mov	di,cx			;di = dest ptr to screen

	mov	dx,bx			;Retrieve starting y coordinate
	sub	dx,ScrollLines		;start y - # scans to scroll
	       				;= y source for scroll
	mov	cx,ax			;Retrieve starting x coordinate
	Call	MapGraphXYToVideoOffset	;Result: cx = offset, dl = bit mask
	mov	si,cx			;si = src ptr to screen
	push	ax			;Save start x

;------Calculate left and right edge masks for scroll------
	xor	bh,bh			;Clear bh
	mov	bl,dl			;bx = bit mask
	mov	cl,LEdge[bx]		;Fetch the left edge mask
	add	ax,DeltaCols		;starting x + deltax
	dec	ax			;- 1 = right edge x coordinate
	mov	dx,ax			;Save right edge x coordinate
	and	ax,7			;X MOD 8
	neg	ax			;- (X MOD 8)
	add	ax,7			;7 - (X MOD 8)
	mov	bx,ax			;bx = index to bit mask
	mov	ch,REdge[bx]		;Fetch the right edge mask
	mov	DstEdgeMasks,cx 	;Save the scrolling dest edge masks

;------Calculate # bytes in inner loop count------
	shr	dx,1
	shr	dx,1
	shr	dx,1			;byte containing right edge x
	pop	ax
	shr	ax,1
	shr	ax,1
	shr	ax,1			;byte containing start x

	sub	dx,ax
	inc	dx
	or	cl,cl
	jz	NoLeftEdgeD
	dec	dx
NoLeftEdgeD:
	or	ch,ch
	jz	NoRightEdgeD
	dec	dx
NoRightEdgeD:
	mov	DeltaCols,dx	;Save inner loop cnt
	
;------Finish setting up for Scroll------
	mov	bx,DstEdgeMasks ;Fetch the left and right edge masks
	mov	cx,Scr1_Buf_Seg
	test	byte ptr es:BIOSPAGE,1
	jnz	SDnPg
	mov	cx,Scr0_Buf_Seg
SDnPg:
	mov	es,cx		;es = Display Buffer Segment
	mov	ds,cx		;ds = Display Buffer Segment

	assume	ds:nothing

;------Check to see if the entire window is to be blanked-----
	xor	cx,cx		;Clear cx
	add	cx,ScrollLines	;Scroll Cnt + cx to set flags
	jnz	SGoDown

;--------Blank the window-----------
	mov	cx,cs:DeltaRows ;Fetch # of rows to blank
	jmp	BlankDown1	;Blank the window


;------Execute the Scroll---------
SGoDown:
	mov	cx,cs:DeltaRows ;Fetch # of Rows to copy
GoDown:
	push	cx		;Save row count
	push	si		;Save src
	push	di		;Save dst

;------Handle the left edge-----
	or	bl,bl		;Is there a left edge to handle?
	jz	GoDownInner	;No, scroll the inner loop

	mov	ah,es:[di]	;Fetch the destination, and load the latches
	and	ah,bl		;Mask out the portion to be modified

	lodsb			;Load the latches with source data
	not	bl		;Create the source edge mask
	and	al,bl		;Mask out the unwanted source data
	not	bl		;Restore the destination edge mask
	or	al,ah		;Combine the source and destination
	stosb			;Save the desired result

;-----Scroll down the inner portion-----
GoDownInner:
	mov	cx,cs:DeltaCols ;Fetch inner loop count
rep	movsb			;Copy one row

;-----Handle the right edge-----
	or	bh,bh		;Is there a right edge to handle?
	jz	AdvDwnPtrs	;No, Update the screen pointers

	mov	ah,es:[di]	;Fetch the destination, and load the latches
	and	ah,bh		;Mask out the portion to be modified

	lodsb			;Load the latches with source data
	not	bh		;Create the source edge mask
	and	al,bh		;Mask out the unwanted source data
	not	bh		;Restore the destination edge mask
	or	al,ah		;Combine the source and destination
	stosb			;Save the desired result

;------Update the Screen Pointers-----
AdvDwnPtrs:
	pop	di
	sub	di,GrNextScan
	jns	PntNxtDwnDst		;Update Src ptr
	add	di,NextScanAdjust	;Adjust Dst Ptr to screen

PntNxtDwnDst:
	pop	si
	sub	si,GrNextScan		;Point to next src row
	jns	DstDPtrOnScr		;Src Ptr updated
	add	si,NextScanAdjust	;Adjust Src Ptr to screen

DstDPtrOnScr:
	pop	cx		;Retrieve row count
	loop	GoDown		;Copy next row

	mov	cx,ScrollLines	;# of rows to blank
BlankDown1:
	pop	si		;Blanking Character
BlankDown:
	push	cx				;Save row count
	push	di

;------Handle the left edge-----
	or	bl,bl		;Is there a left edge to handle?
	jz	BlkDwnInner	;No, blank the inner loop

	mov	ah,es:[di]	;Fetch the destination, and load the latches
	and	ah,bl		;Mask out the portion to be modified

	mov	dx,si		;dl = blanking value
	not	bl		;Convert to Src Edge Mask
	and	dl,bl		;Mask out unwanted portion of blanking byte
	not	bl		;Convert back to Dst Edge Mask

	or	ah,dl		;Combine the source and destination
	xchg	ah,al
	stosb			;Save the desired result

;-----Blank the inner portion-----
BlkDwnInner:
	mov	cx,cs:DeltaCols ;Fetch column count
	mov	ax,si		;Fetch blanking value
rep	stosb			;Blank one row

;-----Handle the right edge-----
	or	bh,bh		;Is there a right edge to handle?
	jz	AdvBlkDwnPtr	;No, Update the screen pointers

	mov	ah,es:[di]	;Fetch the destination, and load the latches
	and	ah,bh		;Mask out the portion to be modified

	mov	dx,si		;dl = blanking value
	not	bh		;Create the source edge mask
	and	dl,bh		;Mask out the unwanted source data
	not	bh		;Restore the destination edge mask

	or	ah,dl		;Combine the source and destination
	xchg	ah,al
	stosb			;Save the desired result

;------Update the Screen Pointers-----
AdvBlkDwnPtr:
	pop	di
	sub	di,GrNextScan		;Point to next dest row
	jns	BDstDwnPtrOnScr		;Dest Ptr updated
	add	di,NextScanAdjust	;Adjust Dst Ptr to screen

BDstDwnPtrOnScr:
	pop	cx		;Retrieve row count
	loop	BlankDown	;Copy next row

	push	cs		;restore DS & ES
	pop	ds

	assume	ds:code

	mov	es,ZERO
	Ret	;Finished Scroll Down
GScrollDown	endp

code	ends
	end
