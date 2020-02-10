	assumes		DS,DGROUP
	assumes		SS,DGROUP
	assumes		ES,NOTHING

fail1:
	jmp	fail2

;***	EditMgrKeyInit(char far * lpTable)
;*
;*
cProc	EditMgrKeyInit,<FAR,PUBLIC>,<di,si,ds>
parmD	lpTable
cBegin
	push	ds
	pop	es
	assumes	es,DGROUP
	
	lds	si,lpTable		;* Get a pointer to the structure
	assumes	DS,NOTHING
	
	;*
	;*	First come a pair of length prefixed, zero terminated
	;*	strings. (The length includes the zero byte).
	
	mov	di, dataOFFSET SzActionPrim	; Destination
	lodsb				;* Get the length
	cbw				;* 
	mov	cx,ax			;*
	repz	movsb			;* Copy over the name
	
	mov	di, dataOFFSET SzCancelPrim	; Destination
	lodsb				;* Get the length
	cbw				;* 
	mov	cx,ax			;*
	repz	movsb			;* Copy over the name
	
	;*
	;*	Second are the action and cancel keys to use
	;*
	
	lodsw				;* Get Primary Action Key
	mov	[vkActionPrim],ax	;* And store
	lodsw				;* Get Secondary Action Key
	mov	[vkActionSec],ax	;* And store
	lodsw				;* Get Primary Cancel Key
	mov	[vkCancelPrim],ax	;* And store
	lodsw				;* Get Secondary Cancel Key
	mov	[vkCancelSec],ax	;* And store
	
	;*
	;*	Third thing in the structure is 33 bytes for the 
	;*		DoCharDispatch table
	;*
	
	mov	di,dataOFFSET DoCharDispatch ;* Destination to copy to
	mov	cx,33			;* 33 bytes of info
	repz	movsb			;* Copy the table over
	
	;*
	;*	Next comes the Vk tables
	;*
	;*	Vk
	
	lodsw				;* Get the length of the table
	push	ax			;* Save the length
	push	ds			;* Save this register
	push	es			;* Set DS = DGROUP
	pop	ds
	assumes	ds,DGROUP
	
	mov	cx,1
	save	<es>
	cCall	PpvAllocCb,<cx,ax>	;* (sbHeap, cbReq)
	or	ax,ax			;* ERROR?
	jz	fail1			;* Yes -- abort
	pop	ds			;* ds:si points to the input table
	assumes	ds,nothing		;*
	pop	cx			;* count of bytes
	mov	di,ax			;* Pointer to allocated area
	mov	pVkTable,di		;* Save pointer to allocated table

	repz	movsb			;* Copy from input table to allocated
					;*     table
	
	
	;*	Cntl Vk
	
	lodsw				;* Get the length of the table
	push	ax			;* Save the length
	push	ds			;* Save this register
	push	es			;* Set DS = DGROUP
	pop	ds
	assumes	ds,DGROUP
	
	mov	cx,1
	save	<es>
	cCall	PpvAllocCb,<cx,ax>	;* (sbHeap, cbReq)
	or	ax,ax			;* ERROR?
	jz	fail1			;* Yes -- abort
	pop	ds			;* ds:si points to the input table
	assumes	ds,nothing		;*
	pop	cx			;* count of bytes
	mov	di,ax			;* Pointer to allocated area
	mov	pVkCtrlTable,di		;* Save pointer to allocated table

	repz	movsb			;* Copy from input table to allocated
					;*     table
	

	;*	Prefix tables

	lodsw				;* Get count of prefix table entries
	mov	cx,ax			;* Move to where it is suppose to be
	jcxz	P20
	
P1:
	push	cx			;* Save counter
	
	lodsw				;* Get the length of the table
	push	ax			;* Save the length
	push	ds			;* Save this register
	push	es			;* Set DS = DGROUP
	pop	ds
	assumes	ds,DGROUP
	
	mov	cx,1	
	save	<es>
	cCall	PpvAllocCb,<cx,ax>	;* (sbHeap, cbReq)
	or	ax,ax			;* ERROR?
	jz	fail3			;* Yes -- abort
	pop	ds			;* ds:si points to the input table
	assumes	ds,nothing		;*
	pop	cx			;* count of bytes
	mov	di,ax			;* Pointer to allocated area
	lodsw				;* Offset in PrefixTable to save at
	mov	bx,ax			;* Move to index register
	mov	PrefixTable[bx],di	;* Save pointer to allocated table

	repz	movsb			;* Copy from input table to allocated
					;*     table
	
	pop	cx			;* Get count back
	loop	P1			;* Loop until done
P20:
	;*	VK prefix tables
	
	lodsw				;* Get count of vk prefix table entries
	mov	cx,ax			;* Move to where it is suppose to be
	jcxz	P40			;* No entries -- done
	
	inc	ax			;* Add one for termination routine
	shl	ax,1			;* Number of bytes needed for table
	push	cx
	save	<es>
	cCall	PpvAllocCb,<cx,ax>	;* (sbHeap, cbReq)
	or	ax,ax			;* ERROR?
	jz	fail3			;* Yes -- abort
	mov	[pDoVkDispatch],ax	;* Save pointer to table
	
	mov	bx,ax			;* Point to table
	pop	cx			;* Restore count
public P22
P22:
	push	cx			;* Save the counter
	lodsw				;* Get the length of the table
	push	ax			;* Save the length
	push	ds			;* save this register
	push	es			;* Set DS = DGROUP
	pop	ds
	assumes	ds,DGROUP
	
	mov	cx,1			;*
	save	<es,bx>			;*
	cCall	PpvAllocCb,<cx,ax>	;* (sbHeap, cbReq)
	or	ax,ax			;* ERROR?
	jz	fail3			;* Yes -- abort
	pop	ds			;* ds:si points to input table
	assumes	ds,nothing		;*
	pop	cx			;* Count of bytes
	mov	di,ax			;* Pointer to allocated area
	lodsw				;* VK character for table
	mov	[bx],ax			;* Save character
	mov	[bx+2],di		;* Save pointer to table
	add	bx,4			;* Move to next table entry
	repz	movsb			;* Copy input table to allocated table	
	pop	cx			;* Restore the countern
	loop	P22			;* Loop until finished
P30:
	xor	ax,ax			;* 
	mov	[bx],ax			;* Add terminator to table
P40:
@@:
cEnd
fail2:
fail3:
	int 3
	mov	ax,1			;* Return failure
	jmp	short @B

	assumes	es,nothing
	assumes	ds,DGROUP

cProc	DefaultEditMgrInit,<FAR,PUBLIC>,<si,di,ds>
cBegin
	mov	ax,initOFFSET DefaultEditMgrTbls
	cCall	EditMgrKeyInit,<cs,ax>
cEnd

