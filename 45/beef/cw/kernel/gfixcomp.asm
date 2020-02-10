;*
;*	COW : Character Oriented Windows
;*
;*	gfixcomp.asm : global fixed compaction
;*	* NOTE :

	TITLE	GFIXCOMP - Global Memory compacted for fixed blocks

.xlist
include	kernel.inc
include	galloc.inc
.list

IFDEF	REALLOC_FIXED
;*****************************************************************************

sBegin	DATA

externW	<psLom>
externW	<pGlobalHeap>

;* Put back as locals latter

globalW	cpTotalFree,(?)
globalW	cpCodeInData,(?)
globalW	cpFixedToEOM,(?)
globalW	cpUsedFixedToEOM,(?)
globalW	cpFreeUnder,(?)
globalW	psFixed,(?)
globalW	psEOM,(?)

sEnd	DATA

;*****************************************************************************


sBegin	KERNEL
	assumes	CS,KERNEL
	assumes	DS,nothing
	assumes	SS,DGROUP

externNP	gsplice				;* galloc.asm
externNP	gjoin				;* galloc.asm
externNP	gnotify				;* gwinterf.asm
externNP	gdiscard			;* gcompact.asm
externNP	gcompact			;* gcompact.asm


;***	gCopyUp(psDest, psSrc) - This routine is used to copy between heap
;*		entries.  It has the advantage that:
;*		1) It does not copy the handles and
;*		2) It does not change ANY registers
;*
cProc	gCopyUp,<NEAR,PUBLIC>,<bx,cx,es,ds,di,si>
parmW	psDest				;* Destination
parmW	psSource			;* Source
parmW	cpSource			;* Size of source
cBegin	
	mov	bx,cpSource		;* Get size of data
					;*	(does not include handle)
	mov	cx,psSource		;* Move over handle
	inc	cx
	add	cx,bx
	mov	ds,cx
	mov	cx,psDest		;* Move over handle
	inc	cx
	add	cx,bx
	mov	es,cx
	STD
	
CM10:
	mov	cx,0fffh		;* - 64Kb (in paras)
	cmp	bx,cx			;* > 64Kb to copy?
	ja	CM20			;* Yes - Do 64K now
	mov	cx,bx			;* No - do this many paras
CM20:
	sub	bx,cx			;* Update count of paras left

	mov	di,es			;* Change base to copy from
	sub	di,cx			;*   --
	mov	es,di
	mov	di,ds
	sub	di,cx
	mov	ds,di

	shl	cx,1			;* Convert from paras to words
	shl	cx,1			;*
	shl	cx,1
;	xor	di,di			;* Zero this out
;	xor	si,si			;* Zero this out
	mov	di,cx
	dec	di			;* move into previous paragraph
	shl	di,1			;* Convert to bytes
	mov	si,di

	repz	movsw			;* Do the copy
	or	bx,bx			;* Done coping?
	jz	CM99			;* Yes - exit
	jmp	short CM10
CM99:
	cld				;* Save us from ourselfs
cEnd


;***	
;*
;*

cProc	CopyHandle,<NEAR,PUBLIC>
cBegin	
	mov	ax,ds:[di].ga_owner	;* Copy Owner
	mov	es:[di].ga_owner,ax	;*
	
	mov	ah,ds:[di].ga_flags	;* Copy Flags
	mov	es:[di].ga_flags,ah	;*
	
	mov	ax,ds:[di].ga_handle	;* Copy Handle
	mov	es:[di].ga_handle,ax	;*

	xor	ax,ax			;* And Free the old block
	mov	ds:[di].ga_owner,ax	;*	Mark as free
	mov	ds:[di].ga_handle,ax	;*	Zero the handle
cEnd	


;*
;*	Assumptions:
;*	1.  Currently assumes that the free segment and	the data segment
;*		are adjacent.
;*	2.  Free segment is above data segment
;*	3.  Doing a compress up
;*	
;*
;*	Inputs:
;*		psFree = free segment to place in
;*		psData = segment to move
;*
;*	Outputs
;*		es = new address of segment
;*		ax = new address of free segment
;*

cProc	gfixedSlide,<NEAR,PUBLIC>
parmW	psFree
parmW	psData
cBegin
	push	ds
	mov	ds,ax
	mov	bx,es:[di].ga_size
	cmp	bx,ds:[di].ga_size
	ja	gfs50
	je	gfs90

;*
;*  	Size of free heap entry is greater than size of Data heap entry
;*
	mov	ax,ds:[di].ga_next	;* Compute address where data
	sub	ax,es:[di].ga_size	;*  heap entry will start after
	dec	ax			;*  the copy is finished
	pop	ds			;* Set up for the splice routine
	mov	si,ax
	mov	es,[psFree]
	call	gsplice

	;*  psData = old
	;*  si = new
gfs10:
	mov	es,psData
	mov	al,GN_MOVE
	mov	bx,es:[di].ga_handle
	mov	dx,es:[di].ga_owner
	mov	cx,si
	inc	cx
	mov	ds,[pGlobalHeap]
	call	gnotify

	mov	es,psData
	mov	bx,es:[di].ga_size
	cCall	gCopyUp,<si,es,bx>
	push	ds
	mov	ax,es			;* Copy some flags around
	mov	ds,ax
	mov	es,si
	cCall	copyHandle
	pop	ds
	
	jmp	gfs99

	;*
	;*	This means same number of paragraphs in both
	;*	source and destination
	;*
gfs90:
	mov	si,psFree
	pop	ds
	jmp	short gfs10

	;*
	;*	Fewer paragraphs in free segment than in data segment
	;*
;*****
gfs50:
	mov	cx,ds:[di].ga_next		;* Address of end of free
	sub	cx,bx				;* Less size of data segment
	dec	cx				;* and one more is
	mov	psFree,cx			;* start of new data segment
	mov	ax,ds				;* setup for gjoin
	mov	es,ax				;* es:di = psFree
	pop	ds				;* ds:di = psGlobalHeap
	push	bx				;* Save size of Data 
	call	gjoin
	
	inc	cx
	mov	bx,es:[di].ga_handle
	mov	dx,es:[di].ga_owner
	mov	ds,[pGlobalHeap]
	mov	al,GN_MOVE
	call	gnotify
	
	pop	bx
	cCall	gCopyUp,<psFree,psData,bx>
	
	mov	es,psData
	mov	si,psFree
	call	gsplice
	
	push	ds
	mov	es,psFree
	mov	ds,psData
	cCall	copyHandle
	pop	ds
	
gfs99:
cEnd


;*
;*  This subroutine is used to do a compact suitable for doing a
;*	realloc on a fixed data segment.  This routine will push
;*	the moveable blocks following the fixed segment up in memory
;*	to make room for it to grow.
;*
;*  This routine is not optimal in either performance or sucess rate.
;*	For instance it does not deal with the possiblity of placing
;*	moveable data segments above the fixed block into free space
;*	below it.  (This could however be done ineffiecently by doing
;*	a normal compact first.)
;*
;*  Inputs:	DX = New size for entry
;*		DS:DI = address of global heap information
;*		ES:DI = header of fixed block to be expanded
;*
;*  Outputs:	AX = size of largest contiguous free block
;*		ES:DI = arena header of largest contiguous free block
;*		DX = minimum # contiguous bytes needed
;*
;*  Destroys:	
;*

cProc	gfixedCompact,<NEAR,PUBLIC>,<si>
cBegin
	mov	psFixed,es		;* Save Segment of Fixed block
	mov	ax,ds:[di].hi_last	;* Last entry in the heap
	sub	ax,ds:[di].gi_reserve	;* Subtract the fence size
	mov	psEOM,ax
	sub	dx,es:[di].ga_size	;* Subtract number of bytes already there

;*	Do an estimate to see if the compact could succeed
;*	-	Compute
;*		-	Total Amount of free space in the system
;*		-	Amount of code in data area
;*		-	Amount of space between this fixed block
;*			and the next fixed block, the code fence,
;*			locked block, or the end of user memory
;*		-	Free space below this fixed block
	xor	ax,ax	
	mov	cpTotalFree,ax		;* Zero out the counters
	mov	cpCodeInData,ax
	mov	cpFixedToEOM,ax
	mov	cpUsedFixedToEOM,ax
	mov	cpFreeUnder,ax

	mov	ax,ds:[di].hi_first	;* Start with first entry
lab10:
	mov	es,ax			;* Point to the entry
	mov	bx,es:[di].ga_size	;* Get the size of the block
	inc	bx			;* Include the header in size
	
	;*
	;*	if (block is free) {
	;*		cpTotalFree += block size;
	;*		if (psBlock < psFixed)
	;*			cpFreeUnder += block size
	;*		else if (psBlock < psEOM)
	;*			cpFixedToEOM += block size
	;*	} else {
	;*		if (psBlock <= psFixed)
	;*		       continue;
	;*		if (psBlock is Locked)
	;*		       break;
	;*		if (psBlock < psEOM) {
	;*			cpFixedToEOM += block size
	;*			if (block is code)
	;*				cpCodeInData += block size
	;*		}
	;*	}
	;*
	cmp	di,es:[di].ga_owner	;* Is is free?
	jne	lab20
	add	cpTotalFree,bx		;* Yes -- add to total free
	
	cmp	ax,psFixed		;* Is it below the fixed entry?
	jae	lab15			;* No - move on
	add	cpFreeUnder,bx		;* Yes -- add to total free below fixed
	jmp	short lab80		;*	 Move to next entry

lab15:	
	je	lab80			;* Is the fixed entry -- skip to next
	cmp	ax,psEOM		;* Above the fence?
	jae	lab80			;* Yes - move to next entry
	add	cpFixedToEOM,bx		;* No -- add size below fence to total
	add	bx,ax			;* Find end of segment
	sub	bx,psEOM		;* Below the fence?
	jbe	lab80			;* yes -- Goto next entry
	sub	cpFixedToEOM,bx		;* No -- subtract overflow from total
	jmp	short lab80		;* Move to next entry

lab20:
	cmp	ax,psFixed		;* Is is below or equal to the fixed entry?
	jbe	lab80			;* Yes -- move to next entry
	
	mov	si,es:[di].ga_handle	;* Get the blocks handle
	cmp	ds:[si].he_count,0	;* Is the block locked?
	jnz	l90			;* Yes -- exit the loop

	cmp	ax,psEOM		;* Is it above the fence?
	jae	lab80			;* Yes -- move to next entry
	
	push	ax			;* Get only overflow size
	add	ax,bx			;* Total size
	sub	ax,psEOM		;* Where are we relative to psEOM
	jbe	@F			;* End point is below psEOM -- bx = all
	sub	bx,ax			;* Subtract overflow
@@:
	pop	ax			;* Restore register
	add	cpFixedToEOM,bx		;* Add to amount between fixed and EOM
	add	cpUsedFixedToEOM,bx	;* Add to used amount between fixed and EOM

	test	es:[di].ga_flags,GA_DISCCODE ;* Is this a code segment?
	jz	lab80			;* No - move to next entry
	add	cpCodeInData,bx		;* Yes - add in
	sub	cpUsedFixedToEOM,bx	;* Add to used amount between fixed and EOM

lab80:
	mov	ax,es			;* Move to next entry
	cmp	ax,es:[di].ga_next	;* Last Entry? (next == cur)
	je	l90

	mov	ax,es:[di].ga_next	;* search forward along the link
	jmp	lab10
l90:

;-	No - return failure
;	-	If (amount of space < space needed) -- fail
	mov	ax,cpFixedToEOM
	sub	ax,cpUsedFixedToEOM
	cmp	dx,ax
	jbe	la10
	jmp	gfcExit

;*
;*	-	If ((Total Free space + Code in Data) < space needed) -- fail
;*
la10:
	mov	ax,cpTotalFree
	add	ax,cpCodeInData
	cmp	ax,dx
	jae	la20
	jmp	gfcExit

;*	Compute parameters for compaction
;*	Do the compaction
;*
;*	-	discard code as needed
;*		#paras = (Requested - (Total Free - Free Space below))
la20:
	push	dx			;* Save request size
	sub	dx,cpTotalFree		;* Compute amount of code to discard
	add	dx,cpFreeUnder		;*
	or	dx,dx			;* Do we need any?
	jle	la25			;* no --
	call	gdiscard

la25:	
	pop	dx			;* Restore Request size

;*
;*	-	Walk from this until sufficent free space has been covered
;*		Starting at fixed block
;*		free space = 0
;*		while (not to end of region) {
;*		    if (Block is free) {
;*		       add block size to free space
;*		       if (free space > space requested)
;*		          break;
;*		    } else {
;*			if (block is fixed) fail
;*			if (past fence) fail
;*		    go to next block
;*		}

	mov	es,psFixed		;* Starting at block to grow
	xor	cx,cx			;* Initially no free space found
	jmp	short la50		;* Move to next block

la30:	;*** Start of loop

	cmp	di,es:[di].ga_owner	;* Is the block free?
	jne	la40			;* No - jmp
					;* Yes (block was free)
	mov	bx,es:[di].ga_size	;* Get the size of the block
	inc	bx			;* Add one for handle
	add	cx,bx			;* Total free found to-date
	cmp	cx,dx			;* enough yet?
	jae	la60			;* Yes -- exit
	jmp	short la50		;* No  -- go to next block

la40:					;* was not a free block

la50:					;* Move to next block
	cmp	ax,es:[di].ga_next	;* Last entry in heap?
	je	la55			;* Yes -- exit

	mov	ax,es:[di].ga_next	;* Move to next entry
	mov	es,ax			;* Set up segment register
	jmp	la30			;* Go to start of loop

	;***	End of Loop

	;*	If we get here then we did not find enough area
	;*	We should therefore do an immediate fail
la55:
	jmp	gfcExit

	;*	Sufficent space has been found to do the shift up
	;*
la60:
;*
;*		assert current block is free
;*
	AssertEq di,es:[di].ga_owner
;*
;*		if (free space < space requested) fail
;*
	cmp	cx,dx
	jae	la70
	jmp	gfcExit
la70:
;*
;*	-	Compute end point
;*		if (free space == space requested) {
;*		    end point = current block
;*		} else {
;*		    delta = free space - space requested
;*		    split current block by removing last delta paras
;*		    current block = first of split pair
;*		}
	public	phase2
phase2:
	je	la80
	sub	cx,dx			;* extra free = Total Free - Space needed
	and	cx,NOT 1		;* make it even
					;* cx is now the number of excess
					;* paragraphs.
	jz	la80			;* if cx == 0 then start moving data
	mov	ax,es:[di].ga_next	;* Start of next heap entry
	sub	ax,cx			;* Less size of excess -- start of
					;* new block
	push	si			;* Set-up for subroutine call
	mov	si,ax
	call	gsplice			;* Create a new block
	pop	si
la80:
	mov	ax,es:[di].ga_prev	;* Move to previous heap entry
;		
;	-	walk down until fixed is found pushing each non-free segment
;		up and combining free segments
;		
;		move to previous block
;		while (block is not fixed) {
;		   if (block is free) {
;		      combine block and next block into a single
;			block
;		   } else {
;		      slide this block to end of next block
;		   }
;		   move to previous block
;		}
b10:
	mov	es,ax
	cmp	di,es:[di].ga_owner	;* Is this block free?
	jnz	b50			;* No - go to else clause
	
	push	es			;* save es
	mov	ax,es:[di].ga_next	;* Where is the next block
	mov	es,ax			;*
	cmp	di,es:[di].ga_owner	;* Is it free?
	pop	es			;* restore
	jnz	b90			;* No - move to next block in list
	
	mov	es,ax			;* Set-up for the join
	call	gjoin			;* Join together the free blocks
	jmp	b90			;* Move to the next block in the list

b50:					;* Not a free block
	mov	ax,es:[di].ga_next	;* Slide up in memory
	cCall	gfixedSlide,<ax,es>	;*
b90:
	mov	ax,es			;*
	AssertNe ax,es:[di].ga_prev	;* FIRST IN LOOP!!!!

	mov	ax,es:[di].ga_prev	;* Next forward
	cmp	ax,[psFixed]
	je	b99
	jmp	short b10

b99:
;		
;		assert next block is free
;		test size of next block >= requested
;		true - succeed
;		false - fail
;
gfcExit:
cEnd	gfixedCompact

sEnd	CODE

ENDIF	; REALLOC_FIXED
	end


		
