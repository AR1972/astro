;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; This file has code which is differnt for real and protected modes and has  ;
; proper ifdefs.							     ;
;								             ;
; History:								     ;
;									     ;
;	 Fri June-15-1990.	-by-  Amit Chatterjee [amitc]		     ;
;	 Adapted for the Dos Task Switcher.				     ;
;									     ;
;        Fri June-30-1989.  	-by-  Amit Chatterjee [amitc]		     ;
;        Created for Windows. (Added the History legend)      		     ;
;----------------------------------------------------------------------------;

;----------------------------------------------------------------------------;

	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	include macros.mac
	include	njmp.mac
	include	woaerr.inc
	include	woaarena.inc
	.list

	.286p

;----------------------------------------------------------------------------;
; define all public names of functions and variables here.		     ;
;----------------------------------------------------------------------------;

	public	GetAppXmsBase
	public	EmergencyExit
	public	SetSelectorBaseLim64
	public	GetSelectorBase

;----------------------------------------------------------------------------;
; declare  the external function calls.     				     ;
;----------------------------------------------------------------------------;



;----------------------------------------------------------------------------;

sBegin	Data

;----------------------------------------------------------------------------;
; define the global variables defined elsewhere			       	     ;
;----------------------------------------------------------------------------;

externW WoaCsSize			;size of protected mode code seg
externW WoaDsSize			;size of winoldap data segment
externW StubSegSize			;size of real mode stub segment
externW	LowMemArenaSel			;selector/seg for low heap arena
externW	ArenaWalkSel			;temp selector for walkimg arena chain
externW	ArenaRWSel			;temp sel for reading/writing arenas
externW WoaStubSel			;selector for stub code segment
externW XmsHeapWalkSel			;used for walking xms heap
externW	LowMemParaSize			;size of low heap in paragraphs
externD	XmsBankSize			;size of XMS to be allocated
externB ErrorType			;save area for error code
externW	SegResizeBlock			;block to resize in real mode
externW	SizeReservedArea		;size of area reserved area at heap st.
externD SwapFileOffset			;lseek offset for swap in/out
externW	UsableLowHeapSel		;sel/seg for reusable part of low heap
externW LowMemSel			;sel for available mem
externB	WinSaveXms			;if ff, XMS to be saved at win swap out
externW	HighMemXmsHandle		;handle of the high heap XMS block
externW AppXmsBaseSel			;selector for apps XMS base.
externW	AppUsesXMS			;app used XMS allocated to it or not
externB NodeToSwitchTo			;node that we want to switch to

; declare the storage for the registers needed to communicate with the stub.

externW	RealMode_AX			;space for AX
externW	RealMode_BX			;space for BX
externW	RealMode_CS			;space for CS
externW	RealMode_DS			;space for DS
externW	RealMode_ES			;space for ES
externW	RealMode_IP			;space for IP

	;-------------------------------------------------------;
	; define any locally used constants			;
	;-------------------------------------------------------;


	;-------------------------------------------------------;
	; define any external contants.				;
	;-------------------------------------------------------;


;---------------------------------------------------------------------------;
; now define the other variables that will be needed.			    ;
;---------------------------------------------------------------------------;

		public	lpXmsControl
		public	SegAfterReservedArea

lpStubEntry		dd	?  	;call gate for stub code
SegAfterReservedArea	dw	?	;segment after the reserved area
SwapHandle		dw	?	;temporary space
lpXmsControl		dd	?	;XMS control function address

;----------------------------------------------------------------------------;

sEnd Data

;----------------------------------------------------------------------------;
; now declare the existence of the realmode stub segment and the variables   ;
; that we want to reference here.					     ;
;----------------------------------------------------------------------------;

createSeg   _WOARLMSEG,StubSeg,word,public,code
sBegin	StubSeg

externB XmsStartLine			;start of XMS handler in real mode stub

sEnd	StubSeg

;----------------------------------------------------------------------------;
sBegin	Code

	assumes	cs,Code
	assumes	ds,Data
	assumes	es,nothing

;----------------------------------------------------------------------------;
; declare the external winoldap functions used here.			     ;
;----------------------------------------------------------------------------;

	externNP ErrorManager		;(WOAERR.ASM)
	externNP MoveWOA		;(WOAMGR.ASM)
	externNP LoadRealModeStub	;(WOAMGR.ASM)
	externNP CreateFile		;(WOAMGR.ASM)
	externNP OpnFile		;(WOAFILE.ASM)
	externNP ReadFile		;(WOAFILE.ASM)
	externNP WriteFile		;(WOAFILE.ASM)
	externNP LseekFile		;(WOAFILE.ASM)
	externNP CloseFile		;(WOAFILE.ASM)
	externNP DeleteFile		;(WOAPMRM.ASM)
	externNP SetNormalAttributes	;(WOAFILE.ASM)
	externNP SetHiddenAttributes	;(WOAFILE.ASM)
	externNP GetSizeInBytes		;(WOAUTILS.ASM)
	externNP RestoreSwappedGroups   ;(WOAUTILS.ASM)
	externNP IsEnoughDiskSpace	;(WOAUTILS.ASM)
	externNP SwapOutNonZeroBlocks	;(WOAUTILS.ASM)


;----------------------------------------------------------------------------;
; SaveFirstDosBlock:						             ;
;									     ;
; This routine is invoked to copy out the first arena header to the swap     ;
; file and to skip over the _DATA and _TEXT segment.			     ;
; If SFDBOpCode is 0 this will not write anything to disk. The size of the   ;
; swap area will be returned in CX:DX and the size of the skipped part in AX.;
;----------------------------------------------------------------------------;

cProc	SaveFirstDosBlock,<NEAR,PASCAL,PUBLIC>,<es>

	parmW	FileHandle		;handle of the swap file
	parmB	SFDBOpCode		;get size only or save

cBegin

	cld				;do not take chances with this
	mov	es,LowMemArenaSel	;get the selector for the arena

; if the OpCide is for get size only, then bypass the File IO

	cmp	SFDBOpCode,0		;get size only ?
	jz	SFDBByPassIO1		;yes

; first swap out the arena.

	xor	ax,ax			;need for hiword of count and for offset
	mov	cx,16			;16 bytes of arena header
	save	<es>			;need to save this
	cCall	WriteFile,<FileHandle,es,ax,ax,cx>
	jc	SaveFirstBlockRet	;will not continue with error

SFDBByPassIO1:

; now calculate the part that will be skipped over - the portion between
; LowMemSel and _WOARLMSEG.

	mov	ax,_WOARLMSEG		;start of stub segment
	sub	ax,LowMemSel		;start of the first block
	xor	cx,cx			;size written out to be in CX:DX
	mov	dx,16			;we just wrote out the arena

SaveFirstBlockRet:

cEnd
;----------------------------------------------------------------------------;
; RestoreFirstDosBlock:							     ;
;									     ;
; This routine restore the arena of the first block from the swap file.      ;
;----------------------------------------------------------------------------;

cProc	RestoreFirstDosBlock,<NEAR,PUBLIC,PASCAL>,<es>

	parmW	FileHandle		;handle of the swap file

cBegin

	cld				;do not take chances with this

; read in the arena header.

	xor	ax,ax			;nedd for hiword of count and for offset
	mov	es,LowMemArenaSel	;get the selector for the arena
	mov	cx,16			;16 bytes of arena header
	save	<es>			;need to save this
	cCall	ReadFile,<FileHandle,es,ax,ax,cx>

cEnd
;----------------------------------------------------------------------------;
; RestoreSwap
;----------------------------------------------------------------------------;
; SaveAppXmsContext:							     ;
;									     ;
; This routine takes a file handle as a parameter and saves the apps XMS     ;
; context from the current location in the file. The context is saved as a   ;
; 4 byte size followed by the actual XMS block if the size is non zero.      ;
;----------------------------------------------------------------------------;

cProc	SaveAppXmsContext,<NEAR,PUBLIC,PASCAL>,<es,si,di>

	parmW	FileHandle		;handle of the dos swap file

cBegin

	cld				;do not take chances with this

; irrespective of whether size is 0 or not, we need to save the size at least

	mov	dx,wptr [XmsBankSize]	;get low word of size
	mov	cx,wptr [XmsBankSize+2]	;get high word of size

; swap out the size and the XMS block to the disk if size is not zero, else
; just write the size into the swap file.

	cCall	RMSwapOutXmsBlock,<FileHandle,cx,dx>

SaveAppXmsContextRet:

cEnd
;----------------------------------------------------------------------------;
; RestoreAppXmsContext: 						     ;
;									     ;
; Restores the apps xms context. This routine takes a file handle as a param-;
; -eter and the 4 bytes at the current position of the file is the size of   ;
; the XMS memory allocated to the app. If the size is non zero, the current  ;
; contents of the XMS area must be saved into the XMS swap file before       ;
; restoring the apps XMS memory.       					     ;
;									     ;
; A very important point to note here is that, at this point the app that we ;
; are trying to restore may not have been loaded by this instance of winoldap;
; So the value of 'XmsBankSize' that we need for restoring is not a property ;
; of the app we are trying to restore. We will reset the value of the        ;
; variable from the value stored in the old apps file.			     ;
;----------------------------------------------------------------------------;

cProc	RestoreAppXmsContext,<NEAR,PUBLIC,PASCAL>,<es,si,di>

	parmW	FileHandle		;handle of the swap file

	localD	TempBuffer		;will read in size of xms block here
	localD	lpSwapFile		;name of the swap file (ptr to it)

cBegin

	cld				;do not take chances with this

; read in the 'AppUsesXMS' field.

	mov	di,DataOFFSET AppUsesXMS;will read in here directly
	xor	ax,ax			;hiword of count is 0
	mov	bx,2			;need to read 2 bytes 
	cCall	ReadFile,<FileHandle,ds,di,ax,bx>
	jc	RestoreAppXmsContextRet	;cannot procedd with error

; read in the 4 byte XMS memory size

	smov	es,ss			;temp buffer is in stack
	lea	di,TempBuffer		;es:di points to buffer
	xor	ax,ax			;hiword of count is 0
	mov	bx,4			;need to read 4 bytes (loword of count)
	cCall	ReadFile,<FileHandle,es,di,ax,bx>
	jc	RestoreAppXmsContextRet	;cannot procedd with error

; test to see if the size of XMS memory is zero or not.

	mov	cx,seg_TempBuffer	;get the high word
	mov	dx,off_TempBuffer	;get the low word

; reset 'XmsBankSize', we will need it to be what is in cx:dx when we finally
; need to save the apps context

	mov	wptr [XmsBankSize+2],cx	;save the high word
	mov	wptr [XmsBankSize],dx	;save the low word

; test to see whether there is at all any context to restore

	mov	ax,cx
	or	ax,dx			;is the memory block size 0 ?
	jz	RestoreAppXmsContextRet	;yes, there is no context to restore.

; test to see if 'AppUsesXMS' is zero, if so then there will be no XMS to 
; swap in.

	cmp	AppUsesXMS,0		;did it use XMS ?
	jz	RestoreAppXmsContextRet	;no, there is no context to restore.

; now swap in the dos xms context from the dos app swap file.

	xor	ax,ax			;opcode = 0 => read from file
	cCall	RMSwapInXmsBlock,<FileHandle,TempBuffer,ax>

RestoreAppXmsContextRet:

cEnd
;----------------------------------------------------------------------------;
; RMSwapOutXmsBlock:							     ;
;									     ;
; Given a file handle and a dword worth of block size, this routine swap the ;
; xms block image preceeded by the size into the file.			     ;
;									     ;
; (This code works in real mode only, where the XMS memory is not directly   ;
;  accessible and has to be moved into conventional memory before swapping   ;
;  out. However it has been ensured that the portion of conventional memory  ;
;  not holding the winoldap stub is free at this point).		     ;
;									     ;
; We will read in chunks of extended memory into conventional memory and for ;
; the chunk read in we will write out the non zero blocks using our general  ;
; swap out routine.							     ;
;----------------------------------------------------------------------------;

cProc	RMSwapOutXmsBlock,<NEAR,PUBLIC,PASCAL>

	parmW	FileHandle		;handle of the file
	parmD	BankSize		;size of the bank

	localD	lpXmsInvoke		;xms entry function address
	localV	RMXOHeader,8		;to write out node header
	localV	XmsMoveStruct,16	;the move parameter block.
	localW	SegForXmsXfer		;seg to be used for move

cBegin

	cld				;do not take chances with this

; save  the 'AppUsesXms' variable. This will govern whether we swap out
; actual XMS or not.

	mov	ax,2			;want to write 2 bytes
	xor	cx,cx			;high word of count is 0
	mov	si,DataOFFSET AppUsesXMS;ds:si points to size
	cCall	WriteFile,<FileHandle,ds,si,cx,ax>
	njc	RMSwapOutXmsBlockRet	;cannot proceed with error

; save the length of the block
	
	mov	ax,4			;want to write 4 bytes
	xor	cx,cx			;high word of count is 0
	lea	si,BankSize		;ss:si points to size
	cCall	WriteFile,<FileHandle,ss,si,cx,ax>
	njc	RMSwapOutXmsBlockRet	;cannot proceed with error

; if the block size is 0, we do not have to write it out

	mov	si,seg_BankSize		;load the high word
	or	si,off_BankSize		;is the size 0 ?
	njz 	RMSwapOutXmsBlockRet	;return

; if 'AppUsesXMS' is 0 we do not have to write XMS out.

	cmp	AppUsesXMS,0		;did it use XMS ?
	njz 	RMSwapOutXmsBlockRet	;return

; the following code can destroy the DOS memory arena chain currently
; within the low heap area.  It's not safe to make DOS calls with the arena
; list in an inconstant state, so we zap the low heap arena to indicate it's
; the end of the list.	This was found when running SHARE.EXE under DOS
; 4.0.	When doing a set file attribute Int 21h shortly after this
; routine executes, SHARE would hang trying to walk the arena list.  Another
; routine will restore the Low heap arena in a little while.

	mov	es,LowMemArenaSel
	mov	bptr es:[0],'Z'

; now calculate the amount of bytes of reusable low heap area. We will use the
; area from the start of the StubSeg on.

	mov	ax,_WOARLMSEG		;start of stub seg (swapped out)
	mov	SegForXmsXfer,ax	;will use for XMS transfer
	sub	ax,LowMemSel		;amount of reserved area (CS + DS)
	neg	ax			;want to subtract it from -
	add	ax,LowMemParaSize	;- the total low heap size
	call	GetSizeInBytes		;AX:BX has the size in bytes
	and	bx,0fff0h		;make it para multiple

; (note that we have just made the size of conventional memory that we can
;  use for the move to be a multiple of para size and the amount of xms that
;  we want to swap out is also a multiple of a para size.)

; now initialize the xms move structure.

	smov	es,ss			;will have structure built in stack
	lea	si,XmsMoveStruct	;the move structure
	mov	cx,HighMemXmsHandle	;the source handle
	mov	es:[si+4],cx		;save the source handle
	xor	cx,cx			;source offset will start at zero.
	mov	es:[si+6],cx		;low word of source offset
	mov	es:[si+8],cx		;high word of source offset
	mov	es:[si+10],cx		;destination handle = 0
	mov	es:[si+12],cx		;destination offset = 0
	mov	cx,SegForXmsXfer	;area to transfer bytes into
	mov	es:[si+14],cx		;destination segment
	mov	cx,wptr [lpXmsControl+2]
	mov	seg_lpXmsInvoke,cx	;save loword of xms call function
	mov	cx,wptr [lpXmsControl]
	mov	off_lpXmsInvoke,cx	;save hiword of xms call function
	mov	cx,seg_BankSize		;get the hiword of the block size
	mov	dx,off_BankSize		;get the loword of the block size

; move in as much of the XMS block as possible and write it out into the
; file.

RMSwapOutXmsLoop:

; compare size left to swap out and size of area that can be used for the move
; and decide what part to swap out.

	cmp	ax,cx			;compare high words
	nja	RMSwapOutLast		;last portion to swap out
	jb	RMSwapOutNext		;next part to swap out
	cmp	bx,dx			;compare the low words
	nja	RMSwapOutLast		;last portion to swap out

RMSwapOutNext:

	mov	es:[si],bx		;save low word of move length
	mov	es:[si+2],ax		;save high word of move length
	pushem	ds,ax,bx		;save these registers
	mov	ah,0bh			;function code for move
	smov	ds,ss			;ds:si points to move structure
	call	lpXmsInvoke		;call the function to do the move
	mov	di,ax			;save the return code
	popem	ds,ax,bx		;restore the registers
	or	di,di			;was it successful ?
	njz	RMSwapOutFail		;no, return back

; now write the block out to the disk, after zero compressing it.

	pushem	ax,bx,cx,dx,si		;save the relevant registers
	mov	dx,es:[si]		;low word of size moved
	mov	cx,es:[si+2]		;cx:dx has amount to write
	mov	ax,es:[si+14]		;segment where block was moved
	mov	si,0ffffh		;want to actually swap out
	cCall	SwapOutNonZeroBlocks,<FileHandle,ax,cx,dx,si>
	popem	ax,bx,cx,dx,si		;restore registers
	jc	RmSwapOutXmsBlockRet	;error in write, return back


; also write out a gap node with all 0s

	pushem	ax,bx,cx,dx,si		;save the relevant registers
	push	es
	smov	es,ss			;need to access header structure
	lea	di,RMXOHeader		;es:di points to the node header area
	push	di			;save the offset
	xor	ax,ax			;want to write 0
	mov	cx,4			;size of header	in words
	rep	stosw			;initialize it
	pop	di			;es:di points to the NodeHeader
	mov	cx,8			;8 bytes of header to write
	xor	dx,dx			;hiword of number of bytes to write
	cCall	WriteFile,<FileHandle,es,di,dx,cx>
	pop	es			;restore
	popem	ax,bx,cx,dx,si		;restore registers
	jc	RmSwapOutXmsBlockRet	;error in write, return back

; update variables for next move

	sub	dx,bx			;update amount left to swap out
	sbb	cx,ax			;cx:dx has amount left to move
	mov	di,cx			;check to see whether we are done
	or	di,dx			;no more left ?
	jz	RmSwapOutDone		;yes, we are done.

; update move structure for next portion.

	add	es:[si+6],bx		;update loword of source offset
	adc	es:[si+8],ax		;update hiword of source offset
	jmp	RMSwapOutXmsLoop	;continue till done.

RMSwapOutLast:

	mov	ax,cx			;get amount left into ax:bx
	mov	bx,dx			;ax:bx has size of last portion
	jmp	RMSwapOutNext		;move and write it out

RMSwapOutDone:

; need to write out a dummy header to mark the end of the memory image area

	smov	es,ss			;need to access header structure
	lea	di,RMXOHeader		;es:di points to the node header area
	push	di			;save the offset
	xor	ax,ax			;need to zero it out
	mov	cx,4			;size of header	in words
	rep	stosw			;initialize it
	pop	di			;es:di points to the NodeHeader
	mov	cx,8			;8 bytes of header to write
	xor	dx,dx			;hiword of number of bytes to write
	cCall	WriteFile,<FileHandle,es,di,dx,cx>
	jmp	short RMSwapOutXmsBlockRet

RMSwapOutFail:

	stc				;swap out failed

RMSwapOutXmsBlockRet:

cEnd
;----------------------------------------------------------------------------;
; RMSwapInXmsBlock:						   	     ;
;									     ;
; Given a file handle and dword worth of XMS context size, it reads back the ;
; image into it's correct place.				             ;
;									     ;
; It will read in swap groups from the file over zeroed out memory and do the;
; moves.								     ;
;									     ;
; There is a third parameter to this routine, 'OpCode', which when 0ffh will ;
; actually ZEROINT the XMS allocation instead of trying to read from the     ;
; file (in this case the FileHandle is ignored).			     ;
;----------------------------------------------------------------------------;

cProc	RMSwapInXmsBlock,<NEAR,PUBLIC,PASCAL>

	parmW	FileHandle		;handle of the file
	parmD	BankSize		;size of the context
	parmB	OpCode			;ZEROINT or not

	localD	lpXmsInvoke2		;xms entry function address
	localV	XmsMoveStruct2,16	;the move parameter block.
	localW	SegForXmsXfer2		;seg to be used for move

cBegin

	cld				;do not take chances with this

; if the block size is 0, we do not have to swap it in

	mov	si,seg_BankSize		;load the high word
	or	si,off_BankSize		;is the size 0 ?
	njz 	RMSwapInXmsBlockRet	;return

; now calculate the amount of bytes of reusable low heap area. We will start
; from the beginig of the StubSeg (not swapped in yet)

	mov	ax,_WOARLMSEG		;beginig of stubseg
	mov	SegForXmsXfer2,ax	;will use for XMS transfer
	sub	ax,LowMemSel		;reserved area (CS+DS)
	neg	ax			;want to subtract it from -
	add	ax,LowMemParaSize	;- the total low heap size
	call	GetSizeInBytes		;AX:BX has the size in bytes
	and	bx,0fff0h		;make it para multiple

; (note that we have just made the size of conventional memory that we can
;  use for the move to be a multiple of para size and the amount of xms that
;  we want to swap out is also a multiple of a para size.)

; now initialize the xms move structure.

	smov	es,ss			;will have structure built in stack
	lea	si,XmsMoveStruct2	;the move structure
	mov	cx,HighMemXmsHandle	;the source handle
	mov	es:[si+10],cx		;save the destination handle
	xor	cx,cx			;source offset will start at zero.
	mov	es:[si+12],cx		;low word of destination offset
	mov	es:[si+14],cx		;high word of destination offset
	mov	es:[si+4],cx		;source handle = 0
	mov	es:[si+6],cx		;source offset = 0
	mov	cx,SegForXmsXfer2	;area to transfer bytes into
	mov	es:[si+8],cx		;source segment
	mov	cx,wptr [lpXmsControl+2]
	mov	seg_lpXmsInvoke2,cx	;save loword of xms call function
	mov	cx,wptr [lpXmsControl]
	mov	off_lpXmsInvoke2,cx	;save hiword of xms call function
	mov	cx,seg_BankSize		;get the hiword of the block size
	mov	dx,off_BankSize		;get the loword of the block size

; move in as much of the XMS swapped out block that can be held in memory
; and move it back to its original area

RMSwapInXmsLoop:

; compare size left to swap in and size of area that can be used for the move
; and decide what part to swap in.

	cmp	ax,cx			;compare high words
	ja	RMSwapInLast		;last portion to swap in
	jb	RMSwapInNext		;next part to swap in
	cmp	bx,dx			;compare the low words
	ja	RMSwapInLast		;last portion to swap out

RMSwapInNext:

	mov	es:[si],bx		;save low word of move length
	mov	es:[si+2],ax		;save high word of move length

; if OpCode is ZEROINIT then memory is already zero-ed out, just move it.

	cmp	OpCode,0ffh		;ZEROINIT required ?
	jz	RMXmsInBlockReadIn	;yes, skip reading from file.

; zero out the block first.

	pushem	ax,bx,cx,dx
	mov	ax,_WOARLMSEG		;begining of stub
	sub	ax,LowMemSel		;size of reserved area in paras
	cCall	ZeroOutMemory,<LowMemSel,LowMemParaSize,ax>
	popem	ax,bx,cx,dx		

; read one swap group. 

	save	<ax,bx,cx,dx,si>	;save
	cCall	RestoreSwappedGroups,<FileHandle>
	jc	RMSwapInXmsBlockRet	;error, cannot proceed

RMXmsInBlockReadIn:

; now move it to its original position

	pushem	ds,ax,bx		;save these registers
	mov	ah,0bh			;function code for move
	smov	ds,ss			;ds:si points to move structure
	call	lpXmsInvoke2		;call the function to do the move
	mov	di,ax			;save the return code
	popem	ds,ax,bx		;restore the registers
	or	di,di			;was it successful ?
	jz	RMSwapInFail		;no, return back

; update variables for next move

	sub	dx,bx			;update amount left to swap out
	sbb	cx,ax			;cx:dx has amount left to move
	mov	di,cx			;check to see whether we are done
	or	di,dx			;no more left ?
	jz	RmSwapInDone		;yes, we are done.

; update move structure for next portion.

	add	es:[si+12],bx		;update loword of destination offset
	adc	es:[si+14],ax		;update hiword of destination offset
	jmp	RMSwapInXmsLoop		;continue till done.

RMSwapInLast:

	mov	ax,cx			;get amount left into ax:bx
	mov	bx,dx			;ax:bx has size of last portion
	jmp	RMSwapInNext		;read it in and move it

RMSwapInDone:

	clc				;no error while moving
	jmp	short RMSwapInXmsBlockRet

RMSwapInFail:

	stc				;swap out failed

RMSwapInXmsBlockRet:

cEnd
;----------------------------------------------------------------------------;
; GetXmsFreeArenaSel:							     ;
;									     ;
; This routine is a NOP in real mode. In protected mode, it prepares the     ;
; selector for the base of the area to be allocated for xms and finds the    ;
; first free windows high heap arena selector with which we can start        ;
; swapping the windows xms image out.					     ;
;----------------------------------------------------------------------------;

cProc	GetXmsFreeArenaSel,<NEAR,PUBLIC,PASCAL>

cBegin


cEnd
;----------------------------------------------------------------------------;
; GetAppXmsBase:							     ;
;						        		     ;
; This routine returns in CX:DX the base of the extended memory to be alloca-;
; -ted to the dos application.						     ;
;----------------------------------------------------------------------------;

GetAppXmsBase  proc  near

; lock the XMS block to get its base and then release the lock. We will not
; be doing any error checking here as we are sure to have a valid handle.

	mov	dx,HighMemXmsHandle	;handle of the extended memory block
	mov	ah,0ch			;function code for lock block
	call	lpXmsControl		;lock it.
	mov	cx,bx			;get loword of base
	xchg	cx,dx			;cx:dx has the base
	pushem	cx,dx			;save the base
	mov	ah,0dh			;function code for unlocking the block
	mov	dx,HighMemXmsHandle	;handle of the xms block
	call	lpXmsControl		;unlock the block
	popem	cx,dx			;get back the base
	ret

GetAppXmsBase	endp

;----------------------------------------------------------------------------;
; GetRealModeSeg:							     ;
;									     ;
; This routine takes a selector as parameter and returns in AX the correspo- ;
; -nding real mode segment.						     ;
;----------------------------------------------------------------------------;

cProc	GetRealModeSeg,<NEAR,PUBLIC,PASCAL>

	parmW	SrcSel			;selector whose address to be extracted

cBegin

	mov	ax,SrcSel		;this itself is the segment

cEnd
;----------------------------------------------------------------------------;
; ShuffleSegments:							     ;
;							                     ;
; Relocates the winoldap segments to where they will be resident when the Dos;
; application resides and transfers parameters to the stub segment.	     ;
;									     ;
; The exact actions, will depend on whether we are executing real or protect-;
; -ed mode winoldap.						             ;
;----------------------------------------------------------------------------;

cProc	ShuffleSegments,<NEAR,PUBLIC,PASCAL>

cBegin

	cld				;do not take chances with this

; now load data into the real mode stub.

	cCall	LoadRealModeStub

; now zero out the memory after the stub.

	cCall	ZeroOutMemory,<LowMemSel,LowMemParaSize,SizeReservedARea>

; if XMS is allocated to the app, ZEROINIT the xms block

	mov	ax,word ptr [XmsBankSize];get the low word
	mov	dx,word ptr [XmsBankSize+2]
	push	ax			;save
	or	ax,dx			;is the size 0 ?
	pop	ax			;dx:ax has bank size
	jz	@f			;no xms allocation
	mov	bx,0ffffh		;ZEROINIT opcode
	cCall	RMSwapInXmsBlock,<bx,dx,ax,bx>
@@:

cEnd
;----------------------------------------------------------------------------;
; MoveWoaIntoStubArea:							     ;
;									     ;
; This routine relocates the main winoldap code and data segment down to the ;
; start of either the low or the high heap area to make room for rolling the ;
; dos app in.								     ;
;									     ;
; In protected mode winoldap, they moved to the start of the high heap area  ;
; where as in real mode winoldap, they are moved to the start of the low heap;
; area.								             ;
;----------------------------------------------------------------------------;

cProc	MoveWoaIntoStubArea,<NEAR,PASCAL,PUBLIC>

cBegin

	cld				;do not take chances with this

;***	cCall	MoveWoa,<LowMemSel,WoaHighCsSel,WoaHighDsSel>

cEnd
;----------------------------------------------------------------------------;
; InvokeStubCode:							     ;
;									     ;
; This routine invokes the real mode stub code and control will come back    ;
; on a context switch or once the old app ends.				     ;
;									     ;
; The invocation code will be different for real and protected modes.	     ;
;----------------------------------------------------------------------------;

cProc	InvokeStubCode,<NEAR,PUBLIC,PASCAL>

cBegin


; prepare the call address from the values of CS,IP in the client register set

	mov	ax,RealMode_CS
	mov	wptr [lpStubEntry+2],ax	;save segment of stubcode
	mov	ax,RealMode_IP
	mov	wptr [lpStubEntry],ax	;save entry offset

; load the other segment registers.

	pushem	ds,bp			;save in new stack
	mov	ax,RealMode_DS
	mov	es,ax			;initialize
	mov	ax,RealMode_AX
	call	lpStubEntry		;invoke the stub code
	popem	ds,bp			;get back saved ds
	mov	RealMode_AX,ax
	mov	RealMode_BX,bx
	mov	AppUsesXMS,cx		;tells us whether app used XMS or not
	mov	NodeToSwitchTo,dl	;for directed hot key switch

	smov	es,ds			;set es to be same as ds

cEnd
;----------------------------------------------------------------------------;
; GetSegSize:								     ;
;									     ;
; This routine gets the size of a segment. Code is different for real and    ;
; protected modes.							     ;
;----------------------------------------------------------------------------;

cProc	GetSegSize,<NEAR,PUBLIC,PASCAL>

	parmW	SegSel			;segment or selector value

cBegin

	push	es			;save it
	mov	ax,SegSel		;get the segment value
	dec	ax			;the associated arena segment
	mov	es,ax			;have it in es
	mov	ax,es:[3]		;get the size in paragraphs
	shiftl	ax,4			;gives the size in bytes
	pop	es			;restore
	xor	dx,dx			;hope segments are never exactly 64k

cEnd
;----------------------------------------------------------------------------;
; ZeroOutMemory:							     ;
;									     ;
; Given a start selector and a DWORD size, this routine clears out that      ;
; amount of memory from the base of the selector.			     ;
;----------------------------------------------------------------------------;

cProc	ZeroOutMemory,<NEAR,PUBLIC,PASCAL>,<es,di,cx>

	parmW	BaseSel			;selector for base
	parmW	MemParaSize		;size of area in paras
	parmW	ResSize			;reserved size at top.
	localD	SelBase			;original base

cBegin.
  	
	mov	bx,BaseSel		;get the base
	add	bx,ResSize		;add in reservbed size in para
	mov	es,bx			;start sel
	mov	ax,MemParaSize		;get the para size
	sub	ax,ResSize		;subtract reserved area size
	call	GetSizeInBytes		;AX:BX has size in bytes
	mov	dx,ax			;DX:BX has the size.
	xor	di,di			;start at 0 offset

zero_loop:

	or	dx,dx			;still > 64K ?
	jz	zero_last		;no.
	mov	cx,8000h		;32K word
	xor	ax,ax
	rep	stosw			;zero out the segment.
	dec	dx			;one more 64k band done.
	mov	cx,es			;get the selector
	add	cx,1000h		;next 64k band
	mov	es,cx			;next segment
	jmp	short zero_loop		;continue.

zero_last:

	mov	cx,bx			;get remaining size
	shr	cx,1			;convert to word
	xor	ax,ax			;zero put
	rep	stosw			;zero the last segment

cEnd
;----------------------------------------------------------------------------;
; GetSelectorBase:							     ;
;									     ;
; Entry:     	BX    --   Selector					     ;
; Return:    CX:DX    --   32 bit base					     ;
; Destroys:     AX							     ;
;----------------------------------------------------------------------------;

GetSelectorBase proc near

	mov	dx,bx			;get the segment value
	mov	cl,4			;need to shift by 4
	shl	dx,cl			;low word of base
	xor	ax,ax			;will prepare hiword here
	mov	al,bh			;get the upper byte of seg address
	shr	al,cl			;ax has high word of base
	mov	cx,ax			;now CX:DX has the base
	ret

GetSelectorBase endp
;----------------------------------------------------------------------------;
; SetSelectorBaseLim64:							     ;
;									     ;
; Entry:     	BX    --   Selector					     ;
;            CX:DX    --   base of the selector				     ;
; Return:    selector set to have specified base and limit of 64k and access ;
;            rights of data.						     ;
; Destroys:     AX							     ;
;----------------------------------------------------------------------------;

SetSelectorBaseLim64  proc near

	pushem	cx,dx			;save
	mov	ax,cx			;have the high base word
	mov	cl,4			;need shift by 4
	shr	dx,cl			;prepare low thre nibbles
	ror	ax,cl			;get low nibble of AX into hi pos.
	or	dh,ah			;dx has the proper segment value
	mov	bx,dx			;return it in bx
	popem	cx,dx			;restore
	ret

SetSelectorBaseLim64 endp
;----------------------------------------------------------------------------;
; EmergencyExit:							     ;
;									     ;
; This routine is called to exit back to dos once windows swap in attempt    ;
; fails. In protected mode windows, we have to use a special INT 2F function ;
; to first switch to the dos extenders PSP and then exit. In real mode, wino-;
; -ld ap executes always on the kernel's PSP so we can directly do the exit. ;
;----------------------------------------------------------------------------;

EmergencyExit	proc  near

	assumes	ds,Data

; if the there is a vailid 'HighMemXmsHandle', unlock and free the xms
; block.

	mov	dx,HighMemXmsHandle	;get the handle
	or	dx,dx			;is it null ?
	jz	@f			;no xms to free

; unlock the xms block and free it.

	mov	ah,0dh			;unlock block code
	call	[lpXmsControl]		;block unlocked
	mov	ah,0ah			;free xms block code
	call	[lpXmsControl]		;all of xms allocated to win released

@@:

; now do the exit

	mov	ax,4c01h		;exit code,exit with error
	int	21h			;exit back to dos

EmergencyExit	endp
;----------------------------------------------------------------------------;
; This routine invokes the error manager.			             ;
;----------------------------------------------------------------------------;

ErrorHandler:
	
	call	ErrorManager		;this never returns

;----------------------------------------------------------------------------;

sEnd Code

end
