;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; This file has utility programs which are invoked mostly from WOAMGR.ASM    ;
;									     ;
; History:								     ;
;									     ;
;	 Fri June-15-1990.	-by-  Amit Chatterjee [amitc]		     ;
;	 Adapted for the Dos Task Switcher.				     ;
;									     ;
;        Mon June-26-1989.  	-by-  Amit Chatterjee [amitc]		     ;
;        Created for Windows. (Added the History legend)       		     ;
;----------------------------------------------------------------------------;

	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	include macros.mac
	include	njmp.mac
	include dosx.inc
	include	woapif.inc
	include woaerr.inc
	include	woaarena.inc
	include woaswch.inc
	.list

	.286p

	public	SaveWinEmsContext
	public	RestoreWinEmsContext
	public	GetSizeInBytes
	public	ConvKToBytes

;----------------------------------------------------------------------------;
; define external FAR winoldap procedures.				     ;
;----------------------------------------------------------------------------;

sBegin	Data

;----------------------------------------------------------------------------;
; define the global variables defined elsewhere				     ;
;----------------------------------------------------------------------------;

externW	ArenaWalkSel			;temp selector for walkimg arena chain
externW	ArenaRWSel			;temp sel for reading/writing arenas
externW	XmsHeapWalkSel			;scratch selector for walking xms heap
externW WoaStubSel			;sel for current loaction of stub code
externB	EmsFlag				;EMS present or not.
externB fBreak				;state of CTRL+C flag.
externW	HighMemXmsHandle		;handle of the high heap XMS block
externB	DosAppSwapFileName		;name of the dos app swap file
externB ErrorType			;save area for error code
externW WoaCsSize			;size of protected mode code seg
externW WoaDsSize			;size of winoldap data segment
externW StubSegSize			;size of real mode stub segment
externB DiskSwap1Drive			;swap drive index for fast path
externB DiskSwap2Drive			;swap drive index for slow path
externW	Swap1MinK			;space to be left on 1st drive
externW Swap2MinK			;space to be left on second drive
externW Int15UsershApp			;hApp of INT 15 user
externW hApp				;windows handle for this instance
externD XmsBankSize			;size of apps XMS bank
externW LowMemParaSize			;para size of available memory
externW LowMemArenaSel			;pointer to the available memory arena
externW LowMemSel			;sel for the start of the vail block
externW	SizeReservedArea		;size of area reserved area at heap st.
externW CurrentDosSwapSeed		;swap file seed being used now
externW	AppUsesXMS			;app used it's allocated XMS or not

ifdef	JAPAN
externD KkcDataSize			; Data area size of KKC
endif

	;-------------------------------------------------------;
	; define any locally used constants			;
	;-------------------------------------------------------;


	;-------------------------------------------------------;
	; define any locally used constants			;
	;-------------------------------------------------------;

		ZERO_BLOCK_SIZE equ 128

	;-------------------------------------------------------;
	; define any external contants.				;
	;-------------------------------------------------------;

		externB	  WoaAppSwapErr		;(WOAERR.ASM)
		externA   IS_WINOLDAP_ACTIVE	;(WOAMGR.ASM)

;---------------------------------------------------------------------------;
; now define the other global variables that will be needed.		    ;
;---------------------------------------------------------------------------;
		

;---------------------------------------------------------------------------;
; now define the other variables that will be needed.			    ;
;---------------------------------------------------------------------------;

WinEmsSaveArea		db 256 dup (0)	;save windows EMS context here
globalW AppXmsBaseSel,0			;selector to base of apps XMS block


		;-------------------------------------------;
		; Debug messages used if DEBUGMODE is set.  ;
		;-------------------------------------------;

ifdef	DEBUGMODE

endif	;DEBUGMODE

sEnd Data

;----------------------------------------------------------------------------;
; now declare the existence of the realmode stub segment and the variables   ;
; that we want to reference here.					     ;
;----------------------------------------------------------------------------;

createSeg   _WOARLMSEG,StubSeg,word,public,code
sBegin	StubSeg

sEnd	StubSeg

;----------------------------------------------------------------------------;
; we define the switch_cs macro here.					     ;
;----------------------------------------------------------------------------;

switch_cs macro
	local	_x
	local	_y
_x	proc	far
	lea	ax,_y
	push	ax
	ret
_y:
_x	endp
	endm

;----------------------------------------------------------------------------;

sBegin	Code

	assumes	cs,Code
	assumes	ds,Data
	assumes	es,nothing

	;-------------------------------------------------------;
	; external OLDAPP procedures.			        ;
	;-------------------------------------------------------;

	externNP	DeleteFile		;(WOAFILE.ASM)
	externNP	ErrorManager		;(WOAERR.ASM)
	externNP	CreateFile		;(WOAFILE.ASM)
	externNP	LseekFile		;(WOAFILE.ASM)
	externNP	OpnFile			;(WOAFILE.ASM)
	externNP	ReadFile		;(WOAFILE.ASM)
	externNP	WriteFile		;(WOAFILE.ASM)
	externNP	CloseFile		;(WOAFILE.ASM)
	externNP	DeleteFile		;(WOAFILE.ASM)
	externNP	SetNormalAttributes	;(WOAFILE.ASM)
	externNP	SetHiddenAttributes	;(WOAFILE.ASM)
	externNP	GetSelectorBase		;(WOAPMRM.ASM)
	externNP	SetSelectorBaseLim64	;(WOAPMRM.ASM)
	externNP	GetDosAppSwap1FileName	;(WOAMGR.ASM)
	externNP	GetDosAppSwap2FileName	;(WOAMGR.ASM)
	externNP	SaveFirstDosBlock	;(WOAPMRM.ASM)
	externNP	RestoreFirstDosBlock	;(WOAPMRM.ASM)
	externNP	RestoreAppXmsContext	;(WOAPMRM.ASM)
	externNP	AppendUniqueNumber	;(WOAMGR.ASM)
	externNP	ZeroOutMemory		;(WOAPMRM.ASM)
	externNP	GetSwitcherEntry	;(WOAPMRM.ASM)

;----------------------------------------------------------------------------;
; SaveDosMemory:							     ;
;									     ;
; This routine takes the name of the dos swap file as the input. It writes   ;
; the first DOS block (or a part of it in real mode) to the swap file and    ;
; then traverses memory looking for non zero values and then swaps them      ;
; out in groups.						             ;
;									     ;
; This routine also takes an OpCode, SDMOpCode which if 0 returns just the   ;
; size needed to save the memory and if non zero actually saves the memory.  ;
; If the OpCode is 0, the first parameter is insignificant.		     ;
;----------------------------------------------------------------------------;

cProc	SaveDosMemory,<NEAR,PUBLIC,PASCAL>

	parmD	lpFileName		;name of the swap file
	parmB	SDMOpCode		;save or get size 

	localW	FileHandle		;handle of the swap file
	localV	NodeHeader,8		;DWORDs of base and lock size
	localD	CurrentSize		;size of the node being prepared
	localD	CurrentBase		;base of the node being prepared
	localD	SizeLeft		;amount of memory left
	localD	SDMSize			;size of save area

cBegin

	cld				;do not take chances with this

; Initialize the SDMSize field

	mov	seg_SDMSize,0
	mov	off_SDMSize,4		;4 bytes for XMS link

; create the swap file, at this point the file should not be existing, but
; we do not care to check.  Ifthe OpCode is for get size only, we will not
; try to create any file.

	cmp	SDMOpCode,0		;get size only
	jz	SDMByPassIO1		;yes
	xor	ax,ax			;want to create a normal file
	cCall	CreateFile,<lpFileName,ax>
	jc	SaveDosMemoryRet	;cannot proceed with error.
	mov	FileHandle,ax		;save the handle of the swap file

; we need to leave 4 bytes at the start of the file for storing the offset 
; of the start of the XMS swap out area. This is actually needed only in 
; real mode winoldap where the XMS memory has to be swapped in first.

	xor	ax,ax			;need zeros for hiword of offset/org.

ifdef	JAPAN
	mov	cx,8			;offset for seek
else
	mov	cx,4			;offset for seek
endif

	cCall	LseekFile,<FileHandle,ax,cx,ax>

SDMByPassIO1:

; we will save the first block arena selector separately and then skip over
; the _DATA and _TEXT segments. The following routine will do that, it will
; return the size of the block skipped over (top PDB, _DATA & _TEXT) in AX
; and the size wriiten to file (actuall 16) in CX:DX.

	cCall	SaveFirstDosBlock,<FileHandle,SDMOpCode>;saves the block containing woa stub
	jc	SaveDosMemoryRet	;cannot proceed with error.

; AX has the para size of the first block. CX:DX has the size wriiten out.
; Calculate the amount of memory left to be swapped.

	add	off_SDMSize,dx		;add in size wriitent out
	adc	seg_SDMSize,cx		;update hiword

	sub	ax,LowMemParaSize	;get difference
	neg	ax			;convert to positive
	call	GetSizeInBytes		;AX:BX has the size
	mov	seg_SizeLeft,ax 	;save hiword
	mov	off_SizeLeft,bx 	;save loword

; SetArenaWalkSel to the start of the block from where we want to swap on 
; (_WOARLMSEG) 
	       
	mov	ax,_WOARLMSEG		;want to swap from here
	mov	ArenaWalkSel,ax		;save it

; call the generalized routine to swap out the non zero blocks

	cCall	SwapOutNonZeroBlocks,<FileHandle,ArenaWalkSel,SizeLeft,SDMOpCode>
	jc	SaveDosMemoryRet	;cannot proceed with error.

; update the size and add 8 for the following node too. The above routine 
; would have returned the size written out in DX:AX

	add	ax,8			;for the terminating header
	adc	dx,0			;update hiword
	add	off_SDMSize,ax		;update total size
	adc	seg_SDMSize,dx		;update hiword

	cmp	SDMOpCode,0		;get size only ?
	jz	SaveDosMemoryRet	;yes.

; need to write out a dummy header to mark the end of the memory image area

	smov	es,ss			;need to access header structure
	lea	di,NodeHeader		;es:di points to the node header area
	push	di			;save the offset
	xor	ax,ax			;need to zero it out
	mov	cx,4			;size of header	in words
	rep	stosw			;initialize it
	pop	di			;es:di points to the NodeHeader
	mov	cx,8			;8 bytes of header to write
	xor	dx,dx			;hiword of number of bytes to write
	cCall	WriteFile,<FileHandle,es,di,dx,cx>

SaveDosMemoryRet:

	mov	ax,FileHandle		;need to return the handle of the file

; if the SDMOpCode is zero, return with the size in DX:AX.

	pushf				;save state of carry flag
	cmp	SDMOpCode,0		;get size only ?
	jnz	@f			;no.
	mov	dx,seg_SDMSize		;get hiword of size
	mov	ax,off_SDMSize		;get low eord of size
@@:
	popf				;restore carry flag (success or failure code)

cEnd
;----------------------------------------------------------------------------;
; SwapOutNonZeroBlocks:							     ;
; 									     ;
; This routine takes the following parameters:				     ;
;	 								     ;
;	FileHandle	---	Data will be written into this file	     ;
;	Start sel	---	Swap out area starts here (offset 0)	     ;
;	Size		---	Total size of the area in bytes (dword)	     ;
;       SONZBOpCode	---     If 0 will just get the swap area size.
;									     ;
; It scans the range specified for non zero blocks of zeros and writes out   ;
; swap groups (block preeceded by 8 byte header) to the file. It returns     ;
; with carry set if the writes failed.					     ;
;----------------------------------------------------------------------------;

cProc	SwapOutNonZeroBlocks,<NEAR,PUBLIC,PASCAL>,<es>

	parmW	FHandle			;handle for the file
	parmW	StartSel		;start selector
	parmD	SLeft			;size for the block
	parmB	SONZBOpCode		;get size only or not.

	localV	NHeader,8		;DWORDs of base and lock size
	localD	CSize			;current swap group size
	localD  CBase			;current swap group base
	localW	LoopSize		;size being tested in the loop
	localW	RestartOffset		;restart point
	localD	SONZBSize		;size of the swap area.

cBegin

; initialize the swap area size variable.

	mov	seg_SONZBSize,0
	mov	off_SONZBSize,0

; initialize the base and size for the next swap group

	mov	bx,StartSel		;get the selector
	mov	ArenaWalkSel,bx		;save it
	call	GetSelectorBase		;get the base in cx:dx

	mov	seg_CSize,0		;initialize current size of swap node
	mov	off_CSize,0		;initialize 32 bits
	mov	seg_CBase,cx		;save high word of current base
	mov	off_CBase,dx		;save low word of current base

ScanForZerosLoop:

	mov	es,ArenaWalkSel		;get the selector into es
	mov	ax,seg_SLeft		;are we all done ?
	or	ax,off_SLeft		
	njz	WriteSwapGroup		;write the group and end

; need to scan the segment for zeros.

	mov	cx,8000h		;assume complete segment (in words)
	cmp	seg_SLeft,0		;are we in the last 64k seg ?
	jnz	start_check_for_0	;no.
	mov	cx,off_SLeft		;get the size left to look.
	shr	cx,1			;convert to words

start_check_for_0:

	mov	LoopSize,cx		;save the size in words that we test

; scan for zeros in this segment.

	xor	ax,ax			;looking for zeros.
	xor	di,di			;start at top of segment.

continue_check_for_zero:

	repne	scasw			;look for zero.
	jcxz	go_to_next_seg		;no zeros found.

; the zero stretch should atleast be ZERO_STRETCH_SIZE long.

	inc	cx			;ignore the match.
	sub	di,2			;start of the zeros.
	mov	bx,di			;save

; look ahead ZERO_STRTECH_SIZE  or to the end of segment which ever is smaller

	pushem	ax,bx
	mov	ax,cx			;words left in the segment
	shl	ax,1			;convert to bytes
	mov	bx,ZERO_BLOCK_SIZE	;min zero block size
	jc	enough_in_segment	;complete segment is left
	cmp	ax,bx			;which is smaller ?
	jbe	look_ahead_size_obt	;ax has look ahead size

enough_in_segment:

	mov	ax,bx			;bx is min

look_ahead_size_obt:

	mov	dx,ax			;have it in dx
	popem	ax,bx

; from di onwards we want to have a block of DX size as 0s

	add	di,dx			;look ahead
	jc	scan_block_for_all_0s	;goes into next seg, see if its all 0
	cmp	byte ptr es:[di],0	;is it still 0 ?
	jz	scan_block_for_all_0s	;possibly 0 stretch obtained.
	sub	di,dx			;take it back to start of block

short_zero_block_size:

	add	di,dx			;update past the short block
	shr	dx,1			;convert to words
	sub	cx,dx			;update count left in segment
	jmp	continue_check_for_zero	;keep looking ahead

scan_block_for_all_0s:

	mov	di,bx			;go back to start of block
	pushem	cx,di			;save
	mov	cx,dx			;block size to look for
	shr	cx,1			;convert size to words.
	repe	scasw			;look for non zero.
	or	cx,cx			;all zeros ?
	popem	cx,di			;retrieve cx
	jnz	short_zero_block_size	;no, zero block not of min size
	add	di,dx			;past the zero block
	jc	go_to_next_seg		;forget about these zeros.
	and	di,0fff0h		;take it back to para start.
	mov	RestartOffset,di	;save next start scan point

; we have got a zero block of atleast the minimum size.

	
	mov	di,bx			;get the start of the block.
	add	di,15			;want to align to the next para
	and	di,0fff0h		;para aligned.
	jmp	ZeroBlockFound		;have got a block of zeros.

go_to_next_seg:

; update size left to be scanned.

	mov	cx,LoopSize		;get the size
	shl	cx,1			;get byte size
	sbb	seg_SLeft,0		;update hiword
	sub	off_SLeft,cx		;subtract from low word
	sbb	seg_SLeft,0		;update hiword

; update the sizes left and the current size.

	mov	cx,LoopSize		;get the size that we inspected
	shl	cx,1			;convert to bytes.
	adc	seg_CSize,0		;add cary to hiword
	add	off_CSize,cx		;add loword
	adc	seg_CSize,0		;add cary to hiword

; update to the next segment.

	mov	bx,ArenaWalkSel		;get the current selector
	call	GetSelectorBase		;get the base
	inc	cx			;next segment
	call	SetSelectorBaseLim64	;update to next seg
	mov	ArenaWalkSel,bx		;save
	jmp	ScanForZerosLoop	;continue.

ZeroBlockFound:

; update the sizes for the current swap group and the amount left to look

	add	off_CSize,di		;include till the zeros
	adc	seg_CSize,0		;update high word

	mov	di,RestartOffset	;we have basically scanned this much
	sub	off_SLeft,di		;amount left
	sbb	seg_SLeft,0

WriteSwapGroup:

	mov	bx,ArenaRWSel		;load the temp selector	for read/write
	mov	cx,seg_CBase		;load hiword of base of block
	mov	dx,off_CBase		;load the loword of the base
	call	SetSelectorBaseLim64	;point the selector to the base
	mov	ArenaRWSel,bx		;save the modified selector

; update the size that is being written.

	mov	dx,seg_CSize		;get the hiword
	mov	ax,off_CSize		;offset
	add	ax,8			;for the node header
	adc	dx,0			;update hiword
	add	off_SONZBSize,ax	;update loword of total size
	adc	seg_SONZBSize,dx	;update hiword.

; if the opcode is for get size only, then bypass the write.

	cmp	SONZBOpCode,0		;get size only ?
	jz	SONZBByPassIO1		;yes.
	
; write the header for the node, which consists of a dword of the value of the
; base of the block followed by a dword of the size of the block

	smov	es,ss			;need to access NHeader in stack
	lea	di,NHeader		;es:di points to the node header
	push	di			;save offset
	mov	ax,off_CBase		;get the low word of base
	stosw				;save it
	mov	ax,seg_CBase		;get the high word of base
	stosw				;save it
	mov	ax,off_CSize		;get the low word of size
	stosw				;save it
	mov	ax,seg_CSize		;get the high word of size
	stosw				;save it
	pop	di			;es:di points to the NHeader
	mov	cx,8			;8 bytes of header to write
	xor	dx,dx			;hiword of number of bytes to write
	cCall	WriteFile,<FHandle,es,di,dx,cx>
	njc	SwapNonZeroBlocksRet	;cannot proceed with error.

; now write out the allocated block contents.

	xor	ax,ax			;need for the start offset
	cCall	WriteFile,<FHandle,ArenaRWSel,ax,CSize>
	njc	SwapNonZeroBlocksRet	;cannot proceed with error.

SONZBByPassIO1:

; if we are done, we should write the end header and leave.

	mov	ax,seg_SLeft		;get the size left 
	or	ax,off_SLeft		;is it 0 ?
	njz	SwapNonZeroBlocksRet	;yes, done with the swap

ScanForNonZerosloop:

; if the RestartOffset is 0, we should go to the next segment.

	cmp	RestartOffset,0		;is it 0
	jnz	restart_in_same_seg	;no, we will restart in same seg.

; update to next segment.

	mov	bx,ArenaWalkSel		;get the current selector
	call	GetSelectorBase		;get the base
	inc	cx			;next segment
	call	SetSelectorBaseLim64	;update to next seg
	mov	ArenaWalkSel,bx		;save

restart_in_same_seg:

	mov	es,ArenaWalkSel		;get the selector into es
	mov	ax,seg_SLeft		;are we all done ?
	or	ax,off_SLeft		
	njz	SwapNonZeroBlocksRet	;yes, done with the swap

; get the min of (seg size in words, amount left in this segment , amount
;  left altogether).

	mov	ax,8000h		;seg size in words
	cmp	seg_SLeft,0		;last segment ?
	jnz	@f			;no.
	push	bx
	mov	ax, off_SLeft
	mov	bx, RestartOffset
	neg	bx
	cmp	ax, bx			;
	jbe	ROWontOverShoot		;S_Left is smaller
	or	bx,bx			;maybe bx is zero
	jz	ROWontOverShoot

; we have to ensure that di which will get the value of RestartOffset
; later, does not cros 0ffffh. If ax > bx this could happen.

	mov	ax,bx			;bx is non zero AND less than ax
ROWontOverShoot:
	pop	bx
	shr	ax,1			;convert to words
	jmp	short start_check_for_non_0	
@@:
	cmp	RestartOffset,0		;is it at start of segment ?
	jz	start_check_for_non_0	;yes
	mov	ax,RestartOffset	;get the start
	neg	ax			;amount left in seg
	shr	ax,1			;convert to words

start_check_for_non_0:

	mov	cx,ax			;get scan size in segment in words

; scan for non zeros in this segment.

	xor	ax,ax			;looking for zeros.
	mov	di,RestartOffset	;start from this point
	repe	scasw			;look for zero.
	or	cx,cx			;0?
	jnz	non_zero_found		;no.

; update size left.
	
	push	di			;save
	sub	di,RestartOffset	;amount scanned 
	sub	off_SLeft,di		;subtract
	sbb	seg_SLeft,0		;hiword.
	pop	di

; if both RestartOffset and current DI are 0 we have skipped a complete 
; segment.

	push	di			;save
	or	di,RestartOffset	;skipped one complete segment ?
	pop	di			;restore di
	mov	RestartOffset,0		;next scan from here.	
	jnz	ScanForNonZerosLoop	;no.
	dec	seg_SLeft		;0=>64k
	jmp	ScanForNonZerosLoop	;continue

non_zero_found:

; this segment has some non zero bytes. Start swapping from the previous 
; paragraph though.

	sub	di,2			;go back to start of non zero stretch
	and	di,0fff0h		;go back to start of para

; update the size left.

	mov	bx,di			;get the current offset
	sub	bx,RestartOffset	;place we started from
	sub	off_SLeft,bx		;account for the skipped part.
	sbb	seg_SLeft,0		;update hiword.
	mov	bx,es			;get the current selector
	call	GetSelectorBase		;get the base
	add	dx,di			;update to the start of non 0 block
	adc	cx,0			;update hiword.

; update vars for the next swap group.

	mov	seg_CSize,0		;initialize current size of swap node
	mov	off_CSize,0		;initialize 32 bits
	mov	seg_CBase,cx		;save high word of current base
	mov	off_CBase,dx		;save low word of current base
	call	SetSelectorBaseLim64	;set selector to point to next base
	mov	ArenaWalkSel,bx		;update
	jmp	ScanForZerosLoop	;continue.

SwapNonZeroBlocksRet:

; if the opcode was for get size only, return with the size in DX:AX

	pushf				;save carry flag state
	cmp	SONZBOpCode,0		;get size on;y ?
	jnz	@f			;no.
	mov	dx,seg_SONZBSize	;get hiword of swap size
	mov	ax,off_SONZBSize	;get low word of swap size
@@:
	popf				;restore state of carry flag

cEnd
;----------------------------------------------------------------------------;
; GetBackAppXmsNeeds:							     ;
;									     ;
; This routine looks at the dos app swap file to locate the size of the XMS  ;
; block that the app needs and saves it in 'XmsBankSize'		     ;
;----------------------------------------------------------------------------;

cProc	GetBackAppXmsNeeds,<NEAR,PUBLIC,PASCAL>

	localD	DwordBuffer		;can read in 4 bytes here
	localW	FileHandle		;save file handle here

cBegin

	cld				;do not take chances with this
	mov	ErrorType,ER_APP_SWAP_IN;in case file can't be found
	mov	si,DataOFFSET DosAppSwapFileName
	cCall	SetNormalAttributes,<ds,si>
	mov	ax,2			;open a normal file
	cCall	OpnFile,<ds,si,ax>	;try to open the file
	njc	ErrorHandler		;cannot	swap in
	mov	FileHandle,ax		;save the handle

; now read in the offset to the start of the xms swap area

	lea	ax,DwordBuffer		;read in offset into ss:ax
	xor	cx,cx			;hiword of count of 4 bytes
	mov	bx,4			;need to read in 4 bytes
	cCall	ReadFile,<FileHandle,ss,ax,cx,bx>
	njc	ErrorHandler		;file too small,error

; now seek to the offset that has just been read in

	xor	ax,ax			;need to seek from the start of the file
	cCall	LseekFile,<FileHandle,DwordBuffer,ax>
	njc	ErrorHandler		;file too small,error

; read in the 4 byte XMS memory size

	lea	di,DwordBuffer		;ss:di points to buffer
	xor	ax,ax			;hiword of count is 0
	mov	bx,4			;need to read 4 bytes (loword of count)
	cCall	ReadFile,<FileHandle,ss,di,ax,bx>
	njc	ErrorHandler		;error in reading in

; save the file size in 'XmsBankSize' global variable.

	mov	ax,seg_DwordBuffer	;get the hiword of size
	mov	wptr [XmsBankSize+2],ax	;save it
	mov	ax,off_DwordBuffer	;get the loword of size
	mov	wptr [XmsBankSize],ax	;save it

; return with the handle of the file

	mov	ax,FileHandle		;handle of dos file

cEnd
;----------------------------------------------------------------------------;
; RestoreXmsAndDosAllocatedBlocks:	  				     ;
;									     ;
; This routine takes a long pointer to a swap file name and read in the dos  ;
; memory allocation image that belongs to the old app. The swap file is      ;
; discussed in the header of the routine above.				     ;
;----------------------------------------------------------------------------;

cProc	RestoreXmsAndDosAllocatedBlocks,<NEAR,PUBLIC,PASCAL>

	parmD	lpFileName		;the name of the swap file

	localW	FileHandle		;handle of the swap file
	localD	FileOffset		;place to read in 4 byte offset
	localV	NodeHeader,8		;will read in the node header here

cBegin

	cld				;do not take chances with this

; try to open the swap file. Set normal attributes for the file.

	cCall	SetNormalAttributes,<lpFileName>
	mov	ax,2			;want to open a read only file
	cCall	OpnFile,<lpFileName,ax>	;try to open the file
	jc	RestoreDosAllocatedBlocksRet;cannot proceed with error.
	mov	FileHandle,ax		;save the handle of the swap file

; we first need to restore the XMS context first, so get the offset of the
; start of the XMS area (saved as the first 4 bytes of the file).

	lea	ax,FileOffset		;read in offset into ss:ax
	xor	cx,cx			;hiword of count of 4 bytes
	mov	bx,4			;need to read in 4 bytes
	cCall	ReadFile,<FileHandle,ss,ax,cx,bx>
	jc	RestoreDosAllocatedBlocksRet	

; now seek to the offset that has just been read in

	xor	ax,ax			;need to seek from the start of the file
	cCall	LseekFile,<FileHandle,FileOffset,ax>

; now read in the XMS context. if the app needs XMS memory,we must swap out the 
; block into the XMS file before loading applications XMS blocks

	cCall	RestoreAppXmsContext,<FileHandle>;restore the XMS context
	jc	RestoreDosAllocatedBlocksRet;cannot proceed with error.

; now seek back to the start of the file past the 4 byte offset area for reading
; in the image of the dos app.

	xor	ax,ax			;need zeros for hiword of offset/org.

ifdef	JAPAN
	mov	cx,8			;offset for seek
else
	mov	cx,4			;offset for seek
endif

	cCall	LseekFile,<FileHandle,ax,cx,ax>

; zero out the area where we will read apps context in.	We must zero out 
; from the start of the StubSeg onwards.


	mov	ax,_WOARLMSEG		;start of stub segment
	sub	ax,LowMemSel		;paragraphs at the begining
	cCall	ZeroOutMemory,<LowMemSel,LowMemParaSize,ax>

; now read in the first allocated block. In protected mode the complete block
; had been saved and the size of the block can be obtained after reading in
; the arena. In real mode, the portion of memory right after the arena which
; holds the top pdb and main code and data segments had not been saved and 
; should be skipped during the swap back

  	cCall	RestoreFirstDosBlock,<FileHandle>
	jc	RestoreDosAllocatedBlocksRet;cannot proceed with error.

; now get into a loop reading in header for every node and reading the 
; associated memory image into the right space, till we get a node header
; with a base of zero.

	cCall	RestoreSwappedGroups,<FileHandle>
	mov	ax,FileHandle		;must return with file handle

RestoreDosAllocatedBlocksRet:

cEnd
;----------------------------------------------------------------------------;
; RestoreSwappedGroups:							     ;
;									     ;
; Takes a handle for a swap file as a parameter and reads back swapped out   ;
; groups from the file. The structure of each swap group is a dword of the   ;
; base of the memory where the block is to be read back, a dword of size of  ;
; the block to read in, followed by the image of the block itself. The swap  ;
; groups are terminated by a node header where the base is 0.		     ;
;----------------------------------------------------------------------------;

cProc	RestoreSwappedGroups,<NEAR,PUBLIC,PASCAL>

	parmW	FileHandle		;handle of the swap file

	localV	NodeHeader,8		;will read in the node header here

cBegin

	cld				;do not take chances with this

ReadBlockLoop:

; read in the next node header.

	smov	es,ss			;fill read the header into the stack
	lea	di,NodeHeader		;es:di points to read buffer
	mov	cx,8			;need to read 8 bytes in
	xor	ax,ax			;hiword of count is 0
	pushem	es,di			;save the pointer
	cCall	ReadFile,<FileHandle,es,di,ax,cx>
	popem	es,di			;restore the pointer
	jc	RestoreSwappedGroupsRet ;cannot proceed with error.

; if the base of the block is 0, we are done

	mov	ax,es:[di]		;get the low word of base
	or	ax,es:[di+2]		;the high word of base
	jz	ReadBlockDone		;we have read back  the complate image

; prepare a word selector for reading back the block and read it back.

	mov	cx,es:[di+2]		;get the hiword of the base
	mov	dx,es:[di]		;get the low word of the base
	mov	bx,ArenaRWSel		;load the temp selector
	cCall	SetSelectorBaseLim64	;set up with the proper base
	mov	ArenaRWSel,bx		;save the modified value
	mov	cx,es:[di+6]		;get the high word of count
	mov	dx,es:[di+4]		;get the low word of count
	xor	ax,ax			;need zero for the start offset
	cCall	ReadFile,<FileHandle,ArenaRWSel,ax,cx,dx>
	jc	RestoreSwappedGroupsRet ;cannot proceed with error.

; now read back the other blocks.

	jmp	short ReadBlockLoop	;read in the resto of the blocks

ReadBlockDone:

	clc				;successful read back done

RestoreSwappedGroupsRet:

cEnd
;----------------------------------------------------------------------------;
; GetSizeInBytes:							     ;
;									     ;
; On entry AX has the size of an object in paragraphs, this routine returns  ;
; the size in bytes in AX:BX						     ;
;----------------------------------------------------------------------------;

GetSizeInBytes	proc near

	xor	bh,bh			;hibyte of hiword will be zero
	mov	bl,ah			;need the high nibble
	shiftr	bl,4			;move it to bit positions 16-19
	shiftl	ax,4			;effective mul by 16
	xchg	ax,bx			;ax:bx has the size in bytes
	ret

GetSizeInBytes	endp
;----------------------------------------------------------------------------;
; ConvKToBytes:							  	     ;
;									     ;
; On entry AX has the size of an object in KBytes and this routine returns   ;
; the size in bytes in AX:BX.						     ;
;----------------------------------------------------------------------------;

ConvKToBytes proc near

	push	dx			;save
	xor	dx,dx			;for the mul
	mov	bx,1024			;mult fctor
	mul	bx			;dx:ax has the result
	xchg	ax,dx			;ax:dx has the result
	mov	bx,dx			;ax:bx has the result
	pop	dx			;restore
	ret

ConvKToBytes endp
;----------------------------------------------------------------------------;
; This routine save the state of the EMS page mapping registers if EMS driver;
; is present.							             ;
;----------------------------------------------------------------------------;

SaveWinEmsContext proc near

	cmp	EmsFlag,0ffh		;is the EMS driver present ?
	jnz	@f			;no, nothing to save
	mov	di,DataOFFSET WinEmsSaveArea
	mov	ax,4e00h		;want to get the page mapping resgisters
	smov	es,ds			;ES:DI points to the save area
	int	67h			;page map resgisters obtained
@@:
	ret

SaveWinEmsContext  endp
;----------------------------------------------------------------------------;
; This routine restores the previously saved state of the Ems page mapping   ;
; registers, if the EMS drivers is present.			    	     ;
;----------------------------------------------------------------------------;

RestoreWinEmsContext  proc near

	cmp	EmsFlag,0ffh		;is the EMS driver present ?
	jnz	@f			;no, nothing to restore
	mov	si,DataOFFSET WinEmsSaveArea
	mov	ax,4e01h		;set map resgister code
	int	67h			;the mapper resgisters reprogrammed
@@:
	ret	

RestoreWinEmsContext  endp
;----------------------------------------------------------------------------;
; WhichSwapPathToUse:							     ;
;									     ;
; This routine finds out the amount of space that will be needed for swapping;
; out the dos app's context and decides which swap path it should use.       ;
;									     ;
; It will will up the 'DosAppSwapFileName' buffer appropriately, but will    ;
; return with carry if no space is available in either places. It wil also   ;
; update the global structure for the top entry to reflect what path it is   ;
; using.								     ;
;----------------------------------------------------------------------------;

cProc	WhichSwapPathToUse,<NEAR,PUBLIC,PASCAL>

	localD	WSPSize			;size of swap area

cBegin

; initialize the variable to hold the size of the swap area.

	mov	off_WSPSize,1024	;size of IDT area
	mov	seg_WSPSize,0		;reset high word

; find out the size of the conventional memory context.

	xor	ax,ax			;get size only
	cCall	SaveDosMemory,<ax,ax,ax>;1st two parameters insignificant
	add	off_WSPSize,ax		;add low word of size
	adc	seg_WSPSize,dx		;update high word

; Add in the size of the XMS Context. We will have to consider the complete
; XMS size as we have no way of knowing what the actual size will be, at this
; point.  However if 'AppUsesXMS' is 0, we will not swap the XMS out and thus
; we will not consider it's size

	cmp	AppUsesXMS,0		;did it use XMS ?
	jz	@f			;no.
	mov	ax,wptr [XmsBankSize]	;get low word of size
	mov	dx,wptr [XmsBankSize+2]	;get high word of size
	add	off_WSPSize,ax		;add low word of size
	adc	seg_WSPSize,dx		;update high word
@@:

ifdef	JAPAN
	mov	ax,wptr [KkcDataSize]	; get data size of KKC
	mov	dx,wptr [KkcDataSize+2]	;
	add	off_WSPSize,ax		;add low word of size
	adc	seg_WSPSize,dx		;update high word
endif

; find out if there is enough disk space on the first drive.

	mov	cx,seg_WSPSize		;get low word of size
	mov	dx,off_WSPSize		;cx:dx has swap area size
	mov	ax,Swap1MinK		;min area to be left
	call	ConvKToBytes		;ax:bx has size of space to leave
	add	dx,bx			;add in low word
	adc	cx,ax			;cx:ax = desired space
	cCall	IsEnoughDiskSpace,<DiskSwap1Drive,cx,dx>
	jc	TryDisk2		;not enough on disk 1
	call	GetDosAppSwap1FileName	;fill up file name
	mov	bl,1			;1st path to be used
	jmp	short UpdatePathInfo	;update global block

TryDisk2:

	mov	cx,seg_WSPSize		;get low word of size
	mov	dx,off_WSPSize		;cx:dx has swap area size
	mov	ax,Swap2MinK		;min area to be left
	call	ConvKToBytes		;ax:bx has size of space to leave
	add	dx,bx			;add in low word
	adc	cx,ax			;cx:ax = desired space
	cCall	IsEnoughDiskSpace,<DiskSwap2Drive,cx,dx>
	jc	WhichSwapPathToUseRet	;no space on any drive
	call	GetDosAppSwap2FileName	;fill up file name
	mov	bl,2			;second path to be used

UpdatePathInfo:

	pushem	es,si			;save
	push	bx			;save
	cCall	GetSwitcherEntry,<CurrentDosSwapSeed>
	jc	WhichSwapPathToUseRet	;error, can't find node:can't swapout
	mov	es,dx			;ignore logical node num in bx
	mov	si,ax			;es:si points to the desired entry
	pop	bx			;get back path id
	mov	es:[si].Path_Id,bl	;save path code
	popem	es,si			;restore
	clc				;no error

WhichSwapPathToUseRet:

cEnd
;----------------------------------------------------------------------------;
; IsEnoughDiskSpace:							     ;
;									     ;
; This routine checks to see if the amount of bytes of disk space left is    ;
; enough to swap out the dword of bytes that is passed in as a parameter.    ;
; Carry is set if there is not enough free space else it is set.	     ;
;----------------------------------------------------------------------------;

cProc	IsEnoughDiskSpace,<NEAR,PASCAL,PUBLIC>
	
	parmB	IEDDriveId		;drive ID
	parmD	SpaceNeeded		;amount of space needed

cBegin

; get the amount of disk space left.

	mov	ah,36h			;get free space call
	mov	dl,IEDDriveId		;get the swap drive number
	sub	dl,40h			;A=1,B=2 etc
	int	21h			;get various parameters
	xor	dx,dx			;need for multiplication

; AX*BX*CX is the no of bytes of free space.

	mul	bx			;DX:AX has AX*BX
	or	dx,dx			;is dx already 1 ?
	jnz	IsEnoughDiskSpaceRet	;will be enough space
	mul	cx			;DX:AX has amount of free space

; check to see if this is enough

	cmp	dx,seg_SpaceNeeded	;compare hiwords
	jne	IsEnoughDiskSpaceRet	;carry set or unset for result
	cmp	ax,off_SpaceNeeded	;compare low words

; carry set if not enough space or cleared if enough space

IsEnoughDiskSpaceRet:

cEnd
;----------------------------------------------------------------------------;
; AppSwapOutErrHandler:							     ;
;									     ;
; This routine is called when an application swap out attempt fails. The     ;
; screen had been put into a text mode by a previous grabber call.  This     ;
; routine displays an error msg across the top of the screen and waits	     ;
; for a key to be hit.							     ;
;----------------------------------------------------------------------------;

cProc	AppSwapOutErrHandler,<NEAR,PUBLIC,PASCAL>

cBegin

; if a partial swap file had been created for the application, delete it.

	mov	si,DataOFFSET DosAppSwapFileName;ds:si points to the name
	mov	dx,si			;ds:dx has the pointer now.
	mov	ah,41h			;delete file code
	int	21h			;delete the file

; print the error message. First home the cursor.

	mov	ah,0Fh			;get & set the current mode
	int	10h			;  so that the screen is clear
	xor	ah,ah			;  and the cursor is homed
	int	10h			;  (works for mode 3 & 7)

	mov	dx,DataOFFSET WoaAppSwapErr;the error message
	mov	ah,09h			;display string code
	int	21h			

; now remove the cursor from the screen.

	mov	ah,2			;position cursor
	xor	bh,bh			;page 0
	mov	dx,1900h		;position to 25th line
	int	10h			;cursor removed.

; now wait for a key to be hit.

	mov	ax,0C07h		;flush kbd buffer & do a raw read
	int	21h
		
cEnd
;----------------------------------------------------------------------------;
; ResetClassInt15Word:							     ;
;									     ;
; This takes an  app ID as the parameter and if the INT 15 word in the global;
; structure is the same as the handle then it resets the word.   	     ;
;									     ;
; This routine must also reset the 'Int15UsershApp' variable if the handle   ;
; is the same.							             ;
;----------------------------------------------------------------------------;

cProc	ResetClassInt15Word,<NEAR,PUBLIC,PASCAL>

	parmW	hAppId			;hApp passed in

cBegin

; get the current word.

	cCall	GetInt15Word		;get the current ID
	cmp	ax,hAppId		;does it compare ?
	jnz	@f			;no.
	xor	ax,ax			;need to set it to 0
	mov	Int15UsershApp,ax	;reset the ID
	cCall	SetInt15Word,<ax>	;reset the word
@@:
cEnd
;----------------------------------------------------------------------------;
; GetInt15Word:								     ;
;									     ;
; This routine gets the current value of the INT 15 Users ID saved in the    ;
; global switch structure.						     ;
;----------------------------------------------------------------------------;

cProc	GetInt15Word,<NEAR,PUBLIC,PASCAL>,<es,di>

cBegin

	mov	ax,4a05h		;opcode
	mov	si,CGET_GLOBAL_SWITCH_DATA
	int	2fh			;dx:ax has the long pointer
	mov	es,dx			;load it into es
	mov	di,ax			;es:di -> info structure
	mov	ax,es:[di].Int_15_Users_Id;get the ID

cEnd
;----------------------------------------------------------------------------;
; SetInt15Word:								     ;
;									     ;
; This routine sets the current value of the INT 15 Users ID saved in the    ;
; global switch structure to the passed in parameter value.  		     ;
;----------------------------------------------------------------------------;

cProc	SetInt15Word,<NEAR,PUBLIC,PASCAL>,<es,di>

	parmW	Value			;value of the word

cBegin

	mov	ax,4a05h		;opcode
	mov	si,CGET_GLOBAL_SWITCH_DATA
	int	2fh			;dx:ax has the long pointer
	mov	es,dx			;load it into es
	mov	di,ax			;es:di -> info structure
	mov	ax,Value		;get the new value
	mov	es:[di].Int_15_Users_Id,ax ;set it

cEnd
;----------------------------------------------------------------------------;
; GetNSetCtrlCFlag:							     ;
;									     ;
; This routine gets the current state of the CTRL+C flag and sets it off     ;
; after saving the state. The state would be restored in the stub segment    ;
; and also before the next EnableDos call.				     ;
;									     ;
; Entry:								     ;
; 	 DS - Winoldap's Data segment.					     ;
;             No other registers relevant.				     ;
;----------------------------------------------------------------------------;

cProc	GetNSetCtrlCFlag,<NEAR,PUBLIC,PASCAL>	

cBegin

; flush the keyboard buffer

	mov	ax,0c00h		;flush buffer code
	int 	21h

; get the current state	of the CtrlC flag.

	mov	ax,3300h		;get CTRL+C flag
	int	21h			;state in DL.
	mov	fBreak,dl		;save it

; now set the state off.

	
	mov	ax,3301h		;set the blag
	xor	dl,dl			;to off
	int	21h			;flag set off

cEnd
;----------------------------------------------------------------------------;
; RestoreCtrlCFlag:							     ;
;									     ;
; Restores the state of the CTRL+C flag that had been saved before.	     ;
;									     ;
; Entry:								     ;
; 	 DS - Winoldap's Data segment.					     ;
;             No other registers relevant.				     ;
;----------------------------------------------------------------------------;

cProc	RestoreCtrlCFlag,<NEAR,PUBLIC,PASCAL>

cBegin

; restore the saved state

	mov	ax,3301h		;set the blag
	mov	dl,fBreak		;get the saved state.
	int	21h			;flag restored

cEnd
;----------------------------------------------------------------------------;
; This routine invokes the error manager.			             ;
;----------------------------------------------------------------------------;

ErrorHandler:
	
	call	ErrorManager		;this never returns

;----------------------------------------------------------------------------;

sEnd Code

end
