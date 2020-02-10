.386p
	page 58,132
;=============================================================================
	title	A L L O C M E M - allocates memory above 1MB for CEMM
;=============================================================================
;==
;== (C) Copyright MICROSOFT Corp. 1989-1991
;== (C) Copyright COMPAQ Computer Corp. 1989-1991
;==
;==	Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;==
;==	Module: AllocMem - Extended memory allocation routines for EMS pool
;==			   and work space.
;==
;==	Version: 1.00
;==
;==	Date:	August 22,1989
;==
;==	Author: Leo Cohen
;==
;=============================================================================
;==
;==	Change Log:
;==
;==	DATE	 REVISION	Description
;==	-------- --------	--------------------------------------------
;==     08/22/89 0.00	        Original
;==
;=============================================================================
;==
;==   Functional Description:
;==
;==   This module was designed to buffer CEMM.EXE from the different allocation
;==   methods which exist for obtaining memory above 1MB.  Three modules
;==   interface to CEMM.EXE:
;==
;==   MemInit: This subroutine is used to initialize data structures which
;==	       will be used by the MemGet routine.  It creates a linked list
;==	       of all available memory above 1 meg in the system (free memory
;==	       list).  It returns in EAX a zero if no free memory is available
;==	       or the top of physical memory if non-zero.
;==
;==   MemGet:  This subroutine is called by CEMM.EXE when memory above 1MB
;==	       is required.  It searches thru the free memory list to find
;== 	       a contiguous memory range to give back.  If a contiguous memory
;==	       range is not found, it will return the largest one found.
;==
;==   MemExit: This subroutine will terminate the allocation process.  It will
;==	       return all free memory and allocate all used memory.  Then it
;==	       will restore all entries to the null list.  It must be
;==	       called before any other program allocates memory.
;==
;==
;==   This may be an overkill, but future PCs may have up to 4GBs of memory,
;==   which may be fragmented throughout the 4GB address space.  Also, bus
;==   master/DMA devices may cause nightmarish boundary restrictions on buffers.
;==   Thus, the complexity of this module allows CEMM maximum flexibility in
;==   allocating memory efficiently.
;==
;==
;==
;==
;=============================================================================
;==	P U B L I C   D E C L A R A T I O N S
;=============================================================================
	public	MemInit
	public	MemGet
	public	MemExit
	public	TotalFreeMem
	public	DoMoveBlock
ifdef ROMcomp
	public	ProtectROM
	public	UnProtectROM
endif
	public	fRSRVD
	public	fXMSMem
	public	fBIMMem
	public	fExtMem
	public	fSupExt

	public	PARA_BOUNDARY
	public	PAGE_BOUNDARY
	public	EMS_BOUNDARY
	public	DMA_BOUNDARY
;=============================================================================
;==	E X T E R N A L  D E C L A R A T I O N S
;=============================================================================
R_CODE	segment
	extrn	ext_rem:word
R_CODE	ends
;=============================================================================
;==	L O C A L   C O N S T A N T S
;=============================================================================
	include vdmseg.inc
	include desc.inc
	include oemdep.inc
	include vdisk.inc
	include	xmm.inc
;=============================================================================
;==	D A T A   S T R U C T U R E S
;=============================================================================
sHeadCell	struc
 HCnext		dw	$	; Pointer to next MCS
 HCprev		dw   $-HCprev	; Pointer to previous MCS
ifndef LC910610
 HCcount	dw	0       ; Linked list count (initially null)
else
 HCcount	db	0       ; Linked list count (initially null)
endif
sHeadCell	ends

sMemConStr	struc
 MCSnext	dw	-1	; Pointer to next MCS
 MCSprev	dw	-1	; Pointer to previous MCS
 MCSflags	dw	0	; Flags for type of memory (below)
 MCShandle	dw	0	; XMS handle
 MCSlen		dd	0	; Length of memory block in bytes
 MCSbase	dd	0	; Base address of memory block
sMemConStr	ends
;
; MCSflags			; Flags for type of memory
;
fRSRVD	equ	00000001b	; reserved
fXMSMem equ	00000010b	; XMS memory
fBIMMem	equ	00000100b	; BIM memory
fExtMem	equ	00001000b 	; Extended memory
fSupExt	equ	00010000b	; Super extended memory
;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
LAST	segment
	assume	cs:LAST,ds:LAST
;=============================================================================
;==	L O C A	L   D A T A
;=============================================================================
MemFlags	dw	0	; Flags which control program flow
 fBottomTop	equ	fRSRVD	; bottom to top allocation (int 15 & BIM)
 fXMSused	equ	fXMSMem	; XMS blocks used
 fBIMused	equ	fBIMMem	; BIM was used
 fI15used	equ	fExtMem	; Int 15h memory was used

XMScontrol	dd	0	; Entry point for XMM
XMSversion      dw      0       ; XMS version
XMSintrev       dw      0       ; XMM internal revision number

TopOfMemory	dd	1000000h; default to 16MB

PARA_BOUNDARY	equ	0	; paragraph boundary conditions
PAGE_BOUNDARY	equ	1	; 4K boundary condition
EMS_BOUNDARY	equ	2	; 16K boundary condition
DMA_BOUNDARY	equ	3	; no 64K or 128K boundary crossings

Alignment label	dword
	dd	10h
	dd	1000h
	dd	4000h

MGTBfunc:
	dw	offset TBalignX
	dw	offset TBalignX
	dw	offset TBalignX
	dw	offset TBDMABound
LAST_BOUND	equ	($-MGTBfunc)/2

MGBTfunc:
	dw	offset BTalignX
	dw	offset BTalignX
	dw	offset BTalignX
	dw	offset BTDMABound

;==============================================================================
;==
;==  MemInit: This procedure will initialize data required for use by the
;==	      MemGet routine to obtain extended memory.  It returns in EAX
;==	      a zero if no free memory is available or if non-zero it returns
;==	      the top of physical memory (last addressable byte of physical
;==	      memory).
;==
;==  Entry: (Real Mode)
;==	EAX = EMS pool size requested
;==
;==  Exit:
;==	EAX = Top of physical memory.  Zero, if no memory is available.
;==
;==============================================================================

MemInit	proc	near
	push	esp
	push	ds
	push	eax

	movzx	esp,sp			; access only 64K

	push	cs			; proper data segment
	pop	ds

	or	[MemFlags],fBottomTop	; assume XMS allocation
	call	ChkXMS			; allocate via XMS
	jnc	short MITop		; if OK, exit

	and	[MemFlags],not fBottomTop ; assume INT 15h allocation
	call	ChkInt15Mem		; check extended memory
	call	ChkBIM			; check for BIM

ifdef 910317	; If XMM, allocate memory via XMS
	call	ChkInt15Mem		; check extended memory
ifndef MSFLAG
	call	ChkBIM			; check for BIM
endif
	cmp	[FreeHC].HCcount,0 	;Q: Any free memory available?
	ja	short MItotal		; Y: make sure we have some
MIXMS:
	or	[MemFlags],fBottomTop	; try XMS allocation
	call	ChkXMS
MItotal:
	call	TotalFreeMem
	or	eax,eax			;Q: Any free memory available?
	jz	short MIexit		; N: return a zero

	cmp	eax,[esp]		;Q: Enough for EMS pool?
	ja	short MITop		; Y: get top of memory

	test	[MemFlags],fXMSused	;Q: XMS already used?
	jnz	short MITop 		; Y: get top of memory

	call	ChkXMM			;Q: XMM present?
	jnc	short MITryXMS		; Y: abort Int 15/BIM and try via XMS
endif

MITop:
	mov	eax,[TopOfMemory]       ; top of physical memory
	dec	eax			; last addressable physical memory

MIexit:
	add	sp,4
	pop	ds
	pop	esp
	ret

ifdef 910317
MITryXMS:
	mov	eax,-1			; abort Int15/BIM allocation
	call	MemExit
	jmp	short MIXMS
endif

MemInit	endp

;==============================================================================
;==
;==  MemGet: This procedure returns contiguous memory available.
;==
;==  Entry: (Real Mode)
;==  	EAX = Number of Bytes needed.
;==	 BX = 0 : Paragraph boundary
;==           1 : 4K boundary
;==           2 : DMA boundaries: Don't cross 64K or 128K boundary
;==	EBX = Type of memory (shl 16 fBIMMem,fExtMem,or/and fSupExt)
;==
;==  Exit:
;==  	EAX = Number of Bytes given. (carry flag set if less than asked for.)
;==	EBX = Starting address (flags if carry flag set)
;==
;==============================================================================

MemGet	proc	near
	push	ecx
	push	edx
	push	esi
	push	edi
	push	ds
	push	ebx

	push	cs			; proper data segment
	pop	ds

	cmp	bx,LAST_BOUND		;Q: Correct boundary condition?
	jb	short MGbound		; Y: continue
	xor	ebx,ebx			; N: default to no boundary conditions

MGbound:
	movzx	edi,bx			; move boundary condition flags

	shr	ebx,16			; type of memory to search for
	or	bx,bx			;Q: Specific types required?
	jnz	short MGflags 		; Y: leave specific mask
	not	bx			; N: allow all types
MGflags:
	xor	edx,edx			; init largest block to zero

	test	[MemFlags],fBottomTop	;Q: Allocating from bottom to top (XMS)?
	jnz	short MGBotTop		; Y: search free list from bottom

;
;  Search for contiguous block from top of memory in free list
;
	movzx	esi,[FreeHC].HCprev
	movzx	ecx,[FreeHC].HCcount    ;Q: Any free entries?
	jcxz	MGnotEnough		; N: no memory to give

MGTBloop:
	push	ebx
	test	[esi].MCSflags,bx	;Q: Correct type of memory?
	jz      short MGTBnext		; N: try next entry

	call	word ptr [MGTBfunc][edi*2] ;Q: Satisfy boundary conditions?
	jnc	short MGfound		; Y: found block

	cmp	edx,ebx			;Q: Largest free block so far?
	jae	short MGTBnext		; N: next entry
	mov	edx,ebx			; Y: save the size

MGTBnext:
	pop	ebx
	movzx	esi,[esi].MCSprev
	loop	MGTBloop

	jmp	short MGnotEnough

;
;  Search for contiguous block from bottom of memory in free list (XMS)
;
MGBotTop:
	movzx	esi,[FreeHC].HCnext
	movzx	ecx,[FreeHC].HCcount    ;Q: Any free entries?
	jcxz	MGnotEnough		; N: no memory to give

MGBTloop:
	push	ebx
	test	[esi].MCSflags,bx	;Q: Correct type of memory?
	jz      short MGBTnext		; N: try next entry

	call	word ptr [MGBTfunc][edi*2] ;Q: Satisfy boundary conditions?
	jnc	short MGfound		; Y: found block

	cmp	edx,ebx			;Q: Largest free block so far?
	jae	short MGBTnext		; N: next entry
	mov	edx,ebx			; Y: save the size

MGBTnext:
	pop	ebx
	movzx	esi,[esi].MCSnext
	loop	MGBTloop

;
;  EDX = size of largest block found
;
MGnotEnough:
	mov	eax,edx	    		; largest block available
	pop	ebx			; restore original flags
	stc                             ; error
	jmp	short MGexit

;
;  Found memory.  Delete from free list and add to used list.
;
MGfound:
	mov	ecx,eax
	mov	eax,dword ptr [esi].MCSflags
	call	AddUsedEntry
	or	[MemFlags],ax		; indicate usage of this type of memory
	mov	eax,ecx
	add	esp,8			; restore stack
	clc

MGexit:
	pop	ds
	pop	edi
	pop	esi
	pop	edx
	pop	ecx
	ret
MemGet	endp

;==============================================================================
;==
;==  MemExit: This procedure will allocate all memory received by the
;==	      MemGet routine.
;==	      * The only dependence to the rest of CEMM is that the amount
;==		of INT 15h memory allocated is placed in R_CODE:[ext_rem] *
;==
;==  Entry: (Real Mode)
;==	EAX = -1: abort, <>-1: allocate
;==
;==  Exit:
;==
;==
;==============================================================================

MemExit	proc	near
	push	ds

	push	cs			; proper data segment
	pop	ds

	test	[MemFlags],fXMSused 	;Q: Using XMS allocation?
	jz	short MEint15	    	; N: allocate int 15 and BIM

	call	AllocXMS		; allocate via XMS
	jmp	short MEexit

MEint15:
	cmp	eax,-1			;Q: Abort in progress?
	je	short MEexit		; Y: return all entries to null list

	test	[MemFlags],fI15used 	;Q: Was INT 15h memory used?
	jz	short MEBIM	    	; N: no need to update INT 15h handler

	call	AllocInt15		; allocate extended memory via INT 15h

MEBIM:
	test	[MemFlags],fBIMused 	;Q: Was BIM memory used?
	jz	short MEexit	    	; N: no need to update BIM data structure

	call	AllocBIM		; allocate BIM

MEexit:
	call	ResNullList		; just incase we pass through again

	pop	ds
	ret
MemExit	endp

;==============================================================================
;==
;==  TotalFreeMem: This procedure adds all the free memory in the free list.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==	EAX = Total free memory
;==
;==============================================================================

TotalFreeMem	proc	near
	push	ecx
	push	esi
	push	ds

	mov	ax,seg LAST
	mov	ds,ax

	xor	eax,eax			; initialize free memory amount

;
;  For each entry in the free memory list, add its size
;
	movzx	esi,[FreeHC].HCnext
	movzx	ecx,[FreeHC].HCcount	;Q: Any entries?
	jcxz	short TFMexit		; N: no free memory

TFMnextEntry:
	add	eax,[esi].MCSlen	; add free memory
	movzx	esi,[esi].MCSnext	; next entry
	loop	TFMnextEntry

TFMexit:
	pop	ds
	pop	esi
	pop	ecx
	ret
TotalFreeMem	endp

;==============================================================================
;==
;==  ChkInt15Mem: This procedure adds any extended memory to free memory list
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================

ChkInt15Mem 	proc near
	push	eax
	push	ebx
	push	ecx
	push	es

	mov	ah,88h 			; Get amount of extended memory
	int	15h

	or	ax,ax	        	;Q: Any extended memory?
	jz	short CIexit		; N: exit

	push	seg R_CODE
	pop	es

assume	es:R_CODE
	mov	es:[ext_rem],ax		; save for CEMM's INT 15h handler
assume	es:LAST

;
;  Add extended memory to free memory list
;
	mov	ebx,100000h		; base address at 1 meg
	movzx	ecx,ax			; length of extended in 1K blocks
	shl	ecx,10			; now in bytes
	mov	eax,fExtMem 	   	; flags: extended memory
ifndef MSFLAG
	call    AddFreeEntry
	call	ChkVDISK		; subtract any VDISK memory
endif
CIexit:
	pop	es
	pop	ecx
	pop	ebx
	pop	eax
	ret
ChkInt15Mem 	endp

;==============================================================================
;==
;==  ChkVDISK: This procedure checks for existing VDISK style allocation and
;==	       if found, deletes it from free memory list.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================

ChkVDISK proc	near
	push	eax
	push	ebx
	push	ecx
	push	si
	push	di
	push	es

	les	di,pINT19Vec		; es:di address of int 19h vector
	les	di,es:[di]		; es:di contains int 19h vector
	cld

;
;  Check for "VDISK" string
;
	mov	di,offset V_VDISK	; point to VDISK header
	mov	si,offset VDISK_name	; ds:si points to "VDISK"
	mov	cx,5			; length of "VDISK" string
	repe	cmpsb			;Q: Match?
	jne	short CVexit		; N: no VDISK!

;
;  Now for VDISK type "28h"
;
	mov	di,offset V_TYPE	; point to VDISK type byte
	mov	si,offset VDISK_type
	cmpsb				;Q: vol label attr match?
	jne	short CVexit		; N: not a vdisk
					; Y: must deduct VDISK memory

;
;  Deduct VDISK memory from free list and put it in used list
;
	mov	ebx,100000h		; base address of VDISK's
	movzx	eax,es:[V_ADDRHI] 	; bits 23:16 of last addr used by VDISK
	shl	eax,16			; high byte
	mov	ax,es:[V_ADDRLO] 	; bits 15:00 of last addr used by VDISK
	sub	eax,ebx			; length of VDISKs

;
;  Adjust free memory so VDISK memory is not included
;
	mov	si,[FreeHC].HCnext
ifndef LC910610
	mov	cx,[FreeHC].HCcount	;Q: Any free entries?
else
	movzx	cx,[FreeHC].HCcount	;Q: Any free entries?
endif
	jcxz	short CVerror		; N: error, where did VDISK come from?

	cmp	[si].MCSbase,ebx	;Q: Memory starting @ 1M?
	jne	short CVerror		; N: error, where did VDISK come from?

	add	[si].MCSbase,eax	; new base for free extended memory

	sub	[si].MCSlen,eax		;Q: Enough memory for VDISK?
	jb	short CVerror		; N: error, where did VDISK come from?
	ja	short CVexit		; Y: leave remaining free memory

	call	DelFreeEntry		; it all belongs to the VDISK

CVexit:
	pop	es
	pop	di
	pop	si
	pop	ecx
	pop	ebx
	pop	eax
	ret

CVerror:
;int 1	;error
	jmp	short CVexit
ChkVDISK	endp

;==============================================================================
;==
;==  ChkBIM: This procedure checks available BIM on a COMPAQ 386 machine and
;==	     if available memory is found, it's added to the free list.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================

ChkBIM  proc	near
	push	eax
	push	ebx
	push	ecx
	push	si
	push	di
	push	es

;
;  Check to see if we're on a Compaq 386 machine
;
	les	di,[pCOMPAQ]		; es:di points to possible COMPAQ signature
	mov	si,offset szCOMPAQ 	; "03COMPAQ"
	mov	cx,8
	cld
	rep	cmpsb			;Q: COMPAQ 386 machine?
	jne	short CBexit		; N: no BIM on this machine

	mov	bx,pBIMCtlStruct	; Y: es:bx points to ptr of BIM data structure
	mov	bx,es:[bx]		; es:bx points to BIM data structure

	cmp	es:[bx].AVAILABLE,-1	;Q: Is there a 32-bit memory installed?
	je	short CBexit		; N: return
	movzx	ecx,es:[bx].AVAILBIM 	;Q: Any BIM available?
	jcxz	short CBexit		; N: return
ifndef LC910611
	cmp	cx,es:[bx].TOTALBIM 	;Q: Is there more avail than total?
	ja	short CBexit		; Y: something is wrong!
endif					; N: add to free memory list

;
;  Add available BIM to free memory list
;

	shl	ecx,4		    	; size in bytes
	mov	ax,es:[bx].LASTUSED 	; last used paragraph
	sub	ax,es:[bx].AVAILBIM
;
;  One more check, some dirty programs (i.e., NORTON's PCSHADOW.SYS) will not
;  leave the BIM data structure in a stable state.
;
	add	ax,es:[bx].TOTALBIM	;Q: Can never be above 16MB!
	jc	short CBexit		; Y: it is, don't add memory
	cmp	ax,0E000h		;Q: BIM can never be above 16MB-128K!
	ja	short CBexit		; Y: it is, don't add memory
	sub	ax,es:[bx].TOTALBIM

	movzx	ebx,ax
 	add	ebx,0F0000h		; starting paragraph address for BIM
	shl	ebx,4			; starting address of BIM

	mov	eax,fBIMMem     	; flags: BIM memory
	call	AddFreeEntry		; EAX=flags, EBX=start, ECX=size

CBexit:
	pop	es
	pop	di
	pop	si
	pop	ecx
	pop	ebx
	pop	eax
	ret
ChkBIM	endp

;==============================================================================
;==
;==  ChkXMS: This procedure checks if XMS memory is available, and if it's
;==	     found, it's added to the free memory list.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================

ChkXMS	proc	near
	pushad

	call	ChkXMM			;Q: Is XMM installed?
	jc	CXexitX			; N: exit (no XMS memory) set carry
;910317	jc	CXexit			; N: exit (no XMS memory)
					; Y: get XMM entry point
;
;  XMM is installed
;
	mov	ax,4310h	        ; get XMM routine address
	int	2fh
	mov	word ptr [XMScontrol],bx; save entry point
	mov	word ptr [XMScontrol+2],es

	mov	ah,XMM_GET_VERSION	; get XMS version number
	call	[XMScontrol]	        ; (note: due to problems with HIMEM.SYS
	mov	[XMSversion],ax	        ;  prior to VER 2.06, we might not want
	mov	[XMSintrev],bx 	        ;  to use anything lower).

;
;  Grab all free blocks from the XMM and place them in the free list
;
CXloop:
	mov	ah,XMM_QUERY_FREE_EXTMEM; query largest contiguous block size
	call	[XMScontrol]
	movzx	ecx,ax			;Q: Any more free memory?
	jcxz    short CXunlock		; N: unlock all the blocks allocated

	mov	ah,XMM_ALLOC_EMB	; allocate this block
	mov	dx,cx			; size to allocate
	call	[XMScontrol]
	or	ax,ax			;Q: Did we allocate the block?
	jz	short CXerror		; N: this should never happen

	mov	ah,XMM_LOCK_EMB		; lock the address of this block
	mov	si,dx			; save handle for later use

	call	[XMScontrol]
	or	ax,ax			;Q: Did we lock the block?
	jz	short CXerror		; N: this should never happen

	mov	ax,si			; fill in info about block
	shl	eax,16			; handle
	or	eax,fXMSMem		; flag indicating XMS memory
	shl	edx,16                  ; base address
	and	ebx,0FFFFh
	or	ebx,edx
	cmp	ebx,1000000h		;Q: Super extended (>16MB)?
	jb	short CXTopMem		; N: check if top of memory
	or	eax,fSupExt		; Y: flag it
CXTopMem:
	shl	ecx,10			; size in bytes
	add	ebx,ecx			; top of block
	cmp	ebx,[TopOfMemory]	;Q: New top of memory?
	jbe	short CXAddEntry	; N: don't update
	mov	[TopOfMemory],ebx	; Y: new top of memory
CXAddEntry:
	sub	ebx,ecx			; restore base
	call	AddFreeEntry		; add to free memory list
	or	[MemFlags],fXMSused	; indicate allocation via XMS

	jmp	short CXloop 		; next free memory block

CXunlock:
;
;  Unlock all the blocks (The addresses and lengths are all known)
;
	movzx	esi,[FreeHC].HCnext
	movzx	ecx,[FreeHC].HCcount	;Q: Any entries?
	jcxz	short CXexit		; N: no XMS blocks allocated

CXnextEntry:
	mov	ah,13			; unlock function
	mov	dx ,[esi].MCShandle	; get handle
	call	[XMScontrol]
	or	ax,ax			;Q: Did we unlock the block?
	jz	short CXerror		; N: this should never happen

	movzx	esi,[esi].MCSnext	; next entry
	loop	CXnextEntry

CXexit:
	clc
CXexitX:
	popad
	ret

CXerror:
int 1;QLEO ;error	; If errors are ever allowed in this subroutine,
	stc		; we must unlock the handles already allocated before
	popad		; leaving!!!
	ret
ChkXMS	endp

;==============================================================================
;==
;==  ChkXMM: This procedure checks if an XMM is installed on the system.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================

ChkXMM	proc	near
	push	ax

	mov	ax,4300h		; XMM detect function
	int	2fh
	cmp	al,80h			;Q: Is XMM installed?
	pop	ax
	jne	CXnotInstalled		; N: exit (no XMS memory)
	clc				; Y: indicate an XMM present
	ret

CXnotInstalled:
	stc
	ret
ChkXMM	endp

;==============================================================================
;==
;==  AllocXMS: This procedure allocates via XMS.  If an abort is in progress,
;==	       a release of all blocks will occur.  If no abort, a block will
;==	       either be resized or released.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================

AllocXMS proc	near

	cmp	eax,-1			;Q: Abort in progress?
	je	short AXabort		; Y: release all handles/blocks

	call	AllocXMSused		; release free XMS memory
	jmp	short AXexit

AXabort:
	call	ReleaseXMS              ; release all XMS memory

AXexit:
	ret
AllocXMS	endp

;==============================================================================
;==
;==  AllocInt15: This procedure allocates Int 15h memory.
;==	         * The only dependence to the rest of CEMM is that the amount
;==		   of INT 15h memory remaining free is placed in
;==		   R_CODE:[ext_rem]. *
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================

AllocInt15 proc	near
	push	eax
	push	ebx
	push	edx
	push	edi
	push	es

;
;  Get the range of used INT 15h memory
;
	mov	edi,offset UsedHC	; used memory list
	mov	edx,fExtMem		; extended memory flag
	call	MemRange

;
;  EBX = starting address of used extended memory (INT 15h)
;
	sub	ebx,100000h		; number of bytes still free
	shr	ebx,10			; round down to 1k blocks

	mov	ax,seg R_CODE
	mov	es,ax

assume	es:R_CODE

	mov	es:[ext_rem],bx		; save for CEMM's INT 15h handler

assume	es:LAST

	pop	es
	pop	edi
	pop	edx
	pop	ebx
	pop	eax
	ret
AllocInt15	endp

;==============================================================================
;==
;==  AllocBIM: This procedure allocates needed BIM
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================

AllocBIM proc	near
	push	eax
	push	ebx
	push    edx
	push	di
	push	es

;
;  Get the range of used BIM memory
;
	mov	edi,offset UsedHC	; used memory list
	mov	edx,fBIMMem		; BIM memory flag
	call	MemRange

;
;  EBX = starting address of used BIM memory
;
	sub	ebx,0F00000h		; get paragraph starting address
	shr	ebx,4

	mov	di,RomSeg		; put ROM segment in ES
	mov	es,di
	mov	di,pBIMCtlStruct	; es:bx points to ptr of BIM data structure
	mov	di,es:[di]		; es:bx points to BIM data structure
ifdef ROMcomp
	cmp	es:[di].AVAILABLE,-1	;Q: If ROM compression is BIM available?
	je	short ABexit		; N: exit, don't mark allocated
endif
	sub	bx,es:[di].LASTUSED 	; number of paragraphs used

	call	UnProtectROM

	add	es:[di].AVAILBIM,bx 	; free BIM
	add	es:[di].LASTUSED,bx 	; last used paragraph

	call	ProtectROM

ifdef ROMcomp
ABexit:
endif
	pop	es
	pop	di
	pop	edx
	pop	ebx
	pop	eax
	ret
AllocBIM	endp

;==============================================================================
;==
;==  AllocXMSused: This procedure allocates used memory via XMS.  All allocated
;==	           XMS blocks will either be released, resized, or unchanged.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================

AllocXMSused proc	near
	pushad
	push	esp
	push	es

assume	es:R_CODE
	mov	ax,seg R_CODE
	mov	es,ax
	mov	ah,88h
	int	15h			; don't change current INT 15h reading
	mov	es:[ext_rem],ax		; save for CEMM's INT 15h handler
	pop	es
assume	es:LAST

	movzx	esp,sp			; access only 64K
	xor	edx,edx
;
;  For each handle in the free memory block, either resize it or release it
;

	mov	edi,offset FreeHC
	movzx	ecx,[edi].HCcount
	or	cx,cx			;Q: Any entries?
	jz	AXuExit			; N: all free memory allocated

	movzx	esi,[edi].HCnext	; first entry

AXuNextEntry:
	cmp	edx,dword ptr [esi].MCSflags ;Q: Same handle?
	jne	short AXuNewHandle	     ; N: process this handle
AXuCont:
	movzx	esi,[esi].MCSnext	     ; Y: get next handle
	loop	AXuNextEntry

	jmp	AXuExit			; finished all handles

;
;  Get this handles range in the free memory list
;
AXuNewHandle:
	mov	edx,dword ptr [esi].MCSflags ; get handle/flags

	call	MemRange		;Q: Handle range in this (free) list?
	jc	AXuError		; N: we know the handle is in free list

	push	eax
	push	ebx
	push	edi

;
;  Get this handle's range in the used memory list
;
	mov	edi,offset UsedHC
	call	MemRange	      	;Q: Handle range in used list?
	pop	edi
	jc	short AXuRelease	; N: handle must be released

;
;  Figure out the used size of the handle
;
	sub	ebx,[esp][0]		;Q: Do we have the original base?
	jbe	short AXuResize		; Y: conitnue
	add     eax,ebx			; N: increase length
	xor	ebx,ebx			; set up to get original base

AXuResize:
	add	ebx,[esp][0]		; now we have original handle base

	add	eax,400h-1		; round to 1K boundary
	and	eax,not (400h-1)

;
;  Resize this handle to the amount used
;
	shr	eax,10			; 1K blocks
	mov	bx,ax
	push	edx			; save handle/flags
	shr	edx,16			; handle
	mov	ah,15			; resize function
	call	[XMScontrol]
	pop	edx
	or	ax,ax			;Q: Did the resize occur?
	jz	short AXuErrorX		; N: CEMM will keep the entire block
	jmp	short AXuStack		; Y: clean up stack

;
;  Release this handle
;
AXuRelease:
	push	edx			; save handle/flags
	shr	edx,16			; handle
	mov	ah,10			; release this handle (free block)
	call	[XMScontrol]
	pop	edx
	or	ax,ax			;Q: Is the memory free?
	jz	short AXuErrorX		; N: CEMM will hog all of memory

AXuStack:
	add	esp,8			; clean up stack
	jmp	AXuCont			; get next entry

AXuExit:
	clc
	pop	esp
	popad
	ret

AXuErrorX:
	add	esp,8			; clean up stack
AXuError:
;int 1	;error
	stc
	pop	esp
	popad
	ret
AllocXMSused	endp

;==============================================================================
;==
;==  ReleaseXMS: This procedure releases all XMS handles.  No error checking
;==		 because it may release the same handle multiple times.  This
;==		 will cause errors on releases after the first.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================

ReleaseXMS proc	near
	push	ax
	push	dx
	push	ecx
	push	esi

;
;  For each handle in the free memory block, release it!
;
	movzx	esi,[FreeHC].HCnext
	movzx	ecx,[FreeHC].HCcount	;Q: Any entries?
	jcxz	short RXused		; N: all free memory allocated

RXnextFree:
	mov	ah,10			; free memory function
	mov	dx ,[esi].MCShandle	; get handle to release
 	call	[XMScontrol]

	movzx	esi,[esi].MCSnext	; next entry
	loop	RXnextFree

;
;  For each handle in the used memory block, release it!
;
RXused:
	movzx	esi,[UsedHC].HCnext
	movzx	ecx,[UsedHC].HCcount	;Q: Any entries?
	jcxz	short RXexit		; N: all free memory allocated

RXnextUsed:
	mov	ah,10			; free memory function
	mov	dx ,[esi].MCShandle	; get handle to release
	call	[XMScontrol]

	movzx	esi,[esi].MCSnext	; next entry
	loop	RXnextUsed

RXexit:
	pop	esi
	pop	ecx
	pop	dx
	pop	ax
	ret
ReleaseXMS	endp

;==============================================================================
;==
;==  TBalignX: Returns the first block of memory satisfying the alignment
;==	      criteria (starting at the top of memory). Or the largest
;==	      contiguous block.
;==
;==  Entry: (Real Mode)
;==	EAX = Size of contiguous memory required
;==     ESI = Free entry pointer
;==	EDI = Boundary condition
;==
;==  Exit:
;==	EAX = Size  (carry not set)
;==	EBX = Base Address
;==     ESI = Free entry pointer
;==
;==	EAX = same  (carry set)
;==	EBX = Size of largest contiguous block
;==     ESI = Free entry pointer
;==
;==============================================================================

TBalignX	proc	near
	push	edx
;
;  Get upper block (top)
;
	mov	edx,cs:[Alignment][edi*4] ; alignment
	call	TBAlign

	pop	edx
	ret
TBalignX	endp

;==============================================================================
;==
;==  BTalignX: Returns the first block of memory satisfying the alignment
;==	      criteria (starting at the bottom of memory). Or the largest
;==	      contiguous block.
;==
;==  Entry: (Real Mode)
;==	EAX = Size of contiguous memory required
;==     ESI = Free entry pointer
;==	EDI = Boundary condition
;==
;==  Exit:
;==	EAX = Size  (carry not set)
;==	EBX = Base Address
;==     ESI = Free entry pointer
;==
;==	EAX = same  (carry set)
;==	EBX = Size of largest contiguous block
;==     ESI = Free entry pointer
;==
;==============================================================================

BTalignX	proc	near
	push	edx
;
;  Get lower block (bottom)
;
	mov	edx,cs:[Alignment][edi*4] ; alignment
	call	BTAlign

	pop	edx
	ret
BTalignX	endp

ifdef QEMS
;==============================================================================
;==
;==  TBParaBound: Returns the top portion of the memory block required or the
;==		  largest contiguous block.
;==
;==  Entry: (Real Mode)
;==	EAX = Size of contiguous memory required
;==     ESI = Free entry pointer
;==
;==  Exit:
;==	EAX = Size  (carry not set)
;==	EBX = Base Address
;==     ESI = Free entry pointer
;==
;==	EAX = same  (carry set)
;==	EBX = Size of largest contiguous block
;==     ESI = Free entry pointer
;==
;==============================================================================

TBParaBound proc	near
	push	edx
;
;  Get upper block (top)
;
	mov	edx,10h			; paragraph alignment
	call	TBAlign

	pop	edx
	ret
TBParaBound	endp

;==============================================================================
;==
;==  BTParaBound: Returns the bottom portion of the memory block required or the
;==		  largest contiguous block.
;==
;==  Entry: (Real Mode)
;==	EAX = Size of contiguous memory required
;==     ESI = Free entry pointer
;==
;==  Exit:
;==	EAX = Size  (carry not set)
;==	EBX = Base Address
;==     ESI = Free entry pointer
;==
;==	EAX = same  (carry set)
;==	EBX = Size of largest contiguous block
;==     ESI = Free entry pointer
;==
;==============================================================================

BTParaBound proc	near
	push	edx
;
;  Get lower block (bottom)
;
	mov	edx,10h			; paragraph alignment for i486
	call	BTAlign

	pop	edx
	ret
BTParaBound	endp

;==============================================================================
;==
;==  TBPageBound: Returns the top 4K page aloigned memory block in the free
;==		  entry or the largest 4K page aligned contiguous block.
;==
;==  Entry: (Real Mode)
;==	EAX = Size of contiguous memory required
;==     ESI = Free entry pointer
;==
;==  Exit:
;==	EAX = Size  (carry not set)
;==	EBX = Base Address
;==     ESI = Free entry pointer
;==
;==	EAX = same  (carry set)
;==	EBX = Size of largest contiguous block
;==     ESI = Free entry pointer
;==
;==============================================================================

TBPageBound proc	near
	push	edx
;
;  Get upper 4K aligned block (top)
;
	mov	edx,1000h		; 4K byte (page) alignment
	call	TBAlign

	pop	edx
	ret
TBPageBound	endp

;==============================================================================
;==
;==  BTPageBound: Returns the bottom 4K page aligned block int the free entry
;==		  or the largest 4K page aligned contiguous block.
;==
;==  Entry: (Real Mode)
;==	EAX = Size of contiguous memory required
;==     ESI = Free entry pointer
;==
;==  Exit:
;==	EAX = Size  (carry not set)
;==	EBX = Base Address
;==     ESI = Free entry pointer
;==
;==	EAX = same  (carry set)
;==	EBX = Size of largest contiguous block
;==     ESI = Free entry pointer
;==
;==============================================================================

BTPageBound proc	near
	push	edx
;
;  Get lower 4K aligned block (bottom)
;
	mov	edx,1000h		; 4K byte (page) alignment
	call	BTAlign

	pop	edx
	ret
BTPageBound	endp
endif

;==============================================================================
;==
;==  TBDMABound: Returns the top portion of memory which does not cross a
;==		 64/128K boundary.  If larger than 64K, it will be 128K aligned.
;==		 If not found, the size of the largest block which meets the
;==		 above criteria will be returned.
;==
;==  Entry: (Real Mode)
;==	EAX = Size of contiguous memory required
;==     ESI = Free entry pointer
;==
;==  Exit:
;==	EAX = Size  (carry not set)
;==	EBX = Base Address
;==     ESI = Free entry pointer
;==
;==	EAX = same  (carry set)
;==	EBX = Size of largest contiguous block
;==     ESI = Free entry pointer
;==
;==============================================================================

TBDMABound proc	near
	push	edx

	cmp	eax,10000h		;Q: Less than a 64K buffer needed?
	jle	short TBDB64K		; Y: don't cross a 64K boundary.

;
;  DMA buffer > 64K is needed. For it to be useful it must be 128K aligned.
;
	mov	edx,20000h		; 128K aligned block
	call	TBAlign

	jmp	short TBDBexit

;
;  DMA buffer < 64K is needed. It may not cross a 64K boundary.
;
TBDB64K:
	mov	edx,10000h		; don't cross a 64K boundary
	call	TBNoCross

TBDBexit:
	pop	edx
	ret
TBDMABound	endp

;==============================================================================
;==
;==  BTDMABound: Returns the bottom portion of memory which does not cross a
;==		 64/128K boundary.  If larger than 64K, it will be 128K aligned.
;==		 If not found, the size of the largest block which meets the
;==		 above criteria will be returned.
;==
;==  Entry: (Real Mode)
;==	EAX = Size of contiguous memory required
;==     ESI = Free entry pointer
;==
;==  Exit:
;==	EAX = Size  (carry not set)
;==	EBX = Base Address
;==     ESI = Free entry pointer
;==
;==	EAX = same  (carry set)
;==	EBX = Size of largest contiguous block
;==     ESI = Free entry pointer
;==
;==============================================================================

BTDMABound proc	near
	push	edx

	cmp	eax,10000h		;Q: Less than a 64K buffer needed?
	jle	short BTDB64K		; Y: don't cross a 64K boundary.

;
;  A DMA buffer > 64K is needed. For it to be useful it must be 128K aligned.
;
	mov	edx,20000h		; 128K aligned block
	call	BTAlign

	jmp	short BTDBexit

;
;  DMA buffer < 64K is needed. It may not cross a 64K boundary.
;
BTDB64K:
	mov	edx,10000h		; don't cross a 64K boundary
	call	BTNoCross

BTDBexit:
	pop	edx
	ret
BTDMABound	endp

;==============================================================================
;==
;==  TBAlign: Returns the top aligned memory block in the free entry or the
;==	      largest aligned contiguous block.
;==
;==  Entry: (Real Mode)
;==	EAX = Size of contiguous memory required
;==	EDX = Alignment required
;==     ESI = Free entry pointer
;==
;==  Exit:
;==	EAX = Size  (carry not set)
;==	EBX = Base Address
;==	EDX = undefined
;==     ESI = Free entry pointer
;==
;==	EAX = same  (carry set)
;==	EBX = Size of largest contiguous block
;==	EDX = Alignment required
;==     ESI = Free entry pointer
;==
;==============================================================================

TBAlign proc	near

;
;  Get upper aligned block (top)
;
	mov	ebx,[esi].MCSbase  	; starting address of free memory range
	add	ebx,[esi].MCSlen	; ending address of free memory range
	neg	edx			; mask to round down for alignment

	sub	ebx,eax			;Q: Possible starting address > 0
	jbe	short TBAnotEnough	; N: definitely not enough

	and	ebx,edx			; aligned address

	cmp	ebx,[esi].MCSbase       ;Q: Still in free memory range?
	jb	short TBAnotEnough	; N: get largest block with alignment
	neg	edx			; Y: found an aligned block
	clc
	jmp	short TBAexit

TBAnotEnough:
	mov	ebx,[esi].MCSbase	; starting address of free memory range
	dec	ebx
	and	ebx,edx			; low alignement
	neg	edx			; alignemnt
	add	ebx,edx			; correct alignment
	sub	ebx,[esi].MCSbase	; amount lost due to alignment

	cmp	ebx,[esi].MCSlen	;Q: Used up all memory during alignment?
	jbe	short TBAmax		; N: get max size
	xor	ebx,ebx			; Y: no memory available with this alignment
	stc
	jmp	short TBAexit

TBAmax:
	neg	ebx
	add	ebx,[esi].MCSlen	; largest aligned block allowed
	stc

TBAexit:
	ret
TBAlign	endp

;==============================================================================
;==
;==  BTAlign: Returns the bottom aligned block in the free entry or the
;==	      largest aligned contiguous block.
;==
;==  Entry: (Real Mode)
;==	EAX = Size of contiguous memory required
;==	EDX = Alignment required
;==     ESI = Free entry pointer
;==
;==  Exit:
;==	EAX = Size  (carry not set)
;==	EBX = Base Address
;==	EDX = undefined
;==     ESI = Free entry pointer
;==
;==	EAX = same  (carry set)
;==	EBX = Size of largest contiguous block
;==	EDX = Alignment required
;==     ESI = Free entry pointer
;==
;==============================================================================

BTAlign proc	near
;
;  Get lower aligned block
;
	neg	edx			; mask for alignement
	mov	ebx,[esi].MCSbase	; starting address of free memory range
	dec	ebx
	and	ebx,edx			; low alignement
	neg	edx			; alignemnt
	add	ebx,edx			; correct alignment
	sub	ebx,[esi].MCSbase	; amount lost due to alignment

	cmp	ebx,[esi].MCSlen	;Q: Used up all memory during alignment?
	jb	short BTAmax		; N: get max size
	xor	ebx,ebx			; Y: no memory available with this alignment
	stc
	jmp	short BTAexit

BTAmax:
	neg	ebx
	add	ebx,[esi].MCSlen	; largest aligned block allowed

	cmp	ebx,eax			;Q: Enough for request?
	jae	short BTAfound		; Y: get starting address
	stc				; N: return largest size available
	jmp	short BTAexit

BTAfound:
	neg	edx			; mask for alignement
	mov	ebx,[esi].MCSbase	; starting address of free memory range
	dec	ebx
	and	ebx,edx			; low alignement
	neg	edx			; alignemnt
	add	ebx,edx			; correct alignment
	clc

BTAexit:
	ret
BTAlign	endp

;==============================================================================
;==
;==  TBNoCross: Returns the top memory block in the free entry or the largest
;==	      	contiguous block which does not cross a boundary.
;==		NOTE: EAX <= EDX must hold for this routine to make any sense!
;==
;==  Entry: (Real Mode)
;==	EAX = Size of contiguous memory required
;==	EDX = Boundary not to cross
;==     ESI = Free entry pointer
;==
;==  Exit:
;==	EAX = Size  (carry not set)
;==	EBX = Base Address
;==	EDX = undefined
;==     ESI = Free entry pointer
;==
;==	EAX = same  (carry set)
;==	EBX = Size of largest contiguous block
;==	EDX = Boundary not to cross
;==     ESI = Free entry pointer
;==
;==============================================================================

TBNoCross proc	near
	push	ecx
	push	edx

	mov	ebx,[esi].MCSbase
	add	ebx,[esi].MCSlen
	mov	ecx,ebx			; end of free memory block
	dec	ecx
	neg	edx
	and	ecx,edx			; top boundary
	sub	ebx,eax                 ; possible starting address

	cmp	ebx,ecx			;Q: Did it cross a boundary?
	jb	short TBNCTopMax	; Y: get maximum above boundary

	cmp	ebx,[esi].MCSbase	;Q: Within range?
	jae	short TBNCFound		; Y: found a block
	mov	ebx,[esi].MCSlen        ; N: largest available size

	jmp	short TBNCLargest

TBNCTopMax:
	mov	edx,[esi].MCSbase	; get largest block above boundary
	add	edx,[esi].MCSlen
	sub	edx,ecx

;
;  Try below the boundary address (ECX).
;
	mov	ebx,ecx
	sub	ebx,eax                 ; possible starting address

	cmp	ebx,[esi].MCSbase	;Q: Within range?
	jae	short TBNCFound		; Y: found a block
	mov	ebx,ecx			; N: get largest block below boundary

	sub	ebx,[esi].MCSbase

;
;  Figure out which block was larger, above or below the boundary.
;

	cmp	edx,ebx			;Q: Above the boundary?
	jae	short TBNCAbove		; Y: get largest size above 64K alignment
					; N: largest block below 64K alignment
	jmp	short TBNCLargest

TBNCAbove:
	mov	ebx,edx			; largest block above 64K alignment

;
;  Requested size not found, will return the largest available.
;
TBNCLargest:
	stc
	jmp 	short TBNCexit

;
;  Block was found
;
TBNCFound:
	clc

TBNCexit:
	pop	edx
	pop	ecx
	ret
TBNoCross	endp

;==============================================================================
;==
;==  BTNoCross: Returns the lowest memory block in the free entry or the
;==	      	largest contiguous block which does not cross a boundary.
;==		NOTE: EAX <= EDX must hold for this routine to make any sense!
;==
;==  Entry: (Real Mode)
;==	EAX = Size of contiguous memory required
;==	EDX = Boundary not to cross
;==     ESI = Free entry pointer
;==
;==  Exit:
;==	EAX = Size  (carry not set)
;==	EBX = Base Address
;==	EDX = undefined
;==     ESI = Free entry pointer
;==
;==	EAX = same  (carry set)
;==	EBX = Size of largest contiguous block
;==	EDX = Boundary not to cross
;==     ESI = Free entry pointer
;==
;==============================================================================

BTNoCross proc	near
	push	ecx
	push	edx

	mov	ebx,[esi].MCSbase	; starting address
	mov	ecx,ebx			; get lowest boundary
	dec	edx
	add	ecx,edx
	not	edx
	and	ecx,edx
	mov	edx,ecx
	sub	edx,ebx			; size before hitting boundary

	cmp	edx,eax			;Q: Enough room below boundary?
	jb	short BTNCBotMax	; N: get maximum size below boundary

	cmp	edx,[esi].MCSlen	;Q: Enough room in free entry?
	jbe	short BTNCFound		; Y: found the block

	mov	ebx,[esi].MCSlen	; largest block available
	jmp	short BTNCLargest

;
;  Find largest size below boundary
;
BTNCBotMax:
	cmp	edx,[esi].MCSlen	;Q: Enough room in free entry?
	jbe	short BTNCTop		; Y: get size above boundary
	mov	ebx,[esi].MCSlen	; N: largest block
	jmp	short BTNCLargest

;
;  Get size above the boundary
;
BTNCTop:
	mov	ebx,ecx			; boundary
	sub	ecx,[esi].MCSbase	; amount lost by boundary
	neg	ecx
	add	ecx,[esi].MCSlen	; amount above boundary

	cmp	ecx,eax			;Q: Enough memory to fulfill request?
	jae	short BTNCFound		; Y: found the block

	mov	ebx,ecx                 ; size of buffer above boundary

	cmp	ebx,edx			;Q: Largest buffer above boundary?
	ja	short BTNCLargest	; Y: return size
	mov	ebx,edx			; N: get size of buffer below boundary

;
;  Requested size not found, will return the largest available.
;
BTNCLargest:
	stc
	jmp	short BTNCexit

;
;  Block was found
;
BTNCFound:
	clc

BTNCexit:
	pop	edx
	pop	ecx
	ret
BTNoCross	endp

;==============================================================================
;==
;==  MemRange: This procedure returns the range of a specific memory in a list
;==
;==  Entry: (Real Mode)
;==     EDI = Head cell for list
;==	EDX = Flags/handle
;==
;==  Exit:
;==	EAX = Size
;==	EBX = Base Address
;==
;==============================================================================

MemRange proc	near
	push	ecx
	push	edi

	xor	eax,eax 		; initialize
	xor	ebx,ebx

	movzx	ecx,[edi].HCcount	;Q: Any entries?
	jcxz	short MRerror		; N: should not happen

;
;  Find an entry with the handle in list
;
MRfindHandle:
	movzx	edi,[edi].MCSnext
	cmp	edx,dword ptr [edi].MCSflags ;Q: Correct handle?
	je	short MRfoundHandle	     ; Y: found handle
	loop	MRfindHandle

	jmp	short MRnotFound	; handle not found
;
;  Handle was found in list.
;
MRfoundHandle:
	mov	ebx,[edi].MCSbase       ; first handle is base
	jmp	short MRcont

MRnextEntry:
	movzx	edi,[edi].MCSnext
	cmp	edx,dword ptr [edi].MCSflags ;Q: Same handle?
	jne	short MRlast		     ; N: done for this handle
MRcont:
	mov	eax,[edi].MCSlen
	add	eax,[edi].MCSbase       ; last address of memory range
	loop	MRnextEntry

MRlast:
	sub	eax,ebx			; size of range

	clc
	pop	edi
	pop	ecx
	ret

MRerror:
;int 1	;error
MRnotFound:
	stc
	pop	edi
	pop	ecx
	ret
MemRange	endp

;==============================================================================
;==
;==  ResNullList: This procedure deletes all entries from the used and free
;==		  lists and enters them in the null list
;==
;==  Entry: (Real Mode)
;==
;==  Exit:  (same)
;==
;==============================================================================

ResNullList 	proc	near
	push	eax
	push	ecx
	push    esi
	push	edi

;
;  Delete all free list entries
;
	mov	edi,offset FreeHC
	movzx	ecx,[edi].HCcount	;Q: Any entries?
	jcxz    short RNLused		; N: delete used entries
	movzx	esi,[edi].HCnext        ; Y: get first entry
RNLnxtFree:
	movzx	eax,[esi].MCSnext       ; save next entry (before unlinking)
	cmp	esi,edi			;Q: Head cell?
	je	short RNLerror		; Y: the count was wrong
	call	DelFreeEntry		; N: delete this entry
	mov	esi,eax			; get next entry
	loop	RNLnxtFree

;
;  Delete all used list entries
;
RNLused:
	mov	edi,offset UsedHC
	movzx	ecx,[edi].HCcount	;Q: Any entries?
	jcxz    short RNLexit		; N: exit
	movzx	esi,[edi].HCnext        ; Y: get first entry
RNLnxtUsed:
	movzx	eax,[esi].MCSnext       ; save next entry (before unlinking)
	cmp	esi,edi			;Q: Head cell?
	je	short RNLerror		; Y: the count was wrong
	call	DelUsedEntry		; N: delete this entry
	mov	esi,eax			; get next entry
	loop	RNLnxtUsed

RNLexit:
	clc
	pop	edi
	pop	esi
	pop	ecx
	pop	eax
	ret

RNLerror:
	stc
	pop	edi
	pop	esi
	pop	ecx
	pop	eax
	ret

ResNullList	endp

;==============================================================================
;==
;==  AddUsedEntry: This procedure is passed a used memory range found within
;==		   a free memory range.  It then removes the used memory from
;==		   the free MCS entry and links the entry into the used list.
;==		   The original free MCS entry may need to be: 1) removed from
;==		   the free MCS list (if the used entry encompasses all of its
;==		   memory range), 2) resized (if the used entry's range is at
;==		   beginning or the end of the free entry's range), and 3) split
;==		   into two entries (if the used entry's range splits the old
;==		   free entry's range into two non-contiguous regions).
;==
;==  Entry: (Real Mode)
;==	EAX = Flags in low 16 bits, XMS handle in high 16 bits
;==  	EBX = Base address of memory block
;==	ECX = Length of memory block
;==
;==  Exit:  (same)
;==
;==============================================================================

AddUsedEntry	proc	near
	push	eax
	push	ebx
	push	ecx
	push	esi
	push	edi

	call	GetNullEntry      	; get an MCS entry

	mov	[esi].MCSbase,ebx       ; fill in info
	mov	[esi].MCSlen,ecx
	mov	dword ptr [esi].MCSflags,eax

	mov	edi,offset UsedHC	; link entry into used list
	call	LinkEntry

;
;  Look for free entry corresponding to used entry: [ESI]
;
	mov	ebx,offset FreeHC	; free chain HC
	movzx	ecx,[ebx].HCcount	;Q: Any entries?
	jcxz	short AUEerror		; N: error
AUEloop:                                ; Y: search thru them
	movzx	ebx,[ebx].MCSnext       ; get next MCS entry in chain
	mov	eax,[ebx].MCSbase	; get base address
	add	eax,[ebx].MCSlen	; ending address
	sub	eax,[esi].MCSbase	;Q: In this range?
	ja	short AUEchkHi		; Y: found the correct entry
	loop	AUEloop			; N: try next entry

	jmp	short AUEerror		; error! did not find a matching entry

;
;  Free entry has been found
;
AUEchkHi:
	sub	eax,[esi].MCSlen	;Q: Used the latter part of the range?
	je	short AUEchkLo		; Y: no fragmentation ocurred.
	jb	short AUEerror		; -: error, larger than free range!
					; N: create new free entry: EAX=size
;
;  Create an entry for the tail portion of the free range
;
	push	ebx			; save original free entry

	mov	ecx,eax			; size of new free range
	mov	eax,dword ptr [ebx].MCSflags ; flags/handle from original
	mov	ebx,[esi].MCSbase	; start of used range
	add	ebx,[esi].MCSlen	; start of new free range

	call	AddFreeEntry		; add to free entry list
 	pop	ebx			; restore original free entry

AUEchkLo:
	mov	eax,[esi].MCSbase	; get base address of used entry
	sub	eax,[ebx].MCSbase	;Q: Used first portion of range?
	ja	short AUEresize		; N: need to resize
	jb	short AUEerror		; -: error, not within free range!
;
;  Free entry no longer needed, unlink from free list and link to null list
;
	xchg	ebx,esi			; free entry in esi and used in ebx
	call	DelFreeEntry
	mov	esi,ebx
	jmp	short AUEexit

AUEresize:
	mov	[ebx].MCSlen,eax	; resize free entry

AUEexit:
	clc
	pop	edi
	pop	esi
	pop	ecx
	pop	ebx
	pop	eax
	ret

AUEerror:
;int 1	;error
	stc
	pop	edi
	pop	ecx
	pop	ebx
	pop	eax
	ret

AddUsedEntry	endp

;==============================================================================
;==
;==  DelUsedEntry: This procedure deletes a used memory block from the used
;==		   MCS list and places it in the null MCS list.
;==
;==  Entry: (Real Mode)
;==  	ESI = Address of MCS entry
;==
;==  Exit:  (same)
;==
;==============================================================================

DelUsedEntry	proc	near
	push	edi

	mov	edi,offset UsedHC	; unlink from used list
	call	UnlinkEntry

	mov	edi,offset NullHC	; link to null list
	call	LinkEntry

	pop	edi
	ret
DelUsedEntry	endp

;==============================================================================
;==
;==  AddFreeEntry: This procedure adds a free memory block to the free MCS
;==		   linked list.
;==
;==  Entry: (Real Mode)
;==	EAX = Flags in low 16 bits, XMS handle in high 16 bits
;==  	EBX = Base address of memory block
;==	ECX = Length of memory block
;==
;==  Exit:  (same)
;==
;==============================================================================

AddFreeEntry	proc	near
	push	esi
	push	edi

	call	GetNullEntry      	; get an MCS entry

	mov	[esi].MCSbase,ebx       ; fill in info
	mov	[esi].MCSlen,ecx
	mov	dword ptr [esi].MCSflags,eax

	mov	edi,offset FreeHC       ; link into free chain
	call	LinkEntry

	pop	edi
	pop	esi
	ret
AddFreeEntry	endp

;==============================================================================
;==
;==  DelFreeEntry: This procedure deletes a free memory block from the free
;==		   MCS list and places it in the null MCS list.
;==
;==  Entry: (Real Mode)
;==  	ESI = Address of MCS entry
;==
;==  Exit:  (same)
;==
;==============================================================================

DelFreeEntry	proc	near
	push	edi

	mov	edi,offset FreeHC	; unlink from free list
	call	UnlinkEntry

	mov	edi,offset NullHC	; link to null list
	call	LinkEntry

	pop	edi
	ret
DelFreeEntry	endp

;==============================================================================
;==
;==  GetNullEntry: This procedure gets the first null MCS entry available.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:  (Real Mode)
;==  	ESI = Address of MCS entry
;==
;==============================================================================

GetNullEntry	proc	near
	push	edi

	mov	edi,offset NullHC 	; HC for null chain
	movzx	esi,[edi].HCnext	; first entry
	call	UnlinkEntry

	pop	edi
	ret
GetNullEntry    endp

;==============================================================================
;==
;==  UnlinkEntry: Unlinks an entry from a double linked list
;==
;==  Entry: (Real Mode)
;==	ESI = Entry address
;==	EDI = Chain head cell address
;==
;==  Exit:  (Real Mode)
;==  	ESI = Entry address (unlinked from chain)
;==  	[EDI].HCcount = decremented by one
;==
;==============================================================================

UnlinkEntry	proc	near
	push	eax
	push	ebx

	cmp	[edi].HCcount,0		;Q: Any entries
	je      short UEerror		; N: it should never happen!

	movzx	ebx,[esi].MCSnext	; get next MCS address
	movzx	eax,[esi].MCSprev       ; get prev MCS address

	mov	[eax].MCSnext,bx        ; next MCS address in prev's next field
	mov	[ebx].MCSprev,ax        ; prev MCS address in next's prev field

	dec	[edi].HCcount           ; decrement HC count
	clc				; indicate no error

	pop	ebx
	pop	eax
	ret

UEerror:				; just for debugging purposes!
;int 1	;error
	stc
	pop	ebx
	pop	eax
	ret

UnlinkEntry	endp

;==============================================================================
;==
;==  LinkEntry: Link a MCS entry in ascending base address order
;==
;==  Entry: (Real Mode)
;==	ESI = Entry address
;==	EDI = Destination chain head cell address
;==
;==  Exit:  (Real Mode)
;==  	[EDI].HCcount = incremented by one
;==
;==============================================================================

LinkEntry	proc	near
	push	eax
	push	ebx

	mov	ebx,edi			; HC address in ebx
	mov	eax,[esi].MCSbase       ; base address of memory block to link

LEloop:
	movzx	ebx,[ebx].MCSnext       ; get next MCS entry in chain
	cmp	ebx,edi                 ;Q: Is it the HC?
	je	short LElink		; Y: link this entry last
	cmp	[ebx].MCSbase,eax       ;Q: Should we link here?
	jbe	LEloop                  ; N: check next MCS entry
					; Y: link the entry between this one
LElink:                                 ;    and the prevoius one.
	movzx	eax,[ebx].MCSprev       ; get prevoius entry address

	mov	[esi].MCSnext,bx        ; update pointers on entry to be linked
	mov	[esi].MCSprev,ax

	mov	[eax].MCSnext,si        ; update pointers on prev and next entries
	mov	[ebx].MCSprev,si

	inc	[edi].HCcount

	pop	ebx
	pop	eax
	ret
LinkEntry	endp

;=============================================================================
;==
;==  UnProtectROM: Allow writes to system RAM which is mapped in ROM space.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==
;=============================================================================

UnProtectROM 	proc	near

	mov	cs:[RAMRelocBuffer],UNLOCK_ROM	; unprotect command

	call	WriteRAMRelReg

	ret
UnProtectROM	endp

;=============================================================================
;==
;==  ProtectROM: Write protect system RAM which is mapped in ROM space.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==
;=============================================================================

ProtectROM 	proc	near

	mov	cs:[RAMRelocBuffer],LOCK_ROM	; write protect command

	call	WriteRAMRelReg

	ret
ProtectROM 	endp

;=============================================================================
;==
;==  WriteRAMRelReg: Write to RAM Relocation Register (80C00000h).
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==
;=============================================================================

WriteRAMRelReg 	proc	near
	push	esi
	push	edi
	push	cx


	mov	si,seg LAST		; source: [RAMRelocBuffer]
	and	esi,0FFFFh
	shl	esi,4
	add	esi,offset LAST:RAMRelocBuffer

	mov	edi,80C00000h		; destination: RAM Relocation register

	mov	cx,2            	; 1 word transfer

	call	DoMoveBlock

	pop	cx
	pop	edi
	pop	esi
	ret
WriteRAMRelReg	endp

;=============================================================================
;==
;==  DoMoveBlock: Do Int 15h, AH=87h move block.
;==
;==  Entry: (Real Mode)
;==	ESI = Source address (32-bit)
;==	EDI = Destination address (32-bit)
;==	 CX = Byte count
;==
;==  Exit:
;==	(same)
;==     CY = Set if an error occurred
;==
;=============================================================================

DoMoveBlock	proc	near
	pushad
	push	ds
	push	es

	mov	ax,seg LAST
	mov	ds,ax
	mov	es,ax

	mov	eax,esi			; set source descriptor
	call	SetSrcSel

	mov	eax,edi			; set destination descriptor
	call	SetDesSel

	inc	cx			; convert bytes to words
	shr	cx,1

	lea	si,MoveBlockGDT		; es:si points to GDT

	mov	ah,87h			; move block function
	clc
	push	gs			; possible bug in IBM ROM BIOS!
	int	15h  			;Q: Was move block succesful?
	pop	gs
	jc	short DMBerror		; N: report error

	pop	es
	pop	ds
	popad
	ret

DMBerror:
;int 1	;error
	pop	es
	pop	ds
	popad
	ret
DoMoveBlock	endp

;=============================================================================
;==
;==  SetSrcSel: Set source selector in MoveBlockGDT
;==
;==  Entry: (Real Mode)
;==	EAX = 32 bit address of source
;==	 CX = Number of bytes
;==
;==  Exit:
;==	(same)
;==
;=============================================================================

SetSrcSel proc	near
	push	eax
	push	bx

	mov	bx,size sDescriptor * 2		; source descriptor offset

	jmp	short SetSelector
SetSrcSel	endp

;=============================================================================
;==
;==  SetDesSel: Set destination selector in MoveBlockGDT
;==
;==  Entry: (Real Mode)
;==	EAX = 32 bit address of destination
;==	 CX = Number of bytes
;==
;==  Exit:
;==	(same)
;==
;=============================================================================

SetDesSel proc	near
	push	eax
	push	bx

	mov	bx,size sDescriptor * 3		; destination descriptor offset

;
;  Set appropriate selector
;
SetSelector:

	mov	word ptr [MoveBlockGDT][bx][BaseL],ax	; bits 0.15
	shr	eax,16
	mov	byte ptr [MoveBlockGDT][bx][BaseH],al	; bits 16.23
	mov	byte ptr [MoveBlockGDT][bx][BaseX],ah	; bits 24.31

	mov	word ptr [MoveBlockGDT][bx][LimitF],cx	; size in bytes

	pop	bx
	pop	eax
	ret
SetDesSel	endp

;=============================================================================
;==	VDISK Variables: Used for searching previously installed VDISK's
;=============================================================================

VDISK_name	db	'VDISK'		; 1st 5 bytes of header name
VDISK_type	db	28h		; 1st byte of label2 (header type)

pINT19Vec 	label	dword	; Pointer to int 19 Vector in IVT
		dw	19h*4   ; Offset
		dw	0       ; Segment zero

;=============================================================================
;==			BIM Control Structure
;=============================================================================
pBIMCtlStruct  	equ     0FFE0h

BIMCtlStruct  	struc
    AVAILABLE   dw	0	; Set to -1 if BIM isn't around
    TOTALBIM    dw     	0	; Total amount of BIM in the system
    AVAILBIM    dw     	0	; Amount of BIM available in paragraphs
    LASTUSED    dw     	0	; Paragraph address of last (lowest) used
BIMCtlStruct  	ends

;=============================================================================
;==	BIM Variables: Used for locating COMPAQ BIM
;=============================================================================
pCOMPAQ 	label	dword		; Pointer to COMPAQ signature
		dw	0FFE8h
RomSeg		dw	0F000h

szCOMPAQ    	db     '03COMPAQ'	; COMPAQ signature

RAMRelocBuffer	dw	 0

sDescriptor	struc
 LimitF	dw	0 	 		; limit
 BaseL	dw	0       		; base 0..15
 BaseH	db	0       		; base 16.23
	db	0       		; type
	db	0       		; reserved
 BaseX	db	0       		; base 24.31
sDescriptor	ends

MoveBlockGDT	label	dword
	sDescriptor<>
	sDescriptor<>
	sDescriptor<1,0,0,D_DATA3,0,0>	; source selector
	sDescriptor<1,0,0,D_DATA3,0,0>	; destination selector
	sDescriptor<>
	sDescriptor<>

;=============================================================================
;==	Memory Control Structures Double Linked List
;=============================================================================
NUM_MCS_ENTRIES equ	500

FreeHC	sHeadCell	<$,$,0>	; Free memory head cell

UsedHC	sHeadCell 	<$,$,0>	; Used memory head cell

NullHC 	sHeadCell <FirstMCS,LastMCS,NUM_MCS_ENTRIES> ; Null MCS entries list

ALIGN	4			; align in DWORD boundary
MCSPool	label	byte            ; start of MCS pool

FirstMCS sMemConStr <$+size sMemConStr,NullHC,0,0,0,0> ; first MCS entry
rept	NUM_MCS_ENTRIES-2
 sMemConStr <$+size sMemConStr,$-size sMemConStr,0,0,0,0> ; Space for MCS entries
endm
LastMCS sMemConStr <NullHC,$-size sMemConStr,0,0,0,0>  ; last MCS entry

LAST	ends
	end
