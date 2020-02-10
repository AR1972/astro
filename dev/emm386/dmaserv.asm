.386p
	page 58,132
;=============================================================================
	title	D M A S E R V - DMA/bus master services under Virtual 8086 mode
;=============================================================================
;==
;== (C) Copyright MICROSOFT Corp. 1989-1991
;== (C) Copyright COMPAQ Computer Corp. 1989-1991
;==
;==	Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;==
;==	Module: DMAServ  - DMA/bus master services under Virtual 8086 mode
;==
;==	Version: 1.00
;==
;==	Date:	September 22,1989
;==
;==	Author: Leo Cohen
;==
;=============================================================================
;==
;==	Change Log:
;==
;==	DATE	 REVISION	Description
;==	-------- --------	--------------------------------------------
;==     09/22/89 0.00	        Original
;==
;=============================================================================
;==
;==   Functional Description:
;==
;==   This module provides DMA/bus master services while in Virtual 8086 mode.
;==
;=============================================================================
;==	P U B L I C   D E C L A R A T I O N S
;=============================================================================
	public	pINT4BhHandler
;;	public	rINT4BhHandler
;=============================================================================
;==	L O C A L   C O N S T A N T S
;=============================================================================
	include allocmem.inc
	include vdmseg.inc
	include vdmsel.inc
	include desc.inc
	include dma.inc
	include page.inc
	include oemdep.inc
	include vm386.inc
	include	emm386.inc
	include	emmfunct.inc
	include	emmdata.inc
;=============================================================================
;==	E X T E R N A L   R E F E R E N C E S
;=============================================================================
_TEXT	segment
	extrn	ReflectInterrupt:near
	extrn	MoveBuffer:near
	extrn	ContigCheck:near
_TEXT	ends

_DATA	segment
	extrn	DMARegSav:byte
_DATA	ends
;=============================================================================
;==	L O C A	L   D A T A
;=============================================================================
_DATA	segment

DMABuffID   dw	0		; Current DMA buffer ID
DBSDisTable db	8 dup (0)	; disable translation count per channel

DBSTable label	word
	dw	offset _TEXT:DBSInvalidFunction	; AL=0  reserved
	dw	offset _TEXT:DBSInvalidFunction	; AL=1  reserved
	dw	offset _TEXT:DBSGetVersion	; AL=2  Get Version Function
	dw	offset _TEXT:DBSLockDMARegion	; AL=3  Lock DMA Region
	dw	offset _TEXT:DBSUnlockDMARegion	; AL=4  Unlock DMA Region
	dw	offset _TEXT:DBSScatGathLock	; AL=5  Scatter/Gather Lock Region
	dw	offset _TEXT:DBSScatGathUnlock	; AL=6  Scatter/Gather Unlock Region
	dw	offset _TEXT:DBSReqDMABuffer	; AL=7  Request DMA Buffer
	dw	offset _TEXT:DBSRelDMABuffer	; AL=8  Release DMA Buffer
	dw	offset _TEXT:DBSCopyToBuffer	; AL=9  Copy to DMA Buffer
	dw	offset _TEXT:DBSCopyFromBuffer	; AL=10 Copy from DMA Buffer
	dw	offset _TEXT:DBSDisDMATrans	; AL=11 Disable DMA Translation
	dw	offset _TEXT:DBSEnaDMATrans	; AL=12 Enable DMA Translation
DBSLastFunc	equ	($-DBSTable)/2
	dw	offset _TEXT:DBSInvalidFunction	; AL=>13 reserved
_DATA	ends
;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
_TEXT	segment
	assume	cs:_TEXT,ds:_DATA,es:_DATA,ss:STACK
;==============================================================================
;==
;==  DBSdipatcher - Dispatches DMA/bus master virtual mode services functions
;==
;==  Entry: (Protected Mode Ring 0)
;==	AH = 81h
;==	AL = Function number
;==	DX = Flags
;==	ES:EDI    = DMA Descriptor Structure (DDS)
;==	BX:CX/ECX = May be a buffer offset
;==	EBP= Virtual mode stack 0 image
;==	DS = _DATA
;==	GS = R_CODE
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL = Status
;==	CY = clear if OK, set if error
;==
;==============================================================================
DBSdispatcher	proc	near

	cmp	al,2			;Q: Get version function?
	je	short DBSGetVersion	; Y: go get it
					; N: dispatch to proper function
	pushad
	movzx	esp,sp			; access only 64K
	mov	[ebp][-4],esp		; pointer to PUSHAD image
	push	es

	push	DATA32_GSEL		; access all of memory
	pop	es

	cmp	al,DBSLastFunc		;Q: Valid function?
	jb	short DBSDindex		; Y: index into function
	mov	al,DBSLastFunc		; N: invalid function

DBSDindex:
	and	eax,0FFh		; clear all but function number
	call	[DBSTable][eax*2]       ; vector to proper function
	mov	ebx,[ebp][-4]		; pointer to PUSHAD image
	mov	byte ptr ss:[ebx].reg_EAX,al ; return correct status code

	and	[ebp][VTFO].VMTF_EFLAGS,not FLAGS_CF ; assume success
	or	al,al				     ;Q: Need to set carry?
	jz	short DBSDexit			     ; N: exit
	or	[ebp][VTFO].VMTF_EFLAGS,FLAGS_CF     ; Y: set carry

DBSDexit:
	pop	es
	popad
	ret
DBSdispatcher	endp
;==============================================================================
;==
;==  DBSInvalidFunction - Invalid function number.
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = 0,1,13,14,15,...
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL = 15, illegal function
;==	CY = set
;==
;==============================================================================
DBSInvalidFunction proc	near

	mov	al,eDBSInvalidFunc	; error return, invalid function
	ret

DBSInvalidFunction	endp
;==============================================================================
;==
;==  DBSGetVersion - returns version of the DMA bus/master services.
;==
;==  Entry: (Real Mode)
;==	AL = 2
;==	DX = 0
;==
;==  Exit:  (Real Mode)
;==	AH = Major version
;==	AL = Minor version
;==	BH = OEM number
;==	BL = OEM revision
;==
;==	SI:DI = Maximum DMA buffer size
;==	DX = Bit 1 set: if DMA buffer is located below 1 megabyte
;==
;==============================================================================
fDBSLegal=0
DBSGetVersion	proc	near

	test	dx,not fDBSLegal	;Q: Illegal flags used?
	jnz	short DGVbadFlags	; Y: return error

	mov	ah,DBSMajVer		; major version
	mov	al,DBSMinVer		; minor version

	mov	di,word ptr [DMABufferSize]	; size of DMA buffer
	mov	si,word ptr [DMABufferSize+2]
	mov	bx,DBSNumOEM		; OEM number
	mov	cx,DBSRevOEM		; OEM revision

	xor	dx,dx			; no flags
	test	[DMAFlags],fDMABuffXT	;Q: Buffer below 1 meg?
	jz	short DGVexit		; N: exit
	mov	dx,DBSDMAXT		; Y: PC/XT architecture supported

DGVexit:
	and	[ebp][VTFO].VMTF_EFLAGS,not FLAGS_CF ; success: clear carry
	ret

DGVbadFlags:
	mov	al,eDBSInvalidFlag		; error return
	or	[ebp][VTFO].VMTF_EFLAGS,FLAGS_CF; set carry
	ret
DBSGetVersion	endp
;==============================================================================
;==
;==  DBSLockDMARegion - Attempts to get a physical buffer.
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = 3
;==	DX = Flags: bit
;==		    1 = copy to buffer (invalid: 2=1/DMA buffer not used)
;==		    2 = If not contiguous DMA buffer should not be allocated
;==		    3 = auto remap invalid
;==		    4 = 64K aligned
;==		    5 = 128K aligned
;==	ES:(E)DI = pointer to DDS
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL = status value
;==	CY clear:   0 = OK
;==	CY set:	    1 = region not contiguous
;==		    2 = alignment crossed
;==		    3 = unable to lock pages
;==		    4 = no buffer available
;==		    5 = DMA buffer is too small
;==		    6 = DMA buffer currently in use
;==		   10h= illegal flags requested
;==
;==============================================================================
fDBSLegal=fDBSBufferCopy+fDBSDisAutoBuff+fDBSDisAutoRem+fDBS64kAlign+fDBS128kAlign
DBSLockDMARegion proc	near

	test	dx,not fDBSLegal	;Q: Illegal flags used?
	jz	short DLDRptr		; N: continue
	mov	al,eDBSInvalidFlag	; Y: error return
	jmp	DLDRexit

DLDRptr:
	movzx	edi,di			; convert to 32 bit pointer
	call	GetDDSPtr		; convert V86 ES:EDI to protected ES:EBX
	mov	es:[ebx].DDSBuffID,0	; asssume DMA buffer not used

	call	GetUserBufferPtr	; ESI/ECX is user buffer ptr/size
	mov	eax,esi

	mov	edi,edx			; save flags
	xor	edx,edx			; assume no boundary restrictions

	test	edi,fDBS64kAlign+fDBS128kAlign ;Q: Any boundary conditions?
	jz	short DLDRbound		       ; N: continue with no restrictions
	mov	edx,20000h		       ; Y: assume a 128K restriction

	test	edi,fDBS64kAlign	;Q: Is it a 64K restriction?
	jz	short DLDRbound		; N: go with 128K restriction
	shr	edx,1			; Y: change to 64K restriction

DLDRbound:
	call	ContigCheck		;Q: Is user buffer OK?
	jnc	short DLDRok		; Y: easy, fill in DDS
	xchg	ecx,es:[ebx].DDSSize	; N: contiguous size in DDS

	test	edi,fDBSDisAutoBuff	;Q: Disable auto DMA buffer allocation?
	jz	short DLDRsizeDMAbuff	; N: check if DMA buffer size is OK
	mov	al,eDBSNotContig	; Y: assume user buffer not contiguous

	or	edx,edx			;Q: Boundary crossing?
	jz	short DLDRexit		; N: buffer is not contiguous
	mov	al,eDBSBoundCross	; Y: boundary crossed error code
	jmp	short DLDRexit

DLDRsizeDMAbuff:
	cmp	ecx,[DMABufferSize]	;Q: Smaller than DMA buffer
	jbe	short DLDRcheckDMAbuff	; Y: check DMA buffer status
	mov	al,eDBSBufferSize	; N: can't use DMA buffer due to size
	jmp	short DLDRexit		;    being too small

DLDRcheckDMAbuff:
	bts	[DMAFlags],fDMABufferBusyBit ;Q: Is the DMA buffer in use?
	jnc	short DLDRallocDMAbuff	     ; N: allocate it
	mov	al,eDBSBufferBusy	     ; Y: return DMA buffer busy status
	jmp	short DLDRexit

DLDRallocDMAbuff:
	mov	es:[ebx].DDSSize,ecx	; restore original buffer size
	mov	edx,edi			; restore flags

	test	edx,fDBSBufferCopy	;Q: Need to copy data to DMA buffer?
	jz	short DLDRuseDMAbuff	; N: use DMA buffer (get address)
	call	GetUserBufferPtr	; Y: get source address and size
	mov	edi,[DMABufferAddress]	;    get destination
	call	MoveBuffer

DLDRuseDMAbuff:
	call	GetBufferID		; get a Buffer ID
	mov	es:[ebx].DDSBuffID,ax	; fill in DMA buffer ID
	mov	eax,[DMABufferAddress]	; DMA buffer physical address

DLDRok:
	mov	es:[ebx].DDSPhyAddr,eax	; fill in physical address in DDS
	xor	al,al			; no error

DLDRexit:
	ret

DBSLockDMARegion	endp
;==============================================================================
;==
;==  DBSUnlockDMARegion - Unlocks previously locked buffer.
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = 4
;==	DX = Flags: bit
;==		    1 = copy from buffer
;==	ES:(E)DI = pointer to DDS
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL = status value
;==	CY clear:   0h = OK
;==	CY set:	   08h = memory was not locked
;==		   0Ah = invalid buffer ID
;==		   10h = illegal flags requested
;==
;==============================================================================
fDBSLegal=fDBSBufferCopy
DBSUnlockDMARegion proc	near

	test	dx,not fDBSLegal	;Q: Illegal flags used?
	jz	short DUDRptr		; N: continue
	mov	al,eDBSInvalidFlag	; Y: error return
	jmp	short DUDRexit

DUDRptr:
	movzx	edi,di			; convert to 32 bit pointer
	call	GetDDSPtr		; convert V86 ES:EDI to protected ES:EBX
	mov	edi,[DMABufferAddress]	; get address of DMA buffer

	cmp	es:[ebx].DDSPhyAddr,edi	;Q: DMA buffer address?
	je	short DUDRbuffID	; Y: check for DMA buffer ID

	test	dx,fDBSBufferCopy	;Q: Buffer copy flag?
	jnz	short DUDRInvBuffID	; Y: error, invalid buffer ID

	cmp	es:[ebx].DDSBuffID,0	;Q: Non-DMA buffer ID?
	je	short DUDRok		; Y: unlock region
DUDRInvBuffID:
	mov	al,eDBSInvalidBuff	; N: invalid buffer ID
	jmp	short DUDRexit

DUDRbuffID:
	mov	ax,es:[ebx].DDSBuffID
	call	ValidateBufferID	;Q: DMA buffer ID?
	je	short DUDRcheck		; Y: check if it is busy?
	mov	al,eDBSInvalidBuff	; N: invalid buffer ID
	jmp	short DUDRexit

DUDRcheck:
	test	[DMAFlags],fDMABufferBusy ;Q: Is DMA buffer currently busy?
	jnz	short DUDRcont		  ; Y: unlock it
	mov	al,eDBSLockRegion	  ; N: DMA buffer was never locked
	jmp	short DUDRexit

DUDRcont:
	test	dx,fDBSBufferCopy	;Q: Need to copy data from buffer?
	jz	short DUDRunlock	; N: unlock DMA buffer

	call	GetUserBufferPtr	; ESI/ECX is user buffer ptr/size
	xchg	esi,edi			; DMA buffer -> user buffer

	call	MoveBuffer

DUDRunlock:
	and	[DMAFlags],not fDMABufferBusy ; DMA buffer no longer in use

DUDRok:
	xor	al,al			; success

DUDRexit:
	ret

DBSUnlockDMARegion	endp
;==============================================================================
;==
;==  DBSScatGathLock - Attempts to lock a scatter/gather region.
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = 5
;==	DX = Flags: bit
;==		    6 = PTE's should be used
;==		    7 = not present pages should not be locked
;==	ES:(E)DI = pointer to extended DDS
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL = status value
;==	BX = offset into first PTE for region (only if bit 6 of flags is set).
;==	CY clear:   0 = OK
;==	CY set:	    3 = unable to lock pages
;==		    9 = # scatter/gather regions is greater than table size
;==		   10h= illegal flags requested
;==
;==============================================================================
fDBSLegal=fDBSPTEs+fDBSNotPresent
DBSScatGathLock proc	near
	push	fs

	test	dx,not fDBSLegal	;Q: Illegal flags used?
	jz	short DSGLptr		; N: continue
	mov	al,eDBSInvalidFlag	; Y: error return
	xor	si,si			; no available entries used
	jmp	DSGLexit

DSGLptr:
	movzx	edi,di			; convert to 32 bit pointer
	call	GetDDSPtr		; convert V86 ES:EDI to protected ES:EBX
 	movzx	edi,es:[ebx].DDSEAvail	; available entries

	call	GetUserBufferPtr	; ESI/ECX is user buffer ptr/size
	push	es:[ebx].DDSsize	; save size field for succesfull return
	mov	es:[ebx].DDSsize,0	; clear size field in case of error
 	mov	es:[ebx].DDSEUsed,0	; used entries

	test	edx,fDBSPTEs		;Q: Array of PTE's?
	jnz	short DSGLPTEs		; Y: list PTE entries
;
;  Each entry in the array is composed of a physical address and a size for
;  which it is contiguous.  Each are DWORD fields.
;
	mov	eax,esi			; user buffer linear address
	xor	edx,edx			; no boundary restrictions
	xor	esi,esi

DSGLnextRegion:
	push	ecx			; save size to transfer
	push	eax			; save linear address

	call	ContigCheck		; get contiguous buffer

	cmp	esi,edi			;Q: Any entries left?
	jae	short DSGLRegCont	; N: don't record information

	mov	es:[ebx][DDSElist][esi*8].DDSERPhyAddr,eax ; save address
	mov	es:[ebx][DDSElist][esi*8].DDSERsize,ecx	   ; save size
	add	es:[ebx].DDSsize,ecx	; amount of buffer described by entries

DSGLRegCont:
	inc	esi			; next region
	pop	eax
	add	eax,ecx			; new linear address

	sub	[esp],ecx		;Q: Still need more?
	pop	ecx			; amount still needed
	jnz	short DSGLnextRegion	; Y: get next region
	jmp	short DSGLcomplete	; N: all regions completed

;
;  Each entry in the array is composed of a PTE.
;
DSGLPTEs:
	mov	ax,PAGET_GSEL		; access page tables
	mov	fs,ax
	xor	eax,eax

	xchg	eax,esi
	add	ecx,eax
	or	ecx,ecx			;Q: End of buffer @ 0?
	jz	short DSGLcont		; Y: don't wrap on 4GB?
	dec	ecx			; N: last addressable byte in buffer
DSGLcont:
	push	eax			; save starting address

	shr	eax,12			; starting PTE index
	shr	ecx,12			; ending PTE index

DSGLloop:
	cmp	esi,edi			;Q: Any entries left?
	jae	short DSGLPTEcont	; N: don't record information

	mov	edx,eax			; assume linear and physical are the same
	shl	edx,12
	cmp	eax,[MaxPTEIndex]	;Q: Above the page tables?
	jae	short DSGLphys		; Y: linear and physical are the same
	mov	edx,fs:[eax*4]		; N: get PTE
DSGLphys:
	and	dx,0F000h
	or	dx,1			; page present and locked bit
	mov	es:[ebx][DDSElist][esi*4].DDSEP,edx ; save PTE
	add	es:[ebx].DDSsize,1000h	; amount of buffer described by entries

DSGLPTEcont:
	inc	eax
	inc	esi
	cmp	eax,ecx			;Q: Need any more PTEs
	jbe	short DSGLloop		; Y: get next PTE

	pop	eax			; restore starting buffer address
	and	eax,0FFFh		; clear upper 20 bits

	mov	edx,[ebp][-4]		     ; pointer to PUSHAD image
	mov	word ptr ss:[edx].reg_EBX,ax ; offset of buffer in first PTE

	or	edi,edi			;Q: Any entries available?
	jz	short DSGLcomplete	; N: don't update size field
	sub	es:[ebx].DDSsize,eax	; Y: reduce by offset into first PTE

DSGLcomplete:
	pop	ecx			; size field for succesfull return
	mov	es:[ebx].DDSEUsed,si	; number of entries required
	mov	al,eDBSTableSize	; assume need more space error code

	cmp	esi,edi			;Q: More entries required than available?
	ja	short DSGLexit		; Y: need more space error
	xor	al,al			; N: no error
	mov	es:[ebx].DDSsize,ecx	; restore original size field: succesfull return

DSGLexit:
	pop	fs
	ret

DBSScatGathLock	endp
;==============================================================================
;==
;==  DBSScatGathUnlock - Attempts to unlock a scatter/gather region.
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = 6
;==	DX = Flags: bit
;==		    6 = PTE's should be used
;==		    7 = not present pages should not be locked
;==	ES:(E)DI = pointer to extended DDS
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL = status value
;==	CY clear:   0 = OK
;==	CY set:	    7 = invalid memory region
;==		    8 = memory was not locked
;==		   10h= illegal flags requested
;==
;==============================================================================
fDBSLegal=fDBSPTEs+fDBSNotPresent
DBSScatGathUnlock proc	near

	test	dx,not fDBSLegal	;Q: Illegal flags used?
	jnz	short DSGUerror		; Y: error
	xor	al,al			; N: buffer unlocked
	ret

DSGUerror:
	mov	al,eDBSInvalidFlag	; error return
	ret
DBSScatGathUnlock	endp
;==============================================================================
;==
;==  DBSReqDMABuffer - Request a DMA Buffer
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = 7
;==	DX = Flags: bit
;==		    1 = copy to buffer
;==	ES:(E)DI = pointer to DDS
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL = status value
;==	CY clear:   0 = OK
;==	CY set:	    4 = no buffer available
;==		    5 = DMA buffer is too small
;==		    6 = DMA buffer currently in use
;==		   10h= illegal flags requested
;==
;==============================================================================
fDBSLegal=fDBSBufferCopy
DBSReqDMABuffer proc	near

	test	dx,not fDBSLegal	;Q: Illegal flags used?
	jz	short DRqDBptr		; N: continue
	mov	al,eDBSInvalidFlag	; Y: error return
	jmp	short DRqDBexit

DRqDBptr:
	movzx	edi,di			  ; convert to 32 bit pointer
	test	[DMAFlags],fDMABufferBusy ;Q: Is DMA buffer currently busy?
	jz	short DRqDBbuffer   	  ; N: get user buffer pointer
	mov	al,eDBSBufferBusy	  ; Y: DMA buffer is busy
	jmp	short DRqDBexit

DRqDBbuffer:
	call	GetDDSPtr		; convert V86 ES:EDI to protected ES:EBX

	mov	ecx,[DMABufferSize]
	cmp	ecx,es:[ebx].DDSsize	;Q: Is the DMA buffer large enough?
	jae	short DRqDBUseBuffer	; Y: use the DMA buffer
	mov	al,eDBSBufferSize	; N: DMA buffer is too small
	jmp	short DRqDBexit

DRqDBUseBuffer:
	or	[DMAFlags],fDMABufferBusy ; mark DMA buffer in use
	call	GetBufferID               ; get a unique buffer ID
	mov	es:[ebx].DDSBuffID,ax	  ; DMA buffer ID
;QLEO	mov	es:[ebx].DDSsize,ecx	  ; specify the size of the DMA buffer

	mov	edi,[DMABufferAddress]	; get address of DMA buffer
	mov	es:[ebx].DDSPhyAddr,edi ; put in DDS

	test	dx,fDBSBufferCopy	;Q: Need to copy data to DMA buffer?
	jz	short DRqDBok		; N: return

	call	GetUserBufferPtr	; ESI/ECX is user buffer ptr/size

	call	MoveBuffer

DRqDBok:
	xor	al,al			; success

DRqDBexit:
	ret

DBSReqDMABuffer	endp
;==============================================================================
;==
;==  DBSRelDMABuffer - Release DMA Buffer
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = 8
;==	DX = Flags: bit
;==		    0 = 32 bit addressing mode used (ES:EDI)
;==		    1 = copy from buffer
;==	ES:(E)DI = pointer to DDS
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL = status value
;==	CY clear:   00h = OK
;==	CY set:	    0Ah = invalid buffer ID
;==		    10h = illegal flags requested
;==
;==============================================================================
fDBSLegal=fDBSBufferCopy
DBSRelDMABuffer proc	near

	test	dx,not fDBSLegal	;Q: Illegal flags used?
	jz	short DRDBptr		; N: continue
	mov	al,eDBSInvalidFlag	; Y: error return
	jmp	short DRDBexit

DRDBptr:
	movzx	edi,di			; convert to 32 bit pointer
	call	GetDDSPtr		; convert V86 ES:EDI to protected ES:EBX
	mov	edi,[DMABufferAddress]	; get address of DMA buffer

	mov	ax,es:[ebx].DDSBuffID
	call	ValidateBufferID	;Q: DMA buffer ID?
	jne	short DRDBerror		; N: invalid buffer ID

	test	[DMAFlags],fDMABufferBusy ;Q: Is DMA buffer currently busy?
	jz	short DRDBerror		  ; N: invalid buffer ID

	test	dx,fDBSBufferCopy	;Q: Need to copy data from buffer?
	jz	short DRDBunlock	; N: unlock DMA buffer

	call	GetUserBufferPtr	; ESI/ECX is user buffer ptr/size
	xchg	esi,edi			; DMA buffer -> user buffer

	call	MoveBuffer

DRDBunlock:
	and	[DMAFlags],not fDMABufferBusy ; DMA buffer no longer in use

DRDBok:
	xor	al,al			; success

DRDBexit:
	ret

DRDBerror:
	mov	al,eDBSInvalidBuff	; DMA buffer was never locked
	ret

DBSRelDMABuffer	endp
;==============================================================================
;==
;==  DBSCopyToBuffer - Copy data to DMA buffer
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = 9
;==	DX = Flags: bit (must be zero)
;==	ES:(E)DI  = pointer to DDS
;==	BX:CX/ECX = starting offset into DMA buffer
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL = status value
;==	CY clear:   00h = OK
;==	CY set:	    0Ah = invalid buffer ID
;==		    0Bh = copy count + offset is larger than DMA buffer size
;==		    10h = illegal flags requested
;==
;==============================================================================
fDBSLegal=0
DBSCopyToBuffer proc	near

	test	dx,not fDBSLegal	;Q: Illegal flags used?
	jz	short DCTBptr		; N: continue
	mov	al,eDBSInvalidFlag	; Y: error return
	jmp	short DCTBexit

DCTBptr:
	movzx	edi,di			; convert to 32 bit pointer
	shl	ebx,16			; convert BX:CX -> ECX
	mov	bx,cx
	mov	ecx,ebx			; offset into DMA buffer

	call	GetDDSPtr		; convert V86 ES:EDI to protected ES:EBX
	mov	edi,[DMABufferAddress]	; get address of DMA buffer
	add	edi,ecx			; starting transfer address

	mov	ax,es:[ebx].DDSBuffID
	call	ValidateBufferID	;Q: DMA buffer ID?
	jne	short DCTBerrID		; N: invalid buffer ID

	add	ecx,es:[ebx].DDSsize	; size of DMA buffer needed for transfer
	cmp	[DMABufferSize],ecx	;Q: DMA buffer large enough?
	jb	short DCTBerrCount	; N: error

	call	GetUserBufferPtr	; ESI/ECX is user buffer ptr/size

	call	MoveBuffer
	xor	al,al

DCTBexit:
	ret

DCTBerrID:
	mov	al,eDBSInvalidBuff	; invalid buffer ID
	ret

DCTBerrCount:
	mov	al,eDBSBuffRange	; copy range not within DMA buffer
	ret

DBSCopyToBuffer	endp
;==============================================================================
;==
;==  DBSCopyFromBuffer - Copy data from DMA buffer
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = 10
;==	DX = Flags: bit (must be zero)
;==	ES:(E)DI  = pointer to DDS
;==	BX:CX/ECX = starting offset into DMA buffer
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL = status value
;==	CY clear:   00h = OK
;==	CY set:	    0Ah = invalid buffer ID
;==		    0Bh = copy count + offset is larger than DMA buffer size
;==		    10h = illegal flags requested
;==
;==============================================================================
fDBSLegal=0
DBSCopyFromBuffer proc	near

	test	dx,not fDBSLegal	;Q: Illegal flags used?
	jz	short DCFBptr		; N: continue
	mov	al,eDBSInvalidFlag	; Y: error return
	jmp	short DCFBexit

DCFBptr:
	movzx	edi,di			; N: convert to 32 bit pointer
	shl	ebx,16			; convert BX:CX -> ECX
	mov	bx,cx
	mov	ecx,ebx			; offset into DMA buffer

	call	GetDDSPtr		; convert V86 ES:EDI to protected ES:EBX
	mov	edi,[DMABufferAddress]	; get address of DMA buffer
	add	edi,ecx			; starting transfer address

	mov	ax,es:[ebx].DDSBuffID
	call	ValidateBufferID	;Q: DMA buffer ID?
	jne	short DCFBerrID		; N: invalid buffer ID

	add	ecx,es:[ebx].DDSsize	; size of DMA buffer needed for transfer
	cmp	[DMABufferSize],ecx	;Q: DMA buffer large enough?
	jb	short DCFBerrCount	; N: error

	call	GetUserBufferPtr	; ESI/ECX is user buffer ptr/size
	xchg	esi,edi			; DMA buffer -> user buffer

	call	MoveBuffer
	xor	al,al

DCFBexit:
	ret

DCFBerrID:
	mov	al,eDBSInvalidBuff	; invalid buffer ID
	ret

DCFBerrCount:
	mov	al,eDBSBuffRange	; copy range not within DMA buffer
	ret
DBSCopyFromBuffer	endp
;==============================================================================
;==
;==  DBSDisDMATrans - Disable DMA Translation
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = 11
;==	BX = DMA channel number
;==	DX = 0
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL = status value
;==	CY clear:   00h = OK
;==	CY set:	    0Ch = invalid DMA channel number
;==		    0Dh = disable count overflow
;==		    10h = illegal flags requested
;==
;==============================================================================
fDBSLegal=0
DBSDisDMATrans	proc	near

	test	dx,not fDBSLegal	;Q: Illegal flags used?
	jz	short DDDTptr		; N: continue
	mov	al,eDBSInvalidFlag	; Y: error return
	jmp	short DDDTexit

DDDTptr:
	cmp	bx,8			;Q: Valid DMA channel?
	jae	short DDDTerrChannel	; N: error

	add	DBSDisTable[bx],1	;Q: Disable count overflow?
	jc	short DDDTerrOver	; Y: error
	xor	al,al			; N: successful status

	cmp	DBSDisTable[bx],1	;Q: Was disable count = 0 ?
	jne	SHORT DDDTexit		; N: exit, disable flag already set

	mov	cx,bx
	xor	bx,bx
	jcxz	short DDDTRegRec

DDDTloop:
	add	bx,size DMARegRec
	loop	DDDTloop

DDDTRegRec:
	or	[DMARegSav][bx].DMAChnFlgs,fNoTrans ; set disable trans flag

DDDTexit:
	ret


DDDTerrOver:
	mov	DBSDisTable[bx],-1	; maintain translation disabled
	mov	al,eDBSCountOver	; overflow
	ret

DDDTerrChannel:
	mov	al,eDBSInvalidDMA	; invalid DMA channel
	ret

DBSDisDMATrans	endp
;==============================================================================
;==
;==  DBSEnaDMATrans - Enable DMA Translation
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = 12
;==	BX = DMA channel number
;==	DX = 0
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL = status value
;==	CY clear:   00h = OK (ZF, if count is zero, else NZ)
;==	CY set:	    0Ch = invalid DMA channel number
;==		    0Dh = disable count underflow
;==		    10h = illegal flags requested
;==
;==============================================================================
fDBSLegal=0
DBSEnaDMATrans	proc	near

	test	dx,not fDBSLegal	;Q: Illegal flags used?
	jz	short DEDTptr		; N: continue
	mov	al,eDBSInvalidFlag	; Y: error return
	jmp	short DEDTexit

DEDTptr:
	cmp	bx,8			;Q: Valid DMA channel?
	jae	short DEDTerrChannel	; N: error

	and	[ebp][VTFO].VMTF_EFLAGS,not FLAGS_ZF ; assume count not zero

	sub	DBSDisTable[bx],1	;Q: Disable translation?
	jc	short DEDTerrUnder	; -: error, underflow
	jne	short DEDTok		; N: just decrement disable count

	or	[ebp][VTFO].VMTF_EFLAGS,FLAGS_ZF ; count is zero

	mov	cx,bx
	xor	bx,bx
	jcxz	short DEDTRegRec

DEDTloop:
	add	bx,size DMARegRec
	loop	DEDTloop

DEDTRegRec:
	and	[DMARegSav][bx].DMAChnFlgs,not fNoTrans	; translation enabled

DEDTok:
	xor	al,al

DEDTexit:
	ret

DEDTerrUnder:
	mov	DBSDisTable[bx],0	; maintain translation enabled
	mov	al,eDBSCountUnder	; overflow
	ret

DEDTerrChannel:
	mov	al,eDBSInvalidDMA	; invalid DMA channel
	ret

DBSEnaDMATrans	endp
;==============================================================================
;==
;==  GetBufferID - Returns a unique buffer ID in AX.
;==
;==  Entry: (Protected Mode Ring 0)
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AX = Unique Buffer ID
;==
;==============================================================================
GetBufferID	proc	near
	mov	ax,[DMABuffID]		; get previous DMA ID
	inc	ax			;Q: ID of Zero?
	jnz	short GBIDcont		; N: OK, current ID
	inc	ax			; Y: make current ID a one.
GBIDcont:
	mov	[DMABuffID],ax		; save current ID
	ret
GetBufferID	endp
;==============================================================================
;==
;==  ValidateBufferID - Verifies a valid buffer ID in AX. (This routine is
;==			provided for easy expandability to multiple DMA
;==			buffers)
;==
;==  Entry: (Protected Mode Ring 0)
;==	AX = Proposed buffer ID
;==
;==  Exit:  (Protected Mode Ring 0)
;==	ZR = Buffer ID is valid
;==	NZ = Buffer ID is invalid
;==
;==============================================================================
ValidateBufferID proc	near
	cmp	ax,[DMABuffID]		;Q: Valid buffer ID? (CY- if invalid)
	ret
ValidateBufferID	endp
;==============================================================================
;==
;==  GetDDSPtr - Returns ES:EBX pointing to DDS.
;==
;==  Entry: (Protected Mode Ring 0)
;==	EDI = offset into virtual ES segment
;==	EBP = base pointer for virtual stack frame
;==
;==  Exit:  (Protected Mode Ring 0)
;==	EBX = pointer to DDS
;==
;==============================================================================
GetDDSPtr proc	near
	movzx	ebx,[ebp].VTFO+VMTF_ES	; get virtual mode segment
	shl	ebx,4
	add	ebx,edi			; DDS pointer
	ret
GetDDSPtr	endp
;==============================================================================
;==
;==  GetUserBufferPtr - Returns ES:ESI pointing to user buffer.
;==
;==  Entry: (Protected Mode Ring 0)
;==	EBX = pointer to DDS
;==
;==  Exit:  (Protected Mode Ring 0)
;==	ESI = pointer to user buffer
;==	ECX = size of user buffer
;==
;==============================================================================
GetUserBufferPtr proc	near
	movzx	esi,es:[ebx].DDSseg	; get segment for buffer
	shl	esi,4
	add	esi,es:[ebx].DDSLinOffs	; user buffer pointer

	mov	ecx,es:[ebx].DDSsize	; user buffer size
	ret
GetUserBufferPtr	endp
;==============================================================================
;==
;==  pINT4BhHandler: DMA/bus master virtual 8086 mode services
;==
;==  Entry: (Protected Mode)
;==
;==  Exit:
;==
;==============================================================================
pINT4BhHandler:
	push	ebp
	movzx	ebp,sp
	sub	esp,4		; used to store PUSHAD pointer in dispatcher

	push	VDMD_GSEL	; DS/GS are setup for CEMM's 2 data areas.
	pop	ds
	push	RCODEA_GSEL
	pop	gs
	ASSUME	ds:_DATA,gs:R_CODE

	btr	gs:[TrapFlags],fI4BtrapBit ;Q: Reflect back?
	jnc	short pI4Breflect	   ; Y: reflect interrupt

	call	DBSdispatcher		; dispatch function

	add	esp,4
	pop	ebp
	iretd

pI4Breflect:
	add	esp,4
	push	4Bh			; reflect interrupt into virtual mode
	jmp	ReflectInterrupt

_TEXT	ends				; end of segment


	end				; end of module
