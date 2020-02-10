.386p
	page 58,132
;=============================================================================
	title	X M S - XMS routines/support
;=============================================================================
;==
;== (C) Copyright MICROSOFT Corp. 1990-1991
;== (C) Copyright COMPAQ Computer Corp. 1990-1991
;==
;==	Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;==
;==	Module: XMS  - support routines for XMS
;==
;==	Version: 1.00
;==
;==	Date:	July 20,1990
;==
;==	Author: Leo Cohen
;==	(some routines extracted from XMM.ASM and A20TRAP.ASM: PC &
;==						               Harish Naidu)
;==
;=============================================================================
;==
;==	Change Log:
;==
;==	DATE	 REVISION	Description
;==	-------- --------	--------------------------------------------
;==     07/20/90 0.00	        Original (several routines from old modules)
;==
;==	01/30/91 M005		Hook QueryA20 (function 07h) also.
;==
;==	02/14/91 M009		B#5875. Forced EMM386 to use XMS for
;==				A20 control.
;==
;=============================================================================
;==
;==   Functional Description:
;==
;==   This module includes all support routines needed for XMS.
;==
;=============================================================================
;==	P U B L I C   D E C L A R A T I O N S
;=============================================================================
	public	pXMMentry
	public	XMMcheck
	public	XMMcontrol	; all XMM calls uses this dword address

	public	QueryA20	; query A20 state
	public	XmsVer
	public	HimemVer
	public	get_XMM_ver
	public	XMMQueryExtended
	public	UMBlink
	public	AvailUMB
	public	LargestUMB
	public	HMAmin
	public	HMAfree

ifdef MSFLAG
	public	EnableCount
endif
;=============================================================================
;==	E X T E R N A L  D E C L A R A T I O N S
;=============================================================================
LAST	segment
	extrn	VxDInit:near
	extrn	UMBtable:byte
	extrn	NumOfUMBwindows:byte
	extrn	Page4K:byte
	extrn	RAMSet:word

	extrn	XmmControlBase:dword	; base address of XMM control headers
	extrn	XmmControlJmpVal:byte	; offset byte of short jmp instruction

	extrn	XMMHookAddr:dword

LAST	ends

R_CODE	segment

EnableCount	dw	0	; A20 enable/disable counter
XMMcontrol	dd	0	; address of XMM entry point

	extrn	XMMAllocHMAFar:dword
	extrn	XMMDeallHMAFar:dword
	extrn	checkXMMFar:dword

R_CODE	ends

R1_CODE	segment
	extrn	PrevXmm:dword
	extrn	rXMMentry:byte
R1_CODE	ends



_TEXT	segment
	extrn	UpdateHMA:near
_TEXT	ends
;=============================================================================
;==	L O C A L   C O N S T A N T S
;=============================================================================
	include vdmseg.inc
	include vdmsel.inc
	include desc.inc
	include emm386.inc
	include vm386.inc
	include romstruc.equ
	include emmfunct.inc
	include emmdata.inc
	include	xmm.inc
	include oemdep.inc
;=============================================================================
;==	D A T A  S E G M E N T
;=============================================================================
_DATA	segment
fGlobalEnable	dw	0	; Global A20 enable/disable call
fCanChangeA20	db	1
HMAfree		db	TRUE	; HMA availability
HMAmin		dw	0	; HMA minimum size for allocation
_DATA	ends
;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
_TEXT	segment
	assume	cs:_TEXT,ds:_DATA,es:_DATA,gs:R_CODE,ss:STACK
;=============================================================================
;==	L O C A	L   D A T A
;=============================================================================
XMMproc	label word
	dw	offset GetVersion	; 0
	dw	offset RequestHMA   	; 1
	dw	offset ReleaseHMA   	; 2
	dw	offset GlobalEnableA20  ; 3
	dw	offset GlobalDisableA20 ; 4
	dw	offset LocalEnableA20   ; 5
	dw	offset LocalDisableA20  ; 6
	dw	offset IsA20On   	; 7 M005
	dw	offset QueryEMB   	; 8
	dw	offset AllocateEMB   	; 9
	dw	offset FreeEMB   	; 10
	dw	offset MoveEMB   	; 11
	dw	offset LockEMB   	; 12
	dw	offset UnlockEMB   	; 13
	dw	offset GetEMBInfo   	; 14
	dw	offset ReallocateEMB   	; 15
	dw	offset RequestUMB   	; 16
	dw	offset ReleaseUMB   	; 17
MaxXMMprocs	equ	($-XMMproc)/2
;==============================================================================
;==
;==  pXMMentry: Services XMM requests in protected mode.
;==
;==  Entry: (Protected Mode)
;==	AH  = XMM function number
;==	ECX = XMM function number
;==	GS  = R_CODE
;==
;==  Exit:
;==
;==============================================================================
pXMMentry proc	near

	push	VDMD_GSEL	; DS/GS are setup for CEMM's 2 data areas.
	pop	ds
	assume	ds:_DATA,es:nothing,fs:nothing,gs:R_CODE

	call	cs:[XMMproc][ecx*2]
	ret
pXMMentry	endp

;==============================================================================
;==
;==  XMMnullProcedures: These XMS routines are not serviced by CEMM.  They are
;==			passed to the next XMM in the chain.
;==
;==  Entry: (Protected Mode)
;==
;==  Exit:
;==
;==============================================================================
XMMnullProcedures proc	near
GetVersion:
QueryEMB:
AllocateEMB:
FreeEMB:
MoveEMB:
LockEMB:
UnlockEMB:
GetEMBInfo:
ReallocateEMB:
	ret
XMMnullProcedures	endp

;==============================================================================
;==
;==  RequestHMA: If CEMM is providing a virtual HMA and an XMM has already
;==		 been loaded, then it probably will not recognize the HMA
;==		 at run-time.  Thus, CEMM will hook the existing XMM and
;==		 provide the HMA services.  If an XMM is not present on
;==		 the system, CEMM will report 64K of memory via INT 15h
;==		 AH=88h.
;==
;==  Entry: (Protected Mode)
;==	AH = 1 request HMA function
;==	DX = Size of HMA required
;==
;==  Exit:
;==	AX = 1 if the HMA function was succesful.
;==	     0 if the HMA function failed.
;==	       BL = 91h if the HMA is already in use
;==	       BL = 92h if DX is less than the HMAMIN parameter
;==
;==============================================================================
RequestHMA proc	near

	cmp	[HMAfree],TRUE
	jne	short ReqHuse

	cmp	dx,[HMAmin]
	jb	short ReqHmin

	mov	[HMAfree],FALSE
	mov	ax,1

ReqHexit:
	ret

ReqHuse:
	mov	bl,XMMuseHMA
	xor	ax,ax
	jmp	short ReqHexit

ReqHmin:
	mov	bl,XMMminHMA
	xor	ax,ax
	jmp	short ReqHexit
RequestHMA	endp

;==============================================================================
;==
;==  ReleaseHMA
;==
;==  Entry: (Protected Mode)
;==	AH = 2 release HMA function
;==
;==  Exit:
;==	AX = 1 if the HMA function was succesful.
;==	     0 if the HMA function failed.
;==	       BL = 93h if the HMA is already free
;==
;==============================================================================
ReleaseHMA proc	near

	cmp	[HMAfree],TRUE
	je	short RelHfree

	mov	[HMAfree],TRUE
	mov	ax,1

RelHexit:
	ret

RelHfree:
	mov	bl,XMMfreeHMA
	xor	ax,ax
	jmp	short RelHexit
ReleaseHMA	endp

;==============================================================================
;==
;==  RequestUMB
;==
;==  Entry: (Protected Mode)
;==	AH = 16 request UMB function
;==	DX = size of UMB requested
;==
;==  Exit:
;==	AX = 1 if the UMB function was succesful.
;==	       BX = segment of UMB allocated
;==	       DX = actual size of UMB
;==	     0 if the UMB function failed.
;==	       BL = B0h if a smaller UMB is available
;==	       BL = B1h if no UMBs are available
;==	       DX = size of largest UMB available.
;==
;==============================================================================
RequestUMB proc	near
	push	fs

	mov	ax,DATA32_GSEL
	mov	fs,ax

	xor	bx,bx
	movzx	ecx,gs:[UMBptr]
	shl	ecx,4
RUloop:
	cmp	fs:[ecx].Sig,'M'	;Q: Valid ARENA header?
	je	short RUown		; Y: valid, see if its owned
	cmp	fs:[ecx].Sig,'Z'	;Q: Valid ARENA header?
	jne	RUnotFound		; N: arena header corruption
RUown:
	cmp	fs:[ecx].Own,0		;Q: Is this block available?
	jne	short RUnext		; N: get next ARENA header

	cmp	fs:[ecx].Len,bx		;Q: Largets block so far?
	jbe	short RUnext		; N: get next ARENA header
	mov	bx,fs:[ecx].Len		; Y: save this size
	cmp	bx,dx			;Q: Large enough block?
	jae	short RUfound		; Y: found a UMB
RUnext:
	cmp	fs:[ecx].Sig,'Z'	;Q: Last ARENA header in chain?
	je	RUnotFound		; Y: no blocks satisfy request

	mov	ax,fs:[ecx].Len		; get size of block in paragraphs
	inc	ax			; increment to memory block
	shr	ecx,4			; paragraph pointer
	add	cx,ax			; get to next ARENA header
	jc	short RUnotFound		; if carry, ARENAs are corrupt
	shl	ecx,4			; byte pointer
	jmp	short RUloop		; try the next arena
;
;  A UMB was found
;
RUfound:
	shr	ecx,4
	mov	ax,cx			; save paragraph in AX
	shl	ecx,4
	inc	ax
	push	ax			; save UMB address for return
;
;  Mark ARENA used
;
	mov	fs:[ecx].Own,ax		; paragraph as owner
	mov	fs:[ecx].Len,dx		; size
	mov	dword ptr fs:[ecx].Nam," BMU"; UMB is the name
	mov	dword ptr fs:[ecx][4].Nam,"    "
;
;  Is there enough memory to create another ARENA
;
	sub	bx,dx			;Q: Any more room in this ARENA?
	jbe	short RUcont		; N: just mark used
	dec	bx			; Y: size of new ARENA

	mov	al,'M'			; save ARENA signature
	xchg	fs:[ecx].Sig,al		; mark with "M" signature

	shr	ecx,4
	inc	cx
	add	cx,dx
	shl	ecx,4
;
;  Create new ARENA
;
	mov	fs:[ecx].Sig,al		; mark with "M/Z" signature
	mov	fs:[ecx].Own,0		; free
	mov	fs:[ecx].Len,bx		; size of free memory
	mov	dword ptr fs:[ecx].Nam,0; no name
	mov	dword ptr fs:[ecx][4].Nam,0

RUcont:
	mov	ax,1			; UMB allocated
	pop	bx			; restore paragraph address of UMB
	jmp	short RUexit
;
;  A UMB was NOT found
;
RUnotFound:
	mov	dx,bx			; save size of largest block
	mov	bx,XMMnoUMB		; assume no UMBs are available
	or	dx,dx			;Q: Any blocks available?
	jz	short RUerr		; N: error code is correct
	mov	bx,XMMsmallUMB		; Y: but they're too small
RUerr:
	xor	ax,ax			; error indication
RUexit:
	pop	fs
	ret
RequestUMB	endp

;==============================================================================
;==
;==  ReleaseUMB
;==
;==  Entry: (Protected Mode)
;==	AH = 17 request UMB function
;==	DX = segment number of UMB to release
;==
;==  Exit:
;==	AX = 1 if the UMB function was succesful.
;==	     0 if the UMB function failed.
;==	       BL = B2h if the UMB segment number was invalid
;==
;==============================================================================
ReleaseUMB proc	near
	push	fs

	mov	ax,DATA32_GSEL
	mov	fs,ax
;
;  Get address of UMB
;
	movzx	ecx,dx
	dec	cx
	shl	ecx,4
;
;  Check if UMB is valid
;
	cmp	fs:[ecx].Own,dx		;Q: Was it allocated via XMS UMB?
	jne	RelUMBerr		; N: invalid segment number

	cmp	dword ptr fs:[ecx].Nam," BMU"	;Q: Allocated via XMS UMB?
	jne	RelUMBerr			; N: invalid segment number
	cmp	dword ptr fs:[ecx][4].Nam,"    ";Q: Allocated via XMS UMB?
	jne	RelUMBerr			; N: invalid segment number
;
;  Free up the UMB
;
	mov	fs:[ecx].Own,0	      	; free
	mov	dword ptr fs:[ecx].Nam,0; no name
	mov	dword ptr fs:[ecx][4].Nam,0
;
;  Garbage collect UMB ARENAs
;
	call	UMBGarbageCollect

	mov	ax,1			; succesful release
	xor	bx,bx			; no error code

RelUMBexit:
	pop	fs
	ret

RelUMBerr:
	xor	ax,ax			; indicate an error
	mov	bx,XMMsegUMB		; invalid segment number error code
	jmp	short RelUMBexit

ReleaseUMB	endp

;==============================================================================
;==
;==  UMBGarbageCollect:  Garbage collects all ARENA headers.
;==
;==  Entry: (Protected Mode)
;==	DS	 = _DATA
;==	FS	 = DATA32_GSEL
;==	GS	 = _R_CODE
;==	[UMBptr] = first ARENA header
;==
;==  Exit:
;==	All free memory blocks above [UMBptr] which are adjacent, will be
;==	combined.
;==
;==============================================================================
UMBGarbageCollect proc	near
	push	eax
	push	ebx
	push	ecx
;
;  Get first ARENA pointer
;
	movzx	eax,gs:[UMBptr]		; get pointer to first ARENA
	shl	eax,4
;
;  Make sure it is a valid ARENA (base)
;
UGCloop:
	cmp	fs:[eax].Sig,'Z'	;Q: Last ARENA?
	je	short UGCexit		; Y: no more garbage collecting needed!
	cmp	fs:[eax].Sig,'M'	;Q: Valid base ARENA?
	jne	short UGCexit		; N: error, invalid ARENA
;
;  Check if base ARENA is FREE
;
UGCvalid:
	cmp	fs:[eax].Own,0		;Q: Is this base ARENA free?
	jne	short UGCnextBase	; N: get another base ARENA
	movzx	ebx,fs:[eax].Len	; Y: get to next ARENA
	inc	bx
	shl	ebx,4
	add	ebx,eax
;
;  Check if next ARENA is also FREE
;
	xchg	eax,ebx			; check if next ARENA is free
	cmp	fs:[eax].Own,0		;Q: Is next ARENA free?
	jne	short UGCnextBase	; N: get another base ARENA
	xchg	eax,ebx			; Y: combine base and next ARENAs
;
;  Use signature of next ARENA for base
;
	mov	cl,fs:[ebx].Sig
	mov	fs:[eax].Sig,cl
;
;  Add size of next ARENA to base
;
	mov	cx,fs:[ebx].Len		; get size of next ARENA
	inc	cx
	add	fs:[eax].Len,cx		; add size to base ARENA
;
;  Clear next ARENA: it was swallowed by base ARENA
;
	xor	ecx,ecx
	mov	fs:[ebx][0],ecx
	mov	fs:[ebx][4],ecx
	mov	fs:[ebx][8],ecx
	mov	fs:[ebx][12],ecx
	jmp	short UGCloop		; check new next ARENA if it is also free
;
;  Get a new base ARENA
;
UGCnextBase:
	movzx	ecx,fs:[eax].Len
	inc	cx
	shl	ecx,4
	add	eax,ecx
	cmp	fs:[eax].Sig,'Z'	;Q: End of chain?
	jne	short UGCloop		; N: continue

UGCexit:
	pop	ecx
	pop	ebx
	pop	eax
	ret
UMBGarbageCollect endp

;*----------------------------------------------------------------------*
;*									*
;*  GlobalEnableA20 -					FUNCTION 03h    *
;*									*
;*	Globally enable the A20 line					*
;*									*
;*  ENTRY:  PROTECTED MODE						*
;*  ARGS:   CLI								*
;*  RETS:   AX = 1 if the A20 line is enabled, 0 otherwise.  BL = Error	*
;*  REGS:   AX, BX CX, SI, DI and Flags clobbered			*
;*									*
;*  INTERNALLY NON-REENTRANT						*
;*									*
;*----------------------------------------------------------------------*
GlobalEnableA20 proc near


	cmp	[fGlobalEnable],1	; Is A20 already globally enabled?
	je	GEARet

GEAEnable:
	call    LocalEnableA20		; Attempt to enable A20
	or	ax,ax
	jz	GEAA20Err

	mov	[fGlobalEnable],1	; Mark A20 global enabled

GEARet:
	mov	ax,1			; return success
	xor	bl,bl
	ret

GEAA20Err:
	mov	bl,XmmerrA20		; some A20 error occurred
	xor	ax,ax
	ret
GlobalEnableA20 endp

;*----------------------------------------------------------------------*
;*									*
;*  GlobalDisableA20 -					FUNCTION 04h    *
;*									*
;*	Globally disable the A20 line					*
;*									*
;*  ENTRY:  PROTECTED MODE, CLI						*
;*  ARGS:   								*
;*  RETS:   AX=1 if the A20 line is disabled, 0 otherwise.  BL = Error	*
;*  REGS:   AX, BX, CX, SI, DI and Flags are clobbered			*
;*									*
;*  INTERNALLY NON-REENTRANT						*
;*									*
;*----------------------------------------------------------------------*
GlobalDisableA20 proc near

	cmp	[fGlobalEnable],0	; Is A20 already global-disabled?
	je	GDARet

	call    LocalDisableA20		; Attempt to disable it
	or	ax,ax			;   (also zaps CX, SI, DI)
	jz	GDAA20Err

	mov	[fGlobalEnable],0	; mark as global-disabled

GDARet:
	mov	ax,1			; return success
	xor	bl,bl
	ret

GDAA20Err:
	mov	bl,XMMerrA20		; some A20 error occurred
	xor	ax,ax
	ret
GlobalDisableA20 endp

;*----------------------------------------------------------------------*
;*									*
;*  LocalEnableA20 -					FUNCTION 05h    *
;*									*
;*	Locally enable the A20 line					*
;*									*
;*  ENTRY:  PROTECTED MODE, CLI						*
;*  ARGS:   								*
;*  RETS:   AX = 1 if the A20 line is enabled, 0 otherwise.  BL = Error	*
;*  REGS:   AX, BX, CX, SI, DI and Flags clobbered			*
;*									*
;*  INTERNALLY NON-REENTRANT						*
;*									*
;*----------------------------------------------------------------------*
LocalEnableA20 proc near

	cmp	[fCanChangeA20],1	; Can we change A20?
	jne	LEARet			; No, don't touch A20


	test	gs:[Current_State],fState_A20ena
					; Q: is A20 ON
	jnz	LEAIncIt		; Y: just increment count
					; N: turn it ON virtually
	or	gs:[Current_State],fState_A20Ena
	call	UpdateHMA

LEAIncIt:
	inc	gs:[EnableCount]
LEARet:
	mov	ax,1			; return success
	xor	bl,bl
	ret

LEAA20Err:
	mov	bl,XMMerrA20		; some A20 error occurred
	xor	ax,ax
	ret
LocalEnableA20 endp

;*----------------------------------------------------------------------*
;*									*
;*  LocalDisableA20 -					FUNCTION 06h    *
;*									*
;*	Locally disable the A20 line					*
;*									*
;*  ENTRY:  PROTECTED MODE, CLI						*
;*  ARGS:   		 						*
;*  RETS:   AX=1 if the A20 line is disabled, 0 otherwise.  BL = Error	*
;*  REGS:   AX, BX, CX, SI, DI and Flags are clobbered			*
;*									*
;*  INTERNALLY NON-REENTRANT						*
;*									*
;*----------------------------------------------------------------------*
LocalDisableA20 proc near

	cmp	[fCanChangeA20],0	; Can we change A20?
	je	LDARet			; No, don't touch A20

	cmp	gs:[EnableCount],0		; make sure the count's not zero
	je	LDAA20Err

;;
;;	xor	ax, ax			; assume A20 is OFF
;;					; Q: is A20 ON
;;	test	gs:[Current_State],fState_A20ena
;;	jz	LDAcont			; N: AX = 0
;;	mov	ax, 1			; Y: AX = 1
;;LDAcont:
					; M005: use routine instead of inline
	call	IsA20On			; M005: currently on or off
			

	cmp     gs:[EnableCount],1	; Only if the count = 1 should A20 be
	jnz     LDAStayOn		;   turned off, otherwise it stays on

	or	ax,ax			; If A20 is already off, don't
	jz	LDADecIt		;   bother to turn off again

	xor     ax,ax			; It's on, but should be turned off
	jmp     short LDASetIt

LDAStayOn:
	or	ax,ax			; A20 must stay on, if it is on, just
	jnz	LDADecIt		;   dec count, else force A20 on

LDASetIt:
	and	gs:[Current_State], NOT fState_A20Ena
					; Disable A20 virtually
	call	UpdateHMA

LDADecIt:
	dec	gs:[EnableCount]

LDARet:
	mov	ax,1			; return success
	xor	bl,bl
	ret

LDAA20Err:
	mov     bl,XMMerrA20		; some A20 error occurred
	xor     ax,ax
	ret
LocalDisableA20 endp

;*----------------------------------------------------------------------*
;*  M005								*
;*									*
;*  IsA20On -						FUNCTION 07h    *
;*									*
;*	Returns the state of the A20 line				*
;*									*
;*  ENTRY:  PROTECTED MODE, CLI						*
;*	    GS = R_CODE							*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = 1 if the A20 line is enabled, 0 otherwise		*
;*	    BL = 0							*
;*  REGS:   AX								*
;*									*
;*									*
;*----------------------------------------------------------------------*

IsA20On	proc	near

	xor	ax, ax			; assume A20 is OFF
					; Q: is A20 ON
	test	gs:[Current_State],fState_A20ena
	jz	IsA20Exit		; N: AX = 0
	mov	ax, 1			; Y: AX = 1

IsA20Exit:
	xor	bl, bl			; return success
	ret

IsA20On	endp

	

_TEXT	ends				; end of segment

;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
LAST	segment
	assume	cs:LAST,ds:_DATA,gs:R_CODE
;=============================================================================
;==	L O C A	L   D A T A
;=============================================================================
XMSver		dw	0
HimemVer	dw	0
;==============================================================================
;==
;==  UMBlink: Link all UMBs for XMM/DOS/RUNHI/LOADHI.
;==
;==  Enter:
;==     RAMset[] 	completely filled
;==	Page4K[] 	completely filled
;==	UMBtable[]      completely filled
;==
;==  Exit:
;==     		Arena headers on the UMB blocks
;==	[UMBptr]	pointer to first arena header.
;==
;==============================================================================
UMBlink	proc	near
	pusha
	push	es
	push	fs
;
;  Initialize Windows VxD for UMBs
;
;;	call	VxDInit

	xor	ax,ax

	xor	si,si
	movzx	cx,cs:[NumOfUMBwindows]
	or	cx,cx			;Q: Any UMBs?
	jz	UlExit			; N: exit
UlLoop:
	movzx	bx,cs:[UMBtable][si]	; get 4K page number

	or	bx,bx			;Q: Valid UMB entry?
	jz	short UlNext		; N: try next entry

	test	cs:[Page4K][bx],RAM	;Q: User specified RAM?
	jz	short UlCont		; N: check if user specified ROM

	call	FindSetRanges		; Y: mark ARENAs
	jmp	short UlNext		; try next page

UlCont:
	test	cs:[Page4K][bx],ROM	;Q: User specified ROM?
	jnz	short UlNext		; Y: don't use this page
;
;  Mark the entire 4K page as a UMB
;
	shl	bx,8			; starting paragraph
	mov	dx,bx
	add	dx,0FFh			; last paragraph

	call	CreateARENAs		; create ARENA header

UlNext:
	inc	si
	dec	cx
	jnz	UlLoop
;
;  Now fix up adjacent ARENAs: first delete ZERO length arenas
;
	mov	bx,gs:[UMBptr]
	or	bx,bx			;Q: Any UMBs found?
	jz	UlExit			; N: exit
	mov	dx,bx			; Y: first ARENA
	mov	fs,dx
	add	bx,fs:[0].Len
	inc	bx

UlAdj:
	mov	fs,bx
	mov	al,fs:[0].Sig
	add	bx,fs:[0].Len
	inc	bx

	cmp	fs:[0].Len,0
	jne	short UlAdjNext

	cmp	al,"Z"
	je	short UlAdjCont

	call	ClearARENA

	mov	fs,bx
	add	bx,fs:[0].Len
	inc	bx

UlAdjCont:
	call	ClearARENA

	mov	fs,dx
	mov	fs:[0].Sig,al
	mov	fs:[0].Len,bx
	sub	fs:[0].Len,dx
	dec	fs:[0].Len

UlAdjNext:
	mov	dx,fs
	cmp	al,"Z"		;Q: Last ARENA?
	jne	short UlAdj	; N: next ARENA

;
;  Need to see if first UMB is adjacent to last DOS ARENA
;
	mov	bx,-1			; sanity checker
	mov	ax,5200h		; add 1K to arena header
	int	21h                     ; get SYSINIT variables
	cmp	bx,-1			;Q: Did the call work?
	je	UlExit			; N: don't add 1K to arena
	mov	bx,es:[bx][-2]		; Y: UNDOCUMENTED start of arena chain
UlArenaLoop:
	mov	es,bx			; access current arena
	add	bx,es:[0].Len		; get segment to next arena
	jc	short UlExit		; if overflow, incorrect chain - EXIT!
	inc	bx
	cmp	es:[0].Sig,'Z'		;Q: Last arena header?
	jne	short UlArenaLoop	; N: check next arena
	cmp	bx,gs:[UMBptr]		;Q: Is first UMB adjacent?
	jne	short UlExit		; N: don't add to DOS chain
;
;  First UMB is adjacent, add to DOS memory
;
	mov	fs,bx			; access first UMB arena
	mov	bx,fs:[0].Len		; size of first UMB arena
	inc	bx
	cmp	fs:[0].Sig,'Z'		;Q: Last UMB?
	je	short UlDOScont	; Y: Don't add an extra arena
	inc	bx
UlDOScont:
	add	es:[0].Len,bx		; add to DOS memory
;
;  Fix INT 12h at 40:13h
;
	mov	bx,es
	add	bx,es:[0].Len
	inc	bx
	shr	bx,6			; kilobyte increment
	mov	ax,romdata		; get pointer to BIOS data area
	mov	fs,ax
	assume 	fs:romdata
	mov	fs:[Int12hMem],bx	; increment INT 12h by first UMB size

	or	gs:[GenFlags], fBackfill; we don't want to start win /3 if
					; back fill.
;
;  Find new first UMB pointer
;
	xor	bx,bx
	xchg	gs:[UMBptr],bx		; assume no UMBs
	mov	fs,bx			; get first UMB
	add	bx,fs:[0].Len
	inc	bx
	cmp	fs:[0].Sig,'Z'		;Q: Last UMB arena?
	pushf
	call	ClearARENA
	popf
	je	short UlExit		; Y: exit

UlDOSarena:
	mov	fs,bx			; get first UMB
	mov	gs:[UMBptr],bx		; assume first UMB
	add	bx,fs:[0].Len
	inc	bx
	cmp	fs:[0].Own,0		;Q: New first UMB?
	je	short UlExit		; Y: exit
	call	ClearArena		; N: clear arena
	jmp	short UlDOSarena

UlExit:
	pop	fs
	pop	es
	popa
	ret
UMBlink	endp

;==============================================================================
;==
;==  ClearARENA: Create ARENAs from set ranges in a 4K page.
;==
;==  Enter:
;==	FS =	Segment address for ARENA header
;==
;==  Exit:
;==     ARENA header has been cleared (zeroed)
;==
;==============================================================================
ClearArena proc	near
	push	eax

	xor	eax,eax
	mov	fs:[0],eax
	mov	fs:[4],eax
	mov	fs:[8],eax
	mov	fs:[12],eax

	pop	eax
	ret
ClearArena	endp

;==============================================================================
;==
;==  FindSetRanges: Create ARENAs from set ranges in a 4K page.
;==
;==  Enter:
;==	AX =	Previous ARENA describing invalid memory
;==	BX =	4K page index
;==
;==  Exit:
;==	AX =	New previous ARENA
;==     ARENA headers on this 4K page specified by user.
;==
;==============================================================================
FROM	equ	2
FindSetRanges	proc	near
	push	bx
	push	cx
	push	si

	shl	bx,8			; start of 4K page
;
;  Find lowest address in this 4K page
;
FSRloop:
	mov	dx,-1
	xor	si,si
FSRlow:
	mov	cx,cs:[RAMSet+2][si]	; get FROM range
	or	cx,cx			;Q: End of user specified range?
	jz	short FSRdone		; Y: check TO range
	add	si,2			; N: next range

	cmp	ch,bh			;Q: Same page?
	jne	short FSRlow		; N: try next paragraph

	cmp	cx,dx			;Q: Lowest paragraph?
	jae	short FSRlow		; N: get next one
	mov	dx,cx			; Y: save value
	mov	di,si			; save index
	sub	di,2
	jmp	short FSRlow

;
;  Lowest address found
;
FSRdone:
	cmp	dx,-1			;Q: Any values found?
	je	short FSRnoMore		; N: last ARENA

	cmp	dx,ax			;Q: Higher than last ARENA?
	jbe	short FSRexit		; N: problem, probably overlap!
;
;  FROM or TO value?
;
	test	di,FROM			;Q: FROM paragraph?
	jnz	short FSRto		; N: TO paragraph
	mov	bx,dx			; Y: new ARENA
	mov	cs:[RAMset+2][di],1	; mark entry used
	add	di,2
;
;  Get TO value
;
	cmp	bh,byte ptr cs:[RAMset+3][di] ;Q: Is TO in same 4K page index
	je	short FSRto		      ; Y: get TO address
	or	dx,0FFh			      ; N: use the rest of the page
	jmp	short FSRcreate

FSRto:
	mov	dx,1			; used to mark entry
	xchg	dx,cs:[RAMset+2][di]	; get address and mark used
	cmp	bx,dx			;Q: Is start and end the same?
	jne	short FSRcont		; N: continue
	or	dx,0FFh			; Y: mark the entire page
FSRcont:
	call	CreateARENAs		; create ARENA
	jmp	short FSRloop		; find any other ranges
;
;  Use entire page if no ranges found, else exit
;
FSRnoMore:
	cmp	ah,bh			;Q: Last ARENA in this page?
	je	short FSRexit		; Y: user specified range found
	mov	dx,bx			; N: use entire page as RAM
	add	dx,0FFh

FSRcreate:
	call	CreateARENAs

FSRexit:
	pop	si
	pop	cx
	pop	bx
	ret
FindSetRanges	endp

;==============================================================================
;==
;==  CreateARENAs:  Create ARENA headers
;==
;==  Enter:
;==     AX =	Previous ARENA header describing invalid memory
;==	BX =	Start of current memory area
;==	DX =	Last paragraph of current memory area
;==
;==  Exit:
;==	AX =	Last paragraph of current memory becomes new previous ARENA
;==     ARENA headers on this memory block. Also, last ARENA updated.
;==
;==============================================================================
CreateARENAs	proc	near

	or	ax,ax			;Q: Previous ARENA?
	jz	short CAfirst		; N: this is the first

	mov	fs,ax			; access last ARENA
	mov	fs:[0].Sig,"M"		; it is not the last
	mov	fs:[0].Len,bx		; calculate the new size
	sub	fs:[0].Len,ax
	dec	fs:[0].Len
	jmp	short CAcurr		; create current ARENA

CAfirst:
	mov	gs:[UMBptr],bx		; this is the first ARENA

CAcurr:
	mov	fs,bx			; access current ARENA
	mov	fs:[0].Sig,"M"		; not the last
	mov	fs:[0].Own,0		; free memory

	mov	fs:[0].Len,dx		; calculate the size
	sub	fs:[0].Len,bx
	dec	fs:[0].Len

	mov	byte ptr fs:[0].RSVD,0	; zero out rest of ARENA
	mov	word ptr fs:[0].RSVD+1,0
	mov	dword ptr fs:[0].Nam,0
	mov	dword ptr fs:[0].Nam+4,0

	mov	fs,dx			; access last ARENA

	mov	fs:[0].Sig,"Z"		; mark last ARENA
	mov	fs:[0].Own,InvMem	; owned by "SM" (invalid memory)
	mov	fs:[0].Len,0		; don't know length yet
	mov	byte ptr fs:[0].RSVD,0	; "SM"
	mov	word ptr fs:[0].RSVD+1,0
	mov	dword ptr fs:[0].Nam,"  MS"
	mov	dword ptr fs:[0].Nam+4,"    "

	mov	ax,fs			; last ARENA becomes previous ARENA

	ret
CreateARENAs	endp

;==============================================================================
;==
;==  XMMCheck: This routine checks if an XMM is on the system
;
;==	ENTRY	: GS = R_CODE
;==
;==============================================================================
XMMcheck proc	near

	call	dword ptr gs:[checkXMMFar]
					;Q: Is XMM present?
	jc	short XMMnotDetected	; N: OK only if an ISA machine.
	call	get_XMM_ver		; Y: check version

	call	XMMhook			; hook XMM

ifndef MSFLAG
	cmp	[ROMID],ROMIDISA	;Q: ISA/EISA machine?
	je	short XMMcont		; Y: don't need to invoke XMM
endif
	call	XMMQueryExtended	; allocate all memory via XMS (Microsoft)

XMMcont:
	test	gs:[Weitek_State],fWeitek_Map ;Q: Weitek enabled?
	jz	short XMMexit		      ; N: don't allocate HMA


	call	gs:[XMMAllocHMAFar]	;Q: HMA available for WEITEK?
	jnc	short XMMexit		; Y: exit
	mov	[HMAfree],FALSE		; N: take for virtual HMA

	cmp	bl,XMMnoHMA		;Q: HMA exist?
	je	short XMMexit		; N: WEITEK is possible
	mov	[HMAfree],TRUE		; Y: not available for WEITEK

	and	gs:[Weitek_State],not fWeitek_Map ; no WEITEK possible
	or	gs:[msg_flag],W_NO_HMA_MSG        ; "Unable to enable WEITEK"
	jmp	short XMMexit

XMMnotDetected:
ifndef MSFLAG
	cmp	[ROMID],ROMIDISA	;Q: ISA/EISA machine?
	je	short XMMexit		; Y: no XMM is ok!
endif
	or	gs:[msg_flag],NO_XMM_MSG; will not load with no XMM

XMMexit:
	ret
XMMcheck 	endp

;---------------------------------------------------------------------------
;
; XMMHook
;
; ENTRY		: GS -> R_CODE
;		  R1_CODE not moved yet.
;
; Hooks the XMS handler and points it to our handler rXMMentry in util.asm
;
; USES		: NONE
;
; Code ported from Smartdrv.
;
;--------------------------------------------------------------------------

XMMhook	proc	near

	push	es
	push	bx
	push	ax
	push	cx
	push	si
	push	di

	push	fs
	mov	bx, seg R1_CODE
	mov	fs, bx
	assume	fs:R1_CODE

	mov	bx,word ptr gs:[XMMcontrol]
	mov	es,word ptr gs:[XMMcontrol+2]   ; ES:BX = ptr to 1st XMM header
NextXmmHeader:
	mov	word ptr fs:[PrevXmm+2],es	; save seg of prev control adr
	mov	word ptr cs:[XmmControlBase+2],es
	mov	word ptr cs:[XmmControlBase],bx
	mov	cx,word ptr es:[bx]
	cmp	cl,0EBh				; compare short jmp opcode
	je	ShortJmp
	cmp	cl,0EAh				; compare far jmp opcode
	jne	XmmChainHosed			; bad XMM control chain
FarJmp:
	mov	si,word ptr es:[bx+1]		; SI = offset of jmp
	mov	es,word ptr es:[bx+1+2]		; ES = segment of jmp
	mov	bx,si
	jmp	NextXmmHeader			; continue down control chain
ShortJmp:
	cmp	word ptr es:[bx+2],9090h	; check NOPs
	jne	XmmChainHosed			; bad XMM control chain
	cmp	byte ptr es:[bx+4],90h
	jne	XmmChainHosed			; bad XMM control chain
	mov	di,bx				; DI = ptr to XMM header
	xor	ax,ax
	mov	al,ch				; AX = offset of short jmp
	mov	cs:[XmmControlJmpVal],al	; save offset of short jmp
	add	ax,2				; add length of jmp instr
	add	bx,ax				; BX = target of jmp
	mov	word ptr fs:[PrevXmm],bx  	; save previous control addr

;	Install ourselves in XMM control chain.

	mov	byte ptr es:[di],0EAh		; far immediate jmp opcode
	mov	word ptr es:[di+1],offset R1_CODE:rXMMentry
	mov	word ptr es:[di+3],fs

	add	di, 3
	mov	word ptr cs:[XMMHookAddr+2], es
	mov	word ptr cs:[XMMHookAddr], di

ifndef MSFLAG
ifndef LC910611
;
;  Make sure to hook the A20 API if the undocumented NoA20Trap is used!
;  This is not complete (910611)
;
	test	[GenFlags],fNoA20Trap	;Q: Was the NoA20Trap switch used?
	jz	short @f		; N: continue
					; Y: trap all XMS A20 calls
	mov	ax,offset R1_CODE:[XeTrap]
	mov	fs:[XMMTrapTable][3*2],ax
	mov	fs:[XMMTrapTable][4*2],ax
	mov	fs:[XMMTrapTable][5*2],ax
	mov	fs:[XMMTrapTable][6*2],ax
	mov	fs:[XMMTrapTable][7*2],ax
@@:
endif
endif

	jmp	short Xh_done

XmmChainHosed:
	or	gs:[msg_flag],NO_XMM_MSG	; will not load with no XMM

Xh_done:
	pop	fs
	pop	di
	pop	si
	pop	cx
	pop	ax
	pop	bx
	pop	es

	ret
XMMhook	endp

;**************************************************************************
;
;	Function	:	get_XMM_ver
;
;	Stores the XMS version # and the Himem version # in XmsVer and
;	HimemVer respectively.
;
;**************************************************************************

get_XMM_ver	proc	near

	push	ds
	push	es
	push	ax
	push	bx

	push	cs
	pop	ds
	assume	ds:LAST		; set up ds

	mov	ah, XMM_GET_VERSION
	push	seg R_CODE
	pop	es
	assume	es:R_CODE	; es -> R_CODE
	call	es:[XMMcontrol]

	mov	[XmsVer],ax
	mov	[HimemVer],bx

	cmp	bx,GOOD_HIMEM_VERSION		;Q: Good HIMEM.SYS version?
	jae	SHORT GXVexit			; Y: continue
	or	es:[msg_flag],BAD_XMM_MSG	; N: warning message

GXVexit:
	pop	bx
	pop	ax
	pop	es
	pop	ds
	assume	ds:_DATA, es:_DATA
	ret

get_XMM_ver	endp

;*****************************************************************************;
;***	XMMQueryExtended - Query amount of Extended Memory		      ;
;									      ;
;	This routine is used to query the amount of extended 		      ;
;	memory in the system using XMM calls.				      ;
;									      ;
;	ENTRY	none		;ds = _DATA				      ;
;	EXIT	AX = Size of largest free extended memory block    	      ;
;		DX = Total amount of free extended memory available           ;
;									      ;
; USES: flags								      ;
;	USES	ax, flags modified					      ;
;									      ;
;*****************************************************************************;
XMMQueryExtended proc	near

	mov	ah,XMM_QUERY_FREE_EXTMEM
	push	ds
	push	seg R_CODE
	pop	ds
	assume	ds:R_CODE
	call	[XMMcontrol]
	pop	ds
	assume	ds:_DATA
	ret

XMMQueryExtended	endp

;*****************************************************************************;
;***	QueryA20 - query A20 state					      ;
;									      ;
;	This routine is used to query the A20 state in		 	      ;
;	the system using XMM calls.					      ;
;									      ;
;	ENTRY	none		;ds = _DATA				      ;
;	EXIT	CF set if a20 is on currently				      ;
;		CF clear if a20 is off currently			      ;
;									      ;
; USES: flags								      ;
;	USES	ax, flags modified					      ;
;									      ;
;*****************************************************************************;
QueryA20 proc	near

	mov	ah, XMM_QUERY_A20
	push	ds
	push	seg R_CODE
	pop	ds
	assume	ds:R_CODE
	call	[XMMcontrol]
	pop	ds
	assume	ds:_DATA
	or	ax, ax
	jz	short QA20_ON
	stc					; ON
	ret
QA20_ON:
	clc					; OFF
	ret

QueryA20	endp

;==============================================================================
;==
;==  AvailUMB: Total available UMB space in paragraphs.
;==
;==  Enter:
;==
;==  Exit:
;==	EAX = size of FREE UMB space
;==
;==============================================================================
AvailUMB proc	near

	push	bx
	call	ScanUMBs
	mov	ax,bx
	pop	bx
	ret
AvailUMB	endp

;==============================================================================
;==
;==  LargestUMB: Largest FREE UMB space in paragraphs.
;==
;==  Enter:  es = R_CODE	
;==
;==  Exit:
;==	EAX = Largest FREE UMB space in paragraphs
;==
;==============================================================================
LargestUMB proc	near

	push	bx
	call	ScanUMBs
	pop	bx
	ret
LargestUMB	endp

;==============================================================================
;==
;==  ScanUMBs: Scan UMB space for FREE and largest FREE UMB.
;==
;==  Enter: es= R_CODE
;==
;==  Exit:
;==	AX = Largest FREE UMB space in paragraphs
;==	BX = Total available FREE space in paragraphs
;==
;==============================================================================
ScanUMBs proc	near
	assume	es:R_CODE

	push	cx
	push	fs
;
;  Initialize variables to zero and get paragraph address to first UMB
;
	xor	ax,ax
	xor	bx,bx
	mov	cx,es:[UMBptr]
SUloop:
	mov	fs,cx
	cmp	fs:[0].Sig,'M'
	je	short SUvalid
	cmp	fs:[0].Sig,'Z'
	jne	short SUexit
;
;  Valid ARENA
;
SUvalid:
	cmp	fs:[0].Own,0
	jne	short SUnext
;
;  FREE UMB: add to total available and check if it's the largest
;
	add	bx,fs:[0].Len
	cmp	ax,fs:[0].Len		;Q: Larget FREE UMB so far?
	jae	short SUnext		; N: continue
	mov	ax,fs:[0].Len		; Y: save size
;
;  Get next UMB
;
SUnext:
	add	cx,fs:[0].Len
	inc	cx
	cmp	fs:[0].Sig,'Z'
	jne	short SUloop
SUexit:
	pop	fs
	pop	cx
	ret
ScanUMBs	endp


LAST	ENDS
	END
