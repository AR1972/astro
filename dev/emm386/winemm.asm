.386p
page	58,132
;******************************************************************************
	title	WINEMM
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1989-1991
;   (C) Copyright COMPAQ Computer Corp. 1989-1991
;
;   Title:    EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;   Module:   WINEMM - Routines to fill in data structure to pass to win386
;
;   Version:  0.001
;
;   Date:     July 26,1989
;
;   Author:   Harish K. Naidu
;	      Leo Cohen
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   07/26/89  Original
;   08/11/89  4.10	Add WinInit & updated Windows/CEMM interface. 	(LC)
;   07/31/90  4.20      Win 3.1 and UMB support. 			(LC)
;
;   02/14/91  M010	Added support to validate LoadHi Vxd path at win 
;			startup int 2f.
;
;   02/27/91  M013	First Check if standard mode has issued the int 2f 
;			startup in rint2fhHandler. If so just chain.
;
;   03/07/91  M015	Clear out the high word of edx when initializing 
;			dx with the arena length in VxdUsedUMBList.
;
;******************************************************************************
;
;   Functional Description:
;
;
;******************************************************************************
;=============================================================================
;==	P U B L I C   D E C L A R A T I O N S
;=============================================================================
	public	pWinEMMGlobImpDisp
	public	rINT2FhHandler
	public	GetPathName
	public	StorePath		; M010
	public	VxDInit

	public	EMMGIDataVer
	public	CEMMpath
	public	InstanceData
	public	Win386VxDRefDat

;=============================================================================
;==	L O C A L   C O N S T A N T S / E Q U A T E S
;=============================================================================
	include VDMseg.inc
	include winemm.inc
	include	emmfunct.inc
	include	emmdata.inc
	include	vdmsel.inc
	include	emm386.inc
	include vm386.inc
	include XMM.inc
	include driver.equ
ifdef BETA
	include winemm.pub
	include	dbg.inc
endif
;=============================================================================
;==	E X T E R N A L  D E C L A R A T I O N S
;=============================================================================
_DATA	SEGMENT
	extrn	EMS_window_location:word
	extrn	register_set:word
	extrn	number_EMS_windows:word
	extrn	context_save_area_size:word
	extrn	HMAfree:byte
	extrn	_PFUser:word
_DATA	ENDS

R_CODE	segment
	extrn	GoVirtual:near
	extrn	ChkA20:near
	extrn	GoProtMode:near
	extrn	GoVirtualMode:near
	extrn	GoRealMode:near
	extrn	EMM_rEntry:near
	extrn	Devname:byte
	extrn	WINEMM_Mess:byte
	extrn	ReInitDeb:near
	extrn	CopyInstData:dword
	extrn	ValidPathFar:dword
	extrn	EMM_rFarEntry:word

R_CODE	ends

R1_CODE	segment
	extrn	WinBackfillMess:byte
	extrn	WinInvPathMess:byte	; M010
R1_CODE	ends


_TEXT	SEGMENT
	extrn	MapHandlePage:near
	extrn	EnterVirtual:far
	extrn	Log2Phy:near
	extrn	Lin2Phy:near
_TEXT	ENDS

LAST	SEGMENT
	extrn	tempBuffer:dword
LAST	ENDS
	extrn	MajVer:abs
	extrn	MinVer1:abs
	extrn	MinVer2:abs
;=============================================================================
;==	L O C A	L   D A T A
;=============================================================================
R_CODE	segment

WinVer	label	word		; Version of Windows V86MMGR
WinVerMin	db	0	; Minor version
WinVerMaj	db	0	; Major version

EMMGIDataVer label word
EMMGIDataVerMin	db	0	; Minor version
EMMGIDataVerMaj	db	1	; Major version

ifndef MSFLAG
EMMGIVendor	db	"COMPAQ              "
EMMGIProduct	db	"CEMM ",MajVer+'0',".",MinVer1+'0',MinVer2+'0',"           "
else
EMMGIVendor	db	"MICROSOFT           "
EMMGIProduct	db	"EMM386 ",MajVer+'0',".",MinVer1+'0',MinVer2+'0',"         "
endif

pRDSdata	dw	0

R_CODE	ends

R1_CODE	segment


ConfigBuffer	equ	128	; up to 128 character path will be kept
Win386Struc	Win386StartupStruc <>

MaxUMBInstObj	equ	10	; up to 10 instance objects in UMB area supported
InstanceData	label	byte
rept MaxUMBInstObj
  	InstanceDataStruct<>	; possible XBDA/UMB instance data
endm
	dd	0	; termination of structure array

CEMMpath	db	ConfigBuffer dup (0)

R1_CODE	ends

_DATA	segment

Win386VxDRefDat	VxDRefDataStruct<>

_DATA	ends

;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
_TEXT	segment
	assume	cs:_TEXT,ds:nothing,es:nothing,fs:nothing,gs:nothing
;==============================================================================
;==  pWinEMMGItable jump table
;==============================================================================
pWinEMMGItable	label	word
	dw	offset UpdateStructure
	dw	offset UpdateInternals
	dw	offset WinBroadcast
pWEGImax	equ	$-pWinEMMGItable
;==============================================================================
;==
;==  pWinEMMGlobImpDisp: This is a dispatcher for the routine which update
;==			 the EMM Global Import data structure during a Windows'
;==			 Virtual Disable Call and gets info from the structure
;==			 during a Windows Enable Call.
;==
;==  Entry: (Protected Mode)
;==	AX	= 0 	Windows Disable Call
;==		= 1 	Windows Enable Call
;==		= 2     Windows Broadcast
;==	GS	= R_CODE segment
;==
;==  Exit:
;==	[ebp][VTFO].VMTF_EFLAGS	carry flag set if error, else zero
;==
;==============================================================================
pWinEMMGlobImpDisp proc	near
	push	eax

	shl	eax,24
	mov	ax,VDMD_GSEL	; DS/GS are setup for CEMM's 2 data areas.
	mov	ds,ax
	mov	ax,DATA32_GSEL	; access 4GB address space
	mov	fs,ax
	assume	ds:_DATA,es:nothing,fs:ABS0,gs:R_CODE

	shr	eax,24
	call	cs:[pWinEMMGItable][eax*2]	; call function

	pop	eax
	ret
pWinEMMGlobImpDisp	endp

;==============================================================================
;==
;==  UpdateStructure: This routine updates the EMM Global Import data structure
;==		      during a Windows' Virtual Disable Call.
;==
;==  Entry: (Protected Mode)
;==	AX	= 0   Windows Disable Call
;==	DS	= _DATA
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:
;==	[ebp][VTFO].VMTF_EFLAGS	carry flag set if error, else zero
;==
;==============================================================================
UpdateStructure proc	near
	push	ebx
	push	esi
;
;  Get FS:[EBX] pointing to EMM Global Import Data Structure
;
	call	GetEMMGlobImpDatArea

	mov	fs:[ebx].EMMGI_Len,EMMGIDS100size  ; assume 1.00 version size

	call	Fill_Flags
	call	Fill_EMMCtxt
	call	Fill_HandleInfo

;
;  If version 1.11 or later continue, else end of structure
;
	cmp	word ptr fs:[ebx][EMMGI_Vers],EMMGI_VERSION_111	;Q: Atleast 1.10?
	jb	short USexit					; N: exit
	mov	fs:[ebx].EMMGI_Len,EMMGIDS110size		; Y: size as 1.10 version

	call	FillFreeMem
	call	FillXMS
	call	FillFreeUMBs

	call	FillProductName

;
;  ESI contains the size of the variable portion of the Data Structure
;
USexit:
	add	fs:[ebx].EMMGI_Len,si	; add variable portion of data structure
	pop	esi
	pop	ebx
	ret
UpdateStructure	endp

;==============================================================================
;==
;==  UpdateInternals: This routine updates CEMM's internals using the  EMM
;==		      Global Import data structure during a Windows' Virtual
;==		      Enable Call.
;==
;==  Entry: (Protected Mode)
;==	AX	= 1   Windows Enable Call
;==	DS	= _DATA
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:
;==	[ebp][VTFO].VMTF_EFLAGS	carry flag set if error, else zero
;==
;==============================================================================
UpdateInternalsFar proc	far
	push	fs

	mov	ax,DATA32_GSEL
	mov	fs,ax

	call	UpdateInternals

	pop	fs
	ret
UpdateInternalsFar 	endp

UpdateInternals proc	near
	push	ebx
;
;  Get FS:[EBX] pointing to EMM Global Import Data Structure
;
	call	GetEMMGlobImpDatArea
;
;  Update current EMS mapping
;
	call	Update_Mapping_State

	pop	ebx
	ret
UpdateInternals	endp

;==============================================================================
;==
;==  WinBroadcast: Windows INT 2Fh AX=1605 broadcast.  Need to sync up the
;==		   version numbers and fill the VxD information for Win 3.0
;==		   and UMB information.
;==
;==  Entry: (Protected Mode)
;==	AX	= 2   Windows Broadcast Call
;==	DS	= _DATA
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:
;==	[ebp][VTFO].VMTF_EFLAGS	carry flag set if error, else zero
;==
;==============================================================================
WinBroadcast	proc	near
	push	eax
;
;  Check if Windows Init is still OK
;
	mov	ax,[bp][VTFO].VMTF_DS

	or	cx,cx			;Q: Anybody fail Windows Init?
	jnz	short WBerror		; Y: fail Win Init

	or	ax,si			;Q: Is a Virtual Disable Call set?
	jnz	short WBerror		; Y: impossible/error - fail init

	mov	gs:[WinVer],di		; save Windows version
	mov	gs:[EMMGIDataVer],EMMGI_VERSION_HI ; most recent version supported
	cmp	di,WIN_VERSION_HI	;Q: Version supported?
	jae	short WBSetEntry	; Y: continue
;
;  As new Windows versions get added and supported, need to place the most
;  recent EMM Global Import Data Structure supported in [EMMGIDataVer]
;
	cmp	di,WIN_VERSION_LO	;Q: Version supported?
	jb	short WBerror		; N: cannot do interface
	mov	gs:[EMMGIDataVer],EMMGI_VERSION_LO ; assume CEMM can handle latest version
	or	gs:[GenFlags],fWin30	; Win 3.0 broadcast occurred
;
;  Add VxD information to Windows Version 3.00 (called only for this version!)
;
	call	VxDSupport
;
;  Set Virtual Disable Call entry point
;
WBSetEntry:
	mov	si,seg R_CODE
	mov	[bp][VTFO].VMTF_DS,si
	mov	si,offset rWinV86Proc
WBexit:
	pop	eax
	ret

WBerror:
	mov	cx,-1			; fail Windows Init
	jmp	short WBexit
WinBroadcast	endp

;==============================================================================
;==
;==  GetEMMGlobImpDatArea: Gets pointer to start of EMM Global Import Data
;==			   structure.  Also, clears the page directory entries
;==			   which correspond to the Page Fault Tables.  The Page
;==			   Fault Tables will not be used while Windows is in
;==			   control.
;==
;==  Entry: (Protected Mode)
;==	DS	= _DATA
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:  (Page Fault Handler Reset)
;==
;==	FS:[EBX]= pointer to start of EMM Global Import Data Structure
;==
;==============================================================================
GetEMMGlobImpDatArea	proc	near
	push	eax
	push	ecx
;
;  Get FS:[EBX] pointing to the Page Directory and clear Page Fault Table entries
;
	mov	ebx,[page_directory]
	mov	ecx,2			; two page fault tables
GEGIDAloop:
	movzx	eax,[_PFUser][ecx*2-2]
	or	ax,ax			;Q: Is Page Fault Table being used?
	jz	short GEGIDAnext	; N: skip it
	mov	dword ptr fs:[ebx][eax],0;Y: reset it
GEGIDAnext:
	loop	GEGIDAloop
;
;  Get FS:[EBX] pointing to EMM Global Import Data Structure
;
	add	ebx,PAGE_SIZE		; address for PF0
	pop	ecx
	pop	eax
	ret
GetEMMGlobImpDatArea	endp

;############################################################################
;
;	Procedure name		: Fill_Flags
;
;	ENTRY			: Protected mode
;				  DS->_DATA
;				  FS->DATA32_GSEL
;				  GS->R_CODE
;
;	EXIT			: Fills the EMMOSKey flag and fills the
;				  EMMFlags with the regiser set info.
;
;	REGS MOD		: NONE
;
;############################################################################
Fill_Flags	proc	near
	push	eax
	push	cx
;
;  Initialize data structure
;
	mov	fs:[ebx].EMMGI_Flags,0
;
;  EMM Global Import Interface version number
;
	mov	ax,gs:[EMMGIDataVer]
	mov	word ptr fs:[ebx].EMMGI_Vers,ax
;
;  OS Key
;
	mov	eax,[OS_Key]
	mov	fs:[ebx].EMMGI_OSKey,eax

	mov	si,size RegisterSet_Struc	; access the register set 1
	movzx	cx, BYTE PTR [total_register_sets]
	dec	cx				; not register set 0

chk_active_reg_set:
	cmp	register_set[si].active,TRUE	  ;Q: Is register set active?
	jne	short next_reg_set		  ; N: OK, continue
	or	fs:[ebx].EMMGI_Flags,NONZR_REG_SET; Y: set flag accordingly
	jmp	short fill_flags_done

next_reg_set:
	add	si,size RegisterSet_Struc
	loop	chk_active_reg_set
;
;  If version 1.10 or later set HMA flag accordingly
;
	cmp	word ptr fs:[ebx][EMMGI_Vers],EMMGI_VERSION_111	;Q: Atleast 1.10?
	jb	short fill_flags_done				; N: exit

	or	fs:[ebx].EMMGI_Flags,NO_XMS_HANDLE ; EMBs not managed by CEMM

	cmp	gs:[UMBptr],0			;Q: Is CEMM managing UMBs?
	jne	FFcont				; Y: continue
	or	fs:[ebx].EMMGI_Flags,NO_XMS_UMB	; N: UMBs not managed by CEMM
FFcont:
	cmp	gs:[HMAptr],100000h		;Q: Virtual HMA?
	je	short fill_flags_done		; N: exit

	cmp	[HMAfree],TRUE			;Q: Is virtual HMA free?
	jne	short fill_flags_done		; N: exit
	or	fs:[ebx].EMMGI_Flags,HMA_INFO	; Y: allow windows to use it

fill_flags_done:
	pop	cx
	pop	eax
	ret
Fill_Flags	endp

;########################################################################
;
;	Procedure name	: Fill_EmmCtxt
;
;	ENTRY	:	Protected mode
;			DS -> _DATA
;			FS->DATA32_GSEL
;			GS->R_CODE
;
;	EXIT	:	EMMCtxt array initialized with current values.
;
;	Regs Mod:	NONE
;
;	written	:	7/26/89 HKN
;
;##########################################################################
Fill_EmmCtxt	proc	near
	push	ebp
	push	eax
	push	ecx
	push	edx
	push	edi
	push	es

	mov	dx,PAGET_GSEL
	mov	es,dx
;
;  Fill in the context save map size for CEMM and initialize UMB count
;
	mov	ax,[context_save_area_size]
	mov	fs:[ebx].EMMGI_CntxtSz,al
	mov	fs:[ebx].EMMGI_UMBcnt,0
;
;  Loop through first megabyte looking for EMS/UMBs
;
	xor	esi,esi
	xor	edi,edi
	xor	edx,edx
next_16K_page:
;
;  Initialize context
;
	mov	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_Flags,0
  	mov	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_HMap,-1
	mov	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_Lpag,-1
	mov	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_PPag,-1
 	mov	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_EFlgs,0
;
;  Get physical EMS window number for current 16K context
;
	call	Get_Phys_PNum

	cmp	eax, -1		;Q: Valid physical page number?
	je	short get_UMBs	; N: check for UMBs
	call	GetEMSInfo	; Y: get EMS information

ifdef QEMS
;
;  Index into the EMS_window array to obtain handle and the logical page to
;  which this physical page belongs. Note that the logical page number in the
;  EMS_window array refers 4K pages and hence has to be divided by 4 to get
;  the 16K logical page #
;
;  eax is the physical page number. For each physical page number there are 
;  3 bytes in the EMS_window array. The current EMS_window array is pointed
;  to by the EMS_window_ptr. Therefore [EMS_window_ptr]+eax*3 will access 
;  the appropriate entry
;
	mov	ebp, eax
	shl	ebp, 1
	add	ebp, eax	     	; ebp = eax * 3
	add	ebp, [EMS_window_ptr]	; ebp = offset in EMS_window
	
;AR	mov	cl,EMS_window[eax+eax*2].handle ; handle is only 8 bits
	mov	cl,ds:[ebp].handle 	; handle is only 8 bits

	mov	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_HMap,cl

;AR	mov	cx,EMS_window[eax+eax*2].logical_4k_page
	mov	cx,ds:[ebp].logical_4k_page

	shr	cx,2
	mov	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_Lpag,cx

;
; fill in physical page number
;
	mov	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_PPag,al
;
; indicate that it is an EMM page
;
	mov	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_Flags,EMM_PAGE
	cmp	dx,8000h		;Q: Possible LIM 3.2 page frame?
	jbe	short get_next          ; N: too low (no page frame)
	cmp	ax,4
	jae	short get_next
	or	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_Flags,FRAME_32
endif

	jmp	short get_next

get_UMBs:
	cmp	word ptr fs:[ebx][EMMGI_Vers],EMMGI_VERSION_111	;Q: Atleast 1.10?
	jb	short get_next					; N: exit

	cmp	gs:[UMBptr],0				;Q: UMBs on the system?
	jb	short get_next				; N: exit
;
;  Need to get UMB information
;
	call	GetUMBInfo

get_next:
	add	edi,size EMMContext
	add	dx, 0400h
	jnc	next_16K_page

	pop	es
	pop	edi
	pop	edx
	pop	ecx
	pop	eax
	pop	ebp
	ret
Fill_EMMCtxt	endp

;############################################################################
;
;	Procedure name	:	Fill_HandleInfo
;
;	ENTRY		:	Protected mode
;				DS -> _DATA
;				FS->DATA32_GSEL
;				GS->R_CODE
;				ESI= total size of variable sized entries in structure
;
;	EXIT		: 	EMM_HandleInfo array initialized
;
;	REGs MOD	:	NONE
;
;	written		: 	7/27/89 HKN
;
;############################################################################
Fill_HandleInfo		proc	near
	push	eax
	push	edx
	push	edi
	push	ebp
	push	cx
	push	es

	push	esi				; access to HndlCnt field

	mov	ax,PAGED_GSEL
	mov	es,ax

	mov	fs:[ebx][esi].EMMGI_HndlCnt,0	;(initialize)

	cmp	word ptr fs:[ebx][EMMGI_Vers],EMMGI_VERSION_111	
						; Q: Atleast 1.10?
	jb	FH_hndl0			; N: we cannot pass a null
						;    map as win 3.0 can't 
						;    handle it.

;;	cmp	gs:[NoEMSset],TRUE		; Q: Is NoEMS mode active?
;;	je	FH_exit		     		; Y: no handle info

	cmp	gs:[VCPIset], -1		; Q: has noems been specifed
	jne	FH_exit				; Y: no handle info

FH_hndl0:
;
;  Fill out the handle array
;
	mov	edi,[hndl_tbl_ptr]
	mov	ebp,[hndl_nam_ptr]
	mov	cx,[total_handles]
	xor	edx,edx			; clear handle index
get_handle_info:
	cmp	ds:[edi][edx*4].base_PTE_index,FREE;Q: Is handle allocated
	je	next_handle		           ; N: process next handle

	pop	eax				   ; Y: restore original offset
	inc	fs:[ebx][eax].EMMGI_HndlCnt	   ; increment handle array
	push	eax

	mov	fs:[ebx][EMMGI_Hndls][esi].HandleNum,dl ; handle number
;
;  Initialize Handle structure
;
	mov	fs:[ebx][EMMGI_Hndls][esi].HandleFlags,0
	mov	dword ptr fs:[ebx][EMMGI_Hndls][esi].HandleName,0
	mov	dword ptr fs:[ebx][EMMGI_Hndls][esi].HandleName[4],0
;
;  Get the # of logical pages alloc'd to this handle. Note that the number_PTEs
;  field gives the # of 4K logical pages, so divide by 4.
;
	mov	ax,ds:[edi][edx*4].number_PTEs
	shr	ax,2
	mov	fs:[ebx][EMMGI_Hndls][esi].NumLogPages,ax
;
;  Get Physical address of PTEs. The base_PTE_index field in the handle table
;  points to the required handle space. The selector for this is PAGED_GSEL.
;
	movzx	eax,ds:[edi][edx*4].base_PTE_index
	shl	eax,2				; make an offset
	call	Log2Phy				; physical address for ES:EAX
	mov	fs:[ebx][EMMGI_Hndls][esi].PhysPtrToPTEs,eax
;
;  Check if handle has a name
;					; check if handle is named
	mov	eax,ds:[ebp][edx*8]
	or	eax,eax			;Q: Name?
	jnz	short named_hndl	; Y: get name
	mov	eax,ds:[ebp][edx*8][4]  ; N: check second dword
	or	eax,eax			;Q: Name?
	jz	short chk_context	; N: check if it has a context
;
;  Get/Save name of handle
;
named_hndl:
	mov	eax,ds:[ebp][edx*8]
	mov	dword ptr fs:[ebx][EMMGI_Hndls][esi].HandleName,eax
	mov	eax,ds:[ebp][edx*8][4]
	mov	dword ptr fs:[ebx][EMMGI_Hndls][esi].HandleName[4],eax
	or	fs:[ebx][EMMGI_Hndls][esi].HandleFlags,HANDLE_HAS_NAME
;
;  Check if handle has a context
;
chk_context:
	cmp	byte ptr save_flag[edx],0 		 ;Q: Saved context?
	jne	short proc_next_handle        		 ; N: next handle
	or	fs:[ebx][EMMGI_Hndls][esi].HandleFlags,HANDLE_HAS_CTXT; Y: indicate in handle flag
	or	fs:[ebx].EMMGI_Flags,ANY_HANDLE_CTXT     ;    & also in the EMMFlags
;
;  Next handle structure
;
proc_next_handle:
	add	esi,size EMM_Handle		; point to next EMM_Handle struc
;
;  Get next handle
;
next_handle:
	inc	dx
	dec	cx
	jnz	get_handle_info

FH_exit:
	add	sp,4				; restore stack
	pop	es
	pop	cx
	pop	ebp
	pop	edi
	pop	edx
	pop	eax
	ret
Fill_HandleInfo	endp

;==============================================================================
;==
;==  FillFreeMem: Fill free EMS memory structures, virtual HMA info, and Int 67h
;==		  entry point information into the EMM Global Import Data Structure.
;==
;==  Entry: (Protected Mode)
;==	FS:[EBX]= start of EMM Global Import Data Structure
;==	ESI	= size of variable fields in EMM Global Import Data Structure
;==	DS	= _DATA
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:
;==
;==============================================================================
FillFreeMem	proc	near
	push	eax
	push	ecx
	push	edx
	push	edi
	push	es
ifdef QEMS
	mov	ax,PAGET_GSEL
	mov	es,ax
endif
;
;  Fill Int 67h entry point field
;
	mov	ax,seg R_CODE
	shl	eax,16
	mov	ax,offset R_CODE:EMM_rEntry
	mov	fs:[ebx][esi].EMMGI_Int67,eax
;
;  Fill physical HMA pointer
;
	mov	fs:[ebx][esi].EMMGI_HMA,0
	mov	eax,[HMAptr]			; get HMA physical memory
	cmp	eax,100000h			;Q: Phy=Lin (Virtual HMA)?
	je	short FFMFreeMem		; N: no virtual HMA
						; Y: get PTEs for virtual HMA
	mov	eax,ds:[page_tables]
	add	eax,(100000h shr 12)*4		; linear address of first HMA PTE
	call	Lin2Phy				; Y: pointer to virtual HMA PTEs
	mov	fs:[ebx][esi].EMMGI_HMA,eax
;
;  Fill free memory region structures
;
FFMFreeMem:
	mov	fs:[ebx][esi].EMMGI_FreeMEMcnt,0; initialize count to zero
	mov	edx,esi
	xor	edi,edi
	mov	di,FIRST_HANDLE_PTE
FFMloop:
	call	GetFreeEMSRange
	cmp	eax,-1			;Q: Free EMS range found?
	je	short FFMexit		; N: done, exit

	inc	fs:[ebx][edx].EMMGI_FreeMEMcnt
	mov	fs:[ebx][EMMGI_FreeMEM][esi].MEMstart,eax
	mov	fs:[ebx][EMMGI_FreeMEM][esi].MEMcount,ecx

	add	si,size MEM_FREE_Map
	add	eax,ecx			; search for next free range
	jmp	short FFMloop

FFMexit:
	pop	es
	pop	edi
	pop	edx
	pop	ecx
	pop	eax
	ret
FillFreeMem	endp

;==============================================================================
;==
;==  FillXMS: Fill XMS information into the EMM Global Import Data Structure.
;==
;==  Entry: (Protected Mode)
;==	FS:[EBX]= start of EMM Global Import Data Structure
;==	ESI	= size of variable fields in EMM Global Import Data Structure
;==	DS	= _DATA
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:
;==
;==============================================================================
FillXMS	proc	near
;
;  No XMS EMB management is done in CEMM
;
	mov	fs:[ebx][esi].EMMGI_XMScnt,0

	ret
FillXMS	endp

;==============================================================================
;==
;==  FillFreeUMBs: Fill free UMB memory structures in the EMM Global Import
;==		   Data Structure.
;==
;==  Entry: (Protected Mode)
;==	FS:[EBX]= start of EMM Global Import Data Structure
;==	ESI	= size of variable fields in EMM Global Import Data Structure
;==	DS	= _DATA
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:
;==
;==============================================================================
FillFreeUMBs	proc	near
	push	eax
	push	ecx
	push	edi
	push	dx

;
;  Fill free UMB memory region structures
;
	mov	ecx,esi
	mov	fs:[ebx][ecx].EMMGI_FreeUMBcnt,0; initialize count to zero

	movzx	edi,gs:[UMBptr]
	or	di,di				;Q: UMBs provided?
	jz	short FFUexit			; N: exit
	shl	edi,4				; Y: offset to first ARENA
FFUloop:
	cmp	fs:[edi].Sig,'M'		;Q: Is it an ARENA?
	je	short FFUlcont			; Y: OK
	cmp	fs:[edi].Sig,'Z'		;Q: Is it an ARENA?
	jne	short FFUexit			; N: exit, corruption
;
;  Valid ARENA: Get starting paragraph and length
;
FFUlcont:
	mov	eax,edi				; get current paragraph
	shr	eax,4
	inc	ax				; AX is starting paragraph
	mov	dx,fs:[edi].Len			; get length

	cmp	fs:[edi].Own,0			;Q: Is it a FREE UMB?
	jne	short FFUnext			; N: get next ARENA
;
;  Free UMB encountered: place info in structure
;
	inc	fs:[ebx][ecx].EMMGI_FreeUMBcnt
	mov	fs:[ebx][EMMGI_FreeUMB][esi].UMBseg,ax
	mov	fs:[ebx][EMMGI_FreeUMB][esi].UMBsize,dx

	add	si,size UMB_FREE_Map

FFUnext:
	cmp	fs:[edi].Sig,'Z'		;Q: Is it end of ARENA chain?
	je	short FFUexit			; Y: exit
	add	ax,dx				; N: next ARENA
	shl	eax,4
	mov	edi,eax
	jmp	short FFUloop

FFUexit:
	pop	dx
	pop	edi
	pop	ecx
	pop	eax
	ret
FillFreeUMBs	endp

;==============================================================================
;==
;==  FillProductName: Fill in the name of the vendor and the product 
;==
;==  Entry: (Protected Mode)
;==	FS:[EBX]= start of EMM Global Import Data Structure
;==	ESI	= size of variable fields in EMM Global Import Data Structure
;==	DS	= _DATA
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:
;==
;==============================================================================
FillProductName	proc	near

	push	esi
	push	edi
	push	ecx
	push	ds
	push	es

	mov	ecx, esi

	lea	edi, fs:[ebx][ecx].EMMGI_Vendor
	push	fs
	pop	es		; es:di -> destination

	
	lea	esi, gs:[EMMGIVendor]
	push	gs
	pop	ds		; ds:si -> source string

	mov	cx, 20		; move 40 bytes

	REP32MOVSW

	pop	es
	pop	ds
	pop	ecx
	pop	edi
	pop	esi

	ret

FillProductName	endp


;############################################################################
;
;	Procedure name		: Update_mapping_state
;
;	ENTRY			: Protected mode
;				: DS -> _DATA
;			  	  FS->DATA32_GSEL
;			  	  GS->R_CODE
;
;	EXIT			: mapping state is updated with the info
;				  passed by win386 in EMMCtxt array
;
;	REGs MOD		: NONE
;
;
;	written			: 7/27/89 HKN
;
;############################################################################
Update_mapping_state	proc	near
	push	ebx
	push	esi
	push	edi
	push	ax
	push	cx
	push	dx
	push	es

	mov	di,DATA32_GSEL	; need ES = DATA32_GSEL for MapHandlePage function
	mov	es,di
;
;  Loop through the 64 Context structures and map the EMS ones
;
	mov	edi,ebx		; need EBX for MapHandlePage routine
	xor	esi,esi
	mov	cx,64
UMSloop:
;
; Check if this is a 16K EMS window
;
	test	fs:[edi][EMMGI_Cntxt][esi].EMMCntx_Flags,EMM_PAGE
	jz	short UMSnext	; no - try next context structure

;;	cmp	gs:[NoEMSset],TRUE	;Q: NoEMS mode active?
;;	je	short UMSnext		; Y: must be an error, skip it

	cmp	gs:[VCPIset], -1	; Q: has noems been specifed
	jne	short UMSnext		; Y: must be an error, skip it

;
;  Get current state from this EMS context structure
;
	movzx	ax,fs:[edi][EMMGI_Cntxt][esi].EMMCntx_PPag
	mov	bx,fs:[edi][EMMGI_Cntxt][esi].EMMCntx_LPag
	movzx	dx,fs:[edi][EMMGI_Cntxt][esi].EMMCntx_HMap
;
;  We are using the MapHandlePage function to set the mapping.
;  SS:[EBP].reg_EAX must have access to the AX register.
;

;	push	ax
;	push	ebp
;	mov	bp,sp
;	sub	bp,reg_EAX-4
;	push	edi
;	push	esi

	pushad
	mov	bp, sp

	call	MapHandlePage

;	pop	esi
;	pop	edi
;	add	bp,reg_EAX-4
;	pop	ebp
;	pop	ax

	popad

;
;  Check for an error during the mapping
;
	or	ah,ah				;Q: Succesful mapping?
	jnz	short UMSerror			; N: set error flag (carry)

UMSnext:

	add	esi,size EMMContext		; next EMMCntx entry
	loop	UMSloop
	clc

UMSexit:
	pop	es
	pop	dx
	pop	cx
	pop	ax
	pop	edi
	pop	esi
	pop	ebx
	ret

UMSerror:
	cmp	ah,INVALID_HANDLE	;Q: Handle which didn't unmap?
	je	short UMSnext		; Y: continue

	stc				; set carry (error)
	jmp	short UMSexit

Update_mapping_state	endp

;==============================================================================
;==
;==  Get_Phys_PNum
;==
;==  Entry: (Protected mode)
;== 	DS = _DATA
;==	DX = segment value (0 thru A000) (16K bounded
;==	DS = _DATA
;==	FS = DATA32_GSEL
;==	GS = R_CODE
;==	EBX= global import data
;==
;==  Exit: (Protected Mode)
;==	EAX = modified
;==
;==		: if ( segment in DX is a mappable EMM Page )
;==			     &&
;==			     (
;==			       (import version >= 1.11 && noems not specified)
;==			       ||
;==			       import version < 1.11
;==			     )
;==
;==				EAX = Physical page number
;==			  else
;==				EAX = -1
;==
;==============================================================================
Get_Phys_Pnum	proc	near
	push	edx

	mov	eax,-1		; assume this is not an EMS window
;
;  Because of a bug in Win 3.0, we cannot pass a null if in NoEMS mode
;

	cmp	word ptr fs:[ebx][EMMGI_Vers],EMMGI_VERSION_111 ;Q: Win 3.0?
	jb	short GPPcont		; Y: can't pass null ems map as

	cmp	gs:[VCPIset],-1		;Q: Has noems been specifed
	jne	short GPPexit		; Y: assumption is correct
GPPcont:
	mov	dl,dh
	movzx	edx,dl
	movsx	eax,[EMSsegLoc][edx]

GPPexit:
	pop	edx
	ret
Get_Phys_Pnum	endp

ifdef QEMS
;###########################################################################
;
;	Procedure name	: Get_Phys_PNum
;
;	ENTRY		: Protected mode
;			: DS -> _DATA
;			: DX = segment value (0 thru A000) (16K bounded
;			: DS -> _DATA
;			: FS->DATA32_GSEL
;			: GS->R_CODE
;			: EBX -> global import data
;
;	EXIT		: if ( segment in DX is a mappable EMM Page )
;			     &&
;			     ( 
;			       (import version >= 1.11 && noems not specified)
;			       || 
;			       import version < 1.11 
;			     )
;
;				EAX = Physical page number
;			  else
;				EAX = -1
;
;	REGs MOD	: EAX
;
;	written		: 7/26/89 HKN
;
;###########################################################################
Get_Phys_Pnum	proc	near
	push	dx
	push	cx

	cmp	word ptr fs:[ebx][EMMGI_Vers],EMMGI_VERSION_111	
					; Q: Atleast 1.10?
	jb	GPPcont			; N: can't pass null ems map as 
					;    win3.0 cannot handle it.

;;	cmp	gs:[NoEMSset],TRUE	; Q: Is NoEMS mode active?
;;	je	short GPPnotFound	; Y: don't check for EMS

	cmp	gs:[VCPIset], -1		; Q: has noems been specifed
	jne	short GPPnotFound	; Y: don't check for EMS

GPPcont:
	shr	dx,8			; convert segment in dx to PTE offset
	mov	cx,[number_EMS_windows]
	xor	eax,eax

next_window:
	cmp	dx,EMS_window_location[eax*2]
	je	short got_phys_page
	inc	eax
	loop	next_window

GPPnotFound:
	mov	eax, -1

got_phys_page:
	pop	cx
	pop	dx
	ret
Get_Phys_Pnum	endp
endif
even
;==============================================================================
;==
;==  GetEMSInfo: Get handle and the logical page to which this physical page
;==	         belongs.
;==
;==  Entry: (Protected Mode)
;==	EAX	= index for EMS windows
;==	EDX	= segment address for 16K block size
;==	FS:[EBX]= start of EMM Global Import Data Structure
;==	ESI	= offset into current UMB structure
;==	EDI	= offset into current CONTEXT structure
;==	DS	= _DATA
;==	ES	= PAGET_GSEL
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:
;==
;==============================================================================
GetEMSinfo proc	near
	push	edx
	push	esi
	push	es

	push	ebx
;
;  Fill in physical page number
;
	mov	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_PPag,al

;
;  Indicate that it is an EMM page
;
	mov	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_Flags,EMM_PAGE

	cmp	dx,8000h		;Q: Possible LIM 3.2 page frame?
	jbe	short GEiCont		; N: too low (no page frame)

	cmp	al,4			;Q: Is it page frame?
	jae	short GEiCont		; N: must be windows 0.3

	or	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_Flags,FRAME_32
GEiCont:
;
;  Setup for searching through Handles
;
	movzx	esi,dx
	shr	si,8
	mov	eax,es:[esi*4]	; get page currently mapped at this window
	shr	eax,12		; clear control bits

	mov	si,PAGED_GSEL	; access handle space
	mov	es,si

	mov	cx,[total_handles]	; number of handles to loop through
GEiLoop:
;
;  Search each handle for ownership of this page
;
	dec	cx			; handle index
	movzx	esi,cx			; handle index
	shl	si,2
	add	esi,[hndl_tbl_ptr]	; (EDI) = pointer to handle structure
	.errnz	(SIZE HandleTable_struc-4)

	movzx	edx,ds:[si].number_PTEs	    ; get number of PTEs
	movzx	esi,ds:[si].base_PTE_index  ; get base_PTE_index

	cmp	si,FREE 		    ;Q: Handle in use?
	je	short GEiNextHandle	    ; N: next handle

	shr	dx,2			; number of EMS pages to this handle
	shl	esi,2			; starting offset for handle space

	or	dx,dx			;Q: Does this handle have pages?
	jz	short GEiNextHandle	; N: it can't be the one we're looking for
GEiFindPage:
	dec	dx
	shl	dx,2			; adjust to 4K page number
	mov	ebx,es:[esi][edx*4]	; get logical page
	shr	dx,2			; re-adjust to 16K page number
	shr	ebx,12			; make a PTE index

	cmp	eax,ebx			;Q: Is there a match?
	je	short GEiExit		; Y: exit

	or	dx,dx			;Q: More logical pages for this handle
	jnz	short GEiFindPage	; Y: continue

GEiNextHandle:
	or	cx,cx			;Q: Anymore handles?
	jnz	short GEiLoop		; Y: continue

;
;  This window is currently not mapped
;
	mov	cl,FREE		; this window is free
	mov	dx,NULL_PAGE	; no logical page
;
;  At this point, CL is the handle index and DX is the logical page number
;
GEiExit:
	pop	ebx
	mov	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_HMap,cl
	mov	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_Lpag,dx

	pop	es
	pop	esi
	pop	edx
	ret
GetEMSinfo	endp

even
;==============================================================================
;==
;==  GetUMBInfo: Detects a UMB in a 16K area.  If detected, the EMM Global
;==		 Import Data Structure will be updated in the CONTEXT structure
;==		 and the UMB structure.
;==
;==  Entry: (Protected Mode)
;==	EDX	= segment address for 16K block size
;==	FS:[EBX]= start of EMM Global Import Data Structure
;==	ESI	= offset into current UMB structure
;==	EDI	= offset into current CONTEXT structure
;==	DS	= _DATA
;==	ES	= PAGET_GSEL
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:
;==
;==============================================================================
GetUMBinfo proc	near
	push	eax
	push	ecx
	push	dx

;
;  ESI is offset into current UMB array structure entry: Initialize it
;
	mov	fs:[ebx][EMMGI_UMBs][esi].EUM_Page0,0
	mov	fs:[ebx][EMMGI_UMBs][esi].EUM_Page1,0
	mov	fs:[ebx][EMMGI_UMBs][esi].EUM_Page2,0
	mov	fs:[ebx][EMMGI_UMBs][esi].EUM_Page3,0

	xor	ecx,ecx			; start with first 4K page
	shr	dx,8			; linear index into page table
GUiLoop:
;
;  Assume this page is "unuseable"
;
	shl	cx,1
	mov	ax,CNTXT_NOTUSED
	shl	ax,cl
	or	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_EFlgs,al
	shr	cx,1
;
;  Check linear=physical
;
	cmp	dx,0FFh			;Q: Last ROM page?
	je	short GUiNext		; Y: Windows can't write protect it!

	mov	eax,es:[edx*4]		; get PTE entry
	bt	eax, fWINPageBit	;Q: WINdows page?
	jnc	short GUiNotWinPage	; N:

	call	GUiUseable		; Y: mark page "useable" for Windows
	jmp	short GUiNext

GUiNotWinPage:
	shr	eax,12			; turn into physical index

	cmp	eax,edx			;Q: Linear=Physical?
	je	short GUiNext		; Y: not a UMB and thus assume unuseable
;
;  UMB found: unmark "unuseable" assumption
;
	call	GUiUseable
;
;  Indicate a UMB
;
	push	ax
	mov	ax,1
	shl	ax,cl
	shl	ax,CNTXT_UMB0bit
	or	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_Flags,al
	pop	ax
;
;  Save physical index in UMB structure
;
	mov	fs:[ebx][EMMGI_UMBs][esi].EUM_Page0,eax
;
;  Next 4K page
;
GUiNext:
	add	si,type EUM_Page0
	inc	dx
	inc	cx
	cmp	cx,4
	jb	short GUiLoop
	sub	si,size EMM_UMB_Map
;
;  Update UMB array pointer
;
	test	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_Flags,-1 ;Q: Any UMBs?
	jz	short GUiExit			   ; N: next context
	mov	al,fs:[ebx].EMMGI_UMBcnt	   ; Y: save UMB array index

	mov	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_UMBindex,al
	inc	fs:[ebx].EMMGI_UMBcnt		; increment UMB array count
	add	si,size EMM_UMB_Map		; add to variable portion

GUiExit:
	pop	dx
	pop	ecx
	pop	eax
	ret
GetUMBinfo	endp


;  mark page as "useable"

GUiUseable proc near

;  Unmark "unusable" assumption

	shl	cx,1
	push	ax
	mov	ax,CNTXT_NOTUSED
	shl	ax,cl
	not	ax
	and	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_EFlgs,al ; "unknown" status
;
;  Mark as "useable"
;
	mov	ax,CNTXT_USEABLE
	shl	ax,cl
	or	fs:[ebx][EMMGI_Cntxt][edi].EMMCntx_EFlgs,al
	shr	cx,1
	pop	ax
	ret

GUiUseable endp

;==============================================================================
;==
;==  GetFreeEMSRange: Finds next free EMS range.
;==
;==  Entry: (Protected Mode)
;==	EDI	= starting index in handle space to search
;==	DS	= _DATA
;==	ES	= PAGET_GSEL
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:
;==	EAX	= starting page number of free range (-1 if range not found)
;==	ECX	= free region size in number of pages
;==
;==============================================================================
GetFreeEMSRange	proc	near
	push	ebx
	push	edx
	push	si

	mov	edx,[page_directory]
GFERloop1:
	cmp	di,[TopOfFreeEMSspace]
	jae	short GFERdone

	mov	eax,fs:[edx][edi*4]
	add	di,4

	test	ax,fEMSPageAllocated
	jnz	short GFERloop1

	mov	ecx,4
	mov	ebx,eax
GFERloop2:
	add	ebx,4000h

	cmp	di,[TopOfFreeEMSspace]
	jae	short GFERcont

	cmp	ebx,fs:[edx][edi*4]
	jne	short GFERcont

	add	cx,4
	add	di,4
	jmp	short GFERloop2

GFERcont:
	shr	eax,12

;
;  EAX = free pool start (or -1) and ECX = count of free pool
;
GFERexit:
	pop	si
	pop	edx
	pop	ebx
	ret
GFERdone:
	mov	eax,-1
	jmp	short GFERexit
GetFreeEMSRange	endp

ifdef QEMS
;==============================================================================
;==
;==  GetFreeEMSRange: Finds next free EMS range.
;==
;==  Entry: (Protected Mode)
;==	EAX	= starting page number for search
;==	DS	= _DATA
;==	ES	= PAGET_GSEL
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:
;==	EAX	= starting page number of free range (-1 if range not found)
;==	ECX	= free region size in number of pages
;==
;==============================================================================
GetFreeEMSRange	proc	near
	push	ebx
	push	edx
	push	si
;
;  Initialize local variables
;
GFERinit:
	mov	si,word ptr [AllocMapPtr]

	xor	ebx,ebx
	xor	ecx,ecx
	xor     edx,edx
	dec	edx
;
;  Figure out the EMS pool above and below where we are
;
	cmp	gs:[starting_conv_mem_PTE],0
	jz	short GFERhigh

	cmp	ax,gs:[starting_conv_mem_PTE]
	jb	short GFERcon2
	movzx	ebx,gs:[starting_conv_mem_PTE]
	mov	cx,gs:[number_conv_mem_PTEs]
	jmp	short GFERhigh
GFERcon2:
	movzx	edx,gs:[starting_conv_mem_PTE]
	ror	ecx,16
	mov	cx,gs:[number_conv_mem_PTEs]
	ror	ecx,16

GFERhigh:
	cmp	gs:[starting_high_mem_PTE],0
	jz	short GFERext

	cmp	ax,gs:[starting_high_mem_PTE]
	jb	short GFERhigh2
	cmp	bx,gs:[starting_high_mem_PTE]
	ja	short GFERext
	movzx	ebx,gs:[starting_high_mem_PTE]
	mov	cx,gs:[number_high_mem_PTEs]
	jmp	short GFERext
GFERhigh2:
	cmp	dx,gs:[starting_high_mem_PTE]
	jb	short GFERext
	movzx	edx,gs:[starting_high_mem_PTE]
	ror	ecx,16
	mov	cx,gs:[number_high_mem_PTEs]
	ror	ecx,16

GFERext:
	cmp	gs:[starting_ext_mem_PTE],0
	je	short GFERcont

	cmp	ax,gs:[starting_ext_mem_PTE]
	jb	short GFERext2
	cmp	bx,gs:[starting_ext_mem_PTE]
	jae	short GFERcont
	movzx	ebx,gs:[starting_ext_mem_PTE]
	mov	cx,gs:[number_ext_mem_PTEs]
	jmp	short GFERcont
GFERext2:
	cmp	dx,gs:[starting_ext_mem_PTE]
	jb	short GFERcont
	movzx	edx,gs:[starting_ext_mem_PTE]
	ror	ecx,16
	mov	cx,gs:[number_ext_mem_PTEs]
	ror	ecx,16
;
;  We now have the lower pool in EBX,ECX(low) and high pool at EDX,ECX(high)
;
GFERcont:
	add	cx,bx
	cmp	ax,cx		;Q: Are we in lower pool?
	jae	short GFERhiPool; N: try higher pool
	movzx	edx,cx		; Y: last PTE+1 of this pool
	jmp	short GFERusedloop

GFERhiPool:
	shr	ecx,16
	mov	eax,edx		; start of next pool
	add	dx,cx		; end of this pool
	cmp	eax,-1		;Q: Have we reached the end of EMS memory?
	je	short GFERexit	; Y: exit

GFERusedloop:
	bt	ds:[si],ax			;Q: Allocated?
	jnc	short GFERfree			; N: this range is free
	inc	ax				; Y: try next bit
	cmp	ax,dx				;Q: Have we reached the end of pool?
	jb	short GFERusedloop		; N: continue searching
	jmp	GFERinit			; Y: go to next pool

GFERfree:
	mov	bx,ax				; free pool start
	xor	ecx,ecx				; initialize count to zero
	inc	bx				; we have already tested first page
	inc	cx				; we know we have atleast one page
GFERfreeloop:
	bt	ds:[si],ax			;Q: Allocated?
	jc	short GFERexit			; Y: end of the free pool
	inc	cx
	inc	bx
	cmp	bx,dx				;Q: Reached end of EMS pool?
	jb	short GFERfreeloop		; N: see if next PTE is free
;
;  EAX = free pool start (or -1) and ECX = count of free pool
;
GFERexit:
	pop	si
	pop	edx
	pop	ebx
	ret
GetFreeEMSRange	endp
endif
ifdef QEMS
;==============================================================================
;==
;==  GetFreeEMSRange: Finds next free EMS range.
;==
;==  Entry: (Protected Mode)
;==	EAX	= starting page number for search
;==	DS	= _DATA
;==	ES	= PAGET_GSEL
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:
;==	EAX	= starting page number of free range (-1 if range not found)
;==	ECX	= free region size in number of pages
;==
;==============================================================================
	push	ebx
	push	edx
	push	si

	mov	si,word ptr [AllocMapPtr]
	xor	ecx,ecx

GFERloop:
	movzx	ebx,gs:[starting_conv_mem_PTE]
	movzx	edx,gs:[number_conv_mem_PTEs]

	call	CheckRange
	jnc	short GFERcont

	movzx	ebx,gs:[starting_ext_mem_PTE]
	movzx	edx,gs:[number_ext_mem_PTEs]

	call	CheckRange
	jnc	short GFERcont

	movzx	ebx,gs:[starting_high_mem_PTE]
	movzx	edx,gs:[number_high_mem_PTEs]

	call	CheckRange
	jnc	short GFERcont
	mov	eax,-1
	jmp	short GFERexit

GFERcont:
	mov	ebx,eax				; start of range
	bt	ds:[si],ax			;Q: Allocated?
	jnc	short GFERfree			; N: this range is free

;QEMS	test	es:[eax*4],PAGE_ALLOCATED_BIT_MASK;Q: Allocated?
;QEMS	jz	short GFERfree			  ; N: this range is free
	inc	eax				  ; Y: try next PTE

	cmp	eax,edx				;Q: Reached end of EMS pool?
	jb	short GFERcont			; N: try next PTE
	jmp	short GFERloop			; Y: try next EMS pool

GFERfree:
	inc	ecx				; free range size
	inc	ebx				; try next PTE

	cmp	ebx,edx				;Q: Reached end of EMS pool?
	jae	short GFERexit			; Y: exit

	bt	ds:[si],bx			;Q: Allocated?
	jnc	short GFERfree			; N: increase range size

;QEMS	test	es:[ebx*4],PAGE_ALLOCATED_BIT_MASK;Q: Allocated?
;QEMS	jz	short GFERfree			  ; N: increase range size
GFERexit:					  ; Y: return free region
	pop	si
	pop	edx
	pop	ebx
	ret
GetFreeEMSRange	endp
;==============================================================================
;==
;==  CheckRange: Find an EMS range.
;==
;==  Entry: (Protected Mode)
;==	EAX	= entry to search higher than
;==	EBX	= start of a EMS pool range
;==	EDX	= number of PTE entries in pool range
;==	DS	= _DATA
;==	ES	= PAGET_GSEL
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:
;==	EAX	= starting page number to start search
;==	EDX	= last PTE entry for this pool + 1
;==	NC	= within EMS range
;==	CY	= outside EMS range
;==
;==============================================================================
CheckRange proc	near

	add	edx,ebx		; boundary for EMS pool

	or	edx,edx		;Q: Valid EMS pool range?
	jz	short CRerror	; N: exit

	cmp	eax,ebx		;Q: Below EMS pool?
	jbe	short CRbelow	; Y: start at base of EMS pool

	cmp	eax,edx		;Q: Above EMS pool?
	jb	short CRok	; N: OK, within range
CRerror:
	stc			; check next EMS pool range
	jmp	CRexit

CRbelow:
	mov	eax,ebx		; start at the beginning of EMS pool
CRok:
	clc			; Check this EMS pool
CRexit:
	ret
CheckRange 	endp
endif

;==============================================================================
;==
;==  VxDSupport: Add a Win386 Startup Info Structure for a VxD which handles
;==		 UMBs.
;==
;==  Entry: (Real Mode)
;==	DS	= _DATA
;==	ES	= PAGET_GSEL
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==	DI	= Version number: major in upper byte, minor in lower
;==
;==  VMTF_ES:BX	= points to next data structure in linked chain
;==
;==
;==  Exit:
;==  VMTF_ES:BX	= pointer to our data structure
;==
;==============================================================================
VxDSupport proc	near


	push	eax
;
;  Check Page Table 0 for aliasing of first megabyte
;
	call	VxDlistUMB
;
;  If no aliasing in first megabyte, VxD not needed.
;
	cmp	ds:[Win386VxDRefDat].RDSUMBstart,0
	je	short VxDSexit
;
;  Fill in used UMB information in VxD structure
;
	call	VxDUsedUMBList
;
;  Delete EMS windows from the UMB list
;
	call	VxDEMSPages
;
;  Delete C6 and C7 if Cega installed and length > 24 K
;
	call	VxdCROMPages
;
;  We now initialize the WSSnextPtr and WSSVxDFilePtr fields in Win386Struc.
;  The former is initilazed to ES:BX in segment:offset form. The latter 
;  is initialized to CEMMpath that is defined in the R1_CODE segment as 
;  a linear address. Note that the segment of R1_CODE segment is obtained
;  from any one of the entries in the UMBFARTABLE defined in umbseg.asm.
;
;
	push	edi
	movzx	edi, word ptr gs:[EMM_rFarEntry+2]
	shl	edi, 4
	add	edi, OFFSET R1_CODE:Win386Struc

	;
	; edi = 32 bit pointer to Win386Struc defined in R1_CODE
	;

	mov	ax,[bp][VTFO].VMTF_ES
	shl	eax,16
	mov	ax,bx
	mov	fs:[edi].WSSnextPtr,eax

	mov	ax, gs:[EMM_rFarEntry+2]
	shl	eax,16
	lea	ax,R1_CODE:CEMMpath
	mov	fs:[edi].WSSVxDFilePtr,eax
	shr	eax, 16

;
;  Make ES:BX point to Win386 Startup Structure for UMB VxD
;
	mov	[bp][VTFO].VMTF_ES,ax
	mov	bx,OFFSET R1_CODE:Win386Struc
	pop	edi

VxDSexit:
	pop	eax
	ret
VxDSupport	endp

;==============================================================================
;==
;==  VxDlistUMB: Initialize Windows 386 UMB VxD refrence data struture with
;==		 physical addresses of UMBs.
;==
;==  Entry: (Real Mode)
;==	DS	= _DATA
;==	ES	= PAGET_GSEL
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:
;==
;==============================================================================
VxDlistUMB proc	near
	push	eax
	push	ebx
	push	esi
	push	edi
	push	cx
;
;  Make ESI point to PT0 and EDI point to [Win386VxDRefDat].RDSUMBarray
;
	mov	esi,ds:[page_tables]

;;	mov	di,seg R_CODE
;;	movzx	edi,di
;;	shl	edi,4
	mov	edi, gs:[p_DATA];
	add	edi,offset ds:[Win386VxDRefDat].RDSUMBarray

	mov	ebx,0A0h
	mov	cx,100h-0A0h		; loop thru PTEs from A000 to 1MB
VlUFindFirst:
	mov	eax,fs:[esi][ebx*4]
	shr	eax,12
	cmp	eax,ebx			;Q: Have we reached the first lin<>phy?
	jne	short VlUFirst		; Y: first UMB page
	inc	bx			; get next PTE
	loop	VlUFindFirst
	jmp	short VlUexit

;
;  First UMB has been detected
;
VlUFirst:
	mov	ds:[Win386VxDRefDat].RDSUMBstart,ebx
VlUFound:
	cmp	eax,ebx			; Q: lin = phy
	jne	VlUFcont		; N:
	xor	eax,eax			; Y: stuff 0 in UMBarray
VlUFcont:
	mov	fs:[edi],eax
	add	edi,4
	inc	bx
	mov	eax,fs:[esi][ebx*4]
	shr	eax,12
	loop	VlUFound

VlUexit:
	mov	gs:[pRDSdata],di	; address of next entry in Win386VxDRefData

	pop	cx
	pop	edi
	pop	esi
	pop	ebx
	pop	eax
	ret
VxDlistUMB	endp

;==============================================================================
;==
;==  VxDUsedUMBList: Fills VxD data structure with used UMB area.
;==
;==  Entry: (Protected Mode)
;==	DS	= _DATA
;==	ES	= PAGET_GSEL
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:
;==
;==============================================================================
VxDUsedUMBList	proc	near
	push	eax
	push	edx
	push	edi
;
;  VxD Used List only if UMBptr
;
	cmp	gs:[UMBptr],0		;Q: UMB ARENAs on the system?
	je	VUULexit		; N: exit
;
;  Get free UMB memory region areas
;
	movzx	edi,gs:[UMBptr]
	or	di,di				;Q: UMBs provided?
	jz	VUULexit			; N: exit
	shl	edi,4				; Y: offset to first ARENA
VUULloop:
	cmp	fs:[edi].Sig,'M'		;Q: Is it an ARENA?
	je	short VUULlcont			; Y: OK
	cmp	fs:[edi].Sig,'Z'		;Q: Is it an ARENA?
	jne	short VUULexit			; N: exit, corruption
;
;  Valid ARENA: Get starting paragraph and length
;
VUULlcont:
	mov	eax,edi				; get current paragraph
	shr	eax,4
	movzx	edx,fs:[edi].Len		; M015: get length and 
						; M015: Zxtend it.
	add	dx,2

	cmp	fs:[edi].Own,0			;Q: Is it a FREE UMB?
	jne	short VUULnext			; N: get next ARENA

	push	eax
	push	edx

	shl	eax,4
	shl	edx,4

	add	edx,eax
	add	eax,1000h-1

	shr	eax,12
	shr	edx,12

	sub	eax,ds:[Win386VxDRefDat].RDSUMBstart
	sub	edx,ds:[Win386VxDRefDat].RDSUMBstart
	cmp	eax,edx
	jae	short VUULnoFreePage
VUULclear:
	mov	ds:[Win386VxDRefDat][eax*4].RDSUMBarray,0
	inc	eax
	cmp	eax,edx
	jb	short VUULclear
VUULnoFreePage:
	pop	edx
	pop	eax

VUULnext:
	cmp	fs:[edi].Sig,'Z'		;Q: Is it end of ARENA chain?
	je	short VUULexit			; Y: exit
	add	ax,dx				; N: next ARENA
	dec	ax
	shl	eax,4
	mov	edi,eax
	jmp	VUULloop

VUULexit:
	pop	edi
	pop	edx
	pop	eax
	ret
VxDUsedUMBList	endp

;==============================================================================
;==
;==  VxDEMSPages: Clear EMS pages from the VxD UMB list
;==
;==  Entry: (Protected Mode)
;==	DS	= _DATA
;==	ES	= PAGET_GSEL
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:
;==
;==============================================================================
VxDEMSPages	proc	near
	push	eax
	push	esi
	push	cx

;
;  Check if any EMS pages exist
;
	cmp	gs:[NoEMSset],TRUE	;Q: EMS available?
	je	short VEPexit		; N: exit

	cmp	gs:[NoPFset],TRUE	;Q: FRAME=NONE specified?
	je	short VEPexit		; Y: exit

;
;  Check to see if EMS pages are part of the UMB list
;
	xor	esi,esi
	mov	cx,[number_EMS_windows]	; number of EMS windows

VEPEMSLoop:
	movzx	eax,[EMS_window_location][esi*2]	; get EMS linear page
	sub	eax,ds:[Win386VxDRefDat].RDSUMBstart	; offset into UMB list
	jb	short VEPnext				; not pat of UMB list

;
;  Delete the EMS page from the UMB list
;
	push	cx
	mov	cx,4
VEPClearLoop:
	mov	ds:[Win386VxDRefDat][eax*4].RDSUMBarray,0
	inc	eax
	loop	VEPClearLoop
	pop	cx
VEPnext:
	inc	si
	loop	VEPEMSLoop

VEPexit:
	pop	cx
	pop	esi
	pop	eax
	ret
VxDEMSPages	endp

;==============================================================================
;==
;==  VxDCROMPages: Clear the Video ROM alias pages if a non-CPQ ROM is detected.
;==		   This is to prevent Win 3.0 from crashing. Also clear page
;==		   page at FF/reset vector.
;==
;==  Entry: (Protected Mode)
;==	DS	= _DATA
;==	ES	= PAGET_GSEL
;==	FS	= DATA32_GSEL
;==	GS	= R_CODE
;==
;==  Exit:
;==
;==============================================================================
VxDCROMPages	proc	near
	push	eax
	push	cx
;
;  Do not alias PTE 0FFh because Win 3.0 will not write protect it.

	mov	eax, 0ffh
	sub	eax,ds:[Win386VxDRefDat].RDSUMBstart	
					; offset into UMB list
	mov	ds:[Win386VxDRefDat][eax*4].RDSUMBarray,0 
					; no aliasing
;
;  Do not alias VIDEO ROM if it's greater than 24K - due to a Win 3.0 problem.
;
	test	gs:[Current_State], fstate_CEGAinst ;Q Shadowed Video ROM installed?
	jz	VCRdone				    ; N: don't worry

	cmp	gs:[CROM_Length],600h	;Q: Larger than 24K
	jbe	short VCRdone		; N: no problem with Win 3.0!

	mov	cx,gs:[CROM_Length]	; get actual size
	shr	cx,8			; size in pages
	mov	eax,0C0h		; starting page

	sub	eax,ds:[Win386VxDRefDat].RDSUMBstart	; offset into UMB list
VCRloop:
	mov	ds:[Win386VxDRefDat][eax*4].RDSUMBarray,0 ; no aliasing
	inc	eax					  ; next page
	loop	VCRloop


ifdef 901004					; CROM_length has length in paras
	cmp	gs:[CROM_Length], 0600h	; Q: is Rom length > 24 K
	jbe	VCRdone			; N:

					; Y: zero out c6 and c7
	mov	eax, 0c6h
	sub	eax,ds:[Win386VxDRefDat].RDSUMBstart	; offset into UMB list
	mov	ds:[Win386VxDRefDat][eax*4].RDSUMBarray,0
	inc	eax
	mov	ds:[Win386VxDRefDat][eax*4].RDSUMBarray,0
endif

VCRdone:
	pop	cx
	pop	eax
	ret
VxDCROMPages	endp

_TEXT	ends

;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
R1_CODE segment
	assume	cs:R1_CODE,ds:nothing,es:nothing
;==============================================================================
;==
;==  rINT2FhHandler: This is an interrupt 2Fh handler which monitors for the
;==	      	     Windows 386 and DOS Extender Initialization.
;==
;==  Entry: (Real Mode)
;==	AX	= 1605h Windows Initialization
;==	ES:BX	= 0:0
;==	DS:SI	= 0:0
;==	DI	= Version number: major in upper byte, minor in lower
;==	CX	= 0
;==	DX	= Flags
;==	  Bit 0 = 0 Windows 386 inititalization
;==	  	  1 Windows 286 DOS extender inititalization
;==
;==  Exit:
;==
;==	AX	= 1605h Windows Initialization
;==	ES:BX	= pointer to linked instance data structure chain
;==	DS:SI	= far call entry point for Virtual Disable Call
;==	DI	= Version number: major in upper byte, minor in lower
;==	CX	= 0 if OK for Windows to initialize
;==	        <>0 if Windows should NOT initialize
;==	DX	= Flags
;==	  Bit 0 = 0 Windows 386 inititalization
;==	  	  1 Windows 286 DOS extender inititalization
;==
;==============================================================================
rINT2FhHandler:

	pushf

	cmp	ah,16h			;Q: Windows Init or Exit?
	jne	short rI2Fquick 	; N: chain

	push	ds
	push	seg R_CODE
	pop	ds
	assume	ds:R_CODE

	cmp	al,05h			;Q: Windows Init?
	je	short rI2FWinInit	; Y: process
	cmp	al,06h			;Q: Windows Exit?
	jne	short rI2FoldHandler	; N: exit
	and	[GenFlags],not fWin30	; Y: Win 3.0 broadcast over

	mov	[EMMGIDataVer],EMMGI_VERSION_LO
					; set import veriosn back to base 1.00 

	cmp	[Current_Mode],MODE_OFF ;Q: Is CEMM off?
	je	short rI2FoldHandler	; Y: chain
	mov	[DevName],'E'		; N: make sure EMS on

;;        cmp     cs:[NoEMSset],TRUE	;Q: Is CEMM in NoEMS mode?
;;        jne	short rI2FoldHandler	; N: chain

	mov	[DevName+3], 'X'	; ensure that 4th char in device
					; name is X rather than Q.

	cmp	[VCPIset], -1		; Q: has noems been specifed.
        je	short rI2FoldHandler	; N: chain

	cmp	[VCPIset], FALSE	; Q: did user specify only NOEMS
	jne	short rI2FSpecialSig	; N: noems+poolsize was specfied

	mov	[DevName],'$'		; Y: make sure EMS is off
	jmp	short rI2foldHandler

rI2FSpecialSig:
	mov	[Devname+3], 'Q'	; set up sig to EMMQXXX.

rI2FoldHandler:
	pop	ds
rI2Fquick:
	popf
	jmp	cs:[PrevInt2F]

;============================================================================
;=  Instance data, install a driver, and Virtual Disable Call entry point
;============================================================================
rI2FWinInit:
        test    dx,1				; M013: Q: Win Standard Mode?
        jnz	short rI2FStdInit		; M013: Y: do std. mode init

	cmp	[Current_Mode],MODE_OFF 	;Q: Is CEMM off?
        je      short rI2FoldHandler		; Y: chain

	call	[ValidPathFar]			; M010: Q: is the path valid
	jc	short rI2FInvPath		; M010: N: fail win start up

	test	[GenFlags], fBackfill		; Q: is backfill on
	jnz	short rI2Fbackfill		; Y: fail win start up

	test	[Current_State],fState_Active	;Q: Is CEMM active?
	jnz	short rI2Fhook			; Y: hook broadcast

	mov	[DevName],'$'			; N: turn EMS off
        jmp	short rI2FoldHandler		; chain

;============================================================================
;	Standard mode init: Change device name to EMMQXXX 
;============================================================================

rI2fStdInit:
	cmp	di, WIN_VERSION_LO	; Q: is version > 3.0
	jbe	rI2foldHandler		; N: just chain
	mov	[Devname], 'E'		; Y: change device name
	mov	[Devname+3], 'Q'
	jmp	short rI2foldHandler	

;==============================================================================
;==  Fail Windows start up due to base memory back fill
;==============================================================================

rI2Fbackfill:
	push	dx
	lea	dx, R1_CODE:WinBackfillMess
	jmp	short rI2fPrintFail		; print msg and ret failure

;==============================================================================
;==  Fail Windows start up due to invalid path
;==============================================================================

rI2FInvPath: 					; M010 - Start
	push	dx
	lea	dx, R1_CODE:WinInvPathMess	; print error message

rI2fPrintFail:
	push	ax
	mov	ax, cs
	mov	ds, ax			; ds = segment of R1_CODE
	assume	ds:nothing
	mov	ah, 09h
	int	21h			; print error msg
	pop	ax
	pop	dx
	mov	cx, 1				; indicate failure cx != 0
	jmp	short rI2FoldHandler		; M010 - End

;==============================================================================
;==  Call previous INT 2Fh to get instance data or installable device
;==============================================================================
	assume	ds:R_CODE

rI2Fhook:
	mov	[DevName],'E'		; make sure EMS is on
	mov	[DevName+3], 'X'
;
;  Call previous INT 2Fh to get instance data or installable device
;
	pop	ds
	assume	ds:NOTHING
	popf				; restore original flags
	pushf				; set up for emulated int 2Fh
	call	cs:[PrevInt2F]

	push	ax
	push	ds
	mov	ax, seg R_CODE
	mov	ds, ax
	assume	ds:R_CODE
	mov	ax,2			; code for Win broadcast
;==============================================================================
;==  Update Windows Version and VxD (if Win 3.0)
;==============================================================================
	cli
	or	[TrapFlags],fWinTrap	; dispatch to Ein Global Import
	pop	ds
	assume	ds:NOTHING
	int	ProtTrap		; enter protected mode
	pop	ax
	iret

R1_CODE ends

;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
R_CODE	segment
	assume	cs:R_CODE,ds:nothing,es:nothing
;==============================================================================
;==  rWinV86Proc jump table
;==============================================================================
rWVPtable	label	word
	dw	offset rWinVirDisable
	dw	offset rWinVirEnable
rWVPmax	equ	$-rWVPtable
;==============================================================================
;==
;==  rWinV86Proc: This routine is called by Windows to disable/enable V86
;==		  mode.  During V86 disable, the EMM Global Import data
;==		  structure is updated to reflect CEMM's current EMS and
;==		  linear to physical mapping state.  During V86 enable,
;==		  CEMM will update its internal data structures and page
;==		  tables with information from the EMM Global data structure.
;==		  This allows critical information to be passed to Windows
;==		  during V86 disable, and to CEMM from Windows during V86
;==		  enable.
;==
;==  Entry: (Real/V86 Mode)
;==	AX	= 0	Disable Virtual 8086 mode (enter Real Mode)
;==		= 1	Re-enable Virtual 8086 mode (from Real Mode)
;==
;==  Exit:  (V86/Real Mode)
;==
;==============================================================================
rWinV86Proc	proc	far
	push	bx

	mov	bx,ax
	shl	bx,1

	cmp	bx,rWVPmax		;Q: Valid function?
	jae	short rWinV86exit	; N: error

	clc				; assume success
	call	cs:[rWVPtable][bx]	; call function

rWinV86exit:
	pop	bx
	ret
rWinV86Proc	endp

;==============================================================================
;==
;==  rWinVirDisable: This routine is called by Windows to disable V86 mode
;==		     During V86 disable, the EMM Global Import data structure
;==		     is updated to reflect CEMM's current EMS and linear to
;==     	     physical mapping state. This allows critical information
;==		     to be passed to Windows during V86 disable.
;==
;==  Entry: (V86 Mode)
;==	AX	= 0	Disable Virtual 8086 mode (enter Real Mode)
;==
;==  Exit:  (Real Mode)
;==
;==============================================================================
rWinVirDisable	proc	near
;
;  Windows Virtual Disable Call
;
	and	cs:[GenFlags],not fWin30	; Win 3.0 broadcast complete

	test	cs:[Current_State],fState_Active;Q: Virtual Mode?
	jz	rWVDerror			; N: error

;==============================================================================
;==  Update EMM Global Import data structure from internals
;==============================================================================
	cli
	or	cs:[TrapFlags],fWinTrap ; dispatch to Ein Global Import
	int	ProtTrap		; enter protected mode
;
;  Leave A20 enabled for Windows and indicate Windows is in control
;
	or	cs:[Current_State],fState_A20Ena+fState_WinCntrl

	pushad

;;	push	cs
;;	pop	es

	mov	di, cs:[EMM_rFarEntry+2]	
	mov	es, di			; es = segment of R1_CODE
	assume	es:R1_CODE					
	lea	di,es:[InstanceData]	; es:di -> data struc into which prev.
					; instance info. is to be copied
	mov	cx,MaxUMBInstObj	; maximum instance objects in UMBs
	call	cs:[CopyInstData]
;QLEO
;;	lea	ax,cs:[Win386Struc]
;QLEO
	popad

;
;  V86 disable
;
	call	CEMMoff			; turn CEMM off
	mov	cs:[Devname],'$'	; inactivate CEMM
;
;  Return to Windows
;
rWVDexit:
	clc
	ret
rWVDerror:
	stc
	ret
rWinVirDisable	endp

;==============================================================================
;==
;==  rWinVirEnable: This routine is called by Windows to enable V86 mode.
;==		    During V86 enable, CEMM will update its internal data
;==		    structures and page tables with information from the
;==		    EMM Global data structure.
;==		    This allows critical information to be received from
;==		    Windows during V86 enable.
;==
;==  Entry: (Real Mode)
;==	AX	= 1	Re-enable Virtual 8086 mode (from Real Mode)
;==
;==  Exit:  (V86 Mode)
;==
;==============================================================================
rWinVirEnable	proc	near

	test	cs:[Current_State],fState_Active;Q: Virtual Mode?
	jnz	rWVEerror	 		; Y: already on!

ifdef	MSFLAG
	bt	cs:[Current_State],fState_WinCntrlBit;Q: Waiting for Windows to exit?
else
	btr	cs:[Current_State],fState_WinCntrlBit;Q: Waiting for Windows to exit?
endif

	jnc	rWVEerror	 		     ; N: why this call?

;
;  The user stack is changed to the CEMM Real Stack
;
	mov	cs:[UserSS],ss
	mov	cs:[UserSP],sp
	push	seg R_STACK
	pop	ss
	lea	sp,R_STACK:RealStackTop

	call	ReInitDeb
;
;  The user's segment registers are pushed on the stack.
;
	push	gs
	push	fs
	push	es
	push	ds

	cli				;;; no ints now
	call	GoProtMode
	jc	short WINEMM_rExit

;
; The DS and GS selectors are set up.
;
	push	VDMD_GSEL
	pop	ds
	assume	ds:_DATA
	push	RCODEA_GSEL
	pop	gs
	assume	gs:R_CODE


;
;  Save CEMM real stack and change to protected mode stack
;
	mov	gs:[RealSS],ss
	mov	gs:[RealSP],sp
	push	VDMS_GSEL
	pop	ss
	mov	sp,gs:[StackTop]
;
; The function number is saved and the call done.
;
	PCall	VDMC_GSEL,_TEXT:UpdateInternalsFar
;
;  Restore CEMM real stack
;
	mov	ss,gs:[RealSS]
	mov	sp,gs:[RealSP]

	jc	short gotoreal			; error, don't turn on

	PCall	VDMC_GSEL,_TEXT:EnterVirtual
	call	GoVirtualMode

	jc	short WINEMM_rExit

	or	cs:[Current_State],fState_Active
	mov	cs:[Devname],'E'		     ; activate CEMM
	jmp	short WINEMM_rExit

gotoreal:

	call	GoRealMode
	mov	cs:[Devname],'$'		  ; inactivate CEMM
	and	cs:[Current_State],not fState_Active

	mov	dx,offset WINEMM_Mess		; Y: print out message
	mov	ah,9
	int	21h				; print message

WINEMM_rExit:

ifdef	MSFLAG
	and	cs:[Current_State], NOT fState_WinCntrl
endif

	pop	ds
	pop	es
	pop	fs
	pop	gs
;
;  Restore user stack
;
	mov	ss,cs:[UserSS]
	mov	sp,cs:[UserSP]

WINEMM_done:
	ret

WINEMM_error:
	stc
	ret

ifdef QLEO	; look into problems with switching to V86 mode???? QLEO
;
;  The user stack is changed to the CEMM Real Stack
;
	mov	cs:[UserSS],ss
	mov	cs:[UserSP],sp
	push	seg R_STACK
	pop	ss
	lea	sp,R_STACK:RealStackTop

	call	GoVirtual		;Q: Enter V86 mode
	jc	rWVPerrorX	 	; N: error
;
;  Restore user stack
;
	mov	ss,cs:[UserSS]
	mov	sp,cs:[UserSP]
;
;  Update internal with EMM Global Import data structure
;
	cli
	or	cs:[TrapFlags],fWinTrap ; dispatch to EMM Global Import
	int	ProtTrap		; enter protected mode
	jnc	short rWVPexit		; if no error, return to Windows

;
;  Error restoring EMM Global Import state: set carry
;
	mov	dx,offset WINEMM_Mess	; print out message
	mov	ah,9
	int	21h
	stc				; indicate error

;
;  Restore user stack
;
rWVPerrorX:
	mov	ss,cs:[UserSS]
	mov	sp,cs:[UserSP]
endif

;
;  Set carry due to error
;
rWVEerror:
	stc				; indicate error
	ret
rWinVirEnable	endp

;==============================================================================
;==
;==  CEMMoff: Procedure to turn CEMM off via diagnostic ports.
;==
;==  Entry: (V86 Mode)
;==
;==  Exit:  (Real Mode)
;==
;==============================================================================
CEMMoff	proc	near
	push	ax
	pushf
	cli
;
;  CEMM traps the following IOs and turns off
;
	mov	al,0Fh
	out	84h,al
	mov	al,00h
	out	85h,al
	jmp	$+2

	popf
	pop	ax
	ret
CEMMoff	endp


R_CODE	ends

;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
LAST	segment
	assume	cs:LAST,ds:nothing,es:nothing
;==============================================================================
;==
;==  VxDInit: Initialize Windows 386 VxD.
;==
;==  Entry: (Real Mode)
;==	GS	= R_CODE
;==	DS 	= _DATA
;==	R1_CODE has not been moved.
;==
;==  Exit:
;==
;==============================================================================
VxDInit proc	near

	push	eax
	push	ebx
	push	es
	
	push	fs
	
	mov	ax, seg R1_CODE		; R1_CODE seg not moved yet.
	mov	fs, ax
	assume	fs:R1_CODE
;
;  Place virtual device file name pointer
;
;;	mov	ax,fs
;;	shl	eax,16
;;	lea	ax,fs:[CEMMpath]
;;	mov	fs:[Win386Struc].WSSVxDFilePtr,eax
;
;  Place VxD reference data structure pointer
;
;;	xor	ax,ax
;;	shr	eax,12
;;	add	eax,offset gs:[Win386VxDRefDat]
;;	mov	gs:[Win386Struc].WSSVxDRefData,eax

	mov	eax, gs:[p_DATA]
	add	eax,offset [Win386VxDRefDat]
	mov	fs:[Win386Struc].WSSVxDRefData,eax

;
;  Now fill information for VxD in VxDRefDataStruct: start of device driver chain
;
	xor	eax,eax
	mov	ebx,eax
	mov	ah,52h		; get address of start of device driver chain
	int	21h
	mov	ax,es		; address of ES:[BX][22h]
	shl	eax,4
	add	eax,ebx
	add	eax,22h
	mov	ds:[Win386VxDRefDat].RDSDDchain,eax
;
;  Place instance data structure pointer
;
	xor	eax, eax
	mov	ax, fs
	shl	eax, 4
	add	eax, offset fs:[InstanceData]
	mov	ds:[Win386VxDRefDat].RDSdata,eax

	pop	fs
	pop	es
	pop	ebx
	pop	eax
	ret
VxDInit	endp

;==============================================================================
;==
;==  GetPathName: This procedure will get the path and name of the CEMM device
;==	      	  driver from the CONFIG.SYS buffer.
;==
;==  Entry: (Real Mode)
;==	ES:DI	= pointer to CONFIG.SYS line
;==	GS	= R_CODE
;==	R1_CODE has not been moved.
;==
;==  Exit:
;==
;==============================================================================
GetPathName	proc	near

	push	ax
	push	cx
	push	si
	push	di
	push	ds
	push	es
;
;  Move ES:DI to DS:SI and place R1_CODE:[CEMMpath] in ES:DI
;
	mov	ax,es
	mov	si,gs:[EMM_rFarEntry+2]
				; si = R1_CODE segment
	mov	ds,ax
	mov	es,si
	mov	si,di
	lea	di,R1_CODE:[CEMMpath]

	cld
	mov	cx,ConfigBuffer	; max length cannot be greater than 128 chars

	push	di		; save pointer to destination buffer

GPNnext:
	lodsb			; get a char from buffer
	cmp	al, 0dh		; Q: is it CR
	je	GPNfullname	; Y: try to get full name
	cmp	al, 0ah		; Q: is is LF
	je	GPNfullname	; Y: try to get full name
	cmp	al, ' '		; Q: is it a blank
	je	GPNfullname	; Y: try to get a full name
	cmp	al, 0		; M010: Q: is it a NULL
	je	GPNfullname	; M010: Y: try to get a full name
	stosb			; save char in destination buffer
	loop	GPNnext		; get next char

	;
	; We shall now make an attempt to get the full actual path of the
	; device by calling Int 21 function ah=60h.
	;
GPNfullname:
	mov	byte ptr es:[di], 0
				; M010: NULL terminate
	pop	di		; restore destination buffer pointer

	push	ax		; save last char obtained
	mov	byte ptr ds:[si-1], 0
				; terminate name with NULL
	sub	cx, ConfigBuffer+1		
	neg	cx		; cx = length of string
	sub	si, cx		; rewind si to start of string
	mov	ah, 060h	; call $nametrans (undocumented call)
	int	21h		; returns full path in es:di
				; Note: if this call returns it's not bad as
				; we already copied the device name into the
				; destination.
	pop	ax		; restore last char found in source string
	add	si, cx		; si pointing to end of name
	mov	byte ptr ds:[si-1], al
				; put last char back

GPNexit:
	pop	es
	pop	ds
	pop	di
	pop	si
	pop	cx
	pop	ax
	ret
GetPathName	endp

;---------------------------------------------------------------------------
;
;	M010:
;
;	Procedure Name	: StorePath
;
;	Inputs		: DS:SI -> points to path spec
;	Output		: stores path in CEMMPath
;
;	Uses		: None
;
;---------------------------------------------------------------------------

StorePath	proc	near
	
	push	es
	push	di

	push	ds
	pop	es
	mov	di, si		; set es:di to path
	call	GetPathName

	pop	di
	pop	es
	ret

StorePath	endp
	
	
LAST	ends

	END

