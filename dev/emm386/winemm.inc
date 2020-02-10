ifndef INC_LIST
.xlist
endif
;******************************************************************************
;
; (C) Copyright MICROSOFT Corp. 1989-1991
; (C) Copyright COMPAQ Computer Corp. 1989-1991
;
; Title:	EMM386.EXE  - MICROSOFT Expanded Memory Manager 386 Driver
;
; Module:	WINEMM.INC - defines for EMM code.
;
; Version:	0.001
;
; Date:		July 26, 1989
;
; Author:	HKN, Leo Cohen
;
;******************************************************************************
;
; Change log:
;
; DATE		REV	DESCRIPTION
; --------	------	-----------------------------------------------------
; 07/26/86	0.0
;******************************************************************************

;=============================================================================
;==	D A T A   S T R U C T U R E S
;=============================================================================
Win386StartupStruc	struc
  WSSversion	db	3,0	; version number
  WSSnextPtr	dd	?	; pointer to next entry in the chain
  WSSVxDFilePtr	dd	0	; name/path of VxD to load
  WSSVxDRefData	dd	?	; pointer to VxD defined structure
  WSSInstData	dd	0	; pointer to instace data structure
Win386StartupStruc	ends

VxDRefDataStruct	struc
  RDSDDchain	dd	?	; logical address of start of device driver chain
  RDSdata	dd	?	; pointer to data structure filled in by Ralphs code.
  RDSUMBstart	dd	?	; linear 4K page number with first UMB
  RDSUMBarray	dd (100h-0A0h) dup (0)	; physical PTE index from previous field to 1 megabyte
VxDRefDataStruct	ends

InstanceDataStruct	struc
  IDSptr	dd	0	; logical address of instance data area
  IDSsize	dw	0	; size of instance data area
InstanceDataStruct	ends

;==============================================================================
;==  F L A G S
;==============================================================================
;==============================================================================
;==  EMMGI_Flags
;==============================================================================
ANY_HANDLE_CTXT	equ 00001b	; Conext saved in some handle
NONZR_REG_SET	equ 00010b	; reg set other than 0 allocated
NO_XMS_HANDLE	equ 00100b	; No XMS handle information
HMA_INFO	equ 01000b	; Virtual HMA is free at Win init time
NO_XMS_UMB	equ 10000b	; No XMS UMB information
;==============================================================================
;==  EMMGI_Vers
;==============================================================================
MAJOR_INTERFACE_VER	equ	1
MINOR_INTERFACE_VER_0	equ	0
MINOR_INTERFACE_VER_11	equ	11
WIN_VERSION_LO		equ	300h
WIN_VERSION_HI		equ	30Ah
EMMGI_VERSION_LO	equ (MAJOR_INTERFACE_VER shl 8)+MINOR_INTERFACE_VER_0
EMMGI_VERSION_100	equ (MAJOR_INTERFACE_VER shl 8)+MINOR_INTERFACE_VER_0
EMMGI_VERSION_111	equ (MAJOR_INTERFACE_VER shl 8)+MINOR_INTERFACE_VER_11
EMMGI_VERSION_HI	equ (MAJOR_INTERFACE_VER shl 8)+MINOR_INTERFACE_VER_11
;==============================================================================
;==  EMMCntx_Flags
;==============================================================================
EMM_PAGE		equ 00000001b	; this is a mappable EMM page (either 3.2 or 4.0)
FRAME_32		equ 00000010b	; this page is part of a 3.2 frame
CNTXT_UMB0	 	equ 00000100b	; 1st 4K is mapped as a UMB
CNTXT_UMB1 		equ 00001000b	; 2nd 4K is mapped as a UMB
CNTXT_UMB2	 	equ 00010000b	; 3rd 4K is mapped as a UMB
CNTXT_UMB3	 	equ 00100000b	; 4th 4K is mapped as a UMB
CNTXT_UMB0bit	 	equ 2
;==============================================================================
;==  EMMCntx_EFlgs
;==============================================================================
CNTXT_USEABLE	 	equ 00000001b	; 4K page usable
CNTXT_NOTUSED 		equ 00000010b	; 4K page not usable
;==============================================================================
;==  EMMHndl_Flags
;==============================================================================
HANDLE_HAS_NAME	EQU	0001b	; this handle has a name
HANDLE_HAS_CTXT	EQU	0010b   ; this handle has a saved context
HF_UNUSED	equ 	0100b	; this is not a valid handle entry
;==============================================================================
;==  XMSflags
;==============================================================================
HANDLE_UNUSED	equ	0001b	; this handle is currently not allocated.
;==============================================================================
;==  S T R U C T U R E S
;==============================================================================
;==============================================================================
;==  EMMContext structure
;==============================================================================
EMMContext 	struc
  EMMCntx_Flags	db	?	; one of the above equates
  EMMCntx_HMap	db	?	; if EMM page contains handle number else if UMB
  				; contains index into EMM_UMB_Map struc
  EMMCntx_Lpag	dw	?	; logical page number within handle
  EMMCntx_PPag	db	?	; physical page # (index into EMS_window_location array )
  EMMCntx_EFlgs db	?	; Non EMM/UMB flags
EMMContext	ends
EMMCntx_UMBindex  equ  EMMCntx_HMap ; If UMB, this is index into array
;==============================================================================
;==  EMM_UMB_Map structure
;==============================================================================
EMM_UMB_Map	struc
  EUM_Page0	dd	?	; physical 4K page number
  EUM_Page1	dd	?	; physical 4K page number
  EUM_Page2	dd	?	; physical 4K page number
  EUM_Page3	dd	?	; physical 4K page number
EMM_UMB_Map	ends
;==============================================================================
;==  EMM_Handle structure
;==============================================================================
EMM_Handle	struc
  HandleNum   	db	?	; Handle Number
  HandleFlags	db	?	; Flags: This handle has a name
  				;        This handle has a saved context
  HandleName 	db 8 dup (?)	; Handle Name
  NumLogPages   dw	?	; Number of 16K EMS logical pages
  PhysPtrToPTEs dd	?	; Physical pointer to PTE entries ..
EMM_Handle	ends		; describing Handle space (PTE struc)
;==============================================================================
;==  MEM_FREE_Map structure
;==============================================================================
MEM_FREE_Map	struc
  MEMstart	dd	?	; starting page number of region
  MEMcount	dd	?	; count of contiguous free pages
MEM_FREE_Map	ends
;==============================================================================
;==  XMS_Handle structure
;==============================================================================
XMS_Handle	struc
  XMShandle	dw	?	; handle number
  XMSflags	dw	?	; flags for this handle
  XMSsize	dd	?	; size of handle in KBs
  XMSaddr	dd	?	; lock address (physical)
XMS_Handle	ends
;==============================================================================
;==  UMB_FREE_Map structure
;==============================================================================
UMB_FREE_Map	struc
  UMBseg	dw	?	; starting page number of region
  UMBsize	dw	?	; count of contiguous free pages
UMB_FREE_Map	ends

EMMGICntxtNum	equ	64	; 64 16K contexts in first megabyte
;==============================================================================
;==  EMM Global Import Data Structure
;==============================================================================
EMM_Global_Import_Data	struc

  EMMGI_Flags		dw	0	; flags
  EMMGI_Len		dw	0	; length of structure
  EMMGI_Vers		db	0,0	; version of data structure
  EMMGI_OSKey		dd	0	; OS key (0=none)

  EMMGI_Cntxt		db (EMMGICntxtNum*size EMMContext) dup (0)

  EMMGI_CntxtSz		db 	0	; Int 67h AX=4E03h return value

  EMMGI_UMBcnt		db	0	; UMB array size in entries

  EMMGI_HndlCnt		db	0	; handle array size in entries
;
; End of Version 1.00 EMM Global Import Data Structure
;
  EMMGI_Int67		dd	0	; back door to Int 67h hook address

  EMMGI_HMA		dd	0	; pointer to 16 PTEs providing HMA

  EMMGI_FreeMEMcnt	db	0	; free memory page array size in entries

  EMMGI_XMScnt		db	0	; EMB handle array count

  EMMGI_FreeUMBcnt	db	0	; free UMB array size in entries

  EMMGI_Vendor		db	40 dup (0)

EMM_Global_Import_Data	ends
;==============================================================================
;==  EMM Global Import Data Structure: Start of variable length fields
;==============================================================================
EMMGI_UMBs 	equ EMMGI_UMBcnt+1    	; start of UMB array
EMMGI_Hndls	equ EMMGI_HndlCnt+1	; start of handle array
EMMGI_FreeMEM	equ EMMGI_FreeMEMcnt+1	; start of free EMS page array
EMMGI_XMS	equ EMMGI_XMScnt+1	; start of EMB handle array
EMMGI_FreeUMB	equ EMMGI_FreeUMBcnt+1	; start of free UMB array

EMMGIDS100size	equ EMMGI_Hndls  	; size of version 1.00 data structure
EMMGIDS110size	equ size EMM_Global_Import_DATA ; size of version 1.10 data structure
;QEMS EMMGIDS110size	equ EMMGI_FreeUMB	; size of version 1.10 data structure
.list	; end WinEMM.inc
