PAGE 58,132
;******************************************************************************
TITLE LoadHi -- UMB related support routines for main VXD.
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1986-1991
;
;   Title:	UMB.ASM -
;
;   Author:	AC
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;
;==============================================================================
;
;   DESCRIPTION:
;
;******************************************************************************

	.386p


;******************************************************************************
;			      I N C L U D E S
;******************************************************************************



	.XLIST
	INCLUDE VMM.Inc
	.LIST

;******************************************************************************
;				 E Q U A T E S
;******************************************************************************

DEV_ARENA_TYPE	equ	44h		;arena type associated with devices
CHAR_DEV	equ	8000h		;character device

;******************************************************************************
;			     S T R U C T U R E S
;******************************************************************************

		;----------------------------------------------;
		; structure of a device header		       ;
		;----------------------------------------------;

SYSDEV	STRUC
SDEVNEXT	DD	?	;Pointer to next device header
SDEVATT 	DW	?	;Attributes of the device
SDEVSTRAT	DW	?	;Strategy entry point
SDEVINT 	DW	?	;Interrupt entry point
SDEVNAME	DB	8 DUP (?) ;Name of device (only first byte used for block)
SYSDEV	ENDS

		;----------------------------------------------;
		; structure of an arena header.		       ;
		;----------------------------------------------;

ArenaHeader STRUC
ArenaType	DB	?	;type of arena
ArenaOwner	DW	?	;owner of the arena
ArenaSize	DW	?	;size of arena in para
ArenaUnused	DB	3 DUP (?) ;unused
ArenaName	DB	8 DUP (?) ;name of owner
ArenaHeader ENDS

;******************************************************************************
;			   F L A G   E Q U A T E S
;******************************************************************************

;******************************************************************************
;			    L O C A L	D A T A
;******************************************************************************

VxD_DATA_SEG

LimPageMapPtr		dd	? 	;pointer to LIMs log-phys mapping
LimStartUMBPage 	dd	? 	;starting UMB page number
DeviceHeaderStartPtr	dd	?	;start of the device header list
LimInstanceDataPtr	dd	?	;pointer to instance data list

VxD_DATA_ENDS


VxD_CODE_SEG


VxD_CODE_ENDS


;******************************************************************************
;		    I N I T I A L I Z A T I O N   C O D E
;******************************************************************************

VxD_REAL_INIT_SEG

;******************************************************************************
;
;   Real_Mode_Hook
;
;   DESCRIPTION:
;
;      This routine is called from the Real_Mode_Init routine of the VXD and
;      lets the limulator part of the VXD do it's own real mode initialization
;      stuff.
;
;   ENTRY:
;
;	ALL REGISTERS SETUP AS FOR A REAL, REAL_MODE_INIT call.
;
;   EXIT:
;
;      Setup BX,SI registers appropriatesly. Thats is, if a default hook which
;      does noting should zero out BX & SI (no pages to exclude and no instance
;      data items) and AX should have the default return code .
;
;   USES:
;	AX, BX, DX, SI, Flags
;
;==============================================================================
BeginProc Real_Mode_Hook,PUBLIC

	xor	bx, bx				; No pages to exclude
	xor	si, si				; No instance data
	mov	ax, Device_Load_Ok		; OK to load device
	ret

EndProc Real_Mode_Hook
;----------------------------------------------------------------------------;
VxD_REAL_INIT_ENDS

VxD_ICODE_SEG

		PUBLIC Get_UMB_Info

;----------------------------------------------------------------------------;
; Get_UMB_Info:								     ;
; 	       								     ;
; This routine knows about the interface between the LoadHi VXD and the      ;
; Limulator that load it.  The code here assumes the following interface:    ;
;									     ;
; Entry:								     ;
;	 EDX - 32 bit linear address to the following structure:	     ;
;									     ;
;	       assume that the first UMB page is physical page number I      ;
;	       and the last UMB page is physical page number J. In between   ;
;	       there probably will be pages that are not UMB pages.	     ;
;       								     ;
;	       DWORD    -     Start of Dos device header chain (linAddr)     ;
;	       DWORD	-     I (first page).				     ;
;	       DWORD	-     page which is mapped into page I		     ;
;	       DWORD	-     page which is mapped into (I+1):0 if not UMB pg;
;	       DWORD	-     page which is mapped into (I+2):0 if not UMB pg;
;		 ---- similar DWORD for pages (I+3) through (J-1) ----       ;
;	       DWORD	-     page which is mapped into page J		     ;
;	       DWORD	-     pointer to additional instance data.	     ;
;								             ;
; Exit:									     ;
;	 EAX  - First UMB page number. (I)				     ;
;	 EDX  - # of consecutive pages that may have UMBs in them. (J-I+1)   ;
;									     ;
; Uses:  EAX,EDX and Flags.						     ;
;----------------------------------------------------------------------------;
BeginProc Get_UMB_Info

; get and save the linear address of the start of the device header chain

	mov	eax,[edx]
	mov	DeviceHeaderStartPtr,eax
	add	edx,4

; get the pointer to additional instance data table

	mov	eax,[edx]
	mov	LimInstanceDataPtr,eax
	add	edx,4

; EDX has the linear address of the array of logical and physical pages. 
; Save the start UMB page number and the array.

	mov	eax,[edx]		;get the start UMB page number
	mov	LimStartUMBPage,eax	;save it
	add	edx,4			;next is the actual map table
	mov	[LimPageMapPtr],edx	;save it for later routines

; get the number of consecutive pages that may have UMBs in them.

	mov	edx, 0ffh + 1 		;1 past the last V86 page num
	sub	edx,eax
	ret

EndProc Get_UMB_Info
;----------------------------------------------------------------------------;
; Get_Device_Info:						 	     ;
;									     ;
;   DESCRIPTION:							     ;
;	Tries to get the starting address and size of a device which is      ;
;	loaded high (interested in CONFIG.SYS devices only).		     ;
;									     ;
;   ENTRY:								     ;
;	ESI -> 8 character device name string				     ;
;	    THIS STRING MUST EXACTLY MATCH THE NAME AS IT WILL BE FOUND	     ;
;	    IN THE DEVICE HEADER IN LOW MEMORY!!!!!!!			     ;
;	    No case conversion is performed (name in dev hdr is upper case)  ;
;	    Length is 8 (name must be space padded as DOS device names are)  ;
;	    NO ':'s!! There is no ':' in the name in the device header.	     ;
;									     ;
;   EXIT:								     ;
;	Carry clear if the device is loaded high and was loaded from         ;
;	config.sys. (This implementaion assumes that devices loaded from     ;
;	config.sys will have the special DOS4/DOS5 arena associated with     ;
;	them). In this case:						     ;
;	   								     ;
;	       EDX - Start of the device part that has to be instanced.	     ;
;	       EAX - Length of the portion in bytes.			     ;
;								             ;
;	Carry set if:							     ;
;	       o Device cannot be found in the device header chain	     ;
;	       o Device not loaded high					     ;
;	       o Device is not a character device.		             ;
;	       o Device cannot be sized properly			     ;
;									     ;
;   USES:								     ;
;	FLAGS,EAX,EDX							     ;
;									     ;
;----------------------------------------------------------------------------;

BeginProc Get_Device_Info

; Search the device header list for a match for this device name 
	
	push	edi
	push	ecx
	push	ebx
	mov	edi,DeviceHeaderStartPtr;pointer to the first dev.
	mov	edx,edi			;get the first pointer
	shr	edx,4			;make up atritrary segment value
	shl	edx,16			;get the segment in hiword
	mov	ebx,edi			;get the linear address
	and	ebx,0fh			;get the balance in a para
	add	edx,ebx			;edx has seg:off of current guy
	xor	ebx,ebx			;previous segment

ChkDev:

	test	[edi.SDEVATT], CHAR_DEV	;deal with character devices only
	jz	short NextDev		;Skip non character devices
	push	esi
	push	edi			;save pointers
	lea	edi,[edi.SDEVNAME]	;start of this device's name
	mov	ecx,8
	cld
	repe	cmpsb			;check to see if it matches
	pop	edi
	pop	esi			;restore pointers
	jz	short FoundDev		;correct device

NextDev:

	mov	ebx,edx			;save segment:off of 'prev' guy
	mov	edi,[edi.SDEVNEXT]	;pointer to next device
	mov	edx,edi 		;EDX is SEG:OFF of device
	cmp	di, 0FFFFh		;End of Device list?
	je	DevExit			;Y: Done with device instance data
	mov	ecx,edi
	and	ecx,0000FFFFh		;ecx is offset of dev
	shr	edi,16
	mov	eax,edi 		;eax is Segment of device for BIOS test
	shl	edi,4
	add	edi,ecx 		;edi is lin addr of device
	jmp	short ChkDev		;continue searching

FoundDev:

; EDI has a pointer to the device, check to see if this is past the UMB line.

	mov	eax,LimStartUMBPage	;get the start of the UMB page
	shl	eax,12			;convert to linear address
	cmp	edi,eax			;is it above the UMB line ?
	jb	DevExit		       	;no, not interested

; check to see if the device has a valid arena header

	sub	edi,16			;point to the arena
	cmp	[edi.ArenaType],DEV_ARENA_TYPE
	jnz	SHORT DevMayBeCombined 	;may be a combined case
	movzx	eax,[edi.ArenaOwner]	;get the owner
	dec	eax			;back one up
	shl	eax,4			;convert to linear address
	cmp	eax,edi			;correct owner ?
	jnz	SHORT DevMayBeCombined	;may be a combined case

; this device has a valid arena header. However, if the prev device has 
; the same segment start value as this one, then this device is the 
; first in a combined list.

	push	ebx			;save last dev's seg:off
	push	edx			;save cur dev's seg:off
	shr	ebx,16			;last dev's seg
	shr	edx,16			;cur dev's seg
	cmp	ebx,edx			;are they same
	pop	edx
	pop	ebx			;restore
	jz	SHORT DevNotLastInMany	;first device of a combined list

; return with a pointer to the start of the device in EDX and a size in 
; EAX and carry clear.

	movzx	eax,[edi.ArenaSize]	;get size in paragraphs
	shl	eax,4			;convert to size in bytes
	lea	edx,[edi][16]		;start of the device
	jmp	SHORT DevSuccess	;did it right.

DevNotLastInMany:

; EDI points to a device arena which is the either the first or one of the 
; middle ones in a group of devices that have been loaded together.
; EDX has the seg:off of this device and EBX has the seg:off
; of the previous device. Their difference should give the size.

; however, this may not be a case of devices loaded together at all. We will
; not try to size the device if the offset of the previous device is less 
; than the offset of the current device. (we have established that the
; segments are the same so we can simply compare EDX and BX)

	cmp	edx,ebx			;is current in higher mem address ?
	ja	SHORT DevExit 		;yes, cannot size this device.

	add	edi,16			;point to the start of the device
	xchg	edx,edi			;EDX has the start of the device
	mov	eax,ebx			;get last's SEG:OFF
	sub	eax,edi			;difference of offs as segs are same
	jmp	SHORT DevSuccess	;did it right.

DevMayBeCombined:

; This device does not have an associated arena header. It may be either the
; middle one or the last one amongst a group of devices loaded together.
; EDX has the seg:off of the current device and EBX has the seg:off of the
; previous one. If the segments match, then this is a middle one and the
; difference of EBX and EDX is the size.

	push	ebx			;save last dev's seg:off
	push	edx			;save cur dev's seg:off
	shr	ebx,16			;last dev's seg
	shr	edx,16			;cur dev's seg
	cmp	ebx,edx			;are they same
	pop	edx
	pop	ebx			;restore
	jz	SHORT DevNotLastInMany	;middle device in a group of many

; this device could be the last one in the chain. We have to scan forward
; and see when we get to a case where a device has the same segment as well
; as a device header.

	add	edi,16			;back to the device.

DevScanFwd:

	mov	eax,[edi.SDEVNEXT]	;pointer to next device
	shr	eax,16			;get the segment
	push	edx			;save our dev's seg:off
	shr	edx,16			;get it's segment
	cmp	eax,edx			;still the same ?
	pop	edx			;restore
	jnz	SHORT DevIsItTheFirst  	;did we get the first one.
	mov	edi,[edi.SDEVNEXT]	;pointer to next device
	mov	ecx,edi
	and	ecx,0000FFFFh		;ecx is offset of dev
	shr	edi,16
	mov	eax,edi 		;eax is Segment of device for BIOS test
	shl	edi,4
	add	edi,ecx 		;edi is lin addr of device
	jmp	SHORT DevScanFwd	;scan forward

DevIsItTheFirst:

; EDI should now be pointing to the first device in the group of devices.
; check to see if the device has a valid arena header

	sub	edi,16			;point to the arena
	cmp	[edi.ArenaType],DEV_ARENA_TYPE
	jnz	SHORT DevExit		;don't know about this
	movzx	eax,[edi.ArenaOwner]	;get the owner
	dec	eax			;back one up
	shl	eax,4			;convert to linear address
	cmp	eax,edi			;correct owner ?
	jnz	SHORT DevExit		;don't know about this.
	
; EDI points to the arena. Get the size of the combined device.

	movzx	eax,[edi.ArenaSize]	;get size in paragraphs
	shl	eax,4			;convert to size in bytes
	movzx	ebx,dx			;get the offset of our device
	sub	eax,ebx			;size of our device.
	shr	edx,16			;get the segment
	shl	edx,4			;convert to linear address
	add	edx,ebx			;add in the offset

DevSuccess:

	clc				;indicate success
	jmp	SHORT DevRet		;return

DevExit:

	stc				;failure

DevRet:
	
	pop	ebx			
	pop	ecx
	pop	edi
	ret


	

EndProc Get_Device_Info
;----------------------------------------------------------------------------;
; Get_Ptr_To_Instance_Data_List:					     ;
;									     ;
;   DESCRIPTION:							     ;
;	Returns a pointer to the list of instance data address and size that ;
;       was copied out of the Windows/386 3.00 initialization data structure.;
;	                                                                     ;
;   ENTRY:								     ;
;	  None.							             ;
;	                                                                     ;
;   EXIT:								     ;
;	EDI - 32 bit pointer to an array where each entry is of 6 bytes      ;
;             (except last). The first DWORD is a seg:off address of the     ;
;	      start of a data area that is to be instanced and the last      ;
;             WORD is the size of the area in bytes.                         ;
;									     ;
;             The last entry in the array must be a DWORD of 0.		     ;
;									     ;
;   USES:								     ;
;	FLAGS,EDX							     ;
;----------------------------------------------------------------------------;
BeginProc Get_Ptr_To_Instance_Data_List

	mov	edi,LimInstanceDataPtr		;obtained at start up
	ret

EndProc Get_Ptr_To_Instance_Data_List
;----------------------------------------------------------------------------;
VxD_ICODE_ENDS

;----------------------------------------------------------------------------;

VxD_CODE_SEG

		PUBLIC	Get_Mapped_Page_Num	

;----------------------------------------------------------------------------;
; Control_Call_Hook:							     ;
;									     ;
; This procedure should handle all the control calls that the LIMULATOR wants;
; to do additional processing on. 					     ;
;								             ;
; This routine will be called before the VXD does it's own processing and the;
; VXD would do a PUSHAD/POPAD accross the call.			             ;	   
;								 	     ;
; Return CARRY SET if the call has to be failed. Not all calls can be failed.;
; While you may fail calls like Sys_Critical_Init and Create_VM, you should  ;
; not fail calls like Device_Init which would leave the VXD in an inconsistent
; state.
;									     ;
; The code provided below is just a stub and returns without hooking any of  ;
; calls.							             ;
;----------------------------------------------------------------------------;

BeginProc Control_Call_Hook,PUBLIC

; EXAMPLE of how this call should be handled:
;
;	Control_Dispatch Sys_Critical_Init, Lim_Sys_Critical_Init
;	Control_Dispatch Device_Init, Lim_Device_Init
;			   .
;			   .
;			   .
;       (where Lim_Sys_Critical_Init, Lim_Device_Init are local procedures)
;
;

	clc				;don't fail the call.
	ret

EndProc Control_Call_Hook
;----------------------------------------------------------------------------;
; Get_Mapped_Page_Num:							     ;
;									     ;
; Entry:								     ;
;	  EAX - Page number in the range of pages that have UMBs.	     ;
;									     ;
; Exit:									     ;
;	  EAX - Entry time EAX.						     ;
;	  EDX - Page that is mapped into the page num in EAX (0 if no page   ;
;		is mapped in there.					     ;
;									     ;
; Uses:   EDX								     ;
;----------------------------------------------------------------------------;
BeginProc Get_Mapped_Page_Num						

	mov	edx,eax			;get the desired page slot #
	sub	edx,LimStartUMBPage	;bias it relative to the first page
	shl	edx,2			;each entry is a dword
	add	edx,LimPageMapPtr	;point to the slot for the page in EAX
	mov	edx,[edx]		;get the mapped page number
	ret

EndProc Get_Mapped_Page_Num
;----------------------------------------------------------------------------;
VxD_CODE_ENDS


	END

