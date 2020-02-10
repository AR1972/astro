.386p
page 58,132
;******************************************************************************
	title	ELIMFUNC - CEMM functions module
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;	Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;	Module: ELIMFUNC - entry point for VDM functions
;
;	Version: 2.00
;
;	Date:	May 24,1986
;
;	Author: Steve Preston
;
;******************************************************************************
;
;	Change Log:
;
;	DATE	 REVISION	Description
;	-------- --------	--------------------------------------------
;	04/24/86 Original	From EMML LIM driver.
;	06/28/86 0.02		Name change from CEMM386 to CEMM (SBP).
;	07/05/86 0.04		Added segment R_CODE (SBP).
;	07/10/86 0.05		jmp $+2 before "POPF" (SBP).
;	05/12/87 2.00		moved code to R_CODE (SBP).
;	05/21/87 2.00		added debug functions (SBP).
;	05/31/88 ?.??		set active flag AFTER GoVirtual (SBP).
;
;******************************************************************************
;   Functional Description:
;	This module contains the ON/OFF functionality code for activating/
;   deactivating CEMM from DOS.
;
;******************************************************************************
.lfcond
	page
;******************************************************************************
;			P U B L I C   D E C L A R A T I O N S
;******************************************************************************
;
	public	b2asc10
	public	b2asc10_far
	public	b2ascHEX
	public	b2ascHEX_far
	public	ELIM_Entry
	public	EFunTab
	public	EFUN_CNT

ifdef	QHKN
	public	Debug_PhysIO
	public	E_Debug
endif

	public	E_GetStatus
	public	E_ONOFF
	public	E_Weitek
	public	I_Message_Display
	public	E_XStatus_Display

;
	page
;******************************************************************************
;			L O C A L   C O N S T A N T S
;******************************************************************************
;
	include vdmseg.inc
	include vdmsel.inc
	include emm386.inc
	include page.inc
	include	emmfunct.inc
	include emmdata.inc
	include winemm.inc

;******************************************************************************
;			E X T E R N A L    R E F E R E N C E S
;******************************************************************************
;
R_CODE	SEGMENT
	EXTRN	ttl_hndls: WORD
	extrn	GoVirtual:near
;;	extrn	check_XMM:far
;	extrn	XMMAllocateHMA:far
;	extrn	XMMDeallocateHMA:far
	extrn	XMMAllocHMAFar:dword
	extrn	XMMDeallHMAFar:dword
	extrn	checkXMMFar:dword
	extrn	Devname:byte
R_CODE	ENDS


LAST	SEGMENT
	extrn	StatusMess:byte
	extrn	LIMVerMess:byte
	extrn	TotalPagesMess:byte
	extrn	AvailPagesMess:byte
	extrn	TotalHanMess:byte
	extrn	AvailHanMess:byte
	extrn	PFBAMess:byte
	extrn	MemSizeMess:byte
	extrn	ISizeMess:byte
	extrn	InitMessage:byte
	extrn	NoEMSmess:byte
	extrn	UMBstatusMess:byte
	extrn	UMBavail:byte
	extrn	UMBlargest:byte
	extrn	UMBstart:byte
	extrn	AvailUMB:near
	extrn	LargestUMB:near
LAST	ENDS


	page
;******************************************************************************
;			S E G M E N T	D E F I N I T I O N
;******************************************************************************
;
;******************************************************************************
;
;	Code Segment R_CODE
;
;******************************************************************************
;
R_CODE	segment
	assume	CS:R_CODE, DS:R_CODE, ES:R_CODE
;
;	ELIM functions table	- far calls
;
EFunTab label	dword
	dw	offset	E_GetStatus
	dw	seg	R_CODE

	dw	offset	E_ONOFF
	dw	seg	R_CODE

	dw	offset	E_Weitek
	dw	seg	R_CODE

ifdef	QHKN
	dw	offset	E_Debug
	dw	seg	R_CODE
endif

EFUN_CNT	equ	($-EFunTab)/4

	page
;******************************************************************************
;	ELIM_Entry - entry point for general ELIM functions
;
;	THIS IS A FAR CALL ROUTINE
;
;	ENTRY:	REAL or VIRTUAL mode only
;		AH = 0	=> get current status of VDM/EMM386
;		AH = 1	=> ON/OFF/AUTO
;		AH = 2	=> Weitek functions
;		AH = 3	=> DEBUG entry
;		AH = 4	=> Message Display
;		AH = 5	=> Status Display
;
;	EXIT: EMM386 is activated/deactivated if possible
;	      NC => no errors.
;	      CY => ERROR occured.
;			AH = error number
;			AH= 01 =>invalid function.
;
;	USED: none
;
;******************************************************************************
ELIM_Entry	proc	far
;

	push	ebx
	push	ds
;
	mov	bx,seg R_CODE
	mov	ds,bx
	assume	ds:R_CODE
;
	cmp	ah,EFUN_CNT	;Q: valid function #
	jae	SHORT EE_inv_func ;  N: return error
	xor	bx,bx		;  Y: exec function
	mov	bl,ah		; bx = function #
	shl	bx,2		; dword index
	call	CS:EFunTab[bx]	; call the function
;
EE_exit:
	pop	ds
	pop	ebx
	ret
;
EE_inv_func:
	mov	ah,01
	stc
	jmp	short EE_exit
;
ELIM_Entry	endp

;******************************************************************************
;	E_GetStatus - get ELIM/VDM status
;
;	ENTRY:	AH = 0
;		DS = R_CODE
;
;	EXIT:	AH = 0 => ELIM ON
;		   = 1 => ELIM OFF
;		   = 2 => ELIM in AUTO mode (ON)
;		   = 3 => ELIM in AUTO mode (OFF)
;
;	USED: none
;
;******************************************************************************
E_GetStatus	proc	far
;
	xor	ah,ah			; assume ON
	test	[Current_State],fState_Active
					;Q: CEMM currently ON?
	jnz	SHORT EGS_chkAUTO	;  Y: chk for AUTO mode
	mov	ah,1			;  N: set to OFF
EGS_chkAUTO:
	cmp	[Current_Mode],MODE_AUTO	;Q: Auto Mode?
	jne	SHORT EGS_exit			;  N: leave
	add	ah,2				;  Y: map to 2,3
EGS_exit:
	ret
;
E_GetStatus	endp

;******************************************************************************
;	E_ONOFF  - general ON/OFF code for ELIM
;
;	ENTRY:	AH = 1
;		 AL = 0 => ON
;		 AL = 1 => OFF
;		 AL = 2 => AUTO
;		DS = R_CODE
;
;	EXIT:	Virtual mode and ELIM ON
;		OR Real mode and ELIM OFF
;
;	USED: none
;
;******************************************************************************
E_ONOFF proc	far
;
	cmp	al,0			;Q: -> CEMM ON ?
	jne	SHORT EOO_chkOFF	;  N: check for OFF/AUTO

	mov	[Current_Mode],MODE_ON	; set mode to ON
	mov	[Devname],'E'		; set to allow presense detect
	jmp	SHORT EOO_ON		;     go to ON state

EOO_chkOFF:
	cmp	al,1			;Q: -> CEMM OFF ?
	jne	SHORT EOO_chkAUTO	;  N: check for AUTO
	CMP	[handle_count], 1	; Q:  entries in handle dir <= 1?
	JA	EOO_inv			; N: refuse the request
	cmp	[UsedVCPIPages],0	;Q: Any VCPI pages allocated
	jnz	EOO_inv			; Y: continue
ifdef QEMS
	MOV	AX, [free_pages]	; Get original free 4K pages	;@PIW
	CMP	AX, [free_4k_pages]	; Q: Is any page (de)allocated?	;@PIW
	jne	EOO_inv			; Y:
endif
	cmp	cs:[UMBHMA],TRUE	;Q: Are UMBs defined?
	je	EOO_inv			; Y: don't turn off
	mov	[Current_Mode],MODE_OFF ; N: set mode to OFF
	mov	[Devname],'$'		;set to prevent presense detect
	jmp	SHORT EOO_OFF

EOO_chkAUTO:
	cmp	al,2				;Q: -> CEMM AUTO ?
	jne	SHORT EOO_inv 			; N: invalid function call
	mov	[Current_Mode],MODE_AUTO	; Y: set mode to AUTO

	or	[Auto_State],fAuto_Users	; assume users: active

	cmp	[handle_count],1		;Q: EMS users active?
	jne	short EOOcont			; Y: continue
	cmp	cs:[UMBHMA],TRUE		;Q: Is virtual HMA/UMB provided?
	je	short EOOcont			; Y: continue
	cmp	[UsedVCPIPages],0		;Q: Any VCPI pages allocated
	jnz	EOOcont				; Y: continue	
ifdef QEMS
	mov	ax,[free_pages]			; Get original free 4K pages
	cmp	ax,[free_4k_pages]		;Q: Is any page allocated?
	jne	short EOOcont			; Y: continue
endif
	and	[Auto_State],not fAuto_Users	; N: CEMM may be turned inactive

EOOcont:
	mov	[Devname],'E'		;set to allow presense detect
	cmp	[Auto_State],0		;Q: turn on for AUTO mode ?
	je	SHORT EOO_OFF 		;  N: go to OFF state
					;  Y: go to ON state

	;
	; go to Virtual Mode operation and activate CEMM
	;
EOO_ON:
	test	[Current_State],fState_Active	;Q: currently active ?
	jnz	SHORT EOO_OK			;  Y: just leave
	call	GoVirtual			;  N: go to virtual mode & (sbp)
	jc	short EOOerror
	or	[Current_State],fState_Active	;     set active flag	   (sbp)
	jmp	short EOO_OK
	;
	; go to Real Mode operation and deactivate CEMM
	;
EOOerror:
	and	[Current_State],not fState_Active; make inactive
	mov	[Current_Mode],MODE_OFF 	 ; set mode to OFF
	mov	[Devname],'$'			 ; set to prevent presense detect

EOO_OFF:
	test	[Current_State],fState_Active	;Q: currently inactive ?
	jz	SHORT EOO_OK			;  Y: just leave
	and	[Current_State],NOT fState_Active ;N: reset active flag &
	push	ax				; and return to real mode
	pushf
	cli				;;; protect this sequence
	mov	al,0Fh			;;;
	out	84h,al			;;; port 84/85 return to real
	mov	al,00h			;;;	sequence ...
	out	85h,al			;;;
	jmp	$+2			;;; clear prefetch/avoid race cond
	popf
	pop	ax
	cmp	[Current_Mode],MODE_OFF
	jne	short EOO_OK
;
;   leave with no errors
;
EOO_OK:
	clc
EOO_exit:
	ret
;
;  invalid ON/OFF/AUTO function call
;
EOO_inv:
	stc
	ret
;
E_ONOFF endp

;******************************************************************************
;	E_Weitek  - Weitek support functions
;
;	ENTRY:	AH = 2
;		 AL = 0 => sense current Weitek state
;		 AL = 1 => turn Weitek ON
;		 AL = 2 => turn Weitek OFF
;		DS = R_CODE
;
;	EXIT:	STC and AH = 1 for invalid subfunction (AL)
;		for AL = 0 (sense Weitek state)
;		    AL -> bit 0 = Weitek installed flag
;			  bit 1 = Weitek mapped flag
;		for AL = 1 or 2
;		    STC and AH = 2 => Weitek not installed
;			CEMM ON or OFF depending on current
;			CEMM mode (ON/OFF/AUTO).
;
;	USED: none
;
;******************************************************************************
E_Weitek	proc	far
;
	push	bx
	cmp	al,0			;Q: sense status ?
	jne	SHORT EW_chkON		;  N: check next function code
	mov	al,[Weitek_State]	;  Y: return current state
	jmp	EW_OK

;
; Weitek ON function
;   When Weitek state is set to ON, CEMM goes active whenever it is in
;   AUTO mode and the page table mapping is set to point to the Weitek.
;
EW_chkON:
	cmp	al,1			;Q: Weitek ON function ?
	jne	SHORT EW_chkOFF		;    N: check for OFF function
	test	[Weitek_State],fWeitek_Inst ;Y: Q: Weitek installed ?
	jz	SHORT EWerror		    ;	  N: report error

	call	dword ptr [checkXMMFar]	;Q: Is there an XMM in the system
	jc	short EWok		; N: OK to take HMA for WEITEK

	test	[GenFlags],fHMA		;Q: Does CEMM own the HMA?
	jnz	short EWok		; Y: map WEITEK

	call	dword ptr [XMMAllocHMAFar]
				 	;Q: Is the HMA available
	jc	short EWerror		; N: report error

EWok:
	or	[Weitek_State],fWeitek_Map  ;	  Y: set Weitek ON
	or	[Auto_State],fAuto_Weitek   ;	     and auto mode flag
	;
	; update CEMM's state
	;   if CEMM is inactive, check for AUTO mode.
	;      if AUTO mode, CEMM goes active.
	;
	test	[Current_State],fState_Active	;Q: CEMM active ?
	jnz	SHORT EW_ONPg 			; Y: nothing to do here ...

	cmp	[Current_Mode],MODE_AUTO	;Q: CEMM in AUTO mode ?
	jne	SHORT EW_ONPg 			; N: CEMM stays inactive

	call	GoVirtual			;(sbp)	Y: go to virtual mode &
	jc	short EWerror
	or	[Current_State],fState_Active	;(sbp)	   set active flag
	;
	; now change 1meg wraparound to point to Weitek addresses
	;
EW_ONPg:
	mov	bx,0		; Weitek mapping ON
	jmp	SHORT EW_UpdPageMap
;
; HMA not available for WEITEK
EWerror:
	mov	ah,2
	stc
	jmp	SHORT EW_error

;
; Weitek OFF function
;   When Weitek state is set to OFF, CEMM active/inactive state is unaffected
;   by Weitek in AUTO mode.  The 1Meg wraparound is placed back into the
;   page tables.
;
EW_chkOFF:
	cmp	al,2			;Q: Weitek OFF function ?
	jne	SHORT EW_InvFunc	;    N: invalid Weitek subfunction
	test	[Weitek_State],fWeitek_Inst ;Y: Q: Weitek installed ?
	jz	short EWerror		;	  N: report error

	call	dword ptr [checkXMMFar]	;Q: Is there an XMM in the system
	jc	short EWOok		; N: OK to release HMA for WEITEK

	test	[GenFlags],fHMA		;Q: Does CEMM own the HMA?
	jz	short EWOok		; N: OK to release HMA for WEITEK

	call	dword ptr [XMMDeallHMAFar]
					;Q: Is the HMA released?
	jc	short EWerror		; N: report error

EWOok:
	and	[Weitek_State],NOT fWeitek_Map	; Y: set Weitek OFF
	and	[Auto_State],NOT fAuto_Weitek	;    and reset auto mode flag
	;
	; update CEMM's state
	;   if CEMM is in AUTO mode, it is active and CEMM may be able
	;    to go inactive now.
	;
	cmp	[Current_Mode],MODE_AUTO	;Q: CEMM in AUTO mode ?
	jne	SHORT EW_OFFPg			;  N: then no CEMM state change
	cmp	[Auto_State],0			;  Y: Q: CEMM stays active ?
	jne	SHORT EW_OFFPg			;	Y: then leave
	and	[Current_State],NOT fState_Active ;	N: reset active flag &
	pushf					; and return to real mode
	cli				;;; protect this sequence
	mov	al,0Fh			;;;
	out	84h,al			;;; port 84/85 return to real
	mov	al,00h			;;;	sequence ...
	out	85h,al			;;;
	jmp	$+2			;;; clear prefetch/avoid race cond
	popf
	;
	; now enable 1meg wraparound
	;
EW_OFFPg:
	mov	bx,1		; Weitek mapping OFF
;
;  Update page table mapping of 1meg linear wraparound area
;	Entry: BX = 0 => Weitek mapping ON
;		  = 1 => 1 meg wraparound (Weitek OFF)
;
EW_UpdPageMap:
	test	[Current_State],fState_Active	;Q: CEMM active?
	jz	SHORT EW_InactMap		;  N: change mapping
	pushf					;  Y: prot mode call to chg it
	cli				;;; protect this sequence
	Call_CEMM_PMF	CEMM_84_Weitek
	popf
	jmp	SHORT EW_OK

EW_InactMap:			; CEMM inactive, update Weitek mapping
EW_OK:
	clc
EW_exit:
	pop	bx
	ret

EW_InvFunc:
	mov	ah,1
	stc
	jmp	SHORT EW_error
EW_error:		; error exit
	stc
	jmp	EW_exit
;
E_Weitek	endp

;LEO ;******************************************************************************
;LEO ; WeitekPageMap - change tables to include/exclude Weitek in page tables
;LEO ;
;LEO ;	NOTE: THIS IS A FAR ROUTINE
;LEO ;
;LEO ;   ENTRY: PROTECTED or REAL mode ONLY
;LEO ;		ES -> page tables
;LEO ;		BX = 0 => Weitek mapping ON
;LEO ;		   = 1 => Weitek mapping OFF (1meg wraparound)
;LEO ;   EXIT: same as entry
;LEO ;   USED: none
;LEO ;******************************************************************************
;LEO WeitekPageMap	proc	far
;LEO 	pushad
;LEO 	cld
;LEO 	cmp	bx,0			;Q: weitek map on function ?
;LEO 	jne	SHORT WPM_chkoff	;  N: check for off function
;LEO 	;				;  Y: change page tables
;LEO 	;  For Weitek 1167 mapping addresses,
;LEO 	; change page table entries for (00100000h,0010EFFFh) ->
;LEO 	;				(C0000000h,C000EFFFh)
;LEO 	;
;LEO 	mov	eax,0C0000000h	; start with physical addr = C0000000h
;LEO 	jmp	SHORT WPM_set_PT	; set page tables
;LEO
;LEO WPM_chkoff:
;LEO 	cmp	bx,1			;Q: weitek map off function ?
;LEO 	jne	SHORT WPM_exit		;  N: leave
;LEO 	xor	eax,eax			;  Y: 1meg wraparound
;LEO
;LEO 	;
;LEO 	; set 60k worth of entries in page tables for 1meg linear
;LEO 	; entry: EAX = begin physical addr
;LEO 	;
;LEO WPM_set_PT:
;LEO 	mov	di,100h*4	; ES:DI -> PTE for 1Meg linear
;LEO 	or	al,P_AVAIL		; make pages available to all
;LEO 	mov	cx,0Fh			; set 60k worth of entries
;LEO WPM_set_entry:
;LEO 	stosd				; store EAX to ES:[DI]
;LEO 					; ES:[DI] pts to next page table entry
;LEO 	add	eax,1000h		; EAX = next physical page (with attrs)
;LEO 	loop	WPM_set_entry		;Q: done with page table entries ?
;LEO 					;  N: loop again
;LEO 					;  Y: all done
;LEO WPM_exit:
;LEO 	mov	eax,cr3		; clear the TLB !!!
;LEO 	mov	cr3,eax
;LEO 	popad
;LEO 	ret
;LEO WeitekPageMap	endp

ifdef	QHKN

	page
;******************************************************************************
;	Debug Functions table
;******************************************************************************
;
DebugTab	label	word
	dw	offset	DebugR_GetPTE
	dw	offset	DebugR_PhysIO
DEBTAB_CNT	equ	($-DebugTab)/2

;******************************************************************************
;	E_Debug  - Debug functions entry point
;
;	ENTRY:	REAL or VIRTUAL mode
;		AH = 3
;		 AL = 0 => Get physical address
;		 AL = 1 => Get physical DMA contents
;		DS = R_CODE
;
;	EXIT:	Same processor mode as entry
;		CLC => no errors
;		STC => error
;
;	USED: BX
;
;******************************************************************************
E_Debug proc	far
;
	cmp	al,DEBTAB_CNT	;Q: valid function #
	jae	SHORT ED_inv_func ;  N: return error
	xor	bx,bx		;  Y: exec function
	mov	bl,al		; bx = function #
	shl	bx,1		; word index
	call	CS:DebugTab[bx] ; call the function
;
ED_exit:
	ret
ED_inv_func:
	stc
	ret
;
E_Debug endp
;
	page
;******************************************************************************
;	DebugR_GetPTE  - Returns the Page Table Entry for a Linear Address
;
;	ENTRY:	REAL or VIRTUAL mode
;		AH = 3
;		AL = 0 => Get page table entry
;		ECX = linear address
;		DX = 0	=> don't map linear address first (for >1meg addrs)
;		DX = 1	=> map linear address first
;		DS = R_CODE
;
;	EXIT:	Same processor mode as entry
;		STC => error
;		CLC => no errors and
;			ECX = page table entry
;
;	USED: none
;
;******************************************************************************
DebugR_GetPTE	proc	near
	test	[Current_State], fState_Active	;Q: in Real mode now ?
	jnz	SHORT DRGP_VM 			;  N: go to Virtual Mode hndlng
						;  Y: ok ..
	push	eax
	push	di
	push	es
	mov	ebx,[PageD_Addr] 	; EBX = 32 bit addr of page dir
	shr	ebx,4			; BX = seg addr of page dir
	mov	es,bx			; ES -> page dir
	mov	eax,ecx			; EAX = linear addr
	PDOff	ax			; EAX = page dir offset
	mov	di,ax			; DI = page dir offset for this addr
	mov	ebx,ES:[di]		; EBX = PTE for page table
	test	bx,P_PRES		;Q: is this page table present ?
	jz	SHORT DRGP_RM_error	;  N: report error
	and	bx,(P_SIZE-1)		;  Y: remove status bits
	shr	ebx,4			; BX = seg addr of page table
	mov	es,bx			; ES -> page table
	mov	eax,ecx			; EAX = linear addr
	PTOff	ax			; EAX = page table offset
	mov	di,ax			; DI = page table offset for this addr
	mov	ecx,ES:[di]		; ECX = PTE for page table
	clc			; no errors

DRGP_RM_exit:
	pop	es
	pop	di
	pop	eax
	ret
DRGP_RM_error:
	stc
	jmp	DRGP_RM_exit
;
; In virtual mode now.	We need to go to protected mode to be sure we can
; access the page tables.
DRGP_VM:
	push	bx
	mov	bl,al			; BL = Diag function # for GetPTE
	Call_CEMM_PMF	CEMM_84_Diag	; exec diag function
	pop	bx
	ret
DebugR_GetPTE	endp

	page
;******************************************************************************
;	DebugR_PhysIO - do physical I/O (at ring 0 PM privilege level)
;
;	ENTRY:	REAL or VIRTUAL mode
;		AH = 3
;		AL = 1 => Do Physical I/O
;		    DX = I/O address
;		    ECX/CX/CL = for OUTs, the value to output
;		    SI = 0 for input
;		       = 1 for output
;		    DI = 0 for byte I/O
;		       = 1 for word I/O
;		       = 2 for dword I/O
;		DS = R_CODE
;
;	EXIT:	Same processor mode as entry
;		STC => error
;		CLC => no errors and
;		    ECX/CX/CL = value input
;
;	USED: none
;
;******************************************************************************
DebugR_PhysIO	proc	near
	test	[Current_State], fState_Active	;Q: in Real mode now ?
	jnz	SHORT DRPIO_VM			;  N: go to Virtual Mode hndlng
	call	FAR PTR Debug_PhysIO		;  Y: do I/O in real mode
	ret					;     and return
;
; here if virtual mode	=> call CEMM Protected Mode Function
;
DRPIO_VM:
	push	bx
	mov	bl,al			; BL = Diag function # for PhysIO
	Call_CEMM_PMF	CEMM_84_Diag	; exec diag function
	pop	bx
	ret
DebugR_PhysIO	endp

	page
;******************************************************************************
;	Debug_PhysIO - do physical I/O
;
;	ENTRY:	REAL MODE or Ring 0 PROT MODE
;		    DX = I/O address
;		    ECX/CX/CL = for OUTs, the value to output
;		    SI =  0 for input
;		       <> 0 for output
;		    DI = 0 for byte I/O
;		       = 1 for word I/O
;		       = 2 for dword I/O
;		DS = R_CODE
;
;	EXIT:	Same processor mode as entry
;		    ECX/CX/CL = value input
;		CLC => no error
;		STC => invalid input
;
;	USED: none
;
;******************************************************************************
Debug_PhysIO	proc	far
	push	eax
	or	si,si		;Q: Input ?
	jnz	SHORT DPIO_Out	;  N: do output
				;  Y: do input
	cmp	di,0		;Q: byte input ?
	jne	SHORT DPIO_InW	;  N: chk for word
	in	al,dx		;  Y: do input
	mov	cl,al		;     and set value into CL
	jmp	SHORT DPIO_OK 	;      and leave
DPIO_InW:
	cmp	di,1		;Q: word input ?
	jne	SHORT DPIO_InDW	;  N: chk for dword
	in	ax,dx		;  Y: do input
	mov	cx,ax		;     and set value into CL
	jmp	SHORT DPIO_OK 	;      and leave
DPIO_InDW:
	cmp	di,2		;Q: dword input ?
	jne	SHORT DPIO_error ;  N: bad xfer size
	in	eax,dx		;  Y: do input
	mov	ecx,eax		;     and set value into CL
	jmp	SHORT DPIO_OK 	;      and leave

DPIO_Out:
	cmp	di,0		;Q: byte output ?
	jne	SHORT DPIO_OutW	;  N: chk for word
	mov	al,cl		;  Y: get byte for out
	out	dx,al		;     do output
	jmp	SHORT DPIO_OK 	;      and leave

DPIO_OutW:
	cmp	di,0		;Q: word output ?
	jne	DPIO_OutW	;  N: chk for dword
	mov	ax,cx		;  Y: get word for out
	out	dx,ax		;     do output
	jmp	SHORT DPIO_OK 	;      and leave

DPIO_OutDW:
	cmp	di,2		;Q: dword output ?
	jne	SHORT DPIO_error;  N: bad xfer size
	mov	eax,ecx		;  Y: get dword for out
	out	dx,eax		;     do output
				;      and leave
DPIO_OK:
	clc
DPIO_exit:
	pop	eax
	ret

DPIO_error:
	stc
	jmp	DPIO_exit
Debug_PhysIO	endp

endif

R_CODE	ENDS

LAST	segment
	assume	CS:LAST, DS:LAST, ES:R_CODE


;******************************************************************************
;	I_Message_Display - display banner
;
;	ENTRY:	NONE
;
;	EXIT:	none
;
;	USED: none
;
;******************************************************************************
I_Message_Display	PROC	near

	push	ds
	mov	dx, cs
	mov	ds, dx
	mov	dx,offset LAST:InitMessage
	mov	ah,PRINT_STRING
	int	MS_DOS			; output init message
	pop	ds
	RET
I_Message_Display	ENDP

;******************************************************************************
;	E_XStatus_Display - display ELIM/VDM status
;
;	ENTRY:	DS = R_CODE
;
;	EXIT:	none
;
;	USED: none
;
;******************************************************************************
E_XStatus_Display	proc	near
;

	push	ds
	push	es
	push	di

	push	ds
	pop	es	; es = R_CODE

	mov	ax, cs	; si = LAST
	mov	ds, ax	; ds = LAST

	mov	dx,offset LAST:NoEMSmess	; assume EMS N/A

;;	cmp	cs:[NoEMSset],TRUE	;Q: Is EMS available?
;;	je	short EXSprint		; N: print EMS N/A message

	cmp	es:[VCPIset], -1 	; Q: has noems been specifed
	jne	EXSprint		; Y: print EMS N/A message

EXSDcont:
ifdef QEMS
	mov	ax,es:[free_4k_pages]
	shl	ax,2			; amount in K
endif
	mov	ax,es:[TopOfHandleSpace]	; top of free EMS handle space
	sub	ax,FIRST_HANDLE_PTE	; subtract start of handle space
	sub	ax,es:[UsedVCPIPages]	; number of free EMS pages
	shr	ax,2			; convert to 16K pages
	sub	ax,es:[UsedEMSPages]	; number of free EMS pages
	shl	ax,4			; number in KBs
	mov	di,offset LAST:ISizeMess; store decimal size in ASCII here.
	mov	cx,5
	call	b2asc10			; convert to ASCII...(R_CODE)

	mov	dx,offset LAST:MemSizeMess
	mov	ah,PRINT_STRING
	int	MS_DOS			; output message

	mov	al,EMM_VERSION
	xor	ah,ah
	ror	ax,4
	shr	ah,4
	or	ax,'00'
	mov	di,offset LAST:LIMVerMess
;;	stosb
	mov	byte ptr ds:[di], al
	inc	di

	inc	di
	xchg	al,ah
;;	stosb
	mov	byte ptr ds:[di], al
	inc	di

	mov	ax,es:[TopOfHandleSpace]; top of free EMS handle space
	sub	ax,FIRST_HANDLE_PTE	; subtract start of handle space
	shr	ax,2
;QEMS	mov	ax,es:[total_4k_pages]
;QEMS	shr	ax,1
;QEMS	shr	ax,1
	mov	cx,4
	mov	di,offset LAST:TotalPagesMess
	call	b2asc10

	mov	ax,es:[TopOfHandleSpace]; top of free EMS handle space
	sub	ax,FIRST_HANDLE_PTE	; subtract start of handle space
	sub	ax,es:[UsedVCPIPages]	; number of free EMS pages
	shr	ax,2			; convert to 16K pages
	sub	ax,es:[UsedEMSPages]	; number of free EMS pages
;QEMS	mov	ax,es:[free_4k_pages]
;QEMS	shr	ax,1
;QEMS	shr	ax,1
	mov	cx,4
	mov	di,offset LAST:AvailPagesMess
	call	b2asc10
	mov	ax,es:[ttl_hndls]
	mov	cx,4
	mov	di,offset LAST:TotalHanMess
	call	b2asc10

	mov	ax,es:[handle_count]
	mov	cx,4
	mov	di,offset LAST:AvailHanMess
	call	b2asc10

	CMP	es:[PF_Base], FREE	; Q: Is there a page frame?	;@PIW
	JE	SHORT get_status_mess	; N: Skip this part		;@PIW
	mov	ax,es:[PF_Base]		; page frame base addr
	mov	di,offset LAST:PFBAMess	; here for page frame base addr
	call	b2ascHEX
	; Append " H" to hex value
        MOV     BYTE PTR [DI], ' '      ; Ensure blank for localized ver (IPG)
        MOV	BYTE PTR [DI+1], 'H'	; Put H at end of the segment	;@PIW

get_status_mess:
	mov	dx,offset LAST:StatusMess
EXSprint:
	mov	ah,PRINT_STRING
	int	MS_DOS			; output message

ifndef	MSFLAG
	call	dword ptr es:[checkXMMFar]  
					;Q: Is there an XMM in the system
	jc	short EXSexit		; N: exit
endif

	cmp	es:[UMBptr],0		;Q: Any UMBs?
	je	short EXSexit		; N: exit

	call	AvailUMB
	shr	ax,6
	mov	di,offset LAST:UMBavail; store decimal size in ASCII here.
	mov	cx,4
	call	b2asc10			; convert to ASCII...(R_CODE)

	call	LargestUMB
	shr	ax,6
	mov	di,offset LAST:UMBlargest; store decimal size in ASCII here.
	mov	cx,4
	call	b2asc10			; convert to ASCII...(R_CODE)

	mov	ax,es:[UMBptr]		 ; first UMB address
	mov	di,offset LAST:UMBstart
	call	b2ascHEX
	mov	byte ptr [di+1],'H'	; Put H at end of the segment

	mov	dx,offset LAST:UMBStatusMess
	mov	ah,PRINT_STRING
	int	MS_DOS			; output message

EXSexit:
	pop	di
	pop	es
	pop	ds
	ret
;
E_XStatus_Display	endp

;******************************************************************************
;
;	b2asc10 - Converts binary to ascii decimal and stores at ES:DI.
;		  Stores CX ascii chars (decimal # is right justified and
;		  filled on left with blanks).
;
;	entry:	ax = binary number
;		cx = digits to display
;		es:di = place to store ascii chars.
;
;	exit:	ASCII decimal representation of number stored at DS:DI
;
;	used:	none
;
;	stack:
;
;******************************************************************************
;
power10		dw	1,10,100,1000,10000
;
b2asc10_far	proc	far
	call	b2asc10
	ret
b2asc10_far	endp
;
b2asc10 proc	near
;
	push	ax
	push	bx
	push	cx
	push	dx
	push	si
;
	mov	si,cx			; index to base 10 table
	dec	si
	shl	si,1			;word index
	xor	bl,bl			; leading zeroes flag
	cld
;
;   convert binary number to decimal ascii
;
b2_loop:
	xor	dx,dx			; clear word extension
	mov	cx,cs:power10[si]
	div	cx			; divide by power of 10
	or	bl,bl
	jnz	short b2_ascii
;
	or	ax,ax			; q: zero result?
	jnz	short b2_ascii		;  n: go convert to ascii
;
	mov	al,' '                  ;  y: go blank fill
	jmp	short b2_make_strg		;
;
b2_ascii:
	add	al,'0'                  ; put into ascii format
	mov	bl,1			; leading zeroes on
;
b2_make_strg:
;;	stosb				; put ascii number into string
	mov	byte ptr ds:[di], al
	inc	di

	mov	ax,dx
	sub	si,2			; decrement power of 10 pointer
	jg	short b2_loop 		; Q: Last digit?  N: Jump if not
	mov	al,dl
	and	al,0Fh
	add	al,'0'
;;	stosb
	mov	byte ptr ds:[di], al
	inc	di

;
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret				; *** return ***
;
b2asc10 endp

;******************************************************************************
;
;	b2ascHEX- converts binary to ascii HEX and store at ES:DI
;		  stores 4 ascii chars (HEX # is right justified and
;		  filled on left with blanks)
;
;	entry:	ax = binary number
;		es:di = place to store ascii chars.
;
;	exit:	ASCII HEX representation of number stored at DS:DI
;
;	used:	none
;
;	stack:
;
;******************************************************************************
;
hextab		db	"0123456789ABCDEF"
;
b2ascHEX_far	proc	far
	call	b2ascHEX
	ret
b2ascHEX_far	endp
;
b2ascHEX proc	near
;
	push	ax
	push	bx
	push	cx
	push	dx

	cld				;forward,increment
	mov	dx,ax
	mov	cx,12
b2hex_loop:
	mov	bx,dx
	shr	bx,cl			; get highest nibble
	and	bx,0Fh			; nibble only
	mov	al,CS:hextab[bx]	; get ASCII for it
;;	stosb				; store ASCII
	mov	byte ptr ds:[di], al
	inc	di

	sub	cx,4
	jge	short b2hex_loop

	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
;
b2ascHEX endp

LAST	ends
	end

