.386p
;******************************************************************************
	title	UTIL - general CEMM utilities
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:    EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;   Module:   UTIL - utilities
;
;   Version:  2.0
;
;   Date:     June 11, 1986
;
;   Author:	Steve Preston
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   06/11/86  Original	from i286.asm
;   06/18/86  0.01	in GoVirtual - added code to init VDM state
;			variables (SBP).
;   06/25/86  0.02	in GoVirtual - more DiagByte state variable.
;   06/28/86  0.02	Name change from CEMM386 to CEMM (SBP).
;   06/29/86  0.02	Changed check code for ROM write protect state (SBP).
;   07/01/86  0.03	Added call to InitDMA in GoVirtual (SBP).
;   07/05/86  0.04	Moved code to InitLOCK (SBP).
;   07/05/86  0.04	Added FarGoVirtual and moved IsReal to PRINT.ASM (SBP).
;   07/06/86  0.04	Changed assume to _DATA and moved stack
;			out of _DATA(SBP) .
;   05/08/87  2.00	Moved GoVirtual to R_CODE (SBP).
;   04/27/88  3.30	Change old Int11h save to be init'ed to zero (RDV).
;   05/27/88  3.30	Change CHKA20 to not access 8042 (RDV).
;   07/26/88  3.31	Better CHKA20 (RDV).
;   09/09/88  3.31	Fix better CHKA20 (RDV).
;   08/09/89  4.10	EnterVirtual: add CEMM/Windows initialization (LC).
;
;   02/13/91  M008	Do not make int 16s on a Compaq Deskpro
;			386/16 or a Compaq portable 386.
;
;
;******************************************************************************
;
;   Functional Description:
;
;
;******************************************************************************
.lfcond 				; list false conditionals

	public	EnterVirtual
	public	ExitVirtual
	public	UpdateHMA
;;	public	Int11_Hook
;;	public	Int10_Hook
	public	GoVirtual
	public	FarGoVirtual
	public	ChkA20
	public	SelToSeg
	public	Log2Phy
	public	Log2Lin
	public	Lin2Phy
;;	public	chk_machine_state

ifdef MSFLAG
	public	UpdateHMAfar
	public	GetA20cnt
	public	SetA20cnt
endif

;******************************************************************************
;	D E F I N E S
;******************************************************************************
	include VDMseg.inc
	include VDMsel.inc
	include desc.inc
	include oemdep.inc
	include emm386.inc
	include emmfunct.inc
	include emmdata.inc
	include page.inc
	include winemm.inc
	include	xmm.inc

;******************************************************************************
;	E X T E R N A L   R E F E R E N C E S
;******************************************************************************

_TEXT	SEGMENT
	extrn	InitDMA:near		; (dmatrap.asm)
	extrn	Kybd_Watch:near 	; (a20trap.asm)
	extrn	InitReset:near
ifdef PICtrap
	extrn	EnterVirtPIC:near	; (pictrap.asm)
	extrn	ExitVirtPIC:near	; (pictrap.asm)
endif
_TEXT	ENDS

R_CODE	SEGMENT
	extrn	GoProtMode:near 	; (modesw.asm)
	extrn	GoVirtualMode:near	; (modesw.asm)
	extrn	Cache_Off:near		; (cemm386.asm)
ifdef MSFLAG
	extrn	XMMcontrol:dword
	extrn	EnableCount:word	; (xms.asm)
	extrn	DisableA20:near		; (modesw.asm)
	extrn	EnableA20:near		; (modesw.asm)
endif
R_CODE	ENDS

R1_CODE	segment
	extrn	PrevInt10:dword
	extrn	PrevInt11:dword
R1_CODE	ends

	page
;******************************************************************************
;		S E G M E N T	D E F I N I T I O N S
;******************************************************************************

;******************************************************************************
;		_TEXT segment
;******************************************************************************

_TEXT SEGMENT
	assume cs:_TEXT, ds:_DATA, es:_DATA

;******************************************************************************
;**	EnterVirtual - Initialization for entering virtual mode operation
;
;	    **** THIS IS A FAR ROUTINE ****
;
;	ENTRY	PROTECTED MODE
;
;	EXIT	PROTECTED MODE
;
;	USES	flags
;
;******************************************************************************
EnterVirtual	proc	far
	push	ax
	push	ds
	push	gs
	push	fs

	mov	ax,VDMD_GSEL
	mov	ds,ax			; DS -> _DATA
	mov	ax,RCODEA_GSEL
	mov	gs,ax
	mov	ax,DATA32_GSEL
	mov	fs,ax
;
;   init VDM state variables
;
ifdef TSSQLEO
	push	es
	mov	ax,TSSD_GSEL
	mov	es,ax		; ES -> TSS
endif
	call	Kybd_Watch	; init a20 line watch

;TSSQLEO pop	es


;
; reset Return to real variables
;
	mov	[RR_Last],0
	mov	[RR85save],0FFh
	mov	[RR84save],0FFh

;
; initialize DMA virtualization logic
;
	call	InitDMA 		; init DMA watcher

ifdef PICtrap
;
; initialize PIC vector mapping
;
	call	EnterVirtPIC 		; virtualize PIC
endif

;
; initialize EGA mapping logic
;
	call	EnterEGA

;
; initialize HMA page table entries according to state of A20/Weitek
;
	call	UpdateHMA
;
; initialize the reset vector at F000:FFF0 to point to ResetRoutine
;
	call	InitReset

	pop	fs
	pop	gs
	pop	ds
	pop	ax
	ret
EnterVirtual	endp

;******************************************************************************
;**	EnterEGA - EGA related initialization for entering Virtual Mode
;
;	ENTRY	PROTECTED MODE
;		R_CODE:[Current_State]
;		R_CODE:[PrevInt10] = vector for Int 10 chains
;
;	EXIT	PROTECTED MODE
;		R_CODE:[EGA_Seg] = saved EGA_Segment (E000h)
;		R_CODE:[PrevInt10+2] = segment -> C000h
;		Int 1fh and Int43h vectors -> C000h segment
;		(if they -> E000h before)
;
;	USES	flags
;
;******************************************************************************
EnterEGA	proc	near

	push	ax
	push	ds
	push	fs
	mov	ax,RCODEA_GSEL
	mov	ds,ax
	ASSUME	DS:R_CODE

	mov	ax,R1CODEA_GSEL
	mov	fs,ax
	ASSUME	FS:R1_CODE

	test	[Current_State],fState_CEGAmove ;Q: do EGA move ?
	jz	SHORT EEGA_exit			;  N: leave
	mov	word ptr fs:[PrevInt10+2],0C000h;  Y: seg to 0C000h
	push	es
	mov	ax,RM_IDT_GSEL
	mov	es,ax			; ES -> 0
	ASSUME	ES:ABS0
	cmp	es:[int5+2],0E000h	;Q: Int 5 (print screen) -> EGA ROM ?
	jne	short EEGA_chk1F	;  N: chk 1Fh vector
	mov	es:[int5+2],0C000h	;  Y: point it to C000h
EEGA_chk1F:
	cmp	ES:[int1F+2],0E000h	;Q: int 1f -> EGA ROM ?
	jne	SHORT EEGA_chk43	;  N: chk 43h vector
	mov	ES:[int1F+2],0C000h	;  Y: point it to C000h
EEGA_chk43:
	cmp	ES:[int43+2],0E000h	;Q: int 43  -> EGA ROM ?
	jne	SHORT EEGA_chk6d	;  N: chk 6dh vector
	mov	ES:[int43+2],0C000h	;  Y point it to C000h
EEGA_chk6d:
	cmp	ES:[int6d+2],0E000h	;Q: int 6d  -> EGA ROM ?
	jne	SHORT EEGA_vecdone	;  N: vextors patched
	mov	ES:[int6d+2],0C000h	;  Y point it to C000h
EEGA_vecdone:
	pop	es
	ASSUME	ES:_DATA
EEGA_exit:
	pop	fs
	assume	fs:nothing
	pop	ds
	ASSUME	DS:_DATA
	pop	ax
	ret
EnterEGA	endp

ifdef 900801
;******************************************************************************
;**	EnterWeitek - Weitek related initialization for entering Virtual Mode
;
;	Checks the current state of the Weitek mapping flag and updates
;	the page tables entries for the 1meg wraparound area to the proper
;	physical pages.
;
;	ENTRY	PROTECTED MODE
;		DS -> _DATA
;
;	EXIT	PROTECTED MODE
;
;	USES	flags
;
;******************************************************************************
EnterWeitek	proc	near
	push	bx
	push	es

	push	RCODEA_GSEL
	pop	es		;ES -> R_CODE
	ASSUME	ES:R_CODE

	xor	bx,bx				; BX = 0 => Weitek mapping
	test	ES:[Weitek_State],fWeitek_Map	;Q: is Weitek enabled ?
	jnz	SHORT EW_setPT			;  Y: set up Weitek mapping
	inc	bx				;  N: BX =1 => 1meg wrap
W_setPT:
	push	PAGET_GSEL
	pop	es				; ES -> page tables
	PCall	RCODE_GSEL,R_CODE:WeitekPageMap ; go map it
	pop	es
	pop	bx
	ret
	ASSUME	ES:_DATA

EnterWeitek	endp

endif

;==============================================================================
;==
;==  Log2Phy: Translates the logical address ES:EAX to a physical address.
;==
;==  Entry: (Protected Mode)
;==	ES:EAX	= logical address
;==
;==  Exit:
;==	EAX	= 32-bit physical address
;==
;==============================================================================
Log2Phy proc	near

	call	Log2Lin
	call	Lin2Phy

	ret
Log2Phy endp

;==============================================================================
;==
;==  Log2Lin: Translates the logical address ES:EAX to a linear address.
;==
;==  Entry: (Protected Mode)
;==	ES:EAX	= logical address
;==
;==  Exit:
;==	EAX	= 32-bit linear address
;==
;==============================================================================
Log2Lin proc	near
	push	ebx
	push	si
	push	fs

	mov	si,GDTD_GSEL		; access GDT via FS
	mov	fs,si
;
;  Get index into GDT
;
	mov	si,es
	and	si,SELECTOR_MASK
;
;  Get base address of selector
;
	mov	ebx,fs:[si][2]		; get low 24 bits
	rol	ebx,8
	mov	bl,fs:[si][7]		; upper 8 bits
	ror	ebx,8
;
;  Add base to offset to get linear address
;
	add	eax, ebx 		; linear address in eax

	pop	fs
	pop	si
	pop	ebx
	ret
Log2Lin	endp

;==============================================================================
;==
;==  Lin2Phy: Translates the linear address EAX to a physical address.
;==
;==  Entry: (Protected Mode)
;==	EAX	= 32-bit linear address
;==
;==  Exit:
;==	EAX	= 32-bit physical address
;==
;==============================================================================
Lin2Phy proc	near
	push	ebx
	push	fs

	mov	bx,PAGET_GSEL		; access page tables via FS
	mov	fs,bx

	push	eax			; save linear address
	shr	eax,12			; convert linear address tp PTE index
;
;  Get base physical address
;
	mov	ebx,fs:[eax*4]		; get base physical address
	and	ebx,PTE_ADDRESS_BIT_MASK; clear PTE control bits

	pop	eax			; restore linear address
	and	eax,00000FFFh 		; keep offset into physical page
;
;  Calculate physical address
;
	add	eax,ebx			; eax has physical address

	pop	fs
	pop	ebx
	ret
Lin2Phy	endp

;===============================================================================
;==
;==  UpdateHMA : Update page tables with current state of HMA
;==
;==  Entry:	(Protected Mode)
;==	RCODEA_GSEL:[Current_State](fState_A20Ena) = Current A20 state.
;==	RCODEA_GSEL:[Weitek_State](fWeitek_Map) = Current Weitek map state.
;==
;==
;==  Exit:	(Protected Mode)
;==	same (page tables reflect A20/Weitek state)
;==
;===============================================================================
ifdef MSFLAG
UpdateHMAfar	proc	far
	call	UpdateHMA
	ret
UpdateHMAfar	endp
endif

UpdateHMA proc	near
	push	eax
	push	ecx
	push	edi
	push	es
	push	ds
	cld

	mov	ax,RCODEA_GSEL		; RCODE alias
	mov	ds,ax			;
	mov	ax,PAGET_GSEL  		; PAGET segment
	mov	es,ax
	assume	ds:R_CODE

	mov	eax,0C0000000h		; assume WEITEK map on
	test	[Weitek_State],fWeitek_Map ;Q: WEITEK on?
	jnz	short UHMAcont		   ; Y: WEITEK map

	xor	eax,eax			; assume A20 disabled
	test	[Current_State],fState_A20Ena ;Q: Enable A20?
	jz	short UHMAcont		      ; N: wrap with page tables
	mov	eax,[HMAptr]		      ; Y: no wrap (functional HMA)
;QLEO	mov	eax,100000h		      ; Y: no wrap (start at 1Meg)
UHMAcont:
	mov	edi,100h		; ES:DI*4 -> PTE for 1Meg linear
	mov	cx,10h			; 64k worth of entries
UHMAloop:
	and	dword ptr es:[edi*4],0FFFh ; do not disturb the user bits
	or	es:[edi*4],eax
	inc	di
	add	eax,1000h		; EAX = next physical page (with attrs)
	dec	cx			; dec/jnz faster than loop!!!
	jnz	short UHMAloop		; loop until done with PTEs?

	mov	eax,cr3			; clear the TLB !!!
	mov	cr3,eax

	pop	ds
	pop	es
	pop	edi
	pop	ecx
	pop	eax
	assume ds:_DATA, es:_DATA
	ret

UpdateHMA	endp

;******************************************************************************
;**	ExitVirtual - clean up when leaving virtual mode operation
;
;	    **** THIS IS A FAR ROUTINE ****
;
;	ENTRY	PROTECTED MODE
;		DS -> _DATA
;
;	EXIT	PROTECTED MODE
;
;	USES	flags
;
;******************************************************************************
ExitVirtual	proc	far

;
;  DMA/bus master services are not needed in real mode
;
	push ds

	push DATA32_GSEL
	pop ds
	assume ds:ABS0

	and [DBSflag],not fDBSactive   ; deactivate DMA/bus master interface
	pop ds

ifdef PICtrap
	call	ExitVirtPIC	; remap PIC vectors to real mode value
endif

	call	ExitEGA
	ret
ExitVirtual	endp

;******************************************************************************
;**	ExitEGA - EGA related initialization for entering Virtual Mode
;
;	ENTRY	PROTECTED MODE
;		R_CODE:[PrevInt10+2] -> C000h
;		R_CODE:[EGA_Seg] = saved EGA segment (E000h)
;		Int 1f and Int 43h vectors' segment don't point to E000h
;
;	EXIT	PROTECTED MODE
;		R_CODE:[PrevInt10+2] -> R_CODE:[EGA_Seg]
;		Int 1f and Int43h vector's segments -> E000h if they
;		pointed to C000h.
;
;	USES	flags
;
;******************************************************************************
ExitEGA proc	near

	push	ax
	push	ds
	push	fs

	mov	ax,RCODEA_GSEL
	mov	ds,ax
	ASSUME	DS:R_CODE

	mov	ax,R1CODEA_GSEL
	mov	fs,ax
	ASSUME	FS:R1_CODE


	test	[Current_State],fState_CEGAmove	;Q: EGA moved ?
	jz	SHORT XEGA_exit			; N: leave

	test	[Current_State],fState_WinCntrl	;Q: Window Interface?
	jnz	short XEGA_exit			; Y: don't use E000!

	mov	word ptr fs:[PrevInt10+2],0E000h;  Y: seg back to 0E000h
	push	es
	mov	ax,RM_IDT_GSEL
	mov	es,ax		; ES -> 0
	ASSUME	ES:ABS0
	cmp	es:[int5+2],0C000h	;Q: Int 5 (print screen) -> EGA ROM ?
	jne	short XEGA_chk1F	;  N: chk 1Fh vector
	mov	es:[int5+2],0E000h	;  Y: point it to E000h
XEGA_chk1F:
	cmp	ES:[int1F+2],0C000h	;Q: int 1f moved ?
	jne	SHORT XEGA_chk43	;  N: chk 43h vector
	mov	ES:[int1F+2],0E000h	;  Y: point it to E000h
XEGA_chk43:
	cmp	ES:[int43+2],0C000h	;Q: int 43 moved ?
	jne	SHORT XEGA_chk6d	;  N: chk 6dh vector
	mov	ES:[int43+2],0E000h	;  Y point it to E000h
XEGA_chk6d:
	cmp	ES:[int6d+2],0C000h	;Q: int 6d moved ?
	jne	SHORT XEGA_vecdone	;  N: vectors patched
	mov	ES:[int6d+2],0E000h	;  Y point it to E000h
XEGA_vecdone:
	pop	es
	ASSUME	ES:_DATA
XEGA_exit:
	pop	fs
	assume	fs:nothing
	pop	ds
	ASSUME	DS:_DATA
	pop	ax
	ret
	ret
ExitEGA endp

_TEXT	ends

	page
R_CODE	SEGMENT
	assume cs:R_CODE, ds:R_CODE, es:R_CODE

;******************************************************************************
;		R_CODE CODE
;******************************************************************************

;******************************************************************************
;	FarGoVirtual - far link for GoVirtual
;
;	NOTE: this is a FAR routine.
;
;    ENTRY:	Real Mode
;
;    EXIT:	Virtual Mode
;		VDM state variables initialized
;
;    USED:	none
;
;******************************************************************************
FarGoVirtual	proc	far
	call	GoVirtual
	ret
FarGoVirtual	endp

;******************************************************************************
;	GoVirtual - go to virtual mode
;
;    ENTRY:	Real Mode
;
;    EXIT:	Virtual Mode
;		VDM state variables initialized
;
;    USED:	none
;
;******************************************************************************
GoVirtual	proc	near
	pusha
	push	ds
	push	es
	push	fs
	push	gs
	pushf

;
;  chk on current state of A20
;
	call	ChkA20

;
; Go to protected mode
;
	cli				;;; no ints now
	call	GoProtMode
	jc	short GVerror

;
; initialize VDM variables
;
	PCall	VDMC_GSEL,_TEXT:EnterVirtual
;
; Go to Virtual Mode
;
	call	GoVirtualMode
;
; now in virtual mode, return
;
	popf
	clc

GVexit:
	pop	gs
	pop	fs
	pop	es
	pop	ds
	popa
	ret

GVerror:
	popf
	stc
	jmp	short GVexit

GoVirtual	endp

;******************************************************************************
;***	ChkA20 - check current state of Enable A20
;
;	This routine checks the current state of the A20 line by reading
;	the Output Port of the 8042 and testing the state of the A20 enable
;	bit.  NOTE: This routine uses the COMPAQ specific 8042 Special Read
;	command.
;	ENTRY	Real Mode
;	EXIT	R_CODE:[Current_State]	- fState_A20Ena = 0 if A20 disabled
;							= 1 if A20 enabled
;	USES	none
;******************************************************************************
ChkA20	proc	near

	push	ax
	push	bx
	push	ds
	push	es
	push	di
	push	si

	call	Cache_Off		 ; if cache, turn it off (status in bx)

;
;  Check if HMA is wrapped into the IVT area
;
	or	cs:[Current_State],fState_A20Ena ; assume A20 is enabled
ifdef MSFLAG
	push	bx			; save regs destroyed by XMM call

	mov	ah, XMM_QUERY_A20
	push	seg R_CODE
	pop	ds
	call	[XMMcontrol]

	pop	bx			; restore regs
	or	ax, ax			; Q: is A20 enabled
	jnz	short CA20_exit		; Y: A20 is enabled
					; N: A20 is disabled
	and	cs:[Current_State],not fState_A20Ena
else
	xor	ax,ax			; DS=0h and ES=FFFFh
	mov	ds,ax
	not	ax
	mov	es,ax
	xor	si,si			; compare 0:0 and FFFF:10

	pushf
	cli

	mov	ax,es:[si+10h]		; get HMA location
	cmp	ax,ds:[si]		;Q: Same as IVT location?
	jne	short CA20cont		; N: A20 is enabled

	inc	ax
	mov	es:[si+10h],ax		; change the HMA location
	dec	ax

	cmp	ax,ds:[si]		;Q: Did the IVT location change?
	mov	es:[si+10h],ax		; - restore HMA location
	je	short CA20cont			    ; N: A20 is enabled
	and	cs:[Current_State],not fState_A20Ena; Y: A20 is disabled

CA20cont:
	popf
endif

CA20_exit:

	mov	ax,0F000h		 ;look at ROM mem struct
	mov	es,ax			 ;ROM seg

	cmp	word ptr es:[0FFE9h],'C3';Q: ROM ID Compaq 386?
	jne	short nocache 		 ; N: can't have cache then

	test	cs:[GenFlags], fCPQ16	 ; M008: Q: is this a compaq deskpro 
					 ; M008: 386/16
	jnz	short nocache		 ; M008: Y: no cache

	cmp	bx,0E201h		 ; Q: was cache enabled upon entry?
	jne	SHORT nocache 		 ;   N:
	mov	ax,0F401h		 ;   Y: restore by enabling
	int	16h			 ;

nocache:
	pop	si
	pop	di
	pop	es
	pop	ds
	pop	bx
	pop	ax
	ret
ChkA20	endp

ifdef MSFLAG

;---------------------------------------------------------------------------
;
; Procedure Name : GetA20cnt
;
; Determines the enable count in himem by disabling a20 util it is disabled
;
; ENTRY : NONE
; EXIT  : EnableCount has the actual himem enable count
;	  If we are unable to turn a20 OFF then EnableCount=0
;
; USES  : NONE
;
;------------------------------------------------------------------------

GetA20cnt	proc NEAR

	test	cs:[Current_State], fState_WinCntrl
					; Q: is windows making a virtual
					;    enable call
	jnz	GA_done			; Y: enable count is same as what it
					;    was when windows turned us OFF

	push	cx
	push	bx
	push	ax

    ;
    ; loop locally disabling A20, until it is actually disabled  (to count
    ;	enables)
    ;
	xor	cx, cx
	xor	bx,bx
GA_dis:
	call	DisableA20
	jc	A20_disabled	    ; function failed
	inc	cx
	xor	bx,bx		    ; Assume bomb out (zero "ons" count)
	cmp	cx,1000h	    ; Reached RIDICULOUS value?
	ja	short A20_disabled  ; YES, give the hell up.....
	mov	ah, XMM_QUERY_A20
	call	cs:[XMMcontrol]
	or	ax, ax		    ; Q: is A20 really disabled
	jnz	short GA_dis	    ; N: disable again
				    ; Y: A20 is disabled we have enable count

; redo the enables
	push	cx		    ; Save ons count
GA_en:
	call	EnableA20
	loop	GA_en
	pop	bx
A20_disabled:
	mov	cs:[EnableCount], bx ; store current enable count

	pop	ax
	pop	bx
	pop	cx

GA_done:
	ret

GetA20cnt	endp

;---------------------------------------------------------------------------
;
; Procedure Name	: SetA20cnt
;
; Set's up HIMEM's enablec count to EMM386's enable count. This is called
; when EMM386 is turning OFF
;
; ENTRY: NONE
; EXIT : Himem's enable count = Emm386's
;	 If we are unable to turn A20 Off then Himem's enable count != Emm's
;
; USED : NONE
;
;
;---------------------------------------------------------------------------

SetA20cnt	proc	near

	test	cs:[Current_State], fState_WinCntrl
					; Q: is windows making a virtual
					;    disable call
	jnz	SA_fin			; Y: windows has already obtained
					;    enable count from us

	push	ax
	push	cx
	push	bx

	xor	cx,cx

SA_dis:
	call	DisableA20
	inc	cx
	cmp	cx, 01000h	    ; guard against possibility of A20 line
				    ; not getting disabled at all
	ja	SA_done
	mov	ah, XMM_QUERY_A20
	call	cs:[XMMcontrol]
	or	ax, ax		    ; Q: is A20 really disabled
	jnz	short SA_dis	    ; N: disable again
				    ; Y: A20 is disabled (HIMEM's enablecount
				    ;    is 0

	cmp	cs:[EnableCount], 0 ; Q: is our enable count 0
	je	SA_done		    ; Y: done. we're coherent with Himem
				    ; N: make Himem's enable count = ours

	mov	cx, cs:[EnableCount]
SA_en:
	call	EnableA20
	loop	SA_en

SA_done:
	pop	bx
	pop	cx
	pop	ax

SA_fin:
	ret

SetA20cnt	endp

endif

;******************************************************************************
;**	SelToSeg - convert selector to a segment number
;
;	The protected mode selector value is converted to a
;	real mode segment number.
;
;	ENTRY	PROTECTED MODE
;		BX = selector
;
;	EXIT	AX = segment number
;
;	USES	BX, Flags, other regs preserved
;
;******************************************************************************
SelToSeg proc near
	push	ds
	mov	ax,GDTD_GSEL		; selector is in the GDT
	mov	ds,ax
	and	bl,0F8h

	mov	ax,word ptr ds:[bx + 2] ; low 16 bits of base address
	mov	bh,ds:[bx + 4]		; high 8 bits of base address
	shr	ax,4
	shl	bh,4
	mov	bl,0
	add	ax,bx			; AX = segment number for selector
	pop	ds
	ret
SelToSeg endp


R_CODE	ENDS

	end
