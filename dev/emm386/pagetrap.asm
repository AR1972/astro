.386p
	page 58,132
;=============================================================================
	title	P A G E T R A P - traps page faults and writes into shadow ROMs
;=============================================================================
;==
;== (C) Copyright MICROSOFT Corp. 1990-1991
;== (C) Copyright COMPAQ Computer Corp. 1990-1991
;==
;==	Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;==
;==	Module: PageTrap  - Traps page faults and writes into shadow ROMs
;==
;==	Version: 1.00
;==
;==	Date:	September 30,1990
;==
;==	Author: Leo Cohen (original page fault handler: Patrick Wu & Leo Cohen)
;==
;=============================================================================
;==
;==	Change Log:
;==
;==	DATE	 TAG 		Description
;==	-------- --------	--------------------------------------------
;==     09/30/90 0.00	        Original
;==
;==	01/09/91 M000		clear int flag at page fault time and restore
;==				it in trapwrites. Also clear the Trap flag on
;==				on the stack in trapwrites before restoring 
;==				it from PFWflags.
;==
;=============================================================================
;==
;==   Functional Description:
;==
;==   This module monitors page faults:
;==
;==   1) If a page fault occurs due to a missing page, a page table is
;==	 created dynamically.  This emulates the ROMs moveblock which has
;==	 no boundary checking.
;==
;==   2) If a page fault occurs due to a write protection violation:
;==	 a) The PTE is modified inorder to allow the write to occur.
;==	 b) The data which will be modified by the write is saved.
;==	 c) The trap flag is set inorder to get control after the write
;==	    instruction.
;==	 d) After getting control again, the data is restored.
;==	 e) The PTE is write-protected again.
;==
;==   3) Unknown page fault - shutdown.
;==
;=============================================================================
;==	P U B L I C   D E C L A R A T I O N S
;=============================================================================
	public	PageFaultHandler
	public	TrapWrites
	public	CheckPageProt
	public	SaveResetVector
ifdef	ROMCOMP
	public	FixIVTptrs
	public	FixROMptrs
endif
	public	InitReset
	public	ResetRoutine
	public	ShutDown
	public	RestoreIVT
	public	NoResetRoutine
;=============================================================================
;==	L O C A L   C O N S T A N T S
;=============================================================================
	include vdmseg.inc
	include vdmsel.inc
	include dma.inc
	include page.inc
	include vm386.inc
	include	emm386.inc
	include	emmdata.inc
	include desc.inc

;=============================================================================
;==	E X T E R N A L   R E F E R E N C E S
;=============================================================================
_TEXT	segment
	extrn	ExceptHandler1:near
_TEXT	ends
LAST	segment

ifdef	ROMCOMP
	extrn	UnProtectROM:near
	extrn	ProtectROM:near
	extrn	ROMstart:word
endif

	extrn	ROMSet:word
LAST	ends
;=============================================================================
;==	L O C A	L   D A T A
;=============================================================================
_DATA	segment

PFTindex	dw	0		; index of last referenced PFT
PFTaddr		dw	1000h		; PFT0 offset from page directory
		dw	2000h		; PFT1 offset from page directory
PFTuser		dw	0		; owner of page fault table (PFT) 0
   		dw	0		; owner of page fault table (PFT) 1

PFWdata		dd	0		; data to restore on a write
PFWaddr		dd	0		; address to restore on a write
PFWPTE		dd	0		; write protected PTE address
PFWflags	dw	0		; save user flags

IDT1Save	dd	0,0		; saved interrupt descriptor for IDT 1
IDT1 label dword
IDT_ENTRY VDMC_GSEL,<offset _TEXT:ExceptHandler1>,D_386INT3 ; 01 Debug

pResetVec	dd	(0F000h shl 16)+0FFF0h

NumOfVectors	equ	80h
OldIVT		dd	NumOfVectors dup (0)

_DATA	ends

R_CODE	segment

OldResetVec	dd	TRUE

R_CODE	ends

ifdef	ROMCOMP
LAST	segment
ROMptrs	dw	09FA8h
	dw	0DB59h
	dw	0DB73h
	dw	0ED03h
	dw	0ED14h
	dw	0F8E9h
	dw	0FEF3h
	dw	0FEF3h
	dw	0FFE0h
	dw	0FFE2h
NumROMptrs	equ	($-ROMptrs)/2
LAST	ends
endif

;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
_TEXT	segment
	assume	cs:_TEXT
;==============================================================================
;==
;==  PageFaultHandler: This procedure handles all page faults
;==
;==  Entry: (Protected Mode)
;==	SS:[BP]	= pointer to exception IRETD image.
;==
;==  Exit:  (Protected Mode)
;==	NC	= no error
;==	CY	= error, exception is unexplained - shutdown the system.
;==
;==============================================================================
PageFaultHandler proc	near
	pushad
	push	ds
	push	es
	push	fs
;
;  Setup selectors
;
	mov	ax,VDMD_GSEL
	mov	ds,ax
	mov	bx,PAGED_GSEL
	mov	es,bx
	mov	cx,DATA32_GSEL
	mov	fs,cx
	mov	dx,RCODEA_GSEL
	mov	gs,dx
	assume	ds:_DATA,es:nothing,fs:nothing,gs:R_CODE
;
;  Get faulting linear address
;
	xor	ebx,ebx
	mov	eax,cr2			; get linear address
	mov	cr2,ebx			; clear CR2

	mov	ebx,eax
	shr	eax,22			; directory index
;
;  Check to see if table/page was present
;
	test	word ptr es:[eax*4],P_PRES ;Q: Is the table present?
	jnz	short PFHwrite		   ; Y: check for write fault

;------------------------------------------------------------------------
;
; We shall not check the error code on the stack as in some cases this
; may not be reliable. Note that we have already ensured that the page 
; directory and hence the page table entry associated with faulting page
; is not present. 
;
;	test	dword ptr [bp][VTFO],P_PRES ;Q: Error code: present page?
;	jnz	PFHerror		    ; Y: should never happen
;
;------------------------------------------------------------------------

;
;  Get a page fault table to dynamically set up
;
	movzx	ebx,[PFTindex]		; index to the user pointer
	movzx	edx,[PFTuser][ebx*2]
	movzx	edi,[PFTaddr][ebx*2]	; get page fault table address
	xor	[PFTindex],1		; set index of table to be used next
	or	dx,dx			;Q: Is the table free?
	jz	short PFHsetupTable	; Y: setup table
	mov	dword ptr es:[edx*4],0	; N: free the page table
;
;  Setup the page fault table
;
PFHsetupTable:
	mov	[PFTuser][ebx*2],ax	; mark table user
	mov	ebx,cr3			; address of page directory
	and	bx,0F000h
	add	ebx,edi			; address of page fault table
	or	bx,P_AVAIL
	mov	es:[eax*4],ebx		; set up page table in directory
;
;  Initialize tables with address equal to linear address
;
	shl	eax,22
	or	ax,P_AVAIL		; set page attribute
	mov	ecx,P_SIZE/4		; initialize all entries
	cld
PFHnextEntry:
	stosd
	add	eax,P_SIZE
	loop	PFHnextEntry
	clc				; no error
	jmp	short PFHexit
;
;  Check if fault was due to write protection
;
PFHwrite:

;------------------------------------------------------------------------
;
; We shall not check the error code on the stack as in some cases this
; may not be reliable. Note that we have already ensured that the page 
; directory and hence the page table entry associated with the faulting page
; is present. We shall just check the corresponding PTE to ensure that the
; fault was caused due to a write into a READ ONLY page.
;
;	test	dword ptr [bp][VTFO],P_PRES ;Q: Error code: present page?
;	jz	short PFHerror		    ; N: should never happen
;
;	test	dword ptr [bp][VTFO],P_WRITE ;Q: Check error code for write fault?
;	jz	short PFHerror		     ; N: cannot fix!
;
;--------------------------------------------------------------------------

;
;  Save value which will be destroyed
;
	mov	edx,fs:[ebx]		; get value being destroyed
	mov	[PFWdata],edx		; save it
	mov	[PFWaddr],ebx		; save address
;
;  Get and save PTE address
;
	mov	edx,es:[eax*4]		; get page table address
	and	dx,0F000h

	mov	eax,ebx
	shl	eax,10
	shr	eax,22
	lea	eax,fs:[edx][eax*4]
	mov	[PFWPTE],eax

	test	word ptr fs:[eax],P_WRITE  ;Q: Is the page writable?
	jnz	PFHerror		   ; Y: should never happen

;
;  Allow write to occur
;
	or	word ptr fs:[eax],P_WRITE

;  
;  Save user flags and set trap flag
; 
	mov	bx, [bp][VTFOE].VMTF_EFLAGS 
	mov	[PFWflags], bx
					; save user flags
	or	[bp][VTFOE].VMTF_EFLAGS,FLAGS_TF

	and	[bp][VTFOE].VMTF_EFLAGS, not FLAGS_IF
					; M000: clear IF so that an int 
					; M000: does not overwrite this page
	
;
;  Make sure Single Step exception belongs to correct handler
;
	mov	ebx,gs:[pIDT]		; get 32-bit address of IDT
	mov	edx,[IDT1]		; get correct IDT 1 descriptor
	xchg	edx,fs:[ebx][1*8]	; save current INT 1 descriptor
	mov	[IDT1save],edx
	mov	edx,[IDT1][4]		; get correct IDT 1 descriptor
	xchg	edx,fs:[ebx][1*8+4]	; save current INT 1 descriptor
	mov	[IDT1save][4],edx
	clc

PFHexit:
	mov	ebx,cr3			; clear TLB
	mov	cr3,ebx

	pop	fs
	pop	es
	pop	ds
	popad
	ret

PFHerror:
	stc				; error
	jmp	short PFHexit
	assume	ds:nothing
PageFaultHandler 	endp

;==============================================================================
;==
;==  TrapWrites: This procedure determines if the trap ocurred due to a write.
;==
;==  Entry: (Protected Mode)
;==	SS:[BP]	= pointer to exception IRETD image.
;==
;==  Exit:  (Protected Mode)
;==	NC	= continue
;==	CY	= trap due to write - return to client.
;==
;==============================================================================
TrapWrites proc	near
	push	eax
	push	ebx
	push	edx
	push	ds
	push	es
	push	fs
;
;  Setup selectors
;
	mov	ax,VDMD_GSEL
	mov	ds,ax
	mov	bx,DATA32_GSEL
	mov	fs,bx
	mov	ax,RCODEA_GSEL
	mov	gs,ax
	assume	ds:_DATA,es:nothing,fs:nothing,gs:R_CODE
;
;  Check for trap due to a write
;
	xor	eax,eax
	xchg	eax,[PFWPTE]
	or	eax,eax			;Q: Write?
	jz	short TWexit	        ; N: exit
;
;  Restore PTE, Trap Flag, IF, data, and Single Step Exception descriptor
;
	and	word ptr fs:[eax],not P_WRITE ; PTE is write protected again

					; M000: reset trap flag
	and	[bp][VTFO].VMTF_EFLAGS,not FLAGS_TF 

	mov	bx, [PFWflags]
	and	bx, FLAGS_TF+FLAGS_IF	; M000: restore trap flag and IF
	or	[bp][VTFO].VMTF_EFLAGS, bx
					; restore user flags

	mov	ebx,[PFWaddr]		; address of data which was written
	mov	eax,[PFWdata]		; get previous value
	mov	fs:[ebx],eax		; restore previous value

	mov	ebx,gs:[pIDT]		; get 32-bit address of IDT
	mov	edx,[IDT1save]		; get saved IDT 1 descriptor
	mov	fs:[ebx][1*8],edx	; restore INT 1 descriptor
	mov	edx,[IDT1save][4]	; get saved IDT 1 descriptor
	mov	fs:[ebx][1*8+4],edx	; restore INT 1 descriptor

	mov	eax, cr3		; reload cr3 to flush TLB
	mov	cr3, eax		

	test	[PFWflags], FLAGS_TF	; Q: was TF set at page fault time
	jnz	short TWexit		; Y: flag was set. Note that carry
					;    has been cleared by test inst.
					;    except1_handler will reflect 
					;    thru real mode IDT

	stc				; indicate return to client
TWexit:
	pop	fs
	pop	es
	pop	ds
	pop	edx
	pop	ebx
	pop	eax
	ret
TrapWrites	endp

;===============================================================================
;==
;==  CheckPageProt : Check if trying to write into a write protected page.
;==		     If starting page is write protected - skip it.
;==		     if a write protected page is encountered - adjust ECX to
;==		     stop EDI->EDI+ECX short of the page.
;==		     (It may be necessary in the future to skip write protected
;==		      pages, but continue with the write operation if a
;==		      non-write protected page exists in the buffer after it)
;==
;==  Entry: (Protected Mode)
;==	ES  = zero based - 4GB selector
;==	ESI = source buffer pointer
;==	EDI = destination buffer pointer (checked for write protection)
;==	ECX = size of buffer in bytes
;==
;==  Exit:  (Protected Mode)
;==	ECX = size of transfer (bytes) not affected by write protection
;==
;===============================================================================
CheckPageProt	proc	near
	push	edx
;
;  Check if buffer is below 1MB
;
	cmp	edi,100000h		;Q: Is buffer in first meg?
	jae	short CPPexit		; N: no more checking necessary

	lea	edx,es:[edi+ecx]	; end of buffer

	cmp	edx,80000h		;Q: Is buffer below 512K?
	jbe	short CPPexit		; N: no more checking necessary
;
;  Further checking is needed
;
	push	eax
	push	ebx
;
;  Get PT0 address
;
	mov	eax,cr3			; get address of page directory
	and	ax,0F000h
	mov	eax,es:[eax]		; page table 0 address
	and	ax,0F000h
;
;  Get PTE index
;
CPPindex:
	lea	edx,es:[edi+ecx]	; again (might have skipped first page)
	mov	ebx,edi			; pointer to buffer
	shl	ebx,10			; get PTE index to start of buffer
	shr	ebx,22
	shl	edx,10			; get PTE index to end if buffer
	shr	edx,22

	sub	dx,bx			; number of PTEs spanned
	inc	dx
CPPloop:
	test	word ptr es:[eax][ebx*4],P_WRITE
	jz	short CPPwrite
	inc	bx
	dec	dx
	jnz	short CPPloop

CPPok:
	pop	ebx
	pop	eax
CPPexit:
	pop	edx
	ret
;
;  A write protected page has been detected: return size of allowable transfer
;
CPPwrite:
	shl	ebx,12			; end of buffer
	sub	ebx,edi			;Q: Size of buffer > 0?
	jbe	short CPPfirstPage	; N: first page is write protected
	mov	ecx,ebx
	jmp	short CPPok
;
;  The first page is write protected - skip it
;
CPPfirstPage:
	add	ebx,P_SIZE		; number of bytes in first page
	add	edi,ebx			; skip to next page
	add	esi,ebx			; increment source buffer accordingly
	sub	ecx,ebx			;Q: Last page?
	ja	short CPPindex		; N: skip first page
	xor	ecx,ecx			; Y: return with zero sized buffer
	jmp	short CPPok

CheckPageProt	endp

;==============================================================================
;==
;==  InitReset: This routine points the reset vector at F000:FFF0 to
;==		the ResetRoutine and saves the state of the IVT.
;==
;==  Entry: (Protected Mode)
;==	DS	= VDMD_GSEL
;==	FS	= DATA32_GSEL
;==	GS	= RCODEA_GSEL
;==
;==  Exit:  (Protected Mode)
;==
;==============================================================================
InitReset proc	near
	push	eax

	call	SaveIVT

	cmp	gs:[OldResetVec],FALSE	;Q: Using a ResetRoutine?
	je	short IRexit		; N: don't change ROM

	mov	ax,seg R_CODE
	shl	eax,16
	lea	ax,gs:[ResetRoutine]
	mov	fs:[0FFFF0h][1],eax
IRexit:
	pop	eax
	ret
InitReset	endp

;==============================================================================
;==
;==  SaveIVT: This routine saves the state of the IVT at the time CEMM
;==	      becomes active.
;==
;==  Entry: (Protected Mode)
;==	DS	= _DATA
;==	FS	= DATA32_GSEL
;==
;==  Exit:  (Protected Mode)
;==
;==============================================================================
SaveIVT proc	near

	test	gs:[Current_State],fState_WinCntrl
				;Q: is this due to windows shutting down
	jz	SIcont		;N: Save the int vector table
	ret			;Y: return
SIcont:

	push	eax
	push	ecx

	mov	ecx,NumOfVectors
SIloop:
	mov	eax,fs:[ecx*4-4]
	mov	ds:[OldIVT][ecx*4-4],eax
	loop	SIloop

	pop	ecx
	pop	eax
	ret
SaveIVT	endp

;==============================================================================
;==
;==  RestoreIVT: This routine restores the state of the IVT to the state it
;==	         was at the time CEMM became active.
;==
;==  Entry: (Protected Mode)
;==	DS	= _DATA
;==	FS	= DATA32_GSEL
;==
;==  Exit:  (Protected Mode)
;==
;==============================================================================
RestoreIVT proc	near
	push	eax
	push	ecx

	mov	ecx,NumOfVectors
RIloop:
	mov	eax,ds:[OldIVT][ecx*4-4]
	mov	fs:[ecx*4-4],eax
	loop	RIloop

	pop	ecx
	pop	eax
	ret
RestoreIVT	endp

_TEXT	ends
;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
R_CODE	segment
	assume	cs:R_CODE
;==============================================================================
;==
;==  ResetRoutine: This routine returns to real mode - and returns to the ROM
;==		   reset routine.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:  (Real Mode)
;==
;==============================================================================
ResetRoutine	proc	near

	;
	; Change to EMM386's real mode stack. We know at this point we are
	; going to reboot and hence are going to turn off. If the user had
	; done a jmp ffff:0 with his stack in a UMB we'd be hosed if we
	; don't change stacks.
	;
	push	seg R_STACK
	pop	ss
	lea	sp,R_STACK:RealStackTop

	call	Shutdown
	jmp	cs:[OldResetVec]

ResetRoutine	endp
;==============================================================================
;==
;==  ShutDown: This routine returns to real mode and restores the interrupt
;==	       vectors to a pre-CEMM condition.  This is to prevent vectors
;==	       from pointing to the UMB region which disappears after CEMM
;==	       returns to real mode.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:  (Real Mode)
;==
;==============================================================================
ShutDown proc	near
	push	ax
	pushf
	cli

	or	cs:[Current_State],fState_WinCntrl
					; we shall set this flag so that set
					; and get a20cnt routines in util.asm
					; do not do xms calls. We do not care
					; about resetting this bit as we are
					; going to reboot any way.


;
;  Indicate a CPU shutdown - restore IVT to avoid vectors pointing to UMB region
;
	or	cs:[GenFlags],fShutDown

	mov	al,0Fh
	out	84h,al		; port 84/85 return to real sequence
	mov	al,00h
	out	85h,al
	jmp	$+2		; clear prefetch/avoid race cond

	popf
	pop	ax
	ret
ShutDown	endp


R_CODE	ends
;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
LAST	segment
	assume	cs:LAST,ds:_DATA,gs:R_CODE

ifdef	ROMCOMP

;==============================================================================
;==
;==  FixIVTptrs: Fixes pointers on IVT for ROM compression on CPQ machines.
;==
;== 		  32K ROMs F000:0    - F000:7FFF = mirror image of
;==			   F000:8000 - F000:FFFF
;==
;==   		    	386G  Deskpr 386/16, 386/20, 386/25
;==   			386F  Deskpro 386s
;==   			386H  Deskpro 386/20e (Horizon only)
;==
;== 		  40K ROMs F000:0    - F000:5FFF   junk
;==			   F000:6000 - F000:FFFF = system ROM
;==
;==   			386H  Deskpro 386/20e (Gambler only)
;==   			386H  Deskpro 386/25e
;==   			386L  Deskpro 386/33
;==
;==		  64K ROMs F000:0    - F000:FFFF
;==
;==	   		386P  Portable 386
;==			386E  Systempro/Deskpro 486/25
;==			386D  Deskpro 386n
;==
;==  		  The pointers which need to be changed in order to
;==		  reclaim the lower 32K space include:
;==
;==			1BD0	dummy end of interrupt handler
;==			1C1F	IRQ 9 entry point
;==			1C28	287 error entry point
;==			20FB	diskette parameter table entry point
;==			2E12	fixed disk BIOS entry point
;==			2FA4	old fixed disk hardware entry point
;==			3343	fixed disk hardware entry point
;==			4A97	real-time clock entry point
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================
FixIVTptrs proc	near
	pusha
	push	ds
	push	es
	push	fs

ifdef 901023
	test	gs:[GenFlags],fROMComp	;Q: ROM compression on this machine?
	jz	FIpExit			; N: can't use ROM space
	mov	cx,cs:[ROMstart]	; Y: start of ROM after compression

;
;  Make sure BIOS points to INT 13h entry point on upper 32K image by making an
;  INT 2Fh AH=13h (DOS 3.20 and later): changes the INT 13h calls from IBMBIO.
;  Note: Does not affect DOS 3.00-3.10, but valid only on DOS 3.20 and later.
;
	mov	ah,13h		; get current int 13h vectors
	int	2fh
	cmp	dx,cx		;Q: Does INT 13h need updating?
	jae	short FIpSetI13h; N: continue
	or	bx,8000h	; Y: int 13h vector used by boot.
	or	dx,8000h	; int 13h handler
FIpSetI13h:
	mov	ah,13h		; restore Int 13h pointer
	int	2fh		; make sure they are pointing to upper 32K image

	mov	bx,cx		; start of ROMs in BX
;
;  Point all vectors of IVT to actual ROM (image above F000:8000)
;QLEO: Need to do this after we know for sure that CEMM will load????
;
	xor	di,di
	mov	fs,di
	mov	cx,256
FIpLoop:
	cmp	word ptr fs:[di][2],0F000h ;Q: Vector pointing to System ROM
	jne	short FIpContLoop	   ; N: continue
	cmp	word ptr fs:[di],bx	   ;Q: Invalid region of System ROM?
	jae	short FIpContLoop	   ; N: continue
	add	word ptr fs:[di],8000h	   ; Y: reflect to image above 32K!
FIpContLoop:
	add	di,4			; next vector
	loop	FIpLoop			; get all 256 of them

endif	; 901023
FIpExit:
	pop	fs
	pop	es
	pop	ds
	popa
	ret
FixIVTptrs	endp

;==============================================================================
;==
;==  FixROMptrs: Fixes pointers in the ROM for ROM compression on CPQ machines.
;==
;== 		  32K ROMs F000:0    - F000:7FFF = mirror image of
;==			   F000:8000 - F000:FFFF
;==
;==   		    	386G  Deskpr 386/16, 386/20, 386/25
;==   			386F  Deskpro 386s
;==   			386H  Deskpro 386/20e (Horizon only)
;==
;== 		  40K ROMs F000:0    - F000:5FFF   junk
;==			   F000:6000 - F000:FFFF = system ROM
;==
;==   			386H  Deskpro 386/20e (Gambler only)
;==   			386H  Deskpro 386/25e
;==   			386L  Deskpro 386/33
;==
;==		  64K ROMs F000:0    - F000:FFFF
;==
;==	   		386P  Portable 386
;==			386E  Systempro/Deskpro 486/25
;==			386D  Deskpro 386n
;==
;==  		  The pointers which need to be checked/changed in order to
;==		  reclaim the lower 32K space include:
;==
;==				      9FA8
;==                                   DB59
;==				      DB73
;==				      ED03
;==				      ED14
;==				      F8E9
;==				      FEF3
;==				      FEF3
;==			BIM pointer @ FFE0
;==			CPU type ptr@ FFE2
;==
;==  Entry: (Real Mode)
;==	BX = Start of ROM after compression.
;==	FS = F000h
;==
;==  Exit:
;==
;==============================================================================
FixROMptrs proc	near
	pusha
	pushf
	cli

ifdef 901023
	test	gs:[GenFlags],fROMComp	;Q: ROM compression on this machine?
	jz	FRpExit			; N: can't use ROM space
	mov	cx,cs:[ROMstart]	; Y: start of ROM after compression

	call	UnProtectROM		; unwrite protect the Shadow ROM

	mov	ax,0F000h
	mov	fs,ax

	xor	si,si
	mov	cx,NumROMptrs
FRpLoop:
	mov	di,cs:[ROMptrs][si]
	cmp	word ptr fs:[di],bx
	jae	short FRpNext
	or	word ptr fs:[di],8000h
FRpNext:
	add	si,2
	loop	FRpLoop

ifdef 901021
	cmp	fs:[0FFE0h],bx	;Q: BIM data structure OK?
	jb	short FRpFixPtrs; N: fix it
	cmp	fs:[0FFE2h],bx	;Q: Is this pointer OK?
	jae	short FRpExit	; Y: continue

FRpFixPtrs:
	call	UnProtectROM	; unwrite protect the Shadow ROM
	or	word ptr fs:[0FFE2h],8000h ; point to upper image
	mov	di,fs:[0FFE0h]	; get address of BIM data structure
	cmp	di,bx		;Q: Need to fix BIM data structure?
	jae	short FRpChkSum	; N: ROM check sum
	or	word ptr fs:[0FFE0h],8000h ; Y: point to upper image

ifdef 901015 ; check sum on BIM data structure will change as memory is allocated
	mov	cx,4		; 4 words
FRpBIM:
	mov	ax,fs:[di]	; move BIM data structure to upper image
	mov	fs:[8000h][di],ax
	add	di,2
	loop	FRpBIM
endif
FRpChkSum:
ifdef 901015 ; check sum on BIM data structure will change as memory is allocated
	mov	di,bx		; start of ROM
	xor	dx,dx		; check sum init
	xor	cx,cx		; assume 64K ROM
	sub	cx,bx		; actual size ROM
	dec	cx		; don't include check sum
FPrChkSumLoop:
	add	dl,fs:[di]	; calculate Check Sum
	inc	di		; next byte
	loop	FPrChkSumLoop
	neg	dl
	mov	fs:[0FFFFh],dl	; save check sum
endif
endif	;901021


	call	ProtectROM	; protect Shadow ROM from writes

endif	;901023
FRpExit:
	popf
	popa
	ret
FixROMptrs	endp

endif	; ROMCOMP

;==============================================================================
;==
;==  NoResetRoutine: No ResetRoutine is required on COMPAQ machines because
;==		     CEMM is turned off by an OUT 84,0Fh and OUT 85h,0 sequence.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================
NoResetRoutine proc	near
	push	ebx
	push	es

	mov	byte ptr cs:[ROMSet],2	   ; no ROM alias at F000:F000
	mov	dword ptr cs:[ROMSet][2],0 ; .. is needed

	mov	gs:[OldResetVec],FALSE	; no reset routine will be used

	pop	es
	pop	ebx
	ret
NoResetRoutine	endp
;==============================================================================
;==
;==  SaveResetVector: This routine saves the ROM reset vector. This is the
;==		      location which is jumped to by the ROM at F000:FFF0.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================
SaveResetVector	proc	near
	push	ebx
	push	es

	cmp	gs:[OldResetVec],TRUE	;Q: Using a ResetRoutine?
	jne	short SRVexit		; N: exit

	les	bx,[pResetVec]		; get address of jump instruction
	mov	ebx,es:[bx][1]		; get address jumped to
	mov	gs:[OldResetVec],ebx	; save this address

SRVexit:
	pop	es
	pop	ebx
	ret
SaveResetVector	endp
LAST	ends
	end
