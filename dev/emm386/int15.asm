.386p
	page 58,132
;=============================================================================
	title	I N T 1 5 - Hooks INT 15: Move Block & Traps CTRL-ALT-DEL
;=============================================================================
;==
;== (C) Copyright MICROSOFT Corp. 1989-1991
;== (C) Copyright COMPAQ Computer Corp. 1989-1991
;==
;==	Title: EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;==
;==	Module: INT15 - Emulates Move Blocks & Traps Ctrl-Alt-Del
;==
;==	Version: 1.00
;==
;==	Date: February 15, 1990
;==
;==	Author: Leo Cohen
;==
;=============================================================================
;==
;==	Change Log:
;==
;==	DATE	 REVISION	Description
;==	-------- --------	--------------------------------------------
;==     02/15/90 0.00	        Original
;==
;==	03/06/91 M014		Restore cx before checking to see if it is 0.
;==
;=============================================================================
;==
;==   Functional Description:
;==
;==   This module emulates move blocks and traps CTRL-ALT-DEL. If a CTRL-ALT-DEL
;==   is detected, it switches the system from virtual to real mode because
;==   certain ROMs having priviliged instructions in the REBOOT code sequence.
;==   Also, the amount of extended memory (INT 15h & AH=88h) is trapped and
;==   the value returned is the amount of free extedned memory left in the
;==   system.
;==
;=============================================================================
;=============================================================================
;==	P U B L I C   D E C L A R A T I O N S
;=============================================================================
	public	pINT15hHandler
	public	rINT15hHandler
	public	rINT19hHandler
	public	MoveBlock
	public	MBMoveW
	public	MBrepMovD
	public	MoveBlockParity

;=============================================================================
;==	L O C A L   C O N S T A N T S
;=============================================================================
	include vdmseg.inc
	include vdmsel.inc
	include desc.inc
	include page.inc
	include oemdep.inc
	include vm386.inc
	include emm386.inc
	include	emmfunct.inc
	include emmdata.inc
	include XMM.inc

;=============================================================================
;==	L O C A L   E Q U A T E S
;=============================================================================
;=============================================================================
;==  Move Block Local Data Structure (and flags)
;=============================================================================
MBStack	struc
  MBStat 	db	0		; move block status
  MBFlags 	db	0		; move block flags
MBStack	ends

MBData	equ	size MBStack
;=============================================================================
;== Flags for MBFlags
;=============================================================================
fMBDiagSrc	equ	00000001b	; diagnostic port is source
fMBDiagTrg	equ	00000010b	; diagnostic port is target
fMBDiagCache	equ	00000100b
fMBA20		equ	00001000b
fMBWeitek	equ	00010000b
fMBHMA		equ	00100000b	; HMA involved in transfer
fMBHMABit	equ	5		; HMA involved in transfer
;=============================================================================
;==  Move Block GDT Data Structure
;=============================================================================
MB_GDT	struc
  MG_dummy	dd 2 dup (?)		; dummy descriptor entry
  MG_GDT	dd 2 dup (?)		; GDT entry
  MG_Source	dd 2 dup (?)		; source segment entry
  MG_Target	dd 2 dup (?)		; target segment entry
MB_GDT	ends
;=============================================================================
;==  System Descriptor Vector Fields
;=============================================================================
sSysDescVec	struc
  SDVcount	dw	0	; size of System Descriptor Vector
  SDVmodel	db	0	; machine model
  SDVsubModel	db	0	; machine submodel
  SDVRevLev	db	0	; BIOS rev level
  SDVInfoFlgs	db	0	; Information flags
  SDVrsvd	db 4 dup (0)	; reserved
sSysDescVec	ends
SDVInfoXBDA	equ	100	; XBDA area is allocated
;=============================================================================
;==  Scan Code
;=============================================================================
SC_DEL		equ	0053h		; Delete
SC_INS		equ	0052h		; Insert
;=============================================================================
;==  Keyboard Control Byte Bit Positions (ROM Data Area)
;=============================================================================
KP_ALT		equ	00001000b
KP_CTRL		equ	00000100b
KP_LSHIFT	equ	00000010b
KP_RSHIFT	equ	00000001b
;=============================================================================
;==	E X T E R N A L   R E F E R E N C E S
;=============================================================================
_TEXT	segment
	extrn	CheckPageProt:near
	extrn	UpdateHMA:near
	extrn	ReflectInterrupt:near
_TEXT	ends

_DATA	segment
	extrn	pLastVMTF:word
	extrn	LastVMTF:word
_DATA	ends

R_CODE	segment
	extrn	ShutDown:near
R_CODE	ends
;=============================================================================
;==	L O C A	L   D A T A
;=============================================================================
R_CODE	segment

SysDescVec  sSysDescVec<>	; buffer for system descriptor vector

R_CODE	ends
;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
_TEXT	segment
	assume	cs:_TEXT,ds:_DATA,es:_DATA,gs:R_CODE,ss:STACK
;==============================================================================
;==
;==  pINT15hHandler: Determines if a Move Block needs to be emulated or prevent
;==		     a multiprocessor machine from dispatching other processors.
;==		     Other processors in the system are not aware of the page
;==		     tables (mapping) on the main processor and thus linear
;==		     addresses will not correspond to similar physical addresses.
;==
;==  Entry: (Protected Mode)
;==
;==  Exit:  (Virtual Mode)
;==
;==============================================================================
pINT15hHandler	proc	far
	push	ebp
	sub	sp,MBData		; room for local variables
	movzx	ebp,sp

	push	edx
	mov	dx,DATA32_GSEL
	mov	ds,dx
	mov	dx,seg R_CODE
	movzx	edx,dx
	shl	edx,4			; DS:[EDX] points to R_CODE
	assume	ds:R_CODE


	cmp	ah, 04fh
	je	short pI15KeyBoard

	cmp	ah,87h			;Q: Move Block function?
	je	short pINT15MB		; Y: do move block

	test	[edx][GenFlags],fEISA	;Q: EISA/SYSTEMPRO?
	jz	short pINT15Reflect     ; N: reflect interrupt

	cmp	ah,0E2h			;Q: Multiprocessor availability?
	ja	short pINT15Reflect     ; N: reflect interrupt
	cmp	ah,0E0h			;Q: Multiprocessor dispatch?
	jb	short pINT15Reflect     ; N: reflect interrupt
	xor	al,al			; Y: no MP support
	or	[bp][MBData][VTFO].VMTF_EFLAGS,FLAGS_CF ; set carry flag
	jmp	pINT15exit

pINT15Reflect:
	pop	edx
	add	sp,MBData		; local variables
	mov	bp,sp
	push	15h
	jmp	ReflectInterrupt
;
; Emulate move block function
;
pINT15MB:
	btr	[edx][TrapFlags],fI15trapBit ;Q: Trap flag set?
	jc	short pI15MB		     ; Y: do emulation of Move Block
;
;QLEO: Fast MB function response: Check if IVT is hooked, if not, don't reflect
;
	assume	ds:ABS0
	shl	edx,12
	mov	dx,offset R_CODE:rINT15hHandler
	cmp	edx,dword ptr [int15]
	jne	short pINT15Reflect
	shr	edx,16
	shl	edx,4

pI15MB:
	assume ds:R_CODE
        test    [bp][MBData][VTFO].VMTF_EFLAGS,FLAGS_IF ;Q: Did client have IF set?
        jz      short pI15MoveBlock                     ; N: don't turn them on
        test    [edx][GenFlags],fNoINT                  ;Q: Allow interrupts?
        jnz     short pI15MoveBlock                     ; N: don't turn them on
	assume ds:nothing
	sti
pI15MoveBlock:
	call	MoveBlock			; emulate move block
	cli					; do not allow interrupts

pINT15exit:
	pop	edx
	add	sp,MBData		; local variables
	pop	ebp
	iretd

;========================================================================
pI15KeyBoard:

	cmp	al,SC_INS		; Q: Ins ScanCode?
	je	pI15ctrlalt		; Y: check to see if it ctrl-alt
					;    keys pressed

	cmp	al,SC_DEL		;Q: Del ScanCode?
	jne	pINT15reflect		; N: chain to previous handler

pI15ctrlalt:
	push	edx
	mov	dx,seg romdata
	movzx	edx,dx
	shl	edx,4			; DS:[EDX] points to romdata
	assume	ds:romdata


	push	bx
	mov	bl,[edx][KeyState]		; get kbd ctrl status
	and	bx,KP_ALT+KP_CTRL	; look only at these keys
	cmp	bx,KP_ALT+KP_CTRL	;Q: are both keys pressed ?
	pop	bx
	pop	edx
	assume	ds:R_CODE
	jne	short pINT15reflect	; N: chain to previous handler

	;
	; ctrl-alt with ins or del pressed
	;

	cmp	al, SC_INS		; Q: Ins ScanCode
	je	pI15chkzen		; Y: check to see if zenith
					; N: => del pressed

	push	ebx
	mov	ebx, dword ptr [bp][MBData][VTFO].VMTF_CS
	shl	ebx, 4
	add	ebx, dword ptr [bp][MBData][VTFO].VMTF_EIP
					; ebx = return address of caller
	call	InUMBs			; Q:is the int 15 coming from Umbs
	pop	ebx
	jc	pINT15reflect		; Y: do not turn off
	;
	; At this point we know that a ctrl-alt-del sequence has happened and
	; the int 15 ah=4f has not been issued from a UMB. So we need to 
	; let the int be processed and get back control so that we can 
	; decide whether to turn off or not (see rI15KeyBoard in this file).
	; We set the following flag to indicate to the int reflection code 
	; in pictrap.asm to set up the stack frame so that control is first
	; obtained in rI15KeyBorad before the int 15 caller.
	;
pI15TurnOff:
	or	[edx][GenFlags], fTurnOff
	jmp	pINT15reflect


pI15chkzen:
	test	[edx][GenFlags],fZenith	; Q: is this a zenith machine
	jz	pINT15reflect		; N: reflect the int 15
					; Y: check to see in int 15 is
					;    coming from F000 segment
	cmp	dword ptr [bp][MBData][VTFO].VMTF_CS, 0F000h
					; Q: is int 15 coming from below f000
	jb	pINT15reflect		; Y: do not set flag

	;
	; At this point we know that a ctrl-alt-ins sequence has happened 
	; on a Zenith machine and that the int 15 is coming from the Zenith
	; ROM. So we set the fTurnOff flag so that the int reflection code 
	; in pictrap.asm sets up the stack frame in such a manner that 
	; control is first obtained in rI15KeyBorad before the int 15 caller.
	;
	jmp	short pI15TurnOff
	assume	ds:nothing


pINT15hHandler	endp
;==============================================================================
;==
;==  MoveBlock: Emulates 386 ROM Move Block (INT 15h & AH=87h).
;==
;==  Entry:  (Protected Mode)
;==	   CX = number of words to move
;==	  EDX = 32bit pointer to R_CODE
;==	   DS = DATA32_GSEL
;==	ES:SI = points to the Move Block GDT structure (ES in VMTF)
;==
;==                     ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
;==                     º          MG_Target            º +10h
;==                     ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                     º          MG_Source            º +0Ch
;==                     ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                     º                               º +08h
;==                     ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                     º                               º +00h
;==                     ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
;==
;==  Exit:   (Protected Mode)
;==	   AH = 00 : no error 			  Flags: (ZF & NC)
;==		01 : parity error                 Flags: (NZ & CY)
;==		02 : exception error              Flags: (NZ & CY)
;==		03 : gate address bit A20 fails.  Flags: (NZ & CY)
;==
;==============================================================================
MoveBlock proc	near
	push	eax
	push	ebx
	push	ecx
	push	esi
	push	edi
;
;  Initialize local data and flags
;
	movzx	ecx,cx			; use only 16 bits
	mov	[bp].MBStat,0		; init status of move block
	mov	[bp].MBFlags,0		; init flags of move block
	mov	bx,DATA32_GSEL
	cld
	mov	es,bx			; ES = DATA32_GSEL
;
;  Move Block cannot move more than 8000h words
;
	cmp	cx,8000h		;Q: word count too high
	ja	MBExcpError		; Y: report exception error
					; N: check length of segments
;
;  Access to user mini GDT (descriptor table) via DS:[EBX]
;
	movzx	ebx,[bp][MBData][VTFO].VMTF_ES
	movzx	esi,si
	shl	ebx,4
	add	ebx,esi
;
;  Make sure LIMIT field is OK, else return an exception error
;
	mov	ax,cx
	shl	ax,1
	dec	ax

	cmp	[ebx][MG_Source].LIMIT,ax ;Q: Source limit too low?
	jb	MBExcpError		  ; Y: report exception error

	cmp	[ebx][MG_Target].LIMIT,ax ;Q: Target limit too low?
	jb	MBExcpError		  ; Y: report exception error

	lea	esi,[ebx].MG_Source	; DS:ESI pts to input source descr
	call	DiagsOn386SX		;Q: Accessing diag register on 386SX?
	jnc	short MBScont		; N: continue
	or	[bp].MBFlags,fMBDiagSrc ; Y: set flag for diag addr
MBScont:
;
;  Need to Unwrap page tables if HMA area is used
;
	call	HMARegion		;Q: Within HMA?
	jnc	MBScont2		; N: continue
	or	[bp].MBFlags,fMBHMA	; Y: set flag for HMA usage
MBScont2:
	lea	esi,[ebx].MG_Target	; DS:ESI pts to target descriptor
	call	DiagsOn386SX		;Q: Accessing diag register on 386SX?
	jnc	short MBTcont		; N: continue
	or	[bp].MBFlags,fMBDiagTrg	; Y: set flag for diag addr
MBTcont:
;
;  Need to Unwrap page tables if HMA area is used
;
	call	HMARegion		;Q: Within HMA?
	jnc	MBTcont2		; N: continue
	or	[bp].MBFlags,fMBHMA	; Y: set flag for HMA usage
MBTcont2:
;
;  Verify descriptors passed in
;
	assume	ds:R_CODE
	mov	eax,[edx][pGDT]

	mov	esi,[ebx].MG_Source
	mov	edi,[ebx].MG_Target
	mov	[eax].MBSRC_GSEL,esi
	mov	[eax].MBTAR_GSEL,edi

	mov	esi,[ebx].MG_Source[4]
	mov	edi,[ebx].MG_Target[4]
	mov	[eax].MBSRC_GSEL[4],esi
	mov	[eax].MBTAR_GSEL[4],edi

	mov	ax,MBSRC_GSEL
	verr	ax		;Q: source selector valid ?
	jnz	MBExcpError	; N: return exception error

	mov	ax,MBTAR_GSEL
	verw	ax		;Q: target selector valid ?
	jnz	MBExcpError	; N: return exception error
;
;  Load 32bit pointers into ESI and EDI
;
	rol	esi,8			; bits 24.31 and 16.23 in DI
	rol	si,8			; swap bytes
	shl	esi,16			; high word
	mov	si,word ptr [ebx].MG_Source[2]	; low word

	rol	edi,8			; bits 24.31 and 16.23 in DI
	rol	di,8			; swap bytes
	shl	edi,16			; high word
	mov	di,word ptr [ebx].MG_Target[2]	; low word
;
;  Check for read or write from/to diag byte location (not both)
;
	test	[bp].MBFlags,fMBDiagSrc+fMBDiagTrg	;Q: Diag on 386s?
	jnz	MBDiag  				; Y: use 386s port
;
;  Check for read/write to/from HMA
;
	btr	[bp].MBFlags,fMBHMABit	;Q: Using HMA?
	jnc	short MBwriteProt	; N: let it procede

	assume	ds:R_CODE
	test	[edx][Current_State],fState_A20Ena;Q: Is A20 enabled
	jnz	short MBWeitek			  ; Y: check WEITEK map
	or	[bp].MBFlags,fMBA20		  ; N: restore A20

MBWeitek:
	test	[edx][Weitek_State],fWeitek_Map	;Q: WEITEK mapped?
	jz	short MBUpdate			; N: need to update
	or	[bp].MBFlags,fMBWeitek		; Y: restore Weitek

MBUpdate:
	test	[bp].MBFlags,fMBA20+fMBWeitek	;Q: Need to update HMA?
	jz	short MBwriteProt		; N: let it procede
;
;  Either a disabled A20 or a mapped Weitek is in effect. Access HMA
;
	or	[edx][Current_State],fState_A20Ena  ; enable A20
	and	[edx][Weitek_State],not fWeitek_Map ; unmap Weitek
	or	[bp].MBFlags,fMBHMA		    ; remember to restore HMA
	call	UpdateHMA			    ; fix up HMA
;
;  Make sure nothing is written into a write protected page
;
MBwriteProt:
	shl	ecx,1
	call	CheckPageProt
 	shr	ecx,1			; M014: restore cx. cx has size of
					; M014: Xfer in words.
	jcxz	short MBexit

;
;  Finally, do the move block! (dwords)
;
MBOddMove:
	test	cx,01h			;Q: move an odd # of words?
	jz	short MBMoveD		;  N: move dwords
MBMoveW:
	MOVS_WORD_USING_DS_ESI		;  Y: move the first one => dwords now
MBMoveD:
	shr	cx,1			; move CX dwords...
	REP_MOVS_DWORD_USING_DS_ESI
MBrepMovD:
MBExit:
	test	[bp].MBFlags,fMBHMA	;Q: Fix HMA
	jz	short MBLeave		; N: let it procede

	test	[bp].MBFlags,fMBA20		      ;Q: Is A20 disabled?
	jz	short MBWeitekX			      ; N: check WEITEK map
	and	[edx][Current_State],not fState_A20Ena;Y: disable A20

MBWeitekX:
	test	[bp].MBFlags,fMBWeitek		;Q: Is Weitek mapped?
	jz	short MBUpdateX			; N: update
	or	[edx][Weitek_State],fWeitek_Map	; Y: map WEITEK

MBUpdateX:
	call	UpdateHMA			; fix up HMA

MBLeave:
	pop	edi
	pop	esi
	pop	ecx
	pop	ebx
	pop	eax
	mov	ah,[bp].MBStat
;
;  Set return flags
;
	or	[bp][MBData][VTFO].VMTF_EFLAGS,FLAGS_ZF
	and	[bp][MBData][VTFO].VMTF_EFLAGS,not FLAGS_CF

	or	ah,ah		;Q: error occured ?
	jz	short MBRet	;  N: continue
				;  Y: set error in client's flags
	and	[bp][MBData][VTFO].VMTF_EFLAGS,not FLAGS_ZF
	or	[bp][MBData][VTFO].VMTF_EFLAGS,FLAGS_CF

MBRet:
	ret

MBExcpError:
	mov	[bp].MBStat,2
	jmp	short MBExit

;===============================================================================
;==
;==  On 386s, a read/write from diag port is rerouted from memory to IO map.
;==	memory map: 80C00000h --> IO map: port 878h
;==
;==  Enter:  DS:ESI = Source buffer
;==	     ES:EDI = Target buffer
;==	     DS:EDX = R_CODE
;==
;===============================================================================
MBDiag:
	push	bx
	push	dx
	pushf

	cli				; no interrupts
	shl	cx,1			; byte count
	mov	dx,PORT386s		; 386s I/O port
;
; assume diag is target
;
	mov	bx,di
	test	[bp].MBFlags,fMBDiagSrc	;Q: Is diag the source?
	jz	short MBDdesc		; N: assumption correct
	mov	bx,si			; Y: source work descriptor in GDT

MBDdesc:
	add	dx,bx
	call	DisableNMI		; disable NMI

	test	[bp].MBFlags,fMBDiagTrg	;Q: Is diag the target?
	jnz	short MBWriteDiag	; Y: write to proper ports

MBReadDiag:
	cmp	dx,PORT386s+7 		;Q: 80C00000-80C00007h?
	ja	short MBDexit		; N: port is invalid
	db 67h				; ** USE ES:EDI **
	insb				; Y: read next I/O port value
	inc	dx			; next port
	loop	MBReadDiag
	jmp	short MBDexit

MBWriteDiag:
	cmp	dx,PORT386s+3 		;Q: 80C00000-80C00003h?
	ja	short MBDexit		; N: port is invalid
	db 67h				; ** USE DS:ESI **
	outsb				; Y: read next I/O port value
	inc	dx			; next port
	loop	MBWriteDiag

MBDexit:
	call	EnableNMI

	popf				; restore interrupts
	pop	dx
	pop	bx
	jmp	MBExit

MoveBlock	endp

;===============================================================================
;==
;==  DiagsOn386SX : Check if trying to access diagnostic register on a COMPAQ
;==		    386SX machine.
;==
;==  ENTER: DS:ESI = GDT selector
;==	    DS:EDX = R_CODE
;==
;==  EXIT: NC = no conflict
;==        CY = conflict
;==
;===============================================================================
DiagsOn386SX	proc	near
	assume	ds:R_CODE

	test	[edx][GenFlags],f386SX ;Q: is it a COMPAQ 386SX base machine?
	jz	short DO386SXno	       ; N: continue

	cmp	byte ptr [esi][7],080h ;Q: Is it diag address port (80C00000h)
	jne	short DO386SXno	       ; N: continue

	cmp	byte ptr [esi][4],0C0h ;Q: Is it diag address port (80C00000h)
	jne	short DO386SXno	       ; N: continue

	stc
	ret

DO386SXno:
	clc
	ret
DiagsOn386SX	endp

;===============================================================================
;==
;==  HMARegion : Check if trying to access the HMA region
;==
;==  ENTER: DS:ESI = GDT selector
;==
;==  EXIT: NC = not in HMA region
;==        CY = in HMA region
;==
;===============================================================================
HMARegion proc	near

	cmp	byte ptr [esi][4],11h ;Q: Is it above HMA (>110000h)
	jae	short HMARnoHMA	      ; Y: continue

	cmp	byte ptr [esi][4],0Fh ;Q: Is it below HMA (<100000h)
	jbe	short HMARnoHMA	      ; Y: continue

	cmp	byte ptr [esi][7],0   ;Q: Is it HMA (100000h-110000h)
	jne	short HMARnoHMA	      ; Y: continue

	stc
	ret

HMARnoHMA:
	clc
	ret
HMARegion	endp

;===============================================================================
;==
;==  DisableNMI : Disables NMI interrupt via external HW registers/gates.
;==
;===============================================================================
DisableNMI proc	near
	push	ax

	in	al,NMI386s
	mov	[bp].MBStat,al		; save status for NMI port
	or	al,08h			; disable IOCHK and NMI
	out	NMI386s,al

	pop	ax
	ret
DisableNMI	endp

;===============================================================================
;==
;==  EnableNMI : Enables NMI interrupt via external HW registers/gates.
;==
;===============================================================================
EnableNMI proc	near
	push	ax

	xor	ax,ax
	xchg	al,[bp].MBStat		; get NMI status and clear status
	out	NMI386s,al		; restore IOCHK and NMI

	pop	ax
	ret
EnableNMI	endp

;===============================================================================
;==
;==  MoveBlockParity: Parity error handler during MoveBlock routine.
;==                   This routine writes to the parity error location to
;==                   clear the parity error on the memory board, then it
;==		      clears the parity error on the system board.
;==
;==  Entry: (Protectyed Mode)
;==	ESI = pointing to word/dword after the location which caused the error
;==	DS = DATA32_GSEL
;==	ES = DATA32_GSEL
;==
;===============================================================================
MoveBlockParity:
	pop	ebp

	dec	si
	dec	si		; DS:SI pts to address causing parity error

	mov	ax,[esi] 	; retrieve value and write it back
	mov	[esi],ax 	; to reset parity on memory board.

	in	al,PPI		; get parity error flags, reset then set
	xchg	ah,al
	in	al,80h		; delay with dummy IN
	xchg	al,ah
;
;  Dissable IOCHECK & PCHECK
;
	or	al,PPO_MASK_IOCHECK+PPO_MASK_PCHECK
	out	PPO,al		; disable them
	xchg	ah,al
	in	al,80h		; delay with dummy IN
	xchg	al,ah
;
;   Enable IOCHECK & PCHECK
;
	and	al,NOT (PPO_MASK_IOCHECK+PPO_MASK_PCHECK)
	out	PPO,al		; enable them: system board parity now reset

	mov	[bp].MBStat,1	; set parity error
	add	sp,12		; remove NMI stuff from stack
	jmp	MBExit	 	; and exit move block

;===============================================================================
;==
;==  InUMBs: checks to see if 32 bit address in ebx is in UMBs or not
;==
;==  Entry: (Protectyed Mode)
;==	DS = DATA32_GSEL
;==	EDX = 32bit pointer to R_CODE
;==
;===============================================================================

InUMBs 	proc	near

	push	edi
	push	esi
	push	eax
	push	ecx

	cmp	[edx][UMBptr],0		;Q: UMB ARENAs on the system?
	je	IUexit			; N: exit


	movzx	edi,[edx][UMBptr]
	shl	edi,4				; offset to first ARENA
	mov	eax,edi				;
IUloop:
	cmp	[edi].Sig,'M'			;Q: Is it an ARENA?
	je	short IUcont			; Y: OK
	cmp	[edi].Sig,'Z'			;Q: Is it an ARENA?
	jne	short IUexit			; N: exit, corruption
;
;  Valid ARENA: Get starting paragraph and length
;
IUcont:
	add	eax, 010h			; add size of arena
	movzx	ecx,[edi].Len			; get length in paras
	shl	ecx, 4				; length in bytes

	mov	esi, eax			; eax = start of block
	add	esi, ecx			; esi -> next arena

	cmp	[edi].Own,InvMem		;Q: Is it a UMB?
	je	short IUnext			; N: get next ARENA

	cmp	ebx, eax			; Q: is ebx below start
	jb	IUexit				; Y: done

	cmp	ebx, esi			; Q: is ebx below end of block
	jb	IUdone				; Y: CY SET. exit

IUnext:
	mov	eax,esi				; eax = next ARENA
	cmp	[edi].Sig,'Z'			;Q: Is it end of ARENA chain?
	je	short IUexit			; Y: exit

	mov	edi,eax
	jmp	short IUloop

IUexit:
	clc
IUdone:
	pop	ecx
	pop	eax
	pop	esi
	pop	edi
	ret
InUMBs	endp

_TEXT	ends

;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
R_CODE	segment
	assume	cs:R_CODE, ds:_DATA, es:_DATA
;==============================================================================
;==
;==  rINT15hHandler: Traps CTRL-ALT-DEL and returns system to real mode.
;==		     Reports free extedned memory.
;==		     If move block, sets a trap flag so CEMM handles it in
;==	             the protected mode Int 15h handler.
;==
;==
;==  Entry: (Real/Virtual Mode)
;==
;==  Exit:  (Real/Virtual Mode)
;==
;==============================================================================
.8086 ; **NOTE** Some programs (i.e. PC-ANYWHERE) emulate 8086 instructions
      ; and will break if 386 instructions are inserted in this code.*910408*
rINT15hHandler	proc	far
	cli				; in case of pushf/call far
	pushf				; save entry flags
;
;  Move Block
;
	cmp	ah,87h			;Q: Move block?
	je	short rI15MoveBlock	; Y: emulate move block
;
;  Extended Memory Size
;
	cmp	ah,88h			;Q: extended memory size ?
	je	short rI15MemSize	; Y: return memory size
;
;  System Configuration Parameters: Needed due to a Win 3.0 EBIOS bug!!!!
;
	cmp	ah,0C0h			;Q: Configuration sense?
	je	short rI15Config	; Y: check if it's Win 3.0

; Ctrl+Alt+Del keyboard intercept?

	.erre	fTurnOff AND 0FF00h			 ;Q: Ctrl+Alt+Del kbd
	test	byte ptr cs:[genflags+1], fTurnOff SHR 8 ;   intercept?
	jnz	short rI15CAD		; Y: check if shut down now or later

rI15oldHandler:
	popf				; retrieve entry flags
	jmp	cs:[PrevInt15]		; chain to previous handler

rI15CAD:
	.erre	fTurnOff AND 0FF00h				; clr flag for
	and	byte ptr cs:[genflags+1], NOT (fTurnOff SHR 8)	;   next time

	.erre	fTurnOffNOW AND 0FF00h			    ;Q: shutdown b4
	test	byte ptr cs:[genflags+1], fTurnOffNOW SHR 8 ;  chaining on?
	jz	short rI15oldHandler

	call	ShutDown		; Y: turn off, about to reboot

	jmp	short rI15oldHandler	; chain to previous interrrupt

;==============================================================================
;==  Emulate Move Block
;==============================================================================
rI15MoveBlock:
	test	cs:[Current_State],fState_Active;Q: In Virtual mode ?
	jz	short rI15oldHandler		; N: jmp to old handler

	or	cs:[TrapFlags],fI15trap		; Y: trap flag

	popf				; retrieve entry flags
	int	15h			; emulate move block

	push	bp			; save bp
	mov	bp,sp

	push	ax			; save ax
	pushf
	pop	ax			; return flags

	xchg	ax,[bp].INT_FLAGS	; entry flag in AX, return flags on stack

	and	ax,FLAGS_IF		; restore entry IF
	or	[bp].INT_FLAGS,ax

	pop	ax
	pop	bp
	iret

;==============================================================================
;==  Extended memory size
;==============================================================================
rI15MemSize:
	popf				; restore flags

	push	bp			; save bp
	mov	bp,sp

	push	ds

;910408	push	seg R_CODE		; access R_CODE
;910408	pop	ds
	mov	ax,seg R_CODE		; access R_CODE
	mov	ds,ax
	assume	ds:R_CODE

	mov	ax,[ext_rem]		; return remaining extended memory
	and	[bp].INT_FLAGS,NOT FLAGS_CF ; return no carry

	pop	ds
	assume	ds:_DATA
	pop	bp
	iret
;==============================================================================
;==  System Configuration Parameter: Due to Win 3.0 bug
;==
;==  Win 3.0 EBIOS VxD is unable to cope with the XBDA relocated below
;==  itself.  The problem is that the Win 3.0 EBIOS will declare the 4K page in
;==  which the XBDA resides as PRIVATE, thus preventing instance data within the
;==  page.  Unfortuantely, DOS loads FILES and BUFFERS in memory after the last
;==  installable device driver.  If the last installable device driver is CEMM,
;==  the EBIOS driver will prevent Windows from instancing the FILES and BUFFERS
;==  data.  This handler fixes the problem by telling the Win 3.0 EBIOS VxD that
;==  no XBDA exists if it has relocated it to low memory.  Since low memory is
;==  global, by default, the XBDA is treated correctly by Win 3.0.
;==
;==============================================================================
.386p
rI15Config:
	test	cs:[GenFlags],fWin30	;Q: Win 3.0 broadcast occurred?
	jz	short rI15oldHandler	; N: jmp to old handler

	test	cs:[GenFlags],fXBDAnotRel	;Q: XBDA not relocated?
	jnz	short rI15oldHandler		; Y: jmp to old handler

	call	cs:[PrevInt15]		; chain to previous handler
	pushf
	push	eax
	push	si
	cmp	es:[bx].SDVcount,8	;Q: Is SysDescVec larger than buffer size
	ja	short rI15Cexit		; Y: don't mess with it
	lea	si,[SysDescVec]		; N: copy to local buffer
;
;  Copy System Descrition Vector
;
	mov	eax,es:[bx]
	mov	cs:[si],eax
	mov	eax,es:[bx][4]
	mov	cs:[si][4],eax
	mov	ax,es:[bx][8]
	mov	cs:[si][8],ax
;
;  Change ES:BX and reset XBDA flag
;
	mov	bx,si
	mov	si,cs
	mov	es,si
	and	es:[bx].SDVInfoFlgs,not SDVInfoXBDA

rI15Cexit:
	pop	si
	pop	eax
	popf
	retf	2

rINT15hHandler	endp
.386p
;==============================================================================
;==
;==  rINT19hHandler: Return to real mode and clear int 67 vector.
;==
;==
;==  Entry: (Real/Virtual Mode)
;==
;==  Exit:  (Real/Virtual Mode)
;==
;==============================================================================
rINT19hHandler	proc	far

	cli

	test	cs:[Current_State],fState_Active ;Q: in virtual mode?
	jz	short rI19OldHandler		 ; N: chain to previous handler

	and	cs:[Current_State], not fState_Active
					; clear our flag so that we pass on
					; in our int 13 hook. Note that we
					; we are always behind smartdrive's
					; int 13 hook.
	call	ShutDown

ifdef 901004

	or	cs:[Current_State],fState_WinCntrl
					; we shall set this flag so that set
					; and get a20cnt routines in util.asm
					; do not do xms calls. We do not care
					; about resetting this bit as we are
					; going to reboot any way.

	push	ax			; virtual to real mode
	mov	al,0Fh
	out	84h,al			; port 84/85 return to real sequence
	mov	al,00h
	out	85h,al
	jmp	$+2			; clear prefetch/avoid race cond
	pop	ax
endif
rI19oldHandler:
	xor	eax,eax
	mov	es, ax
	assume	es:ABS0			; clear int 67h vector
	mov	dword ptr es:[int67], eax

	jmp	cs:[PrevInt19]		; chain to previous handler

rINT19hHandler	endp

;==============================================================================
;
;	Control is obtained here if a ctrl-alt-del or a ctrl-alt-ins (on a
;	Zenith is detected). See the protected mode int 15 handler for more
;	details. At this point the int 15 ah=4f call has been processed and
;	the stack is right for us to return to the caller. 
;
;	We now check to see 
;
;		if the carry is clear. If so somebode along the chain wants
; 		the BIOS to ignore the keystroke and hence the BIOS is not 
;		going to reboot. Hence we don't turn off.
;
;		else if the scan code in al has changed. If so somebody 
;		along the chain has effectively changed the keystroke and
;		so the BIOS will not reboot. Hence we don't turn off.
;
;	NOTE: If some one changes the scan code to INS we'll still turn off
;	as we don not check to see if this is a Zenith machine here. We could
;	if we wanted with a few bytes of code. Since a lot of machines use 
;	ctrl-alt-ins to bring up their ROM BIOS setup we do not excpect that 
;	a program will change the DEL to an INS in order to avoid rebooting.
;	
;==============================================================================
.8086

public	rI15KeyBoard
rI15KeyBoard:

	jnc	rI15ret			; if nc return.

	pushf
	cmp	al, SC_DEL		; Q: scan code still del
	je	rI15off			; Y: turn off
	cmp	al, SC_INS		; Q: scan code still ins
	jne	rI15quit		; N: just return
rI15off:
	call	ShutDown		; N: turn ourselves off
rI15quit:
	popf
rI15ret:
	retf	2

R_CODE	ends
	end

