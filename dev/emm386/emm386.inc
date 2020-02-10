ifndef INC_LIST
.xlist
endif
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;	Title:	EMM386 - MICROSOFT Expanded Memory Manager 386
;
;	Module: EMM386.INC - general includes for CEMM
;
;	Version: 2.00
;
;	Date:	May 25,1986
;
;	Author:	Steve Preston
;
;******************************************************************************
;
;	Change Log:
;
;	DATE	 REVISION	Description
;	-------- --------	--------------------------------------------
;	04/25/86 Original
;	06/28/86 0.02		Name change from CEMM386 to CEMM (SBP).
;	05/12/87 2.00		Added CEMM mode equates (SBP).
;	01/12/89 4.00           User abort istall message added. RDV
;
;	02/13/91 M008		Added fCPQ16 & fCPQ16Bit
;
;******************************************************************************
;******************************************************************************
; General equates
;******************************************************************************
	; low word of EFLAGS
FLAGS_CF	equ	0001h	; carry flag
FLAGS_PF	equ	0004h	; parity flag
FLAGS_AF	equ	0010h	; auxillary carry flag
FLAGS_ZF	equ	0040h	; zero flag
FLAGS_SF	equ	0080h	; sign flag
FLAGS_TF	equ	0100h	; trap flag
FLAGS_IF	equ	0200h	; int flag
FLAGS_DF	equ	0400h	; direction flag
FLAGS_OF	equ	0800h	; overflow flag
FLAGS_IOPL	equ	3000h	; IOPL bits
FLAGS_NT	equ	8000h	; nested task flag
	; high word of EFLAGS
FLAGS_RF	equ	0001h	; resume flag
FLAGS_VM	equ	0002h	; virtual mode flag

ROMIDPS2	equ 	0F8h	; machine ID for PS2s
ROMIDISA	equ	0FCh	; machine ID for ISA and EISA machines

;******************************************************************************
; State equates
;******************************************************************************

;
; CEMM State Flags
;
fState_ActiveBit	equ 0	; CEMM is active
fState_pfE000Bit	equ 1	; page frame is E000h
fState_UseHiSysBit	equ 2	; use Hi system RAM (if avail)
fState_CEGAinstBit	equ 3	; COMPAQ EGA in system
fState_CEGAmoveBit	equ 4	; CEGA in system must be moved to C000h
fState_A20EnaBit	equ 5	; A20 line enabled => no wrap
fState_WinCntrlBit	equ 6	; Windows took control of system

fState_Active	equ 	1 shl fState_ActiveBit
fState_pfE000	equ     1 shl fState_pfE000Bit
fState_UseHiSys	equ     1 shl fState_UseHiSysBit
fState_CEGAinst	equ     1 shl fState_CEGAinstBit
fState_CEGAmove	equ     1 shl fState_CEGAmoveBit
fState_A20Ena	equ     1 shl fState_A20EnaBit
fState_WinCntrl	equ     1 shl fState_WinCntrlBit

;
; CEMM Modes
;
MODE_ON		equ	0
MODE_OFF	equ	1
MODE_AUTO	equ	2

;
; Auto mode state info - if any of these bits are set,
;	then AUTO mode implies CEMM should be active.
;
fAuto_Users	equ 00000001b	; users active
fAuto_Weitek	equ 00000010b	; weitek mapping on

;
; Weitek State
;
fWeitek_Inst	equ 00000001b	; Weitek in system
fWeitek_Map	equ 00000010b	; Weitek should be mapped now

;==============================================================================
;==  CEMM General Flags [GenFlags]
;==============================================================================
fEISAbit	equ 0	; Detected an EISA machine
fXMMbit		equ 1	; Detected an XMM on the system
fHMAbit		equ 2	; CEMM owns the HMA area
fP8042bit  	equ 3	; Password 8042 system detected
fP92bit		equ 4	; A20 toggle via port 92h is possible
f386SXbit	equ 5	; COMPAQ 386SX based machine
fNoINTbit	equ 6	; No interrupts allowed in protected mode
fShutDownBit	equ 7	; CPU is shutting down or POST
fAbove16MBit	equ 8	; ISA memory above 16 meg supported
fNoLocUMBbit	equ 9	; No local UMBs in Windows
fDebugActiveBit equ 10	; Supported debugger is loaded
;fCADelBit	equ 11	; An INT 15h AH=4Fh was issued for a Cntrl-Alt-Del
fTurnOffBit	equ 11
fZenithBit	equ 12	; A zenith machine
fTurnOffNOWBit	equ 13	; Turn off before chaining Int 15h for a Ctrl+Alt+Del
fEMMhltBit	equ 14 	; An sti hlt has been executed
fBackfillBit	equ 15  ; Emm386 has back filled base memory
fMCABit		equ 16	; This is an MCA machine
fCPQ16Bit	equ 17	; M008: This is a Compaq Deskpro 386/16 or a 
			; portable 386
fNoEMSIntBit	equ 18	; Do not allow interrupts while EMS functions
fEMSsemBit	equ 19	; EMS re-entrancy semaphore
fNoA20TrapBit	equ 20	; Do not trap for A20 switching
fVir8042Bit	equ 22	; Virtualize keyboard data buffer using IRQ1 (port 60h)


fEISA		equ 	1 shl fEISAbit
fXMM		equ 	1 shl fXMMbit
fHMA		equ 	1 shl fHMAbit
fP8042  	equ 	1 shl fP8042bit
fP92		equ 	1 shl fP92bit
f386SX		equ 	1 shl f386SXbit
fNoINT		equ 	1 shl fNoINTbit
fShutDown	equ 	1 shl fShutDownBit
fAbove16M	equ	1 shl fAbove16MBit
fNoLocUMB	equ 	1 shl fNoLocUMBbit
fDebugActive	equ	1 shl fDebugActiveBit
;fCADel		equ 	1 shl fCADelBit
fTurnOff	equ 	1 shl fTurnOffBit

fZenith		equ 	1 shl fZenithBit	
fTurnOffNOW	equ	1 shl fTurnOffNOWBit
fEMMhlt		equ 	1 shl fEMMhltBit	
fBackfill	equ	1 shl fBAckfillBit
fMCA		equ	1 shl fMCABit
fCPQ16		equ	1 shl fCPQ16Bit		; M008
fNoEMSInt	equ	1 shl fNoEMSIntBit
fEMSsem		equ	1 shl fEMSsemBit
fNoA20Trap	equ	1 shl fNoA20TrapBit
fVir8042	equ	1 shl fVir8042Bit

;==============================================================================
;==  CEMM protected mode trap interrupt
;==============================================================================
ProtTrap	equ	21h
;==============================================================================
;==  CEMM trap flags [TrapFlags]
;==============================================================================
fI13trapBit 	 equ 	0      	; trap in protected mode INT 4Bh handler
fI15trapBit	 equ	1	; trap in protected mode Move Block
fI4BtrapBit 	 equ 	2      	; trap in protected mode INT 4Bh handler
fI67trapBit	 equ	3	; trap in protected mode INT 67h handler
fI67noReflectBit equ	4	; INT 67h not reflected
fIntEndBit	 equ	5	; trap for end of interrupt
fXMMtrapBit	 equ	6	; trap for XMS service
fWinTrapBit	 equ	7	; trap for Windows EMM Global Import service
fWin30Bit	 equ	8	; trap for Win 3.0 XBDA sense
fXBDAnotRelBit	 equ	9	; XBDA not Relocated
fSetInstPtrBit	 equ	10
fpModeDebInitBit equ	11	; trap for protected mode Debugger Init

fI13trap	equ 1 shl fI13trapBit
fI15trap	equ 1 shl fI15trapBit
fI4Btrap	equ 1 shl fI4BtrapBit
fI67trap	equ 1 shl fI67trapBit
fI67noReflect	equ 1 shl fI67noReflectBit
fIntEnd		equ 1 shl fIntEndBit
fXMMtrap	equ 1 shl fXMMtrapBit
fWinTrap	equ 1 shl fWinTrapBit
fWin30		equ 1 shl fWin30Bit
fXBDAnotRel	equ 1 shl fXBDAnotRelBit
fSetInstPtr	equ 1 shl fSetInstPtrBit
fpModeDebInit	equ 1 shl fpModeDebInitBit
;==============================================================================
;==  Protected Mode Stack Size
;==============================================================================
Stack0Size	equ	400h	; dword entries in protected mode stack
;
; Equip status bits for high word of EAX
;
fI11h_Weitek_Inst	equ 0000000100000000b ; Weitek installed
fI11h_Weitek_Map	equ 0000000010000000b ; Weitek mapped

;******************************************************************************
; Equates for Port 84h/85h CEMM functions
;******************************************************************************
CEMM_85_Value	equ	02h	; Major # for CEMM
CEMM_84_Weitek	equ	00h	;	Minor # for Weitek
CEMM_84_Diag	equ	01h	;       Minor # for Diagnostics

RR85_Value	equ	00h	; Major # for System ROM
RR84_Value	equ	0Fh	;	Minor for Return to Real code

;******************************************************************************
;   Real/Virtual Mode Routine Id
;******************************************************************************
FIRST_RMODE_RTN equ	100h	; must be > 0FFh

QUERY_XMS_PAGES equ	100h	; query free XMS usable as EMS
ALLOC_XMS_BLOCK equ	101h	; allocate XMS block
FREE_XMS_BLOCK	equ	102h	; free XMS block

LAST_RMODE_RTN	equ	FREE_XMS_BLOCK

;******************************************************************************
;   Init equates
;******************************************************************************
;==============================================================================
;==  Message Flag Equates
;==============================================================================
OVERLAP_MSG	equ 000000000000000000000001b; User specified ranges overlap
INC_DOS_MSG	equ 000000000000000000000010b; Incorrect DOS message
MEM_ERR_MSG	equ 000000000000000000000100b; Memory error message
INS_ERR_MSG	equ 000000000000000000001000b; Already Installed message
NO_PF_MSG	equ 000000000000000000010000b; Nowhere to put PF message
SIZE_ADJ_MSG	equ 000000000000000000100000b; EMM Pool size adjusted message
BASE_ADJ_MSG	equ 000000000000000001000000b; Page Frame Base adjusted msg
MAPA_ADJ_MSG	equ 000000000000000010000000b; Map register address adjusted msg
INV_PARM_MSG	equ 000000000000000100000000b; Invalid parameter message
PF_WARN_MSG	equ 000000000000001000000000b; PF located over ROM warning message
W_NI_MSG	equ 000000000000010000000000b; Weitek Not Installed msg
NO_XMM_MSG	equ 000000000000100000000000b; XMS manager not installed
BAD_XMM_MSG	equ 000000000001000000000000b; Possible bad version of HIMEM
W_NO_HMA_MSG	equ 000000000010000000000000b; HMA not available for WEITEK
NO_LIM_PF_MSG 	equ 000000000100000000000000b; Page frame is not LIM 3.2 compatible
OTHER_EMM_MSG	equ 000000001000000000000000b; Other EMM installed message
HMAon_MSG       equ 000000010000000000000000b; HMAon is invalid on this machine
UMBmem_MSG      equ 000000100000000000000000b; No more memory for UMBs
UserAbort_MSG	equ 000001000000000000000000b; User requested abort
;
;  These messages cause CEMM to not install
;
KILLER_MSG=INC_DOS_MSG+MEM_ERR_MSG+INS_ERR_MSG+NO_XMM_MSG+OTHER_EMM_MSG+UserAbort_MSG
;KILLER_MSG=INC_DOS_MSG+MEM_ERR_MSG+INS_ERR_MSG+NO_XMM_MSG+OTHER_EMM_MSG+NO_PF_MSG+UserAbort_MSG
;KLLR_MSG	EQU	NO_PF_MSG	; If in XMA2EMS mode, install anyway
;

;
;*****************************************************************************
; Structure for obtaining device parameters
;*****************************************************************************

devparmbuf struc

SFn		db	0 	; special functions
DevTy 		db 	0 	; device type
DevAtt 		dw 	0 	; device attributes
NumCyl 		dw 	0 	; number of cylinders
MedTy 		db 	0 	; media type
 ; start of BPB
BPS		dw 	0 	; bytes per sector
SPA 		db 	0 	; sectors per allocation unit
RS 		dw 	0 	; reserved sectors
NF 		db 	0 	; number of FATs
NRD 		dw 	0 	; # of root directories
SinL 		dw 	0 	; # of sectors in logical image or 0
MD 		db 	0 	; media descriptor
NFsec 		dw 	0 	; # of FAT sectors
SPT 		dw 	0 	; # of sectors per track
Heads 		dw 	0 	; # of heads
HS 		dd 	0 	; # of hidden sectors
LS 		dd 	0 	; # of logical sectors

devparmbuf ends


;*****************************************************************************
; Structure for accessing PUSHAD data on stack
;*****************************************************************************

;  Format of PUSHAD stack frame

Pushad_Struc	STRUC
Pushad_EDI	dd	?		; Client's EDI
Pushad_ESI	dd	?		; Client's ESI
Pushad_EBP	dd	?		; Client's EBP
Pushad_ESP	dd	?		; ESP at pushall
Pushad_EBX	dd	?		; Client's EBX
Pushad_EDX	dd	?		; Client's EDX
Pushad_ECX	dd	?		; Client's ECX
Pushad_EAX	dd	?		; Client's EAX
Pushad_Struc	ENDS


;
;******************************************************************************
; MACROS
;******************************************************************************
;

;
;  Protected Mode FAR CALL
;	sel = segment to call into
;	offset = offset/routine to call
PCall	MACRO	sel,off
	db	09Ah		; far call
	dw	offset &off
	dw	&sel
	ENDM

;
;  Protected Mode FAR JUMP
;	sel = segment to call into
;	offset = offset/routine to call
PJmp	MACRO	sel,off
	db	0EAh		; far call
	dw	offset &off
	dw	&sel
	ENDM
;
;	Call_CEMM_PMF - call CEMM protected mode function
;
;	ENTRY:	Virtual mode
;		AL = CEMM PMF minor code
;
;	EXIT:	Depends on call
;
;	USED: AL
;
Call_CEMM_PMF	MACRO	minor_code
	mov	al,minor_code
	out	84h,al			; CEMM PMF Minor code
	mov	al,CEMM_85_Value
	out	85h,al			; CEMM Major code => exec function
	ENDM

REP32MOVSW	MACRO
	db	067h
rep	movsw
	db	067h
	nop				; B1 stepping 386 processor errata
	ENDM


.list	; end of EMM386.inc
