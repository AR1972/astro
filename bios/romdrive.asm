	page	,132
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
	TITLE	ROMDRIVE

break	macro	arg
	subttl	arg		; bogus macro for include files
	endm

;	adapted from RAMDRIVE.SYS from MS-DOS 5.0


.xlist
	include devsym.inc
	include	biosseg.inc
.list

	include	msgroup.inc
SUBTTL	<I/O Packet offset declarations>

;
; Define I/O packet offsets for useful values.
;
; SEE ALSO
;	MS-DOS Technical Reference manual section on Installable Device Drivers
;

; READ/WRITE PACKET OFFSETS
RW_COUNT	EQU	WORD PTR (SIZE SRHEAD) + 5
RW_TRANS	EQU	DWORD PTR (SIZE SRHEAD) + 1
RW_START	EQU	WORD PTR (SIZE SRHEAD) + 7

; MEDIA CHECK PACKET OFFSETS
MCH_RETVAL	EQU	BYTE PTR (SIZE SRHEAD) + 1
MCH_MEDIA	EQU	BYTE PTR (SIZE SRHEAD) + 0

; BUILD BPB PACKET OFFSETS
BPB_BUFFER	EQU	DWORD PTR (SIZE SRHEAD) + 1
BPB_MEDIA	EQU	BYTE PTR (SIZE SRHEAD) + 0
BPB_BPB 	EQU	DWORD PTR (SIZE SRHEAD) + 5



SUBTTL	<Device header>


ASSUME	CS:bios_data,DS:NOTHING,ES:NOTHING,SS:NOTHING

	public	ptrsavx
	public	devexit
	public	rombase
	public	ssize,seclim,rdrivebpb
	public	bpb_rd_length
	extrn	xfer_from_rom:near
	extrn	ROM$INIT:near

	public	rdrive
rdrive:
ROMDEV	LABEL	WORD
	DW	-1,-1
DEVATS	DW	DEVOPCL
	DW	STRATEGY
	DW	ROM$IN
	DB	1			;1 ROMDRIVE


SUBTTL	<Command dispatch table>

;**
;
; This is the device driver command dispatch table.
;
;

ROMTBL	LABEL	WORD
	DB	15			; Max allowed command code
	DW	ROM$INIT
	DW	MEDIA$CHK
	DW	GET$BPB
	DW	CMDERR
	DW	ROM$READ
	DW	DEVEXIT
	DW	DEVEXIT
	DW	DEVEXIT
	DW	ROM$WRT	; writing not supported, read only
	DW	ROM$WRT	; writing not supported, read only
	DW	DEVEXIT
	DW	DEVEXIT
	DW	DEVEXIT
	DW	DEVEXIT
	DW	DEVEXIT
	DW	ROM$REM


RDRIVEBPB:
SSIZE	DW	512		; Physical sector size in bytes
CSIZE	DB	0		; Sectors/allocation unit
RESSEC	DW	1		; Reserved sectors for DOS
FATNUM	DB	1		; No. allocation tables
DIRNUM	DW	64		; Number directory entries
SECLIM	DW	0		; Number sectors
	DB	0F8H		; Media descriptor
FATSEC	DW	1		; Number of FAT sectors
	DW	1		; Number of sectors per track
	DW	1		; Number of heads
	DW	0		; Number of hidden sectors

;	now allocate enough extra bytes so that we can guarantee that
;	  the amount we read is always divisible by 4.  This allows
;	  actual ROM interface modules to utilize DWORD moves without
;	  a bunch of extra effort.

	db	((offset rdrivebpb - offset $) and 3) dup (0)

bpb_rd_length = (offset $) - (offset rdrivebpb)


SUBTTL	<Common Device code>

rombase		dd	0

;	ROMDRIVE DEVICE ENTRY POINTS - STRATEGY, ROM$IN

ptrsavx		DD	0		; Storage location for packet addr

;**	STRATEGY - Device strategy routine
;

STRATP	PROC	FAR

STRATEGY:
	MOV	WORD PTR [ptrsavx],BX	; Save packet addr
	MOV	WORD PTR [ptrsavx+2],ES
	RET

STRATP	ENDP

;**	ROM$IN - Device interrupt routine
;
;	ENTRY	ptrsavx has packet address saved by previous STRATEGY call.
;	EXIT	Dispatch to appropriate function handler
;			CX = Packet RW_COUNT
;			DX = Packet RW_START
;			ES:DI = Packet RW_TRANS
;			DS = bios_data
;			STACK has saved values of all regs but FLAGS
;		    All function handlers must return through one of
;			the standard exit points
;	USES	FLAGS
;

ROM$IN:
	PUSH	SI
	PUSH	AX
	PUSH	CX
	PUSH	DX
	PUSH	DI
	PUSH	BP
	PUSH	DS
	PUSH	ES
	PUSH	BX

	LDS	BX,[ptrsavx]	       ;GET POINTER TO I/O PACKET
    ;
    ; Set up registers for READ or WRITE since this is the most common case
    ;
	MOV	CX,DS:[BX.RW_COUNT]	;CX = COUNT
	MOV	DX,DS:[BX.RW_START]	;DX = START SECTOR
	MOV	AL,DS:[BX.REQFUNC]	; Command code
	MOV	AH,BYTE PTR [ROMTBL]	; Valid range
	CMP	AL,AH
	JA	CMDERR			; Out of range command code
	CBW				; Make command code a word
	mov	si,ax			; get to indexable register
	add	si,si
	les	di,ds:[bx.RW_TRANS]	; es:di -> transfer address
	jmp	cs:word ptr ROMTBL+1[si] ; jump to routine


;**	EXIT - ALL ROUTINES RETURN THROUGH ONE OF THESE PATHS
;
;	Exit code entry points:
;
;


ROM$WRT:
	mov	al,0			; write protect error
	jmp	short err$exit
CMDERR:
	MOV	AL,3			;UNKNOWN COMMAND ERROR
	JMP	SHORT ERR$EXIT

ERR$CNT:
	LDS	BX,[ptrsavx]
	MOV	[BX.RW_COUNT],0 	; NO sectors transferred
ERR$EXIT:				; Error in AL
	MOV	AH,(STERR + STDON) SHR 8  ;MARK ERROR RETURN
	JMP	SHORT ERR1

EXITP	PROC	FAR

DEVEXIT:
	MOV    AH,STDON SHR 8
ERR1:
	LDS	BX,[ptrsavx]
	MOV	[BX.REQSTAT],AX 	; Set return status

	POP	BX
	POP	ES
	POP	DS
	POP	BP
	POP	DI
	POP	DX
	POP	CX
	POP	AX
	POP	SI
	RET				;RESTORE REGS AND RETURN
EXITP	ENDP


;**	MEDIA$CHK - Device Driver Media check routine
;
;	ROMDRIVE Media check routine. ALWAYS returns media not changed
;
;	ENTRY from ROM$IN
;	EXIT through DEVEXIT
;	USES DS,BX
;

MEDIA$CHK:
	LDS	BX,[ptrsavx]
	MOV	[BX.MCH_RETVAL],1	; ALWAYS NOT CHANGED
	JMP	DEVEXIT

;**	GET$BPB - Device Driver Build BPB routine
;
;	ROMDRIVE Build BPB routine. Returns pointer to BPB at RDRIVEBPB
;
;	EXIT through DEVEXIT
;	USES DS,BX

GET$BPB:
	LDS	BX,[ptrsavx]
	MOV	WORD PTR [BX.BPB_BPB],OFFSET RDRIVEBPB
	MOV	WORD PTR [BX.BPB_BPB + 2],CS
	JMP	DEVEXIT

;**	ROM$REM - Device Driver Removable Media routine
;
;	ROMDRIVE Removable Media routine. ALWAYS returns media not removable
;	NOTE: This routine is never called if running on DOS 2.X
;
;	ENTRY from ROM$IN
;	EXIT through ERR1
;	USES AX
;

ROM$REM:
	MOV	AX,STBUI + STDON	; Media NOT removable
	JMP	ERR1

;**	ROM$READ - Device Driver READ routine
;
;	ROMDRIVE READ routine. Perform device READ.
;
;		ES:DI is transfer address
;		CX is sector transfer count
;		DX is start sector number
;	EXIT through DEVEXIT or ERR$CNT
;	USES ALL
;

ROM$READ:
	jcxz	devexit		; zero sectors, read okay.

	xchg	ax,dx		; get start sector
	mul	ssize		;  now dx:ax is the start of block
	push	dx		; save dx for now
	mov	bx,ax		; get low 16 bits into correct register
	mov	ax,cx		; get sector count
	mul	ssize		; get byte count
	or	dx,dx		; if overflow, use 64k
	jz	romread1

	xor	ax,ax		; 0 means 64K

romread1:
	mov	cx,ax		; pass count in cx
	pop	dx		; dx:bx is sector address
	call	xfer_from_rom	; es:di is DMA address
	jnc	devexit
	JMP	ERR$CNT


bios_data ENDS
	END
