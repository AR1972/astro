;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;========================================================
COMMENT #

	DEVICE.ASM


	=================================================
	Device driver to activate the version table in
	MS-DOS 6.0. Upon initialization the driver will
	set the DWORD PTR in the DOS data area at offset
	05dh to point to the version table in the device
	driver and also calculates the minimum install
	size needed to include only the valid entries
	in the default version table.


	================================================

	johnhe - 12/30/90

END COMMENT #

; =======================================================

INCLUDE		VERSIONA.INC

CMD		EQU	2		; Command field offset in packet
STATUS		EQU	3		; Return status field offset
DEV_LEN		EQU	14		; Device length field offset

DOS_TABLE	EQU	5dh		; Lie table ptr offset in dos data
TABLE_LEN	EQU	2048		; Max size of lie table

; ====================================================================

A_DEVICE SEGMENT BYTE PUBLIC 'CODE'
	ASSUME	CS:A_DEVICE, DS:NOTHING, ES:NOTHING

PUBLIC	ENTRY
PUBLIC	DeviceInit

; ====================================================================


DeviceHeader:
		dw	-1,-1
		dw	1000000000000000b; Device attributes (character device)
		dw	Strategy	; Device strategy entry offset
		dw	Entry		; Device entry offset
		db	'SETVERXX'
ExtendedHeader:				; Extended header is used by the
					; SETVER.EXE program to determine
					; where the version table is located
					; within the .EXE file
VerMinor	db	0		; Version 1.0 of Setver
VerMajor	db	1		;
TableOffset	dd	OFFSET VerList	; Offset of table from device start
TableLength	dw	TABLE_LEN	; Max table size
PtrSave	dd	(?)			; Address of device packet

; ====================================================================

; ====================================================================

StratProc PROC	FAR

Strategy:
	mov	WORD PTR CS:[PtrSave],BX	; Save device packet for
	mov	WORD PTR CS:[PtrSave][2],ES	; use on call to dev entry
	ret

StratProc	ENDP	
	
; ====================================================================

; ====================================================================

Entry PROC FAR				; Device driver entry location
	push	BX
	push	DS
	
	lds	BX,[PtrSave]		; DS:BX --> Cmd structure
	mov	AL,DS:[BX].CMD		; AL == command from sysinit
	cbw
	or	AX,AX			; Check for init function zero
	jnz	CmdError		; If not init then error
	jmp	DeviceInit		; Jmp to initialize device

CmdError:
	mov	AL,3			; Return invalid function code
	mov	AH,10000001b		; Signal error in AH

SetStatus:
	mov	[BX].Status,AX		 ; Copy status to packet

	pop	DS
	pop	BX
	ret

Entry ENDP

; ====================================================================
; ====================================================================

SIG	db	'PCMN'
TblLen	dw	TABLE_LEN

; ====================================================================
; ====================================================================

	public	VerList
                                                ; B# refer to MSDOS 6 bug database
VerList	db	10,"KERNEL.EXE"         ,5,00   ; GeoWorks Pro 1.2 B#1344
        db      8, "NETX.COM"           ,5,00   ; NOVELL netx.com 3.26 B#707
        db      8, "NETX.EXE"           ,5,00   ; Novell Redir 3.31 B#4243
        db      8, "NET5.COM"           ,5,00   ; Microsoft Device Library
	db      9, "BNETX.COM"          ,5,00   ; Novell Burst Mode Redir B#3843
        db      9, "BNETX.EXE"          ,5,00   ; Novell Burst Mode Redir B#4377
        db      11,"EMSNETX.EXE"        ,5,00   ; Novell EMS Redir B#4775
        db      11,"EMSNET5.EXE"        ,5,00   ; Novell EMS Redir B#4775
        db      11,"XMSNETX.EXE"        ,5,00   ; Novell XMS Redir B#4775
        db      11,"XMSNET5.EXE"        ,5,00   ; Novell XMS Redir B#4775
        db      10,"DOSOAD.SYS"         ,5,00   ; Iomega bernoulli B#2223
        db      11,"REDIR50.EXE"        ,5,00   ; IBM PLCP 1.34    B#1459
        db      10,"REDIR5.EXE"         ,5,00   ; PATHWORKS 4.1    B#1646
        db      12,"REDIRALL.EXE"       ,5,00   ; Banyan Vines 4.115 B#1347
        db      12,"REDIRNP4.EXE"       ,5,00   ; Banyan redirector B#3283
        db      9, "EDLIN.EXE"          ,5,00   ; Microsoft Device Library
IFNDEF OEMBASE
        db      10,"BACKUP.EXE"         ,5,00   ; Microsoft Device Library
ENDIF
        db      10,"ASSIGN.COM"         ,5,00   ; Microsoft Device Library
        db      11,"EXE2BIN.EXE"        ,5,00   ; Microsoft Device Library
        db      8, "JOIN.EXE"           ,5,00   ; Microsoft Device Library
        db      11,"RECOVER.EXE"        ,5,00   ; Microsoft Device Library
        db      12,"GRAFTABL.COM"       ,5,00   ; Microsoft Device Library
        db      11,"LMSETUP.EXE"        ,5,00   ; LanMan 2.1 Setup B#969
        db      11,"STACKER.COM"        ,5,00   ; Stacker B#2338
        db      10,"NCACHE.EXE"         ,5,00   ; Norton Utilities B#1329
        db      11,"NCACHE2.EXE"        ,5,00   ; Norton Utilities B#3819
        db      12,"IBMCACHE.SYS"       ,5,00   ; B#2228
        db      11,"XTRADRV.SYS"        ,5,00   ; XtraDrive compression B#2371
        db      8, "2XON.COM"           ,5,00   ; Superstor floppy reader B#5272
	db	11,"WINWORD.EXE"	,4,10	; winword 1.0
	db	9, "EXCEL.EXE"		,4,10	; excel 2.x
        db      7, "LL3.EXE"            ,4,01   ; LapLink 3
        db      10,"REDIR4.EXE"         ,4,00   ; IBM PCLP
        db      11,"REDIR40.EXE"        ,4,00   ; IBM DLR
	db      11,"MSREDIR.EXE"        ,4,00   ; 3Com 3+Share
        db      10,"WIN200.BIN" 	,3,40  	; windows 2.x
	db      9, "METRO.EXE"          ,3,31   ; Lotus Metro

	db	(TABLE_LEN - ($ - VerList)) dup (0)
	db	0


; ====================================================================
; Device initialization function first determines minimum size the
; driver needs to be and then sets the DWORD PTR in the DOS data area
; to the location of the version table.
; ====================================================================

DeviceInit:

	push	BX
	push	CX
	mov	AH,30h			; Get version
	int	21h
	pop	CX
	pop	BX

	cmp	AX,expected_version
	je	SetupScan
	xor	AX,AX			; Set end of device to 0
	jmp	SHORT SetDevEnd
	
SetupScan:
	push	SI
	push	DS
	mov	AX,CS
	mov	DS,AX
	mov	SI, OFFSET VerList	; DS:SI --> Version table

	xor	AX,AX			; Clear high byte of AX
ScanLoop:
	lodsb				; Grab the name length
 	or	AX,AX			; Test for end of the table
	jz	FoundEnd
	inc	AX			; Add 2 bytes for the version number
	inc	AX
	add	SI,AX			; Make SI so it points to next entry
	jmp	SHORT ScanLoop

FoundEnd:
	mov	AX,SI			; AX == Offset of end of table
	inc	AX			; Need 1 zero byte at end of table
	pop	DS
	pop	SI

SetTablePtr:
	push	BX
	push	ES

	push	AX			; Save end of device offset
	mov	AH,52h			; Get the DOS data segment
	int	21h
	pop	AX			; Restore end of device offset to AX

	cli				; Safety measure when altering DOSdata
	mov	WORD PTR ES:[DOS_TABLE], OFFSET VerList ; Offset of lie table
	mov	WORD PTR ES:[DOS_TABLE][2],CS	; Segment of lie table
	sti
	pop	ES
	pop	BX

SetDevEnd:
	mov	WORD PTR DS:[BX].DEV_LEN,AX ; Set end of driver @ end of list
	mov	DS:[BX].DEV_LEN[2],CS	; Set device segment
	mov	AH,00000001b		; Normal status return

	jmp	SetStatus		; End of init code

; ====================================================================

A_DEVICE ENDS

; ====================================================================

	END
