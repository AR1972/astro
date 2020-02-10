;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;	SCCSID = @(#)forproc.asm	1.2 85/07/25

.xlist
.xcref
BREAK	MACRO	subtitle
	SUBTTL	subtitle
	PAGE
ENDM

	include bpb.inc
	INCLUDE FORCHNG.INC
	INCLUDE SYSCALL.INC
	INCLUDE FOREQU.INC
	INCLUDE FORMACRO.INC
	INCLUDE FORSWTCH.INC
	INCLUDE IOCTL.INC
.cref
.list
data	segment public para 'DATA'
data	ends

code	segment public para 'CODE'
	assume	cs:code,ds:data

	PUBLIC	FormatAnother?,Yes?,REPORT,USER_STRING
	public	fdsksiz,badsiz,syssiz,datasiz,biosiz
	public	AllocSize,AllocNum

	extrn	std_printf:near,crlf:near,PrintString:near
	extrn	Multiply_32_Bits:near
	extrn	AddToSystemSize:near

;No more SAFE module
;	EXTRN	UpdateSystemSize:NEAR

data	segment public	para	'DATA'
	extrn	driveLetter:byte
	extrn	msgInsertDisk:byte
	extrn	msgFormatAnother?:byte
	extrn	msgQuickFormatAnother?:byte
	extrn	msgTotalDiskSpace:byte
	extrn	msgSystemSpace:byte
	extrn	msgBadSpace:byte
	extrn	msgDataSpace:byte
	extrn	Read_Write_Relative:byte
	extrn	msgAllocSize:byte
	extrn	MsgAllocNum:Byte
	extrn	deviceParameters:byte
	extrn	bios:byte
	extrn	dos:byte
	extrn	command:byte
IFDEF DBLSPACE_HOOKS
	extrn	DblSpaceBin:byte
ENDIF
	extrn	Serial_Num_Low:Word
	extrn	Serial_Num_High:Word
	extrn	msgSerialNumber:Byte
	extrn	SwitchMap:Word
	extrn	SwitchCopy:Word
	extrn	inbuff:byte


fdsksiz dd	0

syssiz	dd	0
biosiz	dd	0

badsiz	dd	0

datasiz dd	0

AllocSize dd	0
AllocNum dw	0
	dw	offset driveLetter
data	ends

;***************************************************************************
; Wait for key. If yes return carry clear, else no. Insures
;   explicit Y or N answer.
;***************************************************************************

FormatAnother? proc near

	test	SwitchCopy,SWITCH_Q		;use different message with /Q
	jz	@F
	Message msgQuickFormatAnother?
	jmp	SHORT CheckResponse

@@:
	Message msgFormatAnother?

CheckResponse:
	CALL	Yes?
        pushf                                   ; save result
        call    CrLf                            ; send a new line
        popf                                    ; retrieve the result
	jnc	WAIT20
        jz      Wait20
	JMP	SHORT FormatAnother?
WAIT20:
	RET
FormatAnother? endp

;***************************************************************************
;Routine name:Yes?
;***************************************************************************
;
;Description: Validate that input is valid Y/N for the country dependent info
;	      Wait for key. If YES return carry clear,else carry set.
;	      If carry is set, Z is set if explicit NO, else key was not Yes or No.
;
;Called Procedures: Message (macro)
;		    User_String
;
;Change History: Created	4/32/87 	MT
;
;Input: None
;
;Output: CY = 0 Yes is entered
;	 CY = 1, Z = No
;	 CY = 1, NZ = other
;
;Psuedocode
;----------
;
;	Get input (CALL USER STRING)
;	IF got character
;	   Check for country dependent Y/N (INT 21h, AX=6523h Get Ext Country)
;	   IF Yes
;	      clc
;	   ELSE (No)
;	      IF No
;		 stc
;		 Set Zero flag
;	      ELSE (Other)
;		 stc
;		 Set NZ
;	      ENDIF
;	   ENDIF
;	ELSE  (nothing entered)
;	   stc
;	   Set NZ flag
;	ENDIF
;	ret
;***************************************************************************

Procedure YES?

	call	User_String		;Get character

	jz	$$IF1			;Got one if returned NZ
	mov	AL,23h			;See if it is Y/N
	mov	dl,[InBuff+2]		;Get character
	DOS_Call GetExtCntry		;Get country info call
	cmp	AX,Found_Yes		;Which one?

	jne	$$IF2			;Got a Yes
	clc				;Clear CY for return

	jmp	SHORT $$EN2		;Not a Yes
$$IF2:
	cmp	AX,Found_No		;Is it No?

	jne	$$IF4			;Yep
	stc				;Set CY for return

	jmp	SHORT $$EN4		;Something else we don't want
$$IF4:
	xor	AL,AL			;Set NZ flag for ret
	cmp	AL,1			; " "	 " "
	stc				;And CY flag for good measure

$$EN4:
$$EN2:

	jmp	SHORT $$EN1		;No char found at all
$$IF1:
	xor	AL,AL			;Set NZ flag for ret
	cmp	AL,1
	stc				;And CY flag for good measure

$$EN1:
	ret

Yes?	endp


;***************************************************************************

USER_STRING:

;***************************************************************************
; Get a string from user. Z is set if user typed no chars (imm CR)
;  We need to flush a second time to get rid of incoming Kanji characters also.

	mov	AX,(STD_CON_INPUT_FLUSH SHL 8) + 0 ; Clean out input
	int	21h
	mov	DX,OFFSET InBuff
	mov	AH,STD_CON_STRING_INPUT
	int	21h
	mov	AX,(STD_CON_INPUT_FLUSH SHL 8) + 0 ; Clean out input
	int	21h
	cmp	byte ptr [InBuff+1],0
	ret

;*********************************************
; Make a status report including the following information:
; Total disk capacity
; Total system area used
; Total bad space allocated
; Total data space available
; Number of allocation units
; Size of allocation units

Report:
	call	crlf

	call	Calc_System_Space		;calc system space
	call	Calc_Total_Addressible_Space	;calc total space

	Message msgTotalDiskSpace
						;call std_printf
	cmp	word ptr SysSiz,0
	jnz	SHOWSYS
	cmp	word ptr SysSiz+2,0
	jz	CHKBAD
ShowSys:
	Message msgSystemSpace
						;CALL	 std_printf
						;Report space used by system
ChkBad:
	cmp	word ptr BadSiz,0
	jnz	ShowBad
	cmp	word ptr BadSiz+2,0
	jz	ShowData
ShowBad:
	Message msgBadSpace
						;call	 std_printf
ShowData:
	mov	CX,word ptr Fdsksiz
	mov	BX,word ptr Fdsksiz+2
	sub	CX,word ptr BadSiz
	sbb	BX,word ptr BadSiz+2
	sub	CX,word ptr SysSiz
	sbb	BX,word ptr SysSiz+2
	mov	word ptr datasiz,CX
	mov	word ptr datasiz+2,BX
	Message msgDataSpace			;call	 std_printf

	call	crlf
	mov	AX,deviceParameters.DP_BPB.BPB_BytesPerSector ;
	mov	CL,deviceParameters.DP_BPB.BPB_SectorsPerCluster ;
	xor	CH,CH
	mul	CX				;Get bytes per alloc

	mov	word ptr AllocSize,AX		;Save allocation size
	mov	word ptr AllocSize+2,DX 	; for message
	Message msgAllocSize			;Print size of cluster
	call	Get_Free_Space				;get disk space

	mov	word ptr AllocNum,BX		;Put result in msg
	Message msgAllocNum			; = cluster/disk
	call	crlf
	test	switchmap, SWITCH_8		;If 8 tracks, don't display
	jnz	NoSerialNumber			;serial number
	Message msgSerialNumber 		;Spit out serial number
	call	crlf

NoSerialNumber:
	ret

;***************************************************************************
;Routine name: Read_Disk
;***************************************************************************
;
;description: Read in data using Generic IOCtl
;
;Called Procedures: None
;
;
;Change History: Created	5/13/87 	MT
;
;Input: AL = Drive number (0=A)
;	DS:BX = Transfer address
;	CX = Number of sectors
;	Read_Write_Relative.Start_Sector_High = Number of sectors high
;	DX = logical sector number low
;
;Output: CY if error
;	 AH = INT 25h error code
;
;Psuedocode
;----------
;	Save registers
;	Setup structure for function call
;	Read the disk (AX=440Dh, CL = 6Fh)
;	Restore registers
;	ret
;***************************************************************************

Procedure Read_Disk

					;This is setup for INT 25h right
					;Change it to Read relative sect
	push	BX			;Save registers
	push	CX
	push	DX
	push	SI
	push	DI
	push	BP
	push	ES
	push	DS
	mov	SI,data
	mov	ES,SI

	assume	ES:data,DS:nothing
					;Get transfer buffer add
	mov	ES:Read_Write_Relative.Buffer_Offset,BX
	mov	BX,DS
	mov	ES:Read_Write_Relative.Buffer_Segment,BX ;Get segment
	mov	BX,data 		;Point DS at parameter list
	mov	DS,BX

	assume	DS:data,ES:data

	mov	Read_Write_Relative.Number_Sectors,CX ;Number of sec to read
	mov	Read_Write_Relative.Start_Sector_Low,DX ;Start sector
	mov	BX,offset Read_Write_Relative
	mov	CX,0ffffh		;Read relative sector
	int	25h			;Do the read
	pop	DX			;Throw away flags on stack
	pop	DS
	pop	ES
	pop	BP
	pop	DI
	pop	SI
	pop	DX			;Restore registers
	pop	CX
	pop	BX
	ret


Read_Disk endp

;***************************************************************************
;Routine name: Write_Disk
;***************************************************************************
;
;description: Write Data using Generic IOCtl
;
;Called Procedures: None
;
;
;Change History: Created	5/13/87 	MT
;
;Input: AL = Drive number (0=A)
;	DS:BX = Transfer address
;	CX = Number of sectors
;	Read_Write_Relative.Start_Sector_High = Number of sectors high
;	DX = logical sector number low
;
;Output: CY if error
;	 AH = INT 26h error code
;
;Psuedocode
;----------
;	Save registers
;	Setup structure for function call
;	Write to disk (AX=440Dh, CL = 4Fh)
;	Restore registers
;	ret
;***************************************************************************

Procedure Write_Disk


					;This is setup for INT 26h right
					;Change it to Read relative sect

	push	BX			;Save registers
	push	CX
	push	DX
	push	SI
	push	DI
	push	BP
	push	ES
	push	DS
	mov	SI,data
	mov	ES,SI

	assume	ES:data, DS:nothing
					;Get transfer buffer add
	mov	ES:Read_Write_Relative.Buffer_Offset,BX
	mov	BX,DS
	mov	ES:Read_Write_Relative.Buffer_Segment,BX ;Get segment
	mov	BX,data 		;Point DS at parameter list
	mov	DS,BX

	assume	DS:data, ES:data

	mov	Read_Write_Relative.Number_Sectors,CX ;Number of sec to write
	mov	Read_Write_Relative.Start_Sector_Low,DX ;Start sector
	mov	BX,offset Read_Write_Relative
	mov	CX,0ffffh		;Write relative sector
	int	26h			;Do the write
	pop	DX			;Throw away flags on stack
	pop	DS
	pop	ES
	pop	BP
	pop	DI
	pop	SI
	pop	DX			;Restore registers
	pop	CX
	pop	BX
	ret

Write_Disk endp

;=========================================================================
; Calc_Total_Addressible_Space	: Calculate the total space that is
;				  addressible on the the disk by DOS.
;
;	Inputs	: none
;
;	Outputs : Fdsksiz - Size in bytes of the disk
;=========================================================================

Procedure Calc_Total_Addressible_Space

	push	AX				;save affected regs
	push	DX
	push	BX

	call	Get_Free_Space			;get free disk space

	push	BX				;save avail. cluster
	push	DX				;save total. cluster

	mov	AX,DX				;get total clusters

	xor	BX,BX				;clear BX
	xor	CX,CX				;clear CX
						;get total sectors
	mov	CL,DeviceParameters.DP_BPB.BPB_SectorsPerCluster
	call	Multiply_32_Bits

	xor	CX,CX				;clear CX
						;get total bytes
	mov	CX,DeviceParameters.DP_BPB.BPB_BytesPerSector
	call	Multiply_32_Bits		; multiply

	mov	word ptr FdskSiz,AX		;save high word
	mov	word ptr FdskSiz+2,BX		;save low word

	pop	DX				;get total clusters
	pop	BX				;get avail clusters

	mov	AX,DX				;get total clusters
	sub	AX,BX				;get bad clusters

	xor	BX,BX				;clear BX
	xor	CX,CX				;clear CX
						;get total sectors
	mov	CL,DeviceParameters.DP_BPB.BPB_SectorsPerCluster
	call	Multiply_32_Bits

	xor	CX,CX				;clear CX
						;get total bytes
	mov	CX,DeviceParameters.DP_BPB.BPB_BytesPerSector
	call	Multiply_32_Bits

	sub	AX,word ptr SysSiz
	sbb	BX,word ptr SysSiz+2		;size

	mov	word ptr BadSiz,AX		;save high word
	mov	word ptr BadSiz+2,BX		;save low word

	pop	BX
	pop	DX				;restore regs
	pop	AX

	ret

Calc_Total_Addressible_Space	endp


;=========================================================================
; Get_Free_Space	: Get the free space on the disk.
;
;	Inputs	: none
;
;	Outputs : BX - Available space in clusters
;		  DX - Total space in clusters
;=========================================================================

Procedure Get_Free_Space

	xor	AX,AX				;clear ax
	mov	AH,36h				;Get disk free space
	mov	DL,DriveLetter			;get drive letter
	sub	DL,"A"				;get 0 based number
	inc	DL				;make it 1 based
	int	21h
	ret

Get_Free_Space	endp

;=========================================================================
; Calc_System_Space	: This routine calculates the space occupied by
;			  the system on the disk.
;
;	Inputs	: DOS.FileSizeInBytes
;		  BIOS.FileSizeInBytes
;		  Command.FileSizeInBytes
;
;	Outputs : SysSiz			- Size of the system
;=========================================================================

Procedure Calc_System_Space

	push	AX					;save regs
	push	DX

	mov	word ptr SysSiz+0,00h			;clear variable
	mov	word ptr SysSiz+2,00h

	mov	AX,word ptr [Dos.FileSizeInBytes+0]	;get low word
	mov	DX,word ptr [Dos.FileSizeInBytes+2]	;get high word
	call	AddToSystemSize 			;add in values

	mov	AX,word ptr [Bios.FileSizeInBytes+0]	;get bios size
	mov	DX,word ptr [Bios.FileSizeInBytes+2]
	call	AddToSystemSize 			;add in values

	mov	AX,word ptr [Command.FileSizeInBytes+0] ;get command size
	mov	DX,word ptr [Command.FileSizeInBytes+2]
	call	AddToSystemSize 			;add in values

IFDEF DBLSPACE_HOOKS
	mov	ax, word ptr [DblSpaceBin.FileSizeInBytes]	;get dblspace
	mov	dx, word ptr [DblSpaceBin.FileSizeInBytes+2]	; size--may be
	call	AddToSystemSize 				; zero
ENDIF

;No more SAFE module
;	call	UpdateSystemSize			;add on restore file size

	pop	DX									;restore regs
	pop	AX
	ret

Calc_System_Space	endp

code	ends
	end

