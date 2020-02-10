;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;===========================================================================
; 
; FILE: DSKFRMT.ASM
;
;===========================================================================

;===========================================================================
;Declaration of include files
;===========================================================================

debug	 equ	 0
	 .xlist
	 INCLUDE VERSION.INC
;	 INCLUDE VERSIONA.INC
	 INCLUDE DOSMAC.INC
	 INCLUDE SYSCALL.INC
	 INCLUDE ERROR.INC
;	 INCLUDE DPB.INC
;	 INCLUDE CPMFCB.INC
;	 INCLUDE DIRENT.INC
;	 INCLUDE CURDIR.INC
;	 INCLUDE PDB.INC
	 INCLUDE BPB.INC
	 INCLUDE FOREQU.INC
	 INCLUDE FORMACRO.INC
	 INCLUDE IOCTL.INC
	 INCLUDE FORSWTCH.INC
;	 INCLUDE SYSVAR.INC
	 .list

;===========================================================================
; Declarations for all publics in other modules used by this module
;===========================================================================

;Bytes
	EXTRN	fBigFat			:BYTE
	EXTRN	ExitStatus		:BYTE
	EXTRN	DblFlg			:BYTE
	EXTRN	Drive			:BYTE
	EXTRN	msgFormatFailure	:BYTE
	EXTRN	msgDiskUnusable 	:BYTE
	EXTRN	msgNotSystemDisk	:BYTE
	EXTRN	msgParametersNotSupported:BYTE
	EXTRN	msgParametersNotSupportedByDrive:BYTE
	EXTRN	msgHardDiskWarning	:BYTE
	EXTRN	msgInsertDisk		:BYTE
	EXTRN	msgCrLf			:BYTE
	EXTRN	msgCurrentTrack		:BYTE
	EXTRN	msgFormatComplete	:BYTE
	EXTRN	msgVerify		:BYTE
	EXTRN	ContinueMsg		:BYTE
	EXTRN	Extended_Error_Msg	:BYTE
	EXTRN	Clustbound_Flag 	:BYTE
	EXTRN	Fatal_Error		:BYTE

;Words
	EXTRN	FirstHead		:WORD
	EXTRN	FirstCylinder		:WORD
	EXTRN	TracksLeft		:DWORD	; M018
	EXTRN	TracksPerDisk		:DWORD	; M018
	EXTRN  	Formatted_Tracks_Low	:WORD
	EXTRN	Formatted_Tracks_High	:WORD
	EXTRN   Paras_Per_Fat		:WORD
	EXTRN	StartSector		:WORD
	EXTRN	SwitchMap		:WORD
	EXTRN	Relative_Sector_High	:WORD
	EXTRN	Relative_Sector_Low 	:WORD
	EXTRN	Clustbound_Buffer_Seg 	:WORD
	EXTRN	Clustbound_Adj_Factor	:WORD
	EXTRN	Clustbound_Spt_Count  	:WORD
	EXTRN	sector_in_buffer	:WORD
	EXTRN	sector_to_read		:WORD
	EXTRN	TotalClusters		:WORD

;Pointers
	EXTRN	SysSiz 			:DWORD
	EXTRN	BioSiz 			:DWORD
	EXTRN	Msg_Allocation_Unit_Val	:DWORD
	EXTRN	FatSpace	  	:DWORD
	EXTRN	FatSector		:DWORD

;Functions
	EXTRN	SetStartSector		:NEAR
	EXTRN	SetfBigFat		:NEAR
	EXTRN	Phase2Initialisation	:NEAR
;	EXTRN	More			:NEAR
	EXTRN	GetBioSize 		:NEAR
	EXTRN	GetDosSize 		:NEAR
	EXTRN	GetCmdSize 		:NEAR
	EXTRN	LastChanceToSaveIt 	:NEAR
	EXTRN	AccessDisk 		:NEAR
	EXTRN	Yes?  			:NEAR
	EXTRN	ExitProgram 		:NEAR
	EXTRN	User_String 		:NEAR
	EXTRN	Read_Disk		:NEAR
	EXTRN	Seg_Adj			:NEAR

;No more SAFE module
;	EXTRN	calc_sector_and_offset	:NEAR
;	EXTRN	ReadFatSector		:NEAR
;	EXTRN	GetFatSectorEntry	:NEAR
;	EXTRN	GetSetFatEntry		:NEAR


;Structures
	EXTRN	DeviceParameters	:BYTE
	EXTRN	FormatPacket		:BYTE
	EXTRN	Read_Write_Relative	:BYTE

;Constants
	EXTRN	EXIT_FATAL		:ABS
	EXTRN	EXIT_NO			:ABS
	EXTRN	EXIT_DRV_NOT_READY	:ABS
	EXTRN	EXIT_WRIT_PROTECT	:ABS
	EXTRN	EXIT_FATAL 		:ABS
	EXTRN	EXIT_NO			:ABS


;===========================================================================
; Data segment
;===========================================================================

DATA    SEGMENT PUBLIC PARA 'DATA'

RWPacket		a_TrackReadWritePacket	<>

RW_TRF_Area		db	512	dup(0)
fLastChance		db	FALSE	; Flags reinvocation from
					; LastChanceToSaveIt. Used by DskPrm
FormatError		db	0
Format_End		db	FALSE
Track_Action		db	?	; Actual operation performed on disk

odd_entry		db	1	; flag for 12-bit FAT entry alignment

entry_offset		dw	?	; Offset of entry from start of sector

PercentComplete 	dw	0FFFFh	; Init non-zero so msg will display
					; first time
Fat_Init_Value		dw	0	; initializes the Fat
SysTrks 		dw	?
Sectors 		dw	?
CurrentHead		dw	0
CurrentCylinder		dw	0
Tracks_To_Format	dw	?
Track_Count		dw	?

CurrentCluster		dw	?	; holds the cluster currently being checked
					; in QuickFormat
BadClusValue		dw	?	; holds FAT entry value for bad cluster

DATA	ENDS

;===========================================================================
; Executable code segment
;===========================================================================

CODE	SEGMENT PUBLIC PARA	'CODE'
	ASSUME	CS:CODE, DS:DATA, ES:DATA

;===========================================================================
;EXTRNs needed in code segment
;===========================================================================

;Labels
;	EXTRN	SysLoop			:NEAR
	EXTRN	FatalExit		:NEAR
	EXTRN	CrLf			:NEAR

;===========================================================================
; Declarations for all publics in this module
;===========================================================================

	PUBLIC	Disk_Format_Proc
	PUBLIC	FrmtProb
	PUBLIC	fLastChance
	PUBLIC	CurrentHead
	PUBLIC	CurrentCylinder
	PUBLIC	Multiply_32_Bits
	PUBLIC	SetDeviceParameters
	PUBLIC	PercentComplete
	PUBLIC	Prompt_User_For_Disk

	PUBLIC	calc_sector_and_offset
	PUBLIC	ReadFatSector
	PUBLIC	GetSetFatEntry
	PUBLIC	GetFatSectorEntry
	PUBLIC	DetermineTrackAction
	PUBLIC	QuickFormat

;========================================================================
;
;  DISK_FORMAT_PROC : 	This procedure is used to call
;			the real DiskFormat procedure.
;  Returns :	NC --> Format successful
;		CY --> Format unsuccessful, go to next disk
;
;========================================================================

Disk_Format_Proc	proc	near

	call	DetermineTrackAction	; Check for safe format, and validity
					; of quick format switch
	push	ES

	call	Fat_Init		; initialize the Fat

	les	DI, FatSpace		; ES:DI --> FatSpace buffer
					; Store media descriptor byte
	mov	AL, DeviceParameters.DP_BPB.BPB_MediaDescriptor
	mov	AH, 0ffH
	stosw
	mov	AX, 00ffH
	test	fBigFat, TRUE
	jz	NotBig
	mov	AX, 0ffffH

NotBig:
	stosw
	pop	ES

	test	SwitchMap,SWITCH_Q	; Check for quick format
	jz	RegularFormat

	call	QuickFormat
	jc	FarExit			; procedure exit is too far for direct jump
	Message	msgFormatComplete
FarExit:
	jmp	Exit_Disk_Format_Proc

RegularFormat:
	call	DiskFormat		; Format the disk
	jnc	GetTrk			; check for problems

FrmtProb:

	test	SwitchMap,Switch_Select	; SELECT option?
	jnz	CheckForMore		; No - display message
	Message msgFormatFailure
	mov	ExitStatus, EXIT_FATAL

CheckForMore:
	stc				; Signal error which will be handled
					; in main routine (by prompting for
					; next disk)
	jmp	Exit_Disk_Format_Proc

		;Mark any bad Sectors in the Fats
		;And keep track of how	many bytes there are in	bad Sectors
GetTrk:
	call	BadSector		; Do bad track fix-up
	jc	FrmtProb		; Had an error in Fmt - can't recover
	CMP	AX,0			; Are we finished?
	jnz	TrkFnd			; No - check error conditions
	jmp	Exit_Disk_Format_Proc	; Yes

;--------------------------------------------------------------------------
; BUG NOTE:  The two sections in "if ibmcopyright..." here and	below are
;   to	correct	a bug.	If one of the Sectors just above the 32M boundary
;   were bad, it thought they were in the system area,	and hence that the
;   disk was unusable.
;
; PYS: IBMCOPYRIGHT removed
;
;--------------------------------------------------------------------------

TrkFnd:
						; Added IBM fix
	mov	BX,Relative_Sector_High		; get high word of the sector
	or	BX,BX				; is it	zero?
	jnz	ClrTest 			; if not, disk is usable

	mov	BX,WORD PTR Relative_Sector_Low ;get the low word of the sector
	cmp	BX,StartSector			; Any sec in the sys area bad?

	jae	ClrTest 			; MZ 2.26 unsigned compare
	Message msgDiskUnusable
	jmp	FrmtProb			; Bad disk -- try again

ClrTest:
	mov	Sectors,AX			; Save # sectors on the track
	test	SwitchMap,SWITCH_S		; If sys requested calc size
	jz	Bad100

	cmp	BYTE PTR DblFlg,0		; Is sys space aready calced?
	jnz	cmpTrks				; Yes - all ready for the compare
	inc	BYTE PTR DblFlg			; No -	set the	flag

	call	GetBioSize			; Get the size	of the Bios
	mov	DX,WORD PTR SysSiz+2
	mov	AX,WORD PTR SysSiz
	mov	WORD PTR BioSiz+2,DX
	mov	WORD PTR BioSiz,AX
	call	GETDOSSIZE
	call	GETCMDSIZE
	mov	DX,WORD PTR BioSiz+2
	mov	AX,WORD PTR BioSiz
	div	DeviceParameters.DP_BPB.BPB_BytesPerSector
	add	AX,StartSector
	mov	SysTrks,AX			; Space Fat,Dir,and system
						; files require
cmpTrks:
						; Added IBM fix
	mov	BX, Relative_Sector_High	; get the high word of the sec
	or	BX, BX				; is it	zero?
	jnz	Bad100				; if not, DIsk is usable

	mov	BX,WORD PTR Relative_Sector_Low ;get the low word of the sector
	cmp	BX,SysTrks
	JA	Bad100				; MZ 2.26 unsigned compare
	mov	ExitStatus, EXIT_FATAL
	Message msgNotSystemDisk
	AND	SwitchMap,NOT SWITCH_S		; Turn off system transfer switch
	mov	WORD PTR SysSiz+2,0		; No system to transfer
	mov	WORD PTR SysSiz,0		; No system to transfer

Bad100:
	cmp	DeviceParameters.DP_DeviceType, DEV_HARDDISK ;hard disk?
	jne	$$IF5				; yes
	call	Get_Bad_Sector_Hard		; see if a sector is bad
	jmp	SHORT $$EN5			;floppy disk

$$IF5:
	call	Get_Bad_Sector_Floppy		; mark entire track bad

$$EN5:
	jmp	GetTrk

Exit_Disk_Format_Proc:
	ret			

Disk_Format_Proc	ENDP

;==========================================================================
;
; DetermineTrackAction  :	This procedure sets the value of 
;				Track_Action based on the setting of
;				SWITCH_U, the unconditional format switch.
;				Track_Action is the function that is
;				actually performed on each track of the
;				disk to be formatted.
;
;  Inputs  :	SwitchMap
;  Output  :    SWITCH_U set   - Track_Action = Format and Verify
;		SWITCH_U clear - Track_Action = Verify only
;
;==========================================================================

DetermineTrackAction	proc	near

	test	SwitchMap,SWITCH_U
	jz	Verify_Only

Format_And_Verify:
	mov	Track_Action,FORMAT_TRACK		; regular format
	ret

Verify_Only:
	mov	Track_Action,VERIFY_TRACK	     	; safe format
	ret
DetermineTrackAction	endp

; =========================================================================
;    DiskFormat:
;	 Format	the tracks on the disk
;	 Since we do our SetDeviceParameters here, we also need	to
;	 detect	the legality of	/N /T if present and abort with	errors
;	 if not.
;	 This routine stops as soon as it encounters a bad track
;	 Then BadSector	is called to report the	bad track, and it continues
;	 the format
;
;    Algorithm:
;	 current track = first
;	 while not done
;	    if format track fails
;	       DiskFormatErrors	= true
;	       return
;	    next track
; =========================================================================

DiskFormat proc near
						; Initialise FatSpace

;	test	SwitchMap, SWITCH_C		; don't bother format if
;	jz	Keep_Going			; /c was given
;	jmp	FormatDone			; FormatDone is	to far away
;Keep_Going:

	mov	DeviceParameters.DP_SpecialFunctions, (INSTALL_FAKE_BPB or TRACKLAYOUT_IS_GOOD)
	lea	DX, DeviceParameters
	call	SetDeviceParameters
;	call	Cluster_Buffer_Allocate	; get room for retry buffer
;	call	Prompt_User_For_Disk
	test	SwitchMap,switch_8	; DCL 5/12/86 avoid Naples AH=18h
	jnz	stdBpB			; lackof support for 8 Secs/track

		; DCL 5/12/86 -	Always do the Status_FOR_FORMAT test, as we
		; don't know if the machine has this support.	For 3.2 /N:
		; & /T:	were not documented & therefore not fully supported
		; thru the ROM of Aquarius & Naples & Royal Palm


		;	test	SwitchMap, SWITCH_N or SWITCH_T ; IF (/N or /T)
		;	jz	StdBPB
		; THEN check if
		; supported

		; Check to see if device driver can handle specified parameters
	mov	FormatPacket.FP_SpecialFunctions, Status_FOR_FORMAT
	mov	AX, (IOCTL shl	8) or GENERIC_IOCTL
	mov	BL, Drive
	inc	BL
	mov	CX, (RAWIO shl	8) or FORMAT_TRACK
	lea	DX, FormatPacket
	int	21h

		;		switch ( FormatStatusCall)
		;	cmp	FormatPacket.FP_SpecialFunctions, \
		;		Format_No_ROM_Support
		;	jb	NTSupported	; 0 returned from IBMBIO
		;	ja	IllegalComb	; 2 returned - ROM Support
		;		Illegal Combination!

	cmp	FormatPacket.FP_SpecialFunctions,0 ; 0 --> Can support
	je	NTSupported
	cmp	FormatPacket.FP_SpecialFunctions,2 ; 2 --> Cannot support
	jne	$$IF28

	Message msgParametersNotSupportedByDrive
	mov	Fatal_Error,Yes			; Indicate quittin'type	err!
	jmp	SHORT $$EN28

$$IF28:
	cmp	FormatPacket.FP_SpecialFunctions,3 ; 3 --> No disk in drive
	jne	$$IF30
	mov	AX,Error_Not_Ready		; flag not ready
	call	CheckError			; set error level
	jmp	FrmtProb			; exit	program
	jmp	SHORT $$EN30			; DCL No ROM support is okay

$$IF30:
						; except for /N: & /T:
	test	SwitchMap, SWITCH_N or SWITCH_T ; DCL 5/12/86
	jz	$$IF32
	Message msgParametersNotSupported
	mov	Fatal_Error,Yes			; Indicate quittin 'type err!

$$IF32:
$$EN30:
$$EN28:
	cmp	Fatal_Error,Yes
	jne	StdBPB
	jmp	FatalExit
						; We have the support to carry
						; out the FORMAT
NTSupported:
StdBPB:
	mov	FormatPacket.FP_SpecialFunctions, 0
	mov	AX, FirstHead
	mov	FormatPacket.FP_Head, AX
	mov	AX, FirstCylinder
	mov	FormatPacket.FP_Cylinder, AX

;M018 - begin
	mov	AX, word ptr TracksPerDisk
	mov	word ptr TracksLeft, AX
	mov	AX, word ptr TracksPerDisk+2
	mov	word ptr TracksLeft+2, AX
;M018 - end

	mov	Format_End,False		; flag not at end of format
	call	Calc_MAX_Tracks_To_Format	; Max track count for
						; FormatTrack call
FormatLoop:
	call	Format_Loop			; Format until CY occurs
	cmp	Format_End,True			; End of Format?
	jne	$$IF36
	mov	FormatError,0			; signal good format
	clc					; clear CY
	jmp	SHORT $$EN36			; bad format

$$IF36:
	call	CheckError			; determine type of error
	jc	$$IF38
	call	LastChanceToSaveIt		; acceptable error?
	jnc	$$IF39				; yes
	mov	FormatError,1			; signal error type
	clc					; clear CY
	jmp	SHORT $$EN39			; not acceptable error

$$IF39:
	call	SetStartSector			; start from scratch
	call	SetfBigFat
	push	AX
	call	Phase2Initialisation
	clc
	pop	AX
	jmp	DiskFormat			; try again

$$EN39:
$$IF38:
$$EN36:
	return

FormatDone:
	mov	FormatError,0
	clc
	return

DiskFormat endp

;=========================================================================
; Fat_INIT:		 This routine initializes the Fat based	on the
;			 number	of paragraphs.
;
; input -  FatSpace
;	   FatSpace+2
;	   paras_per_Fat
;	   Fat_init_value
; output - Fat	space is initialized
;
; Assumes:	DS:DATA
;
;=========================================================================
	PUBLIC Fat_Init
Fat_Init PROC NEAR

	push	ES
	push	DI
	push	AX
	push	BX
	push	CX

	mov	DI, WORD PTR FatSpace+2	; Get segment of Fat space
	mov	ES,DI
	mov	DI, WORD PTR FatSpace
	mov	BX,Paras_Per_Fat	; Get number of paras
	mov	AX,Fat_init_value
	push	DX
	mov	DX,ES			; Grab ES into DX

$$DO87:
	cmp	BX,0			; do while BX not = 0
	je	$$EN87			; exit if 0
	mov	CX,10h			; Word move of paragraph
	rep	stosb			; Move the data to Fat
	xor	DI,DI			; Offset always init to 0
	inc	DX			; Next paragraph
	mov	ES,DX			; Put next para in ES
	dec	BX			; Loop iteration counter
	jmp	SHORT $$DO87

$$EN87:
	pop	DX
	pop	CX
	pop	BX
	pop	AX
	pop	DI
	pop	ES

	ret

Fat_Init ENDP

;=========================================================================
;   SetDeviceParameters:
;	Set the	device parameters
;
;   Input:
;	Drive
;	DS:DX - pointer to device parameters
;=========================================================================

SetDeviceParameters proc near

	 mov	 AX, (IOCTL shl	8) or GENERIC_IOCTL
	 mov	 bl, Drive
	 inc	 bl
	 mov	 CX, (RAWIO shl	8) or SET_DEVICE_PARAMETERS
	 int	 21H
	 return

SetDeviceParameters endp


;=========================================================================
; Prompt_User_For_Disk		 : This	routine	prompts	the user for the
;				   disk	to be formatted.  An appropriate
;				   message is chosen based on the type
;				   of switch entered.  If the /SELECT
;				   switch is entered, the disk prompt is
;				   issued through the int 2fh services
;				   provided by SELECT.
;
;	 Inputs	 : SwitchMap	 - Switches chosen for format
;
;	 Outputs : Message printed as appropriate.
;=========================================================================

Procedure Prompt_User_For_Disk

	push	AX
	test	SwitchMap, (SWITCH_Backup or SWITCH_Select or SWITCH_AUTOTEST)
	jnz	$$IF186
	call    DskPrm				; prompt user for disk

$$IF186:
	 test	SwitchMap, (Switch_Select)	; /SELECT requested?
	 jz	$$IF188
	 mov	AL, Drive			; get drv to access for format
	 call	AccessDisk			; access the disk
	 mov	AX,Select_Disk_Message		; display disk prompt
	 int	2fh				; through int 2fh services

$$IF188:
	 pop	 AX
	 ret
Prompt_User_For_Disk	 ENDP

;==========================================================================
; DiskPrompt:
;
; This routine prompts for the insertion of the correct diskette
; into the Target Drive, UNLESS we are being re-entrantly invoked
; from LastChanceToSaveIt. If the target is a Hardisk we issue a
; warning message.
;
;	 INPUTS:
;		 DeviceParameters.DP_DeviceType
;		 fLastChance
;
;	 OUTPUTS:
;		 Prompt	string
;		 fLastChance	 := FALSE
;
;	 Registers affected:
;				 Flags
;
;==========================================================================

DskPrm PROC NEAR

	cmp	fLastChance,TRUE
	je	PrmptRet

	cmp	DeviceParameters.DP_DeviceType, DEV_HARDDISK
	jne	GoPrnIt
	Message	msgHardDiskWarning
	call	Yes?

	pushf
	Message	msgCrlf
	popf

	jnc	OkToFormatHardDisk
	mov	ExitStatus, EXIT_NO
	jmp	ExitProgram

OkToFormatHardDisk:
	call	CrLf
	call	CrLf
	return

GoPrnIt:
	mov	AL, Drive
	call	AccessDisk
	Message msgInsertDisk
	Message ContinueMsg
	call	USER_STRING			; Wait for any key
	call	CrLf
	call	CrLf

PrmptRet:
	mov	fLastChance, FALSE
	ret

DskPrm	ENDP

;=========================================================================
;    CheckError:
;	 Input:
;	    AX - extended error	code
;	 Ouput:
;	    carry set if error is Fatal
;	    Message printed if Not Ready or Write Protect
;=========================================================================

CheckError proc near
	cmp	AX, error_write_protect
	je	WriteProtectError
	cmp	AX, error_not_ready
	je	NotReadyError
	cmp	CurrentCylinder, 0
	jne	CheckRealErrors
	cmp	CurrentHead, 0
	je	BadTrackZero

CheckRealErrors:
	cmp	AX, error_CRC
	je	JustABadTrack
	cmp	AX, error_sector_not_found
	je	JustABadTrack
	cmp	AX, error_write_fault
	je	JustABadTrack
	cmp	AX, error_read_fault
	je	JustABadTrack
	cmp	AX, error_gen_failure
	je	JustABadTrack

	stc
	ret

JustABadTrack:
	clc
	ret

WriteProtectError:
	test	SwitchMap,Switch_SELECT	; SELECT option?
	jnz	$$IF56				; no - display messages

	Message msgCrLf
	Message msgCrLf
	Extended_Message
	jmp	SHORT $$EN56			; yes - set error level

$$IF56:
	mov	ExitStatus,EXIT_WRIT_PROTECT	; signal write protect error

$$EN56:
	 stc					; signal Fatal error
	 ret					; return to caller

NotReadyError:
	test	SwitchMap,Switch_SELECT		; SELECT option?
	jnz	$$IF59				; no - display messages

	Message msgCrLf
	Message msgCrLf
	Extended_Message
	jmp	SHORT $$EN59			; yes - set error level

$$IF59:
	mov	ExitStatus,EXIT_DRV_NOT_READY	;signal Drive not ready

$$EN59:
	stc
	ret

BadTrackZero:
	Message msgDiskUnusable
	stc
	ret

CheckError endp

;=========================================================================
;
; Calc_MAX_Tracks_To_Format	 : This	routine	determines the maximum
;				   number of tracks to format at 1 time.
;
;	 Inputs	 : DeviceParameters - SectorsPerTrack
;				      BytesPerSector
;
;	 Outputs : Track_Count	    - MAX. # of	tracks to format in 1 call
;				      to FormatTrack
;=========================================================================

Procedure Calc_Max_Tracks_To_Format

	push	AX				; Save regs
	push	BX
	push	DX

	mov	AX,DeviceParameters.DP_BPB.BPB_SectorsPerTrack
	mov	BX,DeviceParameters.DP_BPB.BPB_BytesPerSector
	xor	DX,DX
	mul	BX				; Get total byte count
	mov	BX,AX				; Put count in BX
	mov	AX,MAX_Format_Size		; Max bytes to format
	div	BX				; Get track count
	mov	Track_Count,AX

	pop	DX
	pop	BX
	pop	AX

	ret

Calc_Max_Tracks_To_Format ENDP

;=========================================================================
; Format_Loop			 : This	routine	provides the main template
;				   for the formatting of a disk.  A disk
;				   will	be formatted as	long as	there are
;				   tracks remaining to be formatted.
;				   This	routine	can be exited on a carry
;				   condition; i.e., bad	track, last track, etc.
;
;	 Inputs	 : none
;
;	 Outputs : CY -	Set on exit from this routine
;		   AX -	Possible error condition code
;
;=========================================================================

Procedure Format_Loop

	clc					; Initialize to NC

$$DO173:					; While NC
	jc	$$EN173			   	; Exit on CY
	call	Calc_Current_Head_Cyl		; Head and cylinder calc.
	call	Determine_Format_Type		; Floppy/hard media?
	call	Determine_Track_Count		; How many tracks?
	call	FormatTrack			; Format track(s)
	jnc	$$IF175				; Formattrack failed

	pushf					; Save flags
	cmp	DeviceParameters.DP_DeviceType,Dev_HardDisk ; Harddisk?
	jne	$$IF176

	popf					; Restore flags
	call	Format_Track_Retry		; Find failing track
	jmp	SHORT $$EN176

$$IF176:
	popf					; Restore flags

$$EN176:
$$IF175:
	jnc	$$IF180				; Format error?
	pushf					; Yes - save flags
	push	AX				; Save return code
	call	CheckRealErrors 		; Check error type
	jc	$$IF181				; If non-Fatal
	call	DisplayCurrentTrack		; Display % formatted

$$IF181:
	pop	AX				; Restore regs
	popf

$$IF180:
	jc	$$EN173				; Exit on CY

	call	DisplayCurrentTrack		; Tell how much formatted
	call	Adj_Track_Count 		; Decrease track counter
	call	NextTrack			; Adjust head and cylinder
	jmp	SHORT $$DO173

$$EN173:
	ret

Format_Loop ENDP

;=========================================================================
; Calc_Current_Head_Cyl : Obtain the current head and cylinder	of the
;			   track being formatted.
;
;	 Inputs: FP_Cylinder	 - Cylinder of track being formatted
;		 FP_Head	 - Head	of track being formatted
;=========================================================================

Procedure Calc_Current_Head_Cyl

	push	CX				; save CX
	mov	CX,FormatPacket.FP_Cylinder	; get current cylinder
	mov	CurrentCylinder,CX		; put into variable
	mov	CX,FormatPacket.FP_Head		; get current head
	mov	CurrentHead,CX			; put into variable
	pop	CX				; restore CX
	ret

Calc_Current_Head_Cyl	 endp

; =========================================================================
; Determine_Format_Type :  This	routine	determines the type of format
;			   that	is to occur based on the media type.
;
;	 Inputs	  : Dev_HardDisk		 - Media type (harddisk)
;		    Multi_Track_Format	 - EQU 02h
;		    Single_Track_Format	 - EQU 00h
;
;	 Outputs  : FP_SpecialFunctions	 - Set appropriately for single
;					   or multi track format
; =========================================================================

Procedure Determine_Format_Type

	cmp	 DeviceParameters.DP_DeviceType,Dev_HardDisk	;harddisk?
	jne	 $$IF158
					; Set for multi track format
	mov	 FormatPacket.FP_SpecialFunctions,Multi_Track_Format
	jmp	 SHORT $$EN158

$$IF158:				; Set for single track format
	mov	 FormatPacket.FP_SpecialFunctions,Single_Track_Format

$$EN158:
	ret

Determine_Format_Type ENDP

;=========================================================================
;
; Determine_Track_Count	 : This	routine	determines the number of
;				   tracks to be	formatted, based on whether
;				   or not we have a hard disk.	If we have
;				   a hard disk we can use multi-track
;				   format/verify, otherwise we use the
;				   single track	format/verify.
;
;	 Inputs	 : Device_Type			 - Media type
;
;	 Outputs : Tracks_To_Format		 - MAX.	number of tracks
;						   to be formatted in one
;						   call
;=========================================================================

Procedure Determine_Track_Count

						; Harddisk?
	cmp	DeviceParameters.DP_DeviceType,Dev_HardDisk
	jne	$$IF163
	call	Calc_Track_Count		; Calc Tracks_To_Format
	jmp	SHORT $$EN163			; Removable media

$$IF163:
	mov	Tracks_To_Format,0001h		; Default to 1 track
;	$endif
$$EN163:

	ret

Determine_Track_Count ENDP

;=========================================================================
;
; Calc_Track_Count	 : This	routine	determines if we have enough tracks
;			   remaining to	use the	Max. number of tracks
;			   in the FormatTrack call.  If	the tracks remaining
;			   to be formatted is less that	the mAX. number	of
;			   allowable tracks for	the call, the mAX. number
;			   of allowable	tracks is set to the remaining track
;			   count.
;
;	 Inputs	 : Track_Count - MAX. number of	allowable tracks to be
;				 formatted in 1	FormatTrack call.
;		   TracksLeft  - Track count of	remaining tracks to be
;				 formatted.
;
;	 Outputs : Tracks_To_Format - Count of the tracks to be	formatted
;				      in the next FormatTrack call.
;
;=========================================================================

Procedure Calc_Track_Count

	push	AX				; Save regs

	mov	AX,Track_Count			; Max bytes to format

	cmp	word ptr TracksLeft+2,0		; M018; More than 64K of tracks?
	jnz	$$IF166				; M018; Then surely use Track_Count

	cmp	AX,word ptr TracksLeft		; M018; Too many tracks?
	JNA	$$IF166
	mov	AX,word ptr TracksLeft		; M018; Format remaining tracks

$$IF166:
	mov	Tracks_To_Format,AX		; Save track count
	pop	AX
	ret

Calc_Track_Count ENDP

;=========================================================================
; FormatTrack		 : This	routine	performs multi track or	single
;			   track formatting based on the state of the
;			   SpecialFunctions byte.
;
;	 Inputs	 : Tracks_To_Format	 - # of	tracks to format in 1 call
;		   FormatPacket		 - Parms for IOCTL call
;
;	 Outputs : NC			 - formatted track(s)
;		   CY			 - error in format
;		   AX			 - extended error on CY
;
;=========================================================================


Procedure FormatTrack

	mov	AX,(IOCTL shl 8) or Generic_IOCTL
	mov	BL,Drive			; Get Drive number
	inc	BL				; Make it 1 based
	mov	CX,(RawIO shl 8) 
	or 	CL,Track_Action 		; Track_Action is either
						; Format and Verify, or
						; Verify only
	mov	DX,Tracks_To_Format		; Get track count
	mov	FormatPacket.FP_TrackCount,DX	; Put count in parms list
	lea	DX,FormatPacket 		; Ptr to parms

;***
;***
;***	test	IsReal,0ffh
;***	jnz	DoRealInt21
;***
;***	xor	AX,AX
;***	clc
;***	jmp	SHORT  FormatTrackExit
;***
;***DoRealInt21:
;***
;***

	int	21h

	jnc	FormatTrackExit			; Error?
	mov	AH,59h				; Get extended error
	xor	BX,BX				; Clear BX
	int	21h

	cmp	AX,67				; Induced error from ENHDISK?
	jne	notInducedError
	clc
	ret
notInducedError:
	stc					; Flag an error

FormatTrackExit:
	ret

FormatTrack ENDP

;=========================================================================
;
; Format_Track_Retry	 : This	routine	performs the retry logic for
;			   the format multi-track.  It will retry each track
;			   until the failing track is encountered through
;			   a CY	condition.
;
;	 Inputs	 : none
;
;	 Outputs : CY -	indicates either a failing track or end	of format
;
;
;=========================================================================

Procedure Format_Track_Retry

	clc					; Clear existing CY
	mov	Tracks_To_Format,1		; Only format 1 track

$$DO168:					; While we have good tracks
	jc	$$EN168 			; Exit on bad track

	call	FormatTrack			; Format the track
	jc	$$IF170 			; Error?

	call	DisplayCurrentTrack		; Adjust percent counter
	call	Adj_Track_Count
	call	NextTrack			; Calc next track

$$IF170:
	jmp	SHORT $$DO168

$$EN168:
	ret

Format_Track_Retry ENDP

;=========================================================================
;
;  DisplayCurrentTrack :	This procedure prints the percentage of disk
;				formatted so far.
;				If /select is present, format can be exited
;				by returning AX!=0 from the int 2fh call, which
;				is handled by install.  This is to permit the
;				user to stop the format in progress.
;
;===========================================================================

DisplayCurrentTrack proc near

	push	DX
	push	CX
	push	AX

	push	DI		; M018
	push	SI		; M018

	mov	AX,Tracks_To_Format	 	;get track count

	add	Formatted_Tracks_Low,AX		;Indicate formatted a track
	adc	Formatted_Tracks_High,0
	mov	AX,Formatted_Tracks_Low
	mov	BX,Formatted_Tracks_High
	mov	CX,100				;Make integer calc for	div
	call	Multiply_32_Bits		; BX:AX = (Cyl	* Head *100)
	mov	DX,BX				;Set up divide
;M018	div	TracksPerDisk			;% = (Cyl * Head *100)/ # tracks
;M018 - begin
; DX:AX: 100*head*cylinder (should not be bigger than a DWORD)
; DI:SI: heads*cylinder 
; We need to assure a word division
	mov 	DI,word ptr TracksPerDisk+2
	mov	SI,word ptr TracksPerDisk
SetUpDivide:
	or	DI,DI
	jz	DivideOK
	shr	DI,1				; shift DI:SI 1 bit right
	rcr	SI,1
	shr	DX,1				; shift DX:AX 1	bit right
	rcr	AX,1
	jmp	short SetUpDivide
DivideOk:
	div	SI	
;M018 - end

	cmp	AX,PercentComplete		;Only print message when change
	je	ScreenUpdateDone

		; johnhe 02-27-90
		; Change added here for DOS 5.0 install program to do a
		; special interrupt to display the percent complete on
		; a gage. AX == special code, BX == percent complete

	test	SwitchMap,Switch_Select		; Was format spawned by the
	jz	NormalDisplay			; install program?
	mov	BX,AX				; BX == percent completed
	mov	AX,GAGE_UPDATE			; AX == special function code
	clc
	int	2fh				; Multiplex interrupt
	or	AX,AX				; AX <> 0 --> user wants to exit
	jz	ScreenUpdateDone
	jmp	FatalExit

		; End of code added for DOS 5.0 install program

NormalDisplay:
	mov	PercentComplete,AX		; Save it if changed
	Message msgCurrentTrack

ScreenUpdateDone:
	pop	SI				; M018
	pop	DI				; M018
	
	pop	AX
	pop	CX				; Restore register
	pop	DX
	return

DisplayCurrentTrack endp

;=========================================================================
; Adj_Track_Count	 : This	routine	adjusts	the track count	by the
;			   number of tracks that have been formatted
;			   in one FormatTrack call.
;
;	 Inputs	 : TracksLeft	 - # of	tracks remaining to be formatted
;		   Tracks_To_Format - Tracks formatted in 1 call
;
;	 Outputs : TracksLeft	 - # of	tracks remaining to be formatted
;=========================================================================

Procedure Adj_Track_Count

	push	AX			; save regs
	push	DX			; M018

;M018	mov	AX,TracksLeft		; get tracks remaining
;M018	sub	AX,Tracks_To_Format	; subtract amount formatted
;M018	mov	TracksLeft,AX		; save new tracks remaining value

;M018 - begin
	mov	DX,word ptr TracksLeft+2; get tracks remaining
	mov	AX,word ptr TracksLeft

	sub	AX,Tracks_To_Format	; subtract amount formatted
	sbb	DX,0

	mov	word ptr TracksLeft,AX	; save new tracks remaining value
	mov	word ptr TracksLeft+2,DX
;M018 - end
	
	pop	DX			; M018
	pop	AX			; restore regs
	ret

Adj_Track_Count endp

;=========================================================================
;
; NextTrack	 : This	routine	determines the next track to be
;		   formatted.
;
;	 Inputs	 : TracksLeft		 - # of	tracks remaining
;		   Tracks_To_Format	 - # of	tracks to format in 1 call
;		   FP_Head		 - disk	head
;		   FP_Cylinder		 - disk	cylinder
;
;	 Outputs : TracksLeft		 - # of	tracks remaining
;		   FP_Head		 - disk	head
;		   FP_Cylinder		 - disk	cylinder
;		   CY			 - no tracks left to format
;		   NC			 - tracks left to format
;
;=========================================================================

Procedure NextTrack


;M018	cmp	TracksLeft,00			; End of format?
;M018 - begin
	mov	CX,word ptr TracksLeft+2
	or	CX,Word ptr TracksLeft
;M018 - end
	jne	$$IF149 			; Yes

	stc					; Signal end of format
	mov	Format_End,True
	jmp	SHORT $$EN149

$$IF149:
	mov	CX,Tracks_To_Format		; Get mAX track count for call

$$DO151:					; While tracks remain
;M018: I don't get the next two lines. We are already sure TrackLeft<>0
;M018	cmp	TracksLeft,00			; End of format?
;M018	je	$$EN151 			; Yes

	cmp	CX,00				; End of head/cyl. adjustment?
	je	$$EN151 			; Yes

	inc	FormatPacket.FP_Head		; Next head
	mov	AX,FormatPacket.FP_Head 	; Get head for comp
	cmp	AX,DeviceParameters.DP_BPB.BPB_Heads ; Exceeded head count?
	jne	$$IF154 			; Yes

	mov	FormatPacket.FP_Head,00 	; Reinit. head
	inc	FormatPacket.FP_Cylinder	; Next cylinder

$$IF154:
	dec	CX				; Decrease counter
	jmp	SHORT $$DO151

$$EN151:
	clc					; Clear CY

$$EN149:
	ret

NextTrack ENDP

;=========================================================================
;	CurrentLogicalSector:
;	 Get the current logical sector	number
;
;    Input:
;	 current track = TracksPerDisk - TracksLeft
;	 SectorsPerTrack
;
;    Output:
;	 BX = logical sector number of the first sector	in the track we
;	      just tried to format
;=========================================================================

CurrentLogicalSector PROC NEAR

	push	AX				; Save regs
	push	BX
	push	DX

;M018	mov	AX, TracksPerDisk
;M018	sub	AX, TracksLeft
;M018	xor	DX,DX				; Clear DX
;M018 - begin
	mov	DX, word ptr TracksPerDisk+2
	mov	AX, word ptr TracksPerDisk
	sub	AX, word ptr TracksLeft
	sbb	DX, word ptr TracksLeft+2
;M018 - end
	mul	DeviceParameters.DP_BPB.BPB_SectorsPerTrack
	mov	WORD PTR Relative_Sector_High,DX ; Save high word of sector #
	mov	WORD PTR Relative_Sector_Low,AX ; Save low word of sector #

	pop	DX				; Restore regs
	pop	BX
	pop	AX

	return

CurrentLogicalSector ENDP

;=========================================================================
;
;    BadSector:
;	 Reports the bad Sectors.
;	 Reports the track where DiskFormat stopped.
;	 From then on it formats until it reaches a bad	track, or end,
;	 and reports that.
;
;    Output:
;	 Carry:	set -->	Fatal error
;	 if Carry not set
;	    AX - The number of consecutive bad Sectors encountered
;		 AX == 0 --> no	More bad Sectors
;	    BX - The logical sector number of the first	bad sector
;
;    Algorithm:
;	 if DiskFormatErrors
;	    DiskFormatErrors = false
;	    return current track
;	 else
;	    next track
;	    while not done
;	       if format track fails
;		  return current track
;	       next track
;	    return 0
;=========================================================================

BadSector proc	 near
						; Don't bother to do the format
						; /c was given
;	test	SwitchMap, SWITCH_C
;	jnz	NoMoreTracks

	test	FormatError, 0ffH
	jz	ContinueFormat

	mov	FormatError, 0
	jmp	SHORT ReportBadTrack

ContinueFormat:
	call	Adj_Track_Count 		; Decrease track counter
	call	NextTrack			; Adjust head and cylinder
	cmp	Format_End,True 		; End of format?
	je	$$IF44				; No

	call	Format_Loop			; Format until CY
	cmp	Format_End,True 		; End of format?
	je	$$IF45				; No

	call	CheckError			; Must be error - which error?
	jc	$$IF46				; Non-Fatal error?

	call	CurrentLogicalSector		; Yes - get position
						; set tracksize
	mov	AX,DeviceParameters.DP_BPB.BPB_SectorsPerTrack
	clc					; Signal O.K. to continue

$$IF46:
	jmp	SHORT $$EN45
$$IF45:
	jmp	SHORT NoMoreTracks		  ;End of format
$$EN45:
	jmp	SHORT $$EN44
$$IF44:
	jmp	SHORT NoMoreTracks		; end of format
$$EN44:
	return

ReportBadTrack:
	call	CurrentLogicalSector
	mov	AX, DeviceParameters.DP_BPB.BPB_SectorsPerTrack
	clc
	return

NoMoreTracks:					; Don't display done msg
	test	SwitchMap,(Switch_Select or SWITCH_AUTOtest)
	jnz	$$IF52				; if EXEC'd by SELECT
	Message msgFormatComplete

$$IF52:
	mov	AX, 0
	clc
	return

BadSector endp

;=========================================================================
; Get_Bad_Sector_Hard	 : Determine the bad sector.
;
;	 Inputs	 :
;		Head of failing track
;		Cylinder of failing track
;		Relative_Sector_Low	- 1st. sector in track
;		Relative_Sector_High
;
;		ClustBound_Adj_Factor	- The number of Sectors
;					  that	are to be read
;					  at one time.
;		ClustBound_SPT_Count	- Used by Calc_Cluster_Boundary
;					  to track how	many Sectors
;					  have been read.
;		ClustBound_Flag	 	- True (Use cluster buffer)
;					- False (Use internal buffer)
;		ClustBound_Buffer_Seg	- Segment of buffer
;
;	 Outputs : Marked cluster as bad
;=========================================================================

	PUBLIC Get_Bad_Sector_Hard
Procedure Get_Bad_Sector_Hard

	push	CX				; Save CX
	mov	CX,0001h			; Set counter to start at 1
	mov	ClustBound_SPT_Count,00h	; Clear sector counter
	mov	ClustBound_Adj_Factor,01h	; Default value

						; DO WHILE Sectors left
$$DO115:					
	cmp	CX,DeviceParameters.DP_BPB.BPB_SectorsPerTrack	;At end?
	ja	$$EN115 			; Yes,exit
	push	CX				; Save CX

	cmp	ClustBound_Flag,True		; Full buffer there?
	jne	$$IF117 			; Yes
	call	Calc_ClustBound_		; See if on boundary
	mov	AX,ClustBound_Buffer_Seg
						; Point to transfer area
	mov	WORD PTR RWPacket.TRWP_Transferaddress[0],0
	mov	WORD PTR RWPacket.TRWP_Transferaddress[2],AX
	jmp	SHORT $$EN117			; Default to  internal buffer

$$IF117:					; Point to transfer area
	mov	WORD PTR RWPacket.TRWP_Transferaddress[0],offset RW_TRF_Area
	mov	WORD PTR RWPacket.TRWP_Transferaddress[2],DS

$$EN117:
	call	Verify_Structure_Set_Up 	; Set up verify vars
	mov	AX,(IOCTL shl 8) or GENERIC_IOCTL
	xor	BX,BX				; Clear BX
	mov	BL,Drive			; Get Drive
	inc	BL				; Adjust it

;	mov	CX,(IOC_DC shl	8) or VERIFY_TRACK
	mov	CX,(IOC_DC shl	8) or READ_TRACK ; BUG BUG BUG
						; Buffer is only cluster size

	lea	DX,RWPacket			; Point to parms
	int	21h

	pop	CX				; Restore CX
	push	CX				; Save CX

	jnc	$$IF120 			; An error occurred
	call	Calc_Cluster_Position		; Determine which cluster
	call	BadClus 			; Mark the cluster as bad

$$IF120:
	pop	CX
	add	CX,ClustBound_Adj_Factor	; Adjust loop counter
	mov	AX,ClustBound_Adj_Factor	; Get adjustment factor
	xor	DX,DX
	add	AX,Relative_Sector_Low		; Add in low word
	adc	DX,Relative_Sector_High 	; Pick up carry in high word
	mov	Relative_Sector_Low,AX		; Save low word
	mov	Relative_Sector_High,DX 	; Save high word
	jmp	SHORT $$DO115

$$EN115:
	pop	CX
	ret

Get_Bad_Sector_Hard ENDP

;=========================================================================
; Get_Bad_Sector_Floppy : This	routine	marks an entire	track as bad
;			   since it is a floppy	disk.
;
;	 Inputs	 : Relative_Sector_Low	 - first sector
;
;	 Outputs : Fat marked with bad Sectors
;=========================================================================

Procedure Get_Bad_Sector_Floppy

	push	BX				; Save regs
	push	CX
						; Get Sectors/track
	mov	CX,DeviceParameters.DP_BPB.BPB_SectorsPerTrack

$$DO123:					; While Sectors left
	cmp	CX,00				; At end
	je	$$EN123 			; Yes

	push	BX				; Save BX we destroy it
	push	CX				; Save CX we destroy it
	call	Calc_Cluster_Position		; Get cluster position
	call	BadClus 			; Mark it as bad
	pop	CX				; Restore regs
	pop	BX
	dec	CX				; Decrease loop counter
	inc	Relative_Sector_Low		; Next sector
	jmp	SHORT $$DO123

$$EN123:
	pop	CX				; Restore regs
	pop	BX
	ret

Get_Bad_Sector_Floppy ENDP

;=========================================================================
;
;   Inputs:	BX = Cluster number
;   Outputs:	The given cluster is marked as	invalid
;		Zero flag is set if the cluster was already marked bad
;
;   Registers modified: DX
;			SI
;
; BADCLUS	 :	 Marks off a bad cluster in the	Fat
;			 If a cluster has already been marked bad it
;			 will return with ZR.
;
;	 Inputs	 :	 DX:AX - Cluster Number
;
;	 Outputs :	 Cluster is marked invalid
;			 ZR set	if cluster already marked bad
;
;=========================================================================

BadClus	 proc		 near		; mark bad clusters

	push	DI			; save affected regs
	push	AX
	push	BX
	push	CX
	push	DX
	push	ES

	mov	ES, WORD PTR FatSpace + 2 ; obtain seg of Fat
	cmp	fBigFat,TRUE		; 16 bit Fat?
	je	$$IF8			; no - 12-bit Fat

	push	AX			; saves low cluster number
	mov	SI,DX			; pick up high word of cluster
	mov	DI,AX			; pick up low word of cluster
	mov	CX,2			; divide by 2
	call	divide_32_Bits		; 32 bit divide

	add	AX,DI			; add in low word of result
	adc	DX,SI			; pick up low word carry
					; cluster = cluster * 1.5
	add	AX,WORD PTR FatSpace	; add 0
	adc	DX,0			; pick up carry

	mov	BX,DX			; get high word for adjust
	mov	CX,ES			; place seg in AX
	call	BadClus_address_Adjust	; adjust segment offset
	mov	ES,CX			; new segment
	mov	SI,AX			; new offset

	mov	DX,0ff7h		; bad cluster flag
	mov	AX,0fffh		; mask value

	pop	CX			; restore AX in CX - low cluster #
	test	CX,1			; is old clus num odd?
	jz	$$IF9			; yes

	mov	CL,4			; set shift count
	shl	AX,CL			; get only 12 bits - fff0
	mov	CL,4			; set shift count
	shl	DX,CL			; get 12 bits - ff70

$$IF9:
	jmp	SHORT $$EN8		; 16-bit Fats here

$$IF8:
	xor	SI,SI			; clear si
	mov	BX,DX			; get high word for multiply
	mov	CX,2			; multiply by 2
	call	Multiply_32_Bits	; 32 bit multiply due to 2 bytes per
					; Fat cell. This gives us an offset
					; into the FAT

	mov	CX,ES			; place seg in CX
	call	BadClus_Address_Adjust	; adjust segment:offset
	mov	ES,CX			; new segment
	mov	SI,AX			; new offset

	mov	DX,0fff7h		; bad cluster value
	mov	AX,0ffffh		; mask value

$$EN8:
	mov	CX,ES:[SI]		; get contents of Fat cell
	and	CX,AX			; make it 12 or 16 bit
					; depending on value in AX
	not	AX			; set AX to 0

	and	ES:[SI],AX		; clear Fat entry

	or	ES:[SI],DX		; flag it a bad cluster
	cmp	DX,CX			; return op == badval;

	pop	ES
	pop	DX
	pop	CX
	pop	BX
	pop	AX
	pop	DI
	return

badclus	endp

;=========================================================================
; Verify_Structure_Set_Up	 : Set up the fields for the Read IOCTL
;				   to verify the Sectors in a failing
;				   track.  Also, it displays the
;				   message notifying the user of the
;				   Sectors it is verifying.
;=========================================================================

Procedure	 Verify_Structure_Set_Up		; Set up verify structure

	mov	RWPacket.TRWP_SpecialFunctions,00h	; Reset special functions

	mov	AX,FormatPacket.FP_Head 		; Get current head
	mov	RWPacket.TRWP_Head,AX			; Get current head

	mov	AX,FormatPacket.FP_Cylinder		; Get current cylinder
	mov	RWPacket.TRWP_Cylinder,AX		; Get current cylinder

	dec	CX					; Make sector 0 based
	mov	RWPacket.TRWP_FirstSector,CX		; Get sector	to read

	mov	AX,ClustBound_Adj_Factor		; Get # of Sectors to read
	mov	RWPacket.TRWP_SectorsToReadWrite,AX	; Read only # sector(s)

	call	Calc_Cluster_Position			; Determine cluster number
	mov	WORD PTR Msg_Allocation_Unit_Val[+2],DX ; Save high word of cluster
	mov	WORD PTR Msg_Allocation_Unit_Val[+0],AX ; Save low word of cluster
	message MsgVerify

	ret

Verify_Structure_Set_Up ENDP

;=========================================================================
; Calc_Cluster_Position : This	routine	calculates which cluster the
;			   failing sector falls	in.
;
;	 Inputs	 : Relative_Sector_High	 - high	word of	sector position
;		   Relative_Sector_Low	 - low word of sector position
;
;	 Outputs : DX:AX - Cluster position
;=========================================================================

Procedure Calc_Cluster_Position

	push	CX				; Save regs
	push	DI
	push	SI

	mov	DX,WORD PTR Relative_Sector_High ; Get the high sector word
	mov	AX,WORD PTR Relative_Sector_Low  ; Get the low sector word

	sub	AX,StartSector			; Get relative sector #
	sbb	DX,0				; Pick up borrow

	mov	SI,DX				; Get high word
	mov	DI,AX				; Get low word
	xor	CX,CX				; Clear CX
						; Get Sectors/cluster
	mov	CL,DeviceParameters.DP_BPB.BPB_SectorsPerCluster
	call	divide_32_Bits			; 32 bit division

	mov	DX,SI				; Get high word of result
	mov	AX,DI				; Get low word of result
	add	AX,2				; Adjust for cluster bias
	adc	DX,0				; Pick up carry

	pop	SI				; Restore regs
	pop	DI
	pop	CX
	ret

Calc_Cluster_Position ENDP

;=========================================================================
; Calc_ClustBound_ : This routine will determine where, within a
;			   cluster, a sector resides.
;
;	 Inputs	 : Relative_Sector_Low		- Sector
;		   Relative_Sector_High
;
;	 Outputs : ClustBound_Adj_Factor	- The number of Sectors
;						   remaining in	the cluster.
;		   ClustBound_SPT_Count	 	- The count of	Sectors
;						  having been accessed	for
;						  a track.
;=========================================================================

Procedure Calc_ClustBound_

	push	AX				; Save regs
	push	BX
	push	CX
	push	DX
	push	SI
	push	DI

	xor	DX,DX				; Clear high word
	mov	DX,WORD PTR Relative_Sector_High
	mov	AX,WORD PTR Relative_Sector_Low
	sub	AX,StartSector			; Get relative sector #
	sbb	DX,0				; Pick up borrow

	mov	SI,DX				; Get high word
	mov	DI,AX				; Get low word
	xor	CX,CX				; Clear CX
	mov	CL,DeviceParameters.DP_BPB.BPB_SectorsPerCluster
	call	divide_32_Bits			; 32 bit division

	or	CX,CX				; See if remainder exists
	jz	$$IF132 			; Remainder exists

	xor	BX,BX
	mov	BL,DeviceParameters.DP_BPB.BPB_SectorsPerCluster
	sub	BX,CX
	mov	ClustBound_Adj_Factor,BX	; Remainder = sector count
	jmp	SHORT $$EN132			; Noremainder

$$IF132:
	xor	BX,BX				; Clear BX
	mov	BL,DeviceParameters.DP_BPB.BPB_SectorsPerCluster
	mov	ClustBound_Adj_Factor,BX

$$EN132:

	mov	AX,ClustBound_SPT_Count 	; Get current sector count
	xor	DX,DX				; Clear high word
	add	AX,ClustBound_Adj_Factor	; Get next sector count
						; Exceeded Sectors/track?
	cmp	AX,DeviceParameters.DP_BPB.BPB_SectorsPerTrack
	jna	$$IF135 			; Yes
						; only use difference
	mov	AX,DeviceParameters.DP_BPB.BPB_SectorsPerTrack
	sub	AX,ClustBound_SPT_Count 	; Get next sector count
	mov	ClustBound_Adj_Factor,AX

$$IF135:
	mov	AX,ClustBound_SPT_Count 	; Get sector count
	xor	DX,DX				; Clear high word
	add	AX,ClustBound_Adj_Factor	; Get new sector count
	mov	ClustBound_SPT_Count,AX 	; Save it

	pop	DI				; Restore regs
	pop	SI
	pop	DX
	pop	CX
	pop	BX
	pop	AX

	ret

Calc_ClustBound_ ENDP

;=========================================================================
;
; BadClus_address_Adjust	 - This	routine	adjusts	the segment and
;				   offset to provide addressibility into
;				   the Fat table.
;
;	 Inputs	 : BX	 - high	word to	adjust segment for
;		   AX	 - low word to adjust segment for
;		   CX	 - segment to be adjusted
;
;	 Outputs : CX	 - new segment value
;		   AX	 - new offset value
;
;=========================================================================

Procedure BadClus_address_Adjust

	push	BX				; Save regs
	push	DX
	push	DI
	push	SI

	mov	DX,CX				; Save segment value
	mov	SI,BX				; Get high word for divide
	mov	DI,AX				; Get low word for divide
	xor	CX,CX				; Clear CX
	mov	CL,Paragraph_Size		; Divide by 16
	call	divide_32_Bits			; Perform division

	add	DX,DI				; Adjust segment for result
	mov	AX,CX				; Pick up the remainder
	mov	CX,DX				; Pass back new segment

	pop	SI				; Restore regs
	pop	DI
	pop	DX
	pop	BX

	ret

BadClus_address_Adjust ENDP

;=========================================================================
;Routine name:	Multiply_32_Bits
;=========================================================================
;Description: A real sleazy 32	bit x 16 bit multiply routine. Works by	adding
;	       the 32 bit number to itself for each power of 2 contained in the
;	       16 bit number. Whenever a bit that is set in the	multiplier (CX)
;	       gets shifted to the bit 0 spot, it means	that that amount has
;	       been multiplied so far, and it should be	added into the total
;	       value. Take the example CX = 12 (1100). Using the associative
;	       rule, this is the same as CX = 8+4 (1000	+ 0100). The
;	       multiply	is done	on this	principle - whenever a bit that	is set
;	       is shifted down to the bit 0 location, the value	in BX:AX is
;	       added to	the running total in DI:SI. The	multiply is continued
;	       until CX	= 0. The routine will exit with	CY set if overflow
;	       occurs.
;
;
;Called Procedures: None
;
;Change History: Created	 7/23/87	 MT
;
;Input: BX:AX = 32 bit	number to be multiplied
;	 CX = 16 bit number to be multiplied. (Must be even number)
;
;Output: BX:AX	= output.
;	  CY set if overflow
;
;=========================================================================

	PUBLIC	Multiply_32_Bits
Multiply_32_Bits PROC

	push	DI
	push	SI
	xor	DI,DI			; Init result to zero
	xor	SI,SI
	cmp	CX,0			; Multiply by 0?
	je	$$IF75			; Keep going if not

$$DO76: 				; This works by adding result
	test	CX,1			; Need to add in sum of this bit?
	jz	$$IF77			; Yes
	add	SI,AX			; add in the total so far for
	adc	DI,BX			; this	bit multiplier (CY oflow)
	jmp	SHORT $$EN77		; Don't split multiplier

$$IF77:
	clc				; Force	non exit

$$EN77:
	jc	$$EN76			; Leave on overflow
	shr	CX,1			; See if need to multiply value
	cmp	CX,0			; by 2
	je	$$EN76			; Done if CX shifted down to zero

	add	AX,AX			; Each time CX is shifted, add
	adc	BX,BX			; value	to itself (Multiply * 2)
	jnc	$$DO76			; CY set on overflow

$$EN76:
	jc	$$IF83			; If no overflow, add in DI:SI
	mov	AX,SI			; which contains the original
	mov	BX,DI			; value if odd, 0 if even. This
	clc				; Set no overflow flag

$$IF83:
	jmp	SHORT $$EN75
$$IF75:
	xor	AX,AX
	xor	BX,BX			; Multiply by 0

$$EN75:
	pop	 SI
	pop	 DI
	ret

Multiply_32_Bits endp

;=========================================================================
; divide_32_Bits	 - This	routine	will perform 32bit division
;			   It works by first dividing the high word
;			   and leaving the remainder in DX and then
;			   dividing the low word with the remainder
;			   still in DX
;
;	 Inputs	 : SI:DI - value to be divided
;		   CX	 - divisor
;
;	 Outputs : SI:DI - result
;		   CX	 - remainder
;=========================================================================

Procedure divide_32_Bits

	push	AX			; Save regs
	push	BX
	push	DX


	xor	DX,DX			; clear DX
	mov	AX,SI			; get high word
	div	CX			; get high word result
	mov	SI,AX			; save high word result


	mov	AX,DI			; get low word
	div	CX			; get low word result
	mov	DI,AX			; save low word result
	mov	CX,DX			; pick up remainder

	pop	DX			; restore regs
	pop	BX
	pop	AX

	ret

divide_32_Bits	 endp


;=========================================================================
;
;  QuickFormat :	This procedure will perform a Quick format by
;			simply copying any bad cluster markers from the 
;			old FAT on the disk to the new FAT.  The old FAT is
;			read in one sector at a time using FatSector buffer.
;			The new FAT is held in FatSpace buffer.
;
;  Registers Destroyed : SI,AX,BX,CX
;
;  Assumes:	DS:DATA,ES:Nothing
;
;=========================================================================

QuickFormat	proc	near

	mov	SI,DATA
	mov	ES,SI				; Set ES to data segment

	assume	ES:DATA,DS:Nothing		; Assembler directive

						; Set device parameters here
	mov	ES:DeviceParameters.DP_SpecialFunctions,(INSTALL_FAKE_BPB or TRACKLAYOUT_IS_GOOD)
	lea	DX,ES:DeviceParameters
	call	SetDeviceParameters

	mov	ES:sector_in_buffer,0ffffh	; force first read to ensure
						; buffer validity

	test 	ES:fBigFAT,0ffh			; See if 16 bit fat
	jz	Set12BitEntry			; If zero then 12 bit fat
	mov	BX,0fff7h			; Set 16 bit value for bad cluster
	jmp	SHORT InitClusCount

Set12BitEntry:
	mov	BX,0ff7h			; Set 12 bit value for bad cluster

InitClusCount:
	mov	ES:CurrentCluster,2		; M015; No need to do the first 2
	mov	ES:BadClusValue,BX

QuickLoop:
	mov	AX,ES:CurrentCluster
	call	calc_sector_and_offset		; determine location of this entry

	mov	BX,ES:sector_in_buffer
	mov	AX,ES:sector_to_read
	cmp	AX,BX	      			; check if required sector is in buffer
	je	DontHaveToRead

	call	ReadFatSector			; read a sector of the FAT into buffer
	jc	ExitQuickFormat			; check for error

DontHaveToRead:
	mov	AX,ES:CurrentCluster		; AX = current cluster
	xor	CX,CX				; CX = get cluster contents signal
	lds	SI,ES:FatSector			; DS:SI --> FAT buffer
	call	GetFatSectorEntry		; AX = contents of FAT entry

	mov	BX,ES:BadClusValue		; Restore bad cluster value
	cmp	AX,BX				; Is this cluster marked bad?
	jne	NextCluster			; If AX<>BX good cluster

MarkInFormatBuffer:
	mov	CX,BX				; CX = value to set in FAT buffer
	mov	AX,ES:CurrentCluster		; AX = this cluster number
	lds	SI,ES:FatSpace			; DS:SI --> Format's FAT buffer
	call	GetSetFatEntry	       	   	; Set the cluster in Format's buffer
	
NextCluster:
;	call	DisplayProgress			; Show percentage complete
	inc	ES:CurrentCluster		; go to next cluster

	mov	AX,ES:CurrentCluster
	mov	BX,ES:TotalClusters
	cmp	AX,BX				; check for last cluster in FAT
	jna	QuickLoop

	clc

ExitQuickFormat:
	mov	BX,DATA	
	mov	DS,BX				; restore DS to DATA segment

	assume	DS:DATA,ES:DATA			; Assembler directive

	ret
QuickFormat	endp




;===========================================================================
;
; calc_sector_and_offset :	This procedure computes the logical sector
;				number the given FAT entry is in, and its
;				offset from the start of the sector.
;
;  Inputs :	AX = entry number
;		fBigFat = flag for 12- or 16-bit FAT entries
;		Number of reserved sectors
;
;  Output :	sector_to_read = logical disk sector holding FAT entry
;		entry_offset   = offset from start of sector
;		odd_entry      = flag for 12-bit entry alignment (1=odd,0=even)
;
;  Registers Destroyed : AX,BX,CX,DX 
;
;  Strategy :	This procedure assumes the sector size is 512 bytes.
;		The byte offset from the start of the FAT is first
;		calculated.  This is then divided by 512, so that
;			required sector = quotient
;			offset	        = remainder
;		The logical sector number is obtained by adding on the
;		number of reserved sectors.
;
; M017: The code does not assume 512 BytesPerSector (it is even simpler!)
;============================================================================

calc_sector_and_offset	proc	near

	assume	DS:NOTHING,ES:DATA
	xor	DX,DX
	test	ES:fBigFat,0ffh		; See if 16 bit FAT
	jz	FindOffset12		; If not do 12 bit FAT

FindOffset16:
	shl	AX,1			; AX *= 2
	adc	DX,0			; Add the carry
					; Now offset from start of FAT is
					; in DX:AX
	jmp	SHORT	FindSector

FindOffset12:
	mov	BX,AX			; BX = cluster number
	shl	AX,1
	add	AX,BX			; AX *= 3

	mov	ES:odd_entry,AL 	; lsb of AX determines even or odd
	and	ES:odd_entry,1

	shr	AX,1			; Divide by 2
					; Now offset from start of FAT is
					; in DX:AX

FindSector:
;M017 - begin
;	mov	ES:entry_offset,AX 	; required offset is in low 9 bits
;	and	ES:entry_offset,1ffh
;	
;	shr	AX,1
;	ror	DX,1			; place lsb into msb
;	or 	AX,DX			; need to rotate number in DX:AX, so
;					; bring lsb in DX to msb in AX
;	mov	CL,8			; perform 8 more shifts
;	shr	AX,CL				

; Since DX:AX can at most be 128K (ie DX<=1 by the above code)
; and GetTotalCluster assured us we had at least 32 BytesPerCLuster
; no check is required prior to the division.

	div	ES:DeviceParameters.DP_BPB.BPB_BytesPerSector
	mov	ES:entry_offset,DX
;M017 - end

	add	AX,ES:DeviceParameters.DP_BPB.BPB_RESERVEDSECTORS
	mov	ES:sector_to_read,AX	

	ret
calc_sector_and_offset	endp






;===========================================================================
;
; ReadFatSector :	This procedure will read in a sector of the FAT
;			into the FatSector buffer.  This is done by loading
;			the required parameters and calling ReadWriteSectors.
;
; Input :	sector_to_read
; Output:	loaded buffer
;		sector_in_buffer
;
; Registers destroyed: AX,BX,CX,DX
;
;===========================================================================

ReadFatSector	proc	near

	assume	DS:NOTHING,ES:DATA
	
	push	DS			; Preserve DS

	mov	AL,ES:Drive		; AL = DOS drive number
	mov	CX,1			; 1 sector only
	mov	DX,ES:sector_to_read	; DX = starting sector
	lds	BX,ES:FatSector		; DS:BX --> read buffer
						; we are accessing < 32mb
	mov	ES:Read_Write_Relative.Start_Sector_High,0
 
	call	Read_Disk		; perform read

	mov	ES:sector_in_buffer,DX	; update sector in memory

	pop	DS			; Restore DS

	ret
ReadFatSector	endp






;===========================================================================
; Routine name:	GetSetFatEntry
;===========================================================================
;
; Description: Returns or sets the contents of the specified fat
;	       entry from a specified buffer. The buffer may be
;	       up to 128K in length for 16 bit FATs and 16K for
;	       12 bit FATs.
;
; Arguments:		DS:SI --> Start of FAT buffer
;			AX = Cluster number
;			CX = 0 if get cluster else set cluster from CX
; ----------------------------------------------------------------
; Returns:   		AX    --> Contents of FAT entry
; -----------------------------------------------------
; Registers destroyed:	AX BX CX DX
; ----------------------------------------
; Strategy
; --------
;===========================================================================

GetSetFatEntry PROC

	assume	DS:NOTHING,ES:DATA

	push	SI			; Save regs for 'C' compatibility
	push	DS
	push	ES

;	mov	BX,DATA			; ES = DATA
;	mov	ES,BX

	call	Seg_Adj			; Normalize the pointer
	xor	DX,DX			; Find offset of entry in the buffer
	test	ES:fBigFat,0ffh		; See if 16 bit FAT
	jz	FindEntry12		; If not do 12 bit FAT

FindEntry16:
	shl	AX,1			; DS:AX *= 2
	adc	DX,0			; Add the carry

AddToBufStart:				; Offset in DX:AX may be > 64K
	mov	BX,16			; Convert DX:AX to paragraphs
	div	BX			; (AX = DX:AX / 16) (DX = DX:AX % 16)

	mov	BX,DS			; Add paragraphs to DS
	add	AX,BX
	mov	DS,AX

	add	SI,DX			; Add remaining offset in DX to SI
	jcxz	GetFatEntry16		; If CX == 0 then get entry

	mov	[SI],CX			; Set the entry
	jmp	SHORT GetSetEntryExit	; AX = FAT entry

GetFatEntry16:
	mov	AX,[SI]			; Move the entry into AX
	jmp	SHORT GetSetEntryExit	; AX = FAT entry

FindEntry12:
	mov	BX,AX			; BX = cluster number
	shl	AX,1
	add	AX,BX			; AX *= 3

	test	AX,1			; Test lsb of AX to see if even or odd
	pushf				; Save zero flag

	shr	AX,1			; Divid by 2
	add	SI,AX			; Address the cluster
	mov	AX,[SI]			; AX = entry + part of another entry
	popf				; Get bit test off the stack
	jnz	OddCluster		; If not zero then it's an odd cluster

EvenCluster:
	jcxz	GetEvenCluster		; Check for get or set
	and	AX,0f000h		; Zero out the value in 12 lsb
	or	AX,CX			; Set the new value
	mov	[SI],AX
	jmp	SHORT GetSetEntryExit	; AX = FAT entry

GetEvenCluster:
	and	AX,0fffh		; Mask off high 4 bits
	jmp	SHORT GetSetEntryExit	; AX = FAT entry

OddCluster:
	jcxz	GetOddCluster		; Check for get or set

	shl	CX,1			; Set the value
	shl	CX,1			; Shift left 4 bits
	shl	CX,1
	shl	CX,1
	and	AX,0fh			; Zero out existing value in 12 msb
	or	AX,CX			; Insert new value
	mov	[SI],AX
	jmp	SHORT GetSetEntryExit	; AX = FAT entry

GetOddCluster:
	shr	AX,1			; Shift over 4 bits to get entry
	shr	AX,1
	shr	AX,1
	shr	AX,1

GetSetEntryExit:
	pop	ES			; Restore regs for 'C' compatibility
	pop	DS
	pop	SI
	ret

GetSetFatEntry ENDP





;===========================================================================
; Routine name:	GetFatSectorEntry
;===========================================================================
;
; Description: Returns the contents of the specified fat
;	       entry from the FatSector buffer. It is assumed that the 
;	       required sector is in the buffer already.  If the entry is
;	       12 bits and overlaps sectors, the next sector will be read
;	       into the FatSector buffer.
;
; Arguments:		DS:SI --> Start of FatSector buffer
;			entry_offset = offset of entry from start of buffer
;			fBigFat = flag for 12- or 16-bit FAT
; -------------------------------------------------------------------------
; Returns:   		AX    --> Contents of FAT entry
; -----------------------------------------------------
; Registers destroyed:	AX BX CX DX
; ---------------------------------
;
;===========================================================================

GetFatSectorEntry PROC

	assume	DS:NOTHING,ES:DATA

	push	SI			; Save regs for 'C' compatibility
	push	DS
	push	ES

;	mov	BX,DATA			; ES = DATA
;	mov	ES,BX

	call	Seg_Adj			; Normalize the pointer

	test	ES:fBigFat,0ffh		; See if 16 bit FAT
	jz	Get12		; If not do 12 bit FAT

Get16:
	mov	DX,ES:entry_offset
	add	SI,DX  			; Add offset value in DX to SI
	mov	AX,[SI]			; Move the entry into AX
	jmp	SHORT GetFatSectorEntryExit	; AX = FAT entry			

Get12:
	mov	DX,ES:entry_offset
	add	SI,DX			; Address the cluster entry

	cmp	DX,1ffh			; Entry straddles two FAT sectors if
	jb	Does_Not_Straddle	; offset is 511 (last byte in sector) 

Does_Straddle:
	call	ReadInTwoParts		; Handle special case by reading in
	jmp	SHORT  GetFatSectorEntryExit	; the next FAT sector also

Does_Not_Straddle:
	mov	AX,[SI]			; AX = entry + part of another entry

	test  	ES:odd_entry,1
	jnz	GetOddCluster		; If not zero then it's an odd cluster

IsEvenCluster:
	and	AX,0fffh		; Mask off high 4 bits
	jmp	SHORT GetFatSectorEntryExit	; AX = FAT entry

IsOddCluster:
	shr	AX,1			; Shift over 4 bits to get entry
	shr	AX,1
	shr	AX,1
	shr	AX,1

GetFatSectorEntryExit:
	pop	ES			; Restore regs for 'C' compatibility
	pop	DS
	pop	SI
	ret

GetFatSectorEntry ENDP





;===========================================================================
;
;  ReadInTwoParts :	This procedure will determine the value of a 12-bit 
;			FAT entry which is straddled across two consecutive
;			sectors in the FAT.  The value is found by reading
;			the available part from the first sector, and then
;			loading the next sector in the FatSector buffer and
;			reading and combining the next part.
;  Input : odd_entry
;	   sector_to_read
;	   DS:SI pointer to required entry location in buffer (this is
;	    offset 511 from start of FatSector buffer)
;
;  Output: AX = entry value
;	   FatSector contains next FAT sector
;
;  Registers destroyed : AX
;
;  Strategy :	The position of the entry is different for odd and even
;		entries.  The 3-byte layout for two consecutive 12-bit entries
;		is shown below.  (The byte |X1 X0| or |Y2 X2| is the last 
;		in the buffer).
;
;		|X1 X0|Y0 X2|Y2 Y1|
;
;		Even entries start at X1 (thus need to get X2X1X0).
;		Odd entries start at Y0 (thus need to get Y2Y1Y0).
;
;===========================================================================

ReadInTwoParts	proc	near

	assume	DS:NOTHING,ES:DATA

	push	BX				; Save registers
	push	CX
	push	DX
	push	SI

	xor	AX,AX
	mov	AL,BYTE PTR [SI]		; read last byte of buffer

	mov	BX,ES:sector_to_read		; compute next sector to read
	inc	BX
	mov	ES:sector_to_read,BX

	test	ES:odd_entry,1
	jnz	OddEntry

EvenEntry:
	push	AX
	call	ReadFatSector			; read in the next FAT sector
	pop	AX
	
	lds	SI,ES:FatSector			; set DS:SI to start of buffer
	xor	BX,BX

	mov	BH,BYTE PTR [SI]		; read first byte of buffer
	and	BH,0fh				; mask out Y2
	or	AX,BX				; place X2 into position

	jmp	SHORT EndReadInTwo

OddEntry:
	and	AL,0f0h				; mask out X2
	mov	CL,4
	shr	AX,CL				; place Y0 into position
	
	push	AX
	call	ReadFatSector			; read in the next FAT sector
	pop	AX

	lds	SI,ES:FatSector			; set DS:SI to start of buffer
	
	xor	BX,BX
	mov	BL,BYTE PTR [SI]		; read first byte of buffer
	mov	CL,4
	shl	BX,CL	
	or	AX,BX				; place Y2 Y1 into position
	
EndReadInTwo:
	pop	SI				; Restore registers
	pop	DX
	pop	CX
	pop	BX

	ret
ReadInTwoParts	endp




;========================================================================
;
;  DisplayProgress :	This procedure will give the user a message 
;			indicating the percentage completion of the
;			formatting task.
;
;  Assumes:	ES:DATA,DS:Nothing
;
;========================================================================
;
;DisplayProgress	proc	near
;
;	push	AX
;	push	BX
;	push	DX
;
;	mov	AX,ES:CurrentCluster
;
;	test	AX,1			; Reduce #divides by half by displaying
;	jnz	UpdateDone		; only for even values of CurrentCluster
;
;	mov	BX,100
;	mul	BX
;	mov	BX,ES:TotalClusters
;	div	BX
;	
;	cmp	AX,ES:PercentComplete
;	je	UpdateDone
;	
;	mov	ES:PercentComplete,AX
;	Message	msgCurrentTrack
;
;UpdateDone:
;	pop	DX
;	pop	BX
;	pop	AX
;
;	ret
;DisplayProgress	endp


CODE ENDS


END
