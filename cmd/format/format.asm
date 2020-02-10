;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;page	 84,132
;
;	 SCCSID	= @(#)format.asm 1.26 85/10/20
;	 SCCSID	= @(#)format.asm 1.26 85/10/20
; =======================================================================
;
;	 86-DOS	FORMAT DISK UTILITY
;
;	 This routine formats a	new disk,clears	the FAT	and DIRECTORY then
;	 optionally copies the SYSTEM and COMMAND.COM to this new disk
;
;	 SYNTAX: FORMAT	 [drive][/switch1][/switch2]...[/switch16]
;
;	 Regardless of the drive designator , the user will be prompted	to
;	 insert	the diskette to	be formatted.
;
; =======================================================================
;
;	     5/12/82 ARR Mod to	ask for	volume ID
;	     5/19/82 ARR Fixed rounding	bug in CLUSCAL:
;   REV 1.5
;	     Added rev number message
;	     Added dir attribute to DELALL FCB
;   REV 2.00
;	     Redone for	2.0
;   REV 2.10
;	     5/1/83 ARR	Re-do to transfer system on small memory systems
;   REV 2.20
;	     6/17/83 system size re-initialization bug -- mjb001
;   Rev 2.25
;	     8/31/83 16-bit fat	insertion
;   Rev 2.26
;	     11/2/83 MZ	fix signed compare problems for	bad sectors
;   Rev 2.27
;	     11/8/83 EE	current	directories are	always saved and restored
;   Rev 2.28
;	     11/9/83 NP	Printf and changed to an .EXE file
;   Rev 2.29
;	     11/11/83 ARR Fixed	ASSIGN detection to use	NameTrans call to see
;			 if drive letter remapped. No longer IBM only
;   Rev 2.30
;	     11/13/83 ARR SS does NOT =	CS, so all use of BP needs CS override
;   Rev 2.31
;	     12/27/83 ARR REP STOSB instruction	at Clean: changed to be
;			 sure ES = CS.
;
;   Rev 5.00 Summer '90  SA  Reworked Format code.
; =======================================================================

;
;----------------------------------------------------------------------------
;
; M00x : Assume Media is formatted if Query_BLock_IOCTL is not supported
;	 Bug #4801.
;
;
; M024 : B#5495. Added "Insufficient memory" message when FORMAT cannot
;		allocate memory for FAT, Directory... etc. Reclaimed
;		the msgBadDrive which was not being used. Removed the
;		unwanted EXTRN msgBadDrive.
;
; M025 : Removed obsolete IBMCOPYRIGHT conditional
;
;---------------------------------------------------------------------------
;

;----------------------------------------------------------------------------
;
CODE	 SEGMENT PUBLIC PARA 'CODE'
CODE	 ENDS

; =======================================================================


DATA	 SEGMENT PUBLIC PARA 'DATA'
DATA	 ENDS

; =======================================================================

End_Of_Memory SEGMENT PUBLIC PARA 'BUFFERS'
End_Of_Memory ENDS

; =======================================================================

;===========================================================================
;Declaration of include files
;===========================================================================

debug	 equ	 0
	 .xlist
	 INCLUDE VERSION.INC
;	 INCLUDE VERSIONA.INC
	 INCLUDE DOSMAC.INC
	 INCLUDE SYSCALL.INC
;	 INCLUDE ERROR.INC
	 INCLUDE DPB.INC
	 INCLUDE CPMFCB.INC
	 INCLUDE DIRENT.INC
	 INCLUDE CURDIR.INC
;	 INCLUDE PDB.INC
	 INCLUDE BPB.INC
	 INCLUDE FOREQU.INC
	 INCLUDE FORMACRO.INC
	 INCLUDE IOCTL.INC
	 INCLUDE FORSWTCH.INC
;	 INCLUDE SYSVAR.INC
       	 INCLUDE SAFE.INC		; Extrn	declarations for SAFE.ASM
	 INCLUDE SAFEDEF.INC
	 .list

;===========================================================================
; Declarations for all publics in other modules used by this module
;===========================================================================

;Bytes
	EXTRN	fBigFat	 		:BYTE
	EXTRN	CommandFile		:BYTE
IFDEF DBLSPACE_HOOKS
	EXTRN	DblSpaceFile		:BYTE
ENDIF
	EXTRN	msgNoRoomDestDisk	:BYTE
	EXTRN	msgAssignedDrive	:BYTE
	EXTRN	msgBadDosVersion	:BYTE
	EXTRN	msgDirectoryWriteError	:BYTE
	EXTRN	msgFormatComplete	:BYTE
	EXTRN	msgFormatNotSupported	:BYTE
	EXTRN	msgFatwriteError	:BYTE
	EXTRN	msgLabelPrompt		:BYTE
	EXTRN	msgNeedDrive		:BYTE
	EXTRN	msgNoSystemFiles	:BYTE
	EXTRN	msgNetDrive		:BYTE
	EXTRN	msgInsertDisk		:BYTE
	EXTRN	msgHardDiskWarning	:BYTE
	EXTRN	msgSystemTransfered	:BYTE
	EXTRN	msgFormatAnother?	:BYTE
	EXTRN	msgBadCharacters	:BYTE
;	EXTRN	msgBadDrive		:BYTE		; M024
	EXTRN	msgParametersNotSupported:BYTE
	EXTRN	msgReInsertDisk 	:BYTE
	EXTRN	msgInsertDosDisk	:BYTE
	EXTRN	msgFormatFailure	:BYTE
	EXTRN	msgNotSystemDisk	:BYTE
	EXTRN	msgDiskUnusable 	:BYTE
	EXTRN	msgOutOfMemory		:BYTE
	EXTRN	msgCurrentTrack 	:BYTE
	EXTRN	msgWriteProtected	:BYTE
	EXTRN	msgInterrupt		:BYTE
	EXTRN	msgCrLf 		:BYTE
	EXTRN	msgShowKBytes		:BYTE
	EXTRN	msgShowMBytes		:BYTE
	EXTRN	msgDecimalMBytes	:BYTE
	EXTRN	msgDecimalNumberofDecimal:BYTE
	EXTRN	msgSysWarning		:BYTE
	EXTRN	msgVerifyShowKBytes	:BYTE
	EXTRN	msgVerifyShowMBytes	:BYTE
	EXTRN	msgVerifyDecimalMBytes	:BYTE
	EXTRN	msgSavingUNFORMATInfo	:BYTE
	EXTRN	msgQuickFormatShowKBytes:BYTE
	EXTRN	msgQuickFormatShowMBytes:BYTE
	EXTRN	msgQuickFormatDecimalMBytes:BYTE

	EXTRN	msgFileCreationError	:BYTE
	EXTRN	ContinueMsg		:BYTE
	EXTRN	Fatal_Error		:BYTE
	EXTRN	Read_Write_Relative	:BYTE
	EXTRN	Parse_Error_Msg 	:BYTE
	EXTRN	Extended_Error_Msg	:BYTE

	EXTRN	CMCDDFlag		:BYTE		; M033
	EXTRN	GetDeviceParameters	:NEAR

;Words
	EXTRN	PSP_Segment		:WORD
;	EXTRN	sector_in_buffer	:WORD
	EXTRN	TotalClusters		:WORD

;Pointers

;Functions
	EXTRN	Global_Init		:NEAR
	EXTRN	Phase1Initialisation	:NEAR
	EXTRN	Disk_Format_Proc	:NEAR
	EXTRN	SetDeviceParameters	:NEAR
	EXTRN	Prompt_User_For_Disk	:NEAR
	EXTRN	Multiply_32_Bits	:NEAR
;	EXTRN	Fat_Init		:NEAR
	EXTRN	calc_sector_and_offset	:NEAR
	EXTRN	ReadFatSector		:NEAR
	EXTRN	GetSetFatEntry		:NEAR
	EXTRN	GetFatSectorEntry	:NEAR
	EXTRN	Yes?			:NEAR
	EXTRN	Mirror			:NEAR

;Structures

;Labels
	EXTRN	WriteDos 		:NEAR

;===========================================================================
; Data segment
;===========================================================================

DATA    SEGMENT PUBLIC PARA 'DATA'

;===============================
;
; Exit Status defines
;
;===============================

EXIT_OK			equ	0
EXIT_CTRLC		equ	3
EXIT_FATAL		equ	4
EXIT_NO			equ	5
EXIT_DRV_NOT_READY	equ	6	; Drive not ready error
EXIT_WRIT_PROTECT	equ	7	; write protect error

DOSVER_LOW		equ	0300H+20
DOSVER_HIGH		equ	0300H+20

RECLEN			equ	fcb_RECSIZ+7
RR			equ	fcb_RR+7

Fbig			equ	0ffh		; flag for big Fat
PSP_Environ		equ	2ch		; location of
						; environ. segment
						; in PSP
MIRROR_SIGNATURE	equ	5050h		; Parameter to Mirror to tell
						; it Format's calling it

SavedParams		A_DEVICEPARAMETERS	<>	;default
DeviceParameters	A_DEVICEPARAMETERS	<>	;dynamic
SwitchDevParams		A_DEVICEPARAMETERS	<>	;switch-based
Disk_Access		A_DiskAccess_Control	<>
FormatPacket		A_FormatPacket		<>

DirectorySector 	dd	0	; pointer to root directory buffer
FatSpace		dd	?	; pointer to FAT buffer
FatSector		dd	?	; pointer to 1-sector buffer used for
					;  reading old FAT from disk
;No more SAFE module
;HeaderBuf		dd	0	; pointer to header buffer for restore file
DirBuf			dd	0	; pointer to DIR buffer for reading
					;  old fat chains


; ========================================================================

Bios			a_FileStructure <>
dos			a_FileStructure <>
command 		a_FileStructure <>
IFDEF DBLSPACE_HOOKS
DblSpaceBin		a_FileStructure <>
ENDIF

;***	public IsReal
;***IsReal	DB	1



ValidSavedDeviceParameters	db		0
NoDPChange		db	0	; It is sometimes necessary not to
					; modify the drive parameters, even
					; in a Fatal exit.  This flag is set
					; in that case.
FirstHead		dw	?
FirstCylinder		dw	?
TracksLeft		dd	?	; M018
TracksPerDisk		dd	?	; M018


;M025 - obsolete data items removed here
;IF ibmcopyright
;
;file_ptr		dw	0
;			dw	20290, 19282
;
;VER			db	"D"	; version number e.g., 410
;VER_YYY 		db	"410"	; the first 3 chars are placed here
;			db	" R"	; revision number e.g., 01
;VER_XX			db	"00"	; the next 2 chars are placed here
;
;ENDIF

; ========================================================================

Formatted_Tracks_Low	dw	0
Formatted_Tracks_High	dw	0


NumSectors		dw	0FFFFh
TrackCnt		dw	0FFFFh

Old_Dir 		db	FALSE

SectorsInRootDirectory	dw	?

PrintStringPointer	dw	0

ExitStatus		db	0


; =======================================================================

RootStr 		db	?
			db	":\",0
DblFlg			db	0	;Initialize flags to zero
mStart			dw	?	; Start of sys	file buffer (para#)
mSize			dw	?	; Size	of above in paragraphs

					; Storage for users current directory

UserDirs		db	DIRSTRLEN + 3 DUP(?)

; ===========================================================================
	PUBLIC	Paras_Per_Fat
; ===========================================================================

Paras_Per_Fat		dw	0000h		; holds Fat para count


CommandFile_Buffer	db	127	 dup(0) ; allow room for copy


VolFcb			db	-1,0,0,0,0,0,8
VolDrive		db	0
VolNam			db	"           "
			db	8
			db	26 DUP(?)

DelFcb			db	-1,0,0,0,0,0,8
DelDrive		db	0
dELnam			db	"???????????"
			db	8
			db	26 DUP(?)

TranSrc 		db	"A:CON",0,0 ; Device so we don't hit the Drive
TranDst 		db	"A:\",0,0,0,0,0,0,0,0,0,0

BegSeg			dw	?
SwitchMap		dw	?
SwitchCopy		dw	?
Fat			dw	?
			dw	?
ClusSiz 		dw	?
SecSiz			dw	?
Sectors 		dw	?
InBuff			db	80,0
			db	80 dup(?)


Drive			db	0
DriveLetter		db	"x"
SystemDriveLetter	db	"x"

Ctrl_Break_Vector	dd	?		 ; Holds CTRL-Break
						 ; vector

Command_Path		dd	 ?		 ; hold pointer to
						 ; COMMAND's path



Environ_Segment 	dw	 ?			 ; hold segment of
						 ; environ. vector
; =======================================================================
;
; Disk Table
; Used if NumberOfFats in BPB
; is 0.
;		I documented this table format some, but I don't know what
;		the low byte of the 3rd word is used for; couldn't find
;		a user!  - jgl
;
; =======================================================================

;				disk sectors	sec/	root	12/16 bit
;			     loword    hiword	clus   dirents	fat

DiskTable		dw	0,	32680,	0803h,	512,	0
			dw	4h,	0000h,	0402h,	512,	Fbig
			dw	8h,	0000h,	0803h,	512,	Fbig
			dw	10h,	0000h,	1004h,	512,	Fbig
			dw	20h,	0000h,	2005h,	512,	Fbig

Org_AX			dw	?			 ;AX	on entry

ClustBound_Adj_Factor	dw	?


ClustBound_SPT_Count	dw	?


ClustBound_Flag		db	False



ClustBound_Buffer_Seg	dw	?

Relative_Sector_Low	dw	?
Relative_Sector_High	dw	?

Fat_Flag		db	?

Msg_Allocation_Unit_Val dd	?

SizeInK			dw	0		; Variables used in format size message
SizeInM			dw	0
DecSizeInM		dw	0

RWErrorCode		dw	0		; Used to save error code returned
						; from Int25/26 in ReadWriteSectors,
						; module SAFE. Used by Phase1Initialisation
FoundN			db	FALSE		; flag used in search for N contiguous clusters
Cluster			dw	0		; cluster variable used in FAT search
SpecialCase		db	FALSE		; special case when /S is specified, with safe format

sector_to_read		DW	?		; Logical sector number of FAT required
sector_in_buffer	DW	0ffffh		; FAT sector currently in memory, init.
						;  to high value to force first read
NumClusters		DW	?		; Holds #clusters required for 1.5K
						;  (Will be 1,2 or 3)

EndValue		DW	?		; Holds FAT entry value for end of chain

;NeedSysDisk		db	0		; flag for sys disk required,
						; when buffer not big enough
						; to read in all sys files
IFDEF DBLSPACE_HOOKS
fDblSpace		db	FALSE		; TRUE if DblSpace.bin found
ENDIF

DATA	ENDS

;===========================================================================
; Executable code segment
;===========================================================================

CODE	 SEGMENT PUBLIC  PARA	 'CODE'

	 ASSUME  CS:CODE,DS:NOTHING,ES:NOTHING


; =======================================================================
;
; Define as public for	debugging
;
; =======================================================================

; procedures

	PUBLIC	ShrinkMemory
	PUBLIC	InitSysParm
	PUBLIC	ZeroAllBuffers
	PUBLIC	ZeroBuffer
	PUBLIC	FindNClusters
	PUBLIC	WriteDiskInfo
	PUBLIC	RestoreDevParm
;	PUBLIC	GetSize
	PUBLIC	AddToSystemSize
	PUBLIC	Div32
	PUBLIC	Phase2Initialisation
	PUBLIC	ShowFormatSize
	PUBLIC	Done
;	PUBLIC	PrintErrorAbort
	
	PUBLIC	GetCmdSize
	PUBLIC	Start
;	PUBLIC	FatAllocated
;	PUBLIC	SysLoop

;	PUBLIC	Cleared
;	PUBLIC	Louse
;	PUBLIC	LouseP
;	PUBLIC	FatWrt
;	PUBLIC	SysOk
;	PUBLIC	Status
;	PUBLIC	ReportC
;	PUBLIC	OnClus
	PUBLIC	More
	PUBLIC	FatalExit
	PUBLIC	SysPrm
;	PUBLIC	DoPrompt
	PUBLIC	IsRemovable
	PUBLIC	DoSafe
;	PUBLIC	CheckRemove
;	PUBLIC	IsRemove
;	PUBLIC	NotRemove
	PUBLIC	CrLf
	PUBLIC	PrintString
	PUBLIC	Std_Printf
;	PUBLIC	NotBigTotalSectors
;	PUBLIC	WriteDIRloop
	PUBLIC	Main_Routine
	PUBLIC	ControlC_Handler
	PUBLIC	GetBioSize
	PUBLIC	GetDosSize

; bytes
	PUBLIC	RootStr
	PUBLIC	DblFlg
	PUBLIC	Drive
	PUBLIC	UserDirs
	PUBLIC	VolFcb
	PUBLIC	VolNam
	PUBLIC	TranSrc
	PUBLIC	TranDst
	PUBLIC	InBuff
	PUBLIC	DriveLetter
	PUBLIC	SystemDriveLetter
	PUBLIC	ExitStatus
	PUBLIC	VolDrive
	PUBLIC	DelFcb
	PUBLIC	DelDrive
	PUBLIC	Fat_Flag
	PUBLIC  Old_Dir
	PUBLIC	ClustBound_Flag
	PUBLIC	ValidSavedDeviceParameters
	PUBLIC	NoDPChange
;	PUBLIC	NeedSysDisk
IFDEF DBLSPACE_HOOKS
	PUBLIC	fDblSpace
ENDIF

; words
	PUBLIC	FatSpace
	PUBLIC	FirstHead
	PUBLIC	FirstCylinder
	PUBLIC	TracksLeft
	PUBLIC	TracksPerDisk
	PUBLIC	SectorsInRootDirectory
	PUBLIC	PrintStringPointer
	PUBLIC	mStart
	PUBLIC	mSize
	PUBLIC	BegSeg
	PUBLIC	SwitchMap
	PUBLIC	SwitchCopy
	PUBLIC	Fat
	PUBLIC	ClusSiz
	PUBLIC	SecSiz
	PUBLIC	Formatted_Tracks_High
	PUBLIC	Formatted_Tracks_Low
	PUBLIC  NumSectors
	PUBLIC  TrackCnt
	PUBLIC  Org_AX
	PUBLIC	ClustBound_Adj_Factor
	PUBLIC	ClustBound_SPT_Count
	PUBLIC	ClustBound_Buffer_Seg
	PUBLIC	Relative_Sector_Low
	PUBLIC	Relative_Sector_High
	PUBLIC	Environ_Segment
	PUBLIC	RWErrorCode
	PUBLIC	SizeInK
	PUBLIC	SizeInM
	PUBLIC	DecSizeInM
	PUBLIC	sector_to_read
	PUBLIC	sector_in_buffer

;constants
	PUBLIC  EXIT_OK
	PUBLIC	EXIT_CTRLC		
	PUBLIC	EXIT_FATAL	
	PUBLIC	EXIT_NO			
	PUBLIC	EXIT_DRV_NOT_READY	
	PUBLIC	EXIT_WRIT_PROTECT	

;pointers
	PUBLIC	DirectorySector
	PUBLIC	FatSpace
	PUBLIC  FatSector
	PUBLIC  DirBuf
;	PUBLIC  HeaderBuf

	

; other
	PUBLIC	DeviceParameters
	PUBLIC	SavedParams
	PUBLIC	SwitchDevParams
	PUBLIC	Disk_Access
	PUBLIC	FormatPacket
	PUBLIC	bios
	PUBLIC	dos
	PUBLIC	command
IFDEF DBLSPACE_HOOKS
	PUBLIC	DblSpaceBin
ENDIF
	PUBLIC	Msg_Allocation_Unit_Val
	PUBLIC  DiskTable
	PUBLIC	ExitProgram
	PUBLIC	SEG_ADJ

;For FORPROC and FORMES modules

	PUBLIC	ClusSiz
	PUBLIC	InBuff
	PUBLIC	CrLf
	PUBLIC	Std_Printf
	PUBLIC	Drive
	PUBLIC	DriveLetter
	PUBLIC	PrintString

	EXTRN	CheckSwitches		:NEAR
	EXTRN	LastChanceToSaveIt	:NEAR
	EXTRN	VolId			:NEAR
	EXTRN	WriteBootSector 	:NEAR
	EXTRN	OemDone 		:NEAR
	EXTRN	AccessDisk		:NEAR
	EXTRN	Main_Init		:NEAR
	EXTRN	Read_Disk		:NEAR
	EXTRN	Write_Disk		:NEAR

; =======================================================================

DATA	SEGMENT PUBLIC	 PARA	 'DATA'

	EXTRN	BiosFile		:BYTE
	EXTRN	DosFile 		:BYTE
	EXTRN	SysSiz			:DWORD
	EXTRN	BioSiz			:DWORD
	EXTRN	UnformattedHardDrive	:BYTE
DATA	ENDS

; =======================================================================
;
; For FORPROC module
;
; =======================================================================

	EXTRN	FormatAnother?		:NEAR
;	EXTRN	Yes?			:NEAR
	EXTRN	report			:NEAR
	EXTRN	user_string		:NEAR

; =======================================================================

;No more SAFE module		
					; *rup 10-10-89
;	EXTRN	BuildRestoreFile	:NEAR
;	EXTRN	DoDirCopy		:NEAR
;	EXTRN	ClearDirSector		:NEAR
;	EXTRN	FatFixup		:NEAR
;	EXTRN	SwapFats		:NEAR
;	EXTRN	Hook_Int_24		:NEAR
;	EXTRN	Restore_Int_24		:NEAR
					; jh
; =======================================================================


;************************************************************************							
; =======================================================================
;
; Entry point to DOS format program.
;
; =======================================================================
;************************************************************************

Start:
	xor	BX,BX
	push	BX
	Set_Data_Segment
	mov	Org_AX,AX			; save AX on entry

	call	Main_Init

	mov	DX,SwitchMap			; save a copy of SwitchMap
	mov	SwitchCopy,DX

Main_Routine:				
	call	ShrinkMemory			; set memory requirements

; M033 - begin
; M031: With memory card, we cannot do a GetDefaultBPB prior to having
; inserted the media. This code should normally be put in glblinit.asm
; but version.inc is not included (Why?).

	cmp	CMCDDFlag, Yes
	jne	the_usual			; If not CMCDD, do the old logic

	call	Prompt_User_For_Disk		; Else ask the user to insert
						; the disk NOW
the_usual:
	call	Global_Init    			; allocate buffers, read in
						; system files if needed
	jnc	FatAllocated	      		; check for failure

FatalExiting:					; M031; just the label
	Message msgFormatFailure
	inc	NoDPChange			; Not necc. to modify drive
						; parameters if Global_Init failed

	jmp	FatalExit

FatAllocated:
SysLoop:
	mov	NoDPChange,0			; M004; assume we will restore
	call	InitSysParm 			; initialize some parameters
						; for each format iteration
	call	ZeroAllBuffers			; initialize buffers


	cmp	CMCDDFlag, Yes
	jne	the_usual2			; If CMCDD, let's make sure

	lea	DX, DeviceParameters		; Get the default drive parameters
						; (again!)
	mov	DeviceParameters.DP_SpecialFunctions, 0
	call	GetDeviceParameters
	jc	NotThisDisk

	mov	AX, DeviceParameters.DP_BPB.BPB_SectorsPerFat
	cmp	AX, SavedParams.DP_BPB.BPB_SectorsPerFat
	ja	NotThisDisk

	mov	AX, DeviceParameters.DP_BPB.BPB_RootEntries
	cmp	AX, SavedParams.DP_BPB.BPB_RootEntries
	jbe	go_on

NotThisDisk:
	Message msgFormatNotSupported
	jmp	SHORT FatalExiting

the_usual2:
	call	Prompt_User_For_Disk		; Else ask the user to insert the disk

go_on:

; M033- end

	call	Get_Disk_Access			; ensure disk access
	call	Phase1Initialisation		; determine deviceparameters
	jnc	@f

;	Message	msgFormatFailure		; m004; fatal error from Phase1
	inc	NoDPChange			; Not necc. to modify drive
						; parameters if Phase1 failed
	jmp	SHORT NextDisk			; M004; prompt for next disk

@@:
	call	DoSafe				; build recovery file if needed
; M005 - begin
	jnc	@f
	or	ax,ax				; is it failure write protect?
	jz	NextDisk
	jmp	SHORT ExitProgram		; terminate program (requested by user)

@@:
; M005 - end
	call	Phase2Initialisation		; determine starting points
	call	ShowFormatSize			; size being formatted message

;==========================================================================
;IF DEBUG_MODEL
;	jmp	 DrtFat				; If debugging don't
;						; need to really do format
;ENDIF
;==========================================================================

	call	Disk_Format_Proc
	jc	NextDisk			; Prompt for next disk if error

DrtFat:
	call	WriteDiskInfo			; write out the control information
;	jc	FatalExit			; M013; check for error
	cmp	ExitStatus,EXIT_NO		; does user want to continue?
	jz	ExitProgram			; terminate program

NextDisk:
	call	RestoreDevParm			; Restore device parameters
	call	More				; See if More disks to format
	jnc	SysLoop				; Continue if no carry

ExitProgram:
;	xor	BX,BX				; Prepare BX for IsRemovalbe call
;	mov	BL,Drive			; Get Drive number
;	inc	BL				; Make it 1 based
;	call	IsRemovable			; Do not restore original device
;	jnc	DoNotRestore			; parameters here, for removable media

;Now always do Restore Parameters


DoNotRestore:
	call	Format_Access_Wrap_Up		; Determine access status

	mov	AH,DISK_RESET			; Do a disk reset (flush buffers)
	int	21h	

	mov	AL,ExitStatus			; Get Errorlevel
	DOS_Call Exit				; Exit program
	int	20h				; If other exit	fails


FatalExit:
	Set_Data_Segment			; Ensure addressibility
	mov	ExitStatus,EXIT_FATAL
	call	RestoreDevParm			; Restore device parameters
	jmp	SHORT ExitProgram		; Perform normal exit




;=========================================================================
;  SHRINKMEMORY :	This procedure resizes the memory block allocated
;			to the format utility by calling Int 21H Function
;			4AH (74).  This is done in order to make room for
;			the FAT buffers.
;
;  CALLS :		none
;  CALLED BY :		Main
;  MODIFIES :		BX, ES, AH
;
;=========================================================================

ShrinkMemory	proc	near

	mov	BX,PSP_Segment			; Shrink to free space for Fat
	mov	ES,BX
	mov	BX,End_Of_Memory
	sub	BX,PSP_Segment
	Dos_Call Setblock
	ret

ShrinkMemory	endp

;=========================================================================
; Get_Disk_Access	 : This	routine	will determine the access state	of
;			   the disk.  If access is currently not allowed, it
;			   will be allowed by calling Set_Disk_Access_On_Off.
;
;
;	 Inputs	 : DX -	pointer	to buffer
;	 Outputs : Disk_Access.DAC_Access_Flag - 0ffh signals access allowed
;						 to the	disk previously.
;		   Access to the disk will be allowed.
;
;  CALLS :	Set_Disk_Access_On_Off
;  CALLED BY :  Main
;  MODIFIES :	Disk_Access.DAC_Access_Flag
;
;  M00x : This routine was re-worked for this modification
;
;=========================================================================

Procedure Get_Disk_Access

	push	AX				; Save regs
	push	BX
	push	CX
	push	DX

	mov	UnformattedHardDrive,FALSE	; Assume formatted disk
	mov	Disk_Access.DAC_Access_Flag, 0ffh; Assume we already have
						;  access to disk

	mov	AX,(IOCTL shl 8) or IOCTL_QUERY_BLOCK ; Check if function supported
	xor	BX,BX				; Clear BX
	mov	BL,Drive			; Get Drive letter
	inc	BL				; Make it 1 based
	mov	CX,(RAWIO shl 8) or Get_Access_Flag ; Determine disk access
	lea	DX,Disk_Access			; Point to parm list
 	int 	21h
	jc	gda_exit			;Not supported on carry

	mov	AX,(IOCTL shl 8) or Generic_IOCTL  ;Now can perform generic IOCtl call
	int	21h
	cmp	Disk_Access.DAC_Access_Flag,01h	; Access is currently allowed?
	jne	@f
	mov	Disk_Access.DAC_Access_Flag,0ffh; Mark that we already have
						;  access to disk
	jmp	short gda_exit
@@:
						; not previously allowed
	mov	UnformattedHardDrive,TRUE	; Won't do CheckExistingFormat
	inc	Disk_Access.DAC_Access_Flag	; signal disk access
	call	Set_Disk_Access_On_Off		; allow disk access

gda_exit:
	pop	DX				; Restore regs
	pop	CX
	pop	BX
	pop	AX
	ret
Get_Disk_Access ENDP

;===========================================================================
;
;  ZeroAllBuffers :	This procedure initializes all allocated buffers
;			by filling them with zeroes.
;
;  Buffers Modified :	DirectorySector
;			FatSpace
;			FatSector
;			xxxHeaderBufxxx (No more SAFE module)
;			DirBuf
;
;  Registers Modified:	AX,BX,CX,DI
;
;===========================================================================

ZeroAllBuffers	proc	NEAR

	Set_Data_Segment
	push	ES

	les	DI,DirectorySector	; ES:DI --> DirectorySector buffer
	mov	CX,DeviceParameters.DP_BPB.BPB_BytesPerSector
	call	ZeroBuffer

;---------------------------------------------------------------------------
;					; The size of FatSpace is given by the
;					; larger of the FAT, root directory and
;					; #clusters for 1.5K
;	les	DI,FatSpace  		
;	mov	AX,DeviceParameters.DP_BPB.BPB_BytesPerSector
;	mul	DeviceParameters.DP_BPB.BPB_SectorsPerFat
;	mov	BX,AX			; Save FAT size in bytes in BX
;
;	mov	AX,DeviceParameters.DP_BPB.BPB_RootEntries
;	mov	CL,5
;	shl	AX,CL			; Multiply by 32 to get total byte size
;
;	cmp	AX,BX			; now see which is bigger
;	ja	ChkClust		; Use root dir size if AX > BX
;
;UseFatSize:
;	mov	AX,BX			; FAT is bigger
;
;
;ChkClust:
;	mov	BX,AX			; Now BX contains the larger of the
;					; FAT and RootDir size, in bytes
;					; Calculate the byte size of the #
;					; clusters needed for 1.5K
;	mov	AX, DeviceParameters.DP_BPB.BPB_BytesPerSector
;	mov	CL,deviceParameters.DP_BPB.BPB_SectorsPerCluster
;	xor	CH,CH			; Sectors per cluster in CX
;	mul	CX			; DX:AX= bytes per cluster
;	mov	CX,AX			; CX = #bytes per cluster
;	mov	AX,1536			; AX = 1.5K
;	div	CX			; Calc. # clusters needed for 1.5K
;	or	DX,DX			; Non-zero remainder?
;	jz	Rounded			; No need to round up for zero remainder
;	inc	AX			; Increment #clusters needed, for non-zero remainder
;Rounded:
;	mul	CX			; Calculate byte size needed (#Clusters * BytesPerCluster)
;	cmp	BX,AX			; Is Max(FAT,RootDir) > Clusters in 1.5K?
;	jna	ZeroAsIs		; Clusters in 1.5K is bigger
;
;UseOldMax:
;	mov	AX,BX			; Clusters in 1.5K is smaller
;
;ZeroAsIs:
;	mov	CX,AX			; CX = bytes to zero out (size of FatSpace)
;	call	ZeroBuffer
;---------------------------------------------------------------------------

				; Not neccessary to init. FatSpace here since
				; this is done in DSKFRMT	
;	call	Fat_Init		; Fills FatSpace buffer with Fat_Init_Value

	les	DI,FatSector		; ES:DI --> FatSector buffer
	mov	CX,DeviceParameters.DP_BPB.BPB_BytesPerSector
	call	ZeroBuffer

;	les	DI,HeaderBuf		; ES:DI --> HeaderBuf buffer
;	mov	CX,deviceParameters.DP_BPB.BPB_BytesPerSector
;	add	CX,HEADER_SIZE
;	call	ZeroBuffer

	les	DI,DirBuf
	mov	CX,DeviceParameters.DP_BPB.BPB_BytesPerSector
	call	ZeroBuffer

	pop	ES

	ret
ZeroAllBuffers	endp

;=========================================================================
;
;  ZeroBuffer :		This procedure initializes buffer space to zero.
;			Note: This routine will not work for buffer sizes
;			greater than 64K, due to segment wrap.
;
;  Assumes     :	ES:DI gives location of buffer
;			CX = size of buffer in bytes
;  Modifies    :	AX, CX
;
;=========================================================================

ZeroBuffer	proc	near

	xor	AX,AX
	shr	CX,1		; Get buffer size in words
	rep	stosw

	ret
ZeroBuffer	endp

; ==========================================================================
;
; IsRemovable :  Determine if the Drive indicated in BX is removable or not.
;
;
;   Inputs:	 BX has	Drive (0=def, 1=A)
;   Outputs:	 Carry clear
;		     Removable
;		 Carry set
;		     not removable
;   Registers modified: DX
; ==========================================================================

IsRemovable PROC NEAR

	SaveReg <AX>
	mov	AX,(IOCTL shl 8) OR 8		; Rem media check
	int	21H
	jnc	CheckRemove

	mov	AX,(IOCTL shl 8) + 9		; Is it a NET Drive?
	int	21h
	jc	NotRemove			; Yipe, say non-removable

	test	DX,1000h
	jnz	NotRemove			; Is NET Drive, flag non-removeable
	jmp	SHORT IsRemove			; Is local, say removable

CheckRemove:
	test	AX,1
	jnz	NotRemove

IsRemove:
	clc
	RestoreReg <AX>
	return

NotRemove:
	stc
	RestoreReg <AX>
	ret

IsRemovable ENDP

;=========================================================================
; Set_Disk_Access_On_Off: This	routine	will either turn access	on or off
;			   to a	disk depending on the contents of the
;			   buffer passed in DX.

;	 Inputs	 : DX -	pointer	to buffer

;=========================================================================

Procedure Set_Disk_Access_On_Off

	push	AX				; Save regs
	push	BX
	push	CX
	push	DX

	xor	BX,BX				; Clear BX
	mov	BL,Drive			; Get Drive number
	inc	BL				; Make it 1 based
	call	IsRemovable			; See if removable media
	jnc	$$IF126 			; Not removable

	mov	AX,(IOCTL shl 8) or IOCTL_QUERY_BLOCK ; Check if function supported
	xor	BX,BX				; Clear BX
	mov	BL,Drive			; Get Drive letter
	inc	BL				; Make it 1 based
	mov	CX,(RAWIO shl 8) or Set_Access_Flag ; Allow access to disk
	int	21h
	jc 	$$IF126				; Not supported on carry
	mov	AX,(IOCTL shl 8) or Generic_IOCTL  ; Can now perform generic IOCTL
   	int	21h

$$IF126:
	pop	DX				; Restore regs
	pop	CX
	pop	BX
	pop	AX

	ret
Set_Disk_Access_On_Off ENDP


;=========================================================================
;
; DoSafe :   This procedure checks in case SWITCH_U is set.  If it
;	     isn't, the restoration file is created by calling Mirror.
;	     Before invoking Mirror it is necessary to de-allocate the
;	     FatSpace buffer.  This buffer has already been allocated so
;	     as to be big enough for all the buffers Mirror will require.
;
;	     The interface used with Mirror is as follows.
;		Passed In :	AX = MIRROR_SIGNATURE (5050h)
;				BL = 0-based target drive (0 = A)
;
;		Passed Out:	AL = Exit code from Mirror, as follows
;					0 - Success
;					4 - Physical error
;					5 - Logical error
;
; M005; if AL=4, AH=INT 26 return code
;
;	     If Mirror is successful the FatSpace buffer is re-allocated,
;	     but this time it is made only as big as the FAT.  Format
;	     then proceeds as normal.
;
;	     If Mirror is unsuccessful the user is asked whether to continue with
;	     an unconditional format (since the recovery file was not built).
;	     If he chooses to continue, the FatSpace buffer is re-allocated,
;	     Switch_U is set and Switch_Q is turned off, and Format proceeds
;	     as normal.  If he chooses not to continue, ExitStatus is set
;	     to EXIT_NO, and the program will be terminated upon returning
;	     from DoSafe.
;
;  Inputs :	None
;  Output :	NC --> Continue with Format
;		CY --> Exit program
;
;  Destroys:	Possibly all, except DS,ES which are set to DATA
;
;=========================================================================

DoSafe  proc	near

	assume	DS:DATA,ES:DATA
	test	SwitchMap,SWITCH_U		; Check	for unconditional format
	jz	BuildSafeFile			; Must do a safe format

	clc					; No recovery file needed
	ret					; Continue with Format

BuildSafeFile:
	mov	AH,Dealloc			; De-allocate FatSpace buffer
	mov	ES,WORD PTR FatSpace+2		; ES --> FatSpace segment
	assume	ES:NOTHING

	int	21h

	jnc	MemoryFree			; check for error here
	jmp	FatalExit			; de-allocation error occurred

MemoryFree:
	Message msgSavingUNFORMATInfo
	mov	AX,MIRROR_SIGNATURE		; Load interface parameters
	xor	BX,BX
	mov	BL,Drive			; BL = target drive (0 = A)

	call	Mirror				; invoke Mirror utility

	Set_Data_Segment
	assume	DS:DATA,ES:DATA

	or	AL,AL				; check exit code
	jz	GetMemory			; recovery file built
	
	cmp	ax,3*256+4			; M005;
	jz	WriteProtect

	Message	msgFileCreationError		; ask user whether to continue
	call	Yes?				;  with unconditional format

	pushf					; save flags for response
	Message	msgCrLf				; cursor on next line
	popf					; restore flags

	jnc	MakeUnconditional		; user wants to continue

	mov	ExitStatus,EXIT_NO		; set user-terminated exit code
	mov	ax,1				; M005; terminate format
DSFailed:
	stc					; signal termination required
	ret					; return to main routine

; M005 - begin
WriteProtect:
	Message msgCrLf
	Message msgCrLf
	mov	ax,13h				; Write protect error
	Extended_Message
	Message msgFormatFailure
	mov	ExitStatus,EXIT_FATAL		; M006;
	xor	ax,ax
	jmp	SHORT DSFailed
; M005 - end

MakeUnconditional:
			; allow continuation of quick format/track verify
			; PYS: but make it /u /q if /q
	test	SwitchMap,SWITCH_Q
	jz	GetMemory

	or	SwitchMap,SWITCH_U		; make unconditional
;	and	SwitchMap,not SWITCH_Q		; turn off Quick Format

GetMemory:
	mov	BX,Paras_Per_Fat		; BX = FAT size in paras

	mov	AH,Alloc			; allocate memory block
	int	21h

	jnc	MemoryAllocated			; check for error here
	jmp	FatalExit

MemoryAllocated:
	mov	WORD PTR FatSpace+2,AX		; save buffer pointer
	xor	AX,AX
	mov	WORD PTR FatSpace,AX

				; Not neccessary to init. FatSpace here since
				; this is done in DSKFRMT	
;	call	Fat_Init			; fill FatSpace buffer with zeroes

	clc					; signal to continue with Format
	ret

DoSafe	ENDP


;========================================================================
;
;  INITSYSPARM :	This procedure initializes parameters for each
;			iteration of the disk format process.
;
;  CALLS :	none
;  CALLED BY :	Main
;  MODIFIES :	SysSiz
;		SysSiz+2
;		ExitStatus
;		DblFlg
;		SwitchMap
;		sector_in_buffer
;		RWErrorCode
;		old_dir
;		DeviceParameters.DP_BPB (reset to SavedParams)
;
;========================================================================

InitSysParm	proc	near

	mov	WORD PTR SysSiz,0		; Must intialize for each
	mov	WORD PTR SysSiz+2,0		; iteration
	mov	BYTE PTR DblFlg,0
	mov	ExitStatus, EXIT_OK
	mov	DX,SwitchCopy			; restore original SwitchMap
	mov	SwitchMap,DX			; for each disk formatted
	mov	sector_in_buffer,0ffffh		; Initialize to force first read
	mov	RWErrorCode,0			; error code from reading disk
	mov	Old_Dir,FALSE
	mov	SpecialCase,FALSE		; used in WriteDiskInfo
						; Copy SavedParams into
						;  DeviceParameters
	push	DS				; copy DS into ES
	pop	ES

;	lea	SI,SavedParams.DP_BPB		; DS:SI --> source BPB
;	lea	DI,DeviceParameters.DP_BPB	; ES:DI --> dest. BPB
;	mov	CX,size a_BPB			; bytes to move

	mov	SavedParams.DP_SpecialFunctions,0 ; restore to original value
	mov	SI,OFFSET SavedParams		; DS:SI --> source parameters
	mov	DI,OFFSET DeviceParameters	; ES:DI --> dest. parameters
	mov	CX,SIZE A_DEVICEPARAMETERS	; bytes to move

	cld
	rep	movsb	

	ret
InitSysParm	ENDP


; ==========================================================================
; Calculate the size in bytes of the system rounded up to sector and
; cluster boundries, the store answer in SysSiz
; ==========================================================================

GetSize proc	 near

	call	GetBioSize
	call	GetDosSize
	call	GetCmdSize
IFDEF DBLSPACE_HOOKS
	call	GetDblSize
ENDIF
	return

GetSize endp

; ==========================================================================

GetBioSize proc near
	mov	AX,WORD PTR Bios.fileSizeInBytes
	mov	DX,WORD PTR Bios.fileSizeInBytes+2
	call	AddToSystemSize
	return
GetBioSize endp

; ==========================================================================

GetDosSize proc near

	mov	AX,WORD PTR dos.fileSizeInBytes
	mov	DX,WORD PTR dos.fileSizeInBytes+2
	call	AddToSystemSize
	return

GetDosSize endp

; ==========================================================================

GetCmdSize proc near
	 mov	 AX,WORD PTR command.fileSizeInBytes
	 mov	 DX,WORD PTR command.fileSizeInBytes+2
	 call	 AddToSystemSize
	 return
GetCmdSize endp

IFDEF DBLSPACE_HOOKS
; ==========================================================================
GetDblSize proc near
	 mov	 AX,WORD PTR DblSpaceBin.fileSizeInBytes
	 mov	 DX,WORD PTR DblSpaceBin.fileSizeInBytes+2
	 call	 AddToSystemSize
	 return
GetDblSize endp
ENDIF

; ==========================================================================
;
; Calculate the	number of Sectors used for the system
;
; Input:	DX:AX holds size to be added on
; Ouput:	Updated SysSiz variable
;
; ==========================================================================

	PUBLIC	 AddToSystemSize
AddToSystemSize proc near

	push	BX
	div	DeviceParameters.DP_BPB.BPB_BytesPerSector
	or	DX,DX
	jz	FNDSIZ0
	inc	AX			; Round up to next sector
FNDSIZ0:
	push	AX
	xor	DX,DX
	xor	BX,BX
	mov	bl, DeviceParameters.DP_BPB.BPB_SectorsPerCluster
	div	BX
	pop	AX
	or	DX,DX
	jz	OnClus
	sub	DX, BX
	neg	dx
	add	AX,DX			; Round up sector count to cluster
					; boundry
OnClus:
	mul	DeviceParameters.DP_BPB.BPB_BytesPerSector
	add	WORD PTR SysSiz,AX
	adc	WORD PTR SysSiz+2,DX
	pop	BX
	return

AddToSystemSize endp

; ==========================================================================
;
; ChkSpace - Check free space to see if there is enough room to load the
;	      system.
;	 On entry: DL =	Drive
;	 On exit:  carry flag set if not enough	room
;		   no other registers are affected
;
; ==========================================================================

Procedure ChkSpace
	push	AX			; Save	registers
	push	BX
	push	CX
	push	DX
	mov	AH,36h			; Get free space
	int	21h
					; 16 bit math okay here
					; no danger of overflow
	mul	CX			; Get bytes/cluster
	mov	CX,AX
	mov	AX,WORD PTR SysSiz	; Get # of bytes for system
	mov	DX,WORD PTR SysSiz+2
	div	CX			; Get # of clusters for system

	cmp	AX,BX			 ; Is there enough space?
	jbe	EnoughSpace		 ;  Y: Go clear	carry
	stc				 ;  N: Set carry
	jmp	short RestoreRegs

EnoughSpace:
	clc

RestoreRegs:
	pop	DX			 ; Restore registers
	pop	CX
	pop	BX
	pop	AX
	ret

ChkSpace endp

; ==========================================================================
;
;  More :	This procedure prompts the user for the formatting of
;		another disk.
;
;  Output  :	User wants to continue - CY clear
;		User wants to exit     - CY set
;
; ==========================================================================

More PROC NEAR
	
	mov	Formatted_Tracks_Low,0		; Reinit the track counter
	mov	Formatted_Tracks_High,0		; in case of another format

; Begin M035		
	cmp	CMCDDFlag, Yes			; If flash disk we don't
	jne	@f				; allow multiple formats
	stc					; so signal an exit
	jmp	SHORT Exit_More
@@:
;end m035
						; If exec'd from select, then
						; don't give user choice
	test	SwitchMap,(SWITCH_SELECT or SWITCH_AUTOTEST or SWITCH_BACKUP)
	jz	@F
	stc  					; flag automatic 'no' response
	jmp	SHORT Exit_More

@@:						; Would not want to format
						; another hard disk!
	cmp	DeviceParameters.DP_DeviceType,DEV_HARDDISK
	jnz	NotAHardDisk
	stc  					; flag automatic 'no' response
	jmp	SHORT Exit_More

NotAHardDisk:
	call	FormatAnother?			; Get yes or no	response
        pushf                                   ; Save the result
        call    CrLf                            ; One new line
        popf                                    ; Get the result back
	jnc	WantsToContinue
;M002; Do not change ExitStatus
	stc
	jmp	SHORT Exit_More

WantsToContinue:
	call	CrLf

						; M033
	cmp	CMCDDFlag, Yes
	jne	more_standard_exit

	call	Prompt_User_For_disk		; If FLASH, we ask the
						; disk here if yes to more

more_standard_exit:

	clc

Exit_More:
	ret

More ENDP

; ==========================================================================
;
;  RestoreDevParm :	This procedure will prepare for exiting the program
;			by restoring the device parameters to their original
;			value.
;			Note: A call to SetDeviceParameters has the following
;			results:
;			With bit 0 of the SpecialFunctions byte SET,
;				BPB in parameter block is copied into BDS_BPB
;			With bit 0 of the SpecialFunctions byte RESET,
;				BPB in parameter block is copied into BDS_RBPB
;
; ==========================================================================

RestoreDevParm	proc	near

; M034 - begin
	cmp	CMCDDFlag, Yes			; This extra set_DPB would
	je	EndRestoreDevParm		; make the current card have
						; the size of the first card.
; M034 - end

	test	ValidSavedDeviceParameters, 0ffH
	jz	EndRestoreDevParm

	cmp	ExitStatus,EXIT_FATAL
	jnz	Non_Fatal			; NZ --> ExitStatus!=EXIT_FATAL

	test	NoDPChange,0ffh			; Check if drive parameters should not be modified
	jnz	Non_Fatal			; NoDPChange=1 --> do not modify

			; For a Fatal exit, it is necessary to reset the
			; BDS_BPB to the default settings, since it may have
			; been set to an invalid value

	mov	SavedParams.DP_TrackTableEntries,0	; There is no track layout info in SavedParams
	mov	SavedParams.DP_SpecialFunctions,INSTALL_FAKE_BPB or TRACKLAYOUT_IS_GOOD

	lea	DX, SavedParams
	call	SetDeviceParameters

	mov	FormatPacket.FP_SpecialFunctions,STATUS_FOR_FORMAT
	mov	AX,(IOCTL shl 8) or GENERIC_IOCTL
	mov	BL,Drive
	inc	BL
	mov	CX,(RAWIO shl 8) or FORMAT_TRACK
	lea	DX,FormatPacket
	int	21h

Non_Fatal:
	mov	SavedParams.DP_TrackTableEntries,0	; There is no track layout info in SavedParams
	mov	SavedParams.DP_SpecialFunctions,TRACKLAYOUT_IS_GOOD
	lea	DX, SavedParams
	call	SetDeviceParameters

EndRestoreDevParm:
	ret

RestoreDevParm	endp

;==========================================================================
;
;  SysPrm :	This procedure prompts the user for a system diskette
;		in the default drive.
;
;===========================================================================

SysPrm	proc	near

	mov	AH,GET_DEFAULT_Drive		; Find out the default Drive
	int	21h				; Default now in AL
	mov	BL,AL
	inc	BL				; A = 1
	add	AL,41h				; Now in Ascii
	mov	SystemDriveLetter,AL		; Text now ok
	call	IsRemovable
	jnc	DoPrompt

		; Media is non-removable. Switch sys disk to Drive A.
		; Check, though, to see if Drive A is removable too.

	 mov	 AL,"A"
	 mov	 BYTE PTR [SystemDriveLetter],AL
	 mov	 [BiosFile],AL
	 mov	 [DosFile],AL
	 mov	 [CommandFile],AL
IFDEF DBLSPACE_HOOKS
	 mov	 [DblSpaceFile], al
ENDIF
	 mov	 BX,1
	 call	 IsRemovable
	 jnc	 DoPrompt
	 Message msgNoSystemFiles

	 jmp	 FatalExit

DoPrompt:
	 mov	 AL, SystemDriveLetter
	 sub	 AL, 'A'
	 call	 AccessDisk
	 Message msgInsertDOSDisk
	 Message ContinueMsg

	 call	 USER_STRING			; Wait for a key
	 call	 CrLf
	 call	 CrLf

         ret
SysPrm	endp
;===========================================================================

ControlC_Handler:
	mov	AX, seg data
	mov	DS, AX
	Message msgInterrupt
	mov	ExitStatus, EXIT_CTRLC

				; Restore original Device Settings, as would
				; be done after completion of normal format

; M034 - begin
	cmp	CMCDDFlag, Yes		; This extra set_DPB would
	je	GotoExitProgram		; make the current card have
					; the size of the first card.

					; Note that this one is less critical
					; than the one in RestoreDevParams
					; (the disk is probably non-functional
					; anyway).
; M034 - end

	mov	SavedParams.DP_TrackTableEntries,0	; There is no track layout info in SavedParams
	mov	SavedParams.DP_SpecialFunctions,TRACKLAYOUT_IS_GOOD
	lea	DX, SavedParams
	call	SetDeviceParameters

GotoExitProgram:
	jmp	ExitProgram


CrLf:
	mov	DX,offset msgCrLf		; CR,LF	added to message
PrintString:
Std_Printf:
	call	Display_Interface
	return

;M018 - begin
;----------------------------------------------------------------------------
;
; Procedure Name : DIV32 (borrowed from dos\disk3.asm)
;
; Inputs:
;       DX:AX = 32 bit dividend   BX= divisor
; Function:
;       Perform 32 bit division:  DX:AX/BX = CX:AX + DX (rem.)
; Outputs:
;       CX:AX = quotient , DX= remainder
; Uses:
;       All registers except AX,CX,DX preserved.
;----------------------------------------------------------------------------
Div32	proc	near

	mov	cx,ax		; Save least significant word
	mov	ax,dx
	xor	dx,dx
	div	bx		; 0:AX/BX
	xchg	cx,ax		; Restore least significant word and save
				; most significant word
	div	bx		; DX:AX/BX
	ret

Div32	endp
;M018 - end

; ==========================================================================
;
;    Phase2Initialisation:
;	 Use device parameters to build	information that will be
;	 required for each format
;
;    Algorithm:
;	 Calculate first head/cylinder to format
;	 Calculate number of tracks to format
;	 Calculate the total bytes on the disk and save	for later printout
;	 First initialise the directory	buffer
;
; ==========================================================================

Phase2Initialisation proc near
					; Calculate first track/head to format
					; (round up - kludge)
	mov	AX, DeviceParameters.DP_BPB.BPB_HiddenSectors
	mov	DX, DeviceParameters.DP_BPB.BPB_HiddenSectors + 2
	add	AX, DeviceParameters.DP_BPB.BPB_SectorsPerTrack
	adc	DX, 0
	dec	AX
	sbb	DX, 0

;M018	div	DeviceParameters.DP_BPB.BPB_SectorsPerTrack
;M018	xor	DX,DX

;M018 - begin
	mov	BX,DeviceParameters.DP_BPB.BPB_SectorsPerTrack
	call	Div32
	mov	DX,CX			; Forget remainder. DX:AX= tracks*heads
					; before M018 we were assuming tracks*head
					; fitted in a word.
;M018 - end

	div	DeviceParameters.DP_BPB.BPB_Heads

	mov	FirstCylinder,	AX
	mov	FirstHead, DX
					; Calculate the total number of tracks
					; to be formatted (round down - kludge)
	mov	AX, DeviceParameters.DP_BPB.BPB_TotalSectors
	xor	DX,DX
					; if (TotalSectors == 0) then use
					; BigTotalSectors
	or	AX,AX
	jnz	NotBigTotalSectors
	mov	AX, DeviceParameters.DP_BPB.BPB_BigTotalSectors
	mov	DX, DeviceParameters.DP_BPB.BPB_BigTotalSectors + 2

NotBigTotalSectors:
;M018	div	DeviceParameters.DP_BPB.BPB_SectorsPerTrack
;M018	mov	TracksPerDisk,	AX

;M018 - begin
	mov	BX,DeviceParameters.DP_BPB.BPB_SectorsPerTrack
	call	div32
	mov	word ptr TracksPerDisk, AX
	mov	word ptr TracksPerDisk+2, CX
;M018 - end

	mov	AX, DeviceParameters.DP_BPB.BPB_BytesPerSector
	xor	DX, DX
	mov	BX, size dir_entry
	div	BX
	mov	CX, AX

	les	BX, DirectorySector
					; If Old_Dir =	TRUE then put the first
					; letter of each must be 0E5H
	xor	AL, AL
	cmp	old_Dir, TRUE
	jne	StickE5
	mov	AL, 0e5H

StickE5:
	mov	ES:[BX], AL
	add	BX, size dir_entry
	loop	stickE5

	ret

Phase2Initialisation endp

;========================================================================
;
; ShowFormatSize :	This procedure calculates the size of the disk
;			being formatted, and displays an appropriate
;			message.
;
; Strategy :	The total number of bytes on the volume are first calculated.
;		This is converted to K by dividing by 1024.  If the number
;		is less than 1000, the size in K is printed.  Otherwise
;		the number is converted to Megs, as follows.  If size in K
;		is less than 10,000
;			Megs = Kbytes / 1000
;		else
;			Megs = Kbytes / 1024
;		Nonzero decimals will be printed for megs.
;
; Calls :	Multiply_32_Bits
;
; Registers Destroyed :	AX,BX,CX,DX
;
;
;========================================================================

ShowFormatSize	proc	near

	mov	CX,DeviceParameters.DP_BPB.BPB_BytesPerSector
	xor	DX,DX
	mov	AX,DeviceParameters.DP_BPB.BPB_TotalSectors
	or	AX,AX				; If zero, use BigTotalSectors
	jnz	UseSmall

UseBigSectors:
	mov	AX,DeviceParameters.DP_BPB.BPB_BigTotalSectors
	mov	BX,DeviceParameters.DP_BPB.BPB_BigTotalSectors[2]
	call	Multiply_32_Bits
	jnc	NoOverflow
	ret

NoOverflow:
	mov	DX,BX				; Now DX:AX has total bytes
	jmp	SHORT DoDivide

UseSmall:
	mul	CX				; Now DX:AX has total bytes

DoDivide:
	mov	CX,10				; Set up shift count
Div1024:
	shr	DX,1   				; Rotate DX:AX
	rcr	AX,1
	loop	Div1024					

	cmp	AX,999				; Check if DX:AX >= 1000
	ja	GetMegs
	or	DX,DX				; DX nonzero --> Very big number!
	jnz	GetMegs

	mov	SizeInK,AX
	mov	dx,offset data:msgQuickFormatShowKBytes
	test	SwitchMap,SWITCH_Q
	jnz	GotoDisplaySize
	mov	dx,offset data:msgShowKBytes
	test	SwitchMap,SWITCH_U
	jnz	GotoDisplaySize
	mov	dx,offset data:msgVerifyShowKBytes

GotoDisplaySize:
	jmp	DisplaySize	

GetMegs:
	cmp	AX,10000			; Check if DX:AX > 10000
	ja	UseRealMegs
	or	DX,DX
	jnz	UseRealMegs

UseFloppyMegs:
	mov	BX,1000				; DX:AX <= 10000
	div	BX				; Get size in Megs
	jmp	SHORT DoneDivision

UseRealMegs:
	mov	BX,1024				; DX:AX > 10000
	div	BX				; Get size in Megs

;M003 - begin
	push	AX				; bring reminder
	mov	AX,DX				; to 1/1000 instead
	xor	DX,DX				; of 1/1024
	mov	BX,250				; multiply by 250
	mul	BX
	mov	DH,DL				; divide by 256
	mov	DL,AH
	pop	AX
;M003 - end
	
DoneDivision:
	mov	SizeInM,AX
	cmp	DX,10				; Check for nonzero decimals
	jnb	ShowDecimals
	mov	dx,offset data:msgQuickFormatShowMBytes
	test	SwitchMap,SWITCH_Q
	jnz	DisplaySize
	mov	dx,offset data:msgShowMBytes
	test	SwitchMap,SWITCH_U
	jnz	DisplaySize
	mov	dx,offset data:msgVerifyShowMBytes
	jmp	short DisplaySize	

;M007 - begin

ShowDecimals:
	mov	AX,DX				; set up for division of
	xor	DX,DX				;  remainder by 10
	mov	BX,10
	div	BX
	mov	CX,AX
	xor	DX,DX
	div	BX
	or 	DX,DX				; Do not display 2d.p.s if 2nd d.p. is zero
	jnz	TwoDecPlaces			; Must display 2 d.p.

OneDecPlace:
	mov	DecSizeInM,AX			; Display only 1 d.p.
	mov	msgDecimalNumberofDecimal,1
	jmp	SHORT ShowDecMessage

TwoDecPlaces:
	mov	DecSizeInM,CX			; Display original 2 d.p.s
	mov	msgDecimalNumberofDecimal,2

ShowDecMessage:
	mov	dx,offset data:msgQuickFormatDecimalMBytes
	test	SwitchMap,SWITCH_Q
	jnz	DisplaySize
	mov	dx,offset data:msgDecimalMBytes
	test	SwitchMap,SWITCH_U
	jnz	DisplaySize
	mov	DX,offset data:msgVerifyDecimalMBytes

DisplaySize:
	call	Display_Interface
	ret

;M007 - end

ShowFormatSize	endp

;=========================================================================
; WRITE_Fat	 :	 This routine writes the logical sector	count requested.
;			 It will write a mAXimum of 40h	Sectors.  If More
;			 than 40h exists it will continue looping until
;			 all Sectors have been written.
;
;	 Inputs	 :	 AL - Drive letter
;			 DS:BX - Segment:offset	of transfer address
;			 CX - Sector count
;			 DX - 1st. sector
;
;	 Outputs :	 Logical Sectors written
;=========================================================================

procedure write_Fat

	mov	CX, ES:DeviceParameters.DP_BPB.BPB_SectorsPerFat

;	$do					; while Sectors left
$$DO67:
	cmp	CX,00h				; any Sectors?
	je	$$EN67				; no

  	cmp	CX,40h
	jna	$$IF69				; yes

	push	CX				; save CX
	mov	CX,40h
	push	AX				; save AX
	call	write_disk			; write it
	pop	AX				; restore AX
	pop	CX				; restore CX
	jc	Write_Exit			; exit if fail

	mov	SI,8000h
	call	seg_adj				; adjust segment
	mov	BX,SI				; new offset
	add	DX,40h
	sub	CX,40h
	jmp	SHORT $$EN69			; < 64k

$$IF69:
	push	AX				; save AX
	call	write_disk			; write it
	pop	AX				; restore AX
	xor	CX,CX				; set CX to 0 - last read

$$EN69:
	jmp	SHORT $$DO67

$$EN67:
	Write_Exit:
	ret

write_Fat ENDP

;=========================================================================
; SEG_ADJ	 :	 This routine adjusts the segment:offset to prevent
;			 address wrap.
;
;	 Inputs	 :	 SI - Offset to	adjust segment with
;			 DS - Segment to be adjusted
;
;	 Outputs :	 SI - New offset
;			 DS - Adjusted segment
;=========================================================================

procedure seg_adj

	push	AX
	push	BX
	push	DX

	mov	AX,SI				; get offset
	mov	BX,0010h			; 16
	xor	DX,DX				; clear DX
	div	BX				; get para count
	jnc	$$IF73				; overflow?
	adc	BX,0				; pick it up

$$IF73:
	mov	BX,DS				; get seg
	add	BX,AX				; adjust for paras
	mov	DS,BX				; save new seg
	mov	SI,DX				; new offset

	pop	DX
	pop	BX
	pop	AX
	ret

seg_adj  ENDP

; =========================================================================
;	 format	is done... so clean up the disk!
; =========================================================================

Done PROC NEAR


	call	OemDone
	return

Done	 ENDP


; =========================================================================
;	PrintErrorAbort:
;	 Print an error	message	and abort
;
;    Input:
;	 DX - Pointer to error message string
; =========================================================================

PrintErrorAbort PROC NEAR

	push	DX
	call	CrLf
	pop	DX
	call	PrintString
	jmp	FatalExit

PrintErrorAbort ENDP





;=========================================================================
; Ctrl_Break_Write	 : This	routine	takes the control break	request
;			   an returns.	In essence, it disables	the CTRL-BREAK.
;			   This	routine	is used	during the writing of the
;			   Fat,	DIR, and SYSTEM.
;
;=========================================================================

Ctrl_Break_Write:

	iret					; return to caller


;=========================================================================
; Ctrl_Break_Save	 : This	routine	gets the current vector	of
;			   int 23h and saves it	in CTRL_BREAK_VECTOR.
;	 Inputs	 : none
;
;	 Outputs : CTRL_BREAK_VECTOR - holds address of	int 23h	routine
;=========================================================================

Ctrl_Break_Save PROC NEAR

	push	ES
	push	BX
	push	AX

	mov	AX,3523h			; Get CTRL-BREAK
						; Interrupt vector
	int	21h

	mov	WORD PTR Ctrl_Break_Vector,BX	; Get vector offset
	mov	WORD PTR Ctrl_Break_Vector+2,ES ; Get vector segment

	pop	AX
	pop	BX
	pop	ES

	ret


Ctrl_Break_Save ENDP

;=========================================================================
; Set_Ctrl_Break	 : This	routine	sets the CTRL-Break vector to one
;			   defined by the user.
;
;	 Inputs	 : none
;
;	 Outputs : CTRL_BREAK_VECTOR - holds address of	int 23h	routine
;=========================================================================

Set_Ctrl_Break PROC NEAR

	push	DS				; Save ds
	push	AX				; Save AX
	push	BX				; Save BX
	push	DX				; Save DX

	push	CS				; Swap cs with DS
	pop	DS				; Point to code seg

	mov	DX,offset Ctrl_Break_Write	; Get interrupt vec.
	mov	AX,2523h			; Set CTRL-BREAK
						; Interrupt vector
	int	21h

	pop	DX				; Restore DX
	pop	BX				; Restore BX
	pop	AX				; Restore AX
	pop	DS				; Restore DS

	ret

Set_Ctrl_Break ENDP

;=========================================================================
; Reset_Ctrl_Break	 : This	routine	resets the CTRL-Break vector to	that
;			   originally defined.

;	 Inputs	 : CTRL_BREAK_VECTOR - holds address of	int 23h	routine

;	 Outputs : none
;=========================================================================

Reset_Ctrl_Break PROC NEAR

	push	DS
	push	AX
	push	BX
	push	DX

	mov	AX,WORD PTR Ctrl_Break_Vector+2 ; Get seg. of vector
	mov	BX,WORD PTR Ctrl_Break_Vector	; Get off. of vector
	mov	DS,AX				; Get seg.
	mov	DX,BX				; Get off.
	mov	AX,2523h			; Set CTRL-BREAK
						; Interrupt vector
	int	21h

; ========================================================================
;IF SAFE
;	call	Restore_Int_24			; Restore int 24h handler
;ENDIF
; ========================================================================

	pop	DX
	pop	BX
	pop	AX
	pop	DS

	ret


Reset_Ctrl_Break ENDP


; =========================================================================
; Get_PSP_Parms
; =========================================================================

Procedure Get_PSP_Parms

	Set_Data_Segment
	mov	AX,PSP_Segment			; Get segment of PSP
	mov	DS,AX

	assume	DS:notHING			; Setup segment of Environment
	mov	AX,DS:PSP_Environ		; string, get from PSP
	mov	ES:Environ_Segment,AX
	Set_Data_Segment
	ret

Get_PSP_Parms ENDP


;=========================================================================
; Cap_Char	 : This	routine	will capitalize	the character passed in
;		   DL.
;
;	 Inputs	 : DL -	Character to be	capitalized
;
;	 Outputs : DL -	Capitalized character
;=========================================================================

Procedure Cap_Char

	push	AX				; Save AX
	mov	AX,6520h			; Capitalize character
	int	21h
	pop	AX				; Restore AX
	ret

Cap_Char ENDP


;=========================================================================
;
; Set_CDS_Off			 - This	routine	disallows access to a
;				   disk	if a format fails on a non-Fat
;				   formatted disk.
;
;=========================================================================

Procedure Set_CDS_Off

	push	AX				; Save regs
	push	DX

	mov	AX,5f08h			; Reset CDS
	mov	DL,Drive			; Drive to reset
	int	21h

	pop	DX				; Restore regs
	pop	AX

	ret

Set_CDS_Off ENDP


;=========================================================================
;
; Format_Access_Wrap_Up	 -	This routine determines whether or
;				not access should be allowed to the
;				disk based on the exit Status of
;				format.
;
;=========================================================================

Procedure Format_Access_Wrap_Up

	cmp	Disk_Access.DAC_Access_Flag,0ffh ; Access prev. allowed?
	je	$$IF140 			; No

	cmp	ExitStatus,EXIT_OK		; Good exit?
	je	$$IF141 			; No
	cmp	ExitStatus,EXIT_NO		; User said no?
	je	$$IF141 			; No

	lea	DX,Disk_Access			; Point to parm block
	mov	Disk_Access.DAC_Access_Flag,00h ; Signal no disk access
	call	Set_Disk_Access_On_Off		; Don't allow disk access
	jmp	SHORT $$EN141			; Bad exit

$$IF141:
	lea	DX,Disk_Access			; Point to parm block
	mov	Disk_Access.DAC_Access_Flag,01h ; Signal disk access
	call	Set_Disk_Access_On_Off		; Allow disk access

$$EN141:
$$IF140:
	cmp	Fat_Flag,No			; Non-Fat format?
	jne	$$IF145 			; Yes

	cmp	ExitStatus,EXIT_OK		; Good exit?
	je	$$IF146 			; No

	call	Set_CDS_Off			; Disallow Fat access

$$IF146:
$$IF145:
	ret

Format_Access_Wrap_Up ENDP

;=====================================================================
;
;  FindNClusters :	This procedure will search through the FAT on
;			disk to find N consecutive free clusters.
;
;			N = number of clusters required to allow space for
;			    a file of size 1.5Kbytes (i.e. IO.SYS) e.g. with
;			    2 sectors/cluster, N=2.
;
;			If these are found, a disk reset will be performed
;			and disk allocation will be forced to start from
;			the start of these cluster blocks.  This is to ensure
;			that when the system files are written out, IO.SYS
;			(which is written first), will  be contiguous, and
;			hence the disk can be bootable.
;
;  Arguments :	NumClusters = N
;
;  Returns   :  NC --> Success
;		CY --> Failure  (no action taken)
;
;  Calls     :	calc_sector_and_offset
;		ReadFatSector
;		GetFatSectorEntry
;
;  Destroys  :	AX,BX,CX,DX,DS
;
;=======================================================================

FindNClusters	proc	NEAR

	mov	AX,DATA
	mov	ES,AX				; ES --> DATA
	assume	DS:NOTHING,ES:DATA

	mov	ES:FoundN,FALSE			;initialize flag
	mov	ES:Cluster,2			;search from cluster 2
	mov	ES:sector_in_buffer,0ffffh	;force first FAT read

Search:	mov	AX,ES:Cluster			;load current cluster
	call	calc_sector_and_offset		;find location in FAT

	mov	BX,ES:sector_in_buffer
	mov	AX,ES:sector_to_read
	cmp	AX,BX				;is required sector in buffer?
	jz	IsInBuffer

	call	ReadFatSector			;read it in
	jnc	IsInBuffer			;check for error
	jmp	ErrorExit			;abort if error

IsInBuffer:
	lds	SI,ES:FatSector			;DS:SI --> FAT buffer
	call	GetFatSectorEntry		;AX = cluster value

	or	AX,AX				;is this cluster free?
	jnz	FreeNotFound			;nonzero --> not free

	mov	ES:FoundN,TRUE			;initialize to success
	mov	CX,ES:NumClusters		;CX = N, loop control variable

CheckFreeChain:
	mov	AX,ES:Cluster			;load current cluster
	push	CX				;preserve CX!
	call	calc_sector_and_offset		;find location in FAT
	pop	CX				;restore CX

	mov	BX,ES:sector_in_buffer		;N cluster block may overlap FAT sectors
	mov	AX,ES:sector_to_read
	cmp	AX,BX				;is required sector in buffer?
	jz	IsInBuffer2

	push	CX				;preserve CX!
	call	ReadFatSector			;read it in
	pop	CX				;restore CX
	jc	ErrorExit			;abort if error

IsInBuffer2:
	lds	SI,ES:FatSector			;DS:SI --> FAT buffer
	push	CX				;preserve CX!
	call	GetFatSectorEntry		;AX = cluster value
	pop	CX				;restore CX

	or	AX,AX				;is this cluster free?
	jnz	FreeChainTooSmall		;did not find N consecutive free clusters

	inc	ES:Cluster			;check next consecutive cluster
	loop	CheckFreeChain			;check for N consecutive free clusters

	jmp	SHORT CheckExitState

FreeChainTooSmall:
	mov	ES:FoundN,FALSE			;not enough free consecutive clusters
	jmp	SHORT CheckExitState		;terminate loop now

FreeNotFound:
	inc	ES:Cluster			;advance current cluster

CheckExitState:
	cmp	ES:FoundN,TRUE			;did we succeed yet?
	jz	Success				;yes, set NC

	mov	AX,ES:Cluster
	cmp	AX,ES:TotalClusters		;no more clusters?
	ja	Failure				;M015; reached last cluster, set CY

	jmp	Search				;search until found or clusters exhausted

Success:
				;at this point we have
				;starting cluster of free block = Cluster - N

	mov	AH,DISK_RESET			;DOS disk reset function 0dh
	int	21h				;reset disk to flush all buffers
	jc	ErrorExit			;check for errors

	mov	DL,ES:Drive			;get drive number
	inc	DL				;convert to DOS drive (1=A)
	mov	AH,GET_DPB			;undocumented DOS function
	int	21h
	cmp	AL,-1				;check for errors
	jz	ErrorExit

	mov	AX,ES:Cluster			;find start of free cluster block
	sub	AX,ES:NumClusters

	mov	[BX.dpb_next_free],AX		;start allocation from this cluster
	clc
	jmp	SHORT ExitFindN

Failure:
ErrorExit:
	stc

ExitFindN:
	ret

FindNClusters	endp



;=========================================================================
;
;  WriteDiskInfo :	This procedure writes out all the control info to
;			the disk, after it has been formatted/verified/
;			quick formatted.  This includes the Boot Sector,
;			Root Directory and FAT.  If /s is present System
;			files will also be written out, if there is enough
;			disk space.
;
;  STRATEGY :		If a safe format is being done (/U not present), it
;			is not necessary to have a directory entry for the
;			recovery file.  A special case arises when a safe
;			format is being done, and /S is present (system
;			required).  In this case it is necessary to write out
;			the system files with the old FAT intact, so as to
;			prevent over-writing any old files.  The FAT chains
;			must then be copied to the new FAT, which is then
;			written out to disk.
;
;  DESTROYS :		AX,BX,CX,DX,SI,DI
;
;=========================================================================

WriteDiskInfo	proc	NEAR

	Set_Data_Segment			;DS,ES = DATA
	assume	DS:DATA,ES:DATA

	test	SwitchMap,SWITCH_S		;if system requested, calculate size
	jz	Cleared

	test	SwitchMap,SWITCH_U		;check for not(/U) & /S combination
	jnz	@F				;normal case

	mov	SpecialCase,TRUE		;special case of not(/U) & /S

@@:	cmp	BYTE PTR DblFlg,0		;is sys space already calculated?
	jnz	Cleared				;yes

	inc	BYTE PTR DblFlg			;no --	set the	flag
	call	GetSize				;calculate the	system size

Cleared:
	call	Ctrl_Break_Save			;disallow Ctrl_C here
	call	Set_Ctrl_Break

	call	WriteBootSector			;write out Boot Sector
	jnc	BootSectorOk			;check for error

	call	Reset_Ctrl_Break		;error occurred
;	Message	msgDiskUnusable			; M013
	jmp	Problems

BootSectorOk:
	push	DS				;preserve DS
	lds	BX,DirectorySector		;set up for call
	call	ClearDirSector			;fill root dir sector with zeroes
	pop	DS				;restore Ds

	call	WriteRootDir			;write out Root Directory
	jnc	RootDirOk			;check for error

	call	Reset_Ctrl_Break		;error occurred
	Message	msgDirectoryWriteError
	jmp	Problems

RootDirOk:
	cmp	SpecialCase,TRUE
	jz	@F				;don't write FAT yet for special case

DestroyOldFat:
	call	WriteFat			;write out FAT
	jnc	FatOk				;check for error

	call	Reset_Ctrl_Break		;error occurred
	Message	msgFatWriteError
	jmp	Problems

@@:
FatOk:
						;restore good tracklayout for drive
	mov	SavedParams.DP_TrackTableEntries,0
	mov	SavedParams.DP_SpecialFunctions,TRACKLAYOUT_IS_GOOD
	lea	DX,SavedParams
	call	SetDeviceParameters

				;now perform an undocumented GET_DPB call to
				;force allocation to be reset from the start
				;of the disk, and force free disk space to be
				;calculated

	push	DS				;preserve DS

	mov	DL,Drive			;set up for function call
	inc	DL				;convert to DOS drive (1=A)
	mov	AH,GET_DPB
	int	21h
	cmp	AL,-1				;check for error
	jnz	GotDpb

	pop	DS				;error occurred, restore DS here
	call	Reset_Ctrl_Break
	Message	msgDiskUnusable
	jmp	Problems

GotDpb:	mov	[BX.dpb_next_free],0		;reset allocation to start of disk
	mov	[BX.dpb_free_cnt],-1		;force free space to be computed

	pop	DS				;restore DS

	test	SwitchMap,SWITCH_S		;is system desired?
	jnz	@F				;yes, transfer it
	jmp	ResetCtrlBreak			;no, go finish up

@@:	mov	DL,Drive			;system is required, set up for call
	inc	DL				;DL = drive code (1 = A)
	call	ChkSpace			;check if there's enough space
	jnc	SpaceOK				;  Y: Go load system files

NoRoom:	cmp	SpecialCase,TRUE		;is SAFE format being done?
	jnz	DiskTooSmall			;no - disk is actually too small

						;there is no space for system
						; due to old disk files
	Message	msgSysWarning			;ask user for choice
	call	Yes?
	
	pushf	       				;save flags
	Message	msgCrLf				;cursor to next screen line
	popf					;restore flags
	
	jc	SaysNo				;user entered "No"

						;user wants unconditional sys transfer
	mov	SpecialCase,FALSE		;write out sys as for unconditional format
	jmp	DestroyOldFat			;write out new FAT & system files

SaysNo:	mov	ExitStatus,EXIT_NO		;terminate Format after this disk
						; (since sys size is zeroed out)
	jmp	SHORT CancelSystem		;skip the message below
	
DiskTooSmall:					;disk is physically too small
	Message msgNoRoomDestDisk		;  N: Print error message

CancelSystem:
	mov	WORD PTR SysSiz+2,0		;no system transferred
	mov	WORD PTR SysSiz,0		;reset system sizes to zero

	xor	AX,AX
	mov	word ptr [Dos.FileSizeInBytes+0],AX	;get low word
	mov	word ptr [Dos.FileSizeInBytes+2],AX	;get high word
	mov	word ptr [Bios.FileSizeInBytes+0],AX	;get bios size
	mov	word ptr [Bios.FileSizeInBytes+2],AX
	mov	word ptr [Command.FileSizeInBytes+0],AX ;get command size
	mov	word ptr [Command.FileSizeInBytes+2],AX

IFDEF DBLSPACE_HOOKS
	mov	word ptr [DblSpaceBin.FileSizeInBytes+0], ax	; clr dblspace
	mov	word ptr [DblSpaceBin.FileSizeInBytes+2], ax	;   size
ENDIF

	cmp	SpecialCase,TRUE		;is this a special case?
	jz	WriteFatNow			;yes, write FAT after sys transfer
	jmp	ResetCtrlBreak			;no, FAT is already written

WriteFatNow:
	call	WriteFat			;write out FAT now, since no space for system files
	jc	FatError			;check for error
	jmp	ResetCtrlBreak			

FatError:
	call	Reset_Ctrl_Break		;error occurred
	Message	msgFatWriteError
	jmp	Problems

SpaceOK:
	cmp	SpecialCase,TRUE		;is this a special case?
	jnz	WriteSys			;no, go ahead and transfer system

	call	ComputeN			;determine value of NumClusters

	cmp	NumClusters,1			;do we need only 1 cluster for IO.SYS?
	jz	@F				;yes, no need to find contiguous clusters

	push	DS				;preserve DS
	call	FindNClusters			;search for N contiguous clusters
						; and force allocation to start from there
	pop	DS				;restore DS
	jnc	WriteSys			;NC --> successful
	jmp	NoRoom				;CY --> unsuccessful.  Give
						; message & write FAT

@@:
WriteSys:
	mov	AL,Drive
	call	AccessDisk			;note what is current logical drive

;				; this func. was used to call writedos in a loop
;	call	WriteSysFiles
;
	push	DS				; preserve DS & ES!
	push	ES

	call	WriteDos			;write	the BIOS & DOS

	pop	ES				; restore DS & ES!
	pop	DS

	jnc	SysOk				;check for error

	Message msgNotSystemDisk		;no system transferred
	mov	WORD PTR SysSiz+2,0		;reset system size to zero
	mov	WORD PTR SysSiz,0

	cmp	SpecialCase,TRUE		;is this a special case?
	jnz	CleanUp				;no

	call	WriteRootDir			;write out Root Directory again
						; to zero out any new entry
	jnc	RootDirOk2			;check for error

	call	Reset_Ctrl_Break		;error occurred
	Message	msgDirectoryWriteError
	jmp	SHORT Problems

RootDirOk2:
	jmp	WriteFatNow			;write out new FAT now, since
						; system transfer failed
CleanUp:
	jmp	SHORT ResetCtrlBreak		;go finish up

SysOk:						;don't display if EXEC'd by Select
	test	SwitchMap,(SWITCH_SELECT or SWITCH_AUTOTEST)
	jnz	@F				;skip message

	Message msgSystemTransfered

@@:	cmp	SpecialCase,TRUE		;is this a special case?
	jnz	ResetCtrlBreak			;no, not necc to copy FAT chains

	mov	AL,Drive			;signal drive to access (0 = A)
	call	CopyFatChains			;copy system file chains to new FAT
	jmp	WriteFatNow			;now write out the FAT

ResetCtrlBreak:
	call	Reset_Ctrl_Break		;restore CTRL-Break
	call	CrLf

	mov	AH,DISK_RESET			;do a disk reset
	int	21h

	call	DONE				;final call to OEM module
	jnc	ReportC				;check for error

	jmp	SHORT Problems			;report an error

ReportC:					;temp fix for /AUTOtest
	test	SwitchMap,(SWITCH_AUTOTEST or SWITCH_8)
	jnz	@F				;volume label not supported with /8
	call	VolId				;handle volume label

@@: 						;need to shut down the report?
	test	SwitchMap,(SWITCH_SELECT or SWITCH_AUTOTEST)
	jnz	Successful_End		 	;no report if exec'd by Select

	call	Report				;print report
	jmp	SHORT Successful_End

Problems:
	test	SwitchMap,SWITCH_SELECT		;SELECT option?
	jnz	@F				;no message if EXEC'd

	Message msgFormatFailure
	mov	ExitStatus,EXIT_FATAL
	stc
	jmp	SHORT End_WriteDiskInfo

Successful_End:
	clc

End_WriteDiskInfo:
	ret

WriteDiskInfo	ENDP


;========================================================================
;
;  WriteRootDir :	This procedure writes out a zeroed root directory
;			to disk.
;
;  RETURNS  :	NC --> success
;		CY --> failure
;
;  DESTROYS :	AX,BX,CX,DX
;
;========================================================================

WriteRootDir	proc	NEAR

	assume	DS:DATA,ES:DATA
					;find sector offset of root dir on disk
	xor	DX,DX
	xor	BX,BX
	mov	BL,DeviceParameters.DP_BPB.BPB_NumberOfFats
	mov	AX,DeviceParameters.DP_BPB.BPB_SectorsPerFat
	mul	BX				;AX = total FAT Sectors

	mov	DX,DeviceParameters.DP_BPB.BPB_ReservedSectors
	add	DX,AX				;DX = root dir start sector

	mov	CX,SectorsInRootDirectory	;CX = sectors to write, loop control

WriteASector:
	mov	AL,Drive
				; Assume dir is always contained in first
				; 32mb of partition
	mov	Read_Write_Relative.Start_Sector_High,0

	push	CX				;preserve loop count CX
	mov	CX,1				;CX = sectors to write

	push	DS				;preserve DS
	lds	BX,DirectorySector		;DS:BX --> zeroed sector
	assume	DS:NOTHING,ES:DATA

	call	Write_Disk			;write all the sectors
	pop	DS				;restore DS
	assume	DS:DATA,ES:DATA

	pop	CX				;restore CX
	jc	@F				;if error occurred, break loop

	inc	DX				;write to next sector
	loop	WriteASector			;write all sectors of root dir

@@:	ret
WriteRootDir	endp



;==========================================================================
;
;  WriteFat :		This procedure copies the contents of the FatSpace
;			buffer to each of the FAT areas on disk.
;
;  RETURNS  :	NC --> success
;		CY --> failure
;
;  CALLS    :   Write_Fat
;
;  DESTROYS :	AX,BX,CX,DX,SI
;
;==========================================================================

WriteFat	proc	NEAR

	assume	DS:DATA,ES:DATA

	push	DS				;preserve DS

	xor	CX,CX
	mov	CL,DeviceParameters.DP_BPB.BPB_NumberOfFats	;loop control
	or	CX,CX				;check for zero
	jz	ExitWriteFat

	mov	DX,DeviceParameters.DP_BPB.BPB_ReservedSectors	;starting sector
	mov	AL,Drive

	lds	BX,FatSpace			;DS:BX --> FatSpace
	assume	DS:NOTHING,ES:DATA

	mov	SI,BX				;Set up for add. calc
	call	SEG_ADJ 			;Get adjusted seg:off
	mov	BX,SI				;Get new offset

WriteFatLoop: 					;loop while FATs > 0
	push	BX				;save Fat offset
	push	DS				;save Fat segment
	push	CX				;save Fat count
	push	DX				;reserved Fat sector
	call	Write_Fat			;write the Fat
	pop	DX				;get 1st. Fat sector
	pop	CX				;get Fat count
	pop	DS				;restore Fat segment
	pop	BX				;restore Fat offset
	jc	ExitWriteFat			;check for errors

	add	DX,ES:DeviceParameters.DP_BPB.BPB_SectorsPerFat	;next FAT start sector

	loop	WriteFatLoop			;write all FATs

	clc					;signal success

ExitWriteFat:
	pop	DS				;restore DS
	assume	DS:DATA

	ret
WriteFat	endp







;===============s============================================================
; Routine name: CopyFatChains
;===========================================================================
;
; Description: Copies the system FAT file chains from the existing
;	       FAT into the new FAT in memory that was created by
;	       by format.
;
; Arguments:		AL = DOS drive number
;			ES = DATA
; -------------------------------------------
; Returns:   		Carry set if disk error
; ---------------------------------------------
; Registers destroyed:	AX BX CX DX
; ---------------------------------
; Strategy
; --------
;	Read in first sector of the root directory
;	Set up a loop to find the starting cluster of each
;	file and then copy it's cluster chain to the new
;	FAT the was created during FORMAT.
;
;===========================================================================

CopyFatChains PROC NEAR

	Set_Data_Segment
	assume	DS:DATA,ES:DATA

	push	DI
	push	SI
	push	DS
	push	ES

	mov	sector_in_buffer,0ffffh	; force first read to ensure valid buffer

; Read in the root directory
	lds	BX,DirBuf		; DS:BX --> Read buffer
	assume	DS:NOTHING,ES:DATA

	mov	CX,1			; CX = Number of sector to read

	push	AX			; Preserve AX

					; Calculate root dir starting sector
	mov	AX,ES:DeviceParameters.DP_BPB.BPB_SECTORSPERFAT
						; Find sectors in all fats
	mul	ES:DeviceParameters.DP_BPB.BPB_NUMBEROFFATS
						 ; Add reserved sectors
	add	AX,ES:DeviceParameters.DP_BPB.BPB_RESERVEDSECTORS

	mov	DX,AX			; DX = Start sector
	pop	AX			; Restore AX
					; we are accessing < 32mb
	mov	ES:Read_Write_Relative.Start_Sector_High,0

	call	Read_Disk		; Read in the sector
	jnc	SetupCount		; If no error then walk the FAT
	jmp	SHORT CopyChainExit	; Error detected

SetupCount:
	mov	CX,NUM_SYS_FILES	; Set number of files to walk

IFDEF DBLSPACE_HOOKS
	cmp	es:fDblSpace, TRUE	; One less file to walk if
	je	AllSysFiles		;   DblSpace.bin wasn't copied
	dec	cx
AllSysFiles:
ENDIF

	mov	SI,BX			; DS:SI --> First directory entry

	test	ES:fBigFAT,0ffh		; See if 16 bit fat
	jz	Set12BitCluster		; If zero then 12 bit fat
	mov	BX,0fff8h		; Set 16 bit value for end chain
	jmp	SHORT ClearDirection

Set12BitCluster:
	mov	BX,0ff8h		; Set 12 bit value for end chain

ClearDirection:
	cld				; Clear the direction flag
	mov	ES:EndValue,BX

; Loop once for each entry in the Dir buffer
CopyChainLoop:
	push	CX			; Save loop count
	push	SI			; Save pointers to directory
	push	DS

	mov	AL,[SI]			; Get first char of entry name
	or	AX,AX			; Check for null entry
	jz	SetupNextWalk
	cmp	AL,0e5h			; See if entry is deleted
	je	SetupNextWalk		; If deleted nothing to do this loop
	mov	AX,[SI+26]		; AX = Starting cluster number
	or	AX,0			; Make sure cluster is not zero
	jz	SetupNextWalk
	mov	DI,AX			; DI will always hold current cluster


GetNextFatCluster:
	call	calc_sector_and_offset	; Find entry location in FAT

	mov	BX,ES:sector_in_buffer
	mov	AX,ES:sector_to_read
	cmp	AX,BX			; check if required sector is in buffer
	je	NoNeedToRead

	call	ReadFatSector		; read a sector of the FAT into buffer
	jc	CopyChainExit		; check for error

NoNeedToRead:
	lds	SI,ES:FatSector		; DS:SI --> Fat buffer
	call	GetFatSectorEntry	; AX = next cluster in the chain

CopyToFormatBuffer:
	mov	CX,AX			; CX = Value to set in format's buf
	mov	AX,DI			; AX = This cluster number
	mov	DI,CX			; Set up for next loop
	lds	SI,ES:FatSpace		; DS:SI --> Format's fat buffer
	call	GetSetFatEntry		; Set the cluster in format's buffer

	mov	AX,DI			; set up for next offset calculation

	mov	BX,ES:EndValue		; Restore end of chain value
	cmp	DI,BX			; Check for end of cluster chain
	jb	GetNextFatCluster	; Not end of chain

SetupNextWalk:
	pop	DS			; DS:SI --> This dir entry + 1
	pop	SI
	pop	CX			; CX = Count
	add	SI,DIR_ENTRY_LEN	; DS:SI --> Next dir entry
	loop	CopyChainLoop		; Keep looping until CX = 0
	clc				; Signal no errors

CopyChainExit:
	pop	ES
	pop	DS
	pop	SI
	pop	DI
	ret

CopyFatChains ENDP



;=========================================================================
;
;  ComputeN :	This procedure calculates the number of clusters needed
;		to hold 1.5Kbytes.  This value is stored in the variable
;		NumClusters.
;
;  ARGUMENTS:	DeviceParameters.DP_BPB.BPB_BytesPerSector
;		DeviceParameters.DP_BPB.BPB_SectorsPerCluster
;
;  DESTROYS :
;
;=========================================================================

ComputeN	proc	NEAR

	assume	DS:NOTHING,ES:DATA

	mov	AX,ES:DeviceParameters.DP_BPB.BPB_BytesPerSector	; Sector size
	xor	CX,CX
	mov	CL,ES:DeviceParameters.DP_BPB.BPB_SectorsPerCluster
	mul	CX				; AX = bytes per cluster
	mov	CX,AX				; CX = bytes per cluster
	mov	AX,1536				; Find how many clusters needed
						; for 1.5K
	xor	DX,DX
	div	CX				; now AX= No. clusters needed
	or	DX,DX				; round up if remainder nonzero
	jz	RoundedUp
	inc	AX
RoundedUp:
	mov	ES:NumClusters,AX

	ret
ComputeN	endp


;===========================================================================
; Routine name: ClearDirSector
;===========================================================================
;
; Description: Fill a sector size area of memory with zeros
;
; Arguments:		DS:BX --> Sector to clear
;			ES    = DATA
; ---------------------------
; Returns:   		Void
; ---------------------------
; Registers destroyed:	NONE
; ----------------------------------
; Strategy
; --------
;	Save all registers used and set ES:DI to DS:BX
;	Then do a store string, cleanup and leave
;
;===========================================================================

ClearDirSector PROC NEAR

	push	AX			; Can't destroy anything
	push	CX
	push	DI
	push	ES
	
	mov	CX,ES:deviceParameters.DP_BPB.BPB_BytesPerSector ;Num of bytes
	shr	CX,1			; Convert bytes to words

	mov	AX,DS			; Set ES:DI == DS:BX
	mov	ES,AX
	mov	DI,BX

	xor	AX,AX
	cld				; Clear the direction flag
	rep	stosw			; Fill the buffer with 0s

	pop	ES			; Leave with everything intact
	pop	DI
	pop	CX
	pop	AX
	ret

ClearDirSector ENDP

; ==========================================================================

CODE	 ENDS
	 END	 Start

; ==========================================================================

