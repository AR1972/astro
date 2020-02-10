;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;===========================================================================
; 
; FILE: PHASE1.ASM
;
;===========================================================================

;===========================================================================
;Include file declarations
;===========================================================================
debug	 equ	 0
	 .xlist
;	 INCLUDE VERSION.INC
;	 INCLUDE VERSIONA.INC
	 INCLUDE DOSMAC.INC
	 INCLUDE SYSCALL.INC
	 INCLUDE ERROR.INC
;	 INCLUDE DPB.INC
;	 INCLUDE CPMFCB.INC
	 INCLUDE DIRENT.INC
;	 INCLUDE CURDIR.INC
;	 INCLUDE PDB.INC
	 INCLUDE BPB.INC
	 INCLUDE FOREQU.INC
	 INCLUDE FORMACRO.INC
	 INCLUDE IOCTL.INC
	 INCLUDE FORSWTCH.INC
;	 INCLUDE SYSVAR.INC
	 .list
;
;---------------------------------------------------------------------------
;
; M020 : Looked for EXT_BOOT_SIG before assuming that the BPB in the boot
;	 sector is an extended one. Bug #4946
;
;---------------------------------------------------------------------------
;



;===========================================================================
; Declarations for all publics in other modules used by this module
;===========================================================================

;Constants
	EXTRN	EXIT_NO			  :ABS
	EXTRN	EXIT_FATAL		  :ABS
;Bytes
	EXTRN  CMCDDFlag:Byte			; m035

	EXTRN	ValidSavedDeviceParameters:BYTE
	EXTRN	Drive			  :BYTE
	EXTRN	msgFormatNotSupported	  :BYTE
	EXTRN	msgInsertDisk		  :BYTE
	EXTRN	msgInvalidDeviceParameters:BYTE
	EXTRN	ContinueMsg		  :BYTE
	EXTRN	msgNotCompatablePart	  :BYTE
	EXTRN	msgExistingFormatDiffers  :BYTE
	EXTRN	msgNoQuickFormat	  :BYTE
	EXTRN	msgCrLf			  :BYTE
	EXTRN	msgCheckExistingDiskFormat:BYTE
	EXTRN	Extended_Error_Msg	  :BYTE
	EXTRN	old_dir			  :BYTE
	EXTRN	ExitStatus		  :BYTE

;Words
	EXTRN	SectorsInRootDirectory	  :WORD
	EXTRN	Paras_Per_Fat		  :WORD
	EXTRN	SwitchMap		  :WORD
	EXTRN	SwitchCopy		  :WORD
	EXTRN	DiskTable		  :WORD
	EXTRN	RWErrorCode		  :WORD

;Pointers
	EXTRN	FatSpace		  :DWORD	; M016

;Functions
	EXTRN	AccessDisk		  :NEAR
	EXTRN	USER_STRING		  :NEAR
	EXTRN	CrLf			  :NEAR
	EXTRN	CheckSwitches		  :NEAR

;No more SAFE module
;	EXTRN	ReadWriteSectors	  :NEAR
	EXTRN	Read_Disk		  :NEAR

	EXTRN	Yes?			  :NEAR

		;Now use DeviceAttributes field in DevParms to check for removable
;	EXTRN	IsRemovable		  :NEAR

;Structures
	EXTRN	SavedParams		  :BYTE
	EXTRN	DeviceParameters	  :BYTE
	EXTRN	SwitchDevParams		  :BYTE
	EXTRN	Read_Write_Relative	  :BYTE

;Labels
	EXTRN	FatalExit		  :NEAR
	EXTRN	ExitProgram		  :NEAR

;===========================================================================
; Data segment
;===========================================================================

DATA    SEGMENT PUBLIC PARA 'DATA'

fBig			EQU	0ffh		; flag for 12- or 16-bit FAT

fBigFat			DB	FALSE
ThisSysInd		DB	0		; indicates size of FAT

StartSector		DW	?		; holds first data sector
TotalClusters		DW	?		; holds total #clusters on disk

UnformattedHardDrive	DB	?

MediaSensePacket	A_MEDIA_SENSE		<> ; structure used in media
						   ; sensing call

; the following table provides templates for
; BPBs used in CP/M disks.
; Order is very important (used by both MSFOR and PHASE1)

CustomCPMBPBs	LABEL	BYTE
BPB320	a_BPB	<512, 2, 1, 2, 112,  2*8*40, 0ffh, 1,  8, 2, 0, 0, 0, 0>
BPB160  a_BPB   <512, 1, 1, 2,  64,  1*8*40, 0feh, 1,  8, 1, 0, 0, 0, 0>
BPB360  a_BPB  	<512, 2, 1, 2, 112,  2*9*40, 0fdh, 2,  9, 2, 0, 0, 0, 0>
BPB180  a_BPB	<512, 1, 1, 2,  64,  1*9*40, 0fch, 2,  9, 1, 0, 0, 0, 0>

EndCustomCPMBPBs LABEL	BYTE

; This must folow CustomCPMBPBs

BPB12	a_BPB	<512, 1, 1, 2, 224, 2*15*80, 0F9h, 7, 15, 2, 0, 0, 0, 0>
BPB720	a_BPB	<512, 2, 1, 2, 112, 2* 9*80, 0F9h, 3,  9, 2, 0, 0, 0, 0>
BPB1440	a_BPB	<512, 1, 1, 2, 224, 2*18*80, 0F0h, 9, 18, 2, 0, 0, 0, 0>
BPB2880	a_BPB	<512, 2, 1, 2, 240, 2*36*80, 0F0h, 9, 36, 2, 0, 0, 0, 0>
EndStandardBPBs	LABEL	BYTE	

				; the following table indicates the switches
				; which must be set for the given CP/M media
CPMSwitchTable	LABEL	BYTE
	dw	Switch_4 + Switch_8		;320K
	dw	Switch_1 + Switch_4 + Switch_8	;160K
	dw	Switch_4			;360K
	dw	Switch_1 + Switch_4		;180K

; ========================================================================
; Tables added for media sense support in 5.00.
; ========================================================================

MediaTable	LABEL WORD

	dw	0			; 0
	dw	0 			; 1
	dw	OFFSET BPB720		; 2
	dw	0			; 3
	dw	0			; 4
	dw	0			; 5
	dw	0			; 6
	dw	OFFSET BPB1440		; 7
	dw	0			; 8
	dw	OFFSET BPB2880		; 9

EndMediaTable	LABEL WORD

DATA	ENDS

;===========================================================================
; Executable code segment
;===========================================================================

CODE	SEGMENT PUBLIC PARA	'CODE'
	ASSUME	CS:CODE, DS:DATA, ES:DATA


;===========================================================================
; Declarations for all publics in this module
;===========================================================================


	PUBLIC	Phase1Initialisation
	PUBLIC	MediaSense
	PUBLIC	TargPrm
	PUBLIC	CopyToSwitchDevParams
	PUBLIC	CompareDevParams
	PUBLIC	LoadSwitchDevParams
	PUBLIC	DetermineExistingFormat
	PUBLIC	IsValidBpb
	PUBLIC	ResetDeviceParameters
	PUBLIC	DetermineCPMFormat
;	PUBLIC	ModifySwitchMap		; M019
	PUBLIC	SetCPMParameters
	PUBLIC	Set_BPB_Info
	PUBLIC	Scan_Disk_Table
	PUBLIC	Calc_Big_Fat
	PUBLIC	Calc_Small_Fat
	PUBLIC	SetStartSector
	PUBLIC	SetfBigFat
	PUBLIC	GetTotalClusters
	PUBLIC	fBigFat
	PUBLIC	StartSector
	PUBLIC	TotalClusters
	PUBLIC	CustomCPMBPBs
	PUBLIC	CPMSwitchTable
	PUBLIC	EndStandardBPBs
	PUBLIC	BPB720
	PUBLIC	UnformattedHardDrive

; ==========================================================================
; Phase1Initialisation:
;    This routine sets up fBigFat
;    It also does most	of the other initialisation
;
;    Algorithm:
;	Perform media sensing and if present adjust DeviceParameters
;	Check switches against parameters
;	Use switches to modify device parameters
;	Save a copy of current DeviceParameters in SwitchDevParams
;
;	IF (!SWITCH_U)
;	{
;	  IF (!ValidBootRecord || !ValidBPB)
;	    set SWITCH_U
;	  ELSE
;	  {
;	    get device layout from BPB on disk
;	    IF (DeviceParameters = SwitchDevParams)
;	      do safe/quick format
;	    ELSE
;	    {
;	      IF (Switch_N || Switch_T || Switch_F)
;	      {
;	        Issue warning
;	        Format with BPB from SwitchDevParams if user continues
;	      }
;	      ELSE	
;	        do safe/quick format
;	    }
;	  }
;	}
;	
;	Calculate start sector (first sector not used by DOS)
;	fBigFat = (((TotalSectors - StartSector)/SectorsPerCluster) >= 4086)
; ==========================================================================

Phase1Initialisation proc near

;	xor	BX,BX				; Prepare BX for IsRemovable call
;	mov	BL,Drive			; Get Drive number
;	inc	BL				; Make it 1 based
;	call	IsRemovable			; Do not do media sensing for non-removable media!!
;	jc	@F				; CY --> disk is not removable
				; use DevParms to check for removable instead of call
	test	DeviceParameters.DP_DeviceAttributes,1
	jnz	@F				; Bit 0=1 --> not removable

		; New media sensing call added for 5.00 will see if
		; see if media sensing is avaliable and if it is will
		; reset DeviceParameters to the real parameters for
		; the type of disk being formatted.

	call	MediaSense
@@:
					; Ensure that there is	a valid #
					; of Sectors in	the track table
	mov	SavedParams.DP_TrackTableEntries, 0
	mov	ValidSavedDeviceParameters, 1

					; Initialise to zero to see if
					; CheckSwitches define track layout
	mov	DeviceParameters.DP_TrackTableEntries,0

	call	Set_BPB_Info		; Check to see if we are on
					; Fat system.If not set BPB to proper
					; values for format.
SetMTsupp:
					; Check switches against parameters
					; and use switches to modify device
					; parameters
	call	CheckSwitches
	retc

	call	CopyToSwitchDevParams	; Save a copy of deviceparameters as
					; returned by CheckSwitches

	mov	ax,SwitchMap		; No need to check existing format
	and	ax,SWITCH_U+SWITCH_Q
	cmp	ax,SWITCH_U		; if unconditional format specified
	jnz	CheckExistingFormat
	jmp	DevParamsOk

CheckExistingFormat:
		; New call added for 5.00 to see if the disk has been
		; previously formatted, and if so this will reset
		; DeviceParameters to those of the existing format. 

	call	DetermineExistingFormat   
	jnc	ValidExistingFormat	; carry clear if valid existing format

InvalidExistingFormat:
	and	RWErrorCode,0ffh	; check low byte for 'drive not ready' error	
	cmp	RWErrorCode,ERROR_I24_NOT_READY	;'not ready' error  code = 2
	jne	CheckForQ		; no error reading disk
		
					; 'not ready' error occurred, give msg
	mov	AX,21			; load AX with extended error code for not ready
	Extended_Message		; deliver message "Not Ready"
	mov	ExitStatus,EXIT_FATAL	; M006;
	stc
	jmp	EndPhase1

CheckForQ:
	test	SwitchMap,SWITCH_Q	; Need to give message if /q was specified
	jz	MakeUnconditional

	test	SwitchCopy,(SWITCH_T or SWITCH_N or SWITCH_F)	; did user specify size?
	jnz	TurnOffQ		; do an unconditional format at specified size

	Message	msgNoQuickFormat	; Inform user quick format cannot be done
	call	Yes?			; Continue with unconditional format?

	pushf
	Message	msgCrLf
	popf

	jnc	TurnOffQ
	mov	ExitStatus,EXIT_NO	; load exit code 5 (response is 'no')
	jmp	ExitProgram		; User wants to exit

TurnOffQ:
	and	SwitchMap,NOT SWITCH_Q	; Turn off /Q to continue

MakeUnconditional:
	or	SwitchMap,SWITCH_U	; Enable /U since invalid existing format
	jmp	SHORT DevParamsOk	; Device parameters will not have been
					; modified since invalid existing format

ValidExistingFormat:
	call	CompareDevParams	; see if SwitchDevParams = DeviceParameters
	jnc	DevParamsOk		; they are equal

					; Check if user had specified a format
					; size, since DeviceParameters on disk
					; are different.

;M001
;	test	SwitchCopy,(SWITCH_T or SWITCH_N or SWITCH_F \
;		            or SWITCH_1 or SWITCH_4 or SWITCH_8)	; M000
;	jz	DevParamsOk		; use the ones found on the disk

	test	SwitchMap,SWITCH_Q	; special case where size was specified
					; together with /Q :- use size specified
					; only if invalid existing format
	jnz	DevParamsOk		; use the parameters found on disk

	Message	msgExistingFormatDiffers ; Warn user that SAFE format cannot be done
	call	Yes?

	pushf
	Message	msgCrLf
	popf

	jnc	WantsToContinue	 
	mov	ExitStatus,EXIT_NO	; load exit code 5 (response is 'no')
	jmp	ExitProgram		; User wants to exit

WantsToContinue:
	or	SwitchMap,SWITCH_U	; Enable /U since new format specified
	call	LoadSwitchDevParams	; Set deviceparameters to SwitchDevParams
					; i.e. follow user-specified size
DevParamsOk:
	call	SetDOS_Dpb		; m035 Setup default DOS DPB for this
					;      drive (for memory cards).

IF ShipDisk

	test	SwitchMap,Switch_Z	; 1 sector/cluster disk?
	jz	$$IF19
					; set BPB accordingly
	mov	DeviceParameters.DP_BPB.BPB_SectorsPerCluster,01h
	call	Calc_Small_Fat		; calc Fat size
$$IF19:

ENDIF

	cmp	DeviceParameters.DP_TrackTableEntries,0
	jne	TrackLayoutSet		; There is a good track layout

					; Store sector table info (layout of
					; each track)
	mov	CX, DeviceParameters.DP_BPB.BPB_SectorsPerTrack;CX = loop count
	mov	DeviceParameters.DP_TrackTableEntries,CX
	mov	AX, 1					       ;AX = sector #
	mov	BX, DeviceParameters.DP_BPB.BPB_bytesPerSector ;BX = sector size
	lea	DI, DeviceParameters.DP_SectorTable

LoadSectorTable:
	stosw				; Write the sector number
	xchg	AX, BX			; Get the sector size in bytes
	stosw				; Write the sector size
	xchg	AX, BX
	inc	AX			; Go to the next sector
	loop	LoadSectorTable

TrackLayoutSet:	    
;	call	SetParasPerFat		; Now paras_per_fat is set when buffer is allocated
	call	SetStartSector
	call	SetfBigFat
	call	GetTotalClusters
	clc

EndPhase1:
	return

Phase1Initialisation endp

;==========================================================================
;
; SetParasPerFat :	This procedure will initialize the value of
;			the variable Paras_Per_Fat, based on the settings
;			of DeviceParameters.
;
;==========================================================================

;SetParasPerFat	proc	near
;
;	mov	AX, DeviceParameters.DP_BPB.BPB_BytesPerSector
;	add	AX, 0fH
;	shr	AX, 1
;	shr	AX, 1
;	shr	AX, 1
;	shr	AX, 1
;	mul	DeviceParameters.DP_BPB.BPB_SectorsPerFat
;	mov	Paras_Per_Fat,AX	; 128k Fat
;
;	ret
;SetParasPerFat	endp

; =========================================================================
;
;   MediaSense
;	Checks for media sensing via IOCtl 440d subfuction 0868.
;	If sensing is supported the user will be prompted to insert
;	the disk if it's not detect and then the device parameters
;	will be set according to the type of media being used.
;
;	Before we can use the type returned we must be sure it's
;	not a larger size disk than is formattable in the drive.
;	We can do this by checking the media type byte in the
;	saved device parameters.
;
;   Input:
;	Drive	- Must have already been set
;
; =========================================================================

MediaSense PROC NEAR

	mov	BL, Drive
	inc	BX
	mov	CX, (RAWIO shl 8) or SENSE_MEDIA_TYPE
	lea	DX,MediaSensePacket

					; First check if BIOS supports call
	mov	AX, (IOCTL shl	8) or IOCTL_QUERY_BLOCK
	int	21h
	jc	MediaSenseExit

					; Now do actual call
	mov	AX, (IOCTL shl	8) or GENERIC_IOCTL
	int	21h

	jnc	GotMediaSense

	cmp	AL,error_not_ready
	jne	MediaSenseExit		; Machine does not support media sensing

	call	TargPrm			; Insert disk prompt
	jmp	SHORT MediaSense	; Retry the operation

		; See if the type of media inserted is the same as the
		; default for this type of drive and if not check to
		; be sure it's 

GotMediaSense:
;	test	MediaSensePacket.MS_ISDEFAULT,1	; Is it default media type
;	jnz	MediaSenseExit		; If yes don't need to modify DevParms

	mov	AL,MediaSensePacket.MS_DEVICETYPE ; AL == media type
	cmp	SavedParams.DP_DEVICETYPE,AL 	  ; If the media in the
	jl	MediaSenseExit		; drv is > default size, use default

					; Load BPB for sensed media
	xor	AH,AH
	shl	AX,1			; AX == word offset in media table
	mov	BX, offset MediaTable	; BX -> Start of media table
	add	BX, AX			; BX -> Sensed type in media table

	cmp	BX, offset EndMediaTable ; Make sure we're still in the table
	jge	MediaSenseExit


	mov	SI,[BX]			; DS:SI -> Sensed device parameters
	or	SI,SI
	je	MediaSenseExit		; Unknown Media?!

	lea	DI, DeviceParameters.DP_BPB ; ES:DI -> Format parameters
	mov	CX, size a_BPB		; CX = bytes to move

	cld
	rep	movsb

MediaSenseExit:
	ret

MediaSense ENDP

;========================================================================
;
;  TargPrm :	This procedure prompts the user to insert the target disk
;		into the drive.
;
;========================================================================

TargPrm PROC NEAR

	 mov	 AL, Drive
	 call	 AccessDisk
	 Message MsgInsertDisk
	 Message ContinueMsg
	 call	 USER_STRING
	 call	 CrLf
	 ret

TargPrm ENDP

;=========================================================================
;
;  CopyToSwitchDevParams :	This procedure copies the structure
;				DeviceParameters into SwitchDevParams.
;  Registers destroyed :	CX,DI,SI
;  Assumes :			DS:DATA, ES:Nothing
;  
;=========================================================================

CopyToSwitchDevParams	proc	NEAR

	push	DS
	pop	ES

	mov	DI,OFFSET SwitchDevParams		; ES:DI --> dest. parms

	mov	SI,OFFSET DeviceParameters		; DS:SI --> src. parms

	mov	CX,SIZE A_DEVICEPARAMETERS		; byte transfer count

	cld
	rep	movsb

	ret
CopyToSwitchDevParams	endp

;=========================================================================
;
;  CompareDevParams :		This procedure compares the structure
;				DeviceParameters with SwitchDevParams.
;  Registers destroyed :	CX,DI,SI
;  Assumes :			DS:DATA, ES:Nothing
;
;=========================================================================

CompareDevParams	proc	NEAR

	push	DS
	pop	ES

	mov	DI,OFFSET SwitchDevParams	; ES:DI --> dest. parms

	mov	SI,OFFSET DeviceParameters	; DS:SI --> src. parms

	mov	CX,SIZE A_DEVICEPARAMETERS	; Set up count in bytes
	cld					; Set the direction
	repe	cmpsb				; Compare the two BPBs
	jz	EqualParams			; If ZR then BPBs matched

NotEqualParams:
	stc					; Signal BPBs don't match
	jmp	SHORT CompareParamsExit

EqualParams:
	clc					; Signal BPB matches

CompareParamsExit:
	ret
CompareDevParams	endp
;=========================================================================
;
;  LoadSwitchDevParams :	This procedure copies the structure 
;				SwitchDevParams into DeviceParameters.
;  Registers destroyed :	CX,DI,SI
;  Assumes :			DS:DATA,ES:Nothing
;
;=========================================================================

LoadSwitchDevParams	proc	NEAR

	push	DS
	pop	ES

	mov	DI,OFFSET DeviceParameters		; ES:DI --> dest. parms

	mov	SI,OFFSET SwitchDevParams		; DS:SI --> src. parms

	mov	CX,SIZE A_DEVICEPARAMETERS		; byte transfer count

	cld
	rep	movsb

	ret
LoadSwitchDevParams	endp

;=========================================================================
;
;  DetermineExistingFormat :	This procedure will check if there is a
;				valid format existing on the disk, in 
;				which case DeviceParameters will be reset
;				to that format.
;				
;				It is assumed the destination disk is 
;				already in the drive.
;
;
;  Calls :	IsValidBpb
;		ResetDeviceParameters
;		DetermineCPMFormat
;
;  Called by :  Phase1Initialisation
;
;=========================================================================

DetermineExistingFormat	proc	near

	push	DS
	push	ES
	Set_Data_Segment			;ensure addressibility

	cmp	UnformattedHardDrive,TRUE
	je	InvalidBootRecord

	Message	msgCheckExistingDiskFormat

;M016 - begin

 	xor	DX,DX				; Starting sector  to 0
	mov	AL,Drive			; Set drive number
	mov	AH,DH				; Signal this is a read AH=0
	lds	BX,FatSpace			; Load transfer address
	assume	DS:NOTHING,ES:DATA

	mov	CX,2				; # of sectors to read
						; we are accessing < 32mb
	mov	ES:Read_Write_Relative.Start_Sector_High,0
 
	call	Read_Disk			; Disk sector read
 
	mov	ES:RWErrorCode,AX		; Save error code (if any)
	jc	InvalidBootRecord		; Check for read error

	cmp	BYTE PTR [BX],0e9h		; Check for JMP opcode
	je	TestBootSignature		; If Ok then check signature
						; we can not know #reserved sectors)
	cmp	BYTE PTR [BX],0ebh		; Else check for SHORT jmp	
	jne	TryCPM				; No match then not valid boot
	cmp	BYTE PTR [BX+2],90h		; Now check for NOP opcode
	jne	TryCPM				; No match then not valid boot

TestBootSignature:
	cmp	WORD PTR [BX + 510],0aa55h	; Check for 55 AA sequence
	jne	TryCPM				; Error if not equal

CheckTheBpb:
	call	IsValidBpb
	jc	TryCPM				; CY --> Invalid format

	call	ResetDeviceParameters		; set DeviceParameters to
	clc					; existing ones on the disk
	jmp	SHORT EndDetermine

TryCPM:
						; check in case a CP/M disk is present
;	xor	BX,BX				; first check for removable media
;	mov	BL,Drive
;	inc	BL				; make it 1-based
;	call	IsRemovable
;	jc	InvalidExistingFormat		; No need to check further for non-removable media
						; use DevParms to check for removable instead of call
	test	ES:DeviceParameters.DP_DeviceAttributes,1
	jnz	InvalidBootRecord		; Bit 0=1 --> not removable

	call	DetermineCPMFormat
	jmp	SHORT EndDetermine 		; CP/M disk present, DeviceParameters
						; will have been modified
						; Carry propagated up to
						; Note: DS can be anything
;M016 - end
InvalidBootRecord:
	stc					;flag invalid format

EndDetermine:
	pop	ES
	pop	DS

	ret

DetermineExistingFormat	endp

;=========================================================================
;
; IsValidBpb :	This procedure will inspect the BPB loaded into
;			memory by the DetermineExistinFormat procedure.  
;
; Input  :	DS:BX Buffer holding boot sector (FatSpace) ; M016
; Output :	BPB is valid   - NC
;		BPB is invalid - CY
;
; Assumes:	DS:BX: FatSpace (preserved); M016
;
;=========================================================================

IsValidBpb	proc	near

	assume	DS:NOTHING,ES:DATA

	push	BX			; M016; preserve BX
	add	BX,OFFSET_BPB		; DS:BX points to start of BPB

	cmp	WORD PTR [BX],200h	; check BytesPerSector=512
	jne	NotValidBpb

	and	WORD PTR [BX+8h],0ffffh	; check that both TotalSectors
	jnz	ResetBigTotalSectors	; and BigTotalSectors are not zero
	and	WORD PTR [BX+15h],0ffffh; low word
	jnz	CheckMore
	and	WORD PTR [BX+17h],0ffffh; high word
	jz	NotValidBpb
	jmp	SHORT CheckMore

ResetBigTotalSectors:			; if TotalSectors<>0 set 
	and	WORD PTR [BX+15h],0h	; BigTotalSectors to zero
	and	WORD PTR [BX+17h],0h

CheckMore:
	and	WORD PTR [BX+0bh],0ffffh; check SectorsPerFat <> 0
	jz	NotValidBpb

	cmp	WORD PTR [BX+0dh],1h	; check 0 < SectorsPerTrack < 64
	jb	NotValidBpb
	cmp	WORD PTR [BX+0dh],3fh
	ja	NotValidBpb
	
	cmp	WORD PTR [BX+0fh],1h	; check 0 < Heads < 256
	jb	NotValidBpb
	cmp	WORD PTR [BX+0fh],0ffh
	ja	NotValidBpb

BpbIsValid:
	clc
	jmp	SHORT EndIsValidBpb

NotValidBpb:
	stc

EndIsValidBpb:
	pop	BX			; M016; restore BX
	ret

IsValidBpb	endp

;=========================================================================
;
; ResetDeviceParameters :	This procedure will copy the BPB of the
;				disk into DeviceParameters.  It will also
;				set the fields DP_CYLINDERS and  DP_MEDIATYPE,
;				for removable media.
;
; Inputs :	DS:BX Boot sector held in FatSpace ; M016
; Output :	Modified DeviceParameters
; Modifies:	ES,SI,DI,CX,DX,BX,AX
; Assumes:	DS:BX Boot sector, ES:DATA
;
;=========================================================================

ResetDeviceParameters	proc	near

	assume	DS:NOTHING,ES:DATA

	mov	SI,BX			; Use SI instead of BX for copy
	add	SI,OFFSET_BPB		;DS:SI source BPB in buffer

				;No need to modify DP_CYLINDERS,DP_MEDIATYPE
				;(and DP_DEVICETYPE) for fixed disks.
;	xor	BX,BX			; Prepare BX for IsRemovable call
;	mov	BL,ES:Drive		; Get Drive number
;	inc	BL			; Make it 1 based
;	call	IsRemovable
;	jc	CopyBpb			; CY --> disk is not removable
				; use DevParms to check for removable instead of call
	test	ES:DeviceParameters.DP_DeviceAttributes,1
	jnz	CopyBpb				; Bit 0=1 --> not removable

					;first compute total cylinders as
					;total sectors /(sectors per track)*#heads
	xor	DX,DX
	mov	AX,WORD PTR [SI+8]	;get total sectors
	or	AX,AX			;do we need to use Big total sectors?
	jnz	GotTotalSectors		;don't need to if not zero

UseBigTotalSectors:
	mov	AX,WORD PTR [SI+15h]	;load big total sectors
	mov	DX,WORD PTR [SI+17h]

GotTotalSectors:			;now DX:AX has total #sectors
	mov	BX,WORD PTR [SI+0Dh]	;get sectors per track
	div	BX
	xor	DX,DX			;clear the remainder
	mov	CX,WORD PTR [SI+0Fh]	;get number of heads
	div	CX
	or	DX,DX
	jz	CylindersOk
	inc	AX

;BUGBUG: Arithmetic may result in CYLINDERS being 1 less than actual value,
;	 for big disks (hence this calculation is skipped for fixed disks)
; PYS: fixed using same code as MSINIT.ASM

CylindersOk:
	mov	ES:DeviceParameters.DP_CYLINDERS,AX

					;now determine DP_MEDIATYPE & DP_DEVICETYPE

	mov	ES:DeviceParameters.DP_MEDIATYPE,0	; init. to zero
	cmp	AX,40					; only 360K or less has 40 cylinders
;	ja	Not360K
	jne	CopyBpb					; MEDIATYPE has been set

	cmp	ES:DeviceParameters.DP_DEVICETYPE,DEV_5INCH96TPI
	jne	CopyBpb

Is360K:
	mov	ES:DeviceParameters.DP_MEDIATYPE,1	; set to 1 only for 360K in 1.2M


;BUGBUG: Changing the value of DEVICETYPE can result in SwitchDevParams !=
;	 DeviceParameters, and hence a just-formatted 360K disk may not be
;	 recognized!  -is it really necessary to set DEVICETYPE?

;	mov	ES:DeviceParameters.DP_DEVICETYPE,DEV_5INCH	; found 360K
;	jmp	SHORT CopyBpb
;
;Not360K:	    			; BX = sectors per track
;	cmp	BX,9			; is it 720K?
;	jnz	Not720
;	mov	ES:DeviceParameters.DP_DEVICETYPE,DEV_3INCH720KB	; found 720K
;	jmp	SHORT CopyBpb
;
;Not720:
;	cmp	BX,15			; is it 1.2M?
;	jnz	Not96TPI
;	mov	ES:DeviceParameters.DP_DEVICETYPE,DEV_5INCH96TPI	; found 1.2M
;	jmp	SHORT CopyBpb
;
;Not96TPI:
;	cmp	BX,18			; is it 1.44M?
;	jnz	Not144
;	mov	ES:DeviceParameters.DP_DEVICETYPE,DEV_3INCH1440KB	; found 1.44M
;	jmp	SHORT CopyBpb
;
;Not144:
;	mov	ES:DeviceParameters.DP_DEVICETYPE,DEV_3INCH2880KB	; found 2.88M


;Following code was taken out earlier, due to its ignorance of 3.5in media
;	cmp	BX,9			;now determine media type
;	ja	HighDensity
;
;	cmp 	ES:DeviceParameters.DP_DEVICETYPE,1	;set MEDIATYPE to 1 only if
;	jne	HighDensity				;DEVICETYPE is 1
;	
;	mov	ES:DeviceParameters.DP_MEDIATYPE,1	;320/360KB since 9sectors/track
;	jmp	SHORT CopyBpb
;
;HighDensity:
;	mov	ES:DeviceParameters.DP_MEDIATYPE,0	;1.2M since >9sectors/track
	
CopyBpb:
	mov	DI,OFFSET ES:DeviceParameters.DP_BPB
					;ES:DI destination BPB in DeviceParameters
	mov	CX,BPB_LENGTH		;byte transfer count
;
; M020 - BEGIN
;
	cmp	byte ptr [si+26h], 29h	; extended BPB ?
	je	@f
	mov	cx,(BPB_LENGTH-6)	; small BPB
@@:
;
; M020 - END
;

	cld				;set the direction  
	rep	movsb			;write the new BPB
	ret

ResetDeviceParameters	endp

;=========================================================================
;
; DetermineCPMFormat :		This procedure will check the media 
;				descriptor in the FAT of the disk.  The
;				disk has a valid CP/M format if this is
;				in the range FCh - FFh.
;
;  Assumes :	DS:BX points to boot sectors. ; M016
;  Modifies :	DS ; M016
;  Returns :	NC - Valid CP/M format detected
;		     DeviceParameters modified
;		CY - Invalid format
;
;==========================================================================

DetermineCPMFormat	proc	NEAR

;M016 - begin
	assume	DS:NOTHING,ES:DATA	

	cmp	ES:DeviceParameters.DP_BPB.BPB_BytesPerSector,512
	stc				; Checking default for drive
					; (cannot check BPB since disk
					;  may not have one)
	jne	ExitDetCPMFormat
	add	BX,512			; DS:BX points to first FAT


	mov	CL,BYTE PTR [BX]  	; load media descriptor byte into CL
	cmp	CL,0fch
	jb	ExitDetCPMFormat	; below = carry, how practical!

	Set_Data_Segment		; For the two following calls

;M016 - end

;	call	ModifySwitchMap		; M019; set the required switches
	call	SetCPMParameters	; modify DeviceParameters accordingly

ExitDetCPMFormat:
	ret
DetermineCPMFormat	endp

;M019 - begin
;========================================================================
;
; ModifySwitchMap :	This procedure sets the required switches for the
;			type of CP/M format detected. Also, old_dir is set
;			to TRUE if /8 is set.
;
; Returns :		Modified SwitchMap
;			Modified old_dir
;
; Assumes :		CL has media descriptor
;
; Modifies :		AX,SI
;
;=========================================================================
;
;ModifySwitchMap	proc	NEAR
; 	
;	xor	AX,AX			; find offset in table by subtracting
;	mov	AL,0ffh			; media descriptor from ffh
;	sub	AL,CL
;
;	shl	AX,1			; get word offset
;	lea	SI,CPMSwitchTable	; get required switches from table
;	add	SI,AX
;
;	mov	AX,WORD PTR [SI]	; load mask into AX
;	or	SwitchMap,AX
;
;	test	SwitchMap,SWITCH_8
;	jz	ExitModifySwitchMap
;
;	mov	old_dir,TRUE
;
;ExitModifySwitchMap:
;	ret
;ModifySwitchMap	endp
;M019 - end

;=========================================================================
;
; SetCPMParameters :	This procedure copies the required BPB from the
;			CP/M BPB table into DeviceParameters.BPB.  Also,
;			DeviceParameters.MediaType is set to 1, and
;			DeviceParameters.Cylinders is set to 40.
;			
;			In case the disk has a 160K or 320K format, the /8
;			switch is set, so that 
; Returns :	NC - DeviceParameters updated
;		CY - Error (out of table boundaries)
;
; Modifies :	AX,BX,CX,DX,SI,DI,ES
;		DeviceParameters
;
; Assumes :	CL contains media descriptor byte
;
;=========================================================================

SetCPMParameters	proc	NEAR

	xor	AX,AX			; find index into CP/M BPB table by
	mov	AL,0ffh			; subtracting media descriptor from ffh
	sub	AL,CL

	mov	BX,SIZE A_BPB		; now find byte offset by multiplying
	mul	BX			; by entry size
	
	lea	SI,CustomCPMBPBs
	add	SI,AX
	cmp	SI,OFFSET EndCustomCPMBPBs ; check we are still in table
	ja	NotInTable

	lea	DI,DeviceParameters.DP_BPB
	mov	CX,SIZE A_BPB		; set up byte transfer count

	push	DS			; set ES=DS
	pop	ES		

	rep	movsb			; load the BPB

	mov	BYTE PTR DeviceParameters.DP_MediaType,1
	mov	BYTE PTR DeviceParameters.DP_Cylinders,40

	clc
	jmp	SHORT ExitSetCPMParm

NotInTable:
	stc

ExitSetCPMParm:
	ret
SetCPMParameters	endp

;=========================================================================
; Set_BPB_Info	 :	 When we have a	Fat count of 0,	we must	calculate
;			 certain parts of the BPB.  The	following code
;			 will do just that.
;
;	 Inputs	 : DeviceParameters
;
;	 Outputs : BPB information
;=========================================================================

Procedure Set_BPB_Info			; Calc new BPB

	Set_Data_Segment		; Set up addressibility
					; See if we have 0 Fats specified
	cmp	DeviceParameters.DP_BPB.BPB_NumberOfFats,00h
	jne	$$IF101 		; Yes, 0 FatS specified
	call	Scan_Disk_Table 	; Access disk table
	mov	BL,BYTE PTR DS:[SI+8]	; Get Fat type
	mov	CX,WORD PTR DS:[SI+4]	; Get Sectors/cluster
	mov	DX,WORD PTR DS:[SI+6]	; Number of entries for the root DIR

	mov	DeviceParameters.DP_BPB.BPB_RootEntries,DX
	mov	DeviceParameters.DP_BPB.BPB_SectorsPerCluster,CH
	mov	DeviceParameters.DP_BPB.BPB_BytesPerSector,0200h
	mov	DeviceParameters.DP_BPB.BPB_ReservedSectors,0001h
	mov	DeviceParameters.DP_BPB.BPB_NumberOfFats,02h

	cmp	BL,FBIG			; Big Fat?
	jne	$$IF102 		; Yes
	call	Calc_Big_Fat		; Calc big Fat info
	jmp	SHORT $$EN102
$$IF102:
	call	Calc_Small_Fat		; Calc small Fat info

$$EN102:
$$IF101:
	ret

Set_BPB_Info ENDP

;=========================================================================
; Scan_Disk_Table	 : Scans the table containing information on
;			   the disk's attributes.  When	it finds the
;			   applicable data, it returns a pointer in
;			   DS:SI for reference by the calling proc.
;
;	 Inputs	 : DiskTable - Contains	data about disk	types
;
;	 Outputs : DS:SI     - Points to applicable disk data
;=========================================================================

Procedure Scan_Disk_Table

	cmp	DeviceParameters.DP_BPB.BPB_TotalSectors,00h ; small disk?
	je	$$IF106 			; Yes

	mov	DX,00h				; Set high to 0
	mov	AX,WORD PTR DeviceParameters.DP_BPB.BPB_TotalSectors
	jmp	SHORT $$EN106

$$IF106:
	mov	DX,WORD PTR DeviceParameters.DP_BPB.BPB_BigTotalSectors[+2]
	mov	AX,WORD PTR DeviceParameters.DP_BPB.BPB_BigTotalSectors[+0]

$$EN106:
	mov	SI,offset DiskTable		; Point to disk data
Scan:

	cmp	DX,WORD PTR DS:[SI]		; Below?
	jb	Scan_Disk_Table_Exit		; Yes, exit
	ja	Scan_Next			; No, continue

	cmp	AX,WORD PTR DS:[SI+2]		; Below or equal?
	jbe	Scan_Disk_Table_Exit		; Yes, exit

Scan_Next:
	add	SI,5*2				; Adjust pointer
	jmp	Scan				; Continue scan

Scan_Disk_Table_Exit:

	ret

Scan_Disk_Table ENDP

;=========================================================================
; Calc_Big_Fat	 :	 Calculates the	Sectors	per Fat	for a 16 bit Fat.
;
;	 Inputs	 : DeviceParameters.DP_BPB.BPB_BigTotalSectors	 or
;		   DeviceParameters.DP_BPB.BPB_TotalSectors
;
;	 Outputs : DeviceParameters.DP_BPB.BPB_SectorsPerFat
;=========================================================================

Procedure Calc_Big_Fat

	cmp	DeviceParameters.DP_BPB.BPB_TotalSectors,00h ; Small disk?
	je	$$IF109 		; Yes

	mov	DX,00h			; Set high to 0
	mov	AX,WORD PTR DeviceParameters.DP_BPB.BPB_TotalSectors
	jmp	SHORT $$EN109

$$IF109:
	 mov	DX,WORD PTR DeviceParameters.DP_BPB.BPB_BigTotalSectors[+2]
	 mov	AX,WORD PTR DeviceParameters.DP_BPB.BPB_BigTotalSectors[+0]

$$EN109:
	mov	CL,04h			; 16 DIR entries per	sector
	push	DX			; Save total	Sectors (high)
					; Get root entry count
	mov	DX,DeviceParameters.DP_BPB.BPB_RootEntries
	shr	DX,CL			; Divide by 16
	sub	AX,DX
	pop	DX			; Restore DX
	sbb	DX,0
	sub	AX,1			; AX	= T - R	- D
	sbb	DX,0
	mov	BL,02h
					; Get Sectors per cluster
	mov	BH,DeviceParameters.DP_BPB.BPB_SectorsPerCluster
	add	AX,BX			; AX	= T-R-D+256*SPC+2
	adc	DX,0
	sub	AX,1			; AX	= T-R-D+256*SPC+1
	sbb	DX,0
	div	BX			; Sec/Fat = CEIL((TOTAL-DIR-RES)/
					; (256*SECPERCLUS+2)
					; sectors/cluster

	mov	WORD PTR DeviceParameters.DP_BPB.BPB_SectorsPerFat,AX
	ret

Calc_Big_Fat ENDP


;=========================================================================
; Calc_Small_Fat:	 Calculates the	Sectors	per Fat	for a 12 bit Fat.
;
;	 Inputs	 : DeviceParameters.DP_BPB.BPB_BigTotalSectors	 or
;		   DeviceParameters.DP_BPB.BPB_TotalSectors
;
;	 Outputs : DeviceParameters.DP_BPB.BPB_SectorsPerFat
;=========================================================================

Procedure Calc_Small_Fat

	cmp	DeviceParameters.DP_BPB.BPB_TotalSectors,00h ;small disk?
	je	$$IF112 			; Yes

	mov	DX,00h				; Set high to 0
	mov	AX,WORD PTR DeviceParameters.DP_BPB.BPB_TotalSectors
	jmp	SHORT $$EN112

$$IF112:
	mov	DX,WORD PTR DeviceParameters.DP_BPB.BPB_BigTotalSectors[+2]
	mov	AX,WORD PTR DeviceParameters.DP_BPB.BPB_BigTotalSectors[+0]

$$EN112:
	xor	BX,BX
	mov	BL,DeviceParameters.DP_BPB.BPB_SectorsPerCluster
	div	BX
						; Now multiply	by 3/2
	mov	BX,3
	mul	BX				; Div by log 2 of Sectors/clus
	mov	BX,2
	div	BX
	xor	DX,DX
						; Now divide by 512
	mov	BX,512
	div	BX
	inc	AX
						; DX:AX contains number of Fat
						; sectors necessary
	mov	DeviceParameters.DP_BPB.BPB_SectorsPerFat,AX
	ret

Calc_Small_Fat ENDP

; ==========================================================================
; StartSector = number	of reserved Sectors
;	  + number of Fat Sectors	 ( Number of FatS * Sectors Per	Fat )
;	  + number of directory	Sectors	 ( 32* Root Entries / bytes Per	Sector )
;					 ( above is rounded up )
;
; Calculate the number	of directory Sectors
; ==========================================================================

SetStartSector	proc near

	 mov	 AX, DeviceParameters.DP_BPB.BPB_RootEntries
	 mov	 BX, size dir_entry
	 mul	 BX
	 add	 AX, DeviceParameters.DP_BPB.BPB_bytesPerSector
	 dec	 AX
	 xor	 DX,DX
	 div	 DeviceParameters.DP_BPB.BPB_bytesPerSector
	 mov	 SectorsInRootDirectory,AX
	 mov	 StartSector, AX				;not done yet!

; Calculate the number	of Fat Sectors
	 mov	 AX, DeviceParameters.DP_BPB.BPB_SectorsPerFat
	 mul	 DeviceParameters.DP_BPB.BPB_numberOfFats
; add in the number of	boot Sectors
	 add	 AX, DeviceParameters.DP_BPB.BPB_ReservedSectors
	 add	 StartSector, AX

	 return

SetStartSector	endp

; ==========================================================================
;
; fBigFat = ( ( (TotalSectors - StartSector) / SectorsPerCluster) >= 4086 )
;
; ==========================================================================

SetfBigFat proc near

						; > 32mb part?
	cmp	DeviceParameters.DP_BPB.BPB_BigTotalSectors+2,0
	je	$$IF21				; Yes, big Fat

	mov	fBigFat, TRUE			; Set flag
	mov	ThisSysInd,6
	jmp	SHORT $$EN21			; Nope, < 32,b

$$IF21: 					; Assume this used
	mov	AX,DeviceParameters.DP_BPB.BPB_BigTotalSectors
	cmp	AX,0				; Was this field used?

	jne	$$IF23				; Nope, use other sector field
	mov	AX, DeviceParameters.DP_BPB.BPB_TotalSectors

$$IF23: 					; ** Fix for PTM PCDOS P51
	mov	ThisSysInd,1			; Set small Fat for default
	sub	AX,StartSector			; Get Sectors in data area
	xor	DX,DX
	xor	BX,BX
	mov	bl,DeviceParameters.DP_BPB.BPB_SectorsPerCluster
	div	BX				; Get total clusters

	cmp	AX,BIG_Fat_THRESHOLD		; Is clusters >= 4086?	
	jnae	$$IF25

	mov	fBigFat,TRUE			; 16 bit Fat if	>=4096
						; ** END fix for PTM PCDOS P51
	mov	ThisSysInd,4			; set large Fat
$$IF25:
$$EN21:
	return

SetfBigFat endp

;==========================================================================
;
; GetTotalClusters :	This procedure initializes the variable TotalClusters.
;			This is utilized by Quick Format to check for when all
;			the clusters have been processed.
; Destroys :	AX,BX,CX,DX
; Strategy :	TotalClusters = (TotalSectors-Fats-Root-Reserved)/SectorsPerCluster
;
;==========================================================================

GetTotalClusters	proc	NEAR

	xor	DX,DX
	mov	AX,DeviceParameters.DP_BPB.BPB_TotalSectors
	or	AX,AX			; Check if BigTotalSectors must be used
	jnz	GoSubstract		; M015; Substrack Fats, Root and reserved

GetBigSectors:
	mov	AX,DeviceParameters.DP_BPB.BPB_BigTotalSectors
	mov	DX,DeviceParameters.DP_BPB.BPB_BigTotalSectors[2]

;M015 - begin
GoSubstract:
	sub	AX,DeviceParameters.DP_BPB.BPB_ReservedSectors
	sbb	DX,0

	xor	CH,CH
	mov	CL,DeviceParameters.DP_BPB.BPB_NumberOfFats
	jcxz	GoDivide		; M017; if non fat, don't even do the root

SubstractAFat:
	sub	AX,DeviceParameters.DP_BPB.BPB_SectorsPerFat
	sbb	DX,0
	loop	SubstractAFat

GoSubstractRoot:
;M017 - Begin
; Assumes that BytesPerSectors is a power of 2 and at least 32
; Those are valid assumptions since BIOS requires the same.

	mov	BX,DeviceParameters.DP_BPB.BPB_BytesPerSector
	mov	CL,5			; divide by 32
	shr	BX,CL			; BX = root entries per sector (a power of 2)

	or	BX,BX			; Sanity check for infinite looping
	jz	SayWhat

	mov	CX,DeviceParameters.DP_BPB.BPB_RootEntries

SubstractRootLoop:
	test	BX,1
	jnz	SubstractRootReady
	shr	BX,1
	shr	CX,1
	jmp	short SubstractRootLoop

SubstractRootReady:
;M017 - end

	sub	AX,CX
	sbb	DX,0

GoDivide:
	xor	BH,BH
	mov	BL,DeviceParameters.DP_BPB.BPB_SectorsPerCluster
	cmp	DX,BX
	jae	SayWhat
	
	div	BX

	inc	AX			; M015; Bump by 1 since start with 2
	cmp	AX,0FFEFh		; M017; Sanity check
	ja	SayWhat			; M017;

	mov	TotalClusters,AX
	ret

SayWhat:
	Message	msgInvalidDeviceParameters
	jmp	FatalExit

;M015 - end
	
GetTotalClusters	endp

	; m035 Start - Need to set the DPB for a memory card because the
	;	       default will be for the last disk accessed in the
	;	       the drive and may not be correct for the current
	;	       disk.

SetDOS_Dpb PROC

	cmp	CMCDDFlag, Yes
	je	@f
	ret
@@:

	push	AX
	push	BX
	push	DX
	push	BP
	push	SI
	push	ES

	push	DS
	mov	AH,32h
	mov	DL,Drive
	inc	DL
	int	21h				; Get DPB in DS:BX

	push	DS
	pop	ES
	mov	BP,BX				; ES:BP -> DPB

	pop	DS
	mov	SI, OFFSET DeviceParameters.DP_BPB ; DS:SI --> BPB for disk

	mov	AH,53h
	int	21h				; Create correct DPB from BPB

	pop	ES
	pop	SI
	pop	BP
	pop	DX
	pop	BX
	pop	AX

	ret

SetDOS_Dpb ENDP

	; m035 End
;==========================================================================

CODE	ENDS

END

