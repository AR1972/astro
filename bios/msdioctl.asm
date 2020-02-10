;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1981-1991
; *                      All Rights Reserved.
; */
	page	,132

	TITLE	Disk Ioctl functions


	include version.inc	; set build flags
	include	biosseg.inc	; define bios segment structure

	INCLUDE devsym.inc
	INCLUDE bpb.inc
	INCLUDE ioctl.inc
	INCLUDE bootform.inc
	INCLUDE msdskpr.inc
	INCLUDE msequ.inc
	INCLUDE msbds.inc

	INCLUDE msgroup.inc		; Establish Bios_Data segment


DSK_TIMEOUT_ERR 	EQU	80h	; Time out error (no media present).
DSK_CHANGELINE_ERR	EQU	6h	; Change line error
DSK_ILLEGAL_COMBINATION EQU	0ch	; Return code of ah=18h function.
MULTI_TRK_ON		EQU	10000000b ; User spcified mutitrack=on,
					  ; or system turns
;
;----------------------------------------------------------------------------
;
; M00x : Setting 'lstdrv' properly at Setown ioctl call. Earlier it used
;		to set lstdrv to -1 on a setown call which got qbasic confused
;		Now the lstdrv update is done inside checksingle
;
; M060 : Bug # 5065. Added retries for INT 13 ah=18h call in case of errors
;
; M066 : B#5833. Modification M060 was required only for Toshiba machines
;		and the problem on Toshiba machines will be solved by the
;		'setmedia' driver. So the retry for ah=18 call is not
;		required. Also because of the retry & disk reset, format
;		becomes slow on IBM external floppy disk drives which does
;		not support the set_media_type call.
;
;----------------------------------------------------------------------------
;

; ==========================================================================
;	Most of the disk routines keep ES:[DI] set up pointing to the
;	  currently selected bds.  This is often assumed to be the standard
;	  operating environment and in some cases may not be mentioned in
;	  the subroutine headers where it will be assumed.
;
;	Most of the ioctl routines use DS:[BX] as a pointer to their
;	  request packet for at least part of their life.
; ==========================================================================


	EXTRN	ZeroSeg:WORD
	EXTRN	PtrSav:DWORD

	EXTRN	Xfer_Seg:WORD
	EXTRN	MultiTrk_Format_Flag:BYTE
	EXTRN	Multrk_Flag:WORD
	EXTRN	Start_Sec_H:WORD

	EXTRN	Dpt:DWORD
	EXTRN	fHave96:BYTE

	EXTRN	Formt_EOT:BYTE
	EXTRN	HdNum:BYTE
	EXTRN	TrkNum:WORD
	EXTRN	Gap_Patch:BYTE
	EXTRN	rFlag:BYTE
	EXTRN	CurTrk:WORD
	EXTRN	CurSec:BYTE
	EXTRN	CurHd:BYTE

	EXTRN	SpSav:WORD
	EXTRN	SecCnt:WORD
	EXTRN	Eot:BYTE
	EXTRN	Step_Drv:BYTE
	EXTRN	Start_Bds:DWORD
	EXTRN	fSetOwner:BYTE
	EXTRN	Tim_Drv:BYTE
	EXTRN	DiskSector:BYTE

		; These are some special variables defined for us

;**	EXTRN	Max_Sectors_Curr_Sup:abs
	EXTRN	SectorsPerTrack:WORD
	EXTRN	TrackTable:BYTE
	EXTRN	MediaType:BYTE
	EXTRN	Media_Set_For_Format:BYTE
	EXTRN	Had_Format_Error:BYTE
	EXTRN	TempDpt:DWORD

; ==========================================================================

; close data, open Bios_Code segment

	ToCode

	EXTRN	CheckSingle:NEAR
	EXTRN	GetBP:NEAR
	EXTRN	MapError:NEAR
	EXTRN	HasChange:NEAR
	EXTRN	DiskIO:NEAR
	EXTRN	Done:NEAR
	EXTRN	Mov_Media_Ids:NEAR
	EXTRN	Disk:NEAR
	EXTRN	IoSetup:NEAR
	EXTRN	Set_Changed_DL:NEAR
	EXTRN	SetDrive:NEAR
	EXTRN	BC_CmdErr:NEAR
	EXTRN	Bios_Data_WORD:WORD

; ==========================================================================
;
; NOTE: GetAccessFlag/SetAccessFlag is unpublished function.
;
;      This function is intended to give the user to control the
;      bds table flags of unformatted_media bit.
;      GetAccessFlag will show the status -
;	 a_DiskAccess_Control.dac_access_flag = 0 disk i/o not allowed
;						1 disk i/o allowed
;      SetAccessFlag will set/reset the unformatted_media bit in flags -
;	 a_DiskAccess_Control.dac_access_flag = 0 allow disk i/o
;						1 disallow disk i/o
; ==========================================================================

			; generic ioctl dispatch tables

IoReadJumpTable db	8			;maximum number (zero based)
		dw	GetDeviceParameters	;60h
		dw	ReadTrack		;61h
		dw	VerifyTrack		;62h
		dw	Cmd_Error_Proc		;overlapped with os2 subfunction
		dw	Cmd_Error_Proc
		dw	Cmd_Error_Proc
		dw	GetMediaId		;66h
		dw	GetAccessFlag		;67h unpublished function
		dw	SenseMediaType		;68

IoWriteJumpTable db	7
		dw	SetDeviceParameters	;40h
		dw	WriteTrack		;41h
		dw	FormatTrack		;42h
		dw	Cmd_Error_Proc
		dw	Cmd_Error_Proc
		dw	Cmd_Error_Proc
		dw	SetMediaId		;46h
		dw	SetAccessFlag		;47h unpublished function

; ==========================================================================
; IOC_DC_Table
;
; This table contains all of the valid generic IOCtl Minor codes for
; major function 08 to be used by the Ioctl_Support_Query function.
; Added for 5.00
; ==========================================================================

IOC_DC_Table  LABEL BYTE

	db	GET_DEVICE_PARAMETERS	; 60H
	db	SET_DEVICE_PARAMETERS	; 40H
	db	READ_TRACK		; 61H
	db	WRITE_TRACK		; 41H
	db	VERIFY_TRACK		; 62H
	db	FORMAT_TRACK		; 42H
	db	GET_MEDIA_ID		; 66h changed from 63h
	db	SET_MEDIA_ID		; 46h changed from 43h
	db	GET_ACCESS_FLAG 	; 67h Unpublished func changed frm 64h
	db	SET_ACCESS_FLAG 	; 47h Unpublished func changed frm 44h
	db	SENSE_MEDIA_TYPE	; 68 Added in 5.00

IOC_DC_TABLE_LEN EQU $ - OFFSET IOC_DC_Table


; ==========================================================================
; Do_Generic_IOCtl:	perform generic ioctl request
;
;    input:	AL contains logical drive
;
;	functions are dispatched through a call. On return, carry indicates
;	error code in al.  Note::bES:b& ds undefined on return from
;	subfunctions.
;
; ==========================================================================

	PUBLIC	Do_Generic_IOCtl
Do_Generic_IOCtl PROC	NEAR

	ASSUME	DS:Bios_Data,ES:NOTHING

	call	SetDrive		; ES:DI Points to bds for drive.

	push	ES
	les	BX,[PtrSav]		; ES:BX Points to request header.
	cmp	ES:[BX].MajorFunction,RAWIO
	mov	AL,ES:[BX].MinorFunction
	pop	ES
	jne	IoctlFuncErr

		; cas note:  Could do the above two blocks in reverse order.
		; Would have to preserve al for SetDrive

	mov	SI,OFFSET IoReadJumpTable
	test	AL,GEN_IOCTL_FN_TST		; test of req. function
	jnz	NotGenericWrite			; function is a read.

	mov	SI,OFFSET IoWriteJumpTable

NotGenericWrite:
	and	AL,NOT GEN_IOCTL_FN_TST		; get rid of read/write bit
	sub	AL,40h				; offset for base function
	cmp	AL,CS:[SI]
	ja	IoctlFuncErr

	cbw
	shl	AX,1
	inc	SI
	add	SI,AX
	call	CS:[SI]

	mov	DS,Bios_Data_Word	; Exit code now assumes this
	ASSUME	DS:Bios_Data

	mov	AH,81h			; Return this status in case of carry
	ret				; Pass carry flag through to exit code

		; Cmd_Error_Proc is called as a proceedure and also use
		; as a fall through from above

Cmd_Error_Proc:
	ASSUME	DS:Bios_Data
	pop	DX			; Clear up stack

IoctlFuncErr:
	ASSUME	DS:Bios_Data
	jmp	BC_CmdErr

Do_Generic_IOCtl ENDP

; ==========================================================================
;**	GetDeviceParameters:
;
;	GetDeviceParameters implements the generic ioctl function:
;	majorcode=RAWIO, minorcode=GetDeviceParameters (60h)
;
;	ENTRY	(ES:di) = BDS for drive
;		PtrSav = long pointer to request header
;	EXIT	??? BUGBUG
;	USES	??? BUGBUG
; ==========================================================================


GetDeviceParameters PROC NEAR
	ASSUME	DS:Bios_Data,ES:NOTHING

		; Copy info from bds to the device parameters packet

	lds	BX,[PtrSav]		; DS:BX points to request header.
	ASSUME	DS:NOTHING
	lds	BX,[BX].GenericIOCtl_Packet	; (DS:BX) = return buffer

	mov	AL,ES:[DI].BDS_FormFactor
	mov	[BX].DP_DeviceType,AL
	mov	AX,ES:[DI].BDS_Flags
	and	AX,fNon_Removable + fChangeLine ; Mask off other bits
	mov	[BX].DP_DeviceAttributes,AX
	mov	AX,ES:[DI].BDS_cCyln
	mov	[BX].DP_Cylinders,AX

	xor	AL,AL			; Set media type to default
	mov	[BX].DP_MediaType,AL

		; copy recommended bpb

	lea	SI,ES:[DI].BDS_RBPB
	test	[BX].dp_specialfunctions,build_device_bpb
	jz	UseBpbPresent

		; get the correct disk in the drive

	push	DS			; Save request packet segment
	mov	DS,Bios_Data_Word	; Point back to Bios_Data
	ASSUME	DS:Bios_Data

	call	CheckSingle
	call	GetBP			; Build the bpb from scratch

	pop	DS			; Restore request packet segment
	ASSUME	DS:NOTHING

	jc	GetParmRet

	lea	SI,ES:[DI].BDS_BPB	; Use this subfield of bds instead

UseBpbPresent:
	lea	DI,[BX].DP_BPB		; This is where the result goes
					; BUGBUG - why use "small" version? jgl
	mov	CX,SIZE A_BPB - 6	; For now use 'small' bpb

		; Shoot! Our segments are backwards for a copy!
		; Damn!	Reverse 'em!

	push	DS
	push	ES
	pop	DS
	pop	ES
	rep	movsb
	clc

GetParmRet:
	ret
GetDeviceParameters ENDP

; ==========================================================================
; SetDeviceParameters:
;
; input: ES:di points to bds for drive
; ==========================================================================

SetDeviceParameters PROC NEAR

	ASSUME	DS:Bios_Data
	lds	BX,[PtrSav]		; DS:BX points to request header.

	ASSUME	DS:NOTHING
	lds	BX,DS:[BX].GenericIOCtl_Packet

		; Make sure the fCHANGED_BY_FORMAT flag gets set to kick
		; Dos into looking at the BPB

	or	ES:[DI].BDS_Flags,fCHANGED_BY_FORMAT or fCHANGED
	test	[BX].dp_specialfunctions,only_set_tracklayout
	jnz	setTrackTable

		; Copy info from the device parameters packet to bds

	mov	AL,[BX].DP_DeviceType
	mov	ES:[DI].BDS_FormFactor,AL

	mov	AX,[BX].DP_Cylinders
	mov	ES:[DI].BDS_cCyln,AX

		; If change line is not loaded then ignore changeling flag

	mov	AX,[BX].DP_DeviceAttributes
	push	DS			; Save packet segment
	mov	DS,Bios_Data_Word
	ASSUME	DS:Bios_Data
	cmp	[fHave96],0		; Do we have changeline support?
	pop	DS
	ASSUME	DS:NOTHING
	jnz	HaveChange
	and	AX,NOT fChangeLine

		; Ignore all bits except non_removable and changeline
HaveChange:
	and	AX,fNon_Removable or fChangeLine
	mov	CX,ES:[DI].BDS_Flags
	and	CX,NOT(fNon_Removable OR fChangeLine OR Good_TrackLayOut OR UNFORMATTED_MEDIA)
	or	AX,CX
	mov	ES:[DI].BDS_Flags,AX

	mov	AL,[BX].DP_MediaType	; Set media type
	push	DS			; Save packet segment
	mov	DS,Bios_Data_Word
	ASSUME	DS:Bios_Data
	mov	MediaType,AL
	pop	DS			; Restore packet segment
	ASSUME	DS:NOTHING

		; The media changed (maybe) so we will have to do a set dasd
		; the next time we format a track

	or	ES:[DI].BDS_Flags,SET_DASD_TRUE

	push	DI			; Save bds pointer

		; Figure out what we are supposed to do with the bpb
		; were we asked to install a fake bpb?

	test	[BX].DP_SpecialFunctions,INSTALL_FAKE_BPB
	jnz	SHORT InstallFakeBpb

		; were we returning a fake bpb when asked to build a bpb?

	test	ES:[DI].BDS_Flags,RETURN_FAKE_BPB
	jz	SHORT InstallRecommendedBpb

		; we were returning a fake bpb but we can stop now

	and	ES:[DI].BDS_Flags,NOT RETURN_FAKE_BPB

InstallRecommendedBpb:
	mov	CX,SIZE A_BPB
	lea	DI,ES:[DI].BDS_RBPB
	jmp	SHORT CopyTheBpb

InstallFakeBpb:
	or	ES:[DI].BDS_Flags,RETURN_FAKE_BPB ; problem reported by whs.
					; BUGBUG - why use "small" version? jgl
	mov	CX,SIZE A_BPB - 6	; move 'smaller' bpb
	lea	DI,ES:[DI].BDS_BPB

CopyTheBpb:
	lea	SI,[BX].DP_BPB
	rep	movsb

Donewithbpbstuff:
	push	DS			; Save packet segment
	mov	DS,Bios_Data_Word	; Setup for ds -> Bios_Data

	ASSUME	DS:Bios_Data
	call	RestoreOldDpt		; Restore the old Dpt from TempDpt
	pop	DS			; Restore packet segment
	pop	DI			; Restore bds pointer

setTrackTable:				; Set up track table (if neccessary)
	mov	CX,[BX].DP_TrackTableEntries
	push	DS			; Save packet segment
	mov	DS,Bios_Data_Word

	ASSUME	DS:Bios_Data
	mov	SectorsPerTrack,CX
	pop	DS			; Restore packet segment

	ASSUME	DS:NOTHING
	and	ES:[DI].BDS_Flags,NOT Good_TrackLayOut
	test	[BX].DP_SpecialFunctions,TrackLayOut_Is_Good
	jz	UglyTrackLayOut

	or	ES:[DI].BDS_Flags,Good_TrackLayOut

UglyTrackLayOut:
	cmp	CX,MAX_SECTORS_IN_TRACK
	ja	TooManyPerTrack
	jcxz	SectorInfoSaved

	mov	DI,OFFSET TrackTable
	lea	SI,[BX].DP_SectorTable
	mov	ES,Bios_Data_Word	; Trash our bds pointer

StoreSectorInfo:
	inc	DI			; Skip over cylinder
	inc	DI			; Skip over head
	lodsw				; Get sector id
	stosb				; Copy it
	lodsw				; Get sector size
	call	SectSizeToSectIndex
	stosb				; Store sector SIZE index
	loop	StoreSectorInfo

SectorInfoSaved:
	clc
	ret

TooManyPerTrack:
	mov	AL,0ch
	stc
	ret

SetDeviceParameters ENDP


; ==========================================================================
; FormatTrack:
; if specialfunction byte is 1,then this is a status call to see if there is
; rom support for the combination of sec/trk and # of cyln,and if the
; combination is legal. if specialfunction byte is 0,then format the track.
;
; input: ES:di points to bds for drive
;
; output:
;	for status call:
;	specialfunction byte set to:
;		0 - rom support + legal combination
;		1 - no rom support
;		2 - illegal combination
;		3 - no media present
;	carry cleared.
;
;	for format track:
;		carry set if error
;
; ==========================================================================

FormatTrack PROC NEAR

	ASSUME	DS:Bios_Data
	lds	BX,[PtrSav]		; ES:BX points to request header.

	ASSUME	DS:NOTHING
	lds	BX,DS:[BX].GenericIOCtl_Packet
	test	[BX].DP_SpecialFunctions,STATUS_FOR_FORMAT
	jz	DoFormatTrack

	push	DS			; Save packet
	mov	DS,Bios_Data_Word	; Point to Bios_Data

	ASSUME	DS:Bios_Data
	call	SetMediaForFormat	; Also moves current Dpt to TempDpt
	pop	DS			; Restore packet

	ASSUME	DS:NOTHING
	mov	[BX].DP_SpecialFunctions,AL
	clc
	ret

DoFormatTrack:
	cmp	ES:[DI].BDS_FormFactor,DEV_HARDDISK
	jne	DoFormatDiskette

	mov	DS,Bios_Data_Word	; Setup ds-> Bios_Data for verify
	jmp	VerifyTrack

DoFormatDiskette:
	mov	CX,[BX].FP_Head
	mov	DX,[BX].FP_Cylinder	; Load cylinder & head from pkt
	test	[BX].FP_SpecialFunctions, DO_FAST_FORMAT ; Fast format request?
	mov	DS,Bios_Data_Word	; Done with pkt seg, get to Bios_Data

	ASSUME	DS:Bios_Data
	jz	DoFormatDiskette_1	; Go ahead if not multi-trk

	jmp	VerifyTrack_Err 	; Error, same as VerifyTrack_Err

DoFormatDiskette_1:
	call	SetMediaForFormat	; Also moves current Dpt to TempDpt
	cmp	AL,1		;	; ROM support for sec/trk,# trks comb?
	jz	NeedToSetDasd		; Old rom.

	cmp	AL,3			; Time out error?
	jnz	NoSetDasd		; No,fine.(at this point,don't care
					; about the illegal combination.)
	jmp	SHORT FormatFailed

NeedToSetDasd:
	push	DX
	call	SetDasd 		; AH=17h,int 13h
	pop	DX

NoSetDasd:
	call	CheckSingle		; Do any needed diskette swapping
	mov	AX,DX			; Get track from packet
	mov	[TrkNum],AX
	mov	BYTE PTR [HdNum],CL	; Store head from packet
	mov	AH,CL
	mov	BX,OFFSET TrackTable

	mov	CX,SectorsPerTrack

StoreCylinderHead:
	mov	[BX],AX 		; Store into TrackTable
	add	BX,4			; Skip to next sector field
	loop	StoreCylinderHead
	mov	CX,MaxErr		; Set up retry count

FormatRetry:
	push	CX
	mov	BX,OFFSET TrackTable
	mov	AL,BYTE PTR SectorsPerTrack
	mov	AH,RomFormat
	mov	Xfer_Seg,DS
	call	ToRom
	pop	CX
	jc	FormatError

		; Now verify the sectors just formatted.
		; NOTE: because of bug in some BIOSes we have to
		; set ES:BX to 00:00

	push	CX
	push	bx

	xor	bx,bx
	mov	Xfer_seg,bx
	mov	AL,BYTE PTR SectorsPerTrack
	mov	AH,RomVerify
	mov	CL,1
	call	ToRom

	pop	bx
	pop	CX
	jnc	FormatOk

FormatError:
	call	ResetDisk
	mov	[Had_Format_Error],1

	push	AX
	push	CX
	push	DX

	call	SetMediaForFormat
	cmp	AL,1
	jnz	WhileErr

	call	SetDasd

WhileErr:
	pop	DX
	pop	CX
	pop	AX
	loop	FormatRetry

FormatFailed:
	mov	[Had_Format_Error],1	; Set the format error flag.
	cmp	AH,DSK_CHANGELINE_ERR	; =06h. convert change line
	jne	DoMapIt 		; Error to time out error.

	mov	AH,DSK_TIMEOUT_ERR	; =80h

DoMapIt:
	jmp	MapError

FormatOk:
	mov	[Had_Format_Error],0	;reset the format error flag.
	ret

FormatTrack ENDP			; fall into VerifyTrack

; ==========================================================================
;
; VerifyTrack:
;
; input: ES:di points to bds for drive
; ==========================================================================

VerifyTrack PROC NEAR

	ASSUME	DS:Bios_Data,ES:NOTHING

	push	DS			; Save Bios_Data
	lds	BX,[PtrSav]		; DS:BX points to request header.

	ASSUME	DS:NOTHING
	lds	BX,[BX].GenericIOCtl_Packet

		; Come here with DS:[BX] -> packet, ES:[DI] -> bds

	mov	CX,[BX].VP_Cylinder	; get some stuff cuz ds will be moved
	mov	AX,[BX].VP_Head
	mov	DX,[BX].FP_TrackCount	; & number of tracks to verify
	mov	BL,[BX].FP_SpecialFunctions ; Get option flag word

	pop	DS			; Restore DS -> Bios_Data
	ASSUME	DS:Bios_Data

	mov	rFlag,romverify
	mov	[CurTrk],CX
	mov	[CurHd],AL		; **** ASSUME heads < 256
	mov	CX,[SectorsPerTrack]	;cl = sectors/track

		; Check specialfunctions to see if DO_FAST_FORMAT has been
		; specified if not we should go to the normal track verification
		; routine. If fast format has been specified we should get the
		; number of tracks to be verified and check it to see if it is
		; > 255. If it is then it is an error and we should go to
		; VerifyTrack_Err. If not multiply the number of tracks by the
		; sectors per track to get the total number of sectors to be
		; verified. This should also be lESs than equal to 255
		; otherwise we go to same error exit. If everything is okay
		; we initalise cx to the total sectors. use ax as a temporary
		; register.

	test	BL,DO_FAST_FORMAT	; Special function requESted?
	jz	NormVerifyTrack

	mov	AX,DX			; Get ax = number of trks to verify
	or	AH,AH
	jnz	VerifyTrack_Err 	; #tracks > 255
	mul	CL
	or	AH,AH			; #sectors > 255
	jnz	VerifyTrack_Err
	mov	CX,AX			; #sectors to verify
					; set the multi track request flag
	test	ES:[DI].BDS_Flags,fNon_Removable ; Hard disk?
	jz	NormVerifyTrack

	test	Multrk_Flag,MULTI_TRK_ON ; Multitrack operation = on?
	jz	NormVerifyTrack

	mov	MultiTrk_Format_Flag,1	; Then set the flag

NormVerifyTrack:
	xor	AX,AX			; 1st sector
				; Use 0:0 as the transfer address for verify
	xor	BX,BX
	mov	Xfer_Seg,BX		; Set transfer segment to zero, too
	call	TrackIo
	mov	MultiTrk_Format_Flag,0	; Reset the flag.
	ret

VerifyTrack_Err:
	mov	AH,1
	jmp	MapError

VerifyTrack ENDP

; ==========================================================================
;
; ReadTrack:
;
; input: ES:di points to bds for drive
;
; ==========================================================================

ReadTrack	proc	NEAR

	ASSUME	DS:Bios_Data,ES:NOTHING

	mov	rFlag,ROMREAD
	jmp	SHORT readWriteTrack

ReadTrack	ENDP


; ==========================================================================
;
; WriteTrack:
;
; input: ES:di points to bds for drive
;
; ==========================================================================

WriteTrack	proc	NEAR

	ASSUME	DS:Bios_Data,ES:NOTHING

	mov	rFlag,ROMWRITE

WriteTrack	ENDP			; Fall into readWriteTrack

; ==========================================================================
;
; readWriteTrack:
;
; input:
;    ES:di points to bds for drive
;    rFlag - 2 for read,3 for write
;
; ==========================================================================

ReadWriteTrack PROC NEAR

	ASSUME	DS:Bios_Data,ES:NOTHING

		; save bds pointer segment so we can use it to access
		; our packet. Notice that this is not the standard register
		; assignment for accessing packets

	push	ES
	les	BX,[PtrSav]			; ES:BX -> to request header.
	les	BX,ES:[BX].GenericIOCtl_Packet
	mov	AX,ES:[BX].TrWp_Cylinder
	mov	[CurTrk],AX
	mov	AX,ES:[BX].TrWp_Head
	mov	[CurHd],AL			; Assume heads < 256!!!
	mov	AX,ES:[BX].TrWp_FirstSector
	mov	CX,ES:[BX].TrWp_SectorsToReadWrite
	les	BX,ES:[BX].TrWp_TransferAddress ; Get transfer address

		; we just trashed our packet address, but we no longer care

	mov	Xfer_Seg,es			; Pass transfer segment
	pop	ES				; Restore bds segment
ReadWriteTrack ENDP				; Fall into TrackIo


; ==========================================================================
;
; TrackIo:
;    performs track read/write/verify
;
;   input:
;      rFlag	- 2 = read
;		  3 = write
;		  4 = verify
;      AX	- Index into track table of first sector to io
;      CX	- Number of sectors to io
;      Xfer_Seg:BX - Transfer address
;      ES:DI	- Pointer to bds
;      CurTrk	- Current cylinder
;      CurHd	- Current head
;
; ==========================================================================

TrackIo PROC NEAR
					; Procedure `disk' will pop stack to
	mov	SpSav,sp		; SpSav and return if error
	call	CheckSingle		; Ensure correct disk is in drv
	cmp	[Media_Set_For_Format],1 ; See if we have already set disk
	jz	Dptalreadyset		; base table

	push	AX			; set up tables and variables for i/o
	push	CX
	call	IoSetup
	pop	CX
	pop	AX

Dptalreadyset:				; Point si at the table entry of the
	mov	SI,OFFSET TrackTable	; first sector to be io'd
	shl	AX,1
	shl	AX,1
	add	SI,AX

		; WE WANT:
		; CX to be the number of times we have to loop
		; DX to be the number of sectors we read on each iteration

	mov	DX,1
	test	ES:[DI].BDS_Flags,Good_TrackLayOut
	jz	ionextsector

	xchg	DX,CX			; HEY! We can read all secs in one blow

IoNextSector:
	push	CX
	push	DX

	inc	SI			; Skip over the cylinder and head in
	inc	SI			; the track table

	lodsb				; Get sector ID from track table
	mov	[CurSec],AL

		;assumptions for a fixed disk multi-track disk i/o
		; 1). In the input CX (# of sectors to go) to TrackIo,only CL
		;     is valid.
		; 2). Sector size should be set to 512 bytes.
		; 3). Good track layout.

	test	ES:[DI].BDS_Flags,fNon_Removable ; Fixed disk?
	jz	IoRemovable			; No
	test	Multrk_Flag,MULTI_TRK_ON	; Allow multi-track operation?
	jz	IoRemovable			; No,don't do that.

	mov	[SecCnt],DX			; # of sectors to i/o
	mov	AX,DX
	call	Disk

	pop	DX
	pop	CX
	clc
	ret

IoRemovable:
	lodsb				; Get sector size index from track
	push	AX			; table and save it

					; Patch sector size in Dpt
	push	SI
	push	DS			; Save Bios_Data

	push	AX			; Preserve whatever might be in ah
	mov	AH,[Eot]		; Fetch Eot while ds-> Bios_Data
	lds	SI,Dpt
	ASSUME	DS:NOTHING
	mov	BYTE PTR [SI].disk_sector_siz,AL
	mov	[SI].disk_Eot,AH	; Set up the max number of sec/track
	pop	AX			; Restore whatever was in ah
	pop	DS			; Restore Bios_Data
	ASSUME	DS:Bios_Data
	mov	AL,DL
	mov	[SecCnt],AX		; Set up the count of sectors to i/o
	call	Disk

	pop	si			; Advance buffer pointer by adding
	pop	ax			; sector size
	call	SectorSizeIndexToSectorSize
	add	BX,AX
	pop	DX
	pop	CX
	loop	IoNextSector

	cmp	[Media_Set_For_Format],1
	je	NoNeedDone

	call	Done			; set time of last access,and reset

NoNeedDone:
	clc				; entries in Dpt.
	ret

TrackIo ENDP

; ==========================================================================
;
; The sector size in bytes needs to be converted to an index value for the ibm
; rom. (0=>128,1=>256,2=>512,3=>1024). It is assumed that only these values
; are permissible.
;
; On Input   AX contains sector size in bytes
; On Output  AL Contains index
; All other registers preserved
;
; ==========================================================================

SectSizeToSectIndex PROC NEAR

	ASSUME	DS:NOTHING,ES:NOTHING

	cmp	AH,2			; examine upper byte only
	ja	OneK
	mov	AL,AH			; value in AH is the index!
	ret
OneK:
	mov	AL,3
	ret

SectSizeToSectIndex ENDP

; ==========================================================================
;
; ==========================================================================

SectorSizeIndexToSectorSize PROC NEAR
	mov	CL,AL
	mov	AX,128
	shl	AX,CL
	ret
SectorSizeIndexToSectorSize ENDP


; ==========================================================================
;
; SetDASD
;
; Set up the rom for formatting.
; we have to tell the rom bios what type of disk is in the drive.
;
; On Input   - ES:di - Points to bds
;
; ==========================================================================

SetDasd	proc	NEAR

	ASSUME	DS:Bios_Data,ES:NOTHING

	cmp	[Had_Format_Error],1	; See if we've previously set dasd type
	je	DoSetDasd

	test	ES:[DI].BDS_Flags,SET_DASD_TRUE
	jz	DasdHasBeenSet

	and	ES:[DI].BDS_Flags,NOT SET_DASD_TRUE

DoSetDasd:
	mov	[Had_Format_Error],0	; Reset it
	mov	[Gap_Patch],50h 	; Format gap for 48tpi disks
	mov	AL,4
	cmp	ES:[DI].BDS_FormFactor,DEV_3INCH720KB
	jz	DoSet

	cmp	ES:[DI].BDS_FormFactor,DEV_5INCH96TPI
	jz	GotBig

	mov	AL,1			; 160/320k in a 160/320k drive
	jmp	SHORT DoSet

GotBig:
	mov	AL,2			; 160/320k in a 1.2 meg drive
	cmp	[MediaType],0
	jne	DoSet

	mov	AL,3			; 1.2meg in a 1.2meg drive
	mov	[Gap_Patch],54h

DoSet:
	push	DS			; Preserve caller's DS, si
	push	SI

		; Get the disk parameter table address (DWORD address) from the
		; location 0:[dskadr] and fix the head settle time in this to
		; be 0fh.

	mov	DS,ZeroSeg		; Point to interrupt vectors
	ASSUME	DS:NOTHING

	lds	SI,DWORD PTR DS:[DskAdr]
	mov	DS:[SI].Disk_Head_Sttl,0fh
	pop	SI
	pop	DS			; Restore caller's DS, si

	ASSUME	DS:Bios_Data
	mov	AH,17h			; Set command to set dasd type
	mov	DL,ES:[DI].BDS_DriveNum ; Set drive number
	int	13h			; Call rom-bios

DasdHasBeenSet:
	mov	AH,BYTE PTR ES:[DI].BDS_BPB.BPB_SECTORSPERTRACK
	mov	[Formt_EOT],AH
	ret

SetDasd	ENDP

; ==========================================================================
;
; Set Media Type for Format
; Performs the int 13 with ah = 18h to see if the medium described in the
; BPB area in the BDS can be handled by the rom.
; On Input, ES:DI -> current BDS.
; The status of the operation is returned in AL
;
;	- 0 - if the support is available,and the combination is valid.
;	- 1 - no rom support
;	- 2 - illegal combination
;	- 3 - no media present (rom support exists but cannot determine now)
;
; Flags also may be altered. All other registers preserved.
; If the call to rom returns no error,then the current Dpt is "replaced" by
; the one returned by the rom. This is Done by changing the pointer in [Dpt]
; to the one returned. the original pointer to the disk base table is stored
; in TempDpt, until it is restored.
;
; ==========================================================================

SetMediaForFormat PROC NEAR

	ASSUME	DS:Bios_Data,ES:NOTHING

	push	CX
	push	DX

		; If we have a format error, then do not change Dpt, TempDpt.
		; but we need to call int 13h, ah=18h again.

	cmp	[Had_Format_Error],1
	je	SkipSaveDskAdr

	xor	AL,AL			; If already done return 0
	cmp	[Media_Set_For_Format],1
	jnz	DoSetMediaForFormat

	jmp	SetMediaRet		; Media already set

DoSetMediaForFormat:
	push	ES			; Preserve caller's ES, si
	push	SI
	mov	ES,ZeroSeg		; Point to interrupt vectors
	les	SI,DWORD PTR ES:[DskAdr] ; Get pointer to disk base table
	mov	WORD PTR [Dpt],SI
	mov	WORD PTR [Dpt+2],es	; Save pointer to table

		; Initialize the head settle time to 0fh. See the offsets
		; given in dskprm.inc.

	mov	ES:[SI].Disk_Head_Sttl,0fh
	pop	SI			; Restore caller's ES, si
	pop	ES

SkipSaveDskAdr:
	mov	CX,ES:[DI].BDS_cCyln	; Get number of cylinders
	dec	CX			; Cylinder must be zero based
	and	CH,03h			; Blank out unnecessary bits
	ror	CH,1			; Put in int form
	ror	CH,1
	xchg	CH,CL
	or	CL,BYTE PTR ES:[DI].BDS_BPB.BPB_SECTORSPERTRACK ;get number of sectors
	mov	DL,ES:[DI].BDS_DriveNum ; Get drive number

	push	ES			; CAS - really need to save 'em all?
	push	DS
	push	SI
	push	DI

	mov	AH,18h			; Set media for format	M066
	int	13h			; Call rom bios		M066
	jc	FormaStatErr		;			M066

COMMENT ^
	mov	si, MaxErr		; retry count		M060
next_18:				;			M060
	mov	AH,18h			; Set media for format	M060
	int	13h			; Call rom bios		M060
	jnc	@f			;			M060
	dec	si			;			M060
	jz	FormaStatErr		;			M060
	xor	ah, ah			;			M060
	int	13h			;			M060
	jmp	next_18			;			M060

		; ES:DI points to a disk base table for this combination
		; for this drive.
@@:					;			M060
ENDCOMMENT ^

	cmp	[Had_Format_Error],1	; Did we have a format error?
	je	skip_disk_base_setting

	push	ES			; Save segment returned by the rom

	mov	ES,ZeroSeg		; Point to interrupt vector segment
	les	SI,DWORD PTR ES:[DskAdr] ; Get current disk base table

	mov	WORD PTR [TempDpt],SI
	mov	WORD PTR [TempDpt+2],es ; Save it

		; CAS -- didn't used to reload ES -> ZeroSeg here.
		; Seemed like a bug

	mov	ES,ZeroSeg
	mov	WORD PTR ES:[DskAdr],DI
	pop	WORD PTR ES:[DskAdr+2]	; replace with one returned by rom
	mov	[Media_Set_For_Format],1

skip_disk_base_setting:
	xor	AL,AL			; Legal combination + rom support code
	mov	[Had_Format_Error],AL	; Reset the flag
	jmp	SHORT PopStatRet

FormaStatErr:
	cmp	AH,DSK_ILLEGAL_COMBINATION ; Illegal combination = 0ch
	je	FormatStatIllegalComb
	cmp	AH,DSK_TIMEOUT_ERR	; 80h
	je	FormatStatTimeOut

	mov	AL,1			; Function not supported.
	jmp	SHORT PopStatRet

FormatStatIllegalComb:			; Function supported,but
	mov	AL,2			; Illegal sect/trk,trk combination.
	jmp	SHORT PopStatRet

FormatStatTimeOut:			; Function supported,but
	mov	AL,3			; Media not present.

PopStatRet:
	pop	DI			; Restore caller's DS ES DI SI
	pop	SI
	pop	DS
	pop	ES

SetMediaRet:
	pop	DX
	pop	CX
	ret

SetMediaForFormat ENDP

	ASSUME	DS:NOTHING,ES:NOTHING

; ==========================================================================
;
; RESET THE DRIVE
;
; we also set [Step_Drv] to -1 to force the main disk routine to use the
; slow head settle time for the next operation. this is because the reset
; operation moves the head to cylinder 0,so we need to do a seek the next
; time around - there is a problem with 3.5" drives in that the head does
; not settle down in time,even for read operations!!
;
; ==========================================================================

	PUBLIC ResetDisk
ResetDisk proc	NEAR

	ASSUME	DS:Bios_Data,ES:NOTHING

	push	AX
	cmp	[Media_Set_For_Format],1; Reset while formatting?
	jne	ResetDisk_cont		; Then verify operation in "fmt & vrfy"
	mov	[Had_Format_Error],1	; Might have failed.

ResetDisk_cont: 			; So signals that we had a format error

	xor	AH,AH			; Set command to reset disk
	int	13h			; Call the rom-bios
	mov	[Step_Drv],-1		; Zap up the speed
	pop	AX
	ret

ResetDisk ENDP

; ==========================================================================
;
; This routine sets up the drive parameter table with the values needed for
; format,does an int 13. values in Dpt are restored after a verify is done.
;
; on entry  -	ES:DI - points to bds for the drive
;		Xfer_Seg:BX - points to trkbuf
;		AL    - number of sectors
;		AH    - int 13 function code
;		CL    - sector number for verify
;		DS    - Bios_Data
;
; ON EXIT   -	DS,DI,ES,BX remain unchanged.
;		AX and flags are the results of the int 13
;
; ==========================================================================

ToRom	proc	NEAR

	ASSUME	DS:Bios_Data,ES:NOTHING
	push	BX
	push	SI

		; Compaq bug fix - check whether we are using new ROM
		; functionality to set up format, not merely if it exists.
		; This was formerly a check against [new_rom]

	test	[Media_Set_For_Format],1
	jnz	GotValidDpt

	push	AX
	push	ES			; Save bds segment

	cmp	ES:[DI].BDS_FormFactor,ffsmall	; is it a 3.5" drive?
	pushf				; Save the result for when ES: is busy
	mov	ES,ZeroSeg
	les	SI,DWORD PTR ES:[DskAdr] ; Get pointer to disk base table
	mov	WORD PTR [Dpt],SI
	mov	WORD PTR [Dpt+2],es	; Save pointer to table
	mov	AL,[Formt_EOT]
	mov	ES:[SI].disk_eot,AL
	mov	AL,[Gap_Patch]
	mov	ES:[SI].Disk_Formt_Gap,AL ; Important for format
	mov	ES:[SI].Disk_Head_Sttl,15 ; Assume we are doing a seek operation

				; Set up motor start correctly for 3.5" drives
	popf			; Get result of earlier cmp
	jnz	MotorStrtOK

	mov	ES:[SI].Disk_Motor_Strt,4

MotorStrtOK:
	pop	ES			; Restore bds segment
	pop	AX

GotValidDpt:
	mov	DX,[TrkNum]		; Set track number
	mov	CH,DL			; Set low 8 bits in ch
	mov	DL,ES:[DI].BDS_DriveNum ; Set drive number
	mov	DH,[HdNum]		; Set head number
	push	ES			; Save bds segment
	mov	ES,Xfer_Seg
	int	13h			; Call the rom-bios  routines
	pop	ES			; Restore bds segment

	pop	SI
	pop	BX
	ret
ToRom	ENDP

; ==========================================================================
;
; get the owner of the physical drive represented by the logical drive in al.
; the assumption is that we **always** keep track of the owner of a drive!!
; if this is not the case, the system may hang, just following the linked list.
;
; ==========================================================================

	PUBLIC	IOCtl_GetOwn
IOCtl_GetOwn proc	NEAR

	ASSUME	DS:Bios_Data

	call	SetDrive
	mov	AL,ES:[DI].BDS_DriveNum 	; Get physical drive number
	les	DI,[Start_Bds]			; Get start of bds chain

OwnLoop:
	cmp	ES:[DI].BDS_DriveNum,AL
	jne	GetNextBDS

	test	ES:[DI].BDS_Flags,FI_Own_Physical
	jnz	ExitOwn

GetNextBDS:
	les	DI,ES:[DI].BDS_Link
	jmp	OwnLoop

IOCtl_GetOwn ENDP

; ==========================================================================
;
; set the ownership of the physical drive represented by the logical drive in
; al to al.
;
; ==========================================================================

	PUBLIC IOCtl_SetOwn
IOCtl_SetOwn PROC	NEAR

	ASSUME	DS:Bios_Data,ES:NOTHING

	call	SetDrive
	mov	BYTE PTR [fSetOwner],1		; set flag for CheckSingle to
						; look at.
	call	CheckSingle			; set ownership of drive
	mov	BYTE PTR [fSetOwner],0		; reset flag

; M00x - BEGIN
;	push	ES
;	mov	ES,ZeroSeg
;	mov	BYTE PTR ES:[lstdrv],-1 	; set up sdsb as well
;	pop	ES				; restore bds pointer
; M00x - END

IOCtl_SetOwn ENDP				; fall into ExitOwn

; ==========================================================================
;
; if there is only one logical drive assigned to this physical drive, return
; 0 to user to indicate this.  Enter with ES:di -> the owner's bds.
;
; ==========================================================================

ExitOwn PROC	NEAR

	ASSUME	DS:Bios_Data,ES:NOTHING

	xor	CL,CL
	test	ES:[DI].BDS_Flags,FI_Am_Mult
	jz	ExitNoMult

	mov	CL,ES:[DI].BDS_DriveLet ; Get logical drive number
	inc	cl			; Get it 1-based

ExitNoMult:
	lds	BX,[PtrSav]

	ASSUME	DS:NOTHING
	mov	DS:[BX].unit,CL
	clc				; Exit normal termination
	ret

ExitOwn ENDP

; ==========================================================================
;
; moves the old Dpt that had been saved in TempDpt back to Dpt. this is done
; only if the first byte of TempDpt is not -1.
; all registers (including flags) are preserved.
;
; ==========================================================================

RestoreOldDpt PROC NEAR

	ASSUME	DS:Bios_Data,ES:NOTHING

		; if we have already restored the disk base table earlier,
		; do not do it again.

	push	AX
	xor	AL,AL

	mov	[Had_Format_Error],AL		; Reset flag and get current
	xchg	[Media_Set_For_Format],AL	; flag setting
	or	AL,AL
	jz	DontRestore

	push	SI
	push	DS
	push	ES
	lds	SI,[TempDpt]

	ASSUME	DS:NOTHING
	mov	ES,Bios_Data_Word		; CAS -- bleeeech!

	ASSUME	ES:Bios_Data
	mov	ES,ZeroSeg
	mov	WORD PTR ES:[DskAdr],SI
	mov	WORD PTR ES:[DskAdr+2],DS
	pop	ES
	pop	DS

	ASSUME	DS:Bios_Data
	pop	si

DontRestore:
	pop	ax
	clc					; Clear carry
	ret

RestoreOldDpt ENDP

; ==========================================================================
;	get media id
; ==========================================================================
;
; FUNCTION: get the volume label,the system id and the serial number from
;	    the media that has the extended boot record.
;	    for the conventional media,this routine will return "unknown
;	    media type" error to dos.
;
; INPUT :   ES:di -> bds table for this drive.
;
; OUTPUT:   the request packet filled with the information,if not carry.
;	    if carry set,then al contains the device driver error number
;	    that will be returned to dos.
;	    register DS,DX,AX,CX,DI,SI destroyed.
;
; SUBROUTINES TO BE CALLED:
;	BootIo:NEAR
;
; LOGIC:
;	to recognize the extended boot record,this logic will actually
;	access the boot sector even if it is a hard disk.
;	note:the valid extended bpb is recognized by looking at the mediabyte
;	field of bpb and the extended boot signature.
;
; {
;	get logical drive number from bds table;
;	rFlag = read operation;
;	BootIo;		 /*get the media boot record into the buffer
;	if (no error) then
;	     if (extended boot record) then
;		{ set volume label,volume serial number and system id
;		  of the request packet to those of the boot record;
;		};
;	     else		  /*not an extended bpb */
;		{ set register al to "unknown media.." error code;
;		  set carry bit;
;		};
;	else
;	     ret;		/*already error code is set in the register al
;
; ==========================================================================

GetMediaId	PROC	NEAR

	ASSUME	DS:Bios_Data,ES:NOTHING

	call	ChangeLineChk

	mov	AL,ES:[DI].BDS_DriveLet ; Logical drive number
	mov	rFlag,ROMREAD		; Read operation
	call	BootIo			; Read boot sector into DiskSector
	jc	IOCtl_If1

	cmp	DiskSector.EXT_BOOT_BPB.BPB_MEDIADESCRIPTOR,0f0h
	jb	IOCtl_If2		; brif not valid (0f0h - 0ffh)

	cmp	DiskSector.EXT_BOOT_SIG,ext_boot_signature ; =29h
	jnz	IOCtl_If2		; Extended boot record

	les	DI,[PtrSav]		; ES:di points to request header.
	les	DI,ES:[BX].GenericIOCtl_Packet
	mov	SI,OFFSET DiskSector.EXT_BOOT_SERIAL
	add	DI,mi_serial
	mov	CX,SIZE EXT_BOOT_SERIAL+SIZE EXT_BOOT_VOL_LABEL+SIZE EXT_SYSTEM_ID
	rep	movsb			; Move frm Bios_Data into request packet
	clc
	ret

IOCtl_If2:
	mov	AL,Error_UnKnown_Media	; =7
	stc
IOCtl_If1:
	ret
GetMediaId	ENDP

; ==========================================================================
;  set media id
; ==========================================================================

; function: set the volume label, the system id and the serial number of
;	    the media that has the extended boot record.
;	    for the conventional media, this routine will return "unknown
;	    media.." error to dos.
;	    this routine will also set the corresponding informations in
;	    the bds table.
;
; input :   ES:di -> bds table for this drive.
;
; output:   the extended boot record in the media will be set according to
;	    the request packet.
;	    if carry set, then al contains the device driver error number
;	    that will be returned to dos.
;
; subroutines to be called:
;	BootIo:NEAR
;
; logic:
;
;
; {
;	get drive_number from bds;
;	rFlag = "read operation";
;	BootIo;
;	if (no error) then
;	     if (extended boot record) then
;		{ set volume label,volume serial number and system id
;		  of the boot record to those of the request packet;
;		  rFlag = "write operation";
;		  get drive number from bds;
;		  BootIo;	  /*write it back*/
;		};
;	     else		  /*not an extended bpb */
;		{ set register al to "unknown media.." error code;
;		  set carry bit;
;		  ret;	 /*return back to caller */
;		};
;	else
;	     ret;		 /*already error code is set */
;
; ==========================================================================

SetMediaId	PROC	NEAR

	ASSUME	DS:Bios_Data,ES:NOTHING

	call	ChangeLineChk
	mov	AL,ES:[DI].BDS_DriveLet ; Logical drive number
	mov	DL,AL			; Save it for the time being.
	mov	rFlag,ROMREAD		; Read operation
	push	DX			; Save drive number
	call	BootIo			; Read boot sec to Bios_Data:DiskSector
	pop	DX			; Restore drive number
	jc	IOCtl_If6

					; Valid? (0f0h-0ffh?)
	cmp	DiskSector.EXT_BOOT_BPB.BPB_MEDIADESCRIPTOR,0f0h
	jb	IOCtl_If7		; Brif not

	cmp	DiskSector.EXT_BOOT_SIG,ext_boot_signature ; =41 (=29h)
	jnz	IOCtl_If7		; Extended boot record

	push	ES			; Save BDS pointer
	push	DI
	push	DS			; Point ES To boot record
	pop	ES

	mov	DI,OFFSET DiskSector.EXT_BOOT_SERIAL
	lds	SI,[PtrSav]		; DS:si points to request header.
	ASSUME	DS:NOTHING
	lds	SI,DS:[SI].GenericIOCtl_Packet
	add	SI,mi_serial
	mov	CX,SIZE EXT_BOOT_SERIAL+SIZE EXT_BOOT_VOL_LABEL+SIZE EXT_SYSTEM_ID
	rep	movsb

	push	ES			; point ds back to Bios_Data
	pop	DS

		;	if	dhigh ; cas - only disable for binary compare

	ASSUME	DS:Bios_Data

		;	endif

	pop	DI			;restore bds pointer
	pop	ES
	call	Mov_Media_Ids		; update the bds media id info.
	mov	AL,DL			; set drive number for BootIo
	mov	rFlag,ROMWRITE
	call	BootIo			; write it back.
	mov	[Tim_Drv],-1		; make sure chk_media check the driver
	ret				; return with error code from BootIo

IOCtl_If7:
	mov	AL,Error_UnKnown_Media	; =7
	stc

IOCtl_If6:
	ret

SetMediaId	ENDP

; ==========================================================================
;	BootIo
; ==========================================================================
;
; function: read/write the boot record into boot sector.
;
; input :
;	    al=logical drive number
;	    rFlag = operation (read/write)
;
; output:   for read operation,the boot record of the drive specified in bds
;	    be read into the DiskSector buffer.
;	    for write operation,the DiskSector buffer image will be written
;	    to the drive specified in bds.
;	    if carry set,then al contains the device driver error number
;	    that will be returned to dos.
;	    AX,CX,DX register destroyed.
;	    if carry set,then al will contain the error code from DiskIO.
;
; subroutines to be called:
;	DiskIO:NEAR
;
; logic:
;
; {
;	first_sector = 0;	 /*logical sector 0 is the boot sector */
;	sectorcount = 1;	 /*read 1 sector only */
;	buffer = DiskSector;	 /*read it into the DiskSector buffer */
;	call DiskIO (rFlag,drive_number,first_sector,sectorcount,buffer);
; }
; ==========================================================================

BootIo	PROC	NEAR

	ASSUME	DS:Bios_Data

	push	ES
	push	DI
	push	BX
	push	DS
	pop	ES			; Point ES: to Bios_Data

		; Call DiskIO to read/write the boot sec. The parameters which
		; need to be initialized for this subroutine out here are
		; - Transfer address to Bios_Data:DiskSector
		; - Low sector needs to be initalized to 0. this is a reg. param
		; - Hi sector in [Start_Sec_H] needs to be initialised to 0.
		; - Number of sectors <-- 1

	mov	DI,OFFSET DiskSector	; ES:di -> transfer address
	xor	DX,DX			; First sector (h) -> 0
	mov	[Start_Sec_H],DX		; Start sector (h) -> 0
	mov	CX,1			; One sector
	call	DiskIO

	pop	BX
	pop	DI
	pop	ES
	ret
BootIo	ENDP


; ==========================================================================
;	ChangeLineChk
; ==========================================================================
;
; when the user calls get/set media id call before dos establishes the media
; by calling "media_chk",the change line activity of the drive is going to be
; lost.	this routine will check the change line activity and will save the
; history in the flags.
;
; FUNCTION: check the change line error activity
;
; INPUT :  ES:di -> bds table.
;
; OUTPUT:   flag in bds table will be updated if change line occurs.
;
; SUBROUTINES TO BE CALLED:
;	Set_Changed_DL
;
; ==========================================================================

ChangeLineChk	PROC	NEAR

	ASSUME	DS:Bios_Data

	mov	DL,ES:[DI].BDS_DriveNum
	or	DL,DL				; Fixed disk?
	js	ChanngeLnChkRet			; Yes, skip it.

	test	ES:[DI].BDS_Flags,RETURN_FAKE_BPB ;Don't do it duing format.
	jnz	ChanngeLnChkRet

	cmp	[fHave96],1			; This rom support change line?
	jne	ChanngeLnChkRet
	call	HasChange			; This drive support change line?
	jz	ChanngeLnChkRet			; Do nothing

		; Execute the rom disk interrupt to check changeline activity.

	mov	AH,16h
	int	13h
	jnc	ChanngeLnChkRet			; No change line activity?

	push	bx
	mov	BX,fCHANGED			; Update flag in BDS for this
	call	Set_Changed_DL			; physical drive
	pop	bx

ChanngeLnChkRet:
	ret

ChangeLineChk	ENDP

; ==========================================================================
;	GetAccessFlag
; ==========================================================================
;
; FUNCTION: get the status of UNFORMATTED_MEDIA bit of flags in bds table
;
; INPUT :
;	    ES:di -> bds table
;
; OUTPUT:   a_DiskAccess_Control.dac_access_flag = 0 if disk i/o not allowed.
;						 = 1 if disk i/o allowed.
; ==========================================================================

GetAccessFlag	PROC

	ASSUME	DS:Bios_Data,ES:NOTHING
	lds	BX,[PtrSav]		; DS:BX points to request header.

	ASSUME	DS:NOTHING
	lds	BX,DS:[BX].GenericIOCtl_Packet

	mov	AL,0			; Assume result is unformatted
	test	ES:[DI].BDS_Flags,UNFORMATTED_MEDIA ; Is it unformtted media?
	jnz	GafDone			; Done if unformatted
	inc	al			; Return true for formatted

GafDone:
	mov	[BX].dac_access_flag,AL
	ret

GetAccessFlag	ENDP

; ==========================================================================
;	SetAccessFlag
; ==========================================================================
;
; function: set/reset the UNFORMATTED_MEDIA bit of flags in bds table
;
; input :
;	    ES:di -> bds table
;
; output:   unformtted_media bit modified according to the user request
; ==========================================================================

SetAccessFlag	PROC

	ASSUME	DS:Bios_Data

	lds	BX,[PtrSav]		; ES:BX points to request header.
	lds	BX,DS:[BX].GenericIOCtl_Packet
	and	ES:[DI].BDS_Flags,NOT UNFORMATTED_MEDIA
	cmp	[BX].DAC_Access_Flag,0
	jne	saf_Done
	or	ES:[DI].BDS_Flags,UNFORMATTED_MEDIA

saf_Done:
	ret

SetAccessFlag	ENDP

; ==========================================================================
; Ioctl_Support_Query
; ==========================================================================
;
; New device command which was added in DOS 5.00 to allow a query of a 
; specific specific GENERIC IOCtl to see if it is supported. Bit 7 in the
; device attributes specifies if this function is supported.
;
; ==========================================================================

	PUBLIC Ioctl_Support_Query
Ioctl_Support_Query PROC NEAR

	ASSUME	DS:Bios_Data,ES:NOTHING

	push	ES
	les	BX,[PtrSav]		; ES:BX Points to request header.
	mov	AX,WORD PTR ES:[BX].MajorFunction ; AL == Major, AH == Minor

	cmp	AL,IOC_DC		; See if major code is 8
	jne	NoSupport

	push	CS			; ES == Code segment
	pop	ES

	ASSUME	ES:Bios_Code

	mov	CX,IOC_DC_TABLE_LEN
	mov	DI,OFFSET IOC_DC_Table	; ES:DI -> Major table
	xchg	AL,AH			; Put minor code in AL
	repne	scasb			; Scan for minor code in AL
	jne	NoSupport		; Was it found

	mov	AX,100h			; Signal ioctl is supported
	jmp	SHORT IoctlSupExit

IoctlSupExit:
	pop	ES
	clc
	ret

NoSupport:
	pop	ES
	jmp	BC_CmdErr

Ioctl_Support_Query ENDP


; ==========================================================================
;	GetMediaSenseStatus
; ==========================================================================
;
; FUNCTION: Will return the type of diskette media in the specified DOS
;	    diskette drive and whether the media is the default type
;	    for that drive. (default type means the max size for that
;	    drive)
;
; INPUT :   ES:DI -> BDS table
; OUTPUT:   If carry clear
;	    DS:BX -> Updated IOCtlPacket
;
;			 Special Function at offset 0:
;				0	- Media detected is not default type
;				1	- Media detected is default type
;
;			 Device Type at offset 1:
;				2       - 720K 3.5" 80 tracks
;				7	- 1.44M 3.5" 80 tracks
;				9	- 2.88M 3.5" 80 tracks
;
; Error Codes returned in AX if carry set:
;
; 8102 - Drive not ready	- No disk is in the drive.
; 8107 - Unknown media type	- Drive doesn't support this function or
;				  the media is really unkown, any error
;				  other than "media not present"
; 
; ==========================================================================

SenseMediaType PROC

	ASSUME	DS:Bios_Data,ES:NOTHING
	lds	BX,[PtrSav]		; DS:BX points to request header.

	ASSUME	DS:NOTHING
	lds	BX,DS:[BX].GenericIOCtl_Packet
	mov	WORD PTR DS:[BX],00	; Initialize the 2 packet bytes

	mov	DL,ES:[DI].BDS_DriveNum	; Get int 13h drive number from BDS
	xor	DH,DH			; DX == physical drive number
	mov	AH,20h			; Get Media Type function
	int	13h			; If no carry media type in AL
	jc	MediaSenseErr		; ELSE error code in AH

	inc	BYTE PTR DS:[BX]	; Signal media type is default (bit 1)

DetermineMediaType:	
	dec	AL
	cmp	AL,2			; Chk for 720K ie: (3-1) = 2
	je	GotMediaType

	add	AL,4
	cmp	AL,7			; Chk for 1.44M  ie: (4-1+4) = 7
	je	GotMediaType

	cmp	AL,9			; Chk for 2.88M  ie: (6-1+4) =  9
	jne	UnknownMediaType	; Just didn't recognize media type

GotMediaType:
	mov	DS:[BX+1],AL		; Save the return value
	clc				; Signal success
	ret
	
		; We come here if carry was set after the int 13h call.
		; Before we process the error we need to see if it was
		; a real error or just that the media type is not the
		; default for the type of drive but was readily identified.

MediaSenseErr:
	cmp	AH,32h			; See if not default media erro
	je	DetermineMediaType	; Not really an error

	mov	AL,2			; Now assume drive not ready
	cmp	AH,31h			; See if media was present
	je	SenseErrExit		; Return drive not ready

UnknownMediaType:
	mov	AL,7			; Just don't know the media type

SenseErrExit:
	mov	AH,81h			; Signal error return
	stc
	ret

SenseMediaType ENDP

; ==========================================================================

Bios_Code	ends

	end



