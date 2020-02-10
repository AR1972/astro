;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;===========================================================================
; 
; FILE: GLBLINIT.ASM
;
;===========================================================================

;===========================================================================
;Declaration of include files
;===========================================================================

;
;---------------------------------------------------------------------------
;
; M024 : B#5495. Added "Insufficient memory" message when FORMAT cannot
;		allocate memory for FAT, Directory... etc 
;
;---------------------------------------------------------------------------
;
debug	 equ	 0

        .xlist

	INCLUDE		BPB.INC
;	INCLUDE		VERSION.INC
;	INCLUDE		VERSIONA.INC
	INCLUDE		DOSMAC.INC
	INCLUDE		SYSCALL.INC
;	INCLUDE		DPB.INC
	INCLUDE		FOREQU.INC	
	INCLUDE		FORMACRO.INC
	INCLUDE		IOCTL.INC
	INCLUDE		FORSWTCH.INC
	INCLUDE		SAFEDEF.INC
	INCLUDE		SYSVAR.INC
	.list


;===========================================================================
; Declarations for all publics in other modules used by this module
;===========================================================================

;Bytes
	EXTRN	msgOutOfMemory		  :BYTE
	EXTRN	msgInsufficientMemory	  :BYTE		; M024
	EXTRN	Drive			  :BYTE
	EXTRN	ClustBound_Flag		  :BYTE
	EXTRN	FileStat		  :BYTE
	EXTRN	SystemDriveLetter	  :BYTE

;Words
	EXTRN	SwitchMap		  :WORD
	EXTRN 	mSize			  :WORD
	EXTRN	mStart			  :WORD
	EXTRN	ClustBound_Buffer_Seg	  :WORD
	EXTRN	Paras_per_fat		  :WORD

;Pointers
	EXTRN	DirectorySector		  :DWORD
	EXTRN	FatSpace	  	  :DWORD
	EXTRN	FatSector		  :DWORD
;No more SAFE module
;	EXTRN	HeaderBuf		  :DWORD
	EXTRN	DirBuf			  :DWORD

;Functions

;Messages
	EXTRN	msgFormatNotSupported	  :BYTE

;Structures
	EXTRN	SavedParams		  :BYTE
	EXTRN	DeviceParameters	  :BYTE
	EXTRN	Bios			  :BYTE
	EXTRN  	Dos			  :BYTE
	EXTRN	Command 		  :BYTE

;Labels
	EXTRN	FatalExit		  :NEAR
	EXTRN	ReadDos			  :NEAR
	EXTRN	SysPrm			  :NEAR

;===========================================================================
; Data segment
;===========================================================================

DATA    SEGMENT PUBLIC PARA 'DATA'

SECTORS_FOR_MIRROR	EQU	7		; # extra buffer sectors
						; required by Mirror utility,
						; apart from FAT & Root Dir
DATA	ENDS

;===========================================================================
; Executable code segment
;===========================================================================

CODE	SEGMENT PUBLIC PARA	'CODE'
	ASSUME	CS:CODE, DS:DATA, ES:DATA


;===========================================================================
; Declarations for all publics in this module
;===========================================================================

	PUBLIC	Global_Init
	PUBLIC	GetDeviceParameters

; for debug

	PUBLIC	Copy_Device_Parameters
	PUBLIC	Alloc_Dir_Buf
	PUBLIC	Alloc_Fat_Buf
	PUBLIC	Alloc_Fat_Sec_Buf
	PUBLIC	Alloc_DirBuf2
	PUBLIC	Alloc_Cluster_Buf
	PUBLIC	Do_Switch_S


;===========================================================================
;
;  Global_Init  :	This procedure first gets the default drive parameters.
;			It then allocates buffer space for the root directory
;			sector, FAT,a fat sector, a file header and first
;			root DIR sector based on these parameters.  It
;			then checks for the /s switch and if this is present,
;			a buffer is allocated for the system files and these
;			are read into memory.  A prompt to insert the system
;			disk will be given in the case of removable media.
;
;===========================================================================

Global_Init	proc	near

	lea	DX, DeviceParameters	; Get the default drive parameters
	mov	DeviceParameters.DP_SpecialFunctions, 0
	call	GetDeviceParameters
	
	jnc	GotDeviceParameters
	Message msgFormatNotSupported
	stc 				; Let the jump to FatalExit be made
	ret				;  in the main routine, upon returning

GotDeviceParameters:			
	call	Copy_Device_Parameters	; Save the device parameters
					; for when we exit
	call	Alloc_Dir_Buf		; Allocate root directory buffer
	jc	gi_memerr		; M024

	call	Alloc_Fat_Buf		; Allocate FAT buffer
	jc	gi_memerr		; M024

	call	Alloc_Fat_Sec_Buf	; Allocate fat sector buffer
	jc	gi_memerr		; M024

;No more SAFE module
;	call	Alloc_Header_Buf	; Allocate buffer for restoration file
;	retc
	call	Alloc_DirBuf2		; Allocate 1-sector buffer DirBuf (general-
					; purpose use)
	jc	gi_memerr		; M024

	call	Alloc_Cluster_Buf	; get room for retry buffer

	call	Do_Switch_S		; Load system files if needed

					; carry flag determined by Do_Switch_S
;	clc				; Signal no error
	ret
gi_memerr:				; M024
	Message msgInsufficientMemory	; M024
	stc				; M024
	ret				; M024
Global_Init	endp

; =========================================================================
;
;   GetDeviceParameters:
;	Get the	device parameters
;
;   Input:
;	Drive
;	DX - pointer to	device parameters
; =========================================================================

GetDeviceParameters proc near

	mov	AX, (IOCTL shl 8) or GENERIC_IOCTL
	mov	bl, Drive
	inc	bl
	mov	CX, (RAWIO shl 8) or GET_DEVICE_PARAMETERS
	int	21H
	return

GetDeviceParameters endp

;==========================================================================
;
; Copy_Device_Parameters :	This procedure saves a copy of the original
;				device parameters in the structure 
;				SavedParams.
;
;==========================================================================

Copy_Device_Parameters	proc	near
					
	lea	SI, DeviceParameters	
	lea	DI, SavedParams
	mov	CX, size a_DeviceParameters
	push	DS
	pop	ES

	rep	movsb

	ret
Copy_Device_Parameters	endp


;==========================================================================
;
;  Alloc_Dir_Buf  :  This procedure allocates a memory block for the root 
;		     directory buffer, based on the device parameters only.
;
;  Inputs   	  :  DeviceParameters.DP_BPB.BPB_BytesPerSector
;  Outputs	  :  CY CLEAR - DirectorySector pointer to buffer
;		     CY SET   - failure
;  Modifies	  :  AX, BX, DirectorySector
;
;==========================================================================

Alloc_Dir_Buf	proc	near
					; DirectorySector =
				 	; malloc( Bytes Per Sector )
	mov	BX, DeviceParameters.DP_BPB.BPB_BytesPerSector
	add	BX, 0fH
	shr	BX, 1			; Divide by 16 to get #paragraphs
	shr	BX, 1
	shr	BX, 1
	shr	BX, 1
	mov	AH, Alloc
	int	21h
	jc      Exit_Alloc_Dir_Buf
					; Base address of newly allocated
					; block is AX:0000
	mov	WORD PTR DirectorySector+2,AX
	xor	AX,AX
	mov	WORD PTR DirectorySector,AX

Exit_Alloc_Dir_Buf:
	ret

Alloc_Dir_Buf	endp

;==========================================================================
;
;  Alloc_Fat_Buf  :  This procedure allocates a memory block for the FAT
;		     buffer, based on the device parameters only.  In order
;		     to ensure there is enough buffer space for the Mirror
;	  	     utility, the FatSpace buffer is initially allocated
;		     with size:
;			FAT + RootDir + 6 sectors + 1 surplus sector
;		     which is all the buffer space required by Mirror.
;
;  Inputs   	  :  DeviceParameters.DP_BPB.BPB_BytesPerSector
;		     DeviceParameters.DP_BPB.BPB_SectorsPerFat
;		     DeviceParameters.DP_BPB.BPB_RootEntries
;
;  Outputs	  :  CY CLEAR - FatSpace pointer to buffer
;		     CY SET   - failure
;
;  Modifies	  :  AX, BX, DX, FatSpace
;
;==========================================================================

Alloc_Fat_Buf	proc	near
					; FatSpace =
					; malloc( BytesPerSec * SecPerFat +
					; 32 * RootEntries + 6 * ByesPerSec)
	mov	AX, DeviceParameters.DP_BPB.BPB_BytesPerSector
	add	AX, 0fH			; round up for next para
	shr	AX, 1			; convert to paras
	shr	AX, 1
	shr	AX, 1
	shr	AX, 1
	mul	DeviceParameters.DP_BPB.BPB_SectorsPerFat
	mov	BX,AX			; Save FAT size in paras in BX

;No more SAFE module
; Old Logic Was :    Since this buffer is used in SAFE to hold the root
;		     directory from the disk, the size of the buffer is
;		     allocated as the larger of the drive defaults for
;		     the FAT or the whole root directory.
;		     The size of FatSpace must be the largest of
;			1) FAT
;			2) Root Dir
;			3) Cluster size needed for 1.5K
;
;	mov	AX,DeviceParameters.DP_BPB.BPB_RootEntries
;	shl	AX,1			; Multiply by 2 to get total para size
;					; (Each entry is 32 bytes)
;
;	cmp	AX,BX			; now see which is bigger
;	jna	CheckClusters		; Use FAT size if BX >= AX
;
;UseDirSize:
;	mov	BX,AX			; Root directory is bigger
;
;CheckClusters:				; Now BX contains the larger of the
;					; FAT and RootDir size, in paras
;					; Calculate the para size of the #
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
;	add	AX, 0fH			; Convert to paras
;	shr	AX, 1
;	shr	AX, 1
;	shr	AX, 1
;	shr	AX, 1
;
;	cmp	AX,BX			; Now AX=para size for clusters in 1.5K
;	jna	AllocateAsIs		; Use larger of FAT,RootDir if not bigger
;
;UseClustSize:
;	mov	BX,AX			; Clusters in 1.5K is bigger

SaveFatSize:
	mov	Paras_per_fat,BX	; Set paras_per_fat here, to 
					;  avoid having to calculate it later
					; Now add on root dir + extra sectors
	mov	AX,DeviceParameters.DP_BPB.BPB_RootEntries
	shl	AX,1			; AX = para size of root dir

	add	BX,AX			; BX = FAT + root dir

	mov	AX, DeviceParameters.DP_BPB.BPB_BytesPerSector
	add	AX, 0fH			; round up for next para
	shr	AX, 1			; convert to paras
	shr	AX, 1
	shr	AX, 1
	shr	AX, 1			; AX = sector size in paras

	mov	CX,SECTORS_FOR_MIRROR	; CX = # additional sectors needed by Mirror
	mul	CX			; AX = total extra sector size in paras

	add	BX,AX			; BX = FAT + root dir + extra sectors
					;  in paras
	mov	AH,Alloc
	int	21h
	jc      Exit_Alloc_Fat_Buf

	mov	WORD PTR FatSpace+2,AX
	xor	AX,AX
	mov	WORD PTR FatSpace,AX

Exit_Alloc_Fat_Buf:
	ret

Alloc_Fat_Buf	endp

;==========================================================================
;
;  Alloc_Fat_Sec_Buf : This procedure allocates a memory block for the fat 
;		       sector buffer which is used when copying chains from
;		       the old FAT to the new FAT.
;
;  Inputs   	  :  DeviceParameters.DP_BPB.BPB_BytesPerSector
;  Outputs	  :  CY CLEAR - FatSector pointer to buffer
;		     CY SET   - failure
;  Modifies	  :  AX, BX, FatSector
;
;==========================================================================

Alloc_Fat_Sec_Buf	proc	near
					; FatSector =
				 	; malloc( Bytes Per Sector )
	mov	BX, DeviceParameters.DP_BPB.BPB_BytesPerSector
	add	BX, 0fH
	shr	BX, 1			; Divide by 16 to get #paragraphs
	shr	BX, 1
	shr	BX, 1
	shr	BX, 1
	mov	AH, Alloc
	int	21h
	jc      Exit_Alloc_Fat_Sec_Buf
					; Base address of newly allocated
					; block is AX:0000
	mov	WORD PTR FatSector+2,AX
	xor	AX,AX
	mov	WORD PTR FatSector,AX

Exit_Alloc_Fat_Sec_Buf:
	ret

Alloc_Fat_Sec_Buf	endp

;===========================================================================
; Routine name:	Alloc_Header_Buf
;===========================================================================
;
; Description: Allocate the work buffers that will be needed by SAFE to
;	       build the restore file.
;
; Arguments:		None
; ---------------------------
; Returns:   		Carry set if error
; ----------------------------------------
; Registers destroyed:	AX BX DX
; ----------------------------------------
; Strategy:
; ---------
;	Allocate Buffer for file header	+ first root DIR sector
;===========================================================================

;Alloc_Header_Buf	 proc	 near
;
;	mov	BX,deviceParameters.DP_BPB.BPB_BytesPerSector ; Sector size
;	add	BX,(HEADER_SIZE + 15)		; Add header size and round
;						; for para conversion
;ConvertToParas:
;	shr	BX,1				; Convert to paragrphs by
;	shr	BX,1				; dividing by 16
;	shr	BX,1
;	shr	BX,1
;
;	mov	AH,48h				; DOS allocate memory function
;	int	21h
;	jc	Exit_Alloc_Header_Buf		; Check for error
;
;SaveAddresses:
;	mov	WORD PTR HeaderBuf[2],AX	; Save header buffer segment
;	mov	WORD PTR HeaderBuf,0		; Set offset to 0
;
;	add	AX,(Header_Size SHR 4)		; Find header buffer segment
;	mov	WORD PTR DirBuf[2],AX		; Save DIR buf segment address
;	mov	WORD PTR DirBuf,0		; Set offset to 0
;
;Exit_Alloc_Header_Buf:
;	ret
;
;Alloc_Header_Buf	endp



;==========================================================================
;
;  Alloc_DirBuf2  :  This procedure allocates a memory block for a 1-sector
;		     buffer.  This buffer is used when reading in the boot
;		     sector in Phase1.
;
;  Inputs   	  :  DeviceParameters.DP_BPB.BPB_BytesPerSector
;  Outputs	  :  CY CLEAR - DirBuf pointer to buffer
;		     CY SET   - failure
;  Modifies	  :  AX, BX, DirBuf
;
;==========================================================================

Alloc_DirBuf2	proc	near
					; DirBuf =
				 	; malloc( Bytes Per Sector )
	mov	BX, DeviceParameters.DP_BPB.BPB_BytesPerSector
	add	BX, 0fH
	shr	BX, 1			; Divide by 16 to get #paragraphs
	shr	BX, 1
	shr	BX, 1
	shr	BX, 1
	mov	AH, Alloc
	int	21h
	jc      Exit_Alloc_DirBuf2
					; Base address of newly allocated
					; block is AX:0000
	mov	WORD PTR DirBuf+2,AX
	xor	AX,AX
	mov	WORD PTR DirBuf,AX

Exit_Alloc_DirBuf2:
	ret

Alloc_DirBuf2	endp



;=========================================================================
; Alloc_Cluster_Buf	         : This	routine	will allocate a	buffer
;				   based on a cluster's	size.  If enough
;				   space does not exist, a cluster will
;				   be redefined	to a smaller size for
;				   purposes of sector retries.
;				   Note: This buffer is used only for bad
;				   tracks on hard disks.
;
;	 Inputs	 : DeviceParameters.DP_BPB.BPB_BytesPerSector
;		   DeviceParameters.DP_BPB.BPB_SectorsPerCluster
;
;	 Outputs : ClustBound_Flag	 - True	(space available)
;					   False(not enough space)
;		   ClustBound_Buffer_Seg - Pointer to buffer
;=========================================================================

Procedure Alloc_Cluster_Buf

	push	AX				; Save regs
	push	BX

	mov	AX,(Alloc shl 8)		; Allocate memory
	mov	BX,0ffffh			; Get available memory
	int	21h

	mov	AX, DeviceParameters.DP_BPB.BPB_BytesPerSector
	add	AX, 0fH
	shr	AX, 1
	shr	AX, 1
	shr	AX, 1
	shr	AX, 1
	mul	DeviceParameters.DP_BPB.BPB_SectorsPerCluster

	cmp	BX,AX				; Enough room
	jna	$$IF137 			; Yes

	mov	BX,AX				; Allocate needed memory
	mov	AX,(Alloc shl 8)
	int	21h
	mov	ClustBound_Buffer_Seg,AX	; Save pointer to buffer
	mov	ClustBound_Flag,True		; Signal space available
	jmp	SHORT $$EN137			; Not enough room

$$IF137:
	mov	ClustBound_Flag,False		; Signal not enough space

$$EN137:
	pop	BX				; Restore regs
	pop	AX

	ret

Alloc_Cluster_Buf ENDP

;=========================================================================
;
;  DO_SWITCH_S  :	This procedure will load the system files into
;			memory (if there's space) if the /s switch is
;			specified.
;
;  CALLS  :		ReadDos
;			SysPrm
;  CALLED BY :		Global_Init
;  STRATEGY :		The largest block of memory available is first
;			determined.  The program is aborted if this is zero.
;			This block is then allocated, and the system files
;			are read into it.  A prompt for the system disk
;			will be given if the system files are not found.
;
;=========================================================================

Do_Switch_S	proc	near

	test	SwitchMap,SWITCH_S
	jz	End_Do_Switch_S			; System files not required
						; allocate memory for system files
	mov	BX,0ffffh			; This call will actually fail
	mov	AH,Alloc			; so that BX returns max block avlbl
	int	21h

	or	BX,BX
	jz	MemErr  			; No memory
	mov	[mSize],BX			; Now allocate the largest block
	mov	AH,alloc
	int	21h
	jnc	Mem_OK

MemErr:
	mov	AX, seg data			; Check for memory allocation error
	mov	DS, AX
	Message msgOutOfMemory			; call PrintString
;	jmp	FatalExit
	stc 				; Let the jump to FatalExit be made
	jmp	SHORT End_Do_Switch_S	;  in the main routine, upon returning

Mem_OK:
	mov	[mStart],AX			; Save the starting paragraph

; =========================================================================
; This call to ReadDos may not be able to read in all of the DOS files if
; there is insufficient memory available. In that case the files will
; be read in after the disk is formatted. If the Drive being formatted is
; also the boot Drive this function will read the files from that
; Drive if there is enough memory. If there is insufficent memory it will
; force the files to be read from Drive A: if the Drive being formatted
; is also the boot Drive
; M011; Wrong: Try Boot, Then Default, Then sysprm (usually "A").
;       If not enough memory at boot time, we fail.
; =========================================================================

RdFrst:

;M011 - begin

	mov	AH,GET_DEFAULT_Drive		; Find out default Drive
	int	21h
	push	AX				; save default Drive
	mov	AH,Get_In_Vars			; Find out boot	Drive
	int	21h
						; get 1 based Drive ID
	mov	AL,BYTE PTR ES:[BX].SysI_Boot_Drive
	add	AL,40h				; Make it ASCII
	pop	BX				; restore default Drive
	cmp	AL,41h				; Q: Booted from Drive A?
	jnz	go_get_Bios			;  N: Not a special case
	cmp	bl,1				; Q: is	B: current Drive
	jnz	go_get_Bios			;  N: Not a special case
	jmp	short check_default		; check	default	Drive

go_get_Bios:					; Here to check booted
	call	Get_Host_Drive			; Translate to DblSpace host
	mov	SystemDriveLetter,AL		;   (if necessary)

	call	ReadDos
	jnc	CheckAllFilesIn

check_default:					; Here to check default
	mov	AH,GET_DEFAULT_Drive		; Find out default Drive
	int	21h
	add	AL,41h				; Make it ASCII, 1 based
	call	Get_Host_Drive			; Translate to DblSpace host
	mov	SystemDriveLetter,AL

TryThisOne:
	call	ReadDos				; Read BIOS and	DOS
	jnc	CheckAllFilesIn			; Files read in OK
NeedSys:
	call	SysPrm				; Prompt for system disk
	jmp	TryThisOne			; Try again

;M011 - end


CheckAllFilesIn:
				; abort program here if all system files
				; have not been read into memory, since
				; program fails when trying to read them
				; in after formatting is complete
	and	FileStat,3fh			; zero out 2 msb
	cmp	FileStat,2ah			; are all 3 sys files in memory?
	jne	MemErr				; no - abort program

	clc					; yes
End_Do_Switch_S:
	ret

Do_Switch_S	ENDP

;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Get_Host_Drive
;*******************************************************************************
;
;Description: Given a drive letter in AL, check to see if it is a dblspace
;	      drive, and if so, translate the drive letter to the host
;	      drive letter.
;
;Called Procedures: None
;
;Input: ASCII drive letter in AL
;
;Output: drive letter in AL
;
;Change History: Created			11/21/92	 MD
;		 Cut and paste from SYS command 12/07/92	 JEM
;
;******************* END OF SPECIFICATIONS *************************************

public Get_Host_Drive

Get_Host_Drive PROC NEAR

        push    ax
   	mov	ax,4a11h	; DBLSPACE multiplex number
	xor	bx,bx		; inquire version number
	int	2fh
	or	ax,ax		; error?
	jnz	not_dblspace
	cmp	bx,'DM'		; stamp returned correctly?
	jnz	not_dblspace

;	DBLSPACE.BIN is loaded.  At this time:
;
;	(dx & 0x7fff) == driver internal version number
;	high bit of DH set of driver has not yet been permanently placed
;	cl == first disk letter reserved for DBLSPACE
;	ch == number of disk letters reserved for DBLSPACE

	mov	ax,4a11h	; DBLSPACE multiplex number
	mov	bx,1		; inquire drive map
        pop     dx
	push	dx
	sub	dl, 'A' 	; convert drv letter to 0 based drv number
	int	2fh
	test	bl,80h		; COMPRESSED bit true?
	jz	not_dblspace

;	Drive is compressed.  At this time:
;
;	(bl & 0x7f) == host drive's CURRENT drive number
;	bh          == CVF extension number
;
        mov     al,bl
	and	al,7Fh
	add	al, 'A' 	; convert drv number to drv letter
        cbw
        pop     dx
        push    ax

not_dblspace:
        pop     ax
        ret

Get_Host_Drive	ENDP

CODE	ENDS

	END
