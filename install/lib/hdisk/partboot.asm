;========================================================
COMMENT #

	PARTBOOT.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================

	Reads in the first sector (boot	record) of a hard
	disk partition.

	int ReadPartBootRec( struct Part *PartEntry, void *Buffer,
			     int DrvNum );

	ARGUMENTS:	PartEntry - Partition table entry structure
			Buffer	- Ptr to read buffer of 512 bytes
			DrvNum	- Physical hard disk number
	RETURNS:	int	- 0 if successful
				- !0 if error

	=================================================

	STRATEGY:

		Does an int 13h function 2 which reads a
		disk sector based on the following register
		settings which are taken from the passed
		parameters.

			AL = Number of sectors to read
			CH = Low 8 bits of max cylinder num
			CL = Bits 6&7 high order bytes of
			     max cylinder num
			     Bits 0-5 max sector num
			DH = Max head num
			DL = Number of drives (ignored)
			ES:BX = Ptr to buffer in memory

	=================================================

	johnhe - 02/07/99

END COMMENT #
; =======================================================

PartitionInfo	struc

	BootSig		db	?
	StartHead	db	?
	StartSector	db	?
	StartCyl 	db 	?

	PartType 	db 	?
	EndHead		db 	?
	EndSec		db 	?
	EndCyl		db 	?
	RelativeSec	dd	?
	TotalSecs	dd 	?

PartitionInfo	ends

; =======================================================

INCLUDE	model.inc

; =======================================================

.CODE

; =======================================================

IF @DataSize
  ReadPartBootRec PROC USES DS ES, PartEntry:PTR, Buffer:PTR, DrvNum:BYTE
ELSe
  ReadPartBootRec PROC             PartEntry:PTR, Buffer:PTR, DrvNum:BYTE
ENDIF

;	lds	BX,PartEntry		; DS:BX -> partition table entry
	LoadPtr	DS, BX, PartEntry	; DS:BX -> partition table entry

	mov	DH,[BX].StartHead	; DH == Disk head number
	mov	CX,WORD PTR [BX].StartSector ; CH == Cylinder, CL == Sector

	mov	DL,DrvNum		; DL == Physical disk number

;	les	BX,Buffer		; ES:BX -> Sector buffer
	LoadPtr	ES, BX, Buffer		; ES:BX -> Sector buffer

	mov	AX,0201h		; AH == Disk read, AL = 1 sector
	int	13h			; ROM BIOS disk access
	mov	AX,-1			; Setup for possible error
	jc	ReadBootRecExit		; Error check
	xor	AX,AX			; Signal no error

ReadBootRecExit:
	ret

ReadPartBootRec ENDP

; =======================================================

IF @DataSize
  WritePartBootRec PROC USES DS ES, PartEntry:PTR, Buffer:PTR, DrvNum:BYTE
ELSE
  WritePartBootRec PROC             PartEntry:PTR, Buffer:PTR, DrvNum:BYTE
ENDIF


;	lds	BX,PartEntry		; DS:BX -> partition table entry
	LoadPtr	DS, BX, PartEntry	; DS:BX -> partition table entry
	mov	DH,[BX].StartHead	; DH == Disk head number
	mov	CX,WORD PTR [BX].StartSector ; CH == Cylinder, CL == Sector

	mov	DL,DrvNum		; DL == Physical disk number

;	les	BX,Buffer		; ES:BX -> Sector buffer
	LoadPtr	ES, BX, Buffer		; ES:BX -> Sector buffer

	mov	AX,0301h		; AH == Disk read, AL = 1 sector
	int	13h			; ROM BIOS disk access
	mov	AX,-1			; Setup for possible error
	jc	WriteBootRecExit	; Error check
	xor	AX,AX			; Signal no error

WriteBootRecExit:
	ret

WritePartBootRec ENDP

; =======================================================


END
