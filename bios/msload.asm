;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1981-1991
; *                      All Rights Reserved.
; */
	page	,160
	title	non-contiguous io.sys loader (msload)

; =========================================================================
;	NOTE:  The boot loader should be verifying that the first
;	  block of io.sys is, in fact, at cluster 2.  This would be saving
;	  a whole lot of time during system debugging.
;
;==============================================================================
;
;     for dos 4.00, msload program has been changed to allow:
;	  1. 32 bit calculation,
;	  2. reading a fat sector when needed, instead of reading the whole fat
;	     sectors at once.  this will make the boot time faster, and eliminate
;	     the memory size limitation problem,
;	  3. solving the limitation of the file size (29 kb) of io.sys0,
;	  4. adding the boot error message.  show the same boot error message
;	     and do the same behavior when the read operation of io.sys
;	     fails as the msboot program, since msload program is the
;	     extention of msboot program.
;
; =========================================================================

;
;----------------------------------------------------------------------------
;
; M056 : Added RPL support, so that RPL's fake INT 13 code can be safe from
;		SYSINIT & transient portion of COMMAND.COM
;
;----------------------------------------------------------------------------
;
include bpb.inc
include bootform.inc		; extended bpb, boot record defintion.
include versiona.inc		; version number for sys.com
;include dirent.inc
include	dossym.inc
include	dosmac.inc
include mult.inc

; =========================================================================

PUBLIC DIR_ENTRY_PER_SEC
PUBLIC BiosStart
PUBLIC DskAddr
PUBLIC BootSector
PUBLIC IoSysAddr
PUBLIC Sec9
PUBLIC SysVersion
PUBLIC MyStacks
PUBLIC StackPtr
PUBLIC NumHeads
PUBLIC ClusterSize
PUBLIC StartSecL
PUBLIC StartSecH
PUBLIC TempH
PUBLIC TempCluster
PUBLIC LastFatSector
PUBLIC SectorCount
PUBLIC SecPerFat
PUBLIC HiddenSectorsL
PUBLIC HiddenSectorsH
PUBLIC BytesPerSec
PUBLIC ReservSectors
PUBLIC CurrentCluster
PUBLIC NextBioLocation
PUBLIC FirstSectorL
PUBLIC FirstSectorH
PUBLIC TotalSectorsL
PUBLIC TotalSectorsH
PUBLIC SecPerTrack
PUBLIC BootDrive
PUBLIC FatSize
PUBLIC MediaByte
PUBLIC EndOfFile
PUBLIC OrgDasdPtr
PUBLIC FatSegment
PUBLIC SecPerCluster

; =========================================================================

END_OF_FILE		EQU	0ffh
FAT_12_BIT		EQU	01h
FAT_16_BIT		EQU	04h
ROM_TTY			EQU	14	; Int 10h, teletype function
DIR_ENTRY_PER_SEC 	EQU	16	; Num of directory entries per sec

BiosStart		EQU	51ah	; Bios starting cluster in root dir
Sec9			EQU	522h
BiosOffset		EQU	700h
DskAddr			EQU 	1eh * 4	; ROM BIOS diskette table vector

; =========================================================================

BootSeg 	SEGMENT at 0h
       ORG	7c00h

BootSector	label	byte

BootSeg 	ends

; =========================================================================

DosLoadSeg	SEGMENT at 70h
       ORG	00h

IoSysAddr	label	byte

DosLoadSeg	ends

; =========================================================================

cSeg	SEGMENT PUBLIC para 'code'
	ASSUME	CS:CSEG, DS:NOTHING, ES:NOTHING, SS:NOTHING
	ORG	0

Start:
	jmp	SaveInputValuess

SysVersion		dw	expected_version ; from versiona.inc file
MyStacks		dw	128 DUP (0)	; local stack, increased size to
StackPtr		LABEL	WORD

NumHeads		dw	0
ClusterSize		dw	0
StartSecL		dw	0
StartSecH		dw	0
TempH			dw	0	; for 32 bit calculation
TempCluster		dw	0	; temporary place for cluster number
LastFatSector		dw	-1	;fat sector number starting from the first fat entry.
SectorCount		dw	0
SecPerFat		dw	0
HiddenSectorsL		dw	0
HiddenSectorsH		dw	0
BytesPerSec		dw	0
ReservSectors		dw	0
CurrentCluster		dw	0
NextBioLocation		dw	0
FirstSectorL		dw	0
FirstSectorH		dw	0
TotalSectorsL		dw	0	;.max. number of sectors
TotalSectorsH		dw	0
SecPerTrack		dw	0
BootDrive		db	0
FatSize			db	0
MediaByte		db	0
EndOfFile		db	0
OrgDasdPtr		dd	0
FatSegment		dw	0
SecPerCluster		db	0

; =========================================================================
;	SaveInputValuess
; =========================================================================
;
; INPUT:     none
;
;   dl = int 13 drive number we booted from
;   ch = media byte
;   bx = first data sector (low) on disk (0-based)
;   ds:si = original rom bios diskette parameter table.
;
; if an extended boot record, then ax will be the first data sector
; high word.	save ax and set FirstSectorH according to ax if it is an
; extended boot record.
;
;   ax = first data sector (high) on disk ;
; OUTPUT:
;
;   bx = first data sector on disk
;
;   MediaByte = input ch
;   BootDrive = input dl
;   FirstSectorL = input bx
;   FirstSectorH = input AX, if an extended boot record.;j.k.
;   TotalSectorsL = maximum sector number in this media ;j.k.
;   TotalSectorsH = high word of the above
;   HiddenSectorsL = hidden secotrs
;   HiddenSectorsH
;   ReservSectors = reserved sectors
;   SecPerTrack = sectors/track
;   NumHeads = heads/cylinder
;
;   ds = 0
;   AX,DX,SI destroyed
;
; calls:     none
; =========================================================================
;FUNCTION:
; save input information and bpb informations from the boot record.
; =========================================================================

	PUBLIC SaveInputValueSS
SaveInputValueSS:

	mov	FirstSectorL,BX
	mov	MediaByte,CH
	mov	BootDrive,DL

	mov	WORD PTR OrgDasdPtr,SI
	push	ds
	pop	WORD PTR OrgDasdPtr+2

        xor     CX,CX                   ; segment 0 (obviously)
	mov	DS,CX
	ASSUME	DS:BootSeg

	push	ES
	mov	ES,CX
	ASSUME	ES:BootSeg

	mov	SI,WORD PTR DS:DskAddr
	mov	DS,WORD PTR DS:DskAddr+2 ; DS:si -> current table

	mov	DI,Sec9 		; ES:di -> new table
	mov	CX,11			; taken from ibmboot.asm
	cld
	rep	movsb			; copy table
	push	ES
	pop	DS			; ds = 0

	mov	WORD PTR DS:DskAddr,Sec9
	mov	WORD PTR DS:DskAddr+2,DS ; point disk parm vector to new table
	pop	ES
	ASSUME	ES:NOTHING

	mov	CX,BootSector.ext_boot_bpb.BPB_bytespersector
	mov	CS:BytesPerSec,CX

	mov	CL,BootSector.ext_boot_bpb.BPB_sectorspercluster
	mov	CS:SecPerCluster,CL

	mov	CX,BootSector.ext_boot_bpb.BPB_sectorspertrack	;get sectors per track
	mov	CS:SecPerTrack,CX

	mov	CX,BootSector.ext_boot_bpb.BPB_heads		;get bpb heads per cylinder
	mov	CS:NumHeads,CX

	mov	CX,BootSector.ext_boot_bpb.BPB_sectorsperfat	;get sectors per fat
	mov	CS:SecPerFat,CX

	mov	CX,BootSector.ext_boot_bpb.BPB_reservedsectors	;get reserved sectors
	mov	CS:ReservSectors,CX

	mov	CX,WORD PTR BootSector.ext_boot_bpb.BPB_hiddensectors ;get hidden sectors
	mov	CS:HiddenSectorsL,CX

	mov	CX,BootSector.ext_boot_bpb.BPB_totalsectors
	mov	CS:TotalSectorsL,CX

		; First of all, check if it the boot record is an extended one.
		; This is just a safe guard in case some user just "copy" the
		; 4.00 iosys.com to a media with a conventional boot record.

	cmp	BootSector.ext_boot_sig,ext_boot_signature
	jne	relocate

	mov	CS:FirstSectorH,AX					; start data sector (high)
	mov	AX,WORD PTR BootSector.ext_boot_bpb.BPB_hiddensectors+2
	mov	CS:HiddenSectorsH,AX
	cmp	CX,0							; cx set already before (=totalsectors)
	jne	relocate

	mov	AX,WORD PTR BootSector.ext_boot_bpb.BPB_bigtotalsectors
	mov	CS:TotalSectorsL,AX
	mov	AX,WORD PTR BootSector.ext_boot_bpb.BPB_bigtotalsectors+2
	mov	CS:TotalSectorsH,AX

; =========================================================================
;	Relocate
; =========================================================================
;
; NOTES:
;
;   Relocates the loader code to top-of-memory.
;
; INPUT:     none
;
; OUTPUT:    code and data relocated.
;	     AX,CX,SI,DI destroyed
;
; calls:     none
; =========================================================================
;
; Determine the number of paragraphs (16 byte blocks) of memory.
; this involves invoking the memory size determination interrupt,
; which returns the number of 1k blocks of memory, and then
; converting this to the number of paragraphs.
; Find out whether RPL code is present at top of memory and modify the
; available amount of memory in AX
; leave the number of paragraphs of memory in ax.
;
;-----------------------------------------------------------------------
; copy code from start to top of memory.
;
; the length to copy is EndOfLoader
;
; jump to relocated code
;-----------------------------------------------------------------------

	PUBLIC	Relocate
Relocate:

	ASSUME	DS:NOTHING
	cld
	xor	SI,SI
	mov	DI,SI

	int	12h			; Get system memory size in kbytes
	mov	CL,6
	shl	AX,CL			; Memory size in paragraphs

;M056 - BEGIN
;
;------ Check if an RPL program is present at TOM and do not tromp over it
;
	xor	bx, bx
	mov	ds, bx
	mov	bx, ds:[2fh*4]
	mov	ds, ds:[2fh*4+2]
	cmp	word ptr ds:[bx+3], 'PR'
	jne	SkipRPL
	cmp	byte ptr ds:[bx+5], 'L'
	jne	SkipRPL

	mov	dx, ax			; get TOM into DX
	mov	ax, (multMULT shl 8) + multMULTRPLTOM
	int	2fh			; Get new TOM from any RPL
	mov	ax, dx
SkipRPL:
;
;M056 - END
;
	mov	CL,4
	mov	DX,CS:BytesPerSec
	shr	DX,CL
	inc	DX
	sub	AX,DX
	mov	CS:FatSegment,AX	; This will be used for fat sector

	mov	DX,OFFSET EndOfLoader
	shr	DX,CL
	inc	DX
	sub	AX,DX
	mov	ES,AX			; ES:di -> place be relocated.

	push	CS
	pop	DS			; DS:si -> source
	mov	CX,OFFSET EndOfLoader
	rep	movsb

	push	ES
	mov	AX,OFFSET SetUpStack
	push	AX			; Massage stack for destin of CS:IP

Dumbbb	PROC	FAR

	ret

Dumbbb	ENDP


; =========================================================================
; Start of relocated code
; =========================================================================
;
; Move the stack to just under the boot record and relocation area (0:7c00h)
;
; =========================================================================

	PUBLIC SetUpStack
SetUpStack:

	ASSUME	DS:NOTHING, ES:NOTHING, SS:NOTHING

	mov	AX,CS
	mov	SS,AX			;set up the stack to the known area.
	mov	SP,OFFSET StackPtr

; =========================================================================
;	FindClusterSize
; =========================================================================
;
; INPUT:     bpb information in loaded boot record at 0:7c00h
;
; OUTPUT:
;
;	ds = 0
;	ax = bytes/cluster
;	bx = sectors/cluster
;	si destroyed
; calls:     none
;-----------------------------------------------------------------------
;
; get bytes/sector from bpb
;
; get sectors/cluster from bpb
;
; bytes/cluster = bytes/sector * sector/cluster
; =========================================================================

	PUBLIC FindClusterSize
FindClusterSize:

;for the time being just ASSUME the boot record is valid and the bpb
;is there.

	xor	AX,AX				;segment 0
	mov	DS,AX

	ASSUME DS:BootSeg

	mov	AX,BootSector.ext_boot_bpb.BPB_bytespersector	  ;get bpb bytes/sector
	xor	BX,BX
	mov	BL,BootSector.ext_boot_bpb.BPB_sectorspercluster ;get sectors/cluster
	mul	bx				;bytes/cluster
	mov	CS:ClusterSize,AX		;save it


; =========================================================================
;	CalcFatSize
; =========================================================================
;
; NOTES:
;
;   Determine if fat is 12 or 16 bit fat. 12 bit fat if floppy, read mbr
;   to find out what system id byte is.
;
; INPUT:
;
; OUTPUT:
;
;   CS:FatSize = FAT_12_BIT or FAT_16_BIT
;   all other registers destroyed
;
;----------------------------------------------------------------------

CalcFatSize:

	mov	CS:FatSize,FAT_12_BIT		;ASSUME 12 bit fat

	mov	DX,CS:TotalSectorsH
	mov	AX,CS:TotalSectorsL		; DX:AX = total disk sectors

	sub	AX,CS:ReservSectors
	sbb	DX,0				; DX:AX = Total avail sectors

	mov	BX,CS:SecPerFat 		; BX = Sectors per FAT
	shl	BX,1				; Assume 2 FATs so mult by 2

	sub	AX,BX
	sbb	DX,0				; DX:AX = Total secs - fat secs

		; Total directory entries per sector = (512 / 32) = 16
		; and (Total dir entries / 16) = Total directory sectors

	mov	BX,BootSector.ext_boot_bpb.BPB_rootentries
	mov	CL,4				; Divide by 16
	shr	BX,CL				; BX = Total directory sectors

	sub	AX,BX				; Subtract directory sectors
	sbb	DX,0				; DX:AX = Sectors in data area

	xor	CX,CX
	mov	CL,BootSector.ext_boot_bpb.BPB_sectorspercluster

	push	ax				; 32 bit divide by sectors per
	mov	AX,DX				; cluster to find total number
	xor	DX,DX				; of clusters.
	div	cx
	mov	CS:TempH,AX
	pop	ax
	div	cx

	cmp	AX,4096-10			; 12 bit fat if < 4096 clusters
	jb	ReadInFirstClusters

	mov	CS:FatSize,FAT_16_BIT		; Else 16 bit fat

; =========================================================================
;	ReadInFirstClusters
; =========================================================================
;
; NOTES: read the start of the clusters that covers at least IbmLoadSize
;	 fully.  for example, if sector/cluster = 2, and IbmLoadSize=3
;	 then we are going to re-read the second cluster to fully cover
;	 msload program in the cluster boundary.
;
; INPUT:
;   IbmLoadSize - make sure this value is the same as the one in
;		  msboot program when you build the new version!!!!!
;
;   SecPerCluster
;   ClusterSize
;   FirstSectorL
;   FirstSectorH
;
; OUTPUT: msload program is fully covered in a cluster boundary.
;	  ax = # of clusters we read in so far.
;
; calls:     ReadSectors
; logic:
;	ax; dx = IbmLoadSize / # of sector in a cluster.
;	if dx = 0 then ok. (msload is in a cluster boundary.)
;      else		   (has to read (ax+1)th cluster to cover msload)
;	read (ax+1)th cluster into the address after the clusters we
;	read in so far.
; =========================================================================

	PUBLIC ReadInFirstClusters
ReadInFirstClusters:

	mov	AX,DS:[BiosStart]	; AX = BIOS starting cluster
	dec	AX			; First cluster is 2 so
	dec	AX			; decrement to make 0 based
	mov	CS:CurrentCluster,AX	; Initialize to this cluster

	mov	AX,IbmLoadSize		; AX = Number sectors in MSLOAD
	div	CS:SecPerCluster	; AL = total cluster read in
					; AH = remaining sectors in last cluster

	cmp	AH,0			; Check for remaining sectors
	je	SetNextClusterNum	; Nothing remaining in last cluster

	xor	AH,AH			; AX = total clusters in the loader
	push	AX			; already read in

		; Calculate sector to start reading from in StartSecH
		; and StartSecL

	mov	CX,CS:FirstSectorL	; Put starting sector of disk data
	mov	CS:StartSecL,CX 	; area in StartSecH:StartSecL
	mov	CX,CS:FirstSectorH
	mov	CS:StartSecH,CX 	; Start sector = First sector dat

	mul	CS:SecPerCluster	; AX = Number of sectors already loaded

	add	CS:StartSecL,AX 	; Add number of sectors already loaded
	adc	CS:StartSecH,0		; to start sector

		; Added to allow BIOS to start anywhere on disk so long
		; as MSLOAD is contigous - 5.00

	mov	AX,DS:[BiosStart]	; AX = BIOS starting cluster
	dec	AX			; Convert to 0 based clusters
	dec	AX			; by subtracting 2


	xor	BX,BX
	mov	BL,CS:SecPerCluster	; BX = sectors per cluster
	mul	BX			; DX:AX = logical start sector
	add	CS:StartSecL,AX		; Add logical sectors to start
	adc	CS:StartSecH,DX		; abs start sector for next read of
					; the rest of the last loader cluster

		; End of changed for 5.00

	pop	AX			; AX = total clusters in the loader
	push	AX			; already read in

	mul	CS:ClusterSize		; AX = bytes in full clusters read in
	mov	DI,BiosOffset		; DI -> Addr where loader was read in
	add	DI,AX			; DI -> Start address for next cluster

	xor	AX,AX
	mov	ES,AX			; ES = segment 0

	mov	AL,CS:SecPerCluster	; AX = sectors per cluster (# to read)
	mov	CS:SectorCount,AX	; SectorCount = sectors to read

	call	ReadSectors		; Read in the entire last cluster

	pop	AX			; AX = total clus read by boot loader
	inc	AX			; AX = total clus read in now

	subttl SetNextClusterNum
	page

SetNextClusterNum:

	inc	AX			; AX = total clusters read in based 2
	add	CS:CurrentCluster,AX	; CurrentCluster = Last cluster read
	dec	AX			; AX = number of clusters loaded


; =========================================================================
; SaveLoadedBios
; =========================================================================
;
; NOTES:
;
;   Determine how much of iosys was loaded in when the loader was loaded
;   by the boot record (only the portion that is guaranteed to be contiguous)
;
; INPUT:
;   AX:Total cluster already read in (loader & bios)
;   CS:CurrentCluster = number of clusters used for loader+2
;
; OUTPUT:
;	ES = 70h
;	DI = next offset to load iosys code
;	AX,BX,CX,DX,SI destroyed
;
;	CS:NextBioLocation = di on output
;	CS:last_cluster = last cluster loaded
;
; calls:     none
;
; =========================================================================
;
; Multiply cluster * cluster size in bytes to get total loaded for msload
;
; Subtract total_loaded - (EndOfLoader) to get loaded io.sys in last cluster
;
; Relocate this piece of iosys down to 70:0
;
; =========================================================================

SaveLoadedBios:

	push	DS

	mul	CS:ClusterSize		; Get total bytes loaded by
					; this is always < 64k, so
					; lower 16 bits ok

					;get portion of iosys loaded
	sub	AX,(OFFSET EndOfLoader)-(OFFSET Start)
	mov	CX,AX			; Save length to move

	mov	AX,70h			; Segment at 70h
	mov	DS,AX			; DS = segment 70h
	mov	ES,AX			; ES = segment 70h

	mov	SI,OFFSET EndOfLoader	; Point at start of iosys
	xor	DI,DI			; Point at 70:0
	rep	movsb			; Relocate this code to 70:00

	mov	CS:NextBioLocation,DI	; Save where location for next read
	pop	DS

; =========================================================================
;	GetContigClusters
; =========================================================================
;
; NOTES: go find clusters as long as they are contiguous
;
;
; INPUT:
;   CS:NextBioLocation
;   CS:
;
; OUTPUT:
;
; calls: GetNextFatEntry
; =========================================================================
;
;Set CS:SectorCount to sectors per cluster
;
;Call GetNextFatEntry to get next cluster in file
;
;Call check_for_eof
;
;if (nc returned)
;
;   {call GetNextFatEntry
;
;    if (new cluster is contig to old cluster)
;	{add sectors per cluster to CS:SectorCount
;
;	 call check_for_eof
;
;	 if (nc returned)
;
;
; =========================================================================

	PUBLIC GetContigClusters
GetContigClusters:

	xor	AH,AH
	mov	AL,CS:SecPerCluster	; Assume we will get one cluster
	mov	CS:SectorCount,AX	; Sector count = sectors in 1 cluster

	push	CS:SectorCount
	call	GetNextFatEntry		; Returns next cluster to read in AX
	pop	CS:SectorCount

	mov	CS:CurrentCluster,AX	; Update the last one found
	cmp	CS:EndOfFile,END_OF_FILE
	je	GoToBioInit

	xor	DX,DX
	sub	AX,2			; Zero base the cluster
	xor	CH,CH
	mov	CL,CS:SecPerCluster	; Get sectors per cluster
	mul	CX			; Get how many

	add	AX,CS:FirstSectorL	; See where the data sector starts
	adc	DX,CS:FirstSectorH

	mov	CS:StartSecL,AX 	; Save it (used by ReadSectors)
	mov	CS:StartSecH,DX

	mov	DI,CS:NextBioLocation	; Get where to put code
	push	CS:SectorCount		; Save how many sectors
	mov	AX,DosLoadSeg		; Get area to load code
	mov	ES,AX

	call	ReadSectors
	pop	AX			; Get back total sectors read in

	mul	CS:BytesPerSec		; Get number of bytes we loaded
	add	CS:NextBioLocation,AX	; Point to where to load next
	jmp	GetContigClusters

; =========================================================================
;	GoToBiosInit
; =========================================================================
;
; NOTES:
;
;  Set up required registers for iosys, then jump to it (70:0)
;
; INPUT:     none
;
;   CS:MediaByte = media byte
;   CS:BootDrive = int 13 drive number we booted from
;   CS:FirstSectorL = first data sector on disk (low) (0-based)
;   CS:FirstSectorH = first data sector on disk (high)
;
; OUTPUT:
;
;   required by msinit
;   DL = int 13 drive number we booted from
;   CH = media byte
;   BX = first data sector on disk (0-based)
;   AX = first data sector on disk (high)
;   DI = sectors/fat for the boot media.
;
; calls:     none
; =========================================================================
;
; set up registers for msinit then do far jmp
;
; =========================================================================

	PUBLIC GoToBioInit
GoToBioInit:

	mov	CH,CS:MediaByte 	; Restore regs required for msint
	mov	DL,CS:BootDrive 	; Physical drv number we booted from.
	mov	BX,CS:FirstSectorL
	mov	AX,CS:FirstSectorH	; BX:AX = first data sector of disk

	jmp	FAR PTR IoSysAddr


; =========================================================================
; ReadSectors
; =========================================================================
;
; notES:
;
;  read in the CS:SectorCount number of sectors at ES:di
;
;
; INPUT:
;
;   DI = OFFSET of start of read
;   ES = segment of read
;   CS:SectorCount = number of sectors to read
;   CS:StartSecL = starting sector (low)
;   CS:StartSecH = starting sector (high)
;   following is bpb info that must be setup prior to call
;   CS:NumHeads
;   CS:number_of_sectors
;   CS:BootDrive
;   CS:SecPerTrack
;
; OUTPUT:
;
;   AX,BX,CX,DX,SI,DI destroyed
; =========================================================================
; divide start sector by sectors per track
; the remainder is the actual sector number, 0 based
;
; increment actual sector number to get 1 based
;
; the quotient is the number of tracks - divide by heads to get the cyl
;
; the remainder is actual head, the quotient is cylinder
;
; figure the number of sectors in that track, set al to this
;
; do the read
;
; if error, do reset, then redo the int 13h
;
; if successful read, subtract # sectors read from SectorCount, add to logical
; sector, add #sectors read * BytesPerSec to bx;
;
; if SectorCount <> 0 do next read
; =========================================================================

ReadSectors PROC	NEAR
	ASSUME	DS:NOTHING, ES:NOTHING

DoDivide:
	mov	CX,5			; 5 retries

		; Convert a logical sector into track/sector/head. AX has the
		; logical sector number
TryRead:
	push	CX			; Save it
	mov	AX,CS:StartSecL 	; Get starting sector
	mov	DX,CS:StartSecH

	push	AX
	mov	AX,DX
	xor	DX,DX
	div	WORD PTR CS:SecPerTrack
	mov	CS:TempH,AX
	pop	AX

	div	WORD PTR CS:SecPerTrack ; [TempH];ax = track, dx = sector number
	mov	BX,CS:SecPerTrack	; Get number of sectors we can read in
	sub	BX,DX			; this track
	mov	SI,BX

	cmp	CS:SectorCount,SI	; Is possible sectors in track more
	jae	GotLength		; than what we need to read?

	mov	SI,CS:SectorCount	; Yes, only read what we need to

GotLength:
	inc	DL			; Sector numbers are 1-based
	mov	BL,DL			; Start sector in DL
	mov	DX,CS:TempH		; DX:AX = Track

	push	AX
	mov	AX,DX
	xor	DX,DX
	div	WORD PTR CS:NumHeads	; Start cyl in AX,head in dl
	mov	CS:TempH,AX
	pop	AX

	div	WORD PTR CS:NumHeads	; [TempH];AX = Cyliner, DX = Head

		; At this moment, we assume that TempH = 0,
		; ax <= 1024, dx <= 255

	mov	DH,DL

		; Issue one read request. ES:BX have the transfer address,
		; AL is the number of sectors.

	mov	CL,6
	shl	AH,CL			; Shift cyl high bits up
	or	AH,BL			; Mix in with sector bits
	mov	CH,AL			; Setup cyl low
	mov	CL,AH			; Setup cyl/high - sector
	mov	BX,DI			; Get back OFFSET
	mov	DL,CS:BootDrive 	; Get drive
	mov	AX,SI			; Get number of sectors to read (al)

	mov	AH,2			; Read
	push	AX			; Save length of read
	push	DI

		; Issue one read request. ES:BX have the transfer address,
		; AL is the number of sectors.

	int	13h
	pop	DI
	pop	AX
	pop	CX			; Get retry count back
	jnc	ReadOk

	mov	BX,DI			; Get offset
	xor	AH,AH
	push	CX
	mov	DL,CS:BootDrive
	push	DI
	int	13h
	pop	DI
	pop	CX
	dec	CX
	jz	ReadError

	jmp	TryRead

ReadError:
	jmp	ErrorOut

ReadOk:
	xor	AH,AH			; Mask out read command, just get # read
	sub	CS:SectorCount,AX	; Bump number down
	jz	EndRead

	add	CS:StartSecL,AX 	; Where to start next time
	adc	CS:StartSecH,0
	xor	BX,BX			; Get number sectors read
	mov	BL,AL
	mov	AX,CS:BytesPerSec	; Bytes per sector
	mul	BX			; Get total bytes read
	add	DI,AX			; Add it to OFFSET
	jmp	DoDivide

EndRead:
	ret

ReadSectors	ENDP

; =========================================================================
;	GetNextFatEntry
; =========================================================================
;
; NOTES:
;
;   given the last cluster found, this will return the next cluster of
;   iosys. if the last cluster is (f)ff8 - (f)fff, then the final cluster
;   of iosys has been loaded, and control is passed to goto_iosys
;   msload can handle maximum fat area size of 128 kb.
;
; INPUT:
;
;    CS:CurrentCluster
;    CS:FatSize
;
; OUTPUT:
;
;   CS:CurrentCluster (updated)
;
; calls:  GetFatSector
; =========================================================================
; get CurrentCluster
;
; if (16 bit fat)
;    {if (CurrentCluster = fff8 - ffff)
;	 {jmp goto_iosys}
;     else
;	{get OFFSET by multiply cluster by 2}
;
; else
;    {if (CurrentCluster = ff8 - fff)
;	 {jmp goto_iosys}
;     else
;	{get OFFSET by	- multiply cluster by 3
;
;	 rotate right to divide by 2
;
;	 if (cy set - means odd number)
;	    {shr 4 times to keep high twelve bits}
;
;	 else
;	    {and with 0fffh to keep low 12 bits}
;	}
;    }
;
;
; =========================================================================

GetNextFatEntry PROC NEAR
	ASSUME	DS:NOTHING, ES:NOTHING

	push	ES
	mov	AX,CS:FatSegment
	mov	ES,AX			; ES-> FAT area segment

	ASSUME	ES:NOTHING

	mov	CS:EndOfFile,END_OF_FILE ; Assume last cluster
	mov	AX,CS:CurrentCluster	; Get last cluster
	cmp	CS:FatSize,FAT_12_BIT
	jne	Got16Bit

	mov	SI,AX
	shr	AX,1
	add	SI,AX			; SI = AX * 1.5 = AX + AX/2

	push	DX			; M054
	xor	DX,DX			; M054
	call	GetFatSector
	pop	DX			; M054

	jne	ClusterOk

	mov	AL,BYTE PTR ES:[bx]
	mov	BYTE PTR CS:TempCluster,AL
	inc	SI

	push	DX			; M054
	xor	DX,DX			; M054
	call	GetFatSector		; Read next fat sector
	pop	DX			; M054

	mov	AL,BYTE PTR ES:[0]
	mov	BYTE PTR CS:TempCluster+1,AL
	mov	AX,CS:TempCluster
	jmp	SHORT EvenOdd

ClusterOk:
	mov	AX,ES:[bx]

EvenOdd:
	test	CS:CurrentCluster,1	; Was last cluster odd?
	jnz	OddResult		; If carry set it was odd

	and	AX,0fffh		; Keep low 12 bits
	jmp	SHORT TestEOF

OddResult:
	 mov	CL,4			; Keep high 12 bits for odd
	 shr	AX,CL

TestEOF:
	 cmp	AX,0ff8h		; Is it last cluster?
	 jae	GotClusterDone		; Yep, all done here

	 jmp	SHORT NotLastCluster

Got16Bit:
	push	DX			; M054
	xor	DX,DX			; M054
	shl	AX,1			; Multiply cluster by 2
	adc	DX,0			; M054
	mov	SI,AX			; Get the final buffer OFFSET
	call	GetFatSector
	pop	DX			; M054

	mov	AX,ES:[bx]
	cmp	AX,0fff8h
	jae	GotClusterDone

NotLastCluster:
	mov	CS:EndOfFile,NOT END_OF_FILE ; Assume last cluster

GotClusterDone:
	pop	ES
	ret

GetNextFatEntry ENDP

; =========================================================================
;
;function: find and read the corresponding fat sector into ES:0
;
;in). SI = offset value (starting from fat entry 0) of fat entry to find.
;     ES = fat sector segment
;     CS:BytesPerSec
;
;out). corresponding fat sector read in.
;      BX = offset value of the corresponding fat entry in the fat sector.
;      CX destroyed.
;      zero flag set if the fat entry is splitted, i.e. when 12 bit fat entry
;      starts at the last byte of the fat sector.  in this case, the caller
;      should save this byte, and read the next fat sector to get the rest
;      of the fat entry value.	(this will only happen with the 12 bit fat).
;
; =========================================================================

GetFatSector	PROC	NEAR
	ASSUME	DS:NOTHING, ES:NOTHING

	push	AX
	push	SI
	push	DI

;	push	DX			; M054
;	xor	DX,DX			; M054


	mov	AX,SI
	mov	CX,CS:BytesPerSec
	div	CX			; AX = Sector number, DX = Offset

	cmp	AX,CS:LastFatSector	; The same fat sector?
	je	SplitChk		; Don't need to read it again.

	mov	CS:LastFatSector,AX

	push	DX
	xor	DX,DX
	add	AX,CS:HiddenSectorsL
	adc	DX,CS:HiddenSectorsH
	add	AX,CS:ReservSectors
	adc	DX,0

	mov	CS:StartSecL,AX
	mov	CS:StartSecH,DX 	; Set up for ReadSectors
	mov	CS:SectorCount,1	; 1 sector

	xor	DI,DI
	call	ReadSectors

	pop	DX
	mov	CX,CS:BytesPerSec

SplitChk:
	dec	CX			; CX = SECTOR SIZE - 1
	cmp	DX,CX			; If last byte of sector, splitted entry.
	mov	BX,DX			; set bx to dx

;	pop	DX			; M054

	pop	DI
	pop	SI
	pop	AX
	ret

GetFatSector	ENDP

; =========================================================================

ErrorOut:
	push	cs
	pop	ds
	mov	SI,OFFSET sysmsg
	call	WriteTTY

		; Wait for a keypress on the keyboard. Use the bios keyboard
		; interrupt.

	xor	AH,AH
	int	16h			;read keyboard

		; We have to restore the address of the original rom disk
		; parameter table to the location at [0:DskAddr]. The address
		; of this original table has been saved previously in
		; 0:OrgDasdPtr and 0:OrgDasdPtr+2. After this table address
		; has been restored we can reboot by invoking the bootstrap
		; loader bios interrupt.

	xor	BX,BX
	mov	DS,BX
	les	BX,dWORD PTR DS:OrgDasdPtr
	mov	SI,DskAddr
	mov	WORD PTR DS:[si],BX	;restore offset
	mov	WORD PTR DS:[si+2],ES	;restore segment
	int	19h			;reboot

; =========================================================================
;
;in) DS:si -> asciiz string.
;
; WriteTTY the character in al to the screen.
; use video service 'write teletype to active page' (ROM_TTY)
; use normal character attribute
;
; =========================================================================

WriteTTY PROC  NEAR			; Show error messages

	lodsb
	or	AL,AL
	jz	EndWrite

	mov	AH,ROM_TTY
	mov	BL,7			; "normal" attribute ?
	int	10h			; video write
	jmp	WriteTTY

EndWrite:
	ret

WriteTTY ENDP

; =========================================================================

include msbio.cl1

EndOfLoader label byte

; =========================================================================

cSeg	ENDS
	END	Start

