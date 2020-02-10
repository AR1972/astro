;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

vseg segment byte public 'CODE'
	assume cs:vseg,ds:vseg,es:NOTHING

PUBLIC check_for_dangerous_disk_partitions
PUBLIC last_safe_byte


_PartitionEntry struc

 PartBoot	db	?			; Boot indicator
 PartBeginHead	db	?			; Beginning Head
 PartBeginSect	db	?			; Beginning Sector
 PartBeginCyl	db	?			; Beginning Cylinder
						; Above two bytes are in
						; INT 13h format bits 8 & 9
						; of Cylinder in high 2 bits
						; of PartBeginSect
 PartSystemID	db	?			; Partition ID
 PartEndHead	db	?			; Ending Head
 PartEndSect	db	?			; Ending Sector
 PartEndCyl	db	?			; Ending Cylinder
						; See Comment for PartStartCyl
 PartRelStart	dd	?			; Starting Sector (Relative to
						; beginning of Disk
 PartNumSects	dd	?			; Number of sectors in
						; Partition

_PartitionEntry	ends

PARTENTRY_SIZE	equ	(SIZE _PartitionEntry)

MBR_SIGNATURE	equ	0aa55h
MBR_SIGLENGTH	equ	2

ONTRACK_SIG	equ	055aah			; M013
ONTRACK_SIGLEN	equ	2
ONTRACK_NUMPARTS equ	16
ONTRACK_PARTSIZE equ	(ONTRACK_NUMPARTS * PARTENTRY_SIZE)
SSTOR_NUMPARTS	equ	8
DOS_NUMPARTS	equ	4

MAXMBRCODESIZE	equ	(512-ONTRACK_PARTSIZE-MBR_SIGLENGTH-ONTRACK_SIGLEN)

__MBR		struc
 MBR_Code	db	MAXMBRCODESIZE dup (?)
 MBR_OnTrkSig	dw	?
 MBR_OnTrkPart	db	((ONTRACK_NUMPARTS-SSTOR_NUMPARTS)*(PARTENTRY_SIZE)) dup (?)
 MBR_SStorPart	db	((SSTOR_NUMPARTS-DOS_NUMPARTS)*(PARTENTRY_SIZE)) dup (?)
 MBR_DOSPart	db	(DOS_NUMPARTS * (PARTENTRY_SIZE)) dup (?)
 MBR_Sign	dw	?
__MBR		ends


; Partition system indicators for disk partitions which we may
; not work with:

RiskyPartitions		label	byte

		db	50h,51h		; Ontrack's Disk Manager
		db	56h		; Golden Bow's Vfeature
		db	61h,63h,64h,66h,71h,73h,74h,76h

SSTORParts	label	byte					; M10
		db	0E1h,0E3h,0E4h,0E6h,0F1h,0F3h,0F4h,0F6h
			     ;M07 ;M03	; Storage Dimensions' SpeedStor
NUMSSPARTIDS	equ	$-SSTORParts				; M10

		db	21h,23h,24h,26h,31h,33h,34h,36h
		db	0A1h,0A3h,0A4h,0A6h,0B1h,0B3h,0B4h,0B6h
			     ;M07 ; HP Volume Expansion (SpeedStor derivative)
		db	45h	  ;M03	; Priam

NUM_RISKYPARTITIONS	equ	$-RiskyPartitions

		db	05		; Extended DOS partition; M10

EXTNUM_RISKYPARTS	equ	$-RiskyPartitions		; M10


VfeatureStr	db	"Vfeature"	; string to identify Vfeature

LEN_VFEATURESTR	equ	$-VfeatureStr

PartitionCheck	db	1		; check for incompatible disk
					;  partitions = true

; M10 - BEGIN

ExtPartCheck	db	1		; scan extended master boot record
					; for incompatible disk partitions
; M10 - END

UMBCheck	db	1	      ;M06  ; check that we can read all disks
UMBCheckSig	db	"DOlSMRDO"    ;M06  ; signature	M10
CHECKSIGLEN	equ	$-UMBCheckSig ;M06  ; signature length

check_for_dangerous_disk_partitions proc near

;
; 3.5  Check for dangerous disk partitions.
;
	mov	bp,sp			; BP = saved stack pointer

	push	cs
	pop	es
	assume	es:vseg

;	Determine number of drives through BIOS GetParams call.
;
;	This may not work correctly with some systems (some Compaq's?)
;	that return only the number of drives attached to the corresponding
;	controller.  MS-DOS has decided to support the IBM approach,
;	which returns the total number of hard drives for any GetParams call.

	mov	dl,80h			; DL = physical drive number
	mov	ah,8			; AH = 'Get Drive Parameters'
	int	13h			; call BIOS
	jc	PartitionCheckDone	; no hard drives? - give up

;	DL = number of drives

	xor	dh,dh			; DX = number of drives
	mov	cx,dx			; CX = number of drives

	mov	dl,80h			; DL = physical drive number

GetPartitionTable:
	push	cx			; save number of drives remaining

	mov	bx,offset temp_buffer	; ES:BX = ptr to buffer
	mov	dh,0			; DH = head = 0
	mov	cx,1			; CX = cylinder/sector = 0/1
	mov	ax,0201h		; AX = function/#secs = READ/1
;	push	dx			; save drive number
	stc
	int	13h			; call BIOS disk services
;	pop	dx			; DL = drive number
	jc	PartitionCheckDone	; BIOS is fucking with us - give up

	mov	bx,MBR_DOSPart		; BX = offset of partition table
	mov	cx,DOS_NUMPARTS		; CX = # partition entries in table

NextPartitionEntry:
	push	cx			; save partition entry count
	mov	al,temp_buffer[bx].PartSystemID
					; AL = partition system indicator
	mov	di,offset RiskyPartitions
					; ES:DI = ptr to list of risky
					;  partition system indicators
	mov	cx,NUM_RISKYPARTITIONS	; CX = length of list
	cld
	repne	scasb			; scan list for match
	pop	cx			; CX = partition entry count again
;	je	RiskyPartition		; found a risky partition

 	jne	@f
	jmp	RiskyPartition
@@:
	add	bx,PARTENTRY_SIZE	; BX = offset of next partition entry
	loop	NextPartitionEntry

; M10 - BEGIN
;
;	Look for Extended MBR partition entries SPEEDSTOR has 4 more
;	entries but no signature in the MBR. Ontrack has 12 more entries
;	but will have signature AA55 at offset FC.
;
;	Speedstor uses system indicator 05 (DOS extended partition ID) also.
;	So if we find a 05 partition system indicator in partition entries
;	5 thru 8 we assume that it is a Speeed Stor MBR.
;

	cmp	ExtPartCheck, 0		; do we want to scan extended MBR ?
	je	NoExtPartCheck		; no

	mov	bx, MBR_OnTrkPart	; Setup regs for Ontrack disk scan
	mov	cx, (ONTRACK_NUMPARTS-DOS_NUMPARTS)
	mov	si, NUM_RISKYPARTITIONS

	cmp	word ptr temp_buffer[MBR_OnTrkSig], ONTRACK_SIG
					; Is it an OnTrack Master boot record ?
	je	NextExtPartEntry	; yes

	mov	bx, MBR_SStorPart	; setup regs for SSTOR disk type
	mov	cx, (SSTOR_NUMPARTS-DOS_NUMPARTS)
	mov	si, EXTNUM_RISKYPARTS	; This includes DOS extended partition
					; system indicator 5 also, since
					; SppedStor uses this ID also.

NextExtPartEntry:
;
; M12 - BEGIN
;
	mov	al, temp_buffer[bx].PartBoot
	and	al, 7fh
	or	al, al			; partition boot indicator should
					;  be either 0 or 80h
					;  else we assume that it is an
					;  invalid partition entry
	jnz	next_entry
;
; M12 - END
;
	mov	di, offset RiskyPartitions
	mov	al, temp_buffer[bx].PartSystemID
					; AL = partition system indicator
					; ES:DI = ptr to list of risky
					;  partition system indicators

	push	cx			; save partition entry count
	mov	cx, si			; CX = length of list

	cld
	repne	scasb			; scan list for match
	pop	cx			; CX = partition entry count again
	jne	next_entry
	jmp	RiskyPartition		; found a risky partition
next_entry:
	add	bx,PARTENTRY_SIZE	; BX = offset of next partition entry
	loop	NextExtPartEntry

NoExtPartCheck:
;
; M10 - END
;
	inc	dl			; DL = next drive number
	pop	cx			; CX = number of drives remaining
	loop	GetPartitionTable	; go check partitions on next drive
;
; M10 - BEGIN
;
PartitionCheckDone:
;
;	Chain thru the Extended partitions (id == 05) and make sure that
;	there are no Speedstor partitions within them.
;
	cmp	ExtPartCheck, 0		; do we want to scan extended partns ?
	je	ExtPartitionCheckDone	; no

	mov	dl,80h			; DL = physical drive number
	mov	ah,8			; AH = 'Get Drive Parameters'
	int	13h			; call BIOS
	jc	ExtPartitionCheckDone	; no hard drives? - give up

;	DL = number of drives

	xor	dh,dh			; DX = number of drives
	mov	cx,dx			; CX = number of drives

	mov	dl,80h			; DL = physical drive number

ExtGetPartitionTable:
	push	cx			; save number of drives remaining

	mov	bx,offset temp_buffer	; ES:BX = ptr to buffer
	mov	dh,0			; DH = head = 0
	mov	cx,1			; CX = cylinder/sector = 0/1
	mov	ax,0201h		; AX = function/#secs = READ/1
	push	dx			; save drive number
	stc
	int	13h			; call BIOS disk services
	pop	dx			; DL = drive number
	jc	ExtPartitionCheckDone	; BIOS is fucking with us - give up

NextExtMBR:
	mov	bx,MBR_DOSPart		; BX = offset of partition table
	mov	cx,DOS_NUMPARTS		; CX = # partition entries in table
NextExtPart:
	push	cx			; save partition entry count
	mov	al,temp_buffer[bx].PartSystemID
					; AL = partition system indicator
	mov	di,offset SSTORParts	; point to Speedstor partn IDs
					; ES:DI = ptr to list of risky
					;  partition system indicators
	mov	cx,NUMSSPARTIDS		; CX = number of SpeedStor partn IDs
	cld
	repne	scasb			; scan list for match
	pop	cx			; CX = partition entry count again
	je	RiskyPartition		; found a risky partition

	add	bx,PARTENTRY_SIZE	; BX = offset of next partition entry
	loop	NextExtPart

;
;	scan and find the extended partition entry (if there is one)
;
	mov	bx, MBR_DOSPart
	mov	cx, DOS_NUMPARTS
Next05:
	cmp	temp_buffer[bx]+4, 05	; Extended partition ?
	je	ReadExtendedMBR
	add	bx, PARTENTRY_SIZE
	loop	Next05

;
;	no more extended partition, try the next drive
;
	jmp	short NextExtMBRDrv
;
;	found one extended partition entry
;	temp_buffer[bx] : pointer to the extended partition entry
;
ReadExtendedMBR:

	mov	ax, 201h		; read one sector
	mov	cx, word ptr temp_buffer[bx].PartBeginSect
					; CX = cylinder/sector number
	mov	dh, temp_buffer[bx].PartBeginHead
					; DH = head number
	mov	bx, offset temp_buffer	; into temp_buffer
	stc
	int	13h
	jc	NextExtMBRDrv		; give up on this drive in case of err
	cmp	word ptr temp_buffer[MBR_Sign], MBR_SIGNATURE
					; valid boot ?
	je	NextExtMBR		; start scanning for Speedstor
					; partitions in the newly read in MBR

NextExtMBRDrv:
	inc	dl
	pop	cx
	loop	ExtGetPartitionTable

	jmp	short ExtPartitionCheckDone
;
; M10 - END
;
RiskyPartition:

;	We found a partition that is on our list of at-risk systems.  
;	Complain about it and abort our load.

	mov	sp,bp			; restore stack pointer
	stc	
	ret

ExtPartitionCheckDone:
	mov	sp,bp			; restore stack pointer

	assume	es:NOTHING

;	All the partition system indicators looked ok.
;	Vfeature Deluxe (Golden Bow Systems) could still be out there.
;	Scan the device chain for their device driver.
;	Look at offset 0Bh from each device header for the string "Vfeature".

	mov	ah,52h			; AH = 'Get Sysinit Variables'
	int	21h

;	ES:BX = ptr to sysinit variables

	add	bx,22h			; ES:BX = ptr to NUL device header

NextVfeatureCheck:
	mov	di,bx			; ES:DI = ptr to device header
	add	di,0Bh			; ES:DI = ptr to possible "Vfeature"
	mov	si,offset VfeatureStr	; DS:SI = ptr to "Vfeature" constant
	mov	cx,LEN_VFEATURESTR	; CX = string length
	cld
	repe	cmpsb			; compare the strings
	je	RiskyPartition		; they're the same - go abort

	les	bx,es:[bx]		; ES:BX = ptr to next device header
	cmp	bx,0FFFFh		; check for end of device chain
	jne	NextVfeatureCheck	; not last device - check the next one


NoRisk:
	clc
	ret

check_for_dangerous_disk_partitions endp


last_safe_byte	db	?

temp_buffer 	db 	1 
vseg ends

end 

