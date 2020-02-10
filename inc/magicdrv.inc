;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1992
;	All Rights Reserved.
;

; MagicDrv Int 2Fh Equates

multMagicdrv	equ	4a11h	; Int2f multiplex number

MD_VERSION	equ	0	; Get driver version #
MD_DRIVE_MAP	equ	1	; Get drive mapping info function


; PLAIN_BPB - This is a simple BPB as DOS defines it. Refer to DOS technical
;	      manual for more details

PLAIN_BPB	struc
jmpBOOT 	db	3 dup (?)	;jump to bootstrap routine
achOEMName	db	8 dup (?)	;OEM Name
cbPerSec	dw	?		;count of bytes per sector
csecPerClu	db	?		;count of sectors per cluster
csecReserved	dw	?		;reserved sectors
cFATs		db	?		;count of FATs
cRootDirEntries dw	?		;count of root dir entries
csecTotalWORD	dw	?		;count of total sectors
bMedia		db	?		;media descriptor byte
csecFAT 	dw	?		;count of sectors in FAT
csecPerTrack	dw	?		;count of sectors per track
cHeads		dw	?		;count of disk heads
csecHidden	dd	?		;count of hidden sectors
csecTotalDWORD	dd	?		;count of total sectors
					;  (if csecTotalWORD = 0)
PLAIN_BPB	ends



; MD_BPB - This is the structure that is stored at teh begining of the
;	   MagicDrv Compressed Volume File (CVF).  Early versions of
;	   MagicDrv used different names for the MD_BPB fields;  these
;	   names are documented after this structure.

MD_BPB		struc

dos_bpb 	db  (SIZE PLAIN_BPB) dup (?)	;starts with a normal DOS BPB

secMDFATStart	dw	?	;logical sector of MD FAT start
nLog2cbPerSec	db	?	;Power 2 of sector size
csecMDReserved	dw	?	;sector count reserved for internal tables
secRootDirStart dw	?	;logical sector of root directory start
secHeapStart	dw	?	;logical sector of sector heap start
cluFirstData	dw	?	;first internal 'cluster' which contains data
cpageBitFAT	db	?	;count of pages for bit_fat table
		dw	?	;reserved
nLog2csecPerClu db	?	;Power 2 of csecPerClu (sectors per cluster)
		dw	?	;reserved

cbCVFMin	dd	?	;minimum CVF size in bytes
		dd	?	;reserved
f12BitFAT	db	?	;NZ if 12 bit FAT, Z if 16 bit FAT
cmbCVFMax	dw	?	;maximum CVF size, in megabytes

MD_BPB		ends


; Following is a mapping of the new MD_BPB field names to the old
; "our_bpb" names.
;
;  secMDFATStart	sqztor_cluster_o
;  nLog2cbPerSec	sector_pow2
;  csecMDReserved	log_internal
;  secRootDirStart	first_root
;  secHeapStart 	first_data
;  cluFirstData 	first_cluster
;  cpageBitFAT		b_fat_parts
;  nLog2csecPerClu	sectors_p_unit_pow2
;			pass_off
;  cbCVFMin		min_file
;			max_file
;  f12BitFAT		xfat_12bit
;  cmbCVFMax		max_cap
