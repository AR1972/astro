;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;


; values for various flags in bds_flags.

fnon_removable	    equ     01h 	;for non-removable media
fchangeline	    equ     02h 	;if changeline supported on drive
return_fake_bpb     equ     04h 	; when set, don't do a build bpb
					; just return the fake one
good_tracklayout    equ     08h 	; the track layout has no funny sectors
fi_am_mult	    equ     10h 	;if more than one logical for this physical
fi_own_physical     equ     20h 	;signify logical owner of this physical
fchanged	    equ     40h 	;indicates media changed
set_dasd_true	    equ     80h 	; set dasd before next format
fchanged_by_format  equ    100h 	;media changed by format
unformatted_media   equ    200h 	;an001; fixed disk only

;
; various form factors to describe media
;
ff48tpi 	    equ     0
ff96tpi 	    equ     1
ffsmall 	    equ     2
ffhardfile	    equ     5
ffother 	    equ     7
ff288		    equ     9		; 2.88 MB drive

bds_struc	struc
bds_link	dd		0ffffh	; link to next bds
bds_drivenum	db		80	; int 13 drive number
bds_drivelet	db		3	; dos drive number

;	We want to embed a BPB declaration here, but we can't initialize
;	it properly if we do, so we duplicate the byte/word/dword architecture
;	of the BPB declaration.

;BDS_BPB	db	size BPBSTRUC dup (?)	; actual BPB
BDS_BPB		dw	512		; BPB_BYTESPERSECTOR
		db	1		; BPB_SECTORSPERCLUSTER
		dw	1		; BPB_RESERVEDSECTORS
		db	2		; BPB_NUMBEROFFATS
		dw	16		; BPB_ROOTENTRIES
		dw	0		; BPB_TOTALSECTORS
		db	0f8h		; BPB_MEDIADESCRIPTOR
		dw	1		; BPB_SECTORSPERFAT
		dw	0		; BPB_SECTORSPERTRACK
		dw	0		; BPB_HEADS
		dd	0		; BPB_HIDDENSECTORS
		dd	0		; BPB_BIGTOTALSECTORS

bds_fatsiz	db		0	; flags...
bds_opcnt	dw		0	; open ref. count
bds_formfactor	db		3	; form factor index
bds_flags	dw		0020h	; various flags
bds_ccyln	dw		40	; max number of cylinders

if 0
BDS_RBPB	db size A_BPB dup (0)	; recommended BPB

endif 
bds_track	db		-1	; last track accessed on drive
bds_tim_lo	dw		1	; time of last access. keep
bds_tim_hi	dw		0	; these contiguous.
bds_volid	db "NO NAME    ",0	; volume id of medium
bds_vol_serial	dd	0	  ;current volume serial number from boot record
bds_filesys_id	db	"FAT12   ",0 ; current file system id from boot record
bds_struc	ends

;	The assembler will generate bad data for "size bds_volid", so we'll
;	  define an equate here.

VOLID_SIZ	equ	12

bdsm_ismini	equ	bds_tim_lo	; overlapping bds_tim_lo
bdsm_hidden_trks equ	bds_tim_hi	; overlapping bds_tim_hi

max_mini_dsk_num = 23	      ; max # of mini disk ibmbio can support

