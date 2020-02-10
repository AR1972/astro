	page	,132
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

	.xlist
include	devsym.inc
	.list

PUBLIC	hit_l
PUBLIC	hit_h
PUBLIC	nohit_l
PUBLIC	nohit_h
PUBLIC	dirty_l
PUBLIC	dirty_h
PUBLIC	in_bambi
PUBLIC	in_win
PUBLIC	real_dd_headers
PUBLIC	real_cache_units
PUBLIC	driver_hooks
PUBLIC	secsize_and_align_info
PUBLIC	cache_block_bytes
PUBLIC	cache_block_words

PUBLIC	selected_drive
PUBLIC	max_valid_buffers
PUBLIC	last_buffer

PUBLIC	number_of_cache_elements
PUBLIC	number_of_cache_elements_win


PUBLIC	queueheadptr
PUBLIC	queuelength
PUBLIC	elementidentifierlowordoffset
PUBLIC	elementidentifierhiwordoffset
PUBLIC	word_masks
	ifdef	USE_VALID
PUBLIC	validindexoffset
	endif
PUBLIC	dirtyindexoffset
PUBLIC	Number_Of_Dirty_Elements
PUBLIC	dos_3x


PUBLIC	farptrsav
PUBLIC	save_ax
PUBLIC	in_08
PUBLIC	in_10
PUBLIC	in_09
PUBLIC	in_13
PUBLIC  in_16
PUBLIC	int08chain
PUBLIC	int10chain
PUBLIC	int09chain
PUBLIC	int13chain
PUBLIC	int28chain
PUBLIC	int16chain
PUBLIC	int2fchain
PUBLIC	int19chain
PUBLIC	int15chain
PUBLIC  int21chain
PUBLIC  int25chain
PUBLIC  int26chain

PUBLIC	processor_type

PUBLIC	indosaddr

PUBLIC	XMShandlevalid
PUBLIC	XMShandle
PUBLIC  vxd_name

PUBLIC  commitflag

PUBLIC	block_buffer
	

;	We need to keep this module dword aligned so that we can
;	  align our big buffer to a DWORD.

zseg    segment dword public 'CODE'

	assume  cs:zseg
	assume	ds:nothing

dos_3x			dw	0
hit_l			dw	0
hit_h			dw	0
nohit_l			dw	0
nohit_h			dw	0
dirty_l 		dw	0
dirty_h 		dw	0

in_bambi		db	0
word_masks		db	0		; flag indicates word masks in use

selected_drive		db	-1

in_win			db	0		; not in windows when we load

	align	4

real_dd_headers		dd	26 	dup(?)
real_cache_units 	db	26	dup(-1)

driver_hooks		db size sysdev * 26 dup ('$')

secsize_and_align_info	dw	26 dup (0ff02h)	; align unknown, 512 byte secs

;	The cache block size is a primary global value which can only
;	  be changed by completely committing and reinitializing the
;	  cache data structures.

cache_block_bytes	dw	8192

;	The following global value should always be one-half cache_block_bytes

cache_block_words	dw	4096

last_buffer		dw	0	; offset of dirty_write buffer
max_valid_buffers	dw	0	; number of buffers allocated

;from queueman
queueheadptr	dd	?
queuelength	dw	0		;must be zero at startup so
					;caching will not occur until
					;data is initialized
ElementIdentifierLowordOffset   dw      ?
ElementIdentifierHiwordOffset   dw      ?
DirtyIndexOffset                dw      ?
	ifdef	USE_VALID
validindexoffset		dw	?
	endif
Number_Of_Dirty_Elements	dw	?


farptrsav	dd	?
save_ax		dw	?

in_08		db	0
in_10		db	0
in_09		db	0
in_13		db	0
in_16		db	0	

	align	4
int08chain	dd	?
int10chain	dd	?
int09chain	dd	?
int13chain	dd	?
int28chain	dd	?
int16chain	dd	?
int2fchain	dd	?
int19chain	dd	?
int15chain	dd	?
int21chain	dd	?
int25chain	dd	?
int26chain	dd	?

processor_type	db	?

indosaddr	dd	?


;from cacheman.asm
XMShandlevalid	db	0
	align	2
XMShandle	dw	?

startup_name_seg dw 	?
startup_name_off dw	?

PUBLIC startup_name_seg	     ;used for vxd load
PUBLIC startup_name_off	     ;used for vxd load

;bug bug these were transient, need to get new number ?when?
number_of_cache_elements 	dw	100h	;# elements for DOS
number_of_cache_elements_win	dw	100h	;# elements for WIN
commitflag	db	1

vxd_name	db	69	dup('@')
align	4

block_buffer:	

if 1
;;;BUG BUG this is now used in logphys to ensure our buffer is large
;;;enough for the sector size
public logbuf
logbuf	db	16192	dup(?)	;BUG BUG this is here to keep umb case from
				;trashing itself during init_cache etc
				;BUG BUG this also ensures that the cache
				;is larger than the safedsk driver so
				;it will not tromp the init code in devini.asm
				;when devicestarts up
endif

zseg ends

end
