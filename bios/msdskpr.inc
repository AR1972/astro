;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1981-1991
; *                      All Rights Reserved.
; */
; the following structure defines the disk parameter table
; pointed to by interrupt vector 1eh (location 0:78h)

disk_parms	struc
disk_specify_1	db	?
disk_specify_2	db	?
disk_motor_wait db	?	; wait till motor off
disk_sector_siz db	?	; bytes/sector (2 = 512)
disk_eot	db	?	; sectors per track (max)
disk_rw_gap	db	?	; read write gap
disk_dtl	db	?
disk_formt_gap	db	?	; format gap length
disk_fill	db	?	; format fill byte
disk_head_sttl	db	?	; head settle time (msec)
disk_motor_strt db	?	; motor start delay
disk_parms	ends

romstatus	equ	1
romread 	equ	2
romwrite	equ	3
romverify	equ	4
romformat	equ	5
