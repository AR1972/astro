	Page ,132
TITLE BOOT	SECTOR 1 OF TRACK 0 - BOOT LOADER
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;   Rev 1.0 ChrisP, AaronR and others.	2.0 format boot
;
;   Rev 3.0 MarkZ   PC/AT enhancements
;		    2.50 in label
;   Rev 3.1 MarkZ   3.1 in label due to vagaries of SYSing to IBM drive D's
;		    This resulted in the BPB being off by 1.  So we now trust
;		    2.0 and 3.1 boot sectors and disbelieve 3.0.
;
;   Rev 3.2 LeeAc   Modify layout of extended BPB for >32M support
;		    Move PHYDRV to 3rd byte from end of sector
;		    so that it won't have to be moved again
;		    FORMAT and SYS count on PHYDRV being in a known location
;
;   Rev. 3.3 D.C. L. Changed Sec 9 EOT field from 15 to 18. May 29, 1986.
;
;   Rev 3.31 MarkT  The COUNT value has a bogus check (JBE????) to determine
;		    if we've loaded in all the sectors of IBMBIO. This will
;		    cause too big of a load if the sectors per track is high
;		    enough, causing either a stack overflow or the boot code
;		    to be overwritten.
;
;   Rev 4.00 J. K.  For DOS 4.00 Modified to handle the extended BPB, and
;		    32 bit sector number calculation to enable the primary
;		    partition be started beyond 32 MB boundary.
;
;
; The ROM in the IBM PC starts the boot process by performing a hardware
; initialization and a verification of all external devices.  If all goes
; well, it will then load from the boot drive the sector from track 0, head 0,
; sector 1.  This sector is placed at physical address 07C00h.	The initial
; registers are set up as follows:  CS=DS=ES=SS=0.  IP=7C00h, SP=0400H.
;
; The code in this sector is responsible for locating the MSDOS device drivers
; (IBMBIO) and for placing the directory sector with this information at
; physical address 00500h.  After loading in this sector, it reads in the
; entirety of the BIOS at BIOSEG:0 and does a long jump to that point.
;
; If no BIOS/DOS pair is found an error message is displayed and the user is
; prompted to reinsert another disk.  If there is a disk error during the
; process, a message is displayed and things are halted.
;
; At the beginning of the boot sector, there is a table which describes the
; MSDOS structure of the media.  This is equivalent to the BPB with some
; additional information describing the physical layout of the driver (heads,
; tracks, sectors)
;
;==============================================================================
;REVISION HISTORY:
;AN000 - New for DOS Version 4.00 - J.K.
;AC000 - Changed for DOS Version 4.00 - J.K.
;AN00x - PTM number for DOS Version 4.00 - J.K.
;==============================================================================
;AN001; d52 Make the fixed positioned variable "CURHD" to be local.  7/6/87 J.K.
;AN002; d48 Change head settle at boot time.			     7/7/87 J.K.
;AN003; P1820 New message SKL file				   10/20/87 J.K.
;AN004; D304 New structrue of Boot record for OS2.		   11/09/87 J.K.
;AN005; Changed version to 5.0					   03/08/90 E.A.
;AN006; Changed to remove MSLOAD in first cluster restriction	   04/23/90 J.H.
;==============================================================================


ORIGIN		EQU	7C00H		; Origin of bootstrap LOADER
BIO_SEG		EQU	70H		; Destingation segment of BIOS
BIO_OFFSET	EQU	700H		; Offset of bios
SECTOR_SIZE	EQU	512		; Sector size in bytes
DIR_ENTRY_SIZE	EQU	32		; Size of directory entry in bytes
DIR_OFF		EQU	500h
IBM_LOAD_SIZE	EQU	3		;J.K. Size of IBMLOAD module in sectors
ROM_DISKRD	EQU	2
DSK_PARMS	EQU	1EH*4		;POINTER TO DRIVE PARAMETERS



; ==========================================================================

;include version.inc
;include	dirent.inc

; ==========================================================================

SEGBIOS SEGMENT AT BIO_SEG

	; Define the destination segment of the BIOS, including the
	; initialization label

BIOS	LABEL	BYTE

SEGBIOS ENDS

; ==========================================================================

CODE	SEGMENT
	ASSUME CS:CODE,DS:NOTHING,ES:NOTHING,SS:NOTHING
	ORG	ORIGIN


	Public $START
$START:
			; WARNING -- Don't change this to a short jmp
	jmp	Main			; Jump to start of code


; ==========================================================================

					; Start of BPB area of the boot record

		DB	"MSDOS"

;	BUGBUG -- 7 Dec 1992 -- chuckst -- changed version back to 5.0 to
;					avoid bug in PC-TOOLS DISKFIX
OsVersion	DB	"5.0"		; DOS version number
BytesPerSector	DW	SECTOR_SIZE	; Size of a physical sector
SecsPerClust	DB	8		; Sectors per allocation unit
ReservedSecs	DW	1		; Number of reserved sectors
NumFats		DB	2		; Number of fats
NumDirEntries	DW	512		; Number of direc entries
TotalSectors	DW	4*17*305-1	; Number of sectors - number of hidden
					; sectors (0 when 32 bit sector number)
MediaByte	DB	0F8H		; MediaByte byte
NumFatSecs	DW	8		; Number of fat sectors
SecPerTrack	DW	17		; Sectors per track
NumHeads	DW	4		; Number of drive heads

HiddenSecs	DD	1		; Number of hidden sectors
BigTotalSecs	DD	0		; 32 bit version of number of sectors
BootDrv		DB	80h
CurrentHead	DB	0h		; Current Head
ExtBootSig	DB	41
SerialNum	DD	0
VolumeLabel	DB	'NO NAME    '
FatId		DB	'FAT12   '

		; Danger!!! If not 32 bit sector number calculation,
		; FORMAT should set the value of HiddenSecsHigh and
		; BigTotalSectors to 0 !!!

	PUBLIC uData
uData	LABEL	BYTE
						;Equates to allow access to
						;storage where Main is now
Sec9		EQU	BYTE PTR uData+0	;11 byte diskette parm. table
BiosLow 	EQU	WORD PTR uData+11
BiosHigh	EQU	WORD PTR uData+13
CurTrk		EQU	WORD PTR uData+15
CurSec		EQU	BYTE PTR uData+17
DirLow		EQU	WORD PTR uData+18
DirHigh		EQU	WORD PTR uData+20

; =========================================================================

		; First thing is to reset the stack to a better and more known
		; place. The ROM  may change, but we'd like to get the stack
		; in the correct place.
MAIN:

ret
; =========================================================================

SYSMSG DB    13,10,"Non-System disk or disk error",13,10,"Replace and press any key when ready",13,10,0

; =========================================================================

Bio	DB	"IO      SYS"
Dos	DB	"MSDOS   SYS"

; =========================================================================

Free	EQU (SECTOR_SIZE - 4) - ($-$start)

if Free LT 0
    %out FATAL PROBLEM:boot sector is too large
	.ERR
endif

	org	origin + (SECTOR_SIZE - 2)
	db	55h,0aah			; Boot sector signature

; =========================================================================

CODE	ENDS
	END 

