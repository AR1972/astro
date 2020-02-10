;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;
; MSDOS 2.xx Process Data Block
;
;   Contains all DOS specific data.
;

FilPerProc      EQU     20

; Process Data located in the Program Segment Prefix, just before the
; code and data for the loaded program.
;
; This IS the whole PSP
;
PDB             STRUC
PDB_Exit_Call   DW  ?
PDB_block_len   DW  ?
                DB  ?
PDB_CPM_Call    DB  5 DUP (?)
PDB_Exit        DD  ?
PDB_Ctrl_C      DD  ?
PDB_Fatal_Abort DD  ?
PDB_Parent_PID  DW  ?
PDB_JFN_Table   DB  FilPerProc DUP (?)
PDB_environ     DW  ?
PDB_User_stack  DD  ?
PDB_JFN_Length  DW  ?               ; DOS 3.x only
PDB_JFN_Pointer DD  ?               ; DOS 3.x only
PDB_Next_PDB    DD  ?               ; DOS 3.x only
PDB_PAD1        DB  06h DUP (?)
PDB_Chain       DW  ?               ; Windows only
PDB_Partition   DW  ?               ; Windows only
PDB_NextPDB     DW  ?               ; Windows only
PDB_GlobalHeap  DD  ?               ; Windows only
PDB_Entry_stack DD  ?               ; Windows only
PDB_Call_system DB  5h DUP (?)
PDB_PAD2        DB  7h DUP (?)
PDB_5C_FCB      DB  10h DUP (?)
PDB_6C_FCB      DB  14h DUP (?)
PDB_DEF_DTA     DB  80h DUP (?)
PDB             ENDS


; system file table
;
SFT             STRUC
sftLink         DD      ?
sftCount        DW      ?               ; number of entries
sftFile 	DB	?		; start of entries
SFT             ENDS

; Following SFT Entry structure taken from DOS 4 sources, but used offsets
; are compatible with DOS 3.1+

SFT_ENTRY	STRUC
sf_ref_count	DW	?		; number of processes sharing entry
					;   if FCB then ref count
sf_mode 	DW	?		; mode of access or high bit on if FCB
sf_attr 	DB	?		; attribute of file
sf_flags	DW	?		;Bits 8-15
					; Bit 15 = 1 if remote file
					;	 = 0 if local file or device
					; Bit 14 = 1 if date/time is not to be
					;   set from clock at CLOSE.  Set by
					;   FILETIMES and FCB_CLOSE.  Reset by
					;   other reseters of the dirty bit
					;   (WRITE)
					; Bit 13 = Pipe bit (reserved)
					;
					; Bits 0-7 (old FCB_devid bits)
					; If remote file or local file, bit
					; 6=0 if dirty Device ID number, bits
					; 0-5 if local file.
					; bit 7=0 for local file, bit 7
					;      =1 for local I/O device
					; If local I/O device, bit 6=0 if EOF (input)
					;		Bit 5=1 if Raw mode
					;		Bit 0=1 if console input device
					;		Bit 1=1 if console output device
					;		Bit 2=1 if null device
					;		Bit 3=1 if clock device
sf_devptr	DD	?		; Points to DPB if local file, points
					; to device header if local device,
					; points to net device header if
					; remote
sf_firclus	DW	?		; First cluster of file (bit 15 = 0)
sf_time 	DW	?		; Time associated with file
sf_date 	DW	?		; Date associated with file
sf_size 	DD	?		; Size associated with file
sf_position	DD	?		; Read/Write pointer or LRU count for FCBs
;
; Starting here, the next 7 bytes may be used by the file system to store an
; ID
;
sf_cluspos	DW	?		; Position of last cluster accessed
sf_dirsec	DD	?		; Sector number of directory sector for
					; for this file
sf_dirpos	DB	?		; Offset of this entry in the above
;
; End of 7 bytes of file-system specific info.
;
sf_name 	DB	11 DUP (?)	; 11 character name that is in the
					; directory entry.  This is used by
					; close to detect file deleted and
					; disk changed errors.

; SHARING INFO
sf_chain	DD	?		; link to next SF
sf_UID		DW	?
sf_PID		DW	?
sf_MFT		DW	?
sf_lstclus	DW	?		;AN009; Last cluster accessed
sf_IFS_HDR	DD	?
SFT_ENTRY	ENDS

DPB     STRUC
dpb_drive       DB      ?       ; Logical drive # assoc with DPB (A=0,B=1,...)
DPB     ENDS

devid_device	EQU	0080H	; true if a device
sf_isnet	EQU	8000H	; true if network drive
