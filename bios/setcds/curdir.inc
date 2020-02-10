	BREAK <Current directory list structure>

;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**	CDS - Current Directory Structure
;
; CDS items are used bu the internal routines to store cluster numbers and
; network identifiers for each logical name.  The ID field is used dually,
; both as net ID and for a cluster number for local devices.  In the case
; of local devices, the cluster number will be -1 if there is a potential
; of the disk being changed or if the path must be recracked.
;
;	Some pathnames have special preambles, such as
;
;		\\machane\sharename\...
;	For these pathnames we can't allow ".." processing to back us
;	up into the special front part of the name.  The CURDIR_END field
;	holds the address of the seperator character which marks
;	the split between the special preamble and the regular
;	path list; ".." processing isn't allowed to back us up past
;	(i.e., before) CURDIR_END
;	For the root, it points at the leading /.  For net
;	assignments it points at the end (nul) of the initial assignment:
;	A:/	\\foo\bar	    \\foo\bar\blech\bozo
;	  ^		 ^		     ^


DIRSTRLEN	EQU	64+3		; Max length in bytes of directory strings
TEMPLEN 	EQU	DIRSTRLEN*2

CURDIR_LIST	STRUC
CURDIR_TEXT	DB	DIRSTRLEN DUP (?)   ; text of assignment and curdir
CURDIR_FLAGS	DW	?		; various flags
CURDIR_DEVPTR	DD	?		; local pointer to DPB or net device
CURDIR_ID	DW	2 dup (?)	; cluster of current dir (net ID)
CURDIR_USER_WORD DW	?
CURDIR_END	DW	?		; index to ".." backup limit - see above
CURDIR_TYPE	DB	?		; IFS drive (2=ifs, 4=netuse)
CURDIR_IFS_HDR	DD	?		; Ptr to File System Header
CURDIR_FSDA	DB	2 DUP (?)	; File System Dependent Data Area
CURDIR_LIST	ENDS

curdirLen	EQU	Size CURDIR_list	; Needed for screwed up
						; ASM87 which doesn't allow
						; Size directive as a macro
						; argument
CURDIR_netID	EQU	DWORD PTR CURDIR_ID


;**	Flag values for CURDIR_FLAGS

CURDIR_isnet	EQU	1000000000000000B
CURDIR_isifs	EQU	1000000000000000B
CURDIR_inuse	EQU	0100000000000000B
CURDIR_splice	EQU	0010000000000000B
CURDIR_local	EQU	0001000000000000B
