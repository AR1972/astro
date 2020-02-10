;***
;exehdr.inc - 02-Sep-86 - EXE file header structure
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Defines structure for DOS 3 EXE file header.
;
;******************************************************************************

;	Structure for EXE file header area.

ExeHeader	STRUC

EHSignature	DW	?	;always 5A4DH for EXE files
EHcbLastPage	DW	?	;length of last page in bytes
EHcPages	DW	?	;number of pages (including last partial)
EHcRelocItems	DW	?	;number of relocations items
EHcpHeader	DW	?	;paragraph size of header and reloc table
EHcpMinExtra	DW	?	;minimum paragraphs needed after load image
EHcpMaxExtra	DW	?	;maximum paragraphs needed after load image
EHInitialSS	DW	?	;initial SS segment displacement
EHInitialSP	DW	?	;initial SP offset
EHChecksum	DW	?	;file checksum - negative sum of all words
EHInitialIP	DW	?	;initial IP offset
EHInitialCS	DW	?	;initial SS segment displacement
EHbpRelocStart	DW	?	;byte pointer to relocation items
EHcOverlay	DW	?	;overlay number (0 for resident)

ExeHeader	ENDS

ExeSignature	EQU	5A4DH		;legal signature for EXE files


;
; User Library header block description.
;
;  The first three pointers refer to a series of items containing information
;  concerning the symbols.  This is the TYPE code, symbol name string pointer,
;  and Long Pointer to the CODE/DATA/SEGMENT area.
;

UserLib_Hdr struc

Far_Jmp 	db 5 dup (?)	;[2]Skip the 5 byte far jump to __aulstart
QL_version	db	?	;[2]QLink version number
Magic_Word	dw	?	;[2]Magic word 'ul' for consistency checks.
Code_List	dw	?	;Near Offset to start of CODE symbols.
Data_List	dw	?	;Near Offset to start of DGROUP symbols.
Segment_List	dw	?	;Near Offset to start of SEGMENT symbols.
Code_Start	dw	?	;Segment Pointer to start of CODE in file.
Data_Start	dw	?	;Segment Pointer to start of DGROUP in file.
Sym_Start	dw	?	;Segment Pointer to start of SYMBOLS in file.
Sym_Size	dw	?	;Constant size of Symbol segment.
FData_Start	dw	?	;[2]Segment Pointer to start of FAR_DATA segments.
FData_Size	dd	?	;[2]Constant size of FAR_DATA segments.
FBss_Start	dw	?	;[2]Segment Pointer to start of FAR_BSS segments.
FBss_Size	dd	?	;[2]Constant size of FAR_BSS segments.

UserLib_Hdr ends

ULSignature	EQU	'lu'	;[2]legal magic word for user libraries
FARJMP		EQU	0EAH	;[2]opcode for Far jump at start of header

;
;  This is the description of the User Library symbol definitions.
;

UserLib_Sym struc

User_Type	dw	?	;Special code.
User_pzName	dw	?	;Offset to the zero terminated name string.
User_pName	dd	?	;Long Pointer to actual location.

UserLib_Sym ends

;
;  The following define the ordinals for the User_Type field
;

User_Type_Null		=	0	;terminates a list
User_Type_Code		=	1	;code item
User_Type_Data		=	2	;data item
User_Type_Segment	=	3	;segment item
User_Type_Common	=	4	;common segment
User_Type_Abs		=	5	;ABSolute symbol may be in with
					;DATA or SEGMENT symbols
