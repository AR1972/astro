;*** 
;names.inc
;
;	Copyright <C> 1985, 1986, 1987 Microsoft Corporation
;
;
;*******************************************************************************
;
NAMES_INC = ON
;
;The name table is composed of symbol elements linked together.  Each element
;has the following structure:
NM_NEXT 	EQU	0	;offset to next symbol element
NM_FLAGS	EQU	2	;Flag byte
NM_SIZE 	EQU	3	;# of bytes in NM_BUF
NM_NAME 	EQU	4	;ASCII chars that make up the symbol name

;NM_FLAGS consist of the following
NM_fShared	EQU	01	;TRUE if MakeVariable search logic should search
				;     the module level table for this symbol,
				;     even if there's an active procedure.
NM_fLineNum	EQU	02	;TRUE if symbol entry is for a line number
				;     this flag can only be set by the NameMgr
				;     and can never be reset.
NM_fLineNumLabel EQU	04	;TRUE if symbol entry is either a line number
				;     or a label - this bit is only used by
				;     the text mgr.
NM_fAS		EQU	08H	;TRUE if "x AS" clause exists anywhere in
				;     this module
;Unused:	EQU	010H	;bit 4 unused
;Unused:	EQU	020H	;bit 5 unused
;
				;enumerated constants for bits 6 & 7
NMSP_UNDEFINED	EQU	000H	;initial value of this 2-bit field
NMSP_Sub	EQU	040H	;Set by PrsDefine & PrsMake when creating a
				;prs for a SUB. VarMgr deals w/FUNCTIONs & DEFs
NMSP_Variable	EQU	080H	;passed by MakeVariable when creating a non-
				;constant variable table entry
NMSP_MASK	EQU	0C0H	;used to isolate this 2-bit enumerated field

CB_MAX_NAMENTRY	EQU	255d	; max. length of a name in the name table.
;				; NOTE: DON'T confuse this with CB_IDNAM_MAX!

;----------------------------------------------------------------------------
;The following define offsets into the name table
;----------------------------------------------------------------------------
tNam	= 0			;1st word of the name table - this is also the
				;location of the 1st entry of the chain header
				;table (1st char hash).  The main advantage of
				;tNam being 0 is that oNam's can be used as
				;pointers rather than offsets; saving the time
				;that would otherwise be needed to load the
				;base address.	This scheme works because tNam
				;is a far heap entry.
LineNumHdr1st = tNam+64           ;First Line number chain header
LineNumHdrLast = LineNumHdr1st+16 ;Last  Line number chain header
cbUsed = LineNumHdrLast+2         ;# of cbLogical bytes that have been allocated
			          ;ie - are actually being used.
CurONamHdr = cbUsed+2	          ;scratch variable used by GetNextONam to
			          ;remember which 1st char header chain to look
			          ;in when it reaches the end of it's current
			          ;chain
oNamFirst = CurONamHdr+2          ;1st symbol entry


NM_INIT_TABLE_SZ = oNamFirst + NM_NAME + CB_MAX_NAMENTRY
				;space for hash tables + one entry with max.
				;  name length
;============================================================================
;NamMgr debugging stuff
;============================================================================

;----------------------------------------------------------------------------
;macro to dump all the NM_NAME field of a symbol in tNam
;----------------------------------------------------------------------------
DbOoNam macro	oNam
endm	;DbOoNam

;----------------------------------------------------------------------------
;macro to dump all the fields of every symbol in tNam
;----------------------------------------------------------------------------
DbOtNam macro
endm	;DbOtNam

;============================================================================
;the following EXTRN's are included in ALL files EXCEPT namdebug.asm
;============================================================================


ife	NAMDEBUG_ASM


endif	;NAMDEBUG_ASM

;============================================================================
;the following EXTRN's are included in ALL files EXCEPT nammgr.asm
;============================================================================

ife	NAMMGR_ASM
sBegin	CP

	extrn	ONamOfPsd:near
	extrn	ONamOfPbCb:near
	extrn	ONamOfLn:near
	extrn	CopyONamPb:far
	extrn	TNamInit:near
	extrn	FlagOfoNam:near
	extrn	LnOfoNam:far
	extrn	SetONamMask:near
	extrn	SetONamSpace:near
	extrn	CheckONamSpace:near	
	extrn	ResetTNamMask:near
	extrn	ResetONamMask:near
	extrn	GetNextONam:near
	extrn	GetVarNamChar:near
	extrn	OgNamOfPsd:far		
	extrn	OgNamOfPbCb:near	
	extrn	CopyOgNamPb:far		
	extrn	CopyOgNamPbNear:near	
	extrn	FpNamOfOgNam:near	
	extrn	OgNamOfONam:near	
	extrn	ONamOfOgNam:near	
	extrn	ONamOfOgNamFar:far	
	extrn	CmpOgNamIns:near	
	extrn	CharOfOgNam:far		
	extrn	CbOfOgNam:far		
	extrn	BdAppendOgNam:far	
	extrn	CopyONamBd:near

sEnd	CP
endif	;ife NAMMGR_ASM
