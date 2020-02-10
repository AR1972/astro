;*
;*	loinfo.inc : low memory info (for loader & kernel)
;*
;*	SAR : 86-6-11

;*	* Note : this information is initialized by the loader
;*		and is passed to the kernel


	include newexe.inc		;* new exe header
	include pdb.inc 		;* DOS 2.x PDB

cchBootPathMax	EQU	80		;* (really 2 + 64 + 8 + 1 + 3 + 1)

LOM	STRUC
;* PDB
 pdbLom 	DB	SIZE PDB DUP (?)	;* (Prog descriptor block)
;* boot file path : properly formed : chDrive:....
 szBootPathLom	DB	cchBootPathMax DUP (?)
;* Miscellaneous variables
 fdExeLom	DW	?	;* file handle of Open .EXE file
 ;* Warning : cbNeLom & lfaNeLom MUST be contiguous
 cbNeLom	DW	?	;* # of bytes in new exe file
 lfaNeLom	DD	?	;* long file offset to New portion
 hGlobalHeapLom DW	?	;* handle to global heap
 pGlobalHeapLom DW	?	;* ps of start of global heap
 psDgroupLom	DW	?	;* ps of DGROUP
 psUseMax	DW	?	;* maximum ps that can be used
 offRlbLom	DW	?	;* offset in psLom of RLB information
				;* (far kernel work buffer)
;* Near jumps to far procedures
 opcReloadLom	DB	?	;* JMPF
 offReloadLom	DW	?
 psReloadLom	DW	?
 opcRelruLom	DB	?	;* JMPF
 offRelruLom	DW	?
 psRelruLom	DW	?
 opcRetThunkLom	DB	?	;* JMPF
 offRetThunkLom	DW	?
 psRetThunkLom	DW	?
 fillerLom	DB	?	;* WORD align

;* Exe name for DOS 3 rename protection
 szExeLom	DB	14 dup (?)		;* name only (12+\0+align)

IFDEF DUAL
 pbRleCurLom	DW	?	;* RLB_ENT currently in kernel buffer
ENDIF ;DUAL

;*	* New EXE
 neLom		DB	SIZE NEW_EXE DUP (?)	;* new exe header
;*	* extra information follows here ....
LOM	ENDS

cbLom		EQU	SIZE LOM		;* # of bytes in LOM
cpsLom		EQU	(SIZE LOM + 15) / 16	;* # of paras in LOM

lomOffset	EQU	<>			;* offset of struct

;*	* aliases for PDB
cchCmdLine	EQU	pdbLom.PDB_DEF_DTA
szCmdLine	EQU	pdbLom.PDB_DEF_DTA+1

;* * format of memory :
;*	LOM
;*	swap file names
;*	seg tab
;*	ent tab
;*	FIXED CODE SEGMENTS
;*	* DGROUP
;*	* [preloaded moveable code segments]
;*	* .. free global heap
;*	[LOADER]
;*	* end of heap

