;*
;*	rlb.inc : Compact relocation format
;*

;*	* Magic values for RLB signature
magicWin2	EQU	334EH		;* N3

;*	* Compact Relocations
RLC 	STRUC
rliRlc		DW	?		;* reloc info (see below)
offSrcRlc	DW	?		;* start thread address
RLC 	ENDS

;*	* if fImportRli == 0
RLI	RECORD	fImportRli:1, fAddRli:1, rtRli:2, ithunkRli:12
;*	* LOBYTE(rli) == segTarg for rtFixed
;*	* if fImportRli == 1
RLI2	RECORD	fImportRli2:1, fNameRli:1, rtRli2:2, imodRli:3, entnoRli:9
;*	* imodRli is 0 based

rtFixed		EQU	0		;* Base to fixed segment
					;*   (may be additive)
rtOffset	EQU	1		;* offset of thunk
rtBase		EQU	2		;* base of thunk
rtPtr		EQU	3		;* ptr to thunk


;*	* Relocation Block Info
RLT	RECORD	f2Rlt:1, f1Rlt:1, prelocRlt:14

RLS 	STRUC
rltRls		DW	?		;* see RLT record above
csumRls		DW	?		;* check sum
RLS 	ENDS

RLB	STRUC
magicRlb	DW	?		;* magic value (magicNe)
segSpecialRlb	DW	?		;* first segment
segFirstRlb	DW	?		;* first segment
rgrlsRlb	DB	?		;* Array of RLSs
				;* zero'th element is segSpecial
				;* 1'st element is segFirst
				;* (all remaining sequential from segFirst).
RLB	ENDS


IFDEF DUAL
;*******************************************************************************
;*
;*	DUAL mode extras
;*

;*	NOTE:	This must match the RLE struct in \ut\munge\rlb.h!!
RLB_ENT	STRUC
re_segLast	DB	?	;* last segment handled by this rlb
re_iexe		DB	?	;* swap file index
re_cbRlb	DW	?	;* Rlb size
re_lfaRlb	DD	?	;* lfa relative to beginning of main swap file
re_dlfaBias	DD	?	;* dlfa for this exe
RLB_ENT	ENDS

LO_re_lfaRlb	EQU	[word ptr re_lfaRlb]
HI_re_lfaRlb	EQU	[word ptr re_lfaRlb + 2]

LO_re_dlfaBias	EQU	[word ptr re_dlfaBias]
HI_re_dlfaBias	EQU	[word ptr re_dlfaBias + 2]

;*
;*******************************************************************************
ENDIF ;DUAL
