; File: CONINT.INC
; NOTE: When making changes to this file, be sure to make equivalent
;	changes to file CONINT.H
; NOTE: For comments, see file CONINT.H

CONINT_H = -1			;remember that this file has been included

	IncludeOnce	context	;require definitions from context.inc


;Structure initialization constants - - - for each of PRS, MRS, and GRS we
;	fill the entire struct with UNDEFINED, and then fill the first n bytes
;	with zeros
PRS_CB_ZERO_INIT	EQU	PRS_oVarHash
MRS_CB_ZERO_INIT	EQU	MRS_oMrsNext
GRS_CB_ZERO_INIT	EQU	GRS_oMrsMain


;**==================== Binary SAVE/LOAD Constants ========================

BLOCK_PROC		EQU	00h
BINSAV_1stBYTE		EQU	0FCh	;first byte of binary saved file


;**==================== Bit flags Constants ========================
	;bit flags used with conFlags in context.asm
	F_CON_StaticStructs	EQU	01H	;Set (TRUE) when mrsCur, prsCur
						;  and txdCur are to be used to
						;  access current context. If
						;  reset (FALSE), then these
						;  must be accessed out of the
						;  appropriate tables.
	F_CON_LeaveTMrsEmpty	EQU	02H	;normally, if MrsDiscard removes
						;  the last mrs from tMrs, it
						;  creates a new unnamed one; if
						;  this flag is TRUE, it just
						;  leaves the table empty
						;  an unreferenced empty prs
	F_CON_ResetStkSize	EQU	04H	;Set by NewStmt when not 
						;  CHAINing. Causes next BOS/BOL
						;  to reset the stack to its
						;  default size.
	F_CON_RunFile		EQU	08H	;Set by exStRunFile so that
						;  NewStmt will not show the
						;  debug screen
	F_CON_DontReplaceRs	EQU	10H	;Set when we don't want
						;  RsTableSearch to replace
						;  the found Prs/Mrs name with
						;  the given name (which perhaps
						;  has different case letters)

;**======================================================================**
;**==           External Interface to Context Mgr Component            ==**
;**======================================================================**

sBegin	DATA
ife	CONTEXT_ASM	;don't include in context.asm
	externB	conFlags
endif	;ife CONTEXT_ASM
ife	CONMISC_ASM	;don't include in conmisc.asm
	externB	fChaining
endif
sEnd	DATA

sBegin	CP

ife	CONMISC_ASM	;don't include these in conmisc.asm
	EXTRN ClearTheWorld:NEAR
endif
sEnd	CP

