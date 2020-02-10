;*** 
;pcode.inc
;
;	Copyright <C> 1986, 1987 Microsoft Corporation
;
;Purpose:
; This include file contains constants which refer to operands in pcode
; See rtps.inc for constants which are known be the runtime as well.
;
;
;*******************************************************************************


OPCODE_MASK	=	03FFH	;Opcode is in lowest 10 bits of the word

;-----------------------------------------------------------------
;offsets into opStDeclare, opStSub, opStFunction pcodes:
; (for opStDefFn, add 2 to all but DCL_cntEos)
;
;****************************************************************************
;****************************************************************************
;** [x] WARNING: The format of the pcode for opStDeclare, et al., is       **
;**              exported to Omega so they can type check parameters.      **
;**              Any changes made here MUST be reflected in \eb\hd\ebapi.h **
;****************************************************************************
;****************************************************************************
;
DCL_cntEos	EQU 0		;# bytes from DCL_oPrs field to next opcode
DCL_oPrs	EQU 2		;identifies prs being declared/defined
DCL_atr		EQU 4		;misc attributes for the prs (see DCLA_xxx)
DCL_cParms	EQU 6		;number of formal parameters
				; 0 means proc takes no parameters.
				; In opStDeclare, UNDEFINED means user
				; didn't specify a parameter list in
				; the DECLARE, so no parm type checking
				; should be done.

;DCL_cParms field is followed by 3 words per parameter (DCLP_xxx) and
; then by the ASCII name of the alias (if any)

  ;Masks for values in the DCL_atr word
  DCLA_cdecl	EQU 8000h	;TRUE if DECLARE has CDECL keyword
  DCLA_cbAlias	EQU 7C00h	;#bytes of alias-name (0 if no alias)
  DCLA_procType	EQU 0300h	;always PT_SUB, PT_FUNCTION or PT_DEFFN
  DCLA_Explicit	EQU 0080h	;FUNCTION or DEF FN was explicitly typed
				; via a type char or its type was declared
				; by as AS clause.
  DCLA_oTyp	EQU 007Fh	

 ;Offsets into formal parameter blocks:
 DCLP_id	EQU 0		;For DECLARE, this is always the oNam of
				; the formal.  For SUB, FUNC, DEF FN,
				; this is the oNam in SS_RUDE, or the
				; oVar in SS_PARSE or SS_EXECUTE
 DCLP_atr	EQU 2		;Formal parm attributes (PATR_xxx)
 DCLP_oTyp	EQU 4		;Type of the formal parm as follows:
	  ;If scan-state = SS_RUDE AND
	  ;   neither PATR_fAsClause or PATR_fExplicit are set:
	  ;     oTyp field contains garbage.  Parameter's type is implicit
	  ;     and depends depends on preceeding DEF<type> statements.
	  ;Else if scan-state = SS_RUDE AND
	  ;   either PATR_fAsClause or PATR_fExplicit are set:
	  ;     oTyp = 0                  - AS ANY
	  ;     oTyp = 1 to ET_MAX        - AS INTEGER to STRING or % to $
	  ;     oTyp > ET_MAX             - AS <user type>, since its SS_RUDE,
	  ;				      this field is an oNam for
	  ;				      the user defined type
	  ;  Else if scan-state <> SS_RUDE:
	  ;       oTyp = 0                  - AS ANY
	  ;       oTyp = 1 to ET_MAX        - AS INTEGER to STRING or % to $
	  ;       oTyp > ET_MAX             - AS <user type>, field = oTyp for
	  ;				      user defined type
	  ;  NOTE: Fixed length strings are not allowed as DECLARE, SUB,
	  ;	FUNCTION or DEF FN parameter lists.

;Formal parameter attribute masks (found in parameter lists of opStDeclare,
; opStSub, opStFunction, and opStDefFn opcodes)
;Note: In addition to varmgr input flag dependancies on the below,
;	There also exist dependancies on these constants in ssproc.asm.
PATR_oTyp		EQU 000FH	;used to store oTyp for special cases
PATR_explicit		EQU 0200H	;set if explicit type char %&!#$

PATR_SEG		EQU 0800H	;set if BYSEG was found
					; Must = SegFlag in ssproc.asm
PATR_BYVAL		EQU 1000H	;set if BYVAL was found
					; Must = ByValFlag in ssproc.asm
PATR_array		EQU 0400H	;set if param is an array
					; Must = FVFORCEARRAY
PATR_asClause		EQU 2000H	;set if an AS clause is present
					; Must = FVASCLAUSE

;-----------------------------------------------------------------
; opLine operand constants:
;
LINEBF_none		EQU 0
LINEBF_B		EQU 1
LINEBF_BF		EQU 2

CB_EMPTY_TEXT	equ	4+StartOtx ;size of empty text table (opEndProg + opEot)
