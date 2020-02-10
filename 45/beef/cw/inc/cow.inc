;*
;*	COW : Character Oriented Windows
;*
;*	cow.inc : general assembly include file

DEBPUB = 1		;* always debugging publics

IFDEF ?LOADER
;**** LOADER ****
memS = 1
?WIN = 0

ELSE
;**** NON-LOADER ****
memM = 1

IFDEF COW_SWAPPED
?WIN = 1
?COW = 1
ELSE
?WIN = 0
ENDIF ;!COW_SWAPPED

IFDEF DOS5
?DOS5 = 1
ENDIF

IFDEF PROFILE	;* don't profile loader
PROFILE1 = 1
ENDIF ;PROFILE
ENDIF ;!?LOADER

?PLM = 1
?TF = 1
?DF = 1
?NODATA = 1
	include	cmacros.inc	;* must be version 2.09 or higher

;*	* Create DGROUP
createSeg _DATA,DATA,WORD,PUBLIC,DATA,DGROUP
createSeg _BSS,BSS,WORD,PUBLIC,BSS,DGROUP
defGrp DGROUP,DATA,BSS

	include	version.inc

wo	equ	word ptr
by	equ	byte ptr

;*****************************************************************************
;*	* Model Control *
;*
;*	PUBLIC is always FAR
;*	COW_TINY_SWAPPED		=> private is FAR
;*	!COW_TINY_SWAPPED		=> private is NEAR (all in COW_TEXT)
;*
;*	* PUBLIC => all public (exported) interfaces
;*	* PRIVATE => private normal code (entries not exported)
;*****************************************************************************

;*	* external Absolute values
IFNDEF externA	;* not defined in old CMACROS
externA macro n
?ex1 <n>,0,<ABS>
endm
ENDIF

cPublic	MACRO	n,c,a
	cProc	n,<FAR, PUBLIC, c>,<a>
ENDM

externFPublic MACRO	x
	externFP	<x>
ENDM

IFNDEF COW_TINY_SWAPPED
cPrivate MACRO	n,c,a
	cProc	n,<NEAR, PUBLIC, c>,<a>
ENDM
externFPrivate MACRO	x
	externNP	<x>
ENDM
ELSE
cPrivate MACRO	n,c,a
	cProc	n,<FAR, PUBLIC, c>,<a>
ENDM
externFPrivate MACRO	x
	externFP	<x>
ENDM
ENDIF ;COW_TINY_SWAPPED

; define misc. register pairs
;
regptr  esax,es,ax
regptr  esbx,es,bx
regptr  essi,es,si
regptr  esdi,es,di
regptr  dssi,ds,si
regptr  dsdi,ds,di
regptr  cssi,cs,si
regptr  csdi,cs,di
regptr  dxax,dx,ax
regptr  ssbx,ss,bx
regptr	dsbx,ds,bx

PushArg	MACRO	arg_list		;; Push multiple registers
	irp	x,<arg_list>
		push	x
	endm
ENDM

;============================================================================

;*	* Assembly time Assert *

Assert	MACRO	expr		;;* Assembly time assertion check
    IFE expr
	%OUT Assertion Failed !expr!
	.ERR
    ENDIF
ENDM

;*	* Run time Asserts *

BREAKPOINT MACRO		;;* insert breakpoint if debugging
    IFDEF DEBUG
	INT	3
    ENDIF
ENDM

ifdef	Debug

IFNDEF ?LOADER
IFNDEF ?LDDEBUG
externFP <CowAssertFailed>
ENDIF
ELSE
IFNDEF ?LOADER_ASM
externNP <CowAssertFailed>		;* loader assert trap
ENDIF
ENDIF

AssertEq MACRO	p1,p2		;;* Run time assert 2 values equal
	LOCAL	ok
	cmp	p1,p2
	je	ok
	cCall	CowAssertFailed
ok:
ENDM
AssertNe MACRO	p1,p2		;;* Run time assert 2 values not equal
	LOCAL	ok
	cmp	p1,p2
	jne	ok
	cCall	CowAssertFailed
ok:
ENDM
AssertSet MACRO	p1,m		;;* Run time assert value masked is non-zero
	LOCAL	ok
	test	p1,m
	jnz	ok
	cCall	CowAssertFailed
ok:
ENDM
AssertReset MACRO p1,m		;;* Run time assert value masked is zero
	LOCAL	ok
	test	p1,m
	jz	ok
	cCall	CowAssertFailed
ok:
ENDM
AssertUp MACRO			;;* Run time assert that D flag is clear
	LOCAL	ok
	push	ax
	pushf			;;* push flags
	pop	ax		;;* ax = flags
	test	ax,400h		;;* D flag
	pop	ax		;;* restore AX
	jz	ok
	cCall	CowAssertFailed	;;* D flag not reset !!!
ok:
ENDM
AssertCmp MACRO	a,cond,b	;* assert comparison condition
	LOCAL	ok
	CMP	a,b
	J&cond	ok
	BREAKPOINT
ok:
ENDM
AssertData MACRO sreg		;* assert a segment register is set to DDS (SS)
	LOCAL	ok
	PUSH	AX
	PUSH	BX
	MOV	AX,sreg
	MOV	BX,SS
	CMP	AX,BX
	POP	BX
	POP	AX
	JZ	ok
	BREAKPOINT
ok:
	assumes	sreg,DGROUP
ENDM
AssertZR MACRO			;* assert the zero flag is ZR.
	LOCAL	ok
	JZ	ok
	BREAKPOINT
ok:
ENDM
AssertNZ MACRO			;* assert the zero flag is NZ.
	LOCAL	ok
	JNZ	ok
	BREAKPOINT
ok:
ENDM
AssertCY MACRO			;* assert the carry flag is CY.
	LOCAL	ok
	JC	ok
	BREAKPOINT
ok:
ENDM
AssertNC MACRO			;* assert the carry flag is NC.
	LOCAL	ok
	JNC	ok
	BREAKPOINT
ok:
ENDM

else	; !Debug

AssertEq MACRO	p1,p2
ENDM
AssertNe MACRO	p1,p2
ENDM
AssertSet MACRO	p1,p2
ENDM
AssertReset MACRO p1,p2
ENDM
AssertUp MACRO
ENDM
AssertCmp MACRO	a,cond,b
ENDM
AssertData MACRO sreg
	assumes	sreg,DGROUP
ENDM
AssertZR MACRO
ENDM
AssertNZ MACRO
ENDM
AssertCY MACRO
ENDM
AssertNC MACRO
ENDM

endif	; !Debug

;============================================================================

;***** PROFILE SUPPORT *****
;*	* note do not use the ?PROFILE flag for CMACROS.INC since it will
;*	*   make all far procedures profiled

IFDEF PROFILE1
	externFP <StartCowMeas, StopCowMeas>

StartPublic MACRO
	cCall	StartCowMeas
ENDM
StopPublic MACRO
	cCall	StopCowMeas
ENDM
ELSE
StartPublic MACRO
ENDM
StopPublic MACRO
ENDM
ENDIF ;!PROFILE

;*	* Misc Macros

;********* ExitTrueUnless *********
;*	entry : label = label to jump to to return FALSE
;*	* TRICK : use the MOV AX,.... instruction to hide an XOR AX,AX
;*	exit : if entered from top of macro : AX != 0,
;*		if entered from label : AX == 0.

ExitTrueUnless	MACRO	label
	DB	0B8H			;; MOV AX,....
label:
	XOR	AX,AX
ENDM

;*****************************************************************************
;*	* DUAL mode specifics


IFDEF DUAL
cProcNorF	MACRO	label,save
	cProc	label	<FAR,PUBLIC,ATOMIC>,<save>
ENDM
ELSE ;!DUAL
cProcNorF	MACRO	label,save
	cProc	label	<NEAR,PUBLIC,ATOMIC>,<save>
ENDM
ENDIF ;!DUAL


IFDEF DUAL
externNorFP	MACRO	seg,label
	externFP	label
ENDM
ELSE ;!DUAL
externNorFP	MACRO	seg,label
sBegin	seg
	externNP	label
sEnd	seg
ENDM
ENDIF ;!DUAL


;*****************************************************************************
;*	* Structures:
;*	* NOTE: not generated

_ARC	STRUC		;* Absolute Rectangle
	axLeftArc	DB	?
	ayTopArc	DB	?
	axRightArc	DB	?
	ayBottomArc	DB	?
_ARC	ENDS

STF	RECORD	fEnabledStf:1, styleStf:15		;* style & flag

;*	* WND : window structure *
WND	STRUC
	idWnd		DW	?			;* id
	stfWnd		DB	(WIDTH STF) / 8 DUP (?)
	arcWindow	DB	SIZE _ARC DUP (?)
	arcClipping	DB	SIZE _ARC DUP (?)
	pfnWndProc	DD	?
	pwndParent	DW	?
	pwndSibbling	DW	?
	pwndChild	DW	?
	axCursor	DB	?
	ayCursor	DB	?
	rgwExtra	DW	?			;* array of WORDS
WND	ENDS

BOX	STRUC	;* Single and Double BCS
	chTopLeftBox	DB	?
	chTopRightBox	DB	?
	chBottomLeftBox	DB	?
	chBottomRightBox DB	?
	chTopBox	DB	?
	chBottomBox	DB	?
	chLeftBox	DB	?
	chRightBox	DB	?
BOX	ENDS


;*****************************************************************************
