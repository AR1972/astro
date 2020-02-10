;***
;ulib.inc - 03-Sep-87
;***
	.XLIST

;
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Defines structures for data items shared between resident
;	runtime and userlibrary runtime.
;
;******************************************************************************

;-----------------------------------------------------------------------------
;	To minimize the number of global data items defined in
;	user library specific runtime and used in the resident
;	core runtime, they are defined in the following structure.
;	The user lib runtime will declare b$ULVars to be
;	of this structure type.  The resident runtime then only
;	needs to know the location of b$ULVars to reference
;	shared data items defined in the user lib.
;-----------------------------------------------------------------------------
IFNDEF	FirstInclude		;used so UlibVars is only defined once
	FirstInclude = TRUE	;if this file is included multiple times
ENDIF				;IFNDEF FirstInclude

IF	FirstInclude
UlibVars STRUC
	RTtoULFarProc	DD ?	;UL specific FAR entry point mapper
	RTtoULNearProc	DD ?	;UL specific NEAR entry point mapper
	FrameSetup	DD ?	;ptr to B$FrameSetup in UL runtime
	ULVersion	DW ?	;[10] Version used to create UL
	ULRevision	DB ?	;[10] Revision used to create UL
UlibVars ENDS
ENDIF				;IF FirstInclude

	PAGE
;-----------------------------------------------------------------------------
;	Entry points in user library runtime that compiled code
;	may call through runtime module interrupt service routine
;-----------------------------------------------------------------------------

IFNDEF	ULEntry 	;IF not defined then just NULL
ULEntry MACRO	name
	ENDM		;;ULEntry Macro
ENDIF			;IFNDEF ULEntry

	ULEntry B$RELA
	ULEntry B$RESA
	ULEntry B$OGSA
	ULEntry B$RETA
	ULEntry B$OGTA
	ULEntry B$GOSA
	ULEntry B$FERL
	ULEntry B$RESN
	ULEntry B$RES0
	ULEntry B$FERR
	ULEntry B$STOP
	ULEntry B$OEGA

	ULEntry B$ENRA 	;[8]
	ULEntry B$ENSA
	ULEntry B$ENRD	;[14]
	ULEntry B$ENSD	;[14]
	ULEntry B$EXSA

	ULEntry B$ENFA
	ULEntry B$EXFA

	PAGE
;-----------------------------------------------------------------------------
;	Entry points in user library runtime that resident
;	runtime calls NEAR.  These entry points pass parameters
;	through REGISTERS ONLY.  If this changes, the code in
;	ULtoRT.asm and RTtoUL.asm must also change.
;-----------------------------------------------------------------------------

IFNDEF	ULRTNearOrg	;IF not defined then just NULL
ULRTNearOrg MACRO
	ENDM		;;ULRTNearOrg
ENDIF			;IFNDEF ULRTNearOrg

	ULRTNearOrg

IFNDEF	ULRTNear	;IF not defined then just NULL
ULRTNear MACRO	name
	ENDM		;;ULRTNear Macro
ENDIF			;IFNDEF ULRTNear

	ULRTNear B$GETMODCODE

	PAGE
;-----------------------------------------------------------------------------
;	Entry points in user library runtime that resident
;	runtime calls FAR. These entry points pass parameters
;	on the STACK and may use all temp registers.  If any
;	routines are added which pass parameters through
;	registers, the associated code in ULtoRT and RTtoUL
;	must be examined.
;------------------------------------------------------------------------------

IFNDEF	ULRTFarOrg	;IF not defined then just NULL
ULRTFarOrg MACRO
	ENDM		;;ULRTFarOrg
ENDIF			;IFNDEF ULRTFarOrg

	ULRTFarOrg

IFNDEF	ULRTFar 	;IF not defined then just NULL
ULRTFar MACRO	name, nParms
	ENDM		;;ULRTFar Macro
ENDIF			;IFNDEF ULRTFar

;These procedures have no parameters

	ULRTFar B$ULInit,0
	ULRTFar B$ULTerm ,0
	ULRTFar B$ULDataRelease,0
	ULRTFar B$ULAllocDataImage,0	;[13]

;These procedures have 1 parameter

	ULRTFar B$CONERR,1
	ULRTFar B$ULGetProc,1
	ULRTFar B$ULGetData,1
	ULRTFar B$ULGetSeg,1
	ULRTFar B$ULGetCommon,1	;[2]

	PAGE
;-----------------------------------------------------------------------------
;	Entry points in resident runtime that user library
;	runtime calls NEAR.  These entry points pass parameters
;	through REGISTERS ONLY.  If this changes, the code in
;	ULtoRT.asm and RTtoUL.asm must also change.
;------------------------------------------------------------------------------

IFNDEF	RTNearOrg	;IF not defined then just NULL
RTNearOrg MACRO
	ENDM		;;RTNearOrg
ENDIF			;IFNDEF RTNearOrg

	RTNearOrg

IFNDEF	RTNear		;IF not defined then just NULL
RTNear	MACRO	name
	ENDM		;;RTNear Macro
ENDIF			;IFNDEF RTNear

	RTNear	B$BREAK_CHK
	RTNear	B$FOUTBX
	RTNear	B$LHLOCFDB		;[7]
	RTNear	B$RES_SETUP
	RTNear	B$TYPUI
	RTNear	B$LHFDBLOC		;[7]
	RTNear	B$$WCLF
	RTNear	B$$RCH
	RTNear	B$$TCR
	RTNear	B$$WCH
	RTNear	B$STALCTMP
	RTNear	B$TTY_SOUT
	RTNear	B$STDALC		;[15]
	RTNear	B$STDALCTMP
	RTNear	B$STDALCALLTMP		;[2]
	RTNear	B$PUTNUM
	RTNear	__BRUNERR
	RTNear	B$RUNERR
	RTNear	B$FHHighAlloc
	RTNear	B$FHHighDealloc
	RTNear	B$IFHAlloc		;[13]
	RTNear	B$FHDealloc		;[13]


	PAGE
;-----------------------------------------------------------------------------
;	Entry points in resident runtime that user library
;	runtime calls FAR. These entry points pass parameters
;	on the STACK and may use all temp registers.  If any
;	routines are added which pass parameters through
;	registers, the associated code in ULtoRT and RTtoUL
;	must be examined.
;------------------------------------------------------------------------------

IFNDEF	RTFarOrg	;IF not defined then just NULL
RTFarOrg MACRO
	ENDM		;;RTFarOrg
ENDIF			;IFNDEF RTFarOrg

	RTFarOrg

IFNDEF	RTFar		;IF not defined then just NULL
RTFar	MACRO	name
	ENDM		;;RTFar Macro
ENDIF			;IFNDEF RTFar

	RTFar	B$ULReload
	RTFar	B$IStop
	RTFar	B$END
	RTFar	B$GetNmalloc		;[16]
	RTFar	B$IEND			;[16]

	.LIST
