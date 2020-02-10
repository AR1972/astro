	TITLE		RTTOUL - Transfers control to user lib from runtime.
;***
;RTtoUL - Transfers control from QB runtime to UL specific runtime.
;
;	Copyright <C> 1986, 1987, Microsoft Corporation
;
;Purpose:
;	This module causes control transfers from the QB runtime
;	to the user library specific runtime.  This module is linked
;	into QB.EXE and is used to interface with ULtoRT.asm which is
;	linked into the user library.
;
;******************************************************************************

	INCLUDE switch.inc
	INCLUDE rmacros.inc

;
;	Code Segments
;
	USESEG	<RT_TEXT>

;
;	Data Segments
;
	USESEG	<_DATA>
	USESEG	<_BSS>

	INCLUDE seg.inc
	INCLUDE idmac.inc

externFP exStRunFile
externFP exStChain

sBegin	_BSS
	staticW retseg,,1
	staticW retoff,,1
	staticW SISave,,1
	staticW DISave,,1

	externW b$ULSymSeg
	externW b$pULVars	;ptr to user lib vars
sEnd	_BSS

;	Macro to define code externs for UL to RT dispatch table

	PURGE RTFar,RTFarOrg,RTNear,RTNearOrg,ULRTFar,ULRTNear,ULEntry
RTFar MACRO  name
EXTRN	&name:FAR
	ENDM		;;RTFar Macro

	FirstInclude = TRUE	;define ULVars on first include

	INCLUDE ulib.inc
	PURGE RTFar,RTFarOrg,RTNear,RTNearOrg,ULRTFar,ULRTNear,ULEntry

assumes CS,RT_TEXT
sBegin	RT_TEXT

	SUBTTL	Compiler entry points handled by interpreter
;***
; Runtime entry points which are handled by the interpreter.
;	B$SRUN - RUN entry point
;	B$SCHN - CHAIN entry point
;****

cProc	B$SRUN,<PUBLIC,FAR>
cBegin	<nogen>
	ADD	SP,4		; bash return address
	JMP	exStRunFile	;vector to QB RUN executor
cEnd	<nogen>

cProc	B$SCHN,<PUBLIC,FAR>
cBegin	<nogen>
	ADD	SP,4		; bash return address
	JMP	exStChain	;vector to QB CHAIN executor
cEnd	<nogen>

	PAGE
	SUBTTL	User lib RTM entry points
;***
; User library specific entry points (called from compiled code) which are
; present in user libraries.
; These entry points are equated to 0 in the RTM tables to signify
; that the entry point is in the user library.	The real address will
; be looked up in a UL resident table.
;****

ULEntry MACRO	name
	PUBLIC	&name		;
	&name:			;
	DbHalt 	RT_TEXT,<&name not defined in resident runtime - to call use ULRTFar in ULIB.INC> ;
	ENDM

	SUBTTL Interface from UL runtime to QB runtime
	PAGE
;***
; B$ULtoRTNear - UL rt to resident rt near call mapper
;
;Purpose:
;	This routine performs a near call to a requested
;	near QB resident runtime routine.  This is called
;	when a user library specific runtime routine needs to
;	call a QB resident runtime routine.
;
;Entry:
;	SI - table offset from RTNearStart containing routine address
;	Other regs as setup for requested routine (except for DI)
;	(cannot be on stack).
;Exit:
;	As returned by requested routine (cannot return values through DI).
;Uses:
;	As defined by requested routine.
;Exceptions:
;	As defined by requested routine.
;****

;	Macro to define start of Near UL to RT table

RTNearOrg MACRO
RTNearStart:
	ENDM		;;RTNearOrg

;	Macro to define Near UL to RT table entry

RTNear MACRO  name
EXTRN	&name:NEAR
	DW	&name
	ENDM		;;RTNear Macro

cProc	B$ULtoRTNear,<PUBLIC,FAR>
cBegin
	ADD	DI,RT_TEXTOffset RTNearStart
	CALL	CS:[DI]
cEnd
	PAGE
;***
; B$ULtoRTFar - UL rt to resident rt Far call mapper
;
;Purpose:
;	This routine performs a far call to a requested
;	far QB resident runtime routine.  This is called
;	when a user library specific runtime routine needs to
;	call a QB resident runtime routine.
;
;Entry:
;	BX - table offset from RTFarStart containing routine address
;	Other regs as setup for requested routine (except for BX).
;Exit:
;	As returned by requested routine.
;Uses:
;	As defined by requested routine.
;Exceptions:
;	As defined by requested routine.
;****

;	Macro to define start of Far UL to RT table

RTFarOrg MACRO
RTFarStart:
	ENDM		;;RTFarOrg

;	Macro to define Far UL to RT table entry

RTFar MACRO  name
	DD	&name
	ENDM		;;RTFar Macro

cProc	B$ULtoRTFar,<PUBLIC,FAR>
cBegin
	ADD	BX,RT_TEXTOffset RTFarStart
	JMP	DWORD PTR CS:[BX] ;jump to routine and let it do the far return
cEnd	<nogen>

	SUBTTL Interface from QB runtime to UL runtime
	PAGE
;***
; CallULFar - perform a Far call to a UL specific runtime routine
;
;Purpose:
;	This routine interfaces a Far resident runtime call to
;	a UL specific runtime routine that is defined as Far.
;	We set up an index into a UL resident table and call
;	a far UL specific mapper with the index in AX.	The
;	mapper will then do a Far call to the requested routine.
;Entry:
;	[b$pULVars].RTtoULFarProc - is the far address of the mapper.
;	Parameters set up for requested routine. (Cannot be passed
;	through BX-CX).
;Exit:
;	return value of requested routine.
;	DX:AX = 0 if no user lib loaded
;Uses:
;	None.
;Exceptions:
;	Defined by requested routine.
;****

	FarIndex = 0

ULRTFar MACRO  name, nParms
PUBLIC	&name
&name:
	.ERRE	(FarIndex * 2) LE 255	;;index no larger than 1 byte
	.ERRE	(nParms EQ 0) OR (nParms EQ 1) ;;CallULFar assumes 0 or 1 parm
	MOV	CX,(nParms SHL 8) + (FarIndex * 2) ;;get offset from ulib table
						   ;; base(CL) num of parms(CH)
	JMP	CallULFar	;;jump to common code to call UL runtime
	FarIndex = FarIndex + 1
	ENDM		;;ULRTFar Macro

cProc	CallULFar,<FAR>
cBegin	<nogen>
	CMP	b$ULSymSeg,0	;user lib around?
	JZ	NoUlib		;brif not

DbAssertRel b$pULVars,NZ,0,RT_TEXT,<Invalid b$pULVars in userlib (CallULFar)>
	XOR	CH,CH		;clear number of parms
	MOV	BX,[b$pULVars] ;get ptr to shared user lib variables
	JMP	DWORD PTR [BX].RTtoULFarProc ;jmp to RT to UL far call helper

NoUlib:
	XOR	AX,AX		
	CWD			;return DX:AX = 0 if no user lib loaded
	MOV	BX,AX		;return BX=0 also in case it's B$ULGetCommon
	OR	CH,CH		;any parms?
	JZ	NoParm		;brif not, just RETF
	RET	2		;else RETF and pop a parm
NoParm:
cEnd
	PAGE
;***
; CallULNear - perform a near call to a UL specific runtime routine
;
;Purpose:
;	This routine interfaces a near resident runtime call to
;	a UL specific runtime routine that is defined as near.
;	We set up an index into a UL resident table and call
;	a far UL specific mapper with the index in SI.	The
;	mapper will then do a near call to the requested routine.
;Entry:
;	[b$pULVars].RTtoULNearProc - is the far address of the mapper.
;	Parameters set up for requested routine (cannot be on stack).
;	Can't currently use SI or DI to pass to requested routine.
;Exit:
;	return value of requested routine (Can't currently return SI or DI).
;Uses:
;	None.
;Exceptions:
;	Defined by requested routine.
;****

	NearIndex = 0

ULRTNear MACRO	name
PUBLIC	&name
&name:
	PUSH	SI		 ;;preserve si
	PUSH	DI		 ;;preserve di
	MOV	DI,NearIndex * 2 ;;get offset from ulib table base
	JMP	SHORT CallULNear ;;jump to common code to call UL runtime
	NearIndex = NearIndex + 1
	ENDM			 ;;ULRTNear Macro

cProc	CallULNear,<NEAR>
cBegin
DbAssertRel b$ULSymSeg,NZ,0,RT_TEXT,<Tried to call UL runtime routine when UL not present(CallULNear).>
DbAssertRel b$pULVars,NZ,0,RT_TEXT,<Invalid b$pULVars in userlib (CallULNear)>
	MOV	SI,[b$pULVars] ;get ptr to shared user lib variables
	CALL	DWORD PTR [SI].RTtoULNearProc ;call RT to UL near call helper
	POP	DI		;recover entry di
	POP	SI		;recover entry si
cEnd

;***
; Define interface routines to user library specific runtime.
;****

	FirstInclude = FALSE	;don't define ULVars on second include

	INCLUDE ulib.inc	;define entry points

	PAGE
;***
;B$FRAMESETUP	- set up initial stack frame for error handling.
;void pascal B$FRAMESETUP()
;
;Purpose:
;	B$FrameSetup is defined in user library specific runtime.
;	Since this is a critical routine with complex entry/exit
;	conditions, it is defined seperately from the dispatches
;	above.
;
;Entry:
;	DX:AX	= contains long pointer to start of user module.
;	CL	= value to which the GOSUB count should be set
;Exit:
;	SP	- points to stack after initial frame.
;	BP	- points to initial stack frame.
;Uses:
;	BX.
;Exceptions:
;	Out of memory. (Will vector to error handler with >old< frame, so that
;	the error will be reported on the call, and not the SUB entry, for
;	example).
;****
cProc	B$FRAMESETUP,<PUBLIC,FAR>
cBegin	<nogen>
DbAssertRel b$ULSymSeg,NZ,0,RT_TEXT,<Tried to call UL runtime routine when UL not present(B$FrameSetup)>
DbAssertRel b$pULVars,NZ,0,RT_TEXT,<Invalid b$pULVars in userlib (B$FrameSetup)>
	MOV	BX,[b$pULVars] ;get ptr to shared user lib variables
	JMP	DWORD PTR [BX].FrameSetup ;jmp to B$FrameSetup
cEnd	<nogen>

sEnd	RT_TEXT
	END
