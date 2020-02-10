ifndef INC_LIST
.xlist
endif
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1992
;
;	DEBMAC.inc -- Debugging macros for EMM386
;
;******************************************************************************


;***	pDebugF
;
;	Protected mode debug printf routine.
;
;	This macro can correctly accept 32 bit registers
;	as arguments.
;
;	See WDEB386 documentation for formating options.
;

pDebugF macro	str, args
	local	strptr
ifdef	DEBUG
_DATA	segment
strptr	db	str, 0
_DATA	ends
	ifb	<args>
	pDCall pDebugPrintf,<offset _DATA:strptr>
	else
	pDCall pDebugPrintf,<offset _DATA:strptr, args>
	endif
endif
	endm


;***	rDebugF
;
;	Real/Virtual mode debug printf routine.
;
;	This macro can correctly accept 32 bit registers
;	as arguments.
;
;	See WDEB386 documentation for formating options.
;

rDebugF macro	str, args
	local	strptr, flarp
ifdef	DEBUG
% ifidni <@curseg>,<R_CODE>
	jmp	short flarp
strptr	db	str, 0
flarp:
else
R_CODE	segment
strptr	db	str, 0
R_CODE	ends
endif
	ifb	<args>
	rDCall rDebugPrintf,<offset R_CODE:strptr>
	else
	rDCall rDebugPrintf,<offset R_CODE:strptr, args>
	endif
endif
	endm

;***	pDebugBreak
;
;	Protected mode debug break routine.
;
;	Causes a debuger trap to the next instruction if
;	a supported debugger is loaded.
;

pDebugBreak macro
	local	flarp
ifdef	DEBUG
	pushf
	Pcall	VDMC_GSEL,_TEXT:pTestDbgIns
	jnc	flarp
	int	1
flarp:
	popf
endif
	endm


;***	rDebugBreak
;
;	Real/Virtual mode debug break routine.
;
;	Causes a debuger trap to the next instruction if
;	a supported debugger is loaded.
;

rDebugBreak macro
	local	flarp
ifdef	DEBUG
	pushf
	call	rTestDbgIns
	jnc	flarp
	int	1
flarp:
	popf
endif
	endm


;***	Dcall
;
;	Call a debug procedure using C conventions.  The call
;	will be near. Uses "C" argument pushing convention and
;	removes the arguments after the call.
;
;	This macro can correctly accept 32 bit registers
;	as arguments.
;
;	USES: Flags.


pDCall	macro	name, arglst
	pushf
	?arg	<arglst>
	Pcall	VDMC_GSEL,_TEXT:name
	if	?argc ne 0
		add	sp,(?argc * 2)
	endif
	popf
	endm
	.xcref	pDcall

rDCall	macro	name, arglst
	pushf
	?arg	<arglst>
	call	name
	if	?argc ne 0
		add	sp,(?argc * 2)
	endif
	popf
	endm
	.xcref	rDcall

; Processes argument list - used by Dcall only

?arg	macro	arglst
	?argc = 0			;; number of words on stack (global)
	?numpush = 0			;; number of objects to push
	irp	x,<arglst>
		?dwordarg = FALSE
		irp	reg,<eax,ebx,ecx,edx,ebp,esp,esi,edi>
			ifidni	<x>,<reg>
				?dwordarg = TRUE
				exitm
			endif
		endm
		?argc = ?argc + 1
		if ?dwordarg
			?argc = ?argc + 1
		endif
		?numpush = ?numpush + 1
		?marg	<x>,%?numpush
	endm
	?count = ?numpush
	rept	?numpush
		?invprg <?AM>,%?count
		?count = ?count - 1
	endm
	endm
	.xcref	?arg

; Makes a macro that will push argment when invoked - used by Dcall only

?marg	macro	name, num
	.xcref
	.xcref	?AM&num
	.cref
	?AM&num &macro
		push	name
		&endm
	endm
	.xcref	?marg

; Concatenates, invokes and purges a macro name - used by Dcall and ArgVar

?invprg macro	name1, name2
	name1&name2
	purge	name1&name2
	endm
	.xcref	?invprg


;  Assert Segment Register contains expected value

AssertSegReg	MACRO	reg, value
	local	flarp
ifdef DEBUG
	push	ax
	mov	ax, reg
	cmp	ax, value
	je	short flarp
	INT	3
flarp:	pop	ax
endif
	endm

IFDEF DEBUG
DebFar	EQU	NEAR PTR
ELSE
DebFar	EQU	SHORT
ENDIF

.list	; end of DEBMAC.inc
