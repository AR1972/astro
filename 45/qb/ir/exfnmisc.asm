page	49,132
	TITLE	exfnmisc.asm - misc. intrinsic function executors
;***
;exfnmisc.asm - misc. intrinsic function executors for QBI
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;
;****************************************************************************

	.xlist
	include		version.inc
EXFNMISC_ASM = ON
	IncludeOnce	architec
	IncludeOnce	context
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	extort
	IncludeOnce	heap
	IncludeOnce	opintrsc
	IncludeOnce	opmin
	IncludeOnce	opaftqb4
	.list

assumes es, NOTHING
assumes ss, DATA

sBegin	DATA
	;NOTE: these next two data externs are for RUNTIME data
	externW	b$errlin		;basic error line number
	externW b$errnum		;basic error number


sEnd	DATA

sBegin	CODE
assumes cs, CODE

	    extrn   I4ToU2:near





;=============================================================================
;		Error Handling Functions
;=============================================================================
MakeExe	exFnErr,opFnErr
	push	[b$errnum]
	jmp	short Disp1		;that's it - dispatch


MakeExe	exFnErl,opFnErl
	xor	ax,ax
	push	ax			;force this to be a positive long int
	push	[b$errlin]
Disp1:
	jmp	Disp			;that's it - dispatch

MakeExe	exFnErdev,opFnErdev
	CALLRT	B$ERDV,DispAx

MakeExe	exFnErdev_,opFnErdev_
	CALLRT	B$ERDS,DispMovAx

;=============================================================================
;		Math-related Functions
;=============================================================================
MakeExe	exFnRnd,opFnRnd
	CALLRT	B$RND0,DispR4

MakeExe	exFnRnd1,opFnRnd1
;Added with [6]
	sub	sp,4			;Make room for argument
	mov	bx,sp
	fstp	dword ptr DGROUP:[bx]	;Copy to local stack
	fwait
;End of [6]
	CALLRT	B$RND1,DispR4

;=============================================================================
;		Heap-related Functions
;=============================================================================
MakeExe	exFnFreI2,opFnFre
	CALLRT	B$FRI2,DispMovDxAx


MakeExe	exFnFreSD,opFnFre
	CALLRT	B$FRSD,DispMovDxAx

MakeExe	exFnVarPtr,opFnVarPtr
	pop	ax			;Get the offset
	pop	dx			;Throw away the segment
	jmp	DispAx			;Push ax and dispatch

MakeExe	exFnPeek,opFnPeek
	call	I4toU2			;coerce I4 addr on stack to a U2
	CALLRT	B$PEEK,DispAx

MakeExe	exFnSetmem,opFnSetmem
	mov	di,sp			;di points to I4 argument to SETMEM
	test	byte ptr [di+3],080H	;are we passing a negative number
					;  to SETMEM?
	jz	SetMem_Cont		;brif not (don't want to give OM
					;  error in this case ...)
	call	GrabSpace		;don't let the user squeeze memory
					;  down to the point that we can't 
					;  get back into the user interface ...
	jnz	SetMem_Cont		;brif we did grab enough space

	xor	ax,ax			;change this to a SETMEM(0) call
	mov	word ptr [di],ax
	mov	word ptr [di+2],ax
SetMem_Cont:
	CALLRT	B$SETM
	push	dx
	push	ax
	call	ReleaseSpace		;give back space if we grabbed any
	jmp	DispMov

MakeExe	exFnVarSeg,opFnVarSeg
	pop	ax			;Throw away the offset
	jmp	Disp			;Leave the seg on the stack

MakeExe	exFnVarPtr_,opFnVarPtr_
	LODSWTX				;fetch type constant for variable
	push	ax			;pass type constant to B$VARP
	CALLRT	B$VARP,DispMovAx

MakeExe	exFnPlay,opFnPlay
	CALLRT	B$FPLY,DispAx

	;Added with [8]


	;End of [8]

sEnd	CODE
end
