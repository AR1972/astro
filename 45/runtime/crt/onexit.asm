	page	,132
	title	onexit - saves function pointer for pretermination execution
;***
;onexit.asm - save function for execution on exit
;
;	Copyright (c) 1985-1988, Microsoft Corporation.  All rights reserved.
;
;Purpose:
;	defines onexit(), atexit() - save function for execution at exit
;
;*******************************************************************************

?DF	=	1		; tell cmacros.inc we want to define our own segments

include	version.inc
.xlist
include	cmacros.inc
.list

assumesdata	macro	seg	;;[1] Newer versions of CMACROS reject
assumes	seg,DGROUP		;;[1]
endm				;;[1]

createSeg _TEXT, code,	word,	public, CODE,	<>
createSeg _DATA, data,	word,	public, DATA,	DGROUP

createSeg XOB,	xobseg, word,	public, BSS,	DGROUP
createSeg XO,	xoseg,	word,	public, BSS,	DGROUP ; onexit table
createSeg XOE,	xoeseg, word,	public, BSS,	DGROUP

defGrp	DGROUP			; define DGROUP

codeOFFSET equ	offset _TEXT:
dataOFFSET equ	offset DGROUP:

    CPsize= 4


sBegin	xoseg
assumesdata	ds		;[1]

ontable	db	32*CPsize dup (?) ; in BSS because it is not initialized
onend	label	byte

sEnd


sBegin	data
assumesdata	ds		;[1]

staticW	onptr,<dataOFFSET ontable>

sEnd


sBegin	code
assumes	cs,code
assumesdata	ds		;[1]

page
;***
;onexit(func), atexit(func) - add function to be executed upon exit
;
;Purpose:
;	The onexit/atexit functions are passed a pointer to a function
;	to be called when the program terminate normally.  Successive
;	calls create a register of functions that are executed last in,
;	first out.
;
;Entry:
;	void (*func)() - pointer to function to be executed upon exit
;
;Exit:
;	onexit:
;		Success - return pointer to user's function.
;		Error - return NULL pointer.
;	atexit: (MSC 5.0 Only)
;		Success - return 0.
;		Error - return non-zero value.
;
;Uses:
;
;Exceptions:
;
;*******************************************************************************

cProc	atexit,<PUBLIC>,<> 	; atexit entry point

	parmCP	fname

cBegin
	mov	ax,-1		; error return is non-zero
	jmp	SHORT common	; join common code
cEnd	nogen

cProc	onexit,<PUBLIC>,<> 	; onexit entry point

	parmCP	fname

cBegin
	xor	ax,ax		; error return is 0 (NULL pointer)
common:

				; common code
	mov	cx,ax		; save error value as entry code
	cwd			; dx:ax = error return value
	mov	bx,[onptr]	; get current onexit table pointer
	cmp	bx,dataOFFSET onend ; is the table full?
	je	retax		;   yes - return 0

	add	[onptr],CPsize	; point to next table entry
	mov	dx,word ptr (fname+2) ; get terminator segment
	mov	[bx+2],dx	; save terminator segment
	mov	ax,word ptr (fname) ; get terminator offset
	mov	[bx],ax		; save terminator offset

	or	cx,cx		; atexit or onexit ??
	jz	retax		; onexit -- return func addr
	xor	ax,ax		; atexit -- return 0

retax:				; return



cEnd

page

?PLM = 1			;[1] B_OnExit uses PASCAL calling conventions

;***
;B_OnExit - register a user lib termination routine
;far pascal B$OnExit(fpRoutine)
; far *fpRoutine;
;
;Purpose:
;	Register a routine to be called by the runtime when BASIC
;	terminates, or is restarted.  This allows foreign languages
;	a hook for necessary clean up.
;Entry:
;	fpRoutine - far pointer to termination routine
;Exit:
;		DX:AX = 0 if no space left in table (32 entries max).
;Uses:
;	Per Convention.
;Exceptions:
;	None.
;******************************************************************************

cProc	B_OnExit,<PUBLIC,FAR>	;[1] moved here from USRENTRY.ASM
parmD	fpRoutine
cBegin
	cCall	_onexit,<fpRoutine> ;call C onexit() function to do the work
	ADD	SP,4		;clean parms off of the stack
cEnd

?PLM = 0			;[1] return to C calling conventions


sEnd
	end
