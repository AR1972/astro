	page	,132
	title	brk	- C system call for memory
;***
;brk.asm - DOS brk and sbrk memory allocation
;
;	Copyright (c) 1985-1988, Microsoft Corporation.  All rights reserved.
;
;Purpose:
;	defines brk() and sbrk() memory allocation routines
;
;		NOTE:  this large model BRK.ASM must be present in the BASIC
;		libraries so that brk() will return with an error code.
;		BASIC supports both large and medium model C, and the medium
;		model BRK.ASM is not suitable for linking with BASIC programs.
;
;*******************************************************************************

include	version.inc
.xlist
include	cmacros.inc
include	msdos.inc
include	errno.inc 		; errno.h for assembler
.list
include	brkctl.inc


sBegin	data

	assumes	ds,data

externW	_abrktb			; brk limit table
externW	_atopsp			; top of stack
externW	_asizds			; top of DS
externW	errno			; error return code

sEnd


externP	brkctl


sBegin	code

	assumes	cs,code
	assumes	ds,data

page
;***
;char *sbrk(inc), brk(newval) - memory break functions
;
;Purpose:
;	resets the break value
;
;Entry:
;	int incr - number of bytes to allocate (may be negative)
;	char *newval - new top of memory (heap)
;
;Exit:
;	sbrk returns the old break value or -1 if fails
;
;Uses:
;
;Exceptions:
;
;*******************************************************************************


cProc	sbrk,<PUBLIC>,<>

	parmw	incr

cBegin
ife	sizeD
	mov	ax,[_abrktb].sz	; get old value
	mov	cx,incr		; get incr
	or	cx,cx
	js	sbrkdec		;   negative increment

	xor	bx,bx		; 0
	push	ds
	push	bx		; DS:0
	push	bx
	push	cx		; 0:cx
	errnz	BR_ARGSEG-1
	inc	bx
	push	bx
	call	brkctl		; brkctl(BR_ARGSEG,(long)cx,DS:0)
	jnc	sbrkok
	add	sp,10		;   pop arguments
	jmp	short brkOM

sbrkok:
	add	sp,10		;   pop arguments
	jmp	short sbrkexit	; return

sbrkdec:
	add	cx,ax		; cx = old + incr
	jnc	brkOM		;   overflow

	cmp	cx,[_atopsp]
	jb	brkOM		;   too small

	mov	[_abrktb].sz,cx	; save new value
	jmp	short sbrkexit

endif				;sizeD eq 0
brkOM:				; error if large data
	mov	[errno],ENOMEM	; no memory error
	mov	ax,-1
	cwd

sbrkexit:
cEnd


cProc	brk,<PUBLIC>,<>

	parmdp	newval

cBegin
if	sizeD
	jmp	brkOM		; error if large data
else
	mov	ax,word ptr (newval) ; get new value
	cmp	ax,[_atopsp]
	jb	brkOM		; out of memory
	cmp	ax,[_asizds]
	jae	brkOM
	mov	[_abrktb].sz,ax	; set new brk value
	xor	ax,ax
	jmp	short sbrkexit	; leave through sbrk
endif
cEnd	nogen

sEnd
	end
