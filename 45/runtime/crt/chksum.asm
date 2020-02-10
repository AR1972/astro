	page	,132
	title	chksum - _nullcheck routine for C
;***
;chksum.asm - _nullcheck routine for C
;
;	Copyright (c) 1985-1988, Microsoft Corporation.  All rights reserved.
;
;Purpose:
;	This routine is used to check for assignment through a null pointer.
;	Memory at DGROUP:0 is checked for destructive assignments.  This
;	routine is not particularly effective in compact and large models.
;	A stub may be provoded for this routine without affecting the
;	behavior of a correctly written C program.
;
;*******************************************************************************

?DF=		1			; this is special for c startup
include version.inc
.xlist
include cmacros.inc
.list

createSeg	_TEXT,	code,	word,	public, CODE,	<>


sBegin	code
assumes cs,code


page
;***
;_nullcheck - check-sum of start of DGROUP segment to detect null-ptr-assignmt.
;
;Purpose:
;	_nullcheck cumulatively xor's all the bytes from ds:0 through 1 past end
;	of copyright string, finally xor'ing an arbitrary non-zero constant.
;	This is used to check if a null pointer has been written to.
;
;	This version can be called as many times as the user wants.
;	The function returns zero if the checksum is OK.
;
;	Note that this checksum only detects (DS:0) null pointer assignments
;	but not (0:0) null pointer assignments.
;
;Entry:
;	Assumes DS points to the beginning of DGROUP.
;
;Exit:
;	Returns : AX = 0 if no error; AX = 1 if error.
;
;Uses:
;	BX,CX,DX,ES are destroyed.
;
;Exceptions:
;	If _nullcheck check-sum fails, an error message is written
;	to standard error, and the routine returns an error code (AX = 1).
;
;*******************************************************************************

cProc	_nullcheck,<PUBLIC>,<>
cBegin	nogen				; no arguments - so no frame



	XOR	AX,AX			;[1] checksum always OK for BASIC




	ret

cEnd	nogen


sEnd	code

	end
