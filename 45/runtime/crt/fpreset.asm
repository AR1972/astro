	title	fpreset  - floating point package reset routine

;---------------------------------------------------------------------------
;
;	Copyright (C) Microsoft Corp. 1985
;
;---------------------------------------------------------------------------

include version.inc
.xlist
include cmacros.inc
;[1] include os2supp.inc
.list


createSeg CDATA, cdata,	word,	common, DATA,	DGROUP

sBegin	cdata
externD _fpinit
sEnd	cdata

dgroup	group	cdata

assumes ds,dgroup

sBegin	code
assumes cs,code

;***	_fpreset - reset floating point math and control word

cproc	_fpreset,<PUBLIC>,<>

cBegin
;ifdll - don't _loadds - DS not used>
	mov	cx,word ptr [_fpinit+2] ;Get segment of math vector
	jcxz	nofloat 		;No floating point present
	mov	bx,1
	call	[_fpinit]		; reset chip
	mov	bx,4
	mov	ax,1332h		; ignored by altmath/decmath
	call	[_fpinit]		; set initial control word
nofloat:
cEnd

sEnd

	end
