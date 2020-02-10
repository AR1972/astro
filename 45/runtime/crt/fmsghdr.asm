	page	,132
	title	fmsghdr       - far message handler and finder
;***
;fmsghdr.asm - far message handler and finder
;
;	Copyright (c) 1986-1988, Microsoft Corporation.  All rights reserved.
;
;Purpose:
;	Far message handler and finder.
;
;*******************************************************************************


?DF=	1			; this is special for c startup
include	version.inc
?PLM=	1			; pascal calling conventions
.xlist
include	cmacros.inc
include	msdos.inc
.list

createSeg _TEXT, code,	word,	public, CODE,	<>

createSeg _DATA, data,	word,	public, DATA,	DGROUP

createSeg FAR_HDR,fhdr, byte,	public, FAR_MSG,FMGROUP
createSeg FAR_MSG,fmsg, byte,	public, FAR_MSG,FMGROUP
createSeg FAR_PAD,fpad, byte,	common, FAR_MSG,FMGROUP
createSeg FAR_EPAD,fepad, byte,	common, FAR_MSG,FMGROUP

defGrp	DGROUP			; define DGROUP
defGrp	FMGROUP			; define FMGROUP

codeOFFSET equ	offset _TEXT:
fmsgOFFSET equ	offset FMGROUP:

assumesdata	macro	seg	;;[1] Newer versions of CMACROS reject
assumes	seg,DGROUP		;;[1]
endm				;;[1]

sBegin	fhdr
assumesdata	ds		;[1]

	db	'<<FMSG>>'
stfmsg	label	byte

sEnd

SBegin	fpad
assumesdata	ds		;[1]

	dw	-1		; message padding marker

sEnd

sBegin	fepad
assumesdata	ds		;[1]

	db	-1

sEnd


sBegin	code
assumes	cs,code
assumesdata	ds		;[1]

page
;***
;__FMSG_TEXT(messagenumber) - find message for given message number
;
;Purpose:
;	This routine returns a far pointer to the message associated with
;	messagenumber.	If the message does not exist, then a 0:0 is returned.
;
;Entry:
;	==PASCAL CALLING CONVENTIONS==
;	messagenumber	= WORD number of desired message
;
;Exit:
;	DX:AX	= pointer to message text or 0:0 if message doesn't exist
;
;Uses:
;
;Exceptions:
;
;*******************************************************************************

cProc	__FMSG_TEXT,<PUBLIC>,<ds,si,di,es>  ;[1] pascal calling - added ES

parmW	msgt

cBegin
	mov	ax,FMGROUP
	mov	ds,ax		; ds = FMGROUP (force it always)
	push	ds
	pop	es
	mov	dx,msgt		; dx = message number
	mov	si,fmsgOFFSET stfmsg ; start of far messages

tloop:
	lodsw			; ax = current message number
	cmp	ax,dx
	je	found		;   found it - return address
	inc	ax
	xchg	ax,si
	jz	found		;   at end and not found - return 0
	xchg	di,ax
	xor	ax,ax
	mov	cx,-1
	repne	scasb		; skip until 00
	mov	si,di
	jmp	tloop		; try next entry

found:
	xchg	ax,si
	cwd			; zero out dx in case NULL
	or	ax,ax
	jz	notfound
	mov	dx,ds		; remember segment selector
notfound:
cEnd

sEnd

	end
