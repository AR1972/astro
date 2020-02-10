;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;
; string functions for lattice C
;

.xlist
include	version.inc
include cmacros.inc
.list

sBegin	data
assumes ds,data

externB XLTab
externB XUTab

sEnd

sBegin	code
assumes cs,code

externP strlen

ifdef DBCS
externP IsDBCSLeadByte
externP CheckDBCSTailByte
endif

;
; strbscan (string, set) returns pointer to 1st char in set or end
;
cProc	strbscan,<PUBLIC>,<SI,DI>
parmW	str
parmW	set
cBegin
	push	ds
	pop	es
	cCall	strlen,<set>
	inc	ax
	mov	bx, ax
	mov	si,str
	cld
bscan:
	lodsb
	mov	cx,bx
	mov	di,set
;
; While not in the set
;
ifdef DBCS
bscan_pass:
endif
	repnz	scasb
	jnz	bscan
ifdef DBCS
	dec	di
	cCall	CheckDBCSTailByte,<set,di>
	inc	di
	or	ax,ax
	jnz	bscan_pass		; pass if this is tail byte
endif
	lea	ax,[si-1]
cEnd

;
; strbskip ( string, set ) returns pointer to 1st char not in set
;
cProc	strbskip,<PUBLIC>,<SI,DI>
parmW	str
parmW	set
cBegin
	push	ds
	pop	es
	cCall	strlen,<set>
	inc	ax
	mov	bx, ax
	mov	si,str
	cld
bskip:
	lodsb
	or	al,al
	jz	eskip
	mov	cx,bx
	mov	di,set
;
; While not in the set
;
ifdef DBCS
bskip_pass:
endif
	repnz	scasb
	jz	bskip
ifdef DBCS
	dec	di
	cCall	CheckDBCSTailByte,<set,di>
	inc	di
	or	ax,ax
	jnz	bskip_pass		; pass if this is tail byte
endif
eskip:
	lea	ax,[si-1]
cEnd

;
; strpre (s1, s2) returns -1 if s1 is a prefix of s2, 0 otherwise. Ignores
; case.
;
cProc	strpre,<PUBLIC>,<si,di>
parmW	pref
parmW	str
cBegin
	cld
	mov	si,pref
	mov	di,str
	mov	bx,dataOFFSET xltab
preCompare:
	lodsb
	mov	ah,[di]
	inc	di

ifdef DBCS
	push	ax
	xor	ah,ah
	cCall	IsDBCSLeadByte,<ax>
	mov	dl,al			; save pref char type
	pop	ax
	push	ax
	mov	al,ah
	xor	ah,ah
	cCall	IsDBCSLeadByte,<ax>
	mov	dh,al			; save str char type
	pop	ax
	cmp	dl,dh
	jnz	preDif			; if char type not same
	or	dl,dl
	jz	@f			; if not lead byte
	cmp	al,ah
	jnz	preDif			; if lead byte not same
	lodsb				; get tail byte
	mov	ah,[di]
	inc	di
	cmp	al,ah
	jnz	preDif			; if tail byte not same
	or	al,al
	jz	preYes			; if end
	jmp	short preCompare
@@:
endif

	xlat
	xchg	ah,al
	xlat

	cmp	ah,al
	jnz	preDif
	or	ah,ah
	jnz	preCompare
preYes:
	mov	ax,-1
	jmp	short preDone
preDif:
	or	ah,ah
	jz	preYes
	xor	ax,ax
preDone:
cEnd

sEnd

end
