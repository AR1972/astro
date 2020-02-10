;
; strpre functions for lattice C
;

.xlist
include ..\h\cmacros.inc
.list

sBegin	data
assumes ds,data

externB XLTab
externB XUTab

sEnd

sBegin	code
assumes cs,code

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
