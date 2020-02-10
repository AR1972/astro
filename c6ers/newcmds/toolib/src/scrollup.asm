;
; Scroll a region of the screen up
;
;   09-Dec-1986 bw - Added DOS 5 support



.xlist
include ..\h\cmacros.inc
ifdef OS2
include ..\h\subcalls.inc
endif
.list

sBegin code

assumes cs,code

;
;
; ScrollUp (x1, y1, x2, y2, n, a) scrolls a region up with attribute
;
ifdef OS2
cProc	ScrollUp,<PUBLIC>
parmW	x1
parmW	y1
parmW	x2
parmW	y2
parmW	n
parmW	a
else
cProc	ScrollUp,<PUBLIC>,<si,di>
parmB	x1
parmB	y1
parmB	x2
parmB	y2
parmB	n
parmB	a
endif
cBegin

ifdef OS2
	push	y1
	push	x1
	push	y2
	push	x2
	push	n

	mov	ax,a		    ;OS/2 expects a Cell => ATTR | VALUE
	xchg	ah,al
	mov	al,' '
	mov	a,ax

	push	ss
	lea	ax, a		    ; Assume SS == DS
	push	ax
	xor	ax, ax
	push	ax		    ; VIO handle
	call	VIOSCROLLUP
else
	mov	ah,6

	mov	cl,x1
	mov	ch,y1

	mov	dl,x2
	mov	dh,y2

	mov	bh,a

	mov	al,n
	push	bp
	int	10h
	pop	bp
endif

cEnd

sEnd

end
