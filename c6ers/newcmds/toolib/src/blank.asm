;
; Blank a region of the screen
;
;   09-Dec-1986 bw  Added DOS 5 support
;   26-Jan-1987 bw  Fix 286DOS support
;   30-Oct-1987 bw  Changed 'DOS5' to 'OS2'

.xlist
include ..\h\cmacros.inc
ifdef OS2
include ..\h\subcalls.inc
if1
%out  ! OS2 module
endif
endif
.list

sBegin code

assumes cs,code

;
;
; blank (x1, y1, x2, y2, a) blanks a region
;
;
ifdef  OS2
cProc	blank,<PUBLIC>
parmW	x1
parmW	y1
parmW	x2
parmW	y2
parmB	a
localW	cell
else
cProc	blank,<PUBLIC>,<si,di>
parmB	x1
parmB	y1
parmB	x2
parmB	y2
parmB	a
endif
cBegin

ifdef  OS2
	mov	ah, a
	mov	al, 20H
	mov	cell, ax
	push	y1
	push	x1
	push	y2
	push	x2
	mov	ax, y2
	sub	ax, y1
	inc	ax
	push	ax
	push	ss
	lea	ax, cell
	push	ax
	xor	ax, ax
	push	ax	    ; VIO handle
	call	VIOSCROLLUP
else
        mov     ah,6
	mov	cl,x1
	mov	ch,y1
	mov	dl,x2
	mov	dh,y2
	mov	bh,a
	mov	al,0	    ; use special case al=0 clear entire window
        push    bp
	int	10h
        pop     bp
endif

cEnd

sEnd

end
