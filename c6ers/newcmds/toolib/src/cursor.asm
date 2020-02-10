;
; Curor locationrt for openlist
;
;   09-Dec-1986 bw  Added DOS 5 support
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

; cursor (x, y) sets the current cursor
;
cProc	cursor,<PUBLIC>
ifdef OS2
parmW	x
parmW	y
else
parmB	x
parmB	y
endif
cBegin

ifdef  OS2
	push	y
	push	x
	xor	ax, ax
	push	ax	    ; VIO handle
	call	VIOSETCURPOS
else
	mov	ah,2
	mov	dh,y
	mov	dl,x
	xor	bh,bh
        push    bp
	int	10h
        pop     bp
endif

cEnd

sEnd

end
