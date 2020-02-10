;
; Perform MSDOS absolute disk read/write
;
;   09-Dec-1986 bw  Added DOS 5 switch
;   30-Oct-1987 bw  Changed 'DOS5' to 'OS2'

ifdef OS2
    if2
    %out int25() and int26() are not implemented under OS/2 Protect Mode.
    .ERR
    endif
else

.xlist
include ..\h\cmacros.inc
.list

sBegin	data
assumes ds,data

externW _doserrno

sEnd

sBegin	code
assumes cs,code

cProc	int25,<PUBLIC>,<SI,DI>
parmB	drive
parmD	dst
parmW	count
parmW	sector
cBegin
	push	ds
	mov	al,drive
	lds	bx,dst
	mov	cx,count
	mov	dx,sector
	push	bp
	int	25h
	pop	bx			; INT 25 leaves flags on stack
	pop	bp
	pop	ds
	jc	err25
	xor	ax,ax
	mov	_doserrno,ax
	jmp	short end25
err25:
	mov	_doserrno,ax
	mov	ax,-1
end25:
cEnd

cProc	int26,<PUBLIC>,<si,di>
parmB	drive
parmD	src
parmW	count
parmW	sector
cBegin
	push	ds
	mov	al,drive
	lds	bx,src
	mov	cx,count
	mov	dx,sector
	push	bp
	int	26h			; INT 25 leaves flags on stack
	pop	bx
	pop	bp
	pop	ds
	jc	err26
	xor	ax,ax
	mov	_doserrno,ax
	jmp	short end26
err26:
	mov	_doserrno,ax
	mov	ax,-1
end26:
cEnd

sEnd

endif

end
