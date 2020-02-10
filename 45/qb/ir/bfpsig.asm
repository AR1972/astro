	title	bfpsig - BASIC floating point error routine


comment !
---------------------------------------------------------------------------

    This module defines the __fpsignal routine for BASIC

    Copyright (C) Microsoft Corp. 1984, 1985, 1986

    Written by Gregory F. Whitten

---------------------------------------------------------------------------

    Revision History

	 5/12/84	Greg Whitten
			signal routine moved to this module
			for C/Pascal/FORTRAN math sharing

	09/26/86	Greg Whitten
			converted to BASIC signal handling

        04/16/87        Greg Whitten
                        SS must be equal to DGROUP (DS restored from SS)

        04/21/87        Greg Whitten
                        map invalid errors to overflow because integer
                        overflow generates an invalid exception

	05/13/87	Len Oorthuys
			QBI variation - update the pcode offset of errors
			that occur in executors

	09/04/87	Leo Notenboom [1]
			Ensure that stack passed to B$RUNERR points to the
			address of the exception.

	10/09/87	Brian Lewis [2]
			Bug fix - offset of math error is at bp+08, not
			bp+0ah.

	03/10/88	Brian Lewis
			SizeD versions cannot assume ds == ss.

	04/12/88	Brian Lewis
			Last change was bogus.

---------------------------------------------------------------------------
!

	include 	version.inc
	IncludeOnce	exint
	IncludeOnce	context
	IncludeOnce	qbimsgs

externP B$RUNERR

externFP __fpmath			

sBegin	CODE
	assumes cs,CODE
	assumes es,NOTHING
	assumes ss,DATA


errtab	label	byte
	db	ER_OV			; invalid (assume integer overflow)
	db	ER_FC			; denormal
	db	ER_DV0			; zerodivide
	db	ER_OV			; overflow
	db	ER_OV			; underflow
	db	ER_FC			; precision
	db	ER_FC			; unemulated
	db	ER_FC			; sqrtneg
	db	ER_OV			; intoverflow
	db	ER_FC			; stkoverflow
	db	ER_FC			; stkunderflow


;	process floating point error - dispatch to BASIC error handling
;
;	there is stuff (return addresses) left on the stack

labelFP <PUBLIC,__fpsignal>

	sub	al,81h			; convert to 0 based error numbers
	mov	bx,offset errtab
	xlat	byte ptr cs:[bx]
	cbw				; (ax) = BASIC error code
	xchg	ax,cx			; (cx) = BASIC error code

        push    ss
        pop     ds                      ; (ds) = DGROUP

	mov	bx,sp

nextframe:
	mov	ax,[bx+0ah]		; (ax) = segment of math error
	cmp	ax,seg __fpmath 	; another iret on stack?
	jnz	gotseg
	add	bx,6
	jmp	nextframe		; go try the next frame

gotseg:
	lea	sp,[bx+08h]		; reset stack to that at error time
	mov	dx,cs
	cmp	ax,dx			; executor segment?
	jnz	sigexit 		; Not in the executors - exit
	mov	ax,[bx+08]		; (ax) = offset of math error
	cmp	ax,codeOFFSET __fpsignal; in the mathpack?
	jnb	sigexit 		; In the mathpack - exit
	mov	[grs.GRS_oTxCur],si	; record oTx of error

sigexit:
	mov	bx,cx			; (bx) = BASIC error code
	jmp	B$RUNERR		; goto BASIC error handler


sEnd	CODE
	end
