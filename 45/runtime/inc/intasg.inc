;***
;intasg.inc - 02/20/86 - software interrupt assignment
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************

;	Software Interrupt Vector Numbers for run-time module


	W0__NM= 3Fh		;base interrupt number

	W1__NM= 0F8h
	W2__NM= W1__NM+1
	W3__NM= W1__NM+2


	.LIST
