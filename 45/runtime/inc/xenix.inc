;***
;xenix.inc - 20-Mar-86 - Operating System Equates and Macros
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************

XENERR=	-1			;Xenix error flag (in ax after system call)
XENEOF=	0			;Xenix end-of-file (in ax after system call)
MEMREQ	EQU	0FFF0H		;Initial memory request

	.LIST
