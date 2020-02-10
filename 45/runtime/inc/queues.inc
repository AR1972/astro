;***
;queues.inc - 20-Mar-86
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;*      queues.inc - contains all the needed definitions to use queues as
;*                   defined in GWQUE.ASM
;*      should be included in the 'CODE' segment for the externals to
;*      be in the right place
;
;******************************************************************************

	EXTRN	B$INITQ:NEAR
	EXTRN	B$GETQ:NEAR
	EXTRN	B$PUTQ:NEAR
	EXTRN	B$NUMQ:NEAR
	EXTRN	B$LFTQ:NEAR

QUEUE	STRUC

QUEPUT	DW	1 DUP(?)	;queue put pntr
QUEGET	DW	1 DUP(?)	;queue get pntr
QUEBOT	DW	1 DUP(?)	;bot of queue location
QUETOP	DW	1 DUP(?)	;top of queue location
QUELEN	DW	1 DUP(?)	;length of queue = max q-able bytes + 1
QUENUM	DW	1 DUP(?)	;number of bytes in que

QUEUE	ENDS

QUE_HEADER_SIZE = SIZE QUEUE

	.LIST
