;***
; qstruc.inc - 20-Mar-86
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************

;*******************************************************
;This queue structure is used by the MUSIC queues and
;the COM buffer queues (both input and output). The
;field QUNOTE is not used by the COM buffer queues
;but will be present as a dummy field so that the same
;queue manipulation routines may be used.


QUE_CTRL_BLOCK STRUC

QUNOTE	DW	1 DUP(?)
QUETOP	DW	1 DUP(?)
QUEBOT	DW	1 DUP(?)
QUELEN	DW	1 DUP(?)
QUENUM	DW	1 DUP(?)
QUEGET	DW	1 DUP(?)
QUEPUT	DW	1 DUP(?)

QUE_CTRL_BLOCK ENDS

QUSIZE	=	SIZE QUE_CTRL_BLOCK

QLENTH	=	32*6		;room for 32 notes

	.LIST
