;***
;sb.inc - SB usage definitions
;***
	.XLIST
;***
;
;	Copyright <C> 1988, Microsoft Corporation
;
;Purpose:
;	NOTE: This is a SHARED INCLUDE FILE, used by both the RUNTIME
;	NOTE: and the interpreter projects.  Any changes made to one should
;	NOTE: be carried over to the other!!!
;	NOTE: There are some minor differences between the two versions for
;	NOTE: this file, so DO NOT just copy it!!!
;
;
;******************************************************************************
SB_INC	=	ON


;HMEM_ constants used by SBMGR allocation routines

HMEM_FIXED	EQU	0000H
HMEM_MOVEABLE	EQU	0002H
HMEM_NOCOMPACT	EQU	0010H
HMEM_ZEROINIT	EQU	0040H
HMEM_DISCARDABLE EQU	0F00H
HMEM_EMM	EQU	0004H	

sBegin	DATA
sEnd	DATA

;For QB5, we need to have constants that map to the appropriate place
;in the table of FHD's
SbDgroup=	  2
SbString=	 97
SbFile	=	100
SbRs	=	103
SbGVar	=	106
SbGNam	=	109
SbDirect=	112
