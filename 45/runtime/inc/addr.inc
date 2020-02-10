;***
;addr.inc   -  26-May-86  -  Module Header Definitions
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;	****************************************************************
;	****							    ****
;	****	WARNING!	WARNING!	WARNING!	    ****
;	****							    ****
;	**** PLEASE READ REVISION HISTORY # 4 BEFORE MAKING CHANGES ****
;	****							    ****
;	****************************************************************
;
;Purpose:
;
;******************************************************************************

;-------------------------------------------------------------------------

;	Module Information Block

;	This structure occurs at the beginning of each compiled source
;	module.  This information describes offsets and segment numbers
;	for the runtime memory map.  Some of the fields have special
;	meanings for the initial module loaded since the offsets and
;	segment bases refer to the start of the particular named segment.
;	This allows the overall size of a segment to be determined.
;	For example, the difference between the contents of the OF_FT
;	and OF_DAT fields of the first module gives the overall size
;	of the static variables in all the compiled modules.


	.RADIX	10
;	Code segment offsets

modnam_len= 8			;module name length

MODULE_CODE STRUC

SG_CMP	DW	?		;Signature word - 'bl' or 'bm'

OF_MOD	DB	"        "	;Offset for Module Name
OF_STA	DW	?		;Offset for Statement address table
OF_DS	DW	?		;Offset for BC_DS + 2
OF_DAT	DW	?		;Offset for BC_DATA
OF_FT	DW	?		;Offset for BC_FT
BS_ICN	DW	?		;Base	for BC_ICN
BS_INC	DW	?		;Base	for INIT
BS_DSG	DW	?		;[CONST] Base	for DATA (runtime DATA)
OF_COM	DW	?		;Offset for COMMON
SZ_COM	DW	?		;Size	for COMMON
OF_UCONST DW	?		;Offset for CONST (user CONST)
OF_UDATA DW	?		;Offset for DATA  (user DATA)
OF_CN	DW	?		;Offset for BC_CN
SZ_FRAME DW	?		;Size	for frame
off_alloc DW	?		;Offset  heap allocation routine for ISAM
seg_alloc DW	?		;Segment heap allocation routine
off_dealoc DW	?		;Offset  heap deallocation routine for ISAM
seg_dealoc DW	?		;Segment heap deallocation routine
C_VALUE DW	?		;Value	of  /C:n switch, -1 if not given
U_FLAG	DW	?		;Switches specified at compile time
MODULE_CODE ENDS
O_ENT	=	U_FLAG + 2


RELSIZ= O_ENT+15		;size of module header structure



;	Module header signature constants

; The following are the signature values for the different runtime
; configurations.  The are two be found in the first two bytes of
; the module header (the SG_CMP field).  Notice that the constants
; are defined as byte swapped values, so that the values may be
; handled as word entities.

BM_BRUN =	"mb"		;[3]"br" - QB runtime module OMF
BR_BCOM =	"lb"		;[1]"bl" - QB "calls" module OMF

;	Data segment offsets

MODULE_DATA STRUC

OFD_RESTORE	DB	?	;Restore flag for module
		DB	?	;unused
OFD_DATA	DW	?	;Data statement pointer
OFD_ONERROR	DW	?	;ON ERROR trap address

MODULE_DATA ENDS

;----U_FLAG bit defintitions---------------------------
	u_sw_d	=      1h	;/d switch
	u_sw_i	=      4h	;[4] /fpi switch
	u_sw_a	=      8h	;[4] /fpa switch
	u_sw_x	=     20h	;/x switch
	u_sw_e	=     40h	;/e switch
	u_sw_r	=    100h	;/r switch
	u_sw_v	=    400h	;/v switch
	u_sw_w	=    800h	;/w switch
	u_sw_o	=   1000h	;/o switch
	u_sw_t	=   2000h	;/t switch

;----end of U_FLAG bit defintitions--------------------

	.LIST
