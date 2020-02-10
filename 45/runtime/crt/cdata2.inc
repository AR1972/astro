;***
;cdata2.inc - 07-Sep-88 - More C Run-time Shared Global Data
;***
;***
;
;	Copyright <C> 1988, Microsoft Corporation
;
;Purpose:
;	This file contains special C startup debugger screen swap data items.
;	They are of class COMMON, and therefore can't be in GLOBAL.INC with
;	the rest of the C data items.  They are not included into QB.EXE, but
;	are present in every QLB.  They are present in both the user program
;	and the RTM (for custom RTM capability), and are in each /O exe.
;
;******************************************************************************

sBegin	dbdata
				; Debugger Screen swap setup segment
?PLM = 0				;[2]
globalW	 __aDBswpflg,0			;[1] Flag == __aDBswpyes if should
					;[1] attempt swapping
globalW  __aDBswpchk,___aDBretaddr	;[1] By default points to a model
					;[1] dependent ret
globalW  __aDBrterr,___aDBretaddr	;[1] 
globalW  __aDBexit,___aDBretaddr	;[1]  (Report we are exiting)
globalD  __aDBcallbk,0			;[1] Call back address to debugger
__aDBdoswp	=	0d6d6h
public	 __aDBdoswp
?PLM = 1				;[2]

sEnd	dbdata
