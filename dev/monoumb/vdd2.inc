;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1989-1991
;
;   Title:	vdd2.inc - VDD definitions for Secondary VDD
;
;   Version:	1.00
;
;   Author: MDW
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;   02-Aug-1989 MDW Added service definitions
;==============================================================================


Begin_Service_Table VDD2

;***************
;VDD2 services for other devices and VMM(Get_Version must be first)
;
VDD2_Service	VDD2_Get_Version, LOCAL  ; Get version number and ID string ptr
End_Service_Table VDD2
