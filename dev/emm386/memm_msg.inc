;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;	Title:	EMM386 - MICROSOFT Lightning EMM device driver
;
;	Module: MEMM_MSG.INC - messages for ON/OFF/AUTO utility
;
;	Version: 2.04
;
;	Date:	June 7,1986
;
;	Author: Brad Tate
;
;******************************************************************************
;
;	Change Log:
;
;	DATE	 REVISION	Description
;	-------- --------	--------------------------------------------
;	06/07/86 Original
;	06/28/86 0.02		Name change from CEMM386 to CEMM (SBP).
;	07/14/86 0.06		Added Inaccessible message (SBP).
;	06/07/87 2.00		Added Weitek message (SBP).
;	08/20/87 2.04		Fixed "CEMM is in Auto mode" message (SBP).
;	04/13/88 3.30  (*B)	Change error messages to match doc's (RDV).
;
;******************************************************************************
;
not_there	db	"CEMM driver not installed.",CR,LF,"$"

vmode		db	"CEMM Active.",CR,LF,"$"

rmode		db	"CEMM Inactive.",CR,LF,"$"

inaccess	db	"Expanded memory data is inaccessible until CEMM is"
		db	" re-activated.",CR,LF,"$"

amode		db	"CEMM is in Auto mode.",CR,LF,"$"

verror		db	"Unable to activate CEMM.",CR,LF,"$"

rerr		db	"Unable to de-activate CEMM.",CR,LF,"$"         ;*B

aerr		db	"Unable to place CEMM in Auto mode.",CR,LF,"$"

won_mode	db	"Weitek Coprocessor support is enabled.",CR,LF,"$"  ;*B

woff_mode	db	"Weitek Coprocessor support is disabled.",CR,LF,"$"

won_err 	db	"Unable to enable Weitek Coprocessor support"
		db	CR,LF,"$"

woff_err	db	"Unable to disable Weitek Coprocessor support"
		db	CR,LF,"$"

w_not_inst	db	"Weitek Coprocessor not installed.",CR,LF,"$"

w_inaccess	db	"Weitek Coprocessor is inaccessible until CEMM is"
		db	" re-activated.",CR,LF,"$"

parmerr 	db	"Usage: CEMM [ ON | OFF | AUTO ] [ W=ON | W=OFF ]"
		db	CR,LF,"$"

