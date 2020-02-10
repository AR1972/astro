;msgshort.inc
MSGSHORT_INC	= ON

;===========================================================================
;
;	MSGSHORT.INC - This file is a small subset of the automatically
;			produced qbimsgs.inc/.h (built from 
;			..\ir\basicmsg.doc).
;			Unlike qbimsgs.inc, this file is not updated when
;			basicmsg.doc is. This file is only used for those
;			modules which are tight on symbol space at assembly
;			time. The only message constants that should be
;			hard-coded here are those that are unlikely to be
;			changed (i.e., already existing message constants),
;			and should only be put in on an as-needed basis.
;
;===========================================================================

ER_OM	EQU	 7
ER_DD	EQU	10
ER_ID	EQU	12
ER_CN	EQU	17
