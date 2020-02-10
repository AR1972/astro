;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;-------------------------------------------------------------------------
;
; This is the first file to be linked in the DOS. It contains the appropriate
; offset to which the DOS is to be ORG'd. 
;
; See ..\inc\origin.inc for description
;
;---------------------------------------------------------------------------
 	
include version.inc
include dosseg.inc
include dossym.inc
include origin.inc

DOSCODE SEGMENT

	org	0


ifndef ROMDOS

	dw	PARASTART

	org	PARASTART

endif ; ROMDOS

	
DOSCODE ENDS

	END

