;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;	SCCSID = @(#)comsw.asm	1.1 85/05/14
;	SCCSID = @(#)comsw.asm	1.1 85/05/14

include version.inc

; Eliminate IBM ROM_FIND code (PC Jr ROM cartridges?).
;
; Don't count on turning this back on just by defining
; IBM_ROM_FIND TRUE.  I'm just keeping it around for
; reference, for now.  Microsoft's ROM file finding
; code and IBM's code would probably step on each other.
;
; To keep the DOS 5.0 non-ROM binary unchanged, we'll
; define IBM_ROM_FIND FALSE only for ROM DOS.
;
; BUGBUG : check with davidols
;
ifdef   ROMDOS
IBM_ROM_FIND 	equ 	NOT ROMDOS
else
IBM_ROM_FIND 	equ 	TRUE
endif

