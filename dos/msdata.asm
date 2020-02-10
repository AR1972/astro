;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;
;	SCCSID = @(#)ibmdata.asm	1.1 85/04/10
;
; DATA Segment for DOS.
;

.xlist
.xcref
include version.inc
include mssw.asm
include dosseg.inc
include dossym.inc
INCLUDE SF.INC
INCLUDE CURDIR.INC
INCLUDE DPB.INC
INCLUDE ARENA.INC
INCLUDE VECTOR.INC
INCLUDE DEVSYM.INC
INCLUDE PDB.INC
INCLUDE FIND.INC
INCLUDE MI.INC
include doscntry.inc
include fastopen.inc
include xmm.inc
.cref
.list

TITLE   IBMDATA - DATA segment for DOS
NAME    IBMDATA

installed = TRUE

include msbdata.inc			; M023
include msconst.asm
include const2.asm
include ms_data.asm
include dostab.asm
include lmstub.asm
include	wpatch.inc
include mpatch.asm
;WARNING: read comments in mpatch.asm before adding more data here! scottq
;hkn; include msinit.asm

	END


