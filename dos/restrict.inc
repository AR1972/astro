;***    RESTRICT.INC - List of programs that are allowed to $Exec
;
;       Microsoft Confidential
;       Copyright (C) Microsoft Corporation 1991
;       All Rights Reserved.
;
;       History:
;           20-Jul-1992 bens Initial version
;           27-Jul-1992 randym updated from Symantec list (7/23/92)
;           13-Aug-1992 randym updated from Symantec mods for 8/10/92

;       Description:
;           This file is included by msproc.asm ONLY when the symbol
;           RESTRICTED_BUILD is defined.
;
;           The file names in this include file must be ALL UPPERCASE,
;           and may have no meta characters (:.\/)
;

;* Start of list

rstfile 'INSTALL.EXE'
rstfile 'CLEANUP.EXE'
rstfile 'NAVSETUP.EXE'
rstfile 'NSETUP.OVL'
rstfile 'NAV.EXE'
rstfile 'RESCUE.EXE'
rstfile 'NRESTORE.EXE'
rstfile 'NRESTORE.OVL'
rstfile 'NBCONFIG.OVL'
rstfile 'CALIBRAT.EXE'
rstfile 'DISKTOOL.EXE'
rstfile 'NDD.EXE'
rstfile 'UNFORMAT.EXE'
rstfile 'DISKEDIT.EXE'
rstfile 'UNERASE.EXE'
rstfile 'NDIAG.EXE'
rstfile 'NORTON.EXE'
rstfile 'NORTON.OVL'
rstfile 'STARTUP.EXE'
rstfile 'STARTUP.OVL'

;* End of list
