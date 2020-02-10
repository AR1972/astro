; ========================================================

COMMENT #

	BOOT.ASM

        Microsoft Confidential
        Copyright (c) Microsoft Corporation 1990-1991
        All Rights Reserved.

	File to include partition and master
	boot records.

	johnhe - 02-14-90

END COMMENT #

;========================================================

DOSSEG
.Model	  LARGE,C

;========================================================

.DATA

		PUBLIC NewBootRec
NewBootRec LABEL BYTE

INCLUDE	BOOT.INC

	PUBLIC	MasterBootRec
MasterBootRec LABEL BYTE

INCLUDE	FDBOOT.INC

;========================================================

END
