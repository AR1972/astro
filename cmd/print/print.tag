;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

M000	1/3/91	SR	Bug #4955. Print was leaving the files to be printed
			open and so if a print job is cancelled, the files
			are still open. If Share is present, this causes
			sharing violations on any other access to the file.

M001    5/31/91 MD      Make use of INT 2F Idle calls to spooling, using
                        same code as INT 28 Idle.  Currently in ROM DOS
                        conditionals, should be migrated to the standard
                        version. PRINT_R.ASM.
