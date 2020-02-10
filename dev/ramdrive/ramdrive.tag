;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
Revision tags for RAMDRIVE directory
------------------------------------

M00	DBO	07/30/90	Make sure EMS page frame really exists.

M001    DB      11/27/90        Extend max. RAMDrive size from 4M to 32M-1K.

M002    DB      12/14/90        Decrease min. RAMDrive size from 16K to 4K.
                                Versions >= Dos 5.00 use memory limit
                                provided in INIT packet BREAK address,
				rather than value returned by INT 12h.

