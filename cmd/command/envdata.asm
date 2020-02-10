;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;	SCCSID = @(#)envdata.asm	1.1 85/05/14
;
; This file is included by init.asm and is used as the default environment.


Environment Struc                       ; Default COMMAND environment

Env_PathString 	db	"path="
Env_PathSpec    db      "c:\msdos"
                db      0
Env_PrmptString db      "prompt="
Env_PrmptSpec   db      "$p$g"
                db      0
Env_ComString   db      "comspec="
Env_ComSpec     db      "\command.com"
		db	134 dup (0)

Environment ends


MAX_COMSPEC     equ     SIZE Environment - Env_ComSpec
