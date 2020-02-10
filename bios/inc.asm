;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1981-1991
; *                      All Rights Reserved.
; */
TITLE	FILE - Listing of DOS .INC files

include dosseg.inc

CODE	SEGMENT
	ASSUME	SS:DOSGroup,CS:DOSGroup

include dossym.inc
include devsym.inc

include arena.inc
include biostruc.inc
include bootform.inc
include bpb.inc
include copyrigh.inc
include cpmfcb.inc
include cputype.inc
include curdir.inc
include devmark.inc
include doscntry.inc
include dpb.inc
include ea.inc
include exe.inc
include filemode.inc
include find.inc
include intnat.inc
include ioctl.inc
include mi.inc
include msbds.inc
include msdskpr.inc
include msequ.inc
include msgroup.inc
include mult.inc
include pdb.inc
include sf.inc
include smifssym.inc
include syscall.inc
include sysvar.inc
include vector.inc
include version.inc

CODE ENDS
END

