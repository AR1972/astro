;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1983 - 1991
; *                      All Rights Reserved.
; */
;******************************************************************************
;
;  Change Log:
;
;    Date    Who   #                      Description
;  --------  ---  ---  ------------------------------------------------------
;  12/13/90  EGH  C36  Added support for 8 fixed disks.
;
;******************************************************************************
;       Static Name Aliases
;
        TITLE   bootrec.asm - master boot record images for fdisk

_TEXT   SEGMENT BYTE PUBLIC 'CODE'
_TEXT   ENDS
_DATA   SEGMENT WORD PUBLIC 'DATA'
_DATA   ENDS
CONST   SEGMENT WORD PUBLIC 'CONST'
CONST   ENDS
_BSS    SEGMENT WORD PUBLIC 'BSS'
_BSS    ENDS

DGROUP  GROUP     CONST,  _BSS,   _DATA
        ASSUME  CS: _TEXT, DS: DGROUP, SS: DGROUP, ES: DGROUP

_DATA   SEGMENT  WORD PUBLIC 'DATA'

;
;               extern  struct struct-name BootRecordData;
;
;
;

;                                                                         ;*C36
; The following equate must match MAX_HDISK in FDISK.H                    ;*C36
;                                                                         ;*C36
MAX_HDISK       EQU     8                                                 ;*C36

PUBLIC  _master_boot_record
        public  _master_boot_record
_master_boot_record label   byte

include fdboot.inc
include fdboot.inc

IF MAX_HDISK GT 2                                                         ;*C36
include fdboot.inc      ;for disk 3
ENDIF                                                                     ;*C36
IF MAX_HDISK GT 3                                                         ;*C36
include fdboot.inc      ;for disk 4
ENDIF                                                                     ;*C36
IF MAX_HDISK GT 4                                                         ;*C36
include fdboot.inc	;for disk 5
ENDIF                                                                     ;*C36
IF MAX_HDISK GT 5                                                         ;*C36
include fdboot.inc	;for disk 6
ENDIF                                                                     ;*C36
IF MAX_HDISK GT 6                                                         ;*C36
include fdboot.inc	;for disk 7
ENDIF                                                                     ;*C36
IF MAX_HDISK GT 7                                                         ;*C36
include fdboot.inc      ;for disk 8                                       ;*C36
ENDIF                                                                     ;*C36

_DATA      ENDS

END
