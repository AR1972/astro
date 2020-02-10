;****************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1991
;
;   Title:     dsufastd.asm - Detection code entry point for WDCtrl drive
;                              validation
;
;   Version:   1.00
;
;   Date:      05-FEB-1991
;
;   Author:    MC
;
;----------------------------------------------------------------------------
;
;   Change log:
;
;      DATE     REV             DESCRIPTION
;   ----------- --- ---------------------------------------------------------
;   05-FEB-1991 MC  Initial coding.
;   19-MAR-1991 DD  Changed OkForFastDisk() to GetHardDiskType().
;
;----------------------------------------------------------------------------
;
;   This module contains the source code for the GetHardDiskType() call.
;   GetHardDiskType() is designed to be called from a C module.  It returns
;   an integer representing the system's hard disk flavors.
;
;   This module calls wdrminit.asm, which includes wddetect.inc, which is
;   written and maintained by RalphL.  In fact, RalphL is the only one who
;   can touch the code in wddetect.inc.  Do not attempt to fix anything in
;   wddetect.inc unless you are RalphL!
;
;****************************************************************************

RF_Drive_80h_Ours	EQU	00000001b
RF_Drive_81h_Ours	EQU	00000010b

EXTRN WDCtrl_Real_Mode_Init:NEAR

PUBLIC _IsSafeDiskNeeded

;----------------------------------------------------------------------------

.MODEL LARGE

;------------------------------------------------------------------------------

DGROUP GROUP _DATA

FASTDISK SEGMENT WORD PUBLIC 'CODE'

	 ASSUME CS:FASTDISK
	 ASSUME DS:FASTDISK
	 ASSUME ES:FASTDISK
	 ASSUME SS:FASTDISK

;*****************************************************************************
;
;   INT FAR GetHardDiskType(void);
;
;   ENTRY: None.
;
;   EXIT:  int - Hard disk type, as enumerated in hw_asm_c.inc.
;
;*****************************************************************************

_IsSafeDiskNeeded  PROC

         push  bp
         push  ds
         push  bx
         push  si
         push  di

         ; Set up for WDCtrl_Real_Mode_Init() call.
         mov   ax, cs
         mov   ds, ax
         mov   es, ax

         call  WDCtrl_Real_Mode_Init

         ;
         ; The possible return values from WDCtrl_Real_Mode_Init() in DX are
         ; 0, 1, and 3.
         ;
         ; 0       ==> not ok to install FastDisk
         ; 1, 2, 3 ==> ok to install FastDisk
         ;

         ; Set up TRUE return value (i.e. Not fast disk compatible so
         ; need Double Buffer switch )
         mov   ax, 1

         ; Check WDCtrl_Real_Mode_Init() return value.
         cmp   dx, 0
         je    Ok_Exit

         ; If we get to here it is OK to install FastDisk

         xor   cx, cx

         test  dx, RF_Drive_80h_Ours
         jz    TestOtherBit
         inc   cx

TestOtherBit:
         test  dx, RF_Drive_81h_Ours
         jz    DetermineNumInt13Drives
         inc   cx

DetermineNumInt13Drives:

         push  cx
         mov   ah, 8
         mov   dl, 80h
         int   13h                  ; DL = # of int13 drives.
         pop   cx
         cmp   cl, dl
         ;
         ; Set up TRUE return value (i.e. Not fast disk compatible so
         ; need Double Buffer switch )
         ;
         mov   ax, 1
         jb    Ok_Exit              ; Need Double buffer switch on SD4.
         xor   ax, ax               ; Don't need Double Buffer.

Ok_Exit:
         pop   di
         pop   si
         pop   bx
         pop   ds
         pop   bp
         ret

_IsSafeDiskNeeded  ENDP

FASTDISK ENDS

END
