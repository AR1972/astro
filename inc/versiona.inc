;
;       Microsoft Confidential
;       Copyright (C) Microsoft Corporation 1991
;       All Rights Reserved.
;


major_version       equ     6       ;Major DOS version
minor_version       equ     00      ;Minor DOS Version

expected_version    equ     (MINOR_VERSION SHL 8)+MAJOR_VERSION

; m033 DOSREVNM equ     00000000B       ; bits 0-2 = revision number of DOS
; m033                                  ; currently 0.

; m037 DOSREVNM equ     00000001B       ; m033 - bits 0-2 = revision number of DOS
; m037                                          ; m033 - currently 1.

DOSREVNM        equ     00000000B       ; m037 - bits 0-2 = revision number of DOS
					; currently 0.


DOSINROM        equ     00001000B       ; bit 3 of ver flags returned in BH
DOSINHMA        equ     00010000B       ; bit 4 of ver flags 


;      if1
;      %OUT  ... for DOS Version 5.00 ...
;      endif

       ;******************************
       ;Each assembler program should:
       ;  mov ah,030h                   ;DOS Get Version function
       ;  int 021h                      ;Version ret. in AX,minor version first
       ;  cmp ax,expected_version       ;ALL utilities should check for an
       ;  jne error_handler             ; EXACT version match.
       ;******************************
