;========================================================
COMMENT #

	SMARTDRV.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================

	 Function returns the SMARTDRV version number if it's present else 0

	 unsigned GetSmartDrvVer( void )
 
	 ARGUMENTS:	NONE
	 RETURNS:	unsigned - Smartdrv internal revision number or 0

	================================================

    PAK - 04-30-92

    This only works on SD4 or greater. This is OK since we are testing
    for version greater than 4.

END COMMENT #
; =======================================================

INCLUDE	model.inc

; =======================================================

.CODE

;*--------------------------------------------------------------------------*
;*
;*  GetSmartDrvVersion - Detects presence of Bambi software (Disk Cache)
;*                       (Smartdrv.EXE ver 4.00 or greater)
;*
;*  Returns AX non-zero (version) if Bambi present (TRUE)
;*  Returns AX = zero if Bambi not present (FALSE)
;*
;*--------------------------------------------------------------------------*
GetSmartDrvVer PROC USES ES BX SI DI

      push      bp
      mov       ax,4a10h
      mov       bx,0h
      mov       bp,0h
      int       2fh
      mov       dx, bp              ; save version in case SD4 is installed
      pop       bp                  ; restore bp
      cmp       ax, 0BABEh          ; Is return value BABE
      je        BambiPresent        ; YES, SD4 is installed - Get version.
      xor       ax,ax               ; NO, return FALSE.
      jmp short IBI_Exit            ; Version is not defined.

BambiPresent:
      mov       ax, dx              ; DX contains SD4 version #

IBI_Exit:      
	  ret                           ; return to caller.

GetSmartDrvVer ENDP

; =======================================================

	END
