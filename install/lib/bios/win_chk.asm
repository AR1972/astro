; ========================================================

COMMENT #

	WIN_CHK.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	Change Log:

    	Date      Who   NUMBER		Description
  	--------  ---  ----	---------------------------------------------
        11/06/90  JAH           Created
        09/14/92  ECH           Now detects Standard-mode Windows, and DosShell

END COMMENT #

;========================================================

include BIOS_IO.INC
include	MODEL.INC

;========================================================

.CODE


; ========================================================
; Determines if currently in a Windows 3.0 VM.
; (Stolen from MSD 2.0)
;
; int IsNotWindows( void );
;
; ARGUMENTS:	NONE
; RETURNS:	int	- TRUE if not a windows VM else FALSE
;
; ========================================================

IsNotWindows PROC USES ES

      ; Check for Windows 3.1
        mov     ax,160Ah                ; WIN31CHECK
        int     2Fh                     ; check if running under win 3.1
        or      ax,ax
        jnz     Win30EnhModeCheck
        jmp     short InDosBox

      ; Check for 3.0 Enhanced mode
      Win30EnhModeCheck:
        mov     ax,1600h                ; WIN386CHECK
        int     2Fh
        test    al,7Fh
        jz      Win286Check
        jmp     short InDosBox

      ; Check for Windows/286
      ; Doubt it's needed, but i left it in anyway...
      Win286Check:
        mov     ax,1700h                ; WIN286CHECK
        int     2Fh
        cmp     al,2h                   ; If /286 installed, ver = AL.AH
        jnz     WinOldApCheck           ; /286 is always 2.x
        jmp     short InDosBox

      ; Check for WINOLDAP (also checks for DOS5 task swapper)
      WinOldApCheck:
        mov     ax,4680h                ; IS_WINOLDAP_ACTIVE
        int     2Fh
        or      ax,ax                   ; running under 3.0 derivative ?
        jz      InDosBox

      ;
      ; So now we know we're not in a Windows or DosShell dos box.
      ;
      ; NOTES:
      ;    Doesn't detect DosShell box if task swapper not active.
      ;    Don't yet know if it detects OS/2 1.x or 2.x.
      ;

        mov     ax, 1
        jmp     short ExitIt
InDosBox:
        xor     ax, ax
ExitIt:
        ret

IsNotWindows ENDP






IF 0

; ========================================================
; Determines if currently in a Windows 3.0 VM.
;
; int IsNotWindows( void );
;
; ARGUMENTS:	NONE
; RETURNS:	int	- TRUE if not a windows VM else FALSE
;
; ========================================================

IsNotWindows PROC USES ES

	mov	AX,352fh		; Get int 2fh vector
	int	21h
	mov	AX,ES			; Move segment value to AX
	or	AX,BX			; Is this a NULL ptr
	mov	AX,1			; AX == TRUE if int 2f not valid
	jz	ExitCheck		; Not windows on NULL ptr

	mov	AX,1600h		; Windows int 2fh interface
	int	2fh
	mov	BX,AX			; Move version into BX
	mov	AX,1			; AX == TRUE == windows not runnning

	or	BL,BL			; If BL == 0 then not windows
	je	ExitCheck

	cmp	BL,80h			; Else if BL == 0x80 then not windows
	je	ExitCheck
	xor	AX,AX			; Must be windows so set to FALSE
	

ExitCheck:
	ret

IsNotWindows ENDP

ENDIF


END

