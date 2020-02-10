; ========================================================

COMMENT #

	KEYBOARD.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	Functions to use the ROM BIOS for getting
	input from keyboard via interrupt 16h


	Video initialization function. This function
	must be called before any of the other vio
	library functions.


	johnhe - 03/03/89

END COMMENT #

;========================================================

include	BIOS_IO.INC
include	MODEL.INC

;========================================================


.CODE

; =======================================================
; int  KbdGetKey( void );
;
; Waits for a character from the keyboard and returns the
; character in AL and scan code in AH
; =======================================================

KbdGetKey PROC

        mov     AH,KBD_GET_CHAR
	int	16h
        ret

KbdGetKey ENDP

; =======================================================
; int  KbdIsWaiting( void );
;
; Checks for a character waiting in the keyboard buffer
; Returns 0 in AX if no character is waiting.
; The character is not removed from the buffer and will
; be returned by the next call to KbdGetKey
; =======================================================

KbdIsWaiting PROC 

        mov     AH,KBD_IS_WAITING
	int	16h
        mov     AX,1                    ; Assume char is waiting
        jnz     IsWaitingReturn         ; Char waiting so return character
        xor     AX,AX                   ; No char waiting so return zero
IsWaitingReturn:
        ret

KbdIsWaiting ENDP

; =======================================================
; int  KbdGetStatus( void );
;
; Returns the status of the shift and ctrl keys in AX
; =======================================================

KbdGetStatus PROC

        mov     AH,KBD_GET_STATUS
	int	16h
        cbw                             ; Clear AH
        ret

KbdGetStatus ENDP

; =======================================================
	END
; =======================================================
