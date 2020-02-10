;==========================================================================
COMMENT	#

 GETMODEL.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

END COMMENT #
;==========================================================================

INCLUDE model.inc

.CODE

CONVERTIBLE_MODEL_BYTE equ 0f9h

;==========================================================================
;
; GetModelBytes()
;
; Returns model byte in ah and sub-model byte in al, or 0 in ah and al if the
; BIOS can't tell us.
;
; As far as the MS high-level languages are concerned, we only need to
; save DS, SS, DI, and SI if we trash them.  But to be friendly, we won't
; corrupt anything except AX and flags.
;
;==========================================================================

GetModelBytes PROC USES ES BX

         ; Get model bytes from ROM BIOS using BIOS call.
         ; (Actually, they're stored at f000:fffe.)
         mov   ah, 0c0h
         int   15h

         ;
         ; For PC, PCjr, and really old PC XT and AT BIOSs, this int 15h call
         ; will set the carry flag, and return:
         ;
         ; (AH)    = 80h   PC and PCjr
         ;
         ; (AH)    = 86h   PC XT BIOS dated 11/08/82 and AT BIOS dated
         ;                 1/10/84, or PS/2 (except Model 30) if system model
         ;                 cannot be determined
         ;
         ; If the carry flag is not set by this int 15h call, the system is a
         ; PC XT with ROM BIOS dated 1/10/86 or after, an AT with ROM BIOS
         ; dated 6/10/85 or after, a PC XT Model 286, a PC Convertible, or a
         ; PS/2.  In these cases, the int 15h call will return:      
         ;
         ; (AH)    = 0
         ; (ES:BX) = pointer to system descriptor vector in ROM
         ;
         ; The third byte of this system descriptor vector in ROM is the
         ; model byte, and the fourth byte is the submodel byte.
         ;

         jc    No_Descriptor_Available

         ; Build model byte address.
         inc   bx
         inc   bx

         ; Get model byte.
         mov   ah, es:[bx]

         ; Get sub-model byte.
         inc   bx
         mov   al, es:[bx]

         ;
         ; The USES portion of the PROC directive directs masm to
         ; automatically push used registers on entry and pop them on exit
         ; (i.e., at all rets).  By employing a short jump here
         ; (jmp short exit) instead of two pops and a ret, we would save one
         ; byte of code.  However, the short jump is also slower than using a
         ; ret.  Let's go for speed...
         ;

         ret

No_Descriptor_Available:
         ; Return 0 in ah and al since the BIOS can't tell us the model and
         ; sub-model bytes.
         xor   ax, ax

Exit:
         ret

GetModelBytes ENDP

;==========================================================================
; IsConvertible()
;
; Returns 1 in AX if the system is an IBM PC Convertible,
;         0 in AX if not.
;
; Checks model byte.
;
;==========================================================================

IsConvertible PROC

			         
         call_M  GetModelBytes		; Get model byte to examine.
					; Are we on an IBM PC Convertible?
         cmp   ah, CONVERTIBLE_MODEL_BYTE
         je    Is_Convertible
	
         xor   ax, ax		; No, this is not an IBM PC Convertible.
         ret

Is_Convertible:
         mov   ax, 1		; Yes, this is an IBM PC Convertible.
         ret

IsConvertible ENDP

;==========================================================================

END


