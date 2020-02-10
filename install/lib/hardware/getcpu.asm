;===========================================================================
COMMENT #

 GETCPU.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential


   _A386machine - return type of processor (386 vs. 8088/86/286).
       This routine relies on Intel-approved code that takes advantage
       of the documented behavior of the high nibble of the flag word
       in the REAL MODE of the various processors.  The MSB (bit 15)
       is always a one on the 8086 and 8088 and a zero on the 286 and
       386.  Bit 14 (NT flag) and bits 13/12 (IOPL bit field) are
       always zero on the 286, but can be set on the 386.

       For future compatibility of this test, it is strongly recommended
       that this specific instruction sequence be used.  The exit codes
       can of course be changed to fit a particular need.

       CALLABLE FROM REAL MODE ONLY - near ROUTINE

       this routine was stolen from EMM/386 source code

   int GetCpuType( void );

   ENTRY:  (none)
   EXIT:   ax = 0 == 8086, 1 == 80286, 2 == 80386
           ax = 0 otherwise
   USED:   none
   STACK:  6 bytes

END COMMENT #
;===========================================================================

INCLUDE model.inc

.CODE

;------------------------------------------------------------------------------

GetCpuType PROC

         pushf                            ; save entry flags

         xor         ax, ax               ; 0000 into AX
         push        ax
         popf                             ; try to put that in the flags
         pushf
         pop         ax                   ; look at what really went into flags

TestFor8088:
	 test	     ax, 08000h 	  ; Q: was high bit set ?
	 jz	     short TestFor80286	  ; N: Not an 8086/8088
	 xor	     AX,AX		  ; Signal 8086 processor
	 jmp	     SHORT ExitCpuType

TestFor80286:
	 mov	     ax, 07000h 	  ;   N: try to set the NT/IOPL bits
         push        ax
         popf                             ;      ... in the flags
         sti                              ; (for VDMM/IOPL0)
         pushf
         pop         ax                   ; look at actual flags
         test        ax, 07000h           ; Q: any high bits set ?
	 jnz	     Is80386		  ; Y: must be 386 machine
	 mov	     AX,1		  ; else is 80286
	 jmp	     SHORT ExitCpuType	  ; Must be an 80386
Is80386:
	 mov	     ax,2

ExitCpuType:
         popf                             ; restore flags

	 ret

GetCpuType ENDP


END
