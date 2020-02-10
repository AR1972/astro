;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1983 - 1991
; *                      All Rights Reserved.
; */
; ========================================================

COMMENT #

	REBOOT.ASM

	JH - 12/01/89

END COMMENT #

;-----------------------------------------------------------------------------;
;	K E Y B O A R D   S C A N   C O D E S				      ;
;-----------------------------------------------------------------------------;

KB_INTERCEPT	EQU	4fh

DEL_KEY		EQU	53h
ALT_SHIFT	EQU	08h
CTL_SHIFT	EQU	04h

WARM_BOOT_CODE	EQU	1234h	

MIN_DELAY	EQU	1
MAX_DELAY	EQU	255

;-----------------------------------------------------------------------------;
;             SYSTEM   BOARD   I/O   PORTS                     		      ;
;-----------------------------------------------------------------------------;

;-----------------------------------------------------------------------------;
;   The system bus control port.  It is responsible for generating a          ;
;   "fast" shutdown pulse.                                                    ;
;-----------------------------------------------------------------------------;
SYSTEM_CONTROL_PORT		EQU	92h
	SYSTEM_SHUTDOWN_BIT	EQU	00000001b

;-----------------------------------------------------------------------------;
;   The 8042 microcontroller command port.                                    ;
;-----------------------------------------------------------------------------;
PARALLEL_PORT_A			EQU	60h		; Programmable Interface Adapter of IBM system board used for controlling
	WRITE_OUTPUT_PORT_CMD	EQU	0D1h		; Write to controller output port
	GATE_A20_CMD		EQU	0DFh		; Gate A20 command
	RESET_SYSTEM_CMD	EQU	0FEh		; Shutdown system command

;-----------------------------------------------------------------------------;
;   The 8042 microcontroller status port.                                     ;
;-----------------------------------------------------------------------------;
STAT_CMD_PORT			EQU	64h		; 8042 microcontroller status port.
	INT_BUFF_FULL_STAT	EQU	00000010b	; Key pressed status

;-----------------------------------------------------------------------------;
;   MACRO:   IO_DELAY                                                         ;
;            Insert a delay to allow back-to-back I/O on slow peripherals.    ;
;            Jumps are usually used.  There are four in this macro to 	      ;
;            guarantee working on a 30 MHz+ 486.                              ;
;-----------------------------------------------------------------------------;
IO_DELAY   		MACRO
	jmp		$+2
	jmp		$+2
	jmp		$+2
	jmp		$+2
			ENDM

;-----------------------------------------------------------------------------;
;	BIOS DATA AREA LOCATED AT 40:00
;-----------------------------------------------------------------------------;

ROM_DATA SEGMENT AT 040h

	org	17h
KB_FLAG		LABEL BYTE


	org	072h
WarmBootFlag	LABEL WORD

ROM_DATA ENDS

;-----------------------------------------------------------------------------;
;	CPU POWER-ON STARTUP LOCATION AT ffff:00
;-----------------------------------------------------------------------------;

ROM_BIOS SEGMENT AT 0ffffh
	org	0

PowerOnReset	LABEL FAR

ROM_BIOS ENDS

;-----------------------------------------------------------------------------;

_text	segment byte public 'code'
	assume	cs:_TEXT
	assume	ds:nothing
	assume	es:nothing
	assume	ss:nothing

;-----------------------------------------------------------------------------;
;   Procedure Name:   ShutdownSystem                                          ;
;           Author:   WJK
;             Date:   3/29/88                                                 ;
;      Description:   This procedure tries to shut down a ISA or MCA system   ;
;                     using a combination of hardware and software methods.   ;
;                                                                             ;
;                     The hardware methods are nearly foolproof but require   ;
;                     a keyboard controller or a parallel port that has       ;
;                     main-processor-reset hardware capabilities.  Either of  ;
;                     the hardware methods cause a reset signal to be         ;
;                     generated, thereby resetting the CPU, and in some       ;
;                     system architectures (MCA specifically) all the add-in  ;
;                     hardware in a system as well.  All AT class, new XT     ;
;                     class, and all MCA class systems tested have either     ;
;                     one or both of these hardware capabilities.  The        ;
;                     hardware methods work extremely well for systems that   ;
;                     have 286, 386, 486 CPUs because they can effect a       ;
;                     shutdown independent of processor mode.  The only       ;
;                     exception would be a 386/486 running in virtual mode    ;
;                     with software that trapped port accesses to these ports ;
;                     and prevented the data from being output.               ;
;                                                                             ;
;                     The hardware methods are tried first.  Failing these    ;
;                     shutdown methods, the proc reverts to software          ;
;                     techniques.  The first technique is simply jumping to   ;
;                     processors power-on entry address.  The second is       ;
;                     generating an INT 19h.  Because of the possibility      ;
;                     of DOS/TSR trapping INT 19h & not restoring all the     ;
;                     necessary int vectors properly, the second method       ;
;                     does not work well with all versions of DOS or in       ;
;                     all configurations.  It's included but never executed.  ;
;                     The first software method is used and has worked well   ;
;                     in the systems tested.                                  ;
;                                                                             ;
;           Passed:   None                                                    ;
;         Returned:   Normally, there is NO return from this procedure.       ;
;                                                                             ;
;            Calls:   XmitKybCntrlCmd                                         ;
;                                                                             ;
;        Registers:   ax  bx  cx                                              ;
;                                                                             ;
;        Modified/                                                            ;
;       Referenced:   ROM_BIOS_RAM_AREA:WarmBootFlag                          ;
;-----------------------------------------------------------------------------;
	public	_reboot
_reboot proc	near

	;**   Disable interrupts while shutting down.
;       cli                         ; restore this line if h/w mechanism used

	;**   Set the warm-boot-flag in the ROM BIOS communications area.
	mov	ax, seg WarmBootFlag
	mov	ds, ax
	assume	DS:ROM_DATA

	mov	WarmBootFlag, WARM_BOOT_CODE

COMMENT #
        The hardware reboot mechanisms have been disabled for the time
        being to avoid problems with random hardware with non-standard
        reboot needs.


	;**   Gate A20 on.   -------------------------------------------------;
	;   This is necessary because some old flaky BIOS'/controllers don't  ;
	;   turn on A20 soon enough after a reset generated by the keyboard   ;
	;   controller.  This causes the BIOS to erroneously determine that   ;
	;   there is no memory above 1 Mbyte.  So it's done FIRST before the  ;
	;   shutdown.                                                         ;
	;---------------------------------------------------------------------;
	mov	al, WRITE_OUTPUT_PORT_CMD
	mov	bx, MIN_DELAY
	call	NEAR PTR XmitKybCntrlCmd

	mov	al, GATE_A20_CMD
	out	PARALLEL_PORT_A, al

		; Try shutting down the processor by using the kybd controller.

	mov	al, RESET_SYSTEM_CMD
	mov	bx, MAX_DELAY
	call	NEAR PTR XmitKybCntrlCmd

		; The keyboard controller method didn't work.
		; Try shutting down the system by using a PS2 "fast" shutdown.

	in	al, SYSTEM_CONTROL_PORT
	and	al, NOT (SYSTEM_SHUTDOWN_BIT)
	IO_DELAY
	out	SYSTEM_CONTROL_PORT, al
	or	al, SYSTEM_SHUTDOWN_BIT
	IO_DELAY
	out	SYSTEM_CONTROL_PORT, al

		; Wait while the hardware emits the reset pulse.
	mov	bx, MAX_DELAY
	call	NEAR PTR Delay

	;---------------------------------------------------------------------;
	;   Neither of the h/w reset techniques worked.			      ;
	;---------------------------------------------------------------------;

        FDisk relies on the software reboot mechanisms only now.
END COMMENT #
        
	mov	AX,3515h
	int	21h			; Get int 15h vector in ES:BX
	mov	AX,ES			; AX == Segment
	or	AX,BX			; Is this a NULL ptr
	jz	WarmBoot		; If zero we can't do an int 15h

DoInt15:
	mov	ax, seg WarmBootFlag
	mov	ds, ax
	assume	DS:ROM_DATA

	mov	KB_FLAG,ALT_SHIFT OR CTL_SHIFT
	mov	AX,(KB_INTERCEPT SHL 8) OR DEL_KEY
	int	15h			; Put Ctrl/Alt/Del into key buffer

WarmBoot:
		; Jump to the processor power-on address FFFF:0000h

	jmp	PowerOnReset

_reboot 	ENDP


COMMENT #

        These helper routines also pulled because of hardware compatibility
        problems.
page
;-----------------------------------------------------------------------------;
;   Procedure Name:   XmitKybCntrlCmd                                         ;
;           Author:   W J K
;             Date:   7/12/88                                                 ;
;      Description:   This procedures sends the command in al to the          ;
;                     keyboard microcontroller.                               ;
;                                                                             ;
;           Passed:   al = Keyboard controller command.                       ;
;                     bx = End of keyboard controller command delay constant. ;
;         Returned:   None.                                                   ;
;                                                                             ;
;        Called By:   ShutdownSystem                                          ;
;            Calls:   Delay                                                   ;
;                                                                             ;
;        Registers:   ax  bx  cx                                              ;
;                                                                             ;
;        Modified/                                                            ;
;       Referenced:   Keyboard controller port.                               ;
;-----------------------------------------------------------------------------;

XmitKybCntrlCmd	proc	NEAR

		; Save the command & set max loop count for the wait loop.

	mov	ah, al		; (ah) = kybd controller cmd
	mov	CX,0ffffh
	
xkcc01:		; Wait for keyboard controller to indicate a buffer empty status.
	jcxz	EndLoop1
	in	al, STAT_CMD_PORT
	test	al, INT_BUFF_FULL_STAT
	loopnz	xkcc01

EndLoop1:
		; Restore the command & output the keyboard controller command.

	mov	al, ah		; (al) = kybd controller cmd
	out	STAT_CMD_PORT, al

		; Wait here while the keyboard controller processes the command.

	call	Delay		; (bx) = requested no. of delays
	ret

XmitKybCntrlCmd	endp

;-----------------------------------------------------------------------------;
;   Procedure Name:   Delay                                                   ;
;           Author:   W J K
;             Date:   7/12/88                                                 ;
;      Description:   This procedure waits for a number of CPU cycles.        ;
;                     It's used to wait for the keyboard controller           ;
;                     while it responds to commands that are sent to it.      ;
;                                                                             ;
;           Passed:   bx = End of keyboard controller command delay constant. ;
;         Returned:   None.                                                   ;
;                                                                             ;
;        Called By:   ShutdownSystem                                          ;
;                     XmitKybdCntrlCmd                                        ;
;            Calls:   Nothing                                                 ;
;                                                                             ;
;        Registers:   bx  cx                                                  ;
;                                                                             ;
;        Modified/                                                            ;
;       Referenced:   Nothing                                                 ;
;-----------------------------------------------------------------------------;
Delay	proc	NEAR

		; Exit if no delay is requested.
	or	bx, bx
	jz	del02

		; Delay a fixed large number of CPU cycles.
	xor	cx, cx
del01:	loop	del01

		; Repeat till the number of delays are done.
	dec	bx
	jnz	del01
del02:	ret

Delay		endp

END COMMENT #
; ========================================================

_text	ends

	END

; ========================================================
