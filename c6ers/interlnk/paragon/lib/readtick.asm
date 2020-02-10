;***
;* $Workfile:   readtick.asm  $
;* $Revision:   1.2  $
;*   $Author:   Dave Sewell  $
;*     $Date:   10 Sep 1990 13:34:30  $
;*
;* "read_ticks" routine to read the 16-bit timer.
;***

%		.MODEL memmodel, language

TIMER_0 	EQU	40H	    ;8253 counter 0 port
TIMER_CTL	EQU	43H	    ;8253 control port
TIMER_SET	EQU	00110100B   ;8253 Counter 0, set LOB/HOB, mode 2, binary
TIMER_0_LATCH	EQU	00H	    ;8253 cmd to save channel 0 current count

    EXTRN   NULL_PROC:FAR
                IF	@CodeSize
		            .CODE	PARAGON_TEXT
                ELSE
	            .CODE
                ENDIF

loc_null_proc	PROC	NEAR

		ret

loc_null_proc	ENDP

fread_ticks PROC    FAR

;*	extern	unsigned int far pascal fread_ticks(void);

	    call    read_ticks
	    ret

fread_ticks ENDP

read_ticks  PROC    NEAR

;* NAME
;*	read_ticks -- Return high resolution timer information.
;*
;* SYNOPSIS
;*	time = read_ticks();
;*
;*	unsigned int time;	Current number of ticks (0.838 microseconds).
;*
;* DESCRIPTION
;*	Routine current value in 8253 timer.
;*
;*	NOTE:  values count DOWN instead of up, as they do in "ticks()".
;*
;*	extern	unsigned int near read_ticks(void);

	    PUSHF
	    CLI
	    MOV     AL, TIMER_0_LATCH
	    OUT     TIMER_CTL, AL	;Latch current count in 8253
	    CALL    NULL_PROC
	    IN	    AL, TIMER_0 	;Get low order byte
	    MOV     AH, AL		;Save it in AH
	    CALL    NULL_PROC    	;Insure 5 clocks for 8253 recovery time
	    IN	    AL, TIMER_0 	;Get high order byte
	    POPF
	    XCHG    AH, AL		;Get high, low bytes in right order
	    RET 			;Return value in AX

read_ticks  ENDP

	    END
