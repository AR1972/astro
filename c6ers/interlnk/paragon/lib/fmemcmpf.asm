;***
;* $Workfile:   fmemcmpf.asm  $
;* $Revision:   1.0  $
;*   $Author:   Dave Sewell  $
;*     $Date:   28 Apr 1989 16:50:04  $
;*
;* Far pointer version of "memcmp".
;***

    INCLUDE DOS.MAC

    TEXTSEG


;* NAME
;*	fcmpmemf -- Compare two areas of memory
;*
;* SYNOPSIS
;*	extern int pascal fmemcmpf(byte far *p1, byte far *p2, unsigned int count);
;*
;*	int  result;		Result of comparison
;*	byte far *p1;		Pointer to first area of memory
;*	byte far *p2;		Pointer to second area of memory
;*	unsigned count; 	Number of bytes to compare

PASCALDEF   fmemcmpf

	    procent
	    PUSH    DS
	    LES     DI, DWORD PTR [BP+X+I+FDP]
	    LDS     SI, DWORD PTR [BP+X+I]

;*** Now ES:DI points to first area, DS:SI points to second area

	    MOV     CX, [BP+X]		;And CX gets the count

	    XOR AX, AX			;Preset return value to zero
	    CLD 			;Insure forward compare
	    JCXZ    cmpmem_ret		;Return a zero if count is zero

    REPE    CMPS    BYTE PTR DS:[SI], ES:[DI]

	    JE	    cmpmem_ret		;All bytes were equal

	    INC     AX			;*** Does not affect carry flag!
	    JB	    cmpmem_ret		;Carry means DI (string1) was greater

	    DEC     AX			;Not equal or carry -
	    DEC     AX			;    DI (string1) was smaller

cmpmem_ret:
	    POP     DS
	    procret FDP + FDP + I

PASCALEND   fmemcmpf


    TEXTEND

    END
