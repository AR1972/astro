;***
;* $Workfile:   strtcpyf.asm  $
;* $Revision:   1.0  $
;*   $Author:   Dave Sewell  $
;*     $Date:   28 Apr 1989 16:52:52  $
;*
;* Copy a string and truncate (force trailing null) if needed.	This function
;* operates just like "strcpy", except that a buffer length is passed also,
;* and the function insures that the string is truncated (if necessary) to
;* fit in the buffer.  There will ALWAYS be a trailing null in the destination
;* buffer (unless a zero is passed as the buffer length).
;*
;* NOTE:  this function is NOT identical to the "strtcpy" function in the
;* LDS C-Plus library.	The LDS version null pads the entire buffer if the
;* string is shorter than the buffer length.  This version copies one and
;* only one null into the destination buffer, and leaves the rest of the
;* buffer (if any) unchanged.
;*
;* This routine is the same as strtcpy() execpt that the src is always a far
;* pointer.
;*
;***

	INCLUDE dos.mac

			TEXTSEG

;*  extern void pascal strtcpy(char *dest, char far *src, unsigned count);

dest	EQU	X + I + FDP
src	EQU	X + I
count	EQU	X

PASCALDEF   strtcpyf

	    procent

    IF	    LDATA
	    LES     DI, [BP + dest]
    ELSE
	    PUSH    DS
	    POP     ES
	    MOV     DI, [BP + dest]
    ENDIF

	    PUSH    DS
	    LDS     SI, [BP + src]

	    MOV     CX, [BP + count]
	    JCXZ    cpy_done

	    DEC     CX

cpy_loop:   JCXZ    plant_null

	    LODSB
	    STOSB
	    OR	    AL, AL
	    JZ	    cpy_done

	    LOOP    cpy_loop

plant_null: XOR     AL, AL
	    MOV     ES:[DI], AL

cpy_done:

	    POP     DS

	    procret DP + FDP + I

PASCALEND   strtcpyf


	TEXTEND

	END
