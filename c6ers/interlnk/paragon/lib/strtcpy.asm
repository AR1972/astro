;***
;* $Workfile:   strtcpy.asm  $
;* $Revision:   1.0  $
;*   $Author:   Dave Sewell  $
;*     $Date:   28 Apr 1989 16:52:48  $
;*
;* Assembly language version of "strtcpy".
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
;***

	INCLUDE dos.mac

			TEXTSEG

;*  void pascal strtcpy(dest, src, count)
;*  register char *dest;
;*  register char *src;
;*  register unsigned count;
;*  {
;*	if (count) {
;*			count--;		    /* Reserve one byte for trailing null. */
;*		while (count-- && *src) *dest++ = *src++;
;*		*dest = '\0';
;*	}
;*  }
;*
;*  extern void pascal strtcpy(char *dest, char *src, unsigned count);

dest	EQU	X + I + DP
src	EQU	X + I
count	EQU	X

PASCALDEF   strtcpy

	    procent
    IF	    LDATA
	    PUSH    DS
	    LES     DI, [BP + dest]
	    LDS     SI, [BP + src]
    ELSE
	    PUSH    DS
	    POP     ES
	    MOV     DI, [BP + dest]
	    MOV     SI, [BP + src]
    ENDIF
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
	IF  LDATA
	    POP     DS
	ENDIF
	    procret DP + DP + I

PASCALEND   strtcpy


	TEXTEND

	END
