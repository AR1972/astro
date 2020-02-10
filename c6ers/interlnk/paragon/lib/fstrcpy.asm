;***
;* $Workfile:   fstrcpy.asm  $
;* $Revision:   1.0  $
;*   $Author:   Dave Sewell  $
;*     $Date:   28 Apr 1989 16:50:26  $
;***


    INCLUDE dos.mac

    TEXTSEG

PASCALDEF   fstrcpy

;* NAME
;*	fstrcpy -- Copy a string to a far buffer.
;*
;* SYNOPSIS
;*	extern void pascal fstrcpy(void far *dest, const void *src);
;*	void far *dest;     Pointer to destination area of memory for move
;*	const void *src;    Pointer to source string

	    procent
	    les     di, DWORD PTR [BP + X + DP]
	IF  LDATA
	    push    ds
	    lds     si, DWORD PTR [BP + X]
	ELSE
	    mov     si, WORD PTR [BP + X]
	ENDIF

str_loop:   lodsb
	    stosb
	    or	    al, al
	    jnz     str_loop

	IF  LDATA
	    pop     ds
	ENDIF
	    procret FDP + DP

PASCALEND   fstrcpy

    TEXTEND

    END
