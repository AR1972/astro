;***
;* $Workfile:   memcpyf.asm  $
;* $Revision:   1.0  $
;*   $Author:   Dave Sewell  $
;*     $Date:   28 Apr 1989 16:51:08  $
;***


    INCLUDE dos.mac

    TEXTSEG

PASCALDEF   memcpyf

;* NAME
;*	memcpyf -- Copy a block of memory from a far buffer.
;*
;* SYNOPSIS
;*	extern void pascal memcpyf(void *dest, const void far *src, int count);
;*	void *dest;		Pointer to destination area of memory for move
;*	const void far *src;	Pointer to source area of memory
;*	int count;		Number of bytes to move

	    procent
	IF  LDATA
	    les     di, DWORD PTR [BP + X + I + FDP]
	ELSE
	    push    ds
	    pop     es
	    mov     di, WORD PTR [BP + X + I + FDP]
	ENDIF
	    push    ds
	    lds     si, DWORD PTR [BP + X + I]
	    mov     cx, [BP + X]
	    jcxz    memcpyf_ret 		;Ignore count of zero

	rep movs    BYTE PTR ES:[DI], BYTE PTR DS:[SI]

memcpyf_ret:
	    pop     ds
	    procret DP+FDP+I

PASCALEND  memcpyf

    TEXTEND

    END
