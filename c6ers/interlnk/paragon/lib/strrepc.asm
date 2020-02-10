;***
;* $Workfile:   strrepc.asm  $
;* $Revision:   1.0  $
;*   $Author:   Dave Sewell  $
;*     $Date:   28 Apr 1989 16:52:44  $
;*
;* Replace all occurences of a particular character within a string with
;* another character.
;***

INCLUDE dos.mac

	TEXTSEG

PASCALDEF   strrepc
;***
;* strrepc -- Replace occurrences of a character within a string.
;*
;* extern  void pascal strrepc(char *str, int c, int repc);
;*
;* char *str;	    pointer to target string
;* int	c;	    character in the string to replace
;* int	repc;	    replacement character
;*
;***

str	    EQU     X + I + I
c	    EQU     X + I
repc	    EQU     X

	    procent
    IF	    LDATA
	    PUSH    DS
	    LDS     SI, [BP + str]
    ELSE
	    MOV     SI, [BP + str]
    ENDIF
	    MOV     AH, BYTE PTR [BP + c]
	    MOV     BL, BYTE PTR [BP + repc]

RepLoop:    LODS    BYTE PTR DS:[SI]
	    OR	    AL, AL
	    JZ	    RepDone

	    CMP     AL, AH
	    JNE     RepLoop

	    MOV     [SI - 1], BL    ;Replace target character
	    JMP     RepLoop

RepDone:
    IF	LDATA
	    POP     DS
    ENDIF
	    procret DP + I + I

PASCALEND   strrepc

	TEXTEND

	END
