
?PLM=1      ; PASCAL Calling convention is DEFAULT
?WIN=0      ; Windows calling convention
?386=0      ; Use 386 code?

public SET_INT24_VECTOR
.xlist
include macros.inc
include cmacros.inc
.list
.286p
.MODEL small
.CODE
SET_INT24_VECTOR        PROC FAR

    pushall
    xor ax, ax
    popall
    ret

SET_INT24_VECTOR ENDP
END

