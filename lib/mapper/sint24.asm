public SET_INT24_VECTOR
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
SET_INT24_VECTOR        PROC FAR PASCAL

    pushall
    xor ax, ax
    popall
    ret

SET_INT24_VECTOR ENDP
END

