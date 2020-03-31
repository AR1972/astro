public DOSFINDCLOSE
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSFINDCLOSE        PROC FAR PASCAL

    pushall
    xor ax, ax
    popall
    ret

DOSFINDCLOSE ENDP
END

