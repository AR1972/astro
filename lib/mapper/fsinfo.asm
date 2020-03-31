public DOSQFSINFO
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSQFSINFO        PROC FAR PASCAL

    pushall
    xor ax, ax
    popall
    ret

DOSQFSINFO ENDP
END

