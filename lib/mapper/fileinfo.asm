public DOSQFILEINFO
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSQFILEINFO        PROC FAR PASCAL

    pushall
    xor ax, ax
    popall
    ret

DOSQFILEINFO ENDP
END

