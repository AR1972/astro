public DOSQCURDISK
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSQCURDISK        PROC FAR PASCAL

    pushall
    xor ax, ax
    popall
    ret

DOSQCURDISK ENDP
END

