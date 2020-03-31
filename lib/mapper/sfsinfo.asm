public DOSSETFSINFO
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSSETFSINFO        PROC FAR PASCAL

    pushall
    xor ax, ax
    popall
    ret

DOSSETFSINFO ENDP
END

