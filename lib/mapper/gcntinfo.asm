public DOSGETCTRYINFO
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSGETCTRYINFO        PROC FAR PASCAL

    pushall
    xor ax, ax
    popall
    ret

DOSGETCTRYINFO ENDP
END

