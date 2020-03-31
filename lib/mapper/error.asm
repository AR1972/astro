public DOSERROR
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSERROR        PROC FAR PASCAL

    pushall
    xor ax, ax
    popall
    ret

DOSERROR ENDP
END

