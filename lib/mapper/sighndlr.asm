public DOSSETSIGHANDLER
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSSETSIGHANDLER        PROC FAR PASCAL

    pushall
    xor ax, ax
    popall
    ret

DOSSETSIGHANDLER ENDP
END

