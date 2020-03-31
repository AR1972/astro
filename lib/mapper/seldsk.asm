public DOSSELECTDISK
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSSELECTDISK        PROC FAR PASCAL

    pushall
    xor ax, ax
    popall
    ret

DOSSELECTDISK ENDP
END

