
?PLM=1      ; PASCAL Calling convention is DEFAULT
?WIN=0      ; Windows calling convention
?386=0      ; Use 386 code?

public DOSERROR
.xlist
include macros.inc
include cmacros.inc
.list
.286p
.MODEL small
.CODE
DOSERROR        PROC FAR

    pushall
    xor ax, ax
    popall
    ret

DOSERROR ENDP
END

