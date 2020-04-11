
?PLM=1      ; PASCAL Calling convention is DEFAULT
?WIN=0      ; Windows calling convention
?386=0      ; Use 386 code?

public DOSDELETE
.xlist
include macros.inc
include cmacros.inc
.list
.286p
.MODEL small
.CODE
DOSDELETE       PROC FAR

arg_4       = dword ptr  0Ah

        pushall
        lds dx, [bp+arg_4]
        mov ah, 41h
        int 21h     ; DOS - 2+ - DELETE A FILE (UNLINK)
                    ; DS:DX -> ASCIZ pathname of file to delete (no wildcards allowed)
        jb  short loc_1CD
        sub ax, ax
loc_1CD:
        popall
        retf 8

DOSDELETE ENDP
END

