public DOSDELETE
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSDELETE       PROC FAR PASCAL

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

