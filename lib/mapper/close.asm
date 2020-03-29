public DOSCLOSE
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSCLOSE        PROC FAR PASCAL

arg_0       = word ptr  6

        pushall
        mov bx, [bp+arg_0]
        mov ax, bx
        neg ax
        jns short loc_532
        mov ax, 3E00h
        int 21h     ; DOS - 2+ - CLOSE A FILE WITH HANDLE
                    ; BX = file handle
        jb  short loc_534
loc_532:
        sub ax, ax
loc_534:
        popall
        retf 2

DOSCLOSE ENDP
END

