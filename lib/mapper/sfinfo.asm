
?PLM=1      ; PASCAL Calling convention is DEFAULT
?WIN=0      ; Windows calling convention
?386=0      ; Use 386 code?

public DOSSETFILEINFO
.xlist
include macros.inc
include cmacros.inc
.list
.286p
.MODEL small
.CODE
DOSSETFILEINFO        PROC FAR

arg_2       = dword ptr  8
arg_8       = word ptr  0Eh

        pushall
        mov bx, [bp+arg_8]
        lds si, [bp+arg_2]
        mov dx, [si+8]
        mov cx, [si+0Ah]
        mov ax, 5701h
        int 21h     ; DOS - 2+ - SET FILE'S DATE/TIME
                    ; BX = file handle, CX = time to be set
                    ; DX = date to be set
        jb  short loc_507
        sub ax, ax
loc_507:
        popall
        retf 0Ah

DOSSETFILEINFO ENDP
END

