public DOSALLOCSEG
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSALLOCSEG        PROC FAR PASCAL

arg_2       = dword ptr  8
arg_6       = word ptr  0Ch

        pushall
        mov bx, [bp+arg_6]
        test    bx, 0Fh
        jz  short loc_55E
        and bx, 0FFF0h
        add bx, 10h
loc_55E:
        cmp bx, 0
        jz  short loc_56E
        shr bx, 1
        shr bx, 1
        shr bx, 1
        shr bx, 1
        jmp short loc_571
        nop
loc_56E:
        mov bx, 1000h
loc_571:
        mov ah, 48h
        int 21h     ; DOS - 2+ - ALLOCATE MEMORY
                    ; BX = number of 16-byte paragraphs desired
        jb  short loc_57E
        lds si, [bp+arg_2]
        mov [si], ax
        sub ax, ax
loc_57E:
        popall
        retf 8

DOSALLOCSEG ENDP
END

