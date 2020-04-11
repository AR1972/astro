
?PLM=1      ; PASCAL Calling convention is DEFAULT
?WIN=0      ; Windows calling convention
?386=0      ; Use 386 code?

public DOSSETFILEMODE
.xlist
include macros.inc
include cmacros.inc
.list
.286p
.MODEL small
.CODE
DOSSETFILEMODE        PROC FAR

arg_4       = word ptr  0Ah
arg_6       = dword ptr  0Ch

        pushall
        lds dx, [bp+arg_6]
        mov cx, [bp+arg_4]
        mov ax, 4301h
        int 21h     ; DOS - 2+ - SET FILE ATTRIBUTES
                    ; DS:DX -> ASCIZ file name
                    ; CX = file attribute bits
        jb  short loc_49F
        xor ax, ax
        jmp short $+2
loc_49F:
        popall
        retf 0Ah

DOSSETFILEMODE ENDP
END

