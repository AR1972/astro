
?PLM=1      ; PASCAL Calling convention is DEFAULT
?WIN=0      ; Windows calling convention
?386=0      ; Use 386 code?

public DOSQFILEMODE
.xlist
include macros.inc
include cmacros.inc
.list
.286p
.MODEL small
.CODE
DOSQFILEMODE        PROC FAR

arg_4       = dword ptr  0Ah
arg_8       = dword ptr  0Eh

        pushall
        lds dx, [bp+arg_8]
        mov ax, 4300h
        int 21h     ; DOS - 2+ - GET FILE ATTRIBUTES
                    ; DS:DX -> ASCIZ file name or directory
                    ; name without trailing slash
        jb  short loc_470
        lds si, [bp+arg_4]
        mov [si], cx
        sub ax, ax
        jmp short loc_473
loc_470:
        mov ax, 2
loc_473:
        popall
        retf 0Ch

DOSQFILEMODE ENDP
END

