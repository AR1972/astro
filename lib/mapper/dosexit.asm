public DOSEXIT
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSEXIT PROC FAR PASCAL

arg_0 = word ptr 6
arg_2 = word ptr 8

        pushall
        mov ax, [bp+arg_2]
        cmp ax, 1
        jg  Done
        mov ax, [bp+arg_0]
        mov ah, 4Ch
        int 21h     ; DOS - 2+ - QUIT WITH EXIT CODE (EXIT)
                    ; AL = exit code
        xor ax, ax
Done:
        popall
        retf 4

DOSEXIT ENDP
END

