public DOSEXIT

.286p
.MODEL small
.CODE
DOSEXIT PROC FAR PASCAL

arg_0 = word ptr 6
arg_2 = word ptr 8

        push    bp
        mov bp, sp
        push    bx
        push    cx
        push    dx
        push    si
        push    di
        push    ds
        push    es
        push    ss
        push    bp
        mov ax, [bp+arg_2]
        cmp ax, 1
        jg  Done
        mov ax, [bp+arg_0]
        mov ah, 4Ch
        int 21h     ; DOS - 2+ - QUIT WITH EXIT CODE (EXIT)
                    ; AL = exit code
        xor ax, ax
Done: pop   bp
        pop ss
        pop es
        pop ds
        pop di
        pop si
        pop dx
        pop cx
        pop bx
        mov sp, bp
        pop bp
        retf 4

DOSEXIT ENDP
END