
pushall MACRO
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
ENDM

popall MACRO
        pop bp
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
ENDM

