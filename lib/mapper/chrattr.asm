public VIOWRTCHARSTRATT

.286p
.MODEL small
.CODE
VIOWRTCHARSTRATT PROC FAR PASCAL

arg_2 = dword ptr  8
arg_6 = word ptr  0Ch
arg_8 = word ptr  0Eh
arg_A = word ptr  10h
arg_C = dword ptr  12h

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
        sub bh, bh
        sub ax, ax
        mov dx, [bp+arg_6]
        cmp dl, 50h ; 'P'
        jg  short loc_1D4
        mov ax, [bp+arg_8]
        cmp al, 19h
        jg  short loc_1D4
        mov dh, al
        mov ah, 2
        push    bx
        push    cx
        push    dx
        push    si
        push    di
        push    ds
        push    es
        push    ss
        push    bp
        int 10h     ; - VIDEO - SET CURSOR POSITION
                    ; DH,DL = row, column (0,0 = upper left)
                    ; BH = page number
        pop bp
        pop ss
        pop es
        pop ds
        pop di
        pop si
        pop dx
        pop cx
        pop bx
        lds si, [bp+arg_2]
        mov bl, [si]
        lds si, [bp+arg_C]
        mov di, [bp+arg_A]
loc_184:
        mov al, [si]
        mov ah, 9
        mov cx, 1
        push    bx
        push    cx
        push    dx
        push    si
        push    di
        push    ds
        push    es
        push    ss
        push    bp
        int 10h     ; - VIDEO - WRITE ATTRIBUTES/CHARACTERS AT CURSOR POSITION
                    ; AL = character, BH = display page
                    ; BL = attributes of character (alpha modes) or color (graphics modes)
                    ; CX = number of times to write character
        pop bp
        pop ss
        pop es
        pop ds
        pop di
        pop si
        pop dx
        pop cx
        pop bx
        inc si
        dec di
        inc dl
        cmp dl, 50h ; 'P'
        jnz short loc_1B7
        inc dh
        mov dl, 0
        cmp dh, 19h
        jnz short loc_1B7
        mov ax, 1
        jmp short loc_1D7
        nop
loc_1B7:
        mov ah, 2
        push    bx
        push    cx
        push    dx
        push    si
        push    di
        push    ds
        push    es
        push    ss
        push    bp
        int 10h     ; - VIDEO - SET CURSOR POSITION
                    ; DH,DL = row, column (0,0 = upper left)
                    ; BH = page number
        pop bp
        pop ss
        pop es
        pop ds
        pop di
        pop si
        pop dx
        pop cx
        pop bx
        cmp di, 0
        jnz short loc_184
        sub ax, ax
loc_1D4:
        mov ax, 2
loc_1D7:
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
        retf 10h

VIOWRTCHARSTRATT ENDP
END