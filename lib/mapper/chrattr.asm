
?PLM=1      ; PASCAL Calling convention is DEFAULT
?WIN=0      ; Windows calling convention
?386=0      ; Use 386 code?

public VIOWRTCHARSTRATT
.xlist
include macros.inc
include cmacros.inc
.list
.286p
.MODEL small
.CODE
VIOWRTCHARSTRATT   PROC FAR

arg_2 = dword ptr 8
arg_6 = word  ptr 0Ch
arg_8 = word  ptr 0Eh
arg_A = word  ptr 10h
arg_C = dword ptr 12h

        pushall
        sub bh, bh
        sub ax, ax
        mov dx, [bp+arg_6]
        cmp dl, 50h
        jg  short loc_1D4
        mov ax, [bp+arg_8]
        cmp al, 19h
        jg  short loc_1D4
        mov dh, al
        mov ah, 2
        bios_pushall
        int 10h     ; - VIDEO - SET CURSOR POSITION
                    ; DH,DL = row, column (0,0 = upper left)
                    ; BH = page number
        bios_popall
        lds si, [bp+arg_2]
        mov bl, [si]
        lds si, [bp+arg_C]
        mov di, [bp+arg_A]
loc_184:
        mov al, [si]
        mov ah, 9
        mov cx, 1
        bios_pushall
        int 10h     ; - VIDEO - WRITE ATTRIBUTES/CHARACTERS AT CURSOR POSITION
                    ; AL = character, BH = display page
                    ; BL = attributes of character (alpha modes) or color (graphics modes)
                    ; CX = number of times to write character
        bios_popall
        inc si
        dec di
        inc dl
        cmp dl, 50h
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
        bios_pushall
        int 10h     ; - VIDEO - SET CURSOR POSITION
                    ; DH,DL = row, column (0,0 = upper left)
                    ; BH = page number
        bios_popall
        cmp di, 0
        jnz short loc_184
        sub ax, ax
loc_1D4:
        mov ax, 2
loc_1D7:
        popall
        retf 10h

VIOWRTCHARSTRATT ENDP
END

