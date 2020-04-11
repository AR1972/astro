
?PLM=1      ; PASCAL Calling convention is DEFAULT
?WIN=0      ; Windows calling convention
?386=0      ; Use 386 code?

public VIOSCROLLUP
.xlist
include macros.inc
include cmacros.inc
.list
.286p
.MODEL small
.CODE
VIOSCROLLUP        PROC FAR

arg_2 = dword ptr 8
arg_6 = word  ptr 0Ch
arg_8 = word  ptr 0Eh
arg_A = word  ptr 10h
arg_C = word  ptr 12h
arg_E = word  ptr 14h

        pushall
        mov bx, [bp+arg_6]
        cmp bl, 19h
        jg  short loc_130
        mov al, bl
        jmp short loc_F0
        mov al, 0
loc_F0:
        mov ah, 6
        mov bx, [bp+arg_8]
        cmp bl, 50h
        jg  short loc_130
        mov dl, bl
        mov bx, [bp+arg_A]
        cmp bl, 19h
        jg  short loc_130
        mov dh, bl
        mov bx, [bp+arg_C]
        mov cl, bl
        mov bx, [bp+arg_E]
        mov ch, bl
        lds si, [bp+arg_2]
        mov bx, [si]
        mov bh, bl
        bios_pushall
        int 10h     ; - VIDEO - SCROLL PAGE UP
                    ; AL = number of lines to scroll window (0 = blank whole window)
                    ; BH = attributes to be used on blanked lines
                    ; CH,CL = row,column of upper left corner of window to scroll
                    ; DH,DL = row,column of lower right corner of window
        bios_popall
        sub ax, ax
        jmp short loc_133
loc_130:
        mov ax, 2
loc_133:
        popall
        retf 10h

VIOSCROLLUP ENDP
END

