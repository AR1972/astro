
?PLM=1      ; PASCAL Calling convention is DEFAULT
?WIN=0      ; Windows calling convention
?386=0      ; Use 386 code?

public VIOSETCURPOS
.xlist
include macros.inc
include cmacros.inc
.list
.286p
.MODEL small
.CODE
VIOSETCURPOS       PROC FAR

arg_2 = word ptr  8
arg_4 = word ptr  0Ah

        pushall
        mov bh, 0
        mov ax, [bp+arg_4]
        cmp al, 19h
        jg  short loc_B9
        mov dh, al
        mov ax, [bp+arg_2]
        cmp al, 50h
        jg  short loc_B9
        mov dl, al
        mov ah, 2
        bios_pushall
        int 10h     ; - VIDEO - SET CURSOR POSITION
                    ; DH,DL = row, column (0,0 = upper left)
                    ; BH = page number
        bios_popall
        sub ax, ax
        jmp short loc_BC
        nop
loc_B9:
        mov ax, 2
loc_BC:
        popall
        retf 6

VIOSETCURPOS ENDP
END

