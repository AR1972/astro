public DOSFINDNEXT
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSFINDNEXT        PROC FAR PASCAL

arg_0       = dword ptr  6
arg_4       = word ptr  0Ah
arg_6       = dword ptr  0Ch
arg_A       = word ptr  10h

        pushall
        mov ax, 81Ch
        mov ds, ax
        assume ds:nothing
        mov si, [bp+arg_A]
        cmp si, 1
        jnz short loc_647
        mov si, 167h

loc_647:                ; CODE XREF: DOSFINDNEXT+17j
        test    word ptr [si], 8000h
        jnz short loc_653
        mov ax, 6
        jmp loc_746
; ---------------------------------------------------------------------------

loc_653:                ; CODE XREF: DOSFINDNEXT+20j
        mov si, [si]
        and si, 7FFFh
        mov ds:6, si
        mov ah, 2Fh
        int 21h     ; DOS - GET DISK TRANSFER AREA ADDRESS
                    ; Return: ES:BX -> DTA
        mov ds:0Ah, bx
        mov word ptr ds:0Ch, es
        mov byte ptr ds:0Eh, 1
        mov dx, ds:6
        mov ah, 1Ah
        int 21h     ; DOS - SET DISK TRANSFER AREA ADDRESS
                    ; DS:DX -> disk transfer buffer
        les di, [bp+arg_0]
        mov ax, es:[di]
        mov ds:0, ax
        mov word ptr es:[di], 0
        cmp ax, 0
        jnz short loc_68C
        jmp loc_744
; ---------------------------------------------------------------------------

loc_68C:                ; CODE XREF: DOSFINDNEXT+5Cj
        mov ax, [bp+arg_4]
        mov ds:8, ax
        les di, [bp+arg_6]
        mov ds:2, di
        mov word ptr ds:4, es

loc_69D:                ; CODE XREF: DOSFINDNEXT+116j
        mov ax, 4F00h
        int 21h     ; DOS - 2+ - FIND NEXT ASCIZ (FINDNEXT)
                    ; [DTA] = data block from
                    ; last AH = 4Eh/4Fh call
        jnb short loc_6A7
        jmp loc_746
; ---------------------------------------------------------------------------

loc_6A7:                ; CODE XREF: DOSFINDNEXT+77j
        sub word ptr ds:8, 23h ; '#'
        jnb short loc_6B4
        mov ax, 8
        jmp loc_746
; ---------------------------------------------------------------------------

loc_6B4:                ; CODE XREF: DOSFINDNEXT+81j
        mov si, ds:6
        les di, ds:2
        mov ax, [si+18h]
        mov es:[di], ax
        mov es:[di+4], ax
        mov es:[di+8], ax
        mov ax, [si+16h]
        mov es:[di+2], ax
        mov es:[di+6], ax
        mov es:[di+0Ah], ax
        mov ax, [si+1Ah]
        mov es:[di+0Ch], ax
        mov es:[di+10h], ax
        mov ax, [si+1Ch]
        mov es:[di+0Eh], ax
        mov es:[di+12h], ax
        test    word ptr es:[di+10h], 1FFh
        jz  short loc_703
        and word ptr es:[di+10h], 0FE00h
        add word ptr es:[di+10h], 200h

loc_703:                ; CODE XREF: DOSFINDNEXT+CAj
        xor ax, ax
        mov al, [si+15h]
        mov es:[di+14h], ax
        mov cx, 0Ch
        mov bx, 0

loc_712:                ; CODE XREF: DOSFINDNEXT+F3j
        mov al, [bx+si+1Eh]
        cmp al, 0
        jz  short loc_720
        mov es:[bx+di+17h], al
        inc bx
        loop    loc_712

loc_720:                ; CODE XREF: DOSFINDNEXT+ECj
        mov byte ptr es:[bx+di+17h], 0
        mov ax, bx
        mov es:[di+16h], al
        add di, bx
        mov ds:2, di
        mov word ptr ds:4, es
        les di, [bp+arg_0]
        inc word ptr es:[di]
        dec word ptr ds:0
        jz  short loc_744
        jmp loc_69D
; ---------------------------------------------------------------------------

loc_744:                ; CODE XREF: DOSFINDNEXT+5Ej
                    ; DOSFINDNEXT+114j
        sub ax, ax

loc_746:                ; CODE XREF: DOSFINDNEXT+25j
                    ; DOSFINDNEXT+79j ...
        push    ax
        mov ax, 81Ch
        mov ds, ax
        cmp byte ptr ds:0Eh, 0
        jz  short loc_760
        mov byte ptr ds:0Eh, 0
        lds dx, ds:0Ah
        assume ds:nothing
        mov ah, 1Ah
        int 21h     ; DOS - SET DISK TRANSFER AREA ADDRESS
                    ; DS:DX -> disk transfer buffer

loc_760:                ; CODE XREF: DOSFINDNEXT+126j
        popall
        retf 0Ch

DOSFINDNEXT ENDP
END

