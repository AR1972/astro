public DOSFINDFIRST
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSFINDFIRST        PROC FAR PASCAL

arg_4       = dword ptr  0Ah
arg_8       = word ptr  0Eh
arg_A       = dword ptr  10h
arg_E       = word ptr  14h
arg_10      = dword ptr  16h
arg_14      = dword ptr  1Ah

        pushall
        mov ax, 81Ch
        mov ds, ax
        assume ds:nothing
        mov si, 167h
        mov cx, 8
        les di, [bp+arg_10]
        mov ax, es:[di]
        cmp ax, 0FFFFh
        jz  short loc_35
        cmp ax, 1
        jz  short loc_4A
        mov si, ax
        test    word ptr [si], 8000h
        jnz short loc_4A
        mov ax, 6
        jmp loc_15B
loc_35:
        add si, 2
        dec cx

loc_39:
        test    word ptr [si], 8000h
        jz  short loc_4A
        add si, 2
        loop    loc_39

loc_44:
        mov ax, 4
        jmp loc_15B
loc_4A:

        mov ax, [si]
        or  word ptr [si], 8000h
        and ax, 7FFFh
        mov ds:6, ax
        les di, [bp+arg_10]
        mov es:[di], si
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
        les di, [bp+arg_4]
        mov ax, es:[di]
        mov ds:0, ax
        mov word ptr es:[di], 0
        cmp ax, 0
        jnz short loc_8B
        jmp loc_159
loc_8B:
        lds dx, [bp+arg_14]
        assume ds:nothing
        mov cx, [bp+arg_E]
        mov ax, 4E00h
        int 21h     ; DOS - 2+ - FIND FIRST ASCIZ (FINDFIRST)
                    ; CX = search attributes
                    ; DS:DX -> ASCIZ filespec
                    ; (drive, path, and wildcards allowed)
        jnb short loc_9B
        jmp loc_15B
loc_9B:
        mov ax, 81Ch
        mov ds, ax
        assume ds:nothing
        mov ax, [bp+arg_8]
        mov ds:8, ax
        les di, [bp+arg_A]
        mov ds:2, di
        mov word ptr ds:4, es
loc_B1:
        sub word ptr ds:8, 23h ; '#'
        jnb short loc_BE
        mov ax, 8
        jmp loc_15B
loc_BE:
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
        jz  short loc_10D
        and word ptr es:[di+10h], 0FE00h
        add word ptr es:[di+10h], 200h
loc_10D:
        xor ax, ax
        mov al, [si+15h]
        mov es:[di+14h], ax
        mov cx, 0Ch
        mov bx, 0
loc_11C:
        mov al, [bx+si+1Eh]
        cmp al, 0
        jz  short loc_12A
        mov es:[bx+di+17h], al
        inc bx
        loop    loc_11C
loc_12A:
        mov byte ptr es:[bx+di+17h], 0
        mov ax, bx
        mov es:[di+16h], al
        add di, bx
        mov ds:2, di
        mov word ptr ds:4, es
        les di, [bp+arg_4]
        inc word ptr es:[di]
        dec word ptr ds:0
        jz  short loc_159
        mov ax, 4F00h
        int 21h     ; DOS - 2+ - FIND NEXT ASCIZ (FINDNEXT)
                    ; [DTA] = data block from
                    ; last AH = 4Eh/4Fh call
        jb  short loc_15B
        les di, ds:2
        jmp loc_B1
loc_159:
        sub ax, ax
loc_15B:
        push    ax
        mov ax, 81Ch
        mov ds, ax
        cmp byte ptr ds:0Eh, 0
        jz  short loc_175
        mov byte ptr ds:0Eh, 0
        lds dx, ds:0Ah
        assume ds:nothing
        mov ah, 1Ah
        int 21h     ; DOS - SET DISK TRANSFER AREA ADDRESS
                    ; DS:DX -> disk transfer buffer
loc_175:
        popall
        retf 18h

DOSFINDFIRST ENDP
END

