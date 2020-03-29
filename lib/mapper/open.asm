public DOSOPEN
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSOPEN       PROC FAR PASCAL

var_2       = word ptr -2
arg_4       = word ptr  0Ah
arg_6       = word ptr  0Ch
arg_8       = word ptr  0Eh
arg_A       = dword ptr  10h
arg_E       = dword ptr  14h
arg_12      = dword ptr  18h
arg_16      = dword ptr  1Ch

        pushall
        sub sp, 2
        test    [bp+arg_4], 8000h
        jz  short loc_25A
        lds si, [bp+arg_16]
        mov al, [si]
        cmp al, 61h ; 'a'
        jb  short loc_23E
        cmp al, 7Bh ; '{'
        jnb short loc_23E
        add al, 0E0h ; 'à'

loc_23E:                ; CODE XREF: DOSOPEN+1Dj DOSOPEN+21j
        sub al, 41h ; 'A'
        jb  short loc_254
        cmp al, 1Bh
        jnb short loc_254
        xor ah, ah
        inc ax
        inc ax
        neg ax
        lds si, [bp+arg_12]
        mov [si], ax
        jmp loc_31F
; ---------------------------------------------------------------------------

loc_254:                ; CODE XREF: DOSOPEN+27j DOSOPEN+2Bj
        mov ax, 3
        jmp loc_321
; ---------------------------------------------------------------------------

loc_25A:                ; CODE XREF: DOSOPEN+14j
        lds dx, [bp+arg_16]
        mov ax, 4300h
        int 21h     ; DOS - 2+ - GET FILE ATTRIBUTES
                    ; DS:DX -> ASCIZ file name or directory
                    ; name without trailing slash
        jnb short loc_26C
        cmp ax, 2
        jz  short loc_2AC
        jmp loc_321
; ---------------------------------------------------------------------------

loc_26C:                ; CODE XREF: DOSOPEN+49j
        mov [bp+var_2], cx
        lds si, [bp+arg_E]
        mov word ptr [si], 1
        mov ax, [bp+arg_6]
        and ax, 3
        cmp ax, 3
        jz  short loc_28B
        cmp ax, 1
        jz  short loc_291
        cmp ax, 2
        jz  short loc_2BD

loc_28B:                ; CODE XREF: DOSOPEN+66j
        mov ax, 0Ch
        jmp loc_321
; ---------------------------------------------------------------------------

loc_291:                ; CODE XREF: DOSOPEN+6Bj
        lds si, [bp+arg_E]
        mov word ptr [si], 0
        lds dx, [bp+arg_16]
        mov ax, [bp+arg_4]
        mov ah, 3Dh
        int 21h     ; DOS - 2+ - OPEN DISK FILE WITH HANDLE
                    ; DS:DX -> ASCIZ filename
                    ; AL = access mode
                    ; 0 - read, 1 - write, 2 - read & write
        jb  short loc_321
        lds si, [bp+arg_12]
        mov [si], ax
        jmp short loc_30F
; ---------------------------------------------------------------------------
        align 2

loc_2AC:                ; CODE XREF: DOSOPEN+4Ej
        mov ax, [bp+arg_6]
        and ax, 10h
        cmp ax, 10h
        jz  short loc_2BD
        mov ax, 0Ch
        jmp short loc_321
; ---------------------------------------------------------------------------
        nop

loc_2BD:                ; CODE XREF: DOSOPEN+70j DOSOPEN+9Cj
        lds si, [bp+arg_E]
        mov word ptr [si], 2
        lds dx, [bp+arg_16]
        mov cx, [bp+arg_8]
        mov ah, 3Ch
        int 21h     ; DOS - 2+ - CREATE A FILE WITH HANDLE (CREAT)
                    ; CX = attributes for file
                    ; DS:DX -> ASCIZ filename (may include drive and path)
        jb  short loc_321
        lds si, [bp+arg_12]
        mov [si], ax
        les dx, [bp+arg_A]
        mov cx, es
        mov bx, ax
        mov ax, 4202h
        int 21h     ; DOS - 2+ - MOVE FILE READ/WRITE POINTER (LSEEK)
                    ; AL = method: offset from end of file
        jb  short loc_321
        lds si, [bp+arg_12]
        mov bx, [si]
        lds dx, [bp+arg_E]
        sub cx, cx
        mov ah, 40h
        int 21h     ; DOS - 2+ - WRITE TO FILE WITH HANDLE
                    ; BX = file handle, CX = number of bytes to write, DS:DX -> buffer
        jb  short loc_321
        lds si, [bp+arg_12]
        mov bx, [si]
        mov ah, 3Eh
        int 21h     ; DOS - 2+ - CLOSE A FILE WITH HANDLE
                    ; BX = file handle
        jb  short loc_321
        lds dx, [bp+arg_16]
        mov ax, [bp+arg_4]
        mov ah, 3Dh
        int 21h     ; DOS - 2+ - OPEN DISK FILE WITH HANDLE
                    ; DS:DX -> ASCIZ filename
                    ; AL = access mode
                    ; 0 - read, 1 - write, 2 - read & write
        jb  short loc_321
        lds si, [bp+arg_12]
        mov [si], ax

loc_30F:                ; CODE XREF: DOSOPEN+90j
        mov bx, ax
        add bx, bx
        mov ax, 8D3h
        mov ds, ax
        assume ds:nothing
        mov ax, [bp+var_2]
        mov [bx+4], ax

loc_31F:                ; CODE XREF: DOSOPEN+38j
        sub ax, ax

loc_321:                ; CODE XREF: DOSOPEN+3Ej DOSOPEN+50j ...
        add sp, 2
        popall
        retf    1Ah

DOSOPEN ENDP
END

