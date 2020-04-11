
?PLM=1      ; PASCAL Calling convention is DEFAULT
?WIN=0      ; Windows calling convention
?386=0      ; Use 386 code?

public DOSQCURDISK
.xlist
include macros.inc
include cmacros.inc
.list
.286p
.MODEL small
.CODE
DOSQCURDISK        PROC FAR

arg_0           = dword ptr  6
arg_4           = dword ptr  0Ah

        pushall
        xor al, al
        mov ah, 33h
        int 21h     ; DOS - EXTENDED CONTROL-BREAK CHECKING
                    ; AL = 00h get state / 01h set state / 02h set AND get
                    ; DL = 00h for OFF or 01h for ON
        mov bl, dl
        or  dl, dl
        jz  short loc_10025
        inc al
        xor dl, dl
        int 21h     ; DOS -

loc_10025:
        mov ah, 19h
        int 21h     ; DOS - GET DEFAULT DISK NUMBER
        mov ah, bl
        mov es, ax
        lds bx, [bp+arg_4]
        xor ah, ah
        inc ax
        mov [bx], ax
        dec ax
        mov dh, al
        mov bx, 1
        xor si, si
        xor di, di
        mov cl, dh
        cmp cl, 10h
        jge short loc_1004D
        rol bx, cl
        or  si, bx
        jmp short loc_10054
        nop

loc_1004D:
        sub cl, 10h
        rol bx, cl
        or  di, bx

loc_10054:
        xor dl, dl
        mov bx, 1
        mov cx, 10h

loc_1005C:
        mov ah, 0Eh
        int 21h     ; DOS - SELECT DISK
                    ; DL = new default drive number (0 = A, 1 = B, etc.)
                    ; Return: AL = number of logical drives
        mov ah, 19h
        int 21h     ; DOS - GET DEFAULT DISK NUMBER
        cmp al, dh
        jz  short loc_1006C
        mov dh, al
        or  si, bx

loc_1006C:
        inc dl
        rol bx, 1
        loop    loc_1005C
        mov bx, 1
        mov cx, 0Ah

loc_10078:
        mov ah, 0Eh
        int 21h     ; DOS - SELECT DISK
                    ; DL = new default drive number (0 = A, 1 = B, etc.)
                    ; Return: AL = number of logical drives
        mov ah, 19h
        int 21h     ; DOS - GET DEFAULT DISK NUMBER
        cmp al, dh
        jz  short loc_10088
        mov dh, al
        or  di, bx

loc_10088:
        inc dl
        rol bx, 1
        loop    loc_10078
        lds bx, [bp+arg_0]
        mov [bx], si
        mov [bx+2], di
        mov ax, es
        mov bl, ah
        mov dl, al
        mov ah, 0Eh
        int 21h     ; DOS - SELECT DISK
                    ; DL = new default drive number (0 = A, 1 = B, etc.)
                    ; Return: AL = number of logical drives
        or  bl, bl
        jz  short loc_100AC
        mov dl, bl
        mov al, 1
        mov ah, 33h ;
        int 21h     ; DOS - EXTENDED CONTROL-BREAK CHECKING
                    ; AL = 00h get state / 01h set state / 02h set AND get
                    ; DL = 00h for OFF or 01h for ON

loc_100AC:
        xor ax, ax
        popall
        retf 8

DOSQCURDISK ENDP
END

