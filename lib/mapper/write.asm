
?PLM=1      ; PASCAL Calling convention is DEFAULT
?WIN=0      ; Windows calling convention
?386=0      ; Use 386 code?

public DOSWRITE
.xlist
include macros.inc
include cmacros.inc
.list
.286p
.MODEL small
.CODE
DOSWRITE        PROC FAR

arg_0       = dword ptr  6
arg_4       = word ptr  0Ah
arg_6       = dword ptr  0Ch
arg_A       = word ptr  10h

        pushall
        mov bx, [bp+arg_A]
        lds dx, [bp+arg_6]
        mov cx, [bp+arg_4]
        mov ah, 40h
        int 21h     ; DOS - 2+ - WRITE TO FILE WITH HANDLE
                    ; BX = file handle, CX = number of bytes to write, DS:DX -> buffer
        jb  short loc_1A7
        lds si, [bp+arg_0]
        mov [si], ax
        sub ax, ax
loc_1A7:
        popall
        retf    0Ch

DOSWRITE ENDP
END

