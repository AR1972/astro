public DOSCHGFILEPTR
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSCHGFILEPTR        PROC FAR PASCAL

arg_0       = dword ptr  6
arg_4       = word ptr  0Ah
arg_6       = dword ptr  0Ch
arg_A       = word ptr  10h

        pushall
        les dx, [bp+arg_6]
        mov cx, es
        pop es
        mov ax, [bp+arg_4]
        mov bx, [bp+arg_A]
        mov ah, 42h
        int 21h     ; DOS - 2+ - MOVE FILE READ/WRITE POINTER (LSEEK)
                    ; AL = method:
                    ; 0-from beginnig,1-from current,2-from end
        jb  short loc_4D7
        lds si, [bp+arg_0]
        mov [si], ax
        mov [si+2], dx
        sub ax, ax
loc_4D7:
        popall
        retf 0Ch

DOSCHGFILEPTR ENDP
END

