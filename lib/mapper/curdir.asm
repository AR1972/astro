public DOSQCURDIR
.xlist
include macros.inc
.list
.286p
.MODEL small
.CODE
DOSQCURDIR        PROC FAR PASCAL

arg_0       = dword ptr  6
arg_4       = dword ptr  0Ah
arg_8       = word ptr  0Eh

        pushall
        mov ax, 8DFh
        mov ds, ax
        assume ds:nothing
        mov si, 0Ch
        mov dx, [bp+arg_8]
        mov ah, 47h
        int 21h     ; DOS - 2+ - GET CURRENT DIRECTORY
                    ; DL = drive (0=default, 1=A, etc.)
                    ; DS:SI points to 64-byte buffer area
        jb  short loc_383
        mov di, ds
        mov es, di
        assume es:nothing
        mov di, 0Ch
        mov cx, 80h ; '€'
        mov al, 0
        cld
        repne scasb
        mov dx, 80h ; '€'
        sub dx, cx
        les di, [bp+arg_0]
        assume es:nothing
        mov cx, es:[di]
        cmp cx, dx
        jnb short loc_374
        mov ax, 8
        jmp short loc_383
loc_374:
        mov cx, dx
        mov es:[di], dx
        les di, [bp+arg_4]
        mov si, 0Ch
        rep movsb
        sub ax, ax
loc_383:
        popall
        retf 0Ah

DOSQCURDIR ENDP
END

