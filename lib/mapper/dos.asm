
?PLM=1      ; PASCAL Calling convention is DEFAULT
?WIN=0      ; Windows calling convention
?386=0      ; Use 386 code?

include cmacros.inc
include macros.inc

ifndef SEGNAME
    SEGNAME equ <TEXT>
endif

if ?386
    createSeg _%SEGNAME, CodeSeg, word, use16, CODE
else
    createSeg _%SEGNAME, CodeSeg, word, public, CODE
endif

;=============================================================================

sBegin DATA

sEnd   DATA

;=============================================================================

sBegin CodeSeg

assumes CS,CodeSeg
assumes DS,DATA

;DosExit( ulAction, ulResult );

cProc DosExit, <FAR, PUBLIC>
ParmW   action
ParmW   result
cBegin
    pushall
    mov ax, word ptr action
    cmp ax, 1
    jg  Done
    mov ax, word ptr result
    mov ah, 4Ch
    int 21h
    xor ax, ax
Done:
    popall
cEnd

;DosBeep( ulFrequency, ulDuration );

cProc DosBeep, <FAR, PUBLIC>
ParmW frequency
ParmW duration
cBegin
    pushall
    mov al, 0B6h
    out 43h, al     ; Timer 8253-5 (AT: 8254.2).
    mov dx, 12h
    mov ax, 2970h
    mov cx, word ptr frequency
    mov bx, 25h          ;LOWEST_FREQUENCY
    cmp cx, bx
    jl  loc_4A
    mov bx, 7FFFh        ;HIGEST_FREQUENCY
    cmp cx, bx
    jg  loc_4A
    div cx
    out 42h, al     ; Timer 8253-5 (AT: 8254.2).
    mov al, ah
    out 42h, al     ; Timer 8253-5 (AT: 8254.2).
    in  al, 61h     ; PC/XT PPI port B bits:
                    ; 0: Tmr 2 gate ÍËÍ OR 03H=spkr ON
                    ; 1: Tmr 2 data Í¼  AND  0fcH=spkr OFF
                    ; 3: 1=read high switches
                    ; 4: 0=enable RAM parity checking
                    ; 5: 0=enable I/O channel check
                    ; 6: 0=hold keyboard clock low
                    ; 7: 0=enable kbrd
    mov ah, al
    or  al, 3
    out 61h, al     ; PC/XT PPI port B bits:
                    ; 0: Tmr 2 gate ÍËÍ OR 03H=spkr ON
                    ; 1: Tmr 2 data Í¼  AND  0fcH=spkr OFF
                    ; 3: 1=read high switches
                    ; 4: 0=enable RAM parity checking
                    ; 5: 0=enable I/O channel check
                    ; 6: 0=hold keyboard clock low
                    ; 7: 0=enable kbrd
    mov cx, word ptr duration
loc_39:
    mov bx, 0C4h
loc_3C:
    dec bx
    jnz loc_3C
    loop    loc_39
    mov al, ah
    out 61h, al     ; PC/XT PPI port B bits:
                    ; 0: Tmr 2 gate ÍËÍ OR 03H=spkr ON
                    ; 1: Tmr 2 data Í¼  AND  0fcH=spkr OFF
                    ; 3: 1=read high switches
                    ; 4: 0=enable RAM parity checking
                    ; 5: 0=enable I/O channel check
                    ; 6: 0=hold keyboard clock low
                    ; 7: 0=enable kbrd
    sub ax, ax
    jmp loc_4D
loc_4A:
    mov ax, 2
loc_4D:
        popall
cEnd

;VioWrtCharStrAtt (CharStr, Length, Row, Column, Attr, VioHandle);

cProc VioWrtCharStrAtt, <FAR, PUBLIC>

ParmD chr
ParmW len
ParmW row
ParmW col
ParmD attr
ParmW handle

cBegin
    pushall
    ;mov dx, word ptr handle
    sub bh, bh
    sub ax, ax
    mov dx, word ptr col
    cmp dl, 50h
    jg  loc_1D4
    mov ax, word ptr row
    cmp al, 19h
    jg  loc_1D4
    mov dh, al
    mov ah, 2
    bios_pushall
    int 10h     ; - VIDEO - SET CURSOR POSITION
                ; DH,DL = row, column (0,0 = upper left)
                ; BH = page number
    bios_popall
    lds si, dword ptr attr
    mov bl, [si]
    lds si, dword ptr chr
    mov di, word ptr len
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
    jnz loc_1B7
    inc dh
    mov dl, 0
    cmp dh, 19h
    jnz loc_1B7
    mov ax, 1
    jmp loc_1D7
    nop
loc_1B7:
    mov ah, 2
    bios_pushall
    int 10h     ; - VIDEO - SET CURSOR POSITION
                ; DH,DL = row, column (0,0 = upper left)
                ; BH = page number
    bios_popall
    cmp di, 0
    jnz loc_184
    sub ax, ax
loc_1D4:
    mov ax, 2
loc_1D7:
    popall
cEnd

;VioScrollUp(TopRow, LeftCol, BotRow, RightCol, Lines, Cell, VioHandle);

cProc VioScrollUp, <FAR, PUBLIC>

ParmW toprow
ParmW leftcol
ParmW botrow
ParmW rightcol
ParmW lines
ParmD cell
ParmW handle

cBegin
    pushall
    mov bx, word ptr lines
    cmp bl, 19h
    jg  loc_130
    mov al, bl
    jmp loc_F0
    mov al, 0
loc_F0:
    mov ah, 6
    mov bx, word ptr rightcol
    cmp bl, 50h
    jg  loc_130
    mov dl, bl
    mov bx, word ptr botrow
    cmp bl, 19h
    jg  loc_130
    mov dh, bl
    mov bx, word ptr leftcol
    mov cl, bl
    mov bx, word ptr toprow
    mov ch, bl
    lds si, dword ptr cell
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
    jmp loc_133
loc_130:
    mov ax, 2
loc_133:
    popall
cEnd

;VioSetCurPos(Row, Column, VioHandle);

cProc VioSetCurPos, <FAR, PUBLIC>

ParmW row
ParmW col
ParmW handle

cBegin
        pushall
        mov bh, 0
        mov ax, word ptr row
        cmp al, 19h
        jg loc_B9
        mov dh, al
        mov ax, word ptr col
        cmp al, 50h
        jg loc_B9
        mov dl, al
        mov ah, 2
        bios_pushall
        int 10h     ; - VIDEO - SET CURSOR POSITION
                    ; DH,DL = row, column (0,0 = upper left)
                    ; BH = page number
        bios_popall
        sub ax, ax
        jmp loc_BC
loc_B9:
        mov ax, 2
loc_BC:
        popall
cEnd

;DosDelete (FileName, Reserved);

cProc DosDelete, <FAR, PUBLIC>

ParmD fname
ParmD res

cBegin
    pushall
    lds dx, dword ptr fname
    mov ah, 41h
    int 21h     ; DOS - 2+ - DELETE A FILE (UNLINK)
                ; DS:DX -> ASCIZ pathname of file to delete (no wildcards allowed)
    jb  short loc_1CD
    sub ax, ax
loc_1CD:
    popall
cEnd

;DosOpen(pszFileName, pHf, pulAction, cbFile, ulAttribute, fsOpenFlags, fsOpenMode, peaop2);

cProc DosOpen, <FAR, PUBLIC>

var_2       = word ptr -2
arg_4       = word ptr  0Ah
arg_6       = word ptr  0Ch
arg_8       = word ptr  0Eh
arg_A       = dword ptr  10h
arg_E       = dword ptr  14h
arg_12      = dword ptr  18h
arg_16      = dword ptr  1Ch

cBegin
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
cEnd

;DosWrite (hFile, pBuffer, cbWrite, pcbActual);

cProc DosWrite, <FAR, PUBLIC>

ParmW file
ParmD buffer
ParmW nwrite
ParmD actual

cBegin
    pushall
    mov bx, word ptr file
    lds dx, dword ptr buffer
    mov cx, word ptr nwrite
    mov ah, 40h
    int 21h     ; DOS - 2+ - WRITE TO FILE WITH HANDLE
                ; BX = file handle, CX = number of bytes to write, DS:DX -> buffer
    jb  short loc_1A7
    lds si, dword ptr actual
    mov [si], ax
    sub ax, ax
loc_1A7:
    popall
cEnd

;DosClose (hFile);

cProc DosClose, <FAR, PUBLIC>

ParmW file

cBegin
    pushall
    mov bx, word ptr file
    mov ax, bx
    neg ax
    jns loc_532
    mov ax, 3E00h
    int 21h     ; DOS - 2+ - CLOSE A FILE WITH HANDLE
                ; BX = file handle
    jb  loc_534
loc_532:
    sub ax, ax
loc_534:
        popall
cEnd

sEnd CodeSeg

;=============================================================================

end

