
;=============================================================================

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

;##############################################################################

sBegin DATA

sEnd   DATA

;##############################################################################

sBegin CodeSeg

assumes CS,CodeSeg
assumes DS,DATA

;=============================================================================

;void far pascal DOSEXIT (
;    unsigned Action,
;    unsigned Result );

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

;=============================================================================

;unsigned far pascal DOSBEEP (
;    unsigned Frequency,
;    unsigned Duration );

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

;=============================================================================

;unsigned far pascal VIOWRTCHARSTRATT (
;    char far * Str,
;    unsigned Length,
;    unsigned Row,
;    unsigned Column,
;    char far * Attr,
;    unsigned VioHandle );

cProc VioWrtCharStrAtt, <FAR, PUBLIC>

ParmD chr
ParmW len
ParmW row
ParmW col
ParmD attr
ParmW handle

cBegin

        pushall
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

;=============================================================================

;unsigned far pascal VIOSCROLLUP (
;    unsigned TopRow,
;    unsigned LeftCol,
;    unsigned BotRow,
;    unsigned RightCol,
;    unsigned Lines,
;    char far * Cell,
;    unsigned VioHandle );

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

;=============================================================================

;unsigned far pascal VIOSETCURPOS (
;    unsigned Row,
;    unsigned Column,
;    unsigned VioHandle );

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

;=============================================================================

;unsigned far pascal DOSDELETE (
;    char far * FileName,
;    unsigned long Reserved );

cProc DosDelete, <FAR, PUBLIC>

ParmD fname
ParmD res

cBegin

        pushall
        lds dx, dword ptr fname
        mov ah, 41h
        int 21h     ; DOS - 2+ - DELETE A FILE (UNLINK)
                    ; DS:DX -> ASCIZ pathname of file to delete (no wildcards allowed)
        jb  loc_1CD
        sub ax, ax

loc_1CD:

        popall

cEnd

;=============================================================================

;unsigned far pascal DOSOPEN (
;    char far * FileName,
;    unsigned far * Handle,
;    unsigned far * Action,
;    unsigned long File,
;    unsigned Attribute,
;    unsigned OpenFlags,
;    unsigned OpenMode,
;    unsigned long eaop2 );

cProc DosOpen, <FAR, PUBLIC>

var_2       = word ptr -2
arg_4       = word ptr  0Ah
arg_6       = word ptr  0Ch
arg_8       = word ptr  0Eh
arg_A       = dword ptr  10h
arg_E       = dword ptr  14h
arg_12      = dword ptr  18h
arg_16      = dword ptr  1Ch
ParmW eaop2

cBegin

        pushall
        sub sp, 2
        test    [bp+arg_4], 8000h
        jz  loc_25A
        lds si, [bp+arg_16]
        mov al, [si]
        cmp al, 61h
        jb  loc_23E
        cmp al, 7Bh
        jnb loc_23E
        add al, 0E0h

loc_23E:

        sub al, 41h
        jb  loc_254
        cmp al, 1Bh
        jnb loc_254
        xor ah, ah
        inc ax
        inc ax
        neg ax
        lds si, [bp+arg_12]
        mov [si], ax
        jmp loc_31F

loc_254:

        mov ax, 3
        jmp loc_321

loc_25A:

        lds dx, [bp+arg_16]
        mov ax, 4300h
        int 21h     ; DOS - 2+ - GET FILE ATTRIBUTES
                    ; DS:DX -> ASCIZ file name or directory
                    ; name without trailing slash
        jnb loc_26C
        cmp ax, 2
        jz  loc_2AC
        jmp loc_321

loc_26C:

        mov [bp+var_2], cx
        lds si, [bp+arg_E]
        mov word ptr [si], 1
        mov ax, [bp+arg_6]
        and ax, 3
        cmp ax, 3
        jz  loc_28B
        cmp ax, 1
        jz  loc_291
        cmp ax, 2
        jz  loc_2BD

loc_28B:

        mov ax, 0Ch
        jmp loc_321

loc_291:

        lds si, [bp+arg_E]
        mov word ptr [si], 0
        lds dx, [bp+arg_16]
        mov ax, [bp+arg_4]
        mov ah, 3Dh
        int 21h     ; DOS - 2+ - OPEN DISK FILE WITH HANDLE
                    ; DS:DX -> ASCIZ filename
                    ; AL = access mode
                    ; 0 - read, 1 - write, 2 - read & write
        jb  loc_321
        lds si, [bp+arg_12]
        mov [si], ax
        jmp loc_30F

loc_2AC:

        mov ax, [bp+arg_6]
        and ax, 10h
        cmp ax, 10h
        jz  loc_2BD
        mov ax, 0Ch
        jmp loc_321

loc_2BD:

        lds si, [bp+arg_E]
        mov word ptr [si], 2
        lds dx, [bp+arg_16]
        mov cx, [bp+arg_8]
        mov ah, 3Ch
        int 21h     ; DOS - 2+ - CREATE A FILE WITH HANDLE (CREAT)
                    ; CX = attributes for file
                    ; DS:DX -> ASCIZ filename (may include drive and path)
        jb  loc_321
        lds si, [bp+arg_12]
        mov [si], ax
        les dx, [bp+arg_A]
        mov cx, es
        mov bx, ax
        mov ax, 4202h
        int 21h     ; DOS - 2+ - MOVE FILE READ/WRITE POINTER (LSEEK)
                    ; AL = method: offset from end of file
        jb  loc_321
        lds si, [bp+arg_12]
        mov bx, [si]
        lds dx, [bp+arg_E]
        sub cx, cx
        mov ah, 40h
        int 21h     ; DOS - 2+ - WRITE TO FILE WITH HANDLE
                    ; BX = file handle, CX = number of bytes to write, DS:DX -> buffer
        jb  loc_321
        lds si, [bp+arg_12]
        mov bx, [si]
        mov ah, 3Eh
        int 21h     ; DOS - 2+ - CLOSE A FILE WITH HANDLE
                    ; BX = file handle
        jb  loc_321
        lds dx, [bp+arg_16]
        mov ax, [bp+arg_4]
        mov ah, 3Dh
        int 21h     ; DOS - 2+ - OPEN DISK FILE WITH HANDLE
                    ; DS:DX -> ASCIZ filename
                    ; AL = access mode
                    ; 0 - read, 1 - write, 2 - read & write
        jb  loc_321
        lds si, [bp+arg_12]
        mov [si], ax

loc_30F:

        mov bx, ax
        add bx, bx
        mov ax, 8D3h
        mov ds, ax
        assume ds:nothing
        mov ax, [bp+var_2]
        mov [bx+4], ax

loc_31F:

        sub ax, ax

loc_321:

        add sp, 2
        popall

cEnd

;=============================================================================

;unsigned far pascal DOSWRITE (
;    unsigned Handle,
;    char far * Buffer,
;    unsigned Write,
;    unsigned far * Actual );

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
        jb  loc_1A7
        lds si, dword ptr actual
        mov [si], ax
        sub ax, ax

loc_1A7:

        popall

cEnd

;=============================================================================

;unsigned far pascal DosClose (
;    unsigned File );

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

;=============================================================================

;unsigned far pascal DosQCurDisk (
;    unsigned far *DriveNumber,
;    unsigned long far * LogicalDriveMap );

cProc DosQCurDisk, <FAR, PUBLIC>

ParmD DriveNumber
ParmD LogicalDriveMap

cBegin

        pushall
        xor al, al
        mov ah, 33h
        int 21h     ; DOS - EXTENDED CONTROL-BREAK CHECKING
                    ; AL = 00h get state / 01h set state / 02h set AND get
                    ; DL = 00h for OFF or 01h for ON
        mov bl, dl
        or  dl, dl
        jz  loc_10025
        inc al
        xor dl, dl
        int 21h     ; DOS -

loc_10025:

        mov ah, 19h
        int 21h     ; DOS - GET DEFAULT DISK NUMBER
        mov ah, bl
        mov es, ax
        lds bx, dword ptr LogicalDriveMap
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
        jge loc_1004D
        rol bx, cl
        or  si, bx
        jmp loc_10054
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
        jz  loc_1006C
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
        jz  loc_10088
        mov dh, al
        or  di, bx

loc_10088:

        inc dl
        rol bx, 1
        loop    loc_10078
        lds bx, dword ptr DriveNumber
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
        jz  loc_100AC
        mov dl, bl
        mov al, 1
        mov ah, 33h ;
        int 21h     ; DOS - EXTENDED CONTROL-BREAK CHECKING
                    ; AL = 00h get state / 01h set state / 02h set AND get
                    ; DL = 00h for OFF or 01h for ON

loc_100AC:

        xor ax, ax
        popall

cEnd

;=============================================================================

;unsigned far pascal DosQCurDir (
;    unsigned DriveNumber,
;    char far * DirPath,
;    unsigned far * DirPathLen );

cProc DosQCurDir, <FAR, PUBLIC>

Parmw DriveNumber
ParmD DirPath
ParmD DirPathLen

cBegin

        pushall
        mov ax, 82Fh
        mov ds, ax
        mov si, 6
        mov dx, word ptr DriveNumber
        mov ah, 47h
        int 21h     ; DOS - 2+ - GET CURRENT DIRECTORY
                    ; DL = drive (0=default, 1=A, etc.)
                    ; DS:SI points to 64-byte buffer area
        jb  loc_17AF4
        mov di, ds
        mov es, di
        mov di, 6
        mov cx, 80h
        mov al, 0
        cld
        repne scasb
        mov dx, 80h
        sub dx, cx
        les di, dword ptr DirPathLen
        mov cx, es:[di]
        cmp cx, dx
        jnb loc_17AE5
        mov ax, 8
        jmp loc_17AF4

loc_17AE5:

        mov cx, dx
        mov es:[di], dx
        les di, dword ptr DirPath
        mov si, 6
        rep movsb
        sub ax, ax

loc_17AF4:

        popall

cEnd

;=============================================================================

;unsigned far pascal DosQFileMode (
;    char far * FilePathName,
;    unsigned far * CurrentAttribute,
;    unsigned long Reserved );

cProc DosQFileMode, <FAR, PUBLIC>

ParmD FilePathName
ParmD CurAttribute
ParmD Reserved

cBegin

        pushall
        lds dx, dword ptr FilePathName
        mov ax, 4300h
        int 21h     ; DOS - 2+ - GET FILE ATTRIBUTES
                    ; DS:DX -> ASCIZ file name or directory
                    ; name without trailing slash
        jb  loc_470
        lds si, dword ptr CurAttribute
        mov [si], cx
        sub ax, ax
        jmp loc_473

loc_470:

        mov ax, 2

loc_473:

        popall

cEnd

;=============================================================================

;unsigned far pascal DosAllocSeg (
;    unsigned Size,
;    unsigned far * Selector,
;    unsigned Flags );

cProc DosAllocSeg, <FAR, PUBLIC>

arg_2       = dword ptr  8 ;size
arg_6       = word ptr  0Ch ;selector


cBegin

        pushall
        mov bx, [bp+arg_6]
        test    bx, 0Fh
        jz  loc_55E
        and bx, 0FFF0h
        add bx, 10h

loc_55E:

        cmp bx, 0
        jz  loc_56E
        shr bx, 1
        shr bx, 1
        shr bx, 1
        shr bx, 1
        jmp loc_571

loc_56E:

        mov bx, 1000h

loc_571:

        mov ah, 48h
        int 21h     ; DOS - 2+ - ALLOCATE MEMORY
                    ; BX = number of 16-byte paragraphs desired
        jb  loc_57E
        lds si, [bp+arg_2]
        mov [si], ax
        sub ax, ax

loc_57E:

        popall

cEnd

;=============================================================================

;unsigned far pascal DosChgFilePtr (
;   unsigned FileHandle, 
;   long Distance,
;   unsigned MoveType,
;   unsigned long far * NewPointer );

cProc DosChgFilePtr, <FAR, PUBLIC>

arg_0       = dword ptr  6   ;FileHandle
arg_4       = word ptr  0Ah  ;Distance
arg_6       = dword ptr  0Ch ;MoveType
arg_A       = word ptr  10h  ;NewPointer

cBegin

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
        jb  loc_4D7
        lds si, [bp+arg_0]
        mov [si], ax
        mov [si+2], dx
        sub ax, ax

loc_4D7:

        popall

cEnd

;=============================================================================

;unsigned far DosSetFileInfo (
;    unsigned handle,
;    unsigned InfoLevel,
;    char far * pInfoBuf,
;    unsigned cbInfoBuf );

cProc DosSetFileInfo, <FAR, PUBLIC>

arg_2       = dword ptr  8
arg_8       = word ptr  0Eh

cBegin

        pushall
        mov bx, [bp+arg_8]
        lds si, [bp+arg_2]
        mov dx, [si+8]
        mov cx, [si+0Ah]
        mov ax, 5701h
        int 21h     ; DOS - 2+ - SET FILE'S DATE/TIME
                    ; BX = file handle, CX = time to be set
                    ; DX = date to be set
        jb  loc_507
        sub ax, ax

loc_507:

        popall

cEnd

;=============================================================================

;unsigned far pascal DosSetFileMode (
;    char far * FileName,
;    unsigned NewAttribute,
;    unsigned long Reserved );

cProc DosSetFileMode, <FAR, PUBLIC>

arg_4       = word ptr  0Ah
arg_6       = dword ptr  0Ch

cBegin

        pushall
        lds dx, [bp+arg_6]
        mov cx, [bp+arg_4]
        mov ax, 4301h
        int 21h     ; DOS - 2+ - SET FILE ATTRIBUTES
                    ; DS:DX -> ASCIZ file name
                    ; CX = file attribute bits
        jb  loc_49F
        xor ax, ax
        jmp short $+2

loc_49F:

        popall

cEnd

;=============================================================================

;unsigned far pascal DOSFINDFIRST (
;    char far * pszFileSpec,
;    unsigned far * phdir,
;    unsigned flAttribute,
;    struct FileFindBuf far * pfindbuf,
;    unsigned cbBuf,
;    unsigned far * pcFileNames,
;    unsigned long ulInfoLevel );

cProc DosFindFirst, <FAR, PUBLIC>

arg_4       = dword ptr  0Ah
arg_8       = word ptr  0Eh
arg_A       = dword ptr  10h
arg_E       = word ptr  14h
arg_10      = dword ptr  16h
arg_14      = dword ptr  1Ah

cBegin

        pushall
        mov ax, 81Ch
        mov ds, ax
        assume ds:nothing
        mov si, 167h
        mov cx, 8
        les di, [bp+arg_10]
        mov ax, es:[di]
        cmp ax, 0FFFFh
        jz  loc_35
        cmp ax, 1
        jz  loc_4AA
        mov si, ax
        test    word ptr [si], 8000h
        jnz loc_4AA
        mov ax, 6
        jmp loc_15B

loc_35:

        add si, 2
        dec cx

loop_39:

        test    word ptr [si], 8000h
        jz  loc_4AA
        add si, 2
        loop    loop_39

loc_44:

        mov ax, 4
        jmp loc_15B

loc_4AA:

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
        jnz loc_8B
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
        jnb loc_9B
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

        sub word ptr ds:8, 23h
        jnb loc_BE
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
        jz  loc_10D
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
        jz  loc_12A
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
        jz  loc_159
        mov ax, 4F00h
        int 21h     ; DOS - 2+ - FIND NEXT ASCIZ (FINDNEXT)
                    ; [DTA] = data block from
                    ; last AH = 4Eh/4Fh call
        jb  loc_15B
        les di, ds:2
        jmp loc_B1

loc_159:

        sub ax, ax

loc_15B:

        push    ax
        mov ax, 81Ch
        mov ds, ax
        cmp byte ptr ds:0Eh, 0
        jz  loc_175
        mov byte ptr ds:0Eh, 0
        lds dx, ds:0Ah
        assume ds:nothing
        mov ah, 1Ah
        int 21h     ; DOS - SET DISK TRANSFER AREA ADDRESS
                    ; DS:DX -> disk transfer buffer

loc_175:

        popall

cEnd

;=============================================================================

;extern unsigned far pascal DOSFINDNEXT (
;    unsigned hDir,
;    struct FileFindBuf far * pfindbuf,
;    unsigned cbfindbuf,
;    unsigned far * pcFilenames );

cProc DosFindNext, <FAR, PUBLIC>

arg_0       = dword ptr  6
arg_4       = word ptr  0Ah
arg_6       = dword ptr  0Ch
arg_A       = word ptr  10h

cBegin

        pushall
        mov ax, 81Ch
        mov ds, ax
        assume ds:nothing
        mov si, [bp+arg_A]
        cmp si, 1
        jnz loc_647
        mov si, 167h

loc_647:

        test    word ptr [si], 8000h
        jnz loc_653
        mov ax, 6
        jmp loc_746

loc_653:

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
        jnz loc_68C
        jmp loc_744

loc_68C:

        mov ax, [bp+arg_4]
        mov ds:8, ax
        les di, [bp+arg_6]
        mov ds:2, di
        mov word ptr ds:4, es

loc_69D:

        mov ax, 4F00h
        int 21h     ; DOS - 2+ - FIND NEXT ASCIZ (FINDNEXT)
                    ; [DTA] = data block from
                    ; last AH = 4Eh/4Fh call
        jnb loc_6A7
        jmp loc_746

loc_6A7:

        sub word ptr ds:8, 23h
        jnb loc_6B4
        mov ax, 8
        jmp loc_746

loc_6B4:

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
        jz  loc_703
        and word ptr es:[di+10h], 0FE00h
        add word ptr es:[di+10h], 200h

loc_703:

        xor ax, ax
        mov al, [si+15h]
        mov es:[di+14h], ax
        mov cx, 0Ch
        mov bx, 0

loc_712:

        mov al, [bx+si+1Eh]
        cmp al, 0
        jz  loc_720
        mov es:[bx+di+17h], al
        inc bx
        loop    loc_712

loc_720:

        mov byte ptr es:[bx+di+17h], 0
        mov ax, bx
        mov es:[di+16h], al
        add di, bx
        mov ds:2, di
        mov word ptr ds:4, es
        les di, [bp+arg_0]
        inc word ptr es:[di]
        dec word ptr ds:0
        jz  loc_744
        jmp loc_69D

loc_744:

        sub ax, ax

loc_746:

        push    ax
        mov ax, 81Ch
        mov ds, ax
        cmp byte ptr ds:0Eh, 0
        jz  loc_760
        mov byte ptr ds:0Eh, 0
        lds dx, ds:0Ah
        assume ds:nothing
        mov ah, 1Ah
        int 21h     ; DOS - SET DISK TRANSFER AREA ADDRESS
                    ; DS:DX -> disk transfer buffer

loc_760:

        popall

cEnd

;=============================================================================

;unsigned far pascal DOSQFILEINFO (
;    unsigned FileHandle,
;    unsigned FileInfoLevel,
;    char far * FileInfoBuf,
;    unsigned FileInfoBufSize );

cProc DosQFileInfo, <FAR, PUBLIC>

cBegin

    pushall
    xor ax, ax
    popall

cEnd

;=============================================================================

;unsigned far pascal DosError (
;    unsigned error );

cProc DosError, <FAR, PUBLIC>

ParmW err

cBegin

    pushall
    xor ax, ax
    popall

cEnd

;=============================================================================

;unsigned far DosGetCtryInfo (
;    unsigned Length,
;    struct countrycode far * Country,
;    char far * MemoryBuffer,
;    unsigned far * DataLength );

cProc DosGetCtryInfo, <FAR, PUBLIC>

cBegin

    pushall
    xor ax, ax
    popall

cEnd

;=============================================================================

;unsigned far pascal DosQFSInfo (
;    unsigned DriveNumber,
;    unsigned FSInfoLevel,
;    char far * FSInfoBuf,
;    unsigned FSInfoBufSize );

cProc DosQFSInfo, <FAR, PUBLIC>

cBegin

    pushall
    xor ax, ax
    popall

cEnd

;=============================================================================

;unsigned far pascal DosFindClose (
;    unsigned hDir );

cProc DosFindClose, <FAR, PUBLIC>

cBegin

    pushall
    xor ax, ax
    popall

cEnd

;=============================================================================

;unsigned far pascal DosSetFSInfo (
;    unsigned disknum,
;    unsigned infolevel,
;    char far * pBuf,
;    unsigned cbBuf);

cProc DosSetFSInfo, <FAR, PUBLIC>

cBegin

    pushall
    xor ax, ax
    popall

cEnd

;=============================================================================

;unsigned far pascal DosSelectDisk (
;    unsigned DriveNumber );

cProc DosSelectDisk, <FAR, PUBLIC>

ParmW drivenumber

cBegin

        pushall
        mov ah, 19h
        int 21h     ; DOS - GET DEFAULT DISK NUMBER
        mov cl, al
        mov dx, word ptr drivenumber
        or  dh, dh
        jnz loc_10033
        dec dl
        mov ah, 0Eh
        int 21h     ; DOS - SELECT DISK
                    ; DL = new default drive number (0 = A, 1 = B, etc.)
                    ; Return: AL = number of logical drives
        mov ah, 19h
        int 21h     ; DOS - GET DEFAULT DISK NUMBER
        cmp al, dl
        jnz loc_10033
        xor ax, ax
        jmp loc_1003C

loc_10033:

        mov dl, cl
        mov ah, 0Eh
        int 21h     ; DOS - SELECT DISK
                    ; DL = new default drive number (0 = A, 1 = B, etc.)
                    ; Return: AL = number of logical drives
        mov ax, 0Fh

loc_1003C:

        popall

cEnd

;=============================================================================

;void far pascal Set_Int24_Vector ( VOID );

cProc Set_Int24_Vector, <FAR, PUBLIC>

cBegin

    pushall
    xor ax, ax
    popall

cEnd

;=============================================================================

;unsigned far pascal DOSSETSIGHANDLER (
;    void (far pascal *)() Routine,
;    unsigned long far * PrevAddress,
;    unsigned far * PrevAction,
;    unsigned Action,
;    unsigned SigNumber );

cProc DosSetSigHandler, <FAR, PUBLIC>

cBegin

    pushall
    xor ax, ax
    popall

cEnd

;##############################################################################

sEnd CodeSeg

end

