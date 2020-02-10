;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

?WIN = 0                ;Not windows;
?PLM =	  1		;DO use PL/M calling conventions

FLOPPY_TYPE EQU 1
REMOTE_TYPE EQU 2
HARDDISK_TYPE  EQU 3
RAMDRIVE_TYPE EQU 4
CDROM_TYPE EQU 5
include cmacros.inc

sBegin code
    assumes cs, code
    assumes ds, DGROUP

;
; int getdrivetype(int driveno)
;
cProc  getdrivetype, PUBLIC,  <si,di,ds,es>
parmW driveno
cBegin	getdrivetype
    ;;;; first check is for CDROM
    mov ax,0150bh
    xor bx,bx
    mov cx,driveno
    dec cx
    int 2fh
    cmp bx,0ADADh
    jne notCDROM
    cmp ax,0
    je notCDROM
    mov ax,CDROM_TYPE
    jmp dt_end
notCDROM:
    ;;;;

    mov ax, 4409h
    mov bx, driveno
    int 21h
    jc dt_not_found
    and dh, 10h
    jz	dt_local_check

    mov ax, REMOTE_TYPE
    jmp short dt_end

dt_local_check:
    mov ax, 4408h
    int 21h
    ; ZZZZ
    ; WARNING!! assuming that drive is non-removable as call 1 succeeded!
    ; WINDOWS FM seems to do the same!
    ; jc	dt_not_found
    jc	dt_nonremovable
    or	ax, ax
    jz	dt_removable

dt_nonremovable:
    mov ax, HARDDISK_TYPE
    jmp short	dt_end

dt_removable:
    mov ax, FLOPPY_TYPE
    jmp short dt_end

dt_not_found:
    xor ax, ax

dt_end:

cEnd  getdrivetype

sEnd   code


end
