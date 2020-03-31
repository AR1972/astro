seg000      segment byte public 'CODE'
        assume cs:seg000
start:
        mov ax, 7C0h
        push    ax
        pop ds
        mov bx, msgstart-start
        mov cx, msgend-msgstart
loc_D:
        mov ah, 0Eh
        mov al, [bx]
        push    ds
        push    cx
        push    bx
        xor bx, bx
        int 10h     ; - VIDEO - WRITE CHARACTER AND ADVANCE CURSOR (TTY WRITE)
                    ; AL = character, BH = display page (alpha modes)
                    ; BL = foreground color (graphics modes)
        pop bx
        pop cx
        pop ds
        inc bx
        loop loc_D
        mov ah, 0
        int 16h     ; KEYBOARD - READ CHAR FROM BUFFER, WAIT IF EMPTY
                    ; Return: AH = scan code, AL = character
        mov ah, 0Fh
        int 10h     ; - VIDEO - GET CURRENT VIDEO MODE
                    ; Return: AH = number of columns on screen
                    ; AL = current video mode
                    ; BH = current active display page
        mov ah, 0
        int 10h     ; - VIDEO - SET VIDEO MODE
                    ; AL = mode
        int 19h     ; DISK BOOT
                    ; causes reboot of disk system
; ---------------------------------------------------------------------------
msgstart:
msg     db 0Dh,0Ah
        db 0Dh,0Ah
        db 0Dh,0Ah
        db 0Dh,0Ah
        db '     The MS-DOS 6 Setup was not completed',0Dh,0Ah
        db '     Insert the UNINSTALL #1 diskette in drive A',0Dh,0Ah
        db '     Press the ENTER key to continue',0Dh,0Ah
msgend:

seg000      ends
end
