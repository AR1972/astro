public DOSBEEP

.286p
.MODEL small
.CODE
DOSBEEP PROC FAR PASCAL

arg_2 = word ptr 6
arg_4 = word ptr 8

        push    bp
        mov bp, sp
        push    bx
        push    cx
        push    dx
        push    si
        push    di
        push    ds
        push    es
        push    ss
        push    bp
        mov al, 0B6h      ; ERROR_INVALID_FEQUENCY
        out 43h, al     ; Timer 8253-5 (AT: 8254.2).
        mov dx, 12h
        mov ax, 2970h
        mov cx, [bp+arg_4]
        mov bx, 25h          ;LOWEST_FREQUENCY
        cmp cx, bx
        jl  short loc_4A
        mov bx, 7FFFh        ;HIGEST_FREQUENCY
        cmp cx, bx
        jg  short loc_4A
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
        mov cx, [bp+arg_2]
loc_39:
        mov bx, 0C4h
loc_3C:
        dec bx
        jnz short loc_3C
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
        jmp short loc_4D
loc_4A:
        mov ax, 2
loc_4D:
        pop bp
        pop ss
        pop es
        pop ds
        pop di
        pop si
        pop dx
        pop cx
        pop bx
        mov sp, bp
        pop bp
        retf 4

DOSBEEP ENDP
END