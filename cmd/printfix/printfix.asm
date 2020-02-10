;=============================================================================
;
; Program: PRINTFIX.COM 
;  Author: Doug Olson (DougO)
;    Date: 08/30/91
;
; Purpose: This TSR hooks BIOS interrupt 17h and installs an ISR that resets
;          bit 3 of the status byte returned in register AH from the BIOS
;          int 17 handler.  When bit 3 of the status byte is set, this 
;          indicates an I/O error on the parallel printer interface port.
;          The purpose of resetting bit 3 is to avoid MS-DOS 5.x from
;          reporting a write fault error.
;
; History: MS-DOS 5 checks the status bit 3 when printing to a parallel port.
;          Some systems, for uncertain reasons, return this bit set indicating
;          an I/O error when in fact there is nothing wrong that prevents
;          printing.  When these systems were upgraded to MS-DOS 5, printing
;          to the parallel device resulted in write faults.
;
;   Usage: This TSR should only be installed on systems that exhibit the
;          I/O error problem.  
;
;=============================================================================

code_seg        segment                 ; begin segment
        assume  cs:code_seg
begin:
        org     100h                    ; start at offset 100h

start:  jmp     short init_code         ; jump to initialization code

old_int_17      label   word            ; address of the old interrupt 
old_int_17_addr dd      ?               ; 17h handler

patch   proc    near                    ; ISR entry point
        pushf                           ; push flags
        cmp     ah,02h                  ; is it the Get Printer Status func?
        jne     call_old                ; branch if not
        mov     ah,90h                  ; set bit 4 (printer selected) and
        popf                            ;     bit 7 (printer not busy)
        iret
call_old:
        popf                            ; get the old flags back
        pushf                           ; push flags for interrupt
        call    [old_int_17_addr]       ; call old interrupt 17 handler
        iret                            ; return from interrupt
patch   endp                            ; end of resident portion

init_code       proc    near            ; begin initialization code
        mov     ah,09h                  ; display string function
        mov     dx, offset message      ; get offset of message
        int     21h                     ; print the message
        mov     ax,3517h                ; get interrupt vector (17h)
        int     21h                     ; call DOS service
        mov     old_int_17,bx           ; save address of old interrupt
        mov     old_int_17[2],es        ; 17 handler at old_int_17  
        mov     ax,2517h                ; set interrupt vector
        lea     dx,patch                ; offset of ISR
        int     21h                     ; call DOS service
        mov     dx, (((offset init_code-offset begin)+15)/16) ; Get size
        mov     ah,31h                  ; terminate and stay resident
        int     21h                     ; call DOS service
init_code       endp

        include printfix.inc

        code_seg        ends            ; end segment

        end     start                   ; the end


