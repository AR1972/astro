;***
;* $Workfile:   calldvr.asm  $
;* $Revision:   1.1  $
;*   $Author:   Dave Sewell  $
;*     $Date:   27 Jun 1989 14:55:26  $
;***

                TITLE   Call device driver routine
                PAGE    66, 132

COMMENT @
    calldvr.asm : Alan Butt : January 8, 1989 : Expansion Box Project

    This function calls a device driver

    Inputs:
        rhp     Far pointer to the request header
        header  Far pointer to the device driver header

    Outputs:    None
        The called driver may make changes to rhp
;   extern void call_driver(void far *rhp, struct device_header far *header);

@

device_header   STRUC                   ; Device header structure
    next_header dd  ?                   ; Link to next driver
    attribute   dw  ?                   ; Device driver attribute word
    strategy    dw  ?                   ; Offset to strategy entry point
    inter       dw  ?                   ; Offset to interrupt entry point
    name_num    db  8 DUP (?)           ; Device name or number of units
device_header   ENDS


%               .MODEL  memmodel, language

                .DATA

addr            dd      ?

                .CODE

call_driver     PROC    rhp:FAR PTR, header:FAR PTR device_header

                les     bx, header      ; Get address of strategy routine
                mov     bx, es:[bx].strategy
                mov     WORD PTR addr, bx
                mov     WORD PTR addr + 2, es

                les     bx, rhp         ; point to the rhp and
                call    addr            ; call the strategy routine

                les     bx, header      ; Get address of interrupt routine
                mov     bx, es:[bx].inter
                mov     WORD PTR addr, bx
                mov     WORD PTR addr + 2, es

                call    addr            ; Call the interrupt routine

                ret

call_driver     ENDP

                END
