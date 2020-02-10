;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;
; Portliba  --  routines to support transport of lattice C to Cmerge
;
; Copyright (c) Microsoft Corporation, 1984
;
; This file contains:
;       ctlc                    - M000 Control-C handler
;       CritErr                 - DOS Critical Error (int 24) handler
;

;***    Modification History
;
;       M000    04/02/85        gregti
;
;     - Added routine ctlc that gets installed as a DOS int 23 handler.
;       See program header for details.
;
;       M001    04/04/85        gregti
;
;     - Fixed getcurdir to prepend the current drive, colon and root
;       path character to buffer before curdir call.  Also involved
;       change to msdos.inc
;-----------------------------------------------------------------------
;	MSKK01	June 1, 1987	akik
;		Modify to ADDDRV command
;-----------------------------------------------------------------------



                .xlist
include         cmacros.inc
                .list

                extrn   _on_sig:near            ; M000
                extrn   _old24:far

                public  _ctlc, _CritErr         ; M000
                public  ctlc, CritErr
;               public  _old24

                assumes cs, code
                assumes ds, data
                assumes ss, data

sbegin          code


;***  ctlc - Int 23 handler
;
; This routine calls actual signal handler routine in file SYNCSIGL.C
; and does IRET when that routine returns.
;
; return: Nothing
;
; calls: _on_sig in file SYNCSIGL.C
;
;
; void ctlc()
;
; Notes:
;       This routine does not use the cmacros.inc stuff because of the need
;       to do an IRET and save and restore the world.
;
ctlc    label   near
assume ds:nothing,es:nothing    ; Its an interrupt handler

_ctlc   proc    NEAR

                push    bp              ; Imitate cmacros
                mov     bp,sp
                push    ax              ; Save everything we can think of
                push    bx
                push    cx
                push    dx
                push    di
                push    si
                push    ds
                push    es
                push    ss
                mov     ax,01           ; Array index for _ON_SIG
                push    ax              ; _ON_SIG expects two args on stack
                push    ax
                call    _on_sig         ; Call actual handler
                pop     ax              ; Clear the two args
                pop     ax
                pop     ss              ; Restore everything we saved
                pop     es
                pop     ds
                pop     si
                pop     di
                pop     dx
                pop     cx
                pop     bx
                pop     ax
                mov     sp,bp           ; Dup the cmacros stuff
                pop     bp
                iret                    ; Get back to DOS
_ctlc   endp

;***  CritErr - Int 24 handler
;
; This routine calls actual signal handler routine in file SYNCSIGL.C
; and does IRET when that routine returns.
;
; return: Nothing
;
; calls:
;        _on_sig in file SYNCSIGL.C
;        _old24  The MSDOS INT24 handler.
;
;
; void CritErr()
;
; Notes:
;       This routine does not use the cmacros.inc stuff because of the need
;       to do an IRET and save and restore the world.
;
;       It is written to be heavily application and system dependant:
;       It assumes that FAIL is never returned by the MSDOS INT24 handler
;       It assumes IGNORE and ABORT are equivilant and signals the APP on
;       either.
;

_CritErr   proc    NEAR
CritErr    label   near

                mov     ax,07           ; SIGHDERR for ON_SIG
                push    ax              ; _ON_SIG expects two args on stack
                push    ax
                call    _on_sig         ; Call actual handler
                pop     ax              ; Clear the two args
                pop     ax
;
		pop	ax
		pop	ax
		pop	ax
;
                pop     ax              ; Restore everything
                pop     bx
                pop     cx
                pop     dx
                pop     si
                pop     di
                pop     bp
                pop     ds
                pop     es
                iret                    ; Get back to DOS
_CritErr   endp

send            code


sbegin          data
send            data

                end
