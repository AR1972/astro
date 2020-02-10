;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;;; This is the critical error handler.
;;; It is really dumb--all critical errors are Failed
;;; It is up to the Int21 caller to figure out what went wrong.

?WIN = 0		;Not windows;
?PLM =	  1		;DO use PL/M calling conventions

include cmacros.inc

extrn ErrorCrit:BYTE
sBegin code
    assumes cs, code
    assumes ds, DGROUP

FailCriticalError proc far
    push    ax
    push    ds
    mov     ax,DGROUP
    mov     ds,ax
    mov     ax,di
    mov     ErrorCrit,al	;;; save error code
    pop     ds
    pop     ax
    mov     al,3		;;; fail the int21 call
				;;; the caller is expected to look at
				;;; ErrorCrit
    iret
FailCriticalError endp

cProc  SetCriticalsToFail, PUBLIC , <si,di,ds,es>
cBegin SetCriticalsToFail
    ;;;     ds:dx must point to the critical error handler for set vector
    mov     ax,cs
    mov     ds,ax
    mov     dx,offset cs:FailCriticalError
    mov     ax,2524h
    int     21h
cEnd SetCriticalsToFail
sEnd code

end
