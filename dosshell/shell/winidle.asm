;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;
;  Tell forces greater than ourselves that we are idle so we
;  don't pig up system time doing nothing. 1680 call is for
;  Windows 3.0+ and OS/2, int 28 for TSRS, and int 15 is for
;  power savings(see PC convertible tech ref)
;

?WIN = 0                ;Not windows;
?PLM = 1                ;DO use pl/m
include cmacros.inc

sBegin data
sEnd data

sBegin code
    assumes cs, code
    assumes ds, data

cProc  GlobalIdle, PUBLIC , <si,di,ds,es>
cBegin  GlobalIdle
	;;; do windows and dos idle calls.
	;;; BX,CX are only filled in as signatures just in case
	;;; some-one needs to know
	mov bx,0FEEDh
	mov cx,0FACEh
	mov ax,1680h
	int 2Fh
	mov bx,0FEEDh
	mov cx,0FACEh
	int 28h
	;;;Power down
	mov ax,4100h ;;; wait until any event
	xor bx,bx    ;;; no time limit
	int 15h
	
cEnd  GlobalIdle
sEnd   code
end
