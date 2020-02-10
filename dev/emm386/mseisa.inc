
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:    EMM386 - MICROSOFT Expanded Memory Manager 386
;
;   Module:   mseisa.inc
;
;  $chk_ms_xmm260:  This routine checks for the Microsoft HIMEM.SYS which
;		     was released with Windows 3.0.  This HIMEM, XMS 2.0 and
;		     internal release 2.60, uses the COMPAQ Built In Memory (BIM)
;		     without allocating it.  In order to prevent COMPAQ's
;		     utilities from allocating BIM, this routine is used to
;		     activate HIMEM.
;
;  (The routine preserves all registers)
;******************************************************************************

$ROMSEG            equ  0F000h
$EISAOFF           equ  0FFD9h
$ROMEISA           label dword
		   dw   $EISAOFF
                   dw   $ROMSEG
$XMMcontrol        dd   0

$chk_ms_xmm260     proc near
    push      es                            ;;
    push      bx                            ;;
    push      ax                            ;;
    les	      bx,[$ROMEISA]		    ;; address for "EISA" string
    cmp	      es:[bx],'IE'		    ;;Q: "EI" part of EISA?
    jne	      $no_XMM			    ;; N: not an EISA machine
    cmp	      es:[bx+2],'AS'		    ;;Q: "SA" part of EISA?
    jne	      $no_XMM			    ;; N: not an EISA machine
    mov       ax,4300h                      ;; function for XMS detect
    int       2fh                           ;; check for XMS driver
    cmp       al,80h                        ;; Q: Is XMS installed?
    jne       $no_XMM                       ;;  N: XMS not in use, not enough
    mov       ax,4310h                      ;; Get XMS routine address
    int       2fh                           ;;
    mov       word ptr cs:[$XMMcontrol],bx  ;; Save XMS routine address
    mov       word ptr cs:[$XMMcontrol+2],es;; (offset), (segment)
    mov       ah,0                          ;; Get XMS version number
    call      cs:[$XMMcontrol]              ;; Call XMM
    cmp       ax,200h                       ;; check if version = 2.00
    jne       $no_XMM                       ;; if yes continue
    cmp       bx,260h                       ;; check if internal rev > 205
    jne       $no_XMM                       ;; if yes continue
    mov       ah,8                          ;; How much extended memory is
    call      cs:[$XMMcontrol]              ;; ..... available in Kbytes?
$no_XMM:                                     ;;
    pop       ax                            ;;
    pop       bx                            ;; restore bx
    pop       es                            ;; restore es
    ret                                     ;;
$chk_ms_xmm260      endp                    ;;
