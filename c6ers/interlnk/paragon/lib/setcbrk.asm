;***
;* $Workfile:   setcbrk.asm  $
;* $Revision:   1.0  $
;*   $Author:   Dave Sewell  $
;*     $Date:   28 Apr 1989 16:52:14  $
;*
;* Routine to set Control Break address.
;*****************************************************************************

INCLUDE     dos.mac

	    DATASEG
cbreak	    DB	    ?	    ;Initial state of Control-Break checking
	    DATAEND

	    TEXTSEG

cbrk	    PROC    FAR

	    iret	    ;Ignore Control-C

cbrk	    ENDP

PUBPROCDEF  setcbrk
;	extern void cdecl setcbrk(void);
	    procent
	    mov     ax, 3300H
	    INT     21H
	    mov     cbreak, dl
	    push    ds
	    mov     dx, OFFSET cbrk
	    push    cs
	    pop     ds
	    mov     ax, 2523H	    ;Set INT 23 vector
	    int     21H

	    mov     ax, 3301H
	    mov     dl, 0
	    INT     21H 	    ;Disable Control-Break handling

	    pop     ds
	    procret

PUBPROCEND  setcbrk

PUBPROCDEF  resetcbrk
;	extern void cdecl resetcbrk(void);

	    procent

	    mov     ax, 3301H
	    mov     dl, cbreak
	    INT     21H
	    procret

PUBPROCEND  resetcbrk

	    TEXTEND

	    END
