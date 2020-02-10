;*
;*	COW : Character Oriented Windows
;*
;*	util.asm : user utilities (DOS 3 & 5)

	title	util - low level utilities for COW USER

.xlist
	include	user.inc
.list


sBegin USER_CORE
    assumes CS,USER_CORE
    assumes DS,DATA
    assumes SS,DATA

subttl memset
page
;----
;
;   MemSetW - Memory set Words
;
;   Purpose:
;	Sets the first cnt words of dest to the word value w
;
;   void MemSetW(WORD ps, WORD off, WORD w, unsigned cnt);
;
;   Args:
;	ps:off - the Destination buffer.
;	w - The character to fill the buffer with
;	cnt - The count of the number of bytes to fill.
;
;   Returns:
;	'non'
;
;----

IFDEF WINDOW_OVERLAP

cProc  MemSetW,<PUBLIC, NEAR>,<DI>
    parmW	psDest
    parmW	offDest
    parmW	w
    parmW	cnt
cBegin	MemSetW

	mov	es,psDest	; set es = ds
	mov	di,offDest	; Get destination 
	mov	ax,w		; Get character to fill buffer with
	mov	cx,cnt		; get number of bytes to move
	rep	stosw		; Fill the buffer

cEnd	MemSetW

ENDIF	;WINDOW_OVERLAP

sEnd    USER_CORE
;*****************************************************************************

	END
