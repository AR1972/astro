;*
;*	COW : Character Oriented Windows
;*
;*	glock.asm : Global memory allocator - Locking procedures

	.xlist
	include	kernel.inc
	include	galloc.inc
	.list


sBegin	KERNEL
    assumes CS,KERNEL
    assumes DS,NOTHING			;* DS usually points to MOB
    assumes SS,DATA			;* get variables from here


;*	* low level subroutines
externNP    <glock,gunlock>		; GMEM.ASM
externNP    <xhandle>			; GWINTERF.ASM


; The remainder of this file implements the exported interface to the
; global memory manager.
;   HANDLE	far PASCAL LockSegment( WORD );
;   HANDLE	far PASCAL UnlockSegment( WORD );
;   char far *	far PASCAL GlobalLock( HANDLE );
;   BOOL	far PASCAL GlobalUnlock( HANDLE );

cPublic	LockSegment
;   parmW   seg
cBegin	nogen
	call	xhandle 	    ; Get handle
	jz	xhandlex	    ; Ignore invalid or discarded objects
	call	glock
	jmp	short xhandlex
cEnd	nogen ;LockSegment


cPublic	UnlockSegment
;   parmW   seg
cBegin	nogen
	call	xhandle 	    ; Get handle
	jz	xhandlex	    ; Ignore invalid or discarded objects
	call	gunlock
	jmp	short xhandlex
cEnd	nogen ;UnlockSegment


cPublic	GlobalLock
;   parmW   h
cBegin	nogen
lock0:
	call	xhandle 	    ; Call ghandle with handle in DX
	jz	lock1		    ; Ignore invalid or discarded objects
IFDEF DEBUG
	cmp	ch,0FFh 	    ; Debugging check for count overflow
	jne	lock_ok
	cCall	CowAssertFailed
	DB	"GlobalLock: Object usage count overflow$"
lock_ok:
ENDIF ;DEBUG
	call	glock		    ; Increment lock count
lock1:
	xor	ax,ax
	mov	cx,dx
xhandlex:
        dec     ds:[di].gi_lrulock
        pop     di
        pop     ds
        ret     2

cEnd	nogen ;GlobalLock


cPublic	GlobalUnlock
;   parmW   h
cBegin	nogen
	call	xhandle 	    ; Call ghandle with handle in DX
	jz	xhandlex	    ; Ignore invalid or discarded objects
IFDEF DEBUG
	cmp	ch,00h		    ; Debugging check for count underflow
	jne	unlock_ok
	cCall	CowAssertFailed
	DB	"Global Unlock underflow$"
unlock_ok:
ENDIF ;DEBUG
	call	gunlock
	mov	ax,cx
	jmp	xhandlex

cEnd	nogen ;GlobalUnlock


sEnd	KERNEL

	END
