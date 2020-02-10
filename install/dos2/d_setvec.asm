	page	,132
;============================================================================
	title	d_setvec - Set Interrupt Vector
;============================================================================
;
;   (C) Copyright MICROSOFT Corp. 1991-1992
;
;   Title:    DOS2.EXE - GUI Portion of DOS Install
;
;   Module:   D_SETVEC - Direct interface to DOS "set interrupt vector" 
;			 system call.
;
;   Version:  0.001
;
;   Date:     Jan 26,1992
;
;   Author:   HKN
;
;============================================================================
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   01/26/92  Original
;
;============================================================================

include model.inc
include dpmi.inc

.data

public ProtDskParmsPtr	
public RealDskParmsPtr
public DptPtr	
public PMPtr		
public RMPtr		


ProtDskParmsPtr	dd	?
RealDskParmsPtr	dd	?
DptPtr	label	dword
PMPtr		dw	?
RMPtr		dw	?

.code

page

;===========================================================================
;
;	Procedure 	: _dos_setvect
;
;	Input		: interrupt = interrupt number
;			  handler = address of new handler
;			  ProtDskParmsPtr = Sel:Off of the int 1eh vec at 
;			  program startup
;			  RealDskParmsPtr = Seg:Off of the int 1eh vec at 
;			  program startup
;			
;
;	Output		: Sets the int vector.
;
;	Description:
;		This is provided to serve the needs of the _dos_setvec calls
;	made by the common libraries used by the DOS setup program. The 
;	_dos_setvec calls are made to set and reset the int 1eh vector 
;	when doing general disk io. It is required that the real mode int 
;	1eh vector be changed to reflect the data that is pointed to by 
;	the input parm 'handler'. Since this input parm is in the form of
;	a sel:off we copy 13bytes (length of the DPT) into our DOS buffer
;	that was allocated at program startup (PMPtr=Sel:off of real mode
;	buffer  and RMPtr=Seg:off of real mode buffer) and set the real mode 
;	int 1eh vector to RMPtr. 
;
;===========================================================================

_dos_setvect	proc	uses ds si es di cx, interrupt:word, handler:dword


	mov	ax, ds
	mov	es, ax			; es = data segment
	mov	ax, interrupt
	lds	dx, handler

	cmp	al, 01eh		; Q: are we seeting the DPT vector
	je	DS1			; Y: continue 
					; N: just do int 21
	mov	ah, 25h
	int	21h
	jmp	short DSdone

DS1:
	;
	; If we are just resetting the 1eh vector back to the original 
	; value we just do an int 21h and set the real mode int 1eh vec
	; back to RealDskParmsPtr. else we must copy the contents pointed
	; to by 'handler' to our local real mode buffer and set the int
	; 1eh vec to our local real mode buffer.
	;
	cmp	dx, word ptr es:[ProtDskParmsPtr]
	jne 	DS2			
	mov	si, ds
	cmp	si, word ptr es:[ProtDskParmsPtr + 2]
	jne	DS2			

	mov	ah, 25h
	int	21h

	mov	bl, 01eh
	mov	dx, word ptr es:[RealDskParmsPtr]
	mov	cx, word ptr es:[RealDskParmsPtr + 2]
	mov	ax, SET_REALMODE_INTVEC
	int 	DPMI

DSdone:
	xor	ax,ax			;routine is supposed to return void...
	ret

DS2:
	;
	; We must now copy the contents of the buffer which is of length 
	; equal to DSK_PARAM_LEN defined in disk_io.h = 13. Then we do
	; a DPMI Set Real Mode Interrupt Vector call.
	;
	push	es
	mov	cx, 13
	mov	si, dx		; ds:si -> user buffer
	xor	di, di
	mov	es, es:PMPtr	; es:di -> our DPT buffer
rep	movsb
	pop	ds	    	; ds = data segment
	
	mov	cx, RMPtr
	xor	dx, dx		; cx:dx = real mode address of our DPT buffer
	mov	bl, al
	mov	ax, SET_REALMODE_INTVEC
	int 	DPMI

	mov	dx, es
	mov	ds, dx
	xor	dx, dx
	mov	ax, 251eh
	int	21h
	jmp	short DSdone
	
_dos_setvect	endp

;===========================================================================
;
;	Procedure	: GetDPTVec
;
;	Input		: None
;	Output		: Initializes the foll:
;			  ProtDskParmsPtr to Sel:off of current int 1eh vec
;			  RealDskParmsPtr to Seg:off of current int 1eh	vec
;			  PMPtr to Sel:off of real mode buffer.
;			  RMPtr to Seg:off of real mode buffer
;
;===========================================================================

GetDPTVec	proc 	uses dx bx es

	mov	ax, 0351eh
	int	21h
	mov	word ptr ProtDskParmsPtr, bx
	mov	word ptr ProtDskParmsPtr + 2, es

	mov	bl, 01eh
	mov	ax, GET_REALMODE_INTVEC
	int	DPMI
	mov	word ptr RealDskParmsPtr, dx
	mov	word ptr RealDskParmsPtr + 2, cx
	ret

GetDPTVec 	endp

	end
