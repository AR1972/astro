;**************************************************************************
; _FindMSMouse Source Code
;
; This routine determines if the version number passed to it is newer than
; the currently install Microsoft mouse driver.  If it isn't then 0 is
; returned otherwise a non-zero value is returned.
;
; This routine is intended to be used by applications and languages at
; Microsoft to determine if they should install a mouse driver during
; their setup process.
;
; To determine if a Microsoft mouse driver is installed the routine
; goes through the following logic:
;
;	Is a Mouse Driver Installed?
;	  NO - return 0
;	Is it a Microsoft mouse driver?
;	  NO - return 0
;	Return non-zero value
;
; This code can only detect Microsoft mouse drivers greater or equal to 
; version 3.00.  This should be sufficient.
;
; To call this routine from C use the following statement:
;
;	FindMSMouse(&VersionNumber, &MouseType);
;
;    where VersionNumber is a hexidecimal value containing the version
;    number of the Microsoft driver found.  The upper half of the word
;    contains the major version number and the lower half, the minor
;    version number (e.g. version 6.25 will be 0x625).  MouseType is 
;    the type of driver found.
;
;		MouseType	Type of Driver
;		---------	--------------
;		    0		Regular Mouse Driver
;		    1		Integrated Mouse Driver
;		    2		HP Mouse Driver
;
;    This function returns FALSE (0) if no Microsoft Mouse driver is
;    found and a non-zero value if a MS driver is found.
;
;**************************************************************************

;**************************************************************************
;   E Q U A T E S
;**************************************************************************

FALSE	equ	0
TRUE	equ	1

IRET_OPCODE	equ	0CFh		; op-code for IRET


;  Magic values following copyright notice in our drivers

MOUSE3_THRU_6	equ	5564h		; Mouse versions 3.00 => 6.00
MOUSE6_THRU_624	equ	557Ch		; Mouse versions 6.01Z => 6.24
MOUSE625	equ	0E806h		; Mouse version 6.25
MOUSE7		equ	0EB02h		; Mouse versions 6.26 => 7.04
INTMOUSE	equ	0800h		; Integrated mouse
;  Versions greater than 7.04 use their version number as the magic value
;  (e.g. Version 7.05 has 0507 as its magic value)


MS_STRING1_LEN	equ	23		; Length of MS_String1
MS_STRING2_LEN	equ	14		; Length of MS_String2


_TEXT	segment para public 'CODE'

;**************************************************************************
;   D A T A
;**************************************************************************

;  Copyright notice found in all drivers.  It is split in two because
;  drivers before 7.00 don't have the '-19??' following '1983' while
;  all drivers from 7.00 on, do have this extra text.
MS_String1	db	" This is Copyright 1983"
MS_String2	db	" Microsoft ***"
Version		dw	0
MouseType	dw	0


;**************************************************************************
;   C O D E
;**************************************************************************

	assume	cs:_TEXT, ds:nothing, es:nothing, ss:nothing

	public	_FindMSMouse

;--------------------------------------------------------------------------
; int FindMSMouse(&Version, &MouseType)
;
; Returns:  0 => Don't install mouse driver
;          !0 => OK to install mouse driver
;--------------------------------------------------------------------------

ifdef MODEL_MEDIUM
_FindMSMouse	proc	far
else
ifdef MODEL_COMPACT
_FindMSMouse	proc	near
else
ifdef MODEL_LARGE
_FindMSMouse	proc	far
else
	syntax error, small model not supported.
endif
endif
endif

	push	bp			; Set up for C entry
	mov	bp,sp

	push	ds			; Set DS to CS
	mov	ax,cs
	mov	ds,ax
	assume	ds:_TEXT

;  Get INT 33 vector and verify it is pointing to something
	mov	ax,3533h
	int	21h
	assume	es:nothing
	mov	ax,es			; Verify ES:BX is not 0:0
	or	ax,bx
	jz	TO_NOT_MSDRIVER
	cmp	byte ptr es:[bx],IRET_OPCODE	; Verify ES:BX doesn't point
	jne	@F				;    to an IRET

TO_NOT_MSDRIVER:
	jmp	NOT_MSDRIVER

@@:
;  Get the version number for a 'm' call (needed for pre 6.00 versions 
;  which didn't support function 36 and for version which contained errors)
	sub	di,di
	mov	ax,'m'
	int	33h
	or	di,di			; Is 'm' function call implemented?
	je	GET_ALT_VERSION_NUMBER	;  No - use function 36
	mov	bx,es:[di]		; Get version number
	xchg	bh,bl			; Put Major in BH and Minor in BL
	cmp	bh,3			; Is version < 3.00?
	jb	TO_NOT_MSDRIVER		;  Yes - done
	cmp	bh,5			; Is version > 5.00?
	jbe	CHECK_ID		;  No - skip
	cmp	bh,6			; Is version > 6.01?
	ja	GET_ALT_VERSION_NUMBER	;  Yes - get driver type
	cmp	bl,2
	jb	@F			;  No - continue

GET_ALT_VERSION_NUMBER:
;  Get MOUSE version number and driver type through a function call 36
	sub	bx,bx
	mov	ax,36
	int	33h
	or	bx,bx			; Was function 36 implemented?
	je	TO_NOT_MSDRIVER		;  No - done
	cmp	ch,5			; HP Driver?
	jne	@F			;  No - skip
	mov	word ptr [MouseType],2	; Flag HP Driver 

@@:
;  If the driver identifies itself as 6.01 it may be 6.01 or 6.00 so
;  we need to figure out the difference.
	cmp	bx,601h			; Is it 6.00 or 6.01?
	jne	CHECK_ID		;  No - skip
	cmp	di,1ABh			; Is it really 6.00?
	jne	CHECK_ID		;  No it's 6.01
	sub	bl,bl			; Make it 6.00

CHECK_ID:
	mov	[Version],bx		; Return version number

;  Check the ID string in the driver for our string
	mov	cx,bx			; Save version number
	mov	ax,3533h		; Make sure ES points to MOUSE code
	int	21h
	assume	es:nothing
	mov	bx,cx			; Recover version number
	mov	ax,"**"			; Search first 1000h bytes for
	sub	di,di			;   copyright string (begins with '***')
	mov	dx,800h

CONTINUE_SEARCH:
	mov	cx,dx			; Get length of search
	repne scasw			; Find '***'
	or	cx,cx			; Was '***' found?
	jz	NOT_MSDRIVER		;  No - done
	cmp	es:[di],al		; Skip past all three *'s
	jne	DONT_SKIP
	inc	di

DONT_SKIP:
	mov	dx,cx			; Save remaining length of search
	mov	si,offset _TEXT:MS_String1	; Compare first half of
	mov	cx,MS_STRING1_LEN		;   copyright string
	repe cmpsb
	or	cx,cx			; Was first half there?
	jne	CONTINUE_SEARCH		;  No - search for next '***'
	cmp	bh,7			; Is the version number >= 7
	jb	CHECK_END_OF_STRING	;  No - check last half of copyright
	add	di,5			;  Yes - skip '-19??' characters

CHECK_END_OF_STRING:
	mov	si,offset _TEXT:MS_String2	; Compare last half of
	mov	cx,MS_STRING2_LEN		;   copyright string
	repe cmpsb
	or	cx,cx			; Was last half there?
	jne	CONTINUE_SEARCH		;  No - search of next '***'

	mov	cx,es:[di+1]		; Get Magic number
	cmp	bh,5			; Is version > 5.03?
	ja	VERSION_6		;  Yes - try versions 6+

VERSION_3:
	cmp	cx,MOUSE3_THRU_6	; Was the Magic number there?
	jmp	short BRANCH

VERSION_6:
	cmp	bh,6			; Is version > 6.26?
	jne	VERSION_7		;  Yes - try versions 7+
	or	bl,bl			; Is it version 6.00?
	je	VERSION_3		;  Yes - check 6.00 magic number
	cmp	bl,26h			; Is it version 6.26?
	je	VERSION_626		;  Yes - check 6.26 magic number
	cmp	bl,25h			; Is it version 6.25?
	je	VERSION_625		;  Yes - check 6.25 magic number
	cmp	cx,MOUSE6_THRU_624	; Was the Magic number there?
	jmp	short BRANCH

VERSION_625:
	cmp	cx,MOUSE625		; Was the Magic number there?
	jmp	short BRANCH

VERSION_7:
	cmp	bh,7			; Is version > 7.04?
	ja	VERSION_8		;  Yes - try 7.05+
	cmp	bl,4
	ja	VERSION_8		;  Yes - try 7.05+

VERSION_626:
	cmp	cx,MOUSE7		; Was the Magic number there?

BRANCH:
	je	MSDRIVER_FOUND		; Jif Magic number found

NOT_MSDRIVER:
	sub	ax,ax			; Flag not an MS mouse driver
	jmp	short EXIT		; Done

INTDRIVER:
	mov	word ptr [MouseType],1	; Flag Integrated Driver
	sub	word ptr [Version],900h	; Convert from A.xx to 1.xx
	cmp	cx,INTMOUSE		; Was the Magic number there?
	jmp	BRANCH

VERSION_8:
	cmp	bh,0Ah			; Integrated driver?
	je	INTDRIVER		;  Yes - check its magic number
	mov	cx,es:[di]		; Get version number (Magic number
					;   for versions 7.05+)
	cmp	bh,cl			; Make sure version numbers match
	jne	NOT_MSDRIVER
	cmp	bl,ch
	jne	NOT_MSDRIVER

MSDRIVER_FOUND:
	mov	al,TRUE			; Flag MS mouse driver found

EXIT:
	mov	cx,[Version]
	mov	dx,[MouseType]
	pop	ds			; Clean-up and exit
ifdef MODEL_MEDIUM
	mov	bx,[bp+6]		; Return version number
	mov	[bx],cx
	mov	bx,[bp+8]		; Return driver type
	mov	[bx],dx
else
ifdef MODEL_COMPACT
	mov	bx,[bp+4]		; Return version number
	mov	[bx],cx
	mov	bx,[bp+6]		; Return driver type
	mov	[bx],dx
else
ifdef MODEL_LARGE
	push	es
	les	bx,[bp+6]		; Return version number
	mov	es:[bx],cx
	les	bx,[bp+10]		; Return driver type
	mov	es:[bx],dx
        pop	es
else
	syntax error, small model not supported
endif
endif
endif
	pop	bp
	ret

_FindMSMouse	endp

_TEXT	ends

	end

