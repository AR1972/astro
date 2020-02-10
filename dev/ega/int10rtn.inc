	subttl	Int10Routine
	page

;
; Int10Routine - int 10h ISR dispatcher for EGA.SYS
;
;	As THE entrypoint for EGA.SYS, this routine must be as efficient
;	as possible.  For this reason, there is some duplication of code for
;	each possible thread of execution.  This is faster however, than
;	inserting numerous cmp's and conditional jumps in a single thread.
;
;	Hardware ISR's may call GetInBiosFlag (function 9, subfunction 4)
;	during their initialization to obtain a pointer to the fInBIOS flag
;	via es:bx.  They may subsequently test it themselves for a non-zero
;	value to see if they risk re-entering a video bios call.
;
;	This routine enables interrupts for all of EGA.SYS.  Function calls
;	that cannot tolerate this must explicitly cli themselves.  Also,
;	direction flag is cleared here to prevent having to do it about 12
;	times all over the place.
;
; ENTRY
;	ah	=  major function code
;	all other regs per function being called
; EXIT
;	per function being called
; DESTROYS
;	none, for this ISR itself
;	otherwise, per function being called
;

	assume	cs:CODE, ds:nothing, es:nothing, ss:nothing

Int10Routine	proc	far

	sti
	cmp	ah,EGAShadowStart 	; If not one of our functions,
	jb	NotMine 		;  go check BIOS functions
	cmp	ah,EGAShadowEnd
	ja	ExitInt10
.errnz	(MaxBIOSCall gt EGAShadowEnd)	; Verify we need not go NotMine
	cld
	push	ax			; Many procs depend on this
	push	si			; stack structure!
	push	ds
	mov	al,ah			; Get subfunction into al
	and	ax,000Fh		; Reset ah and mask for dispatch
	shl	ax,1			; *2 for word addresses
	mov	si,ax
	mov	ax,cs
	mov	ds,ax			; DS = CS
	assume	ds:CODE
	call	[MyCallTable][si] 	; Dispatch
	assume	ds:nothing
	pop	ds			; Procs don't need to save these
	assume	ds:nothing
	pop	si			; regs since we restore them
	pop	ax
	iret

SkipInt10:
	assume	ds:nothing
	pop	si			; Restore all regs
	pop	ax			; For hasty exit

ExitInt10:
	assume	ds:nothing
	cli
	jmp	[pOldInt10]		; Where to go if nothing doing

NotMine:
	assume	ds:nothing

if	CallTableNeeded
	cmp	ah,MaxBIOSCall		; If not a function we shadow,
else	; NOT CallTableNeeded
	or	ah,ah			; If not SetMode call,
endif	; NOT CallTableNeeded

	ja	ExitInt10		; just give it to old int 10h
	mov	[SaveAX],ax		; Used by SetMode

if	CallTableNeeded
	or	ah,ah			; Is it a mode init function?
	jnz	@F			; No - skip
endif	; CallTableNeeded

	cmp	[fVga],ah		; Do we have a VGA Card?
	jne	HaveSetMode		; Yes - skip

@@:
	push	ax
	push	si

if	CallTableNeeded
	mov	al,ah			; Get function into al
	cbw				; Reset ah for the dispatch
.errnz	(MaxBIOSCall gt 07Fh)		; Insure CBW is allowed
	mov	si,ax
	mov	al,[biosmode]		; Get current mode
	and	al,01111111b		; Reset noclear bit
	cmp	al,[biostable][si]	; Do we shadow for this mode?
	jb	SkipInt10		; No, GET OUT
	shl	si,1			; *2 for word addresses
endif	; CallTableNeeded

	push	ds
	push	cs
	pop	ds			; DS = CS
	assume	ds:CODE
	inc	[fInBIOS]		; Show video bios is busy
	cld

if	CallTableNeeded
	mov	ah,[biosinfo]		; Get static copy of info byte
	call	[BIOSCallTable][si]	; Dispatch with al=mode/ah=info
else	; NOT CallTableNeeded

ifndef	SYS

PatchSetMode2	equ	$ + 1		; Need to patch because of relocation

endif	; NOT SYS

	call	SetMode
endif	; NOT CallTableNeeded

	assume	ds:nothing
	pop	ds
	assume	ds:nothing
	pop	si
	pop	ax

if	CallTableNeeded
	or	ah,ah			; Watch for setmode
	jnz	@F
endif	; CallTableNeeded

HaveSetMode:
	assume	ds:nothing

if	CallTableNeeded
	or	ah,ah			; Set zero flag
endif	; CallTableNeeded

	mov	[biosmode],al		; Save the mode before we do it

@@:

if	CallTableNeeded
	pushf				; Save flags
endif	; CallTableNeeded

	pushf				; Fake an interrupt
	cli				; (do a good job! -JTP)
	call	[pOldInt10]

if	CallTableNeeded
	popf				; Restore flags
	jz	@F			; Branch if set mode
	dec	[fInBIOS]		; Record that video bios is done
	iret

@@:
endif	; CallTableNeeded

	push	cx
	push	ax			; Currently, same stack as
	push	si			; above is NOT required since SetMode
	push	ds			; gets video mode from SaveAX
	MOV	CX,CS
	MOV	DS,CX			; DS = CS
	assume	ds:CODE

if	CallTableNeeded
	mov	cx,word ptr [CRTCRegs][10] ; Must compute actual cursor
	xchg	cl,ch			; Change to proper format

ifndef	SYS

PatchSetCursorType	equ	$ + 1	; Need to patch because of relocation

endif	; NOT SYS

	call	SetCursorType
endif	; CallTableNeeded

	cmp	[fVga],FALSE		; If VGA not present,
	je	@F	 		;  extended processing done
	CLD				; Do searches forward

ifndef	SYS

PatchSetMode	equ	$ + 1		; Need to patch because of relocation

endif	; NOT SYS

	call	SetMode			; Correct CRT controller addr
	assume	ds:nothing

@@:
	pop	ds
	assume	ds:nothing
	pop	si
	pop	ax
	pop	cx
	dec	[fInBIOS]		; Record that video bios is done
	iret

ifndef	Sys

Int10RoutineLength	equ	($ - Int10Routine)

ifndef	OS2

if2	; Pass 2 of the assembler
.errnz		($ - Int2FHandler)	; Keep these together
endif	; Pass 2 of the assembler

endif	; NOT OS2

endif	; NOT Sys

Int10Routine	endp

ifndef	OS2

;
; This handler is used to allow new copies to communicate with a previously
;	installed copy.  We support:
;
;	Function 0 - Return AL = 0FFH indicating installed
;	Function 6 - Return version number
;		CH = Major version number
;		CL = Minor version number
;		DL = Beta version number (or 0 for no Beta version)
;	Function 7 - Return end of installed code in paragraphs
;		CX = Paragraph value of code end (rounded up)
;		SI = Code segment (side-effect)
;	Function 8 - Return INT 2FH vector
;		CX = Offset of current vector
;		DX = Segment of current vector
;		DI = Offset of original vector
;		ES = Segment of original vector
;	Function 9 - Return INT 10H vector
;		CX = Offset of current vector
;		DX = Segment of current vector
;		DI = Offset of original vector
;		ES = Segment of original vector
;
;	All functions return BX = 'AH' for COM driver and BX = 'TV' for SYS
;		driver.  See cute comments in equate section.
;

	assume	cs:code, ds:nothing, es:nothing, ss:nothing

Int2FHandler	proc	far

	cmp	ah,[INT2FNumber]	; Is this call for me?
	jne	NotMyINT2FNumber	; No - skip
	cmp	al,MaxValidFunction	; Is it a valid function call?
	ja	Int2FHandlerEnd		; No - skip

ifdef	sys
	mov	bx,Int2FPatternSys	; Return our special pattern
else	; NOT sys
	mov	bx,Int2FPatternCom	; Return our special pattern
endif	; NOT sys

.ERRNZ	GetInstalledState		; cmp al,GetInstalledState
	or	al,al			; Is it get installed state function?
	jz	GetStateFunction	; Yes - skip
	cmp	al,GetVersionNumber	; Should we return version number?

ifdef	sys
	jne	Int2FHandlerEnd		; No - skip
else	; NOT sys
	jne	@F			; No - skip
endif	; NOT sys

	mov	cx,(MajVersion shl 8) OR MinVersion ; Return major,minor

ifdef	BetaVersion
	mov	dl,BetaVersion		; Return Beta version
else	; NOT BetaVersion
	xor	dl,dl			; Return no Beta version
endif	; NOT BetaVersion

ifndef	sys
	iret				; All done

@@:
	cmp	al,GetCodeLength	; Should we return code length?
	jne	@F			; No - skip
	mov	cx,offset EndOfResidentCode + 15 ; Get end of the installed code
	shr	cx,1			; Convert to paragraphs
	shr	cx,1			; "
	shr	cx,1			; "
	shr	cx,1			; "
	mov	si,cs			; Can't add CS directly
	add	cx,si			; Add in segment base
	iret				; All done

@@:
	cmp	al,GetINT2F		; Should we return vector?
	jne	@F			; No - skip
;	mov	cx,offset Int2FHandler	; Return offset of current vector
	mov	cx,offset FCB1 + Int10RoutineLength ; Return offset of
					; current vector
	mov	dx,cs			; Return segment of current vector
	les	di,[pOldINT2F]		; Return original vector
	assume	es:nothing
	iret				; All done

@@:
	assume	es:nothing
	cmp	al,GetINT10		; Should we return vector?
	jne	Int2FHandlerEnd		; No - skip
;	mov	cx,offset Int10Routine	; Return offset of current vector
	mov	cx,offset FCB1		; Return offset of current vector
					; (Because Int10Routine is relocated)
	mov	dx,cs			; Return segment of current vector
	les	di,[pOldINT10]		; Return original vector
	assume	es:nothing
endif	; NOT sys

Int2FHandlerEnd:
	assume	es:nothing
	iret				; All done

GetStateFunction:
	not	al			; Say I'm here
	iret				; All done

NotMyINT2FNumber:
	cmp	ah,16h
	jne	Chain_To_Next_2F
	cmp	al,05h
	je	Do_Startup_Instance_Stuff

Chain_To_Next_2F:
	jmp	[pOldINT2F]		; Continue down the chain

Do_Startup_Instance_Stuff:
	pushf
	call	[pOldINT2F]		; Continue down the chain
	assume	es:nothing
	mov	word ptr [Win386_Startup_Info].SIS_Next_Dev_Ptr[0],bx
	mov	word ptr [Win386_Startup_Info].SIS_Next_Dev_Ptr[2],es
	mov	bx,cs
	mov	es,bx
	assume	es:code
	mov	bx,offset Win386_Startup_Info
	iret

ifndef	Sys

Int2FRoutineLength	equ	($ - Int2FHandler)

.errnz		(OpIntRoutineLength - (Int10RoutineLength + Int2FRoutineLength))

endif	; NOT Sys

Int2FHandler	endp

endif	; NOT OS2
