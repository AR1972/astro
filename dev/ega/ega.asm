	page	56,132
	name	EGASYS
	title	EGA Screen Device Driver
	subttl	Header

IFDEF	OS2
	.286				; Oh goody, can use 80286 instructions
ELSE	; NOT OS2
	.8086				; Have to support everything
ENDIF	; NOT OS2

;
; I N C L U D E S
;

	include	ega.inc

;
; C O D E
;

IFDEF	OS2				; Start of OS/2 functions

;
; DoSave - Save state of EGA
;
; ENTRY
;	ds = cs
;
; EXIT
;	ax = 0, operation complete
;
; DESTROYS
;	bx, si
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

DoSave		proc	near

	mov	ax,cs
	mov	es,ax
	assume	es:CODE
	mov	ax,0F901h		; Select new context subfunction
	jmp	short DoCommon

DoSave		endp

;
; DoRestore - Restore state of EGA
;
; ENTRY
;	ds = cs
;
; EXIT
;	ax = 0, operation complete
;
; DESTROYS
;	bx, si, ds
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

DoRestore	proc	near

	mov	ax,cs
	mov	es,ax
	assume	es:CODE
	mov	ax,0F902h		; Select new context subfunction

DoCommon:
	mov	bx,offset ContextCopy
	int	10h			; Restore from ContextCopy

if2	; Pass 2 of the assembler
.errnz		($ - StatusComplete)	; Drop into StatusComplete
endif	; Pass 2 of the assembler

DoRestore	endp

ENDIF	; OS2

;
; StatusComplete - Set completion status
;
; ENTRY
;	none
;
; EXIT
;	ax = 0, request completed
;
; DESTROYS
;	None
;

	assume	cs:CODE, ds:nothing, es:nothing, ss:nothing

StatusComplete	proc	near

	sub	ax,ax			; AX = 0, complete
	ret

StatusComplete	endp

ENDIF	; SYS

	subttl	Miscellaneous Code
	page

;
; UpdateCRTCMap - get the latest values for certain readable CRTC regs
;
;	Called by all routines that return register values to the user,
;	this function updates the current CRTC shadow map with the latest
;	values of the readable CRTC StartAddress (00Ch, 00Dh) and
;	CursorPosition (00Eh, 00Fh).
;
;	Of the numerous EGA regs that are write only, the CRTC contains a
;	handful of readable regs.  Normally, for readable regs like Input
;	Status 1, EGA.SYS requires the user keep track of its value.  But if
;	the goal is for the user of EGA.SYS to be able to rely 100% on the
;	shadow maps, we must properly update readables that we do return.
;	Note that the CRTC lightpen regs do not fall into this category,
;	since we shadow writes to that index, which is really the vertical
;	sync start/stop regs.
;
; ENTRY
;	none
; EXIT
;	none
; DESTROYS
;	AX, SI (if VGA)
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

UpdateCRTCMap	proc	near

	push	dx			; Save environment
	mov	dx,[PortTable][0].prPortAddr
	mov	ax,0C0Dh
	pushf				; Save interrupt flag state
	CLI				; Disable interrupts
	out	dx,al			; Address CRTCStart high
	IOdelay
	inc	dx
	in	al,dx			; Get CRTCStart high
	IOdelay
	dec	dx
	xchg	al,ah
	out	dx,al			; Address CRTCStart low
	IOdelay
	inc	dx
	in	al,dx			; Get CRTCStart low
	IOdelay
	mov	word ptr [CRTCRegs][00Ch],ax ; Stash low:high
	dec	dx
	mov	ax,00E0Fh
	out	dx,al			; Address CRTCCursor pos high
	IOdelay
	inc	dx
	in	al,dx			; Get CRTCCursor pos high
	IOdelay
	dec	dx
	xchg	al,ah
	out	dx,al			; Address CRTCCursor pos low
	IOdelay
	inc	dx
	in	al,dx			; Get CRTCCursor pos low
	popf				; Restore interrupt flag state
	mov	word ptr [CRTCRegs][00Eh],ax ; Stash low:high
	cmp	[fVga],false		; On VGA card?
	je	UpdateCRTCMapDone	; No - skip
	xor	si,si			; Don't change default save maps
	push	bx			; Save environment
	push	cx			; Save environment
	call	ReadVGARegs
	mov	[fPalette],cl		; fPalette = FALSE
	pop	cx			; Restore environment
	pop	bx			; Restore environment

UpdateCRTCMapDone:
	pop	dx			; Restore environment
	ret

UpdateCRTCMap	endp

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

ReadVGARegs	proc	near

;
; Save Miscellaneous Output Register
;

	MOV	DX,MiscOutputRegR	; Get Miscellaneous Output Register
	IN	AL,DX			; Get current state of the register
	IOdelay
	MOV	[MiscOutReg],AL		; value
	or	si,si			; Change default save map?
	jz	@F			; No - skip
	MOV	[DefMiscOutReg],AL	; Save initial

;
; Save Feature Control Register
;

@@:
	MOV	DL,FeatureCtrlReg AND 0FFH ; Get Feature Control Register
	IN	AL,DX			; Get current state of the register
	MOV	[FeatureReg],AL		; value
	or	si,si			; Change default save map?
	jz	@F			; No - skip
	MOV	[DefFeatureReg],AL	; Save initial

;
; Save Sequencer Registers
;

@@:
	MOV	CX,NumSeqRegs		; Initialize 5H registers
	MOV	DL,(EGA_BASE + SEQ_ADDR) AND 0FFH ; Get Sequencer Register
	XOR	BX,BX			; Code to select reset reg
	pushf				; Save interrupt flag state

SaveSequencerRegs:
	MOV	AL,BL			; Get index
	CLI				; Disable interrupts
	OUT	DX,AL			; Send index to the Address Register
	IOdelay
	INC	DX			; Choose read only register
	IN	AL,DX			; Get current state of the register
	STI				; Enable interrupts
	DEC	DX			; Choose index register
	MOV	[SeqRegs][BX],AL	; value
	or	si,si			; Change default save map?
	jz	@F			; No - skip
	MOV	[DefSeqRegs][BX],AL	; Save initial

@@:
	INC	BX			; Move to the next index register
	LOOP	SaveSequencerRegs	; Loop back and initialize another reg

;
; Save Graphics Controller Registers
;

	MOV	BX,CX			; Code to select enable set/reset reg
	MOV	CL,NumGraphicsRegs	; Initialize 9H registers
	MOV	DL,(EGA_BASE + GRAF_CONT) AND 0FFH
					; Get Graphics Control Addr Reg

SaveGraphicsContRegs:
	MOV	AL,BL			; Get index
	CLI				; Disable interrupts
	OUT	DX,AL			; Send index to the Address Register
	IOdelay
	INC	DX			; Choose read only register
	IN	AL,DX			; Get current state of the register
	STI				; Enable interrupts
	DEC	DX			; Choose index register
	MOV	[GraphicsRegs][BX],AL	; value
	or	si,si			; Change default save map?
	jz	@F			; No - skip
	MOV	[DefGraphicsRegs][BX],AL ; Save initial

@@:
	INC	BX			; Move to the next index register
	LOOP	SaveGraphicsContRegs	; Loop back and initialize another reg

;
; Save Attribute Controller Registers
;

	MOV	BX,CX			; Code to select Palette
					; Register and keep video disabled
	MOV	CL,NumAttrRegs		; Initialize 15H registers

SaveAttributeContRegs:
	MOV	DL,BYTE PTR [PortTable][5 * SIZE PortRec].PRPortAddr
	CLI				; Disable interrupts
	IN	AL,DX			; Initialize flip-flop to select address
	MOV	AL,BL			; Get index
	CMP	AL,10H			; Past palette registers?
	JB	PaletteCheckDone	; No - skip
	OR	AL,PaletteAddressSource	; Code to enable video
	jmp	short GetAttrReg

;
;	We are stuck.  In order to correctly read the Palette registers
;	on a VGA, the Palette address source bit (bit 5 of the Attribute
;	Address Register) needs to be 0, but making this 0 will disable
;	video.  Then after reading the Palette registers, we need to
;	re-enable video.  But this happens so often that the screen
;	flashes.  So, we can only read these registers at init time.
;	Only loss of functionality is if the palette registers get
;	out of sync with the shadow maps after init time.
;

PaletteCheckDone:
	cmp	[fPalette],ch		; Did user request the palette regs?
	jne	GetAttrReg		; Yes - skip
	or	si,si			; Init?
	jz	NextAttrReg		; No - skip

GetAttrReg:
	MOV	DL,AttCtrlAddrReg AND 0FFH ; Get Attribute Control Address Reg
	OUT	DX,AL			; Send index to the Address Register
	IOdelay
	INC	DX			; Choose read only register
	IN	AL,DX			; Get current state of the register
	MOV	[AttrRegs][BX],AL	; value
	or	si,si			; Change default save map?
	jz	NextAttrReg		; No - skip
	MOV	[DefAttrRegs][BX],AL	; Save initial

NextAttrReg:
	STI				; Enable interrupts
	INC	BX			; Move to the next index register
	LOOP	SaveAttributeContRegs	; Loop back and initialize another reg

;
; Save CRT Controller Registers
;

	MOV	BX,CX			; Code to select horizontal total reg
	MOV	CL,NumCRTCRegs		; Initialize 19H registers
	MOV	DL,BYTE PTR [PortTable][5 * SIZE PortRec].PRPortAddr
	CLI				; Disable interrupts
	IN	AL,DX			; So that we get back to index again
	IOdelay
	SUB	DL,6			; Get CRT Controller Address Reg

SaveCRTContRegs:
	CLI				; Disable interrupts
	MOV	AL,BL			; Get index
	OUT	DX,AL			; Send index to the Address Register
	IOdelay
	INC	DX			; Choose read only register
	IN	AL,DX			; Get current state of the register
	STI				; Enable interrupts
	DEC	DX			; Choose index register
	MOV	[CRTCRegs][BX],AL	; value
	or	si,si			; Change default save map?
	jz	@F			; No - skip
	MOV	[DefCRTCRegs][BX],AL	; Save initial

@@:
	INC	BX			; Move to the next index register
	LOOP	SaveCRTContRegs		; Loop back and initialize another reg
	popf				; Restore interrupt flag state
	ret

ReadVGARegs	endp

	page

;
; BrstDet - determine number of scan lines for raster
;
;	BrstDet, similar to the IBM function by the same name, determines
;	whether the current raster should be 200 or 350 scanlines based on
;	the switch settings on the rear of the EGA card.  In a nutshell,
;	switch settings 0011 or 1001 indicate 350 lines, otherwise 200 lines.
;
; EXIT
;	cy	=  200 scanlines
;	nc	=  350 scanlines
; DESTROYS
;	None
;

	assume	cs:CODE, ds:nothing, es:nothing, ss:nothing

BrstDet 	proc	near

	push	ax
	mov	al,[biosinfo3]		; Get feature and switch info
	and	al,00001111b		; Mask for switches
	cmp	al,00001001b		; Most common config
	je	@F
	cmp	al,00000011b		; Less common
	je	@F
	stc				; Else set carry

@@:
	pop	ax
	ret

BrstDet 	endp

	page

;
; MakeBase - find proper video params for given mode
;
;	MakeBase, similar to the IBM function by the same name, will return
;	a pointer to the correct table of video parameters to use when
;	initializing the EGA for a given mode.	The root of the list of tables
;	is derived from the ParmPtr in the SavePtr table.
;
; ENTRY
;	ah	=  video mode
;	ds	=  0
; EXIT
;	es:si	-> base of correct parameter table
; DESTROYS
;	ax, flags
;

	assume	cs:CODE, ds:INTVEC, es:nothing, ss:nothing

MakeBase	proc	near

	les	si,[lpSavePtr]		; Load up SavePtr
	assume	es:nothing
	les	si,es:[si]		; Load up ParmPtr
	assume	es:nothing
	test	[Info],01100000b	; If 64K video memory,
	jz	mb64K			;  skip special graphics tests
	add	si,440h			; Bump to alt 640x350x1
	cmp	ah,0Fh 			; If this is what we want,
	je	mbX			;  we are done
	add	si,40h			; Bump to alt 640x350x4
	cmp	ah,10h 			; If this is what we want,
	je	mbX			;  we are done

;
; I assume that the special VGA modes 11h, 12h, and 13h are
;	contiguous and lie after the last EGA table in memory, but
;	I don't know this for sure.
;

	add	si,140h			; Bump for VGA modes 11h, 12h, 13h
	cmp	ah,11h			; Is it a VGA mode 11h?
	je	mbx			; Yes - skip
	add	si,40h			; Bump for VGA mode 12h
	cmp	ah,12h			; Is it a VGA mode 12h?
	je	mbx			; Yes - skip
	add	si,40h			; Bump for VGA mode 13h
	cmp	ah,13h			; Is it a VGA mode 13h?
	je	mbx			; Yes - skip
	sub	si,640h			; Nope, not special graphics

mb64K:
	cmp	ah,3h 			; If not alpha,
	ja	@F			;  skip special alpha tests
	call	BrstDet 		; If not enhanced config,
	jc	@F			;  no adjustment needed
	add	si,4C0h			; Bump to enhanced alpha parms

@@:
	xor	al,al			; Now use mode as final index

ifdef	OS2
	shr	ax,2
else	; NOT OS2
	shr	ax,1			; Funky math does the job
	shr	ax,1
endif	; NOT OS2

	add	si,ax

mbX:
	ret				; ES:SI -> correct table

MakeBase	endp

;-----------------------------------------------------------------------
; HandleIODelay -- Delay for doing close together I/O for hardware
;			to catch it's breath
;
; Entry: None
;
; Exit:  None
;
; Alters: None
;
; Note:	This was implemented because processors (e.g. 80486) keep
;		getting faster and smarter (prefetch and caching)
;		and ruin our old jmp $+2 scheme, so this is a better
;		(i.e. more processor independent) method.
;
;-----------------------------------------------------------------------

	assume	cs:CODE, ds:nothing, es:nothing, ss:nothing

HandleIODelay	proc	near

	push	ax			; Save environment
	in	al,43h			; Delay by reading status register
	in	al,43h			; Delay by reading status register
	pop	ax			; Restore environment
	ret

HandleIODelay	endp

	page

;
; ChangeRegs - reinitialize all shadow maps and EGA state info
;
; ENTRY
;	ah	=  video mode
;	al	=  low byte of CRTC io address
;			Used in EGAChooseMonoColorDisplay macro
; EXIT
;	ds	=  cs
; DESTROYS
;	ax, si
;

	assume	cs:CODE, ds:INTVEC, es:nothing, ss:nothing

ChangeRegs	proc	near

	push	cx			; Save environment
	push	di			; Save environment
	PUSH	BX			; Save environment

ifdef	OS2
	mov	cx,cs			; DS = Code segment
	mov	ds,cx			; Two instructions needed
	assume	ds:CODE
	EGAChooseMonoColorDisplay	; Validate CRTC address
endif	; OS2

	CMP	[fVga],FALSE		; Do we have a VGA Card?
	JE	HandleEGA		; No - skip
	PUSH	DX			; Save environment

ifndef	OS2
	mov	cx,cs			; DS = Code segment
	mov	ds,cx			; Two instructions needed
	assume	ds:CODE
	VGAChooseMonoColorDisplay	; Validate CRTC address
endif	; NOT OS2

	mov	si,1			; Change default save maps
	call	ReadVGARegs
	POP	DX			; Restore environment
	JMP	SHORT FinishChangeRegs

HandleEGA:

ifdef	OS2
	PUSH	ES			; Save environment
	xor	si,si			; DS = Segment 0
	mov	ds,si			; Two instructions needed
	assume	ds:INTVEC
else	; NOT OS2
	assume	ds:INTVEC
	PUSH	ES			; Save environment
	EGAChooseMonoColorDisplay	; Validate CRTC address
endif	; NOT OS2

	call	MakeBase		; ES:SI -> correct table on exit
	assume	es:nothing
	mov	ax,es
	mov	ds,ax			; DS = ES
	assume	ds:nothing
	mov	ax,cs
	mov	es,ax			; ES = CS
	assume	es:CODE
	add	si,5			; Bump past BIOS misc junk
	mov	di,offset StartShadowMaps
	mov	al,3			; Always stash 3 in seq reset reg
	stosb
	mov	cx,(VGAPatch - 2) / 2	; Move data up to extra VGA reg

if	(VGAPatch - 2) AND 1
	movsb				; Odd count adjust
endif	; (VGAPatch - 2) AND 1

rep	movsw
	inc	di			; Bump past VGA reg
	mov	cl,NumGraphicsRegs / 2	; Finish up grph ctrlr regs

if	NumGraphicsRegs AND 1
	movsb				; Odd count adjust
endif	; NumGraphicsRegs AND 1

rep	movsw

	sub	si,(SizeShadowMaps - 2) ; Drop back to beginning
	stosb				; Always stash 3 in seq reset reg
	mov	cl,(VGAPatch - 2) / 2	; Move data up extra VGA reg

if	(VGAPatch - 2) AND 1
	movsb				; Odd count adjust
endif	; (VGAPatch - 2) AND 1

rep	movsw
	inc	di			; Bump past VGA reg
	mov	cl,NumGraphicsRegs / 2	; Finish up grph ctrlr regs

if	NumGraphicsRegs AND 1
	movsb				; Odd count adjust
endif	; NumGraphicsRegs AND 1

rep	movsw
	mov	ax,cs
	mov	ds,ax
	assume	ds:CODE 		; DS = CS
	mov	[PortTable][3 * SIZE PortRec].prNumRegs,NumAttrRegs - 1
					; Adjust size for EGA
	xor	ax,ax			; Get a zero value
	mov	[Gr1PosReg],al		; AX = 0, init Gr1PosRegs
	mov	[DefGr1PosReg],al
	inc	ax
	mov	[Gr2PosReg],al		; AX = 1, init Gr2PosRegs
	mov	[DefGr2PosReg],al
	POP	ES			; Restore environment
	assume	es:nothing

FinishChangeRegs:
	assume	ds:CODE
	xor	ax,ax
	mov	di,ax			; Now clear all dirty flags
	mov	cl,NumPtrData
	mov	bx,offset PortTable	; Start at beginning

@@:
	mov	code:[bx][di].prModFlag,al
	add	di,SIZE PortRec
	loop	@B
	mov	[SingleRegMod],al
	POP	BX			; Restore environment
	pop	di			; Restore environment
	pop	cx			; Restore environment
	ret

ChangeRegs	endp

	subttl	SetMode
	page

;
; SetMode - shadow int 10h SetMode functionality (subfunction 00h)
;
;	This code is executed when a BIOS setmode call is made.  It must
;	predict which mode table the BIOS will use, so the BIOS decision
;	logic is duplicated here.
;
; ENTRY
;	SaveAX		=  original ax on entry with mode in al
;	ds		=  cs
; EXIT
;	none
; DESTROYS
;	ax, si, ds, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

SetMode 	proc	near

	xor	ax,ax
	mov	ds,ax
	assume	ds:INTVEC
	mov	al,byte ptr [EquipFlag]	; Get planar switch setting
	mov	ah,byte ptr [SaveAX]	; AH = mode
	and	ah,01111111b		; Reset noclear bit
	test	[Info],00000010b	; If EGA has color monitor
	jz	smNoMonochrome		;  skip mono tests
	test	al,030h 		; If default video is color,
	jnz	smNoChange		;  get out now
	mov	al,0B4h 		; IO addr = 3Bx
	cmp	ah,00Fh 		; If mono hi-res graphics,
	je	@F			;  do it
	mov	ah,007h 		; Else force mono alpha
	jmp	short @F		;  do it

smNoMonochrome:
	test	al,030h 		; If default video mono,
	jz	smNoChange		;  get out now
	mov	al,0D4h 		; IO addr = 3Dx

@@:
	push	ax			; Save video mode
	call	ChangeRegs		; Initialize shadow maps
	assume	ds:CODE
	pop	ax			; Recover mode
	push	bx			; We need it
	mov	bx,0FF01h		; Assume mono 8x14 font
	cmp	[fVga],FALSE		; Do we have a VGA Card?
	je	@F			; No - skip
	mov	bl,4			; Assume mono 8x16 font

@@:
	cmp	ah,7			; If mode is mono alpha,
	je	smDoFonts		;  do it
	mov	bl,0FFh 		; Assume no fonts (graphics)
	cmp	ah,3			; If mode is graphics,
	ja	smDoFonts		;  do it
	mov	bl,001h 		; Assume 8x14 font
	cmp	[fVga],FALSE		; Do we have a VGA Card?
	je	@F			; No - skip
	mov	bl,004h 		; Assume 8x16 font

@@:
	call	BrstDet 		; If 350 scanlines,
	jnc	smDoFonts		;  do it
	mov	bl,2			; Else show 8x8 font

smDoFonts:
	mov	word ptr [FontBank][0],bx ; Stash bl:bh
	mov	word ptr [FontBank][2],0FFFFh ; Stash 0FFh:0FFh
	pop	bx

smNoChange:
	assume	ds:nothing
	ret

SetMode 	endp

if	CallTableNeeded

	subttl	SetCursorType
	page

;
; SetCursorType - shadow int 10h SetCursorType functionality (subfunction 01h)
;
; ENTRY
;	ch	=  bits 0-4: start scanline for cursor
;		   bits 5-6: visibility attributes
;	cl	=  bits 0-4: stop scanline for cursor
;	ds	=  cs
; EXIT
;	none
; DESTROYS
;	ax, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

SetCursorType	proc	near

	push	bx			; Save environment
	push	ds			; Save environment
	xor	ax,ax
	mov	ds,ax
	assume	ds:INTVEC
	mov	bx,cx			; BX = cursor type
	test	[Info],00001000b	; If EGA not active,
	jnz	sctStash		;  just stash the raw values
	mov	al,bh			; AL = raw start
	and	al,01100000b		; Extract visibility attributes
	cmp	al,00100000b		; If cursor-off flag not set,
	jne	@F			;  continue
	mov	bx,01E00h		; Else emulate cursor off
	jmp	short sctStash

@@:
	test	[Info],00000001b	; If not emulating cursor,
	jnz	sctStash		;  just stash the raw values
	cmp	[CrtMode],3		; If not alpha mode,
	ja	sctNoBump2		;  avoid special alpha tests
	call	BrstDet 		; If 200 lines,
	jc	sctNoBump2		;  avoid 350 line heuristics
	mov	al,005h 		; Magic number
	cmp	bh,al			; If start < 5,
	jb	@F			;  leave it alone
	add	bh,al			; Else bump it up

@@:
	cmp	bl,al			; If stop < 5,
	jb	sctNoBump2		;  leave it alone
	add	bl,al			; Else bump it up

sctNoBump2:
	inc	bl			; Bump stop
	or	bh,bh			; If start = 0,
	jz	@F			;  check for wraparound
	cmp	bl,byte ptr [Points]	; If stop < char cell height,
	jb	@F			;  proceed to final test
	xor	bl,bl			; Else stop = 0

@@:
	mov	ax,bx			; Get copy into ax
	sub	al,ah
	cmp	al,16			; If stop - start != magic
	jne	sctStash		;  continue
	inc	bl			; Else bump stop

sctStash:
	pop	ds			; Restore environment
	assume	ds:CODE
	xchg	bh,bl			; Flip start/stop
	mov	word ptr [CRTCRegs][10],bx ; Stash computed value
	pop	bx			; Restore environment

if2	; Pass 2 of the assembler
.ERRNZ		($ - Ignore)		; Drop into Ignore
endif	; Pass 2 of the assembler

SetCursorType	endp

;
; Ignore - Unsupported device driver calls enter here
;
; ENTRY
;	none
; EXIT
;	none
; DESTROYS
;	none
; NOTE:
;	We are using the RET instruction from SetMode above
;

	assume	cs:CODE, ds:nothing, es:nothing, ss:nothing

Ignore		proc	near		; Enter here just to use the ret

	ret

Ignore		endp

	subttl	ScrollUpDown
	page

;
; ScrollUpDown - shadow int 10h Scroll functionality (subfunctions 06h, 07h)
;
; ENTRY
;	al  =	current video mode
;	ah  =	static copy of ega info byte
;
; EXIT
;	none
; DESTROYS
;	ax, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

ScrollUpDown	proc	near

	push	bp
	mov	bp,sp			; Watch out for stack!
	push	bx
	mov	bh,byte ptr [bp].userAX ; BH = num lines to scroll
	or	bh,bh			; If bh = 0,
	jz	sudSetSeq		;  only change sequencer
	mov	bl,dh			; BL = lower row of window
	sub	bl,ch			; BL = lower row - upper row
	inc	bx			; Make 1-based
	cmp	bl,bh			; If size window = num lines,
	je	sudSetSeq		;  treat as blank whole window
	mov	bl,010h 		; Assume odd/even addressing
	cmp	al,00Fh 		; If mode < 0Fh,
	jb	@F			;  assumption is correct
	test	ah,01100000b		; If EGA memory is 64K,
	jz	@F			;  assumption is correct
	xor	bl,bl			; Else use enhanced default

@@:
	mov	[GraphicsRegs][005h],bl	; Stash the default

sudSetSeq:
	mov	[SeqRegs][002h],00Fh	; Stash another default
	pop	bx
	pop	bp
	ret

ScrollUpDown	endp

	subttl	ReadChar
	page

;
; ReadChar - shadow int 10h ReadChar functionality (subfunction 08h)
;
; ENTRY
;	al  =	current video mode
;	ah  =	static copy of ega info byte
;
; EXIT
;	none
; DESTROYS
;	ax, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

ReadChar	proc	near

	cmp	al,00Fh 		; If mode <  0Fh,
	mov	al,0			; (reuse al and assume default)
	jb	@F			;  no more processing needed
	test	ah,0110000b		; If EGA memory >64K
	jnz	@F			;  no more processing needed
	mov	al,010h 		; Else use crippled default

@@:
	mov	[GraphicsRegs][005h],al	; Put al in r/w mode reg
	ret

ReadChar	endp

	subttl	WriteChar
	page

;
; WriteChar - shadow int 10h WriteChar functionality (subfunctions 09h, 0Ah)
;
; ENTRY
;	al  =	current video mode
;	ah  =	static copy of ega info byte
;
; EXIT
;	none
; DESTROYS
;	ax, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

WriteChar	proc	near

	mov	[GraphicsRegs][003h],0	; Put 000h in data rotate reg
	mov	[SeqRegs][002h],00Fh	; Put 00Fh in map mask reg

scpGetOut:				; Mind if we use your ret?  Thanks.
	assume	ds:nothing
	ret

WriteChar	endp

	subttl	SetCGAPalette
	page

;
; SetCGAPalette - shadow int 10h SetCGAPalette functionality (subfunction 0Bh)
;
;	SetCGAPalette is a quirky function, even on a CGA system.  Note the
;	following points carefully:
;
;	1.  When using bh = 0 to set background/overscan, the call will fall
;	    through to the bh = 1 code because bit 4 is the palette intensity
;	    which must be combined with the current palette selection.	Thus
;	    when making a set background call, you are also implicitly making
;	    a set palette intensity call (subject to constraints that follow).
;
;	2.  In alpha modes, you may only set the overscan color if you are in
;	    a CGA compatible (200 lines, 15KHz) sweep mode.  Attempting to do
;	    this in 350 line alpha will cause display problems and therefore is
;	    a nop in this function.  It is also meaningless to set the palette
;	    and the background color in either alpha sweep mode, and thus is a
;	    nop.
;
; ENTRY
;	bh	=  0: set background/overscan color
;		bl	=  bits 0-3: irgb color for background/overscan
;			   bit	  4: palette intensity
;			   bits 5-7: unused
;	bh	=  1: set foreground palette
;		bl	=  0: set palette 0 (green/red/brown)
;			   1: set palette 1 (cyan/magenta/white)
;	ds	=  cs
; EXIT
;	none
; DESTROYS
;	ax, si, ds, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

SetCgaPalette	proc	near

	xor	ax,ax
	mov	ds,ax
	assume	ds:INTVEC
	cmp	byte ptr [Addr6845],0B4h; If mono active,
	je	scpGetOut		;  get out
	test	[Info],00000010b	; If EGA has no color monitor,
	jnz	scpGetOut		;  get out
	mov	ah,[CrtMode]		; AH = video mode
	mov	al,[CrtPalette] 	; AL = CGA palette byte
	mov	si,cs			; DS = Code segment
	mov	ds,si			; Two instructions needed
	assume	ds:CODE
	push	bx
	or	bh,bh			; If bh != 0,
	jnz	scpPal			;  go set CGA palette colors
	and	al,11100000b		; Clear old backgnd, pal intense
	and	bl,00011111b		; Isolate new backgnd, pal int
	or	al,bl			; AL = new palette byte
	mov	bh,bl			; Get a copy into bh
	shl	bl,1			; Shift intensity into position
	and	bx,0000011100010000b	; And isolate it
					; Isolate rgb in bh
	or	bh,bl			; BH = EGA compatible background
	cmp	ah,3			; If in graphics mode,
	ja	@F			;  do background and overscan
	call	BrstDet 		; If 200 lines,
	jc	scpOverScan		;  do overscan only
	jmp	short scpX		; Else nothing left to do

@@:
	mov	[AttrRegs][000h],bh	; Stash background

scpOverScan:
	mov	[AttrRegs][011h],bh	; Stash overscan
	mov	bl,al			; Recover new palette byte
	and	bl,00100000b		; Isolate palette bit

ifdef	OS2
	rol	bl,3			; And get into bit 0
else	; NOT OS2
	rol	bl,1			; And get into bit 0
	rol	bl,1
	rol	bl,1
endif	; NOT OS2

scpPal:
	cmp	ah,3			; If in alpha mode,
	jbe	scpX			;  don't bother with palettes
	and	bl,00000001b		; Only allow palettes 0 and 1
	and	al,00010000b		; Isolate palette intensity bit
	or	al,bl			; Or intensity into palette
	or	al,2			; Green (2) or cyan (3)
	mov	[AttrRegs][001h],al	; Stash color 1
	inc	ax			; Red (4) or magenta (5)
	inc	ax			; "
	mov	[AttrRegs][002h],al	; Stash color 2
	inc	ax			; Brown/yellow (6) or white (7)
	inc	ax			; "
	mov	[AttrRegs][003h],al	; Stash color 3

scpX:
	pop	bx
	ret

SetCgaPalette	endp

	subttl	WriteDot
	page

;
; WriteDot - shadow int 10h WriteDot functionality (subfunction 0Ch)
;
; ENTRY
;	al  =	current video mode
;	ah  =	static copy of ega info byte
;
; EXIT
;	none
; DESTROYS
;	ax, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

WriteDot	proc	near

	mov	al,0FFh
	mov	[GraphicsRegs][003h],ah	; Put 000h in data rotate reg
	mov	[GraphicsRegs][008h],al	; Put 0FFh in bit mask reg
	mov	[SeqRegs][002h],al	; Put 0FFh in map mask reg
	ret

WriteDot	endp

	subttl	ReadDot
	page

;
; ReadDot - shadow int 10h ReadDot functionality (subfunction 0Dh)
;
; ENTRY
;	al  =	current video mode
;	ah  =	static copy of ega info byte
;
; EXIT
;	none
; DESTROYS
;	ax, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

ReadDot 	proc	near

	push	bx
	mov	bl,003h 		; Assume >64K or modes 0Dh/0Eh
	cmp	al,00Fh 		; If mode < 0Fh,
	jb	@F			;  assumption is correct
	test	ah,01100000b		; If EGA memory > 64K,
	jnz	@F			;  assumption is correct
	dec	bx			; Else use crippled default

@@:
	mov	[GraphicsRegs][004h],bl	; Put in read map reg
	pop	bx
	ret

ReadDot 	endp

	subttl	SetEGAPalette
	page

;
; SetEGAPalette - shadow int 10h SetEGAPalette functionality (subfunction 10h)
;
; ENTRY
;	(Source - Direct quote from IBM Enhanced Graphics Adapter manual)
;   SubFunctions:
;	(AL) = 0               Set individual palette register
;	 BL = Palette register to be set
;	 BH = Value to set
;
;	AL = 1                Set overscan register
;	BH = Value to set
;
;	AL = 2                Set all palette register and overscan
;	ES:BX points to a 17 byte table
;	Bytes 0 - 15 are the palette values, respectively
;	Byte 16 is the overscan value
;
;	AL = 3                Toggle intensity/blinking bit
;		BL - 0        Enable intensity
;		BL - 1        Enable blinking
;
; EXIT
;	none
; DESTROYS
;	ax, si, ds, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

SetEgaPalette	proc	near

	push	bp
	mov	bp,sp
	xor	ax,ax
	mov	ds,ax
	assume	ds:INTVEC
	mov	al,0D4h 		; Assume color
	test	[Info],00000010b	; If EGA has color monitor,
	jz	@F			;  go check Addr6845
	mov	al,0B4h 		; Else try mono

@@:
	cmp	byte ptr [Addr6845],al	; If no agreement with Addr6845,
	jne	sepX0			;  get out
	push	bx
	mov	al,byte ptr [bp].userAX ; AL = original subfunction
	xor	ah,ah			; Allows dec ax instruction
	or	al,al			; If al != 0 (set one reg),
	jnz	@F			;  try next subfunction
	cmp	bl,NumAttrRegs - 1	; If user gave us bogus index,
	ja	sepX1			;  get out or we'll lunch
	mov	al,bh			; Copy data
	xor	bh,bh			; Zero bh for the index
	mov	[AttrRegs][bx],al	; Stash it
	jmp	short sepX1

@@:
	dec	ax			; If al != 1 (set overscan reg),
	jnz	@F			;  try next subfunction
	mov	[AttrRegs][011h],bh	; Stash overscan reg
	jmp	short sepX1

@@:
	push	es
	dec	ax			; If al != 2 (set all regs),
	jnz	@F			;  try next subfunction
	push	cx
	push	di
	mov	ax,es
	mov	ds,ax			; DS = ES
	assume	ds:nothing
	mov	ax,cs
	mov	es,ax			; ES = CS
	assume	es:CODE
	mov	si,dx			; DS:SI -> src table
	mov	di,offset AttrRegs	; ES:DI -> AttrReg array
	mov	cx,8			; Do first 16 bytes
rep	movsw
	inc	di			; Bump past mode control
	movsb				; Stash overscan
	pop	di
	pop	cx
	jmp	short sepX

@@:
	assume	ds:INTVEC, es:nothing
	dec	ax			; If al != 3 (toggle blink),
	jnz	sepX			;  get out

;
; Legal values of bl (0 or 1) should be used to act NOT on the current
; setting of the mode reg, but on the default value pointed to by the parm
; ptr in the save ptr table.  Illegal values for bl cause this reg to be
; restored to the default value given by the parm ptr.	See IBM EGA BIOS
; pg.  148
;

	mov	ah,[CrtMode]		; AH = video mode
	call	MakeBase
	assume	es:nothing
	mov	al,es:[si][51]		; Get default attr mode value
	or	bl,bl			; If bl != 0 (reset bit)
	jnz	@F			;  try next subfunction
	and	al,11110111b		; Reset blink bit
	jmp	short sepStoreBit

@@:
	dec	bl			; If bl still not 0,
	jnz	sepStoreBit		;  give'em a default
	or	al,00001000b		; Set blink bit

sepStoreBit:
	mov	[AttrRegs][010h],al

sepX:
	assume	ds:nothing, es:nothing
	pop	es
	assume	es:nothing

sepX1:
	pop	bx

sepX0:
	assume	ds:INTVEC, es:nothing
	pop	bp
	ret

SetEgaPalette	endp

	subttl	DownloadFont
	page

;
; DownloadFont - shadow int 10h DownloadFont functionality (subfunction 11h)
;
; ENTRY
;	(Source - Direct quote from IBM Enhanced Graphics Adapter manual)
;   SubFunctions:
;	Note: This call will initiate a mode set, completely
;		resetting the video environment but maintaining
;		the regen buffer.
;
;	AL = 00   User alpha load
;		ES:BP - Pointer to user table
;		CX    - Count to store
;		DX    - Character offset into table
;		BL    - Block to load
;		BH    - Number of bytes per character
;	AL = 01   ROM monochrome set
;		BL    - Block to load
;	AL = 02   ROM 8x8 double dot
;		BL    - Block to load
;	AL = 03   Set block specifier
;		BL    - Char gen block specifier
;			D3-D2  Attr bit 3 one,   char gen 0-3
;			D1-D0  Attr bit 3 zero,  char gen 0-3
;			Note:  When using AL = 03 a function call
;				AX = 1000H
;				BX = 0712H
;				is recommended to set the color planes
;				resulting in 512 characters and eight
;				consistant colors
;
; Note : The following interface (AL=1X) is similar in function
;	to (AL=0X) except that :
;		- Page zero must be active
;		- Points (bytes/char) will be recalculated
;		- Rows will be calculated from the following:
;			INT((200 or 350) / points) - 1
;		- CRT_LEN will be calculated from :
;			(rows + 1) * CRT_COLS * 2
;		- The CRTC will be reprogrammed as follows :
;			R09H = points - 1       Max scan line
;				R09H done only in mode 7
;			R0AH = points  2        Cursor start
;			R0BH = 0                Cursor end
;			R12H =                  Vert disp end
;				((rows + 1) * points) - 1
;			R14H = points           Underline loc
;
;	The above register calculations must be close to the
;	original table values or undetermined results will
;	occur.
;
;	Note : The following interface is designed to be
;		called only immediately after a mode set has
;		been issued.  Failure to adhere to this practice
;		may cause undetermined results.
;
;	AL = 10   User alpha load
;		ES:BP - Pointer to user table
;		CX    - Count to store
;		DX    - Character offset into table
;		BL    - Block to load
;		BH    - Number of bytes per character
;	AL = 11   ROM monochrome set
;		BL    - Block to load
;	AL = 12   ROM 8x8 double dot
;		BL    - Block to load
;
;	Note : The following interface is designed to be
;		called only immediately after a mode set has
;		been issued.  Failure to adhere to this practice
;		may cause undetermined results.
;
;	AL = 20   User graphics chars   INT 01FH (8x8)
;		ES:BP - Pointer to user table
;	AL = 21   User graphics chars
;		ES:BP - Pointer to user table
;		CX    - Points (bytes per character)
;		BL    - Row specifier
;
;			BL = 0  User
;				DL - Rows
;			BL = 1  14 (0EH)
;			BL = 2  25 (19H)
;			BL = 3  43 (28H)
;
;	AL = 22   ROM 8 x 14 set
;		BL    - Row specifier
;	AL = 23   ROM 8 x 8 double dot
;		BL    - Row specifier
;
;	AL = 30   Information
;			CX    - Points
;			DL    - Rows
;		BH    - 0       Return current INT 1FH Ptr
;			ES:BP - Ptr to table
;		BH    - 1       Return current INT 44H Ptr
;			ES:BP - Ptr to table
;		BH    - 2       Return ROM 8 x 14 Ptr
;			ES:BP - Ptr to table
;		BH    - 3       Return ROM double dot Ptr
;			ES:BP - Ptr to table
;		BH    - 4       Return ROM double dot Ptr (TOP)
;			ES:BP - Ptr to table
;		BH    - 5       Return ROM alpha alternate 9 x 14
;			ES:BP - Ptr to table
;
; EXIT
;	None
;
; DESTROYS
;	AX, SI
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

DownloadFont	proc	near

	push	bp			; Save environment
	mov	bp,sp
	mov	al,byte ptr [bp].userAX ; Recover original subfunction
	cmp	al,20h			; If not one of the alpha loads,
	jae	dfX0			;  get out
	cmp	al,3			; If only changing banks,
	je	dfSetBlock		;  go do it
	and	al,00001111b		; Else extract sub-subfunction
	cmp	al,3			; If not 0, 1, or 2,
	jae	dfX0			;   get out
	mov	si,bx			; Get row specifier
	and	si,000000000000011b	; Extract font bank
	mov	byte ptr [FontBank][si],al ; Stash font id in table
	xor	ax,ax
	mov	ds,ax			; DS = 0
	assume	ds:INTVEC
	mov	ah,[CrtMode]		; Get current video mode
	push	bx			; Save environment
	mov	bl,ah			; Save a copy for later
	mov	al,byte ptr [Addr6845]	; Get low byte CRTC io address
	call	ChangeRegs		; Re-initialize all regs
	assume	ds:CODE
	mov	al,byte ptr [bp].userAX ; Get original subfunction
	sub	al,010h 		; Check 1x series
	js	dfX			; If not 1x series, we're done
	jz	dfSetUnderLine		; If al was 10h, bh set by user
	mov	bh,14			; Assume 8x14 font
	cmp	[fVga],FALSE		; Do we have a VGA Card?
	je	@F			; No - skip
	mov	bh,16			; Assume 8x16 font

@@:
	dec	al			; If al was 11h,
	jz	dfSetUnderLine		;  go check underline
	mov	bh,008h 		; Assume 8x8 font
	dec	al			; If al was not 12h,
	jnz	dfX			;  then invalid code

dfSetUnderLine:
	cmp	bl,007h 		; If not mono alpha,
	jne	@F			;  don't set underline
	mov	[CRTCRegs][014h],bh	; Set underline position

@@:
	dec	bh			; Make 0-based
	mov	[CRTCRegs][009h],bh	; Set last char line
	mov	ax,350			; Assume 350 total scan lines
	cmp	bl,003h 		; If not color alpha,
	ja	@F			;  skip special alpha tests
	call	BrstDet 		; If 350 lines,
	jnc	@F			;  our assumption was good
	mov	ax,200			; Else show 200 lines

@@:
	inc	bh			; Make char lines 1-based
	div	bh
	mul	bh
	dec	ax			; Make total lines 0-based
	mov	[CRTCRegs][012h],al	; Stash new vertical total

dfX:
	pop	bx			; Restore environment

dfX0:
	pop	bp			; Restore environment
	ret

dfSetBlock:
	mov	[SeqRegs][003h],bl	; Stash new font bank selection
	pop	bp			; Restore environment
	ret

DownloadFont	endp

endif	; CallTableNeeded

	subttl	Read Register
	page

;
; ReadReg - Read a single EGA register from shadow maps
;
; ENTRY
;	urAH	=  0F0h
;	bx	=  register index		if register id is indexed chip
;		   ignored			if register id is single reg
;	dx	=  register id
; EXIT
;	bl	=  current register data
; DESTROYS
;	ax, si, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

ReadReg		proc	near

	cmp	dl,18h			; Is it Attr reg?
	jne	@F			; No - skip
	cmp	bl,10h			; Is it palette reg?
	jae	@F			; No - skip
	mov	[fPalette],dl		; fPalette = TRUE

@@:
	call	UpdateCRTCMap		; Get latest readable regs
	mov	si,dx			; Reg id indexes PortTable
	mov	si,[PortTable][si].prCurrTable
	cmp	dl,NumPtrData * SIZE PortRec ; If not a chip,
	jae	@F			;  no need for bx
	mov	bl,CODE:[si][bx]	; Read indexed chip data
	ret

@@:
	mov	bl,CODE:[si] 		; Read a single reg
	ret

ReadReg		endp

	subttl	Write Register
	page

;
; WriteReg - write to a single EGA register and update shadow maps
;
; ENTRY
;	urAH	=  0F1h
;	bl	=  register index		if register id is indexed chip
;		   data to be written		if register id is single reg
;	bh	=  data to be written		if register id is indexed chip
;		=  ignored			if register id is single reg
;	dx	=  register id
; EXIT
;	none
; DESTROYS
;	ax, bh, dx, si, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

WriteReg	proc	near

	mov	ax,bx
	mov	si,dx			; Reg id indexes PortTable
	cli				; Disable interrupts
	or	[PortTable][si].prModFlag,3
	cmp	dl,NumPtrData * SIZE PortRec ; If not a chip,
	mov	dx,[PortTable][si].prPortAddr
	mov	si,[PortTable][si].prCurrTable
	jae	WRegNotPtr		; Just write it directly
	xor	bh,bh			; Zero bh for indexing
	mov	CODE:[si][bx],ah	; Update shadow map
	cmp	dl,AttCtrlAddrReg AND 0FFh ; If not attribute chip,
	jne	@F			; Skip special processing
	OutWordAttr ax,NoInts		; Write index/data to AttrAddr
	sti				; Restore interrupt state
	ret

@@:
	OutWord	NoInts,DestroyAX,DestroyDX
	sti				; Restore interrupt state
	ret

WRegNotPtr:
	mov	CODE:[si],al 		; Update shadow map
	or	[SingleRegMod],3	; Show a single reg is dirty
	out	dx,al			; And finally send out data
	sti				; Restore interrupt state
	ret

WriteReg	endp

	subttl	Read Register Range
	page

;
; ReadRange - read a range of EGA registers from shadow maps
;
; ENTRY
;	urAH	=  0F2h
;	dx	=  register id			(must be an indexed chip!)
;	cl	=  # of registers to read	(must be > 1 !)
;	ch	=  starting register index
;	es:bx	-> buffer to put reg data
; EXIT
;	none
; DESTROYS
;	ax, cx, si, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

ReadRange	proc	near

	cmp	dl,18h			; Is it Attr reg?
	jne	@F			; No - skip
	cmp	ch,10h			; Is it palette reg?
	jae	@F			; No - skip
	mov	[fPalette],dl		; fPalette = TRUE

@@:
	call	UpdateCRTCMap		; Get latest readable regs
	push	di
	mov	di,bx			; DI = ptr to user table
	mov	si,dx			; Reg id indexes PortTable
	mov	si,[PortTable][si].prCurrTable
	xor	ax,ax			; AX = 0
	xchg	al,ch			; AX = reg offset, ch = 0
	add	si,ax			; Adjust si for offset
	shr	cx,1			; CX now has reg count
rep	movsw				; Blast shadow map to user table
	rcl	cx,1
rep	movsb
	pop	di
	ret

ReadRange	endp

	subttl	Write Register Range
	page

;
; WriteRange - write to a range of EGA registers and update shadow maps
;
; ENTRY
;	urAH	=  0F3h
;	dx	=  register id			(must be an indexed chip!)
;	cl	=  # of registers to write	(must be > 1 !)
;	ch	=  starting register index
;	es:bx	-> buffer to get reg data
; EXIT
;	none
; DESTROYS
;	ax, bx, dx, si, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

WriteRange	proc	near

	push	di
	push	es
	mov	si,bx			; SI = ptr to user table
	mov	di,dx			; Reg id indexes PortTable
	mov	bx,offset PortTable	; Start at beginning
	cli				; Disable interrupts
	or	code:[bx][di].prModFlag,3
	mov	dx,code:[bx][di].prPortAddr
	mov	di,code:[bx][di].prCurrTable ; DI = ptr to shadow map
	mov	ax,es			; XCHG ES,DS
	mov	bx,ds
	mov	es,bx
	assume	es:CODE
	mov	ds,ax
	assume	ds:nothing
	xor	ax,ax			; AX = 0
	xchg	al,ch			; AX = reg offset, ch = 0
	add	di,ax			; Adjust di for offset
	push	cx			; CX now has reg count
	shr	cx,1
rep	movsw				; Blast table to shadow map
	rcl	cx,1
rep	movsb
	mov	ds,bx			; DS = CS
	assume	ds:CODE
	pop	cx			; Restore reg count
	sub	di,cx			; Rewind di
	cmp	dl,AttCtrlAddrReg AND 0FFh ; If not attribute chip,
	jne	WRangeNotAttr		; Skip special processing

@@:
	mov	ah,CODE:[di] 		; AH = reg data, al = reg index
	IODelay
	OutWordAttr	,NoInts		; Write index/data to AttrAddr
	inc	di
	inc	ax			; INC reg index (al)
	loop	@B
	jmp	short @F		; All done for this chip

WRangeNotAttr:
	mov	ah,CODE:[di] 		; AH = reg data, al = reg index
	IODelay
	OutWord	NoInts
	inc	di
	inc	ax			; INC reg index (al)
	loop	WRangeNotAttr

@@:
	pop	es
	assume	es:nothing
	pop	di
	sti				; Restore interrupt state
	ret

WriteRange	endp

	subttl	Read Register Set
	page

;
; ReadSet - read a set of EGA registers from shadow maps
;
; ENTRY
;	urAH	=  0F4h
;	cx	=  # of registers to read	(must be > 1 !)
;	es:bx	-> buffer of contiguous SetRec
;		   structures, one for each register
;		   to be read
; EXIT
;	none
; DESTROYS
;	ax, cx, si, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

ReadSet 	proc	near

	push	cx			; Save count
	push	bx			; Save buffer address

LookForPalette:
	cmp	byte ptr es:[bx][0],18h	; Is it Attr reg?
	jne	@F			; No - skip
	cmp	byte ptr es:[bx][2],10h	; Is it palette reg?
	jae	@F			; No - skip
	mov	[fPalette],cl		; fPalette = TRUE
	jmp	short LookForPaletteDone

@@:
	add	bx,4			; Next entry
	loop	LookForPalette		; Continue through the list

LookForPaletteDone:
	pop	bx			; Restore buffer address
	pop	cx			; Restore count
	call	UpdateCRTCMap		; Get latest readable regs
	push	di
	mov	di,bx

@@:
	mov	si,es:[di].srPortNum	; SI =  reg id
	mov	si,[PortTable][si].prCurrTable ; SI -> our shadow map
	mov	al,es:[di].srPtr	; AL =  reg index
	cbw
	add	si,ax			; Adjust si for index
	add	di,srData		; DI -> stash location
	movsb
	loop	@B
	pop	di
	ret

ReadSet 	endp

	subttl	Write Register Set
	page

;
; WriteSet - write to a set of EGA registers and update shadow maps
;
; ENTRY
;	urAH	=  0F5h
;	cx	=  # of registers to write	(must be > 1 !)
;	es:bx	-> buffer of contiguous SetRec
;		   structures, one for each register
;		   to be written
; EXIT
;	none
; DESTROYS
;	ax, cx, si, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

WriteSet	proc	near

	push	di
	push	dx
	mov	di,bx
	cli				; Disable interrupts

WSetNextEntry:
	mov	si,es:[di].srPortNum	; SI =  reg id
	or	[PortTable][si].prModFlag,3
	mov	dx,[PortTable][si].prPortAddr
	mov	si,[PortTable][si].prCurrTable ; SI -> our shadow map
	mov	al,es:[di].srPtr	; AL =  reg index
	cbw
	add	si,ax			; Adjust si for index
	mov	ah,es:[di].srData	; AH = reg data
	mov	CODE:[si],ah 		; Update shadow map
	cmp	dl,AttCtrlAddrReg AND 0FFh ; If not attribute chip,
	jne	WSetNotAttr		; Skip special processing
	OutWordAttr ax,NoInts		; Write index/data to AttrAddr
	jmp	short @F		; All done for this register

WSetNotAttr:
	OutWord	NoInts,DestroyAX,DestroyDX

@@:
	add	di,SIZE SetRec		; Bump to next record
	loop	WSetNextEntry
	pop	dx
	pop	di
	sti				; Restore interrupt state

DDefExit:				; Mind if we use your ret?  Thanks.
	ret

WriteSet	endp

	subttl	Define Default Shadow Maps
	page

;
; DefineDefault - load up default shadow maps from user buffer
;
; ENTRY
;	urAH	=  0F7h
;	cx	=  'TH' if user wants to program VGA specific color select
;			register.  This was added version 2.07.12.
;	dx	=  register id
;		   If high bit of dx set, an internal RevertDefault will
;		   occur after the default shadow maps have been updated.
;
;	es:bx	-> buffer of default values for specified register id.
;		   If register id is a chip, all values must be present.
; EXIT
;	none
; DESTROYS
;	ax, bx, si, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

DefineDefault	proc	near

	push	di
	push	cx
	mov	di,dx			; Reg id indexes PortTable
	and	di,7FFFh		; Strip "revert" flag
	mov	si,cx			; SI = Possible special VGA code
	xor	ch,ch			; No high byte
	mov	cl,[PortTable][di].prNumRegs
	cmp	[fVGA],ch		; Are we on a VGA?
	je	@F			; No - skip
	cmp	di,18h			; Is it the attribute controller regs?
	jne	@F			; No - skip
	cmp	si,'TH'			; Is it code for VGA color select?
	je	@F			; Yes - skip
	dec	cx			; Don't deal with color select register

@@:
	mov	si,bx			; SI -> user buffer
	mov	al,1
	mov	bx,offset PortTable	; Start at beginning
	or	code:[bx][di].prModFlag,al
	or	[SingleRegMod],al
	mov	di,code:[bx][di].prDefTable ; DI -> default shadow map
	mov	ax,es			; XCHG ES,DS
	mov	bx,ds
	mov	es,bx
	assume	es:CODE
	mov	ds,ax
	assume	ds:nothing
	shr	cx,1
rep	movsw				; Blast to default shadow map
	rcl	cx,1
rep	movsb
	mov	es,ax			; Recover old es
	assume	es:NOTHING
	mov	ds,bx			; DS = CS
	assume	ds:CODE
	pop	cx
	pop	di
	or	dh,dh			; If not "revert" flag,
	jns	DDefExit		;  just leave
					; Else make defaults active

if2	; Pass 2 of the assembler
.errnz		($ - RevertDefault)	; Drop into RevertDefault
endif	; Pass 2 of the assembler

DefineDefault	endp

	subttl	Revert to Default Shadow Maps
	page

;
; RevertDefault - blast default shadow maps to EGA and update shadow maps
;
; ENTRY
;	urAH	=  0F6h
; EXIT
;	none
; DESTROYS
;	ax, si, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

RevertDefault	proc	near

	push	di			; Save environment
	push	es			; Save environment
	push	dx			; Save environment
	push	cx			; Save environment
	push	bx			; Save environment
	mov	ax,ds
	mov	es,ax			; ES = DS = CS
	assume	es:CODE
	mov	bx,offset PortTable	; Start at beginning
	xor	cx,cx

RDefNextPtrData:
	test	CODE:[bx].prModFlag,1	; If dirty flag = 0,
	jz	ToRDefNotModified	;  done with this map
	mov	CODE:[bx].prModFlag,2	; Else clear dirty flag
	mov	cl,CODE:[bx].prNumRegs
	mov	di,CODE:[bx].prCurrTable
	mov	si,CODE:[bx].prDefTable
	mov	dl,cl			; Save count
	shr	cx,1
rep	movsw				; Blast default map to current map
	rcl	cx,1
rep	movsb
	mov	al,ch			; CH still 0 from above
	mov	cl,dl			; Restore count
	sub	si,cx			; Rewind si
	mov	dx,CODE:[bx].prPortAddr
	cmp	dl,AttCtrlAddrReg AND 0FFh ; If not attribute chip,
	jne	RDefNotAttr		; Skip special processing
	pushf				; Save interrupt flag state
	WaitRetrace			; Also disables interrupts
	mov	al,ch			; Restore index (0)
	mov	dl,AttCtrlAddrReg AND 0FFh ; Restore dx to AttrAddr

@@:
	mov	ah,CODE:[si] 		; Get data
	OutWordAttr	,NoInts		; Write index/data to AttrAddr
	IODelay
	inc	si
	inc	ax
	loop	@B
	mov	al,PaletteAddressSource	; Since dx still has AttrAddr,
	out	dx,al			;  Enable video now
	IOdelay
	InitFlipFlop	NoSaveAX	; Be sure to reset the flip-flop
	popf				; Restore interrupt flag state

ToRDefNotModified:
	jmp	short RDefNotModified	; All done for Attr chip

RDefNotAttr:
	mov	ah,CODE:[si]
	cmp	dl,(EGA_BASE + SEQ_ADDR) AND 0FFh ; SAS 06/09/86
	jne	RDefNotAttr1		; SAS 06/09/86
	cmp	al,SeqClMReg		; SAS 06/09/86
	je	@F			; SAS 06/09/86

RDefNotAttr1:
	cmp	dl,(EGA_BASE + GRAF_CONT) AND 0FFh
	jne	RDefOut
	cmp	al,GRAF_MISC_REG
	je	@F

RDefOut:
	OutWord

@@:
	inc	si
	inc	ax
	loop	RDefNotAttr

RDefNotModified:
	add	bx,SIZE PortRec
	cmp	bx,offset PortTable + (NumPtrData * SIZE PortRec)
	jnb	@F
	jmp	RDefNextPtrData

@@:
	InitFlipFlop	NoSaveAX	; Reset FF and get FeatAddr!
	IOdelay
	test	[SingleRegMod],1	; If singles dirty flag = 0,
	jz	@F			;  we're done
	mov	[SingleRegMod],2	; Clear singles dirty flag
	mov	al,[DefFeatureReg]
	mov	[FeatureReg],al 	; Since dx still has FeatAddr,
	out	dx,al			;  program it now
	IOdelay
	mov	dl,MiscAddr AND 0FFh	; Deal with MiscOut reg
	mov	al,[DefMiscOutReg]
	mov	[MiscOutReg],al
	out	dx,al
	IOdelay
	cmp	[fVga],CL		; If VGA is present,
	jne	@F			;  skip GR1&2 processing
	mov	dl,Gr1PosAddr AND 0FFh
	mov	al,[DefGR1PosReg]
	mov	[GR1PosReg],al
	out	dx,al
	IOdelay
	mov	dl,Gr2PosAddr AND 0FFh
	mov	al,[DefGR2PosReg]
	mov	[GR2PosReg],al
	out	dx,al

@@:
	pop	bx			; Restore environment
	pop	cx			; Restore environment
	pop	dx			; Restore environment
	pop	es			; Restore environment
	assume	es:nothing
	pop	di			; Restore environment
	ret

RevertDefault	endp

	subttl	Get Default Shadow Maps
	page

;
; GetDefault - dump default shadow maps to user buffer
;
; ENTRY
;	urAH	=  0F8h
;	dx	=  register id
;	es:bx	-> buffer to receive default values
;		   If register id is a chip, table must have room for all
;		   regs in chip.
; EXIT
;	none
; DESTROYS
;	ax, si, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

GetDefault	proc	near

	push	di
	push	cx
	mov	di,bx			; DI -> user buffer
	mov	si,dx			; Reg id indexes PortTable
	xor	ch,ch
	mov	cl,[PortTable][si].prNumRegs
	mov	si,[PortTable][si].prDefTable ; SI -> default shadow map
	shr	cx,1
rep	movsw				; Blast default map to user buffer
	rcl	cx,1
rep	movsb
	pop	cx
	pop	di
	ret

GetDefault	endp

	subttl	EGA Context Functions
	page

;
; ContextInfo - Save/Restore full EGA.SYS context information
;
;	This entrypoint was added for TSR's and environment managers like
;	Windows and Presentation Manager that must be able to save/restore the
;	EGA.SYS context information as well as the state of the EGA hardware.
;	Without this functionality, Windows is unable to properly preserve the
;	state of variables internal to EGA.SYS such as the dirty flags and/or
;	the state of the EGA data latches.  It also provides software
;	developers a more rapid mechanism for context switching the EGA, as it
;	dispenses with the need to make numerous Read/WriteRange and
;	Read/WriteSet calls.
;
;	Secondly, there are two types of context information that developers
;	must implicitly handle:  1) the screen data, and 2) the font data (for
;	alpha modes).  Since shadowing the EGA registers alone does not
;	provide sufficient information for efficiently handling downloaded
;	font data, a call has been added to retrieve the current status of
;	each alpha font bank programmed through int 010h, subfunction 011h.
;
; ENTRY
;	urAH	=  0F9h
;	urAL	=  000h  GetContextSize
;		=  001h  SaveContext
;		=  002h  RestoreContext
;		=  003h  GetFontInfo
;		=  004h  GetInBiosFlag
;
;	other regs per function being called
; EXIT
;	per function being called
; DESTROYS
;	ax, si for this particular procedure,
;	otherwise,  per function being called
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

ContextInfo	proc	near

	push	bp			; Save environment
	mov	bp,sp			; Set up stack addressing
	mov	al,byte ptr [bp].userAX	; Get old ax from stack
	pop	bp			; Restore environment
	cmp	al,MaxContextCall	; If not one of our subfunctions,
	ja	ciX			;  get out now
	xor	ah,ah			; Reset ah for dispatch
	shl	ax,1			; *2 for word addresses
	mov	si,ax
	jmp	[ContextTable][si]	; MUST jmp for stack integrity

ContextInfo	endp

;
; GetContextSize - return size in bytes needed to save EGA.SYS context
;
; ENTRY
;	none
; EXIT
;	userAX	=  size in bytes needed to save EGA.SYS context
; DESTROYS
;	AX
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

GetContextSize	proc	near

	push	bp			; Save environment
	mov	bp,sp			; Set up stack addressing
	mov	[bp].userAX,SizeEGAContext ; Stash size on stack for ax
	pop	bp			; Restore environment

ciX:					; Mind if we use your ret?  Thanks.
	ret

GetContextSize	endp

	page

;
; SaveContext - dump current EGA.SYS context data to user buffer
;
;	If an app or TSR must change the EGA regs, but wants to restore
;	EGA.SYS and the hardware to the original state on entry, it should
;	first call GetContextSize.  Based on the value returned, it should
;	allocate the necessary buffer (possibly on the stack), set es:bx to
;	point to it, and issue this call.  The app is then free to issue other
;	EGA.SYS or int 10h calls at will.  When finished, it should call
;	RestoreContext data with a pointer to the saved context data.  Since
;	the current state of the dirty flags is included in the context data,
;	these too will be properly restored.
;
;	Note that apps must not assume a given size for the context data.
;	An app should call GetContextSize at least once, beforehand, to
;	determine how much memory to provide.  Also, apps must not assume a
;	given structure for the context data or modify it in any way.
;
;	If an app or TSR plans to make semi-permanent changes to the EGA regs,
;	it may avoid the Save/Restore context calls.  EGA.SYS will shadow any
;	int 10h calls that modify the EGA regs, updating both the default and
;	current shadow maps, but not touching any of the dirty flags.  This
;	allows the interrupted app to continue making RevertDefault calls, yet
;	keeps the new changes in effect until the app explicitly changes a reg
;	which the TSR modified.
;
; ENTRY
;	es:bx	->  user save area buffer for EGA context
; EXIT
;	none
; DESTROYS
;	ax, si, ds, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

SaveContext	proc	near

	push	cx			; Save environment
	push	dx			; Save environment
	push	di			; Save environment
	push	bx			; Save environment
	call	GetLatchLocation	; Get EGA/VGA latch save location
	or	di,di			; Are we in text mode?
	jz	@F			; Yes - skip

;
; Save the EGA/VGA latches
;

	mov	ah,ReadRegNum		; Read 1 register
	mov	dx,10h			; Graphics port register
	mov	bx,5			; Index 5
	int	10h			; Get it
	mov	si,ds			; Find a place for our byte
					; in vram, past visible memory
	mov	dx,egamem
	mov	ds,dx
	assume	ds:egamem
	mov	ax,(MR_SET shl 8) or GRAF_MODE_REG ; Set mode reg
					; to writethrough (mode 1)
	mov	dx,EGA_BASE + GRAF_CONT ; Set the port
	OutWord	,DestroyAX,DestroyDX
	mov	egamem:[di],al		; Actually write to vram and store
					; latches at offset [bx]
	mov	bh,bl			; Position it properly
	mov	bl,5			; Index 5
	mov	ah,WriteRegNum		; Write 1 register
	mov	dx,10h			; Graphics port register
	int	10h			; Restore original port value
	mov	ds,si
	assume	ds:CODE

;
; Save the EGA/VGA registers
;

@@:
	call	UpdateCRTCMap		; Get latest readable regs
	pop	bx			; Restore environment
	mov	di,bx
	mov	si,offset StartEGAContext
	mov	cx,SizeEGAContext / 2

if	SizeEgaContext AND 1
	movsb
endif	; SizeEgaContext AND 1

rep	movsw
	pop	di			; Restore environment
	pop	dx			; Restore environment
	pop	cx			; Restore environment
	ret

SaveContext	endp

	page

;
; RestoreContext - restore EGA.SYS context data from user buffer
;
;	RestoreContext copies a previously saved EGA context pointed to by
;	es:bx to the internal data areas of EGA.SYS and updates the hardware
;	to reflect the restored state.	The only caveat is that the state of
;	the Attribute index/data flip-flop is reset to the "index" state since
;	SaveContext is unable to save this information.  Note that this should
;	not be a problem if the caller checks the fInBIOS flag before
;	attempting to reprogram the EGA and if the interrupted application
;	only uses EGA.SYS or int 010h to modify the hardware.  This is because
;	EGA.SYS and int 010h always reset this flip-flop to the "index" state
;	on return.
;
; ENTRY
;	es:bx	-> previously saved EGA context
; EXIT
;	none
; DESTROYS
;	ax, si, ds, flags
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

RestoreContext	proc	near

	push	cx			; Save environment
	push	dx			; Save environment
	push	di			; Save environment
	push	es			; Save environment

;
; Restore the EGA/VGA latches
;

	call	GetLatchLocation	; Get EGA/VGA latch save location
	or	di,di			; Are we in text mode?
	jz	@F			; Yes - skip
	mov	dx,egamem
	mov	ds,dx
	assume	ds:egamem
	mov	dx,EGA_BASE + GRAF_ADDR	; Setup for writethrough mode
	mov	ax,(DR_SET shl 8) or GRAF_DATA_ROT
	pushf				; Save interrupt flag state
	cli				; No interrupts while changing regs
	OutWord	NoInts,DestroyAX
	IOdelay
	mov	ax,(0ffh shl 8) or GRAF_BIT_MASK ; Don't mask any bits
	OutWord	NoInts,DestroyAX,DestroyDX
	IOdelay
					; DH already set correctly
	mov	dl,SEQ_ADDR		; Enable all planes, set seq
	mov 	ax,(MM_ALL shl 8) or SEQ_MAP_MASK ; Addr reg to point
	OutWord	NoInts,DestroyAX,DestroyDX ; to map mask reg
	popf				; Restore interrupt flag state
	mov	al,egamem:[di]		; Actually write to vram to
					; restore the latches

@@:
	mov	si,bx
	mov	di,offset StartEGAContext
	mov	ax,es			; ES = CS
	mov	dx,cs			; DS = Old ES
	mov	es,dx			; "
	assume	es:CODE
	mov	ds,ax			; Get new clocking mode 1st
	assume	ds:nothing
	mov	al,ds:[si + offset SeqRegs - offset StartEgaContext + 01h]
	cmp	al,CODE:[di + offset SeqRegs - offset StartEgaContext + 01h]
	jne	@F			; If same, compare mem mode
	mov	al,ds:[si + offset SeqRegs - offset StartEgaContext + 04h]
	cmp	al,CODE:[di + offset SeqRegs - offset StartEgaContext + 04h]
	jne	@F			; If same, compare misc regs
	mov	al,ds:[si + offset GraphicsRegs - offset StartEgaContext + 06h]
	cmp	al,CODE:[di + offset GraphicsRegs - offset StartEgaContext + 06h]

@@:
	mov	cx,SizeEGAContext / 2

if	SizeEgaContext AND 1
	movsb
endif	; SizeEgaContext AND 1

rep	movsw
	pop	es			; ES = old ES
	assume	es:nothing
	mov	ds,dx			; DS = CS
	assume	ds:CODE
	mov	cl,NumSeqRegs - 2 	; Don't do clocking/memory modes
	mov	dh,3			; "prefix" for port addresses
	mov	si,offset SeqRegs
	je	@F	 		; Jump if no changes here
	mov	ax,0100h
	mov	dl,(EGA_BASE + SEQ_ADDR) AND 0FFh
	pushf				; Save interrupt flag state
	cli				; RAM refresh will be off now!
	OutWord	NoInts			; Synchronous reset to sequencer
	IOdelay
	inc	ax			; Now select clocking mode reg
	mov	ah,CODE:[si + 01h]
	OutWord	NoInts,DestroyAX	; Write its new value (in ah)
	IOdelay
	mov	al,04h			; Now select memory mode reg
	mov	ah,CODE:[si + 04h]
	OutWord	NoInts,DestroyAX,DestroyDX ; Write its new value (in ah)
	IOdelay
	mov	dl,(EGA_BASE + GRAF_CONT) AND 0FFh
	mov	al,06h			; Now select grphx misc reg
	mov	ah,[GraphicsRegs + 06h]
	OutWord	NoInts,DestroyAX,DestroyDX ; Write its new value, too (ah)
	popf				; Restore interrupt flag state

@@:
	mov	dl,(EGA_BASE + SEQ_ADDR) AND 0FFh ; Setup for seq loop
	mov	al,ch			; AL = 0

recSeqLoop:
	mov	ah,CODE:[si]
	IOdelay
	OutWord 			; No ints can occur until after

@@:
	inc	si
	inc	ax
	cmp	al,01h			; Clocking mode reg next?
	je	@B			; If so, skip it
	loop	recSeqLoop		; Otherwise continue if more
	mov	al,cl			; AL = 0
	mov	cl,NumGraphicsRegs - 1
	mov	si,offset GraphicsRegs
	mov	dl,(EGA_BASE + GRAF_CONT) AND 0FFh

recGrphLoop:
	mov	ah,CODE:[si]
	IOdelay
	OutWord

@@:
	inc	si
	inc	ax
	cmp	al,06h			; Grphx misc reg next?
	je	@B			; If so, skip it
	loop	recGrphLoop		; Otherwise continue if more
	mov	al,cl			; AL = 0
	mov	cl,NumCRTCRegs
	mov	dl,byte ptr [PortTable][0].prPortAddr
	mov	si,offset CRTCRegs

@@:
	mov	ah,CODE:[si]
	IOdelay
	OutWord
	inc	si
	inc	ax
	loop	@B
	mov	cl,[PortTable][3 * SIZE PortRec].prNumRegs
	mov	si,offset AttrRegs
	pushf				; Save interrupt flag state
	WaitRetrace			; Also disables interrupts
	mov	al,ch			; Restore index (0)
	mov	dl,AttCtrlAddrReg AND 0FFh ; Restore dx to AttrAddr

@@:
	mov	ah,CODE:[si] 		; Get data
	IOdelay
	OutWordAttr	,NoInts		; Write index/data to AttrAddr
	inc	si
	inc	ax
	loop	@B
	mov	al,PaletteAddressSource	; Since dx still has AttrAddr,
	IOdelay
	out	dx,al			; enable video now
	IOdelay
	InitFlipFlop	NoSaveAX	; Reset FF and get FeatAddr!
	popf				; Restore interrupt flag state
	IOdelay
	mov	al,[FeatureReg] 	; Since dx still has FeatAddr,
	out	dx,al			; program it now
	IOdelay
	mov	dl,MiscAddr AND 0FFh
	mov	al,[MiscOutReg]
	out	dx,al
	cmp	[fVga],CL		; If VGA is present,
	jne	@F			;  skip GR1&2 processing
	mov	dl,Gr1PosAddr AND 0FFh
	mov	al,[GR1PosReg]
	IOdelay
	out	dx,al
	IOdelay
	mov	dl,Gr2PosAddr AND 0FFh
	mov	al,[GR2PosReg]
	out	dx,al

@@:
	pop	di			; Restore environment
	pop	dx			; Restore environment
	pop	cx			; Restore environment
	ret

RestoreContext	endp

	page

;
; GetLatchLocation - dump current EGA.SYS context data to user buffer
;
; ENTRY
;	None
; EXIT
;	If currently in graphics mode
;		DI -> Latch location
;	Else (Text mode)
;		DI = 0
; DESTROYS
;	AX
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

GetLatchLocation	proc	near

	xor	di,di			; Assume text mode
	mov	al,[biosmode]		; Get current video mode
	and	al,01111111b		; Reset noclear bit
	cmp	al,13h			; Is it a mode we can handle?
	ja	@F			; No - skip (fake text mode)
	sub	al,0dh			; Adjust to zero base
	js	@F			; Skip if text mode
	xor	ah,ah			; No high byte
	add	di,ax			; Position to video mode in table
	mov	di,CODE:[di] + offset EGALatchTable ; Get desired latch location

@@:
	ret

GetLatchLocation	endp

	page

;
; GetFontInfo - dump current font data in FontInfo format to user buffer
;
;	GetInfo is provided for environments like Windows that need to know
;	which alpha fonts have been downloaded and which banks they are in.
;
;	This call copies the current EGA.SYS FontInfo structure to a user
;	buffer pointed to by es:bx.  Based on this information, the caller can
;	determine the need to save alpha font data in plane 2, as well as
;	determine the most efficient way to do it.  For example, font ID's
;	001h and 002h need not be saved since a copy exists in the EGA ROM.
;
;	Graphics font data is maintained by the EGA BIOS via int 01Fh and int
;	043h and does not exist in the EGA hardware.  Since these interrupts
;	can be directly manipulated for Save/Restore operations, EGA.SYS does
;	not attempt to record any graphics font information.
;
;	FontInfo format:
;
;	FontInfo	struc
;	  fibank0	db	?
;	  fibank1	db	?
;	  fibank2	db	?
;	  fibank3	db	?
;	FontInfo	ends
;
;	where each fiBankx contains a byte defined as follows:
;
;	000h	-  user font in specified bank
;	001h	-  08x14 ROM font in specified bank (Default for EGA)
;	002h	-  08x08 ROM font in specified bank
;	004h	-  08x16 ROM font in specified bank (Default for VGA)
;	0FFh	-  empty bank
;
; ENTRY
;	es:bx	-> user buffer for FontInfo
; EXIT
;	none
; DESTROYS
;	ax
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

GetFontInfo	proc	near

	mov	ax,word ptr [FontBank][0]
	mov	es:[bx],ax
	mov	ax,word ptr [FontBank][2]
	mov	es:[bx][2],ax
	ret

GetFontInfo	endp

;
; GetInBiosFlag - return segment:offset of the InBiosFlag
;
; ENTRY
;	none
; EXIT
;	es:bx	->  InBiosFlag
; DESTROYS
;	none
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

GetInBiosFlag	proc	near

	mov	bx,cs
	mov	es,bx
	assume	es:CODE
	mov	bx,offset fInBIOS
	ret

GetInBiosFlag	endp

	subttl	InquireDriver
	page

;
; InquireDriver - return ptr to driver version number
;
; ENTRY
;	none
; EXIT
;	es:bx	-> DriverInfo data area
; DESTROYS
;	None
;

	assume	cs:CODE, ds:CODE, es:nothing, ss:nothing

InquireDriver	proc	near

	mov	bx,ds
	mov	es,bx
	assume	es:CODE
	mov	bx,offset DriverInfo
	ret

InquireDriver	endp

ifdef	Sys
	include	int10rtn.inc		; Contains Int10Routine and Int2FRoutine
					; Also in load.inc for .COM version
endif	; Sys

EndOfResidentCode	label	near

;
; I N C L U D E S
;

	include	load.inc

CODE	ends

	end	main
