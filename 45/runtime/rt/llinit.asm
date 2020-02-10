	TITLE	LLINIT - GW-BASIC Interface Initialization
;***
; LLINIT.ASM - GW-BASIC Interface Initialization
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************

	INCLUDE switch.inc	;switch file [new]
	INCLUDE rmacros.inc	;useseg macro

	USESEG	RT_TEXT 	;core runtime segment
	USESEG	_DATA		
	USESEG	CONST		
	USESEG	_BSS		

	INCLUDE seg.inc 	;segment definitions
	INCLUDE idmac.inc	;internal debug macros
	INCLUDE oscalls.inc	;Dos 5 structures
	INCLUDE ibmunv.inc
	INCLUDE intmac.inc
	INCLUDE const.inc	; b$IOFLAG field definitions.
	INCLUDE llgrp.inc	; Constant definitions
	INCLUDE baslibma.inc	; SKIP macro



sBegin	_DATA			

globalW b$pChkOlivetti,B$NearRet ; pointer to routine in lloga.asm

labelW	<PUBLIC,b$DSP> 	;b$BLDSP and b$WHDSP in one word
globalB b$BLDSP,70H,1		;encoded attr for fct. key reverse video
globalB b$WHDSP,7,1		;encoded attr for fct. key reverse video

labelW	<PUBLIC,b$InsCsrTyp>	; insert mode cursor (init to 1/2 block)
globalB	b$InsCsrStop,7,1	; cursor stop line (*)
staticB	InsCsrStart,4,1		; cursor start line (*)

labelW	<PUBLIC,b$UsrCsrTyp>	; user defined cursor
globalB	b$UsrCsrStop,7,1	; cursor stop line (*)
globalB	b$UsrCsrStart,7,1	; cursor start line (*)

globalW b$OrgCsrTyp,,1 	;startup cursor type

globalW b$OrgScrBst,,1 	;screen BASIC mode and burst at startup
globalB b$OrgBiosMode,,1	;entry BIOS mode save [23] made public
globalW b$OrgPages,,1		;active and visual pages at startup

labelW	<PUBLIC,b$OrgScrDim>	;screen dimensions at startup in one word
staticB	,,1			;width
labelB	<PUBLIC,b_orglen>	;Interpreter reachable label
globalB b$orglen,25,1		;height

externB	b$IOFLAG		; Misc. IO flags.  Defined in GWINI.ASM
externW	b$CURSOR		; Screen cursor.  Defined in GWDATA.ASM
externW b$CurPages		

staticB b$SCNFLAG,0,1		;screen initialization flags
	SCN_RESET = 1		;screen startup mode change
	SCN_GIOINI= 1		;force SetMode for GIO initialization

sEnd	_DATA			

sBegin	CONST

	 ;translation table to translate from BIOS mode to BASIC screen mode
HSMTBL	DB	0,0,0,0,1,1,2,0,3,0,0,0,0,7,8,10,9  
	DB	11,12,13	

sEnd	CONST

sBegin	_BSS			
externB b$ScrWidth		
externB b$ScrHeight		
externB b$ScreenMode		
externB b$BiosMode		
globalW b$VGAmodesL,,1		; flag for VGA BIOS modes 0-F
globalB	b$VGAmodesH,,1		; flag for VGA BIOS modes 10h-13h
staticB	VgaMiscInfo,,1		; miscellaneous VGA-MCGA-PS/2 info
externB b$Burst			

externW  b$SetMode		
externW  b$SetPages		
externW b$PalReset		
globalB b$EgaPalSup,,1 	;EGA palette hardware support flag

externB b$Buf1			

labelW	Equip			;b$OrgEquip and b$CurEquip in one word
globalB b$OrgEquip,,1		;startup BiosEquip flags
globalB b$CurEquip,,1		;current BiosEquip flags

globalW b$KBDVEC_SAVE,,2	;ROM keyboard vector saved here
globalW b$CLKVEC_SAVE,,2	;ROM clock vector saved here

globalB b$NetCard,,1		;boolean indicating if netcard installed
				; under DOS 3.xx
				; b$NetCard=1 --> netcard installed
				; b$NetCard=0 --> not installed
globalB b$DOS_INT_MASK,,1	;8259 interrupt mask is saved here
				;on BASIC entry (IRQ2 set if AT)

globalB b$MACHID,,1		;machine ID. This byte is set by the
				;routine B$GWINI as follows:
				;  0FFH if PC
				;  0FEH if PC XT
				;  0FDH if PC Jr
				;  0FCH if PC AT
				;	or PS/2 model 50 (Centurion)
				;	or PS/2 model 60 (Skylane)
				;  0FAH if PS/2 model 30 (Icarus)
				;  0F8H if PS/2 model 80 (Cardinal)


; b$Adapter and b$Monitor must be contiguous
labelB	<PUBLIC,b_Adapter> ; Interpreter reachable label
globalB b$Adapter,0,1	;current graphics adapter; !0 indicates GWINI called
			;  00000001 = Monochrome Display Adapter (MDPA)
			;  00000010 = Color Graphics Adapter     (CGA)
			;  00000100 = Enhanced Graphics Adapter  (EGA)
			;  00001000 = Video Graphics Array	     (VGA)
			;  00010000 = MultiColor Graphics Array  (MCGA)
			;  00100000 = Hercules Graphics Card     (HGC)
			;  0100xxx0 = Olivetti Graphics Adapter  (OGA)

; The VGA is limited to color modes if an MDPA is found in the machine,
; and limited to monochrome modes if a CGA is found in the machine.  If
; the VGA is the only adapter in the machine it can use all modes.

; b$Adapter and b$Monitor must be contiguous
globalB b$Monitor,,1	;current display monitor
			;  00000001 = Monochrome
			;  00000010 = Color (or Enhanced emulating color)
			;  00000100 = Enhanced Color
			;  00001000 = Analog (supporting mono modes)
			;  00010000 = Analog (supporting color modes)
globalW b$VideoMem,,1		;graphics adapter video memory in K


sEnd	_BSS			

sBegin	RT_TEXT 		
	assumes CS,RT_TEXT	

	externNP B$NearRet	
	externNP B$VIEWINIT	
	externNP B$SETSCNDATA	; init the low-level screen variables
	externNP B$GRMODE	
	externNP B$SCROLL	



	externNP B$GETCSRDATA	


	externNP B$SCNIO	; used in SCNIOS macro!!!!!

	externNP B$FixTextPage	

globalD b$OldClkTic,,1 	;label storing the vectors of
				;of old interrupt 1CH
globalW b$BASDSG,,1		;address of BASIC data segment



;***
;B$GWINI - OEM initialization
;OEM-interface routine
;
;Purpose:
;	This routine is called once during initialization.  It contains
;	machine specific code to put the system into a deterministic
;	initial state.
;
;Responsibilities:
;	After executing this routine, the following items must have
;	occurred:
;	   1) B$VIEWINIT must have been called.
;	   2) B$SCNSWI must have been called.
;	   3) The initial screen state must have been saved so that
;	      screen termination (B$RESETSCN) can restore it.
;	   4) if this runtime is for a QB interpreter, B$SCINIT must
;	      have been called.
;	   5) The variables listed in the Exit: section must be set.
;
;	B$GWINI should not reset the physical screen.	This is done by
;	B$SCINIT which is called before any screen activity.
;
;	This routine is called FAR.
;
;	NOTE:  This is one of a pair of OEM-Dependent initialization
;	routines.  This one deals mainly with Screen and Interrupt
;	initialization.  See B$RTLLINI for more details.
;
;Entry:
;	None.
;
;Exit:
;	b$IOFLAG   - Set fields SCN_INIT and SCN_SCROLL properly
;	b$CurPages - contains initial active and visual pages
;	b$CURSOR   - contains current cursor position.
;	b$ScreenMode - contains current BASIC screen mode
;	b$CurPages - contains current visual and active pages
;
;	PSW.C is set to indicate an error.  The runtime will abort
;	      with a fatal error condition.
;
;Uses:
;	Per convention
;
;Exceptions:
;	None
;****
;
;Algorithm:
;	get initial screen mode
;	store mode for exit
;	compute screen length and width
;	get current cursor position and type
;	save cursor type for exit
;	set up the initial cursor rasters
;	compute BASIC screen mode and color burst toggle
;	call section of SCREEN statement code that initializes
;		the screen variables
;	clear carry before exiting
;#****

cProc	B$GWINI,<PUBLIC,FAR>	
cBegin				

	CMP	[b$Adapter],0	; make sure first time called (mixed lang.)
	JZ	FirstTime	; brif not
	JMP	INIRET		; otherwise abort
FirstTime:			
	and	b$IOFLAG,NOT SCN_INIT	;clear screen init flag

	PUSH	ES
	XOR	BX,BX
	MOV	ES,BX
	mov	al,es:[BiosEquip]   ;get the equipment flags
	mov	ah,al		;copy to AH
	mov	Equip,ax	;save b$OrgEquip and b$CurEquip values
	dec	bx		;segment 0FFFFH
	MOV	ES,BX		;address using the ES register
	MOV	AL,ES:[000EH]	;get the machine ID in [AL]
	MOV	b$MACHID,AL	;store it in b$MACHID
	POP	ES

;   If machine is an AT, enable IRQ2.  In any case, save the
;   current interrupt mask register contents in b$DOS_INT_MASK.

	MOV	DX,INTA1	;get interrupt mask register address
	CLI			;disable interrupts
	IN	AL,DX		;get interrupt mask (IRQ7->IRQ0)
	CMP	b$MACHID,0FCH	;test if machine is AT
	JNZ	NOT_AT		;jump if not an AT machine
	AND	AL,NOT 4	;clear bit 3 (IRQ2) for AT
	OUT	DX,AL		;and write it out to enable it
NOT_AT:
	STI			;re-enable interrupts
	MOV	b$DOS_INT_MASK,AL ;save as the DOS interrupt mask


	CLI			;disable interrupts
				;The 3 SAVINTs are required since
				;B$SNDOFF and B$GWTERM restore them.
				;Interrupts turned on in SNDOFF.

	PUSH	ES
	SAVINT	b$KBDVEC_SAVE,KBDVEC ;save INT EF vector
	SAVINT	b$CLKVEC_SAVE,CLKVEC ;save INT F0 vector
	POP	ES		;restore segments...

	XFRINT	KBDVEC/4,KBDINT/4 ;move INT 09H to INT EFH
	XFRINT	CLKVEC/4,CLKINT/4 ;move INT 04H to INT F0H

	PUSH	DS		;save DS...
	xor	ax,ax
	mov	ds,ax
	push	es
	SAVINT	DS:TICSAV,TIMADR ; save get control on timer INT VEC
	pop	es
	pop	ds

	SCNIOS	vGetVideoState	;get current screen mode
	mov	dl,bh		;let the current active page AND visual
	mov	dh,bh		;  page be the visual page from entry
	MOV	b$OrgBiosMode,AL ;save inital screen mode
	MOV	CL,AH		;CL = screen width
	CMP	AL,40h		; check for Olivetti 640x400 mode
	JNE	Not40h		; brif not
	MOV	AL,4		; BASIC Screen mode 4
	JMP	SHORT NoBurst	; no burst for Screen 4
Not40h:				
	MOV	AH,AL		;AH = bios mode
	MOV	BX,OFFSET DGROUP:HSMTBL ;get init scrn mode table offset
	XLAT			;AL = screen mode
	and	ah,1		;strip bios mode to get color burst
	cmp	al,1		;screen 0 or 1?
	jbe	UseBurst	;go if so, burst is valid
NoBurst:			
	xor	ah,ah		;no burst for other modes
UseBurst:			
	push	ax		;save: screen mode and burst
	push	dx		;	   pages
	push	cx		;	   width

	CALL	B$SetAdapter	; Determine display hardware configuration


	MOV	AL,30h		
	XOR	BX,BX		
	XOR	CX,CX		
	MOV	DX,24		; default to 25 lines (incremented below)
	PUSH	ES		; Preserve (it's a return value)
	SCNIOS	vCharGen	
	POP	ES		
	INC	DX		
	pop	cx		
	mov	ch,dl		;CH = height
	pop	dx		
	pop	ax		
	push	ax		;save:  screen mode and burst
	push	cx		;	    width and height
	call	B$SETSCNDATA	;set up screen data for entry mode
	pop	cx		
	pop	ax		
	;The entry mode may not be one supported by our software
	;configuration. If something is different than what was
	;requested, set a flag so screen initialization will set
	;the new mode when the time comes.
	cmp	al,b$ScreenMode;mode changed?
	jne	ModeChgd	;go if so
	cmp	cl,b$ScrWidth	;width?
	jne	ModeChgd	;go if so
	cmp	ch,b$ScrHeight ;height?
	je	NoChange	;go if not
ModeChgd:			
	OR	b$SCNFLAG,SCN_RESET	;set screen reset flag for B$SCINIT
NoChange:			
	mov	al,b$ScreenMode;save entry screen mode
	mov	ah,b$Burst	;		burst
	mov	b$OrgScrBst,ax 

	mov	ax,b$CurPages	;		pages
	mov	b$OrgPages,ax	

	mov	ax,word ptr [b$ScrWidth] ; width and height in 1 word
	mov	b$OrgScrDim,ax	;(sets b$orglen)

;	If we're in 25 line mode, the cursor start/stop lines vary depending
;	on how many scan lines are on the screen.  If we're in a mode with
;	other than 25 lines, the cursor is from 7 to 7 regardless of scan line
;	count.

	CMP	AH,25		; 25 line mode?
	JNZ	Use7_7		; no, use 7,7 cursor.

	CMP	b$BiosMode,7	; is it b/w mode? (indicates 350 scan lines)
	JNZ	Use7_7		; no, use default
	MOV	AX,0C0CH	; b/w cursor raster is 12,12
	MOV	b$UsrCsrTyp,AX	; save new user cursor
	MOV	b$InsCsrStop,AL ; save new insert cursor stop line

Use7_7:
	CALL	B$GETCSRDATA	; load cursor position into DX
				; and cursor type into CX
				; (updates B$CSRPOS, B$CSRTYP)
	mov	b$OrgCsrTyp,cx ; save entry cursor type
	test	b$SCNFLAG,SCN_RESET	;will we reset screen?
	jnz	HomeCsr 		;go if so to home cursor
	CALL	B$GRMODE 	; graphics mode?
	JZ	NOT_GRAPH	; brif not -- use current cursor location
HomeCsr:			
	MOV	DX,0101h	; use (1,1) as initial high-level cursor
NOT_GRAPH:			

	CMP	DL,b$ScrHeight ; is cursor presently on last line?
	JNZ	NOT_LST		; brif not
				; note: graphics modes will always branch
	DEC	DX		; we want it on previous line
	OR	b$IOFLAG,SCN_SCROLL ; signal B$SCINIT to scroll the
				; screen before first screen operation.
NOT_LST:			

	MOV	b$CURSOR,DX	; update high-level cursor position

	CALL	B$VIEWINIT	; initialize viewports and graphics cursor

	call	B$SCINIT	;initialization.  Do it all now to

	sti
	mov	ah,30h		; if dos 3 then if network installed
	int	21h		; then do not eat CTRL-ALT-BRK.
	cmp	al,3		; version 3 and above
	jb	not3		; brif less than 3.xx
	xor	ah,ah		; netcard installation check
	int	2ah
	or	ah,ah		; [ah] = 0 --> network not installed
	jz	not3		; brif network not installed
	mov	b$NetCard,1	; else set b$NetCard for use in -cevt
not3:
	CLC			;clear carry to indicate no error
INIRET:

cEnd				



;
;	Added as part of revision [17].
;***
;B$SCINIT -- Initialize the screen.
;OEM-interface routine
;
;Purpose:
;
;	This routine exists to delay initialization operations that
;	actually effect the screen until such time as it is determined
;	that an actual screen operation (text output, graphics, etc.)
;	is to take place.  Thus a program which does not use the screen
;	(such as those doing only file I/O or redirection) will never
;	change the screen mode or appearance.
;
;	Performs the remaining screen initialization required before a
;	screen operation can be done. If bit SCN_INIT of b$IOFLAG is not
;	set, it initializes the cursor raster and initializes the palette.
;	Scrolls the screen if bit SCN_SCROLL of b$IOFLAG is set.  Sets
;	bit SCN_INIT of b$IOFLAG to indicate that screen initialization
;	has been done.
;
;	If running under OS/2, the screen should to be cleared by a
;	call to B$CLRSCN with a parameter of 0.
;
;	In the OEM code, the following routines must either call B$SCINIT
;	or make sure that B$SCINIT has been called.
;
;	    LLASCN.ASM
;
;		B$PCOPYS
;
;	    LLCSCN.ASM
;
;		B$SCREEN, B$SCRSTT, B$SWIDTH, B$SETCLR
;
;	    LLSCNIO.ASM
;
;		B$OFFCSR, B$OVWCSR, B$INSCSR, B$USRCSR
;		B$CLRSCN
;
;Entry:
;	Bits SCN_INIT and SCN_SCROLL of b$IOFLAG properly set.
;
;Exit:
;	Bit SCN_INIT of b$IOFLAG set.
;
;Uses:
;	Per convention.
;
;Preserves:
;	AX, BX, CX, DX
;
;Exceptions:
;	None.
;**********************************************************************
;
;	Bit SCN_RESET of b$SCNFLAG may also be set indicating that the
;	startup (current) screen mode is unsupported.  If so, a new mode
;	has already been determined and setup by B$SETSCNDATA (called
;	from B$GWINI) and all that remains here is to actually change the
;	screen to that mode and set valid page data.
;
;	There must be a call to B$SCINIT before each screen operation.
;	The following is a list of where each of the calls are, and what
;	screen statements they handle:
;
;	    CIRCLE.ASM:
;		B$CIRC:		Handles CIRCLE.
;
;	    DRAW.ASM:
;		B$DRAW:		Handles DRAW.
;
;	    GETPUT.ASM:
;		B$GGET:		Handles GET.
;		B$GPUT:		Handles PUT.
;
;	    GRLINE.ASM:
;		B$LINE:		Handles LINE.
;
;	    GRPOINT.ASM:
;		B$PNI2, B$PNR4:	Handles POINT.
;		DO_PSET:		Handles PSET, PRESET.
;
;	    GW2GRP.ASM:
;		B$VIEW, B$VEW0:	Handles VIEW.
;
;	    GWPAL.ASM:
;		B$PAL0, B$PAL2,	Handles PALETTE.
;		B$PALU
;
;	    GWSCR.ASM:
;		B$CSRL:		Handles CSRLIN.  No changes.
;
;	    IOTTY.ASM:
;		B$FPOS:		Handles POS.  No changes.
;
;	    LLASCN:
;		B$PCOPYS:		Handles PCOPY.
;
;	    LLCSCN:
;		B$SCREEN:		Handles SCREEN().
;		B$SCRSTT:		Handles SCREEN.
;		B$SWIDTH:		Handles WIDTH.
;		B$SETCLR:		Handles COLOR.
;
;	    LLSCNIO:
;		B$CSRDSP:		Handles INPUT, LINE INPUT, PRINT,
;						WRITE, LOCATE, KEY, SHELL,
;						VIEW PRINT, TAB, SPC, WINDOW.
;		B$CLRSCN:		Handles CLS.
;		B$SCROUT, B$STRSOUT:	Handles PRINT, WRITE.
;
;	    PAINT.ASM:
;		B$PAIN, B$PNTC:	Handles PAINT.
;
;

cProc	B$SCINIT,<PUBLIC,NEAR>
cBegin
	TEST	b$IOFLAG,SCN_INIT ; init done already?
	JZ	DO_INIT		; brif not -- do it now
INIT_DONE:			; took initialization code out of here
				; so that most calls will fall through
cEnd


cProc	DO_INIT,<NEAR>,<AX,BX,CX,DX>	
cBegin				
	OR	b$IOFLAG,SCN_INIT ; signal screen init done

	test	b$SCNFLAG,SCN_RESET	;startup screen mode change?
	jz	NoReset 	;go if not
	call	B$ChkMonitor	; Set VGA monitor type before setting mode
	call	[b$SetMode]	;set the new screen mode
	call	B$FixTextPage	; fix b$CurrPSize, b$PageTable

NoReset:			
	mov	ax,b$CurPages	
	call	[b$SetPages]	; Set current page data

	; move up one line if on the last one
	TEST	b$IOFLAG,SCN_SCROLL ; are we to scroll the screen?
	JZ	NOT_LAST	; brif not

	CALL	B$SCROLL	; scroll up 1 line
NOT_LAST:

	CALL	[b$PalReset]	;initialize the palette

cEnd				; restore registers & return


;***
; B$SetAdapter
;
;Purpose:
;	Determine configuration of video adapter, monitor and memory.
;	Major rewrite (from $SETCL4) with [21].
;Entry:
;Exit:
;	b$Adapter, b$Monitor, and b$VideoMem updated
;	b$EgaPalSup set as appropriate
;Uses:
;Exceptions:
;****

cProc	B$SetAdapter,<PUBLIC,NEAR>,<AX,BX,CX>
cBegin
	PUSH	ES		
	PUSH	DI		

	
	mov	ax,StdColor	;AH gets 0, AL gets CGA mask
	mov	b$Adapter,al	;Assume CGA, no EGA or VGA card present
	mov	b$VideoMem,16	;  w/16K video memory
	mov	b$Monitor,al	;Assume std color monitor
	mov	b$EgaPalSup,ah ;  no EGA palette support

	PUSH	DS		;set ES=DS
	POP	ES		;  for vGetVgaInfo call
	MOV	DI,OFFSET DGROUP:b$Buf1 ; dest. buffer for VGA info
	XOR	BX,BX		;Bios sub-function to return VGA info
	SCNIOS	vGetVgaInfo	;Returns: [AL] = 1B (vGetVgaInfo) if VGA
				;	      if VGA, 64 bytes of info at ES:DI

	CMP	AL,vGetVgaInfo	;is VGA present?
	JNE	NOTVGA		;no, go check for EGA
	MOV	BL,[DI+31H]	; available video memory
	MOV	AL,[DI+2Dh]	; VGA-MCGA-PS/2 Miscellaneous state info
	MOV	[VgaMiscInfo],AL ; save state info for B$ChkMonitor
	LES	DI,[DI] 	;long pointer to static functionality table
	MOV	AX,ES:[DI]	; find out which VGA modes hardware supports
	MOV	[b$VGAmodesL],AX ; save for Screen mode validation
	MOV	AL,ES:[DI+2]	; same for modes 10h-13h
	MOV	[b$VGAmodesH],AL ; save for Screen mode validation

;	The first three bytes of the static functionality table define (by bit
;	flags) which bios modes are supported by the current graphics adapter.
;	Bios mode 12H is supported by the VGA but not by the MCGA.  We can tell
;	which of these two we are working with by testing the bit which
;	corresponds to that bios mode.

	MOV	b$Adapter,MCGA	;assume MCGA
	MOV	b$Monitor,AnalogColor ; analog color monitor (in case MCGA)
	TEST	AL,VGAmode12h	; is bios mode 12H supported?
	JZ	MonOK		;no, we must have an MCGA
; At this point we know we have a VGA, but we must check if there is
; a second adapter (MDPA, HGC, CGA) installed and, if so, which one is
; currently active.  ADJUST_VGA returns with PSW.C set if VGA not active.
; Also, b$Monitor, b$VGAmodeL, and b$VGAmodeH are adjusted accordingly.
	MOV	b$Monitor,AnalogColor + AnalogMono ; analog monitor
	CALL	ADJUST_VGA	; check/adjust for 2nd adapter
	JC	TRANS		; brif VGA not active adapter
	MOV	b$Adapter,VGA	;using VGA
	MOV	b$EgaPalSup,1	;have EGA palette support
	JMP	SHORT MonOK	;translate video memory size and then exit
NOTVGA: 			

	MOV	BL,10h		;% Bios sub-function to return EGA information
	SCNIOS	vAltSelect	;% Returns: [BH] = 0=color/1=mono mode
				;%	    [BL] = memory on ega card
				;	    [CH] = feature bits
				;	    [CL] = switch settings (monitor)

	TEST	BL,11111100B	;if EGA support, only bit 0,1 have values
	JNZ	TRANS		;exit if no EGA

;	At this point we know we have an EGA card, and whether it has
;	monochrome monitor attached.
;	Use the 30H bits in b$CurEquip to determine
;	the active display. 10H for COLOR40, 20H for COLOR80 or 30h for
;	Monochrome.

	mov	al,b$CurEquip	; Read the current equipment setting
	AND	AX,30H		; Knock off unwanted bits : AH = 0
	CMP	AL,30H		; Is it monochrome?
	JNE	COLOR_ACTIVE	; Brif not - color monitor
	INC	AH		; AH = 1 for Monochrome
COLOR_ACTIVE:			
	CMP	AH,BH		; init mode same as EGA mode?
	JNE	TRANS		; Brif not
	mov	b$Adapter,EGA	;using EGA
	mov	b$EgaPalSup,1	;have EGA palette support
	and	cl,0FH		;monitor 7=color,8=enh as color,9=enh,B=mono
	sub	cl,9		;<0=color,0=enh,2=mono
	jc	MonOk		;go if color (the above assumption)
	MOV	CH,EnhColor	; assume enh (cl=0)
	JZ	IsEnh		; it is enh, CH has bit mask we want
	MOV	CH,Monochrome	; must be mono, set up bit mask accordingly
IsEnh:				
	MOV	b$Monitor,CH	; save it
MonOk:
	;BL=amount of EGA memory: 0 -- 64K, 1 -- 128K, 2 -- 192K,3 -- 256K
	inc	bl		;now increments of 64K
	xor	bh,bh
	mov	cl,6
	shl	bx,cl		;times 64 to put in increments of 1K
	mov	b$VideoMem,bx	;save it
	JMP	SHORT SETEXT	;exit

; this label reached only if EGA card is not installed or is not the
; current active card.

TRANS:

	mov	al,b$CurEquip	;check if equipment set for MDPA
	AND	AL,00110000B	;by isolating bits 4 and 5
	CMP	AL,00110000B	;and checking for both set
	JNZ	SETEXT		;BRIF color correct (not MDPA)
	dec	b$Monitor	;else MDPA
	dec	b$Adapter	;  and mono monitor
	mov	b$VideoMem,4	;w/4K video memory

SETEXT:				
; check for OGA here if CGA, EGA, or VGA
	test	[b$Adapter],CGA+EGA+VGA ; only these could be Olivetti
	jz	ChkHerc		; brif cannot be Olivetti
	xor	ax,ax		; ensure AX = 0 if lloga.ob3 not linked in
	call	[b$pChkOlivetti] ; check if Olivetti 640x400 mode supported
	jz	ChkHerc		; returns AL=0, PSW.Z = 1 if not supported
	or	[b$Adapter],al	; Assumes b$Adapter never CMPed in runtime!
	cmp	[b$VideoMem],32	; will have at least 32K
	ja	ChkHerc		; brif already set
	mov	[b$VideoMem],32 ; set 32K video memory
ChkHerc:			
	mov	dx,-1		; DX will not change if no driver
	SCNIOS	0EFH		;test for Hercules INT10 driver
	cmp	dl,-1		;DL returns -1 if no driver or no HGC
; code to handle CGA plus HGC 
	je	setext2		; brif so, already set to CGA or MDPA above
	test	[b$Adapter],MDPA ; do we have a monochrome display?
	jz	half_mode	; brif not, make CGA work with HGC
	mov	b$Adapter,HGC	;set adapter to HGC
	mov	b$VideoMem,64	;w/64K video memory
	or	dh,dh		; see if driver indicated HALF mode
	jnz	setext2		; brif not, assume only card
	shr	[b$VideoMem],1	; only 32K with second card
half_mode:			; make HGC work with CGA
	mov	dx,03BFh	; Hercules configuration port
	mov	al,HALF		; make sure only in HALF mode
	out	dx,al		; write to HGC config. port

setext2:			

	POP	DI		
	POP	ES		
cEnd

;*** 
;ADJUST_VGA - Check if VGA is active and which modes it supports
;
;Purpose:
;	Checks for multiple adapters when one is a VGA.
;
;Entry:
;	b$Monitor = AnalogColor + AnalogMono
;
;Exit:
;	b$CurEquip possibly updated.
;	PSW.C set if VGA not active adapter.
;	b$VGAmodeL and b$VGAModeH adjusted as necessary.
;	b$Monitor adjusted for active display (if PSW.C, = StdColor)
;
;Uses:
;	AX.
;
;Preserves:
;	ES.
;
;Exceptions:
;	None.
;
;******************************************************************************
cProc	ADJUST_VGA,<NEAR>,<ES>
cBegin				; entire routine
	XOR	AX,AX
	mov	ES,AX
	MOV	AX,ES:[488h]
	TEST	AH,1		; see if VGA handles all modes
	JNZ	vga_exit	; brif so
	TEST	AL,8		; see if display attached
	JNZ	got_display
	XOR	AL,2		; reverse BIT 1
got_display:
	MOV	AH,ES:[BiosEquip] ; read current BiosEquip
	mov	b$CurEquip,ah	; update the current equipment setting
	AND	AH,30H		; Knock off unwanted bits
	CMP	AH,30H		; Is it monochrome?
	JE	flag_Set	; Brif so
	XOR	AL,2		; invert for color monitor
flag_set:
	TEST	AL,2
	JZ	no_vga		; VGA not active
	CMP	AH,30h		; MONO?
	je	setvgamono	; brif so
	and	b$VGAmodesL, NOT (VGAMode7h+VGAModeFh)
	MOV	b$Monitor,AnalogColor
	jmp	short vga_exit
setvgamono:
	mov	b$VGAmodesH,0
	and	b$VGAmodesL, VGAMode7h+VGAModeFh
	MOV	b$Monitor,AnalogMono
	jmp	short vga_exit
no_vga:
	XOR	AX,AX
	MOV	b$VGAmodesH,AL
	MOV	b$VGAmodesL,AX
	MOV	AL,StdColor	; reset back to defaults:
	MOV	b$Adapter,AL	; CGA
	MOV	b$Monitor,AL	; StdColor monitor
	STC			; PSW.C indicates VGA not active
vga_exit:
cEnd







;*** 
;B$ChkMonitor - Make sure VGA set for mono or color appropriately
;
;Purpose:
;	This routine ensures that a VGA card is properly set up for whatever
;	Bios mode we are about to go into, monochrome or color.
;	Added with revision [49].
;	Rewritten with revision [64].
;	Note: this is unnecessary for PS/2 VGA and Olivetti VGA
;
;Entry:
;	[b$BiosMode] set to desired BIOS mode
;	B$Adapter & VgaMiscInfo initialized.
;
;Exit:
;	[b$CurEquip] updated.
;	VGA card set up for desired mode.
;
;Uses:
;	Per convention.
;
;Preserves:
;	ES.
;
;Exceptions:
;	None.
;
;******************************************************************************

cProc	B$ChkMonitor,<PUBLIC,NEAR>
cBegin
	CMP	[b$Adapter],VGA ; VGA? (not MCGA or Olivetti VGA)
	JNE 	EndOfChk	; no - nothing to do
	PUSH	ES		; preserve ES
	XOR	AX,AX		; ES = 0
	MOV	ES,AX
	MOV	AH,ES:[BiosEquip] ; read current BiosEquip
DbAssertTst	AX,NZ,2000h,RT_TEXT,<Unexpected BiosEquip flag in B$ChkMonitor>
	OR	AH,30h		; set equipment flag for MONO (default)
	MOV	AL,[b$BiosMode] ; check desired BIOS mode for 7 of F.
DbAssertRelB	AL,B,17h,RT_TEXT,<Advanced BIOS mode in B$ChkMonitor>
	AND	AL,7		
	SUB	AL,7		; Is it a monochrome mode?
	JZ     NeedMono		; brif & set Adapter to monochrome
	AND	AH,0EFh		; set equipment flag for Color80
	MOV	AL,1		; turn off summing if color monitor
NeedMono:
	MOV	[B$CurEquip],AH ; update [b$CurEquip]
	MOV	ES:[BiosEquip],AH ; update current BiosEquip
	POP	ES
	TEST	[VgaMiscInfo],4	; monochrome analog monitor?
	JNZ	EndOfChk	; brif so -- okay as is
	MOV	BL,33h		; turn gray-scale summing on/off
	SCNIOS	vAltSelect	
EndOfChk:

cEnd


;***
;B$GWTERM - OEM termination
;OEM-interface routine
;
;Purpose:
;	This routine is called once immediately before BASIC terminates.
;	It contains machine specific code to put the system into
;	a deterministic final state.  The final state should be as close
;	to the initial startup state as possible.
;
;	NOTE: All of the screen termination is done by a call to
;	      B$RESETSCN from a different part of the runtime.  This
;	      routine should not try to reset the screen in any way.
;
;Entry:
;	None
;
;Exit:
;	None
;
;Uses:
;	Per convention
;
;Exceptions:
;	None
;****

cProc	B$GWTERM,<PUBLIC,NEAR> 
cBegin

; All of the screen restoration code has been moved from here
; into B$CNTERM.


	IN	AL,MSKREG	; get IMR into [AL]
	OR	AL,01H		; mask out timer interrupt
	PAUSE			; make sure instruction fetch has occurred
	OUT	MSKREG,AL	; write mask to IMR

	CLI
	PUSH	DS
	XOR	AX,AX
	MOV	DS,AX
	ASSUME	DS:NOTHING
	RSTVEC	TMRCTL,DS:TICSAV ;restore clock interrupt
	POP	DS

	ASSUME	DS:DGROUP

	XFRINT	KYBINT,KBDVEC/4 ;move INT EFH to INT 09H

	PUSH	DS		;save seg register...
	RSTVEC	KBDVEC/4,b$KBDVEC_SAVE ;restore INT EF vector
	POP	DS		;restore seg register
	PUSH	DS		;save seg register...
	RSTVEC	CLKVEC/4,b$CLKVEC_SAVE ;restore INT F0 vector
	POP	DS		;restore seg register

	IN	AL,MSKREG	; get IMR into [AL]
	AND	AL,0FEH 	; unmask timer interrupt
	PAUSE			; make sure instruction fetch has occurred
	OUT	MSKREG,AL	; write mask to IMR
	STI

cEnd				


;***
;B$SEGINI - Initialize Variable for B$GETDS
;OEM-interface routine
;
;Purpose:
;	This routine stores the address of the runtime data segment.
;	The value is stored in the code segment so that it can be
;	retrieved later by a call to B$GETDS. These two routines
;	are used to save the data segment across a run, chain, or
;	shell command.	The runtime will call B$SEGINI before it
;	needs to call B$GETDS.
;
;	Note that it is possible that B$SEGINI is called multiple
;	times throughout the execution of a program.
;
;	This is a DOS 3 only routine.
;
;Entry:
;	DS = Final runtime data segment address
;
;Exit:
;	DS stored.
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX, BX, CX, DX, FLAGS
;
;Exceptions:
;	none
;****

cProc	B$SEGINI,<PUBLIC,NEAR>	
cBegin
	MOV	[b$BASDSG],DS	  ;store addr of BASIC data segment
cEnd


;***
;B$GETDS - Get Data Segment for Runtime
;OEM-interface routine
;
;Purpose:
;	This routine is used to obtain the location of
;	the  BASIC data segment. The value is kept in the
;	basic code segment, where it is put by B$SEGINI.
;
;	See also B$SEGINI.
;
;Entry:
;	None.
;
;Exit:
;	[BX] = BASIC data segment
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX, CX, DX, FLAGS
;
;Exceptions:
;	None
;****

cProc	B$GETDS,<PUBLIC,NEAR>	
cBegin				
	MOV	BX,[b$BASDSG]
cEnd				



sEnd	RT_TEXT 		
	END
