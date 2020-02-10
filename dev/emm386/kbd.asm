.386p
page	58,132
;******************************************************************************
	title	KBD.ASM - - protected mode AT keyboard driver
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:    EMM386D.EXE - MICROSOFT Expanded Memory Manager 386 DEBUG Driver
;
;   Module:   KBD.ASM - - protected mode AT keyboard driver for debugger
;
;   Version:  0.04
;
;   Date:     January 31, 1986
;
;   Author:
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   01/31/86  Original
;	      A-WCC	Removed STIs and changed CLI/STIs to keep interrupt
;			status stable (OFF) during debugger execution.	The
;			specific problem was in reporting unexpected traps
;			fielded from Virtual Mode during DOS execution,
;			e.g. timer ticks.
;	      B-WCC	Fixed Ctrl-NumLock, Ctrl-Alt-Del, Ctrl-Break, and
;			Shift-PrtSc
;   05/12/86  C-RRH	Cleanup and segment reorganization
;   06/28/86  0.02	Name changed from CEMM386 to CEMM (SBP).
;   07/05/86  0.04	Moved to DCODE segment (SBP).
;
;******************************************************************************
;
;   Functional Description:
;
;	THIS CODE IS USED BY THE DEBUGGER ONLY !
;
;	This is a PC/AT keyboard driver capable of running
;	in protected mode. It does not require any ROM support.
;
;	The major modifications are:
;
;		- Remove foreign tables, use US only
;		- Hard code machine type rather than looking in ROM
;		- hard code BeepFreq, BeepDur
;		- removed KeyVector, put read-only data in CS
;		- removed Accent stuff, which had code writes
;		- removed code writes in foreign kbd
;		- removed INT 15h sysreq and post
;		- removed T&SR stuff, added buffer read routine "getc"
;		- made it polled, removed all interrupt stuff
;		- changed "data" segment to "romdata"
;
;
;	SCCSID = @(#)keybxx.asm 4.1 85/10/09
;------------------------------------------------------
;
;	KEYBXX - foreign keyboard driver.
;
;	April 1985 by Michael Hanson
;	Copyright 1985 by Microsoft Corporation
;
;	KeybXX is a keyboard handling program using tables
;	supplied in a separate file to do foreign language
;	keyboard support. It is the basis for the KEYB??.EXE
;	programs which use this program and the corresponding
;	table defined in KEYB??.ASM.
;
;	KeybXX.OBJ must be linked with one of the Keyb??.OBJ
;	programs to work, the KEYB?? file must be first.
;	See the accompanying makefile for examples.
;
;	Note: KEYB?? refers to any of KEYBFR ( French ),
;		KEYBGR (German), KEYBUK (United Kingdom),
;		KEYBIT (Italian), KEYBSP (Spanish) and
;		KEYBDV (Dvorak).  These are the currently
;		defined data tables for KEYBXX.
;
;	Compatability notes:
;	1.	The IBM foreign keyboard drivers don't return
;		anything for a CTRL-ALT space.	This is not
;		what I expect from the manuals, but for
;		compatibility, KEYBXX doesn't return anything
;		in this case either.
;
;	2.	For the AT the keyboard driver should do a post
;		call (int 15).	The ROM keyboard driver does, but
;		IBM's foreign keyboard drivers appear not to.
;		Currently KEYBXX does a post code, though only
;		one is issued at any one time (that is, only 1 post
;		call for the 2 characters returned by an illegal
;		accent combination).
;
;	This program is a modified version of the keyboard handler from -
;
;	Microsoft Mach 10 Enhancement Software
;
;	Copyright 1984 by Microsoft Corporation
;	Written June 1984 by Chris Peters
;
;******************************************************************************
.lfcond 				; list false conditionals

include vdmseg.inc
include desc.inc
include kbd.inc
include oemdep.inc

;
;***	Routines used by data table modules (Keyb??)
;
public	SpecKey
public	AlphaKey
public	NormalKey
public	Keys$2$13
public	CapKey
public	Cap$2$13
public	FuncKey
public	PadKey
public	NumKey
public	SpaceKey
public	ShiftKey
public	ScrollKey
public	StateKey
public	AltShiftKey
public	BufferFull
public	ReBoot
public	XBoot
public	PrintScreen
public	SysReq

DCODE	 segment
	assume	cs:DCODE,ds:romdata,es:nothing

;***	Tables for foreign language key layout
;	See Keyb?? files for more details

;*	Tables to map CNTRL ALT char
AltChrs 	label	byte
AltChrsEnd	label	byte
AltMap		label	byte

;*	Tables to map accented characters
AccentChTbl	label	word

AccentMpTbl	label	word

;*	Table of accent characters, shifted, ALTed and CTRLed.
;	defined using the AccChStruc struct
AccChTbl	label	word

;***	Internal variables used by KEYBXX interrupt handler
PCType		db	0fch	; type of PC running on

PC_AT	= 0FCh			;if anything else, assume PC/XT

BeepFreq	dw	ATBeepFreq	;Count for beep half-cycle
BeepDur 	dw	ATBeepDur	;Count of half-cycles to beep

;***	Normal keyboard table, used in CTRL-ALT F1 mode
;
;	See Keyb?? files for structure information.
;
ForeignTable	label	word
KeyMapTable	label	word
	public	KeyMapTable

	db	0,0			;0
	dw	BufferFull
	db	escape,escape 		;1
	dw	SpecKey
	db	"1","!" 		;2
	dw	Keys$2$13
	db	"2","@" 		;3
	dw	Keys$2$13
	db	"3","#" 		;4
	dw	Keys$2$13
	db	"4","$" 		;5
	dw	Keys$2$13
	db	"5","%" 		;6
	dw	Keys$2$13
	db	"6","^" 		;7
	dw	Keys$2$13
	db	"7","&" 		;8
	dw	Keys$2$13
	db	"8","*" 		;9
	dw	Keys$2$13
	db	"9","(" 		;10
	dw	Keys$2$13
	db	"0",")" 		;11
	dw	Keys$2$13
	db	"-","_" 		;12
	dw	Keys$2$13
	db	"=","+" 		;13
	dw	Keys$2$13
	db	8,127			;14
	dw	SpecKey
	db	9,0			;15
	dw	NormalKey
	db	"q","Q" 		;16
	dw	AlphaKey
	db	"w","W" 		;17
	dw	AlphaKey
	db	"e","E" 		;18
	dw	AlphaKey
	db	"r","R" 		;19
	dw	AlphaKey
	db	"t","T" 		;20
	dw	AlphaKey
	db	"y","Y" 		;21
	dw	AlphaKey
	db	"u","U" 		;22
	dw	AlphaKey
	db	"i","I" 		;23
	dw	AlphaKey
	db	"o","O" 		;24
	dw	AlphaKey
	db	"p","P" 		;25
	dw	AlphaKey
	db	"[","{" 		;26
	dw	NormalKey
	db	"]","}" 		;27
	dw	NormalKey
	db	13,10			;28
	dw	SpecKey
	db	CtrlShift,(255-CtrlShift)
	dw	ShiftKey
	db	"a","A" 		;30
	dw	AlphaKey
	db	"s","S" 		;31
	dw	AlphaKey
	db	"d","D" 		;32
	dw	AlphaKey
	db	"f","F" 		;33
	dw	AlphaKey
	db	"g","G" 		;34
	dw	AlphaKey
	db	"h","H" 		;35
	dw	AlphaKey
	db	"j","J" 		;36
	dw	AlphaKey
	db	"k","K" 		;37
	dw	AlphaKey
	db	"l","L" 		;38
	dw	AlphaKey
	db	";",":" 		;39
	dw	NormalKey
	db	"'",'"' 		;40
	dw	NormalKey
	db	"`","~" 		;41
	dw	NormalKey
	db	LeftShift,(255-LeftShift)
	dw	ShiftKey
	db	"\","|"                 ;43
	dw	NormalKey
	db	"z","Z" 		;44
	dw	AlphaKey
	db	"x","X" 		;45
	dw	AlphaKey
	db	"c","C" 		;46
	dw	AlphaKey
	db	"v","V" 		;47
	dw	AlphaKey
	db	"b","B" 		;48
	dw	AlphaKey
	db	"n","N" 		;49
	dw	AlphaKey
	db	"m","M" 		;50
	dw	AlphaKey
	db	",","<" 		;51
	dw	NormalKey
	db	".",">" 		;52
	dw	NormalKey
	db	"/","?" 		;53
	dw	NormalKey
	db	RightShift,(255-RightShift)
	dw	ShiftKey
	db	"*",114 		;55
	dw	PrintScreen
	db	AltShift,(255-AltShift) ;56
	dw	AltShiftKey
	db	" "," " 		;57
	dw	SpaceKey
	db	CapsState,(255-CapsState)
	dw	StateKey
	db	1,1			;59
	dw	FuncKey
	db	2,2			;60
	dw	FuncKey
	db	3,3			;61
	dw	FuncKey
	db	4,4			;62
	dw	FuncKey
	db	5,5			;63
	dw	FuncKey
	db	6,6			;64
	dw	FuncKey
	db	7,7			;65
	dw	FuncKey
	db	8,8			;66
	dw	FuncKey
	db	9,9			;67
	dw	FuncKey
	db	0,0			;68
	dw	FuncKey
	db	NumState,(255-NumState) ;69
	dw	NumKey
	db	ScrollState,(255-ScrollState)
	dw	ScrollKey
	db	0,"7"			;71
	dw	PadKey
	db	1,"8"			;72
	dw	PadKey
	db	2,"9"			;73
	dw	PadKey
	db	3,"-"			;74
	dw	PadKey
	db	4,"4"			;75
	dw	PadKey
	db	5,"5"			;76
	dw	PadKey
	db	6,"6"			;77
	dw	PadKey
	db	7,"+"			;78
	dw	PadKey
	db	8,"1"			;79
	dw	PadKey
	db	9,"2"			;80
	dw	PadKey
	db	10,"3"			;81
	dw	PadKey
	db	11,"0"			;82
	dw	PadKey
	db	12,"."			;83
	dw	ReBoot
	db	0, 0			;84 (On AT only)
	dw	SysReq

;***	Tables for keypad with ALT and control
;	Same for foreign as normal
AltKeyPad	label	byte
	db	7,8,9,-1
	db	4,5,6,-1
	db	1,2,3
	db	0,-1

CtrlKeyPad	label	byte
	db	119,-1,132,-1
	db	115,-1,116,-1
	db	117,-1,118
	db	-1,-1

;***	Table for ALT alphabetical character
;
;	Since uses alpha char as index, this is the same
;	for normal and foreign keyboards.
;
AltTable label	byte
;		 a, b, c, d, e, f, g, h, i, j, k, l, m
	db	30,48,46,32,18,33,34,35,23,36,37,38,50
;		 n, o, p, q, r, s, t, u, v, w, x, y, z
	db	49,24,25,16,19,31,20,22,47,17,45,21,44

	SUBTTL Keyboard Interrupt Handler
;***	Keyboard interrupt handler
;
handler proc	near

KbInt:
	cld
	push	ax
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	ds
	push	es

;	First see if there is any data in the kbd buffer.
;	Return to caller if not.

	in	al, KbStatus
	test	al, 1
	JZ	RestoreRegs
;	jnz	intr1			
intr1:
	mov	ax,romdata
	mov	ds,ax
	call	GetSCode		;Get scan code from keyboard in al
	mov	ah,al			;(ah) = scan code
	cmp	al,-1
;	jnz	KbI1
	JZ	BufferFull		;go handle overrun code
KbI1:
	mov	bx,ax
	and	bx,7fh			;(bl) = scan code without break bit
	cmp	bl,84
;	jle	KBI2
	JG	KeyRet			;ignore code if not in range
KBI2:
	shl	bx,2			;index into lookup table
;	shl	bx,1
	jmp	SHORT KbI23

;*	map CTRL-ALT char
;	look up chars in table, if found then put out corresponding
;	entry from map table.
	mov	si, offset AltChrs - 1	;Set up index to lookup
KbI21:
	inc	si			; Advance to next entry
	cmp	si, offset AltChrsEnd
	jae	SHORT KbI22		;at end of table, so no remap
	cmp	ah, cs:[si]
	jne	KbI21			;this isn't it so loop
;	Found character, so do the mapping
	sub	si, offset AltChrs
	add	si, offset AltMap	;get index into remaped table
	pop	ax			;get scan code
	mov	al, cs:[si]		;get new character to use
	jmp	PutKRet
KbI22:
	pop	ax
KbI23:
	mov	si, offset KeyMapTable
	mov	cx,cs:[si+bx]		;(cx) = lc, uc bytes
	mov	al,cl			;(al) = lc code for key
	mov	dl,[KeyState]		;(dl) = keyboard flags
	jmp	word ptr cs:[si+bx+2]	;Call appropriate key handler
;***
;	for all key handler routines,
;
;	(CX) = uc, lc code bytes from table
;	(DL) = keyboard flags byte (see equates above for bits)
;	(AL) = lc code from cl
;	(AH) = scan code from keyboard

handler endp

	SUBTTL	Key Routines
;***	Key handling routines, called as specified by the key table
;------------------------------------------------------
;
;  Alphabetical key, caps lock works as do CTRL and ALT
;
AlphaKey:
	call	NoBreak
	test	dl,AltShift
	jz	SHORT ak1
	cbw
	add	bx,ax
	mov	ah,[AltTable+bx-"a"]
	jmp	MakeAlt

ak1:	test	dl,CtrlShift
	jz	SHORT ak2
	sub	al,"a"-1
	jmp	PutKRet

ak2:	xor	bh,bh
	test	dl,RightShift+LeftShift
	jz	SHORT ak3
	or	bh,CapsState
ak3:	mov	cl,dl
	and	cl,CapsState
	xor	bh,cl
	jz	SHORT ak4
	mov	al,ch
ak4:	jmp	PutKRet

;------------------------------------------------------
;
;  Keys that do something different when CTRL is down
;
SpecKey:
	call	NoAlt
	test	dl,CtrlShift
	jz	SHORT bsp1
	mov	al,ch
bsp1:	jmp	PutKRet

;-----------------------------------------------------
;
;   Normal, Non Alphabetic key
;
NormalKey:			;These return nothing on ALT
	call	NoAlt
	test	dl,CtrlShift
	jz	SHORT nk0
	jmp	short Ca20 ;ky21

Keys$2$13:			;Keys #2 - 13 have ALT codes 120,...
	call	NoBreak
	test	dl,AltShift
	jz	SHORT Ky2
	add	ah,120-2
	jmp	MakeAlt

ky2:	test	dl,CtrlShift
	jnz	SHORT Ca20	;handle CTRL key same as for CapKey
nk0:
	test	dl,RightShift+LeftShift
	jz	SHORT nk1
	mov	al,ch
nk1:	jmp	PutKRet

;-----------------------------------------------------
;
;   Non Alphabetic key for which cap lock works
;
CapKey: 			; CAPLOCK works, ALT doesn't
	call	NoAlt
	test	dl,CtrlShift
	jz	SHORT ca5
	jmp	short ca20 ;ca3

Cap$2$13:			; KEYS 2-13 with CAPLOCK working
	call	NoBreak
	test	dl,AltShift
	jz	SHORT ca2
	add	ah,120-2
	jmp	MakeAlt

ca2:	test	dl,CtrlShift
	jz	SHORT ca5
ca20:	cmp	ah, 3		;Keep CTRL keys at same scan code locations
	jnz	SHORT ca21
	jmp	MakeAlt
ca21:	cmp	ah, 7
	jnz	SHORT ca22
	mov	al, 30
	jmp	short ca7
ca22:	cmp	ah, 26
	jnz	SHORT ca23
	mov	al, 27
	jmp	short ca7
ca23:	cmp	ah, 27
	jnz	SHORT ca24
	mov	al, 29
	jmp	short ca7
ca24:
	cmp	ah, 43
	jnz	SHORT ca25
	mov	al, 28
	jmp	short ca7
ca25:	cmp	al, '-' 	;Except for - key, which moves around.
	jnz	SHORT ca26
	mov	al, 31
	jmp	short ca7
ca26:	jmp	KeyRet


ca5:	xor	bh,bh
	test	dl,RightShift+LeftShift
	jz	SHORT ca6
	or	bh,CapsState
ca6:	mov	cl,dl
	and	cl,CapsState
	xor	bh,cl
	jz	SHORT ca7
	mov	al,ch
ca7:	jmp	PutKRet

;---------------------------------------------------
;
;  Scroll Lock, Caps Lock, Num Lock
;
ScrollKey:
	test	ah,80h
	jnz	SHORT stk0
	test	dl,CtrlShift
	jz	SHORT stk1
	mov	ax,[KbStart]
	mov	[KbHead],ax
	mov	[KbTail],ax
	mov	[fBreak],80h
	call	EnableKB
	mov	ax,0003 		;*a simulate ^C
	jmp	PutKRet
NumKey: 				; NUM LOCK key
	test	ah,80h
	jnz	SHORT stk0
	test	dl,CtrlShift
	jz	SHORT stk1
	or	[BreakState],HoldState	; CTRL NUMLOCK
	call	VideoOn
nlk1:
	call	handler 		;*a (look for key since no interrupts)
	test	[BreakState],HoldState	; Wait for a key press
	jnz	nlk1
	jmp	RestoreRegs

StateKey:				; Toggle key
	test	ah,80h
	jz	SHORT stk1
stk0:	and	[BreakState],ch 	; Indicate key no longer held down
	jmp	short shf4

stk1:	mov	ah,al
	and	al,[BreakState]
	jnz	SHORT shf4		; Ignore if key already down
	or	[BreakState],ah 	; Indicate key held down
	xor	dl,ah			; Toggle bit for this key
	jmp	short shf3		; And go store it.

;---------------------------------------------------
;
;  Alt Shift
;
AltShiftKey:
	test	ah,80h
	jz	SHORT shf2		; Indicate that ALT key down
	xor	al,al
	xchg	al,[AltKey]		; Find numeric code entered
	or	al,al
	jz	SHORT shf1		; Just reset indicator if none
	and	[KeyState],ch
	xor	ah,ah			; Make it a key with 0 scan code
	jmp	PutKRet

;----------------------------------------------------
;
;  Shift, Ctrl
;
ShiftKey:
	test	ah,80h
	jz	SHORT shf2
shf1:	and	dl,ch			; Unset indicator bit for break code
	jmp	short shf3
shf2:	or	dl,al			; Set indicator bit for make code
shf3:	mov	[KeyState],dl
shf4:	jmp	KeyRet

;----------------------------------------------------
;
;  Reboot System?
;
ReBoot: call	NoBreak 	; Del key pressed, check CTRL ALT DEL
	test	dl,AltShift
	jz	SHORT pdk2
	test	dl,CtrlShift
	jz	SHORT pdkx
XBoot:		     			; Reboot system.
	mov	ax,romdata		; ds = romdata segment
	mov	ds,ax
	mov	[fReset],1234h
;*a
;*a  02/12/86 wcc - use shutdown code 10 and [40:67] to return to real mode
;*a		    and enter the ROM at the CTRL-ALT-DEL entry point
;*a
	cli				; make sure
	mov	al,0Fh or 80h		; shutdown byte address/disable NMI
	out	70h,al			; write CMOS address
	jmp	short $+2		; (delay)
	mov	al,0Ah			; Shutdown code 10 = jump [dword @40:67]
	out	71h,al			; write shutdown code to shutdown byte
;
;   Set up entry point after the reset
;
	mov	ds:[67h],0EA81h 	; offset of CTRL-ALT-DEL entry point
	mov	ds:[67h+2],0F000h	; segment of CTRL-ALT-DEL entry point
;
;   Reset the CPU
;
	mov	al,0FEh 		; FEh = pulse output bit 0 (286 reset)
	out	64h,al			; command to 8042
	hlt				; don't want to coast

;----------------------------------------------------
;
;  Key Pad Key
;
PadKey:
	mov	bl,[AltKey]
	call	NoBreak2
	test	dl,AltShift
	jz	SHORT pdk2		; Not entering a character number
	xor	bx,bx
	mov	bl,cl
	mov	cl,cs:AltKeyPad[bx]	; Get numeric value for this key
	cmp	cl,-1
	jz	SHORT pdk0		; Start over if non-digit key
	mov	al,10
	mul	[AltKey]
	add	al,cl
	jmp	short pdk1
pdk0:	xor	ax,ax
pdk1:	mov	[AltKey],al
pdkx:	jmp	KeyRet

pdk2:	mov	al,0
	test	dl,CtrlShift
	jz	SHORT pdk3
	xor	bx,bx		; Lookup CTRL keypad key code
	mov	bl,cl
	mov	ah,cs:CtrlKeyPad[bx]
	jmp	short pdk6
pdk3:	cmp	ah,74		; - key independent of shift state
	jz	SHORT pdk41
	cmp	ah,78		; + key independent of shift state
	jz	SHORT pdk41
	xor	bx,bx
	test	dl,RightShift+LeftShift
	jz	SHORT pdk4
	or	bh,NumState
pdk4:	mov	cl,dl
	and	cl,NumState
	xor	bh,cl
	jz	SHORT pdk5
pdk41:	mov	al,ch		; use char2 if shifted or in numlock
pdk5:	or	al,al
	jnz	SHORT pdk7
pdk6:	cmp	ah,-1
	jz	SHORT pdk8	; Ignore CTRL with keypad 2, etc.
	cmp	ah,76
	jz	SHORT pdk8
pdk7:	jmp	PutKRet
pdk8:	jmp	KeyRet

;----------------------------------------------------
;
;  Function Key
;
FuncKey:
	call	NoBreak
	test	dl,AltShift+CtrlShift+LeftShift+RightShift
	jz	SHORT fk1		; Normal function key
	add	ah,84-59
	test	dl,AltShift+CtrlShift
	jz	SHORT fk1		; Shifted function key
	add	ah,10
	test	dl,AltShift
	jz	SHORT fk1		; Just CTRL function key
	add	ah,10
	test	dl,CtrlShift
	jz	SHORT fk1		; Just ALT  function key
	mov	bx,offset KeyMapTable
	cmp	ah,104			; CTRL ALT f1 to use normal keyboard
	jz	SHORT fk01
	cmp	ah,105			; CTRL ALT f2 for foreign keyboard
	jnz	SHORT fk1		; if not F1 or F2 then treat as ALT
	mov	bx,offset ForeignTable
fk01:
	cli				; Change translation table used
	jmp	KeyRet

fk1:	jmp	SHORT MakeAlt

;--------------------------------------------------------------------
;
;  Print Screen Key
;
PrintScreen:
	call	NoAlt
	test	dl,CtrlShift
	jz	SHORT ps1
	mov	ah,ch
	jmp	fk1
ps1:	test	dl,LeftShift+RightShift
	jz	pdk7
	call	VideoOn 	;CTRL PrtSc - enable video and do Print Screen
	jmp	SHORT RestoreRegs

;--------------------------------------------------------------------
;
;  Space Key
;
SpaceKey:
	call	NoBreak
	test	dl, CtrlShift
	jz	SHORT sp1
	test	dl, AltShift
;	jz	SHORT sp1
	JNZ	SHORT KeyRet		; Don't return anything on CTRL-ALT space
sp1:
	jmp	SHORT PutKRet

;
;  System Request Key
;
SysReq:
	test	ah, 80h
	jnz	SHORT sys2		;this is break code
	test	BreakState, SysShift
;	jz	sys1
	JNZ	SHORT KeyRet		;Ignore if SysReq already down

sys1:
	or	BreakState, SysShift		;set held down flag
	mov	ax, 08500h
	jmp	short sys3
sys2:
	and	BreakState, Not SysShift	;turn off held down flag
	mov	ax, 08501h
sys3:
	push	ax				; Save SysReq action number
	mov	al,20h				; EOI to control port
	call	EnableKB
	pop	ax
	jmp	SHORT RestoreRegs

;***	Finish up processing of interrupt
;
;*	Make this an ALT seq by removing chr code (ret scan code, 0)
MakeAlt:mov	al,0

;*	Put Key in buffer and return
PutKRet:
	call	PutKey
	cmp	[PCType],PC_AT
	jnz	SHORT KeyRet		; just return for non-AT
	cli
	mov	al,20h			; EOI to control port
	call	EnableKB
	jmp	SHORT RestoreRegs

;*	Common validity check routines (Check for ALT, ignore break codes)
;

NoAlt:	test	dl,AltShift		; Don't allow ALT with this key
	jnz	SHORT IgB1
NoBreak:				; Ignore break code for this key
	mov	bl,0
NoBreak2:
	test	ah,80h
	jnz	SHORT IgB1
	test	[BreakState],HoldState	; in hold state?
	jz	SHORT IgB0		; no...
	and	[BreakState],(255-HoldState)
	jmp	short IgB1
IgB0:	mov	[AltKey],bl
	ret
IgB1:	pop	ax		; pop off return address
	jmp	short KeyRet

;*	buffer is full, beep the speaker and return from interrupt
BufferFull:
	cli
	mov	al,20h
	call	ErrBeep
	jmp	short KeyRet1

;*	Normal return from interrupt, handle EOI and enable KB
KeyRet:
	cli
	mov	al,20h
KeyRet1:
	call	EnableKB
RestoreRegs:
	cli
	pop	es
	pop	ds
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
ppp	proc	near
	ret
ppp	endp

	SUBTTL Subroutines
;***	VideoOn - enable keyboard and video
;
;	ENTRY:	Nothing
;
;	EXIT:	(dx), (al) destroyed.
;
VideoOn proc near
	mov	al,20h		; EOI to control port
	call	EnableKB	; Enable AT keyboard
	cmp	[VidMode],7
	jz	SHORT vdo1	; Do nothing for monochrome monitor
	mov	al,[VidReg]
	mov	dx,3d8h
	out	dx,al		; Enable video controller
vdo1:	ret
VideoOn endp

;***	EnableKB - Enable the keyboard interface on an AT, no effect on PC/XT.
;
;	ENTRY:	Nothing
;
;	EXIT:	(al) destroyed
;
;	EFFECTS:	Enables the Keyboard interface.
;
EnableKB	proc near
	cmp	[PCType], PC_AT
	jne	SHORT ena1		;for non-AT simply ignore
	pushf			;* save interrupt status
	cli
	call	WaitStatus
	mov	al,0AEh 	;output enable keyboard command
	out	KbStatus,al
	popf			;* restore original interrupt status
ena1:
	ret
EnableKB	endp

;***	DisableKB - Disable the keyboard interface on an AT, no effect on PC/XT
;
;	ENTRY:	Nothing
;
;	EXIT:	(al) destroyed
;
;	EFFECTS: Disables the Keyboard interface.
;
DisableKB	proc near
	cmp	[PCType], PC_AT
	jne	SHORT dis1		; Ignore if not an AT
	pushf			;* save interrupt status
	cli
	call	WaitStatus
	mov	al,0ADh 	;output disable command
	out	KBStatus, al
	popf			;* restore original interrupt status
dis1:
	ret
DisableKB	endp

;***	ErrBeep - beep the speaker
;
;	ENTRY:	Nothing
;
;	EXIT:	Nothing
;
;	USES:	(ax) - to access I/O port
;		(bx) - length of beep in cycles
;		(cx) - counter for cycle length
;
;	EFFECTS: Speaker is beeped
;
;	WARNING: Uses in/out to keyboard port to beep speaker directly.
;
ErrBeep proc near
	push	ax
	push	bx
	push	cx
	mov	bx,BeepDur	; count of speaker cycles
	in	al,KbCtl
	push	ax
	and	al,0fch 	; turn off bits 0 and 1 (speaker off)
bee1:	xor	al,2		; toggle speaker bit
	out	KbCtl,al
	mov	cx,BeepFreq
bee2:	loop	bee2		; wait for half cycle
	dec	bx
	jnz	bee1		; keep cycling speaker
	pop	ax		; restore speaker/keyboard port value
	out	KbCtl,al
	pop	cx
	pop	bx
	pop	ax
	ret
ErrBeep endp

;***	PutKey - put key in the buffer
;
;	ENTRY:	(ax) = key code and scn code to go in buffer
;
;	EXIT:	(si), (di) destroyed.
;		ints disabled.
;
;	EFFECTS: KbTail updated
;		(ax) put in buffer at end.
;		On AT - do post call.
;
;	If it isn't possible to put key in buffer (full) then beep
;	and ignore.
;	If (ax) = -1 then the key is not put in buffer.
;
PutKey	proc near
	cmp	ax, -1			; Code to ignore a key
	jz	SHORT put2
	cli				; Make sure only ones using buffer now
	mov	si,[KbTail]
	mov	di,si			; Get old buffer end and save it
	inc	si			; Advance pointer
	inc	si
	cmp	si,[KbEnd]
	jb	SHORT put01
	mov	si,[KbStart]		; Wrap to beginning if at end
put01:
	cmp	si,[KbHead]
	jnz	SHORT put1		; Buffer not Full
	pop	ax			; Drop return address
	jmp	BufferFull		; Go beep and return from interrupt
put1:
	mov	[di],ax 		; Put key in buffer at end
	mov	[KbTail],si
put2:
	ret
PutKey	endp

;***	GetSCode - read the scan code from the keyboard
;
;	ENTRY:	nothing
;
;	EXIT:	(al) = key scan code from keyboard
;		(ah) destroyed
;
;	USES:	PCType - to use PC/AT sequence, for AT - handles LEDs
;
GetSCode	proc near
	cmp	[PCType], PC_AT
	je	SHORT gsc1		;handle AT differently
	in	al,KbData		;get key code
	xchg	bx,ax			;save scan code
	in	al,KbCtl		;acknowledge to keyboard
	mov	ah,al
	or	al,80h
	out	KbCtl,al
	xchg	ah,al
	out	KbCtl,al
	xchg	ax,bx			;(al) = scan code
	ret
gsc1:					;have to do handshake
	call	DisableKB
	pushf				;* save interrupt status
	cli
	call	WaitStatus
	in	al,KbData		;read in character
	popf				;* restore original interrupt status

;	check for and flag control bytes from keyboard
	cmp	al,ATResend
	jne	SHORT gsc2		;it isn't a resend
	cli
	or	[ATKbFlags], KbResend
	pop	bx			;throw away return address
	jmp	KeyRet			;and don't do anything more with key
gsc2:
	cmp	al,ATAck
	jne	SHORT gsc3		;it isn't an ack
	cli
	or	[ATKbFlags], KBAck
	pop	bx			;throw away return address
	jmp	KeyRet			;and don't do anything more with key
gsc3:
	call	UpdateLeds		;update AT's leds
	ret
GetSCode	endp

;***	Don't need to keep code after here when not running on an AT
xt_endcode:

;***	UpdateLeds - update the leds on the AT keyboard
;
;	ENTRY:	Nothing
;
;	EXIT:	All regs preserved
;
;	EFFECTS: Sets the keyboard LEDs according to the status byte.
;
;	WARNING: Assumes it is operating on an AT, must not be called for a PC.
;
UpdateLeds	proc	near
	pushf				;* save interrupt status
	push	ax
	cli
	mov	ah, KeyState		; get the toggle key states
	and	ah, CapsState + NumState + ScrollState
	rol	ah, 1
	rol	ah, 1
	rol	ah, 1
	rol	ah, 1			; in format for ATKbFlags
	mov	al, ATKbFlags
	and	al, 07h
	cmp	ah, al
	jz	SHORT Updn1		; No change in leds, so don't update
	test	ATKbFlags, KBSndLed
	jnz	SHORT Updn1		; Already updating, so don't update
	or	ATKbFlags, KBSndLed
	mov	al, 20h
	mov	al, 0EDh		; Set indicators command
	call	SendByte
	test	ATKbFlags, KBErr
	jnz	SHORT Updn2
	mov	al, ah			; Send indicator values
	call	SendByte
	test	ATKbFlags, KBErr
	jnz	SHORT Updn2
	and	ATKbFlags, 0F8h
	or	ATKbFlags, ah		; Record indicators
Updn2:
	and	ATKbFlags, Not (KBSndLed + KBErr)
Updn1:
	pop	ax
	popf				;* restore original interrupt status
	ret
UpdateLeds	endp

;***	SendByte - send a byte to the keyboard
;
;	ENTRY:	(al) - command/data to send
;
;	EXIT:	BreakState flags set according to success of operation.
;		Ints disabled on completion.
;
;	USES:	(al) - byte to send.
;		(ah) - count of retries.
;		(cx) - time out counter on wait for response.
;
;	Send the byte in al to the AT keyboard controller, and
;	do handshaking to make sure they get there OK.
;	Must not be called for the PC.
;
SendByte	proc near
	push	ax
	push	cx
	mov	ah, 03		; Set up count of retries
Sen1:
	pushf				;* save interrupt status
	cli
	and	ATKbFlags, Not (KBResend + KBAck + KBErr)
	push	ax		; save byte to send
	call	WaitStatus	; Wait for keyboard ready
	pop	ax
	out	KbData, al	; Send byte to keyboard
	popf				;* restore original interrupt status
	mov	cx,2000h	; Time out length, Approximate value for AT ROM
Sen2:				; Wait for ACK
	call	handler 		;*a (look for key since no interrupts)
	test	ATKbFlags, KBResend + KBAck
	jnz	SHORT Sen4
	loop	Sen2
Sen3:				; Timed out - try to resend
	dec	ah
	jnz	Sen1
	or	ATKbFlags, KBErr
	jmp	SHORT Sen5
Sen4:
	call	handler 		;*a (look for key since no interrupts)
	test	ATKbFlags, KBResend
	jnz	Sen3
Sen5:
	cli
	pop	cx
	pop	ax
	ret
SendByte	endp

;***	WaitStatus - wait for status to indicate ready for new command
;
;	ENTRY:	Nothing
;
;	EXIT:	(AL) Destroyed.
;
WaitStatus	proc near
	push	cx
	xor	cx,cx
wai1:				;wait for empty buffer
	in	al,KbStatus
	test	al,BufFull
	loopnz	wai1
	pop	cx
	ret
WaitStatus	endp

	SUBTTL Initialization
;*	Initialization, called when run by DOS, doesn't stay resident.
;
init_bios:
	mov	al,0ffh 		; all OFF
	out	MASTER_IMR,al

	push	ds
	push	cs
	pop	ds		; establish segment, since offsets are from cs
	mov	dx,offset Kbint
	mov	ax,2509h
	int	21h		;Set interrupt 9 (keyboard) vector

	mov	ax, romdata
	mov	ds, ax
init1:	cmp	[KbStart],0
	jnz	SHORT init2		;New PC/AT - KbStart already initialized
	pushf				;* save interrupt status
	cli			;For old PC - initialize pointers to KbBuffer
	mov	ax,offset KbBuffer
	mov	[KbStart],ax
	mov	[KbHead],ax
	mov	[KbTail],ax
	mov	[KbEnd],offset KbBufferEnd
	popf				;* restore original interrupt status
init2:				; Start up in Foreign keyboard mode

	assume	ds:romdata
	pop	ds

	mov	ah, 14
	mov	al, 'i'
	int	10h
ini1:
	call	handler
	jz	ini1
	mov	ah, 14
	int	10h
	jmp	ini1

DCODE	 ends

;***	getc - read character out of keyboard buffer
;
;	This routine gets characters from the buffer
;	in the ROM data area.
;
;	ENTRY
;
;	EXIT	AL - character
;		AH - scan code
;		'Z' = 0
;
;		or 'Z' = 1 if no code available
;
;	USES	flags
;

DCODE	 segment

	assume	cs:DCODE, ds:nothing, es:nothing, ss:nothing

	public	kgetc
kgetc	proc	far

	push	bx
	push	cx
	push	dx
	push	si
	push	di

	mov	bx, 202h
	mov	cx, 303h
	mov	dx, 404h
	mov	si, 505h
	mov	di, 606h

	call	handler 		; pull data into kbd buffer

	mov	bx, 2020h
	mov	cx, 3030h
	mov	dx, 4040h
	mov	si, 5050h
	mov	di, 6060h

	push	ds			; save caller's regs
	push	bx

	mov	bx, romdata
	mov	ds, bx			; DS -> ROM data area

	cli
	mov	bx, ds:[KbHead] 	; bx = start of buffer
	cmp	bx, ds:[KbTail] 	; is buffer empty
	jz	SHORT ge1

	cli
	mov	ax, [bx]		; AX = character and scan code
	add	bx, 2			; step buffer pointer
	cmp	bx, ds:[KbEnd]		; is it at end of buffer
	jne	SHORT ge2
	mov	bx, ds:[KbStart]	; move it back to start
ge2:
	mov	ds:[KbHead], bx 	; store new start pointer
	and	bx, 0ffffh		; just to clear zero flag
ge1:
	pop	bx
	pop	ds

	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx

	ret

kgetc	endp

DCODE	 ends
	end
