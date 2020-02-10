;
; NAME
;	EGA.SYS
;	Video Subsystem EGA Screen Device Driver
;
;	Copyright (C) Microsoft Corporation 1984-1990
;	Copyright (C) IBM 1986-1987
;	Portions Copyright (C) Hewlett-Packard Co. 1987 (by Jim Christy)
;
; WORK-IN-PROGRESS
;	To become version 2.10, this copy still needs :
;	[1] saving/restoring of memory latches in savecontext/restorecontext 
;	    subfunctions	DONE. Marc Pawliger (6/88)
;	[2] alternate save/restore logic in those functions whenever another,
;	    pre-2.0, version of this interface has been loaded on top of us
;	    (this MUST be detected by temporarily locking out our Int10Routine
;	    and issuing an InquireDriver call; only known culprit currently is
;	    Word 3.11) 
;	[3] fully functional VGA support 
;	[4] to be fully tested!
;
; PURPOSE
;	To provide a means of shadowing the state of the EGA's write-only
;	registers, so that the state of the card can be determined at
;	any instant, whether by a Real-mode-only mouse package, a Real-
;	mode pop-up (TSR), or a video manager as found in Windows or OS/2.
;
;	This is done by providing an Extended Int 10h interface (called
;	the EGA Register Interface) for new and existing Real-mode apps,
;	and by providing a set of device driver functions for the video
;	managers.
;
;	If "SYS" is defined, a device driver model is assembled.  Otherwise,
;	a COM file model is assumed, for TSR operation.  If "OS2" is
;	defined, then "SYS" will automatically be defined, and the generated
;	driver will contain the READ and WRITE functions required for OS/2.
;
; SYNOPSIS
;	EGA.SYS is an interface to the EGA card which allows all of the w/o
;	registers to be read from as well as written to.  This is a
;	requirement in order to use interrupt-driven graphics (such as cursor
;	update code).  Interrupt-driven graphics can be implemented on the EGA
;	if both the synchronous app and the interrupt code use the EGA.SYS
;	interface to the EGA registers.  The Microsoft mouse driver (versions
;	3.0 and greater) use the EGA.SYS interface to the EGA card.
;
;	This file contains the code that intercepts the int 10's and keeps
;	shadow maps of all EGA registers so that these registers can be read
;	as well as written.  Note that the primary consideration was speed,
;	some of the calls need to be made in tight graphics loops.  For this
;	reason, the interface may seem arbitrary and the code may seem
;	unnecessarily "tricky" at times.  I've tried to document all such
;	instances.  Also keep in mind that there is no error checking of
;	parameters.  Illegal parameters may cause unpredictable results,
;	including possibly a system crash.
;
; DESCRIPTION
;	The EGA.SYS interface spec follows.  For a more detailed description,
;	see each function header.  Bear in mind that function headers may
;	report a different register usage then that shown here.  They reflect
;	the state of the machine on entry and exit to that particular function
;	when in fact, some regs are restored by the dispatcher and thus do not
;	impact the caller.  For this release, ax, si, and ds will always be
;	restored by the dispatcher unless the spec indicates it specifically
;	returns a value in these regs.
;
;	In most of the following calls, dx is expected to point to a "register
;	id" which identifies the exact EGA chip to be addressed.  Since the 4
;	LSI chips contain multiple regs, the register index (usually in bl) is
;	also required for these chips.	The valid register ids for dx are:
;
;		port	chips/regs		 id
;		----	--------------		----
;		3x4h	crtc chip		000h
;		3C4h	sequencer chip		008h
;		3CEh	graphics chips		010h
;		3C0h	attribute chip		018h
;		3C2h	misc out reg		020h
;		3xAh	feature reg		028h
;		3CCh	gr 1 pos reg		030h
;		3CAh	gr 2 pos reg		038h
;
;
;	ReadReg - read a single EGA register from shadow maps
;
;	ENTRY
;	      ah      =  0F0h
;	      bx      =  register index 	      if reg id is indexd chip
;			 ignored		      if reg id is single reg
;	      dx      =  register id
;	EXIT
;	      bl      =  current register data
;	DESTROYS
;		None
;
;
;	WriteReg - write to a single EGA register and update shadow maps
;
;	ENTRY
;	      ah      =  0F1h
;	      bl      =  register index 	      if reg id is indexd chip
;			 data to be written	      if reg id is single reg
;	      bh      =  data to be written	      if reg id is indexd chip
;		      =  ignored		      if reg id is single reg
;	      dx      =  register id
;	EXIT
;	      none
;	DESTROYS
;	      bh, dx
;
;
;	ReadRange - read a range of EGA registers from shadow maps
;
;	ENTRY
;	      ah      =  0F2h
;	      dx      =  register id		      must be an indexed chip
;	      cl      =  # of registers to read       (must be > 1 !)
;	      ch      =  starting register index
;	      es:bx   -> buffer to put reg data
;	EXIT
;	      none
;	DESTROYS
;	      cx
;
;
;	WriteRange - write to a range of EGA registers and update shadow maps
;
;	ENTRY
;	      ah      =  0F3h
;	      dx      =  register id		      must be an indexed chip!
;	      cl      =  # of registers to write      (must be > 1 !)
;	      ch      =  starting register index
;	      es:bx   -> buffer to get reg data
;	EXIT
;	      none
;	DESTROYS
;	      bx, cx, dx
;
;
;	ReadSet - read a set of EGA registers from shadow maps
;
;	ENTRY
;	      ah      =  0F4h
;	      cx      =  # of registers to read       (must be > 1 !)
;	      es:bx   -> buffer of contiguous SetRec
;			 structures, one for each register
;			 to be read
;	EXIT
;	      none
;	DESTROYS
;	      cx
;
;
;	WriteSet - write to a set of EGA registers and update shadow maps
;
;	ENTRY
;	      ah      =  0F5h
;	      cx      =  # of registers to write      (must be > 1 !)
;	      es:bx   -> buffer of contiguous SetRec
;			 structures, one for each register
;			 to be written
;	EXIT
;	      none
;	DESTROYS
;	      cx
;
;
;	RevertDefault - blast default shadow maps to EGA, update shadow maps
;
;	ENTRY
;	      ah      =  0F6h
;	EXIT
;	      none
;	DESTROYS
;	      none
;
;
;	DefineDefault - load up default shadow maps from user buffer
;
;	ENTRY
;	      ah      =  0F7h
;	      cx      =  'TH' if user wants to program VGA specific color select
;			 register.  This was added version 2.07.12.
;			 Only checked if on a VGA and defining Attribute
;			 Controller Register.
;	      dx      =  register id
;			 If high bit of dx set, an internal RevertDefault will
;			 occur after the default shadow maps have been updated
;
;	      es:bx   -> buffer of default values for specified register id.
;			 If register id is a chip, all values must be present.
;	EXIT
;	      none
;	DESTROYS
;	      bx
;
;
;	GetDefault - dump default shadow maps to user buffer
;
;	ENTRY
;	      ah      =  0F8h
;	      dx      =  register id
;	      es:bx   -> buffer to receive default values
;			 If register id is a chip,table must have room for all
;			 regs in chip.
;	EXIT
;	      none
;	DESTROYS
;	      none
;
;
;	ContextInfo - Save/Restore full EGA.SYS context information
;
;	ENTRY
;	      ah      =  0F9h
;	      al      =  000h  GetContextSize
;		      =  001h  SaveContext
;		      =  002h  RestoreContext
;		      =  003h  GetFontInfo
;		      =  004h  GetInBiosFlag
;
;	      GetContextSize - return size in bytes needed to save 
;			       EGA.SYS context
;
;	      ENTRY
;		    none
;	      EXIT
;		    ax	    =  size in bytes needed to save EGA.SYS context
;	      DESTROYS
;		    none
;
;
;	      SaveContext - dump current EGA.SYS context data to user buffer
;
;	      ENTRY
;		    es:bx   ->	user save area buffer for EGA context
;	      EXIT
;		    none
;	      DESTROYS
;		    none
;
;
;	      RestoreContext - restore EGA.SYS context data from user buffer
;
;	      ENTRY
;		    es:bx   -> previously saved EGA context
;	      EXIT
;		    none
;	      DESTROYS
;		    none
;
;
;	      GetFontInfo - dump current font data in FontInfo format to 
;			    user buffer
;
;	      ENTRY
;		    es:bx   -> user buffer for FontInfo
;	      EXIT
;		    none
;	      DESTROYS
;		    none
;
;
;	      GetInBiosFlag - return segment:offset of the InBiosFlag
;
;	      ENTRY
;		    none
;	      EXIT
;		    es:bx   -> InBiosFlag
;	      DESTROYS
;		    none
;
;
;	InquireDriver - return ptr to driver version number
;
;	ENTRY
;	      ah      =  0FAh
;	EXIT
;	      es:bx   -> DriverInfo data area
;	DESTROYS
;	      bx, es
;
;
;	Calls 0FBh - 0FDh are reserved. 0FEh & 0FFh are used by
;	environments such as Windows, TopView, and Mondrian.
;
; Added new INT 2FH handler for interprocess communication in 2.07.11
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
; NOTES
;	EGA.SYS is reentrant.  This means that it is legal for an interrupt
;	routine to call EGA.SYS while the synchronous program is in the middle
;	of one of its calls to EGA.SYS.  [currently, this is only true if the
;	interrupt routine only saves and restores registers (i.e.  makes no
;	permanent changes) but this could be changed to accomodate interrupt
;	routines that make permanent changes (e.g.  an interrupt routine that
;	changed the palette on a vertical retrace)].
;
;	A special note about the attribute chip registers:  the flip-flop
;	which determines whether 003C0h is the address or data register will
;	always be left in the "address" state on return from EGA.SYS.  Any
;	code which sets the flip flop to "data" and then depends on it being
;	set that way must disable interrupts for the length of time between
;	setting it to "data" and the last time it is assumed to be "data".
;
;	Writing to the sequencer memory mode register is tricky because on
;	some vendors cards the sequencer will produce a 20 ns glitch on the
;	CAS lines when this register is accessed, which seems to have a chance
;	of glitching VRAM even though write enable is never asserted.
;	Therefore, when writing to this register, it is suggested that you
;	first synchronously reset the sequencer by writing a 001h to the
;	sequencer reset register (index 000h), then write your data to the
;	memory mode register, and finally write a 003h back to the reset
;	register.  Not that interrupts MUST be disabled during this whole
;	operation, as EGA RAM is not being refreshed when the sequencer is in
;	a reset state.
;
;	The clocking mode register in the sequencer has also appeared to cause
;	similar problems on IBM boards with slow memory on the daughterboard.
;	The same procedure as above is recommended, though may not be strictly
;	necessary.
;
;	And the same caveat applies to the graphics controller miscellaneous
;	register because it appears to cause a glitch on the CAS lines in a
;	similar to the sequencer memory mode register.	This problem is
;	particularly insideous because it will only show up on EGA cards with
;	the add on memory cards that have INMOS RAM chips.  Beware!!!
;
;	Due to these problems, the RevertDefault function does not restore
;	either the clocking mode register or the miscellaneous register (at
;	least in this release).  Since the ability to context switch the
;	EGA is a must, the RestoreContext subfunction of ContextInfo does
;	handle all three registers in a manner described above.
;
;	This code has no provisions for reading the input status registers on
;	the EGA.  Any code that wants to read either of the input status
;	registers should do so directly.  It does however return the most
;	up-to-date information about the cursor position and CRTC start
;	address (these are readable) when reading the CRTC shadow map.
;
; HISTORY
;	12/24/84  1.40	brian conte	epoch
;
;	11/11/85  1.50	steve shaiman	mod to RevertDefault for glitch in in
;					graphics controller misc. register
;
;	02/14/86  1.51	steve shaiman	mod to ChangeRegs to clear direction
;					flag instead of assuming it's cleared
;
;	02/21/86  1.52	steve shaiman	made pOldInt10, ChangeRegs,
;					Int10Routine public as needed by erice
;
;	06/09/86  1.53	steve shaiman	mod to RevertDefault for glitch in
;					sequencer clocking mode register
;
;	10/30/86  1.60	steve shaiman	added GetDefault as function 0F8h;
;					added SimWriteReg & SimWriteSet for
;					use by routines that intercept
;					standard int 010h calls and want to
;					modify the shadows regs (but not the
;					actual hardware regs);
;					added int 010h BIOS support for
;					functions 005h (set display page),
;					00Bh (set CGA palette registers), &
;					010h (set EGA palette registers)
;
;	04/20/87  2.00	jim christy	added remainder of int 010h shadow
;					functions;
;					cleaned up original shadow functions;
;					merged tom hensel's VGA code from
;					"other" EGA.SYS into this one;
;					added revert immediate bit for
;					DefineDefault per shaiman's request
;
;	05/04/87  2.01	jim christy	added ContextInfo functions (0F9h)
;					needed by Windows and TSR's;
;					changed stack handling by Int10Routine
;					and friends to be cleaner, avoiding
;					static SaveAX-style variables (truly
;					re-entrant now);
;					optimized all write reg functions and
;					RevertDefault to avoid redundent
;					special casing of the attribute
;					controller;
;					changed from an "include" file to a
;					device driver format;
;					added more comments, optimiziation,
;					fixed bugs introduced in v2.00
;
;	05/14/87  2.02	Jeff Parsons	Added GetInBiosFlag ContextInfo
;					subfunction (F904h).  Added device
;					driver interface and OS2 conditional
;					assembly.  Added wait for vert retrace
;					before writing attribute controller.
;					Optimized rep-movs to avoid jumps, and
;					added/modified some IN/OUT macros.
;
;	08/11/87  2.03	Jeff Parsons	Converted dirty byte-flag into bit
;					flags (bit 0 maintained same as old
;					dirty flag, bit 1 always set when 0
;					set but never cleared until mode set).
;					Bit 1 now a "truly-dirty" bit, to be
;					used in future revisions.  Optimized
;					path for shadowed Int 10h calls to
;					minimize performance impact, effected
;					in part through the addition of
;					biosmode, biosinfo, and biostable in
;					the local ds.  Cleaned up CheckEga and
;					the install logic to address all of
;					Jim Christy's concerns.  Modified
;					meaning of fInBIOS flag (only set for
;					actual ROM BIOS calls).  Protected all
;					sections where attr cont flip-flop is
;					data state.  Added workaround for
;					driver install bug on non-ega systems.
;
;	03/16/88  2.04  Tom Hensel	Fixed "Green screen" problem on VGA.
;					This only appears on a Compaq VGA so
;					far, but could occur on any VGA which
;					can switch between monochrome and
;					color.  The problem is that we need
;					to do the BIOS set mode call before
;					we call our SetMode routine in order
;					to correctly determine if we have
;					monochrome or color (which affects
;					the value of the CRT controller port
;					address).
;
;	04/14/88  2.05  Tom Hensel	Fixed VGA shadow register map
;					initialization by reading values
;					directly from the port registers
;					rather than relying on the "magical"
;					BIOS register mode init table.
;
;	04/22/88  2.06  Tom Hensel	ChangeRegs didn't handle color/mono
;					properly for VGA
;
;	06/27/88  2.07  Marc Pawliger	Added save/restore ega latches on 
;		  Beta 2		a save/restore context call. Latches
;					are stored in all planes at offset
;					9600H plus an added offset used by the
;					mouse driver for its internal data.
;					9600 is just past the memory used by
;					the VGA at highest resolution.
;
;	07/06/88  2.07	Marc Pawliger	OK, OK.  The 9600H scheme didn't work.
;		  Beta 3		So in the true pioneering spirit, the
;					memory at A000:CBEA was chosen since
;					it's right in the middle of free vram.
;					It ain't scientific, but it works.
;
;	07/13/88  2.07	Marc Pawliger	Well, so much for pioneering.  We now
;		  Beta 4		use 7FFF for storage in all but the
;					high-res VGA modes, when FFFF is used.
;					These work for all EGA modes, including
;					mode FH and 10H on a 64K EGA.
;
;	08/02/88  2.07  Tom Hensel	Bad video mode value for INT 10H
;		  Beta 5		VGA initialization fixed
;					Bug fix for EGA latch code in
;					RestoreContext.
;
;	08/31/88  2.07  Tom Hensel	DX register was being destroyed in
;		  Beta 6		SaveContext routine.  Fix stack
;					problems in RestoreContext.
;
;	09/06/88  2.07  Tom Hensel	Don't let EGA latch code change EGA
;		  Beta 7		state in SaveContext routine.
;
;	09/14/88  2.07  Tom Hensel	Choose a different latch location
;		  Beta 8		based on video mode.
;
;	09/15/88  2.07  Tom Hensel	Some more non visible video memory work
;		  Beta 9
;
;	10/19/88  2.07  Tom Hensel	General Cleanup for efficiency
;		  Beta 10		Added better method (although DOS
;					version dependent) to error exit
;					from Init with least amount of
;					resident code for sys driver.
;
;	11/01/88  2.07  Tom Hensel	General Cleanup for efficiency
;		  Beta 11		Added ability to unload .COM version
;					Side effect is new INT 2FH handler
;					Added 80286 instructions for OS/2
;
;	11/02/88  2.07  Tom Hensel	Fix register corruption bug in
;		  Beta 12		DownloadFont from last beta
;					Added CX='TH' input to acknowledge
;					use of VGA specific color select
;					register in Define Defaults call.
;
;	11/03/88  2.07  Tom Hensel	Efficiency
;		  Beta 13
;
;	11/06/88  2.07  Tom Hensel	No messages for OS/2
;		  Beta 14
;
;	11/19/88  2.07  Tom Hensel	Added ChooseMonoColorDisplay macro
;		  Beta 15		to fix bug reported for OS/2 with
;					a Video Seven V-RAM VEGA VGA card
;					version 1.04.  My guess is that the
;					INT 10H to find out color/mono
;					caused the problem.
;
;	11/20/88  2.07  Tom Hensel	Last fix broke DOS version
;		  Beta 16
;
;	01/19/89  2.08  Tom Hensel	Fix warning message
;		  Beta 1
;
;	01/25/89  2.08  Tom Hensel	True blue IBM VGA cards don't
;		  Beta 2		handle read/write for the Attribute
;					Controller Registers (thus violating
;					their own spec), so we have to
;					initialize these like we do the EGA
;					(by reaching into a hardcoded BIOS
;					location, yuck!)
;
;					SetMode for VGA handled incorrectly.
;					Was doing it twice, with a bogus
;					video mode the second time.
;
;	02/09/89  2.09  Tom Hensel	Added queue for message pointers
;					Needed since VGA does a set mode
;					near the end, so we need to queue
;					up the messages and display them
;					on the cleared screen (and avoid
;					having the messages wiped out)
;
;	06/20/89  2.10  Tom Hensel	Efficiency
;		  Beta 1
;
;	06/28/89  2.10  Tom Hensel	Bug fix for open file handles
;		  Beta 2
;
;	07/19/89  2.10  Tom Hensel	Efficiency
;		  Beta 3
;
;	08/08/89  2.10  Tom Hensel	Fix stack bugs in ContextInfo and
;		  Beta 4		GetContextSize caused by efficiency
;
;	08/08/89  2.10  Tom Hensel	Fix problem with FontBank for VGA
;		  Beta 5
;
;	08/14/89  2.10  Tom Hensel	Efficiency
;		  Beta 6
;
;	08/15/89  2.10  Tom Hensel	Handle IBM kludge correctly
;		  Beta 7
;
;	08/16/89  2.10  Tom Hensel	Beef up IOdelay usage (machines are
;		  Beta 8		getting faster these days)
;
;	09/21/89  2.10  Tom Hensel	Handle interrupt flag state better
;		  Beta 9
;
;	11/07/89  2.10  Tom Hensel	Initialize biosinfo3 earlier since
;		  Beta 10		ChangeRegs needs it
;
;	11/16/89  2.10  Tom Hensel	Fix bug in WriteChar and bug in
;		  Beta 11		Int10Routine reported by amitc
;
;	11/30/89  2.10  Tom Hensel	Still some problems with noclear bit
;		  Beta 12
;
;	12/01/89  2.10  Tom Hensel	Added ReadVGARegs routine
;		  Beta 13
;
;	12/04/89  2.10  Tom Hensel	Fix label problem from Beta 13
;		  Beta 14
;
;	12/05/89  2.10  Tom Hensel	Better method of passing AL
;		  Beta 15		to ChangeRegs.  Fix biosmode problem
;					Boundary check in GetLatchLocation
;
;	12/20/89  2.10	Tom Hensel	Fix bad initialization of Attr regs
;		  Beta 16
;
;	1/3/90	  2.10	Tom Hensel	Another try to fix Beta 16 problem
;		  Beta 17
;
;	1/29/90   2.10  Ken Robertson	Enable interrupts whenever possible
;		  Beta 18		during ReadVGARegs.
;
;	4/5/90    2.11  Ken Robertson	Disable interrupts during register
;		  Beta 1		write routines.
;
;	6/4/90    2.11  Tom Hensel	Add INT 2FH instance code for Win 3.0
;		  Beta 2
;
;	6/13/90   2.11  Tom Hensel	Efficiency
;		  Beta 3
;
;	7/16/90   2.11  Tom Hensel	Fix INT 2FH instance code for Win 3.0
;		  Beta 4
;
;	7/27/90   2.11  Tom Hensel	Move Int10Routine into PSP for .COM
;		  Beta 5		for space efficiency
;
;	7/31/90   2.11  Tom Hensel	Efficiency
;		  Beta 6
;
;	8/07/90   2.11  Tom Hensel	Efficiency
;		  Beta 7
;
;	8/14/90   2.11  Tom Hensel	Created CallTableNeeded flag and
;		  Beta 8		turn it off for OS/2
;
;	9/06/90   2.11  Tom Hensel	Efficiency.  Better IODelay method
;		  Beta 9
;

;*****************************************************************************
;
; NOTE: When updating version numbers: comment out the BETA line if the build
;	is for a non-beta version
;
;*****************************************************************************

CODE	segment word public 'CODE'

	assume	cs:CODE, ds:nothing, es:nothing, ss:nothing

;
; E Q U A T E S
;

;/*---------------------------------------------------------------------*/
;/* Mouse Driver and Mouse Utility version number file, VERSNUMS.INC	*/
;/* 									*/
;/* This file can be included both in C and assembly language source	*/
;/* files.  It defines in decimal the values for the major, minor,	*/
;/* and beta version numbers.  To create a release version, comment	*/
;/* out the line with BetaVers using ;/* for the comment delimeter.	*/
;/*									*/
;/* NOTE: The Major and Beta numbers are in decimal, the tens digit of	*/
;/* the Minor number is in the high byte, ones digit in the low byte.	*/
;/*									*/
;/*	Thus version 7.43.65 is written as:				*/
;/*									*/
;/*		MajVersion  = 7;					*/
;/*		MinVersion  = 43;					*/
;/*		BetaVersion = 65;					*/
;/*									*/
;/*---------------------------------------------------------------------*/

MajVersion	=	2;
MinVersion	=	11;
BetaVersion	=	9;

MajVerCharTens	equ	<(MajVersion / 10) + "0">
MajVerCharOnes	equ	<(MajVersion MOD 10) + "0">
MinVerCharTens	equ	<(MinVersion / 10) + "0">
MinVerCharOnes	equ	<(MinVersion MOD 10) + "0">

ifdef	BetaVersion
BetaVerCharTens	equ	<(BetaVersion / 10) + "0">
BetaVerCharOnes	equ	<(BetaVersion MOD 10) + "0">
endif	; BetaVersion

FALSE		EQU	0
TRUE		EQU	(NOT FALSE)

IF1	; Pass 1 of the assembler

IFDEF	OS2
	%out	Assembling as driver for OS/2

IFNDEF	SYS
SYS		equ	OS2		; OS2 implies SYS driver model
ENDIF	; NOT SYS

ELSE	; NOT OS2

IFDEF	SYS
	%out	Assembling as driver for DOS
ELSE	; NOT SYS
	%out	Assembling as COM file for DOS
ENDIF	; NOT SYS

ENDIF	; NOT OS2

	%out		Pass 1
ELSE	; Pass 2 of the assembler
	%out		Pass 2
ENDIF	; Pass 2 of the assembler

PrintQueueSize	equ	7		; Never need more than 7 messages
					; queued up at any given time
					; (Actually, could probably get by
					; on 5, but burn a couple because
					; of the way the QUEUEPRINT macro
					; works.  Also safer, and only used
					; at init time).
CR		equ	13		; ASCII code for carriage return
LF		equ	10		; ASCII code for line feed

	subttl	Equate definitions for EGA/VGA i/o ports
	page

EGA_BASE	equ	300h

SEQ_ADDR	equ	0C4h		; Sequencer addr register
SEQ_DATA	equ	0C5h		; Sequencer data register

SEQ_MAP_MASK	equ	02H		; Write plane enable mask
MM_ALL		equ	00001111b	; Enable all planes

GRAF_CONT	equ	0CEh		; Port for graphics controller
GRAF_ADDR	equ	0CEh		; Controller addr register

GRAF_DATA_ROT	equ	03H		; Graphics rotate register
DR_SET		equ	0h		; Unmodified passthrough

GRAF_MODE_REG	equ	05h
MR_SET		equ	01h		; Writethrough mode

GRAF_MISC_REG	EQU	6h		; Graphics misc register
NumGraphicsRegs	EQU	9h

GRAF_BIT_MASK	equ	08h		; Bit mask register

NumSeqRegs	EQU	5h		; Note BIOS only has data for 4
SeqClMReg	EQU	1h

CRTCAddr	EQU	3D4h		; Will change to 003B4 if mono
NumCRTCRegs	EQU	19h

AttCtrlAddrReg	EQU	3C0h
NumAttrRegs	EQU	15h		; Note extra one for VGA
PaletteAddressSource EQU	00100000b

MiscAddr	EQU	3C2h
MiscOutputRegR	EQU	3CCH
FeatureCtrlReg	EQU	3CAH
FeatAddr	EQU	3DAh		; Will change to 003BA if mono
Gr1PosAddr	EQU	3CCh
Gr2PosAddr	EQU	3CAh

NumPtrData	EQU	4		; Number of chips using indices

ReadRegNum	EQU	0F0H
WriteRegNum	EQU	0F1H
InquireDriverNum EQU	0FAH

EGAShadowStart	EQU	0F0H
EGAShadowEnd	EQU	0FAH

ifdef	OS2
CallTableNeeded		EQU	false	; TRUE if int 10h call table needed
					; (Because > 1 routine supported)
else	; NOT OS2
CallTableNeeded		EQU	true	; TRUE if int 10h call table needed
					; (Because > 1 routine supported)
endif	; NOT OS2

;
; Non visible EGA/VGA memory layout
;
; Basic Interpreter gets first two bytes for current use (reserved space)
; Basic Interpreter gets next two bytes for future use (reserved space)
; EGA.SYS latch save area byte (reserved space)
; Mouse latch save area byte (reserved space)
; Remaining reserved space
; Cursor save area
; Buffer for screen under the cursor
;

NonVisibleStartOff_D EQU (320 / 8) * 200
NonVisibleStartOff_E EQU (640 / 8) * 200
NonVisibleStartOff_F_10 EQU (640 / 8) * 350
NonVisibleStartOff_11 EQU (640 / 8) * 480
NonVisibleStartOff_12 EQU (640 / 8) * 480
NonVisibleStartOff_13 EQU (320 / 8) * 200 * 8	; * 8 because of 256 colors

EGALatchOff_D	EQU	NonVisibleStartOff_D + 4
EGALatchOff_E	EQU	NonVisibleStartOff_E + 4
EGALatchOff_F_10 EQU	NonVisibleStartOff_F_10 + 4
EGALatchOff_11	EQU	NonVisibleStartOff_11 + 4
EGALatchOff_12	EQU	NonVisibleStartOff_12 + 4
EGALatchOff_13	EQU	NonVisibleStartOff_13 + 4

;
; The following equates are device driver definitions
;

IFDEF	SYS
B$DONE		EQU	0000000100000000B
;B$BUSY		EQU	0000001000000000B
B$ERROR		EQU	1000000000000000B
B$UnknownUnit	EQU	1
B$DNR		EQU	2
B$UnknownCommand EQU	3
B$GeneralFailure EQU	0CH
MAX_FUNCTION	EQU	16		; Maximum function number
					; (Output Until Busy function)
ENDIF	; SYS

IFNDEF	OS2

ifndef	sys
GetCodeLength	EQU	7		; Return length of resident code
GetINT2F	EQU	8		; Return original vector function code
GetINT10	EQU	9		; Return original vector function code
Int2FPatternCom	EQU	'AH'		; Choose my sister's initials
					; to identify us
OpIntRoutineLength	=	0EAh	; Size of Int10Routine

if	CallTableNeeded
OpIntRoutineLength	=	OpIntRoutineLength + 37h
endif	; CallTableNeeded

OrgStartOfData	equ	5CH + OpIntRoutineLength ; Location to start data at
					; This is because of relocation into
					; PSP of Int10Routine
endif	; NOT sys

GetInstalledState EQU	0		; Return installed state function code
GetVersionNumber EQU	6		; Return version number
MyInt2FNumber	EQU	0BCH		; An arbitrary choice
MinValidNumber	EQU	80H		; DOS gets 00 - 7F (AH)
MaxValidFunction EQU	0F7H		; DOS gets F8 - FF (AL)
Int2FPatternSys	EQU	'TV'		; Choose Tina Vandersteel's initials to
					; identify us (Who's Tina Vandersteel?
					; Tina is Ann's sister.
					; Who's Ann Vandersteel?
					; Think beautiful blonde California
					; type and you'll be on the right
					; track, but that's another story...)
ENDIF	; NOT OS2

;
; M A C R O S
;

	subttl	Macro definitions
	page

IOdelay		macro

	call	HandleIODelay

		endm

DoVersion	macro

if	MajVerCharTens - "0"
		db	MajVerCharTens
endif	; MajVerCharTens - "0"

		db	MajVerCharOnes
		db	'.'
		db	MinVerCharTens, MinVerCharOnes

ifdef	BetaVersion
		db	'.'
		db	BetaVerCharTens, BetaVerCharOnes
endif	; BetaVersion

		endm

;
; It has been found that some systems cannot handle the rapid back-to-back io
; writes that occur when an out dx,ax is executed to an 8-bit device (such as
; the EGA).  Furthermore, Video-7 reports problems running on Olivetti/AT&T
; machines with this opcode due to a "byte reversal problem".  Therefore, it
; has been implemented has a macro expansion.
;

OutWord 	macro	NoInts,DestroyAX,DestroyDX

ifb	<NoInts>
	pushf				; Save interrupt flag state
	cli
endif	; ifb <NoInts>

	out	dx,al
	IOdelay
	inc	dx
	xchg	al,ah
	out	dx,al

ifb	<NoInts>
	popf				; Restore interrupt flag state
endif	; ifb <NoInts>

ifb	<DestroyDX>
	dec	dx
endif	; ifb <DestroyDX>

ifb	<DestroyAX>
	xchg	al,ah
endif	; ifb <DestroyAX>

		endm

;
; Since IBM has documented that the VGA does NOT support 16-bit I/O to
; the Attribute Controller, we abide by this restriction (even though it
; would appear to work on certain models.)  Also, if ax is passed for the
; scratch argument, it is assumed ax need not be preserved. -JTP
;

OutWordAttr	macro	scratch,NoInts

ifb	<NoInts>
	pushf				; Save interrupt flag state
	cli
endif	; ifb <NoInts>

	InitFlipFlop
	mov	dl,AttCtrlAddrReg AND 0FFh ; Restore AttrAddr
	out	dx,al
	IOdelay

ifidn	<scratch>,<ax>
	mov	al,ah
else	; NOT idn <scratch>,<ax>
	xchg	al,ah
endif	; NOT idn <scratch>,<ax>

	out	dx,al

ifb	<NoInts>
	popf				; Restore interrupt flag state
endif	; ifb <NoInts>

ifdif	<scratch>,<ax>
	xchg	al,ah
endif	; dif <scratch>,<ax>

		endm

comment	\	Not currently used
InByteAttr	macro	NoInts

ifb	<NoInts>
	pushf				; Save interrupt flag state
	cli
endif	; ifb <NoInts>

	InitFlipFlop
	IOdelay
	mov	dl,AttCtrlAddrReg AND 0FFh ; Restore AttrAddr
	out	dx,al			; Tell 3C0 which index
	IOdelay
	inc	dx
	in	al,dx			; 3C1 returns reg data
	IOdelay
	dec	dx
	out	dx,al			; Restore flip-flop state

ifb	<NoInts>
	popf				; Restore interrupt flag state
endif	; ifb <NoInts>

		endm
\

;
; Since initialization of the Attribute Controller flip-flop is so common,
; it appears here as a macro.  It assumes dh = 03, ds = cs, and that al can
; be trashed.
;

InitFlipFlop	macro	NoSaveAX

ifndef	NoSaveAX
	PUSH	AX			; Save environment
endif	; NoSaveAX

	mov	dl,byte ptr [PortTable][5 * SIZE PortRec].prPortAddr
	in	al,dx

ifndef	NoSaveAX
	POP	AX			; Restore environment
endif	; NoSaveAX

		endm

;
; This is also a common operation that appears here as a macro, with the
; same assumptions as above.  If dx is passed for the scratch register, it
; is assumed dx need not be preserved; otherwise the scratch register will be
; used to preserve dx. -JTP
;

WaitRetrace	macro	scratch
	local	t1

ifnb	<scratch>

ifdif	<scratch>,<dx>
	mov	scratch,dx
endif	; dif <scratch>,<dx>

endif	; nb <scratch>

	mov	dl,byte ptr [PortTable][5 * SIZE PortRec].prPortAddr

t1:
	in	al,dx			; Read status reg
	test	al,08h			; Vert retrace bit on?
	jz	t1			; Not yet
	cli				; Disable interrupts asap

ifnb	<scratch>

ifdif	<scratch>,<dx>
	mov	dx,scratch
endif	; dif <scratch>,<ax>

endif	; nb <scratch>

		endm

EGAChooseMonoColorDisplay	macro

	mov	bx,offset PortTable
	mov	byte ptr code:[bx][0].prPortAddr,al
	add	al,6
	mov	byte ptr code:[bx][5 * SIZE PortRec].prPortAddr,al

				endm

IFDEF	OS2

comment	\ Not currently used
PRINT 	macro	message			; OS/2 Device drivers need to
	local	t1, t2			; refrain from using Int 21h

ifnb	<message>
	mov	si,offset message	; DS:SI -> Message to display
endif	; ifnb <message>

	jmp	short t2

 t1:
	mov	ah,14
	int	10h

 t2:
	lodsb
	or	al,al
	jnz	t1

		endm
\

ELSE	; NOT OS2

QueuePrint	macro	message

	mov	bx,[pPrintQueue]	; Get current pointer into PrintQueue

ifb	<message>
	mov	[PrintQueue][bx],dx	; Save message pointer
else	; NOT ifb <message>
	mov	[PrintQueue][bx],offset message ; Save message pointer
endif	; NOT ifb <message>

	add	[pPrintQueue],2		; Move to next spot in PrintQueue

		endm

PRINT		MACRO	message

	mov	ah,9			; Command code to print string

ifnb	<message>
	mov	dx,offset message	; DS:DX -> Message to display
endif	; ifnb <message>

	int	21h			; Let DOS print the message for us

		ENDM

UpperCase	MACRO	char
	local	UpperCaseDone

	cmp	char,'a'		; Is it in range?
	jb	UpperCaseDone		; No - skip
	cmp	char,'z'		; Is it in range?
	ja	UpperCaseDone		; No - skip
	and	char,NOT 20h		; Convert to upper case

UpperCaseDone:

		ENDM

VGAChooseMonoColorDisplay	macro
	local	DisplayCheckDone

	mov	bx,offset PortTable
	MOV	BYTE PTR code:[bx][0].PRPortAddr,0D4H
	MOV	BYTE PTR code:[bx][5 * SIZE PortRec].PRPortAddr,0DAH
	MOV	AH,12H			; Command code to get EGA information
	MOV	BL,10H			; Command code to get EGA information
	INT	10H			; Find out if currently mono or color
	OR	BH,BH			; Is it color
	JZ	DisplayCheckDone	; Yes - skip
	mov	bx,offset PortTable
	MOV	BYTE PTR code:[bx][0].PRPortAddr,0B4H
	MOV	BYTE PTR code:[bx][5 * SIZE PortRec].PRPortAddr,0BAH

DisplayCheckDone:

				endm

ENDIF	; NOT OS2

;
; S T R U C T U R E S
;

	subttl	Structure definitions
	page

;
; UserRegs is used by various context functions to access the original
; regs placed on the stack by the Int10Routine.  If you change the stack
; format, BE SURE TO UPDATE this structure!
;

UserRegs	struc

userBP		dw	?		; Offset of user's bp
userRTN		dw	?		; Return address for near CALL
userDS		dw	?		; Offset of user's ds
userSI		dw	?		; Offset of user's si
userAX		dw	?		; Offset of user's ax

UserRegs	ends

FontInfo	struc			; See GetFontInfo for details

fibank0		db	?
fibank1		db	?
fibank2		db	?
fibank3		db	?

FontInfo	ends

SetRec		struc			; Used in read/write set calls

srPortNum	dw	?		; Register id
srPtr		db	?		; Register index
srData		db	?		; Register data

SetRec		ends

PortRec 	struc

prPortAddr	dw	?		; IO port address (can change)
prCurrTable	dw	?		; Current shadow map table
prDefTable	dw	?		; Default shadow map table
prNumRegs	db	?		; Number of data regs
prModFlag	db	?		; Modified since last "rev def"

PortRec 	ends

IFNDEF	OS2

Win386_Startup_Info_Struc	STRUC

SIS_Version			db	3, 0
SIS_Next_Dev_Ptr		dd	?
SIS_Virt_Dev_File_Ptr		dd	0
SIS_Reference_Data		dd	?
SIS_Instance_Data_Off_Ptr	dw	?
SIS_Instance_Data_Seg_Ptr	dw	?

Win386_Startup_Info_Struc	ENDS

Instance_Item_Struc	STRUC

IIS_Off_Ptr	dw	?
IIS_Seg_Ptr	dw	?
IIS_Size	dw	?

Instance_Item_Struc	ENDS

ENDIF	; NOT OS2

;
; The following structures are device driver definitions
;

IFDEF	SYS

REQUEST_HEADER	STRUC

RHLength	DB	?
UnitCode	DB	?
CommandCode	DB	?
Status		DW	?
Reserved	DB	8 DUP (?)

REQUEST_HEADER	ENDS

INIT_STRUCT	STRUC

REQ_HEADER	DB	SIZE REQUEST_HEADER DUP (?)
NumberOfUnits	DB	?
pEndResidentProgramCode DD ?
pBPB		DD	?
DriveNumber	DB	?

INIT_STRUCT	ENDS

ENDIF	; SYS

;
; S E G M E N T S
;

;*****************************************************************************
; Here we set up a segment in vram.  We mark a byte for storage of the
; hardware latches during the SaveState and RestoreState routines.
;
; Non-visible EGA/VGA memory usage:
;
; EGA.SYS latch save area byte is first byte after visible memory
; Mouse latch save area byte is next byte
; Cursor save area comes next
; Screen buffer for area under the cursor comes next
;

egamem	segment at 0a000h

egamem	ends

	subttl	Data segment definitions for BIOS data
	page

INTVEC		segment at 0000h

	org	10h * 4

IV_VIDEO	LABEL	DWORD

IV_VIDEO_OFF	dw	?		; Video vector
IV_VIDEO_SEG	dw	?		; Video vector

.ERRNZ		($ - IV_VIDEO) - 4	; Keep the vector together

ifndef	OS2

	ORG	4 * 2FH

IV_MULTIPLEX	LABEL	DWORD		; Multiplex interrupt

IV_MULTIPLEX_OFF DW	?
IV_MULTIPLEX_SEG DW	?

.ERRNZ		($ - IV_MULTIPLEX) - 4	; Keep vector together

endif	; NOT OS2

	org	400h + 010h

EquipFlag	dw	?

	org	400h + 049h

CrtMode 	db	?
CrtCols 	dw	?
CrtLen		dw	?
CrtStart	dw	?
CursorPosn	dw	8 dup (?)
CursorMode	dw	?
ActivePage	db	?
Addr6845	dw	?
CrtModeSet	db	?
CrtPalette	db	?

	org	400h + 084h

Rows		db	?
Points		dw	?
Info		db	?
Info3		db	?

	org	400h + 0A8h

lpSavePtr	label	dword

oSavePtr	dw	?
sSavePtr	dw	?

.ERRNZ		($ - lpSavePtr) - 4	; Keep the vector together

INTVEC		ends

;
; E N T R Y   P O I N T
;

IFDEF	SYS

	org	0000h			; Header for device driver model

main:
pNextDevice	dd	-1
Attribute	dw	8000h
pDevStrategy	dw	DevStrategy
pDevInterrupt	dw	DevInterrupt
DevName 	db	"EGA$    "

.ERRNZ		($ - main) - 18		; Keep device driver block together

pRequestHeader	LABEL	DWORD

ReqHeaderOff	DW	?
ReqHeaderSeg	DW	?

.ERRNZ		($ - pRequestHeader) - 4	; Keep vector together

;
; DevStrategy - Device driver strategy entry point
;
; ENTRY
;	es:bx	-> request packet
;
; EXIT
;	packet updated as appropriate
;
; DESTROYS
;	none
;

	assume	cs:CODE, ds:nothing, es:nothing, ss:nothing

DevStrategy	proc	far

	mov	[ReqHeaderOff],BX	; Save offset to request header
	mov	[ReqHeaderSeg],ES	; Save segment to request header
	ret				; Return to DOS

DevStrategy	endp

;
; DevInterrupt - Device driver interrupt entry point
;
; ENTRY
;
; EXIT
;	packet updated as appropriate
;
; DESTROYS
;	none
;

	assume	cs:CODE, ds:nothing, es:nothing, ss:nothing

DevInterrupt	proc	far

	pushf				; Save environment

ifdef	OS2
	pusha				; Save environment
else	; NOT OS2
	push	ax			; Save environment
	push	bx			; Save environment
endif	; NOT OS2

	push	ds			; Save environment
	lds	bx,[pRequestHeader]	; DS:BX -> Request header
	assume	ds:nothing		; But actually request header segment
	mov	al,ds:[bx].CommandCode	; AL = command
	cmp	al,MAX_FUNCTION		; Invalid command?
	ja	DevIntDone2		; Yes - skip
	cmp	al,8			; Can we handle the command?
	ja	DevIntDone1		; No - skip

ifndef	OS2
	push	cx			; Save environment
	push	dx			; Save environment
	push	bp			; Save environment
	push	di			; Save environment
	push	si			; Save environment
endif	; NOT OS2

	push	es			; Save environment
	cbw				; AX = command
	mov	si,ax			; SI = command
	shl	si,1			; SI = command table offset
	mov	ax,cs			; DS = Code segment
	mov	ds,ax			; Two instructions needed
	assume	ds:CODE
	call	[CmdTable][si]		; Call handler for command
	assume	ds:nothing, es:nothing
	or	ax,B$DONE 		; Set done bit
	lds	bx,[pRequestHeader]	; DS:BX -> Request header
	assume	ds:nothing		; But actually request header segment
	mov	ds:[bx].Status,AX	; Store status word in packet
	pop	es			; Restore environment
	assume	es:nothing

ifndef	OS2
	pop	si			; Restore environment
	pop	di			; Restore environment
	pop	bp			; Restore environment
	pop	dx			; Restore environment
	pop	cx			; Restore environment
endif	; NOT OS2

	jmp	short DevIntDone3	; Skip the error status

DevIntDone1:
	assume	ds:nothing		; But actually request header segment
	mov	ds:[bx].Status,B$DONE	; Return done with no error status
	jmp	short DevIntDone3	; Skip the error status

DevIntDone2:
	assume	ds:nothing		; But actually request header segment
	mov	ds:[bx].Status,B$DONE OR B$ERROR OR B$UnknownCommand
					; Return unknown command error

DevIntDone3:
	assume	ds:nothing
	pop	ds			; Restore environment
	assume	ds:nothing

ifdef	OS2
	popa				; Restore environment
else	; NOT OS2
	pop	bx			; Restore environment
	pop	ax			; Restore environment
endif	; NOT OS2

	popf				; Restore environment
	ret				; Return to DOS

DevInterrupt	endp

ForcedResidentEnd	label	near

ELSE	; NOT SYS

	ORG	2CH

pEnvSeg		DW	?		; Segment address of environment

	ORG	5CH

FCB1		DB	16 DUP (?)	; File Control Block 1
					; This marks the first spot in the PSP
					; where we are allowed to destroy
					; We relocate Int10Routine into the
					; PSP starting at this point

	ORG	80H

ParmLength	DB	?		; Length of command line

	ORG	81H

pComLine	DB	127 DUP (?)	; Actual command line

	org	100h			; Just call Init if TSR model

main:
	jmp	Init			; Init will vary depending on model

	org	OrgStartOfData		; Because Int10Routine is getting
					; Relocated in the PSP occupying
					; from 5ch to here

if2	; Pass 2 of the assembler
.errnz	(offset StartOfData - offset FCB1) - OpIntRoutineLength ; Exact fit
.errnz	OpIntRoutineLength LT 167	; Make full use of PSP
endif	; Pass 2 of the assembler

ENDIF	; NOT SYS

;
; V A R I A B L E S
;

	subttl	Data definitions for EGA.SYS (shadow maps)
	page

IFNDEF	OS2

StartOfData	equ	$

ENDIF	; NOT OS2

StartEGAContext label	near

PortTable	label	word		; Code dependent on order

PortRec 	<CRTCAddr, CRTCRegs, DefCRTCRegs, NumCRTCRegs, 0>
PortRec 	<(EGA_BASE + SEQ_ADDR), SeqRegs, DefSeqRegs, NumSeqRegs, 0>
PortRec 	<(EGA_BASE + GRAF_CONT), GraphicsRegs, DefGraphicsRegs, NumGraphicsRegs, 0>
PortRec 	<AttCtrlAddrReg, AttrRegs, DefAttrRegs, NumAttrRegs, 0>
PortRec 	<MiscAddr, MiscOutReg, DefMiscOutReg, 1, 0>
PortRec 	<FeatAddr, FeatureReg, DefFeatureReg, 1, 0>
PortRec 	<Gr1PosAddr, Gr1PosReg, DefGr1PosReg, 1, 0>
PortRec 	<Gr2PosAddr, Gr2PosReg, DefGr2PosReg, 1, 0>

.ERRNZ		($ - PortTable) - (8 * SIZE PortRec) ; Keep table together

SingleRegMod	db	0		; >0 if a single reg modified

;
; This is the current shadow map table...order MUST be same as BIOS
;

StartShadowMaps label	byte

SeqRegs 	db	NumSeqRegs	dup (?)
MiscOutReg 	db	1		dup (?)
CRTCRegs	db	NumCRTCRegs	dup (?)
AttrRegs	db	NumAttrRegs	dup (?)

VGAPatch	EQU	$ - StartShadowMaps

GraphicsRegs	db	NumGraphicsRegs	dup (?)

SizeShadowMaps	EQU	$ - StartShadowMaps

;
; This is the default shadow map table...order MUST be same as BIOS
;

DefSeqRegs	db	NumSeqRegs	dup (?)
DefMiscOutReg	db	1		dup (?)
DefCRTCRegs	db	NumCRTCRegs	dup (?)
DefAttrRegs	db	NumAttrRegs	dup (?)
DefGraphicsRegs	db	NumGraphicsRegs	dup (?)

Gr1PosReg	db	0		; These aren't included in bios
Gr2PosReg	db	1
FeatureReg	db	0		; Never changed
DefGr1PosReg	db	0
DefGr2PosReg	db	1
DefFeatureReg	db	0		; Never changed

.ERRNZ		($ - StartShadowMaps) - ((NumSeqRegs * 2) + (NumCRTCRegs * 2) + (NumAttrRegs * 2) + (NumGraphicsRegs * 2) + 8)

FontBank	FontInfo	<>	; Font ids for 4 banks

SizeEGAContext	EQU	($ - StartEGAContext)

fVga		db	FALSE		; VGA presence flag
SaveAX		dw	?		; Temp variables
biosmode	db	?		; Current video mode #
biosinfo	db	?		; Static copy of ega info byte
biosinfo3	db	?		; Static copy of ega info3 byte

pOldInt10	label	dword

pOldInt10Off	dw	?		; Offset of old INT 10H vector
pOldInt10Seg	dw	?		; Segment of old INT 10H vector

.ERRNZ		($ - pOldInt10) - 4	; Keep vector together

fPalette	db	false		; This is used to flag when the user
					; has requested to read the palette
					; regs.  This is needed for VGA
					; to overcome a problem of rapid
					; video enable/disable needed to
					; read the palette registers

DriverInfo	label	byte		; A historic data area

Version 	db	MajVersion, MinVersion
CopyrightMsg	dw	offset Copyright
CheckSum	dw	0		; Why this ?
CheckSumStart	dw	offset SetMode	; Why this ?
fInBIOS 	db	0
Copyright	db	"*** This is Copyright (c) 1984-1990 Microsoft ***"

.errnz		($ - DriverInfo) - (9 + 49)

IFDEF	OS2
ContextCopy	db	SizeEGAContext dup (?) ; Enough storage for 1 context
ELSE	; NOT OS2

pOldINT2F	LABEL	DWORD

pOldINT2FOff	DW	?
pOldINT2FSeg	DW	?

.ERRNZ		($ - pOldINT2F) - 4	; Keep vector together

Int2FNumber	db	MyInt2FNumber	; User can change this value using
					; FUNC=XX command line parameter
					; (where XX is 80 through FF
					; hexadecimal inclusive)
					;
					; For example:  EGA FUNC=AC

Win386_Startup_Info	Win386_Startup_Info_Struc	<, ?, 0, ?, offset Instance_Item, ?>

Instance_Item		label	byte

RegData		Instance_Item_Struc	<offset StartOfData, ?, SizeOfData>

		dd	0		; Termination code

.errnz		($ - Instance_Item) - (4 + SIZE Instance_Item_Struc)

SizeOfData	equ	($ - StartOfData)

ENDIF	; NOT OS2

;
; Start of non-instance data (data which is not modified)
;

	subttl	Data definitions for EGA.SYS (dispatch tables)
	page

;
; The following table is used to look up the location to save the EGA/VGA
;	latch state depending on the video mode.  The mouse driver does
;	the same thing and uses the byte after the byte we use.
;

EGALatchTable	dw	EGALatchOff_D
		dw	EGALatchOff_E
		dw	EGALatchOff_F_10
		dw	EGALatchOff_F_10
		dw	EGALatchOff_11
		dw	EGALatchOff_12
		dw	EGALatchOff_13

.errnz		($ - EGALatchTable) - 14 ; Keep table together

ContextTable	label	word		; Jump table for ContextInfo

		dw	GetContextSize	; Func 000h
		dw	SaveContext	; Func 001h
		dw	RestoreContext	; Func 002h
		dw	GetFontInfo	; Func 003h
		dw	GetInBiosFlag	; Func 004h

MaxContextCall	EQU	($ - ContextTable) / 2 - 1

.ERRNZ		($ - ContextTable) - (5 * 2) ; Keep table together

MyCallTable	label	word

		dw	offset ReadReg	; Call 0F0h
		dw	offset WriteReg	; Call 0F1h
		dw	offset ReadRange ; Call 0F2h
		dw	offset WriteRange ; Call 0F3h
		dw	offset ReadSet	; Call 0F4h
		dw	offset WriteSet	; Call 0F5h
		dw	offset RevertDefault ; Call 0F6h
		dw	offset DefineDefault ; Call 0F7h
		dw	offset GetDefault ; Call 0F8h
		dw	offset ContextInfo ; Call 0F9h
		dw	offset InquireDriver ; Call 0FAh

MaxMyCall	EQU	($ - MyCallTable) / 2 - 1

.ERRNZ		($ - MyCallTable) - (11 * 2) ; Keep table together

;
; Note that the following table of shadow routines is now complete.  Some
; entries still point to "Ignore" only because the BIOS either never
; changes the EGA regs for these routines, or it calls other int 10h
; functions which are already shadowed to do its work.	An exception to
; this are the entries marked (nop)*.  These entries change the readable
; regs in the CRTC chip, thus shadowing of these functions is simply a
; matter of reading those registers.  That functionality has been delegated
; to the routine UpdateCRTCMaps, which gets called by all of the READ
; procedures before returning any data.
;
; WARNING: If a new routine is added to this list, or if the shadowing
; logic in any of these is modified, you may need to update biostable.	It
; contains the lowest video mode # for which shadowing is performed for each
; BIOS function.
;

if	CallTableNeeded

BIOSCallTable	label	word

		dw	offset SetMode	; Set display mode
		dw	offset SetCursorType ; Set cursor shape
		dw	offset Ignore	; Set cursor position	(nop)*
		dw	offset Ignore	; Get cursor position	(nop)
		dw	offset Ignore	; Get light-pen pos	(nop)
		dw	offset Ignore	; Set display page	(nop)*
		dw	offset ScrollUpDown ; Scroll up
		dw	offset ScrollUpDown ; Scroll down
		dw	offset ReadChar	; Read  attr/char
		dw	offset WriteChar ; Write attr/char
		dw	offset WriteChar ; Write char only
		dw	offset SetCgaPalette ; Set CGA palette regs
		dw	offset WriteDot ; Write dot
		dw	offset ReadDot	; Read  dot
		dw	offset Ignore	; Write tty		(nop)
		dw	offset Ignore	; Get video state	(nop)
		dw	offset SetEgaPalette ; Set EGA palette regs
		dw	offset DownloadFont ; Download EGA fonts
		dw	offset Ignore	; Alternate select	(nop)
		dw	offset Ignore	; Write string		(nop)

MaxBIOSCall	EQU	($ - BIOSCallTable) / 2 - 1

.ERRNZ		($ - BIOSCallTable) - (20 * 2) ; Keep table together

biostable	db	0,0,-1,-1,-1,-1,0Dh,0Dh,8,8,8,0,8,8,-1,-1,0,0,-1,-1

.errnz	($ - biostable) - (MaxBIOSCall + 1)

else	; NOT CallTableNeeded

MaxBIOSCall	EQU	1

endif	; NOT CallTableNeeded

	subttl	Device Driver Code
	page

IFDEF	SYS

;
; Request packet command dispatch table
;

CmdTable	dw	Init		; 0  Initialization
		dw	StatusComplete	; 1  Media Check
		dw	StatusComplete	; 2  Build BPB
		dw	StatusComplete	; 3  Reserved

IFDEF	OS2
		dw	DoSave		; 4  Input (Read)
ELSE	; NOT OS2
		dw	StatusComplete	; 4  Input (Read)
ENDIF	; NOT OS2

		dw	StatusComplete	; 5  Non-Destructive read
		dw	StatusComplete	; 6  Input Status
		dw	StatusComplete	; 7  Input Flush

IFDEF	OS2
		dw	DoRestore	; 8  Output (Write)
ELSE	; NOT OS2
		dw	StatusComplete	; 8  Output (Write)
ENDIF	; NOT OS2

.ERRNZ		($ - CmdTable) - (9 * 2) ; Keep table together
