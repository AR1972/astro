;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1989-1991
;
;   Module:   HIGHVAR.INC - Data common to LOADHIGH and DEVICEHIGH, res seg
;
;   Date:     May 14, 1992
;
;******************************************************************************
;
;   Modification log:
;
;     DATE    WHO      DESCRIPTION
;   --------  -------  --------------------------------------------------------
;   05/14/92  t-richj  Original
;   06/21/92  t-richj  Final revisions before check-in
;
;******************************************************************************
;
; There are two primary definitions which need to be made, selectively, before
; this include file should be used.  These are:
;    HV_Extern - If this has been defined, variables for this module will be
;                declared as external.  Otherwise, variables will be declared
;                public, as well as defined, here.  LoadHigh declares HV_Extern
;                in stub.asm and loadhi.asm, and does not declare it in
;                rdata.asm... DeviceHigh does not declare HV_Extern anywhere
;                (as only one module, sysconf.asm, includes this file).
;    HV_LoadHigh - This should be defined when this module is going into
;                  command.com, for LoadHigh.  All of loadhi.asm, stub.asm and
;                  rdata.asm define this, while io.sys' sysconf.asm does not.
;
;******************************************************************************

;
; To keep track of which UMBs were specified on the DH/LH command lines, and
; to keep track of the minimum sizes given for each, there're two arrays kept
; in { IO.SYS: sysinitseg / COMMAND.COM: DATARES }... each is MAXUMB elements
; big.  16 should be around 14 too many for most users, so there's no expected
; space problem (it's just such a nice round number, eh?).
;

MAXUMB	equ	16

;
; Memory elements owned by the system are marked as PSP address 8 in both the
; USA and Japan; Japanese systems also use 9 under more bizzarre conditions.
;

FreePSPOwner	equ	0	; Free MCBs all have an owner PSP address of 0
SystemPSPOwner	equ	8
JapanPSPOwner	equ	9

; -----------------------------------------------------------------------------
;*** ih_local - declares data+public if HV_Extern not defined, else as extrn
;             -- One module for each executable using highvar.inc should _not_
; define HV_Extern before including it; that way, variables are local to one
; module only, and external to the rest.
; -----------------------------------------------------------------------------
; These macros make stuff external or public+declared depending on whether
; or not HV_Extern has been defined for this module.  For example:
;
;    randomdata	flag, byte, db, 1
;
; will evaluate to
;
;    extrn	randomdata :byte
;
; for any modules where HV_Extern has been defined, and to
;
;    public	randomdata
;    randomdata	db 1
;
; for any modules (there should be only one) which haven't declared HV_Extern.
; -----------------------------------------------------------------------------

ifdef	HV_Extern
	ih_local  macro  data, type, real, val
		extrn	data :type
	endm
else
	ih_local  macro  data, type, real, val, val2, val3
		public	data
		data	real	val	val2	val3
	endm
endif

;
; Now, using these macros, we declare or define the following pieces of data
; for LoadHigh and DeviceHigh:
;
;	fInHigh - Is set to 1 during HideUMBs(), and back to zero in
;	          UnHideUMBs().
;	fUmbTiny - Is set to 1 iff the user has specified /S on the command-
;	           line.
;	SegLoad - Segment address for first UMB specified; set automatically.
;	UmbLoad - The load UMB number; for example, this is 3 if the user has
;	          given a command-line like "/L:3,500;4"
;	UmbUsed - An array of characters, each of which is 1 iff the UMB
;	          matching its index number was specified on the command-line;
;	          for example, after "/L:3,500;4;7", UmbUsed[3], [4] and [7]
;	          will be set to 1.  All others will be set to 0.
;	UmbSize - An array of words, each of which is interpereted as a size
;	          specified by the user for a UMB (in the above example, all
;	          elements would be zero save UmbSize[3], which would be 500.
;	fm_umb - Set to the old UMB link-state (0x80 or 0x00)
;	fm_strat - Set to the old memory-allocation strategy (0$00000???)
;	fm_argc  - Number of arguments received by ParseVar() (see ParseVar()
;	           for details).
;

	ih_local fInHigh,  byte, db, 0
	ih_local fUmbTiny, byte, db, ?
	ih_local SegLoad,  word, dw, ?
	ih_local UmbLoad,  byte, db, ?
	ih_local UmbUsed,  byte, db, MAXUMB dup (?)
	ih_local UmbSize,  word, dw, MAXUMB dup (?)

	ih_local fm_umb,   byte, db, ?
	ih_local fm_strat, byte, db, ?
	ih_local fm_argc,  byte, db, ?

;
; UmbLoad is set to UNSPECIFED, below, until /L:umb is read; at which point
; UmbLoad is set to the UMB number given.
;

UNSPECIFIED	equ	-1

; -----------------------------------------------------------------------------
;*** dataseg - moves a segment ptr (es/ds/etc) to the data segment for HighVar
;            -- LoadHigh keeps HighVar variables in the resident data segment.
; -----------------------------------------------------------------------------
; ENTRY:       Segment ("dataseg ds" etc)
; EXIT:        Given segment points to appropriate data segment for LH or DH
; ERROR EXIT:  None
; USES:        AX, given segment
; ASSUMES:     For LoadHigh, ds:ResSeg points to DATARES
; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------
;*** normseg - returns a given segment (es/ds/etc) to its default segment
; -----------------------------------------------------------------------------
; ENTRY:       Segment ("normseg es", etc)
; EXIT:        Given segment points to appropriate data segment for LH or DH
; ERROR EXIT:  None
; USES:        AX, given segment
; -----------------------------------------------------------------------------

ifdef HV_LoadHigh	; If this is defined, we're in command.com

ifdef HV_Stub		; If this is defined, we're in command.com's stub.asm
	dataseg	macro	sg
		assume	sg:DATARES
		endm
	normseg	macro	sg
		assume	sg:nothing
		endm
else  ; HV_Stub
	dataseg	macro	sg
		mov	sg, ds:ResSeg	; Assume ds:ResSeg points to DATARES
		assume	sg:DATARES
		endm
	normseg	macro	sg
		assume	sg:TRANGROUP
		endm
endif ; HV_Stub

else  ; HV_LoadHigh
	dataseg	macro	sg
		push	cs		; sysinitseg is same as code segment
		pop	sg
		assume	sg:sysinitseg;
		endm
	normseg	macro	sg
		assume	sg:nothing;
		endm
endif  ; HV_LoadHigh

; -----------------------------------------------------------------------------
;*** getdata - retrieves a piece of data from the appropriate data segment.
; -----------------------------------------------------------------------------
; ENTRY:      reg - Register in which to place variable's value
;             off - Variable-name to query              ("getdata al, fInHigh")
; EXIT:       Given register contains value of specified variable.
; ERROR EXIT: None
; USES:       Given register
; ASSUMES:    For LoadHigh, ds:ResSeg points to DATARES
; -----------------------------------------------------------------------------

getdata	macro	reg, off
	push	ds
	dataseg	ds
	mov	reg, off
	pop	ds
	normseg	ds
	endm

; -----------------------------------------------------------------------------
;*** putdata - changes a piece of data in the appropriate data segment.
; -----------------------------------------------------------------------------
; ENTRY:      Variable-name, new value, and segment ("putdata fInHigh, 1, es").
;             I cannot think of a reason why the segment would not be "es",
;             but I've left it open anyway.
; ENTRY:      off - Name of variable to change          ("putdata fInHigh, 1")
;             arg - New value for variable (constant, register, memory, etc)
; EXIT:       Given variable contains value passed by arg (immediate or reg).
; ERROR EXIT: None
; USES:       Given variable
; ASSUMES:    For LoadHigh, ds:ResSeg points to DATARES
; -----------------------------------------------------------------------------

putdata	macro	off, arg
	push	es
	dataseg	es
	mov	off, arg
	pop	es
	normseg	es
	endm

; -----------------------------------------------------------------------------
;*** pushreg - repetitively pushes a variable # of arguments onto the stack
; -----------------------------------------------------------------------------
; ENTRY:      Parameters consist of any number of arguments, pushed left-to-
;             right.
; EXIT:       None; parameters are pushed.
; ERROR EXIT: None
; USES:       Stack
; -----------------------------------------------------------------------------
; CAUTION:    You must reverse "popreg" arguments; they both work left-to-right
;       For example:
;		pushreg	<ax, bx, cx, di, es>
;		...
;		popreg	<es, di, cx, bx, ax>
; -----------------------------------------------------------------------------

pushreg	macro	reglist
	irp	reg, <reglist>
		push	reg
		endm
	endm

; -----------------------------------------------------------------------------
;*** popreg - repetitively pops a variable # of arguments from the stack
; -----------------------------------------------------------------------------
; ENTRY:      Parameters consist of any number of arguments, popped left-to-
;             right.
; EXIT:       None; parameters are popped.
; ERROR EXIT: None
; USES:       Stack
; -----------------------------------------------------------------------------
; CAUTION:    You must reverse "popreg" arguments; they both work left-to-right
;       For example:
;		pushreg	<ax, bx, cx, di, es>
;		...
;		popreg	<es, di, cx, bx, ax>
; -----------------------------------------------------------------------------

popreg	macro	reglist
	irp	reg, <reglist>
		pop	reg
		endm
	endm

