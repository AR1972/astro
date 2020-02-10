;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1989-1991
;
;   Module:   HIGHLOAD.INC - Code used to prepare for LoadHigh or DeviceHigh
;
;   Date:     May 14, 1992
;
;******************************************************************************
;
;   Modification log:
;
;     DATE    WHO      DESCRIPTION
;   --------  -------  ----------------------------------------------
;   05/14/92  t-richj  Original
;   06/21/92  t-richj  Final revisions before check-in
;
;******************************************************************************
;
; This file contains routines needed to parse and implement user-given
; command-line options of the form "/S/L:3,0x500;2;7,127;0x0BE4".  InitVar()
; and Parsevar() are used to parse this data and place it in encoded form into
; the variables in highvar.inc, for use by the rest of the routines.
;
; DeviceHigh accepts this command-line (handled in sysconf.asm, not here):
;    DEVICEHIGH SIZE=hhhhhh module opts
; Or, DeviceHigh and LoadHigh accept any of the following:
;    DH/LH module opts
;    DH/LH [/S][/L:umb[,size][;umb[,size]]*] module opts
;    DH/LH [/L:umb[,size][;umb[,size]]*][/S] module opts
; The initial UMB,SIZE pair designates the module's load address; the remainder
; of the UMB and SIZE pairs are used to indicate specific UMBs to be left
; available during the load.
;
; When an actual load is ready to be performed, a call to HideUMBs() will
; temporarily allocate (as owner 8+"HIDDEN  ") all free elements in any
; upper-memory block which was not specified by the user... in addition, if
; UMBs were marked to shrink (/S option) to a certain size ("umb,size"), any
; elements in that umb SAVE the lower-half of the newly-shrunken one are also
; allocated.  After the load, the function UnHideUMBs() (in highexit.inc) will
; free any UMBs so allocated.
;
; When a device driver loads, there is the additional problem of allocating its
; initial load site; this should be restricted to the first UMB specified on
; the command-line.  The function FreezeUM temporarily allocates all remaining
; free upper-memory elements (as owner 8+"FROZEN  "), except those in the load
; UMB.  Then the initial allocation may be made, and a call to UnFreeze will
; return any so-allocated memory elements to FREE, for the true load.  Note
; that UnFreeze leaves HIDDEN elements allocated; it only frees FROZEN ones.
;
;******************************************************************************
;
;   Public:
;
;___PROCEDURES_________________________________________________________________
;
;   AddrToUmb   - converts a segment address in AX to its appropriate UMB #
;   BigFree     - makes ES:0 point to the largest free MCB in UMB given as AL
;   FixMem      - scans the UM chain and concatenates adjacent free MCBs
;   FreezeUM    - Marks FROZEN all UM elements now FREE, save those in load UMB
;   GetLoadSize - Returns the load UMB minimum size (0 if not specified)
;   GetLoadUMB  - Returns the load UMB number in AL (-1 if not specified)
;   GetSize     - Returns the UMB in AL's minimum size (0 if not specified)
;   GetXNum     - reads a 32-bit ASCII number at ES:SI and returns it in DX:AX
;   HideUMBs    - links UMBs and hides upper-memory as appropriate
;   InitVar     - initializes all the variables used in ParseVar and HideUMBs
;   NextMCB     - moves an MCB pointer forward to the next MCB
;   ParseVar    - parses [/S][/L:umb[,size][;umb[,size]]*] and builds the table
;   PrTable     - produces a printout of the variables in highvar.inc
;   StoLoadSize - Overrides the load UMB minimum size with what's in AX
;   StoLoadUMB  - Overrides the load UMB number with what's in AL
;   UmbHead     - returns in AX the address of the first UMB block (0x9FFF)
;   UnFreeze    - Marks FROZEN elements as FREE
;
;___VARIABLES__________________________________________________________________
;
;   gnradix     - After a call to GetXNum, is 16 or 10, depending on the # read
;
;   Internal:
;___PROCEDURES_________________________________________________________________
;
;   convUMB     - checks after GetXNum to convert an address to a UMB number
;   findUMB     - makes ES:0 point to the first MCB in UMB given as AL
;   fm_link     - links UMBs not already linked in
;   fm_unlink   - unlinks UMBs if fm_umb is set to 0
;   frezMCB     - marks as 8+FROZEN the MCB at ES:0
;   hideMCB     - marks as HIDDEN the MCB at ES:0
;   hideUMB     - marks as HIDDEN all FREE elements in UMB passed as AL
;   hideUMB?    - hides as appropriate the UMB in CL
;   hl_unlink   - unlinks UMBs if fm_umb is set to 0; restores strategy too
;   incArgc     - increments fm_argc, for use with LH command-line parsing
;   isEOL       - returns with ZF set iff AL contains CR or LF, or 0
;   isFreeMCB   - returns with ZF set if current MCB (ES:0) is FREE
;   isFrozMCB   - returns with ZF set if current MCB (ES:0) is FROZEN
;   isSpecified - sets ZF if UMB in AL wasn't specified in DH/LH line.
;   isSysMCB    - sets ZF iff ES points to an MCB owned by "SC" + (8 or 9)
;   isTiny      - returns with ZF set if user didn't specify /S
;   isWhite     - returns with ZF set iff AL contains whitespace (or "=")
;   loadLow     - returns AL==0 if UMB0 == 0, else AL==1
;   mul32       - multiplies the number in DX:AX by gnradix
;   parseL      - parses ":nnnn[,nnnn][;nnnn[,nnnn]]*" for ParseVar
;   setUMBs     - links umbs and sets allocation strategy for a load
;   shrinkMCB   - breaks an MCB into two pieces, the lowest one's size==AX
;   stowSiz     - marks a given UMB as having a given minimum size
;   stowUMB     - marks a given UMB as used, if it hasn't been so marked before
;   toDigit     - converts a character-digit to its binary counterpart
;   toPara      - divides DX:AX by 16; result in AX only
;   toUpper     - accepts one argument (probly a register), and upper-cases it.
;   unHideMCB   - marks as FREE the MCB at ES:0
;   unMarkUMB   - marks a given UMB as unused, even if previously marked used
;
;******************************************************************************

SWTCH	equ	'/'		; Switch character

DOS_CHECK_STRATEGY  equ	5800h	; Int 21h, Func 58h, Svc 0 = check alloc strat
DOS_SET_STRATEGY    equ	5801h	; Int 21h, Func 58h, Svc 1 = set alloc strategy
DOS_CHECK_UMBLINK   equ	5802h	; Int 21h, Func 58h, Svc 2 = check link state
DOS_SET_UMBLINK     equ	5803h	; Int 21h, Func 58h, Svc 3 = set link state
DOS_GET_DOS_LISTS   equ	  52h	; Int 21h, Func 52h = return list of lists
DOS_UMB_HEAD        equ	  8Ch	; Offset from ES (after func52h) to get UMBHead

CR	equ	0Dh		; Carriage Return
LF	equ	0Ah		; Line Feed
TAB	equ	09h		; Tab character (^I)

; -----------------------------------------------------------------------------
; This file needs arena.inc, but it should already have been included.  Because
; of MASM's strange method of two-pass definitions, we can't just include it
; inside of an ifndef, or it won't make it to stage two.  Odd odd odd.
; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------
;*** toUpper - accepts one argument (probly a register), and upper-cases it.
;    BUGBUG: t-richj 6/3/92 - This method, though used elsewhere (for example,
;            used in sysinit2.asm to upper-case config.sys), relies on ASCII
;            conventions.  Might be bad.
; -----------------------------------------------------------------------------

toUpper	macro	arg
	and	arg, 0DFh
	endm

; -----------------------------------------------------------------------------
;*** NextMCB - moves an MCB pointer forward to the next MCB
; -----------------------------------------------------------------------------
; ENTRY:       First arg - seg pointer (ES?); second, a scratch register (AX?)
; EXIT:        Seg pointer moved forward, scratch register = distance moved
; ERROR EXIT:  None
; USES:        Flags, arguments
; -----------------------------------------------------------------------------

NextMCB	macro	seg, reg
	mov	reg, seg
	add	reg, seg:[arena_size]
	inc	reg			; Inc 1 paragraph for the header
	mov	seg, reg
	endm

; -----------------------------------------------------------------------------
;*** InitVar - initializes all the variables used in ParseVar and HideUMBs
; -----------------------------------------------------------------------------
; ENTRY:       None
; EXIT:        Variables listed in highvar.inc are initialized
; ERROR EXIT:  None
; USES:        Flags, variables in highvar.inc
; -----------------------------------------------------------------------------
; Note that element 0 references UMB 0 (conventional), not UMB 1.  Its contents
; are largely ignored, but it is initialized nonetheless.
; -----------------------------------------------------------------------------

	public	InitVar

InitVar	proc	near
	pushreg	<ax, cx, di, es>
	dataseg	es			;Point ES into appropriate data segment

	xor	ax, ax
	mov	fUmbTiny, al		;Shrink UMBs? (made 1 if /S given)
	mov	fInHigh,  al		;Set to 1 when DH/LH has been called
	mov	SegLoad,  ax		;Load Address (seg), used for DH only
	mov	UmbLoad, UNSPECIFIED	;Later is the # of the 1st spec'd UMB
	mov	fm_argc,  al		;Start with zero args having been read

	cld

	mov	cx, MAXUMB		;For each entry
	mov	di, offset ES:UmbUsed	;on the UmbUsed array,
	rep	stosb			;	Store 0

	mov	cx, MAXUMB		;Okay... for each entry
	mov	di, offset ES:UmbSize	;on the UmbSize array,
	rep	stosw			;	Store 0

	normseg	es		; Return ES

	popreg	<es, di, cx, ax>
 	ret

InitVar	endp

; -----------------------------------------------------------------------------
;*** FixMem - scans the upper memory chain and concatenates adjacent free MCBs
; -----------------------------------------------------------------------------
; ENTRY   : None
; EXIT    : None
; ERROR   : None
; USES    : Flags, fm_umb, fm_strat
; -----------------------------------------------------------------------------

	public	FixMem

FixMem	proc	near

	pushreg	<ax, bx, cx, dx, es>

	call	fm_link		; Link in UMBs

	call	UmbHead		; Get first upper-memory MCB address (0x9FFF)
	jc	fmX		; (if couldn't get it, leave now).

	mov	es, ax		; It returns in AX, so move it to ES.

;
; - Walk MCB Chain ------------------------------------------------------------
;

	xor	dx, dx		; We're keeping the address of the last MCB
	mov 	cx, dx		; in CX... and the last owner
	inc	dx		; in dx as we go through the loop:

; ------------------------------------------
; FM10--DX  = last MCB's owner's PSP address
;       CX  = last MCB's address (segment)
; ------------------------------------------

fm10:	mov	al, es:[arena_signature]	; if 'Z', don't repeat loop
	mov	bx, es:[arena_owner]		; if not zero, do nothing
	or	bx, dx				; dx was owner of previous MCB
	jnz	fm30				; If not both zero, don't cat.

	; - Coalesce memory blocks at ES:00 and CX:00 -------------------------

fm20:	mov	bx, es:[arena_size]		; Grab this block's Size,
	mov	es, cx				; Go back to prev MCB's address
	mov	es:[arena_signature], al	; & move the SECOND sig here

	add	bx, es:[arena_size]		; Size += first MCB's size
	add	bx, 1h				; And add one for the header
	mov	es:[arena_size], bx		; Write the size

	; ---------------------------------------------------------------------

fm30:	mov	cx, es			; Put this address on the stack
	mov	dx, es:[arena_owner]	; And remember its owner

	NextMCB	es, bx			; Move to the next MCB

	cmp	al, arena_signature_end
	jnz	fm10			; If signature != 'Z', there are more.

fmX:	call	fm_unlink		; Unlink UMBs

	popreg	<es, dx, cx, bx, ax>
	ret

FixMem	endp

; -----------------------------------------------------------------------------
;*** fm_link - links UMBs not already linked in
; -----------------------------------------------------------------------------
; ENTRY:    None
; EXIT:     fm_umb == 0 if not linked in previously, 1 if already linked in
; ERROR:    None
; USES:     AX, BX, fm_umb
; -----------------------------------------------------------------------------

fm_link	proc	near
	mov	ax, DOS_CHECK_UMBLINK
	int	21h			; Current link-state is now in al

	putdata	fm_umb, al		; So store it in fm_umb for later

	mov	ax, DOS_SET_UMBLINK
	mov	bx, 1
	int	21h

	ret
fm_link	endp

; -----------------------------------------------------------------------------
;*** fm_unlink - unlinks UMBs if fm_umb is set to 0
; -----------------------------------------------------------------------------
; ENTRY:    fm_umb == 1 : leave linked, else unlink
; EXIT:     None
; ERROR:    None
; USES:     AX, BX
; -----------------------------------------------------------------------------

fm_unlink	proc	near
	xor	bx, bx
	getdata	bl, fm_umb		; fm_umb already has the old link-state
	mov	ax, DOS_SET_UMBLINK
	int	21h			; so just use that, and call int 21h
	ret
fm_unlink	endp

; -----------------------------------------------------------------------------
;*** ParseVar - parses [/S][/L:umb[,size][;umb[,size]]*] and builds the table
; laid out in highvar.inc
; -----------------------------------------------------------------------------
; ENTRY:    ES:SI points to command tail of LoadHigh/DeviceHigh (whitespace ok)
; EXIT:     ES:SI points to first character in child program name
; ERROR:    ES:SI points to character which caused error, carry set, AX == code
; USES:     ES:SI, AX, flags, variables in highvar.inc
; -----------------------------------------------------------------------------
; Error codes (in AX if carry set on return):
;
PV_InvArg	equ	1	; Invalid argument passed
PV_BadUMB	equ	2	; Bad UMB number passed (duplicate?)
PV_InvSwt	equ	3	; Unrecognized switch passed
;
; This routine exects ES:SI to point to a string much like the following:
;    "/S/L:1,200;2 module options"
; Optionally, the string can begin with whitespace; neither /S nor /L is
; required, though that's what this routine is supposed to parse.
;
optS		equ	'S'	; /S
optL		equ	'L'	; /L:...
;
; -----------------------------------------------------------------------------
; LoadHigh has a list of arguments, returned by cparse, which is used to create
; a command-line for spawning a child process.  For a typical LH command, say,
;     lh /l:1,1000;2 print/d:lpt2
; the arguments would look like (one per line):
;     lh
;     /l
;     1
;     1000
;     2
;     print
;     /d
;     :lpt2
; In short, if "print" were, say, "43", there'd be no way to determine which
; arg was the filename.  So, inside this routine, we keep a running counter
; of the number of arguments LH will need to skip in order to get to the
; program name.  The "lh" is implicit--it'll always have to skip that.  So if
; there's no "/l" or "/s", fm_argc will be 0 ... other than that, 1 is added
; for:
;    Each /L
;    Each /S (there should be only one)
;    Each UMB number (they follow ":" or ";")
;    Each UMB size   (they follow ",")
; So, in the above example, fm_argc would be 4-- and LH would skip right to
; "print".  Note that InitVar initializes fm_argc to zero.
; -----------------------------------------------------------------------------

	public	ParseVar

ParseVar	proc	near
	pushreg	<di, ds, es>

	push	es		; Make DS:SI point to it, as well as ES:SI
	pop	ds		; (regardless if we're in devhigh or loadhigh)
	cld

; ------------------------------------------------
; PV10--ES:SI = any whitespace on the command-line
; ------------------------------------------------

pv10:	lodsb			; here, ES:SI=="  /L..."--must eat whitespace
	call	isWhite
	jz	pv10		;       ES:SI==" /L..."--keep eating.
	cmp	al, SWTCH
	jz	pv20		;       ES:SI=="/L..."--go process a switch

	dec	si		; Backup--it's now "odule options", and we need
	clc			; that "m" we just read (or whatever it is).
	jmp short pvX		; Then return with carry clear == we're done.

pv20:	lodsb			; Just read 'S' or 'L', hopefully
	toUpper	al		; So we make it upper-case, and...
	cmp	al, optS	; just read 'S'?
	jnz	pv30

	call	incArgc		; If it's /S, it's another arg for LH to skip.

	putdata	fUmbTiny, 1	; /S, so ES:SI=="  /L..." or " module opts", or
	jmp short pv10		; possibly even "/L...".

pv30:	cmp	al, optL	; If it's not 'L' either, then 'tis a bad
	jnz	pvE1		; switch!

	call	incArgc		; If it's /L, it's another arg for LH to skip.

	call	parseL
	jnc	pv10		; If no carry, go back and look for more

	dec	si		; Else, back up and exit.
	jmp short pvErr		; AX has already been set by parseL

pvE1:	mov	ax, PV_InvSwt	; Unrecognized switch passed
pvErr:	dec	si
	dec	si
	stc
pvX:	popreg	<es, ds, di>
	ret
ParseVar	endp

; -----------------------------------------------------------------------------
;*** parseL - parses ":nnnn[,nnnn][;nnnn[,nnnn]]*" for ParseVar
; -----------------------------------------------------------------------------
; ENTRY:    ES:SI points to colon
; EXIT:     ES:SI points to first character not parsed
; ERROR:    Carry set; rewind three characters and return (see ParseVar)
; USES:     ES:SI, flags, AX, CX, DX, variables in highvar.inc
; -----------------------------------------------------------------------------
; If the string here is terminated with anything other than whitespace or a
; switchchar (perhaps it's /S or another /L:... ), then we return with carry
; set, indicating that they've screwed up the syntax.  The 3-character rewind
; makes sure the app /L: is reported as being the culprit.
; -----------------------------------------------------------------------------

parseL	proc	near

	lodsb
	cmp	al, ':'		; Make sure they did /L:
	jnz	plE1		; If they didn't, return with carry set.

; ------------------------------------------
; PL10--ES:SI = a UMB number, after /L: or ;
; ------------------------------------------

pl10:	call	GetXNum		; After this, 'tis ",size" or ";umb" or " mod"
	jc	plE2		; And error if it's a bad number.
	call	convUMB		; Convert any address to a UMB number

	mov	cl, al		; Remember the UMB number
	call	stowUMB		; Mark this UMB # as used;
	jc	plE2		; If it was already marked, it'll error

	call	incArgc		; Each UMB number is another arg for LH to skip

	lodsb
	cmp	al, ';'		; Did "umb;" ?
	jz	pl10		; Yep: go back and get another UMB.

	call	isWhite		; Did "umb " ?
	jz	plX		; Yep: return (it'll go back to whitespace)

	call	isEOL		; Did "umb" ?
	jz	plSwX		; If so, backup and exit like everything's ok

	cmp	al, SWTCH	; Did "umb/" ? (as in, "/L:1,100;2/S")
	jz	plSwX		; If so, back up ES:SI one character and return

	cmp	al, ','		; Did "umb," ?
	jnz	plE1		; Just what the heck DID they do? Return error.

; --- Read a size -------------------------------------------------------------

	call	GetXNum		; Stop on "size;" or "size " or anything else
	jc	plE1		; And error if it's a bad size.

	call	toPara		; Convert from bytes to paragraphs

	call	stowSiz		; CL still has the UMB number for this routine

	call	incArgc		; Each UMB size is another arg for LH to skip

	lodsb
	cmp	al, ';'		; They did "umb,size;", so get another UMB.
	jz	pl10		;

	call	isWhite		; Did it end with whitespace?
	jz	plX		; If so, we're done here--go back.

	call	isEOL		; Did they do "umb,size" and end??? (stupid)
	jz	plSwX		; If so, backup and exit like everything's ok

	cmp	al, SWTCH	; Did they do "umb,size/" ?
	jz	plSwX		; If so, again, we're done here.

plE1:	mov	ax, PV_InvArg	; If not, we don't know WHAT they did...
	dec	si
	stc
	ret

plE2:	mov	ax, PV_BadUMB	; In this case, they've specified a UMB twice
	stc
	ret

plSwX:	dec	si		; If we hit a '/' character, back up one char
				; so the whitespace checker will see it too.

plX:	clc			; Then just return with carry clear, so
	ret			; ParseVar will go about its business.
parseL	endp

; -----------------------------------------------------------------------------
;*** incArgc - increments fm_argc, for use with LoadHigh command-line parsing
; -----------------------------------------------------------------------------
; ENTRY:    None
; EXIT:     None
; ERROR:    None
; USES:     fm_argc, flags
; -----------------------------------------------------------------------------

incArgc	proc	near
	push	ax

	getdata	al, fm_argc	; Obtain previous value of fm_argc,
	inc	al		; Increment it,
	putdata	fm_argc, al	; And store it right back.

	pop	ax
	ret
incArgc	endp

; -----------------------------------------------------------------------------
;*** isEOL - returns with ZF set iff AL contains CR or LF, or 0
; -----------------------------------------------------------------------------
; ENTRY:    AL contains character to test
; EXIT:     ZF set iff AL contains CR or LF, or 0
; ERROR:    None
; USES:     ZF
; -----------------------------------------------------------------------------

isEOL	proc	near
	cmp	al, 0		; Null-terminator
	jz	ieX
	cmp	al, CR		; Carriage Return
	jz	ieX
	cmp	al, LF		; LineFeed
ieX:	ret
isEOL	endp

; -----------------------------------------------------------------------------
;*** isWhite - returns with ZF set iff AL contains whitespace (or "=")
; -----------------------------------------------------------------------------
; ENTRY:    AL contains character to test
; EXIT:     ZF set iff AL contains space, tab, or equals
; ERROR:    None
; USES:     ZF
; -----------------------------------------------------------------------------

isWhite	proc	near
	cmp	al, ' '		; Space
	jz	iwX
	cmp	al, '='		; Equals (treat as whitespace)
	jz	iwX
	cmp	al, TAB		; Tab
iwX:	ret
isWhite	endp

; -----------------------------------------------------------------------------
;*** unMarkUMB - marks a given UMB as unused, even if previously marked used
; -----------------------------------------------------------------------------
; ENTRY:    AL contains UMB number
; EXIT:     None
; ERROR:    None
; USES:     Flags, variables in highvar.inc
; -----------------------------------------------------------------------------

unMarkUMB	proc	near
	pushreg	<ax, bx, di, es>
	dataseg	es

	xor	ah, ah
	mov	bx, ax
	mov	es:UmbUsed[bx], 0
	cmp	UmbLoad, al
	jnz	umu10

	mov	UmbLoad, 0	; If unmarked the load UMB, load into convent.

umu10:	popreg	<es, di, bx, ax>
	normseg	es
	ret
unMarkUMB	endp

; -----------------------------------------------------------------------------
;*** stowUMB - marks a given UMB as used, if it hasn't been so marked before
;            -- accepts a UMB # in AL, and makes sure it hasn't yet been
; listed in the /L:... chain.  If it's the first one specified, it sets UmbLoad
; to that UMB #... and in any case, it marks the UMB as specified.
; -----------------------------------------------------------------------------
; ENTRY:    AL contains UMB number, as specified by the user
; EXIT:     None
; ERROR:    Carry set if UMB # is less than 0 or >= MAXUMB (see highvar.inc)
; USES:     AX, Flags, variables in highvar.inc
; -----------------------------------------------------------------------------

stowUMB	proc	near
	cmp	al, MAXUMB
	jb	su10
	stc
	ret			; Ooops-- UMB>=MAXUMB

su10:	pushreg	<bx, di, si, ds, es>

	dataseg	es		; Point ES into appropriate data segment
	dataseg	ds		; Point DS into appropriate data segment

	cmp	UmbLoad, UNSPECIFIED	; If this, we haven't been here before
	jne	su20
	mov	UmbLoad, al	; So remember this UMB as the load UMB slot.

su20:	or	al, al		; If they gave UMB 0, there's really nothing
	jz	su30		; that we should do here.

	mov	bl, al
	xor	bh, bh
	mov	ax, 1		; Now, AX = 1, and BX = UMB Number

	xchg	ES:UmbUsed[bx], al

	or	al, al		; If it was already 1, then al==1... and that
	jz	su30		; means an error.

	stc			; OOOPS!  This one's been used before.  :(

su30:	popreg	<es, ds, si, di, bx>
	normseg	ds
	normseg	es
	ret
stowumb	endp

; -----------------------------------------------------------------------------
;*** stowSiz - marks a given UMB as having a given minimum size
; -----------------------------------------------------------------------------
; ENTRY:    CL contains UMB number, AX contains size
; EXIT:     None
; ERROR:    None
; USES:     AX, DX, Flags, variables in highvar.inc
; -----------------------------------------------------------------------------

stowSiz	proc	near
	pushreg	<bx, di, es>
	dataseg	es			; Point ES into appropriate data seg

	mov	bl, cl			; Now bl==UMB number, AX==size
	mov	bh, 0			;     bx==UMB number, AX==size
	shl	bl, 1			;     bx==offset into array, AX=size
	mov	es:UmbSize[bx], ax	; Store the size

	popreg	<es, di, bx>
	normseg	es			; Return ES to where it was
	ret
stowSiz	endp

; -----------------------------------------------------------------------------
;*** toDigit - converts a character-digit to its binary counterpart
;            -- verifies that CL contains a valid character-digit; if so, it
; changes CL to its counterpart binary digit ((CL-'0') or (CL-'A'+10)).  A-F
; are considered valid iff gnradix is 16.
; -----------------------------------------------------------------------------
; ENTRY:    CL contains a digit ('0' to '9' or, if gnradix==16, 'A' to 'F')
; EXIT:     CL contains digit in binary (0 to 9 or, if gnradix==16, 0 to 15)
; ERROR:    Carry set indicates invalid digit; carry clear indicates good digit
; USES:     CL, Flags
; -----------------------------------------------------------------------------
; If the string is preceeded with "0x", the value is read as hexadecimal; else,
; as decimal.  After a read, you may check the radix by examining gnradix--it
; will be 10 or 16.
; -----------------------------------------------------------------------------

public	gnradix
gnradix	dw	?		; Must be a word--16x16 multiplication

toDigit	proc	near
	cmp	gnradix, 16
	jnz	td20		; Don't check hex digits if radix isn't 16

	cmp	cl, 'a'
	jb	td10
	cmp	cl, 'f'
	ja	tdE		; Nothing valid above 'z' at all...
	sub	cl, 'a'-10	; Make 'a'==10 and return.
;	clc			; <- CLC is implicit from last SUB
	ret

td10:	cmp	cl, 'A'
	jb	td20		; Below 'A'?  Not a letter...
	cmp	cl, 'F'
	ja	tdE		; Above 'F'?  Not a digit.
	sub	cl, 'A'-10	; Make 'A'==10 and return.
;	clc			; <- CLC is implicit from last SUB
	ret

td20:	cmp	cl, '0'		; If less than zero,
	jb	tdE		; Done.
	cmp	cl, '9'		; Or, if greater than nine,
	ja	tdE		; Done.
	sub	cl, '0'		; Okay--make '0'==0 and return.
;	clc			; <- CLC is implicit from last SUB
	ret

tdE:	stc
	ret
todigit	endp

; -----------------------------------------------------------------------------
;*** GetXNum - reads a 32-bit ASCII number at ES:SI and returns it in DX:AX
; -----------------------------------------------------------------------------
; ENTRY:    ES:SI points to an ascii string to scan
; EXIT:     ES:SI moved to first invalid digit, DX:AX contains value read
; ERROR:    Carry set if # is too big, or has no digits (EOL possibly)
; USES:     ES:SI, DX, AX, Flags, gnradix
; -----------------------------------------------------------------------------
; If the string is preceeded with "0x", the value is read as hexadecimal; else,
; as decimal.  After a read, you may check the radix by examining gnradix--it
; will be 10 or 16.
; -----------------------------------------------------------------------------

	public	GetXNum

GetXNum	proc	near
	pushreg	<bx, cx, ds>

	cld
	xor	ax, ax
	xor	bx, bx
	xor	cx, cx
	xor	dx, dx			; Start with 0 (makes sense)

	mov	gnradix, 10		; And default to a radix of 10 (dec)

	mov	cl, byte ptr es:[si]	; Now AX=0, BX=0, CH=0/CL=char, DX=0
	call	toDigit
	jc	gxnE			; If it's not a digit, leave now.

	or	cl, cl
	jnz	gxn20			; Doesn't have '0x'
	mov	cl, byte ptr es:[si+1]
	cmp	cl, 'x'			; Either 'x'...
	jz	gxn10
	cmp	cl, 'X'			; ...or 'X' means it's hexadecimal
	jnz	gxn20

gxn10:	mov	gnradix, 16
	inc	si			; Since we read "0x", march over it.
	inc	si

; ------------------------------------------------------
; GXN20--ES:SI = a digit in a number; if not, we're done
;        DX:AX = current total
;        BX    = 0
;        CH    = 0
; ------------------------------------------------------

gxn20:	mov	cl, byte ptr es:[si]	; Now DX:AX=current total, CH=0/CL=char
	inc	si

	call	toDigit		; Accepts only valid digits, A-F -> 10-16
	jc	gxnQ		; <- Ah... wasn't a digit.  Stop.

	call	mul32		; Multiply DX:AX by gnradix
	jc	gxnX		; (if it's too big, error out)

	add	ax, cx		; Add the digit
	adc	dx, bx		; (BX is 0!)--Adds 1 iff last add wrapped
	jc	gxnX		; If _that_ wrapped, it's too big.
	jmp short gxn20

gxnE:	stc			; In this case, we need to set the carry
	jmp short gxnX		; and leave--there were no digits given.

gxnQ:	dec	si		; Don't read in the offensive character.
	clc			; And clear carry, so they know it's okay.

gxnX:	popreg	<ds, cx, bx>
	ret
GetXNum	endp

; -----------------------------------------------------------------------------
;*** mul32 - multiplies the number in DX:AX by gnradix
; -----------------------------------------------------------------------------
; ENTRY:   DX:AX = the number to be multiplied, BX = 0, gnradix = multiplier
; EXIT:    DX:AX has been multiplied by gnradix if carry clear; BX still 0
; ERROR:   Carry set if number was too large
; USES:    Flags, AX, DX
; -----------------------------------------------------------------------------

mul32	proc	near
	push	ax		; DX=old:hi, AX=old:lo, TOS=old:lo, BX=0
	mov	ax, dx		; DX=old:hi, AX=old:hi, TOS=old:lo, BX=0
	mul	gnradix		; DX=?,      AX=new:hi, TOS=old:lo, BX=0
	jc	m32E		; Too big?

	mov	dx, ax		; DX=new:hi, AX=new:hi, TOS=old:lo, BX=0
	pop	ax		; DX=new:hi, AX=old:lo, TOS=orig,   BX=0

	xchg	dx, bx		; DX=0,      AX=old:lo, TOS=orig,   BX=new:hi
	mul	gnradix		; DX=carry,  AX=new:lo, TOS=orig,   BX=new:hi
	xchg	dx, bx		; DX=new:hi, AX=new:lo, TOS=orig,   BX=carry
	add	dx, bx		; DX=new:hi, AX=new:lo, TOS=orig,   BX=carry
	xor	bx, bx		; DX=new:hi, AX=new:lo, TOS=orig,   BX=0
	ret

m32E:	pop	ax
	ret
mul32	endp

; -----------------------------------------------------------------------------
;*** toPara - divides DX:AX by 16; result in AX only (discards extra DX data)
; -----------------------------------------------------------------------------
; ENTRY:   DX:AX = the number to be divided
; EXIT:    Interpereting DX:AX as bytes, AX=paragraph equivalent, 0xFFFF max
; ERROR:   None
; USES:    Flags, AX, DX
; -----------------------------------------------------------------------------
; Note: The 386 has a 32-bit SHR, which would work perfectly for this... but we
;       can't ensure a 386 host machine.  Sorry.
; -----------------------------------------------------------------------------

toPara	proc	near
	push	cx		; DX:AX=HHHH hhhh hhhh hhhh:LLLL llll llll llll

	mov	cl, 4		;
	shr	ax, cl		; DX:AX=HHHH hhhh hhhh hhhh:0000 LLLL llll llll
	xchg	ax, dx		; DX:AX=0000 LLLL llll llll:HHHH hhhh hhhh hhhh
	mov	cl, 12
	shl	ax, cl		; DX:AX=0000 LLLL llll llll:hhhh 0000 0000 0000
	or	ax, dx		;    AX=hhhh LLLL llll llll

	pop	cx
	ret
toPara	endp

ifdef DEBUG_PRTABLE
; -----------------------------------------------------------------------------
;*** PrTable - produces a printout of the variables in highvar.inc
;            -- The data are reported in this format:
;
;	ShrinkUMBs? (01/00)	LoadAddr (DH only)	UMB0 (load UMB number)
;	00	conv_used?	conv_size (ignored)
;	01	umb1_used?	umb1_size (00==not specified)
;	02	umb2_used?	umb2_size
;		...
;	0F	umb15_used?	umb15_size
;
; -----------------------------------------------------------------------------
; ENTRY/EXIT : Nothing
; USES:        Flags, AX
; -----------------------------------------------------------------------------
; This routine is for debugging only, and should not be included in the final
; versions of loadhigh and devicehigh (if it's called, you'll know).  To use
; it, you'll need to add "include wordout.inc" if it's not available already.
; -----------------------------------------------------------------------------

	public	PrTable

PrTable	proc	near
	pushreg	<cx, si, ds>
	dataseg	ds			; Point DS into appropriate data seg

	mov	al, fUmbTiny
	call	byteout
	printtab
	mov	ax, SegLoad
	call	wordout
	printtab
	mov	al, UmbLoad
	call	byteout
	printtab
	mov	al, fInHigh
	call	byteout
	printcr
	printcr

	xor	cx,  cx			; For each entry

; ----------------------------
; PT10--CL = index into arrays
; ----------------------------

pt10:	mov	al, cl
	call	byteout
	printtab

	mov	si, offset DS:UmbUsed	; on the UmbUsed array,
	mov	ax, cx
	add	ax, si
	mov	si, ax
	lodsb				; Get used element
	call	byteout
	printtab

	mov	si, offset DS:UmbSize	; on the UmbSize array,
	mov	ax, cx
	shl	ax,  1			; ax *= 2 (two bytes per element)
	add	ax, si
	mov	si, ax
	lodsw				; Get size element
	call	wordout
	printcr

	inc	cx
	cmp	cx, MAXUMB
	jb	pt10

	popreg	<ds, si, cx>
	normseg	ds			; Return DS
	ret
PrTable endp

endif	; DEBUG_PRTABLE

; -----------------------------------------------------------------------------
;*** UmbHead - returns in AX the address of the first UMB block (0x9FFF)
; -----------------------------------------------------------------------------
; ENTRY:  Nothing
; EXIT:   AX contains 0x9FFF for most systems
; ERROR:  Carry set if pointer is 0xFFFF (if not set up yet--DH runs into this)
; USES:   Flags, AX
; -----------------------------------------------------------------------------
; Early in the boot-cycle, the pointer used to obtain this value isn't set up;
; to be precise, before a UMB provider is around.  In this event, the pointer
; is always set to 0xFFFF; it changes once a provider is around.  On most
; machines (all of 'em I've seen), it changes to 0x9FFF at that point.
; -----------------------------------------------------------------------------

	public	UmbHead

UmbHead	proc	near
	pushreg	<si, ds, es>

	mov	ah, DOS_GET_DOS_LISTS	; Call int 21h, function 52h...
	int	21h

	mov	ax, es:[DOS_UMB_HEAD]	; And read what's in ES:[008C]
	cmp	ax, 0FFFFh
	jz	uhE			; If it's 0xFFFF, it's an error...

	clc				; Else, it isn't (CLC done by prev cmp)
	jmp short uhX

uhE:	stc
uhX:	popreg	<es, ds, si>
	ret
UmbHead	endp

; -----------------------------------------------------------------------------
;*** isSysMCB - sets ZF iff ES points to an MCB owned by "SC" + (8 or 9)
; -----------------------------------------------------------------------------
; ENTRY:  ES:0 should point to a valid MCB
; EXIT:   ZF set if owned by SC+8 or SC+9 (for japan)
; USES:   Flags
; -----------------------------------------------------------------------------

isSysMCB	proc	near
	push	ax

	mov	ax, es:[arena_owner]	; Check the owner...
	cmp	ax, SystemPSPOwner	; 8 (for US OR Japan) is valid
	jz	ism10
	cmp	ax, JapanPSPOwner	; 9 (for Japan) is valid
	jz	ism10
	jmp short ismX			; Anything else isn't.

ism10:	mov	ax, word ptr es:[arena_name]	; Check the name...
	cmp	ax, 'CS'

ismX:	pop	ax
	ret
isSysMCB	endp

; -----------------------------------------------------------------------------
;*** AddrToUmb - converts a segment address in AX to its appropriate UMB number
; -----------------------------------------------------------------------------
; ENTRY:  AX contains a segment address
; EXIT:   AX will contain the UMB number which contains the address (0==conv)
; ERROR:  If the address is above UM Range, AX will return as FFFF.
; USES:   Flags, AX
; -----------------------------------------------------------------------------
; An address in the following areas is treated as:
;    0      <-> umbhead (0x9FFF)          = Conventional memory
;    0x9FFF <-> addr of first UM sys MCB  = UMB #1
;      ...
;    addr of last UM sys MCB <-> TOM      = invalid; returns #0xFFFF
; -----------------------------------------------------------------------------

AddrToUmb	proc	near
	pushreg	<cx, dx, es>

	mov	dx, ax		; DX = address to search for

	call	UmbHead		; AX = first segment
	jc	atuE		; If it couldn't get it, error out.

	mov	es, ax		; ES = first UMB segment
	xor	cx, cx		; Pretend we're on UMB 0 for now... (cx = UMB#)

; ----------------------------------------
; ATU10--ES - Current MCB address
;        DX - Address given for conversion
;        CX - Current UMB #
; ----------------------------------------

atu10:	mov	ax, es
        cmp	ax, dx		; Present segment >= given segment?
	jae	atuX		; Yep--done.

	call	isSysMCB	; Returns with ZF set if this is a system MCB
	jnz	atu20

	inc	cx		; If it _was_ a system MCB, we're in a new UMB.

atu20:	mov	al, es:[arena_signature]
	cmp	al, arena_signature_end
	jz	atu30		; 'Z' means this was the last MCB... that's it.

	NextMCB	es, ax

	jmp short atu10

; -----------------------------------------------------------------------------
; if we get to atu30, they specified a number that was past the last MCB.
; make sure it's not _inside_ that MCB before we return an error condition.
; -----------------------------------------------------------------------------

atu30:	mov	ax, es
	add	ax, es:[arena_size]
	cmp	ax, dx		; Present >= given?
	jae	atuX		; Yep! It _was_ inside.

atuE:	xor	cx, cx		; Else, fall through with UMB # == -1
	dec	cx		; (that makes it return 0xFFFF and sets CF)

atuX:	mov	ax, cx		; Return the UMB number in AX
	popreg	<es, dx, cx>
	ret
AddrToUmb	endp

; -----------------------------------------------------------------------------
;*** convUMB - checks after GetXNum to convert an address to a UMB number
;            -- if GetXNum read a hex number, we interperete that as a segment
; address rather than a UMB number... and use that address to look up a UMB.
; This routine checks for that condition and calls AddrToUmb if necessary.
; -----------------------------------------------------------------------------
; ENTRY:  AX contains a UMB number or segment, gnradix has been set by GetXNum
; EXIT:   AX will contain a UMB number
; ERROR:  None
; USES:   Flags, AX
; -----------------------------------------------------------------------------

convUMB	proc	near
	cmp	gnradix, 16
	jnz	cu10		; If it didn't read in hex, it's not an address
	call	AddrToUmb	; Else, convert the address to a UMB number
	cmp	ax, 0FFFFh
	jnz	cu10
	inc	ax		; If too high, ignore it (make it conventional)
cu10:	ret
convUMB	endp

; -----------------------------------------------------------------------------
;*** setUMBs - links umbs and sets allocation strategy for a load
;            -- if LoadHigh, the allocation strategy MAY be LOW_FIRST instead
; of the usual HIGH_FIRST.  See the code.
; -----------------------------------------------------------------------------
; ENTRY:  None
; EXIT:   None
; ERROR:  None
; USES:   Flags, fm_umb, fm_strat
; -----------------------------------------------------------------------------

setUMBs	proc	near
	pushreg	<ax, bx>

	call	fm_link

ifdef HV_LoadHigh
	mov	ax, DOS_CHECK_STRATEGY
	int	21h

	putdata	fm_strat, al	; Store the current strategy for later restore

	and	ax, 007Fh	; 0000.0000.0111.1111 == All that other stuff
	push	ax		; Watch this carefully...

	call	loadLow		; returns al==0 if load low, al==1 if loadhigh
	ror	al, 1		; Shift that to al==0 or al==0x80

	pop	bx		; ...pushed as AX above
	or	bl, al		; Now we have 0000.0000.?111.1111 in BX;

	mov	ax, DOS_SET_STRATEGY ; with ? ==1 if load highfirst.  Perfect!

	int	21h
endif

	popreg	<bx, ax>
	ret
setUMBs	endp

; -----------------------------------------------------------------------------
;*** loadLow - returns AL==0 if UMB0 == 0, else AL==1
; -----------------------------------------------------------------------------
; ENTRY:  None
; EXIT:   AL==0 if mem strategy should be set to LOW_FIRST, else AL==1
;         Carry set if UMB0 not specified (_NOT_ an error)
; ERROR:  None
; USES:   Flags, fm_strat, fm_umb
; -----------------------------------------------------------------------------
; We want to set the memory strategy to LOW_FIRST if the user specified a
; load UMB, and it is 0.  That 0 can be either from the user having _specified_
; zero (/L:0;...), or from having specified a too-big min size (/L:1,99999999)
; such that the load UMB is too small, and shouldn't be used.
; -----------------------------------------------------------------------------


loadLow	proc	near
	push	ds
	dataseg	ds		; Point DS into appropriate data segment

	mov	al, UmbLoad
	cmp	al, UNSPECIFIED
	jnz	ll10

	mov	al, 1		; Return with AL==1 && STC if no UMBs specified
	stc
	jmp short llX

ll10:	or	al, al		; AL=the load UMB: Is it == 0?
	jz	llX		; Yep... CF==0 (from OR) && AL=0, so just exit

	mov	al, 1
	clc

llX:	pop	ds		; Return DS to where it was
	normseg	ds		;
	ret
loadLow	endp

; -----------------------------------------------------------------------------
;*** HideUMBs - links UMBs and hides upper-memory as appropriate
; -----------------------------------------------------------------------------
; ENTRY:  None
; EXIT:   None
; ERROR:  None
; USES:   Flags, fm_strat, fm_umb
; -----------------------------------------------------------------------------

	public	HideUMBs

HideUMBs	proc
	pushreg	<ax, cx, ds, es>

	call	UmbTest		; See if we REALLY linked in anything...
	jc	husX		; ...if not, there's nothing for us to do.

	call	FixMem		; Concatenate adjacent free MCBs in upper mem
	call	setUMBs		; Link UMBs and set memory-allocation strategy

	putdata	fInHigh, 1	; Remember that we're now running high

	call	GetLoadUMB	; See if they gave us a list to leave free
	cmp	al, UNSPECIFIED	; If they didn't,
	jz	husX		; then we shouldn't do this loop:

	xor	cx, cx

; -----------------------------------------------
; HUS10-CX - UMB number (after inc, 1==first UMB)
; -----------------------------------------------

hus10:	inc	cx		; For each UMB:
	cmp	cx, MaxUMB
	jae	hus20

	mov	al, cl		; (stopping as soon as we're outside of the
	push	es
	call	findumb		; valid range of UMBs)
	pop	es		; push/pop: trash what findumb finds.  :-)
	jc	hus20

	call	hideUMB?	; hide what we need to hide.

	jmp short hus10

hus20:	call	GetLoadUMB	; Now check if they offered /L:0
	or	al, al		; --Is the load UMB 0? (-1==unspecified)
	jnz	husX		; If not, we're done.

	call	hl_unlink	; If so, however, fix UMBs and strategy.

husX:	popreg	<es, ds, cx, ax>
	ret
HideUMBs	endp

; -----------------------------------------------------------------------------
;*** GetLoadUMB - Returns the load UMB number in AL (-1 if not specified)
; -----------------------------------------------------------------------------
; ENTRY:  None
; EXIT:   AL == load UMB
; ERROR:  None
; USES:   Flags, AX
; -----------------------------------------------------------------------------

	public	GetLoadUMB

GetLoadUMB	proc	near
	getdata	al, UmbLoad
	ret
GetLoadUMB	endp

; -----------------------------------------------------------------------------
;*** GetLoadSize - Returns the load UMB minimum size (0 if not specified)
; -----------------------------------------------------------------------------
; ENTRY:  None
; EXIT:   AX == load UMB minimum size
; ERROR:  None
; USES:   Flags, AX
; -----------------------------------------------------------------------------

	public	GetLoadSize

GetLoadSize	proc	near
	pushreg	<bx, si, ds>
	dataseg	ds

	mov	al, UmbLoad

	xor	ah, ah			;    ax==UMB
	mov	bx, offset DS:UmbSize	;                     bx==array
	shl	al, 1	                ;    ax==offset
	add	ax, bx			;    ax==element index
	mov	si, ax			; ds:si==element index

	lodsw				;    ax==size

	popreg	<ds, si, bx>
	normseg	ds
	ret
GetLoadSize	endp

; -----------------------------------------------------------------------------
;*** GetSize - Returns the UMB in AL's minimum size (0 if not specified)
; -----------------------------------------------------------------------------
; ENTRY:  AL == a UMB number
; EXIT:   AX == UMB minimum size, as specified by the user
; ERROR:  None
; USES:   Flags, AX
; -----------------------------------------------------------------------------

GetSize	proc	near
	pushreg	<bx, si, ds>
	dataseg	ds

	xor	ah, ah			;    ax==UMB
	mov	bx, offset DS:UmbSize	;                     bx==array
	shl	al, 1	                ;    ax==offset
	add	ax, bx			;    ax==element index
	mov	si, ax			; ds:si==element index

	lodsw				;    ax==size

	popreg	<ds, si, bx>
	normseg	ds
	ret
GetSize	endp

; -----------------------------------------------------------------------------
;*** StoLoadUMB - Overrides the load UMB number with what's in AL
; -----------------------------------------------------------------------------
; ENTRY:   AL == new load UMB
; EXIT:    None
; ERROR:   None
; USES:    Flags, AX
; -----------------------------------------------------------------------------
; CAUTION: Should only be used if /L:... was used.  Logically, that is the only
;          time you would ever need this, so that's okay.
; -----------------------------------------------------------------------------

	public	StoLoadUMB

StoLoadUMB	proc	near
	putdata	UmbLoad, al
	ret
StoLoadUMB	endp

; -----------------------------------------------------------------------------
;*** StoLoadSize - Overrides the load UMB minimum size with what's in AX
; -----------------------------------------------------------------------------
; ENTRY:  AL == new load size
; EXIT:   None
; ERROR:  None
; USES:   Flags, AX
; -----------------------------------------------------------------------------

	public	StoLoadSize

StoLoadSize	proc
	push	dx

	getdata	dl, UmbLoad		; Put UMB# in DL and size in AX
	cmp	dl, UNSPECIFIED
	jz	sls10

	call	stowsiz			; We've got a function to do just this

sls10:	pop	dx
	ret
StoLoadSize	endp

; -----------------------------------------------------------------------------
;*** hideUMB - marks as HIDDEN all FREE elements in UMB passed as AL
; -----------------------------------------------------------------------------
; ENTRY:    AL must indicate a valid UMB; 0==conv && is invalid.
; EXIT:     None; free elements in UMB marked as hidden
; ERROR:    None
; USES:     Flags
; -----------------------------------------------------------------------------

hideUMB	proc	near
	pushreg	<ax, es>

	call	findUMB		; Returns with carry if err, else ES == MCB
	jc	huX

; ------------------------------------------------
; HU10--ES - MCB inside UMB; if it's a system MCB,
;            we're not in the same UMB, so exit.
; ------------------------------------------------

hu10:	call	isSysMCB	; Returns with ZF set if owner is SYSTEM
	jz	huX		; If it is, we've finished the UMB.
	call	isFreeMCB	; Returns with ZF set if owner is 0
	jnz	hu20

	call	hideMCB

hu20:	mov	al, es:[arena_signature]
	cmp	al, arena_signature_end
	jz	huX		; 'Z' means this was the last MCB... that's it.

	NextMCB	es, ax		; Go on forward.

	jmp short hu10

huX:	popreg	<es, ax>
	ret
hideUMB	endp

; -----------------------------------------------------------------------------
;*** isTiny - returns with ZF set if user didn't specify /S
; -----------------------------------------------------------------------------
; ENTRY:    None
; EXIT:     ZF set if user DIDN'T specify /S
; ERROR:    None
; USES:     Flags
; -----------------------------------------------------------------------------

isTiny	proc	near
	push	ax
	getdata	al, fUmbTiny
	or	al, al
	pop	ax
	ret
isTiny	endp

; -----------------------------------------------------------------------------
;*** isFreeMCB - returns with ZF set if current MCB (ES:0) is FREE
; -----------------------------------------------------------------------------
; ENTRY:    ES:0 should point to an MCB
; EXIT:     ZF set if MCB is free, else !ZF
; ERROR:    None
; USES:     Flags
; -----------------------------------------------------------------------------

isFreeMCB	proc	near
	or	ES:[arena_owner], 0
	ret
isFreeMCB	endp

; -----------------------------------------------------------------------------
;*** hideMCB - marks as HIDDEN the MCB at ES:0
; -----------------------------------------------------------------------------
; ENTRY:    ES:0 should point to an MCB
; EXIT:     None; MCB marked as HIDDEN
; ERROR:    None
; USES:     None
; -----------------------------------------------------------------------------

hideMCB	proc	near
	mov	es:[arena_owner], SystemPSPOwner
	mov	word ptr es:[arena_name+0], 'IH'
	mov	word ptr es:[arena_name+2], 'DD'
	mov	word ptr es:[arena_name+4], 'NE'
	mov	word ptr es:[arena_name+6], '  '
	ret
hideMCB	endp

; -----------------------------------------------------------------------------
;*** unHideMCB - marks as FREE the MCB at ES:0
; -----------------------------------------------------------------------------
; ENTRY:    ES:0 should point to an MCB
; EXIT:     None; MCB marked as FREE
; ERROR:    None
; USES:     None
; -----------------------------------------------------------------------------

unHideMCB	proc	near
	push	ax
	mov	es:[arena_owner], FreePSPOwner
	mov	ax, '  '
	mov	word ptr es:[arena_name+0], ax
	mov	word ptr es:[arena_name+2], ax
	mov	word ptr es:[arena_name+4], ax
	mov	word ptr es:[arena_name+6], ax
	pop	ax
	ret
unHideMCB	endp

; -----------------------------------------------------------------------------
;*** findUMB - makes ES:0 point to the first MCB in UMB given as AL
;            -- returns UmbHEAD pointer (0x9FFF) if passed AL==0
; -----------------------------------------------------------------------------
; ENTRY:    AL should be to a valid UMB number
; EXIT:     ES:0 points to first MCB in UMB (_not_ the 8+SC MCB that heads it)
; ERROR:    Carry set if couldn't reach UMB (too high)
; USES:     Flags, ES
; -----------------------------------------------------------------------------

findUMB	proc	near
	pushreg	<ax, cx, dx>

	xor	ah, ah		; Zap ah, so al==ax

	mov	dx, ax		; Store the to-be-found UMB number in DX

	call	UmbHead		; Returns first UMB segment in AX
	mov	es, ax
	xor	cx, cx		; Pretend we're on UMB 0 for now...

; ---------------------------------------------
; FU10--CX - This UMB number; 0 == conventional
;       DX - The UMB number they're looking for
;       ES - The current MCB address
; ---------------------------------------------

fu10:	cmp	cx, dx		; If CX==DX, we've found the UMB we're
	jz	fuX		; searching for--so exit.

	call	isSysMCB	; Returns with ZF set if owner is SYSTEM
	jnz	fu20

	inc	cx		; If it _was_ SYSTEM, we're in a new UMB.

fu20:	mov	al, es:[arena_signature]
	cmp	al, arena_signature_end
	jz	fuE		; 'Z' means this was the last MCB... that's it.

	NextMCB	es, ax		; Go on forward.

	jmp short fu10

fuE:	stc
fuX:	popreg	<dx, cx, ax>	; The address is already in ES.
	ret
findUMB	endp

; -----------------------------------------------------------------------------
;*** BigFree - makes ES:0 point to the largest free MCB in UMB given as AL
; -----------------------------------------------------------------------------
; ENTRY:    AL should be to a valid UMB number
; EXIT:     ES:0 points to largest free MCB in UMB, AX returns its size
; ERROR:    Carry set if couldn't reach UMB (0 or too high)
; USES:     Flags, ES
; -----------------------------------------------------------------------------

	public	BigFree

BigFree	proc	near
	pushreg	<bx, cx>

	call	findUMB			; Returns with CF if err, else ES==MCB
	jc	bfX			; (would be "jc bfE"; it just does stc)

	xor	bx, bx			; Segment address of largest free MCB
	xor	cx, cx			; Size of largest free MCB

; ---------------------------------------------
; BF10--ES - Current MCB address
;       BX - Address of largest free MCB so far
;       CX - Size of largest free MCB so far
; ---------------------------------------------

bf10:	call	isSysMCB		; If we've left the MCB, we're done.
	jz	bf30

	call	isFreeMCB		; Returns with ZF set if owner is 0
	jnz	bf20

	cmp	cx, es:[arena_size]	; Compare sizes...
	jg	bf20			; Unless we're bigger,

	mov	bx, es			; Store this new element's address,
	mov	cx, es:[arena_size]	; and its size.

bf20:	mov	al, es:[arena_signature]
	cmp	al, arena_signature_end
	jz	bf30			; 'Z' means this was the last MCB.

	NextMCB	es, ax			; Go on forward.

	jmp short bf10

bf30:	mov	es, bx			; Return the address
	mov	ax, cx			; Return the size
	or	bx, bx
	jnz	bfX			; (if size==0, there's nothing free)

bfE:	stc
bfX:	popreg	<cx, bx>

	ret
BigFree	endp

; -----------------------------------------------------------------------------
;*** isSpecified - sets ZF if UMB in AL wasn't specified in DH/LH line.
; -----------------------------------------------------------------------------
; ENTRY:    AL should be to a valid UMB number
; EXIT:     ZF set if UMB wasn't specified, ZF clear if it was
; ERROR:    None
; USES:     Flags
; -----------------------------------------------------------------------------

isSpecified	proc	near
	push	ax

	xor	bh, bh
	mov	bl, al
	getdata	al, DS:UmbUsed[bx]
	or	al, al			; Sets ZF if al==0 (ie, if unspecified)

	pop	ax
	ret
isSpecified	endp

; -----------------------------------------------------------------------------
;*** shrinkMCB - breaks an MCB into two pieces, the lowest one's size==AX
; -----------------------------------------------------------------------------
; ENTRY:    AX == new size, ES:0 == current MCB
; EXIT:     None; MCB broken if carry clear
; ERROR:    Carry set if MCB isn't as large as AX+0x20 (not a useful split)
; USES:     Flags
; -----------------------------------------------------------------------------
; If the size of the to-be-split MCB isn't at least 0x20 bytes greater than
; the specified new size, the split is useless; if it's onnly 0x10 bytes, that
; 0x10 will be used to make a header that mentions a 0-byte free space, and
; that just sucks up 0x10 bytes for nothing.  So we make 0x20 bytes the
; minimum for performing a split.
; -----------------------------------------------------------------------------

MIN_SPLIT_SIZE	equ	20h

shrinkMCB	proc	near
	pushreg	<bx, cx, es>

	mov	bx, ax			; Move things around... and
	mov	ax, es			; save this one for later.

	mov	cx, es:[arena_size]
	sub	cx, MIN_SPLIT_SIZE
	cmp	bx, cx			; {New size} vs {Current Size-20h}
	ja	smE			; if wanted_size > cur-20h, abort.

	mov	dl, es:[arena_signature]
	mov	cx, es:[arena_size]

	mov	word ptr es:[arena_size], bx
	mov	byte ptr es:[arena_signature], 'M'

	add	ax, bx
	inc	ax
	mov	es, ax			; Move to new arena area

	mov	ax, cx
	sub	ax, bx
	dec	ax			; And prepare the new size

	mov	byte ptr es:[arena_signature], dl
	mov	word ptr es:[arena_owner], 0
	mov	word ptr es:[arena_size], ax
	mov	ax, '  '
	mov	word ptr es:[arena_name+0], ax
	mov	word ptr es:[arena_name+2], ax
	mov	word ptr es:[arena_name+4], ax
	mov	word ptr es:[arena_name+6], ax

	clc
	jmp short smX

smE:	stc

smX:	popreg	<es, cx, bx>
	ret
shrinkMCB	endp

; -----------------------------------------------------------------------------
;*** hideUMB? - hides as appropriate the UMB in CL
; -----------------------------------------------------------------------------
; ENTRY:    CL should be to a valid UMB number, and AX to its address (findUMB)
; EXIT:     None; UMB is hidden as necessary
; ERROR:    None
; USES:     Flags, AX, CX
; -----------------------------------------------------------------------------
; PRIMARY LOGIC:
;
; If the UMB is specified in the DH/LH statement, then:
;    If the largest free segment is too small (check specified size), then:
;       Pretend it wasn't ever specified, and fall out of this IF.
;    Else, if largest free segment is LARGER than specified size, then:
;       If /S was given on the command-line, then:
;          Break that element into two pieces
;          Set a flag that we're shrinking
;       Endif
;    Endif
; Endif
; If the UMB is NOT specified (or was removed by the above):
;    Hide all free elements in the UMB
;    If the flag that we're shrinking was set, then:
;       UN-hide the lower portion of the shrunken UMB
;    ENDIF
; ENDIF
; -----------------------------------------------------------------------------

hideUMB?	proc
	pushreg	<bx, dx, es>

	mov	al, cl
	call	isSpecified	; Returns ZF set if al's umb was NOT specified
	jz	hu?20

	mov	al, cl		; Retrieve the size of the largest
	call	BigFree		; free element in AX; put its address in ES
	jc	hu?20		; Oops.  Errors mean skip this part.

	push	ax		; TOS==size of BigFree in UMB (popped as BX)
	mov	al, cl		; Retrieve the user's specified
	call	GetSize		; minimum size for this umb (into AX)
	pop	bx		; Now BX==BigFree, AX==Specified Size

	or	ax, ax		; If they didn't specify one,
	jz	hu?20		; Skip over all this.

	cmp	ax, bx		; Ah... if (specified > max free)
	jbe	hu?10

	mov	al, cl		;    Then mark that UMB as unused.  Nya nya.
	call	unMarkUMB
	jmp short hu?20

hu?10:	call	isTiny		; Returns ZF clear if user specified /S
	jz	hu?20

	call	shrinkMCB	; They specified /S, so shrink the MCB to AX
	jc	hu?20		; Ah... if didn't shrink after all, skip this:

	mov	dx, es
	jmp short hu?30		; Skip the spec check.. we wanna hide this one.

hu?20:	mov	ax, cx
	call	isSpecified	; If they specified this UMB, we're done...
	jnz	hu?X		; so leave.

	xor	dx, dx

hu?30:	mov	al, cl

	call	hideUMB		; Hides everything in UMB #al

	or	dx, dx		; Did we shrink a UMB?  If not, DX==0,
	jz	hu?X		; So we should leave.

	mov	es, dx		; Ah, but if it isn't, DX==the MCB's address;
	call	unHideMCB	; Un-hides the lower portion of that MCB.

hu?X:	popreg	<es, dx, bx>
	ret
hideUMB?	endp

; -----------------------------------------------------------------------------
;*** UnFreeze - Marks FROZEN elements as FREE
; -----------------------------------------------------------------------------
; Entry:  None
; Exit:   None; all 8+FROZEN elements are marked as FREE, from any UMB.
; Error:  None
; Uses:   Flags
; -----------------------------------------------------------------------------

	public  UnFreeze

UnFreeze	proc	near
	pushreg	<ax, es>

	call	UmbHead		; Returns with carry if err, else ES == MCB
	jc	ufX

	mov	es, ax

; ------------------------------
; UF10--ES - Current MCB address
; ------------------------------

uf10:	call	isFrozMCB	; Returns with ZF set if MCB is FROZEN
	jnz	uf20
	call	unHideMCB

uf20:	mov	al, es:[arena_signature]

	cmp	al, arena_signature_end
	jz	ufX		; 'Z' means this was the last MCB... that's it.

	NextMCB	es, ax		; Go on forward.
	jmp short uf10

ufX:	popreg	<es, ax>
	ret
UnFreeze	endp

; -----------------------------------------------------------------------------
;*** isFrozMCB - returns with ZF set if current MCB (ES:0) is FROZEN
; -----------------------------------------------------------------------------
; ENTRY:    ES:0 should point to an MCB
; EXIT:     ZF set if MCB is frozen, else !ZF
; ERROR:    None
; USES:     Flags
; -----------------------------------------------------------------------------

isFrozMCB	proc	near
	push	ax

	mov	ax, es:[arena_owner]	; Check the owner...
	cmp	ax, SystemPSPOwner	; 8 (for US OR Japan) is valid
	jnz	ifmX

	mov	ax, word ptr es:[arena_name]
	cmp	ax, 'RF'
	jnz	ifmX
	mov	ax, word ptr es:[arena_name+2]
	cmp	ax, 'ZO'
	jnz	ifmX
	mov	ax, word ptr es:[arena_name+4]
	cmp	ax, 'NE'
	jnz	ifmX
	mov	ax, word ptr es:[arena_name+6]
	cmp	ax, '  '

ifmX:	pop	ax
	ret
isFrozMCB	endp

; -----------------------------------------------------------------------------
;*** frezMCB - marks as 8+FROZEN the MCB at ES:0
; -----------------------------------------------------------------------------
; ENTRY:    ES:0 should point to an MCB
; EXIT:     None; MCB frozen
; ERROR:    None
; USES:     None
; -----------------------------------------------------------------------------

frezMCB	proc	near
	mov	es:[arena_owner], SystemPSPOwner
	mov	word ptr es:[arena_name+0], 'RF'
	mov	word ptr es:[arena_name+2], 'ZO'
	mov	word ptr es:[arena_name+4], 'NE'
	mov	word ptr es:[arena_name+6], '  '
	ret
frezMCB	endp

; -----------------------------------------------------------------------------
;*** FreezeUM - Marks FROZEN all UM elements now FREE, save those in load UMB
; -----------------------------------------------------------------------------
; Entry:  None
; Exit:   None; all free elements not in load UMB marked as 8+FROZEN
; Error:  None
; Uses:   Flags
; -----------------------------------------------------------------------------

FreezeUM	proc	near
	pushreg	<ax, cx, dx, es>

	call	GetLoadUMB
	xor	ah, ah		; Zap ah, so al==ax
	mov	dx, ax		; Store the load UMB in DX, so we can skip it

	call	UmbHead		; Returns first UMB segment in AX
	mov	es, ax
	xor	cx, cx		; Pretend we're on UMB 0 for now...

; -----------------------------------------
; FUM10--ES - Current MCB address
;        CX - Current UMB number
;        DX - UMB number to skip (load UMB)
; -----------------------------------------

fum10:	call	isSysMCB	; Returns with ZF set if owner is SYSTEM
	jnz	fum20

	inc	cx		; If it _was_ SYSTEM, we're in a new UMB.

fum20:	cmp	cx, dx		; If this is the load UMB, we don't want to
	jz	fum30		; freeze anything... so skip that section.

	call	isFreeMCB	; Oh.  If it's not free, we can't freeze it
	jnz	fum30		; either.

	call	frezMCB

fum30:	mov	al, es:[arena_signature]
	cmp	al, arena_signature_end
	jz	fumX		; 'Z' means this was the last MCB... that's it.

	NextMCB	es, ax		; Go on forward.
	jmp short fum10

fumX:	popreg	<es, dx, cx, ax>
	ret
FreezeUM	endp

; -----------------------------------------------------------------------------
;*** UmbTest - returns with carry set if UMBs are not available, else CF==false
; -----------------------------------------------------------------------------
; ENTRY:    None
; EXIT:     Carry is clear if UMBs are available, or set if they are not
; ERROR:    None
; USES:     CF (AX,BX,DS,ES pushed 'cause they're used by others)
; -----------------------------------------------------------------------------

	public	UmbTest

UmbTest	proc	near
	pushreg	<ax, bx, ds, es>

	call	fm_link			; Link in UMBs (if not already linked)
	call	WalkMem			; Check to see if they're really linked
	pushf				; And remember what we found out
	call	fm_unlink		; Unlink UMBs (if WE have linked 'em)
	popf				; And restore what we found out.

	popreg	<es, ds, bx, ax>
	ret
UmbTest	endp

; -----------------------------------------------------------------------------
;*** WalkMem - travels memory chain and returns carry clear iff UMBs are linked
; -----------------------------------------------------------------------------
; ENTRY:    None
; EXIT:     Carry SET if MCB chain stops before 9FFF, CLEAR if stops >= 9FFF.
; ERROR:    None
; USES:     Flags
; -----------------------------------------------------------------------------

WalkMem	proc	near
	pushreg	<ax, bx, es>

	mov	ah, DOS_GET_DOS_LISTS	; Call int 21h, function 52h...
	int	21h

	mov	ax, es:[bx -2]
	mov	es, ax

; ------------------------------
; UM10: ES = Current MCB pointer
; ------------------------------

um10:	mov	al, es:[arena_signature]
	cmp	al, arena_signature_end
	jz	um20			; If signature == 'Z', hay no more.

	NextMCB	es, bx			; Move to the next MCB
	jmp short um10			; And restart the loop.

um20:	mov	ax, es
	cmp	ax, 9FFFh		; This sets CF iff ax < 9FFF.

	popreg	<es, bx, ax>
	ret
WalkMem	endp

; -----------------------------------------------------------------------------
;*** hl_unlink - unlinks UMBs if fm_umb is set to 0; restores strategy too
; -----------------------------------------------------------------------------
; ENTRY:    fm_umb == 1 : leave linked, else unlink
; EXIT:     None
; ERROR:    None
; USES:     AX, BX
; -----------------------------------------------------------------------------

hl_unlink	proc	near
	xor	bh, bh
	getdata	bl, fm_umb		; Restore original link-state
	mov	ax, DOS_SET_UMBLINK
	int	21h

ifdef HV_LoadHigh
	xor	bh, bh
	getdata	bl, fm_strat		; Restore original mem-alloc strategy

	mov	ax, DOS_SET_STRATEGY
	int	21h
endif
	ret
hl_unlink	endp

