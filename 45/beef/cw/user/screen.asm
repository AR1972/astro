;*
;*	COW : Character Oriented Windows
;*
;*	screen.asm : installable screen drivers

	title	screen - Screen control code for CW

.xlist
	include	user.inc
	include uisa.inc		;* for isa's
	include	screen.inc		;* screen stuff

	include	inscr.inc
	include	indrv.inc

ifdef	KANJI
	include kanji.inc
endif	;KANJI

.list

;----------------------------------------------------------------------------
;
;	* Video Support Macros *

;********** PrepareDraw **********
;*	entry : axPos = ax position
;*		ayPos = ay position
;*	* Calculate the screen position of the character specified
;*	* set ayDrawing to the row being drawn, clear fMouseLockedOut first
;*	* set axDrawFirst
;*	exit : ES:DI = address of character (lpch)
;*	* TRASHES AX,BX

PrepareDraw MACRO axPos,ayPos
	mov	al,ayPos
	mov	ayDrawing,al			;* set drawing location
	mul	axMac
	mov	bl,axPos
	mov	axDrawFirst,bl
	xor	bh,bh
	add	ax,bx
	shl	ax,1		; ax = (ayPos * axMac + ayPos) * 2
ifdef	DEBUG
	cmp	instCur.psPrimInst,0	;* no buffer! (FAllocInstBuffers)
	jne	@F
	cCall	CowAssertFailed
	DB	"no primary buffer for current mode$"
@@:
endif	;DEBUG
	mov	es,instCur.psPrimInst
	mov	di,ax		;* ES:DI => primary buffer
ENDM

;*****************************************************************************

ifdef	DEBPUB	;* Debugging Publics
	PUBLIC	DoDraw
endif

;*****************************************************************************

sBegin	DATA
	assumes DS,DGROUP

;* PUBLIC
externW <rgsa>			;* array of SA's
externB <inch, insj>

IFNDEF	CBOX
externB	<boxSingle, boxDouble, chShadow>
endif	; !CBOX

ifdef	WINDOW_OVERLAP
externW <psOverlap>		;* windows for overlap check
externW <pwndCur>		;* current drawing window
externB <boxActiveWindowOut>
externB <boxInactiveWindowOut>
externB <boxActiveWindowIn>
externB <boxInactiveWindowIn>
endif	;WINDOW_OVERLAP

;* PRIVATE
externB <ayMouse>		;* current mouse position


globalB	ayDrawing,ayNil		;* current drawing ay
globalB	axDrawFirst,ayNil	;* start drawing ax

ifdef	SCREEN_FFONT
externW	<mpisaffont>		;* ffont values
endif	;SCREEN_FFONT

ifdef	BUILTIN_SNOW
globalB fWaitRetrace,0		;* snow protect for twin compatibility,QC only
endif	;BUILTIN_SNOW

sEnd	DATA

;----------------------------------------------------------------------------

sBegin	BSS
	assumes DS,DGROUP

globalB instCur,<cbInstMin DUP (?)>	; data for installable drivers.

globalW	fRestoreDbcs,0		;* => restore DBCS characters
				;* stays 0 for non-KANJI

ifdef	SCREEN_FFONT
globalW	fFontAvailable,0	;* => ffont support allowed
endif	;SCREEN_FFONT

globalB	axMac,0			; Maximum column number
globalB	ayMac,0			; Maximum row number (start at 0)
globalW	axMacTimes2,0		; for row modulus - WORD !!!

globalB	fMonochrome,0		;* Is b/w display

;*	* Variable for implementing mouse lockout (see kernel/mouse.asm)
globalB	fMouseLockedOut,0	;* set by mouse handler

globalB cHoldUpdate,0		;* used by BeginDraw EndDraw

;* PRIVATE *
globalW	fColorModeOld,0		;* last color mode

globalW	offDrawFirst,0		;* offset where drawing started
staticW	ffontCur,0		;* current ffont value

;*	* KLUDGE for clear screen !!
globalB	caOld,0			;* old screen attribute
	DB	0

sEnd	BSS

;*****************************************************************************


sBegin	SCREEN
	assumes CS,SCREEN
	assumes DS,DGROUP
	assumes SS,DGROUP

;*	* High level routines

;********** FillArc **********
;*	entry : axLeft, ayTop, axRight, ayBottom : rectangle to fill
;*		chFill	: fill character (CHAR -- may be DBCS character)
;*		diCur	: draw info (HIBYTE = dm, LOBYTE = isa).
;*	* apply draw mode & data to specified rectangle
;*		D flag MUST BE CLEARED !!!
;*	exit : n/a

cPrivate FillArc,<ATOMIC>,<SI,DI>		;* PRIVATE
	parmB  axLeft
	parmB  ayTop 
	parmB  axRight 
	parmB  ayBottom			;* also day
ifdef	KANJI
	parmW  chFill			;* CHAR type
ELSE
	parmB  chFill
endif	;!KANJI
	parmW  diCur			;* draw info

	LocalW daxMax			;* line wrap value

cBegin	FillArc

	StartPublic
	AssertUp

;*	* Find row count
	mov	al,ayTop
	sub	ayBottom,al		;* day
ifdef	KANJI
	jz	J_fill_end
else	; KANJI
	jz	fill_end		;* no rows to fill
endif	; KANJI

;*	* Find character count
	mov	cl,axRight
	sub	cl,axLeft
ifdef	KANJI
	jnz	@F
J_fill_end:
	jmp	fill_end
@@:
else	; KANJI
	jz	fill_end		;* no width to fill
endif	; KANJI
	xor	ch,ch

;*	* Prepare for the fill
	PrepareDraw axLeft,ayTop	;* sets ES:DI
	mov	dx,screenOffset mpdmnpfnDrawFR	;* normal fill row
ifdef	KANJI
	mov	ax,chFill		;* fill character
	JmpNotDbc fill_normal
;*	* fill as double byte character
	and	cx,not 1		;* force even
	mov	dx,screenOffset mpdmnpfnDrawFR_DBCS	;* DBCS fill row
fill_normal:
ELSE
	mov	al,chFill		;* fill character
endif
	mov	si,ax			;* SI = char save value

;*	* SI = character (AL for non KANJI), DX = dispatch table, CX = count
fill_row_loop:
;*	* Perform Draw
	mov	bx,diCur
	Save	<dx,cx,di>			;* cx = dax, di + start
	cCall	DoDraw			;* fill it in
	inc	ayDrawing		;* next row
	add	di,axMacTimes2		;* to next row
	dec	ayBottom		;* day
	jnz	fill_row_loop

	cCall	FinishDraw

fill_end:
	AssertUp
	StopPublic

cEnd	FillArc



;********** TextOutAbs **********
;*	entry : axLeft, ayTop : start position
;*		pch	: pointer to string data
;*		cch	: # of characters
;*		diCur	: draw info (HIBYTE = dm, LOBYTE = isa).
;*	* apply draw mode & string data to 1 row of text
;*		D flag MUST BE CLEARED !!!
;*	exit : n/a

cPrivate TextOutAbs,<ATOMIC>,<SI,DI>
	parmB  axLeft
	parmB  ayTop
	parmDP pch  
	parmW  cch
	parmW  diCur			;* draw mode + isa
  
cBegin	TextOutAbs

	StartPublic

	AssertUp

;*	* Prepare for the fill
	PrepareDraw axLeft,ayTop	;* sets ES:DI
	mov	si,pch			;* array

	mov	cx,cch

	mov	dx,screenOffset mpdmnpfnDrawTO
	mov	bx,diCur
	cCall	DoDraw			;* perform Text Out

	cCall	FinishDraw

textout_end:
	AssertUp

	StopPublic

cEnd	TextOutAbs



;********** CharOutAbs **********
;*	entry : axCur, ayCur : start position
;*		chPut	: character to put (CHAR)
;*		diCur	: draw info (HIBYTE = dm, LOBYTE = isa).
;*	* TextOutAbs for a single character
;*		D flag MUST BE CLEARED !!!
;*	exit : n/a

cPrivate CharOutAbs,<ATOMIC>,<SI,DI>
	parmB axCur
	parmB ayCur
	parmW chPut		;* could be double byte
	parmW diCur
cBegin	CharOutAbs

	StartPublic

	AssertUp

	PrepareDraw axCur,ayCur		;* sets ES:DI
	lea	si,chPut

	mov	cx,1			;* 1 character TextOut
ifdef	KANJI
	mov	al,ds:[si]		;* get first byte
	JmpNotDbc single_char_out
	inc	cx
single_char_out:
endif	;KANJI

	mov	dx,screenOffset mpdmnpfnDrawTO
	mov	bx,diCur
	cCall	DoDraw			;* perform Text Out

	cCall	FinishDraw

put_ch_end:
	AssertUp

	StopPublic

cEnd	CharOutAbs


ifdef	DRAW_MODE_MINIMIZE

;*****************************************************************************

;*	* * * Drawing info tables * * *
;*	* Drawing routine table for TextOut
mpdmnpfnDrawTO:
	DW	TO_dmTFB		;* Normal
	DW	TO_dmT			;* Text only
	DW	InvalidMode		;* attribute only for TextOut => bogus
	DW	InvalidMode		;* foreground only
	DW	InvalidMode		;* background only
	DW	InvalidMode		;* text & foreground
	DW	InvalidMode		;* text & background
	DW	InvalidMode		;* Text Map background
	DW	InvalidMode		;* Text Map foreground
	DW	InvalidMode		;* Map background
	DW	InvalidMode		;* Map foreground
	DW	InvalidMode		;* Map attr: back 2 back, fore 2 fore
	DW	InvalidMode		;* Map attribute to attribute

;*	* Drawing routine table for FillRectangle
mpdmnpfnDrawFR:
	DW	FR_dmTFB		;* Normal
	DW	InvalidMode		;* Text only
	DW	FR_dmFB			;* ca only
	DW	InvalidMode		;* foreground only
	DW	InvalidMode		;* background only
	DW	InvalidMode		;* text & foreground
	DW	InvalidMode		;* text & background
	DW	InvalidMode		;* Text Map background
	DW	InvalidMode		;* Text Map foreground
	DW	InvalidMode		;* Map background
	DW	InvalidMode		;* Map foreground
	DW	InvalidMode		;* Map attr: back 2 back, fore 2 fore
	DW	InvalidMode		;* Map attribute to attribute

ifdef	KANJI
;*	* Drawing routine table for FillRectangle for DBCS fill
;*	* (attribute only functions stay the same)
mpdmnpfnDrawFR_DBCS:
	DW	FR_dmTFB_DBCS		;* Normal
	DW	InvalidMOde		;* Text only
	DW	FR_dmFB			;* ca only (same as normal)
	DW	InvalidMode		;* foreground only
	DW	InvalidMode		;* background only
	DW	InvalidMode		;* text & foreground
	DW	InvalidMode		;* text & background
	DW	InvalidMode		;* Text Map background
	DW	InvalidMode		;* Text Map foreground
	DW	InvalidMode		;* Map background
	DW	InvalidMode		;* Map foreground
	DW	InvalidMode		;* Map attr: back 2 back, fore 2 fore
	DW	InvalidMode		;* Map attribute to attribute
endif	;KANJI

else	; !DRAW_MODE_MINIMIZE

;*****************************************************************************

;*	* * * Drawing info tables * * *
;*	* Drawing routine table for TextOut
mpdmnpfnDrawTO:
	DW	TO_dmTFB		;* Normal
	DW	TO_dmT			;* Text only
	DW	InvalidMode		;* attribute only for TextOut => bogus
	DW	TO_dmF			;* foreground only
	DW	TO_dmB			;* background only
	DW	TO_dmTF			;* text & foreground
	DW	TO_dmTB			;* text & background
	DW	TO_dmTMb		;* Text Map background
	DW	TO_dmTMf		;* Text Map foreground
	DW	TO_dmMb			;* Map background
	DW	TO_dmMf			;* Map foreground
	DW	TO_dmMfb		;* Map attr: back 2 back, fore 2 fore
	DW	TO_dmMAttr		;* Map attribute to attribute

;*	* Drawing routine table for FillRectangle
mpdmnpfnDrawFR:
	DW	FR_dmTFB		;* Normal
	DW	FR_dmT			;* Text only
	DW	FR_dmFB			;* ca only
	DW	FR_dmF			;* foreground only
	DW	FR_dmB			;* background only
	DW	FR_dmTF			;* text & foreground
	DW	FR_dmTB			;* text & background
	DW	FR_dmTMb		;* Text Map background
	DW	FR_dmTMf		;* Text Map foreground
	DW	FR_dmMb			;* Map background
	DW	FR_dmMf			;* Map foreground
	DW	FR_dmMfb		;* Map attr: back 2 back, fore 2 fore
	DW	FR_dmMAttr		;* Map attribute to attribute

ifdef	KANJI
;*	* Drawing routine table for FillRectangle for DBCS fill
;*	* (attribute only functions stay the same)
mpdmnpfnDrawFR_DBCS:
	DW	FR_dmTFB_DBCS		;* Normal
	DW	FR_dmT_DBCS		;* Text only
	DW	FR_dmFB			;* ca only (same as normal)
	DW	FR_dmF			;* foreground only
	DW	FR_dmB			;* background only
	DW	FR_dmTF_DBCS		;* text & foreground
	DW	FR_dmTB_DBCS		;* text & background
	DW	FR_dmTMb_DBCS		;* Text Map background
	DW	FR_dmTMf_DBCS		;* Text Map foreground
	DW	FR_dmMb			;* Map background
	DW	FR_dmMf			;* Map foreground
	DW	FR_dmMfb		;* Map attr: back 2 back, fore 2 fore
	DW	FR_dmMAttr		;* Map attribute to attribute.
endif	;KANJI

endif	; !DRAW_MODE_MINIMIZE

;*****************************************************************************

;********** DoDraw **********
;*	entry : ES:DI => screen location
;*		DX = pointer to table (mpdmnpfnDrawXX)
;*		CX = # of screen locations
;*		BX = di (drawing info) : BH = dm, BL = isa
;*		DS:SI => string (Text Out variant)
;*		 (DS => default DS (==SS))
;*		non-kanji LOBYTE(SI) = fill character (Fill Rectangle variant)
;*		kanji SI = fill double byte character (double byte Fill variant)
;*		D flag cleared !!
;*		ayDrawing and axDrawFirst set up !
;*		for overlap pwndCur is the current window (0=>draw always)
;*	* Prepare & call draw routine
;*	exit : ES:DI => after last character/attrib munged over
;*	TRASHES SI !!!!

do_draw_end1:
	jmp	do_draw_end

cProc	DoDraw,<NEAR,ATOMIC>,<BP>
cBegin	DoDraw

	AssertUp
	Assert <caSa EQ rgcaFill+1>		;* caSa MUST be in high byte

	Save	<es, bx, cx, dx>
	cCall	insj.lpfnPrepUpdateCsdInsj, <ayDrawing, axDrawFirst, cx, di, fRestoreDbcs>

	jcxz	do_draw_end1			;* trivial case

ifdef	MOUSE_TEXT
;*	* If ayDrawing == ayMouse{new} then kill mouse
	mov	al,ayMouse
	cmp	al,ayDrawing
	jne	not_drawing_over_mouse
else	; MOUSE_TEXT
;*	* If ABS(ayDrawing - ayMouse{new}) = FE,FF,0,1,2 then kill mouse
	mov	al,ayMouse
	sub	al,ayDrawing
	add	al,2				;* 0, 1, 2, 3, 4 => kill it
	cmp	al,5
	jae	not_drawing_over_mouse
endif	; MOUSE_TEXT
;*	* We may be drawing over mouse
	mov	al,0ffh				;* set value
	xchg	fMouseLockedOut,al		;* set flag, get old contents
	or	al,al
	jnz	already_locked_out
;*	* Turn mouse off
	xor	ax,ax				;* turn off
	Save	<bx,cx,dx>
	cCall	FEnableMouse,<ax>
	mov	fMouseLockedOut,al		;* save old state
already_locked_out:

not_drawing_over_mouse:

	Assert	<SIZE SA EQ 2>
	mov	ah,bh				;* ah = dm
	xor	bh,bh
	shl	bx,1				;* bx = isa * sizeof(SA)
ifdef	SCREEN_FFONT
	Assert	<SIZE SA EQ 2>			;* sizeof(SA) == sizeof(FFONT)
	mov	bp,[bx]+dataOffset mpisaffont
	mov	ffontCur,bp
endif	;SCREEN_FFONT
	mov	bp,[bx].rgcaFill+dataOffset rgsa;* bp = rgcaFill, bp(high) = caSa
	mov	bl,ah				;* ah = bl = dm
	shl	bx,1				;* bl = dm * sizeof(PROC NEAR)
						;*  lose the fdmKeepFfont bit
	xor	bh,bh
	add	bx,dx				;* bx = &mpdmnpfnDrawXX[dm]
	mov	bx,cs:[bx]			;* table look up procedure

ifdef	SCREEN_FFONT
;*	* ah = dm
	or	ah,ah				;* fdmKeepFfont bit set ?
	js	no_draw_ffont			;* keep the old FFONT
	cmp	fFontAvailable,0
	jz	no_draw_ffont
;*	* DI => start offset, CX = # of chars
	push	es
	push	di
	push	cx
	AssertNE instCur.psSecInst,0
	mov	es,instCur.psSecInst
	mov	ax,ffontCur
	rep stosw				;* fill in FFONT values
	pop	cx
	pop	di
	pop	es
no_draw_ffont:
endif	;SCREEN_FFONT

	push	di				;* start buffer offset
	push	cx				;* dax
	call	bx				;* call routine
	pop	cx
	pop	bx

;*	* inform driver that the line is done
	Save	<es>
	cCall	insj.lpfnDoUpdateCsdInsj, <ayDrawing, axDrawFirst, cx, bx, fRestoreDbcs>
					;* (ay, axFirst, dax, offFirst)
					;* note -- offset in primary!
do_draw_end:

cEnd	DoDraw


ifdef	KANJI
;*********** FlushDraw **********
;*	entry: n/a
;*	* update must be drawn NOW
;*	*  calls FinishDraw, sets hold update counter to zero
;*	exit: n/a

cPrivate FlushDraw, <ATOMIC>
cBegin	nogen

	mov	cHoldUpdate,0
	jmp	short finished_drawing

cEnd	nogen
endif	; KANJI


;*********** EndDraw **********
;*	entry: n/a
;*	* decrements the hold update counter which defers the call
;*	*  to DoUpdate.  If necessary, calls FinishDraw.
;*	exit: n/a

cPrivate EndDraw, <ATOMIC>
cBegin	EndDraw

ifdef	KANJI
	cmp	cHoldUpdate,0		;* FlushDraw may occur at any level of
	je	finished_drawing	;*  nesting, stop cHoldUpdate < 0
endif	; KANJI

	dec	cHoldUpdate
	jnz	not_finished_drawing
finished_drawing:			;* from FlushDraw aswell!!
	cCall	FinishDraw
not_finished_drawing:

cEnd	EndDraw

;*********** FinishDraw **********
;*	entry: n/a
;*	* drawing is done for now
;*	* inform the screen driver
;*	*  then turn mouse back on (if it was turned off)
;*	exit: n/a

cProc	FinishDraw, <NEAR, ATOMIC>
cBegin	FinishDraw

	test	cHoldUpdate,0ffh
	jnz	mouse_ok

	cCall	insj.lpfnDoneUpdateCsdInsj		;* ()

	mov	ayDrawing,ayNil			;* clear ayDrawing first
	test	fMouseLockedOut,0ffh
	jz	mouse_ok			;* it was not turned off
	cCall	FEnableMouse,<sp>

ifdef	DEBUG
	or	ax,ax				;* MUST have been turned off
	jz	clear_locked_out_flag
	cCall	CowAssertFailed
	DB	"mouse$"
endif	;DEBUG

clear_locked_out_flag:
	mov	fMouseLockedOut,al

mouse_ok:

cEnd	FinishDraw

;*****************************************************************************

;********** Draw Routines **********
;*	entry : ES:DI => screen (character, attribute at +1)
;*		CX = # of operations
;*		AL = new character (Fill Only)
;*		DS:SI => string (TextOut Only)
;*		DX = CGA video status port (3DAh)
;*		BP = rgcaFill (for map modes only) or high byte = caSa
;*		D flag cleared
;*	available registers :
;*		BX = work register
;*		AL, AH = work
;*	exit : n/a
;*	* NOTE: for overlapping windows, check that psOverlap:DI is pointing to
;*	*  our window (pwndDraw).

DRAW_ROUTINES	PROC NEAR

;*	* TextOut Variants
TO_dmTFB:	;* Text + attributes
	lodsb
	CheckWnd TO_dmTFB_next
	mov	bx,bp				;* set bh = attribute
	mov	bl,al				;* set bl = character
ifdef	BUILTIN_SNOW
	mov	dx,3DAh			;* CGA video status port
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	ax,bx
	stosw					;* only time for 1 store
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
TO_dmTFB_next:
	loop	TO_dmTFB
	ret

TO_dmT:		;* Text only
	lodsb					;* get byte
	CheckWnd TO_dmT_next
	mov	bl,al
ifdef	BUILTIN_SNOW
	mov	dx,3DAh
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	al,bl
	stosb
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
	inc	di
TO_dmT_next:
	loop	TO_dmT
	ret

ifndef	DRAW_MODE_MINIMIZE

TO_dmTF:	;* leave background alone
	mov	bh,ah
l1_tf:	lodsb
	CheckWnd TO_dmTF_next
	mov	bx,bp				;* bh = attribute
	mov	bl,al
ifdef	BUILTIN_SNOW
	mov	dx,3DAh
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	ah,es:[di+1]
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
	and	ah,0F0H
	or	bh,ah
ifdef	BUILTIN_SNOW
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	ax,bx
	stosw
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
TO_dmTF_next:
	loop	l1_tf
	ret

TO_dmB:		;* background only
	CheckWnd TO_dmB_next
	mov	bx,bp				; bh has new bg in hi nybble
	and	bh,0F0H				; change background only
	inc	di
ifdef	BUILTIN_SNOW
	mov	dx,3DAh
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	al,es:[di]			; al = old attribute
	and	al,0Fh				; al = old foreground only
	or	al,bh				; al = old fore & new back
	stosb					;* only time for 1 store
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
TO_dmB_next:
	loop	TO_dmB
	ret

TO_dmTMb:	;* character + map background
	CheckWnd TO_dmTMb_next
ifdef	BUILTIN_SNOW
	mov	dx,3DAh
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	bl,es:[di+1]
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
	lodsb					;* get character in AL
	and	bx,00F0H
	shr	bx,1
	shr	bx,1
	shr	bx,1
	shr	bx,1
	add	bx,bp
	mov	ah,ds:[bx]			;* look up new attribute
	mov	bx,ax
ifdef	BUILTIN_SNOW
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	ax,bx
	stosw
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
TO_dmTMb_next:
	loop	TO_dmTMb
	ret

FR_dmMf:
TO_dmMf:		;* map foreground
	CheckWnd FR_dmMf_next
	inc	di
ifdef	BUILTIN_SNOW
	mov	dx,3DAh
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	bl,es:[di]
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
	and	bx,000FH
	add	bx,bp
	mov	bl,ds:[bx]			;* look up new attribute
ifdef	BUILTIN_SNOW
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	al,bl
	stosb
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
FR_dmMf_next:
	loop	TO_dmMf
	ret


TO_dmMfb:	;* Map attribute: old background maps to new background, 
		;                 old foreground maps to new foreground.
		;  Note: the tables for these guys are 16 + 16 bytes each.

	inc	di
ifdef	BUILTIN_SNOW
	mov	dx,3DAh
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	bl,es:[di]			;* get old attribute
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
	and	bx,000FH			;* mask to foreground
	add	bx,bp
	mov	bl,ds:[bx]			;* look up new foreground
	push	bx				;* save BL = new foreground

ifdef	BUILTIN_SNOW
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	bl,es:[di]			;* get old attribute
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
	and	bx,00F0H			;* mask to background
	shr	bx,1
	shr	bx,1
	shr	bx,1
	shr	bx,1
	add	bx,bp				;* bx = &rgca[background]
	add	bx,16				;* bg's follow fg's in array.

ifdef	BUILTIN_SNOW
	StartDrawCrit
endif	;BUILTIN_SNOW
	pop	ax				;* restore AL = new fground
	or	al,ds:[bx]			;* Merge in new background
	stosb
ifdef	BUILTIN_SNOW
	EndDrawCrit
	dec	cx
	or	cx,cx
	jz	@F
	jmp	TO_dmMfb
@@:
ELSE	;NOT DEFINED BUILTIN_SNOW
	loop	TO_dmMfb
endif	;BUILTIN_SNOW
	ret

TO_dmMAttr:	;* Map entire attribute: old attr maps to new attr.
		;  Note that the tables for these guys are 256 bytes each.

	inc	di
ifdef	BUILTIN_SNOW
	mov	dx,3DAh
	StartDrawCrit
endif	;BUILTIN_SNOW
	xor	bx,bx				; BH = 0 for indexing.
	mov	bl,es:[di]			;* get old attribute
	add	bx,bp
	mov	al,ds:[bx]			;* look up new attribute
	stosb
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
	loop	TO_dmMAttr
	ret

endif	; !DRAW_MODE_MINIMIZE


;*****************************************************************************
;*	* Fill Rectangle Variants
FR_dmTFB:	;* normal
	mov	ax,si				;* al = character
	mov	bx,bp				;* bh = attribute
	mov	bl,al				;* bx = ca:ch
FR_dmTFB_1:
	CheckWnd FR_dmTFB_next
ifdef	BUILTIN_SNOW
	mov	dx,3DAh
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	ax,bx
	stosw					;* store one
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
FR_dmTFB_next:
	loop	FR_dmTFB_1
	ret

ifndef	DRAW_MODE_MINIMIZE

FR_dmT:		;* text only
	mov	bx,si				;* bl = character
	jmp	short FR_dmFB_1

endif	; !DRAW_MODE_MINIMIZE

FR_dmFB:	;* ca only
	mov	bx,bp				;* bh = attribute
	mov	bl,bh				;* bl = value to store
	inc	di				;* point to ca
;*	* this breaks the window test!!!!

FR_dmFB_1:
	CheckWnd FR_dmFB_next
ifdef	BUILTIN_SNOW
	mov	dx,3DAh
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	al,bl
	stosb					;* store one
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
	inc	di				;* to next ca
FR_dmFB_next:
	loop	FR_dmFB_1
	ret

ifndef	DRAW_MODE_MINIMIZE

FR_dmB:	;* fill background only
	mov	bx,bp				;* bh = attribute
	and	bh,0F0H				;* change background only
	mov	bl,00FH				;* keep mask
	jmp	short FR_dmF_1

FR_dmF:	;* fill foreground only
	mov	bx,bp				;* bh = attribute
	and	bh,00FH				;* change foreground only
	mov	bl,0F0H				;* keep mask
FR_dmF_1:
	inc	di				;* point to ca
;*	* this breaks the window test!!!!
	push	bp				;* bp used for temp
FR_dmF_2:
	CheckWnd FR_dmF_next
ifdef	BUILTIN_SNOW
	mov	dx,3DAh
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	al,es:[di]			;* get old background
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
	and	al,bl				;* keep half
	or	al,bh				;* replace half
	mov	bp,ax
ifdef	BUILTIN_SNOW
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	ax,bp
	stosb
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
	inc	di
FR_dmF_next:
	loop	FR_dmF_2
	pop	bp				;* restored trashed bp
	ret

FR_dmTMb:	;* character + map background
	CheckWnd FR_dmTMb_next
ifdef	BUILTIN_SNOW
	mov	dx,3DAh
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	bl,es:[di+1]			;* get old attribute
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
	and	bx,00F0H			;* mask background
	shr	bx,1
	shr	bx,1
	shr	bx,1
	shr	bx,1
	add	bx,bp				;* bx = &rgca[background]
	mov	ax,si				;* load character (into al)
	mov	ah,ds:[bx]			;* look up new attribute
	mov	bx,ax
ifdef	BUILTIN_SNOW
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	ax,bx
	stosw
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
FR_dmTMb_next:
	loop	FR_dmTMb
	ret


FR_dmMb:	;* map background only
	CheckWnd FR_dmMb_next
	inc	di				;* point to attribute
ifdef	BUILTIN_SNOW
	mov	dx,3DAh
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	bl,es:[di]			;* get old attribute
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
	and	bx,00F0H			;* mask background
	shr	bx,1
	shr	bx,1
	shr	bx,1
	shr	bx,1
	add	bx,bp				;* bx = &rgca[background]
	mov	bl,ds:[bx]			;* look up new attribute
ifdef	BUILTIN_SNOW
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	al,bl
	stosb
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
FR_dmMb_next:
	loop	FR_dmMb
	ret

endif	; !DRAW_MODE_MINIMIZE


;*****************************************************************************
;*	* DBCS Fill Rectangle Variants (text modify only)
;*	* all entry points should start with "shr cx,1"

ifdef	KANJI

FR_dmTFB_DBCS:	;* normal
	shr	cx,1
	mov	dx,si				;* dx = DB character (dl first)
	mov	bx,bp				;* bh = attribute
	mov	bl,dl				;* BX = first ca:ch
	mov	dl,dh
	mov	dh,bh				;* DX = second ca:ch
FR_dmTFB_1_DBCS:
	CheckWnd FR_dmTFB_next_DBCS
ifdef	BUILTIN_SNOW
	push	cx
	mov	cx,dx
	mov	dx,3DAh
	StartDrawCrit
	mov	ax,bx
	stosw					;* store first character
	mov	ax,cx
	stosw					;* store second character
	EndDrawCrit
	pop	cx
ELSE	; !BUILTIN_SNOW
	mov	ax,bx
	stosw					;* store first character
	mov	ax,dx
	stosw					;* store second character
endif	;BUILTIN_SNOW

FR_dmTFB_next_DBCS:
	loop	FR_dmTFB_1_DBCS
	ret

ifndef	DRAW_MODE_MINIMIZE

FR_dmT_DBCS:		;* text only
	shr	cx,1
	mov	bx,si				;* bl = 1st, bh = 2nd
FR_dmFB_1_DBCS:
	CheckWnd FR_dmFB_next_DBCS
ifdef	BUILTIN_SNOW
	mov	dx,3DAh
	StartDrawCrit
endif	;BUILTIN_SNOW
	mov	al,bl
	stosb					;* store one
	inc	di				;* to next ca
	mov	al,bh
	stosb					;* store one
ifdef	BUILTIN_SNOW
	EndDrawCrit
endif	;BUILTIN_SNOW
	inc	di				;* to next ca
FR_dmFB_next_DBCS:
	loop	FR_dmFB_1_DBCS
	ret

endif	; !DRAW_MODE_MINIMIZE

endif	;KANJI

;*****************************************************************************

ifndef	DRAW_MODE_MINIMIZE

;*	???? UNIMPLEMENTED !!!!!!
TO_dmF:
TO_dmTB:
TO_dmTMf:
TO_dmMb:
FR_dmTF:
FR_dmTB:
FR_dmTMf:
FR_dmMfb:
FR_dmMAttr:
ifdef	KANJI
FR_dmTF_DBCS:
FR_dmTB_DBCS:
FR_dmTMf_DBCS:
FR_dmTMb_DBCS:
endif	;KANJI

;??????

endif	; !DRAW_MODE_MINIMIZE

InvalidMode:
ifdef	DEBUG
	cCall	CowAssertFailed
	DB	"Invalid draw mode $"
endif	;DEBUG
	ret

DRAW_ROUTINES	ENDP		;* all near

;*****************************************************************************

;********** EndScreen **********
;*	entry:	fClear => clear screen
;*	* Exit procedure - clear the screen
;*	exit:	n/a

cPublic	EndScreen,<PUBLIC, ATOMIC>
	parmW fClear
cBegin	EndScreen

	StartPublic

	mov	cx,fClear
	jcxz	dont_clear

ifdef	REVIEW
	mov	al,caOld
ELSE
	mov	al,7
endif	;!REVIEW
	mov	ah,al				;* duplicate for MONO
	xchg	ax,[rgsa]			;* change isa == 0
	push	ax				;* save old

	xor	ax,ax
	mov	bx,' '
	xor	cx,cx
	xor	dx,dx
	mov	cl,axMac
	mov	dl,ayMac
ifdef	WINDOW_OVERLAP
	xor	ax,ax
	mov	pwndCur,ax
endif	;WINDOW_OVERLAP
	cCall	FillArc,<ax,ax,cx,dx,bx,ax>	;* fill all with ' ' isaDefault

	pop	word ptr [rgsa]			;* restore old isaDefault

	xor	ax,ax
	cCall	insj.lpfnMoveHwCursCsdInsj,<ax,ax,sp>	;* top of screen & on

dont_clear:
;*	* kill the screen driver
	cCall	insj.lpfnTermCsdInsj		;* ()

	StopPublic

cEnd	EndScreen

;*****************************************************************************


;********** ImodeGuessCurrent **********
;*	entry: n/a
;*	* call driver to guess current mode
;*	exit:	AX = imode (or -1 (imodeUnkown) if not known)

labelFP	<PUBLIC, ImodeGuessCurrent>
	jmp	insj.lpfnImodeGuessCurrentCsdInsj



;********** FQueryInst **********
;*	entry:
;*		pinst : pointer to INST structure to fill
;*		imode : index of mode to test
;*	* Get information about modes available
;*	* just call the INSJ procedure
;*	exit:	AX != 0 => ok (*pinst filled in)
;*		== 0 => error (imode too high or can't query)

IFDEF	DEBUG

cPublic	FQueryInst,<PUBLIC>
	parmW pinst
	parmW imode
cBegin	FQueryInst

	cCall	insj.lpfnFQueryInstCsdInsj,<pinst,imode>

	or	ax,ax					; If bogus mode, 
	jz	fqi_done				;   can't check inst.

	mov	bx,pinst

	test	[bx].finstInst,finstExtendedMono	; If ExtendedMono,
	jz	@F					;   then must have
	test	[bx].finstInst,finstAttrFont		;   AttrFont.
	jz	bust
@@:
	test	[bx].finstInst,finstAttrFont
	jz	fqi_done
	test	[bx].finstInst,finstMonochrome		; If AttrFont, then
	jz	bust					;   must have Mono
	test	[bx].finstInst,finstText		;   and Text.
	jnz	fqi_done
bust:	
	cCall	CowAssertFailed
	DB	"FQueryInst fInst bits not in sync.$"

fqi_done:

cEnd	FQueryInst

ELSE	;!DEBUG

labelFP	<PUBLIC, FQueryInst>
	jmp	insj.lpfnFQueryInstCsdInsj

ENDIF	;!DEBUG



;********** FGetColorPalette **********
;*	entry:	co	color
;*		pcoi	color combination index (returned)
;*		rgcov	RGB palette info (returned)
;*	* get current palette setting for co
;*	exit:	AX = 0 => no color palette available, pcoi, rgcov

labelFP	<PUBLIC, FGetColorPalette>
	jmp	insj.lpfnFGetColorPaletteCsdInsj



;********** SetColorPalette **********
;*	entry:	co	color
;*		coi	color combination index
;*		rgcov	RGB palette info
;*	* set current palette setting for co, does nothing if no color palette
;*	exit:	n/a

labelFP	<PUBLIC, SetColorPalette>
	jmp	insj.lpfnSetColorPaletteCsdInsj



;********** MoveHardwareCursor **********
;*	entry:	axCurs, ayCurs = new absolute cursor position
;*		fOn => whether on or off
;*	exit:	n/a

labelFP	<PUBLIC, MoveHardwareCursor>
	jmp	insj.lpfnMoveHwCursCsdInsj



;********** FQueryInft **********
;*	entry:	pinft, ifont
;*	* get font info
;*	exit:	AX = 0 => no more fonts

labelFP	<PUBLIC, FQueryInft>
	jmp	insj.lpfnFQueryInftCsdInsj


;********** GetCharMap **********
;*	entry:	pinft, ifont
;*	* get font info
;*	exit:	AX = 0 => no more fonts

labelFP	<PUBLIC, GetCharMap>
	jmp	insj.lpfnGetCharMapCsdInsj


ifdef	WINDOW_OVERLAP
;********** FAllocOverlapTable **********
;*	entry:	pfnAlloc => allocation function (supplied by App)
;*		pinst    => current INST infor
;*	* Allocate memory for the overlapping windows table
;*	* Note!! pinst must have been set up already (FQueryInst)
;*	exit:	AX != 0 if successful

cPublic	FAllocOverlapTable, <>, <SI>
	parmDP pinst		;* INST info
	parmD  pfnAlloc2		;* FAR PASCAL routine
cBegin	FAllocOverlapTable

	mov	si,pinst

	mov	al,[si].axMacInst
	mov	ah,[si].ayMacInst
	mul	ah
	mov	bx,ax

	mov	ax,fmemFixed
	Save	<bx>
	cCall	pfnAlloc2, <bx, ax>
	AssertEQ ax,0

	or	dx,dx
	jz	end_alloc_overlap		;* return AX == 0

	mov	psOverlap,dx

	mov	ax,sp				;* success

end_alloc_overlap:

cEnd	FAllocOverlapTable




;********** FreeOverlapTable **********
;*	entry:	pfnFree => free function (supplied by App)
;*	* free video driver buffers
;*	exit:	n/a

cPublic	FreeOverlapTable, <>, <SI>
	parmD  pfnFree2		;* FAR PASCAL routine
cBegin	FreeOverlapTable

	xor	cx,cx
	xchg	cx,psOverlap
	jcxz	done_overlap_free
	xor	ax,ax
	cCall	pfnFree2, <cx, ax>
done_overlap_free:

cEnd	FreeOverlapTable
endif	;*WINDOW_OVERLAP




;********** FAllocInstBuffers **********
;*	entry:	pinstAlloc => INST info to fill
;*		pfnAlloc => allocation function (supplied by App)
;*		fFonts => if secondary buffer wanted
;*	* Allocate memory for video driver buffers
;*	exit:	AX != 0 if successful

cPublic	FAllocInstBuffers, <>, <SI>
	parmDP pinstAlloc
	parmD  pfnAlloc		;* FAR PASCAL routine
	parmW  fFonts
cBegin	FAllocInstBuffers

	StartPublic

	mov	si,pinstAlloc

IFNDEF SCREEN_FFONT
	AssertEQ fFonts,0			;* not allowed
endif	;!SCREEN_FFONT

;*	* determine size of buffer (axMac * ayMac WORDS)
	mov	al,[si].axMacInst
	mov	ah,[si].ayMacInst
	mul	ah
	mov	bx,ax

	xor	ax,ax
	mov	[si].bits0Inst,ax		;* clear bits
;*	* first check to see if primary buffer needs allocation
	cmp	[si].psPrimInst,ax		;* == 0 ?
	jne	done_prim_alloc

	mov	ax,fmemFixed
	Save	<bx>
	cCall	pfnAlloc, <bx, ax>
	AssertEQ ax,0

	or	dx,dx
	jz	end_alloc			;* return AX == 0

	mov	[si].psPrimInst,dx
	or	[si].bits0Inst,MASK fAllocPrimInst
done_prim_alloc:

ifdef	SCREEN_FFONT
;*	* allocate secondary buffer if needed (bx = size of buffer)
	mov	cx,fFonts
	jcxz	no_sec_buffer
	test	[si].finstInst,finstFont
	jz	no_sec_buffer
	mov	ax,fmemFixed
	cCall	pfnAlloc, <bx, ax>
	AssertEQ ax,0

	or	dx,dx
	jz	end_alloc			;* return AX == 0

	mov	[si].psSecInst,dx
no_sec_buffer:
endif	;SCREEN_FFONT

;*	* allocate any needed driver specific memory
	mov	cx,[si].cwExtraInst
	jcxz	done_extra_alloc

	mov	ax,fmemFixed
	cCall	pfnAlloc, <cx, ax>
	AssertEQ ax,0

	or	dx,dx
	jz	end_alloc			;* return AX == 0

	mov	[si].psExtraInst,dx
done_extra_alloc:

	mov	ax,sp				;* success

end_alloc:

	StopPublic

cEnd	FAllocInstBuffers



;********** FreeInstBuffers **********
;*	entry:	pinstFree => INST info to free
;*		pfnFree => free function (supplied by App)
;*	* free video driver buffers
;*	exit:	n/a

cPublic	FreeInstBuffers, <>, <SI>
	parmDP pinstFree
	parmD  pfnFree		;* FAR PASCAL routine
cBegin	FreeInstBuffers

	StartPublic

	mov	si,pinstFree

	test	[si].bits0Inst,MASK fAllocPrimInst
	jz	done_prim_free

	xor	ax,ax
	cCall	pfnFree, <[si].psPrimInst, ax>

	and	[si].bits0Inst,NOT (MASK fAllocPrimInst)
done_prim_free:

ifdef	SCREEN_FFONT
	xor	cx,cx
	xchg	cx,[si].psSecInst
	jcxz	done_sec_free
	xor	ax,ax
	cCall	pfnFree, <cx, ax>
done_sec_free:
ELSE
	AssertEQ [si].psSecInst,0
endif	;!SCREEN_FFONT

;*	* free driver buffer
	xor	cx,cx
	xchg	cx,[si].psExtraInst
	jcxz	done_extra_free
	xor	ax,ax
	cCall	pfnFree, <cx, ax>

done_extra_free:

	StopPublic

cEnd	FreeInstBuffers



;*****************************************************************************


;********** FInitScreenInternal **********
;*	entry: pinst => INST structure (NULL => re-init previous mode)
;*	* Initialize the screen as needed
;*	* NOTE : this routine can be called ONLY ONCE.
;*	exit: AX != 0 => ok
;*		* NOTE: failure may destroy old instCur

cPublic FInitScreenInternal,<ATOMIC>, <SI, DI>
	parmW pinst
cBegin	FInitScreenInternal

	StartPublic

	mov	bx,dataOffset instCur
	mov	si,pinst
	or	si,si
	jz	use_current
;*	* set new INST (copy into instCur)
	push	ds
	pop	es
	mov	di,bx
	mov	cx,cbInstMin / 2
	rep movsw
use_current:	;* bx => instCur (instCur filled with *pinst)

	mov	ax,ds:[bx].finstInst
	and	ax,finstAvailable
	jnz	dont_end
end_init_jump:
	jmp	end_init			;* variant not available

dont_end:
	mov	ax,dataOffset inch
	Save	<bx>
	cCall	insj.lpfnFInitCsdInsj, <bx, ax>	;* (pinst, pinch)
	or	ax,ax
	jz	end_init_jump

;*	* move info from INCH to other globals, bx => instCur
	mov	al,[bx].axMacInst
	mov	axMac,al
	xor	ah,ah
	shl	ax,1
	mov	axMacTimes2,ax
	mov	al,[bx].ayMacInst
	mov	ayMac,al
	mov	ax,[bx].finstInst
	and	al,finstMonochrome	;* finstMonochrome in lower byte
	mov	fMonochrome,al

IFNDEF	CBOX
;*	* copy information from INCH into boxes
	lea	bx,inch
	push	ds
	pop	es			;* all in default data segment

;*	* copy single box
	lea	si,[bx]._chTopLeftCorner1Inch
	lea	di,boxSingle
	mov	cx,SIZE BOX / 2
	rep movsw
;*	* copy double box
	lea	si,[bx]._chTopLeftCorner2Inch
	lea	di,boxDouble
	mov	cx,SIZE BOX / 2
	rep movsw

	mov	al,[bx]._chShadowInitInch
	mov	chShadow,al

ifdef	WINDOW_OVERLAP
;*	* base window is single box
	lea	si,[bx]._chTopLeftCorner1Inch
	lea	di,boxActiveWindowOut
	mov	cx,SIZE BOX / 2
	rep movsw
	mov	al,[bx]._chCloseInch
	mov	boxActiveWindowOut.chTopLeftBox,al
	mov	al,[bx]._chZoomOutInch
	mov	boxActiveWindowOut.chTopRightBox,al
	mov	al,[bx]._chTopSide2Inch
	mov	boxActiveWindowOut.chTopBox,al
	lea	si,[bx]._chTopLeftCorner1Inch
	lea	di,boxInactiveWindowOut
	mov	cx,SIZE BOX / 2
	rep movsw
	mov	al,[bx]._chCloseInch
	mov	boxInactiveWindowOut.chTopLeftBox,al
	mov	al,[bx]._chZoomOutInch
	mov	boxInactiveWindowOut.chTopRightBox,al

	lea	si,[bx]._chTopLeftCorner1Inch
	lea	di,boxActiveWindowIn
	mov	cx,SIZE BOX / 2
	rep movsw
	mov	al,[bx]._chCloseInch
	mov	boxActiveWindowIn.chTopLeftBox,al
	mov	al,[bx]._chZoomInInch
	mov	boxActiveWindowIn.chTopRightBox,al
	mov	al,[bx]._chTopSide2Inch
	mov	boxActiveWindowIn.chTopBox,al
	lea	si,[bx]._chTopLeftCorner1Inch
	lea	di,boxInactiveWindowIn
	mov	cx,SIZE BOX / 2
	rep movsw
	mov	al,[bx]._chCloseInch
	mov	boxInactiveWindowIn.chTopLeftBox,al
	mov	al,[bx]._chZoomInInch
	mov	boxInactiveWindowIn.chTopRightBox,al
endif	;WINDOW_OVERLAP

endif	; !CBOX

ifdef	SCREEN_FFONT
;*	* set the fFontAvailable flag
	xor	ax,ax			;* assume off
	mov	cx,instCur.finstInst
	test	cx,finstFont
	jz	set_ffont_available	;* not available in this mode
	mov	cx,instCur.psSecInst
	jcxz	set_ffont_available	;* no secondary buffer => forget it
	inc	ax			;* yes, we can use ffonts
set_ffont_available:
	mov	fFontAvailable,ax
endif	;SCREEN_FFONT

	mov	ax,sp			;* success

end_init:	;* ax = return code

	StopPublic

cEnd	FInitScreenInternal



;********** BltArc **********
;*	entry : axSrc, aySrc : upper left of source
;*		axDest, ayDest : upper left of destination
;*		dax, day : shift amount
;*	* Move a rectangle from one portion of the screen to another.
;*	exit : n/a

cPrivate BltArc,<ATOMIC>,<SI,DI>
	parmB axDest
	parmB ayDest
	parmB dax 
	parmB day
	parmB axSrc
	parmB aySrc

	localW fMouseOn				;* FEnableMouse old state
	localW offDestLim				;* last offset written
cBegin	BltArc

	StartPublic

	xor	ax,ax
	cCall	FEnableMouse,<ax>		;* turn mouse off
	mov	fMouseOn,ax

	AssertUp

	mov	al,day
	or	al,al
	jz	blt_end1			;* trivial case (day == 0)

	CalcCoord axDest,ayDest
	mov	di,ax
	mov	offDrawFirst,ax			;* save for driver inform

;*	* Do either a Prep, Do, Done sequence or a fast BltArcCsd call
	test	instCur.finstInst,finstFastScroll
	jnz	skip_prep

;*	* Prepare screen driver for update
	mov	si,word ptr (dax)	;* low byte is all that's interesting
;*	* si = dax, di = offset in Prim buffer
;*	* ax = ayDest, cx = day
	xor	ax,ax
	mov	al,ayDest
	xor	cx,cx
	mov	cl,day
	push	di
loop_prep:
	Save	<ax,cx>
	cCall	insj.lpfnPrepUpdateCsdInsj, <ax, axDest, si, di, fRestoreDbcs>
	add	di,axMacTimes2			;* point to next row
	inc	ax
	loop	loop_prep
	pop	di
skip_prep:

	CalcCoord axSrc,aySrc
	mov	si,ax			; save pointer to source
	mov	dx,axMacTimes2		;* row modulus
	cmp	ax,di
	jge	blt_hilo		; going from high memory to low

; go from low memory to high.  Do backwards Blt  Blt Row by row either 
; fast or slow depending on the retrace parameter; ax has source, di
; destination of top left hand point

	mov	al,day
	dec	al
	mul	axMac
	mov	bl,dax
	xor	bh,bh
	add	ax,bx
	shl	ax,1
	dec	ax			;* back 1 byte (for movsb)

	add	di,ax			; move source to bottom right
	add	si,ax			; move dest to bottom, right 

	neg	dx			;* negate row modulus
	std				; bltting backwards
	jmp	short blt_go

blt_end1:	;* jump extender
	jmp	blt_end

blt_hilo:
;	cld				;* D flag already cleared
blt_go:
;*	* dx = row modulus (+ or - axMac * 2)
	mov	cl,dax			; setup number of columns
	xor	ch,ch
	jcxz	blt_end1			;* trivial case (dax == 0)
	shl	cx,1			;* ccol -> cb
	mov	bx,cx			;* CX = BX = cb

	push	ds
ifdef	DEBUG
	cmp	instCur.psPrimInst,0	;* no buffer! (FAllocInstBuffers)
	jne	@F
	cCall	CowAssertFailed
	DB	"no primary buffer for current mode$"
@@:
endif	;DEBUG
	mov	es,instCur.psPrimInst
	push	es
	pop	ds			;* DS & ES set to video segment

	xor	ax,ax
	mov	al,day
ifdef	SCREEN_FFONT
	Save	<ax,bx,cx,dx,si,di>	;* needed for second call
	cCall	DoBltArc
	pop	ds
	cmp	fFontAvailable,0
	jz	no_blt_ffont

	push	ds
	mov	es,instCur.psSecInst
	push	es
	pop	ds			;* DS & ES set to video segment
endif	;SCREEN_FFONT
	cCall	DoBltArc
	pop	ds
no_blt_ffont:

	cld					;* Clear D flag (convention)

;* Check for fast scrolling in graphics text modes	
	test	instCur.finstInst,finstFastScroll
	jz	no_fast
	cCall	insj.lpfnBltArcCsdInsj, <axDest, ayDest, dax, day, axSrc, aySrc>
	jmp	skip_updatedone
no_fast:
	
;*	* Inform screen driver of the destination area
	mov	si,word ptr (dax)	;* low byte is all that's interesting
	mov	di,offDrawFirst
;*	* si = dax, di = offset in Prim buffer
loop_inform_blt:
	cCall	insj.lpfnDoUpdateCsdInsj, <ayDest, axDest, si, di, fRestoreDbcs>
	add	di,axMacTimes2			;* point to next row
	inc	ayDest
	dec	day
	jnz	loop_inform_blt

;*	* all done, refresh
	cCall	insj.lpfnDoneUpdateCsdInsj		;* ()

blt_end:

	cld					;* Clear D flag (convention)

skip_updatedone:

	cCall	FEnableMouse,<fMouseOn>		;* restore mouse state

	StopPublic

cEnd	BltArc


;********** DoBltArc **********
;*	entry : ax = day, bx = cx = cb, dx = row modulus, si = source,
;*		di = dest, es and ds => video segment
;*	* blt screen memory
;*	* NOTE: direction flag either set or clear on entry.
;*	exit : n/a

cProc	DoBltArc,<NEAR,PUBLIC,ATOMIC>
cBegin	DoBltArc

	assumes ds,nothing

ifdef	WINDOW_OVERLAP
	push	bp			;* current window do blt
	mov	bp,pwndCur
endif	;WINDOW_OVERLAP

blt_row:
	push	si
	push	di

blt_fast:
;*	* We can BLT fast
ifdef	WINDOW_OVERLAP
	or	bp,bp
	jz	blt_fast_allowed
blt_overlap:
	push	di
	and	di,not 1
	push	ds
	mov	ds,ss:psOverlap
	cmp	bp,[di]
	pop	ds
	je	move_two
;*	* skip this movement
	push	ax
	mov	di,si			;* old position
	lodsw				;* bump by 2 in right direction
	mov	ax,si
	sub	ax,di			;* ax = delta (+2 or -2)
	pop	di			;* old DI
	add	di,ax			;* DI points to new position
	pop	ax
	jmp	short move_overlap_next
move_two:
	pop	di
ifdef	BUILTIN_SNOW
	push	ax
	push	dx
	mov	dx,3DAh			;* CGA video status port
	StartDrawCrit
endif	;BUILTIN_SNOW	
	movsb
	movsb
ifdef	BUILTIN_SNOW
	EndDrawCrit
	pop	dx
	pop	ax
endif	;BUILTIN_SNOW
move_overlap_next:
	dec	cx
	loop	blt_overlap
	jmp	short blt_next_row

blt_fast_allowed:	;* fall through to rep move
endif	;WINDOW_OVERLAP
ifdef	BUILTIN_SNOW
	push	ax
	push	dx
	mov	dx,3DAh			;* CGA video status port
SnowL1:
	StartDrawCrit
	movsb
	EndDrawCrit
	loop	SnowL1
	pop	dx
	pop	ax
ELSE	;NOT DEFINED BUILTIN_SNOW
	rep	movsb			;* move byte to keep backward case
endif	;BUILTIN_SNOW
					;* simple.
blt_next_row:
	pop	di
	pop	si
	mov	cx,bx			;* restore cx as cb
	add	di,dx
	add	si,dx			; move to next/previous row
	dec	ax
	jnz	blt_row

ifdef	WINDOW_OVERLAP
	pop	bp
endif	;WINDOW_OVERLAP

cEnd	DoBltArc

sEnd	SCREEN

;----------------------------------------------------------------------------

	END
