;*	SCCSWHAT( "@(#)editmgr2.asm	1.6 88/04/22 19:46:59	" ) */
;*
;*	COW : Character Oriented Windows
;*
;*	editmgr2.asm : Multi-line edit manager (part2)
;*	(included by editmgr.asm)

;*******************************************************************************
;_Del
;
;Purpose:
;	Delete the current ip character
;Entry:
;	ipCur
;	hBuffer
;	pdCur
;	ldCur
;Exit:
;	ldCur
;Uses:
;	si
;Exceptions:
;
;*******************************************************************************
cProc Del,<NEAR,PUBLIC>,<SI>
cBegin
ifdef	KANJI
	cCall	DbcsAdjCur
endif	; KANJI
	cCall	NoSelection			;remove any selected region
	cCall	ModifyLine			;mark line as modified
	jz	ExitDel
	mov	si, ipCur.ip_ob 		;check if ip after last char
	cmp	si, ldCur.ld_cb
	jb	NormalDel			;brif if ip < last char

	mov	ax, [clnCur]			;check if last line of file
	dec	ax				;last line is offset
	cmp	ax,ipCur.ip_oln
	jne	@F				;brif not last line

	cCall	NearBeep			;can't join line if last
	jmp	SHORT EndDel			;exit
;Here if a join line not del (i.e. del after last char)
@@:
	mov	ax,1				;reverse normal sense of join
						;which is above to below 
	cCall	JoinLine,<ax>			;current line
	jmp	SHORT EndDel			;exit

;Here if a normal del and not join
;si = ipCur.ip_ob
NormalDel:					;del works to right of ip
ifdef	KANJI
	mov	bx,[ldCur.LD_prgch]
	add	bx,si				;pointer to current line pos
	cCall	PchNextDbcs,<bx>		;Get next character
	sub	ax,[ldCur.LD_prgch]		; What is the new offset
	sub	ax,si				;delete 1 or 2 chars
	add	si,ax				;and move 1 or 2 for deleting
else	; KANJI
	mov	ax,1				;delete 1 char.
	inc	si				;so move 1 char for deleting
endif	; KANJI

	cCall	DelCh,<ax,si>
	cmp	si,pdCur.pd_obleft		;check if ip left of margin
	jge	@F				;brif if ip to right of margin
	cCall	DisplayCurPos			;recalc screen position
	jmp	SHORT EndDel			;exit

;Here if del did not move to left of left margin
@@:
	cCall	RefreshLine,<ipCur.ip_oln>	;refresh current line
EndDel:
	mov	[fResetAnchor], 1
ExitDel:
cEnd

;*******************************************************************************
;EMGetWord
;
;Purpose:
; Copies the word the cursor is on (or just after) to fpWord
; The string is null terminated.
; no more than cbMax bytes (including null) will be copied.
;
;Entry:
; fpWord - Where to store the word.
; cbMax  - Max number of chars to copy.
;
;Exit:
; Return number of bytes in the current word.
;
;Uses:
;Exceptions:
;
;*******************************************************************************

cProc EMGetWord,<NEAR,PUBLIC>,<SI,DI>
	parmD	fpWord
	parmW	cbMax

	localW	cb
cBegin

; Leave room for the null terminator
	dec	[cbMax]

	xor	ax,ax
	mov	[cb], ax

	cCall	GetLineScratch,<[ipCur.ip_oln]>
	mov	di, [ipCur.ip_ob]
	mov	ax, [ldEMScratch.ld_cb]
	cmp	di, ax
	jb	GW1
	dec	ax
	mov	di, ax
GW1:
	mov	si, [ldEMScratch.ld_prgch]
	add	si, di

GW2:
	inc	di
	jz	GW5
	dec	di


	cCall	IsWordChar,<[si]>
	or	ax,ax
	jnz	GW3

	dec	si
	dec	di
	jmp	short GW2

GW3:
	inc	di
	jz	GW5
	dec	di

	cCall	IsWordChar,<[si]>
	or	ax,ax
	jz	GW5

	dec	si
	dec	di
	jmp	short GW3
GW5:
	inc	si

	mov	dx, di
	les	di, fpWord

GW6:
	cmp	dx, [ldEMScratch.ld_cb]
	jae	GW7
	mov	ax, [cb]
	cmp	ax, [cbMax]
	jae	GW7
	push	dx
	cCall	IsWordChar,<[si]>
	pop	dx
	or	ax, ax
	jz	GW7

	lodsb
	stosb
	inc	[cb]
	inc	dx
	jmp	short GW6

GW7:
	xor	al,al
	stosb
	mov	ax, [cb]
cEnd

cProc NearBeep,<NEAR,PUBLIC>
cBegin
;;;	cCall	Beep
	xor	ax,ax
	cCall	DoSound,<ax>
cEnd

cProc SetInverseIsa,<FAR,PUBLIC>
	parmW	isa

	localW	coBack
	localW	coFore
cBegin
	;set up the isa isaUserMax-1 to be the inverse of the
	;specified isa.
	push	isa
	lea	ax, coBack
	push	ax
	lea	ax, coFore
	push	ax
	cCall	GetIsaColor
	mov	ax, isaUserMax-1
	push	ax			;*	isa
	push	coFore
	push	coBack
	mov	ax,coFore
	cCall	SetIsaColor

	mov	ax, isaUserMax-1	;return value
cEnd


;/***
;	TabOrBackTab - Handles single or multi line Tab or BackTab.
;	Description:
;		Do Tab/BackTab over all the selected lines (or just the current
;		line if no selection).
;	Input: DX - TRUE for BackTab
;	Output:
;****/
cProc TabOrBackTab,<NEAR,PUBLIC>,<SI,DI>

	localW	fBack
	localD	ipSave
	localB	fInsertModeSave
	localW	olnFirst
	localW	olnLast
	localW	obFirst
	localW	obLast
cBegin
	mov	[fBack], dx
	mov	al, 1
	xchg	al, [fInsertMode]
	mov	[fInsertModeSave],al

	mov	ax, [ipCur.ip_oln]
	mov	word ptr [ipSave.ip_oln],ax
	mov	ax, [ipCur.ip_ob]
	mov	word ptr [ipSave.ip_ob],ax

	lea	ax, olnFirst
	lea	bx, obFirst
	lea	cx, olnLast
	lea	dx, obLast
	cCall	BuildSelection,<ax,bx,cx,dx>
	cCall	GetCurLine

	mov	si, User_EditOFFSET Tab
	cmp	[fBack], 0
	jz	TBT_1

;
; We are BackTabing, figure out how much to backtab.
;
	mov	si, User_EditOFFSET BackTab

	mov	ax, [olnFirst]
	cmp	ax, [ipCur.ip_oln]
	je	TBT_1a
	cCall	GetLine,<ax>
TBT_1a:
	cCall	obGetFirst
	mov	di, ax
	or	ax,ax
	jz	TBT_1
	cCall	obGetPrev
	sub	di,ax

TBT_1:
	mov	ax, [olnFirst]
	cmp	ax, [olnLast]
	jne	TBT_MultiLineSelection

;
; No selection Tab or BackTab is simple
;
	cCall	NoSelection
	call	si
	jmp	TBT_Common

TBT_MultiLineSelection:
;
; For multiline selection, we want to Tab or BackTab each line
;

	cmp	[obLast], 0
	jne	TBT_3
	dec	[olnLast]
TBT_3:
	cCall	UpdateLine
	mov	ax, [olnFirst]
	mov	[ipCur.ip_oln], ax
	or	[emState], ES_NOREDRAW

TBT_NextLine:
	mov	[ipCur.ip_ob],0
	call	si
	inc	[ipCur.ip_oln]
	mov	ax, [olnLast]
	cmp	ax, [ipCur.ip_oln]
	jae	TBT_NextLine

	and	[emState], NOT ES_NOREDRAW
	mov	ax, word ptr [ipSave.ip_oln]
	mov	[ipCur.ip_oln], ax
	mov	ax, word ptr [ipSave.ip_ob]
	mov	[ipCur.ip_ob], ax
	mov	[fResetAnchor], 0

TBT_Common:
	mov	ax, [pdCur.pd_obleft]
	add	ax, [cColumnsCur]
	cmp	ax, [ipCur.ip_ob]
	ja	TBT_4
	cCall	DisplayCurPos
	jmp	short TBT_5

TBT_4:
	cCall	RefreshLines,<[olnFirst],[olnLast]>
	

TBT_5:
	mov	al, [fInsertModeSave]
	mov	[fInsertMode], al
cEnd

;*******************************************************************************
;_Backspace 
;
;Purpose:
;	Do a destructive backspace
;Entry:
;	ipCur
;	ldCur
;Exit:
;	ldCur
;Uses:
;	si - cbDel
;	di - obFirstCur
;	obFirstPref
;Exceptions:
;
;*******************************************************************************
cProc Backspace,<NEAR,PUBLIC>,<SI,DI>
cBegin
ifdef	KANJI
	cCall	DbcsAdjCur
endif	; KANJI
	cCall	NoSelection			;Turn off any selection
	cCall	ModifyLine
	jz	ExitBackspace
	cmp	ipCur.ip_ob,0			;If at start of line do join
	jne	BackspaceNormal 		;brif not at start of line

;Here if backspace at start of line. Then we join with line above
	cmp	ipCur.IP_oln,0
	jne	JoinOk
ifndef SILENT
	cCall	NearBeep
endif
	jmp	SHORT ExitBackspace			;exit

JoinOk:
	sub	ax,ax			;set false to join below
	cCall	JoinLine,<ax>
	jmp	SHORT ExitBackspace			;exit

;Here if backspace is not at start of line
BackspaceNormal:
	mov	ax,[ldCur.LD_cb]
	or	ax,ax
	mov	ax,[ipCur.IP_ob]
	jz	@F
	cCall	obGetFirst			;get beginning text on line
@@:
	mov	di,ax
ifdef	KANJI
	mov	ax,[ldCur.LD_prgch]
	mov	si,[ipCur.ip_ob]
	add	si,ax
	cCall	PchPrevDbcs,<si,ax>
	or	ax,ax				;If NULL, we are off end
	jz	@F
	sub	si,ax
	jmp	SHORT CheckBackspaceIndent
@@:
	mov	si,1				;default deletions = 1
CheckBackspaceIndent:
else	; KANJI
	mov	si,1				
endif	; KANJI

	cmp	ipCur.ip_ob,di			;check to adjust indent
	jne	@F				;if at start of line or blank

;Here if at start of indent on line
	cCall	obGetPrev		;get new indent. level
	mov	si,di
	sub	si,ax			; si = delta to move line left

;Here when ready to delete either 1 or n characters
@@:
	cCall	DelCh,<si,[ipCur.ip_ob]>
	sub	ipCur.ip_ob,si		;adjust ip for deletion
	cCall	DisplayCurPos		;recalc screen position
	cCall	RefreshLine,<[ipCur.ip_oln]>
ExitBackspace:
	mov	[fResetAnchor], 1
cEnd

;*******************************************************************************
;_Tab
;
;Purpose:
;	Insert a tab at current ip. Adjust screen
;Entry:
;	ipCur
;	pdCur.obleft
;Exit:
;	ldCur
;	ipCur
;Uses:
;	si - obCur
;Exceptions:
;
;*******************************************************************************
cProc Tab,<NEAR,PUBLIC>,<SI>
cBegin
	cCall	GetCurLine
	cCall	NextTab,<[ipCur.ip_ob]>
	mov	si, ax			;offset for next tab position
	mov	ax,ldCur.ld_cbMax	;Make sure tab is within line
	dec	ax
	cmp	si, ax			;check if next pos. with line
	jnb	$I511			;brif if tab was within line
	mov	cx, [ldCur.ld_cb]
	jcxz	Tab_JustMove
	jmp	short $I511a

$I511:
	cCall	NearBeep		;warn user
	jmp	SHORT ExitTab

;Here if tab is within max line width and not on a blank line.
$I511a:
	cCall	ModifyLine
	jz	ExitTab
	mov	bx,32
	mov	ax, si			;calc. # of spaces to add
	sub	ax,ipCur.ip_ob
	mov	cl, [fInsertMode]
	cCall	InsCh,<ax,bx,cx>	;insert tabs for spaces
Tab_JustMove:
	mov	ipCur.ip_ob,si		;update current pos. after tabs
ExitTab:
cEnd

;*******************************************************************************
;_InsCh
;
;Purpose:
;	Low level routine to insert a character into ldCur at ip
;Entry:
;	cbAdd
;	InsertCh
;	ipCur
;	ldCur
;Exit:
;	ldCur
;Uses:
;	ob - offset to start insertion
;	cbFill - # of space to insert before char. (if ip of end of line)
;	cbMove - # of character to move to right of ip
;	pIns - buffer pointer to start move
;	di - ldCur.cb
;	si - cbAdd
;Exceptions:
;
;*******************************************************************************
cProc InsCh,<PUBLIC,NEAR>,<SI,DI>
	parmW	cbAdd
	parmB	char
	parmB	fInsert

	localW	pIns
cBegin
ifdef	KANJI
	sub	ah,ah
	mov	al,[fCharIsDbcs]		; See if we have
	dec	ax				;   a DBCS byte
	jns	@F				; If not
	cCall	DbcsAdjCur			;   then Adjust cursor
	jmp	SHORT StartInsCh

@@:						; otherwise
	dec	ax				;   See if we have second byte
	js	StartInsCh			;   If we do
	mov	[fCharIsDbcs],al		;     Then clear flag
StartInsCh:
endif	; KANJI

; registers SI = cbAdd
;           DI = ldCur.cb
;
	mov     si,[cbAdd]
	mov     di,[ldCur.LD_cb]

;
; Are we past the current end of the line?
;
	mov     ax, [ipCur.IP_ob]
	cmp     [ipCur.IP_ob],di
	jle	@F

;
; Fill from end of line to cursor with spaces
;
	sub     ax,di				; cbFill = ipCur.ob - ldCur.cb;

	push    ax
	mov     ax,' '
	push    ax
	mov     ax,[ldCur.LD_prgch]
	add     ax,di
	push    ax
	call    _memset
	add     sp,6

	mov     di,[ipCur.IP_ob]		; ldCur.cb = ipCur.ob;

;
; If cbAdd is too big, figure out how much can be added
;
@@:
	mov     ax,[ldCur.LD_cbMax]
	dec	ax
	cmp     [fInsert],0
	je	@F

	sub     ax,di			; cbMax = ldCur.cbMax - 1 - ldCur.cb
	jmp	short $LL20042

@@:
	sub     ax,[ipCur.IP_ob]	; cbMax = (ldCur.cbMax - 1) - ipCur.ob

$LL20042:
	cmp     ax,si			; Is cbMax >= cbAdd
	jae	@F			; Yes
	mov	si, ax			; cbAdd = cbMax

;
; Now, if there is any room, do it
@@:
	or      si,si
	jle	EndInsCh

	mov     ax,[ipCur.IP_ob]
	add     ax,[ldCur.LD_prgch]
	mov     [pIns],ax

	cmp     [fInsert],0
	je	OverwriteInsCh

;
; We are in Insert Mode so move the stuff after the cursor to make room for
; the stuff we are inserting.
;
	cmp     [ipCur.IP_ob],di
	jge	@F

	mov     ax,di
	sub     ax,[ipCur.IP_ob]

	or      ax,ax
	je	@F

ifdef	PROJECT_QB
	mov     bx,[pIns]
	push	bx				; source
	add     bx,si
	push    bx				; dest
	push	ax				; count
	call	bltbyte			 	; pascal Calling convention
else	;PROJECT_QB
	push    ax
	push    [pIns]
	mov     ax,[pIns]
	add     ax,si
	push    ax
	call	_memmove
	add     sp,6
endif	;PROJECT_QB

@@:
	add     di,si
	jmp	SHORT DoInsCh			; ldCur.cb += cbAdd;
OverwriteInsCh:
	mov     ax,[ipCur.IP_ob]
	add     ax,si
	cmp     ax,di
	jle	DoInsCh

	mov     di,ax				; ldCur.cb = ipCur.ob + cbAdd;

DoInsCh:
	push    si
	mov     al,[char]
	cbw
	push    ax
	push    [pIns]
	call    _memset
	add     sp,6
EndInsCh:
	mov	[ldCur.LD_cb], di		; Save cached value
	mov     ax,si
cEnd

;*******************************************************************************
;_DelCh
;
;Purpose:
;	Low level routine to delete character from ldCur at specified loc.
;Entry:
;	cbDel - # of characters to delete
;	ob - where to delete
;Exit:
;	ldCur
;Uses:
;	cb - temp. for # of bytes
;	si - pszDst - buffer destination pointer
;	di - pszSrc - buffer source pointer
;Exceptions:
;
;*******************************************************************************
cProc DelCh,<NEAR>,<SI,DI>
	parmW	cbDel
	parmW	ob

	localW	cb
cBegin
;
; Adjust if trying to delete past end of line.
;
	mov	ax,[ob]
	sub	ax,[ldCur.ld_cb]
	jle	DelCh1
	cmp	ax, [cbDel]
	jae	DelCh_Exit
	sub	[ob],ax
	sub	[cbDel],ax
DelCh1:

	cmp	ldCur.ld_cb,0		;make sure something to delete
	je	DelCh_Exit		;brif nothing to delete
	cmp	[cbDel],0 		;make sure need to delete something
	je	DelCh_Exit		;brif no need to delete
	mov	ax,ldCur.ld_cb
	cmp	[cbDel],ax
	jb	DelCh2
	xor	ax,ax
	mov	[ldCur.ld_cb],ax
	jmp	short DelCh_Exit

DelCh2:
	cmp	[ob],ax			;check if deleting from with line
	jae	$I535			;brif deleting to right of eol
	sub	ax,[ob]			;trim buffer back display does rest
	mov	[cb],ax
	mov	di,[ob]
	add	di,ldCur.ld_prgch	;di - pointer into buffer
	mov	ax,di	 		;ax - prgch[ob]
	sub	ax,[cbDel]
	mov	si,ax			;dst
	jmp	SHORT $L20049

; UNDONE: fix this
;!!! why isn't this either a memcpy? if not then make it string move!
;While loop till cb == 0 move buffer down by cb.
$WC536:
	mov	al,[di]
	inc	di
	mov	[si],al
	inc	si
$L20049:
	mov	ax,[cb]
	dec	[cb]
	or	ax,ax
	jne	$WC536

;Here after moving buffer down
$I535:
	mov	ax,[cbDel]		;adjust buffer size
	sub	ldCur.ld_cb,ax
DelCh_Exit:
cEnd


;*******************************************************************************
;_obGetFirst
;
;Purpose:
;	Get first non-whilte space character on line
;Entry:
;	ldCur
;
;Exit:
;	return - offset to start of text
;Uses:
;	si - ob current offset 
;	di - obLim limit of search with line (ldCur.cb)
;	bx
;Exceptions:
;
;*******************************************************************************
cProc obGetFirst,<NEAR,PUBLIC>,<DI>
cBegin
	mov	di, [ldCur.ld_prgch]
	mov	cx, [ldCur.ld_cb]
	cCall	$obGetFirst
cEnd

cProc obGetFirstScratch,<NEAR>,<DI>
cBegin
	mov	di, [ldEMScratch.ld_prgch]
	mov	cx, [ldEMScratch.ld_cb]
	cCall	$obGetFirst
cEnd

cProc $obGetFirst,<NEAR>
cBegin
	mov	ax, cx
	jcxz	$obGetFirst_Exit

	push	ds
	pop	es
	mov	al, ' '
	mov	bx, di
	repe scasb
	jne	NoClear
	sub	ax,ax
	jmp	short $obGetFirst_Exit
NoClear:
	mov	ax, di
	sub	ax, bx
	dec	ax

$obGetFirst_Exit:
cEnd

;*******************************************************************************
;_obGetPrev
;
;Purpose:
;	Get the indent level for the line above and to the left
;	of the current ip. If none then 0 returned.
;Entry:
;	ipCur
;Exit:
;	return - indent level
;Uses:
;	si == indent level of ldCur
;Exceptions:
;
;*******************************************************************************
cProc obGetPrev,<NEAR,PUBLIC>,<SI,DI>
cBegin
	mov	ax, [ipCur.ip_ob]
	mov	cx, [ldCur.ld_cb]
	jcxz	GP1
	cCall	obGetFirst
GP1:
	mov	si, ax

	mov	di, [ipCur.ip_oln]
GP2:
	mov	ax, di			; If oln == 0 return 0
	or	ax, ax
	jz	GP4

	dec	di
	cCall	GetLineScratch,<di>

	mov	cx, [ldEMScratch.ld_cb]	; Ignore blank lines.
	jcxz	GP2

	cCall	obGetFirstScratch
	cmp	ax,si
	jae	GP2

GP4:
cEnd

;*******************************************************************************
;_RefreshScreen
;
;Purpose:
;	Redraw the entire visible screen
;Entry:
;	none.
;Exit:
;	none.
;Uses:
;	none.
;Exceptions:
;
;*******************************************************************************
cProc RefreshScreen,<NEAR,PUBLIC>
cBegin
	mov	bx,-1		;refresh max. number of lines
	sub	ax,ax
	cCall	RefreshLines,<ax,bx>	;refresh 0 to max. lines
cEnd

;*******************************************************************************
;_RefreshLines
;
;Purpose:
;	Redraw the specified line if present on screen
;Entry:
;	olnFirst - first line offset to redraw
;	olnLast  - last line offset to redraw
;Exit:
;	none.
;Uses:
;	olnBottom - offset for bottom of screen
;Exceptions:
;
;*******************************************************************************
cProc RefreshLines,<NEAR,PUBLIC>,<SI,DI>
	parmW	olnFirst
	parmW	olnLast
cBegin
	test	[emState], ES_NOREDRAW
	jnz	$EX558
	mov	si,[olnFirst]
	mov	di,[olnLast]
	cmp	si,di		
	jbe	$I561			;brif olnFirst <= olnLast
;Here if order was wrong on range
	xchg	si,di
;Here when order is correct
$I561:
	;!!! get structure definition for this
	mov	cx,[cLinesCur]
	add	cx,[pdCur.pd_olntop]
	dec	cx
	cmp	cx, si			;if first line below screen
	jl	$EX558			;then exit		;????
	cmp	[pdCur.pd_olntop],di	;or if last line above screen
	ja	$EX558			;exit
	cmp	[pdCur.pd_olntop],si
	jbe	$I564			;brif first above top of screen
	mov	si,[pdCur.pd_olntop]	;make first in range top of screen
$I564:
	cmp	cx, di
	jae	$L20052			;brif if last above bottom of screen
	mov	di, cx
	jmp	SHORT $L20052
;Loop refreshing line at a time within range that is entirely on screen
$WC566:
	cCall	RefreshLine,<si>
	inc	si
$L20052:
	cmp	si,di			;
	jbe	$WC566			;brif olnFirst <= olnLast
$EX558:
cEnd

cProc RefreshLine,<NEAR,PUBLIC>,<SI,DI>
	parmW	ln

	localW	row
	localW	olnFirst
	localW	obFirst
	localW	olnLast
	localW	obLast
	localW	attrSave
	localW	ob
	localW	cb
	localW	obRight
	localW	pLineAttr
cBegin
	mov	si,[ln]

	test	[emState], ES_NOREDRAW
	jz	RL1
	jmp	RL_Exit

; Do nothing if line is not in the window
RL1:
	mov	ax,[pdCur.PD_olnTop]
	cmp	si,ax
	jae	RL2
	jmp	RL_Exit
RL2:
	add	ax,[cLinesCur]
	cmp	ax,si
	ja	RL3
	jmp	RL_Exit
RL3:

	mov	ax,si
	sub	ax,[pdCur.PD_olnTop]
	mov	[row],ax

	cCall	GetLineScratchPad,<si>

;
; See if we should display the line (or part of it) in reverse video
;
	test	[emState], ES_NOSELECT
	jnz	RL6a
	mov	bx,[pefCur]
	cmp	[bx.EF_fSelection],0
	je	RL6a

;
; Get the selection extents
;
	lea	ax,[olnFirst]
	lea	bx,[obFirst]
	lea	cx,[olnLast]
	lea	dx,[obLast]
	cCall	BuildSelection,<ax,bx,cx,dx>

;
; See if the entire line should be selected
;
	mov	ax,si
	cmp	[olnFirst],ax
	ja	RL6a
	cmp	[olnLast],ax
	ja	RL6
	cmp	WORD PTR [obLast],0
	je	RL6a
	cmp	[olnLast],ax
	jne	RL6a

RL6:
	mov	ax,[olnLast]
	cmp	[olnFirst],ax
	je	RL7

	mov	[obFirst],0
	mov	[obLast],0ffffH
	jmp	short RL7

RL6a:
; There is a selection but the line we are refreshing is not part of the
; selected area.
	sub	ax,ax
	mov	[obLast],ax
	mov	[obFirst],ax
	jmp	short RL7a			; al MUST be zero


;
; At this point we are ready to build the line attributes
;
RL7:
	mov	bx,[pefCur]
	mov	al, [bx.EF_fSelection]

RL7a:
	mov	di,[CurattrCur]

	or	al, al
	jnz	RL8

	test	[emState], ES_MULTILINE
	jz	RL8
	test	[ldEMScratch.ld_flags],ld_fDirty
	jne	RL8
	cmp	si,[clnCur]
	jae	RL8

;
; There is no selection, the line is not dirty, and not single line edit field.
; So, ask the application for the line attributes.
;
	cCall	GetLineAttrs,<di>
	jmp	SHORT RL9

RL8:
	mov	ax,[obFirst]
	mov	[rgLineAttr0.LA_cb],ax
	mov	[rgLineAttr0.LA_attr],di	; attrDefault

	mov	ax,[obLast]
	sub	ax,[obFirst]
	mov	[rgLineAttr1.LA_cb],ax

	cCall	SetInverseIsa, <di>		; sets up isaUserMax-1
	mov	[rgLineAttr1.LA_attr],ax

	mov	[rgLineAttr2.LA_cb],0ffffH
	mov	[rgLineAttr2.LA_attr],di	; attrDefault

	mov	[rgLineAttr3.LA_attr],0ffffH
	lea	ax,[rgLineAttr0]

RL9:
	mov	[pLineAttr],ax

;
; Ok now, let's print out the line.
;
	mov	ax,[CurattrCur]
	mov	[attrSave],ax

	mov	ax,[cColumnsCur]
	add	ax,[pdCur.PD_obLeft]
	mov	[obRight],ax

	mov	[ob],0

RL10:
	mov	bx,[pLineAttr]
	cmp	WORD PTR [bx.LA_attr],-1
	je	JRL15

	mov	ax,[obRight]
	cmp	[ob],ax
	jae	JRL15

	mov	ax,[bx.LA_cb]
	and	ax,07fffH		;clear high bit, so no overflow in add
	mov	[cb],ax

	mov	ax,[pdCur.PD_obLeft]
	cmp	[ob],ax
	jae	RL12

	mov	ax,[cb]
	add	[ob],ax

	mov	ax,[pdCur.PD_obLeft]
	cmp	[ob],ax
	jae	RL11
	jmp	short RL13

JRL15:
	jmp	RL15

RL11:

	mov	ax,[ob]
	sub	ax,[pdCur.PD_obLeft]
	mov	[cb],ax

	mov	ax,[pdCur.PD_obLeft]
	mov	[ob],ax

RL12:
	cmp	WORD PTR [cb],0
	je	RL13

	mov	di,[pLineAttr]
	mov	ax,[di.LA_attr]
	mov	[CurattrCur],ax

	mov	si,[ob]
	mov	bx,[ldEMScratch.LD_prgch]
	add	si,bx

ifdef	KANJI
	cmp	si,bx
	je	NoKanjiPad
	cCall	PchPrevDbcs,<si,bx>
	cCall	PchNextDbcs,<ax>
	cmp	si,ax
	je	NoKanjiPad
	mov	byte ptr [si],' '
NoKanjiPad:
endif	; KANJI

	mov	cx,[ob]
	sub	cx,[pdCur.PD_obLeft]
	cCall	TextOut,<[pwndEditCur],cx,[row],si,[cb],[CurattrCur]>
	mov	ax,[cb]
	add	[ob],ax

RL13:
	add	[pLineAttr],size LINEATTR
	jmp	RL10

RL15:
	mov	ax,[attrSave]
	mov	[CurattrCur],ax

ifdef	KK_UNIT
	mov	ax,[ln]
	cmp	ax,[ipCur.ip_oln]
	jne	@F
	mov	ax,[ipCur.ip_ob]
	sub	ax,[pdCur.PD_obLeft]
	cCall	DisplayKKBuf,<[pwndEditCur],ax,[row]>
@@:
endif	; KK_UNIT

RL_Exit:
cEnd

;*******************************************************************************
; _HomeScn
;
;Purpose:
;	Move cursor to top of screen
;Entry:
;	pdCur
;	ipCur
;Exit:
;	ipCur
;Uses:
;	none.
;Exceptions:
;
;*******************************************************************************
cProc HomeScn,<NEAR,PUBLIC>
cBegin
	cCall	UpdateLine
	mov	ax,[pdCur.pd_olntop]
	mov	[ipCur.ip_oln],ax	;make current line top of screen
cEnd

;*******************************************************************************
;_EndScn
;
;Purpose:
;	Move cursor to end of screen
;     - the global register set (grs)
;     - the module register set (mrsCur and bdtMrs), via MrsMake
;Entry:
;	pwndEditCur
;	ipCur
;	pdCur
;Exit:
;	ipCur
;	pdCur
;Uses:
;	none.
;Exceptions:
;	If run out of memory trying to allocate a buffer, runtime error
;		"Error during Initialization" or some such.
;*******************************************************************************
cProc EndScn,<NEAR,PUBLIC>
cBegin
	cCall	UpdateLine
	mov	ax,[cLinesCur]	       ;pwndEditCur.cLines
	add	ax,[pdCur.pd_olntop]	;set ip to bottom of screen
	dec	ax
	mov	cx, [clnCur] 
	cmp	ax, cx
	jbe	$I597
	mov	ax, cx
$I597:
	mov	[ipCur.ip_oln],ax
cEnd

;*******************************************************************************
;_BeginPgm
;
;Purpose:
;	Move cursor to beginning of program
;Entry:
;	none.
;Exit:
;	ipCur
;	pdCur
;Uses:
;	none.
;Exceptions:
;
;*******************************************************************************
cProc BegPgm,<NEAR,PUBLIC>
cBegin
	cCall	UpdateLine
	mov	ipCur.ip_oln,0
	cCall	AutoIndent
	cCall	DisplayCurPos
cEnd

;*******************************************************************************
;_EndPgmInitContext()
;
;Purpose:
;	_EndPgm
;Entry:
;	ipCur
;Exit:
;	ipCur
;Uses:
;	none.
;Exceptions:
;
;*******************************************************************************
cProc EndPgm,<NEAR,PUBLIC>
cBegin
	cCall	UpdateLine
	mov	ax, [clnCur]
	mov	ipCur.ip_oln,ax
	mov	ipCur.ip_ob,0
	cCall	DisplayCurPos
cEnd

;*******************************************************************************
;_HomeLine
;
;Purpose:
;	Move Cursor to beginning of line
;Entry:
;	none.
;Exit:
;	ipCur
;Uses:
;	none.
;Exceptions:
;
;*******************************************************************************
cProc HomeLine,<NEAR,PUBLIC>
cBegin
	cCall	AutoIndent
	cCall	DisplayCurPos
cEnd

;*******************************************************************************
;_EndLine
;
;Purpose:
;	Move cursor to end of current line
;Entry:
;	ldCur
;	ipCur
;Exit:
;	ipCur
;Uses:
;	none.
;Exceptions:
;
;*******************************************************************************
cProc EndLine,<NEAR,PUBLIC>
cBegin
	xor	ax,ax
	mov	bx, [ipCur.ip_oln]
	cmp	bx, [clnCur]
	jae	EL_1
	cCall	GetCurLine
	mov	ax,ldCur.ld_cb
EL_1:
	mov	ipCur.ip_ob,ax
	cCall	DisplayCurPos
cEnd

;*******************************************************************************
;_CharRight
;
;Purpose:
;	Move cursor to the right 1 character. Non-destructive	
;Entry:
;	ldCur
;	ipCur
;	pdCur
;	pwndEditCur
;Exit:
;	ipCur
;Uses:
;	none.
;Exceptions:
;*******************************************************************************
cProc CharRight,<NEAR,PUBLIC>
cBegin
ifdef	KANJI
	cCall	DbcsAdjCur
endif	; KANJI
	mov	ax,ldCur.ld_cbMax
	dec	ax
	cmp	ax,ipCur.ip_ob
	ja	$I604		;brif if ip is not a max. line width
ifndef SILENT
	cCall	NearBeep	;warn user
endif
	jmp	SHORT $I606

;Here if ok to move right
$I604:
ifdef	KANJI
	cCall	RightKanji
else	; KANJI
	inc	ipCur.ip_ob
endif	; KANJI
	mov	ax,[cColumnsCur]    ;pwndEditCur.cColumns
	add	ax,pdCur.pd_obleft
	cmp	ax,ipCur.ip_ob
	ja	$I606		;brif if ip to left of screen right edge
	sub	ax,ax		;moved offset screen so page right
	cCall	PageRight,<shiftWidth,ax>
$I606:
cEnd

;*******************************************************************************
;WordRight
;
;Purpose:
;	Move the cursor one word to the right. Moving to the next line if
;	no more word on the current line.
;Entry:
;  DX - if Zero position at start of word
;       if Non-Zero position at end of word
;Exit:
;Uses:
;Exceptions:
;
;*******************************************************************************
cProc WordRight,<NEAR,PUBLIC>,<SI>
cBegin
	push	dx			; Save
	cCall	GetCurLine
	pop	ax			; Restore

	mov	si, User_EditOFFSET RightToWordStart
	or	ax,ax
	jz	WR_CheckEol

	mov	si, User_EditOFFSET RightPastWordEnd

WR_CheckEol:
	mov	ax, [ipCur.ip_ob]
	cmp	ax, [ldCur.ld_cb]
	jae	WR_NextLine

WR_Repeat:
	call	si
	jnz	WR_Exit

WR_NextLine:
	mov	ax, [ipCur.ip_oln]
	inc	ax
	cmp	ax, [clnCur]
	jae	WR_Beep

	mov	[ipCur.ip_oln], ax
	cCall	GetCurLine
	mov	[ipCur.ip_ob], 0
	cCall	FOnWord
	jnz	WR_Exit
	jmp	short WR_Repeat

WR_Beep:
ifndef SILENT
	cCall	NearBeep
endif
WR_Exit:
	cCall	DisplayCurPos
cEnd

;*******************************************************************************
;WordLeft
;
;Purpose:
;	Move left 1 word. See RightWord
;Entry:
;Exit:
;Exit:
;Uses:
;Exceptions:
;
;*******************************************************************************
cProc WordLeft,<NEAR,PUBLIC>
cBegin
	cCall	GetCurLine
	mov	ax, [ldCur.ld_cb]
	cmp	ax, [ipCur.ip_ob]
	ja	WL_Repeat

	mov	[ipCur.ip_ob], ax

WL_Repeat:
	call	LeftToWordStart
	jnz	WL_Exit

	mov	ax, [ipCur.ip_oln]
	or	ax,ax
	jz	WL_NearBeep

	dec	ax
	mov	[ipCur.ip_oln], ax
	cCall	GetCurLine
	mov	ax, [ldCur.ld_cb]
	mov	[ipCur.ip_ob], ax
	jmp	short WL_Repeat

WL_NearBeep:
ifndef SILENT
	cCall	NearBeep
endif
WL_Exit:
	cCall	DisplayCurPos
cEnd

;*******************************************************************************
;_LeftToWordStart
;
;Purpose:
; Moves the cursor left on the current line until the begining of a word
; or the begining of the line is found
;
; if x is a word character
; and . is a non word character
; search for:
;      start position of cursor ---------vvvvvvvvvv
;                                  .....XXXXX.....XXXXX.....
;                                       ^
;      end position of cursor ----------/
;
;Entry:
;Exit:
; Return Z if no word found before begining of line.
;
;Uses:
;Exceptions:
;
;*******************************************************************************
cProc LeftToWordStart,<NEAR,PUBLIC>
cBegin
	cCall	LeftUntilWord
	jz	WSL_Exit		; Return Z - no word found
	cCall	LeftUntilNonWord
	or	sp,sp			; Set NZ flag
WSL_Exit:
cEnd

;*******************************************************************************
;_LeftPastWordEnd
;
;Purpose:
; Moves the cursor left on the current line until the end of a word
; or the begining of the line is found
;
; if x is a word character
; and . is a non word character
; search for:
;      start position of cursor -------------vvvvvvvvvvv
;                                  .....XXXXX.....XXXXX.......
;                                            ^
;      end position of cursor ---------------/
;
;Entry:
;Exit:
; Return Z - if no between word gap is found before start of line.
;
;Uses:
;Exceptions:
;
;*******************************************************************************
cProc LeftPastWordEnd,<NEAR,PUBLIC>
cBegin
	cCall	LeftUntilNonWord
	cCall	LeftUntilWord
cEnd

;*******************************************************************************
;_RightToWordStart
;
;Purpose:
; Moves the cursor right on the current line until the start of a word
; or the end of the line is found
;
; if x is a word character
; and . is a non word character
; search for:
;      start position of cursor --------vvvvvvvvvv
;                                  .....XXXXX.....XXXXX.....
;                                                 ^
;      end position of cursor --------------------/
;
;Entry:
;Exit:
; Return Z - if end of line before begining of word.
;
;Uses:
;Exceptions:
;
;*******************************************************************************
cProc RightToWordStart,<NEAR,PUBLIC>
cBegin
	cCall	RightWhileWord
	cCall	RightWhileNonWord
cEnd


;*******************************************************************************
;_RightPastWordEnd
;
;Purpose:
; Moves the cursor right on the current line until the end of a word
; or the end of the line is found
;
; if x is a word character
; and . is a non word character
; search for:
;      start position of cursor -------------vvvvvvvvvvv
;                                  .....XXXXX.....XXXXXX.....
;                                                       ^
;      end position of cursor --------------------------/
;
;Entry:
;Exit:
; Return Z if no word found before end of line.
;
;Uses:
;Exceptions:
;
;*******************************************************************************
cProc RightPastWordEnd,<NEAR,PUBLIC>
cBegin
	cCall	RightWhileNonWord
	jz	WER_Exit		; Return Z - No word found
	cCall	RightWhileWord
	or	sp,sp			; Return NZ - word found
WER_Exit:
cEnd

;*******************************************************************************
;LeftUntilWord
;
;Purpose:
; Moves the cursor left on the current line until the character to the
; left of the cursor is a word character or until the begining of the line
; is found.
;
; if x is a word character
; and . is a non word character
; search for:
;      start position of cursor ------=------vvvvvv
;                                  .....XXXXX.....XXXXXX.....
;                                            ^
;      end position of cursor ---------------/
;
;Entry:
;Exit:
; Return Z if cursor moved to begining of line.
;
;Uses:
;Exceptions:
;
;*******************************************************************************
cProc LeftUntilWord,<NEAR,PUBLIC>
cBegin
ifdef	KANJI
	cCall	DbcsAdjCur
endif	; KANJI
LUW_Repeat:
	mov	ax, [ipCur.ip_ob]
	or	ax,ax				; Begining of line?
	jz	LUW_Exit			; Yes - Return Z

	cCall	FAfterWord			; Is char to left of cursor
						;    a word char?
	jnz	LUW_Exit			; Yes - Return NZ

ifdef	KANJI
	cCall	LeftKanji
else	; KANJI
	dec	[ipCur.ip_ob]			; Otherwise move cursor left
endif	; KANJI
	jmp	short LUW_Repeat

LUW_Exit:
cEnd

;*******************************************************************************
;LeftUntilNonWord
;
;Purpose:
; Moves the cursor left on the current line until the character to the
; left of the cursor is a non word character or until the begining of the line
; is found.
;
; if x is a word character
; and . is a non word character
; search for:
;      start position of cursor --------vvvvvv
;                                  .....XXXXX.....XXXXXX.....
;                                       ^
;      end position of cursor ----------/
;
;Entry:
;Exit:
; Return Z if cursor moved to begining of line.
;
;Uses:
;Exceptions:
;
;*******************************************************************************
cProc LeftUntilNonWord,<NEAR,PUBLIC>
cBegin
ifdef	KANJI
	cCall	DbcsAdjCur
endif	; KANJI
LUNW_Repeat:
	mov	ax, [ipCur.ip_ob]
	or	ax,ax				; Begining of line?
	jz	LUNW_Exit			; Yes - Return Z

	cCall	FAfterWord			; Is char to left of cursor
						;    a word char?
	jz	LUNW_ExitNZ			; Yes - Return NZ

ifdef	KANJI
	cCall	LeftKanji
else	; KANJI
	dec	[ipCur.ip_ob]			; Otherwise move cursor left
endif	; KANJI
	jmp	short LUNW_Repeat

LUNW_ExitNZ:
	or	sp,sp				; Set NZ flag
LUNW_Exit:
cEnd

;*******************************************************************************
;RightWhileWord
;
;Purpose:
; Moves the cursor right on the current line until the character under
; the cursor is a non word character or until the end of the line is found.
;
; if x is a word character
; and . is a non word character
; search for:
;      start position of cursor --------vvvvvv
;                                  .....XXXXX.....XXXXXX.....
;                                            ^
;      end position of cursor ---------------/
;
;Entry:
;Exit:
; Return Z if cursor moved to end of line.
;
;Uses:
;Exceptions:
;
;*******************************************************************************
cProc RightWhileWord,<NEAR,PUBLIC>
cBegin
ifdef	KANJI
	cCall	DbcsAdjCur
endif	; KANJI
RWW_Repeat:
	mov	ax, [ipCur.ip_ob]
	cmp	ax, [ldCur.ld_cb]		; End of line?
	jae	RWW_ExitZ			; Yes - Return Z

	cCall	FOnWord				; On a word char?
	jz	RWW_ExitNZ			; No - Return NZ

ifdef	KANJI
	cCall	RightKanji
else	; KANJI
	inc	[ipCur.ip_ob]
endif	; KANJI
	jmp	short RWW_Repeat

RWW_ExitNZ:
	or	sp,sp				; Return - NZ
	jmp	short RWW_Exit

RWW_ExitZ:
	xor	ax,ax				; Return - Z
RWW_Exit:
cEnd

;*******************************************************************************
;RightWhileNonWord
;
;Purpose:
; Moves the cursor right on the current line until the character under
; the cursor is a word character or until the end of the line is found.
;
; if x is a word character
; and . is a non word character
; search for:
;      start position of cursor -------------vvvvvv
;                                  .....XXXXX.....XXXXXX.....
;                                                 ^
;      end position of cursor --------------------/
;
;Entry:
;Exit:
; Return Z if cursor moved to end of line.
;
;Uses:
;Exceptions:
;
;*******************************************************************************
cProc RightWhileNonWord,<NEAR,PUBLIC>
cBegin
ifdef	KANJI
	cCall	DbcsAdjCur
endif	; KANJI
RWNW_Repeat:
	mov	ax, [ipCur.ip_ob]
	cmp	ax, [ldCur.ld_cb]		; End of line?
	jae	RWNW_ExitZ			; Yes - Return Z

	cCall	FOnWord				; On a word char?
	jnz	RWNW_Exit			; Yes - Return NZ

ifdef	KANJI
	cCall	RightKanji
else	; KANJI
	inc	[ipCur.ip_ob]
endif	; KANJI
	jmp	short RWNW_Repeat

RWNW_ExitZ:
	xor	ax,ax				; Return - Z
RWNW_Exit:
cEnd

;*******************************************************************************
;FOnWord
;
;Purpose:
; Checks if the cursor is on a word character
;
;
;Entry:
;Exit:
; Return NZ if cursor is on a word.
;
;Uses:
;Exceptions:
;
;*******************************************************************************
cProc FOnWord,<NEAR,PUBLIC>
cBegin
	mov	bx, [ldCur.ld_prgch]
	add	bx, [ipCur.ip_ob]
	mov	al,[bx]
	cCall	IsWordChar,<ax>
	or	ax,ax
cEnd

;*******************************************************************************
;FAfterWord
;
;Purpose:
; Checks if the character to the left of the cursor is a word character.
;
;
;Entry:
;Exit:
; Return NZ if the character to the left of the cursor is a word character.
;
;Uses:
;Exceptions:
;
;*******************************************************************************
cProc FAfterWord,<NEAR,PUBLIC>
cBegin
	mov	bx, [ldCur.ld_prgch]
	add	bx, [ipCur.ip_ob]
	dec	bx
	mov	al,[bx]
	cCall	IsWordChar,<ax>
	or	ax,ax
cEnd


;*******************************************************************************
;_LineUp
;
;Purpose:
;	Scroll screen up on line leaving cursor in place on screen
;Entry:
;	fMultiLine
;	ipCur
;	pdCur
;Exit:
;	ipCur
;	pdCur
;Uses:
;	none
;Exceptions:
;
;*******************************************************************************
cProc LineUp,<NEAR,PUBLIC>
cBegin
	test	[emState], ES_MULTILINE
	jnz	$I653
ifndef SILENT
	cCall	NearBeep
endif
	jmp	SHORT $I655
$I653:
	cCall	UpdateLine
	mov	ax,[pdCur.pd_olntop]
	mov	bx, [ipCur.ip_oln]
	or	bx,bx
	jz	$I655

	cmp	bx,ax			;if at top of screen scroll up
	jne	$I654

	cCall	ScrollUp
$I654:
	cmp	[cLinesCur],1		;if there is only one line in buffer
	je	$I655			;then don't move up.
	dec	ipCur.ip_oln		;else move ip up
$I655:
cEnd

;*******************************************************************************
;_LineDown
;
;Purpose:
;	Scroll file down on screen 1 line. Leave cursor in place.
;Entry:
;	fMultiLine
;	pwndEditCur
;	hBuffer
;	pdCur
;	ipCur
;	
;Exit:
;	ipCur
;Uses:
;	bx
;Exceptions:
;
;*******************************************************************************
cProc LineDown,<NEAR,PUBLIC>
cBegin
	test	[emState], ES_MULTILINE
	jz	$I660
	cCall	UpdateLine
	;!!! get structure
	mov	ax,[cLinesCur]			;pwndEditCur.cLines
	add	ax,pdCur.pd_olntop
	dec	ax
	cmp	ax,ipCur.ip_oln			;if at bottom scroll down
	jne	$I658
	cCall	ScrollDown
$I658:
	mov	ax, [clnCur]			;check not at end of file
	cmp	ax,ipCur.ip_oln
	jbe	$I660				;brif can go down
	cmp	[cLinesCur],1			;if there is only one line in
	je	$I661				;buffer, don't move down.
	inc	ipCur.ip_oln
	jmp	SHORT $I661
$I660:
ifndef SILENT
	cCall	NearBeep
endif
$I661:
cEnd

;*******************************************************************************
;_CharLeft
;
;Purpose:
;	Move cursor to the left 1 character
;Entry:
;	ipCur
;Exit:
;	ipCur
;	pdCur
;	shiftWidth
;Uses:
;	none
;Exceptions:
;
;*******************************************************************************
cProc CharLeft,<NEAR,PUBLIC>
cBegin
ifdef	KANJI
	cCall	DbcsAdjCur
endif	; KANJI
	cmp	ipCur.ip_ob,0			;if already to left don't move
	jne	$I663
ifndef SILENT
	cCall	NearBeep
endif
	jmp	SHORT $I665
$I663:
ifdef	KANJI
	cCall	LeftKanji
else	; KANJI
	dec	ipCur.ip_ob
endif	; KANJI
	mov	ax,pdCur.pd_obleft		;check if past left margin
	cmp	ipCur.ip_ob,ax
	jge	$I665				;brif to right of left margin
	sub	ax,ax
	cCall	PageLeft,<shiftWidth,ax>
$I665:
cEnd

;*******************************************************************************
;_PageUp
;
;Purpose:
;	Move toward start of file 1 page (screen height)
;Entry:
;	fMultiLine
;	ipCur
;	pdCur
;	pwndEditCur
;Exit:
;	fRefreshScreen
;	ipCur
;Uses:
;	bx
;Exceptions:
;
;*******************************************************************************
cProc PageUp,<NEAR,PUBLIC>
cBegin
	test	[emState], ES_MULTILINE
	jnz	$I667
$L20059:
ifndef SILENT
	cCall	NearBeep
endif
	jmp	SHORT $I669
$I667:
	cCall	UpdateLine
	cmp	pdCur.pd_olntop,0		;can't page up if a top file
	je	$L20059
	mov	ax,[cLinesCur]			;pwndEditCur.cLines
	sub	pdCur.pd_olntop,ax		;move up 1 screen 
	jns	$I670				;brif did not wrap past 0
	mov	pdCur.pd_olntop,0		;not enough room so goto top
$I670:
	mov	ax,[cLinesCur]			;pwndEditCur.cLines
	sub	ipCur.ip_oln,ax 		;move ip up 1 sceen
	jns	$I671				;check did not underflow
	mov	ipCur.ip_oln,0			;not enough room
$I671:
	inc	[fRefreshScreen]
$I669:
cEnd

;*******************************************************************************
;_Scrolldown
;
;Purpose:
;	Scroll down 1 screen
;Entry:
;	fMultiLine
;	pdCur
;	ipCur
;Exit:
;	ipCur
;Uses:
;	si = oln
;Exceptions:
;
;*******************************************************************************
cProc ScrollDown,<NEAR,PUBLIC>,<SI>
cBegin
	test	[emState], ES_MULTILINE
	jz	$I675
	cCall	UpdateLine
	mov	si,[cLinesCur]			;pwndEditCur.cLines
	add	si,[pdCur.pd_olntop]		;move top down 1 screen
	mov	ax, [clnCur]			;check if past end of file
	cmp	ax,si				;si - oln
	jb	$I675
	mov	ax,[cLinesCur]			;pwndEditCur.cLInes
	dec	ax				;scroll 1 less then screen
	sub	cx,cx
	mov	dx,1

	push	[pwndEditCur]
	xor	cx,cx
	push	cx				; rxDest = 0
	push	cx				; ryDest = 0
	push	[cColumnsCur]			; drx = cColumnsCur
	mov	ax,[cLinesCur]
	dec	ax
	push	ax				; dry = cLinesCur - 1
	push	cx				; rxSrc = 0
	mov	ax,1
	push	ax				; rySrc = 1
	cCall	BltRrc

	inc	[pdCur.pd_olntop]		;adjust for new top of screen
	cCall	RefreshLine,<si>		;refresh scrolled over line

	mov	ax, [ipCur.ip_oln]
	cmp	ax, [pdCur.pd_olntop]
	jae	$I677
	inc	ipCur.ip_oln
	jmp	SHORT $I677
$I675:
ifndef SILENT
	cCall	NearBeep
endif
$I677:
cEnd

;*******************************************************************************
;ScrollUp
;
;Purpose:
;	Move up 1 line.  See ScrollDown
;Entry:
;	fMultiLine
;	pdCur
;	ipCur
;Exit:
;	ipCur
;Uses:
;	si = oln
;Exceptions:
;
;*******************************************************************************
cProc ScrollUp,<NEAR,PUBLIC>,<SI>
cBegin
	test	[emState], ES_MULTILINE
	jz	$I681
	cCall	UpdateLine
	mov	si,[pdCur.pd_olntop]		;scroll off top line
	dec	si
	js	$I681				;but don't scroll at top file

	cCall	GetLineScratch,<si>

	push	[pwndEditCur]			; pwnd
	xor	cx,cx				; cx = 0
	push	cx				; rxDest = 0
	mov	ax,1
	push	ax				; ryDest = 1
	push	[cColumnsCur]			; drx = cColumnsCur
	mov	ax,[cLinesCur]
	dec	ax
	push	ax				; dry = cLinesCur - 1
	push	cx				; rxSrc = 0
	push	cx				; rySrc = 0
	cCall	BltRrc

	mov	pdCur.pd_olntop,si		;new top of screen
	cCall	RefreshLine,<si>

	mov	ax, [pdCur.pd_olntop]
	add	ax, [cLinesCur]
	cmp	ax, [ipCur.ip_oln]
	ja	$I683
	dec	ipCur.ip_oln
	jmp	SHORT $I683
$I681:
ifndef SILENT
	cCall	NearBeep
endif
$I683:
cEnd

;*******************************************************************************
;_PageDown
;
;Purpose:
;	Move down file 1 page
;Entry:
;	fMultiLine
;	ipCur
;	pdCur
;	hBuffer
;Exit:
;	ipCur
;	fRefreshScreen
;Uses:
;	none
;Exceptions:
;
;*******************************************************************************
cProc PageDown,<NEAR,PUBLIC>
cBegin
	test	[emState], ES_MULTILINE
	jnz	$I685
$L20062:
ifndef SILENT
	cCall	NearBeep
endif
	jmp	SHORT $I687
$I685:
	cCall	UpdateLine
	mov	ax, [clnCur]			;check that page down won't
	mov	cx,[cLinesCur]			;pwndEditCur.cLines
	add	cx,WORD PTR pdCur.pd_olntop
	cmp	cx,ax
	jae	$L20062				;brif if paging off eof
	mov	ax,[cLinesCur]			;pwndEditCur.cLines
	add	pdCur.pd_olntop,ax		;adjust screen down 1 page
	add	ipCur.ip_oln,ax
	mov	ax, [clnCur]			;check if new ip is off eof
	cmp	ax,ipCur.ip_oln
	ja	$I688				;brif new ip still within file
	dec	ax				;else put ip at eof
	mov	ipCur.ip_oln,ax
$I688:
	inc	[fRefreshScreen]
$I687:
cEnd

;*******************************************************************************
;_PageLeft
;
;Purpose:
;	Page file to left.
;Entry:
;	ipCur
;	pdCur
;	pwndEditCur
;Exit:
;	ipCur
;	fRefreshScreen
;Uses:
;	shiftWidth
;	fMoveIp
;Exceptions:
;
;*******************************************************************************
cProc PageLeft,<NEAR,PUBLIC>
	parmW	$shiftWidth
	parmB	fMoveIp
cBegin
	cmp	pdCur.pd_obleft,0		;check if already to far left
	jne	$I692				;brif can still move to left
ifndef SILENT
	cCall	NearBeep			    ;no room to page left
endif
	jmp	SHORT $I693			;exit
$I692:
	mov	ax,pdCur.pd_obleft		;see if room to movu shiftwidth
	cmp	[$shiftWidth],ax
	jbe	$I694				;brif room move left shiftwidth
	mov	pdCur.pd_obleft,0		;else move the way left
	jmp	SHORT $I695
$I694:
	mov	ax,[$shiftWidth]			;adjust left margin
	sub	pdCur.pd_obleft,ax
$I695:
	cmp	[fMoveIp],0	
	je	$I698
	mov	ax,ipCur.ip_ob			;move ip with page left
	cmp	[$shiftWidth],ax
	jbe	$I697
	mov	ipCur.ip_ob,0
	jmp	SHORT $I698
$I697:
	mov	ax,[$shiftWidth]
	sub	ipCur.ip_ob,ax
$I698:
	mov	ax,[cColumnsCur]		;pwndEditCur.cColumns
	add	ax,pdCur.pd_obleft		;check if ip still on screen
	cmp	ax,ipCur.ip_ob
	ja	$I699				;brif so
	mov	ax,pdCur.pd_obleft		;if off screen move to margin
	mov	ipCur.ip_ob,ax
$I699:
	inc	[fRefreshScreen]
$I693:
cEnd

cProc PageRight,<NEAR,PUBLIC>
	parmW	$shiftWidth
	parmB	fMoveIp

	localW	obMax
cBegin
	mov	cx,pdCur.pd_obleft
	mov	dx,ipCur.ip_ob

	mov	ax,ldCur.ld_cbMax		;calc. max. offset can move
	mov	bx,pwndEditCur			;move to the right
	sub	ax,[cColumnsCur]		;pwndEditCur.cColumns
	jle	PR_1				;brif cbMax < cColumns
	mov	[obMax],ax	
	cmp	[obMax],cx	
	ja	$I704				;brif ok to page right

PR_1:
ifndef SILENT
	cCall	NearBeep
endif
	jmp	SHORT $I705			;return
$I704:
	mov	ax,[$shiftWidth]
	add	ax, cx				;ax = $shiftWidth+obleft
;	add	ax,pdCur.pd_obleft		;check if new margin+shiftwidth
	cmp	ax,[obMax]			;check if over limit
	jb	$I706				;brif not over limit
	mov	ax,[obMax]			;else put margin at max.
	mov	cx, ax				;cx = obleft
;	mov	pdCur.pd_obleft,ax
	jmp	SHORT $I707
$I706:
	mov	ax,[$shiftWidth]		;move margin to shift
	add	cx, ax				;cx = obleft+shiftwidth
;	add	pdCur.pd_obleft,ax
$I707:
	cmp	[fMoveIp],FALSE	
	je	$I708				;brif ip not to follow page
	mov	ax,[$shiftWidth]
	add	dx, ax				;dx = ipCur.ob+shiftWidth
;	add	ipCur.ip_ob,ax
$I708:
;	mov	ax,pdCur.pd_obleft
	cmp	dx, cx				;check if ip still on screen
;	cmp	ipCur.ip_ob,ax			;check if ip still on screen
	jge	$I709
	mov	dx, ax
$I709:
	mov	ax,ldCur.ld_cbMax		;could have gone past eol
	cmp	dx, ax
;	cmp	ipCur.ip_ob,ax
	jb	$I710				;brif ip still on line
	dec	ax				;if not then put at eol
	mov	dx, ax
$I710:
	inc	[fRefreshScreen]
$I705:
	mov	ipCur.ip_ob, dx
	mov	pdCur.pd_obleft, cx
cEnd

ifdef	KANJI

cProc LeftKanji,<NEAR,PUBLIC>
cBegin
	cCall	GetLineScratchPad,<[ipCur.ip_oln]>
	mov	ax,[ipCur.ip_ob]
	mov	bx,[ldEMScratch.LD_prgch]
	add	ax,bx
	cCall	PchPrevDbcs,<ax,bx>
	sub	ax,[ldEMScratch.LD_prgch]
	mov	[ipCur.ip_ob],ax
cEnd

cProc RightKanji,<NEAR,PUBLIC>
cBegin
	cCall	GetLineScratchPad,<[ipCur.ip_oln]>
	mov	bx,[ldEMScratch.LD_prgch]
	add	bx,[ipCur.ip_ob]
	cCall	PchNextDbcs,<bx>
	sub	ax,[ldEMScratch.LD_prgch]
	mov	[ipCur.ip_ob],ax
cEnd

cProc DbcsAdjCur,<NEAR,PUBLIC>
cBegin
	cCall	DbcsAdjCursor
	mov	[ipCur.ip_ob],ax
SkipAdjCur:
cEnd

cProc DbcsAdjCursor,<NEAR,PUBLIC>,<si,di>
cBegin
	cCall	GetLineScratchPad,<[ipCur.ip_oln]>
	mov	si,[ipCur.ip_ob]
	mov	bx,[ldEMScratch.LD_prgch]
	add	si,bx
	cCall	PchPrevDbcs,<si,bx>
	xchg	ax,di
	cCall	PchNextDbcs,<di>
	cmp	ax,si
	je	@F
	xchg	ax,di
@@:
	sub	ax,[ldEMScratch.LD_prgch]
cEnd

endif	; KANJI

;*******************************************************************************
;_DisplayCurPos
;
;Purpose:
;	Recalc. the file offset for the top of screen and left margin. This
;	is done when a seek to an arbitrary position within file is done
;	and a total calc. of position relative to currency is needed.
;Entry:
;	fRedraw 
;	pdCur
;	ipCur
;	pwndEditCur
;Exit:
;	ipCur
;	pdCur
;	fRefreshScreen
;Uses:
;	ax, bx, cx, dx, si, di
;	si = margin
;	di - ipCur.ob and olnBottom
;	obRight
;	shiftWidthSave
;Exceptions:
;	If run out of memory trying to allocate a buffer, runtime error
;		"Error during Initialization" or some such.
;*******************************************************************************
cProc DisplayCurPos,<NEAR,PUBLIC>,<SI,DI>

	localW	obRight
	localW	shiftWidthSave
cBegin

ifdef	KANJI
	cCall	DbcsAdjCur
endif	; KANJI
	test	[emState], ES_NOREDRAW
	jz	$JCC6737		;brif ok to redraw screen
	jmp	$EX711			;don't do recalc if no redraw
$JCC6737:
	mov	si,[cLinesCur]		;pwndEditCur.cLines
	shr	si,1			;calc. margin at 1/4 screen width
	shr	si,1			;margin - space left at edge
	;Compute file offset to move to top of screen
	mov	cx, pdCur.pd_olntop
	mov	dx, ipCur.ip_oln
;First check ipCur.oln < pdCur.olntop -  is ip above current top of screen
	cmp	dx, cx
	jae	$I718			;brif if ip below top of screen
	cmp	dx, si			;is ip within margin of beg. file
	jae	$I719			;brif if not
	xor	cx, cx	  		;if that close to bof, set at top
	jmp	SHORT $L20063
$I719:
	mov	ax, cx
	sub	ax,si			;check if ip between top of screen(tos)
	cmp	ax,dx			;and tos-margin (just above screen)
	jae	$I721			;brif if father above then that
	mov	cx,dx			;if that close make it tos
	jmp	SHORT $L20063
$I721:
	mov	cx, dx			;else tos to ip - margin
	sub	cx, si		       
$L20063:
	mov	pdCur.pd_olntop,cx	;adjust to new tos
$I720:
	inc	[fRefreshScreen]

;Here after check for ip above tos
; Now check if ip below last line on screen
$I718:
	mov	di, [cLinesCur] 	;pwndEditCur.cLines
	add	di, cx			;di = olntop + cLines
	dec	di			;di = olnBottom of screen
	cmp	di, dx			;dx = ipCur.ip_oln
	jae	$I723			;brif ip below last line of screen
	mov	ax, di			;check if ip between bot. and margin
	add	ax, si
	cmp	ax, dx			;dx = ipCur.ip_oln
	jbe	$I724			;brif below more then margin 
	mov	ax, dx			;else set tos so ip is at bottom
	sub	ax, di
	add	cx, ax
	jmp	SHORT $I725
$I724:
	mov	cx, dx			;ok to adjust tos to put move by
	sub	cx, si			;by margin distance
$I725:
	mov	pdCur.pd_olntop, cx
	inc	[fRefreshScreen]
	
;Here when ready to recalc the columns
$I723:
	mov	cx, pdCur.pd_obleft
	mov	si,[cColumnsCur]	;pwndEditCur.cColumns
	shr	si,1			;calc. margin for columns again
	shr	si,1			;a factor of 1/4 screen width
	mov	ax,shiftWidth
	mov	[shiftWidthSave],ax	;use new shiftWidth based on margin
	cmp	ax,si
	jbe	$I726			;brif shiftWidth less then margin
	mov	shiftWidth,si		;if bigger use margin as shiftWidth
$I726:
	mov	ax,[cColumnsCur]	;pwndEditCur.cColumns
	add	ax,cx			;calc. left edge of screen
	dec	ax
	mov	[obRight],ax		;line offset for left edge
	mov	di,ipCur.ip_ob		;di - ip col. offset
;Check if ip is to left of left margin
	cmp	cx,di
	jbe	$I727			;brif ip to right of left margin
	cmp	di,si			;is ip with margin of start of line
	jae	$I728			;brif ip more the margin from start
	xor	cx, cx			;just set to start of line
	jmp	SHORT $L20064
$I728:
	mov	ax,cx			;check if ip is within margin of 
	sub	ax,si			;left edge
	cmp	ax,di
	jbe	$I730			;brif ip more then margin away
	mov	cx,di
	sub	cx,si			;adjust by margin
	jmp	SHORT $L20064
$I730:
	mov	cx,di			;else don't use margin to adjust
	sub	cx,shiftWidth		;but shiftWidth
$L20064:
	mov	pdCur.pd_obleft, cx	;set up new left margin
$I729:
	inc	[fRefreshScreen]
;How check if ip past right edge of screen
$I727:
	mov	ax,[cColumnsCur]	;pwndEditCur.cColumns
	add	ax, pdCur.pd_obleft
	cmp	ax,di
	;!!! why can't use obright in above compare with temp inc.
	ja	$I732			;brif to left of right edge
	mov	ax,ldCur.ld_cbMax	;check if ip within full screen of edge
	sub	ax,[cColumnsCur]	;pwndEditCur.cColumns
	cmp	ax,di		
	jbe	$L20065			;brif ?
	mov	ax,[obRight]		
	add	ax,si
	cmp	ax,di			;check if ip with margin region
	jbe	$I735			;brif if not
	mov	ax,shiftWidth		;with margin of edge so use shiftWidth
	sub	ax,[cColumnsCur]	;pwndEditCur.cColumns
	add	ax,di
	jmp	SHORT $L20065
$I735:
	mov	ax,di			;else adjust it by margin
	sub	ax,si
$L20065:
	mov	pdCur.pd_obleft,ax
	inc	[fRefreshScreen]
$I732:
	mov	ax,[shiftWidthSave]	;restore shiftWidth
	mov	shiftWidth,ax
$EX711:
cEnd


cProc AutoIndent,<NEAR,PUBLIC>
cBegin
	xor	ax,ax			; Use 0 for last line.
	mov	bx, [ipCur.ip_oln]
	cmp	bx, [clnCur]
	jae	AI_1

	cCall	GetCurLine
	cCall	obGetFirst		;get offset for first non-white spe
AI_1:
	mov	ipCur.ip_ob,ax
cEnd

;*******************************************************************************
;BackTab
;
;Purpose:
;  Back Tab the current line
;Entry:
;	ipCur
;  di == amount to backtab the line
;Exit:
;Uses:
;Exceptions:
;
;*******************************************************************************
cProc BackTab,<NEAR,PUBLIC>
cBegin
	cCall	ModifyLine
	jz	BackTab_Exit

	or	di,di
	jz	BackTab_Exit

	cCall	obGetFirst
	mov	[ipCur.ip_ob], ax
	cmp	ax, di
	jbe	BT_1
	mov	ax, di

BT_1:
	or	ax,ax
	jz	BackTab_Exit

	cCall	DelCh,<ax,ax>
BackTab_Exit:
	mov	[fResetAnchor], 1
cEnd

;Do what little undo we support
cProc Undo,<NEAR,PUBLIC>
cBegin
	and	ldCur.ld_flags, not ld_fDirty
	mov	pdCur.pd_oln,-1		;set to no current line in ld
	cCall	RefreshLine,<[ipCur.ip_oln]>	;get line from buffer
	cCall	HomeLine
	cCall	ResetAnchor		;set ipAnchor to ipCur
cEnd

cProc Cut,<NEAR,PUBLIC>
	parmB	fCopy

	localW	olnBottom
	localW	olnFirst
	localW	obFirst
	localW	olnLast
	localW	obLast
cBegin
	cCall	fCheckReadOnly
	jnz	ExitCut
	mov	bx,pefCur
	cmp	[bx].ef_fSelection,FALSE
	je	$I751			;if no selection then no cut

	lea	ax,[olnFirst]
	lea	bx,[obFirst]
	lea	cx,[olnLast]	
	lea	dx,[obLast]
	cCall	BuildSelection,<ax,bx,cx,dx>
	mov	bx,pefCur		;after cut don't have any selection
	mov	[bx].ef_fSelection,FALSE
	mov	ax,[olnLast]		;check if single line cut
	cmp	[olnFirst],ax	;i.e. first line == last line
	jne	$I749			;brif if multi-line cut
	mov	al,[fCopy]		;check if distructive cut
	sub	ah,ah
	cCall	CutChars,<olnFirst,obFirst,obLast,ax>
	jmp	SHORT $I751
;Here for mult-line cut
$I749:
	cmp	WORD PTR [obLast],0	;if selection is only first ch. of last
	jne	$I752			;line then don't current last line
	dec	WORD PTR [olnLast]	;this is to cut all of current line
$I752:
	mov	ax,[olnLast]
	sub	ax,[olnFirst]
	inc	ax
	mov	bl,[fCopy]
	sub	bh,bh
	cCall	CutLines,<olnFirst,ax,bx>
$I751:
	cCall	ResetAnchor
ExitCut:
cEnd


cProc CutChars,<NEAR>
	parmW	oln
	parmW	obFirst
	parmW	obLast
	parmB	fCopy
cBegin
	cmp	[fCopy],0
	je	$I759
;Here to copy chars to scrap first
	cCall	CopyCharsToScrap,<oln,obFirst,obLast>
$I759:
	cCall	ModifyLine
	mov	ax,[obFirst]		;first character to delete
	mov	ipCur.ip_ob,ax	
	mov	ax,ldCur.ld_cb
	cmp	[obFirst], ax
	jae	$I762

	cmp	[obLast],ax
	jbe	$I761			;brif last char. is not past eol

	mov	[obLast],ax		;cut off del. to eol
$I761:
	mov	ax,[obLast]
	sub	ax,[obFirst]		;ax - # of chars to delete
	cCall	DelCh,<ax,obLast>
$I762:
	cCall	DisplayCurPos		;recalc. screen position in file
	cCall	RefreshLine,<oln>	;redraw the current line
cEnd

cProc CutLines,<NEAR>
	parmW	olnFirst
	parmW	cln
	parmB	fCopy
cBegin
	cmp	[fCopy],0	;check if destructive?
	je	$I766
	cCall	CopyLines,<olnFirst,cln>
	jz	CutLinesExit
$I766:
	cCall	UpdateLine
	mov	[pdCur.pd_oln], -1
	cCall	DeleteLinesBuf,<hBuffer,olnFirst,cln>
	cCall	EMRefreshCache
	mov	ax,[olnFirst]		;update new insertion point
	mov	ipCur.ip_oln,ax
	cCall	AutoIndent		;adjust to start of valid text
	mov	ax,pdCur.pd_olntop
	cmp	[olnFirst],ax		;check of cut start below screen
	jae	$I768			;brif if start of cut below screen
	mov	ax,[olnFirst]		;move screen top to start of cut
	mov	pdCur.pd_olntop,ax
$I768:
	cCall	DisplayCurPos
CutLinesExit:
	inc	[fRefreshScreen]
cEnd

cProc Copy,<NEAR,PUBLIC>

	localW	olnFirst
	localW	obFirst
	localW	olnLast
	localW	obLast
cBegin
	mov	bx,pefCur
	cmp	BYTE PTR [bx].ef_fSelection,0
	je	$I776

	lea	ax,[olnFirst]
	lea	bx,[obFirst]
	lea	cx,[olnLast]
	lea	dx,[obLast]
	cCall	BuildSelection,<ax,bx,cx,dx>
	mov	ax,[olnLast]			;check if multi-line copy
	cmp	[olnFirst],ax
	jne	$I775				;brif if multi-line copy
	cCall	CopyCharsToScrap,<olnFirst,obFirst,obLast>
	jmp	SHORT $I776			;jmp around multi-copy
$I775:
	cmp	[obLast],0			;check if last line is copied
	jne	$I777				;brif last line included in copy
	dec	[olnLast]			;no first ch. select. not last
						;line copy
$I777:
	mov	ax,[olnLast]	
	sub	ax,[olnFirst]
	inc	ax
	push	ax
	cCall	UpdateLine
	pop	ax
	cCall	CopyLines,<olnFirst,ax>
$I776:
cEnd

cProc CopyCharsToScrap,<PUBLIC,NEAR>
	parmW	oln
	parmW	obFirst
	parmW	obLast
cBegin
	cCall	FreeScrap
	cCall	EMRefreshCache

	mov     ax,MAXCHARS

	push	[oln]
	push	[obFirst]
	push	[obLast]
ifdef	PROJECT_QB
	mov	dx,SEG sb_buffer
	push	dx
	mov	bx,OFFSET sb_buffer
else	;PROJECT_QB
	push	ds
	mov     bx,OFFSET DGROUP:sb_buffer
endif	;PROJECT_QB
	push	bx
	push	ax
	call    CopyChars

	mov     [_scrap.sb_cb],ax
	mov     [_scrap.sbfMultiLine],0
	mov	[fPasteOk],1
cEnd

ifdef	KK_UNIT

cProc AdjustKKDisplay,<PUBLIC,FAR>,<si>
	parmW	cch
cBegin
	mov	si,[ipCur.ip_ob]
	mov	ax,[cch]
	add	[ipCur.ip_ob],ax
	cCall	DisplayCurPos
	mov	[ipCur.ip_ob],si
	cmp	[fRefreshScreen],0
	jz	@F
	cCall	RefreshScreen
@@:
	cCall	Refreshline,<[ipCur.ip_oln]>
cEnd

endif	; KK_UNIT

cProc CopyChars,<NEAR,PUBLIC>,<SI>
	parmW	oln
	parmW	obFirst
	parmW	obLast
	parmD	fpBuffer
	parmW	cbMax
cBegin
;       register si = cbText

	cCall    GetLine,<oln>

	mov     si,[obLast]
	sub     si,[obFirst]

	mov     ax,[ldCur.LD_cbMax]
	sub     ax,[ldCur.LD_cb]
	jz      CC1

	push    ax
	mov     ax,' '
	push    ax
	mov     ax,[ldCur.LD_cb]
	add     ax,[ldCur.LD_prgch]
	push    ax
	call    _memset
	add     sp,6
CC1:

	cmp     [cbMax],si
	ja      CC2
	mov     si,[cbMax]
	dec     si
CC2:
	or      si,si
	je      CC3

	mov     ax,[obFirst]
	add     ax,[ldCur.LD_prgch]
	push    ds
	push    ax
	push    word ptr [fpBuffer+2]
	push    word ptr [fpBuffer]
	push    si
	call	bltbytex
CC3:
	mov     bx,[ldCur.LD_cb]
	add     bx,[ldCur.LD_prgch]
	mov     BYTE PTR [bx],0

	les     bx,[fpBuffer]
	mov     BYTE PTR es:[bx][si],0

	mov     ax,si
cEnd


cProc CopyLines,<NEAR,PUBLIC>,<SI,DI>
	parmW	olnFirst
	parmW	cln
cBegin
	mov	si,[olnFirst]
	mov	di,[cln]
	inc	di
	cCall	FreeScrap
	cCall	EMRefreshCache
	jmp	SHORT $L20067
$WC793:
	cmp	si, [clnCur]
	jae	CopyLines_Done
	cCall	EMRefreshCache
	cCall	GetLine,<si>
	inc	si

	cCall	hBufScrap
	push	ax			; 1st parm of InsertLineBuf
	cCall	LinesInBuf,<ax>
	push	ax			; 2nd parm of InsertLineBuf
	push	ldCur.ld_cb		; 3rd parm of InsertLineBuf
	push	ldCur.ld_prgch		; 4th parm of InsertLineBuf
	call	InsertLineBuf		;insert into scrap
	cbw
	or	ax,ax			;text of insert was a success
	je	$WB794			;break out of out of memory
$L20067:
	dec	di
	jnz	$WC793

CopyLines_Done:
	mov	ax, sp			; Return TRUE

$WB794:
	push	ax			; Save return value
	cCall	EMRefreshCache
	pop	ax

	or	ax,ax
	jz	CopyLines_Fail
					; NZ flag still set
	mov	_scrap.SBfMultiLine,1	; NZ flag still set
	mov	[fPasteOk],1            ; NZ flag still set
	jmp	short CopyLines_Exit    ; return NZ

CopyLines_Fail:
	cCall	FreeScrap		; Ran out of memory - free the scrap
	cCall	EMRefreshCache
	xor	ax,ax			; Return Z
CopyLines_Exit:
cEnd

cProc Paste,<NEAR,PUBLIC>,<SI,DI>
	parmDP	pch
cBegin
	cCall	fCheckReadOnly
	jz	@F
	jmp	ExitPaste
@@:
	mov	di,pch
	cmp	_scrap.SBfMultiLine,0
	je	@F			;brif single line scrap
	or	di,di
	je	MultilinePaste		;brif if nothing to paste
@@:
	cmp	_scrap.SB_cb,0
	jne	@F			;brif something in scrap
	or	di,di
	jne	@F
	jmp	ExitPaste
@@:
;Here if single line paste
	cCall	StartBigEdit
	sub	ax,ax
	cCall	Cut,<ax>
	cCall	ModifyLine
	cCall	EndBigEdit
	cCall	EMRefreshCache
	push	ipCur.ip_ob		;save where to start insertion
	or	di,di
	je	@F			;brif to insert from scrap
	push	ds
	push	di			;get size of insertion
	call	fstrlen
	xchg	si,ax			;si = cb
ifdef	PROJECT_QB
	push	ds			;save source segment
endif	;PROJECT_QB
	jmp	SHORT PasteSpaces
@@:
	mov	si,_scrap.SB_cb	;just insert scrap
ifdef	PROJECT_QB
	mov	di,OFFSET SB_buffer	; source offset
	mov	bx,SEG SB_buffer	; source segment
	push	bx			; save source segment
else	;PROJECT_QB
	mov	di,OFFSET DGROUP:SB_buffer
endif	;PROJECT_QB
PasteSpaces:
	;insert spaces to make room for insertion
	mov	ax,32
	cCall	InsCh,<si,ax,ax>	; InsCh( obFirst, ' ', TRUE );
	mov	si,ax
ifdef	PROJECT_QB
	pop	bx			; BX = source segment
endif	;PROJECT_QB
	pop	cx			; CX = thing saved way above
	or	si,si
	je	@F
ifdef	PROJECT_QB
	push	bx			; source addr
	push	di
	push	ds			; dest addr
	add	cx,ldCur.ld_prgch
	push	cx
	push	si			; length 
	call	bltbytex		; do the copy
else	;PROJECT_QB
	push	si
	push	di
	add	cx,ldCur.ld_prgch
	push	cx
	call	_memmove		;copy over spaces just inserted
	add	sp,6
endif	;PROJECT_QB
@@:
	cCall	RefreshLine,<[ipCur.ip_oln]>
	jmp	SHORT ExitPaste

;Here if multi-line paste
MultilinePaste:
	cCall	StartBigEdit
	sub	ax,ax
	cCall	Cut,<ax>
	cCall	UpdateLine
	cCall	EndBigEdit
	cCall	EMRefreshCache
	push	hBuffer
	push	ipCur.ip_oln
	cCall	hBufScrap
	push	ax
	call	InsertBufInBuf		;insert scrap into source
	cCall	EMRefreshCache
	mov	pdCur.pd_oln,-1 	;invalidate ldCur
	inc	[fRefreshScreen]
ExitPaste:
cEnd

cProc NextTab,<NEAR,PUBLIC>,<SI>
	parmW	obCur
cBegin
	;return (obCur/tabStops + 1)*tabStops)
	mov	si,[obCur]
	mov	ax,si
	sub	dx,dx
	div	tabStops
	inc	ax
	mul	tabStops
cEnd

cProc GetTabs,<FAR,PUBLIC>
cBegin
	mov	ax,tabStops
cEnd

cProc SetTabs,<FAR,PUBLIC>
	parmW	value
cBegin
	mov	ax,[value]
	mov	tabStops,ax
cEnd

cProc ResetAnchor,<NEAR,PUBLIC>
cBegin
	mov	ax, ipCur.ip_ob
	mov	ipAnchor.ip_ob, ax
	mov	ax, ipCur.ip_oln
	mov	ipAnchor.ip_oln, ax
cEnd

;*****************************************************************************
