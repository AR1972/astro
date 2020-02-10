	TITLE	uinhelp.asm - utilities for new help engine.
;***
;uinhelp.asm
;
;	Copyright <C> 1985-1988, Microsoft Corporation
;
;Purpose:
;	Utilities for the new help system.
;
;
;*******************************************************************************

	.xlist
	include	version.inc
	.list
	UINHELP_ASM = ON


	include cw/version.inc
	include cw/windows.inc
	include cw/edityp.inc

	IncludeOnce architec
	IncludeOnce heap
	Include     help.inc
	IncludeOnce parser
	IncludeOnce prstab
	IncludeOnce rtps
	IncludeOnce qbimsgs
	IncludeOnce ui
	IncludeOnce uiint
	IncludeOnce uimenu		
	IncludeOnce uinhelp		

assumes DS,DATA
assumes ES,DATA
assumes SS,DATA

	subttl	DATA segment definitions.
	page

sBegin	DATA
	externW	iMsgStatusLine		; current status line message
	externW iHelpId 		

; GetHelpMessage and MapLineToContext return codes

	globalB fHelpAlloc,0		; Non-zero if help system is doing
					; a far heap allocation
	BdlTemp	BDL	<>		; temporary BDL owner
	externB	bdlHelp			; BDL for keyword help (def in UIHELP.C)
	externW	szDialogHelpContext	; *sz context of current dialog help


	externB b$Buf1			; FILNAML-sized buffer
	externB b$Buf2			; 16-byte buffer
	HtSpot	EQU	b$Buf2		; Static HotSpot structure

	globalB HelpFlags,0,1		;status of the help system
	globalB HelpFlags2,0,1		;more help system state


;BUFFERING:
;
;   The Help system uses a circular buffer of buffers to hold the
;   text of help messages (called contexts) so that scrolling up
;   and down in a file does not have to be continuously hitting the
;   disk.  The buffers are organized as follows:
;
;	HelpBuffer  : Slot 1 : Slot 2 : Slot 3 : ... : Slot MAX_HELPBUFS :
;
;   Each active slot in the Static HelpBuffer is of type HelpBufType.  The
;   entries in HelpBuffer are organized as a circular list so that they
;   may grow on either end without moving the existing entries.  Each
;   active entry in HelpBuffer represents one context that is currently
;   stored in memory.
;

	staticW oFirstBuf,,1		;Offset of first logical buffer
	staticW oLastBuf,,1		;Offset of last logical buffer
	staticB numBuf,0,1		;number of active buffers in HelpBuffer
	staticB fNoDelBufs,0,1		; are we allowed to delete the
					; buffers during a ShrinkHelp?


	MAX_HELPBUFS	= 9		; Number of entries in HelpBuffer
	BUFSIZE 	= SIZE HelpBufType;The size of the entries in HelpBuffer

	BUFFERSIZE	= MAX_HELPBUFS * BUFSIZE ; Size of HelpBuffer
	staticB	HelpBuffer,0,BUFFERSIZE
	BUFFEREND	EQU (OFFSET DGROUP:HelpBuffer) + (BUFFERSIZE)

	DbPub	HelpBuffer

	globalW iCurRequestLine,UNDEFINED,1 ; line # to get attributes for,
					; or UNDEFINED if no attributes
					; available
	globalW oCurTopic,0,1		; offset to the current topic
					; (0 if no current topic)

	globalW iFileSize,0,1		; Size of current help topic
	externW iStaticHelpTopic	; High word of NC during searches

	externW WndHelp 		; Help Window structure
	externW efHelp			; Editmgr structure for Help Wnd.

;QUEUE MANAGEMENT
;
;  In order to support help history (Ctrl-F1), we keep a LIFO queue
;  of the last 20 items that we requested help on.  The items are stored
;  as a circular list, with a pointer to the next item to be filled in
;  and a count of the number of items that exist.
;
;
;  This list (of type HelpHistEntry) is contained in a BDL, with the first
;  few bytes of the BDL containg a header (type HelpHistHeader) of useful
;  information
;

	BdlHelpHist BDL <>		; BDL for help backup (^F1) list

	ncInitial DD 0			

	HelpHistEntry STRUC
	    HH_HiWord	DW ?
	    HH_LoWord	DW ?
	HelpHistEntry ENDS

	HelpHistHeader STRUC		
	    HH_Used	DW ?		; number of history items in use
	    HH_First	DW ?		; offset into seg of next free item
	HelpHistHeader ENDS		

	NUM_HISTORY	= 20		; number of items that can be saved
	HELPHIST_BEGIN = Size HelpHistHeader  ; offset of first item in segment
	HELPHIST_END   = HELPHIST_BEGIN + NUM_HISTORY * SIZE HelpHistEntry
					; end of the last item

;MAGIC EDIT MANAGER Variables
;
;   The following magic locations in the edit manager are used:
;
;	EfHelp.EF_ipCur_ob     = Current column in help window
;	EfHelp.EF_ipCur_oln    = Current line in help window
;	EfHelp.EF_pdCur_olnTop = Top line of help window
;	EfHelp.EF_pdCur_obleft = Leftmost column of help window
;
;	WndHelp.arcClipping.ayBottomArc, WndHelp.arcClipping.ayTopArc
;		specify the top and bottom of the window.  I believe
;		that they are in screen coordinates.
;
;	One thing to note:  The Edit Manager uses 0 relative coordinates
;	while the helpengine (and variable help) use 1 relative coordinates.
;	Thus, you have to be careful in converting from one to another.
;
;	There are only 2 places that a line or column number is stored:
;	  - Bookmarks (0 relative, as the HelpEngine is not involved)
;	  - iCurRequestLine (1 relative, because it is a HelpEngine line#)
;


;HELP System ENTRY/EXIT conditions
;
;  Anytime that code for the help system is executing, we must have the
;  following conditions met:
;
;	1.)  As close as possible to the point where we could display a
;	     dialog box or message box receiveing ANY input from the user
;	     we must have HLP_INHELP set to prevent recursively entering the
;	     help system.  Currently, this occurs in 2 places:
;		- Displaying "Help File Not Found"
;		- Displaying "INsufficient memory for help system"
;
;	2.)  The help system must be active to the level that is needed to
;	     implement the desired functionality.  There are three levels:
;
;	     a.) Help Engine shut down - No functionality at all possible.
;
;	     b.) Help Engine Active, no current topic.
;		  HLP_GOTBUF is true, but oCurTopic = 0.
;
;	     c.) Help Engine Active, Current topic available.
;
;
;	     The following assumptions and conditions are used in checking
;	     entry conditions:
;
;	     - If the help window is open, we must be at level C.
;	     - If the help window is closed, we may be at A or B.
;	     - If we get a message from the help window, it must be open (C).
;	     - If StartHelp returns 0, you are in either B or C.
;
;	3.)  Any allocation could end up causing CompressHelp to be called.
;	     Thus, there has to be flag set ANYTIME we are in the help system
;	     so that CompressHelp will not yank the help system out from
;	     underneeth itself.  Currenly, there are only 2 entry points
;	     for help:
;		- Help()
;		- HelpWndProc()


sEnd	DATA

	externFP B$IFOUT		; Convert number to string
	externFP HelpOpen		; Misc Help engine routines
	externFP HelpNc
	externFP HelpNcCb
	externFP HelpLook
	externFP HelpDecomp
	externFP HelpXRef
	externFP HelpGetLine
	externFP HelpHlNext
	externFP HelpNcNext
	externFP HelpSzContext
	externFP HelpClose		
	externFP HelpShrink		
	externFP _atoi			; CONSIDE: use runtime call for this
	externFP fEditorActive		
	externFP fQhelpActive



sBegin	UI
assumes CS,UI

	staticB szHelpFileEdit,<"EDIT.HLP",0>	; main help file name
	staticB szHelpFileQhelp,<"HELP.HLP",0>	; qhelp help file name
	staticB szHelpFileInterp,<"QBASIC.HLP",0> ; second help file name

	externNP MoveCursorPwndCur	; Move cursor of current window
	externNP MoveCursorPwnd 	; Move cursor of specified window
	externNP WndHelpOpen		; Open help window to a given size
	externNP WndHelpClose		; Close the help window
	externNP SetHelpKeyword		; put keyword into help syntax menu

	externNP HelpBack		

	subttl	Main Help EntryPoints
	page


;***
;KeywordHelp : Display help for a keyword
;
;Purpose:
;	This routine implements SHIFT-F1.  If the cursor is on a keyword
;	in a window, then display help for this keyword in the help window
;	(opening it if needed). If there is no text selected, or it is
;	not a keyword, then return a non-zero value to indicate that F1
;	should be done.
;
;Entry:
;	None.
;
;Exit:
;	None
;
;Uses:
;	AX,BX,CX,DX
;
;****

cProc	KeywordHelp,<NEAR,PUBLIC>,<SI>	
cBegin
	cCall	StartHelp		; make sure we have the help system
	or	ax,ax			; did we succeed?
	jne	KeywordHelpExit 	; no, just return

	cCall	KeywordHelpLookup	; AX = length of current keyword
					; (text placed in bufStdMsg)
	or	ax,ax			; is there a current keyword?
	jne	CurKeyword		; brif so
Moo:
	cCall	CowMoo			; -- beep speaker & return
	jmp	short KeywordHelpExit	
CurKeyword:

	DbChk	HoldBuf1		; grab B$Buf1

	mov	ax,offset dgroup:bufStdMsg ; where the topic was placed
	mov	bx,offset dgroup:B$Buf1 ; new home for the topic string
	push	bx			; param to DisplayHlpWndSz

	cCall	fstrcpy2,<DS,bx,DS,ax>	


	cCall	DisplayHlpWndSz 	; try to display the help

	DbChk	FreeBuf1		; release the buffer

	or	al,al			; Did we succeed with engine hlp
	je	KeywordHelpExit_Ok	; yes, exit.

	test	al,HELP_HANDLED 	; have we displayed anything to
					; the user?
	jnz	KeywordHelpExit 	; yes, just exit
	jmp	short Moo		; otherwise tell user about it

KeywordHelpExit_Ok:
	mov	[uierr],0		; clear any leftover errors
KeywordHelpExit:
cEnd

;***
;KeywordHelpLookup : Find help for a keyword
;
;Purpose:
;	Added with revision [5].
;
;	Looks up the keyword, enables/disables the HelpSyntax menu item, and
;	puts the keyword into the HelpSyntax menu item.
;
;Entry:
;	None.
;
;Exit:
;	AX : =0 if no keyword was found
;	    length of keyword (NZ) if keyword was found
;Uses:
;	AX,BX,CX,DX
;
;****

cProc	KeywordHelpLookup,<NEAR,PUBLIC>
cBegin

	mov	ax,offset dgroup:bufStdMsg
	mov	bx,10			; maximum length of word  (10 is magic)
	mov	cx,GEW_HELPMASK 	; do not include . or ! in search
	cCall	GetEditWordMask,<ax,bx,cx> ; AX = length of word retrieved
	push	ax			; save for return value

	; enable/disable help syntax menu item based on AX (0 ==> disable)
	or	ax,ax			; if NZ, set low bit for stupid-ass
	jz	disableit		; COW C function
	mov	al,1
disableit:
	mov	bx,midHelpSyntax
	cCall	EnableMenuItem,<bx,ax>

	; copy current topic name into help syntax menu buffer
	mov	ax,offset dgroup:bufStdMsg
	cCall	SetHelpKeyword,<ax>

	pop	ax			; return AX = length of keyword
cEnd



;***
;SelectHotLink - Go to the hot link that is specified in the window coords
;
;Purpose:
;	Look in the help file given by the line number and column
;	number.  If there is a hot link at this position, go to the item
;	that it links to.  Otherwise, just returns.
;
;Entry:
;	(rx,ry) - virtual line coordinates of the location of a
;		  supposive hot spot.
;	beep	- TRUE if we are suspose to beep when we don't find anything
;
;Exit:
;	AX = 0 (FALSE) if a hot link was selected
;
;Uses:
;	Per Convention
;****

cProc	SelectHotLink,<PUBLIC,NEAR>,<SI,DI>
parmW	rx
parmW	ry
parmB	GiveBeep
cBegin

	;Since we got a keyboard message from the help window, it must be
	;open, therefor we must be initialized.

	DbAssertTst HelpFlags,ne,HLP_GOTBUF,UI,<SelectHotLink:Buffers Not Initialized>


	DbChk	HoldBuf2		; lock down the HotSpot
	DbChk	HoldBuf1		; for composed hotlink names

	mov	bx,oCurTopic		; get ptr to topic
	lea	bx,[bx].bdlHelpText.BDL_seg ; BX = handle to data seg
	mov	cx,rx
	inc	cx			; make column 1 relative
	mov	HtSpot.colHS,cx 	
	mov	cx,ry			; get line number
	inc	cx			; make it 1 relative
	mov	HtSpot.lineHS,cx	; and stick in HotSpot structure
	mov	ax,OFFSET DGROUP:HtSpot
	xor	cx,cx
	and	HelpFlags,NOT (HLP_FAILFNF OR HLP_FAILOOM) ; clear errors
	cCall	HelpXRef,<BX,CX,DS,AX>	;Get context string for topic
	or	ax,ax			; did we get a topic
	jnz	GotSelection		; yes, go display it
	test	HelpFlags,HLP_FAILFNF OR HLP_FAILOOM ; is error handled?
	jnz	NoBeep			; yes, do not beep
SelectHotLink_Beep:
	cmp	GiveBeep,0		; should we give a beep?
	je	NoBeep			; no, just exit with error code
	cCall	CowMoo			; beep cow's speaker
NoBeep:
	mov	ax,sp			; return with non-zero value
	DbChk	FreeBuf2		; release the HotSpot
	DbChk	FreeBuf1		;  and our other buffer
	jmp	short NoSelection	; and exit

GotSelection:
	;Copy context string into free part of Buf2 and display it in a help
	;window. The source string is in HtSpot.pXrefHS and is 0 terminated
	;We can not use the source directly, as it points into a BDL, and
	;we are not guarenteing a locked heap.

	push	ds				; set ES = DS = DGROUP
	pop	es				
	mov	di,offset dgroup:b$buf1 	; ES:DI = destination
	push	di				; parm for DislplayHlpWnd
	lds	si,DWord Ptr HtSpot.pXrefHS	; DS:SI = source

CopyLoop:
	lodsb					; copy the byte
	stosb					
	or	al,al				; is it the 0 termiantor
	jne	CopyLoop			; no, do next byte
	lodsw					; Copy next word
	stosw					; incase it was local

	push	es				; restore DGROUP
	pop	ds				

	DbChk	FreeBuf2			; release the HotSpot

	; Check to see if the context is !B.  If it is, then we should
	; execute a HelpBack command instead of displaying the context

	cmp	WORD PTR b$Buf1,'B!'		; is it !B?
	jne	NotHelpBack			; no, go display it
	cmp	BYTE PTR b$Buf1+2,0		; is it 0 terminated?
	jne	NotHelpBack			; no, go display it
DbAssertRel	pwndAct,e,<OFFSET DGROUP:wndHelp>,UI,<SelectHotLink:Not in help window>
	pop	ax				; restore stack (param to
						;  DisplayHlpwndSz not used)
	xor	ax,ax				; move to (0,0) so we will
	cCall	MoveCursorPwndCur,<ax,ax>	; goto previous topic
	DbChk	FreeBuf1			; incase HelpBack wants it
	call	HelpBack			; do the help back
	jmp	short ExitSuccess		; and exit

NotHelpBack:
	cCall	DisplayHlpWndSz 		; display text
DbAssertRelB	al,ne,HELP_NF,UI,<SelectHotLink:Returned hot link not found>
						;ignore any errors
	DbChk	FreeBuf1			; release context string

ExitSuccess:
	xor	ax,ax				; return value of success
						; this way we will not put
						; up an error box, then
						; do something useful.
NoSelection:

cEnd



;Rewrote with [5]
;***
;FillContextNext - Fill the next buffer with the desired context information
;
;Purpose:
;	Read a help context into the help buffer given the position that
;	is previous to it.  The information needed for the new context
;	is obtained from the context given.  This routine will read in
;	as many contexts as desired.
;
;	Note: this routine does not return any errors, as its caller
;	does not check the error code (this is used to fill the buffer
;	with topics after the current topic, so if we could not read
;	in a topic for any reason, then we will just leave the buffer
;	partially empty).
;
;Entry:
;	BX = current buffer
;	DI = number of context to read
;
;Exit:
;	oLastBuf = Last valid buffer
;	numBuf updated
;
;****

cProc	FillContextNext,<NEAR>,<DI>
cBegin

DbAssertRelB	fNoDelBufs,ne,0,UI,<FillContextNext:Caller did not set fNoDelBufs>


cEnd


;Added with [5]
;***
;GetHelpMsgNcFill - get a help message from a context number, fill in fields
;
;Purpose:
;	Calls GetHelpMsgNc, and fills in all the fields of the buffer except
;	for the first line number of the context.
;
;Entry:
;	DX:AX = context number to be filled in
;	BX = ptr to context
;Exit:
;	if (AL = 0)
;	    BX = ptr to context
;	    all fields of context are filled in
;	else
;	    AL = HELP_HANDLED.
;
;****
cProc	GetHelpMsgNcFill,<NEAR>
cBegin
	push	bx				; We need to preserve BX
	mov	Word Ptr [bx].ContextNum,ax	; save Context Num in Buffer
	mov	Word Ptr [bx].ContextNum+2,dx
	lea	cx,[bx].bdlHelpText		; point to BLD
	cCall	GetHelpMsgNc,<dx,ax,cx> 	; get message
						; (AL = return code)
	pop	bx				; BX = *Buffer

Fill_Exit:
cEnd

;***
;GetHelpMsgSz - get a help message from a context string
;GetHelpMsgNc - get a help message from a context number
;
;Purpose:
;	This routine calls the help engine to obtain the help text
;	associated with a context string.  The text is returned in a BDL.
;	No buffering of the help context is performed.
;
;	NOTE: Does not require oFirstBuf or oLastBuf to be set.
;
;Entry:
;	szContext : pointer to context string
;	BdlText : pointer to bdl owner structure.
;
;Exit:
;	AL = HELP_OK (0) if success.
;	AL = HELP_HANDLED, HELP_NF
;
;Uses:
;	Per convention
;
;Exceptions:
;	
;****
cProc	GetHelpMsgSz,<NEAR,PUBLIC>
parmW	szContext
parmW	BdlText
cBegin
DbAssertTst	HelpFlags,ne,HLP_GOTBUF,UI,<GetHelpMsgSz:HLP_GOTBUF false>
	mov	ax,szContext		; AX = context string
	cCall	CalcNc,<AX>		; DX:AX = nc for context string
	jcxz	GetHelpMsgSzExit	; return with error code (AL) if error

	cCall	GetHelpMsgNc,<DX,AX,BdlText> ; get message
GetHelpMsgSzExit:
cEnd

DbPub	GetHelpMsgNc
cProc	GetHelpMsgNc,<NEAR>,<SI,DI>
parmW	ncHigh
parmW	ncLow
parmW	BdlText
cBegin
DbAssertTst	HelpFlags,ne,HLP_GOTBUF,UI,<GetHelpMsgNc:HLP_GOTBUF false>
DbAssertRelB	fHelpAlloc,ne,0,UI,<GetHelpMsgNc:fHelpAlloc not set>

	mov	di,ncHigh		;set DI:SI to context string
	mov	si,ncLow

	and	HelpFlags,NOT (HLP_FAILFNF OR HLP_FAILOOM) ; clear flags

	cCall	HelpNcCb,<DI,SI>	; AX = #bytes for decompressed topic

	xchg	ax,cx					; save size in CX
	test	HelpFlags,HLP_FAILFNF OR HLP_FAILOOM	; did we fail it?
	mov	al,HELP_HANDLED 			; assume so
	jnz	GetHelpMsgNcExit_Near			; exit if error

	mov	bx,offset DGROUP:BdlTemp
	cCall	BdlAlloc,<BX,CX>	; alloc bdl for compressed data
	or	ax,ax			; OOM error?
	jz	GetHelpMsgOOM		; yes, go handle it

	lea	bx,[BdlTemp.BDL_seg]	; BX = handle to data segment
	xor	ax,ax			; offset 0
	cCall	HelpLook,<DI,SI,BX,AX>	; AX = # bytes uncompressed
	xchg	cx,ax			; CX = # bytes uncompressed
	jcxz	FreeExitError		; brif error -- release bdl & exit

	inc	fNoDelBufs		; don't delete buffers

	mov	bx,BdlText		; bx = *BDL owner
	cCall	BdlAlloc,<BX, CX>	; alloc BDL for uncompressed data
	or	ax,ax			; ZF ==> error
	jnz	DecompressHelp		; no error, go decompress help topic
	cCall	GiveHelpOOM		; generate OOM error
	dec	fNoDelBufs		; restore buffer flag
	jmp	short FreeExitError	; free Bdl and exit

GetHelpMsgOOM:
	cCall	GiveHelpOOM		; signal an OOM error
	mov	al,HELP_HANDLED 	
GetHelpMsgNcExit_Near:			
	jmp	Short GetHelpMsgNcExit	; brif so -- give error and exit

DecompressHelp:
DbAssertTst	HelpFlags,z,HLP_NOSHRINK,UI,<GetHelpMsgNc:NOSHRINK set>
	or	HelpFlags,HLP_NOSHRINK	; don't shrink during HelpDecomp
					; otherwise it will fail.
	mov	bx,BdlText		; bx = *BDL owner (could have moved)
	xor	ax,ax			; offset 0
	lea	dx,[BX].BDL_seg 	; DX = handle of destination seg
	lea	bx,[BdlTemp.BDL_seg]	; BX = handle of source segment
	cCall	HelpDecomp,<BX,AX,DX,AX,DI,SI>
					; decompress help text
	dec	fNoDelBufs		; reset buffer delete flag
	and	HelpFlags,NOT HLP_NOSHRINK ; reenable shrinking.

	or	ax,ax			; did we get an error?
	jz	FreeExit		; no, free temp BDL and exit
	mov	ax,BdlText		; get pointer to bdl
	cCall	BdlFree,<AX>		; and deallocate it

	test	HelpFlags,HLP_FAILOOM or HLP_FAILFNF ; have we reported?
	jnz	FreeExitError		; yes, get return code
	cCall	GiveHelpOOM		; report the error.

FreeExitError:
	mov	al,HELP_HANDLED 	; prepare to return error

FreeExit:
	push	ax			; save return code
	mov	ax,OFFSET DGROUP:BdlTemp
	cCall	BdlFree,<AX>		; release compressed data

	pop	ax			; AX = return code (NZ ==> success)

GetHelpMsgNcExit:

cEnd


	subttl	Display Text in the Help Window

;Rewrote with [5]
;***
;DisplayHlpWndSz - Display help in a window given a context string
;DisplayHlpWndNc - Display help in a window given a context number
;
;Purpose:
;	Displays a context in the help window given either a context
;	string (DisplayHlpWndSz) or a context number (DisplayHlpWndNc).
;	This routine will open the help window if it is not already
;	opened.
;
;Entry:
;	szContext : pointer to a context string  (DisplayHlpWndSz)
;			   or
;	DX:AX	  : context number		 (DisplayHlpWndNc)
;
;Exit:
;	AL = error code (HELP_OK, HELP_HANDLED, HELP_NF)
;
;Uses:
;	Per Convention
;
;****

cProc	DisplayHlpWndSz,<PUBLIC,NEAR>
parmW	szContext
cBegin

DbAssertTst	HelpFlags,ne,HLP_GOTBUF,UI,<DisplayHlpWndSz:HLP_GOTBUF false>

	mov	ax,szContext		;AX = context string
	cCall	CalcNc,<AX>		; DX:AX = nc for context string
	jcxz	HlpWndSzExit		; there was an error, exit
	cCall	DisplayHlpWndNc 	;display help for this context number

HlpWndSzExit:				;AL = return code
cEnd


cProc	DisplayHlpWndNc,<PUBLIC,NEAR>
cBegin

DbAssertTst	HelpFlags,ne,HLP_GOTBUF,UI,<DisplayHlpWndNc:HLP_GOTBUF false>

	cCall	BufferNc		; BX = ptr to new context
	or	al,al			; was there an error?
	jne	DisplayFailure		; yes, exit with error code

	push	bx			; save buffer pointer
	mov	dx,Word Ptr [bx].ContextNum+2  ; Get context number into
	mov	ax,Word Ptr [bx].ContextNum    ;  DX:AX
	cCall	RecordHelpHistory	; record context number for Ctrl-F1
	pop	bx			; restore buffer pointer

	lea	ax,[bx].bdlHelpText	
	push	ax			; save for call to GetHelpContextLen
	cCall	SizeHelpContext,<ax>	; get # lines in the help topic
					; sets iFileSize
	cCall	GetHelpContextLen	; AX = get suggested size of wnd
	cCall	OpenHelpWindow		; go open the window (to size AX)
	cCall	DrawDebugScr		; force help title to be redrawn

	xor	ax,ax			; return 0 for no-errors

DisplayFailure:
	cCall	CloseCurHelpFile	; shut down current help file, if
					; any opened. (preserves AX, DX)
cEnd




;***
;BufferNc - Given a context number, properly buffer in context
;
;Purpose:
;	This routine will refill the help buffer so that Nc is
;	present.  If Nc did not exist, the buffer is flushed and
;	refilled.
;
;Entry:
;	DX:AX	: Context number to be buffered
;
;Exit:
;	if (successful)
;		AL = HELP_OK  (=0)
;		BX = new buffer ptr
;		numBuf updated
;		oCurTopic = BX
;	else
;		AL = HELP_HANDLED
;
;****
cProc	BufferNc,<PUBLIC,NEAR>,<DI>
cBegin

	;See if the context number is already buffered

	xor	cx,cx
	mov	cl,numBuf		; scan numBuf contexts
	jcxz	DelNoBufs		; special case this...LOOP can't
	mov	bx,oFirstBuf		; Start at first context
CheckNext:
	cmp	ax,Word Ptr [bx].ContextNum ; check low word first
	jne	TryToLoop		; comparison failed, goto next one
	cmp	dx,Word Ptr [bx].ContextNum+2 ; check high word
	jne	TryToLoop		; comparison failed, goto next one
	jmp	FoundMatch		; we have the context already buffered
TryToLoop:
	add	bx,BUFSIZE		; move to next logical buffer
	cmp	bx,BUFFEREND		; did we go off the end?
	jb	Wrap5			; no, continue
	sub	bx,BUFFERSIZE		; adjust back to begining
Wrap5:
	loop	CheckNext		; try the next context

NotBuffered:

	
	; # buffers to free = # needed - # free
	;		    = MAX_HELPBUFS / 2 - (MAX_HELPBUFS - numBufs)
	;		    = numBufs - (MAX_HELPBUFS / 2)
	
	mov	cl,numBuf		; Calculate # buffers to delete
	sub	cl,MAX_HELPBUFS /2	
	ja	DelSomeBufs		; brif a positive number
	xor	cl,cl			; delete 0 buffers (makes
DelSomeBufs:				; oCurTopic == oLastTopic)
	xor	ch,ch			; CBW for CX
	push	ax			; save the context number
	push	dx
	call	FlushBuffer		; delete CX current contexts
	pop	dx			; restores context number
	pop	ax

DelNoBufs:
DbAssertRelB	numBuf,be,MAX_HELPBUFS,UI,<BufferNc: No buffers free after deleting some>

	mov	bx,oLastBuf		; Ptr to last buffer used
	cmp	numBuf,0		; stupid special case?
	jne	UseoLastBuf		; no, index off of oLastBuf
	mov	oFirstBuf,bx		; oFirstBuf = oLastBuf
	jmp	short NoWrap6		
UseoLastBuf:
	add	bx,BUFSIZE		; point to the next one
	cmp	bx,BUFFEREND		; did we go off the end?
	jb	NoWrap6 		
	sub	bx,BUFFERSIZE		
NoWrap6:
	cCall	GetHelpMsgNcFill	; get the help topic
	or	al,al			; any errors?
	jne	BufferNc_Exit		; yes, exit with error code

	inc	numBuf			; indicate one more buf in use

	;Fill the rest of the buffer with contexts after this one

	inc	fNoDelBufs		; do not delete buffers to get these
	mov	ax,MAX_HELPBUFS
	sub	al,numBuf		; AX = number of buffers free
	push	bx			; save it for exit
	xchg	di,ax			; set DI = number of free buffers

	mov	oLastBuf,bx		;set oLastBuf = last legal buffer

FillAnotherNext:
	mov	dx,Word Ptr [bx].ContextNum+2 ; DX:AX = current NC
	mov	ax,Word Ptr [bx].ContextNum
	push	bx
	cCall	GetNextNc		
	pop	bx
	je	FillContextDone 	; no more contexts

	add	bx,BUFSIZE		;Point BX at next buffer
	cmp	bx,BUFFEREND		; did we go off end of buffer?
	jb	NoWrap3 		;no, continue
	sub	bx,BUFFERSIZE		;wrap around to begining
NoWrap3:
	cCall	GetHelpMsgNcFill	;get message DX:AX into *(BX)
	or	al,al			; did an error occur
	jne	FillContextDone 	; yes, stop filling in buffers

	mov	oLastBuf,bx		; set oLastBuf = last legal buffer
	inc	numBuf			;record that a buffer is now in uses
	dec	di			;one more context use
	jne	FillAnotherNext 	;get another one if allowed
FillContextDone:

	;Ignore any errors, we have at least one buffer
	mov	uierr,0 		; clear any errors
	pop	bx			; pointer to buffer with our context

	dec	fNoDelBufs		; reset buffer delete pointer

FoundMatch:
	xor	ax,ax			; ax = 0 (no errors)
	mov	oCurTopic,bx		; update current buffer ptr.

BufferNc_Exit:
cEnd

;Rewrote with [45]
;***
;OpenHelpWindow - Open the help window and ready it to print text
;
;Purpose:
;	This routine will not only open the help window to the correct
;	size, but will adjust the size of the window to account
;	for the existance/non existance of the scroll bar.
;
;Entry:
;     ax = Size of window
;
;Exit:
;     None.
;****

DbPub OpenHelpWindow
cProc OpenHelpWindow,<NEAR>
cBegin
	cmp	pwndAct,OFFSET DGROUP:WndHelp ; in help window?
	je	InHelpWindow		; no, do not resize

	inc	ax			; add a line for scroll bar
InHelpWindow:
	cCall	WndHelpOpen,<AX>	; AX = actual size of window

	; We must always move the cursor to (0,0), otherwise the edit
	; manager may not realize that this is a new topic and would not
	; position it so that the first line is visible.

	xor	bx,bx			; cursor position is 0,0
	mov	ax,OFFSET DGROUP:wndHelp; window to receive the message
	push	ax			; save for second call
	cCall	MoveCursorPwnd,<AX,BX,BX> ; goto 0,0 in window

	cCall	GetInitialPos		; get initial position on screen

	cCall	MoveCursorPwnd,<DX,AX>	; goto hot link spot
cEnd

;***
;GetInitialPos - Calculate the initial cursor position in the help window
;
;Purpose:
;	Returns the initial cursor position in the help window.  This is
;	the (0 relative) coordinates of the first hot link (if it exists
;	and is visible), or (0,0).
;
;Entry:
;	None
;
;Exit:
;	(DX,AX) - initial Row/Column of the screen (0,0) or first hot spot
;
;Uses:
;	Per C Convention.
;****
cProc	GetInitialPos,<PUBLIC,NEAR>
cBegin
	DbChk	HoldBuf2		; lock down the hotspot

	test	HelpFlags,HLP_VARHELP	; are we in var help?
	jnz	Use00			; yes, there are no hotlinks

	mov	ax,1			; current row/column position
	mov	HtSpot.ColHS,ax 	
	mov	HtSpot.lineHS,ax	

DbAssertRel	oCurTopic,ne,0,UI,<GetInitialPos:oCurTopic invalid>
	mov	bx,oCurTopic
	lea	bx,[bx].bdlHelpText.BDL_seg
	mov	cx,OFFSET DGROUP:HtSpot
	dec	ax			; ax = 0
	cCall	HelpHlNext,<AX,BX,AX,DS,CX>
	or	ax,ax			; did we get a hot link?
	je	Use00			; no, use (0,0) as position
	mov	ax,HtSpot.ColHS 	; get position of hotlink
	mov	dx,HtSpot.lineHS	
	dec	dx			; make position 0 relative
	dec	ax
	
	; Make sure the hot link is visible.
	
	mov	cl,WndHelp.arcClipping.ayBottomArc
	sub	cl,WndHelp.arcClipping.ayTopArc ; CX = size of help window

	cmp	cl,dl			; within visible range?
	jae	GetInitialPos_Exit	; no, ignore the hotlink
Use00:
	xor	dx,dx
	xor	ax,ax

GetInitialPos_Exit:
	DbChk	FreeBuf2		; release HotSpot
cEnd

	subttl	Edit Manager Support Routines
	page



;Extracted and modified with revision [29].
;***
;GetHelpTitleStr - Returns the title for a given help topic
;
;Purpose:
;	Calculates the title, and returns it in bufStdMsg.
;	All titles are prefixed by "HELP: ".
;
;	Uses the ":n" information if the help file, if it exists.
;	Otherwise, returns the current context string.
;
;Entry:
;	pHelpBdl = *bdl containing decompressed help topic.
;	cbMaxSize = max title length allowed
;
;	If window help, curNc is the help context #
;	If dialog box help, szDialogHelpContext points to the current szContext
;
;Exit:
;	AX = number of characters in title
;	bufStdMsg = contains title.
;
;Uses:
;	Per Convention
;
;****
cProc	GetHelpTitleStr,<NEAR,PUBLIC>,<SI,DI>
parmW	pHelpBdl
parmW	cbMaxSize
cBegin

DbAssertTst	HelpFlags,nz,HLP_GOTBUF,UI,<GetHelpTitleStr:HLP_GOTBUF false>

	inc	fNoDelBufs		; do not delete buffers, we have
					; a generic ptr to a buffer

	; Setting fNoDelBufs is probably not needed as currently
	; pHelpBdl points to either oCurTopic or to a static
	; Bdl used by dialog help.	However, I doubt that we do any
	; allocs in this code, and I can not guarentee that pHelpBdl
	; is safe.


	mov	ax,MSG_HelpTitleQH	; first part of title ("MS-DOS Help: ")
	test	cmdSwitches,CMD_SW_QHELP ; /QHELP viewer?
	jnz	ghts1			;   YES, got title

	mov	ax,MSG_HelpTitle	; first part of title ("HELP: ")
ghts1:
	cCall	ListStdMsg,<AX> 	; put in bufStdMsg

	push	ax			; save length of static portion
	add	ax,OFFSET DGROUP:bufStdMsg ; ds:di = useful buffer
	xchg	di,ax
	mov	al,'n'			; look for ":nTITLE"
	cCall	GetHelpControlInfo,<pHelpBdl,cbMaxSize,ax> ; SI = *embedded title, if any
	or	si,si			; title found?
	jnz	AppendTitle		; brif so

	mov	si,szDialogHelpContext	; use current context string as title
					; for dialog box help
	cmp	pHelpBdl,offset DGROUP:bdlHelp	; title for dialog box help?
	je	AppendTitle		; brif so -- append string to title

	mov	bx,oCurTopic		; ptr to current topic data
	or	bx,bx			; is it valid?
	je	GetHelpTitleStr_Fail	; no, use MSG_HelpTitle for now

	push	ds			;push far address to load szContext
	push	di
	push	Word Ptr [bx].ContextNum+2 ; push context number
	push	Word Ptr [bx].ContextNum
	cCall	HelpSzContext		;Get text
	or	ax,ax			; did we succeed?
	je	GetHelpTitleStr_Fail	; no, exit with error

	;Strip off the filename at the begining of the context

	cCall	szSrchExcl,<di> 	; search for '!' in *di
	DbAssertRel ax,ne,0,UI,<GetHelpTitleStr: Illegal string from HelpSzContext>
	inc	ax			; point to chr beyond '!'
	xchg	si,ax			;di = *buffer, si = *szContext

AppendTitle:				; append title to bufStdMsg
					; DS:SI= *title
					; DS:DI= where to put it

	pop	bx			; BX = length of static portion
	push	ds			; ES = DS for string ops
	pop	es			

AnotherChar:				
	lodsb				; append szTitle to "HELP:" in
	stosb				; bufStdMsg
	inc	bx			; seen another char
	or	al,al			; more chars to do?
	jnz	AnotherChar		; brif so
	dec	bx			; don't  include NULL in count

	cmp	bx,cbMaxSize		; is it larger than we want to return
	jbe	GetHelpTitleStr_Exit	; no, exit
	mov	bx,cbMaxSize-1		; return maximum size (without NULL)
	mov	byte ptr [bufstdMsg+bx+1],al ; truncate the string

GetHelpTitleStr_Exit:			
	xchg	ax,bx			; return count in AX

	SKIP1_PSW			; skip the pop ax
GetHelpTitleStr_Fail:			
	pop	ax			; get length in case of error
	dec	fNoDelBufs		; reenable buffer deletes.
	DbAssertRel ax,be,cbMaxSize,UI,<GetHelpTitleStr: title too big>
cEnd

;Added with revision [29].
;***
;GetHelpContextLen - Returns the length to use for a given help topic
;
;Purpose:
;	Returns the suggested # lines to use to display a topic.
;	Uses ":lLENGTH" if it exists. Otherwise, returns the total # lines
;	in the topic.
;
;Entry:
;	pHelpBdl = *bdl containing decompressed help topic.
;
;Exit:
;	AX = number of lines to use
;
;Uses:
;	Per Convention
;
;****
cProc	GetHelpContextLen,<NEAR,PUBLIC>,<SI,DI>
parmW	pHelpBdl
cBegin
	inc	fNoDelBufs		; do not delete buffers, we have
					; a generic ptr to a buffer

	; Setting fNoDelBufs is probably not needed as currently
	; pHelpBdl points to either oCurTopic or to a static
	; Bdl used by dialog help.	However, I doubt that we do any
	; allocs in this code, and I can not guarentee that pHelpBdl
	; is safe.

	mov	di,OFFSET DGROUP:bufStdMsg ; ds:di = useful buffer
	mov	cx,6			; max len we care about
	mov	al,'l'			; look for ":l<LENGTH>"
	cCall	GetHelpControlInfo,<pHelpBdl,cx,ax> ; SI = *embedded len,
	or	si,si			; length found?
	jz	GetTotalLines		; brif not -- get the line count

	push	si			; arg on stack
	call	_atoi			; AX = result
	pop	bx			; clean stack (C calling conventions)
	jmp	SHORT GetHelpContextLen_Exit

GetTotalLines:
	cCall	SizeHelpContext,<pHelpBdl> ;AX = # lines in context
					; sets iFileSize

GetHelpContextLen_Exit:
	dec	fNoDelBufs		 ; reenable buffer deletes.
cEnd


;Extracted and modified with revision [29].
;***
;GetHelpControlInfo
;
;Purpose:
;	Returns the control information in a given help context, if any.
;
;Entry:
;	pHelpBdl = * bdl containing decompressed help topic
;	cbMaxSize = biggest line to get.
;	cChar = control char to look for (usually 'n' or 'l').
;
;	DS:DI = * place to put result
;
;Exit:
;	DS:SI = * embedded help control info
;
;Uses:
;	Per convention, plus SI
;****

cProc	GetHelpControlInfo,<NEAR>
parmW	pHelpBdl		; * bdl of current help context
parmW	cbMaxSize		; biggest string allowed
parmW	cChar			; control char to search for (l or n)
cBegin

DbAssertRelB	fNoDelBufs,ne,0,UI,<GetHelpControlInfo:fNoDelBufs not set>

	; This routine does not have to worry about error handling for
	; HelpGetLine.	If there is any error in trying to get the
	; proper line, we will just assume that there were no more
	; lines.

	mov	bx,pHelpBdl	; BX = *bdl of current help topic

	GETSEG	es,[bx].BDL_seg,bx,SIZE	; ES = context data segment
	mov	ax,-1			; AX = undefined for blasting help
	mov	es:[lnCurTH],ax 	; reset current line counter
					; so engine doesn't get confused
	xchg	es:[linCharTH],al	; don't ignore any lines
	push	ax			; save line rejection char

	xor	si,si			; start at line 0

AnotherLine:
	inc	si			; advance to next line
	lea	bx,[bx].BDL_seg 	; BX = handle of data segment
	xor	ax,ax			; AX = context data offset (0)
	cCall	HelpGetLine,<si,cbMaxSize,ds,di,bx,ax>
	mov	bx,pHelpBdl		; BX = *bdl of current help topic
	or	ax,ax			; any more lines?
	jz	NoEmbeddedInfo		; brif not
	mov	ax,[di]			; AX = first 2 chars
	cmp	al,':'			; ":" command?
	jne	NoEmbeddedInfo		; brif not -- they must come first
	cmp	ah,byte ptr (cChar)	; the one we are searching for?
	jne	AnotherLine		; brif not -- get another line

	mov	si,di			; DS:SI = *szTitle
	inc	si			; skip the ":"
	inc	si			; skip the control char
	SKIP2_PSW			; skip the XOR SI,SI
NoEmbeddedInfo:
	xor	si,si			; no szTitle found
	pop	ax			; restore old line rejection char
	GETSEG	es,[bx].BDL_seg,bx,SIZE ; ES = context data segment
	xchg	es:[linCharTH],al	; restore old line rejection char
DbAssertRel	AX,E,-1,UI,<GetHelpControlInfo: AX should be -1>
	mov	es:[lnCurTH],ax 	; reset current line counter
					; so engine doesn't get confused

cEnd

;***
;SizeHelpContext
;
;Purpose:
;	Added with revision [2].
;
;	Returns total # of lines of help text for a given context, for use
;	in dialog box re-sizing, and scroll bar updating.
;
;
;Entry:
;	bdlText	= * BDL containing a decompressed help context.
;
;Exit:
;	AX = iFileSize = # lines of help text present in this context.
;
;Uses:
;	Per Convention
;
;****

cProc	SizeHelpContext,<PUBLIC,NEAR>,<SI,DI>
ParmW	bdlText
cBegin
	mov	di,bdlText			; DI = *context BDL
	xor	si,si				; SI = count of # lines

Again:
	inc	si				
	mov	cx,offset DGROUP:bufStdMsg	; CX = &dummy sz
	mov	dx,CB_bufStdMsg			; DX = cbMax
	xor	ax,ax				; AX = context data offset (0)
	lea	bx,[di].BDL_seg 		; BX = handle of data seg
	cCall	HelpGetLine,<SI,DX,DS,CX,BX,AX>	
	or	ax,ax				; success?
	jnz	Again				; brif so -- try again

	xchg	ax,si				; AX = # lines + 1
	dec	ax				; return AX = # lines
	mov	iFileSize,ax			; save # lines
cEnd


;***
;CmdHelpClose - Close the help window (from ESC)
;
;Purpose:
;	Close the help window.	Attached to the ESC accelerator.
;
;Entry:
;	None.
;
;Exit:
;	None.
;
;Uses:
;	Per C Convention
;
;****

cProc	CmdHelpClose,<PUBLIC,NEAR>
cBegin
	mov	oCurTopic,0		; indicate no valid help
	cCall	WndHelpClose		; close the window
cEnd


	subttl	Miscellanious Help Routines
	page

;***
;GetNextNc	- Advance a NC to the next topic
;
;Purpose:
;	Get the next physical context number given a context num.
;	Will not wrap from one file to another.
;
;Entry:
;	DX:AX = current context number
;
;Exit:
;	if ZF set (zero)
;	   No next topic
;	   CX = 0
;	   AL = error code	(HELP_HANDLED, HELP_NF)
;	else
;	   DX:AX is context number
;	   CX != 0
;
;Uses:
;	Per C Convention
;
;****

cProc	GetNextNc,<PUBLIC,NEAR>
cBegin
	and	HelpFlags,NOT (HLP_FAILFNF OR HLP_FAILOOM); clear errors
	push	dx				; value to compare against
	push	dx				; push current context number
	push	ax
	cCall	HelpNcNext			; get next Context Num
	pop	bx
	cmp	bx,dx				; is it in a different file?
	jne	GetNextNc_Err			; yes, give error
	mov	cx,ax				; check if no more contexts
	or	cx,dx				; NZ & CX != 0 if no error
	jnz	GetNextNC_exit			; exit if success
GetNextNc_Err:
	mov	al,HELP_HANDLED 		; assume handled error
	test	HelpFlags,HLP_FAILFNF OR HLP_FAILOOM ; was it?
	jnz	GetNextNc_Err2			; yes, use this code
	mov	al,HELP_NF			; context not found
GetNextNc_Err2:
	xor	cx,cx				; set CX = 0, ZF
GetNextNc_exit:
cEnd


;***
;CreateContext - convert a number into a context string
;char *(NEAR CreateContext(iContextNumber ))
;
;Purpose:
;	This routine takes a number and an initial character and
;	creates a context string out of it. The context string will
;	be of the form	"-1234" were '-' is the character and "1234"
;	is an ASCII representation of the number.  The string is
;	returned in a static buffer.
;
;	The number must be 0 < iContextNumber < 32768
;
;Entry:
;	iContextNumber : integer which will be remaining digits
;
;Exit:
;	AX = pSz : near pointer to string with results
;
;Uses:
;	AX, BX, CX, DX.
;
;****

cProc	CreateContext,<NEAR,PUBLIC>
	parmW	HelpId			
cBegin
	mov	al,VT_I2		;Format an integer
	lea	bx,HelpId		; BX = ptr to number to format

	DbAssertRel [bx],g,0,UI,<CreateContext: Bad Help ID>

	call	B$IFOUT 		;BX = address of 0 terminated string

	mov	BYTE PTR [bx],PREFIX_MESSAGE ; Replace space with character
					; that belongs in first postion
	xchg	ax,bx			;return *sz in AX
cEnd

;***
;ShrinkHelp - Compress Help system memory usage
;
;Purpose:
;	Reduce the amount of memory that the help system uses without
;	impeeding the functionallity of the help system.
;
;	WARNING!!!
;	    Any time that this routine may be called (i.e. any time an alloc
;	    is done), you must either set fNoDelBufs or guarentee that
;	    numBuf = 0, or guarentee that oCurTopic, oFirstBuf and oLastBuf
;	    are correct.
;
;Entry:
;	None.
;
;Exit:
;	None.
;
;Uses:
;	Per C Convention.
;
;****
cProc	ShrinkHelp,<PUBLIC,FAR>
cBegin

	DbHeapMoveOff			; FH callback can't cause movement

	test	HelpFlags,HLP_NOSHRINK	; are we allowed to call HelpShrink?
	jnz	NoHelpShrink		; no, check for deleting buffers
	cCall	HelpShrink		; Tell help engine to shrink down
NoHelpShrink:


	cmp	fNoDelBufs,0		; Can we delete help buffers also?
	jnz	ShrinkHelp_Exit 	; no, just return

	xor	cx,cx
	mov	cl,numBuf		; cx = # buffers in use
	jcxz	ShrinkHelp_Exit 	; exit if there are none
	cmp	oCurTopic,0		; do we have a current topic?
	jz	NoCurrentTopic		; no, do not keep it in memory
	dec	cx			; do not delete the current topic
NoCurrentTopic:
	cCall	FlushBuffer		; delete CX buffers
ShrinkHelp_Exit:


	DbHeapMoveOn			; remove our lock on the heap
cEnd

;***
;CompressHelp - Close down the help system
;
;Purpose:
;	This routine is called before we execute any user code so that
;	the help system can release all the memory that it does not need.
;
;Entry:
;	None.
;
;Exit:
;	None.
;
;Uses:
;	AX,BX,CX,DX
;
;Preserves:
;	ES
;****

cProc	CompressHelp,<PUBLIC,FAR>,<ES>	
cBegin
	DbHeapMoveOff			; we are called from FHAlloc
					; not allowed to move heaps

	test	HelpFlags,HLP_GOTBUF	; Help system started?
	jz	NoFreeHelpBuffer	; brif not -- nothing to do

	cmp	fHelpAlloc,0		; doing a help alloc?
	jnz	NoFreeHelpBuffer	; yes, do not shut down help system

	xor	ax,ax			; Nc of 0 => close all files
	cCall	HelpClose,<AX,AX>	; Shut down the help engine

	; we will ignore any file errors on the close


	cCall	DiscardHelpBookMarks	; free bookmark ptrs into help


NoHelpBuffer:
	xor	ax,ax			; get a convenient 0
	mov	iStaticHelpTopic,ax	; clear current search topic
	mov	oCurTopic,ax		; clear current topic ptr.
	mov	iCurRequestLine,ax	; clear GetHelpLine succeeded flag

	; zero the initial context number
	mov	WORD PTR ncInitial,ax	
	mov	WORD PTR ncInitial+2,ax

	or	HelpFlags,HLP_COMPRESS	; flag help has been compressed
	cCall	DrawDebugScr		; make sure screen is redrawn.

DbAssertRel	BdlHelpHist.BDL_Seg,ne,UNDEFINED,UI,<CompressHelp:HLP_GOTBUF is true, but no HistoryBuf Allocated>
	PUSHI	ax,<OFFSET DGROUP:BdlHelpHist>	; Free up the history buf
	cCall	BdlFree 		
	mov	cl,numBuf		; number of buffers to deallocate
	xor	ch,ch			
	cCall	FlushBuffer		; Delete all the help buffers
	and	HelpFlags,NOT HLP_GOTBUF ; indicate buffer unallocated

NoFreeHelpBuffer:
	DbHeapMoveOn			; remove our heap lock
cEnd



; ReWrote with [39]
;***
;GiveHelpOOM - Queue a message to give OOM for Help
;
;Purpose:
;	Indicate to the error handling code that an OOM error has occured
;	in the help system.  This routine may be called as many times as
;	desired before DisplayHelpOOM is called, and the error will only
;	be reported once.
;
;Entry:
;	None.
;
;Exit:
;	None.
;
;Uses:
;	By Convention
;
;****

cProc	GiveHelpOOM,<PUBLIC,NEAR>
cBegin
	or	HelpFlags,HLP_FAILOOM	; indicate We failed due to OOM
	mov	ax,MSG_HelpOOM		
	cCall	SetUiErr,<AX>		
cEnd

;Added with [39]
;***
;DisplayHelpOOM - Display an OOM error
;
;Purpose:
;	Displays an OOM error messages box for help, closes the help window,
;	and resets static variables.
;
;Entry:
;	None.
;
;Exit:
;	None.
;
;****

cProc	DisplayHelpOOM,<PUBLIC,NEAR>
cBegin

DbAssertTst	HelpFlags,e,HLP_INHELP,UI,<DisplayHelpOOM: Recursion lock already set>
	or	HelpFlags,HLP_INHELP	; set the recursion lock

	
	;We must save the HelpId in case the user does something like:
	;   Error 1	      -> displays a dialog box
	;   F1		      -> gets help, runs out of memory
	;   we call MsgBoxStd -> blows away iHelpId
	;   OK to message     -> back to original dialog box
	;   F1		      -> trys help again, iHelpId is invalid
	
	push	iHelpId

	PUSHI	ax,MB_OK		; Box has OK button
	PUSHI	ax,MSG_HelpOOM		; message to be displayed
	call	MsgBoxStd		; put up a dialog box
	and	HelpFlags, NOT HLP_INHELP ;remove recursion lock
	push	Word Ptr fHelpAlloc	; save the help alloc flag
	mov	fHelpAlloc,0		; zero it so CompressHelp will
	call	CompressHelp		; get help system out of the way
	mov	uierr,0 		; clear out the error condition
	pop	ax			; get fHelpAlloc back
	mov	fHelpAlloc,al		; and restore it.
	pop	iHelpId 		; restore HelpId
cEnd


;Rewritten with [38]
;***
;FlushBuffer - Deallocates buffers from HelpBuffer
;
;Purpose:
;	This routine will deallocate all buffers from (but not including)
;	the current topic to the end of the buffer.  If this is not enough,
;	it will start deleting buffers from the begining, until the specified
;	number has been reached.
;
;	If there is no current topic, we will just delete buffers from the
;	begining until we have freed enough.
;
;Entry:
;	CX = minimum # buffers to deallocate
;
;Exit:
;	None.
;
;Uses:
;	Per Convention
;****

cProc	FlushBuffer,<NEAR>,<SI,DI>
cBegin

	DbHeapMoveOff

DbAssertRelB	cl,be,MAX_HELPBUFS,UI,<FlushBuffer: Deallocating more buffers than exist>

	mov	di,cx			; DI = # buffers to delete
	cmp	oCurTopic,0		; is oCurTopic invalid?
	je	DelBegin		; yes, just delete from the begining
	mov	si,oLastBuf		; get pointer to last buffer
DelEnd_Next:

	cmp	si,oCurTopic		; have we reached the current topic?
	je	DelBegin		; yes, start deleting from begining

	lea	ax,[si].bdlHelpText	; ax = ptr to BDL in first buffer
	cCall	BdlFree,<ax>		; get rid of it

	dec	numBuf			; indicate it is no longer in use
	sub	si,BUFSIZE		; point at the last buffer
	cmp	si,OFFSET DGROUP:HelpBuffer ;did we go off the end?
	jae	NoWrap5 		; no
	add	si,BUFFERSIZE		; wrap around
NoWrap5:
	mov	oLastBuf,si		; update oLastBuf
	dec	di			; one more buffer deleted
	jmp	short DelEnd_Next	; go try again

DelBegin:
	or	di,di			; is it 0 or negative
	jle	EndFlushBuffer		; yes, exit
	mov	si,oFirstBuf		; get pointer to first buffer
DelBegin_Next:
	lea	ax,[si].bdlHelpText	; ax = ptr to BDL in first buffer
	cCall	BdlFree,<ax>		; get rid of it

	dec	numBuf			; indicate it is no longer in use

	add	si,BUFSIZE		; Point SI at next buffer
	cmp	si,BUFFEREND		; did we go off end of buffer?
	jb	NoWrap1 		; no, continue
	sub	si,BUFFERSIZE		; wrap around to begining
NoWrap1:
	dec	di			; one less to do
	jnz	DelBegin_Next		; loop again
	mov	oFirstBuf,si		; restore ptr to first buffer

EndFlushBuffer:
	DbHeapMoveOn			; reeanble heap movement

cEnd


;Reorganized with [43]
;***
;StartHelp : make sure the help system is initialized (or reinitialized)
;
;Purpose:
;	This routine is called before any references to the help buffers
;	or the help engine are made.  It makes sure the help engine is
;	activated, the help file exists, and the buffers for the virtual
;	line system are initialized.
;
;Entry:
;	None.
;
;Exit:
;	AX = 0	if all OK
;	AX = HELP_HANDLED if an error occured (will be handled).
;
;Uses:
;	Per Convention
;
;****

cProc	StartHelp,<PUBLIC,NEAR>
cBegin
DbAssertRelB	fHelpAlloc,ne,0,UI,<StartHelp:fHelpAlloc not set>

	test	HelpFlags,HLP_GOTBUF	; do we have the buffers?
DJMP	jnz	StartHelp_Success	; yes, exit with return code

	
	;Try to start help engine
	
StartHelpEngine:

DbAssertRel	BdlHelpHist.BDL_Seg,e,UNDEFINED,UI,<StartHelp:History Buffer Valid without HLP_GOTBUF>
	PUSHI	ax,<OFFSET DGROUP:BdlHelpHist>
	PUSHI	ax,HELPHIST_END 	; # bytes to allocate
	cCall	BdlAlloc		; allocate the memory
	or	ax,ax			; did we get the memory?
	jnz	InitHistBuff		; yes, go initialize it
	cCall	GiveHelpOOM		; queue an OOM error

	mov	ax,sp			; return a non-zer value
	jmp	short StartHelp_Exit

InitHistBuff:
	GETSEG	es,BdlHelpHist.BDL_Seg,bx,SIZE	; get segment
	xor	bx,bx
	mov	ES:[bx].HH_Used,bx	; initialize # elements in use
	mov	WORD PTR ES:[bx].HH_First,HELPHIST_BEGIN ; initialize ptr
	
	;Initialize HelpBuffer
	
	push	di			; Save register
	push	ds			; Set ES=DS=DGROUP
	pop	es
	mov	ax,UNDEFINED		; value to initialize buffers with
	mov	cx,BUFFERSIZE/2 	; number of words in buffer
	mov	di,OFFSET DGROUP:HelpBuffer   ; start of buffer
	mov	oFirstBuf,DI		; initialize buffer pointers
	mov	oLastBuf,DI
	rep	stosw			; initialize buffer
	mov	numBuf,CL		; initialize num in use (=0)

	pop	di			; Restore register
	or	HelpFlags,HLP_GOTBUF

StartHelp_Success:
	xor	ax,ax			; return code of success

StartHelp_Exit:
cEnd


;Rewritten with revision [57]
;***
;CalcNc - Calculate a Context Number from a Context string
;
;Purpose:
;	Go though our list of help files, calling HelpNc with the
;	initial context number for each one.  If a help file is not
;	opened, then try to open it before calling HelpNc.
;
;	If the context number is not found and there have been any
;	dialog boxes displayed (or OOM), then return HELP_HANDLED.
;	Otherwise return HELP_NF.
;
;Entry:
;	npsz - Near pointer to context string
;
;Exit:
;	if (CX <> 0)
;	    DX:AX = Context Number
;	else
;	    AL = Error Code (HELP_NF, HELP_HANDLED)
;
;Uses:
;	Per Convention
;
;****

cProc	CalcNc,<PUBLIC,NEAR>,<SI,DI>
parmW	npsz
localB	retVal				; value to return on error
cBegin
	mov	retVal,HELP_NF		; default error code is HELP_NF

	mov	cx,offset szHelpFileQhelp; cs:cx = ptr to file name
	call	fQhelpActive		; are we in the help viewer
	jnz	InQhelp 		; brif so, have proper file name
	mov	cx,offset szHelpFileEdit; cs:cx = ptr to file name
	call	fEditorActive		; are we in the editor
	jnz	InQhelp 		; brif so, have proper file name
	mov	cx,offset szHelpFileInterp ; use help file for interpreter
InQhelp:

	mov	dx,WORD PTR ncInitial	; dx = high word of ncInitial
	mov	ax,WORD PTR ncInitial+2 ; ax = low word of ncInitial

	mov	bx,npsz 		; get a ptr to the string
	cmp	Byte Ptr [bx],0 	; is it a local context?
	jne	NotLocalContext 	; no, check if we have to open file
DbAssertRel	oCurTopic,ne,0,UI,<CalcNc:oCurTopic invaid>
	mov	bx,oCurTopic		; get ptr to current topic
	mov	ax,Word Ptr [bx].ContextNum ; use current NC as the initial NC
	mov	dx,Word Ptr [bx].ContextNum+2

NotLocalContext:
	mov	bx,ax			; do we have a context #?
	or	bx,dx
	jnz	GotHelpFile		; brif so -- try the lookup

	cCall	HelpOpen,<cs,cx>	; try to open the file.
					; DX:AX = ncInitial, or 0:x if failure
	or	dx,dx			; was there an error?
	jnz	SaveInitialNc		; no
	cmp	ax,HELPERR_MAX		; check the low word
	jae	SaveInitialNc		; no error, continue processing

	.erre	HELPERR_MAX LT 256	; make sure all errors fit in a byte
	.errnz	HELPERR_FNF - 1 	; these two errors should be first
	.errnz	HELPERR_READ - 2

;HELPERR_READ => error already given
;HELPERR_FNF  => error already given
; error 7     => error already given
;HELPERR_LIMIT => assert that this doesn't happen (25 help files)
;HELPERR_BADAPPEND => give ER_BFM
;HELPERR_NOTHELP => give ER_BFM
;HELPERR_BADVERS => give ER_BFM
;others        => whatever is convenient

	mov	retVal,HELP_HANDLED	; set a new return value
DbAssertRel	ax,ne,HELPERR_LIMIT,UI,<CalcNC:HELPERR_LIMIT from HelpEngine>
	cmp	ax,HELPERR_READ 	; have we already informed user?
DJMP	jbe	CalcNcExit_Err		; yes, exit in error
	cmp	ax,7			; another error we told user about?
DJMP	je	CalcNcExit_Err		; yes, exit

DbAssertTst	HelpFlags,z,HLP_INHELP,UI,<HelpStart:Recursion lock set>
	or	HelpFlags,HLP_INHELP	; set recursion lock
	PUSHI	ax,MB_OK		; only an OK button
	PUSHI	ax,ER_BFM		; It must be a bad help file
	cCall	MsgBoxStd		; display error
	and	HelpFlags,NOT HLP_INHELP ; clear recursion lock
	jmp	CalcNCExit_Err		; and exit

SaveInitialNc:
	mov	WORD PTR ncInitial+2,ax	; save NC
	mov	WORD PTR ncInitial,dx	

GotHelpFile:
	and	HelpFlags,NOT (HLP_FAILOOM OR HLP_FAILFNF) ; clear flags

	cCall	HelpNc,<ds,npsz,dx,ax>	; DX:AX = nc for this context str

	test	HelpFlags,HLP_FAILOOM OR HLP_FAILFNF ; a handled error?
	jz	CheckForSuccess 	; no, check to see if we succeeded
	mov	retVal,HELP_HANDLED	; update return code otherwise

CheckForSuccess:
	mov	cx,dx
	or	cx,ax			; cx != 0 iff we got the topic
	jnz	CalcNcExit		; brif ok -- we got the NC

CalcNcExit_Err:
	xor	cx,cx			; set cx in case of error
	mov	al,retVal		; return error code
CalcNcExit:				;DX:AX = context #, or 0
cEnd


; Constants used by DoStatusButton().  All added with revision [10].

; NOTE: These messages must remain in sync with QBIMSGS.TXT so that the
; NOTE: status line buttons work properly.
; NOTE:
; NOTE: How to change the text of a message:
; NOTE: 
; NOTE: 1. Make sure that the new message will not overflow the status
; NOTE:    line.  If it will, text changes are required, or a button
; NOTE:    must be removed.
; NOTE: 2. Make the change to the text in QBIMSGS.TXT
; NOTE: 3. Make the change to the text in UINHELP.ASM
; NOTE: 4. Adjust the button position #'s in UINHELP.ASM
; NOTE:
; NOTE: How to add/remove a button:
; NOTE: 
; NOTE: 1. Perform the steps required to change the status line text
; NOTE: 2. Inc/Dec the # of buttons constant in the appropriate
; NOTE:    StatusXXXTable list.
; NOTE: 3. Add/Remove the Button_XXX constant corresponding to the
; NOTE:    desired key from the appropriate StatusXXXTable list.
; NOTE:


                                ; <Shift+F1=Help> <Possible other fields>
                        ;         ^              ^
All_ob1_F1      EQU 1       ;---------+                              |
All_ob2_F1      EQU 16 ;------------------------+

;Editing
;        000000000011111111112222222222333333333344444444445555555555
;        012345678901234567890123456789012345678901234567890123456789
;                       <Shift+F1=Help> <F6=Window> <F2=Subs> <F5=Run> <F8=Step>
;                                                                 ^                      ^^               ^^              ^^                    ^
Edit_ob1_F6 EQU 17          ;-+                       ||                    ||              ||       |
Edit_ob2_F6 EQU 28          ;------------+|                 ||              ||       |
Edit_ob1_F2 EQU 29          ;-------------+                 ||              ||       |
Edit_ob2_F2 EQU 38          ;----------------------+|                       ||       |
Edit_ob1_F5 EQU 39          ;-----------------------+                       ||       |
Edit_ob2_F5 EQU 47          ;-------------------------------+|               |
Edit_ob1_F8 EQU 48          ;--------------------------------+               |
Edit_ob2_F8 EQU 57          ;-----------------------------------------+

;Program running (debugging):
;        000000000011111111112222222222333333333344444444445555555555
;        012345678901234567890123456789012345678901234567890123456789
;         <Shift+F1=Help> <F5=Continue> <F9=Toggle Bkpt> <F8=Step>
;                         ^            ^^               ^^        ^
Running_ob1_F5 EQU 17       ;-+                             ||                                ||                    |
Running_ob2_F5 EQU 30       ;--------------+|                                 ||                    |
Running_ob1_F9 EQU 31       ;---------------+                                 ||                    |
Running_ob2_F9 EQU 47       ;-------------------------------+|                      |
Running_ob1_F8 EQU 48       ;--------------------------------+                      |
Running_ob2_F8 EQU 57       ;-----------------------------------------+

;Immediate window active:
;        000000000011111111112222222222333333333344444444445555555555
;        012345678901234567890123456789012345678901234567890123456789
;         <Shift+F1=Help> <F6=Window> <Enter=Execute Line>
;                         ^          ^^                   ^
Immed_ob1_F6    EQU 17      ;-+                      ||                                              |
Immed_ob2_F6    EQU 28      ;------------+|                                          |
Immed_ob1_ENTER EQU 29      ;-------------+                                          |
Immed_ob2_ENTER EQU 49      ;---------------------------------+

;Help window active:
;        000000000011111111112222222222333333333344444444445555555555
;        012345678901234567890123456789012345678901234567890123456789
;         <Shift+F1=Help> <F6=Window> <Esc=Cancel> <Ctrl+F1=Next> <Alt+F1=Back>
;                         ^          ^^           ^^             ^^            ^
Help_ob1_F6 EQU 17  ;-+                      ||                       ||                              ||                             |
Help_ob2_F6 EQU 28  ;------------+|                   ||                              ||                             |
Help_ob1_ESC    EQU 29      ;-------------+                   ||                              ||                             |
Help_ob2_ESC    EQU 41      ;-------------------------+|                              ||                             |
Help_ob1_CTRLF1 EQU 42      ;--------------------------+                              ||                             |
Help_ob2_CTRLF1 EQU 56      ;----------------------------------------+|                              |
Help_ob1_ALTF1 EQU 57       ;-----------------------------------------+                              |
Help_ob2_ALTF1 EQU 70       ;------------------------------------------------------+

;[67] Editing in QEDIT
;        000000000011111111112222222222333333333344444444445555555555
;        012345678901234567890123456789012345678901234567890123456789
;         MS-DOS Editor  <F1=Help> Press ALT to activate menus
;                        ^       ^
;                        |       |
QEdit_ob1_F1    EQU 16       ;  |           ;[73]
QEdit_ob2_F1    EQU 25       ;-------+                      ;[73]


;[67] Help window active: QEDIT
;        00000000001111111111222222222233333333334444444444555555555566666
;        01234567890123456789012345678901234567890123456789012345678901234
;         <F1=Help> <F6=Window> <Esc=Cancel> <Ctrl+F1=Next> <Alt+F1=Back>
;         ^       ^ ^          ^^           ^^             ^^            ^           ^
QHelp_ob1_F1    EQU 01     ;                ||                       ||                              ||                             |
QHelp_ob2_F1    EQU 09     ;                ||                       ||                              ||                             |
QHelp_ob1_F6    EQU 11  ;           ||                       ||                              ||                             |
QHelp_ob2_F6    EQU 22  ;------ |                    ||                              ||                             |
QHelp_ob1_ESC   EQU 23  ;-------+                    ||                              ||                             |
QHelp_ob2_ESC   EQU 35  ;-------------------+|                               ||                             |
QHelp_ob1_CTLF1 EQU 36      ;--------------------+                            ||                             |
QHelp_ob2_CTLF1 EQU 50      ;----------------------------------+|                            |
QHelp_ob1_ALTF1 EQU 51  ;-----------------------------------+                               |
QHelp_ob2_ALTF1 EQU 64  ;------------------------------------------------+


; QHELP help viewer start-up status: MSG_StatusQHStart
;        000000000011111111112222222222333333333344444444445555555555
;        012345678901234567890123456789012345678901234567890123456789
;	  MS-DOS Help Viewer  <F1=Help> Press ALT to activate menus
;			      ^       ^
;			      |       |
QHStart_ob1_F1    EQU 21 ;       |
QHStart_ob2_F1    EQU 29      ;-------+


; QHELP help window active: MSG_StatusQHHelp
;	 00000000001111111111222222222233333333334444444444555555555566666666667
;	 01234567890123456789012345678901234567890123456789012345678901234567890
;         <Alt+C=Contents> <Alt+N=Next> <Alt+B=Back>
;         ^               ^^           ^^           ^
QHHelp_ob1_CONTS EQU 01 ; ||           ||           |
QHHelp_ob2_CONTS EQU 17 ;-+|           ||           |
QHHelp_ob1_NEXT  EQU 18 ;--+           ||           |
QHHelp_ob2_NEXT  EQU 30 ;--------------+|           |
QHHelp_ob1_BACK  EQU 31 ;---------------+           |
QHHelp_ob2_BACK  EQU 43 ;---------------------------+


labelNP <StatusTable>
	DW      UIOFFSET StatusEditTable
	DW      UIOFFSET StatusRunningTable
	DW      UIOFFSET StatusImmedTable
	DW      UIOFFSET StatusHelpTable
	DW      UIOFFSET StatusQeditTable       
	DW      UIOFFSET StatusQhelpTable       
	DW	UIOFFSET StatusQHStartTable
	DW	UIOFFSET StatusQHhelpTable

StatusButton STRUC
SB_Command      db      ?
SB_obStart      db      ?
SB_obEnd        db      ?
StatusButton ends

LabelNP <StatusEditTable>
	DB      5               ; # of buttons in Edit status message
	StatusButton    <midHelpHelp, ALL_ob1_F1, ALL_ob2_F1>
	StatusButton    <midNextWindow, Edit_ob1_F6, Edit_ob2_F6>
	StatusButton    <midViewSubs, Edit_ob1_F2, Edit_ob2_F2>
	StatusButton    <midRunContinue, Edit_ob1_F5, Edit_ob2_F5>
	StatusButton    <midStep, Edit_ob1_F8, Edit_ob2_F8>

LabelNP <StatusRunningTable>
	DB      4               ; # of buttons in Running status message
	StatusButton    <midHelpHelp, ALL_ob1_F1, ALL_ob2_F1>
	StatusButton    <midRunContinue, Running_ob1_F5, Running_ob2_F5>
	StatusButton    <midDebugToggleBp, Running_ob1_F9, Running_ob2_F9>
	StatusButton    <midStep, Running_ob1_F8, Running_ob2_F8>

LabelNP <StatusImmedTable>
	DB      3               ; # of buttons in Immediate status message
	StatusButton    <midHelpHelp, ALL_ob1_F1, ALL_ob2_F1>
	StatusButton    <midNextWindow, Immed_ob1_F6 , Immed_ob2_F6>
	StatusButton    <midEnter, Immed_ob1_ENTER, Immed_ob2_ENTER>

LabelNP <StatusHelpTable>
	DB      5               ; # of buttons in Help status message
	StatusButton    <midHelpHelp, ALL_ob1_F1, ALL_ob2_F1>
	StatusButton    <midNextWindow, Help_ob1_F6, Help_ob2_F6>
	StatusButton    <midHelpClose, Help_ob1_ESC, Help_ob2_ESC>
	StatusButton    <midHelpNext, Help_ob1_CTRLF1, Help_ob2_CTRLF1>
	StatusButton    <midHelpBack, Help_ob1_ALTF1, Help_ob2_ALTF1>

LabelNP <StatusQeditTable>
	DB      1               ; # of buttons in Edit status message
	StatusButton    <midHelpSyntax, Qedit_ob1_F1, Qedit_ob2_F1>

LabelNP <StatusQhelpTable>
	DB      5               ; # of buttons in Help status message
	StatusButton    <midHelpSyntax, QHelp_ob1_F1, QHelp_ob2_F1>
	StatusButton    <midNextWindow, QHelp_ob1_F6, QHelp_ob2_F6>
	StatusButton    <midHelpClose, QHelp_ob1_ESC, QHelp_ob2_ESC>
	StatusButton    <midHelpNext, QHelp_ob1_CTLF1, QHelp_ob2_CTLF1>
        StatusButton    <midHelpBack, QHelp_ob1_ALTF1, QHelp_ob2_ALTF1>

LabelNP <StatusQHStartTable>
	DB	1		; # of buttons in QHELP start status message
	StatusButton	<midHelpHowToUse, QHStart_ob1_F1, QHStart_ob2_F1>

LabelNP <StatusQHhelpTable>
        DB      3               ; # of buttons in QHELP Help status message
        StatusButton    <midHelpHelp,    QHHelp_ob1_CONTS, QHHelp_ob2_CONTS>
        StatusButton    <midHelpNext,    QHHelp_ob1_NEXT , QHHelp_ob2_NEXT >
        StatusButton    <midHelpBack,    QHHelp_ob1_BACK , QHHelp_ob2_BACK >


;***
;HStatusButton
;       Given a mouse position in the status line window, return
;       a handle to the status button at that position.
;
;Purpose:
;       Heavy modification in revision [52]
;
;Entry:
;       posX = 0-relative mouse position
;Exit:
;       AX - handle to the status button or NULL if no button.
;            The handle is actually a pointer into the code segment.
;Uses:
;       Per Convention
;****

cProc   HStatusButton,<PUBLIC,NEAR>,<SI>
	ParmW   posX
cBegin
	xor     si,si                   ;Default return value: NULL

	mov     bx,iMsgStatusLine       ; bx = current line displayed
	sub     bx,MSG_StatusEdit       ; bx = 0, 1, 2, 3, or invalid
        cmp     bx,(MSG_StatusQHhelp - MSG_StatusEdit) ; out of range?
	ja      ButtonExit              ; brif so -- just exit

	shl     bx,1                    ; word index
	mov     si,word ptr cs:[bx].StatusTable

	mov     bx,[posX]               ; bx = 0-relative position
	lods    byte ptr cs:[si]        ; AL = number of entries for this line
	cbw                             
	xchg    cx,ax                   ; CX = number of entries
ButtonLoop:
	inc     si                      ;Ignore the SB_Command
	lods    word ptr cs:[si]        ;AH = obEnd, AL = obStart
	cmp     bl, al
	jb      ButtonNotFound
	cmp     bl, ah
	jb      ButtonFound
	loop    ButtonLoop
ButtonNotFound:
	mov     si,SIZE StatusButton    ;return NULL
ButtonFound:
	sub     si,SIZE StatusButton    ;point back to begining of structure.
ButtonExit:
	mov     ax, si
cEnd

;***
;ObStatusButton - Returns the position of a Status Line Button
;
;Purpose:
;       Return the position of the specified Status Button.
;
;       new for revision [52]
;
;Entry:
;       hButton - handle to button (near pointer in CS)
;
;Exit:
;       AX - Offset in characters from the beginning of the status line.
;
;Preserves:
;
;****
cProc ObStatusButton,<NEAR,PUBLIC>
	parmW   hButton
cBegin
	mov     bx, [hButton]
	xor     ax,ax
	mov     al, cs:[bx.SB_obStart]
cEnd

;***
;CbStatusButton - Returns the number of characters in a Status Line Button
;
;Purpose:
;       Return the number of characters in the specified Status Button.
;
;       new for revision [52]
;
;Entry:
;       hButton - handle to button (near pointer in CS)
;
;Exit:
;       AX - Size of button.
;
;Preserves:
;
;****
cProc CbStatusButton,<NEAR,PUBLIC>
	parmW hButton
cBegin
	mov     bx, [hButton]
	mov     al, cs:[bx.SB_obEnd]
	sub     al, cs:[bx.SB_obStart]
	xor     ah,ah
cEnd

;***
;DoStatusButton - Posts the key message associated with a Status Line Button
;
;Purpose:
;       Post the WM_CHAR message for the specified Status Button.
;
;       new for revision [52]
;
;Entry:
;       hButton - handle to button (near pointer in CS)
;
;Exit:
;       None.
;
;Preserves:
;
;****
cProc DoStatusButton,<NEAR,PUBLIC>
	parmW   hButton
cBegin
	mov     bx, [hButton]
	xor     dx,dx                           
	mov     dl,cs:[bx.SB_Command]           
	pushi   ax, <DATAOffset wndMain>        
	pushi   ax, WM_COMMAND                  
	push    dx                              
	xor     ax,ax
	push    ax
	push    ax
	call    PostMessage
cEnd

cEnd

;***
;RecordHelpHistory - Add entry to history list
;
;Purpose:
;       Record an entry into the help history list.  If the list is already
;       full, the last item gets bumped.
;
;Entry:
;       DX:AX   : item to be stored
;       BL      : flags to store with it        (EI_VARHELP Only)
;
;Exit:
;       None.
;
;Preserves:
;       AX,DX
;
;****

cProc   RecordHelpHistory,<NEAR,PUBLIC>,<DS>
cBegin
	DbAssertRel BdlHelpHist.BDL_Seg,ne,NOT_OWNER,UI,<RecordHelpHistory:BdlHelpBack not initialized>

	GETSEG  ds,BdlHelpHist.BDL_Seg,bx,SIZE  ; get segment of data

	;NOTE: DS != DGROUP at this point

	ASSUMES DS,NOTHING

	xor     bx,bx                   ; look at location 0
	cmp     WORD PTR [bx].HH_Used,NUM_HISTORY ; all slots being uses?
	je      RecordOverwrite         ; yes, just overwrite last item
	inc     WORD PTR [bx].HH_Used   ; one more item being used
RecordOverwrite:
	mov     cx,[bx].HH_First        ; DX is location to stick item
	push    cx
	add     cx,SIZE HelpHistEntry   ; point CX at next entry
	cmp     cx,HELPHIST_END         ; at end of table?
	jb      NoTableWrap             ; no
	mov     cx,HELPHIST_BEGIN       ; point to begining of table
NoTableWrap:
	mov     [bx].HH_First,cx        ; save new initial location
	pop     bx                      ; restore previous starting loc
	mov     [bx].HH_HiWord,dx       ; save Hi word
	mov     [bx].HH_LoWord,ax       ; save lo word
	ASSUMES DS,DGROUP
cEnd

;***
;RetrieveHelpHistory - Get the last recorded item in help history list
;
;Purpose:
;       Retrieve the last item from the help history list.  Returns with CX=0
;       if no items are left.
;
;Entry:
;       None.
;
;Exit:
;       if CX <> 0
;           DX:AX   : item stored
;           BL      : flags stored with it (IF EI_VARHELP)
;       else
;           No items recorded
;
;****
cProc   RetrieveHelpHistory,<NEAR,PUBLIC>,<DS>
cBegin
	DbAssertRel BdlHelpHist.BDL_Seg,ne,NOT_OWNER,UI,<RetrieveHelpHistory:BldHelBack not initialized>

	GETSEG  ds,BdlHelpHist.BDL_Seg,bx,SIZE  ; get segment of data
	
	;NOTE: DS != DGROUP at this point
	
	ASSUMES DS,NOTHING

	xor     bx,bx                   ; look at location 0
	mov     cx,[bx].HH_Used         ; CX = number of slots in use
	jcxz    RetrieveExit            ; no items, return with error
	
	;NOTE: CX <> 0 is preserved for rest of routine
	
	dec     WORD PTR [bx].HH_Used   ; indicate one less
	mov     ax,[bx].HH_First        ; AX = offset into table
	sub     ax,SIZE HelpHistEntry   ; point to previous entry
	cmp     ax,HELPHIST_BEGIN       ; before first entry?
	jge     NoRetrieveWrap          ; no, continue
	mov     ax,HELPHIST_END - SIZE HelpHistEntry ; set to last item
NoRetrieveWrap:
	mov     [bx].HH_First,ax        ; save new pointer
	xchg    bx,ax                   ; bx = ptr to entry to retrieve
	mov     dx,[bx].HH_HiWord       ; retrieve values and exit
	mov     ax,[bx].HH_LoWord
RetrieveExit:
	ASSUMES DS,DGROUP
cEnd

;***
;HelpHistoryLength - return number of items in the help history list
;
;Entry:
;       None
;
;Exit:
;       CX = number of items
;
;Uses:
;       CX,ES
;
;Preserves:
;       AX,BX,DX
;***

cProc   HelpHistoryLength,<NEAR>,<BX>
cBegin
	DbAssertRel BdlHelpHist.BDL_Seg,ne,NOT_OWNER,UI,<HelpHistoryLength:BldHelBack not initialized>

	GETSEG  es,BdlHelpHist.BDL_Seg,bx,SIZE  ; get segment of data
	mov     cx,ES:[0].HH_Used       ; CX = number of slots in use
cEnd



; added with [24], rewritten with [25]
;***
;DiscardHistoryoRs - Discard all Help + Bookmark information for an oRs
;
;Purpose:
;       Discards any stored information about a given oRs in the help system.
;       If the current help being displayed is dependent on this oRs, then we
;       will do a Help Back until something is found.  If nothing is found,
;       close the help window.
;
;Entry:
;       oRs - oRs to be discarded
;
;Exit:
;       None
;
;Uses:
;       Per C Conventions
;***

cProc   DiscardHistoryoRs,<PUBLIC,FAR>,<SI,DI>
parmW   oRs
cBegin
	mov     ax,UNDEFINED            ; indicates bookmark not valid
	cCall   ReAssignBookMark,<oRs,AX> ; delete all matching bookmarks



DiscardHistory_Exit:
cEnd


;***
;szSrchExcl - search for '!' in a 0 terminated string
;
;Purpose:
;       This routine scans a 0 terminated string to find an '!' in
;       it.
;
;Entry:
;       npsz    - Near Ptr to string to search
;
;Exit:
;       if character not found
;           AX = 0
;       else
;           AX = ptr to first occurance of the character
;
;Uses:
;       Per C convention
;****

cProc   szSrchExcl,<PUBLIC,NEAR>,<SI>
parmW   npsz
cBegin
	mov     si,npsz                 ; start of string to scan
NextChr:
	lodsb
	cmp     al,'!'                  ; is it the character of interest?
	je      FoundChr                ; yes, return its position
	or      al,al                   ; is it the 0 terminator?
	jne     NextChr                 ; no, try the next character.
	cbw                             ; AX = 0
	xchg    ax,si                   ; SI = 0  (return value)
FoundChr:
	xchg    ax,si                   ; AX = Ptr to str OR 0
cEnd


;***
;SendHelpMsg - Stub routine to send a message to the help window
;
;Purpose:
;       Code saving method of sending a message (with wParm only) to
;       the help window.
;
;Entry:
;       msg - message to send
;       wParam - one parameter to be passed
;
;Exit:
;       Return value from Window Proc
;
;
;****

cProc   SendHelpMsg,<PUBLIC,NEAR>
parmW   msg
parmW   wParam
cBegin
	mov     ax,OFFSET DGROUP:wndHelp
	cCall   SendMessage,<AX, msg, wParam, ax,ax>
cEnd


sEnd    UI
	end
