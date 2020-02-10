   TITLE UiUtil.asm - Misc User Interface Functions

COMMENT	\

--------- --- ---- -- ---------- ----
COPYRIGHT (C) 1986 BY MICROSOFT, INC.
--------- --- ---- -- ---------- ----

\

;============================================================================
; Module: UiUtil.asm - Misc User Interface Functions
; System: Quick BASIC Interpreter
;============================================================================

	.xlist
	include		version.inc
	UIUTIL_ASM = ON	
	includeOnce	lister
	includeOnce	qbimsgs
	includeOnce	rtinterp
	includeOnce	rtps
	includeOnce	ui
	.list

assumes	DS,DATA
assumes	SS,DATA
assumes	ES,NOTHING

	externFP B$IValidatePath		
	externFP SetUiErrFar			


;-------------------------------------------------------------------------
;		DATA Segment Declarations
;-------------------------------------------------------------------------
sBegin	DATA

	EXTRN	uierr:word

	externB iHelpId 			

PUBLIC	cbStdMsg				
cbStdMsg	dw	?			; length of message
PUBLIC	bufStdMsg
bufStdMsg	db	CB_bufStdMsg dup(?)	;big enough for biggest msg

TEMPORARY_PADDING   db	(6+16) dup(?)
MORE_PADDING        db  (70)   dup(?)
; [QH1] - To make room for long MS-DOS 6.0 online-help index strings


; We are calling HelpGetLineAttr to get an array of help line attributes
; into bufStdMsg.  We need this padding for Beta Release for 2 reasons:
;
; 1.  Bug in HelpGetLine Attr that causes it to trash 6 bytes past the
;	 end of the array without first checking if there is room.
; 2.  Even if bug #1 was fixed, we still need the room!
;
; The 6 bytes SHOULD keep us from trashing DGROUP (a BDL in release QB!)
; The 16 bytes is extra padding.



sEnd	DATA

;-------------------------------------------------------------------------
;	Ascii Message Utility Functions (for internationalization)
;	Since B$ExecQBC throws away all of QBI except for segment LOADRTM,
;	and it needs to output prompts, we need to keep messages and
;	the code which accesses them in segment LOADRTM.
;-------------------------------------------------------------------------

sBegin	LOADRTM
assumes	cs,LOADRTM

externW tpMsgStd
externB TransTab
externB HuffTab


;*********************************************************************
; ushort NEAR ListStdMsg(iMsg), ListStdMsgRtmNear(), FAR ListStdMsgFar(iMsg)
;
; Purpose:
;	List a standard ASCII message to the global static buffer bufStdMsg.
;	ListStdMsgRtmNear is used by the runtime loader.
;
; Entry:
;	iMsg is standard error index from qbimsgs.h (ER_xxx or MSG_xxx)
;	AX is standard error index for ListStdMsgRtmNear
;
; Exit:
;	bufStdMsg contains a zero terminated string.
;	  note that bufStdMsg is guarenteed to be large enough to
;	  hold the largest message's text.
;	cbStdMsg contains the byte count of the string.
;	as function result, it returns the byte count of the string.
;
;*********************************************************************
cProc	ListStdMsgFar <PUBLIC,FAR,NODATA>
	parmW	iMsg
cBegin	ListStdMsg
	mov	ax,iMsg
	call	ListStdMsgRtmNear	;call common code to list message
cEnd					;message in bufStdMsg, length in AX

cProc	ListStdMsgRtmNear <PUBLIC,NEAR,NODATA>,<si,di>
cBegin
	xchg	ax,si			;si = iMsg
	dec	si			;si = iMsg - 1
	DbAssertRel si,b,IMSG_MAX,LOADRTM,<invalid iMsg in ListStdMsg>
	DbAssertRel CB_MSG_MAX+1,be,CB_bufStdMsg,LOADRTM,<CB_bufStdMsg too small>
	shl	si,1			;si = offset into msg table
	push	ds			;set es pointing to DGROUP
	pop	es
	push	cs			;set ds pointing to LOADRTM segment
	pop	ds
assumes	ds,LOADRTM
assumes	es,DGROUP
	mov	si,tpMsgStd[si]		;si = ptr to standard msg
	or	si,si
	jne	NotZero
	mov	si,[tpMsgStd - 2 + ER_UE * 2] ;si = "Unprintable error"
	mov	iHelpId,ER_UE		; tell help we changed messages
NotZero:
	mov	di,dataOFFSET bufStdMsg
	cCall	dcd,<si,di>		;decode string, returns with size
	push	es			;restore ds pointing to DGROUP
	pop	ds
assumes	DS,DATA
assumes	es,NOTHING
	add	di,ax			;point to end of string
	mov	byte ptr [di],0		;zero terminate string in bufStdMsg
cEnd	ListStdMsgFar


;*******************************************************************
; ushort dcd (pbSrc, pbDst)
; char *pbSrc, *pbDst
;
; Purpose:
;	to decode a bit stream into a string of characters.
;	The bit stream should start off with the number of
;	encoded characters in the bit stream in the first byte.
;
; Algorith:
;	The code should implement the following C code
;
;int dcd (pbSrc, pbDst)
;char *pbSrc, *pbDst
;{
;    char *pn
;    int bc = 0, n, size
;
;    for (size = n = (unsigned char) *pbSrc++; n; n--) {
;	 pn = HuffTab
;	 while (1) {
;	     pn = pn + getbit (pbSrc, bc++)
;	     if (*pn & (1 << 7)) {
;		 *pbDst++ = TransTab[(unsigned char) *pn & ~(1 << 7)]
;		 break
;	     }
;	     pn = HuffTab + (*pn << 1)
;	 }
;    }
;    return (size)
;}
;
;int getbit (b, n)
;char *b
;int n
;{
;    if (b[n/8] & ((1<<7) >> (n%8))) return (1)
;    return (0)
;}
;
;
; Entry:
;	NOTE: DS points to segment LOADRTM, ES points to DGROUP
;	pbSrc is a pointer to the input bit buffer, and pbDst is
;	a pointer to the output string.
;	It expects HuffTab, and TransTab to be defined properly
;	by qbimsgs.asm.
;
; Exit: Returns the number of characters translated in ax.
;
;*******************************************************************

assumes	ds,LOADRTM
assumes	es,DGROUP
cProc	dcd,<NEAR,PUBLIC>,<si,di>
	parmW	<pbSrc,pbDst>
	localW	<n>

cBegin
	mov	di,[pbDst]	;get pointer to output buffer
	mov	si,[pbSrc]	;get pointer to input buffer
	lodsb		 	;get count to al
	DbAssertRelB al,b,80h,LOADRTM,<dcd: count exceeds 127>
	cbw			;clear high byte of ax
	cwd			;initialize bit counter (dx) to 0
	push	ax		;save count for return value

	mov	[n],ax		;and set n to count
	inc	[pbSrc]		;inc input buffer pointer to start of bits

loopChar:			;loop through the characters
	dec	[n]		;Decrement counter
	jl	doneDcd 	;If less than zero, exit.
	mov	si,offset HuffTab;put table pointer into si

loopTab:			;Start of loop through table
	mov	ax,dx		;prepare to divide bc
	mov	bl,8		;by 8
	div	bl		;al = bc/8, ah = bc%8
	inc	dx		;increment bit counter

	mov	ch,10000000b	;prepare to shr (1<<7)
	mov	cl,ah		;by bc%8
	shr	ch,cl		;ch = (1<<7) >> (bc%8)

	DbAssertRelB al,b,80h,LOADRTM,<dcd: count larger than 127>
	cbw			;ax = al = quotient
	xchg	bx,ax		;bx = quotient (i.e. bc/8)

	add	bx,[pbSrc]
	test	[bx],ch		;see if bit ch is on in pbSrc[bc/8]
	jz	dontInc		;if not, don't increment table pointer
	inc	si		;increment table pointer
dontInc:
	sub	bh,bh
	mov	bl,[si] 	;move table entry to bl
	rcl	bl,1		;rotate left to see if high bit is set
	jnc	continue	;if not, continue looking through table
	shr	bl,1		;otherwise, shift back, losing high bit
	mov	al,TransTab[bx] ;translate character through table.
	stosb			;move byte to output buffer
	jmp	SHORT loopChar	;and do another character.

continue:			;continue looking through table
	mov	si,offset HuffTab;re-initialize si to start of table
	add	si,bx		;add already *2 entry to the base of table
	jmp	SHORT loopTab 	;so loop through table again.

doneDcd:			;We are done.
	pop	ax		;return character count.
cEnd


assumes	ds,DGROUP
assumes	es,NOTHING

sEnd	LOADRTM

sBegin	UI
assumes	cs,UI

cProc	ListStdMsg <PUBLIC,NEAR,NODATA>
	parmW	iMsg
cBegin	ListStdMsgFar
	push	iMsg
	call	ListStdMsgFar
cEnd	ListStdMsg

;*********************************************************************
; char *SzGetIth( char *pbStrings, ushort iString )
;
; Purpose:
;   Return pointer to i'th string in a buffer of null terminated strings
;
; Entry:
;    pbStrings - pointer to array of characters containing the null terminated
;                strings.
;    iString - Which string to return.
;
; Exit:
;   return pointer.
;
;*********************************************************************
cProc SzGetIth,<NEAR,PUBLIC>,<DI>
	parmW	pbStrings		
	parmW	iString
cBegin
	push	ds
	pop	es
	mov	di, [pbStrings]
	mov	dx, [iString]
	inc	dx
	xor	ax,ax
	jmp	short PbSzI_LoopEnd

PbSzI_LoopStart:
	mov	cx, 0ffffH
	repnz scasb

PbSzI_LoopEnd:
	dec	dx
	jnz	PbSzI_LoopStart

	mov	ax, di
cEnd

sEnd	UI

sBegin	CP
assumes	CS,CP

;*********************************************************************
; LongToStdMsg
; Purpose:
;	Fill 'bufStdMsg' with a 0-terminated ASCII equivalent of a
;	long positive integer parameter.
;
;*********************************************************************
cProc	LongToStdMsg,<PUBLIC,FAR>,<di>
	parmD	val
cBegin
	lea	bx,val			;bx points to binary number (in DS)
	mov	al,VT_I4		;al = value type (long)
	sub	ah,ah			;no explicit terminating char
	mov	di,DATAOFFSET bufStdMsg	;di points to destination buffer (in DS)
	call	ListNum
cEnd

;*********************************************************************
; HexToStdMsg
; Purpose:
;	Fill 'bufStdMsg' with a 0-terminated ASCII equivalent of
;	an unsigned short parameter.
;	Used to display Compiled-Code-Runtime-Error-Adr
;
;*********************************************************************
cProc	HexToStdMsg,<PUBLIC,FAR>,<di>
	parmW	val
cBegin
	sub	dx,dx			;dx = high word = 0
	mov	ax,[val]		;ax = low word
	mov	di,DATAOFFSET bufStdMsg	;di points to destination buffer (in DS)
	mov	bh,'H'			;convert number to Hexadecimal
	call	ListBaseNum
cEnd

;***
;  WriteFileErr(fhandle, offBuf, cb)
;
;  Write cb bytes to file fhandle, trapping Int24 errors
;
;  Inputs:
;     fhandle	- file handle to write to.
;     offBuf	- address of buffer.
;     cb	- length of buffer.
;
;  Outputs:	AX == 0 if no error, else, non-zero standard error code
;
;****
cProc WriteFileErr,<PUBLIC,FAR>
	ParmW	fhandle
	ParmW	offBuf
	ParmW	cb
cBegin
	mov	ax,CPOFFSET WfExit	;if any runtime errors occur,
	call	RtSetTrap		; branch to WfExit with sp,di =
					; current values.
					; RtFreeTrap called if error occurs
	mov	bx,[fhandle]
	mov	cx,[cb]
	mov	dx,[offBuf]
	mov	ah,40H
	int	21h
	mov	ax,0			;prepare to return "No error"
	jnc	FreeTrap		; brif no error
	mov	ax,ER_IOE		;Return error condition.
FreeTrap:				
	call	RtFreeTrap		;preserves AX
WfExit:					
cEnd

;***************************************************************************
; NormFileNameFar
;	Moved with revision [8].
;
;Purpose:
;	Normalizes the path of a filename and appends an extension if there
;	previously was no extension.  Uses runtime routine B$IValidatePath
;	to generate the fully-qualified pathname from a filename.
;
;	See NormFileName (in \QB\IR\TXTLOAD.ASM) for more information.
;
;Entry:
;	pSource = near pointer to source string
;	cbSource = # bytes in source filespec
;	pDest = near pointer to destination buffer (length = FILNAML)
;	pExt = near pointer to extension to add
;
;Exit:
;	Upper case, fully-specified pathname filled in.
;	ax = length of pathname (NOT including null), or UNDEFINED if error
;
;******************************************************************************
cProc	NormFileNameFar,<PUBLIC,FAR>
parmW	pSource
parmW	cbSource
parmW	pDest
parmW	pExt
cBegin

	call	RtPushHandler		;save caller's runtime error handler
					; (NOTE: alters stack pointer)
					; (preserves ax,bx, dx)
	mov	ax,CPOFFSET NfnfExit	;if any runtime errors occur,
	call	RtSetTrap		; branch to NfnfExit with sp,di =
					; current values  (preserves bx,cx, dx)
	push	pSource			;parm1 = near pointer to filename
	push	cbSource		;parm2 = filename length
	push	pDest			;parm3 = near pointer to destination
	push	pExt			;parm4 = near pointer to extension
	call	B$IValidatePath		;ax = length of resulting filename
					;     NOT including 0 byte
	jmp	short NoRTError		;return ax = length
;ax = error code
NfnfExit:		
	cCall	SetUiErrFar		;notify UI that error AX occurred
	mov	ax,UNDEFINED		;ax = UNDEFINED ==> error
NoRTError:

	call	RtPopHandler		;restore caller's runtime error handler
					; (saved on stack by RtPushHandler)
					; (preserves ax)
cEnd


sEnd	CP



end
