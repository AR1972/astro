page	49,132
	TITLE	sstxutil - scanner text table management utilities
;***
;sstxutil.asm	- scanner text table management utilities
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	The scanner behaves as a filter, reading pcode in one state
;	and emiting it in another.  The SS_EXECUTE state is larger than
;	either of the other two states, as coercion, opBranch and
;	procedure invocation support executors are present in SS_EXECUTE
;	mode only.
;
;	At the beginning of scan, the text table is moved up in memory.
;	It is then scanned to a point starting at the beginning of the segment.
;
;	When the text expands, a test is made to ensure that there is still
;	space between the source side of the filter and the emit side.  If
;	room is tight, then the unscanned pcode is moved farther up within
;	the segment.
;
;	Links through the pcode are updated when they are scanned, not when
;	the text moves.  For each link list in the text there is a control
;	structure that maintains information about that link list.  See the
;	LNK structure.
;
;
;****************************************************************************

	.xlist
	include		version.inc
SSTXUTIL_ASM = ON
	IncludeOnce	scanner
	IncludeOnce	ssint
	IncludeOnce	txtmgr
	.list


assumes DS, DATA
assumes es, NOTHING
assumes ss, DATA



sBegin	SCAN
assumes cs, SCAN

subttl	
page
;***
;SsEnsureGap
;Purpose:
;	Ensure a gap between the scanner emit side and the source side.
;
;	This routine updates the LNK link control structures used to
;	maintain link lists through the text during scan.  The actual link
;	lists are maintained when the pcodes containg the links are updated.
;
;	The text is only actually moved if one of the two conditions is met:
;	1. The movement would be more than SS_oTxMovThreshold.
;	2. The movement is required to ensure the minimum gap of SS_oTxGapMin.
;
;	When an allocation fails, it is retried with SS_cbTxAllocInc.  OME
;	is detected by noting when the allocation size drops below the 
;	allocation required to maintain the minimum gap between source and
;	emit side in the scanner - SS_cbTxGapMin.
;
;
;	Gap control constants are:
;	SS_oTxMovThreshold	- moves below this aren't made unless necessary
;				  to maintain SS_cbTxGapMin.
;	SS_cbTxAllocInc		- amount by which a request is decremented
;				  before retrying.
;	SS_cbTxGapMin		- minimum required gap size
;
;	Variable SscbTxExpand is maintained as the total bytes of text
;	expansion.
;
;Input:
;	si = 	oTxSrc      - first byte to move
;	di =	oTxEmit     - last byte emited by scanner + 1
;	cx =	cbTxGapPref - preferred gap size (SsMakeGap only)
;Output:
;	si = new address of low byte of source side
;	carry flag set if OME    
;
;**********************************************************************
SS_cbTxMovThreshold	= 64	;cb of minimum move
SS_cbTxAllocInc		= 128	;Decrement by this amount when allocation fails
SS_cbTxGapMin		= 20	;At least 20 bytes between source and emit

SsMakeGap:
public	SsMakeGap
	xor	ax,ax			;No current gap
	and	cl,0FEH			;Always move whole words
	cmp	cx,SS_cbTxMovThreshold	;Request more than minimum?
	jb	MinMove
	jmp	short FigMove

SsEnsureGap:
public	ssEnsureGap
	mov	ax,si
	sub	ax,di			;ax = cb Current gap
	cmp	ax,SS_cbTxGapMin	;Is it big enough now?
	jae	EnsureGapOK		;Yes - exit
MinMove:
	mov	cx,SS_cbTxMovThreshold	;Minimum move to try
FigMove:
	mov	dx,SS_cbTxGapMin	;Absolute minimum allowed gap
	sub	dx,ax			;dx = min move to get to required gap

EnsureAllocLoop:
;	cx = cbMovMax (getting smaller each loop)
;	dx = cbMovMin

	push	cx			;Save cbMovMax
	push	dx			;  and cbMovMin
	Arg	si			;pFirst byte of pcode to move
	Arg	cx			;cbMove
	cCall	TxtMoveUpFar		; Attempt to move the text
	GETSEGTXTCUR			;es = the text segment
	pop	dx
	pop	cx
	or	ax,ax			;Test for success
	jnz	EnsureGapGrew		;Movment succeded - exit
	cmp	cx,dx			;Trying minimum move?
	jz	EnsureGapOME
	sub	cx,SS_cbTxAllocInc	;Decrement for retry
	cmp	cx,dx			;Less than minimum?
	jge	EnsureAllocLoop		;Retry if more than minimum
	mov	cx,dx			;Try absolute minimum
	jmp	EnsureAllocLoop		; and retry

EnsureGapOME:
	stc				;Signal OME
	ret

EnsureGapGrew:
	add	SscbTxExpand,cx		;Update total bytes of movement
	add	si,cx
EnsureGapOK:
	clc
	ret

sEnd	SCAN
end
