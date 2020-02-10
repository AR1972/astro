page	,132
	TITLE	ssmisc - Scan Miscellaneous opcodes
;***
;ssmisc - Scan Miscellaneous opcodes
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   This module contains miscellaneous scan utilities.
;
;
;****************************************************************************


	.xlist
	include		version.inc
SSMISC_ASM = ON
	IncludeOnce	conint		
	IncludeOnce	context
	IncludeOnce	qbimsgs 	
	IncludeOnce	ssint
	IncludeOnce	txtmgr		
	IncludeOnce	variable
	.list

;	.sall

assumes DS, DATA
assumes es, NOTHING
assumes ss, DATA


subttl	Static data area definitons.

sBegin	SCAN
assumes cs, SCAN

subttl	Scan Table Access Routines
page
;***
;GetRuleInfo
;Purpose:
;	Fetch the rule byte and word for this opcode.
;Input:
;	bx = opcode
;Output:
;	ax = rule byte
;	bx = rule word
;	PSW.C set iff high bit in mpOpRule set
;Preserves:
;	all
;****************************************************************
public	GetRuleInfo
GetRuleInfo:
	mov	bl,mpOpRule[bx]		;Load rule table offset
	xor	bh,bh			;To byte offset
	mov	al,tRuleByte[bx]	;Load rule byte
	cbw
	shl	bl,1			;To word offset
	mov	bx,tRuleWord[bx]	;Load rule word
	ret

subttl	Scan Stack Access Routines
page

;***
;FindFrame
;Purpose:
;	Return the address of the the requested frame type, if one exists.
;
;Input:
;	ax = Frame identifier for structure entry.
;
;Output:
;	bx = address of frame (if PSW.NZ)
;	PSW.Z set if no such structure found.
;
;Modifies:
;	none
;Preserves:
;	ax
;**********************************************************************
public	FindFrame
FindFrame:
	push	ax
	mov	bx,sp
	add	bx,4			;Back over push and ret address

EnsureFrameLoop:
	mov	cx,[bx]			;Load frame identifier
	cmp	cx,STYP_StackEnd	;Test for end of stack
	jz	EnsureFrameX		;End of stack - exit
	test	cx,ax			;Test for block of requested type
	jnz	EnsureFrameX		;Frame found - exit
	call	CbFrameSTYPCx		;Map STYP in cx to cbFrame in cx
	add	bx,cx			;Move to next frame
	jmp	short EnsureFrameLoop	;Loop to check next frame

EnsureFrameX:
	pop	ax
	ret

;***
;CbFrameSTYPCx
;Purpose:
;	Map a frame id to the number of bytes in that frame type.
;
;	Frame sizes are based on the high byte of the id word.
;	The type bit is converted to an index into an array of
;	frame sizes.  The conversion is performed by shifting the
;	frame id high byte until the single bit indicating the frame
;	type is found.
;
;Input:
;	cx = frame id
;
;Output:
;	cx = count of bytes in the input frame type
;
;Preserves:
;	all
;*******************************************************************

mcbSTYP	LABEL	WORD
	dw	SIZE FIF	;IF
	dw	SIZE FIF	;ELSE
	dw	SIZE FFOR 	;FOR
	dw	SIZE FIF	;DefFn frame same as IF
	dw	SIZE FDO	;DO
	dw	SIZE FDO	;WHILE frame is identical to DO frame
	dw	SIZE FCASE	;CASE

public	CbFrameSTYPCx
CbFrameSTYPCx:
	push	bx
	mov	bx,-2
CbCountSTYPLoop:
	inc	bx
	inc	bx
	shr	ch,1
	jnb	CbCountSTYPLoop
	mov	cx,[bx].mcbSTYP
	pop	bx
	ret

	subttl	Execution Procedure Frame Allocation
	page
;***
;SsAllocOFrame - allocate an oFrame for a frame resident variable
;Purpose:
;	Allocate an oFrame for a frame resident variable.
;	If the oFrame is already allocated then that allocation is honored.
;
;	Note that this means the allocation is always successful.
;
;	prsCur contains information about the current frame
;	allocation.  
;	   PRS_cbFrameVars is the start of the last allocated frame variable.
;	
;Input:
;	ds:bx = pVar of a frame variable that may or may not have an oFrame
;Output:
;	none
;Preserves:
;	all
;**********************************************************************
.errnz	4 - SIZE DM			;Power of two multiply used below

	public	SsAllocOFrame
SsAllocOFrame:
	DbChk	PVar,bx			;Verify that this is a variable
	push	ax
	push	bx
	push	cx
	push	dx
	mov	dx,[bx].VAR_Flags	;[3] Load flags word from variable
	mov	cx,dx
	and	cx,FV_TYP_MASK		;Get oTyp from flags word
	jnz	@F
	mov	cx,[bx].VAR_oTyp	;Load data type from variable
@@:
	add	bx,VAR_value		;Move to value entry
	test	dx,FVARRAY		; Is this an array?
	jz	AllocOFrameNotArray	;Not an array
	add	bx,AFRAME_oFrame	;Move to oFrame for arrays
	mov	al,[bx-AFRAME_oFrame].AFRAME_cDims	;Dimension count
	xor	ah,ah
	shl	ax,1
	shl	ax,1			;4 bytes per dimension
	add	ax,(SIZE AD) - 1	;AD has a one byte field that
					;overlaps the first dimension info
	cmp	word ptr [bx],0		;Already allocated?
	jz	SsAllocAryOrSimple
AllocOFrameNotArray:
	cmp	word ptr [bx],0		;Already allocated?
	jnz	SsAllocOFrameX		;Already allocated
	mov	ax,cx			;Map data type..
	call	CbTypOTypSCAN		; to data byte count.
PadLength:				
	inc	ax
	and	ax,not 1		;Round up to even
	js	StackOverflow		; Variable size is > 32767 bytes
	jz	AllocFS 		; Brif fixed string or text
	TestX	dx,FVFUN		;Function return value?
	jnz     SetRetVal_OBP	        ; add var offset and set oBP

	;Bump cbFrameVars by the size of the current variable.

SsAllocAryOrSimple:
	add	ax,PrsCur.PRS_cbFrameVars ;Increment cb frame variables
					  ; and temporaries
	jo	StackOverflow		
	mov	prsCur.PRS_cbFrameVars,ax ;Save updated size
SetOBP:
	neg	ax			;To be subtracted from bp
SetHandle:
	mov	[bx],ax			;Set oFrame for current var
SsAllocOFrameX:
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret

AllocFS:
	mov	ax,[bx].VAR_cbFixed	; Length of fixed variable
	DbAssertRel ax,ne,0,SCAN,<SsAllocOFrame: VAR_cbFixed is 0>
	jmp	short PadLength 	; Go back and pad


SetRetVal_OBP:				
	add	ax,-FR_FirstVar		; account for offset to first var
	jmp	short SetOBP		

StackOverflow:
	mov	ax,MSG_OutOfStack	
	call	SsError 		
	jmp	short SsAllocOFrameX	


	subttl	control structure binding helpers
	page
;BindExitCur,BindExit - bind chain of EXIT entries
;Purpose:
;	Walk a linked list of EXIT addresses and bind them to
;	the passed oTx. This is used by ENDIF, NEXT, LOOP, and
;	WEND scan routines.
;
;Input:
;	bx = oTx of start of EXIT chain (terminated by UNDEFINED)
;	cx = oTx to bind EXIT chain to.
;
;Output:
;	none.
;
;Preserves:
;	dx

public	BindExit, BindExitCur

BindExitCur:
	mov	cx,di		;bind to current emit address
BindExit:
	cmp	bx,UNDEFINED	;Test for end of list
	jz	BindX		; and exit - end of EXIT list
	mov	ax,PTRTX[bx]	;Get the next link
	mov	PTRTX[bx],cx	;Bind this ref
	xchg	ax,bx		;Next link to bx
	jmp	short BindExit	;Go check for another link in the chain.
BindX:
	ret

;***
;CbTypOTypSCAN, CbTypOTypOMrsSCAN
;Purpose:
;	This routine returns the number of bytes of data required for
;	the input type.
;
;	Copied directly from varmgr as part of revision [7].
;
;	CbTypOTyp assumes that if the oTyp is a user-defined type, it is
;			in the type table for the current mrs
;	CbTypOTypOMrs uses the MRS_bdlVar table in the mrs whose oMrs is
;			given in bx.
;Input:
;	ax = oTyp
;	for CbTypOTypOMrs, bx = oMrs of type table
;Output:
;	ax = cbTyp, or 0 if oTyp == ET_FS or ET_FT
;	PSW flags set based on an OR AX,AX
;Prserves:
;	all (even bx)
;	ES is preserved.  NOTE: for FV_SBSWAP, it is assumed that the ES to be
;				preserved is the text table.
;***************************************************************************
assumes DS, NOTHING		;can be called with DS != SS

mpCbTyp label byte
	.erre	ET_IMP EQ ($-mpCbTyp)
	DB	0		;ET_IMP hole
	.erre	ET_I2 EQ ($-mpCbTyp)
	DB	2		;ET_I2
	.erre	ET_I4 EQ ($-mpCbTyp)
	DB	4		;ET_I4

	.erre	ET_R4 EQ ($-mpCbTyp)
	DB	4		;ET_R4

	.erre	ET_R8 EQ ($-mpCbTyp)
	DB	8		;ET_R8


	.erre	ET_SD EQ ($-mpCbTyp)
	DB	SIZE SD 	;ET_SD


	.erre	ET_FS EQ ($-mpCbTyp)
	DB	0		;ET_FS - - - can't tell size from ET_ type



	.erre	ET_MAX EQ ($-mpCbTyp-1)

	public	CbTypOTypOMrsSCAN
	public	CbTypOTypSCAN
CbTypOTypOMrsSCAN   PROC    NEAR
	push	bx
	jmp	short CbTypOTyp_Cont
CbTypOTypSCAN:
	push	bx
	mov	bx,[grs.GRS_oMrsCur]
	DbChk	oTyp,ax 		;sanity check on input oTyp
CbTypOTyp_Cont:
	cmp	ax,ET_MAX		;Is it a fundamental type?
	ja	NotPredefinedType	;  brif not - user defined

	mov	bx,offset mpCbTyp	;base of lookup table in CS
	xlat	byte ptr cs:[bx]	;al == desired size
	pop	bx
	or	ax,ax			;set PSW flags
	ret

NotPredefinedType:
	PUSH_ES 			;preserve for caller
	test	[conFlags],F_CON_StaticStructs
	jz	Mrs_In_Table		;brif mrsCur not set up

	cmp	bx,[grs.GRS_oMrsCur]
	jz	Want_MrsCur		;brif passed oMrs is for mrsCur

Mrs_In_Table:
	push	si			
	RS_BASE add,bx			;bx points into Rs table
	GETRS_SEG es,si,<SPEED,LOAD>	; get seg of Rs table, trashing si
	pop	si			
	jmp	short Got_pMrs

Want_MrsCur:				;ax is an offset into type table
	lea	bx,mrsCur		;  found in the current mrs
	SETSEG_EQ_SS es
Got_pMrs:
	add	ax,PTRRS[bx.MRS_bdVar.BD_pb] ;ax = pTyp
	xchg	bx,ax			;bx = oTyp, ax = garbage
	mov	ax,[bx].TYP_cbData	;ax = cbData from type table entry
	POP_ES				
	pop	bx
	or	ax,ax			;set PSW flags
	ret
CbTypOTypOMrsSCAN   ENDP

;Added with [11]
;End of [11]
	
sEnd	SCAN				
end
