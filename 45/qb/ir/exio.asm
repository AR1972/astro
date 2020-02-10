page	49,132
	TITLE	exio.asm - executors for IO-specific statements and functions
;***
;exio.asm - executors for IO-specific statements and functions
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;
;****************************************************************************

	.xlist
	include 	version.inc
EXIO_ASM = ON
	IncludeOnce	architec
	IncludeOnce	context 	
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	opintrsc
	IncludeOnce	opstmt
	IncludeOnce	opcontrl
	IncludeOnce	extort
	IncludeOnce	rtps
	.list

assumes es, NOTHING
assumes ss, DATA

sBegin	DATA
	externW b$nuldes		;pointer to an SD for a NULL string
sEnd	DATA

sBegin	CODE
assumes cs, CODE

	page


;***
;I4toU2
;
;Purpose:
;
;   Given an I4 on the stack (before near return address), coerce it to
;   an equivalent U2, replacing the I4 with the U2.
;   Runtime error if overflow.
;
;Preserves:
;
;   bx
;
;****

	public	I4toU2
I4toU2	proc	near
	pop	cx			;near return address
	pop	ax
	pop	dx			;dx:ax is the I4
	push	ax			;assume success
	ror	dx,1			;put low bit of high word in PSW.C
	adc	dx,0			;set PSW.Z if PSW.C == sense of all bits
	jnz	@F			;brif overflow error
	jmp	cx			;return

@@:
	jmp	exMathErrOVF		;Declare overflow error
I4toU2	endp


;=============================================================================
;		IO-specific Instrinsic Function Executors
;=============================================================================


MakeExe exFnCsrlin,opFnCsrlin
	CALLRT	B$CSRL,DispAx

MakeExe exFnInkey_,opFnInkey_
	CALLRT	B$INKY,DispAx

MakeExe exFnIoctl_,opFnIoctl_
	CALLRT	B$FICT,DispAx

MakeExe exFnLpos,opFnLpos
	CALLRT	B$LPOS,DispAx

MakeExe exFnPen,opFnPen
	CALLRT	B$FPEN,DispAx

MakeExe exFnPos,opFnPos
	CALLRT	B$FPOS,DispAx

MakeExe exFnScreen2,opFnScreen2
	xor	ax,ax			;pass 0 for defaulted third parameter
	push	ax
	SkipExHeader
MakeExe exFnScreen3,opFnScreen3
	CALLRT	B$FSCN,DispAx

MakeExe exFnStick,opFnStick
	CALLRT	B$STIK,DispAx

MakeExe exFnStrig,opFnStrig
	CALLRT	B$FSTG,DispAx

MakeExe exFnInp,opFnInp	
	call	I4toU2			;coerce I4 port address to U2
	CALLRT	B$PINP,DispAx

MakeExe exFnEof,opFnEof
	CALLRT	B$FEOF,DispAx

MakeExe exFnFileAttr,opFnFileAttr
	CALLRT	B$FATR,DispDxAx

MakeExe exFnFreefile,opFnFreefile
	CALLRT	B$FREF,DispAx

MakeExe exFnInput_1,opFnInput_1
	xor	ax,ax
	push	ax			;default if no channel number specified
	SkipExHeader
MakeExe exFnInput_2,opFnInput_2
	CALLRT	B$FINP,DispMovSd	

MakeExe exFnLoc,opFnLoc
	CALLRT	B$FLOC,DispDxAx

MakeExe exFnLof,opFnLof
	CALLRT	B$FLOF,DispDxAx

MakeExe	exFnSeek,opFnSeek
	CALLRT B$FSEK,DispDxAx        ;Return I4 file position on stack

;=============================================================================
;		IO-specific Statement Executors
;=============================================================================

MakeExe exStBload1,opStBload1
	xor	ax,ax
	push	ax			;garbage here, but ax = 0 used below
	jmp	short Bload_Common

MakeExe exStBload2,opStBload2
	call	I4toU2			;coerce I4 on stack to a U2
	mov	al,1			;flag - - 2nd parm specified
Bload_Common:
	push	ax			;flag: 0 == 2nd parm defaulted
	CALLRT	B$BLOD,DispMov

MakeExe exStBsave,opStBsave
	call	I4toU2			;coerce length to U2
	pop	bx			;save U2 length in bx
	call	I4toU2			;coerce offset to U2
	push	bx			;put length back on (bx was preserved)
	CALLRT	B$BSAV,DispMov

MakeExe exStCls,opStCls
	CALLRT	B$SCLS,Disp

MakeExe exFieldInit,opFieldInit
	CALLRT	B$FLDP,DispMov

MakeExe exFieldItem,opFieldItem
	CALLRT	B$FIEL,DispMov		;call RT for single FIELD parm pair

MakeExe exStIoctl,opStIoctl
	CALLRT	B$SICT,Disp

MakeExe exStKey,opStKey
	LODSWTX 			;ax = constant for ON, OFF, or LIST
	push	ax
	CALLRT	B$KFUN,Disp

MakeExe exStKeyMap,opStKeyMap
	CALLRT	B$KMAP,Disp

MakeExe exStLocate,opStLocate
	LODSWTX 			;ax = count of parms on stack
	push	ax
	CALLRT	B$LOCT,Disp

MakeExe exStScreen,opStScreen
	LODSWTX 			;ax = count of parms on stack
	push	ax
	CALLRT	B$CSCN,Disp

MakeExe exStViewPrint0,opStViewPrint0
	mov	ax,-1			;pass -1 for two defaulted parameters
	push	ax
	push	ax
	SkipExHeader
MakeExe exStViewPrint2,opStViewPrint2
	CALLRT	B$VWPT,Disp

MakeExe exStWidthLprint,opStWidthLprint
	CALLRT	B$LWID,Disp


MakeExe exStClose,opStClose
	LODSWTX 			;get count of parms
	push	ax
	CALLRT	B$CLOS,DispMov

MakeExe exStGet1,opStGet1
	CALLRT	B$GET1,Disp

MakeExe exStGet2,opStGet2
	CALLRT	B$GET2,DispMov

MakeExe exStGetRec2,opStGetRec2
	LODSWTX 			;get size parm
	cmp	ax,-1			;FS?
	jz	NoCbG2
	push	ax
NoCbG2:
	CALLRT	B$GET3,DispMov

MakeExe exStGetRec3,opStGetRec3
	LODSWTX 			;get size parm
	cmp	ax,-1			;FS?
	jz	NoCbG3
	push	ax
NoCbG3:
	CALLRT	B$GET4,DispMov

MakeExe exStPut1,opStPut1
	CALLRT	B$PUT1,Disp

MakeExe exStPut2,opStPut2
	CALLRT	B$PUT2,Disp

MakeExe exStPutRec2,opStPutRec2
	LODSWTX 			;get size parm
	cmp	ax,-1			;FS?
	jz	NoCbP2
	push	ax
NoCbP2:
	CALLRT	B$PUT3,DispMov

MakeExe exStPutRec3,opStPutRec3
	LODSWTX 			;get size parm
	cmp	ax,-1			;FS?
	jz	NoCbP3
	push	ax
NoCbP3:
	CALLRT	B$PUT4,DispMov

MakeExe exInputEos,opInputEos
	CALLRT	B$PEOS,Disp

MakeExe exInputChan,opInputChan
	CALLRT	B$DSKI,Disp

MakeExe exInputPrompt,opInputPrompt
	test	BYTE PTR es:[si+2],FINP_Prompt
	jnz	Got_Prompt		;brif user specified a prompt string

	PUSHI	ax,<dataOFFSET b$nuldes> ;push pointer to null SD
Got_Prompt:
	push	es
	push	si			;far pointer to input preamble block
	LODSWTX
	inc	ax			;round cbBlock up, because the parser
	and	al,0FEh 		;  never emits odd-size pcodes
	add	si,ax			;move txtptr to next pcode
	CALLRT	B$INPP,DispMov

MakeExe exStReadI2Near,opStRead
	SkipExHeader
MakeExe exStInputI2Near,opStInput
	pop	ax			;convert near pointer to far pointer
	push	ds
	push	ax
	SkipExHeader
MakeExe exStReadI2,opStRead
	SkipExHeader
MakeExe exStInputI2,opStInput
	CALLRT	B$RDI2,Disp

MakeExe exStReadI4Near,opStRead
	SkipExHeader
MakeExe exStInputI4Near,opStInput
	pop	ax			;convert near pointer to far pointer
	push	ds
	push	ax
	SkipExHeader
MakeExe exStReadI4,opStRead
	SkipExHeader
MakeExe exStInputI4,opStInput
	CALLRT	B$RDI4,Disp


MakeExe exStReadR4Near,opStRead
	SkipExHeader
MakeExe exStInputR4Near,opStInput
	pop	ax			;convert near pointer to far pointer
	push	ds
	push	ax
	SkipExHeader
MakeExe exStReadR4,opStRead
	SkipExHeader
MakeExe exStInputR4,opStInput
	CALLRT	B$RDR4,Disp


MakeExe exStReadR8Near,opStRead
	SkipExHeader
MakeExe exStInputR8Near,opStInput
	pop	ax			;convert near pointer to far pointer
	push	ds
	push	ax
	SkipExHeader
MakeExe exStReadR8,opStRead
	SkipExHeader
MakeExe exStInputR8,opStInput
	CALLRT	B$RDR8,Disp


MakeExe exStReadSD,opStRead
	SkipExHeader
MakeExe exStInputSD,opStInput
	pop	ax			;convert near pointer (psd) to far ptr
	push	ds
	push	ax
	xor	ax,ax
	push	ax			;note this is for an SD, not an FS
	SkipExHeader
MakeExe exStReadFS,opStRead
	SkipExHeader
MakeExe exStInputFS,opStInput
	CALLRT	B$RDSD,Disp

MakeExe exStLineInput,opStLineInput
	xor	bx,bx			;note this is for an SD, not an FS
	pop	cx			;pSD
	mov	dx,ds			;Segment of SD to dx
	jmp	short LineInput		

MakeExe exStLineInputFS,opStLineInput
	pop	bx			;cbFS
	pop	cx			;Offset of FS
	pop	dx			;Segment of FS
LineInput:				
	LODSWTX 			;fetch fPromptWord
	test	al,FINP_Prompt		;see if SDPrompt is present on stack
	jnz	Prompt_Present		;  brif it is

	PUSHI	di,<dataOFFSET b$nuldes> ;push pointer to null SD
Prompt_Present:
	push	dx			;Segment
	push	cx			;offset part of far pSd/pFS
	push	bx			;cbFS or 0
	push	ax			;pass fPromptWord as final parameter
	CALLRT	B$LNIN,DispMov		;No movement, but restore DI

MakeExe exStLock,opStLock
	SkipExHeader			;the mode parm differentiates these
MakeExe exStUnLock,opStUnLock
.errnz	LOCK_Def1stArg	-  4000H
.errnz	LOCK_DefLastArg -  8000H

	LODSWTX 			;get mode word, and advance si
	test	al,LOCK_1stToLast	;see if user said 'All records'
	jz	PushRecParms		;  brif so; push garbage record numbers

	test	ah,040H 		;Is "TO" present?
	jnz	Parms_OK		;Both on stack - no more work to do
	test	ah,080H 		;Is "from" the only arg present?
	jz	Parms_OK		;"TO" present only - args both provided
					; by the parser

					;Here if only "from" is present
					; so, duplicate the arg
	pop	bx			;Duplicate the arg on the stack
	pop	cx			
PushRecParms:
	push	cx			;push first I4 record parm
	push	bx
	push	cx			;push second I4 record parm
	push	bx
Parms_OK:
	push	ax			;push mode word
	CALLRT	B$LOCK,Disp		;lock/unlock approriate records

MakeExe exStOpen2,opStOpen2
	PUSHI	ax,-1			;default record length
	SkipExHeader
MakeExe exStOpen3,opStOpen3
	LODSWTX 			;get mode word, and advance si
	push	ax			;push mode parameter
	CALLRT	B$OPEN,DispMov

MakeExe exStOpenOld3,opStOpenOld3
	PUSHI	ax,-1			;default record length
	SkipExHeader
MakeExe exStOpenOld4,opStOpenOld4
	CALLRT	B$OOPN,DispMov

MakeExe exStReset,opStReset
	CALLRT	B$REST,Disp

MakeExe exStSeek,opStSeek
	CALLRT	B$SSEK,Disp		;Return I4 file position on stack


MakeExe exStOut,opStOut
	pop	bx			;data to output
	call	I4toU2			;coerce I4 port address to U2 on stack
	push	bx
	CALLRT	B$POUT,Disp

MakeExe exStWait2,opStWait2
	xor	bl,bl			;make the xor-expression a nop
	jmp	short StWait_Common
MakeExe exStWait3,opStWait3
	pop	bx			;fetch xor-expression
StWait_Common:
	pop	di			;fetch and-expression
	call	I4toU2			;coerce I4 port address to U2 (on stack)
	push	di			;and-expression
	push	bx			;xor-expression
	CALLRT	B$WAIT,DispMov		;NOTE: only using 'Mov' varient because
					;NOTE: we used DI above


MakeExe exStWidth2I2,opStWidth2 	;WIDTH n,m
	CALLRT	B$WIDT,Disp


MakeExe exStWidth2SD,opStWidth2 	;WIDTH "def:",m
	CALLRT	B$DWID,Disp


MakeExe exStWidthfile,opStWidthfile	;WIDTH #n,m
	CALLRT	B$FWID,Disp

MakeExe exChanOut,opChanOut
	CALLRT	B$CHOU,Disp

MakeExe exNop,opNop
	SkipExHeader
MakeExe exLbs,opLbs
	SkipExHeader
MakeExe exStLet,opStLet
Disp1:
	DispMac 		;speed critical - - dispatch directly here

sEnd	CODE
end
