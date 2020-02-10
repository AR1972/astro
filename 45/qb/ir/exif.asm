page	49,132
	TITLE	exif - executors for IF and ELSE, DO/LOOP, and WHILE/WEND varients
;***
;exif - executors for IF and ELSE, DO/LOOP, and WHILE/WEND varients
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Executors for IF and ELSE varients.
;	Also contains Executors for WHILE/WEND, and DO/LOOP varients
;
;****************************************************************************

	.xlist
	include		version.inc
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	opcontrl
	.list

assumes cs, CODE
assumes es, NOTHING
assumes ss, DATA

sBegin	CODE

	EXTRN	SetProgramMode:NEAR

subttl	IF
page
;exStIfR8
;Purpose:
;	These handle the case of
;	   IF a!
;	and
;	   IF a#
;


MakeExe exStR8While,opStWhile
	SkipExHeader
MakeExe exStLoopR8Until,opStLoopUntil
	SkipExHeader
MakeExe exStDoR8While,opStDoWhile
	SkipExHeader
MakeExe	exStElseIfR8,opStElseIf
	SkipExHeader
MakeExe	exStIfR8,opStIf
	SkipExHeader			;Fall through 
MakeExe	exStIfBlockR8,opStIfBlock
;Re-written with [4]
	fldz			;Put zero on stack
	fcompp			;Compare with zero and remove operands
	fstsw	[Stat]		;Save result of comparison
	fwait
	mov	ax,[Stat]
	sahf
	jz	IfFalse
	jmp	short IfTrue
;End of [4]

MakeExe exStI4While,opStWhile
	SkipExHeader
MakeExe exStLoopI4Until,opStLoopUntil
	SkipExHeader
MakeExe exStDoI4While,opStDoWhile
	SkipExHeader
MakeExe	exStElseIfI4,opStElseIf
	SkipExHeader
MakeExe	exStIfI4,opStIf
	SkipExHeader
MakeExe	exStIfBlockI4,opStIfBlock
	pop	ax
	pop	dx
AxDxZtest:				
	or	ax,dx			;PSW.Z set = FALSE
	jnz	IfTrue			;TRUE
IfFalse:
	mov	si,PTRTX[si]		;Take the false branch
	DispMac

MakeExe exStI2While,opStWhile
	SkipExHeader
MakeExe exStLoopI2Until,opStLoopUntil
	SkipExHeader
MakeExe exStDoI2While,opStDoWhile
	SkipExHeader
MakeExe	exStElseIfI2,opStElseIf
	SkipExHeader
MakeExe	exStIfBlockI2,opStIfBlock
	SkipExHeader
MakeExe	exStIfI2,opStIf
	pop	cx			;I2 TRUE or FALSE
	jcxz	IfFalse			;FALSE
IfTrue:
IfLabFalse:
	add	si,2			;Skip operand
	DispMac

MakeExe exStDoI2Until,opStDoUntil
	SkipExHeader
MakeExe exStLoopI2While,opStLoopWhile
	SkipExHeader
MakeExe	exStIfGotoLabI2,opStIfGotoLab
	SkipExHeader			;Fall into next executor
MakeExe	exStIfLabI2,opStIfLab
	pop	cx
	jcxz	IfLabFalse		;False case - fall through label
IfLabTrue:
	mov	si,PTRTX[si]		;TRUE - branch to label
	DispMac

subttl	IfLabDirect
page
;IfLabDirect
;Purpose:
;	Handle the case that there is an IfLab in a direct mode statment.
;	This is kept separate from program mode IfLab as it is speed critical.
;
MakeExe	exStIfLabDirectI2,opStIfLabDirect
	xor	dx,dx
IfLabDirectCom:
	pop	ax
IfLabDirectIntCom:
	or	ax,dx
IfLabDirectFlag:
	jz	IfLabFalse
IfLabDirectTrue:
	mov	si,PTRTX[si]		;Load start offset
	call	SetProgramMode
	DispMac


MakeExe	exStIfLabDirectR8,opStIfLabDirect
;Re-written with [4]
	fldz			;Put zero on stack
	fcompp			;Compare with zero and remove operands
	fstsw	[Stat]		;Save result of comparison
	fwait
	mov	ax,[Stat]
	sahf
	jmp	IfLabDirectFlag
;End of [4]

MakeExe	exStIfLabDirectI4,opStIfLabDirect
	pop	dx
	jmp	short IfLabDirectCom
subttl	IfLab
page

MakeExe exStDoI4Until,opStDoUntil
	SkipExHeader
MakeExe exStLoopI4While,opStLoopWhile
	SkipExHeader
MakeExe	exStIfGotoLabI4,opStIfGotoLab
	SkipExHeader			;Fall into next executor
MakeExe	exStIfLabI4,opStIfLab
	pop	dx
IfLabFalseOrTrue:
	pop	ax
	or	ax,dx
IfLabFlag:
	jz	IfLabFalse		;FALSE - fall through label
	jmp	short IfLabTrue		;TRUE - branch to label


MakeExe exStDoR8Until,opStDoUntil
	SkipExHeader
MakeExe exStLoopR8While,opStLoopWhile
	SkipExHeader
MakeExe	exStIfGotoLabR8,opStIfGotoLab
	SkipExHeader			;Fall into next executor
MakeExe	exStIfLabR8,opStIfLab
;Re-written with [4]
	fldz			;Put zero on stack
	fcompp			;Compare with zero and remove operands
	fstsw	[Stat]		;Save result of comparison
	fwait
	mov	ax,[Stat]
	sahf
	jmp	IfLabFlag
;End of [4]

subttl Else
page
MakeExe	exStElseLabDirect,opStElseLabDirect
	mov	si,PTRTX[si]		;Load start offset
	call	SetProgramMode		;Set up for execution
	SkipExHeader			;Fall into next executor
MakeExe exStDo,opStDo
	SkipExHeader
MakeExe	exStEndIfBlock,opStEndIfBlock
	SkipExHeader			;Fall into next executor
MakeExe	exStElseNop,opStElseNop
	DispMac				;Speed critical NOP for ElseNop

MakeExe exStWend,opStWend
	SkipExHeader
MakeExe exStLoop,opStLoop
	SkipExHeader
MakeExe	exStElseLab,opStElseLab
	SkipExHeader			;Fall into next executor
MakeExe	exStElse,opStElse
	mov	si,PTRTX[si]		;Perform the branch
	DispMac				; and on with the show

sEnd	CODE
end
