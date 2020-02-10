	page	49,132
	TITLE	exos.asm - executors for OS-specific statements and functions
;***
;exos.asm - executors for OS-specific statements and functions
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;
;****************************************************************************

	.xlist
	include		version.inc
EXOS_ASM = ON
	IncludeOnce	architec
	IncludeOnce	context
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	extort
	IncludeOnce	msdos
	IncludeOnce	opintrsc
	IncludeOnce	opstmt
	IncludeOnce	opaftqb4
	IncludeOnce	qbimsgs
	IncludeOnce	ui
	.list

ExToRtDispSd	equ ExToRtDispAx	


sBegin	DATA
	externW	b$nuldes		;SD for a null string
sEnd	DATA

assumes es, NOTHING
assumes ss, DATA

sBegin	CODE
assumes cs, CODE


externNP	RtDispatch		

;=============================================================================
;		OS-specific Instrinsic Function Executors
;=============================================================================

MakeExe exFnCommand_,opFnCommand_
	    CALLRT  B$FCMD,DispSd	

MakeExe	exFnDate_,opFnDate_
	    CALLRT  B$FDAT,DispSd	

MakeExe	exFnTime_,opFnTime_
	    CALLRT  B$FTIM,DispMovSd	


MakeExe exFnEnviron_I2,opFnEnviron_
	CALLRT	B$FEVI,DispSd		

MakeExe exFnEnviron_SD,opFnEnviron_
	CALLRT	B$FEVS,DispSd		

MakeExe	exFnShell,opFnShell
	    CALLRT  B$FSHL,DispAx


MakeExe	exFnTimer,opFnTimer
	DbTimer			;print # of sec.s since last reset if PROFILE
	CALLRT	B$TIMR,DispR4

;=============================================================================
;		OS-specific Statement Executors
;=============================================================================

MakeExe exStChDir,opStChDir
	CALLRT	B$CDIR,Disp



MakeExe	exStDate_,opStDate_
	    CALLRT  B$SDAT,Disp

MakeExe	exStTime_,opStTime_
	    CALLRT  B$STIM,Disp

MakeExe	exStEnviron,opStEnviron
	CALLRT	B$SENV,Disp

MakeExe	exStFiles0,opStFiles0
	PushI	ax,<dataOFFSET b$nuldes>
	SkipExHeader
MakeExe	exStFiles1,opStFiles1
	CALLRT	B$FILS,Disp

MakeExe	exStShell0,opStShell0
	cmp	[grs.GRS_fDirect],FALSE
	jz	StShell0_Cont		;brif not Direct Mode

	;SHELL statement from direct mode. Tell User what he needs to do
	;to get back to QB - - -
	PushI	ax,MSG_ExitToRet	;"Type EXIT to return to QuickBASIC"
	cCall	ListStdMsgFar		;put msg in ds:bufStdMsg
	PushI	bx,<dataOFFSET bufStdMsg> ;pb field of pseudo SD on stack
	push	ax			;cb field of pseudo SD on stack
	mov	ax,sp
	push	ax			;pSD
	CALLRT	B$PESD			;print string to output screen
StShell0_Cont:
	PushI	ax,<dataOFFSET b$nuldes>
	SkipExHeader
MakeExe	exStShell1,opStShell1
	CALLRT	B$SSHL,DispMov



MakeExe	exStKill,opStKill
	CALLRT	B$KILL,Disp

MakeExe	exStMkdir,opStMkdir
	CALLRT	B$MDIR,Disp

MakeExe	exStName,opStName
	CALLRT	B$NAME,Disp

MakeExe	exStRmdir,opStRmdir
	CALLRT	B$RDIR,Disp

MakeExe exStSleep0,opStSleep0		
	xor	ax,ax			
	push	ax			
	push	ax			;default for sleep is 0L
	SkipExHeader

MakeExe exStSleep1,opStSleep1		
	mov	ax,HIGH PB$SLEP + 0100H ; postword for B$SLEP
					; Note that this counts on B$SLEP
					; generating a > 255 postbyte/word
	mov	cx,CODEOFFSET DispMov	; return to DispMov after B$SLEP
	jmp	RtDispatch		; do it

sEnd	CODE
end
