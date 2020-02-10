page	49,132
	TITLE	exprint.asm - executors for [L]PRINT | WRITE [USING]
;***
;exprint.asm - executors for [L]PRINT | WRITE [USING]
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;
;****************************************************************************

	.xlist
	include 	version.inc
EXPRINT_ASM = ON
	IncludeOnce	architec
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	opintrsc
	IncludeOnce	opstmt
	IncludeOnce	context
	IncludeOnce	extort
	IncludeOnce	ui
	.list

assumes es, NOTHING
assumes ss, DATA

	EXTRN	B$IfScrnDev:FAR

sBegin	DATA
	globalB executorFlags,0		;flags that get reset whenever a runtime
					;  error occurs, i.e., flags whose
					;  meaning is only valid within a single
					;  statement
sEnd	DATA

	    externW b$nuldes


sBegin	CODE
assumes cs, CODE

;=============================================================================
;		PRINT-specific Statement Executors
;=============================================================================

;Most of the PRINT executors call the runtime via non-standard means. Since
;  All of these guys must set or reset the F_EXEC_ItemPrinted flag (so that 
;  exPrintEos will know whether it needs to perform special work for the case
;  of PRINT with no arguments), the below code is somewhat jumbled to share
;  this task.
;  Instead of using the CALLRT macro, each of these executors just puts the
;  postbyte for the associated runtime entry point into al. This value is
;  increased by one to match the table of actual executor addresses.
;  A special ExToRt variant is jumped to with the postbyte already set 
;  correctly in ax.
;  
;  NOTE: If any of these postbytes become greater that 254, the assembler will
;  NOTE: give an error; strive to keep these within a single byte.

MakeExe exPrintItemCommaI2,opPrintItemComma
	mov	al,PB$PCI2+1
	jmp	short Print_Disp
	
MakeExe exPrintItemCommaI4,opPrintItemComma
	mov	al,PB$PCI4+1
	jmp	short Print_Disp
	
MakeExe exPrintItemCommaR4,opPrintItemComma
	mov	al,PB$PCR4+1
	jmp	short PrintR4		
	
MakeExe exPrintItemCommaR8,opPrintItemComma
	mov	al,PB$PCR8+1
	jmp	short PrintR8		
	
	
MakeExe exPrintComma,opPrintComma
	PUSHI	ax,<dataOFFSET b$nuldes>
	SkipExHeader		;fall into exPrintItemCommaSD
MakeExe exPrintItemCommaSD,opPrintItemComma
	mov	al,PB$PCSD+1
	jmp	short Print_Disp
	

MakeExe exPrintItemSemiI2,opPrintItemSemi
	mov	al,PB$PSI2+1
	jmp	short Print_Disp
	
MakeExe exPrintItemSemiI4,opPrintItemSemi
	mov	al,PB$PSI4+1
	jmp	short Print_Disp
	
;Rewritten with [7]
MakeExe exPrintItemSemiR4,opPrintItemSemi
	mov	al,PB$PSR4+1
PrintR4:
	or	[executorFlags],F_EXEC_ItemPrinted
					;remember that we've printed something
PrintR4Eos:
	sub	sp,4			;Make room in stack
	mov	bx,sp
	fstp	dword ptr DGROUP:[bx]
	jmp	short PrintWait
	
MakeExe exPrintItemSemiR8,opPrintItemSemi
	mov	al,PB$PSR8+1
PrintR8:
	or	[executorFlags],F_EXEC_ItemPrinted
					;remember that we've printed something
PrintR8Eos:
	sub	sp,8			;Make room in stack
	mov	bx,sp
	fstp	qword ptr DGROUP:[bx]
PrintWait:
	fwait
	jmp	short Print_Disp2
;End of [7]


MakeExe exPrintSemi,opPrintSemi
	PUSHI	ax,<dataOFFSET b$nuldes>
	SkipExHeader		;fall into exPrintItemCommaSD
MakeExe exPrintItemSemiSD,opPrintItemSemi
	mov	al,PB$PSSD+1
	jmp	short Print_Disp
	

MakeExe exPrintItemEosI2,opPrintItemEos
	mov	al,PB$PEI2+1
PrintEos_Disp:
	and	[executorFlags],not F_EXEC_ItemPrinted
					;reset flag
	jmp	short Print_Disp2
;al = runtime index
Print_Disp:
	or	[executorFlags],F_EXEC_ItemPrinted
					;remember that we've printed something
Print_Disp2:
	test	[executorFlags],F_EXEC_Write
	jz	Print_Disp3		;brif not WRITE

	push	ax
	CALLRT	B$WRIT			;tell runtime this is WRITE, not PRINT
	pop	ax
	and	[executorFlags],not F_EXEC_Write
					;turn off Write flag
Print_Disp3:
	cmp	[fDebugScr],0
	jne	ShowOutputScr		;brif debug screen is visible

Print_Disp1:
	xor	ah,ah			;ax = runtime index
	jmp	ExToRtByteDisp

;Branched to when user is tracing through a PRINT stmt.
;native code can cause screen output, we need to swap to output screen
;just in case.
;
ShowOutputScr:
	push	ax			;preserve al = runtime index
	call	B$IfScrnDev		;see if current channel's output
					; will go to screen
	or	ax,ax
	je	NotScrnDev
	call	ShowOutScr
	DbAssertRelB [fDebugScr],e,0,CODE,<exprint: ShowOutScr failed>
NotScrnDev:
	pop	ax			;restore al = runtime index
	jmp	SHORT Print_Disp1

MakeExe exPrintItemEosI4,opPrintItemEos
	mov	al,PB$PEI4+1
	jmp	short PrintEos_Disp
	
MakeExe exPrintItemEosR4,opPrintItemEos
	mov	al,PB$PER4+1
	and	[executorFlags],not F_EXEC_ItemPrinted	;reset flag
	jmp	PrintR4Eos		


MakeExe exPrintItemEosR8,opPrintItemEos
	mov	al,PB$PER8+1
	and	[executorFlags],not F_EXEC_ItemPrinted	;reset flag
	jmp	PrintR8Eos		


Print_No_Arg:
	and	[executorFlags],NOT F_EXEC_Write
					;in case of WRITE [#n,]
	PUSHI	ax,<dataOFFSET b$nuldes>
	SkipExHeader		;fall into exPrintItemCommaSD
MakeExe exPrintItemEosSD,opPrintItemEos
	mov	al,PB$PESD+1
	jmp	PrintEos_Disp
	

MakeExe exPrintEos,opPrintEos
	test	[executorFlags],F_EXEC_ItemPrinted
	jz	Print_No_Arg

	mov	al,PB$PEOS+1
	jmp	PrintEos_Disp

MakeExe exPrintSpc,opPrintSpc
	or	[executorFlags],F_EXEC_ItemPrinted
					;remember that we've printed something
	CALLRT	B$FSPC,Disp
	
MakeExe exPrintTab,opPrintTab
	or	[executorFlags],F_EXEC_ItemPrinted
					;remember that we've printed something
	CALLRT	B$FTAB,Disp
	


MakeExe	exStLprint,opStLprint
	CALLRT	B$LPRT,Disp
	
MakeExe	exStWrite,opStWrite
	;must defer calling the runtime to handle this special case:
	;   WRITE [#n,]
	or	[executorFlags],F_EXEC_Write
	jmp	Disp
	
MakeExe	exUsing,opUsing
	CALLRT	B$USNG,Disp
	

sEnd	CODE
end
