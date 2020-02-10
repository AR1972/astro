#-------------------------------------------------------------------
# File: $(QB5)\ir\makefile:
# This makefile contains make information for QB5-specific
# BASIC Interpreter Source
#
# This is used in conjunction with ..\qb\makefile, ..\test\makefile,
#	and $(QB)\ir\makefile and must be specified BEFORE $(QB)\ir\makefile.
# For example, in a 'test' directory:
# make qb.exe -f makefile -f ..\qb\makefile $(QB5)\ir\makefile $(QB)\ir\makefile
#--------------------------------------------------------------------

!IFDEF ROMBASIC
ROMBAS = -DROMBASIC
!ELSE
ROMBAS =
!ENDIF

# header file dependancies:
# These first lines represent header files which include other header files
#
SB_INC = $(QB)\hd\sb.inc
CMDEQV_INC =
CONTEXT_H = $(QB)\hd\context.h $(QB)\hd\heap.h $(QB)\hd\array.inc
EXECUTOR_INC = $(QB)\hd\executor.inc $(QB)\hd\exint.inc opcodes.inc
EXTORT_INC = $(QB)\hd\rtmint.inc $(QB)\hd\extort.inc
HEAP_INC = $(QB)\hd\heap.inc $(QB)\hd\array.inc $(QB)\hd\pointers.inc
PARSER_H = $(QB)\hd\parser.h $(QB)\hd\heap.h
PARSER_INC = $(QB)\hd\parser.inc $(HEAP_INC)
SSINT_INC = $(QB)\hd\scanner.inc $(QB)\hd\ssint.inc $(QB)\hd\architec.inc
VARIABLE_H = $(QB5)\hd\variable.h $(QB)\hd\varmgr.h $(QB)\hd\heap.h
VARIABLE_INC = $(QB5)\hd\variable.inc $(QB)\hd\varmgr.inc $(HEAP_INC) $(QB)\hd\array.inc $(QB)\hd\types.inc
VERSION_H = version.h $(QB)\hd\debug.h $(QB)\hd\interp.h $(QB)\hd\switch.h
VERSION_INC = version.inc $(QB)\hd\debug.inc $(QB)\hd\cmacros.inc \
	$(QB)\hd\interp.inc $(QB)\hd\switch.inc $(QB)\hd\idmac.inc


#--------------------------------------------------------------------
# C source files (alphabetical order)
#--------------------------------------------------------------------
vardbg.obj: $(QB5)\ir\vardbg.c $(VERSION_H) $(QB)\hd\conint.h $(CONTEXT_H) \
	$(QB)\hd\heap.h $(QB)\hd\names.h qbimsgs.h $(VARIABLE_H)
	$(TL)\szscrn "Compiling  vardbg.c:"
	if exist vardbg.obj del vardbg.obj
	$(TL)\cl $(VARFLAGS) $(CC) -Zp -NT DBG $(QB5)\ir\vardbg.c

varmgrc.obj: $(QB5)\ir\varmgrc.c $(VERSION_H) $(QB)\hd\conint.h $(CONTEXT_H) \
	$(QB)\hd\executor.h $(QB)\hd\names.h $(PARSER_H) qbimsgs.h  \
	$(QB)\hd\scanner.h $(QB)\hd\txtmgr.h $(QB)\hd\util.h $(VARIABLE_H)
	$(TL)\szscrn "Compiling  varmgrc.c:"
	if exist varmgrc.obj del varmgrc.obj
	$(TL)\cl $(VARFLAGS) $(CC) -Zp -NT CP $(QB5)\ir\varmgrc.c

#--------------------------------------------------------------------
# ASM source files (alphabetical order)
#--------------------------------------------------------------------
excontxt.obj: $(QB5)\ir\excontxt.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(QB)\hd\conint.inc $(QB)\hd\context.inc $(EXECUTOR_INC) $(EXTORT_INC) \
	qbimsgs.inc $(QB)\hd\scanner.inc $(QB)\hd\rtinterp.inc \
	$(QB)\hd\rtps.inc $(QB)\hd\rttemp.inc $(QB)\hd\txtmgr.inc \
	$(QB)\hd\ui.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling excontxt.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB5)\ir\excontxt;

exrefarg.obj: $(QB5)\ir\exrefarg.asm $(VERSION_INC) $(EXECUTOR_INC) \
	$(EXTORT_INC) opstmt.inc $(QB)\hd\rtinterp.inc $(VARIABLE_INC) \
	$(CONTEXT_INC) $(QB)\hd\pointers.inc
	$(TL)\szscrn "Assembling exrefarg.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB5)\ir\exrefarg;

init.obj: $(QB5)\ir\init.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(QB)\hd\context.inc $(EXECUTOR_INC) $(HEAP_INC) $(PARSER_INC) \
	qbimsgs.inc $(QB)\hd\rtinterp.inc $(QB)\hd\rtps.inc $(QB)\hd\ui.inc
	$(TL)\szscrn "Assembling init.asm"
	$(TL)\masm $(MASM) $(ROMBAS) $(QB5)\ir\init;

rterror.obj: $(QB5)\ir\rterror.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(EXECUTOR_INC) $(QB)\hd\exint.inc \
	$(HEAP_INC) $(QB)\hd\messages.inc $(QB)\hd\names.inc \
	opcontrl.inc qbimsgs.inc $(QB)\hd\rtinterp.inc $(QB)\hd\rtps.inc \
	$(QB)\hd\txtmgr.inc $(QB)\hd\ui.inc $(QB)\hd\util.inc
	$(TL)\szscrn "Assembling rterror.asm"
	$(TL)\masm $(MASM) $(QB5)\ir\rterror;

varutil.obj: $(QB5)\ir\varutil.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(QB)\hd\conint.inc $(QB)\hd\context.inc $(QB)\hd\executor.inc \
	$(HEAP_INC) $(QB)\hd\names.inc qbimsgs.inc \
	$(QB)\hd\rtinterp.inc $(QB)\hd\scanner.inc $(QB)\hd\util.inc \
	$(VARIABLE_INC)
	$(TL)\szscrn "Assembling varutil.asm"
	$(TL)\masm $(MASM) $(QB5)\ir\varutil;
