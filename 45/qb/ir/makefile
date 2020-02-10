#--------------------------------------------------------------------
# File: $(QB)\ir\makefile:
# This makefile contains make information for all version independent
# BASIC Interpreter Source
#
# This is used in conjunction with ..\qb\makefile and ..\test\makefile.
# For example, in a 'test' directory:
#	make qb3.exe -f makefile -f ..\qb\makefile $(QB)\ir\makefile
#--------------------------------------------------------------------

# header file dependancies:
# These first lines represent header files which include other header files
#
HEAP_INC = $(QB)\hd\heap.inc $(QB)\hd\array.inc $(QB)\hd\pointers.inc
CONTEXT_H = $(QB)\hd\context.h $(QB)\hd\heap.h $(QB)\hd\array.inc
CONTEXT_INC = $(QB)\hd\context.inc $(HEAP_INC) $(QB)\hd\pointers.inc $(SB_INC)
EXECUTOR_INC = $(QB)\hd\executor.inc $(QB)\hd\exint.inc opcodes.inc \
	$(QB)\hd\pointers.inc $(SB_INC)
EXTORT_INC = $(QB)\hd\rtmint.inc $(QB)\hd\extort.inc
LMEM_INC = $(QB)\hd\lmem.inc $(SB_INC)
PARSER_H = $(QB)\hd\parser.h $(QB)\hd\heap.h
PARSER_INC = $(QB)\hd\parser.inc $(HEAP_INC)
SSINT_INC = $(QB)\hd\scanner.inc $(QB)\hd\ssint.inc $(QB)\hd\architec.inc
VERSION_H = version.h $(QB)\hd\debug.h $(QB)\hd\interp.h $(QB)\hd\switch.h
VERSION_INC = version.inc $(QB)\hd\debug.inc $(QB)\hd\cmacros.inc \
	$(QB)\hd\interp.inc $(QB)\hd\switch.inc $(QB)\hd\idmac.inc

#--------------------------------------------------------------------
# Program generated source files (alphabetical order)
#--------------------------------------------------------------------
optables.inc optables.h lsoptab.asm opcodes.inc opcodes.h ssoptab.asm \
	opnamatr.asm ssoprule.inc opid.inc opmin.inc opcontrl.inc \
	opintrsc.inc opstmt.inc opid.h opmin.h opcontrl.h opintrsc.h \
	opstmt.h: $(PEROPCOD_TXT) $(TL)\optabler.exe
	$(TL)\szscrn "making opcodes.inc opcodes.h ssoptab.asm optables.inc "
	$(TL)\szscrn "       optables.h opnamatr.asm ssoprule.inc & lsoptab.asm"
	$(TL)\optabler.exe $(PEROPCOD_TXT) $(PROD)

prstab.inc prstab.h prsirw.inc prsorw.inc prsstate.asm prsrwt.asm contexts: \
	$(BNF_PRS) $(TL)\buildprs.exe
	$(TL)\szscrn "making parser tables from bnf.prs (takes about 4 minutes)"
	$(TL)\buildprs.exe -O1 -v <$(BNF_PRS)

qbimsgs.inc qbimsgs.h qbimsgs.asm : $(QBIMSGS_TXT) $(MAKEMSGS_EXE)
	$(TL)\szscrn "making qbimsgs.asm qbimsgs.inc and qbimsgs.h"
	$(MAKEMSGS_EXE) <$(QBIMSGS_TXT)

localSources: optables.inc optables.h lsoptab.asm opcodes.inc opcodes.h \
	prstab.inc prstab.h prsirw.inc prsorw.inc prsstate.asm prsrwt.asm \
	contexts \
	ssoptab.asm opnamatr.asm ssoprule.inc opid.inc opmin.inc \
	opcontrl.inc opintrsc.inc opstmt.inc opid.h opmin.h opcontrl.h \
	opintrsc.h opstmt.h qbimsgs.inc qbimsgs.h qbimsgs.asm
	$(TL)\szscrn "local sources are all up to date"

#--------------------------------------------------------------------
# C source files (alphabetical order)
#--------------------------------------------------------------------
condebug.obj: $(QB)\ir\condebug.c $(VERSION_H) $(QB)\hd\conint.h $(CONTEXT_H) \
	$(QB)\hd\heap.h $(QB)\hd\names.h qbimsgs.h $(QB)\hd\txtmgr.h \
	$(VARIABLE_H)
	$(TL)\szscrn "Compiling  condebug.c:"
	if exist condebug.obj del condebug.obj
	$(TL)\cl $(DEBFLAGS) $(CC) -Zp -NT DBG $(QB)\ir\condebug.c

debug.obj: $(QB)\ir\debug.c $(VERSION_H) $(CONTEXT_H) $(QB)\hd\heap.h \
	$(QB)\hd\txtmgr.h $(VARIABLE_H)
	$(TL)\szscrn "Compiling  debug.c:"
	if exist debug.obj del debug.obj
	$(TL)\cl $(DEBFLAGS) $(CC) -Zp -NT DBG $(QB)\ir\debug.c

dshcmd.obj: $(QB)\ir\dshcmd.c version.h $(QB)\hd\context.h $(QB)\hd\conint.h \
	$(QB)\hd\debug.h $(QB)\hd\executor.h $(QB)\hd\heap.h $(QB)\hd\lister.h \
	$(QB)\hd\names.h $(PARSER_H) \
	$(QB)\hd\rtinterp.h \
	$(QB)\hd\rttemp.h $(QB)\hd\scanner.h $(QB)\hd\txtmgr.h $(QB)\hd\ui.h \
	$(QB)\hd\util.h $(VARIABLE_H)
	$(TL)\szscrn "Compiling  dshcmd.c:"
	if exist dshcmd.obj del dshcmd.obj
	$(TL)\cl $(UIFLAGS) $(CC) -Zp -NT DBG $(QB)\ir\dshcmd.c

dshstrng.obj: $(QB)\ir\dshstrng.c version.h $(QB)\hd\scanner.h $(QB)\hd\ui.h
	$(TL)\szscrn "Compiling  dshstrng.c:"
	if exist dshstrng.obj del dshstrng.obj
	$(TL)\cl $(UIFLAGS) $(CC) -Zp -NT DBG $(QB)\ir\dshstrng.c

prsdebug.obj: $(QB)\ir\prsdebug.c $(VERSION_H) $(CONTEXT_H) opcodes.h \
	$(PARSER_H) prstab.h $(QB)\hd\psint.h qbimsgs.h
	$(TL)\szscrn "Compiling  prsdebug.c:"
	if exist prsdebug.obj del prsdebug.obj
	$(TL)\cl $(PRSFLAGS) $(CC) -Zp -NT DBG $(QB)\ir\prsdebug.c

qbidata.obj: $(QB)\ir\qbidata.c $(VERSION_H) $(CONTEXT_H) $(VARIABLE_H) \
	$(PARSER_H) $(QB)\hd\scanner.h $(QB)\hd\executor.h $(QB)\hd\psint.h \
	$(QB)\hd\rtinterp.h $(QB)\hd\txtmgr.h $(QB)\hd\txtint.h $(QB)\hd\ui.h
	$(TL)\szscrn "Compiling  qbidata.c:"
	if exist qbidata.obj del qbidata.obj
	$(TL)\cl $(DEBFLAGS) $(CC) -Zp -NT CP $(QB)\ir\qbidata.c

txtdebug.obj: $(QB)\ir\txtdebug.c $(VERSION_H) $(CONTEXT_H) opcodes.h \
	$(PARSER_H) optables.h $(QB)\hd\pcode.inc prstab.h qbimsgs.h \
	$(QB)\hd\rtinterp.h $(QB)\hd\scanner.h $(QB)\hd\txtint.h $(QB)\hd\txtmgr.h \
	$(QB)\hd\ui.h $(VARIABLE_H)
	$(TL)\szscrn "Compiling  txtdebug.c:"
	if exist txtdebug.obj del txtdebug.obj
	$(TL)\cl $(TXTFLAGS) $(CC) -Zp -NT DBG $(QB)\ir\txtdebug.c

vardebug.obj: $(QB)\ir\vardebug.c $(VERSION_H) $(QB)\hd\conint.h $(CONTEXT_H) \
	$(QB)\hd\heap.h $(QB)\hd\names.h qbimsgs.h $(VARIABLE_H)
	$(TL)\szscrn "Compiling  vardebug.c:"
	if exist vardebug.obj del vardebug.obj
	$(TL)\cl $(VARFLAGS) $(CC) -Zp -NT DBG $(QB)\ir\vardebug.c


#--------------------------------------------------------------------
# ASM source files (alphabetical order)
#--------------------------------------------------------------------
bddebug.obj: $(QB)\ir\bddebug.asm $(VERSION_INC) $(CONTEXT_INC) \
	$(HEAP_INC) $(QB)\hd\rttemp.inc $(QB)\hd\util.inc
	$(TL)\szscrn "Assembling bddebug.asm"
	$(TL)\masm $(MASM) $(QB)\ir\bddebug;

bdmgr.obj: $(QB)\ir\bdmgr.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(HEAP_INC) $(PARSER_INC) \
	$(QB)\hd\txtmgr.inc $(QB)\hd\util.inc
	$(TL)\szscrn "Assembling bdmgr.asm"
	$(TL)\masm $(MASM) $(QB)\ir\bdmgr;

binsav.obj: $(QB)\ir\binsav.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(QB)\hd\conint.inc $(CONTEXT_INC) $(HEAP_INC) \
	$(QB)\hd\names.inc $(QB)\hd\qblist.inc qbimsgs.inc \
	$(QB)\hd\rtinterp.inc $(QB)\hd\rtps.inc $(QB)\hd\rttemp.inc \
	$(SB_INC) $(QB)\hd\scanner.inc $(QB)\hd\txtint.inc \
	$(QB)\hd\txtmgr.inc $(QB)\hd\ui.inc $(QB)\hd\util.inc
	$(TL)\szscrn "Assembling binsav.asm"
	$(TL)\masm $(MASM) $(QB)\ir\binsav;

binsav.obc: binsav.obj
	$(TL)\szscrn "Assembling compiler version of binsav.asm"
	$(TL)\masm -DNO_EXTRNS -DBCLIST $(MASM) $(QB)\ir\binsav,binsav.obc;

binxlat.obj: $(QB)\ir\binxlat.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(QB)\hd\conint.inc $(CONTEXT_INC) $(QB)\hd\pcode.inc qbimsgs.inc \
	$(QB)\hd\rtps.inc $(QB)\hd\txtint.inc $(QB)\hd\txtmgr.inc $(SSINT_INC)
	$(TL)\szscrn "Assembling binxlat.asm"
	$(TL)\masm $(MASM) -I$(QB5)\hdcw $(QB)\ir\binxlat;

binxlat.obc: binxlat.obj
	$(TL)\szscrn "Assembling compiler version of binxlat.asm"
	$(TL)\masm -DNO_EXTRNS -DBCLIST -I. -I$(QB)\hd -I$(TL)\..\inc -I$(QB5)\hdcw $(QB)\ir\binxlat,binxlat.obc;

conmisc.obj: $(QB)\ir\conmisc.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(QB)\hd\conint.inc $(CONTEXT_INC) $(EXECUTOR_INC) \
	$(HEAP_INC) $(QB)\hd\names.inc $(PARSER_INC) qbimsgs.inc \
	$(QB)\hd\rtinterp.inc $(QB)\hd\rtps.inc $(QB)\hd\txtmgr.inc \
	$(QB)\hd\scanner.inc $(QB)\hd\ui.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling conmisc.asm"
	$(TL)\masm $(MASM) $(QB)\ir\conmisc;

context.obj: $(QB)\ir\context.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(QB)\hd\conint.inc $(CONTEXT_INC) $(HEAP_INC) \
	$(QB)\hd\names.inc $(PARSER_INC) $(QB)\hd\pcode.inc qbimsgs.inc \
	$(QB)\hd\rtinterp.inc $(SB_INC) $(QB)\hd\scanner.inc \
	$(QB)\hd\txtmgr.inc $(QB)\hd\ui.inc $(QB)\hd\util.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling context.asm"
	$(TL)\masm $(MASM) $(QB)\ir\context;

exaid.obj: $(QB)\ir\exaid.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(QB)\hd\array.inc $(CONTEXT_INC) $(EXECUTOR_INC) $(EXTORT_INC) \
	$(QB)\hd\pcode.inc qbimsgs.inc $(QB)\hd\rtinterp.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling exaid.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exaid;

exarray.obj: $(QB)\ir\exarray.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(EXECUTOR_INC) $(EXTORT_INC) $(LMEM_INC) \
	$(QB)\hd\pcode.inc $(QB)\hd\scanner.inc $(QB)\hd\ui.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling exarray.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exarray;

exaryutl.obj: $(QB)\ir\exaryutl.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(QB)\hd\array.inc $(CONTEXT_INC) $(EXECUTOR_INC) qbimsgs.inc \
	$(QB)\hd\rtinterp.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling exaryutl.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exaryutl;

excase.obj: $(QB)\ir\excase.asm $(VERSION_INC) $(EXECUTOR_INC) \
	opcontrl.inc qbimsgs.inc $(QB)\hd\rtinterp.inc
	$(TL)\szscrn "Assembling excase.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\excase;

exconv.obj: $(QB)\ir\exconv.asm $(VERSION_INC) $(CONTEXT_INC) \
	$(EXECUTOR_INC) $(QB)\hd\rtinterp.inc
	$(TL)\szscrn "Assembling exconv.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exconv;

exdebug.obj: $(QB)\ir\exdebug.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(EXECUTOR_INC) $(EXTORT_INC) $(QB)\hd\ui.inc $(CONTEXT_INC)
	$(TL)\szscrn "Assembling exdebug.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exdebug;

exevent.obj: $(QB)\ir\exevent.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(EXECUTOR_INC) $(EXTORT_INC) $(QB)\hd\rttemp.inc
	$(TL)\szscrn "Assembling exevent.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exevent;

exfnmisc.obj: $(QB)\ir\exfnmisc.asm $(VERSION_INC) \
	$(QB)\hd\architec.inc $(CONTEXT_INC) $(EXECUTOR_INC) \
	$(EXTORT_INC) $(HEAP_INC) $(QB)\hd\rttemp.inc
	$(TL)\szscrn "Assembling exfnmisc.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exfnmisc;

exfor.obj: $(QB)\ir\exfor.asm $(VERSION_INC) $(CONTEXT_INC) \
	$(EXECUTOR_INC) $(QB)\hd\rtinterp.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling exfor.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exfor;

exgoto.obj: $(QB)\ir\exgoto.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(EXECUTOR_INC) qbimsgs.inc $(QB)\hd\rtinterp.inc \
	$(QB)\hd\rtps.inc $(QB)\hd\txtmgr.inc
	$(TL)\szscrn "Assembling exgoto.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exgoto;

exgraph.obj: $(QB)\ir\exgraph.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(QB)\hd\array.inc $(CONTEXT_INC) $(EXECUTOR_INC) $(EXTORT_INC) \
	$(QB)\hd\rttemp.inc
	$(TL)\szscrn "Assembling exgraph.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exgraph;

exid.obj: $(QB)\ir\exid.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(EXECUTOR_INC) $(EXTORT_INC) $(LMEM_INC) \
	$(QB)\hd\pointers.inc $(QB)\hd\scanner.inc $(VARIABLE_INC) \
	$(QB)\hd\pcode.inc
	$(TL)\szscrn "Assembling exid.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exid;

exif.obj: $(QB)\ir\exif.asm $(VERSION_INC) $(EXECUTOR_INC)
	$(TL)\szscrn "Assembling exif.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exif;

exio.obj: $(QB)\ir\exio.asm $(VERSION_INC) $(CONTEXT_INC) \
	$(QB)\hd\architec.inc $(EXECUTOR_INC) $(EXTORT_INC) \
	$(QB)\hd\rtps.inc $(QB)\hd\rttemp.inc
	$(TL)\szscrn "Assembling exio.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exio;

exlit.obj: $(QB)\ir\exlit.asm $(VERSION_INC) $(EXECUTOR_INC) $(EXTORT_INC) \
		$(VARIABLE_INC)
	$(TL)\szscrn "Assembling exlit.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exlit;

exmathop.obj: $(QB)\ir\exmathop.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(EXECUTOR_INC) $(EXTORT_INC) opcontrl.inc \
	qbimsgs.inc $(QB)\hd\rtinterp.inc $(QB)\hd\ssint.inc
	$(TL)\szscrn "Assembling exmathop.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exmathop;

bfpsig.obj: $(QB)\ir\bfpsig.asm $(VERSION_INC) $(CONTEXT_INC) \
	$(EXECUTOR_INC) qbimsgs.inc
	$(TL)\szscrn "Assembling bfpsig.asm
	$(TL)\masm $(MASM)  $(QB)\ir\bfpsig.asm $(EXFLAGS);

exmisc.obj: $(QB)\ir\exmisc.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(QB)\hd\conint.inc $(CONTEXT_INC) $(EXECUTOR_INC) \
	$(QB)\hd\pcode.inc $(QB)\hd\txtmgr.inc $(QB)\hd\ui.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling exmisc.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exmisc;

exos.obj: $(QB)\ir\exos.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(EXECUTOR_INC) $(EXTORT_INC) $(QB)\hd\msdos.inc \
	qbimsgs.inc $(QB)\hd\rttemp.inc $(QB)\hd\ui.inc
	$(TL)\szscrn "Assembling exos.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exos;

exprint.obj: $(QB)\ir\exprint.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(EXECUTOR_INC) $(EXTORT_INC) opstmt.inc \
	$(QB)\hd\rttemp.inc $(QB)\hd\ui.inc
	$(TL)\szscrn "Assembling exprint.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exprint;

exproc.obj: $(QB)\ir\exproc.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(EXECUTOR_INC) $(EXTORT_INC) $(LMEM_INC) \
	opstmt.inc qbimsgs.inc $(QB)\hd\rtinterp.inc $(QB)\hd\rtps.inc \
	$(QB)\hd\scanner.inc $(QB)\hd\ui.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling exproc.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exproc;

exrec.obj: $(QB)\ir\exrec.asm $(VERSION_INC) $(CONTEXT_INC) \
	$(EXECUTOR_INC) $(EXTORT_INC) $(QB)\hd\scanner.inc $(VARIABLE_INC) \
	$(QB)\hd\pcode.inc
	$(TL)\szscrn "Assembling exrec.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exrec;

exstmisc.obj: $(QB)\ir\exstmisc.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(EXECUTOR_INC) $(EXTORT_INC) qbimsgs.inc \
	$(QB)\hd\rtinterp.inc $(QB)\hd\rttemp.inc
	$(TL)\szscrn "Assembling exstmisc.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exstmisc;

exstring.obj: $(QB)\ir\exstring.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(EXECUTOR_INC) $(EXTORT_INC) $(QB)\hd\pcode.inc \
	$(QB)\hd\rttemp.inc $(QB)\hd\ui.inc
	$(TL)\szscrn "Assembling exstring.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exstring;

exststub.obj: $(QB)\ir\exststub.asm $(VERSION_INC) $(EXECUTOR_INC) \
	optables.inc $(QB)\hd\ssint.inc
	$(TL)\szscrn "Assembling exststub.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\exststub;

extort.obj: $(QB)\ir\extort.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(EXECUTOR_INC) $(QB)\hd\rtmint.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling extort.asm"
	$(TL)\masm $(MASM) $(EXFLAGS) $(QB)\ir\extort;

lsid.obj: $(QB)\ir\lsid.asm $(VERSION_INC) $(QB)\hd\architec.inc $(CONTEXT_INC) \
	$(QB)\hd\lister.inc $(QB)\hd\lsint.inc $(QB)\hd\names.inc \
	opcodes.inc $(QB)\hd\pcode.inc prsorw.inc $(QB)\hd\qblist.inc \
	$(QB)\hd\scanner.inc $(QB)\hd\txtmgr.inc $(QB)\hd\types.inc \
	$(VARIABLE_INC) $(CMDEQV_INC)
	$(TL)\szscrn "Assembling lsid.asm"
	$(TL)\masm $(MASM) $(QB)\ir\lsid;

lsid.obc: lsid.obj
	$(TL)\szscrn "Assembling compiler version of lsid.asm"
	$(TL)\masm -DNO_EXTRNS -DBCLIST $(MASM) $(QB)\ir\lsid,lsid.obc;

lsoptab.obj: lsoptab.asm $(VERSION_INC) prsorw.inc $(QB)\hd\psint.inc \
	$(PARSER_INC) $(QB)\hd\qblist.inc
	$(TL)\szscrn "Assembling lsoptab.asm"
	$(TL)\masm $(MASM) lsoptab;

lsoptab.obc: lsoptab.obj
	$(TL)\szscrn "Assembling compiler version of lsoptab.asm"
	$(TL)\masm -DNO_EXTRNS -DBCLIST $(MASM) lsoptab,lsoptab.obc;

lsmain.obj: $(QB)\ir\lsmain.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(CMDEQV_INC) \
	$(HEAP_INC) $(QB)\hd\lister.inc $(QB)\hd\lsint.inc $(QB)\hd\names.inc \
	opcodes.inc $(PARSER_INC) $(QB)\hd\pcode.inc \
	prstab.inc $(QB)\hd\qblist.inc \
	$(QB)\hd\rtps.inc $(QB)\hd\scanner.inc \
	$(QB)\hd\txtmgr.inc $(QB)\hd\util.inc
	$(TL)\szscrn "Assembling lsmain.asm"
	$(TL)\masm $(MASM) $(QB)\ir\lsmain;

lsmain.obc: lsmain.obj
	$(TL)\szscrn "Assembling compiler version of lsmain.asm"
	$(TL)\masm -DNO_EXTRNS -DBCLIST $(MASM) $(QB)\ir\lsmain,lsmain.obc;

lsrules.obj: $(QB)\ir\lsrules.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC)	$(HEAP_INC) $(QB)\hd\lister.inc \
	$(QB)\hd\lsint.inc prsorw.inc \
	$(QB)\hd\qblist.inc $(QB)\hd\rtps.inc $(QB)\hd\scanner.inc
	$(TL)\szscrn "Assembling lsrules.asm"
	$(TL)\masm $(MASM) $(QB)\ir\lsrules;

lsrules.obc: lsrules.obj
	$(TL)\szscrn "Assembling compiler version of lsrules.asm"
	$(TL)\masm -DNO_EXTRNS -DBCLIST $(MASM) $(QB)\ir\lsrules,lsrules.obc;

lsutil.obj: $(QB)\ir\lsutil.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(HEAP_INC) $(QB)\hd\lister.inc $(QB)\hd\lsint.inc \
	$(QB)\hd\names.inc $(QB)\hd\qblist.inc $(QB)\hd\scanner.inc $(QB)\hd\txtmgr.inc \
	$(VARIABLE_INC)
	$(TL)\szscrn "Assembling lsutil.asm"
	$(TL)\masm $(MASM) $(QB)\ir\lsutil;

lsutil.obc: lsutil.obj
	$(TL)\szscrn "Assembling compiler version of lsutil.asm"
	$(TL)\masm -DNO_EXTRNS -DBCLIST $(MASM) $(QB)\ir\lsutil,lsutil.obc;

87toalt.obj: $(QB)\ir\87toalt.asm $(VERSION_INC)
	$(TL)\szscrn "Assembling 87toalt.asm"
	$(TL)\masm $(MASM) $(QB)\ir\87toalt;

namdebug.obj: $(QB)\ir\namdebug.asm $(VERSION_INC) $(QB)\hd\conint.inc \
	$(CONTEXT_INC) $(HEAP_INC) $(QB)\hd\names.inc
	$(TL)\szscrn "Assembling namdebug.asm"
	$(TL)\masm $(MASM) $(QB)\ir\namdebug;

nammgr.obj: $(QB)\ir\nammgr.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(HEAP_INC) $(QB)\hd\names.inc qbimsgs.inc \
	$(SB_INC) $(QB)\hd\ui.inc $(QB)\hd\util.inc
	$(TL)\szscrn "Assembling nammgr.asm"
	$(TL)\masm $(MASM) $(QB)\ir\nammgr;

opnamatr.obj: opnamatr.asm $(VERSION_INC) opcodes.inc
	$(TL)\szscrn "Assembling opnamatr.asm"
	$(TL)\masm $(MASM) opnamatr;

prscg.obj: $(QB)\ir\prscg.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(HEAP_INC) opcodes.inc $(PARSER_INC) \
	$(QB)\hd\pcode.inc prsirw.inc prstab.inc $(QB)\hd\psint.inc \
	qbimsgs.inc $(QB)\hd\rtps.inc $(QB)\hd\scanner.inc $(QB)\hd\txtmgr.inc \
	$(QB)\hd\ui.inc $(QB)\hd\util.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling prscg.asm:"
	$(TL)\masm $(MASM) $(QB)\ir\prscg;

prsctl.obj: $(QB)\ir\prsctl.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(QB)\hd\names.inc \
	opcodes.inc $(PARSER_INC) prsirw.inc prstab.inc \
	$(QB)\hd\psint.inc qbimsgs.inc
	$(TL)\szscrn "Assembling prsctl.asm:"
	$(TL)\masm $(MASM) $(QB)\ir\prsctl;

prsexp.obj: $(QB)\ir\prsexp.asm $(VERSION_INC) opcodes.inc $(PARSER_INC) \
	prstab.inc $(QB)\hd\psint.inc qbimsgs.inc $(VARIABLE_INC) $(CMDEQV_INC)
	$(TL)\szscrn "Assembling prsexp.asm:"
	$(TL)\masm $(MASM) $(QB)\ir\prsexp;

prsid.obj: $(QB)\ir\prsid.asm $(VERSION_INC) $(QB)\hd\architec.inc $(CONTEXT_INC) \
	$(QB)\hd\names.inc opcodes.inc $(PARSER_INC) \
	$(QB)\hd\pcode.inc prsirw.inc \
	prstab.inc $(QB)\hd\psint.inc qbimsgs.inc $(QB)\hd\scanner.inc \
	$(QB)\hd\txtmgr.inc $(QB)\hd\ui.inc $(QB)\hd\util.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling prsid.asm:"
	$(TL)\masm $(MASM) $(QB)\ir\prsid;

prslex.obj: $(QB)\ir\prslex.asm \
	$(VERSION_INC) \
	$(QB)\hd\architec.inc \
	$(CONTEXT_INC) \
	$(QB)\hd\exint.inc $(QB)\hd\pointers.inc $(SB_INC) \
	$(HEAP_INC) \
	$(QB)\hd\names.inc \
	$(PARSER_INC) \
	prsirw.inc \
	prstab.inc \
	$(QB)\hd\psint.inc \
	qbimsgs.inc \
	$(QB)\hd\rtinterp.inc \
	$(QB)\hd\txtmgr.inc \
	$(VARIABLE_INC) $(CMDEQV_INC)
	$(TL)\szscrn "Assembling prslex.asm"
	$(TL)\masm $(MASM) $(QB)\ir\prslex;

prsmain.obj: $(QB)\ir\prsmain.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(HEAP_INC) $(QB)\hd\names.inc \
	opcodes.inc $(QB)\hd\stack2.inc \
	$(PARSER_INC) $(QB)\hd\pcode.inc prsirw.inc prstab.inc \
	$(QB)\hd\psint.inc qbimsgs.inc $(QB)\hd\rtinterp.inc $(QB)\hd\scanner.inc \
	$(QB)\hd\txtmgr.inc $(QB)\hd\ui.inc $(QB)\hd\util.inc
	$(TL)\szscrn "Assembling prsmain.asm:"
	$(TL)\masm $(MASM) $(QB)\ir\prsmain;

prsnt.obj: $(QB)\ir\prsnt.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) \
	opcodes.inc \
	$(PARSER_INC) \
	prsirw.inc \
	prstab.inc \
	$(QB)\hd\psint.inc \
	qbimsgs.inc \
	$(QB)\hd\rtinterp.inc \
	$(QB)\hd\rtps.inc
	$(TL)\szscrn "Assembling prsnt.asm:"
	$(TL)\masm $(MASM) $(QB)\ir\prsnt;

prsnt1.obj: $(QB)\ir\prsnt1.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(QB)\hd\names.inc opcodes.inc $(PARSER_INC) prstab.inc \
	$(QB)\hd\psint.inc qbimsgs.inc $(QB)\hd\util.inc $(CMDEQV_INC)
	$(TL)\szscrn "Assembling prsnt1.asm:"
	$(TL)\masm $(MASM) $(QB)\ir\prsnt1;

prsrwt.obj: prsrwt.asm $(VERSION_INC) opcodes.inc $(PARSER_INC) \
	$(QB)\hd\psint.inc
	$(TL)\szscrn "Assembling prsrwt.asm"
	$(TL)\masm $(MASM) prsrwt;

prsrwt.obc: prsrwt.obj
	$(TL)\szscrn "Assembling compiler version of prsrwt.asm"
	$(TL)\masm -DNO_EXTRNS -DBCLIST $(MASM) prsrwt,prsrwt.obc;

prsstmt.obj: $(QB)\ir\prsstmt.asm $(VERSION_INC) \
	$(QB)\hd\architec.inc $(CONTEXT_INC) \
	$(QB)\hd\msgshort.inc opcodes.inc $(PARSER_INC) prsirw.inc \
	prstab.inc $(QB)\hd\psint.inc $(QB)\hd\util.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling prsstmt.asm:"
	$(TL)\masm $(MASM) $(QB)\ir\prsstmt;

prsstate.obj: prsstate.asm $(VERSION_INC) opcodes.inc $(PARSER_INC) \
	qbimsgs.inc
	$(TL)\szscrn "Assembling prsstate.asm"
	$(TL)\masm $(MASM) prsstate;

prsutil.obj: $(QB)\ir\prsutil.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(HEAP_INC) $(QB)\hd\lister.inc $(PARSER_INC) \
	prsirw.inc prstab.inc $(QB)\hd\psint.inc qbimsgs.inc \
	$(QB)\hd\scanner.inc $(QB)\hd\txtmgr.inc \
	$(VARIABLE_INC) $(QB)\hd\ui.inc
	$(TL)\szscrn "Assembling prsutil.asm"
	$(TL)\masm $(MASM) $(QB)\ir\prsutil;

qblist.obc: $(QB)\ir\qblist.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(HEAP_INC) $(QB)\hd\names.inc \
	$(QB)\hd\qblist.inc $(QB)\hd\pcode.inc $(QB)\hd\rtps.inc
	$(TL)\szscrn "Assembling qblist.asm"
	$(TL)\masm -DNO_EXTRNS -DBCLIST $(MASM) $(QB)\ir\qblist,qblist.obc;

qblststb.obj: $(QB)\ir\qblststb.asm
	$(TL)\szscrn "Assembling qblststb.asm"
	$(TL)\masm $(MASM) $(QB)\ir\qblststb;

qblststb.obc: qblststb.obj
	$(TL)\szscrn "Assembling qblststb.asm"
	$(TL)\masm -DBCLIST $(MASM) $(QB)\ir\qblststb,qblststb.obc;

qbimsgs.obj: qbimsgs.asm
	$(TL)\szscrn "Assembling qbimsgs.asm"
	$(TL)\masm $(MASM) qbimsgs;

rsalpha.obj: $(QB)\ir\rsalpha.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(HEAP_INC) \
	$(QB)\hd\names.inc $(QB)\hd\util.inc
	$(TL)\szscrn "Assembling rsalpha.asm"
	$(TL)\masm $(MASM) $(QB)\ir\rsalpha;

rttemp.obj: $(QB)\ir\rttemp.asm $(VERSION_INC) $(EXECUTOR_INC) \
	$(QB)\hd\msdos.inc $(QB)\hd\rttemp.inc
	$(TL)\szscrn "Assembling rttemp.asm"
	$(TL)\masm $(MASM) $(QB)\ir\rttemp;

ssaid.obj: $(QB)\ir\ssaid.asm $(VERSION_INC) $(CONTEXT_INC) \
	$(QB)\hd\executor.inc $(QB)\hd\pcode.inc qbimsgs.inc $(SSINT_INC) \
	$(QB)\hd\txtmgr.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling ssaid.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssaid;

ssbos.obj: $(QB)\ir\ssbos.asm $(VERSION_INC) $(VARIABLE_INC) \
	$(SSINT_INC) $(CONTEXT_INC) $(HEAP_INC) qbimsgs.inc \
	optables.inc $(QB)\hd\pcode.inc $(QB)\hd\ui.inc
	$(TL)\szscrn "Assembling ssbos.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssbos;

sscase.obj: $(QB)\ir\sscase.asm $(VERSION_INC) $(SSINT_INC) \
	$(QB)\hd\architec.inc qbimsgs.inc $(QB)\hd\txtmgr.inc
	$(TL)\szscrn "Assembling sscase.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\sscase;

ssdata.obj: $(QB)\ir\ssdata.asm $(VERSION_INC)
	$(TL)\szscrn "Assembling ssdata.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssdata;

ssdeclar.obj: $(QB)\ir\ssdeclar.asm $(VERSION_INC) $(CONTEXT_INC) \
	$(QB)\hd\executor.inc optables.inc $(QB)\hd\pcode.inc qbimsgs.inc \
	$(SSINT_INC) $(QB)\hd\txtmgr.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling ssdeclar.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssdeclar;

ssdescan.obj: $(QB)\ir\ssdescan.asm $(VERSION_INC) $(SSINT_INC) \
	$(VARIABLE_INC)  $(CONTEXT_INC) \
	opcontrl.inc opmin.inc optables.inc $(QB)\hd\pcode.inc $(QB)\hd\txtmgr.inc
	$(TL)\szscrn "Assembling ssdescan.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssdescan;

ssdo.obj: $(QB)\ir\ssdo.asm $(VERSION_INC) $(SSINT_INC) \
	qbimsgs.inc
	$(TL)\szscrn "Assembling ssdo.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssdo;

ssfor.obj: $(QB)\ir\ssfor.asm $(VERSION_INC) $(VARIABLE_INC) \
	$(SSINT_INC) $(CONTEXT_INC) qbimsgs.inc $(QB)\hd\txtmgr.inc
	$(TL)\szscrn "Assembling ssfor.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssfor;

ssid.obj: $(QB)\ir\ssid.asm $(VERSION_INC) $(CONTEXT_INC) opid.inc \
	$(QB)\hd\pcode.inc qbimsgs.inc $(SSINT_INC) $(QB)\hd\txtmgr.inc \
	$(VARIABLE_INC)
	$(TL)\szscrn "Assembling ssid.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssid;

ssif.obj: $(QB)\ir\ssif.asm $(VERSION_INC) $(VARIABLE_INC) \
	$(SSINT_INC) $(QB)\hd\architec.inc $(CONTEXT_INC) qbimsgs.inc
	$(TL)\szscrn "Assembling ssif.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssif;

sslit.obj: $(QB)\ir\sslit.asm $(VERSION_INC) $(SSINT_INC) optables.inc
	$(TL)\szscrn "Assembling sslit.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\sslit;

ssmisc.obj: $(QB)\ir\ssmisc.asm $(VERSION_INC) $(QB)\hd\conint.inc \
	$(CONTEXT_INC) qbimsgs.inc $(SSINT_INC) $(QB)\hd\txtmgr.inc \
	$(VARIABLE_INC) $(CMDEQV_INC)
	$(TL)\szscrn "Assembling ssmisc.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssmisc;

ssoperat.obj: $(QB)\ir\ssoperat.asm $(VERSION_INC) $(QB)\hd\pcode.inc \
	qbimsgs.inc  $(SSINT_INC) $(VARIABLE_INC)
	$(TL)\szscrn "Assembling ssoperat.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssoperat;

ssoptab.obj: ssoptab.asm $(VERSION_INC)
	$(TL)\szscrn "Assembling ssoptab.asm"
	$(TL)\masm $(MASM) ssoptab;

ssproc.obj: $(QB)\ir\ssproc.asm $(VERSION_INC) $(CONTEXT_INC) \
	$(QB)\hd\exint.inc $(EXTORT_INC) $(QB)\hd\names.inc \
	$(QB)\hd\pcode.inc qbimsgs.inc $(QB)\hd\rtinterp.inc $(SSINT_INC) \
	$(QB)\hd\txtmgr.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling ssproc.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssproc;

ssrec.obj: $(QB)\ir\ssrec.asm $(VERSION_INC) $(CONTEXT_INC) \
	$(QB)\hd\pcode.inc qbimsgs.inc $(SSINT_INC) $(QB)\hd\txtmgr.inc \
	$(VARIABLE_INC)
	$(TL)\szscrn "Assembling ssrec.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssrec;

ssrefarg.obj: $(QB)\ir\ssrefarg.asm $(VERSION_INC) \
	$(QB)\hd\architec.inc $(CONTEXT_INC) qbimsgs.inc $(QB)\hd\rtps.inc \
	$(SSINT_INC) $(VARIABLE_INC)
	$(TL)\szscrn "Assembling ssrefarg.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssrefarg;

ssrude.obj: $(QB)\ir\ssrude.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) opcodes.inc optables.inc $(QB)\hd\names.inc \
	$(PARSER_INC) $(QB)\hd\pcode.inc qbimsgs.inc $(SSINT_INC) \
	$(QB)\hd\txtmgr.inc $(QB)\hd\ui.inc $(VARIABLE_INC) $(CMDEQV_INC)
	$(TL)\szscrn "Assembling ssrude.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssrude;

ssrules.obj: $(QB)\ir\ssrules.asm $(VERSION_INC) $(CONTEXT_INC) \
	opintrsc.inc $(QB)\hd\pcode.inc qbimsgs.inc $(SSINT_INC) \
	ssoprule.inc $(QB)\hd\txtmgr.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling ssrules.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssrules;

ssscan.obj: $(QB)\ir\ssscan.asm $(VERSION_INC) \
	$(CONTEXT_INC) $(EXECUTOR_INC) opmin.inc optables.inc \
	$(QB)\hd\pcode.inc qbimsgs.inc $(QB)\hd\rtinterp.inc $(SSINT_INC) \
	$(QB)\hd\txtmgr.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling ssscan.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssscan;

ssstmts.obj: $(QB)\ir\ssstmts.asm $(VERSION_INC) qbimsgs.inc \
	$(QB)\hd\rtps.inc $(SSINT_INC) $(VARIABLE_INC)
	$(TL)\szscrn "Assembling ssstmts.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\ssstmts;

sstxutil.obj: $(QB)\ir\sstxutil.asm $(VERSION_INC) $(SSINT_INC) \
	$(QB)\hd\txtmgr.inc
	$(TL)\szscrn "Assembling sstxutil.asm"
	$(TL)\masm $(MASM) $(SSFLAGS) $(QB)\ir\sstxutil;

txtdata.obj: $(QB)\ir\txtdata.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(HEAP_INC) $(QB)\hd\msgshort.inc \
	$(QB)\hd\names.inc opmin.inc $(PARSER_INC) $(QB)\hd\scanner.inc \
	$(SB_INC) $(QB)\hd\txtmgr.inc $(QB)\hd\txtint.inc
	$(TL)\szscrn "Assembling txtdata.asm:"
	$(TL)\masm $(MASM) $(QB)\ir\txtdata;

txtdeb.obj: $(QB)\ir\txtdeb.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(HEAP_INC) $(QB)\hd\lister.inc $(LMEM_INC) \
	opcodes.inc $(PARSER_INC) qbimsgs.inc $(QB)\hd\scanner.inc \
	$(QB)\hd\txtmgr.inc $(QB)\hd\txtint.inc $(QB)\hd\ui.inc
	$(TL)\szscrn "Assembling txtdeb.asm:"
	$(TL)\masm $(MASM) $(QB)\ir\txtdeb;

txtdir.obj: $(QB)\ir\txtdir.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(QB)\hd\executor.inc $(QB)\hd\lister.inc opcodes.inc \
	$(PARSER_INC) $(QB)\hd\pcode.inc qbimsgs.inc $(QB)\hd\rtinterp.inc \
	$(QB)\hd\scanner.inc $(QB)\hd\txtmgr.inc $(QB)\hd\txtint.inc \
	$(QB)\hd\ui.inc
	$(TL)\szscrn "Assembling txtdir.asm:"
	$(TL)\masm $(MASM) $(QB)\ir\txtdir;

txtfind.obj: $(QB)\ir\txtfind.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(EXECUTOR_INC) $(QB)\hd\names.inc \
	opcodes.inc $(QB)\hd\pcode.inc $(QB)\hd\scanner.inc $(QB)\hd\txtmgr.inc \
	$(QB)\hd\txtint.inc $(QB)\hd\ui.inc
	$(TL)\szscrn "Assembling txtfind.asm:"
	$(TL)\masm $(MASM) $(QB)\ir\txtfind;

txtload.obj: $(QB)\ir\txtload.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(QB)\hd\fdb.inc $(HEAP_INC) $(QB)\hd\names.inc \
	opcodes.inc $(PARSER_INC) $(QB)\hd\pcode.inc qbimsgs.inc \
	$(QB)\hd\rtinterp.inc $(QB)\hd\rtps.inc $(QB)\hd\rttemp.inc \
	$(QB)\hd\scanner.inc $(QB)\hd\txtmgr.inc $(QB)\hd\stack2.inc \
	$(QB)\hd\txtint.inc $(QB)\hd\ui.inc $(QB)\hd\util.inc \
	$(QB)\hd\conint.inc
	$(TL)\szscrn "Assembling txtload.asm:"
	$(TL)\masm $(MASM) $(QB)\ir\txtload;

txtmgr.obj: $(QB)\ir\txtmgr.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(HEAP_INC) $(QB)\hd\names.inc opcodes.inc \
	$(PARSER_INC) $(QB)\hd\pcode.inc qbimsgs.inc $(QB)\hd\rtps.inc \
	$(SB_INC) $(QB)\hd\scanner.inc $(QB)\hd\txtmgr.inc \
	$(QB)\hd\txtint.inc $(QB)\hd\ui.inc $(QB)\hd\util.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling txtmgr.asm:"
	$(TL)\masm $(MASM) $(QB)\ir\txtmgr;

txtmove.obj: $(QB)\ir\txtmove.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(HEAP_INC) $(QB)\hd\conint.inc $(CONTEXT_INC) opcodes.inc \
	$(PARSER_INC) $(QB)\hd\pcode.inc $(QB)\hd\rtps.inc $(QB)\hd\scanner.inc \
	$(SB_INC) $(QB)\hd\txtint.inc $(QB)\hd\txtmgr.inc \
	$(QB)\hd\util.inc
	$(TL)\szscrn "Assembling txtmove.asm"
	$(TL)\masm $(MASM) $(QB)\ir\txtmove;

txtsave.obj: $(QB)\ir\txtsave.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(HEAP_INC) $(QB)\hd\lister.inc $(QB)\hd\names.inc \
	opcodes.inc $(PARSER_INC) $(QB)\hd\pcode.inc qbimsgs.inc \
	$(QB)\hd\rtinterp.inc $(QB)\hd\rtps.inc $(QB)\hd\rttemp.inc \
	$(QB)\hd\scanner.inc $(QB)\hd\txtmgr.inc \
	$(QB)\hd\txtint.inc $(QB)\hd\util.inc $(QB)\hd\ui.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling txtsave.asm:"
	$(TL)\masm $(MASM) $(QB)\ir\txtsave;

txtthr.obj: $(QB)\ir\txtthr.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) opcodes.inc qbimsgs.inc $(QB)\hd\txtint.inc \
	$(QB)\hd\txtmgr.inc $(QB)\hd\types.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling txtthr.asm:"
	$(TL)\masm $(MASM) $(QB)\ir\txtthr;

txtutil.obj: $(QB)\ir\txtutil.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(EXECUTOR_INC) $(HEAP_INC) \
	$(QB)\hd\lister.inc $(QB)\hd\msgshort.inc \
	$(QB)\hd\names.inc opcodes.inc \
	$(PARSER_INC) $(QB)\hd\pcode.inc qbimsgs.inc $(QB)\hd\rtinterp.inc \
	$(QB)\hd\rtps.inc $(QB)\hd\scanner.inc $(QB)\hd\txtint.inc \
	$(QB)\hd\txtmgr.inc $(QB)\hd\ui.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling txtutil.asm"
	$(TL)\masm $(MASM) $(QB)\ir\txtutil;

typmgr.obj: $(QB)\ir\typmgr.asm $(VERSION_INC) $(QB)\hd\architec.inc \
	$(CONTEXT_INC) $(HEAP_INC) $(QB)\hd\names.inc qbimsgs.inc \
	$(QB)\hd\rtps.inc $(QB)\hd\scanner.inc $(QB)\hd\txtmgr.inc $(VARIABLE_INC)
	$(TL)\szscrn "Assembling typmgr.asm"
	$(TL)\masm $(MASM) $(QB)\ir\typmgr;

uiutil.obj:	$(QB)\ir\uiutil.asm $(VERSION_INC) \
	$(QB)\hd\lister.inc qbimsgs.inc $(QB)\hd\rtinterp.inc \
	$(QB)\hd\rtps.inc $(QB)\hd\ui.inc
	$(TL)\szscrn "Assembling uiutil.asm"
	$(TL)\masm $(MASM) $(QB)\ir\uiutil;

util.obj: $(QB)\ir\util.asm $(VERSION_INC) $(HEAP_INC) $(CONTEXT_INC)
	$(TL)\szscrn "Assembling util.asm"
	$(TL)\masm $(MASM) $(QB)\ir\util;
