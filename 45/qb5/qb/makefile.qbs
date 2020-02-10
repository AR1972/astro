#--------------------------------------------------------------------
# File: $(QB5)\qb\makefile:
#
# This makefile contains make information for all QBI version dependent
# BASIC Interpreter Source
#
# The following macros are expected to be set up:
#   TL		path to tools\bin directory
#   QB		path to qb root
#   QB5 	path to qb5 root
#   UIBLD	directory to build .obj files to
#   CC		cl command line switches
#   MASM	masm command line switches
#
# This file is used in conjunction with:
#   \qb5\test\makefile	root qb5 makefile (also \qb5\release\makefile)
#   \qb\ir\makefile	version independent interpreter source
#   ..\ir\makefile	QB5 specific interpreter source
#   ..\qb\maketwin	twin user interface code
#   ..\ui\makefile	cow user interface code
#--------------------------------------------------------------------

TWINLIB = twin.lib
HELPLIB = mhelph.lib
KKIFLIB = kkif.lib

!IFDEF ROMBASIC
COWLIB = cow.lib ibmtandy.lib
!ELSE
COWLIB = cow.lib cowtandy.lib
!ENDIF

#------------------------------------------------------------------
#   Macros for masm and cl command line switches specific to this
#	module, and include search paths.
#
CC_MOD = -Zp -Fo$@ $(UIFLAGS)
CC_INCL = -I. -I$(QB5)\hd -I$(QB)\hd -I$(TL)\..\inc
MASM_MOD =
MASM_INCL = -E -I. -I$(QB5)\hd -I$(QB)\hd -I$(TL)\..\inc


#------------------------------------------------------------------
# DMOBJ are Quick BASIC 4.0 "direct mode" object files
# This includes these runtime modules which are essential to
#	direct mode:	bdmgr.obj bddebug.obj
#
DMOBJ = bdmgr.obj bddebug.obj binsav.obj binxlat.obj conmisc.obj condebug.obj \
	context.obj debug.obj dshstubc.obj dshcmd.obj dshstrng.obj init.obj \
	lsid.obj lsmain.obj lsrules.obj lsutil.obj lsoptab.obj \
	nammgr.obj namdebug.obj opnamatr.obj \
	prsmain.obj prsid.obj prsctl.obj prsnt.obj prscg.obj prslex.obj \
	prsexp.obj prsutil.obj prsstate.obj prsrwt.obj prsdebug.obj \
	prsstmt.obj prsnt1.obj qbimsgs.obj qbidata.obj rsalpha.obj rterror.obj \
	txtdata.obj txtdeb.obj txtdir.obj txtload.obj \
	txtmove.obj txtmgr.obj txtsave.obj \
	txtutil.obj txtfind.obj txtthr.obj txtdebug.obj typmgr.obj \
	uiutil.obj util.obj varmgrc.obj vardebug.obj vardbg.obj varutil.obj

#------------------------------------------------------------------
# SSEXOBJ are Quick BASIC 4.0 scanner/executor object files.
#
SSEXOBJ = sscase.obj ssdata.obj ssdo.obj ssfor.obj ssif.obj \
	ssbos.obj ssid.obj ssscan.obj ssstmts.obj ssaid.obj ssmisc.obj \
	ssrude.obj ssrules.obj ssoperat.obj sslit.obj sstxutil.obj \
	ssdeclar.obj ssdescan.obj ssoptab.obj ssrec.obj ssproc.obj \
	ssrefarg.obj excase.obj excontxt.obj exconv.obj exdebug.obj \
	exevent.obj exfor.obj \
	exfnmisc.obj exgoto.obj \
	exgraph.obj exmathop.obj exid.obj exif.obj exio.obj exos.obj \
	exstmisc.obj exstring.obj exarray.obj \
	exaryutl.obj exaid.obj exprint.obj exmisc.obj exlit.obj \
	exrec.obj exrefarg.obj extort.obj exproc.obj bfpsig.obj

#------------------------------------------------------------------
# RTOBJ are temporary runtime-specific objects
#
RTOBJ = rttemp.obj


qedit.obj   : $(QB5)\ir\qedit.asm
	$(TL)\szscrn "Assembling qedit.asm"
	$(TL)\masm $(MASM) $(QB5)\ir\qedit;

help.obj   : $(QB5)\ir\help.asm
	$(TL)\szscrn "Assembling help.asm"
	$(TL)\masm $(MASM) $(QB5)\ir\help;

qbi0.obj: $(QB5)\qb\qbi0.asm
	$(TL)\szscrn "Assembling qbi0.asm:"
	$(TL)\masm $(SSFLAGS) $(MASM) $(QB5)\qb\qbi0;

dshstubc.obj: $(QB5)\qb\dshstubc.c version.h \
	$(QB)\hd\parser.h \
	$(QB)\hd\psint.h \
	$(QB)\hd\heap.h
	$(TL)\szscrn "Compiling  dshstubc.c:"
	if exist dshstubc.obj del dshstubc.obj
	$(TL)\cl $(UIFLAGS) -W3 -Zip -c -X -I. -I$(QB)\hd -I$(TL)\..\inc -AM -FPa -Gcs -NT UI $(QB5)\qb\dshstubc.c

uirstubc.obj:	$(QB5)\qb\uirstubc.c
	$(TL)\szscrn "Compiling  uirstubc.c"
	if exist uirstubc.obj del uirstubc.obj
	$(TL)\cl $(UIFLAGS) -W3 -Zip -c -I$(QB)\hd -I$(TL)\..\inc -AM -FPa -Gs -NTUI -Fo$@ $(QB5)\qb\uirstubc.c

uimisc.obj:	$(QB5)\qb\uimisc.asm $(VERSION_INC) \
	$(QB5)\hd\twin\window.inc \
	$(QB)\hd\architec.inc \
	qbimsgs.inc \
	$(QB5)\hd\uiint.inc
	$(TL)\szscrn "Assembling uimisc.asm"
	$(TL)\masm $(MASM) $(QB5)\qb\uimisc,$(@R);


#==============================================================
# exe file definitions
#==============================================================

# HELP files used by qbasic.exe (COW version)
#
HELP:	qbasic.hlp edit.hlp help.hlp
	echo "Help files up to date."

qbimsgs.rpl: $(QB5)\ir\makehelp.sed qbimsgs.h
	$(TL)\sed -n -f $(QB5)\ir\makehelp.sed qbimsgs.h > qbimsgs.rpl

uihelpid.rpl: $(QB5)\ir\makehelp.sed $(QB5)\hdcw\uihelpid.h
	$(TL)\sed -n -f $(QB5)\ir\makehelp.sed $(QB5)\hdcw\uihelpid.h > uihelpid.rpl

qbasic.hlp: uihelpid.rpl qbimsgs.rpl $(QB5)\ir\qbasic.qh $(TL)\..\binb\hlpmake2.exe
	$(TL)\szscrn "making qbasic.hlp"
	if exist qbasic.hlp del qbasic.hlp
	$(TL)\..\binb\replace qbimsgs.rpl uihelpid.rpl < $(QB5)\ir\qbasic.qh > qbn.qh
	-$(TL)\..\binb\hlpmake2 /e /A: /oqbasic.hlp /S2 /W78 qbn.qh
	REM del qbn.qh

edit.hlp: qbimsgs.rpl uihelpid.rpl $(QB5)\ir\edit.qh $(TL)\..\binb\hlpmake2.exe
	$(TL)\szscrn "making edit.hlp"
	if exist edit.hlp del edit.hlp
	$(TL)\..\binb\replace qbimsgs.rpl uihelpid.rpl < $(QB5)\ir\edit.qh > qbn.qh
	-$(TL)\..\binb\hlpmake2 /e /A: /oedit.hlp /S2 /W78 qbn.qh
	REM del qbn.qh

help.hlp: qbimsgs.rpl uihelpid.rpl $(QB5)\ir\help.qh $(TL)\..\binb\hlpmake2.exe
	$(TL)\szscrn "making help.hlp (takes 10min 35sec on 386/33, with 628kb free,"
	$(TL)\szscrn "and a 3Mb SmartDrv 4.0 write-behind disk cache!)"
	if exist help.hlp del help.hlp
	$(TL)\..\binb\replace qbimsgs.rpl uihelpid.rpl < $(QB5)\ir\help.qh > qbn.qh
	-$(TL)\..\binb\hlpmake2 /e /A: /ohelp.hlp /S2 /W78 qbn.qh
	REM del qbn.qh


# qb is the "latest/current" version of QBASIC
#
# Link order constraints:
# - math: math errors in executors are handled differently than math errors
#         in the runtime (diff in how the oTx of the error is found).
#         __bfpsignal must be defined between executors and $(RTLIB)+$(MATHLIB)
#         __bfpsignal is in bfpsig.obj in ex.lnk.
#
qb.exe: $(SSEXOBJ) $(RTOBJ) $(DMOBJ) $(UIOBJ) $(COWLIB) $(RTLIB) $(HELPLIB) \
	$(UIBLD)\shell.lnk uirstubc.obj qbi0.obj
	$(TL)\szscrn "linking qb"
	if exist qb.exe del qb.exe
	if exist qbasic.exe del qbasic.exe
	copy $(QB5)\qb\opt.lnk+$(QB5)\qb\misc.lnk+$(QB5)\qb\rtint.lnk+$(QB5)\qb\ls.lnk+$(QB5)\qb\prs.lnk dm1.lnk
	copy dm1.lnk+$(QB5)\qb\var.lnk+$(QB5)\qb\ctx.lnk+$(QB5)\qb\txt.lnk dm.lnk
	copy $(UIBLD)\shell.lnk+$(QB5)\qb\rt.lnk+dm.lnk+$(QB5)\qb\ss.lnk+$(QB5)\qb\ex.lnk j.lnk
	$(TL)\..\binb\lk $(LINK_USR) qbi0+@j.lnk+$(RTLIB),qb,$(MAPNAME),$(MATHLIB)+$(COWLIB)+$(CLIB)+$(HELPLIB)/NOD;
	$(TL)\mapsym qb
!IFDEF ROMBASIC
 	$(TL)\szscrn "creating IBM version of QB.EXE"
 	.\rompatch
!ENDIF
	$(TL)\szscrn "creating qbasic.exe"
	copy qb.exe qbasic.exe
	$(TL)\szscrn "Done."

edit.com : qedit.obj
	$(TL)\szscrn "linking qedit.com"
	$(TL)\..\binb\link510 qedit, edit.com /tiny;

help.com : help.obj
	$(TL)\szscrn "linking help.com"
	$(TL)\..\binb\link510 help, help.com /tiny;
