#========================================================================
# Name: QBASIC Runtime Makefile
# Usage:
#      make -f <makefile> [TL=<tools dir>] ["IDB=-DDBG"] [bqb50.lib bqlb45.lib]
#		<makefile>	- name of the make file produced by -o from
#				  buildkit.
#		IDB=-DDBG	- specifies to build an internal debug version.
#		bqb50.lib	- DOS 3 QBASIC interpreter library
#		TL=directory	- specifies where to get the tools from
# Purpose:
#	The make file will rebuild any "out of date" objects and build
#	bqb50.lib. It utilizes the dos development utilities to accomplish this
#	task.  masm is the assembler, and lib is the librarian.  This makefile
#	is meant to be used with a Large model Xenix style Make.
#	When this build has completed, the message "Basic built"
#	will be displayed.
# Notes:
#	If a new module is added/removed, or an include file is
#	added/removed, or an include file is added/removed from a
#	module, then buildkit must be rerun on QB5.BLD.
#	QB5.BLD must also be updated in the LITERAL sections
#	to reflect the appropriate changes. There exists a batch file
#	(bldkit.bat) which will invoke buildkit with all the approprite
#	switches to generate this makefile.
#
#========================================================================

#************************************************************************
#	OBJECT FILE dictionary
#	----------------------
#	.obj	= Common object module with no DOS specific support
#	.ob3	= Dos 3 specific module
#	.or3	= Dos 3 specific module with special user library support.
#
#	NOTE: If a .obj file generates an assemble time error then this
#	can happen as the result of a EI_QB, FO_RTM, or OM_DOS5 switch
#	that has been added to the file.  At this point the file must
#	be multiply assembled.
#*************************************************************************

#*************************************************************************
# If $(IDB) is undefined (release version), use it to set WARNING LEVEL 2
#*************************************************************************

!if "$(IDB)" == ""
IDB = -W2
!endif

#*************************************************************************
#	Aliases for components of the runtime.
#*************************************************************************

#=================== MATH component ======================================

RTMATH =	fin.obj     hexoct.obj	 ifout.obj     \
		mtfloat.obj  helpi4.obj  fout.obj      \
		random.obj   ..\tl\lib3\fpreset.obj    \
		helpfp87.obj


#=================== GRAPHICS component ==================================

GRAPHICS =	grputil.obj  vwclr.obj	 grpmap.obj    \
		grcoord.obj  llagrp.obj  llcgasup.obj  \
		llegasup.obj llxgasup.obj llhgcsup.obj \
		mclprc.obj   gwpal.obj

GRAPH3 =	circle.ob3   draw.ob3	 getput.ob3    \
		grview.ob3   grline.ob3  grpoint.ob3   \
		paint.ob3    grinit.ob3  llcgrp.ob3    \
		lltext.ob3   llcga.ob3	 llega.ob3     \
		llvga.ob3    grspace.ob3 llhgc.ob3     \
		llgrstub.ob3 lloga.ob3	 grfpinit.ob3  \
		grwindow.ob3

#=================== STRING component ====================================

STRING =	mid.obj      strfcn.obj  strnum.obj    \
		string.obj   stinkey.obj stcore.obj    \
		stringfp.obj

STRING3 =	stfree.ob3

#=================== NEAR HEAP component =================================

NEARHEAP3 =	nhlhutil.ob3 nhstutil.ob3 dynamic.ob3 fhutil.obj \
		nhlhcore.ob3

#=================== OPERATING SYSTEM component ==========================

OS3 =		oscmd.ob3    osdate.ob3   osstmt.ob3  \
		ostimer.ob3

#=================== ERROR component =====================================

ERROR = 	erproc.ob3   messages.ob3 erhandlr.obj

#=================== IO components =======================================

IO =		inputf.obj   lininp.obj   tabspc.obj   \
		prtu.obj     pufout.obj   prnval.obj   \
		pr0a.obj     gwlin.obj	  out.obj      \
		dkio.ob3     gwcom.ob3	  prnvalfp.obj \

CONSOLE =	inptty.obj   gwscr.obj	  gwkey.obj    \
		gwscreen.obj keydsp.obj

DISK =		inpdsk.obj   field.obj	  rlset.obj

OTHERIO =	iolpt.obj    width.obj

GWIO =		$(IO) $(CONSOLE) $(DISK) $(OTHERIO)

GWIO3 = 	gwio.ob3     iocons.ob3   llascn.ob3   \
		llcscn.ob3   dvstmt.ob3   llcom3.ob3   \
		dkstmt.ob3   dkopen.ob3   lllpt.ob3    \
		directry.ob3 filename.ob3 dvpipe.ob3   \
		bload.ob3    iotty.ob3	  llevtstg.ob3 \
		llgrtabl.ob3 llscreen.ob3 dkutil.ob3   \
		llscnfcn.ob3

#================== EVENT component =====================================

EVENT = 	gwque.obj

EVENT3 =	gwaevt.ob3   gwcevt.ob3   llaevt.ob3   \
		llcevt.ob3   evtkey.ob3   evtpla.ob3   \
		evtuev.ob3   evtstg.ob3   evtcom.ob3   \
		evttim.ob3   evtpen.ob3

#================== SOUND component =====================================

SOUND3 =	gwplayf.ob3  gwplays.ob3  gwsound.ob3  \
		llsnd.ob3    sninit.ob3

#================== INTERNAL DEBUG component ============================

ID3 =		id_util.ob3  fhdebug.ob3 nhdebug.ob3

#================== RUNTIME CORE component ==============================

MISC =		swap.obj     read.obj	  abs.ob3

MISC3 = 	llinit.ob3   llscnio.ob3  llque.ob3    \
		peek.ob3     llparam.ob3  llkeys.ob3   \
		gwini.ob3    gwdata.ob3   clear.ob3

#================== User library Modules ================================

ULIB =		rtm86.ob3    rtmload.ob3  rttoul.obj

#================== Initialization Modules ==============================

INITS = 	dvinit.obj   dbtrace.obj  dkinit.obj

INIT3 = 	rtllinit.ob3 cninit.ob3   rtinit.ob3   \
		rtterm.ob3   osinit.ob3   fhinit.ob3   \
		nhinit.ob3   fhchn.ob3

#************************************************************************
# Library and Runtime module definitions
#************************************************************************

LIBS =		$(INITS) $(MISC) $(ERROR) $(EVENT)     \
		$(STRING) $(GRAPHICS) $(GWIO) $(RTMATH)

LIB3 =		$(INIT3) $(OS3) $(SOUND3) $(EVENT3)    \
		$(GWIO3) $(MISC3) $(ID3) $(GRAPH3)     \
		$(NEARHEAP3) $(STRING3) $(ULIB)

CLIB3  =	crt0.obj     crt0dat.obj  crt0fp.obj   \
		fmsghdr.obj  chkstk.obj   chksum.obj   \
		ovlmul.obj  \
		..\tl\lib3\afldiv.obj	..\tl\lib3\aflrem.obj  \
		..\tl\lib3\fpreset.obj

#*************************************************************************
# Generate the DOS 3 Quick BASIC interpreter libraries.
#*************************************************************************
QB5:	bqb50.lib
	@echo "QuickBASIC 5.0 runtime libraries built"

#*************************************************************************
# Generate the DOS 3 Quick BASIC 5.0 interpreter library.
#*************************************************************************
bqb50.lib: $(LIB3) $(LIBS) $(CLIB3)
	if exist bqb50.lib del bqb50.lib
	$(TL)\lib @bqb50.dat
	@echo "bqb50.lib built"

#*************************************************************************
# And now the object file dependencies!
#*************************************************************************
INCLUDE=-I..\inc -I..\crt 

dbtrace.obj:	..\rt\dbtrace.asm ..\inc\const.inc ..\inc\addr.inc \
	..\inc\event.inc ..\inc\dc.inc ..\inc\messages.inc \
	..\inc\compvect.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\dbtrace.asm, dbtrace.obj;

dkinit.obj:	..\rt\dkinit.asm ..\inc\compvect.inc ..\inc\rtps.inc \
	..\inc\nhutil.inc ..\inc\files.inc ..\inc\fdb.inc \
	..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc ..\inc\devdef.inc \
	..\inc\baslibma.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\dkinit.asm, dkinit.obj;

dvinit.obj:	..\rt\dvinit.asm ..\inc\compvect.inc ..\inc\rtps.inc \
	..\inc\const.inc ..\inc\nhutil.inc ..\inc\files.inc ..\inc\fdb.inc \
	..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc ..\inc\devdef.inc \
	..\inc\baslibma.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\dvinit.asm, dvinit.obj;

erhandlr.obj:	..\rt\erhandlr.asm ..\inc\stack.inc \
	..\inc\messages.inc ..\inc\idmac.inc ..\inc\baslibma.inc \
	..\inc\addr.inc ..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\erhandlr.asm, erhandlr.obj;

error.obj:	..\rt\error.asm ..\inc\stack.inc ..\inc\messages.inc \
	..\inc\idmac.inc ..\inc\addr.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\error.asm, error.obj;

fhutil.obj:	..\rt\fhutil.asm ..\inc\idmac.inc ..\inc\array.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\fhutil.asm, fhutil.obj;

field.obj:	..\rt\field.asm ..\inc\string.inc ..\inc\lmem.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\idmac.inc ..\inc\rtps.inc \
	..\inc\files.inc ..\inc\fdb.inc ..\inc\pointers.inc ..\inc\sb.inc \
	..\inc\devdef.inc ..\inc\baslibma.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\field.asm, field.obj;

fin.obj:	..\rt\fin.asm ..\inc\string.inc ..\inc\lmem.inc ..\inc\sb.inc \
	..\inc\omega.inc ..\inc\idmac.inc ..\inc\const.inc ..\inc\rtps.inc \
	..\inc\baslibma.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\switch.inc ..\inc\rmacros.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\fin.asm, fin.obj;

fout.obj:	..\rt\fout.asm ..\inc\idmac.inc ..\inc\rtps.inc \
	..\inc\baslibma.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\fout.asm, fout.obj;

grcoord.obj:	..\rt\grcoord.asm ..\inc\baslibma.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\grcoord.asm, grcoord.obj;

grpmap.obj:	..\rt\grpmap.asm ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\grpmap.asm, grpmap.obj;

grputil.obj:	..\rt\grputil.asm ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\grputil.asm, grputil.obj;

gwkey.obj:	..\rt\gwkey.asm ..\inc\string.inc ..\inc\lmem.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\event.inc ..\inc\baslibma.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\gwkey.asm, gwkey.obj;

gwlin.obj:	..\rt\gwlin.asm ..\inc\const.inc ..\inc\messages.inc \
	..\inc\dc.inc ..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\gwlin.asm, gwlin.obj;

gwpal.obj:	..\rt\gwpal.asm ..\inc\array.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\switch.inc ..\inc\rmacros.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\gwpal.asm, gwpal.obj;

gwque.obj:	..\rt\gwque.asm ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\gwque.asm, gwque.obj;

gwscr.obj:	..\rt\gwscr.asm ..\inc\idmac.inc ..\inc\ascii.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\gwscr.asm, gwscr.obj;

gwscreen.obj:	..\rt\gwscreen.asm ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\gwscreen.asm, gwscreen.obj;

helpfp87.obj:	..\rt\helpfp87.asm ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\helpfp87.asm, helpfp87.obj;

helpi4.obj:	..\rt\helpi4.asm ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\helpi4.asm, helpi4.obj;

hexoct.obj:	..\rt\hexoct.asm ..\inc\string.inc ..\inc\lmem.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\hexoct.asm, hexoct.obj;

ifout.obj:	..\rt\ifout.asm ..\inc\rtps.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\ifout.asm, ifout.obj;

inpdsk.obj:	..\rt\inpdsk.asm ..\inc\string.inc ..\inc\lmem.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\idmac.inc ..\inc\rtps.inc \
	..\inc\ascii.inc ..\inc\files.inc ..\inc\fdb.inc ..\inc\pointers.inc \
	..\inc\sb.inc ..\inc\devdef.inc ..\inc\baslibma.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\inpdsk.asm, inpdsk.obj;

inptty.obj:	..\rt\inptty.asm ..\inc\idmac.inc ..\inc\string.inc \
	..\inc\lmem.inc ..\inc\sb.inc ..\inc\omega.inc ..\inc\rtps.inc \
	..\inc\const.inc ..\inc\messages.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\inptty.asm, inptty.obj;

inputf.obj:	..\rt\inputf.asm ..\inc\string.inc ..\inc\lmem.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\idmac.inc ..\inc\rtps.inc \
	..\inc\files.inc ..\inc\fdb.inc ..\inc\pointers.inc ..\inc\sb.inc \
	..\inc\devdef.inc ..\inc\baslibma.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\inputf.asm, inputf.obj;

iolpt.obj:	..\rt\iolpt.asm ..\inc\const.inc ..\inc\rtps.inc \
	..\inc\ascii.inc ..\inc\files.inc ..\inc\fdb.inc ..\inc\pointers.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\devdef.inc ..\inc\baslibma.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\iolpt.asm, iolpt.obj;

keydsp.obj:	..\rt\keydsp.asm ..\inc\rtps.inc ..\inc\const.inc \
	..\inc\idmac.inc ..\inc\ascii.inc ..\inc\nhutil.inc ..\inc\files.inc \
	..\inc\fdb.inc ..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc \
	..\inc\baslibma.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\keydsp.asm, keydsp.obj;

lininp.obj:	..\rt\lininp.asm ..\inc\idmac.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\string.inc ..\inc\lmem.inc ..\inc\sb.inc \
	..\inc\omega.inc ..\inc\rtps.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\lininp.asm, lininp.obj;

llagrp.obj:	..\rt\llagrp.asm ..\inc\ibmunv.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llagrp.asm, llagrp.obj;

llcgasup.obj:	..\rt\llcgasup.asm ..\inc\idmac.inc ..\inc\llgrp.inc \
	..\inc\ibmunv.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llcgasup.asm, llcgasup.obj;

llhgcsup.obj:	..\rt\llhgcsup.asm ..\inc\idmac.inc ..\inc\llgrp.inc \
	..\inc\ibmunv.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llhgcsup.asm, llhgcsup.obj;

llegasup.obj:	..\rt\llegasup.asm ..\inc\llgrp.inc \
	..\inc\baslibma.inc ..\inc\ibmunv.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llegasup.asm, llegasup.obj;

llxgasup.obj:	..\rt\llxgasup.asm ..\inc\llgrp.inc ..\inc\ibmunv.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llxgasup.asm, llxgasup.obj;

mclprc.obj:	..\rt\mclprc.asm ..\inc\idmac.inc ..\inc\string.inc \
	..\inc\lmem.inc ..\inc\sb.inc ..\inc\omega.inc ..\inc\rtps.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\mclprc.asm, mclprc.obj;

mid.obj:	..\rt\mid.asm ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\mid.asm, mid.obj;

mtfloat.obj:	..\rt\mtfloat.asm ..\inc\rtps.inc ..\inc\idmac.inc \
	..\inc\compvect.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\mtfloat.asm, mtfloat.obj;

out.obj:	..\rt\out.asm ..\inc\const.inc ..\inc\idmac.inc \
	..\inc\ascii.inc ..\inc\files.inc ..\inc\fdb.inc ..\inc\pointers.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\devdef.inc ..\inc\baslibma.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\out.asm, out.obj;

ovlmul.obj:	..\rt\ovlmul.asm ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\ovlmul.asm, ovlmul.obj;

pr0a.obj:	..\rt\pr0a.asm ..\inc\rtps.inc ..\inc\files.inc \
	..\inc\fdb.inc ..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc \
	..\inc\devdef.inc ..\inc\baslibma.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\pr0a.asm, pr0a.obj;

prnval.obj:	..\rt\prnval.asm ..\inc\string.inc ..\inc\lmem.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\idmac.inc ..\inc\rtps.inc \
	..\inc\files.inc ..\inc\fdb.inc ..\inc\pointers.inc ..\inc\sb.inc \
	..\inc\devdef.inc ..\inc\baslibma.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\prnval.asm, prnval.obj;

prnvalfp.obj:	..\rt\prnvalfp.asm ..\inc\rtps.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\prnvalfp.asm, prnvalfp.obj;

prtu.obj:	..\rt\prtu.asm ..\inc\string.inc ..\inc\lmem.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rtps.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\prtu.asm, prtu.obj;

pufout.obj:	..\rt\pufout.asm ..\inc\rtps.inc ..\inc\idmac.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\pufout.asm, pufout.obj;

random.obj:	..\rt\random.asm ..\inc\messages.inc \
	..\inc\compvect.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\random.asm, random.obj;

read.obj:	..\rt\read.asm ..\inc\pointers.inc ..\inc\string.inc \
	..\inc\lmem.inc ..\inc\sb.inc ..\inc\omega.inc ..\inc\rtps.inc \
	..\inc\idmac.inc ..\inc\baslibma.inc ..\inc\addr.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\read.asm, read.obj;

rlset.obj:	..\rt\rlset.asm ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\rlset.asm, rlset.obj;

rttoul.obj:	..\rt\rttoul.asm ..\inc\ulib.inc ..\inc\ulib.inc \
	..\inc\idmac.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\rttoul.asm, rttoul.obj;

stcore.obj:	..\rt\stcore.asm ..\inc\idmac.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\stcore.asm, stcore.obj;

stinkey.obj:	..\rt\stinkey.asm ..\inc\const.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\stinkey.asm, stinkey.obj;

strfcn.obj:	..\rt\strfcn.asm ..\inc\const.inc ..\inc\devdef.inc \
	..\inc\files.inc ..\inc\fdb.inc ..\inc\pointers.inc ..\inc\sb.inc \
	..\inc\omega.inc ..\inc\baslibma.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\strfcn.asm, strfcn.obj;

string.obj:	..\rt\string.asm ..\inc\idmac.inc ..\inc\baslibma.inc \
	..\inc\rtps.inc ..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\string.asm, string.obj;

stringfp.obj:	..\rt\stringfp.asm ..\inc\baslibma.inc \
	..\inc\rtps.inc ..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\stringfp.asm, stringfp.obj;

strnum.obj:	..\rt\strnum.asm ..\inc\string.inc ..\inc\lmem.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\baslibma.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\strnum.asm, strnum.obj;

swap.obj:	..\rt\swap.asm ..\inc\pointers.inc ..\inc\baslibma.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\swap.asm, swap.obj;

tabspc.obj:	..\rt\tabspc.asm ..\inc\idmac.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\tabspc.asm, tabspc.obj;

vwclr.obj:	..\rt\vwclr.asm ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\const.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\vwclr.asm, vwclr.obj;

width.obj:	..\rt\width.asm ..\inc\string.inc ..\inc\lmem.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\files.inc ..\inc\fdb.inc \
	..\inc\pointers.inc ..\inc\sb.inc ..\inc\devdef.inc \
	..\inc\baslibma.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\width.asm, width.obj;

abs.ob3:	..\rt\abs.asm ..\inc\error.inc ..\inc\idmac.inc \
	..\inc\stack.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\abs.asm, abs.ob3;

bload.ob3:	..\rt\bload.asm ..\inc\rtps.inc ..\inc\baslibma.inc \
	..\inc\files.inc ..\inc\fdb.inc ..\inc\pointers.inc ..\inc\sb.inc \
	..\inc\omega.inc ..\inc\devdef.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\switch.inc ..\inc\rmacros.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\bload.asm, bload.ob3;

circle.ob3:	..\rt\circle.asm ..\inc\idmac.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\const.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\circle.asm, circle.ob3;

clear.ob3:	..\rt\clear.asm ..\inc\const.inc ..\inc\idmac.inc \
	..\inc\stack2.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\clear.asm, clear.ob3;

cninit.ob3:	..\rt\cninit.asm ..\inc\event.inc ..\inc\error2.inc \
	..\inc\const.inc ..\inc\idmac.inc ..\inc\dc.inc ..\inc\files.inc \
	..\inc\fdb.inc ..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc \
	..\inc\messages.inc ..\inc\compvect.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\cninit.asm, cninit.ob3;

directry.ob3:	..\rt\directry.asm ..\inc\string.inc ..\inc\lmem.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\baslibma.inc ..\inc\files.inc \
	..\inc\fdb.inc ..\inc\pointers.inc ..\inc\sb.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\directry.asm, directry.ob3;

dkio.ob3:	..\rt\dkio.asm ..\inc\rtps.inc ..\inc\idmac.inc \
	..\inc\ascii.inc ..\inc\files.inc ..\inc\fdb.inc ..\inc\pointers.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\devdef.inc ..\inc\baslibma.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\dkio.asm, dkio.ob3;

draw.ob3:	..\rt\draw.asm ..\inc\string.inc ..\inc\lmem.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\rtps.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\draw.asm, draw.ob3;

dvpipe.ob3:	..\rt\dvpipe.asm ..\inc\idmac.inc ..\inc\rtps.inc \
	..\inc\oscalls.inc ..\inc\files.inc ..\inc\fdb.inc \
	..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc ..\inc\devdef.inc \
	..\inc\baslibma.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\dvpipe.asm, dvpipe.ob3;

dynamic.ob3:	..\rt\dynamic.asm ..\inc\sb.inc ..\inc\omega.inc \
	..\inc\string.inc ..\inc\lmem.inc ..\inc\sb.inc ..\inc\stack.inc \
	..\inc\idmac.inc ..\inc\nhutil.inc ..\inc\baslibma.inc \
	..\inc\pointers.inc ..\inc\array.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\dynamic.asm, dynamic.ob3;

erproc.ob3:	..\rt\erproc.asm ..\inc\messages.inc ..\inc\files.inc \
	..\inc\fdb.inc ..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc \
	..\inc\const.inc ..\inc\rtps.inc ..\inc\idmac.inc \
	..\inc\compvect.inc ..\inc\baslibma.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\erproc.asm, erproc.ob3;

evtcom.ob3:	..\rt\evtcom.asm ..\inc\event.inc ..\inc\idmac.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\comdcb.inc \
	..\inc\baslibma.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\evtcom.asm, evtcom.ob3;

evtkey.ob3:	..\rt\evtkey.asm ..\inc\idmac.inc ..\inc\baslibma.inc \
	..\inc\const.inc ..\inc\event.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\evtkey.asm, evtkey.ob3;

evtpen.ob3:	..\rt\evtpen.asm ..\inc\baslibma.inc ..\inc\event.inc \
	..\inc\idmac.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\evtpen.asm, evtpen.ob3;

evtpla.ob3:	..\rt\evtpla.asm ..\inc\idmac.inc ..\inc\baslibma.inc \
	..\inc\event.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\evtpla.asm, evtpla.ob3;

evtstg.ob3:	..\rt\evtstg.asm ..\inc\idmac.inc ..\inc\baslibma.inc \
	..\inc\event.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\evtstg.asm, evtstg.ob3;

evttim.ob3:	..\rt\evttim.asm ..\inc\idmac.inc ..\inc\baslibma.inc \
	..\inc\event.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\evttim.asm, evttim.ob3;

evtuev.ob3:	..\rt\evtuev.asm ..\inc\idmac.inc ..\inc\baslibma.inc \
	..\inc\event.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\evtuev.asm, evtuev.ob3;

fhchn.ob3:	..\rt\fhchn.asm ..\inc\stack2.inc ..\inc\string.inc \
	..\inc\lmem.inc ..\inc\sb.inc ..\inc\omega.inc ..\inc\baslibma.inc \
	..\inc\addr.inc ..\inc\oscalls.inc ..\inc\smchain.inc \
	..\inc\array.inc ..\inc\messages.inc ..\inc\compvect.inc \
	..\inc\idmac.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\fhchn.asm, fhchn.ob3;

fhdebug.ob3:	..\rt\fhdebug.asm ..\inc\array.inc ..\inc\idmac.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\fhdebug.asm, fhdebug.ob3;

fhinit.ob3:	..\rt\fhinit.asm ..\inc\stack2.inc ..\inc\string.inc \
	..\inc\lmem.inc ..\inc\sb.inc ..\inc\omega.inc ..\inc\baslibma.inc \
	..\inc\addr.inc ..\inc\oscalls.inc ..\inc\smchain.inc \
	..\inc\array.inc ..\inc\compvect.inc ..\inc\messages.inc \
	..\inc\idmac.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\fhinit.asm, fhinit.ob3;

filename.ob3:	..\rt\filename.asm ..\inc\rtps.inc ..\inc\idmac.inc \
	..\inc\files.inc ..\inc\fdb.inc ..\inc\pointers.inc ..\inc\sb.inc \
	..\inc\omega.inc ..\inc\devdef.inc ..\inc\baslibma.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\filename.asm, filename.ob3;

getput.ob3:	..\rt\getput.asm ..\inc\array.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\const.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\getput.asm, getput.ob3;

grinit.ob3:	..\rt\grinit.asm ..\inc\compvect.inc ..\inc\idmac.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\grinit.asm, grinit.ob3;

grfpinit.ob3:	..\rt\grfpinit.asm ..\inc\baslibma.inc \
	..\inc\idmac.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\grfpinit.asm, grfpinit.ob3;

grline.ob3:	..\rt\grline.asm ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\const.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\grline.asm, grline.ob3;

grpoint.ob3:	..\rt\grpoint.asm ..\inc\idmac.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\grpoint.asm, grpoint.ob3;

grspace.ob3:	..\rt\grspace.asm ..\inc\array.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\grspace.asm, grspace.ob3;

grview.ob3:	..\rt\grview.asm ..\inc\const.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\grview.asm, grview.ob3;

grwindow.ob3:	..\rt\grwindow.asm ..\inc\idmac.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\grwindow.asm, grwindow.ob3;

gwaevt.ob3:	..\rt\gwaevt.asm ..\inc\idmac.inc ..\inc\compvect.inc \
	..\inc\stack.inc ..\inc\queues.inc ..\inc\event.inc \
	..\inc\intmac.inc ..\inc\baslibma.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\gwaevt.asm, gwaevt.ob3;

gwcevt.ob3:	..\rt\gwcevt.asm ..\inc\queues.inc ..\inc\const.inc \
	..\inc\devdef.inc ..\inc\event.inc ..\inc\baslibma.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\gwcevt.asm, gwcevt.ob3;

gwcom.ob3:	..\rt\gwcom.asm ..\inc\idmac.inc ..\inc\rtps.inc \
	..\inc\comdcb.inc ..\inc\addr.inc ..\inc\ascii.inc ..\inc\devdef.inc \
	..\inc\files.inc ..\inc\fdb.inc ..\inc\pointers.inc ..\inc\sb.inc \
	..\inc\omega.inc ..\inc\baslibma.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\gwcom.asm, gwcom.ob3;

gwdata.ob3:	..\rt\gwdata.asm ..\inc\string.inc ..\inc\lmem.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\comdcb.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\gwdata.asm, gwdata.ob3;

gwini.ob3:	..\rt\gwini.asm ..\inc\rtps.inc ..\inc\const.inc \
	..\inc\idmac.inc ..\inc\nhutil.inc ..\inc\files.inc ..\inc\fdb.inc \
	..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc \
	..\inc\baslibma.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\gwini.asm, gwini.ob3;

gwio.ob3:	..\rt\gwio.asm ..\inc\idmac.inc ..\inc\stack.inc \
	..\inc\messages.inc ..\inc\dc.inc ..\inc\event.inc ..\inc\addr.inc \
	..\inc\devdef.inc ..\inc\intmac.inc ..\inc\files.inc ..\inc\fdb.inc \
	..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc \
	..\inc\baslibma.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\gwio.asm, gwio.ob3;

gwplayf.ob3:	..\rt\gwplayf.asm ..\inc\ibmunv.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\gwplayf.asm, gwplayf.ob3;

gwplays.ob3:	..\rt\gwplays.asm ..\inc\string.inc ..\inc\lmem.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\idmac.inc ..\inc\rtps.inc \
	..\inc\ibmunv.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\gwplays.asm, gwplays.ob3;

gwsound.ob3:	..\rt\gwsound.asm ..\inc\ibmunv.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\gwsound.asm, gwsound.ob3;

id_util.ob3:	..\rt\id_util.asm ..\inc\rtps.inc ..\inc\const.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\id_util.asm, id_util.ob3;

iocons.ob3:	..\rt\iocons.asm ..\inc\rtps.inc ..\inc\devdef.inc \
	..\inc\baslibma.inc ..\inc\idmac.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\iocons.asm, iocons.ob3;

dvstmt.ob3:	..\rt\dvstmt.asm ..\inc\pointers.inc ..\inc\rtps.inc \
	..\inc\const.inc ..\inc\idmac.inc ..\inc\ascii.inc ..\inc\files.inc \
	..\inc\fdb.inc ..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc \
	..\inc\devdef.inc ..\inc\baslibma.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\dvstmt.asm, dvstmt.ob3;

dkstmt.ob3:	..\rt\dkstmt.asm ..\inc\string.inc ..\inc\lmem.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\rtps.inc ..\inc\messages.inc \
	..\inc\idmac.inc ..\inc\files.inc ..\inc\fdb.inc ..\inc\pointers.inc \
	..\inc\sb.inc ..\inc\devdef.inc ..\inc\baslibma.inc ..\inc\ascii.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\dkstmt.asm, dkstmt.ob3;

dkopen.ob3:	..\rt\dkopen.asm ..\inc\idmac.inc ..\inc\string.inc \
	..\inc\lmem.inc ..\inc\sb.inc ..\inc\omega.inc ..\inc\nhutil.inc \
	..\inc\rtps.inc ..\inc\devdef.inc ..\inc\files.inc ..\inc\fdb.inc \
	..\inc\pointers.inc ..\inc\sb.inc ..\inc\baslibma.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\dkopen.asm, dkopen.ob3;

dkutil.ob3:	..\rt\dkutil.asm ..\inc\rtps.inc ..\inc\const.inc \
	..\inc\idmac.inc ..\inc\ascii.inc ..\inc\files.inc ..\inc\fdb.inc \
	..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc ..\inc\devdef.inc \
	..\inc\baslibma.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\dkutil.asm, dkutil.ob3;

iotty.ob3:	..\rt\iotty.asm ..\inc\rtps.inc ..\inc\const.inc \
	..\inc\ascii.inc ..\inc\files.inc ..\inc\fdb.inc ..\inc\pointers.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\devdef.inc ..\inc\baslibma.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\iotty.asm, iotty.ob3;

llaevt.ob3:	..\rt\llaevt.asm ..\inc\event.inc ..\inc\intmac.inc \
	..\inc\idmac.inc ..\inc\ibmunv.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llaevt.asm, llaevt.ob3;

llascn.ob3:	..\rt\llascn.asm ..\inc\ibmunv.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llascn.asm, llascn.ob3;

llcevt.ob3:	..\rt\llcevt.asm ..\inc\const.inc ..\inc\event.inc \
	..\inc\baslibma.inc ..\inc\oscalls.inc ..\inc\intmac.inc \
	..\inc\idmac.inc ..\inc\ibmunv.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llcevt.asm, llcevt.ob3;

llcscn.ob3:	..\rt\llcscn.asm ..\inc\ibmunv.inc ..\inc\llgrp.inc \
	..\inc\intmac.inc ..\inc\idmac.inc ..\inc\oscalls.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llcscn.asm, llcscn.ob3;

llcgrp.ob3:	..\rt\llcgrp.asm ..\inc\ibmunv.inc ..\inc\grmac.inc \
	..\inc\idmac.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llcgrp.asm, llcgrp.ob3;

lltext.ob3:	..\rt\lltext.asm ..\inc\oscalls.inc ..\inc\grmac.inc \
	..\inc\llgrp.inc ..\inc\ibmunv.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\lltext.asm, lltext.ob3;

llcga.ob3:	..\rt\llcga.asm ..\inc\grmac.inc ..\inc\idmac.inc \
	..\inc\llgrp.inc ..\inc\ibmunv.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llcga.asm, llcga.ob3;

llhgc.ob3:	..\rt\llhgc.asm ..\inc\grmac.inc ..\inc\idmac.inc \
	..\inc\llgrp.inc ..\inc\ibmunv.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llhgc.asm, llhgc.ob3;

lloga.ob3:	..\rt\lloga.asm ..\inc\grmac.inc ..\inc\idmac.inc \
	..\inc\llgrp.inc ..\inc\ibmunv.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\lloga.asm, lloga.ob3;

llega.ob3:	..\rt\llega.asm ..\inc\grmac.inc ..\inc\idmac.inc \
	..\inc\llgrp.inc ..\inc\baslibma.inc ..\inc\ibmunv.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llega.asm, llega.ob3;

llvga.ob3:	..\rt\llvga.asm ..\inc\grmac.inc ..\inc\idmac.inc \
	..\inc\llgrp.inc ..\inc\baslibma.inc ..\inc\ibmunv.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llvga.asm, llvga.ob3;

llevtstg.ob3:	..\rt\llevtstg.asm ..\inc\event.inc ..\inc\intmac.inc \
	..\inc\idmac.inc ..\inc\ibmunv.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llevtstg.asm, llevtstg.ob3;

llgrstub.ob3:	..\rt\llgrstub.asm ..\inc\idmac.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llgrstub.asm, llgrstub.ob3;

llgrtabl.ob3:	..\rt\llgrtabl.asm ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llgrtabl.asm, llgrtabl.ob3;

llscreen.ob3:	..\rt\llscreen.asm ..\inc\ibmunv.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llscreen.asm, llscreen.ob3;

llcom3.ob3:	..\rt\llcom3.asm ..\inc\idmac.inc ..\inc\event.inc \
	..\inc\comdcb.inc ..\inc\baslibma.inc ..\inc\ibmunv.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llcom3.asm, llcom3.ob3;

llinit.ob3:	..\rt\llinit.asm ..\inc\egaint10.inc \
	..\inc\baslibma.inc ..\inc\llgrp.inc ..\inc\const.inc \
	..\inc\intmac.inc ..\inc\ibmunv.inc ..\inc\oscalls.inc \
	..\inc\idmac.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llinit.asm, llinit.ob3;

llscnio.ob3:	..\rt\llscnio.asm ..\inc\files.inc ..\inc\fdb.inc \
	..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc ..\inc\const.inc \
	..\inc\llgrp.inc ..\inc\baslibma.inc ..\inc\intmac.inc \
	..\inc\ibmunv.inc ..\inc\oscalls.inc ..\inc\idmac.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llscnio.asm, llscnio.ob3;

llscnfcn.ob3:	..\rt\llscnfcn.asm ..\inc\llgrp.inc ..\inc\intmac.inc \
	..\inc\ibmunv.inc ..\inc\idmac.inc ..\inc\oscalls.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llscnfcn.asm, llscnfcn.ob3;

llque.ob3:	..\rt\llque.asm ..\inc\idmac.inc ..\inc\intmac.inc \
	..\inc\ibmunv.inc ..\inc\oscalls.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llque.asm, llque.ob3;

llparam.ob3:	..\rt\llparam.asm ..\inc\oscalls.inc ..\inc\idmac.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llparam.asm, llparam.ob3;

llkeys.ob3:	..\rt\llkeys.asm ..\inc\error2.inc ..\inc\const.inc \
	..\inc\intmac.inc ..\inc\ibmunv.inc ..\inc\oscalls.inc \
	..\inc\idmac.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llkeys.asm, llkeys.ob3;

lllpt.ob3:	..\rt\lllpt.asm ..\inc\ibmunv.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\lllpt.asm, lllpt.ob3;

llsnd.ob3:	..\rt\llsnd.asm ..\inc\event.inc ..\inc\intmac.inc \
	..\inc\ibmunv.inc ..\inc\idmac.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\llsnd.asm, llsnd.ob3;

messages.ob3:	..\rt\messages.asm ..\inc\messages.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\messages.asm, messages.ob3;

nhdebug.ob3:	..\rt\nhdebug.asm ..\inc\array.inc ..\inc\idmac.inc \
	..\inc\nhutil.inc ..\inc\files.inc ..\inc\fdb.inc \
	..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\string.inc ..\inc\lmem.inc ..\inc\sb.inc \
	..\inc\baslibma.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\nhdebug.asm, nhdebug.ob3;

nhinit.ob3:	..\rt\nhinit.asm ..\inc\idmac.inc ..\inc\messages.inc \
	..\inc\compvect.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\nhinit.asm, nhinit.ob3;

nhlhcore.ob3:	..\rt\nhlhcore.asm ..\inc\array.inc ..\inc\idmac.inc \
	..\inc\nhutil.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\files.inc ..\inc\fdb.inc \
	..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc \
	..\inc\baslibma.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\nhlhcore.asm, nhlhcore.ob3;

nhlhutil.ob3:	..\rt\nhlhutil.asm ..\inc\array.inc ..\inc\idmac.inc \
	..\inc\nhutil.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\files.inc ..\inc\fdb.inc \
	..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc \
	..\inc\baslibma.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\nhlhutil.asm, nhlhutil.ob3;

nhstutil.ob3:	..\rt\nhstutil.asm ..\inc\nhutil.inc ..\inc\idmac.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\nhstutil.asm, nhstutil.ob3;

oscmd.ob3:	..\rt\oscmd.asm ..\inc\string.inc ..\inc\lmem.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\idmac.inc ..\inc\ascii.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\oscmd.asm, oscmd.ob3;

osdate.ob3:	..\rt\osdate.asm ..\inc\string.inc ..\inc\lmem.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\oscalls.inc ..\inc\idmac.inc \
	..\inc\files.inc ..\inc\fdb.inc ..\inc\pointers.inc ..\inc\sb.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\osdate.asm, osdate.ob3;

osinit.ob3:	..\rt\osinit.asm ..\inc\compvect.inc ..\inc\rtps.inc \
	..\inc\idmac.inc ..\inc\files.inc ..\inc\fdb.inc ..\inc\pointers.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\osinit.asm, osinit.ob3;

osstmt.ob3:	..\rt\osstmt.asm ..\inc\intasg.inc ..\inc\const.inc \
	..\inc\idmac.inc ..\inc\string.inc ..\inc\lmem.inc ..\inc\sb.inc \
	..\inc\omega.inc ..\inc\rtps.inc ..\inc\ascii.inc ..\inc\addr.inc \
	..\inc\devdef.inc ..\inc\files.inc ..\inc\fdb.inc \
	..\inc\pointers.inc ..\inc\sb.inc ..\inc\baslibma.inc \
	..\inc\oscalls.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\osstmt.asm, osstmt.ob3;

ostimer.ob3:	..\rt\ostimer.asm ..\inc\files.inc ..\inc\fdb.inc \
	..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc \
	..\inc\oscalls.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\ostimer.asm, ostimer.ob3;

paint.ob3:	..\rt\paint.asm ..\inc\const.inc ..\inc\string.inc \
	..\inc\lmem.inc ..\inc\sb.inc ..\inc\omega.inc ..\inc\idmac.inc \
	..\inc\baslibma.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\paint.asm, paint.ob3;

peek.ob3:	..\rt\peek.asm ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\peek.asm, peek.ob3;

rtinit.ob3:	..\rt\rtinit.asm ..\inc\ibmunv.inc ..\inc\idmac.inc \
	..\inc\addr.inc ..\inc\global.inc ..\crt\cdata3.inc ..\crt\cdata.inc \
	..\inc\rtps.inc ..\inc\array.inc ..\inc\global.inc \
	..\inc\messages.inc ..\inc\error2.inc ..\inc\string.inc \
	..\inc\lmem.inc ..\inc\sb.inc ..\inc\omega.inc ..\inc\stack.inc \
	..\inc\stack2.inc ..\inc\const.inc ..\inc\fdb.inc \
	..\inc\pointers.inc ..\inc\sb.inc ..\inc\compvect.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\rtinit.asm, rtinit.ob3;

rtllinit.ob3:	..\rt\rtllinit.asm ..\inc\idmac.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\rtllinit.asm, rtllinit.ob3;

rtm86.ob3:	..\rt\rtm86.asm ..\inc\rtmint.inc ..\inc\ulib.inc \
	..\inc\stack.inc ..\inc\addr.inc ..\inc\files.inc ..\inc\fdb.inc \
	..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc ..\inc\intmac.inc \
	..\inc\intasg.inc ..\inc\idmac.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\rtm86.asm, rtm86.ob3;

rtmload.ob3:	..\rt\rtmload.asm ..\inc\messages.inc \
	..\inc\exehdr.inc ..\inc\idmac.inc ..\inc\string.inc ..\inc\lmem.inc \
	..\inc\sb.inc ..\inc\omega.inc ..\inc\const.inc ..\inc\ulib.inc \
	..\inc\rtps.inc ..\inc\array.inc ..\inc\files.inc ..\inc\fdb.inc \
	..\inc\pointers.inc ..\inc\sb.inc ..\inc\baslibma.inc \
	..\inc\addr.inc ..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\rtmload.asm, rtmload.ob3;

rtterm.ob3:	..\rt\rtterm.asm ..\inc\event.inc ..\inc\files.inc \
	..\inc\fdb.inc ..\inc\pointers.inc ..\inc\sb.inc ..\inc\omega.inc \
	..\inc\addr.inc ..\inc\messages.inc ..\inc\seg.inc \
	..\inc\cmacros.inc ..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\rtterm.asm, rtterm.ob3;

sninit.ob3:	..\rt\sninit.asm ..\inc\ibmunv.inc ..\inc\idmac.inc \
	..\inc\compvect.inc ..\inc\seg.inc ..\inc\cmacros.inc \
	..\inc\rmacros.inc ..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\sninit.asm, sninit.ob3;

stfree.ob3:	..\rt\stfree.asm ..\inc\idmac.inc ..\inc\string.inc \
	..\inc\lmem.inc ..\inc\sb.inc ..\inc\omega.inc ..\inc\stack2.inc \
	..\inc\seg.inc ..\inc\cmacros.inc ..\inc\rmacros.inc \
	..\inc\switch.inc
	$(TL)\masm $(INCLUDE) $(IDB) -DQBAS -DNORTM -DDOS3 -DEM -DNS -MX -v -e ..\rt\stfree.asm, stfree.ob3;

chkstk.obj:	..\crt\chkstk.asm ..\crt\msdos.inc ..\inc\cmacros.inc \
	..\crt\version.inc
	$(TL)\masm $(INCLUDE) -D_BASIC -D_QB -DEI_QBASIC=-1 -MX -v -e -W2 ..\crt\chkstk.asm, chkstk.obj;

chksum.obj:	..\crt\chksum.asm ..\inc\cmacros.inc \
	..\crt\version.inc
	$(TL)\masm $(INCLUDE) -D_BASIC -D_QB -DEI_QBASIC=-1 -MX -v -e -W2 ..\crt\chksum.asm, chksum.obj;

crt0.obj:	..\crt\crt0.asm ..\inc\stack2.inc ..\crt\brkctl.inc \
	..\crt\msdos.inc ..\inc\cmacros.inc ..\crt\version.inc
	$(TL)\masm $(INCLUDE) -D_BASIC -D_QB -DEI_QBASIC=-1 -MX -v -e -W2 ..\crt\crt0.asm, crt0.obj;

crt0dat.obj:	..\crt\crt0dat.asm ..\crt\cdata2.inc ..\crt\msdos.inc \
	..\inc\cmacros.inc ..\crt\version.inc
	$(TL)\masm $(INCLUDE) -D_BASIC -D_QB -DEI_QBASIC=-1 -MX -v -e -W2 ..\crt\crt0dat.asm, crt0dat.obj;

crt0fp.obj:	..\crt\crt0fp.asm ..\inc\cmacros.inc \
	..\crt\version.inc
	$(TL)\masm $(INCLUDE) -D_BASIC -D_QB -DEI_QBASIC=-1 -MX -v -e -W2 ..\crt\crt0fp.asm, crt0fp.obj;

fmsghdr.obj:	..\crt\fmsghdr.asm ..\crt\msdos.inc \
	..\inc\cmacros.inc ..\crt\version.inc
	$(TL)\masm $(INCLUDE) -D_BASIC -D_QB -DEI_QBASIC=-1 -MX -v -e -W2 ..\crt\fmsghdr.asm, fmsghdr.obj;
