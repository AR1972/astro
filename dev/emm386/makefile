#******************************************************************************
#	title	MAKEFILE - CEMM build file
#******************************************************************************
#
#   (C) Copyright MICROSOFT Corp. 1986-1991
#   (C) Copyright COMPAQ Computer Corp. 1986-1991
#
#   Title:    EMM386 - MICROSOFT Expanded Memory Manager 386
#
#   Module:   MAKEFILE
#
#   Version:  0.05
#
#   Date:     May 12, 1986
#
#   Author:   Steve Preston
#
#******************************************************************************
#
#   Change log:
#
#     DATE    REVISION			DESCRIPTION
#   --------  --------	-------------------------------------------------------
#	      Original
#   01/27/89    4.00         EMM files moved to CEMM directory (RDV)
#
#******************************************************************************
#
#   Functional Description:
#
#   This file assembles and links
#	(1) EMM386.EXE	- Production version of CEMM driver
#	(2) EMM386D.EXE	- Debug (Internal) version of CEMM driver
#
#******************************************************************************

#******************************************************************************
#	R U L E S   and   M A C R O S
#******************************************************************************
.SUFFIXES:
.SUFFIXES:	.c .asm .obj .lst .def .lnk .lrf .exe .com


#	Definition for turning OFF high memory allocation
#		HIFLAG = -DNOHIMEM => turns it off
#		HIFLAG =	   => turns on high memory allocation
HIFLAG	=

# International Modifications:
MSG_DIR=$(LANG_SRC)\$(COUNTRY)\dev\emm386


#	Definitions for assembler
#
ASM	 =  masm
AFLAGS	 = -Mx -t -I$(MSG_DIR) -DMSFLAG $(HIFLAG)
ADFLAGS  = -Mx -t -I$(MSG_DIR) -DMSFLAG -DDEBUG $(HIFLAG)

#	Definitions for linker for old style .exe files
LINK	= link
LFLAGS  = /NOI /M

#	Rules for generating object and linker response and definition files
.asm.obj:
	$(ASM) $(AFLAGS) $*.asm;

#******************************************************************************
#	List of include files required
#******************************************************************************
MEMMINC =	ascii_sm.equ driver.equ pic_def.equ romstruc.equ \
		romxbios.equ desc.inc dma.inc emm386.inc kbd.inc \
		$(MSG_DIR)\oemdep.inc page.inc vdmseg.inc vdmsel.inc \
		vm386.inc emmfunct.inc emmdata.inc winemm.inc \
		allocmem.inc xmm.inc ps2ex.inc ps2equ.inc eisaex.inc \
		i13.inc

MEMMINCD =	ascii_sm.equ driver.equ pic_def.equ romstruc.equ \
		romxbios.equ desc.inc dma.inc emm386.inc kbd.inc \
		$(MSG_DIR)\oemdep.inc page.inc vdmseg.inc vdmsel.inc \
		vm386.inc emmfunct.inc emmdata.inc winemm.inc \
		allocmem.inc xmm.inc ps2ex.inc ps2equ.inc eisaex.inc \
		i13.inc debmac.inc

#******************************************************************************
#	List of object files required
#******************************************************************************

MEMMOBJ =	memm386.obj elimfunc.obj init.obj allocmem.obj emmmes.obj \
		wsinit.obj memmonf.obj WinSrch.obj modesw.obj i286.obj \
		i386.obj tabdef.obj except.obj vminst.obj dmatrap.obj \
		vminit.obj int15.obj retreal.obj rrtrap.obj vdminit.obj \
		errhndlr.obj ekbd.obj util.obj initdeb.obj emm.obj \
		emminit.obj iotrap.obj a20trap.obj emmfunct.obj pictrap.obj\
		vcpi.obj emmdata.obj emmutils.obj emm40.obj dmaserv.obj\
		winemm.obj XMS.obj dmaeisa.obj dmaps2.obj segfix.obj \
		segend.obj dbg.obj pagetrap.obj ps2table.obj umbseg.obj \
		xmsutils.obj

BUGOBJ =	memm386.obj elimfunc.obj allocmem.obj emmmes.obj \
		wsinit.obj memmonf.obj WinSrch.obj modesw.obj i286.obj \
		i386.obj tabdef.obj except.obj vminst.obj dmatrap.obj \
		vminit.obj int15.obj retreal.obj rrtrap.obj vdminit.obj \
		errhndlr.obj ekbd.obj util.obj XMS.obj dmaps2.obj umbseg.obj \
		dmaeisa.obj emm.obj emminit.obj iotrap.obj a20trap.obj \
		emmfunct.obj emmdata.obj dmaserv.obj dbg.obj emm40.obj \
		winemm.obj segfix.obj segend.obj pagetrap.obj ps2table.obj \
		initD.obj initdebD.obj pictrapD.obj emmutilD.obj xmsutilD.obj \
		vcpiD.obj

#******************************************************************************
#	List of libraries required
#******************************************************************************


#BUGLIBS  = buglib.lib


#******************************************************************************
#	MEMM targets
#******************************************************************************
#MEMM: emm386.exe emm386d.exe
bins    =..\..\binaries
cprs    =..\..\compress
dest    =emm386.exe
cpr     =emm386.ex_
deb     =emm386d.exe

#
####################### Dependencies Begin Here ##########################
#

!IFNDEF       BUILDER

all: $(dest) $(deb)

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(cpr)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)

$(cprs)\$(cpr):  $(cpr)
            copy $(cpr) $(cprs)

$(cpr):       $(dest)
            $(compress) $(dest) $(cpr)

!ENDIF


#	production version of EMM386 device driver
#
$(dest): $(MEMMOBJ) memm.lnk
	$(LINK) $(LFLAGS)  @memm.lnk
	mapsym emm386.map
	copy emm386.exe lhvxd\emm386.exe
	del emm386.exe
	cd lhvxd
	nmake
	cd..
        ren emm386nw.exe $(dest)

#	debug version of EMM386 driver
#
$(deb): $(BUGOBJ) $(BUGLIBS) memmd.lnk
	$(LINK) $(LFLAGS) @memmd.lnk
	copy emm386.map emm386.max
	copy emm386d.map emm386.map
	copy emm386.sym emm386.syx
	mapsym emm386.map
	copy emm386.sym emm386d.sym
	copy emm386.syx emm386.sym
	copy emm386.max emm386.map
	del emm386.max
	del emm386.syx

#clean:
#	del *.obj
#	del *.lst

safe:
	fdir makefile /w+
	fdir *.asm /w+
	fdir *.inc /w+
	fdir *.equ /w+
	fdir *.lnk /w+
	fdir *.str /w+
	fdir *.lnk /w+
	fdir *.doc /w+
	fdir *.lib /w+
	fdir *.mac /w+

unsafe:
	fdir *.* /w-

#******************************************************************************
# CEMM driver i/f modules
#******************************************************************************

segend.obj: segend.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

segfix.obj: segfix.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

memm386.obj: memm386.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

elimfunc.obj: elimfunc.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

init.obj: init.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

allocmem.obj: allocmem.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

WinSrch.obj: WinSrch.asm  $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

wsinit.obj: wsinit.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

emmmes.obj: $(MSG_DIR)\emmmes.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $(MSG_DIR)\$*.asm;

memmonf.obj: memmonf.asm VDMSEG.INC ASCII_SM.EQU EMMFUNCT.INC
	$(ASM) $(AFLAGS) $*.asm;

ps2table.obj: ps2table.asm VDMSEG.INC
	$(ASM) $(AFLAGS) $*.asm;


#******************************************************************************
# VDM modules
#******************************************************************************

vdminit.obj: vdminit.asm $(MEMMINC)

i286.obj: i286.asm $(MEMMINC)

i386.obj: i386.asm $(MEMMINC)

modesw.obj: modesw.asm $(MEMMINC) 

tabdef.obj: tabdef.asm $(MEMMINC)

except.obj: except.asm $(MEMMINC)

vminst.obj: vminst.asm $(MEMMINC)

iotrap.obj: iotrap.asm $(MEMMINC)

vminit.obj: vminit.asm $(MEMMINC)

a20trap.obj: a20trap.asm $(MEMMINC)

rrtrap.obj: rrtrap.asm $(MEMMINC)

retreal.obj: retreal.asm $(MEMMINC)

errhndlr.obj: errhndlr.asm $(MEMMINC)

ekbd.obj: ekbd.asm $(MEMMINC)

initdeb.obj: initdeb.asm $(MEMMINC)

pictrap.obj: pictrap.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

pagetrap.obj: pagetrap.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

dmatrap.obj: dmatrap.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

dmaserv.obj: dmaserv.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

emminit.obj: emminit.asm $(MEMMINC)
	$(ASM) $(AFLAGS) emminit.asm;

int15.obj: int15.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

util.obj: util.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

emm.obj: emm.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

XMS.obj: XMS.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

dmaeisa.obj: dmaeisa.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

dmaps2.obj: dmaps2.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

umbseg.obj: umbseg.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;

xmsutils.obj: xmsutils.asm $(MEMMINC)
	$(ASM) $(AFLAGS) $*.asm;


#******************************************************************************
# debug modules
#******************************************************************************

initD.obj: init.asm $(MEMMINCD)
	$(ASM) $(ADFLAGS) init.asm,initD.obj;

initdebD.obj: initdeb.asm $(MEMMINCD)
	$(ASM) $(ADFLAGS) initdeb.asm,initdebD.obj;

pictrapD.obj: pictrap.asm $(MEMMINCD)
	$(ASM) $(ADFLAGS) pictrap.asm,pictrapD.obj;

xmsutilD.obj: xmsutils.asm $(MEMMINCD)
	$(ASM) $(ADFLAGS) xmsutils.asm,xmsutilD.obj;

emmutilD.obj: emmutils.asm $(MEMMINCD)
	$(ASM) $(ADFLAGS) emmutils.asm,emmutilD.obj;

dbg.obj: dbg.asm $(MEMMINCD)
	$(ASM) $(ADFLAGS) $*.asm;

vcpiD.obj : vcpi.asm $(MEMMINC)
	$(ASM) $(ADFLAGS) vcpi.asm,vcpiD.obj;

#tabdefD.obj: tabdef.asm $(MEMMINC)
#	 $(ASM) $(ADFLAGS) tabdef.asm,tabdefD.obj;
#
#vminstD.obj: vminst.asm $(MEMMINC)
#	 $(ASM) $(ADFLAGS) vminst.asm,vminstD.obj;
#
#exceptD.obj: except.asm $(MEMMINC)
#	 $(ASM) $(ADFLAGS) except.asm,exceptD.obj;
#
#vdminitD.obj: vdminit.asm $(MEMMINC)
#	 $(ASM) $(ADFLAGS) vdminit.asm,vdminitD.obj;
#
#emmD.obj: emm.asm $(MEMMINC)
#	 $(ASM) $(ADFLAGS) emm.asm,emmD.obj;
#
#kbd.obj : kbd.asm $(MEMMINC)
#	 $(ASM) $(ADFLAGS) -p $*.asm;
#
#print.obj: print.asm $(MEMMINC)
#	 $(ASM) $(ADFLAGS) -p $*.asm;


#******************************************************************************
# emm modules
#******************************************************************************

emm40.obj : emm40.asm $(MEMMINC)
	$(ASM) $(AFLAGS) emm40.asm;

vcpi.obj : vcpi.asm $(MEMMINC)
	$(ASM) $(AFLAGS) vcpi.asm;

emmutils.obj : emmutils.asm $(MEMMINC)
	$(ASM) $(AFLAGS) emmutils.asm;

emmfunct.obj : emmfunct.asm $(MEMMINC)
	$(ASM) $(AFLAGS) emmfunct.asm;

emmdata.obj : emmdata.asm $(MEMMINC)
	$(ASM) $(AFLAGS) emmdata.asm;

#******************************************************************************
# winemm modules
#******************************************************************************

winemm.obj : winemm.asm $(MEMMINC) 
	$(ASM) $(AFLAGS) winemm.asm;


