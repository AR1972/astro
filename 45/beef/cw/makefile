#############################################################################
#	MAKEFILE for all of CW
#	useable with NMAKE
#	Assumes DOSENV tools
#############################################################################
#
# symbols imported:
#	FINAL = final library name
#	?? more to document here ???
#	MAKE_LOADER	=> make the loader
#	MAKE_CBOX	=> make CBOX variant
#
#############################################################################
## REVIEW: add comments to explain complicated things (like DUAL).
#
#############################################################################

all: tools headers version loader kernel user cbox final

#############################################################################
# General Stuff

.SUFFIXES: .c .asm .h .inc .obj .lib .exe .m3 .m5 .o5

# build directory is prefixed by $
D=$$$(FINAL)
# Add version control directory to INCLUDE path (for version.h/inc)
INCLUDE=$(INCLUDE)
# general object suffix
O=obj
# Tools compile flags
TFLAGS=-DD86 -DCC

# Compilers
CC=$(TL)\cl
CS=tools\csl


# Final Output directory
$D:
!IF ([tools\test -d $D] != 0)
	mkdir $D
!ENDIF

# Empty library
nullib.lib:
	$(TL)\lib nullib,

#############################################################################
# Tools

tools: tools\cowstrip.exe tools\cowseg.exe tools\echotime.exe \
	tools\iterate.exe tools\catresp.exe \
	tools\mkdrv.exe

{tools}.c{tools}.obj:
	$(CC) -c $(TFLAGS) -Fotools\ $*.c

{tools}.c{tools}.exe:
	$(CC) $(TFLAGS) -o $@ $*.c

# multiple file utility programs
COWSTRIP_OBJ = tools\cowstrip.obj tools\misc.obj
tools\cowstrip.exe: $(COWSTRIP_OBJ)
	$(CC) $(TFLAGS) -o tools\cowstrip.exe $(COWSTRIP_OBJ)

#############################################################################
# Version Independent Includes

headers: private public

public: inc\cwindows.h inc\csdm.h inc\csdmtmpl.h inc\cwindows.txt inc\cedit.h \
	inc\cwindows.inc \
	inc\cgraphic.h inc\cgraphic.txt

# Cwindows.h header
CWINDOWS_H = inc\cow.h inc\kinput.h inc\inmou.h inc\kmem.h inc\inkbd.h \
	inc\kkeyboar.h inc\inscr.h inc\indrv.h inc\inscreen.h inc\udialog.h \
	inc\uedit.h inc\uevent.h inc\uisa.h inc\umenu.h inc\uscreen.h \
	inc\uscroll.h inc\uwindow.h inc\insyd.h inc\uutil.h inc\vkey.h \
	inc\ukanji.h inc\ucolor.h

# SDM headers
SDM_H = inc\usdm.h
SDMTMPL_H = inc\sdmver.h inc\usdmtmpl.h

# Graphics Support
CGRAPHIC_H = inc\ingxd.h inc\graphic.h

## Private stuff
PRIVATE_INC = inc\uevent.inc inc\uisa.inc inc\uscroll.inc inc\uscreen.inc \
	inc\vkey.inc inc\uedit.inc inc\indrv.inc inc\inscr.inc inc\inkbd.inc \
	inc\insyd.inc inc\inmou.inc inc\kmem.inc inc\ingxd.inc

private: $(PRIVATE_INC) inc\cwproc.inc

{inc}.h{inc}.inc:
	if exist *.db del *.db
	$(CS) -Q -mW -SsCim $(DEFS) inc\private.c
	-$(TL)\mv $(*B).db $@
	if exist private.im del private.im
        if exist private.sy del private.sy
        if exist *.db del *.db

# WARNING: the following rule does not always work !!!
# Special rule to prevent default rule (above) from replacing cow.inc
# NOTE: it appears that this does not work...

inc\cow.inc: inc\cow.h
	tools\touch inc\cow.inc


# Main header
inc\cwindows.h: inc\cwindows.hdr $(CWINDOWS_H)
	copy inc\cwindows.hdr inc\cwindows.h
	tools\echotime -- Created /t */			>>inc\cwindows.h
	tools\iterate "tools\cowstrip %s >> inc\cwindows.h" @<<
$(CWINDOWS_H)
<<

# Sdm headers
inc\csdm.h: inc\csdm.hdr $(SDM_H)
	copy inc\csdm.hdr inc\csdm.h
	tools\echotime -- Created /t */			>>inc\csdm.h
	tools\cowstrip $(SDM_H)				>>inc\csdm.h

inc\csdmtmpl.h: inc\csdmtmpl.hdr $(SDMTMPL_H)
	copy inc\csdmtmpl.hdr inc\csdmtmpl.h
	tools\echotime -- Created /t */			>>inc\csdmtmpl.h
	tools\cowstrip $(SDMTMPL_H)			>>inc\csdmtmpl.h

# Graphics headers
inc\cgraphic.h: inc\cgraphic.hdr $(CGRAPHIC_H)
	copy inc\cgraphic.hdr inc\cgraphic.h
	tools\echotime -- Created /t */			>>inc\cgraphic.h
	tools\cowstrip $(CGRAPHIC_H)			>>inc\cgraphic.h

# .TXT for Pcode toolbox mechanism
inc\cwindows.txt: inc\cwindows.h inc\csdm.h
	$(TL)\sed -n -e "s/FARPUBLIC/FAR PASCAL/p" inc\cwindows.h  >tmp1
	$(TL)\sed -n -e "s/FARPUBLIC/FAR PASCAL/p" inc\csdm.h     >>tmp1
	$(TL)\fgrep -v typedef tmp1 | fgrep -v Internal | fgrep -v define >inc\cwindows.txt
	if exist tmp1 del tmp1

inc\cgraphic.txt: inc\cgraphic.h
	$(TL)\sed -n -e "s/FARPUBLIC/FAR PASCAL/p" inc\cgraphic.h  >inc\cgraphic.txt

# Multi-line edit stuff (interface is questionable)
inc\cedit.h: inc\cedit.hdr inc\umedit.h
	copy inc\cedit.hdr inc\cedit.h
	tools\echotime -- Created /t */			>>inc\cedit.h
	tools\cowstrip inc\umedit.h			>>inc\cedit.h

# CW procedure templates
inc\cwproc.inc: inc\cwindows.txt
	$(TL)\sed -n -e "s/.*FAR PASCAL./externFP	/p" inc\cwindows.txt >tmp1
	$(TL)\fgrep -v OPTIONAL tmp1			>tmp2
	$(TL)\sed -n -e "s/(.*//p" tmp2			>inc\cwproc.inc
	if exist tmp1 del tmp1
	if exist tmp2 del tmp2

inc\cwindows.inc: $(PRIVATE_INC) inc\cwproc.inc
	copy inc\cwindows.idr inc\cwindows.inc
	tools\echotime ; Created /t */			>>inc\cwindows.inc
	tools\iterate "cat %s >>inc\cwindows.inc" @<<
$(PRIVATE_INC)
<<
	echo IFNDEF NOPROCS				>>inc\cwindows.inc
	$(TL)\cat inc\cwproc.inc				>>inc\cwindows.inc
	echo ENDIF ;!NOPROCS				>>inc\cwindows.inc

#############################################################################
# Version Control

version: $D $D\version.h $D\version.inc

$D\version.h: inc\$(VERSION)
	copy inc\$(VERSION) $D\version.h
	tools\touch $D\version.h

$D\version.inc: $D\version.h
	$(TL)\sed -n -e "s/\/\*/;*/" -e "s/^#define[ 	]*//" -e "/^[^; 	]/s/^[^; 	]*/& = /p" $D\version.h >$D\version.inc


#############################################################################
# DUAL rules

T=0.tmp

{lib}.obj{$D}.m3:
	modobj -s DOS3 -e dual3.xpt $< $@

{lib}.obj{$D}.m5:
	modobj -s OS2 -e dual5.xpt $< $@

{kernel}.asm{$D}.m3:
	$(TL)\masm $(AFLAGS) -Ikernel $(DEFS) $<, $T;
	modobj -s DOS3 -e dual3.xpt $T $@

{kernel}.asm{$D}.m5:
	$(TL)\masm $(AFLAGS) -Ikernel $(DEFSOS2) $<, $T;
	modobj -s OS2 -e dual5.xpt $T $@

{kernel}.asm{$D}.o5:
	$(TL)\masm $(AFLAGS) -Ikernel $(DEFSOS2) $<, $@;

{kernel}.c{$D}.m3:
	$(CC) -c $(CCFLAGS) -AM $(DEFS) -NT $(CORE) -Ikernel -Fo$D\ $<
	modobj -s DOS3 -e dual3.xpt $(@R).obj $(@R).m3

{kernel}.c{$D}.m5:
	$(CC) -c $(CCFLAGS) -AM $(DEFS) -NT $(CORE) -Ikernel -Fo$D\ $<
	modobj -s OS2 -e dual5.xpt $(@R).obj $(@R).m5

$D\lddrvd5.m5: kernel\loaddrv.asm
	cp kernel\loaddrv.asm $D\lddrvd5.asm
	$(TL)\masm $(AFLAGS) -Ikernel $(DEFSOS2) $D\lddrvd5.asm, $T;
	modobj -s OS2 -e dual5.xpt $T $D\lddrvd5.m5

{user}.asm{$D}.m3:
	$(TL)\masm $(AFLAGS) -Iuser $(DEFS) $<, $T;
	modobj -s DOS3 -e dual3.xpt $T $@

{user}.asm{$D}.m5:
	$(TL)\masm $(AFLAGS) -Iuser $(DEFSOS2) $<, $T;
	modobj -s OS2 -e dual5.xpt $T $@

{user}.c{$D}.m3:
	$(CC) -c $(CCFLAGS) -AM $(DEFS) -NT $(CORE) -Iuser -Fo$D\ $<
	modobj -s DOS3 -e dual3.xpt $(@R).obj $(@R).m3

{user}.c{$D}.m5:
	$(CC) -c $(CCFLAGS) -AM $(DEFS) -NT $(CORE) -Iuser -Fo$D\ $<
	modobj -s OS2 -e dual5.xpt $(@R).obj $(@R).m5

$D\khead.m3: $D\knull.obj
	tools\cowseg $D\knull.obj $D\khead.obj
	modobj -s DOS3 -e dual3.xpt $D\khead.obj $D\khead.m3

#############################################################################
# Loader

!IFDEF MAKE_LOADER
loader: $D\loader.exe
!ELSE
loader:
!ENDIF

# "loader" must be first, "loadrelo" must be last.
LOADER_OBJ = $D\loader.obj $D\loadmem.obj $D\loadrelo.obj

{loader}.asm{$D}.obj:
	$(TL)\masm $(AFLAGS) -Iloader $(DEFS) $<, $@;

$D\loader.exe: $(LOADER_OBJ)
	$(TL)\link @<<
$(LOADER_OBJ)
$D\loader.exe
$D\loader.map/MA;
<<
	cd $D
	$(TL)\mapsym loader
	cd ..

#############################################################################
# Kernel

kernel: $D\$(KERNEL)

{kernel}.asm{$D}.obj:
	$(TL)\masm $(AFLAGS) -Ikernel $(DEFS) $<, $@;

{kernel}.c{$D}.obj:	# Medium model for asserts
	$(CC) -c $(CCFLAGS) -AM $(DEFS) -NT $(CORE) -Ikernel -Fo$D\ $<

# - files for swapped version
KERNEL_OBJ = $D\galloc.obj $D\gcompact.obj $D\gmem.obj $D\gwinterf.obj \
	$D\glock.obj $D\handle.obj $D\int24.obj $D\kerninit.obj $D\khead.obj \
	$D\ktimer.obj $D\krun.obj $D\ldreloc.obj $D\ldseg.obj $D\ldstack.obj \
	$D\ldthunk.obj $D\mouse.obj $D\mousedat.obj $D\keyboard.obj \
	$D\cbfree.obj \
	$D\assertsw.obj $D\assert.obj $D\gacheck.obj $D\lddebug.obj \
	$D\setjmp.obj \
!IFNDEF	MAKE_CBOX
	$D\promptc.obj \
!ENDIF
!IFDEF	MAKE_QC
	$D\gfixcomp.obj $D\dump.obj $D\gdsize.obj \
!ENDIF
	$D\loaddrv.obj $D\utildrv.obj \
	$D\inos.obj $D\mouses3.obj $D\kstart.obj

# - files for DUAL
KERNELD_OBJ = $D\cwos2.obj $D\osspec.obj \
	$D\mousedat.obj $D\keyboard.obj \
	$D\khead.obj \
	$D\assert.obj \
	$D\stubs.obj \
	$D\utildrv.obj \
	$D\kerninit.obj

KERNELD_O5 = $D\inos.o5

KERNELD_M3 = $D\galloc.m3 $D\gcompact.m3 $D\gmem.m3 $D\gwinterf.m3 \
	$D\glock.m3 $D\handle.m3 $D\gacheck.m3 \
	$D\ldreloc.m3 $D\ldseg.m3 $D\ldstack.m3 $D\ldthunk.m3 \
	$D\cbfree.m3 \
	$D\mouse.m3 $D\mouses3.m3 \
	$D\int24.m3 $D\ktimer.m3 \
	$D\kstart.m3 \
	$D\krun.m3 \
	$D\assertsw.m3 \
	$D\lddebug.m3 \
	$D\loaddrv.m3 \
!IFNDEF	MAKE_CBOX
	$D\promptc.m3 \
!ENDIF
	$D\setjmp.m3 \
	$D\prtsc3.m3

KERNELD_M5 = $D\startup.m5 $D\mouse5.m5 \
	$D\gmemory5.m5 $D\ktimer5.m5 \
	$D\assertns.m5 \
	$D\lddrvd5.m5 \
	$D\debug5.m5 \
	$D\krun5.m5 \
	$D\prtsc5.m5


dprep: $(KERNELD_M5)

# - files for dos 3 (non-swapped)
KERNEL3_OBJ = $D\startup.obj $D\assert.obj $D\assertns.obj \
	$D\mouse.obj $D\mousedat.obj \
	$D\gmemory3.obj $D\int24.obj $D\keyboard.obj $D\loaddrv.obj \
	$D\inos.obj $D\mouses3.obj

# - files for dos 5
KERNEL5_OBJ = $D\startup.obj $D\assert.obj $D\assertns.obj \
	$D\mouse5.obj $D\mousedat.obj \
	$D\gmemory5.obj $D\ktimer5.obj $D\keyboard.obj $D\loaddrv.obj \
	$D\inos.obj $D\krun5.obj

#### Special case for KHEAD.OBJ
$D\khead.obj: $D\knull.obj
	tools\cowseg $D\knull.obj $D\khead.obj

# - swapped
$D\kernel.lib: $(KERNEL_OBJ) nullib.lib $D\kernel.llk
	$(TL)\lib @$D\kernel.llk

$D\kernel.llk: makefile
	echo nullib				> $D\kernel.llk
	tools\catresp $D\kernel.llk @<<
$(KERNEL_OBJ:.obj=)
<<
	echo $D\kernel.map			>> $D\kernel.llk
	echo $D\kernel.lib			>> $D\kernel.llk

# - DUAL
$D\kerneld.lib: $(KERNELD_OBJ) $(KERNELD_O5) $(KERNELD_M3) $(KERNELD_M5) nullib.lib $D\kerneld.llk
	$(TL)\lib @$D\kerneld.llk

$D\kerneld.llk: makefile
	echo nullib				> $D\kerneld.llk
	tools\catresp $D\kerneld.llk @<<
$(KERNELD_OBJ) $(KERNELD_O5) $(KERNELD_M3) $(KERNELD_M5)
<<
	echo $D\kerneld.map			>> $D\kerneld.llk
	echo $D\kerneld.lib			>> $D\kerneld.llk


# - DOS 3 non-swapped
$D\kernel3.lib: $(KERNEL3_OBJ) nullib.lib $D\kernel3.llk
	$(TL)\lib @$D\kernel3.llk

$D\kernel3.llk: makefile
	echo nullib				> $D\kernel3.llk
	tools\catresp $D\kernel3.llk @<<
$(KERNEL3_OBJ:.obj=)
<<
	echo $D\kernel3.map			>> $D\kernel3.llk
	echo $D\kernel3.lib			>> $D\kernel3.llk


# - OS/2 non-swapped
$D\kernel5.lib: $(KERNEL5_OBJ) nullib.lib $D\kernel5.llk
	$(TL)\lib @$D\kernel5.llk

$D\kernel5.llk: makefile
	echo nullib				> $D\kernel5.llk
	tools\catresp $D\kernel5.llk @<<
$(KERNEL5_OBJ:.obj=)
<<
	echo $D\kernel5.map			>> $D\kernel5.llk
	echo $D\kernel5.lib			>> $D\kernel5.llk

#############################################################################
# User

CCPARM=-c -AS -Iuser -Fo$D\ -NT

user: $D\user.lib

{user}.asm{$D}.obj:
	$(TL)\masm $(AFLAGS) -Iuser $(DEFS) $<, $@;

{user}.c{$D}.obj:
	$(CC) $(CCFLAGS) $(DEFS) $(CCPARM) $(CORE) $<

# - default rule for SMALL_COW_INTERNAL

USER_O = $D\alarm.obj $D\box.obj $D\button.obj $D\color.obj \
	$D\defcolor.obj $D\getchar.obj \
!IFNDEF MAKE_WIN
	$D\eventlow.obj \
!ENDIF
	$D\screen.obj $D\screen2.obj \
!IFNDEF	MAKE_CBOX
	$D\menu.obj $D\menu2.obj $D\menucore.obj $D\msgbox.obj \
	$D\event.obj $D\dirlist.obj $D\dlgcore.obj $D\dlgutil.obj \
	$D\overlap.obj $D\overlutl.obj $D\parse.obj $D\rect.obj \
	$D\scroll.obj $D\sdm.obj $D\sdmasm.obj \
	$D\sdmcab.obj $D\sdmtmc.obj $D\sedit.obj $D\static.obj \
	$D\listbox.obj $D\medit.obj $D\meditasm.obj $D\keystub.obj \
	$D\window.obj $D\general.obj $D\case.obj \
!ENDIF
	$D\graphic.obj $D\helpstub.obj $D\inswap.obj $D\kanji.obj \
	$D\strings.obj $D\trapstub.obj $D\util.obj $D\keysynth.obj

USER_O3 = $D\dirlist3.obj $D\util3.obj

USER_O5 = $D\dirlist5.obj $D\util5.obj

USER_M3 = $D\dirlist3.m3 $D\util3.m3

USER_M5 = $D\dirlist5.m5 $D\util5.m5

!IF "$(OSTARG)" == "DOS3"
USER_OBJ = $(USER_O) $(USER_O3)
!ELSE
!IF "$(OSTARG)" == "OS2"
USER_OBJ = $(USER_O) $(USER_O5)
!ELSE
!IF "$(OSTARG)" == "DUAL"
USER_OBJ = $(USER_O) $(USER_M3) $(USER_M5)
!ENDIF
!ENDIF
!ENDIF

$D\user.lib: $(USER_OBJ) nullib.lib $D\user.llk
	$(TL)\lib @$D\user.llk

$D\user.llk: makefile
	echo nullib				> $D\user.llk
	tools\catresp $D\user.llk @<<
$(USER_OBJ:.obj=)
<<
	echo $D\user.map			>> $D\user.llk
	echo $D\user.lib			>> $D\user.llk

# -----------------------------------------------
# Special swap segments

!IF "$(SWAP)" == "COWS"
# - special rules (COWS variant)
$D\color.obj: user\color.c
	$(CC) $(CCFLAGS) $(DEFS) $(CCPARM) INIT user\color.c
$D\defcolor.obj: user\defcolor.c
	$(CC) $(CCFLAGS) $(DEFS) $(CCPARM) INIT user\defcolor.c
!ENDIF ;SWAP COWS

!IF "$(SWAP)" == "COWT"
	echo --- Error Tiny swapped not supported in Make yet
	abort !!!
!ENDIF ;SWAP COWT

#############################################################################
# CBox:

!IF "$(OSTARG)" == "DUAL"
CBOXHDR = $D\cboxhdr.obj
!ELSE
CBOXHDR = cbox\cboxhdr.obj
!ENDIF

!IFDEF MAKE_CBOX
cbox: $D\version.inc $(CBOXHDR) cbox\runreal.exe $D\cbox.lib
!ELSE
cbox:
!ENDIF

# - files for swapped version

T=0.tmp

CBOX_OBJ = $D\winmain.obj $D\dos.obj $D\inchar.obj \
	$D\fileio.obj $D\drive.obj $D\print.obj $D\printmp.obj \
	$D\win.obj $D\dosio.obj

CBOX_M3 = $(CBOX_OBJ:.obj=.m3) $D\prompt.m3
CBOX_M5 = $(CBOX_OBJ:.obj=.m5)

CBOX_D = $D\video.obj $D\gettbl.obj $D\gen.obj $D\math.obj \
	$D\vidmp.obj $D\vidword.obj

CBOX_M = $(CBOX_M3) $(CBOX_M5) $(CBOX_D)

# version independent
{cbox}.asm{cbox}.m3:
	$(TL)\masm $(AFLAGS) -Icbox $(DEFS) $<, $T;
	modobj -s DOS3 -e dual3.xpt $T $@
	mv $@ $D\$(<B)3.obj
	tools\touch $@

{cbox}.asm{cbox}.m5:
	$(TL)\masm $(AFLAGS) -Icbox $(DEFSOS2) $<, $T;
	modobj -s OS2 -e dual5.xpt $T $@
	mv $@ $D\$(<B)5.obj
	tools\touch $@

{cbox}.asm{cbox}.obj:
	$(TL)\masm $(AFLAGS) -Icbox $(DEFS) $<, $@;

# not version independent
{cbox}.asm{$D}.m3:
	$(TL)\masm $(AFLAGS) -Icbox $(DEFS) $<, $T;
	modobj -s DOS3 -e dual3.xpt $T $@
	mv $@ $D\$(<B)3.obj
	tools\touch $@

{cbox}.asm{$D}.m5:
	$(TL)\masm $(AFLAGS) -Icbox $(DEFSOS2) $<, $T;
	modobj -s OS2 -e dual5.xpt $T $@
	mv $@ $D\$(<B)5.obj
	tools\touch $@

{cbox}.asm{$D}.obj:
	$(TL)\masm $(AFLAGS) -Icbox $(DEFS) $<, $@;

# RunReal.exe : special stub for running real mode version
cbox\runreal.exe: cbox\runreal.obj
	$(TL)\masm cbox\runreal,cbox\runreal;
	$(TL)\link /CP:1 cbox\runreal,cbox\runreal.exe;


!IF "$(OSTARG)" == "DUAL"

$D\cbox.lib: $(CBOX_M) nullib.lib $D\cbox.llk
	$(TL)\lib @$D\cbox.llk

$D\cbox.llk: makefile
	echo nullib				> $D\cbox.llk
	tools\catresp $D\cbox.llk @<<
$(CBOX_M3:.m3=3) $(CBOX_M5:.m5=5) $(CBOX_D)
<<

!ELSE

$D\cbox.lib: $(CBOX_OBJ) $(CBOX_D) nullib.lib $D\prompt.obj $D\cbox.llk
	$(TL)\lib @$D\cbox.llk

$D\cbox.llk: makefile
	echo nullib				> $D\cbox.llk
	tools\catresp $D\cbox.llk @<<
$(CBOX_OBJ) $(CBOX_D) $D\prompt.obj
<<

!ENDIF

	echo $D\cbox.map			>> $D\cbox.llk
	echo $D\cbox.lib			>> $D\cbox.llk

# ---------------------------------------------------------------------------
# Extra Libraries included for COWS and CBOX

!IFDEF MAKE_CBOX

!IF "$(FINAL)" == "cboxs.lib"
EXTRA_LIB = $D\cbox.lib lib\math.d86 lib\winter.e86 lib\sbmgr.e86
!ENDIF

!IF "$(FINAL)" == "dcboxs.lib"
EXTRA_LIB = $D\cbox.lib lib\dmath.d86 lib\dwinter.e86 lib\dsbmgr.e86
!ENDIF

!IF "$(FINAL)" == "cboxw.lib"
EXTRA_LIB = $D\cbox.lib lib\math.d86 lib\winter.e86 lib\sbmgr.e86
!ENDIF

!IF "$(FINAL)" == "dcboxw.lib"
EXTRA_LIB = $D\cbox.lib lib\dmath.d86 lib\dwinter.e86 lib\dsbmgr.e86
!ENDIF

!IF "$(FINAL)" == "cbox5.lib"
EXTRA_LIB = $D\cbox.lib lib\math.os2 lib\winter.os2 lib\sbmgr.os2 lib\doscalls.lib
!ENDIF

!IF "$(FINAL)" == "dcbox5.lib"
EXTRA_LIB = $D\cbox.lib lib\dmath.os2 lib\dwinter.os2 lib\dsbmgr.os2 lib\doscalls.lib
!ENDIF

!IF "$(FINAL)" == "cboxd.lib"
EXTRA_LIB = $D\cbox.lib lib\math.dmd lib\winter.dmd lib\sbmgr.dmd \
	$(TL)\lib\doscalls.lib
!ENDIF

!IF "$(FINAL)" == "dcboxd.lib"
EXTRA_LIB = $D\cbox.lib lib\dmath.dmd lib\dwinter.dmd lib\dsbmgr.dmd \
	$(TL)\lib\doscalls.lib
!ENDIF

!ELSE
# Not-CBOX

!IF "$(FINAL)" == "dcowd.lib"
EXTRA_LIB = lib\doscalls.lib
!ELSE
EXTRA_LIB=
!ENDIF

!ENDIF

#############################################################################
# Windows specific glue (in place of kernel)

{win}.asm{$D}.obj:
	$(TL)\masm $(AFLAGS) -Iwin $(DEFS) $<, $@;

{win}.c{$D}.obj:
	$(CC) $(CCFLAGS) $(DEFS) $(CCPARM) $(CORE) $<

# - files for Windows
KERNEL_WIN = $D\winstart.obj $D\winmouse.obj

$D\win.lib: $(KERNEL_WIN) nullib.lib $D\win.llk
	$(TL)\lib @$D\win.llk

$D\win.llk: makefile
	echo nullib				> $D\win.llk
	tools\catresp $D\win.llk @<<
$(KERNEL_WIN:.obj=)
<<
	echo $D\win.map				>> $D\win.llk
	echo $D\win.lib				>> $D\win.llk

#############################################################################
#############################################################################
# Final:

final:	$(FINAL)

# - names of sub libraries

FIXED3_DRV = lib\fx_csd3.obj lib\fx_kbd3.obj lib\fx_syd3.obj
FIXEDD_DRV = $D\fxdcsd3.m3 $D\fxdcsd5.m5 $D\fxdkbdw.m3 $D\fxdkbd5.m5 \
	$D\fxdsyd3.m3 $D\fxdsyd5.m5
FIXED5_DRV = lib\fx_csd5.obj lib\fx_kbd5.obj lib\fx_syd5.obj


!IF "$(DEBUG)" == "DEBUG"
FIXED3_DRV = lib\fx_csd3.obj lib\dfx_kbd3.obj lib\fx_syd3.obj
!ENDIF

!IF "$(DEBUG)" == "DEBUG"
FIXEDD_DRV = $D\fxdcsd3.m3 $D\fxdcsd5.m5 $D\dfxdkbdw.m3 $D\dfxdkbd5.m5 \
	$D\fxdsyd3.m3 $D\fxdsyd5.m5
!ENDIF


!IF "$(FINAL)" == "cboxw.lib"
FIXED3_DRV = lib\fx_csd3.obj lib\fx_kbdw.obj lib\fx_syd3.obj
!ENDIF

!IF "$(FINAL)" == "dcboxw.lib"
FIXED3_DRV = lib\fx_csd3.obj lib\dfx_kbdw.obj lib\fx_syd3.obj
!ENDIF

!IF "$(FINAL)" == "dcowsqc.lib"
FIXED3_DRV = lib\fx_twin.obj lib\dfx_kbd3.obj lib\fx_syd3.obj
!ENDIF

!IF "$(FINAL)" == "cowsqc.lib"
FIXED3_DRV = lib\fx_twin.obj lib\fx_kbd3.obj lib\fx_syd3.obj
!ENDIF

!IF "$(FINAL)" == "dcowqb.lib"
FIXED3_DRV = lib\fx_twin.obj lib\dfx_kbd3.obj lib\fx_syd3.obj
!ENDIF

!IF "$(FINAL)" == "cowqb.lib"
FIXED3_DRV = lib\fx_twin.obj lib\fx_kbd3.obj lib\fx_syd3.obj
!ENDIF

!IF "$(FINAL)" == "dcowlqb.lib"
FIXED3_DRV = lib\fx_twin.obj lib\dfx_kbd3.obj lib\fx_syd3.obj
!ENDIF

!IF "$(FINAL)" == "cowlqb.lib"
FIXED3_DRV = lib\fx_twin.obj lib\fx_kbd3.obj lib\fx_syd3.obj
!ENDIF

!IF "$(FINAL)" == "dkowqb.lib"
FIXED3_DRV = lib\fx_twink.obj lib\dfx_kbdk.obj lib\fx_syd3.obj
!ENDIF

!IF "$(FINAL)" == "kowqb.lib"
FIXED3_DRV = lib\fx_twink.obj lib\fx_kbdk.obj lib\fx_syd3.obj
!ENDIF

!IF "$(KERNEL)" == "kernel.lib"
ALL_LIB =	$D\kernel.lib	$D\user.lib	$(FIXED3_DRV)	dstamp.obj
!ENDIF

!IF "$(KERNEL)" == "kerneld.lib"
ALL_LIB =	$D\kerneld.lib	$D\user.lib	$(FIXEDD_DRV)	dstampd.obj
!ENDIF

!IF "$(KERNEL)" == "kernel3.lib"
ALL_LIB =	$D\kernel3.lib	$D\user.lib	$(FIXED3_DRV)	dstamp3.obj
!ENDIF

!IF "$(KERNEL)" == "kernel5.lib"
ALL_LIB =	$D\kernel5.lib	$D\user.lib	$(FIXED5_DRV)	dstamp5.obj
!ENDIF

!IF "$(KERNEL)" == "win.lib"
ALL_LIB =	$D\win.lib	$D\user.lib
!ENDIF

$(FINAL): nullib.lib $(ALL_LIB) $(EXTRA_LIB)
	$(TL)\lib @<<
nullib
$(ALL_LIB) $(EXTRA_LIB)
$D\final.map
$(FINAL)
<<

#############################################################################
# Version stamps

dstamp.obj: $(COWSLIB) stamp.asm
	type stamp.asm					> dstamp.asm
	tools\echotime DB 'CW (/t)'			>> dstamp.asm
	type stamp2.asm					>> dstamp.asm
	$(TL)\masm dstamp.asm;

dstampd.obj: $(COWDLIB) stamp.asm
	type stamp.asm					> dstampd.asm
	tools\echotime DB 'CW (/t)'			>> dstampd.asm
	type stamp2.asm					>> dstampd.asm
	$(TL)\masm dstampd.asm;

dstamp3.obj: $(COW3LIB) stamp.asm
	type stamp.asm					> dstamp3.asm
	tools\echotime DB 'CW 3 (/t)'			>> dstamp3.asm
	type stamp2.asm					>> dstamp3.asm
	$(TL)\masm dstamp3.asm;

dstamp5.obj: $(COW5LIB) stamp.asm
	type stamp.asm					> dstamp5.asm
	tools\echotime DB 'OS/2 CW (/t)'		>> dstamp5.asm
	type stamp2.asm					>> dstamp5.asm
	$(TL)\masm dstamp5.asm;

#############################################################################
#############################################################################
# Misc:

clean:
	echo : Not yet defined

depfile: makefile
	echo ##### Dependency File #####			     > depfile
	tools\mkdep -s .obj -P $$D\ -I inc -I loader loader\*.asm          >> depfile
	tools\mkdep -s .obj -P $$D\ -I inc -I kernel kernel\*.asm kernel\*.c >> depfile
	tools\mkdep -s .obj -P $$D\ -I inc -I user user\*.asm user\*.c     >> depfile
	tools\mkdep -s .obj -P $$D\ -I inc -I cbox cbox\*.asm cbox\*.c     >> depfile

#############################################################################
# LATER:
#
## - fixstub utility -- move to tools
#fixstub.exe: fixstub.c
#	$(CC) fixstub.c
#
#############################################################################
