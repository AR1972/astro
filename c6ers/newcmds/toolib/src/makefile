#
#                       SLAVE MAKEFILE FOR TOOLIB\SRC
#
#   Targets:
#	
#	all		- make all libraries
#	real		- make all real libraries
#	stoolsr		- small model real mode
#	mtoolsr		- medium model real mode
#	ltoolsr		- large model real mode
#
#   Build .OBJ's into:
#
#	    ..\sobjr	- Real Mode small mode
#	    ..\mobjr	- Real Mode medium model
#	    ..\lobjr	- Real Mode large model
#
#

!INCLUDE ..\h\librules.mak


CFLAGS=  $(CFLAGS) /I../h

MULTI=	objdir\curdir.obj	objdir\delnode.obj	objdir\enfile.obj   \
	objdir\error.obj						    \
	objdir\exetype.obj	objdir\fappend.obj	objdir\fcopy.obj    \
	objdir\fdelete.obj	objdir\fexpunge.obj	objdir\fgetl.obj    \
	objdir\find.obj		objdir\fmove.obj	objdir\forfile.obj  \
	objdir\forsemi.obj	objdir\fputl.obj	objdir\freespac.obj \
	objdir\getattr.obj	objdir\init.obj		objdir\ctime2l.obj  \
	objdir\match.obj	objdir\mgetl.obj	objdir\ntoi.obj     \
	objdir\pathcat.obj	objdir\pathchr.obj	objdir\pathopen.obj \
	objdir\pname.obj	objdir\setattr.obj	objdir\strcmps.obj  \
	objdir\swchng.obj	objdir\update.obj	objdir\root.obj	    \
	objdir\vector.obj	objdir\zalloc.obj	objdir\string.obj   \
	objdir\xtab.obj 	objdir\stripfx.obj	objdir\strpfx.obj   \
	objdir\mkpath.obj

DUAL=	objdir\blank.obj	objdir\cursor.obj	objdir\getl.obj	    \
	objdir\kbhit.obj	objdir\maxmin.obj	objdir\scrolldn.obj \
	objdir\scrollup.obj	objdir\untab.obj	objdir\move.obj     \
	objdir\strpre.obj						    \
	objdir\args.obj		objdir\envorini.obj			    \
	objdir\heapchk.obj	objdir\heapdump.obj	objdir\mapenv.obj   \
	objdir\mapstr.obj	objdir\rspawn.obj

REAL=	objdir\connect.obj	objdir\int.obj		objdir\msspawn.obj  \
	objdir\msexec.obj	objdir\zsleep.obj

REALOBJS= $(MULTI) $(DUAL) $(REAL)


SREALOBJS= $(REALOBJS:objdir=..\sobjr)

CREALOBJS= $(REALOBJS:objdir=..\cobjr)

MREALOBJS= $(REALOBJS:objdir=..\mobjr)

LREALOBJS= $(REALOBJS:objdir=..\lobjr)

all: real

real: stoolsr ctoolsr mtoolsr ltoolsr

stoolsr:  $(SREALOBJS)
	if exist ..\lib\stoolsr.lib del ..\lib\stoolsr.lib
	$(LIBEXE) ..\lib\stoolsr.lib@<<
$(SREALOBJS:..=+..)
..\lib\stoolsr.map;
<<


ctoolsr:  $(CREALOBJS)
	if exist ..\lib\ctoolsr.lib del ..\lib\ctoolsr.lib
	$(LIBEXE) ..\lib\ctoolsr.lib@<<
$(CREALOBJS:..=+..)
..\lib\ctoolsr.map;
<<


mtoolsr: $(MREALOBJS)
	if exist ..\lib\mtoolsr.lib del ..\lib\mtoolsr.lib
	$(LIBEXE) ..\lib\mtoolsr.lib@<<
$(MREALOBJS:..=+..)
..\lib\mtoolsr.map;
<<


ltoolsr: $(LREALOBJS)
	if exist ..\lib\ltoolsr.lib del ..\lib\ltoolsr.lib
	$(LIBEXE) ..\lib\ltoolsr.lib@<<
$(LREALOBJS:..=+..)
..\lib\ltoolsr.map;
<<
