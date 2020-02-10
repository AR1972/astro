#******************************************************************************
#
#   MAKEFILE FOR MonoUMB
#
#==============================================================================

ROOT	= .
DOCFILE = VXDDOC.TXT

DEVICE	    = MonoUMB
VERSION     = 1.0

BLDALL=FALSE

include .\makeflag.mk
include .\win386.mk

#******************************************************************************
#	MONOUMB targets
#******************************************************************************
#MONOUMB: monoumb.386
bins    =..\..\binaries
cprs    =..\..\compress
dest    =monoumb.386
cpr     =monoumb.38_

#
####################### Dependencies Begin Here ##########################
#

all: $(dest) $(bins)\$(dest) $(cprs)\$(cpr)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)

$(cprs)\$(cpr):  $(cpr)
            copy $(cpr) $(cprs)

$(cpr):       $(dest)
            $(compress) $(dest) $(cpr)


#******************************************************************************
#		 DO NOT MODIFY THE MAKEFILE BEYOND THIS LINE
#******************************************************************************

# Dependencies follow
monoumb.obj monoumb.lst: monoumb.asm .\vdd2.inc .\vmm.inc

OBJS =  monoumb.obj 

allobjs: version $(OBJS) 

$(ROOT)\MonoUMB.386: MonoUMB.def version   $(OBJS)
        $(LINK) @<<
monoumb.obj
$(ROOT)\MonoUMB.386 $(LFLAGS)
MonoUMB.map

MonoUMB.def
<<
	.\addhdr /v:0300 $(ROOT)\MonoUMB.386
