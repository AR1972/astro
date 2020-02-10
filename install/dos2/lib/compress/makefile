#####################################################
# build Lempel-Ziv compression/decompression modules
#####################################################

###############
# Memory models
###############

BUILD_S=1	# Small.
BUILD_M=1	# Medium: Currently, only used for lzcopy.lib.
#BUILD_L=1	# Large: Currently, only used for lzcopy.lib.

!ifdef DEBUG

MASMSWITCHDOS  = -Mx -t -Zi -Zd
CLSWITCHDOS    = -W2 -Zipd -c -Od -Gs
CLSWITCHWIN    = -W2 -Zipd -c -u -Gsw -Od -D LZDLL
LINKSWITCHDOS  = /NOD /MAP /CO /LINENUMBERS
LINKSWITCHWIN  = /NOD /MAP /CO /LINENUMBERS /ALIGN:16 

!else

MASMSWITCHDOS  = -Mx -t
CLSWITCHDOS    = -W2 -Zdp -c -Ot -Gs
CLSWITCHWIN    = -W2  -Zdp -c -u -Gsw -Oas -D LZDLL
LINKSWITCHDOS  = /NOD /MAP /LINENUMBERS
LINKSWITCHWIN  = /NOD /MAP /LINENUMBERS /ALIGN:16

!endif


##################
# inference rules
##################

.c.obj:
!ifdef BUILD_S
	cl $(CLSWITCHDOS) -AS -Fo$*.obj $*.c
!endif
!ifdef BUILD_M
	cl $(CLSWITCHDOS) -AM -Fomedium\$*.obj $*.c
!endif
!ifdef BUILD_L
	cl $(CLSWITCHDOS) -AL -DMLARGE -Folarge\$*.obj $*.c
!endif

.asm.obj:
!ifdef BUILD_S
	masm $(MASMSWITCHDOS) -DmemS=1 $*.asm,$*.obj;
!endif
!ifdef BUILD_M
	masm $(MASMSWITCHDOS) -DmemM=1 $*.asm,medium\$*.obj;
!endif
!ifdef BUILD_L
	masm $(MASMSWITCHDOS) -DmemL=1 $*.asm,large\$*.obj;
!endif

#################
# target modules
#################

target: wlzcopy


###############################
# build Lempel-Ziv Win library
###############################


wlzcopy: wlzcopy.lib

winlzexp.obj:
!ifdef BUILD_S
   cl $(CLSWITCHWIN) -Asnw -Fowinlzexp.obj doslzexp.c
!endif
!ifdef BUILD_M
   cl $(CLSWITCHWIN) -AMnw -Fomedium\winlzexp.obj doslzexp.c
!endif
!ifdef BUILD_L
   cl $(CLSWITCHWIN) -Alnw -Folarge\winlzexp.obj doslzexp.c
!endif


wlzcopy.obj:
!ifdef BUILD_S
   cl $(CLSWITCHWIN) -Asnw -Fowlzcopy.obj lzcopy.c
!endif
!ifdef BUILD_M
   cl $(CLSWITCHWIN) -AMnw -Fomedium\wlzcopy.obj lzcopy.c
!endif
!ifdef BUILD_L
   cl $(CLSWITCHWIN) -Alnw -Folarge\wlzcopy.obj lzcopy.c
!endif


wlzcopy.lib: winlzexp.obj wlzcopy.obj
!ifdef BUILD_S
   del wlzcopy.lib
   lib wlzcopy.lib + winlzexp.obj + wlzcopy.obj;
!endif
!ifdef BUILD_M
	cd medium
	del wlzcopy.lib
	lib wlzcopy.lib + winlzexp.obj + wlzcopy.obj;
	cd ..
!endif
!ifdef BUILD_L
	cd large
	del wlzcopy.lib
   lib wlzcopy.lib + winlzexp.obj + wlzcopy.obj;
	cd ..
!endif


###############
# housekeeping
###############

depend:

doslzexp.obj winlzexp.obj: doslzexp.c ..\common\sulib.h doslzexp.h doslzexp.pro


lzcopy.obj wlzcopy.obj: lzcopy.c ..\common\sulib.h doslzexp.h doslzexp.pro lzcopy.h \
	lzcopy.pro

# END Dependencies 

