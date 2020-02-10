#####################################################
# build Lempel-Ziv decompression modules
#####################################################

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\cmd\expand

!ifdef DEBUG

MASMSWITCHDOS  = -Mx -t -Zi -Zd
CLSWITCHDOS     = -W2 -AS -Zipd -c -Od -Gs -DDEBUG -I$(MSG_DIR)
LINKSWITCHDOS  = /NOD /MAP /CO /LINENUMBERS

!else

MASMSWITCHDOS  = -Mx -t
CLSWITCHDOS    = -W2 -AS -Zdp -c -Ot -Gs -I$(MSG_DIR) -I..\..\h
LINKSWITCHDOS  = /NOD

!endif


##################
# inference rules
##################

.c.obj:
   cl $(CLSWITCHDOS) $*.c

.asm.obj:
   masm $(MASMSWITCHDOS) $*.asm;


#################
# target modules
#################

bins    =..\..\binaries
cprs    =..\..\compress
dest    =expand.exe
pkl     =expand.pkl

!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(dest)

$(bins)\$(dest): $(pkl)
	    copy $(pkl) $(bins)\$(dest)

$(cprs)\$(dest): $(pkl)
	    copy $(pkl) $(cprs)\$(dest)

$(pkl): $(dest)
	    pklite -o $(dest) $(pkl)

!ENDIF


###########################################
# build Lempel-Ziv DOS command-line module
###########################################

expand.exe: expand.obj doslzexp.obj dosdir.obj ..\..\lib\sdecompr.lib
	link $(LINKSWITCHDOS) /NOE /ST:4096 expand.obj doslzexp.obj dosdir.obj,,, ..\..\lib\sdecompr slibce;

!ifdef DEBUG
   mapsym expand
   cvpack /p expand.exe
!endif



###############
# housekeeping
###############

depend:
   del makefile.old
   out makefile
	rename makefile makefile.old
	sed "/^# START Dependencies/,/^# END Dependencies/D" makefile.old > makefile
	echo # START Dependencies >> makefile
	includes -l -C=pro doslzexp.c expand.c dosdir.asm >> makefile
	echo # END Dependencies >> makefile
   del makefile.old



test: test.exe allow.exe deny.exe

test.obj: test.c

allow.obj: allow.c

deny.obj: deny.c

mediabit.obj: mediabit.asm

test.exe: test.obj mediabit.obj
   link $(LINKSWITCHDOS) /NOE test.obj mediabit.obj,,, slibce;

allow.exe: allow.obj setmb.obj
   link $(LINKSWITCHDOS) /NOE allow.obj setmb.obj,,, slibce;

deny.exe: deny.obj setmb.obj
   link $(LINKSWITCHDOS) /NOE deny.obj setmb.obj,,, slibce;



# START Dependencies 
doslzexp.obj: doslzexp.c doslzexp.h doslzexp.pro sulib.h

expand.obj: expand.c dosdir.pro doslzexp.h doslzexp.pro expand.h \
	expand.pro $(MSG_DIR)\expmsgs.h sulib.h

dosdir.obj: dosdir.asm cmacros.inc ioctl.inc

# END Dependencies 
