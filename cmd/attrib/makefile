#************************** makefile for cmd\attrib ***************************

msg     =..\..\messages
inc     =..\..\inc
hinc    =..\..\h
extasw  =-i$(inc) -D$(COUNTRY)
extcsw  =-I$(hinc)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =attrib.exe
pkl     =attrib.pkl


#
####################### dependencies begin here. #########################
#


!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(cpr)

$(bins)\$(dest): $(pkl)
	    copy $(pkl) $(bins)\$(dest)

$(cprs)\$(cpr):  $(pkl)
	    copy $(pkl) $(cprs)\$(dest)

$(pkl):       $(dest)
	    pklite -o $(dest) $(pkl)

!ENDIF

attrib.ctl: attrib.skl \
	$(msg)\$(COUNTRY).msg \
	makefile

_parse.obj: _parse.asm \
	$(inc)\version.inc \
	$(inc)\parse.asm \
	$(inc)\psdata.inc

_msgret.obj: _msgret.asm \
	attrib.ctl        \
	$(inc)\msgserv.asm \
	$(inc)\sysmsg.inc  \
	$(inc)\copyrigh.inc \
	attrib.skl

attriba.obj: attriba.asm

attrib.obj: attrib.c \
	attrib.h \
	parse.h \
	msgret.h \
	proto.h \
	makefile

$(dest):    attrib.obj \
	    attriba.obj \
	    _parse.obj \
	    _msgret.obj \
	    attrib.lnk
      link $(exelink) @attrib.lnk
