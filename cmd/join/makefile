#************************* makefile for cmd\join ************************

msg	=..\..\messages
inc	=..\..\inc
hinc    =..\..\h
extasw  =-I$(inc) -D$(COUNTRY)
extcsw  =-I$(hinc)
doslib  =..\..\lib
bins    =..\..\binaries
cprs    =..\..\compress
dest    =join.exe
cpr     =join.ex_

#
#######################	dependencies begin here. #########################
#

!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(cpr)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)

$(cprs)\$(cpr):  $(cpr)
            copy $(cpr) $(cprs)

$(cpr):       $(dest)
            $(compress) $(dest) $(cpr)

!ENDIF


join.ctl:     join.skl                  \
              $(msg)\$(COUNTRY).msg     \
              makefile

_msgret.obj:  _msgret.asm               \
              $(inc)\msgserv.asm        \
              $(inc)\sysmsg.inc         \
              join.ctl                  \
              join.skl

_parse.obj:   _parse.asm                \
              $(inc)\version.inc        \
              $(inc)\parse.asm          \
              $(inc)\psdata.inc

join.obj:     join.c                    \
              makefile                  \
              joinpars.h                \
              $(hinc)\cds.h             \
              $(hinc)\jointype.h        \
              $(hinc)\sysvar.h          \
              join.h

$(dest):      join.obj                  \
              $(doslib)\mapper.lib      \
              $(doslib)\master.lib      \
              $(doslib)\comsubs.lib     \
              _msgret.obj               \
              _parse.obj                \
              join.lnk
              link $(exelink) @join.lnk
