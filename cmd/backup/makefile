#************************** makefile for cmd\backup ***************************

msg	=..\..\messages
inc	=..\..\inc
hinc    =..\..\h
extasw	=-I$(inc) -D$(COUNTRY)
extcsw  =-I$(hinc)
doslib  =..\..\lib
bins    =..\..\binaries
cprs    =..\..\compress
dest    =backup.exe
cpr     =backup.ex_


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

_parse.obj:   _parse.asm                \
              $(inc)\parse.asm          \
              $(inc)\psdata.inc         \
              makefile

backup.ctl:   backup.skl                \
              $(msg)\$(COUNTRY).MSG

_msgret.obj:  _msgret.asm               \
              backup.ctl                \
              $(inc)\msgserv.asm        \
              $(inc)\sysmsg.inc         \
              $(inc)\versiona.inc       \
              $(inc)\copyrigh.inc       \
              makefile

backup.obj:   backup.c                  \
              backpars.h                \
              backup.h                  \
              makefile                  \
              $(hinc)\doscalls.h

$(dest):      backup.obj                \
              _parse.obj                \
              _msgret.obj               \
              $(doslib)\mapper.lib      \
              $(doslib)\comsubs.lib
              link $(exelink) @backup.lnk
