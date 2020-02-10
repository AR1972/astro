#************************** makefile for cmd\find ***************************

msg	=..\..\messages
inc     =..\..\inc
extasw	=-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =find.exe
cpr     =find.ex_

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

find.ctl: find.skl              \
          $(msg)\$(COUNTRY).msg \
          makefile

find.obj: find.asm              \
          makefile              \
          find.ctl              \
          find.inc              \
          $(inc)\version.inc    \
          $(inc)\parse.asm      \
          $(inc)\psdata.inc     \
          $(inc)\syscall.inc    \
          $(inc)\msgserv.asm    \
          $(inc)\sysmsg.inc

findmes.obj: findmes.asm

$(dest): find.obj findmes.obj find.lnk
        link $(exelink) @find.lnk
