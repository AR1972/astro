#*********************** Makefile for ansi.sys ****************************

msg	=..\..\messages
inc     =..\..\inc
extasw  =-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =ansi.sys
cpr     =ansi.sy_

#
####################### Dependencies Begin Here ##########################
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

ansi.ctl:     ansi.skl                  \
              makefile                  \
              $(msg)\$(COUNTRY).msg

ansi.obj:     ansi.asm                  \
              makefile                  \
              ansi.inc                  \
              $(inc)\vector.inc         \
              $(inc)\mult.inc           \
              $(inc)\struc.inc

ioctl.obj:    ioctl.asm                 \
              makefile                  \
              ansi.inc                  \
              $(inc)\struc.inc

ansiinit.obj: ansiinit.asm              \
              makefile                  \
              ansi.inc                  \
              ansivid.inc               \
              $(inc)\struc.inc

parser.obj:   parser.asm                \
              makefile                  \
              ansi.inc                  \
              $(inc)\sysmsg.inc         \
              $(inc)\version.inc        \
              $(inc)\parse.asm          \
              $(inc)\struc.inc          \
              ansi.ctl                  \
              $(inc)\versiona.inc

ansi.exs:     ansi.obj                  \
              ioctl.obj                 \
              ansiinit.obj              \
              parser.obj                \
              ansi.lnk                  \
              makefile
              link @ansi.lnk

$(dest):      ansi.exs
