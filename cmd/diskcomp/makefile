#************************** makefile for cmd\diskcomp ***************************

msg	=..\..\messages
inc	=..\..\inc
extasw	=-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =diskcomp.com
cpr     =diskcomp.co_

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


diskcomp.ctl: diskcomp.skl              \
              $(msg)\$(COUNTRY).msg     \
              makefile

diskcomp.obj: diskcomp.asm              \
              makefile                  \
              dcmpmacr.inc              \
              $(inc)\bootform.inc       \
              $(inc)\version.inc        \
              diskcomp.equ

compinit.obj: compinit.asm              \
              makefile                  \
              dcmpmacr.inc              \
              $(inc)\bootform.inc       \
              diskcomp.equ

dcompp.obj:   dcompp.asm                \
              makefile                  \
              $(inc)\parse.asm

dcompsm.obj:  dcompsm.asm               \
              makefile                  \
              $(inc)\sysmsg.inc         \
              $(inc)\msghan.inc         \
              $(inc)\versiona.inc       \
              diskcomp.ctl              \
              dcompms.inc               \
              $(inc)\copyrigh.inc

dcomppar.obj: dcomppar.asm              \
              makefile

diskcomp.exc: diskcomp.obj              \
              makefile                  \
              diskcomp.lnk              \
              dcompsm.obj               \
              dcompp.obj                \
              dcomppar.obj              \
              compinit.obj
              link @diskcomp.lnk

$(dest):      diskcomp.exc
