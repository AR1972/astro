#************************** makefile for cmd\diskcopy ***************************

msg	=..\..\messages
inc	=..\..\inc
extasw	=-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =diskcopy.com
cpr     =diskcopy.co_

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

!IFNDEF  IBM

diskcopy.skl:   message.skl               \
                ibm.skl                   \
                $(msg)\$(COUNTRY).msg
          copy /b message.skl+ibm.skl diskcopy.skl
          echo :end >> diskcopy.skl

!ELSE

diskcopy.skl: message.skl               \
              $(msg)\$(COUNTRY).msg
          copy /b message.skl diskcopy.skl
          echo :end >> diskcopy.skl

!ENDIF							



diskcopy.ctl: diskcopy.skl              \
              $(msg)\$(COUNTRY).msg     \
              makefile

diskcopy.obj: diskcopy.asm              \
              makefile                  \
              dcpymacr.inc              \
              diskcopy.equ              \
              $(inc)\bootform.inc       \
              $(inc)\version.inc

copyinit.obj: copyinit.asm              \
              makefile                  \
              dcpymacr.inc              \
              diskcopy.equ

dcopyp.obj:   dcopyp.asm                \
              makefile                  \
              $(inc)\parse.asm

dcopysm.obj:  dcopysm.asm               \
              makefile                  \
              $(inc)\sysmsg.inc         \
              $(inc)\msghan.inc         \
              $(inc)\versiona.inc       \
              diskcopy.ctl              \
              dcopyms.inc               \
              $(inc)\copyrigh.inc

dcopypar.obj: dcopypar.asm              \
              makefile

diskcopy.exc: diskcopy.obj              \
              makefile                  \
              diskcopy.lnk              \
              dcopysm.obj               \
              dcopyp.obj                \
              dcopypar.obj              \
              copyinit.obj
              link @diskcopy.lnk

$(dest):      diskcopy.exc
