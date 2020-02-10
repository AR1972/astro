#************************* makefile for cmd\help ************************

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\cmd\help

inc	=..\..\inc
hinc    =..\..\h
extasw  =-I$(inc) -D$(COUNTRY)
extcsw  =-I$(hinc) -I$(MSG_DIR)

dest	=fasthelp.exe
cpr	=fasthelp.ex_
bins    =..\..\binaries
cprs    =..\..\compress

hlp     =$(MSG_DIR)\doshelp.hlp
hlpcpr  =doshelp.hl_

basehlp =$(MSG_DIR)\doshelp.bse
basecpr =doshelp.bs_



#
#######################	dependencies begin here. #########################
#

!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(cpr) $(bins)\$(hlp) $(cprs)\$(hlpcpr) $(bins)\$(basehlp) $(cprs)\$(basecpr)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)

$(cprs)\$(cpr):  $(cpr)
            copy $(cpr) $(cprs)

$(cpr):     $(dest)
            $(compress) $(dest) $(cpr)

$(bins)\$(hlp): $(hlp)
            copy $(hlp) $(bins)

$(cprs)\$(hlpcpr):  $(hlpcpr)
            copy $(hlpcpr) $(cprs)

$(hlpcpr):  $(hlp)
            $(compress) $(hlp) $(hlpcpr)


$(bins)\$(basehlp): $(basehlp)
            copy $(basehlp) $(bins)

$(cprs)\$(basecpr):  $(basecpr)
            copy $(basecpr) $(cprs)

$(basecpr):  $(basehlp)
            $(compress) $(basehlp) $(basecpr)

!ENDIF



help.obj:     help.c                    \
              $(MSG_DIR)\message.h                 \
              proto.h                   \
              $(hinc)\dos.h             \
              $(hinc)\stdlib.h          \
              $(hinc)\stdio.h

helper.obj:   helper.asm                \
              $(inc)\rombios.inc        \
              $(inc)\version.inc

$(dest):      help.obj                  \
              helper.obj
              link help+helper,$(dest);


