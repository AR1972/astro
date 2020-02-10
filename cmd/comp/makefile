#************************** makefile for cmd\comp ***************************

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\cmd\comp

msg	=..\..\messages
hinc    =..\..\h
extcsw  =-I$(hinc) -I$(MSG_DIR)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =comp.exe
cpr     =comp.ex_

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

comp.obj:     comp.c                    \
              makefile                  \
              comp.h                    \
              $(MSG_DIR)\messages.h     \
              $(hinc)\dos.h             \
              $(hinc)\fcntl.h           \
              $(hinc)\stdlib.h          \
              $(hinc)\stdio.h           \
              $(hinc)\malloc.h          \
              $(hinc)\ctype.h

$(dest):      comp.obj
              link $(exelink) comp;
