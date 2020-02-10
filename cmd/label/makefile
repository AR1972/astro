#************************** makefile for cmd\label ***************************

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\cmd\label

msg	=..\..\messages
inc     =..\..\inc
extasw  =-I$(inc) -D$(COUNTRY)
extcsw  =-I$(inc) -I$(MSG_DIR)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =label.exe
cpr     =label.ex_

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


label.obj:    label.c			\
	      label.h			\
	      $(MSG_DIR)\messages.h		\
	      makefile

$(dest):      label.obj
                link $(exelink) label.obj;
