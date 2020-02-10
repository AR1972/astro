#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */

#************************** makefile for cmd\sort ***************************

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\cmd\sort

msg	=..\..\messages
inc     =..\..\inc
extasw  =-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =sort.exe
cpr     =sort.ex_

#
#######################	dependencies begin here. #########################
#

!IFNDEF       BUILDER

all: iclean $(dest)

!ELSE

all: iclean $(dest) $(bins)\$(dest) $(cprs)\$(cpr)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)

$(cprs)\$(cpr):  $(cpr)
            copy $(cpr) $(cprs)

$(cpr):       $(dest)
            $(compress) $(dest) $(cpr)

!ENDIF



sort.ctl: sort.skl              \
        $(msg)\$(COUNTRY).msg   \
        makefile

sort.obj: sort.asm              \
          makefile              \
          sort.ctl              \
          $(inc)\parse.asm      \
          $(inc)\psdata.inc     \
          $(inc)\syscall.inc    \
          $(inc)\msgserv.asm    \
          $(inc)\sysmsg.inc

sortmes.asm:
	  copy $(MSG_DIR)\sortmes.asm

sortmes.obj: sort.asm           \
	  makefile              \
          $(inc)\syscall.inc

$(dest):  sort.obj sortmes.obj
        link sort+sortmes,sort.exe;
        exefix sort.exe 1 1

#International Modifications
iclean:
	del sortmes.asm
	del sortmes.obj
