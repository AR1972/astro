#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */

#************************ makefile for cmd\subst *************************

msg	=..\..\messages
inc	=..\..\inc
hinc    =..\..\h
extasw  =-I$(inc) -D$(COUNTRY)
extcsw  =-I$(hinc)
doslib  =..\..\lib
bins    =..\..\binaries
cprs    =..\..\compress
dest    =subst.exe
cpr     =subst.ex_

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


subst.ctl:    subst.skl                 \
              $(msg)\$(COUNTRY).msg     \
              makefile

_msgret.obj:  _msgret.asm               \
              makefile                  \
              $(inc)\msgserv.asm        \
              $(inc)\sysmsg.inc         \
              subst.ctl                 \
              subst.skl

_parse.obj:   _parse.asm                \
              makefile                  \
              $(inc)\version.inc        \
              $(inc)\parse.asm          \
              $(inc)\psdata.inc

subst.obj:    subst.c                   \
              makefile                  \
              substpar.h                \
              $(hinc)\cds.h             \
              $(hinc)\jointype.h        \
              $(hinc)\sysvar.h          \
              subst.h

$(dest):      subst.obj                 \
              $(doslib)\mapper.lib      \
              $(doslib)\master.lib      \
              $(doslib)\comsubs.lib     \
              _msgret.obj               \
              _parse.obj                \
              subst.lnk
              link $(exelink) @subst.lnk

