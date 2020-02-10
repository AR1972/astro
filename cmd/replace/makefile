#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */

#************************** makefile for cmd\replace ***************************

msg	=..\..\messages
inc     =..\..\inc
hinc    =..\..\h
doslib  =..\..\lib
extasw  =-I$(inc) -D$(COUNTRY)
extcsw  =-I$(hinc)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =replace.exe
cpr     =replace.ex_

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


replace.ctl:  replace.skl               \
              $(msg)\$(COUNTRY).msg     \
              makefile

_msgret.obj:  _msgret.asm               \
              $(inc)\msgserv.asm        \
              $(inc)\sysmsg.inc         \
              replace.ctl               \
              replace.skl               \
              makefile

_parse.obj:   _parse.asm                \
              $(inc)\version.inc        \
              $(inc)\parse.asm          \
              $(inc)\psdata.inc         \
              makefile

_replace.obj: _replace.asm              \
              makefile

replace.obj:  replace.c                 \
              makefile                  \
              replacep.h                \
              $(hinc)\comsub.h          \
              replace.h

$(dest):      replace.obj               \
              _replace.obj              \
              $(doslib)\mapper.lib      \
              $(doslib)\comsubs.lib     \
              _msgret.obj               \
              _parse.obj                \
              replace.lnk
              link $(exelink) @replace.lnk

