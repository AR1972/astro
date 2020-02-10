#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */

#************************** makefile for cmd\restore ***************************

msg	=..\..\messages
inc	=..\..\inc
hinc    =..\..\h
doslib  =..\..\lib
extasw  =-I$(inc) -D$(COUNTRY)
extcsw  =-I$(hinc)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =restore.exe
cpr     =restore.ex_

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


_parse.obj :  _parse.asm                \
              makefile                  \
              $(inc)\version.inc        \
              $(inc)\parse.asm          \
              $(inc)\psdata.inc

restore.ctl:  restore.skl               \
              makefile                  \
              $(msg)\$(COUNTRY).msg

_msgret.obj:  _msgret.asm               \
              makefile                  \
              restore.ctl               \
              $(inc)\msgserv.asm        \
              $(inc)\sysmsg.inc

restore.obj:  restore.c                 \
              rt.h                      \
              rt1.h                     \
              rt2.h                     \
              restpars.h                \
              makefile                  \
              $(hinc)\comsub.h          \
              $(hinc)\doscalls.h        \
              $(hinc)\error.h

rtt1.obj:     rtt1.c                    \
              rt.h                      \
              rt1.h                     \
              rt2.h                     \
              restpars.h                \
              makefile                  \
              $(hinc)\comsub.h          \
              $(hinc)\doscalls.h        \
              $(hinc)\error.h

rtt3.obj:     rtt3.c                    \
              rt.h                      \
              rt1.h                     \
              rt2.h                     \
              restpars.h                \
              makefile                  \
              $(hinc)\comsub.h          \
              $(hinc)\doscalls.h        \
              $(hinc)\error.h

rtdo.obj:     rtdo.c                    \
              rt.h                      \
              rt1.h                     \
              rt2.h                     \
              restpars.h                \
              makefile                  \
              $(hinc)\comsub.h          \
              $(hinc)\doscalls.h        \
              $(hinc)\error.h

rtdo1.obj:    rtdo1.c                   \
              rt.h                      \
              rt1.h                     \
              rt2.h                     \
              restpars.h                \
              makefile                  \
              $(hinc)\comsub.h          \
              $(hinc)\doscalls.h        \
              $(hinc)\error.h

rtnew.obj:    rtnew.c                   \
              rt.h                      \
              rt1.h                     \
              rt2.h                     \
              restpars.h                \
              makefile                  \
              $(hinc)\comsub.h          \
              $(hinc)\doscalls.h        \
              $(hinc)\error.h

rtnew1.obj:   rtnew1.c                  \
              rt.h                      \
              rt1.h                     \
              rt2.h                     \
              restpars.h                \
              makefile                  \
              $(hinc)\comsub.h          \
              $(hinc)\doscalls.h        \
              $(hinc)\error.h

rtold.obj:    rtold.c                   \
              rt.h                      \
              rt1.h                     \
              rt2.h                     \
              restpars.h                \
              makefile                  \
              $(hinc)\comsub.h          \
              $(hinc)\doscalls.h        \
              $(hinc)\error.h

rtold1.obj:   rtold1.c                  \
              rt.h                      \
              rt1.h                     \
              rt2.h                     \
              restpars.h                \
              makefile                  \
              $(hinc)\comsub.h          \
              $(hinc)\doscalls.h        \
              $(hinc)\error.h

rtfile.obj:   rtfile.c                  \
              rt.h                      \
              rt1.h                     \
              rt2.h                     \
              restpars.h                \
              makefile                  \
              $(hinc)\comsub.h          \
              $(hinc)\doscalls.h        \
              $(hinc)\error.h

rtfile1.obj:  rtfile1.c                 \
              rt.h                      \
              rt1.h                     \
              rt2.h                     \
              restpars.h                \
              makefile                  \
              $(hinc)\comsub.h          \
              $(hinc)\doscalls.h        \
              $(hinc)\error.h

restpars.obj: restpars.c                \
              rt.h                      \
              rt1.h                     \
              rt2.h                     \
              restpars.h                \
              makefile                  \
              $(hinc)\comsub.h          \
              $(hinc)\doscalls.h        \
              $(hinc)\error.h

$(dest):      restore.obj               \
              restpars.obj              \
              rtdo.obj                  \
              rtdo1.obj                 \
              rtfile.obj                \
              rtfile1.obj               \
              rtnew.obj                 \
              rtnew1.obj                \
              rtold.obj                 \
              rtold1.obj                \
              rtt1.obj                  \
              rtt3.obj                  \
              restore.lnk               \
              _msgret.obj               \
              _parse.obj                \
              $(doslib)\comsubs.lib     \
              $(doslib)\mapper.lib
              link $(exelink) @restore.lnk
