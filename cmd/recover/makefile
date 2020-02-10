#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */

#************************** makefile for cmd\... ***************************

msg	=..\..\messages
inc     =..\..\inc
extasw  =-I$(inc)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =recover.exe
cpr     =recover.ex_

#
#######################	dependencies begin here. #########################
#

!IF	"$(DEBUG)" == "soft_ice" || "$(debug)" == "soft_ice" || \
		"$(DEBUG)" == "SOFT_ICE" || "$(debug)" == "SOFT_ICE"
extasw  = -I$(inc) -Zd
exelink = /MAP /LI
!ENDIF

!IF	"$(DEBUG)" == "codeview" || "$(debug)" == "codeview" || \
		"$(DEBUG)" == "CODEVIEW" || "$(debug)" == "CODEVIEW"
extasw  = -I$(inc) -Zi
exelink =/NOD /MAP /CO
!ENDIF

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


recover.ctl:  recover.skl               \
              $(msg)\$(COUNTRY).msg     \
              makefile

recover.obj:  recover.asm               \
              recequ.inc                \
              recmacro.inc              \
              recseg.inc                \
              recdata.inc               \
              recparse.inc              \
              recmsg.inc                \
              recover.ctl               \
              $(inc)\dossym.inc         \
              $(inc)\syscall.inc        \
              $(inc)\sysmsg.inc         \
              $(inc)\parse.asm          \
              makefile

recinit.obj:  recinit.asm               \
              recequ.inc                \
              recmacro.inc              \
              recseg.inc                \
              recdata.inc               \
              recparse.inc              \
              recmsg.inc                \
              recover.ctl               \
              $(inc)\dossym.inc         \
              $(inc)\syscall.inc        \
              $(inc)\sysmsg.inc         \
              $(inc)\parse.asm          \
              makefile


recproc.obj:  recproc.asm               \
              recequ.inc                \
              recmacro.inc              \
              recseg.inc                \
              recdata.inc               \
              recparse.inc              \
              recmsg.inc                \
              recover.ctl               \
              $(inc)\dossym.inc         \
              $(inc)\syscall.inc        \
              $(inc)\sysmsg.inc         \
              $(inc)\parse.asm          \
              makefile

recdisp.obj:  recdisp.asm               \
              recequ.inc                \
              recmacro.inc              \
              recseg.inc                \
              recdata.inc               \
              recparse.inc              \
              recmsg.inc                \
              recover.ctl               \
              $(inc)\dossym.inc         \
              $(inc)\syscall.inc        \
              $(inc)\sysmsg.inc         \
              $(inc)\parse.asm          \
              makefile


$(dest):      recover.obj               \
              recproc.obj               \
              recinit.obj               \
              recdisp.obj               \
              recover.lnk               \
              makefile
              link $(exelink) @recover.lnk
