#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */

#************************** makefile for cmd\nlsfunc ***************************

msg	=..\..\messages
inc     =..\..\inc
extasw  =-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =nlsfunc.exe
cpr     =nlsfunc.ex_

#
#######################	dependencies begin here. #########################
#

!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(dest)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)

$(cprs)\$(dest): $(dest)
            copy $(dest) $(cprs)


!ENDIF


nlsfunc.ctl:  nlsfunc.skl               \
              $(msg)\$(COUNTRY).msg     \
              makefile


nlsfunc.obj:  nlsfunc.asm               \
              funcdbcs.inc              \
              msg2nls.inc               \
              funcparm.inc              \
              nlsfunc.ctl               \
              doesmac.inc               \
              $(inc)\mult.inc           \
              $(inc)\msgserv.asm        \
              $(inc)\doscntry.inc       \
              $(inc)\devsym.inc         \
              $(inc)\struc.inc          \
              $(inc)\copyrigh.inc       \
              makefile

nlsparm.obj:  nlsparm.asm               \
              $(inc)\version.inc        \
              $(inc)\parse.asm          \
              $(inc)\psdata.inc         \
	      makefile



nlsfunc.exe:  nlsfunc.obj               \
              nlsparm.obj
              link $(exelink) nlsfunc+nlsparm;

