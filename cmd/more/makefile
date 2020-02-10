#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */

#************************ makefile for cmd\more *************************

msg     =..\..\messages
inc     =..\..\inc
extasw  =-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =more.com
pkl     =more.pkl

#
####################### dependencies begin here. #########################
#

!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(cpr)

$(bins)\$(dest): $(pkl)
	    copy $(pkl) $(bins)\$(dest)

$(cprs)\$(cpr):  $(pkl)
	    copy $(pkl) $(cprs)\$(dest)

$(pkl):       $(dest)
	    pklite -o $(dest) $(pkl)

!ENDIF


more.ctl:     more.skl                  \
	      $(msg)\$(COUNTRY).msg     \
	      makefile

more.obj:     more.asm                  \
	      more.inc                  \
	      makefile                  \
	      more.ctl                  \
	      $(inc)\syscall.inc        \
	      $(inc)\struc.inc          \
	      $(inc)\msgserv.asm        \
	      $(inc)\sysmsg.inc         \
	      $(inc)\versiona.inc       \
	      $(inc)\copyrigh.inc       \
	      $(inc)\rombios.inc

more.exc:     more.obj                  \
	      more.lnk
	      link @more.lnk

$(dest):      more.exc
