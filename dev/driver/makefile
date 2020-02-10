#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */
#************************ makefile for dev\driver ************************

msg	=..\..\messages
inc     =..\..\inc
extasw  =-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =driver.sys
cpr     =driver.sy_

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

driver.ctl:   driver.skl                \
              makefile                  \
              $(msg)\$(COUNTRY).msg

driver.obj:   driver.asm                \
              makefile                  \
              $(inc)\msbds.inc          \
              $(inc)\version.inc        \
              $(inc)\parse.asm          \
              $(inc)\psdata.inc         \
              driver.ctl                \
              $(inc)\sysmsg.inc         \
              $(inc)\msgserv.asm

driver.exs:   driver.obj                \
              makefile                  \
              driver.lnk
              link @driver.lnk

$(dest):      driver.exs
