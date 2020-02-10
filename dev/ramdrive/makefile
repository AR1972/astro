#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */

# International Modifications:
MSG_DIR=$(LANG_SRC)\$(COUNTRY)\dev\ramdrive

inc     =../../inc
extasw  =-I$(inc)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =ramdrive.sys
cpr     =ramdrive.sy_

#
####################### Dependencies Begin Here ##########################
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




ramdrive.obj:  ramdrive.asm             \
               above.inc                \
               loadall.inc              \
               emm.inc                  \
               mi.inc                   \
               dirent.inc               \
               syscall.inc              \
               devsym.inc               \
               makefile

messages.asm:
	       copy $(MSG_DIR)\messages.asm

messages.obj:  messages.asm  		\
               makefile

ramdrive.exs:  ramdrive.obj             \
               messages.obj
               link @ramdrive.lnk

$(dest):       ramdrive.exs

#International Modifications
iclean:
	del messages.asm
	del messages.obj
