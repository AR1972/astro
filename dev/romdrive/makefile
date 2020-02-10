#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */

# International Modifications:
MSG_DIR=$(LANG_SRC)\$(COUNTRY)\dev\romdrive

inc     =..\..\inc
bios	=..\..\bios
extasw  =-I$(inc) -DROMDRVALONE
bins    =..\..\binaries
cprs    =..\..\compress
dest    =romdrive.sys
cpr     =romdrive.sy_

#
####################### Dependencies Begin Here ##########################
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




romdrive.obj:  $(bios)\romdrive.asm             \
               $(inc)\devsym.inc               \
               makefile
	masm -I. -Mx -t -W2 $(extasw) $(bios)\romdrive.asm;

romrd.obj:  $(bios)\romrd.asm             \
               makefile
	masm -I. -Mx -t -W2 $(extasw) $(bios)\romrd.asm;

romdinit.obj:  $(bios)\romdinit.asm             \
               makefile
	masm -I. -Mx -t -W2 $(extasw) $(bios)\romdinit.asm;

romdmsg.obj:  $(MSG_DIR)\romdmsg.asm             \
               makefile
	masm -I$(bios) -Mx -t -W2 $(extasw) $(MSG_DIR)\romdmsg.asm;

romdrive.exs:  romdrive.obj             \
		romrd.obj		\
		romdinit.obj		\
               romdmsg.obj
               link /map romdrive romrd romdinit romdmsg,romdrive.exs;

$(dest):       romdrive.exs
