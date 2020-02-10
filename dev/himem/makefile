#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1988-1991
#; *                      All Rights Reserved.
#; */
#
#	Make file for HIMEM.SYS
#
# note:  all of our include files our local, so define next one as "."
inc	=.
#
#   Nmake XMSMMGR.EXE to make a debug TSR version used in Win 3.1 Setup.
#

# International Modifications:
MSG_DIR=$(LANG_SRC)\$(COUNTRY)\dev\himem

bins    =..\..\binaries
cprs    =..\..\compress
dest    =himem.sys
cpr     =himem.sy_

!ifdef DEBUG

asmsw   =-W1 -t -Zd -I$(MSG_DIR)
linksw  =/MAP /LI

!else

asmsw   =-W1 -t	-I$(MSG_DIR)
linksw  =/MAP /LI

!endif

#
# Inference Rules
#

.asm.obj:
   masm $(asmsw) $*;

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


himem.obj: himem.asm $(MSG_DIR)\himem.inc

himem1.obj: himem1.asm $(MSG_DIR)\himem.inc

himem2.obj: himem2.asm $(MSG_DIR)\himem.inc

messages.obj: $(MSG_DIR)\messages.asm $(MSG_DIR)\himem.inc
   masm $(asmsw) $(MSG_DIR)\$*;

himem4.obj: himem4.asm himem4a.asm himem4b.asm $(MSG_DIR)\himem.inc

himem5.obj: himem5.asm $(MSG_DIR)\himem.inc

dhimem.obj: himem.asm $(MSG_DIR)\himem.inc
    masm $(asmsw) -ddebug_tsr himem,dhimem;

dhimem1.obj: himem1.asm $(MSG_DIR)\himem.inc
    masm $(asmsw) -ddebug_tsr himem1,dhimem1;

dhimem2.obj: himem2.asm $(MSG_DIR)\himem.inc
    masm $(asmsw) -ddebug_tsr himem2,dhimem2;

dmessage.obj: $(MSG_DIR)\messages.asm $(MSG_DIR)\himem.inc
    masm $(asmsw) -ddebug_tsr messages,dmessage;

dhimem4.obj: himem4.asm $(MSG_DIR)\himem.inc
    masm $(asmsw) -ddebug_tsr himem4,dhimem4;

dhimem5.obj: himem5.asm $(MSG_DIR)\himem.inc
    masm $(asmsw) -ddebug_tsr himem5,dhimem5;

himem.exe: himem.obj himem1.obj himem2.obj messages.obj himem4.obj himem5.obj
    link $(linksw) himem+himem1+himem2+messages+himem4+himem5;

xmsmmgr.exe: dhimem.obj dhimem1.obj dhimem2.obj dmessage.obj dhimem4.obj dhimem5.obj
    link $(linksw) dhimem+dhimem1+dhimem2+dmessage+dhimem4+dhimem5,xmsmmgr;
!if "$(DEBUG)"=="s-ice"
    msym xmsmmgr
!endif

himem.bin: himem.exe
    reloc himem.exe himem.bin 0

$(dest): himem.bin
    stripdd himem.bin $(dest)

