#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */
#************************* Makefile for Display ***************************

inc	=..\..\inc
msg	=..\..\messages
extasw  =-I$(inc) -D$(COUNTRY)

bins    =..\..\binaries
cprs    =..\..\compress
dest    =display.sys
cpr     =display.sy_

#
#######################	dependencies begin here. #########################
#

!IFNDEF       BUILDER

all: $(dest) cpi

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(cpr) cpi

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)

$(cprs)\$(cpr):  $(cpr)
            copy $(cpr) $(cprs)

$(cpr):       $(dest)
            $(compress) $(dest) $(cpr)

!ENDIF

display.ctl:    display.skl $(msg)\$(COUNTRY).msg

display.obj: display.asm makefile \
        macros.inc  \
        def-equ.inc \
        int2fcom.inc \
        int10com.inc \
        cps-func.inc \
        write.inc    \
        f-parser.inc

init.obj: init.asm makefile \
          macros.inc \
          def-equ.inc \
          $(inc)\sysmsg.inc \
          $(inc)\versiona.inc \
          display.ctl \
          tables.inc

parser.obj: parser.asm                   \
        makefile                         \
        def-equ.inc                      \
        $(inc)\struc.inc                 \
        $(inc)\version.inc               \
        $(inc)\parse.asm

$(dest): display.obj init.obj parser.obj display.lnk makefile zero.dat
        link @display.lnk
        reloc display.exe $(dest) 0
        del display.exe

cpi:
	cd ega
        $(MAKE)
        cd ..\lcd
#        $(MAKE)
	cd ..
