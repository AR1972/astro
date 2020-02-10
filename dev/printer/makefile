#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */
#************************* Makefile for Printer ***************************

msg     =..\..\messages
inc     =..\..\inc
extasw  =-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =printer.sys
cpr     =printer.sy_

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


printer.ctl:	printer.skl $(msg)\$(COUNTRY).msg

cpspm10.obj:	cpspm10.asm cpspequ.inc

cpsfont3.obj:	cpsfont3.asm cpspequ.inc

prtint2f.obj:	prtint2f.asm cpspequ.inc $(inc)\struc.inc

cpspi07.obj:	cpspi07.asm cpspequ.inc \
		$(inc)\sysmsg.inc printer.ctl \
		$(inc)\versiona.inc

parser.obj:     parser.asm               \
                cpspequ.inc              \
                $(inc)\version.inc       \
                $(inc)\parse.asm         \
		$(inc)\struc.inc

$(dest):    cpspm10.obj cpsfont3.obj parser.obj cpspi07.obj prtint2f.obj
	link	@printer.lnk
        reloc printer.exe printer.sys 0
	del	printer.exe

cpi:
        cd 4201
        $(MAKE)
        cd ..\4208
        $(MAKE)
        cd ..\5202
        $(MAKE)
        cd ..
