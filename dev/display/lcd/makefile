#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */
#************************ Makefile for Display\LCD ************************

inc	=..\..\..\inc
extasw  =-I$(inc) -D$(COUNTRY)
bins    =..\..\..\binaries
cprs    =..\..\..\compress
dest    =lcd.cpi
cpr     =lcd.cp_

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

865-cpi.obj:  865-cpi.asm

860-cpi.obj:  860-cpi.asm

437-cpi.obj:  437-cpi.asm

863-cpi.obj:  863-cpi.asm

850-cpi.obj:  850-cpi.asm

copyrigh.obj: copyrigh.asm $(inc)\copyrigh.inc

font-r3.obj:  font-r3.asm

$(dest):  font-r3.obj  437-cpi.obj  850-cpi.obj  860-cpi.obj  \
	  863-cpi.obj  865-cpi.obj  copyrigh.obj lcd.lnk makefile
		link @lcd.lnk
                reloc font-r3.exe $(dest)
		del font-r3.exe

