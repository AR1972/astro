#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */
#********************** Makefile for Printer\5202.CPI **********************

inc     =..\..\..\inc
extasw  =-I$(inc)
bins    =..\..\..\binaries
cprs    =..\..\..\compress
dest    =5202.cpi
cpr     =5202.cp_

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

5202.obj:  5202.asm  $(inc)\copyrigh.inc makefile


$(dest):  5202.obj
          link 5202;
          reloc 5202 $(dest)
          del 5202.exe
