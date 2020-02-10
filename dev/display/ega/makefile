#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */
#*********************** Makefile for Display\EGA ************************

inc     =..\..\..\inc
extasw  =-I$(inc) -D$(COUNTRY)
bins    =..\..\..\binaries
cprs    =..\..\..\compress
dest    =ega.cpi
cpr     =ega.cp_

#
####################### dependencies begin here. #########################
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


cpi-head.obj: cpi-head.asm \
	      makefile

437-cpi.obj: 437-cpi.asm \
	     437-8x16.asm \
	     437-8x14.asm \
	     makefile      \
	     437-8x8.asm

850-cpi.obj: 850-cpi.asm \
	     850-8x16.asm \
	     850-8x14.asm \
	     makefile      \
	     850-8x8.asm

852-cpi.obj: 852-cpi.asm \
	     852-8x16.asm \
	     852-8x14.asm \
	     makefile      \
	     852-8x8.asm

860-cpi.obj: 860-cpi.asm \
	     860-8x16.asm \
	     860-8x14.asm \
	     makefile      \
	     860-8x8.asm

863-cpi.obj: 863-cpi.asm \
	     863-8x16.asm \
	     863-8x14.asm \
	     makefile      \
	     863-8x8.asm

865-cpi.obj: 865-cpi.asm \
	     865-8x16.asm \
	     865-8x14.asm \
	     makefile      \
	     865-8x8.asm

copyrigh.obj: copyrigh.asm  $(inc)\copyrigh.inc \
	      makefile

$(dest): cpi-head.obj 437-cpi.obj 850-cpi.obj 852-cpi.obj 860-cpi.obj \
	 863-cpi.obj 865-cpi.obj copyrigh.obj makefile ega.lnk \
	 makefile
	       link @ega.lnk
	       reloc cpi-head.exe ega.cpi
	       del   cpi-head.exe
