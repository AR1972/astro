#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */

#************************** makefile for cmd\xcopy ***************************

msg	=..\..\messages
inc     =..\..\inc
bins    =..\..\binaries
cprs    =..\..\compress
dest    =xcopy.exe
cpr     =xcopy.ex_


!ifdef DEBUG
extasw  =-I$(inc) -Zi -D$(COUNTRY)
exelink =/CO /MAP /LI
!else
extasw  =-I$(inc) -D$(COUNTRY)
!endif

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


xcopy.ctl: xcopy.skl \
   $(msg)\$(COUNTRY).msg

xcopy.obj: xcopy.asm \
   $(inc)\versiona.inc \
   $(inc)\copyrigh.inc \
   $(inc)\sysmsg.inc \
   $(inc)\msgserv.asm \
   dos.equ \
   xcopy.equ \
   xcopy.ctl \
   xmainmsg.equ

xcpyinit.obj: xcpyinit.asm \
   $(inc)\versiona.inc \
   xcopy.equ \
   dos.equ \
   xinitmsg.equ

xcopypar.obj: xcopypar.asm \
   $(inc)\version.inc      \
   $(inc)\psdata.inc       \
   $(inc)\parse.asm

$(dest): xcopy.obj \
   xcopy.lnk  \
   xcopypar.obj \
   xcpyinit.obj
        link $(exelink) @xcopy.lnk
!ifdef DEBUG
        cvpack -p $(dest)
        msym xcopy
!endif

