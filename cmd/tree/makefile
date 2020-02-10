#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */

#************************** makefile for cmd\tree ***************************

msg = ..\..\messages
inc = ..\..\inc
bins    =..\..\binaries
cprs    =..\..\compress

!IFDEF      DEBUG

extasw = -I$(inc) -Zi -DMAKE_EXE -D$(COUNTRY)
extcsw = -Zi
dest    =tree.exe

!ELSE

extasw = -I$(inc) -D$(COUNTRY)
dest    =tree.com
cpr     =tree.co_

!ENDIF


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



tree.ctl:     tree.skl                  \
              $(msg)\$(COUNTRY).msg

tree.obj:     tree.asm                  \
              treequ.inc

treesysp.obj: treesysp.asm              \
              $(inc)\version.inc        \
              $(inc)\psdata.inc         \
              $(inc)\parse.asm

treesysm.obj: treesysm.asm              \
              $(inc)\copyrigh.inc       \
              $(inc)\versiona.inc       \
              $(inc)\sysmsg.inc         \
              $(inc)\msgserv.asm        \
              tree.ctl                  \
              $(inc)\msghan.inc         \
              treems.inc

treepar.obj:  treepar.asm


tree.exc:     tree.obj                  \
              tree.lnk                  \
              treesysp.obj              \
              treesysm.obj              \
              treepar.obj
              link @tree.lnk

tree.com:     tree.exc



!IFDEF        DEBUG

tree.exe:     tree.obj                  \
              tree.lnk                  \
              treesysp.obj              \
              treesysm.obj              \
              treepar.obj
              link /MAP /LI /CO @tree.lnk
              del tree.exe
              rename tree.exc tree.exe
              cvpack -p tree.exe
              del tree.com
              msym tree

!ENDIF


test: test.exe

test.exe: test.obj
   link /CO /LI /MAP test.obj;
   cvpack -p test.exe
   msym test

test.obj: test.asm
   masm $(extasw) test.asm;


