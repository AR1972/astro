#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */

#************************** makefile for cmd\print ***************************

msg	=..\..\messages
inc	=..\..\inc
bins    =..\..\binaries
cprs    =..\..\compress
dest    =print.exe
cpr     =print.ex_

!ifdef DEBUG

extasw  =-I$(inc) -Zi -D$(COUNTRY)
exelink = /CO /LI /MAP

!else

extasw  =-I$(inc) -D$(COUNTRY)
exelink =

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


print.ctl: print.skl $(msg)\$(COUNTRY).msg

print_r.obj: print_r.asm \
    pridefs.inc print.ctl \
    $(inc)\devsym.inc \
    $(inc)\syscall.inc \
    $(inc)\error.inc \
    $(inc)\sysvar.inc \
    $(inc)\find.inc \
    $(inc)\ea.inc

print_rm.obj: print_rm.asm \
    pridefs.asm print.ctl \
    $(inc)\parse.asm \
    $(inc)\psdata.inc

print_t.obj: print_t.asm \
    pridefs.asm \
    $(inc)\devsym.inc \
    $(inc)\syscall.inc \
    $(inc)\error.inc \
    $(inc)\sysvar.inc \
    $(inc)\find.inc

print_tm.obj: print_tm.asm \
    pridefs.asm print.ctl \
    $(inc)\parse.asm \
    $(inc)\psdata.inc \
    $(inc)\msgserv.asm \
    $(inc)\sysmsg.inc


$(dest):      print_r.obj               \
              print_rm.obj              \
              print_t.obj               \
              print_tm.obj
              link $(exelink) @print.lnk
!ifdef DEBUG
              cvpack -p print.exe
              msym print
!endif


