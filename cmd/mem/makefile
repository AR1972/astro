#************************** makefile for cmd\mem ***************************
MSG_DIR=$(LANG_SRC)\$(COUNTRY)\cmd\mem

msg	=..\..\messages
inc	=..\..\inc
hinc    =..\..\h
extasw  =-I$(inc) -D$(COUNTRY)
extcsw  =-I$(hinc) -I$(MSG_DIR)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =mem.exe
cpr     =mem.ex_

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



mem.ctl:        mem.skl                         \
                $(msg)\$(COUNTRY).msg makefile

_msgret.obj:    _msgret.asm                     \
                makefile                         \
                mem.ctl                         \
                $(inc)\msgserv.asm              \
                $(inc)\sysmsg.inc

xms.obj:	xms.asm				\
		makefile

mem.obj:        mem.c                           \
		xmm.h				\
                msgdef.h                        \
		$(MSG_DIR)\mem.h				\
                $(hinc)\version.h               \
                $(hinc)\copyrigh.h

membase.obj:	membase.c			\
		xmm.h				\
                msgdef.h                        \
		$(MSG_DIR)\mem.h				\
                $(hinc)\version.h               \
                $(hinc)\copyrigh.h

memex.obj:	memex.c 			\
		xmm.h				\
                msgdef.h                        \
		$(MSG_DIR)\mem.h				\
                $(hinc)\version.h               \
                $(hinc)\copyrigh.h

submsg.obj:	submsg.c			\
		xmm.h				\
                msgdef.h                        \
		$(MSG_DIR)\mem.h				\
                $(hinc)\version.h               \
                $(hinc)\copyrigh.h

memctrlc.obj:	memctrlc.c			\
		$(hinc)\copyrigh.h

chkvdisk.obj:	chkvdisk.asm

lines.obj:	lines.asm

emm386.obj:	emm386.asm

wintest.obj:	wintest.asm

$(dest):        mem.obj _msgret.obj memex.obj submsg.obj membase.obj \
		mem.lnk chkvdisk.obj memctrlc.obj lines.obj xms.obj \
		emm386.obj wintest.obj
                link $(exelink) @mem.lnk

