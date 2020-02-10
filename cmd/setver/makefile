#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */

#*************************** Makefile for DOS ***************************

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\cmd\setver

!IF	"$(DEBUG)" == "CODEVIEW" || "$(debug)" == "codeview" || \
	"$(DEBUG)" == "CODEVIEW" || "$(debug)" == "codeview"

c_debug_flags = -Zi -Od
a_debug_flags = -Zi
link_opts = /CO

!ELSE

c_debug_flags = -Os
link_opts =

!ENDIF

#************************************************************************

extcsw = $(c_debug_flags) -AS -I$(MSG_DIR)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =setver.exe
cpr     =setver.ex_

!IFDEF BASE
dest2   =setver.bse
cpr2    =setver.bs_
extasw = $(a_debug_flags) -Zd -I..\..\inc -DOEMBASE
!ELSE
dest2   =setver.exe
cpr2    =setver.ex_
extasw = $(a_debug_flags) -Zd -I..\..\inc
!ENDIF


!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest2) $(cprs)\$(cpr2)

$(bins)\$(dest2): $(dest)
            copy $(dest) $(bins)\$(dest2)

$(cprs)\$(cpr2):  $(cpr)
            copy $(cpr) $(cprs)\$(cpr2)

$(cpr):       $(dest)
            $(compress) $(dest) $(cpr)

!ENDIF


$(dest):	setver.obj      \
		parse.obj       \
		dos.obj		\
		device.obj
        link $(link_opts) /MAP/LINE device+setver+parse+dos,$(dest);

setver.obj:	setver.c setver.h $(MSG_DIR)\message.h

parse.obj:	parse.c setver.h

dos.obj:	dos.asm

device.obj:	device.asm

data.obj:	data.asm
