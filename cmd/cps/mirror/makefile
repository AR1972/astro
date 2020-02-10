# This is the 'MAKE' description file for MIRROR.COM	08-13-90.
# Copyright (C) 1991 Microsoft Corp.  All rights reserved.

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\cmd\cps\mirror
inc     =..\..\..\inc		# M006
bins    =..\..\..\binaries
cprs    =..\..\..\compress
dest    =mirror.com
cpr     =mirror.co_
extasw  =-I$(inc)		# M006

!IFNDEF       BUILDER

all: icopy $(dest)

!ELSE

all: icopy $(dest) $(bins)\$(dest) $(cprs)\$(cpr)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)

$(cprs)\$(cpr):  $(cpr)
            copy $(cpr) $(cprs)

$(cpr):       $(dest)
            $(compress) $(dest) $(cpr)

!ENDIF

icopy:
	copy $(MSG_DIR)\mir_msg.inc
	copy $(MSG_DIR)\mir_pmsg.asm

mir_dtrk.obj: mir_dtrk.asm  mir_msg.inc makefile

mir_main.obj: mir_main.asm  mir_msg.inc makefile

mir_prtn.obj: mir_prtn.asm makefile

mir_pmsg.obj: mir_pmsg.asm makefile

mir_last.obj: mir_last.asm makefile

mir_org.obj:  mir_org.asm  makefile

mirror.exc:   mir_org.obj mir_dtrk.obj mir_main.obj \
              mir_prtn.obj mir_pmsg.obj mir_last.obj
              LINK mir_org.obj mir_dtrk.obj mir_main.obj \
                   mir_prtn.obj mir_pmsg.obj mir_last.obj, \
                   mirror.exc;

$(dest):      mirror.exc
