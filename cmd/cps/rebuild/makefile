# This is the 'MAKE' description file for UNFORMAT   08-13-90.
# 12-11-90 GWD Removed references to 'datetime.inc'.
# Copyright (C) Microsoft 1991.  All rights reserved.

bins    =..\..\..\binaries
cprs    =..\..\..\compress
dest    =unformat.com
pkl     =unformat.pkl

!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(dest)

$(bins)\$(dest): $(pkl)
            copy $(pkl) $(bins)\$(dest)

$(cprs)\$(dest):  $(pkl)
            copy $(pkl) $(cprs)\$(dest)

$(pkl):       $(dest)
            pklite -o $(dest) $(pkl)

!ENDIF


uf_main.obj:  uf_main.asm  uf_incl.inc makefile

uf_partn.obj: uf_partn.asm  uf_incl.inc makefile

uf_io.obj:    uf_io.asm makefile

uf_msg.obj:   uf_msg.asm makefile

uf_jims.obj:  uf_jims.asm uf_jmsg.asm makefile

uf_last.obj:  uf_last.asm makefile

uf_org.obj:   uf_org.asm makefile

unformat.exc: uf_org.obj uf_main.obj uf_partn.obj \
              uf_io.obj uf_msg.obj uf_jims.obj uf_last.obj
              link /li /map uf_org.obj uf_main.obj uf_partn.obj \
                   uf_io.obj uf_msg.obj uf_jims.obj uf_last.obj, \
                   unformat.exc,unformat.map;

$(dest):      unformat.exc
