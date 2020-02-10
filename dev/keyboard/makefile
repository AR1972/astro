#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */
# m000 -- updated dependencies on keybmac.inc and keybshar.inc
com     =..\..\inc
bins    =..\..\binaries
cprs    =..\..\compress
dest    =keyboard.sys
cpr     =keyboard.sy_

inc     =..\..\inc
extasw  =-I$(inc) -D$(COUNTRY)

.asm.obj:
	$(asm) $(ainc) $(aflags) $*.asm;

!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(dest)

$(bins)\$(dest): $(dest)
	    copy $(dest) $(bins)

$(cprs)\$(dest): $(dest)
            copy $(dest) $(cprs)

$(cprs)\$(cpr):  $(cpr)
	    copy $(cpr) $(cprs)

$(cpr):       $(dest)
	    $(compress) $(dest) $(cpr)

!ENDIF

KDFNOW.OBJ:   KDFNOW.ASM  $(com)\postequ.inc \
	      $(com)\dseg.inc $(com)\keybshar.inc keybmac.inc keybmac.inc

KDFFR120.OBJ: KDFFR120.ASM    $(com)\keybshar.inc keybmac.inc

KDFFR189.OBJ: KDFFR189.ASM    $(com)\keybshar.inc keybmac.inc

KDFEOF.OBJ:   KDFEOF.ASM    $(com)\COPYRIGH.INC

KDFUK166.OBJ: KDFUK166.ASM    $(com)\keybshar.inc keybmac.inc

KDFUK168.OBJ: KDFUK168.ASM    $(com)\keybshar.inc keybmac.inc

KDFIT141.OBJ: KDFIT141.ASM    $(com)\keybshar.inc keybmac.inc

KDFIT142.OBJ: KDFIT142.ASM    $(com)\keybshar.inc keybmac.inc

KDFGE.OBJ:    KDFGE.ASM    $(com)\keybshar.inc keybmac.inc

KDFSP.OBJ:    KDFSP.ASM    $(com)\keybshar.inc keybmac.inc

KDFNL.OBJ:    KDFNL.ASM    $(com)\keybshar.inc keybmac.inc

KDFNO.OBJ:    KDFNO.ASM    $(com)\keybshar.inc keybmac.inc

KDFDK.OBJ:    KDFDK.ASM    $(com)\keybshar.inc keybmac.inc

KDFSG.OBJ:    KDFSG.ASM    $(com)\keybshar.inc keybmac.inc

KDFSF.OBJ:    KDFSF.ASM    $(com)\keybshar.inc keybmac.inc

KDFPO.OBJ:    KDFPO.ASM    $(com)\keybshar.inc keybmac.inc

KDFBE.OBJ:    KDFBE.ASM    $(com)\keybshar.inc keybmac.inc

KDFCF.OBJ:    KDFCF.ASM    $(com)\keybshar.inc keybmac.inc

KDFLA.OBJ:    KDFLA.ASM    $(com)\keybshar.inc keybmac.inc

KDFSv.OBJ:    KDFSv.ASM    $(com)\keybshar.inc keybmac.inc

KDFBR.OBJ:    KDFBR.ASM    $(com)\keybshar.inc keybmac.inc   # [Mihindu 10/30/90 : added Brazil]

KDFHU.OBJ:    KDFHU.ASM    $(com)\keybshar.inc keybmac.inc	 # [Mihindu 11/30/90 : added Hungary]

KDFPL.OBJ:    KDFPL.ASM    $(com)\keybshar.inc keybmac.inc	 # [Mihindu 11/30/90 : added Poland]

KDFYU.OBJ:    KDFYU.ASM    $(com)\keybshar.inc keybmac.inc	 # [Mihindu 11/30/90 : added Yugoslavia]

KDFCZ.OBJ:    KDFCZ.ASM    $(com)\keybshar.inc keybmac.inc	 # [Mihindu 11/30/90 : added Czech]

KDFSL.OBJ:    KDFSL.ASM    $(com)\keybshar.inc keybmac.inc	 # [Mihindu 11/30/90 : added Slovak]

KDFJP.OBJ:    KDFJP.ASM    $(com)\keybshar.inc keybmac.inc	 # CAS M000 Merged JP from IBM

KEYBOARD.exs: KDFSP.OBJ KDFPO.OBJ KDFFR120.OBJ KDFFR189.OBJ KDFDK.OBJ \
	      KDFSG.OBJ KDFGE.OBJ  KDFIT141.OBJ KDFIT142.OBJ KDFUK166.OBJ \
	      KDFUK168.OBJ KDFSF.OBJ KDFBE.OBJ KDFNL.OBJ KDFNO.OBJ KDFCF.OBJ \
	      KDFHU.OBJ KDFPL.OBJ KDFYU.OBJ KDFCZ.OBJ KDFSL.OBJ \
	      KDFJP.OBJ \
	      KDFLA.OBJ kdfsv.obj KDFBR.OBJ kdfeof.OBJ kdfnow.obj keyboard.lnk
	      LINK @keyboard.lnk

$(dest):      Keyboard.exs


