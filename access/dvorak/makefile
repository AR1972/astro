# m000 -- updated dependencies on keybmac.inc and keybshar.inc
com     =.
bins    =c:\alphabet\bin
cprs    =..\..\compress
compress=compress -f                       
dest    =keyboard.sys
cpr     =keyboard.sy_

#inc     =c:\dos500\inc
inc     =.
extasw  =-I$(inc)

.asm.obj:
        $(asm) $(ainc) $(aflags) $*.asm;

!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\keyboard.500

$(bins)\keyboard.500: $(dest)
            copy $(dest) $(bins)\keyboard.500

$(cprs)\$(cpr):  $(cpr)
            copy $(cpr) $(cprs)

$(cpr):       $(dest)
            $(compress) $(dest) $(cpr)

!ENDIF

KDFNOW.OBJ:   KDFNOW.ASM  $(com)\postequ.inc \
              $(com)\dseg.inc $(com)\keybshar.inc keybmac.inc keybmac.inc

KDFEOF.OBJ:   KDFEOF.ASM    $(com)\COPYRIGH.INC

KDFDV.OBJ:    KDFDV.ASM    $(com)\keybshar.inc keybmac.inc makefile

KDFRH2.OBJ:      KDFRH2.ASM    $(com)\keybshar.inc keybmac.inc makefile       # [YST 21/1/91 : added Belorussian]

KDFLH2.OBJ:      KDFLH2.ASM    $(com)\keybshar.inc keybmac.inc makefile       # [YST 21/1/91 : added Belorussian]

KEYBOARD.exs: KDFDV.OBJ kdfrh2.obj kdflh2.obj \
              kdfeof.OBJ kdfnow.obj keyboard.lnk
              LINK @keyboard.lnk

$(dest):      Keyboard.exs
