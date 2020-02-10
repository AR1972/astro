
inc     =../../inc
extasw  =-I$(inc)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =kkcfunc.sys
cpr     =kkcfunc.sy_


!IF "$(COUNTRY)" == "JAPAN" || "$(COUNTRY)" == "japan"

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



kkcfunc.obj:   kkcfunc.asm              \
               kkcfunc.inc              \
               $(inc)\versiona.inc      \
               $(inc)\syscall.inc       \
               makefile

kkcfunc.exe:   kkcfunc.obj
               link kkcfunc;

$(dest):       kkcfunc.exe
               exe2bin kkcfunc $(dest)

!ENDIF
