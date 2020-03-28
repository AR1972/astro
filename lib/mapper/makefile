
inc    =..\..\inc
hinc   =..\..\h
doslib =..
extasw =-I$(inc)
extcsw =-I$(hinc)

all:            $(doslib)\mapper.lib 

dosbeep.obj:    dosbeep.asm
                ml /c /Cp dosbeep.asm
                 
dosexit.obj:    dosexit.asm
                ml /c /Cp dosexit.asm
                
chrattr.obj:    chrattr.asm
                ml /c /Cp chrattr.asm

curpos.obj:     curpos.asm
                ml /c /Cp curpos.asm

pageup.obj:     pageup.asm
                ml /c /Cp pageup.asm
                
dummy.obj:      dummy.c

mapper.lib:     dosbeep.obj dosexit.obj chrattr.obj \
                curpos.obj pageup.obj dummy.obj
                if exist mapper.lib del mapper.lib
                lib mapper+dosbeep+dosexit+chrattr+curpos+pageup+dummy;

$(doslib)\mapper.lib: mapper.lib
                      copy mapper.lib $(doslib)

clean:
    if exist *.obj    del *.obj
    if exist *.lib    del *.lib

