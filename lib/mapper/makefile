
inc    =..\..\inc
hinc   =..\..\h
doslib =..
extasw =-I$(inc)
extcsw =-I$(hinc)

all:            $(doslib)\mapper.lib 

mapper.obj:     mapper.c

mapper.lib:     mapper.obj
                if exist mapper.lib del mapper.lib
                lib mapper+mapper;

$(doslib)\mapper.lib: mapper.lib
                      copy mapper.lib $(doslib)

clean:
    if exist *.obj    del *.obj
    if exist *.lib    del *.lib
