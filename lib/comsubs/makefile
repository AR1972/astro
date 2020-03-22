
inc    =..\..\inc
hinc   =..\..\h
doslib =..
extasw =-I$(inc)
extcsw =-I$(hinc)

all:            $(doslib)\comsubs.lib 

comsubs.obj:     comsubs.c

comsubs.lib:     comsubs.obj
                 if exist comsubs.lib del comsubs.lib
                 lib comsubs+comsubs;

$(doslib)\comsubs.lib: comsubs.lib
                       copy comsubs.lib $(doslib)

clean:
    if exist *.obj    del *.obj
    if exist *.lib    del *.lib
