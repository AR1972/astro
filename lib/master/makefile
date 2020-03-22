
inc    =..\..\inc
hinc   =..\..\h
doslib =..
extasw =-I$(inc)
extcsw =-I$(hinc)

all:            $(doslib)\master.lib

master.obj:     master.c                  \
                makefile                  \
                $(hinc)\cds.h             \
                $(hinc)\jointype.h        \
                $(hinc)\sysvar.h

master.lib:     master.obj
                if exist master.lib del master.lib
                lib master+master;

$(doslib)\master.lib: master.lib
                      copy master.lib $(doslib)
clean:
    if exist *.obj    del *.obj
    if exist *.lib    del *.lib

