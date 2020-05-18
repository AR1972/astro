
inc    =..\..\inc
hinc   =..\..\h
doslib =..
extasw =-I$(inc)
extcsw =-I$(hinc)

all:            $(doslib)\master.lib

sysvar.obj:     sysvar.c                  \
                makefile                  \
                $(hinc)\types.h           \
                $(hinc)\sysvar.h

cds.obj:        cds.c                     \
                makefile                  \
                $(hinc)\types.h           \
                $(hinc)\sysvar.h          \
                $(hinc)\cds.h             \
                $(hinc)\dpb.h

dpb.obj:        dpb.c                     \
                makefile                  \
                $(hinc)\types.h           \
                $(hinc)\sysvar.h          \
                $(hinc)\dpb.h             \
                $(hinc)\cds.h

errtst.obj:     errtst.c                  \
                makefile                  \
                $(hinc)\types.h           \
                $(hinc)\dpb.h             \
                $(hinc)\stdio.h           \
                $(hinc)\stdlib.h

master.lib:     sysvar.obj                \
                cds.obj                   \
                dpb.obj                   \
                errtst.obj
                if exist master.lib del master.lib
                lib master+sysvar+cds+dpb+errtst;

$(doslib)\master.lib: master.lib
                      copy master.lib $(doslib)
clean:
    if exist *.obj    del *.obj
    if exist *.lib    del *.lib

