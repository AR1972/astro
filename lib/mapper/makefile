
inc =..\..\inc
doslib  =..

all:          $(doslib)\mapper.lib

clean:
    if exist *.obj       del *.obj
    if exist *.exe       del *.exe
    if exist *.map       del *.map
    if exist *.sym       del *.sym
    if exist *.lib       del *.lib

mapper.obj:   mapper.asm                   \
              makefile                     \
              macros.inc                   \
              $(inc)\cmacros.inc

mapper.lib:   mapper.obj
              if exist mapper.lib del mapper.lib
              lib mapper+mapper;

$(doslib)\mapper.lib: mapper.lib
                      copy mapper.lib $(doslib)

