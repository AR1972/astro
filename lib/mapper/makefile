
inc    =..\..\inc
hinc   =..\..\h
doslib =..
extasw =-I$(inc)
extcsw =-I$(hinc)

OBJECTS= beep.obj exit.obj chrattr.obj curpos.obj pageup.obj delete.obj \
         ffirst.obj open.obj write.obj curdir.obj fmode.obj sfmode.obj \
         chfptr.obj sfinfo.obj close.obj alocseg.obj fnext.obj seldsk.obj \
         gcntinfo.obj sint24.obj sfsinfo.obj curdsk.obj error.obj fndclo.obj\
         sighndlr.obj fsinfo.obj fileinfo.obj

all:            $(doslib)\mapper.lib 

{}.asm{}.obj: 
         ml /c /Cp $< $@;

mapper.lib:     $(OBJECTS)
                if exist mapper.lib del mapper.lib
                if exist mapper.map del mapper.map
                lib mapper.lib@<<
$(OBJECTS:..=+..)
mapper.map;
<<

$(doslib)\mapper.lib: mapper.lib
                      if exist $(doslib)\mapper.lib del $(doslib)\mapper.lib
                      copy mapper.lib $(doslib)

clean:
    if exist *.obj    del *.obj
    if exist *.lib    del *.lib
    if exist *.map    del *.map
    if exist *.crf    del *.crf

