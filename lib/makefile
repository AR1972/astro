
msg	=..\messages
inc	=..\inc
hinc =..\h
extasw =-I$(inc) -D$(COUNTRY)
extcsw =-I$(hinc)

all:            comsubs.lib               \
                master.lib                \
			    mapper.lib 
	
comsubs.obj:    comsubs.c

master.obj:	    master.c                  \
                makefile                  \
                $(hinc)\cds.h             \
                $(hinc)\jointype.h        \
                $(hinc)\sysvar.h          \

mapper.obj:	    mapper.c

comsubs.lib:    comsubs.obj
                del comsubs.lib
                lib comsubs.lib +comsubs.obj;
				
mapper.lib:		mapper.obj
                del mapper.lib
				lib mapper.lib +mapper.obj;
				
master.lib:		master.obj
                del master.lib
				lib master.lib +master.obj;

clean:
	-del *.obj

