all: comsubs.lib master.lib mapper.lib 
	
comsubs.obj: comsubs.c	
master.obj:	master.c
mapper.obj:	mapper.c

comsubs.lib: 	comsubs.obj
				lib comsubs.lib +comsubs.obj;
				
mapper.lib:		mapper.obj
				lib mapper.lib +mapper.obj;
				
master.lib:		master.obj
				lib master.lib +master.obj;

clean:
	-del *.obj

