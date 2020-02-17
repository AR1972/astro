
all: comsubs.lib mapper.lib

			
comsubs.obj:	comsubs.c \
				makefile

mapper.obj:		mapper.c \
				makefile

comsubs.lib: 	comsubs.obj
				lib comsubs.lib +comsubs.obj;
				
mapper.lib:		mapper.obj
				lib mapper.lib +mapper.obj;
			  
clean:
	-del *.obj

