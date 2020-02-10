# Memory models

#BUILD_L=1	# Large
BUILD_S=1	# Small.
BUILD_M=1	  # Medium.

!if "$(DEBUG)" == "CODEVIEW" || "$(DEBUG)" == "codeview" || \
	 "$(debug)" == "CODEVIEW" || "$(debug)" == "codeview"

LINK = link /NOE/CO/LI/MAP
CC   = cl -c -W2	-Gw -Zdip -Od -DDEBUG -I. -I$(INC)

!else

LINK = link /NOE/LI/MAP
CC   = cl -c -W2 -Gsw -Zdp -Od -I. -I$(INC)

!endif

INC  = ..\compress
MASM = masm -Mx -Zi -t
NAME = sulib
NAMED = sulibd

#OBJ = infparse.obj dos.obj copy.obj file.obj exe.obj
#OBJD = infparsd.obj dos.obj copy.obj filed.obj exe.obj

OBJ =  dos.obj copy.obj
OBJD = dos.obj copy.obj


# C	= $(CC) -NT _$(SEG) $*.c
# ASM = $(MASM) -DSEGNAME=$(SEG) $*.asm ;

.c.obj:
!ifdef BUILD_S
	$(CC) -AS -Fo$*.obj $*.c
!endif
!ifdef BUILD_M
	$(CC) -AM -Fomedium\$*.obj $*.c
!endif
!ifdef BUILD_L
	$(CC) -AL -DMLARGE -Folarge\$*.obj $*.c
!endif

.asm.obj:
!ifdef BUILD_S
	$(MASM) -DmemS=1 $*.asm,$*.obj;
!endif
!ifdef BUILD_M
	$(MASM) -DmemM=1 $*.asm,medium\$*.obj;
!endif
!ifdef BUILD_L
	$(MASM) -DmemL=1 $*.asm,large\$*.obj;
!endif


all: $(NAME).lib $(NAMED).lib  # findwin.exe


$(NAME).lib: $(OBJ)
!ifdef BUILD_S
	del $(NAME).lib
	lib $(NAME).lib +$(OBJ) ;
!endif
!ifdef BUILD_M
	cd medium
	del $(NAME).lib
	lib $(NAME).lib +$(OBJ) ;
	cd ..
!endif
!ifdef BUILD_L
	cd large
	del $(NAME).lib
	lib $(NAME).lib +$(OBJ) ;
	cd ..
!endif



$(NAMED).lib: $(OBJD)
!ifdef BUILD_S
	del $(NAMED).lib
	lib $(NAMED).lib +$(OBJD) ;
!endif
!ifdef BUILD_M
	cd medium
	del $(NAMED).lib
	lib $(NAMED).lib +$(OBJD) ;
	cd ..
!endif
!ifdef BUILD_L
	cd large
	del $(NAMED).lib
	lib $(NAMED).lib +$(OBJD) ;
	cd ..
!endif


#infparsd.obj :   infparse.c
#!ifdef BUILD_S
#	$(CC) -AS -DDOSONLY -Fo$*.obj $**
#!endif
#
#!ifdef BUILD_M
#	$(CC) -AM -DDOSONLY -Fomedium\$*.obj $**
#!endif
#!ifdef BUILD_L
#	$(CC) -AL -DDOSONLY -DMLARGE -Folarge\$*.obj $**
#!endif


#filed.obj    :   file.c
#!ifdef BUILD_S
#	$(CC) -AS -DDOSONLY -Fo$*.obj $**
#!endif
#!ifdef BUILD_M
#	$(CC) -AM -DDOSONLY -Fomedium\$*.obj $**
#!endif
#!ifdef BUILD_L
#	$(CC) -AL -DDOSONLY -DMLARGE -Folarge\$*.obj $**
#!endif


# COMPRESS must not be defined to build this
#
#findwin.exe: findwin.obj $(NAME).lib
#        $(LINK) /STACK:8192 findwin,,,slibcec.lib ..\compress\lzcopy.lib $(NAME).lib ;

clean:
	del *.lib
	del *.res
	del *.obj
	del *.map
	del *.sym
	del medium\*.obj
	del medium\*.lib
	del large\*.obj
	del large\*.lib

depend:
	mv makefile makefile.old
	sed "/^# START Dependencies/,/^# END Dependencies/D" makefile.old > makefile
	del makefile.old
	echo # START Dependencies >> makefile
	includes -l *.c *.asm >> makefile
	echo # END Dependencies >> makefile

# START Dependencies  
# END Dependencies  

