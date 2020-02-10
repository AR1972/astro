CC  = cl -c -W2 -AS -Gcsw -Ogilt -nologo -G2
ASM = masm -Mx -Zi
LINK= link /NOE/MAP/NOD
NAME= smartmon
LIBS = libw slibcew shell

OBJ  = $(NAME).obj drive.obj rate.obj log.obj status.obj \
       int2f.obj util.obj auto.obj

.c.obj:
	$(CC) $*.c

.asm.obj:
	$(ASM) $*.asm;

goal: $(NAME).exe

$(NAME).exe: $(OBJ)  $(NAME).res $(NAME).def
	$(LINK) @<<
	    $(OBJ),
	    $(NAME),
	    $(NAME),
	    $(LIBS),
	    $(NAME).def
<<
	rc $(NAME).res $(NAME).exe
	mapsym $(NAME).map

$(NAME).res: $(NAME).rc 
	rc -r $(NAME).rc

clean:
	del $(NAME).exe
	del *.res
	del *.obj
	del *.map
	del *.sym
	del *.bak
