hinc    =..\..\h
dest    =stripz.exe
bins    =..\..\

all: $(dest) $(bins)\$(dest)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)
			
stipz.obj:    stripz.c                  \
              makefile                  \
			  
$(dest):      stripz.obj
              link $(exelink) /stack:4096 stripz;
			  
clean:
	-del *.exe
	-del *.obj

