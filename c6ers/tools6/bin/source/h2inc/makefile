hinc    =..\..\h
dest    =h2inc.exe
bins    =..\..\

all: $(dest) $(bins)\$(dest)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)
			
stipz.obj:    h2inc.c                \
              makefile                  
			  
$(dest):      h2inc.obj
              link $(exelink) /stack:4096 h2inc;
			  
clean:
	-del *.exe
	-del *.obj

