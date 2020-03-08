hinc    =..\..\h
dest    =stripdd.exe
bins    =..\..\

all: $(dest) $(bins)\$(dest)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)
			
stripdd.obj:  stripdd.c                \
              makefile                  
			  
$(dest):      stripdd.obj
              link $(exelink) /stack:4096 stripdd;
			  
clean:
	del *.exe
	del *.obj

