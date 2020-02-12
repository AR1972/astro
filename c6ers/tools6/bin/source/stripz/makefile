hinc    =..\..\h
dest    =stripz.exe
bins    =..\..\

all: $(dest) $(bins)\$(dest)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)
			
stipz.obj:    stripz.c                  \
              makefile                  \
              $(hinc)\stdlib.h          \
              $(hinc)\stdio.h           \
              $(hinc)\malloc.h			
			  
$(dest):      stripz.obj
              link $(exelink) stripz;
