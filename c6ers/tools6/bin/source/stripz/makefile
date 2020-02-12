hinc    =..\..\h
dest    =stripz.exe
bins    =..\..\

all: $(dest) $(bins)\$(dest)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)
			
stipz.obj:    stripz.c                  \
              makefile                  \
              comp.h                    \
              $(hinc)\dos.h             \
              $(hinc)\fcntl.h           \
              $(hinc)\stdlib.h          \
              $(hinc)\stdio.h           \
              $(hinc)\malloc.h          \
              $(hinc)\ctype.h
			  
$(dest):      stripz.obj
              link $(exelink) stripz;
