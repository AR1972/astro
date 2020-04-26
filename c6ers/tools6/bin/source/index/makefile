hinc    =..\..\h
dest    =index.exe
bins    =..\..\

all: $(dest) $(bins)\$(dest)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)
			
index.obj:  index.c                  \
            makefile

$(dest):    index.obj
            link $(exelink) /stack:4096 index;
			  
clean:
	del *.exe
	del *.obj

