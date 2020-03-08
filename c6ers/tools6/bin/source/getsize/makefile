hinc    =..\..\h
dest    =getsize.exe
bins    =..\..\

all: $(dest) $(bins)\$(dest)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)
			
getsize.obj:  getsize.c                \
              makefile                  
			  
$(dest):      getsize.obj
              link $(exelink) /stack:4096 getsize;
			  
clean:
	del *.exe
	del *.obj

