hinc    =..\..\h
dest    =buildidx.exe
bins    =..\..\

all: $(dest) $(bins)\$(dest)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)
			
buildidx.obj:    buildidx.c           \
                 makefile

$(dest):      buildidx.obj
              link $(exelink) /stack:4096 buildidx;

clean:
	del *.exe
	del *.obj

