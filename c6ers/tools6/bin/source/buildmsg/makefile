hinc    =..\..\h
dest    =buildmsg.exe
bins    =..\..\

all: $(dest) $(bins)\$(dest)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)
			
buildmsg.obj: buildmsg.c              \
			  buildmsg.h              \
              makefile

$(dest):      buildmsg.obj
              link $(exelink) /stack:4096 buildmsg;
			  
clean:
	del *.exe
	del *.obj

