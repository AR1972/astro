hinc    =..\..\h
dest    =buildmsg.exe
bins    =..\..\

all: $(dest) $(bins)\$(dest)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)
			
buildmsg.obj: buildmsg.c                  \
              makefile                  \
			  
$(dest):      buildmsg.obj
              link $(exelink) buildmsg;

