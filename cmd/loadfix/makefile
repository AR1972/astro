msg	=..\..\messages
inc     =..\..\inc
bins    =..\..\binaries
cprs    =..\..\compress
extasw  =-I$(msg)
dest    =loadfix.com
cpr     =loadfix.co_


!ifdef DEBUG

extasw= -I$(msg) -Zi
exelink=/LI /MAP

!else

extasw=-I$(inc) -I$(msg)
exelink=/MAP

!endif



!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(cpr)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)

$(cprs)\$(cpr):  $(cpr)
            copy $(cpr) $(cprs)

$(cpr):       $(dest)
            $(compress) $(dest) $(cpr)

!ENDIF

$(dest): loadfix.exc

loadfix.exc: loadfix.obj
        $(link)  $(exelink) loadfix,loadfix.exc;

loadfix.obj: loadfix.asm $(msg)\loadmsg.msg
