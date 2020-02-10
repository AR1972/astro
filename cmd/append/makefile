#************************** makefile for cmd\append ***************************

msg	=..\..\messages
inc	=..\..\inc
extasw	=-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =append.exe
cpr     =append.ex_

#
#######################	dependencies begin here. #########################
#


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

append.ctl: append.skl \
            makefile   \
            $(msg)\$(COUNTRY).msg

append.obj: append.asm \
            appendp.inc \
            $(inc)\parse.asm \
            makefile \
            append.ctl

$(dest):    append.obj \
            makefile
        link $(exelink) append;
