#************************** makefile for cmd\fastopen ***************************

msg	=..\..\messages
inc	=..\..\inc
extasw	=-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =fastopen.exe
cpr     =fastopen.ex_

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


fastopen.ctl: fastopen.skl  makefile  $(msg)\$(COUNTRY).msg
        buildmsg $(msg)\$(COUNTRY) fastopen.skl

fastopen.obj: fastopen.asm $(inc)\fastopen.inc \
	makefile

fastinit.obj: fastinit.asm $(inc)\dossym.inc $(inc)\fastopen.inc \
	makefile

fastp.obj: fastp.asm \
	makefile \
	$(inc)\psdata.inc \
	$(inc)\parse.asm

fastsm.obj: fastsm.asm \
	makefile \
	$(inc)\versiona.inc \
	$(inc)\copyrigh.inc \
	$(inc)\sysmsg.inc \
	$(inc)\msgserv.asm \
        fastopen.ctl

$(dest): fastopen.obj \
         fastopen.lnk \
         fastinit.obj \
         fastp.obj \
         fastsm.obj
         link $(exelink) @fastopen.lnk
