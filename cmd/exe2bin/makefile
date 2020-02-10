#************************** makefile for cmd\... ***************************

msg	=..\..\messages
inc     =..\..\inc
extasw	=-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =exe2bin.exe
cpr     =exe2bin.ex_


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

exe2bin.ctl:  exe2bin.skl makefile      \
              $(msg)\$(COUNTRY).msg

e2binit.obj:  e2binit.asm               \
              $(inc)\syscall.inc        \
              e2bparse.inc              \
              e2bmacro.inc              \
              e2bequ.inc                \
              $(inc)\sysmsg.inc         \
              e2btable.inc              \
              exe2bin.ctl               \
              $(inc)\dossym.inc         \
              $(inc)\dosmac.inc         \
              $(inc)\bpb.inc            \
              $(inc)\buffer.inc         \
              $(inc)\sysvar.inc         \
              $(inc)\mult.inc           \
              $(inc)\dirent.inc         \
              $(inc)\dpb.inc            \
              $(inc)\curdir.inc         \
              $(inc)\cpmfcb.inc         \
              $(inc)\find.inc           \
              $(inc)\pdb.inc            \
              $(inc)\sf.inc             \
              $(inc)\arena.inc          \
              $(inc)\intnat.inc         \
              $(inc)\error.inc          \
              $(inc)\syscall.inc        \
              $(inc)\parse.asm          \
              $(inc)\psdata.inc         \
              makefile

display.obj:  display.asm               \
              makefile

$(dest):      display.obj               \
              e2binit.obj               \
              exe2bin.ctl               \
              makefile                  \
              exe2bin.lnk
              link $(exelink) @exe2bin.lnk

