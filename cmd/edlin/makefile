#************************** makefile for cmd\append ***************************

msg	=..\..\messages
inc	=..\..\inc
extasw	=-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =edlin.exe
cpr     =edlin.ex_

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


edlin.ctl:    edlin.skl                 \
              $(msg)\$(COUNTRY).msg     \
              makefile


edlin.obj:    edlin.asm                 \
              edlequ.asm                \
              edlstdsw.inc              \
              makefile                  \
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
              $(inc)\syscall.inc

edlcmd1.obj:  edlcmd1.asm               \
              edlequ.asm                \
              edlstdsw.inc              \
              makefile                  \
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
              $(inc)\syscall.inc

edlcmd2.obj:  edlcmd2.asm               \
              edlequ.asm                \
              edlstdsw.inc              \
              makefile                  \
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
              $(inc)\syscall.inc

edlmes.obj:   edlmes.asm                \
              $(inc)\sysmsg.inc         \
              makefile                  \
              edlin.ctl

edlparse.obj: edlparse.asm              \
              $(inc)\parse.asm          \
              makefile

$(dest):      edlin.obj                 \
              edlcmd1.obj               \
              edlcmd2.obj               \
              edlmes.obj                \
              edlparse.obj              \
              edlin.lnk                 \
              makefile
              link $(exelink) @edlin.lnk
