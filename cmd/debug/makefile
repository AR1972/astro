#************************** makefile for cmd\debug ***************************

msg     =..\..\messages
inc     =..\..\inc
extasw  =-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =debug.exe
pkl     =debug.pkl

#
####################### dependencies begin here. #########################
#

!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(cpr)

$(bins)\$(dest): $(pkl)
	    copy $(pkl) $(bins)\$(dest)

$(cprs)\$(cpr):  $(pkl)
	    copy $(pkl) $(cprs)\$(dest)

$(pkl):       $(dest)
	    pklite -o $(dest) $(pkl)

!ENDIF



debug.ctl:    debug.skl                 \
	      $(msg)\$(COUNTRY).msg     \
	      makefile

debug.obj:    debug.asm                 \
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
	      $(inc)\version.inc        \
	      $(inc)\error.inc          \
	      $(inc)\syscall.inc        \
	      debug.inc                 \
	      makefile

debcom1.obj:  debcom1.asm               \
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
	      $(inc)\version.inc        \
	      $(inc)\error.inc          \
	      $(inc)\syscall.inc        \
	      debug.inc                 \
	      makefile

debcom2.obj:  debcom2.asm               \
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
	      $(inc)\version.inc        \
	      $(inc)\error.inc          \
	      $(inc)\syscall.inc        \
	      debug.inc                 \
	      makefile

debcom3.obj:  debcom3.asm               \
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
	      $(inc)\version.inc        \
	      $(inc)\mi.inc             \
	      $(inc)\error.inc          \
	      $(inc)\syscall.inc        \
	      debug.inc                 \
	      makefile

debasm.obj:   debasm.asm                \
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
	      $(inc)\version.inc        \
	      $(inc)\error.inc          \
	      $(inc)\syscall.inc        \
	      debug.inc                 \
	      makefile

debuasm.obj:  debuasm.asm               \
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
	      $(inc)\version.inc        \
	      $(inc)\error.inc          \
	      $(inc)\syscall.inc        \
	      debug.inc                 \
	      makefile

deberr.obj:   deberr.asm                \
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
	      $(inc)\version.inc        \
	      $(inc)\error.inc          \
	      $(inc)\syscall.inc        \
	      debug.inc                 \
	      makefile

debconst.obj: debconst.asm              \
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
	      $(inc)\version.inc        \
	      $(inc)\error.inc          \
	      $(inc)\syscall.inc        \
	      debug.inc                 \
	      makefile

debdata.obj:  debdata.asm               \
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
	      $(inc)\version.inc        \
	      $(inc)\error.inc          \
	      $(inc)\syscall.inc        \
	      debug.inc                 \
	      makefile

debems.obj:   debems.asm                \
	      $(inc)\version.inc        \
	      debug.inc                 \
	      makefile

debmes.obj:   debmes.asm                \
	      $(inc)\msgserv.asm        \
	      $(inc)\sysmsg.inc         \
	      debug.ctl                 \
	      makefile

$(dest):      debug.obj                 \
	      debcom1.obj               \
	      debcom2.obj               \
	      debcom3.obj               \
	      debems.obj                \
	      debasm.obj                \
	      debuasm.obj               \
	      deberr.obj                \
	      debconst.obj              \
	      debdata.obj               \
	      debmes.obj                \
	      debug.lnk                 \
	      makefile
	      link $(exelink) @debug.lnk
