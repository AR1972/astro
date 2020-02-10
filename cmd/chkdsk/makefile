#************************** makefile for cmd\chkdsk ***************************

msg     =..\..\messages
inc     =..\..\inc
bins    =..\..\binaries
cprs    =..\..\compress
pkl     =chkdsk.pkl
dest    =chkdsk.exe

!IFDEF BASE
extasw  =-I$(inc) -D$(COUNTRY) -DOEMBASE
dest2   =chkdsk.bse
!ELSE
extasw  =-I$(inc) -D$(COUNTRY) -DDBLSPACE_HOOKS
dest2   =chkdsk.exe
!ENDIF

#
####################### dependencies begin here. #########################
#


!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest2) $(cprs)\$(dest2)

$(bins)\$(dest2): $(pkl)
	    copy $(pkl) $(bins)\$(dest2)

$(cprs)\$(dest2):  $(pkl)
	    copy $(pkl) $(cprs)\$(dest2)

$(pkl):       $(dest)
	    pklite -o $(dest) $(pkl)

!ENDIF

chkdsk.ctl:   chkdsk.skl                \
	      $(msg)\$(COUNTRY).msg     \
	      makefile

chkinit.obj:  chkinit.asm               \
	      chkequ.inc                \
	      chkmacro.inc              \
	      chkparse.inc              \
	      chkchng.inc               \
	      chkseg.inc                \
	      $(inc)\ioctl.inc          \
	      makefile

chkdsk1.obj:  chkdsk1.asm               \
	      chkequ.inc                \
	      chkmacro.inc              \
	      chkparse.inc              \
	      chkchng.inc               \
	      chkseg.inc                \
	      $(inc)\ioctl.inc          \
	      $(inc)\dossym.inc         \
	      $(inc)\sysmsg.inc         \
	      $(inc)\devsym.inc         \
	      $(inc)\cpmfcb.inc         \
	      $(inc)\dosmac.inc         \
	      $(inc)\syscall.inc        \
	      makefile

chkdsk2.obj:  chkdsk2.asm               \
	      chkequ.inc                \
	      chkmacro.inc              \
	      chkparse.inc              \
	      chkchng.inc               \
	      chkseg.inc                \
	      $(inc)\ioctl.inc          \
	      $(inc)\dossym.inc         \
	      $(inc)\sysmsg.inc         \
	      $(inc)\devsym.inc         \
	      $(inc)\cpmfcb.inc         \
	      $(inc)\dosmac.inc         \
	      $(inc)\syscall.inc        \
	      makefile

chkproc.obj:  chkproc.asm               \
	      chkequ.inc                \
	      chkmacro.inc              \
	      chkparse.inc              \
	      chkchng.inc               \
	      chkseg.inc                \
	      $(inc)\ioctl.inc          \
	      $(inc)\dossym.inc         \
	      $(inc)\sysmsg.inc         \
	      $(inc)\devsym.inc         \
	      $(inc)\cpmfcb.inc         \
	      $(inc)\dosmac.inc         \
	      $(inc)\syscall.inc        \
	      makefile

chkproc2.obj: chkproc2.asm              \
	      chkequ.inc                \
	      chkmacro.inc              \
	      chkparse.inc              \
	      chkchng.inc               \
	      chkseg.inc                \
	      $(inc)\ioctl.inc          \
	      $(inc)\dossym.inc         \
	      $(inc)\sysmsg.inc         \
	      $(inc)\devsym.inc         \
	      $(inc)\cpmfcb.inc         \
	      $(inc)\dosmac.inc         \
	      $(inc)\syscall.inc        \
	      makefile

chkdisk.obj:  chkdisk.asm               \
	      chkequ.inc                \
	      chkmacro.inc              \
	      chkparse.inc              \
	      chkchng.inc               \
	      chkseg.inc                \
	      $(inc)\ioctl.inc          \
	      $(inc)\dossym.inc         \
	      $(inc)\sysmsg.inc         \
	      $(inc)\devsym.inc         \
	      $(inc)\cpmfcb.inc         \
	      $(inc)\dosmac.inc         \
	      $(inc)\syscall.inc        \
	      makefile

chkfat.obj:   chkfat.asm                \
	      chkequ.inc                \
	      chkmacro.inc              \
	      chkparse.inc              \
	      chkchng.inc               \
	      chkseg.inc                \
	      $(inc)\ioctl.inc          \
	      $(inc)\dossym.inc         \
	      $(inc)\sysmsg.inc         \
	      $(inc)\devsym.inc         \
	      $(inc)\cpmfcb.inc         \
	      $(inc)\dosmac.inc         \
	      $(inc)\syscall.inc        \
	      makefile

chkprmt.obj:  chkprmt.asm               \
	      chkequ.inc                \
	      chkmacro.inc              \
	      chkparse.inc              \
	      chkchng.inc               \
	      chkseg.inc                \
	      $(inc)\ioctl.inc          \
	      $(inc)\dossym.inc         \
	      $(inc)\sysmsg.inc         \
	      $(inc)\devsym.inc         \
	      $(inc)\cpmfcb.inc         \
	      $(inc)\dosmac.inc         \
	      $(inc)\syscall.inc        \
	      makefile

chkdisp.obj:  chkdisp.asm               \
	      chkequ.inc                \
	      chkmacro.inc              \
	      chkmsg.inc                \
	      chkparse.inc              \
	      chkchng.inc               \
	      chkseg.inc                \
	      $(inc)\ioctl.inc          \
	      $(inc)\dossym.inc         \
	      $(inc)\sysmsg.inc         \
	      $(inc)\devsym.inc         \
	      $(inc)\cpmfcb.inc         \
	      $(inc)\dosmac.inc         \
	      $(inc)\syscall.inc        \
	      chkdsk.ctl                \
	      makefile

spawn.obj:    spawn.asm                 \
	      chkseg.inc                \
	      chkmacro.inc              \
	      $(inc)\syscall.inc        \
	      makefile

path.obj:     $(inc)\path.asm           \
	      $(inc)\dossym.inc         \
	      $(inc)\syscall.inc        \
	      $(inc)\pdb.inc            \
	      $(inc)\find.inc           \
	      $(inc)\curdir.inc         \
	      makefile
	      $(asm) $(ainc) $(aflags) $(inc)\path.asm,path.obj;


$(dest):      chkdsk1.obj               \
	      chkdsk2.obj               \
	      chkproc.obj               \
	      chkproc2.obj              \
	      chkprmt.obj               \
	      chkdisk.obj               \
	      chkfat.obj                \
	      chkinit.obj               \
	      chkdisp.obj               \
	      spawn.obj                 \
	      path.obj                  \
	      chkdsk.lnk                \
	      makefile
	      link $(exelink) @chkdsk.lnk
