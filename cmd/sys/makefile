#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */

#************************** makefile for cmd\sys ***************************

msg     =..\..\messages
inc     =..\..\inc
boot    =..\..\boot
bins    =..\..\binaries
cprs    =..\..\compress
pkl     =sys.pkl
dest    =sys.com

!IFDEF BASE
dest2   =sys.bse
extasw  =-I$(inc) -I$(boot) -D$(COUNTRY) -DOEMBASE
!ELSE
dest2   =sys.com
extasw  =-I$(inc) -I$(boot) -D$(COUNTRY) -DDBLSPACE_HOOKS
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



sys.ctl:      sys.skl                   \
	      $(msg)\$(COUNTRY).msg     \
	      makefile

sys1.obj:     sys1.asm                  \
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
	      $(inc)\ioctl.inc          \
	      $(inc)\bootform.inc       \
	      $(boot)\boot.inc          \
	      syshdr.inc                \
	      makefile

sys2.obj:     sys2.asm                  \
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
	      $(inc)\ioctl.inc          \
	      $(inc)\bootform.inc       \
	      syshdr.inc                \
	      makefile

path.obj:     path.asm                  \
	      $(inc)\dossym.inc         \
	      $(inc)\find.inc           \
	      $(inc)\curdir.inc         \
	      $(inc)\pdb.inc            \
	      $(inc)\syscall.inc

syssr.obj:    syssr.asm                 \
	      $(inc)\parse.asm          \
	      $(inc)\psdata.inc         \
	      $(inc)\msgserv.asm        \
	      $(inc)\sysmsg.inc         \
	      sys.ctl                   \
	      makefile

sys.exc:      sys.ctl                   \
	      sys1.obj                  \
	      syssr.obj                 \
	      sys2.obj                  \
	      path.obj
	      link @sys.lnk

$(dest):      sys.exc
