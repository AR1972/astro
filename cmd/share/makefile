#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */

#************************ makefile for cmd\share *************************

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\cmd\share

msg	=..\..\messages
inc     =..\..\inc 
dos     =..\..\dos
extasw  = -I$(inc) -I$(dos) -DIS_SHARE -I$(MSG_DIR) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =share.exe
cpr     =share.ex_

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


msdata.obj:   $(dos)\msdata.asm         \
              $(dos)\mssw.asm           \
              $(inc)\ARENA.INC          \
              $(inc)\CURDIR.INC         \
              $(inc)\DEVSYM.INC         \
              $(inc)\DPB.INC            \
              $(inc)\FIND.INC           \
              $(inc)\MI.INC             \
              $(inc)\PDB.INC            \
              $(inc)\SF.INC             \
              $(inc)\VECTOR.INC         \
              $(inc)\VERSIONA.INC       \
              $(inc)\buffer.INC         \
              $(inc)\dbcs.sw            \
              $(inc)\dirent.INC         \
              $(inc)\doscntry.inc       \
              $(inc)\dosmac.INC         \
              $(inc)\dosseg.inc         \
              $(inc)\dossym.inc         \
              $(inc)\error.INC          \
              $(inc)\fastopen.inc       \
              $(inc)\ms_data.asm        \
              $(inc)\lmstub.asm         \
              $(inc)\version.inc        \
              $(inc)\xmm.inc            \
              $(dos)\dostab.asm         \
              $(inc)\const2.asm         \
              $(dos)\msconst.asm        \
              $(inc)\mshead.asm         \
              $(inc)\BUGTYP.INC         \
              $(inc)\ORIGIN.INC         \
	      $(inc)\bdsize.inc
        masm -t $(extasw) $(dos)\msdata.asm,msdata.obj;

msdos.cl1:    $(dos)\msdos.skl          \
              $(msg)\$(COUNTRY).msg     \
              $(dos)\makefile           \
              makefile                  \
              copy $(dos)\msdos.skl
              nosrvbld msdos.skl $(msg)\$(COUNTRY).msg


share.ctl:    share.skl                 \
              $(msg)\$(COUNTRY).msg     \
              makefile

gshare.obj:   gshare.asm                \
              $(inc)\dossym.inc         \
              $(inc)\mft.inc            \
              $(inc)\dosmac.inc         \
              $(inc)\bpb.inc            \
              $(inc)\curdir.inc         \
              $(inc)\buffer.inc         \
              $(inc)\sysvar.inc         \
              $(inc)\vector.inc         \
              $(inc)\mult.inc           \
              $(inc)\dirent.inc         \
              $(inc)\dpb.inc            \
              $(inc)\cpmfcb.inc         \
              $(inc)\find.inc           \
              $(inc)\pdb.inc            \
              $(inc)\exe.inc            \
              $(inc)\sf.inc             \
              $(inc)\arena.inc          \
              $(inc)\intnat.inc         \
              $(inc)\mi.inc             \
              $(inc)\filemode.inc       \
              $(inc)\error.inc          \
              $(inc)\syscall.inc        \
              $(inc)\bugtyp.inc         \
              makefile

gshare2.obj:  gshare2.asm               \
	      $(MSG_DIR)\shmsg.inc	\
              $(inc)\dossym.inc         \
              $(inc)\dosseg.inc         \
              $(inc)\mft.inc            \
              $(inc)\dosmac.inc         \
              $(inc)\bpb.inc            \
              $(inc)\curdir.inc         \
              $(inc)\buffer.inc         \
              $(inc)\sysvar.inc         \
              $(inc)\vector.inc         \
              $(inc)\mult.inc           \
              $(inc)\dirent.inc         \
              $(inc)\dpb.inc            \
              $(inc)\cpmfcb.inc         \
              $(inc)\find.inc           \
              $(inc)\pdb.inc            \
              $(inc)\exe.inc            \
              $(inc)\sf.inc             \
              $(inc)\arena.inc          \
              $(inc)\intnat.inc         \
              $(inc)\mi.inc             \
              $(inc)\filemode.inc       \
              $(inc)\error.inc          \
              $(inc)\syscall.inc        \
              $(inc)\bugtyp.inc         \
              makefile

sharesr.obj:  sharesr.asm               \
              share.ctl                 \
              $(inc)\parse.asm          \
              $(inc)\psdata.inc         \
	      $(inc)\sysmsg.inc		\
	      $(inc)\msgserv.asm	\
              makefile

sharelnk.obj: sharelnk.asm              \
              makefile

$(dest):      share.ctl                 \
              gshare.obj                \
              gshare2.obj               \
              sharesr.obj               \
              msdata.obj                \
              $(dos)\msdosme.obj        \
              sharelnk.obj              \
              share.lnk
              link $(exelink) @share.lnk

