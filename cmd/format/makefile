#*************************** Makefile for format ***************************

!IF     "$(DEBUG)" == "CODEVIEW" || "$(debug)" == "codeview" || \
	"$(DEBUG)" == "CODEVIEW" || "$(debug)" == "codeview"

debug_flags = -Zi
link_opts = /CO
dest    =format.exe
cpr     =none

!ELSE

debug_flags =
link_opts =/MAP
dest    =format.com

!ENDIF



#************************ makefile for cmd\format *************************

msg     =..\..\messages
inc     =..\..\inc
boot    =..\..\boot
bins    =..\..\binaries
cprs    =..\..\compress
mirror  =..\cps\mirror          # M027.
MIR_MSG=$(LANG_SRC)\$(COUNTRY)\cmd\cps\mirror # Bug2649

!IFDEF BASE
extasw  = $(debug_flags) -I$(inc) -I$(boot) -DSAFE=1 -D$(COUNTRY) -DOEMBASE
dest2   =format.bse
!ELSE
extasw  = $(debug_flags) -I$(inc) -I$(boot) -DSAFE=1 -D$(COUNTRY) -DDBLSPACE_HOOKS
dest2   =format.com
!ENDIF

#
####################### dependencies begin here. #########################
#

!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest2) $(cprs)\$(dest2)

$(bins)\$(dest2): $(dest)
	    pklite -o $(dest)
	    copy $(dest) $(bins)\$(dest2)

$(cprs)\$(dest2): $(dest)
	    copy $(dest) $(cprs)\$(dest2)


!ENDIF


!IF     "$(DEBUG)" != "CODEVIEW" && "$(debug)" != "codeview" && \
	"$(DEBUG)" != "CODEVIEW" && "$(debug)" != "codeview"

format.exe:   display.obj       \
	      boot.cl1          \
	      format.ctl        \
	      forexec.obj       \
	      forlabel.obj      \
	      format.obj        \
	      forinit.obj       \
	      msfor.obj         \
	      forproc.obj       \
	      mir_main.obj      \
	      glblinit.obj      \
	      phase1.obj        \
	      dskfrmt.obj       \
	      switch_s.obj      \
	      path.obj
	link $(link_opts) $(exelink) @format.lnk

$(dest):      format.exe


!ELSE

$(dest):        display.obj     \
		boot.cl1        \
		forexec.obj     \
		forlabel.obj    \
		format.obj      \
		forinit.obj     \
		msfor.obj       \
		forproc.obj     \
		mir_main.obj    \
		glblinit.obj    \
		phase1.obj      \
		dskfrmt.obj     \
		switch_s.obj    \
		path.obj
	link $(link_opts) @format.lnk
	del format.com

!ENDIF

display.obj:  display.asm               \
	      makefile                  \
	      forequ.inc                \
	      formsg.inc                \
	      format.ctl                \
	      $(inc)\sysmsg.inc         \
	      $(inc)\msgserv.asm        \
	      formacro.inc


boot.cl1:     $(boot)\boot.skl          \
	      makefile
	      copy $(boot)\boot.cl1

format.ctl:   format.skl                \
	      makefile                  \
	      $(msg)\$(COUNTRY).msg

forexec.obj:  forexec.asm               \
	      forequ.inc                \
	      makefile                  \
	      $(inc)\syscall.inc        \
	      formacro.inc

forlabel.obj: forlabel.asm              \
	      makefile                  \
	      forequ.inc                \
	      formacro.inc              \
	      $(inc)\syscall.inc        \
	      $(inc)\ioctl.inc          \
	      $(inc)\dosmac.inc         \
	      forswtch.inc

format.obj:   format.asm                \
	      makefile                  \
	      $(inc)\version.inc        \
	      $(inc)\dosmac.inc         \
	      $(inc)\bpb.inc            \
	      $(inc)\dirent.inc         \
	      $(inc)\dpb.inc            \
	      $(inc)\curdir.inc         \
	      $(inc)\cpmfcb.inc         \
	      $(inc)\syscall.inc        \
	      $(inc)\ioctl.inc          \
	      forequ.inc                \
	      formacro.inc              \
	      forswtch.inc              \
	      safedef.inc

forinit.obj:  forinit.asm               \
	      makefile                  \
	      forequ.inc                \
	      formacro.inc              \
	      $(inc)\syscall.inc        \
	      $(inc)\ioctl.inc          \
	      forparse.inc              \
	      forswtch.inc              \
	      $(inc)\parse.asm          \
	      $(inc)\version.inc        \
	      $(inc)\psdata.inc

msfor.obj:    msfor.asm                 \
	      makefile                  \
	      $(inc)\dosmac.inc         \
	      $(inc)\syscall.inc        \
	      $(inc)\bpb.inc            \
	      $(inc)\dirent.inc         \
	      boot.cl1                  \
	      $(inc)\ioctl.inc          \
	      $(boot)\boot11.inc        \
	      $(inc)\bootform.inc       \
	      $(boot)\boot.inc          \
	      filesize.inc              \
	      forequ.inc                \
	      formacro.inc              \
	      forswtch.inc

forproc.obj:  forproc.asm               \
	      makefile                  \
	      $(inc)\syscall.inc        \
	      forequ.inc                \
	      formacro.inc              \
	      forswtch.inc

glblinit.obj: glblinit.asm              \
	      makefile                  \
	      $(inc)\bpb.inc            \
	      $(inc)\dosmac.inc         \
	      $(inc)\syscall.inc        \
	      $(inc)\ioctl.inc          \
	      forequ.inc                \
	      formacro.inc              \
	      forswtch.inc              \
	      safedef.inc

phase1.obj:   phase1.asm                \
	      makefile                  \
	      $(inc)\dosmac.inc         \
	      $(inc)\dirent.inc         \
	      $(inc)\cpmfcb.inc         \
	      $(inc)\error.inc          \
	      $(inc)\syscall.inc        \
	      $(inc)\ioctl.inc          \
	      $(inc)\bpb.inc            \
	      forequ.inc                \
	      formacro.inc

dskfrmt.obj:  dskfrmt.asm               \
	      makefile                  \
	      $(inc)\version.inc        \
	      $(inc)\dosmac.inc         \
	      $(inc)\bpb.inc            \
	      $(inc)\error.inc          \
	      $(inc)\syscall.inc        \
	      $(inc)\ioctl.inc          \
	      forequ.inc                \
	      formacro.inc              \
	      forswtch.inc

switch_s.obj: switch_s.asm              \
	      makefile                  \
	      $(inc)\dirent.inc         \
	      $(inc)\dosmac.inc         \
	      $(inc)\error.inc          \
	      $(inc)\bpb.inc            \
	      $(inc)\syscall.inc        \
	      $(inc)\sysvar.inc         \
	      forequ.inc                \
	      formacro.inc

path.obj:     $(inc)\path.asm           \
	      $(inc)\dossym.inc         \
	      $(inc)\curdir.inc         \
	      $(inc)\find.inc           \
	      $(inc)\pdb.inc            \
	      $(inc)\syscall.inc
	      $(asm) $(ainc) $(aflags) $(inc)\path.asm,path.obj;

# M027 Bug2649
mir_main.obj: $(mirror)\mir_main.asm    \
	      makefile                  \
	      $(MIR_MSG)\mir_msg.inc
	      copy $(mirror)\mir_main.asm
	      $(asm) $(ainc) $(aflags) -I$(MIR_MSG) $*.asm;
# M027
