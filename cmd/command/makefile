#************************** makefile for cmd\command *********************
MSG_DIR=$(LANG_SRC)\$(COUNTRY)\messages

msg     =..\..\messages
inc     =..\..\inc
bins    =..\..\binaries
cprs    =..\..\compress
#extasw  =-I$(inc) -I$(msg) -I$(MSG_DIR) -D$(COUNTRY)

!IFDEF ROMDOS
loc	=..\..\romimg
incloc  =-I$(loc)
!ENDIF

!IFNDEF ROMDOS
dest    =command.com
cpr     =command.co_
sto     =diskobjs
!ELSE
dest    =command.16
cpr     =command.bi_
sto     =romobjs
!ENDIF


!ifdef DEBUG

deb     =-Zi
deblink =/LI /MAP /CO

!endif

exelink =$(deblink)

!IFDEF BASE
extasw  =-I$(inc) -I$(msg) -I$(MSG_DIR) -D$(COUNTRY) $(incloc) $(deb) -DOEMBASE
bin2    =command.bse
cpr2    =command.bs_
!ELSE
extasw  =-I$(inc) -I$(msg) -I$(MSG_DIR) -D$(COUNTRY) $(incloc) $(deb) -DDBLSPACE_HOOKS
bin2    =command.com
cpr2    =command.co_
!ENDIF

#
####################### dependencies begin here. #########################
#

!IFDEF        ROMDOS

all: $(dest) romhead.bin rescom.16 Copy.EM

!ELSE

!IFNDEF       BUILDER

all: $(dest) Copy.EM

!ELSE

all: $(dest) $(bins)\$(bin2) $(cprs)\$(bin2) Copy.EM

$(bins)\$(bin2): $(dest)
	    copy $(dest) $(bins)\$(bin2)

$(cprs)\$(bin2): $(dest)
	    copy $(dest) $(cprs)\$(bin2)

$(cprs)\$(cpr2):  $(cpr)
	    copy $(cpr) $(cprs)\$(cpr2)

$(cpr):       $(dest)
	    $(compress) $(dest) $(cpr)

!ENDIF

!ENDIF


Copy.EM:
#	copy *.obj $(sto)

# buildmsg.exe:  buildmsg.c IPG- BUILDMSG is now built in TOOLS\BIN\SOURCE\BUILDMSG
#               cl buildmsg.c -link /stack:4096

command.ctl:   command.skl              \
	       $(msg)\$(COUNTRY).msg
#	       buildmsg.exe

command1.obj:  command1.asm             \
	       $(inc)\dossym.inc        \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\syscall.inc       \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\curdir.inc        \
	       comequ.asm               \
	       comseg.asm               \
               resmsg.equ

command2.obj:  command2.asm             \
	       $(inc)\dossym.inc        \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\pdb.inc           \
	       $(inc)\syscall.inc       \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\curdir.inc        \
	       comseg.asm               \
	       comequ.asm               \
	       resmsg.equ

copy.obj:      copy.asm                 \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dossym.inc        \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\syscall.inc       \
	       $(inc)\sf.inc            \
	       comseg.asm               \
	       $(inc)\curdir.inc        \
	       comequ.asm

copypr1.obj:   copypr1.asm              \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dossym.inc        \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\syscall.inc       \
	       $(inc)\sf.inc            \
	       comseg.asm               \
	       $(inc)\curdir.inc        \
	       comequ.asm

copypr2.obj:   copypr2.asm              \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dossym.inc        \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\syscall.inc       \
	       $(inc)\sf.inc            \
	       comseg.asm               \
	       $(inc)\curdir.inc        \
	       comequ.asm

cparse.obj:    cparse.asm               \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dossym.inc        \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\devsym.inc        \
	       comseg.asm               \
	       $(inc)\curdir.inc        \
	       comequ.asm

init.obj:      init.asm                 \
	       $(inc)\dossym.inc        \
	       $(inc)\pdb.inc           \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\mult.inc          \
	       $(inc)\syscall.inc       \
	       $(inc)\doscntry.inc      \
	       comsw.asm                \
               $(inc)\version.inc       \
	       comseg.asm               \
	       $(inc)\curdir.inc        \
	       comequ.asm               \
	       resmsg.equ               \
               $(inc)\msbdata.inc       \
	       envdata.asm

iparse.obj:    iparse.asm               \
	       comseg.asm               \
	       $(inc)\parse.asm         \
	       $(inc)\psdata.inc

parse2.obj:    parse2.asm               \
	       $(inc)\dossym.inc        \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\devsym.inc        \
	       comsw.asm                \
               $(inc)\version.inc       \
	       comseg.asm               \
	       $(inc)\curdir.inc        \
	       comequ.asm

path1.obj:     path1.asm                \
	       $(inc)\dossym.inc        \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\syscall.inc       \
	       comsw.asm                \
               $(inc)\version.inc       \
	       comseg.asm               \
	       $(inc)\curdir.inc        \
	       comequ.asm

path2.obj:     path2.asm                \
	       $(inc)\dossym.inc        \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\syscall.inc       \
	       $(inc)\find.inc          \
	       comsw.asm                \
               $(inc)\version.inc       \
	       comseg.asm               \
	       $(inc)\curdir.inc        \
	       comequ.asm

rdata.obj:     rdata.asm                \
               comequ.asm               \
	       comsw.asm                \
               $(inc)\version.inc       \
	       comseg.asm               \
	       resmsg.equ               \
!IFDEF ROMDOS
               $(loc)\command.loc       \
!ENDIF
	       $(MSG_DIR)\comrmsg.inc	\
	       $(inc)\highvar.inc

stub.obj:      stub.asm                 \
	       comseg.asm       	\
	       $(inc)\highexit.inc	\
	       $(inc)\highvar.inc

rucode.obj:    rucode.asm               \
	       $(inc)\dossym.inc        \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\syscall.inc       \
	       $(inc)\filemode.inc      \
	       $(inc)\pdb.inc           \
	       $(inc)\mult.inc          \
	       $(inc)\doscntry.inc      \
	       $(inc)\devsym.inc        \
	       comsw.asm                \
               $(inc)\version.inc       \
	       comseg.asm               \
	       $(inc)\curdir.inc        \
	       comequ.asm               \
	       resmsg.equ

tbatch.obj:    tbatch.asm               \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dossym.inc        \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\syscall.inc       \
	       $(inc)\doscntry.inc      \
	       comseg.asm               \
	       $(inc)\curdir.inc        \
	       comequ.asm

tbatch2.obj:   tbatch2.asm              \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dossym.inc        \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\syscall.inc       \
	       comseg.asm               \
	       $(inc)\curdir.inc        \
	       comequ.asm

dir.obj:       dir.asm                  \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dossym.inc        \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\syscall.inc       \
	       $(inc)\bpb.inc           \
	       $(inc)\filemode.inc      \
	       $(inc)\find.inc          \
	       $(inc)\ioctl.inc         \
	       $(inc)\devsym.inc        \
	       $(inc)\rombios.inc       \
	       comseg.asm               \
	       $(inc)\curdir.inc        \
	       comequ.asm               \
	       ifequ.asm

cratio.obj:    cratio.asm		\
	       comseg.asm		\
	       comequ.asm		\
	       $(inc)\dossym.inc	 \
	       $(inc)\syscall.inc	\
	       $(inc)\magicdrv.inc


tcmd1b.obj:    tcmd1b.asm               \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dossym.inc        \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\bpb.inc           \
	       $(inc)\syscall.inc       \
	       $(inc)\filemode.inc      \
	       $(inc)\sf.inc            \
	       $(inc)\ioctl.inc         \
	       comseg.asm               \
	       $(inc)\curdir.inc        \
	       comequ.asm               \
	       $(inc)\ea.inc

tcmd2a.obj:    tcmd2a.asm               \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dossym.inc        \
	       $(inc)\bpb.inc \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\syscall.inc       \
	       $(inc)\filemode.inc      \
	       $(inc)\sf.inc            \
	       $(inc)\ioctl.inc         \
	       $(inc)\curdir.inc        \
	       $(inc)\rombios.inc       \
	       comseg.asm comequ.asm

tcmd2b.obj:    tcmd2b.asm comsw.asm     \
	       $(inc)\dossym.inc        \
	       $(inc)\syscall.inc       \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\curdir.inc        \
	       $(inc)\pdb.inc           \
	       $(inc)\mult.inc          \
	       comseg.asm               \
	       comequ.asm

tcode.obj:     tcode.asm                \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dossym.inc        \
	       $(inc)\syscall.inc       \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\curdir.inc        \
	       comseg.asm               \
	       comequ.asm               \
	       $(inc)\vector.inc        \
	       $(inc)\pdb.inc           \
	       $(inc)\arena.inc         \
	       $(inc)\mshalo.asm

tdata.obj:     tdata.asm                \
	       comsw.asm                \
               $(inc)\version.inc       \
	       comseg.asm               \
	       $(inc)\dirent.inc        \
	       $(inc)\curdir.inc        \
	       $(inc)\error.inc         \
	       ifequ.asm                \
	       comequ.asm               \
	       $(inc)\sysmsg.inc        \
	       $(inc)\versiona.inc      \
	       command.ctl              \
	       $(inc)\msgdcl.inc        \
	       $(inc)\msgserv.asm       \
	       tranmsg.asm              \
	       $(inc)\ea.inc

tenv.obj:      tenv.asm                 \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dossym.inc        \
	       $(inc)\syscall.inc       \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\curdir.inc        \
	       $(inc)\arena.inc         \
	       comseg.asm               \
	       comequ.asm               \
	       $(inc)\doscntry.inc

tenv2.obj:     tenv2.asm                \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dossym.inc        \
	       $(inc)\syscall.inc       \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\curdir.inc        \
	       $(inc)\find.inc          \
	       comseg.asm               \
	       comequ.asm

tfor.obj:      tfor.asm                 \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dossym.inc        \
	       $(inc)\syscall.inc       \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\curdir.inc        \
	       $(inc)\find.inc          \
	       $(inc)\devsym.inc        \
	       comseg.asm               \
	       comequ.asm               \
	       fordata.asm

tmisc1.obj:    tmisc1.asm               \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dossym.inc        \
	       $(inc)\syscall.inc       \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\curdir.inc        \
	       comseg.asm               \
	       comequ.asm

tmisc2.obj:    tmisc2.asm               \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dossym.inc        \
	       $(inc)\syscall.inc       \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\pdb.inc           \
	       $(inc)\sf.inc            \
	       $(inc)\ioctl.inc         \
	       $(inc)\curdir.inc        \
	       comseg.asm               \
	       comequ.asm

tparse.obj:    tparse.asm               \
	       comsw.asm                \
               $(inc)\version.inc       \
	       comseg.asm               \
	       $(inc)\parse.asm         \
	       $(inc)\psdata.inc

tpipe.obj:     tpipe.asm                \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dossym.inc        \
	       $(inc)\syscall.inc       \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\curdir.inc        \
	       $(inc)\pdb.inc           \
	       comseg.asm               \
	       comequ.asm


tprintf.obj:   tprintf.asm              \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dosmac.inc        \
	       comseg.asm               \
	       comequ.asm               \
	       $(inc)\curdir.inc        \
	       $(inc)\sysmsg.inc        \
	       $(inc)\versiona.inc      \
	       command.ctl              \
	       $(inc)\msgdcl.inc        \
	       $(inc)\msgserv.asm

tspc.obj:      tspc.asm                 \
	       comsw.asm                \
               $(inc)\version.inc       \
	       $(inc)\dossym.inc        \
	       $(inc)\find.inc          \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\curdir.inc        \
	       $(inc)\intnat.inc        \
	       comequ.asm               \
	       comseg.asm               \
	       $(inc)\ea.inc		\
	       $(inc)\magicdrv.inc

tucode.obj:    tucode.asm               \
	       $(inc)\dossym.inc        \
	       $(inc)\syscall.inc       \
	       $(inc)\dbcs.sw           \
	       $(inc)\dosmac.inc        \
	       $(inc)\versiona.inc      \
	       $(inc)\buffer.inc        \
	       $(inc)\dirent.inc        \
	       $(inc)\error.inc         \
	       $(inc)\curdir.inc        \
	       comsw.asm                \
               $(inc)\version.inc       \
	       comseg.asm               \
	       comequ.asm

uinit.obj:     uinit.asm                \
	       comsw.asm                \
               $(inc)\version.inc       \
	       comseg.asm               \
	       ifequ.asm                \
	       $(MSG_DIR)\comimsg.inc

loadhi.obj:     loadhi.asm              \
		comseg.asm              \
		comequ.asm              \
		$(inc)\dossym.inc       \
		$(inc)\syscall.inc      \
		$(inc)\arena.inc	\
		$(inc)\highload.inc	\
		$(inc)\highvar.inc

!IFDEF ROMDOS

romldr.obj:	romldr.asm $(loc)\command.loc

romres.obj:	romres.asm $(loc)\command.loc comseg.asm

!ENDIF

!IFNDEF ROMDOS

command.exc:   command1.obj   tenv2.obj      \
	       command2.obj   tmisc1.obj     \
	       rucode.obj     tmisc2.obj     \
	       rdata.obj      tpipe.obj      \
	       init.obj       parse2.obj     \
	       iparse.obj     path1.obj      \
	       uinit.obj      path2.obj      \
	       tcode.obj      tucode.obj     \
	       tbatch.obj     copy.obj       \
	       tbatch2.obj    copypr1.obj    \
	       tfor.obj       copypr2.obj    \
	       dir.obj        cparse.obj     \
	       tcmd1b.obj     tparse.obj     \
	       tcmd2a.obj     tprintf.obj    \
	       tcmd2b.obj     tdata.obj      \
	       tenv.obj       tspc.obj       \
	       stub.obj       loadhi.obj     \
	       cratio.obj		     \
               command.lnk
	       $(link) $(exelink) @command.lnk

$(dest):       command.exc

!ELSE

command.exc command.map:   command1.obj   tenv2.obj      \
	       command2.obj   tmisc1.obj     \
	       rucode.obj     tmisc2.obj     \
	       rdata.obj      tpipe.obj      \
	       init.obj       parse2.obj     \
	       iparse.obj     path1.obj      \
	       uinit.obj      path2.obj      \
	       tcode.obj      tucode.obj     \
	       tbatch.obj     copy.obj       \
	       tbatch2.obj    copypr1.obj    \
	       tfor.obj       copypr2.obj    \
	       dir.obj        cparse.obj     \
	       tcmd1b.obj     tparse.obj     \
	       tcmd2a.obj     tprintf.obj    \
	       tcmd2b.obj     tdata.obj      \
	       tenv.obj       tspc.obj       \
	       stub.obj       loadhi.obj     \
	       romldr.obj     romres.obj     \
	       cratio.obj		     \
               rom.lnk
	       $(link) $(exelink) @rom.lnk

command.com:   command.exc

command.16:    command.com
               copy command.com command.16

romhead.bin:   romhead.asm $(loc)\command.loc
               $(asm) $(aflags) romhead,,;
               link /nologo /li romhead,,;
               reloc romhead.exe romhead.bin

rescom.16:     command.com command.map
	       addpsp command.com command.tmp
	       split command.tmp command.map INIT rescom1.tmp rescom2.tmp
               split rescom1.tmp command.map CODERES rescom3.tmp rescom.16
               del rescom?.tmp
	       del command.tmp

!ENDIF


