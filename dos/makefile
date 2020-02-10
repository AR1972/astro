###     makefile for MS-DOS
#
#       The variable MFLAGS can be used to pass switches to MASM.
#       For example:
#           nmake MFLAGS=-DRESTRICTED_BUILD
#

!IF     "$(DEBUG)" == "soft_ice" || "$(debug)" == "soft_ice" || \
	"$(DEBUG)" == "SOFT_ICE" || "$(debug)" == "SOFT_ICE"

debug_flags = -Zd
link_opts = /MAP /LI

!ELSE

link_opts = /MAP

!ENDIF

dest    =msdos.sys
cpr     =msdos.sy_
sto     =diskobjs

msg     =..\messages
inc     =..\inc
bins    =..\binaries
cprs    =..\compress

!IFDEF BASE
dest2   =msdos.bse
!ELSE
dest2   =msdos.sys
!ENDIF

extasw  =-I$(inc) $(debug_flags) $(MFLAGS) -D$(COUNTRY)

#
###################### Dependencies begin here ##########################
#

!IFNDEF       BUILDER

all: Build.IO $(dest) Copy.EM

!ELSE

all: Build.IO $(dest) $(bins)\$(dest2) $(cprs)\$(dest2) Copy.EM

$(bins)\$(dest2): $(dest)
	    copy $(dest) $(bins)\$(dest2)

$(cprs)\$(dest2): $(dest)
	    copy $(dest) $(cprs)\$(dest2)

!ENDIF


Build.IO:
	cd ..\bios
	$(MAKE) /a BASE=
	$(MAKE) /a
	cd ..\dos

Copy.EM:
	copy *.obj $(sto)

msdos.cl1: msdos.skl \
	$(msg)\$(COUNTRY).msg

msdos.cl2: msdos.skl \
	$(msg)\$(COUNTRY).msg


dossym.inc: $(inc)\dosmac.inc $(inc)\bpb.inc \
    $(inc)\buffer.inc $(inc)\sysvar.inc $(inc)\vector.inc \
    $(inc)\mult.inc $(inc)\dirent.inc $(inc)\dpb.inc $(inc)\curdir.inc \
    $(inc)\cpmfcb.inc $(inc)\find.inc $(inc)\pdb.inc $(inc)\exe.inc \
    $(inc)\sf.inc $(inc)\arena.inc $(inc)\intnat.inc $(inc)\mi.inc \
    $(inc)\filemode.inc $(inc)\error.inc $(inc)\syscall.inc
	echo "touch dossym.inc; files that are in ..\inc"

msdata.obj:   msdata.asm                \
	      mssw.asm                  \
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
	      dostab.asm                \
	      $(inc)\const2.asm         \
	      msconst.asm               \
	      $(inc)\mshead.asm         \
	      $(inc)\BUGTYP.INC         \
	      $(inc)\ORIGIN.INC         \
	      $(inc)\bdsize.inc         \
	      $(inc)\wpatch.inc         \
	      $(inc)\msbdata.inc        \
	      mpatch.asm                \
	      msdos.cl2

msdosme.obj:  msdosme.asm               \
	      dosmes.inc                \
	      msdos.cl1                 \
	      mssw.asm                  \
	      $(inc)\VERSIONA.INC       \
	      $(inc)\buffer.INC         \
	      $(inc)\dbcs.sw            \
	      $(inc)\dirent.INC         \
	      divmes.inc                \
	      $(inc)\doscntry.inc       \
	      $(inc)\dosmac.INC         \
	      $(inc)\dosmac.inc         \
	      $(inc)\dosseg.inc         \
	      $(inc)\dossym.inc         \
	      $(inc)\error.INC          \
	      $(inc)\version.inc        \
	      $(inc)\yesno.asm

abort.obj abort.lst: abort.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/error.INC $(inc)/mult.inc $(inc)/pdb.inc $(inc)/sf.inc

alloc.obj alloc.lst: alloc.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/arena.inc $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/error.INC

buf.obj buf.lst: buf.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc $(inc)/dpb.inc \
	$(inc)/error.INC $(inc)/version.inc

close.obj close.lst: close.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/bugtyp.inc \
	$(inc)/dbcs.sw $(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/error.INC $(inc)/mult.inc $(inc)/sf.inc

cpmio.obj cpmio.lst: cpmio.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/doscntry.inc $(inc)/dosmac.INC \
	$(inc)/dosseg.inc $(inc)/error.INC $(inc)/sf.inc $(inc)/vector.inc \
	kstrin.asm

cpmio2.obj cpmio2.lst: cpmio2.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/error.INC $(inc)/sf.inc

create.obj create.lst: create.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/curdir.inc \
	$(inc)/dbcs.sw $(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/error.INC $(inc)/filemode.inc $(inc)/mult.inc $(inc)/sf.inc \
	$(inc)/version.inc

crit.obj crit.lst: crit.asm $(inc)/DOSSYM.INC $(inc)/VERSIONA.INC \
	$(inc)/buffer.INC $(inc)/bugtyp.inc $(inc)/dbcs.sw $(inc)/dirent.INC \
	$(inc)/dosmac.INC $(inc)/dosseg.inc $(inc)/error.INC $(inc)/int2a.inc \
	$(inc)/vector.inc

delete.obj delete.lst: delete.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/FASTOPEN.INC $(inc)/VERSIONA.INC $(inc)/buffer.INC \
	$(inc)/dbcs.sw $(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/dpb.inc $(inc)/error.INC $(inc)/fastxxxx.inc \
	$(inc)/filemode.inc $(inc)/mult.inc $(inc)/sf.inc

dev.obj dev.lst: dev.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc $(inc)/dpb.inc \
	$(inc)/error.INC $(inc)/sf.inc $(inc)/version.inc

dinfo.obj dinfo.lst: dinfo.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/bugtyp.inc \
	$(inc)/dbcs.sw $(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/dpb.inc $(inc)/error.INC $(inc)/mult.inc

dir.obj dir.lst: dir.asm $(inc)/VERSIONA.INC $(inc)/buffer.INC \
	$(inc)/dbcs.sw $(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/dossym.inc $(inc)/dpb.inc $(inc)/error.INC $(inc)/fastopen.inc

dir2.obj dir2.lst: dir2.asm $(inc)/VERSIONA.INC $(inc)/buffer.INC \
	$(inc)/curdir.inc $(inc)/dbcs.sw $(inc)/devsym.inc $(inc)/dirent.INC \
	$(inc)/dosmac.INC $(inc)/dosseg.inc $(inc)/dossym.inc $(inc)/dpb.inc \
	$(inc)/error.INC $(inc)/fastopen.inc

dircall.obj dircall.lst: dircall.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/FASTOPEN.INC $(inc)/FASTXXXX.INC $(inc)/VERSIONA.INC \
	$(inc)/buffer.INC $(inc)/curdir.inc $(inc)/dbcs.sw $(inc)/dirent.INC \
	$(inc)/dosmac.INC $(inc)/dosseg.inc $(inc)/dpb.inc $(inc)/error.INC \
	$(inc)/mult.inc $(inc)/sf.inc

disk.obj disk.lst: disk.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc $(inc)/dpb.inc \
	$(inc)/error.INC $(inc)/fastxxxx.inc $(inc)/filemode.inc \
	$(inc)/mult.inc $(inc)/sf.inc $(inc)/version.inc

disk2.obj disk2.lst: disk2.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc $(inc)/dpb.inc \
	$(inc)/error.INC $(inc)/filemode.inc $(inc)/sf.inc $(inc)/version.inc

disk3.obj disk3.lst: disk3.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc $(inc)/dpb.inc \
	$(inc)/error.INC $(inc)/sf.inc $(inc)/version.inc

dosprint.obj dosprint.lst: dosprint.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/error.INC $(inc)/version.inc

dup.obj dup.lst: dup.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/error.INC $(inc)/sf.inc $(inc)/version.inc

fat.obj fat.lst: fat.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/curdir.inc \
	$(inc)/dbcs.sw $(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/dpb.inc $(inc)/error.INC $(inc)/sf.inc

fcb.obj fcb.lst: fcb.asm $(inc)/VERSIONA.INC $(inc)/buffer.INC \
	$(inc)/dbcs.sw $(inc)/devsym.inc $(inc)/dirent.INC \
	$(inc)/doscntry.inc $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/dossym.inc $(inc)/error.INC $(inc)\version.inc mssw.asm

fcbio.obj fcbio.lst: fcbio.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/bugtyp.inc \
	$(inc)/cpmfcb.inc $(inc)/dbcs.sw $(inc)/dirent.INC $(inc)/dosmac.INC \
	$(inc)/dosseg.inc $(inc)/dpb.inc $(inc)/error.INC $(inc)/filemode.inc \
	$(inc)/mult.inc $(inc)/sf.inc $(inc)/syscall.inc $(inc)/version.inc

fcbio2.obj fcbio2.lst: fcbio2.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/bugtyp.inc \
	$(inc)/cpmfcb.inc $(inc)/dbcs.sw $(inc)/dirent.INC $(inc)/dosmac.INC \
	$(inc)/dosseg.inc $(inc)/error.INC $(inc)/filemode.inc $(inc)/sf.inc \
	$(inc)/version.inc

file.obj file.lst: file.asm $(inc)/EA.inc $(inc)/VERSIONA.INC \
	$(inc)/buffer.INC $(inc)/bugtyp.inc $(inc)/dbcs.sw $(inc)/devsym.inc \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/dossym.inc $(inc)/error.INC $(inc)/filemode.inc $(inc)/sf.inc \
	$(inc)/version.inc

finfo.obj finfo.lst: finfo.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/error.INC $(inc)/fastopen.inc $(inc)/fastxxxx.inc \
	$(inc)/mult.inc

getset.obj getset.lst: getset.asm $(inc)/VERSIONA.INC $(inc)/buffer.INC \
	$(inc)/dbcs.sw $(inc)/devsym.inc $(inc)/dirent.INC \
	$(inc)/doscntry.inc $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/dossym.inc $(inc)/error.INC $(inc)/mult.inc $(inc)/version.inc

handle.obj handle.lst: handle.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/EA.inc $(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/bugtyp.inc \
	$(inc)/dbcs.sw $(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/error.INC $(inc)/filemode.inc $(inc)/mult.inc $(inc)/pdb.inc \
	$(inc)/sf.inc $(inc)/syscall.inc $(inc)/version.inc

ioctl.obj ioctl.lst: ioctl.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/bpb.inc $(inc)/buffer.INC \
	$(inc)/curdir.inc $(inc)/dbcs.sw $(inc)/dirent.INC $(inc)/dosmac.INC \
	$(inc)/dosseg.inc $(inc)/dpb.inc $(inc)/error.INC \
	$(inc)/ioctl.inc $(inc)/mult.inc $(inc)/sf.inc $(inc)/vector.inc

isearch.obj isearch.lst: isearch.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/curdir.inc \
	$(inc)/dbcs.sw $(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/dpb.inc $(inc)/error.INC $(inc)/fastopen.inc \
	$(inc)/fastxxxx.inc $(inc)/mult.inc

lock.obj lock.lst: lock.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc $(inc)/dpb.inc \
	$(inc)/error.INC $(inc)/lock.inc $(inc)/mult.inc $(inc)/sf.inc \
	$(inc)/version.inc

macro.obj macro.lst: macro.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/curdir.inc \
	$(inc)/dbcs.sw $(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/dpb.inc $(inc)/error.INC $(inc)/mult.inc

macro2.obj macro2.lst: macro2.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/curdir.inc \
	$(inc)/dbcs.sw $(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/error.INC $(inc)/mult.inc

misc.obj misc.lst: misc.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/bpb.inc $(inc)/buffer.INC \
	$(inc)/bugtyp.inc $(inc)/curdir.inc $(inc)/dbcs.sw $(inc)/dirent.INC \
	$(inc)/dosmac.INC $(inc)/dosseg.inc $(inc)/dpb.inc $(inc)/error.INC \
	$(inc)/filemode.inc $(inc)/mi.inc $(inc)/mult.inc $(inc)/pdb.inc \
	$(inc)/sf.inc $(inc)/vector.inc $(inc)/version.inc

misc2.obj misc2.lst: misc2.asm $(inc)/VERSIONA.INC $(inc)/buffer.INC \
	$(inc)/bugtyp.inc $(inc)/curdir.inc $(inc)/dbcs.sw $(inc)/dirent.INC \
	$(inc)/dosmac.INC $(inc)/dosseg.inc $(inc)/dossym.inc \
	$(inc)/error.INC $(inc)/fastxxxx.inc $(inc)/sf.inc $(inc)/sysvar.inc \
	$(inc)/version.inc

mknode.obj mknode.lst: mknode.asm $(inc)/VERSIONA.INC $(inc)/buffer.INC \
	$(inc)/dbcs.sw $(inc)/devsym.inc $(inc)/dirent.INC $(inc)/dosmac.INC \
	$(inc)/dosseg.inc $(inc)/dossym.inc $(inc)/dpb.inc $(inc)/error.INC \
	$(inc)/fastopen.inc $(inc)/filemode.inc $(inc)/sf.inc

mscode.obj mscode.lst: mscode.asm $(inc)/VERSIONA.INC $(inc)/buffer.INC \
	$(inc)/curdir.inc $(inc)/dbcs.sw $(inc)/devsym.inc $(inc)/dirent.INC \
	$(inc)/dosmac.INC $(inc)/dosseg.inc $(inc)/dossym.inc $(inc)/dpb.inc \
	$(inc)/error.INC $(inc)/fastopen.inc $(inc)/fastxxxx.inc \
	$(inc)/mi.inc $(inc)/msbdata.inc $(inc)/mult.inc \
	$(inc)/vector.inc $(inc)/version.inc mssw.asm $(inc)\win386.inc

msctrlc.obj msctrlc.lst: msctrlc.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/bugtyp.inc \
	$(inc)/dbcs.sw $(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/dpb.inc $(inc)/error.INC $(inc)/exe.inc $(inc)/filemode.inc \
	$(inc)/mi.inc $(inc)/mult.inc $(inc)/pdb.inc $(inc)/sf.inc \
	$(inc)/syscall.inc $(inc)/vector.inc $(inc)/version.inc mssw.asm

msdisp.obj msdisp.lst: msdisp.asm $(inc)/DOSSYM.INC $(inc)/VERSIONA.INC \
	$(inc)/buffer.INC $(inc)/bugtyp.inc $(inc)/dbcs.sw $(inc)/dirent.INC \
	$(inc)/dosmac.INC $(inc)/dosseg.inc $(inc)/error.INC $(inc)/mi.inc \
	$(inc)/pdb.inc $(inc)/syscall.inc $(inc)/vector.inc \
	$(inc)/version.inc mssw.asm scnam.inc

msinit.obj msinit.lst: msinit.asm $(inc)/arena.inc $(inc)/buffer.inc \
	$(inc)/curdir.inc $(inc)/devsym.inc $(inc)/dirent.inc \
	$(inc)/doscntry.inc $(inc)/dosmac.inc $(inc)/dosseg.inc \
	$(inc)/dpb.inc $(inc)/fastopen.inc $(inc)/find.inc $(inc)/mi.inc \
	$(inc)/pdb.inc $(inc)/sf.inc $(inc)/sysvar.inc $(inc)/vector.inc \
	$(inc)/version.inc $(inc)/xmm.inc mssw.asm $(inc)/dossym.inc \
	$(inc)/bugtyp.inc $(inc)\win386.inc

open.obj open.lst: open.asm $(inc)/VERSIONA.INC $(inc)/buffer.INC \
	$(inc)/curdir.inc $(inc)/dbcs.sw $(inc)/devsym.inc $(inc)/dirent.INC \
	$(inc)/dosmac.INC $(inc)/dosseg.inc $(inc)/dossym.inc \
	$(inc)/error.INC $(inc)/fastopen.inc $(inc)/fastxxxx.inc \
	$(inc)/filemode.inc $(inc)/mult.inc $(inc)/sf.inc

origin.obj origin.lst: origin.asm $(inc)/DOSSEG.INC $(inc)/ORIGIN.INC \
	$(inc)/dossym.inc $(inc)/bdsize.inc $(inc)/version.inc

parse.obj parse.lst: parse.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/error.INC

path.obj path.lst: path.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/curdir.inc \
	$(inc)/dbcs.sw $(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/dpb.inc $(inc)/error.INC $(inc)/filemode.inc $(inc)/mult.inc \
	$(inc)/version.inc

msproc.obj proc.lst: msproc.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/EA.INC $(inc)/VERSIONA.INC $(inc)/arena.inc $(inc)/buffer.INC \
	$(inc)/curdir.inc $(inc)/dbcs.sw $(inc)/dirent.INC $(inc)/dosmac.INC \
	$(inc)/dosseg.inc $(inc)/error.INC $(inc)/exe.inc $(inc)/pdb.inc \
	$(inc)/sf.inc $(inc)/syscall.inc $(inc)/vector.inc restrict.inc \
	$(inc)/version.inc

exepatch.obj exepatch.lst: exepatch.asm $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/dosseg.inc  $(inc)/exe.inc \
	$(inc)/version.inc


rename.obj rename.lst: rename.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/error.INC $(inc)/filemode.inc $(inc)/find.inc $(inc)/mult.inc \
	$(inc)/sf.inc

rom.obj rom.lst: rom.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC $(inc)/EA.inc \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc $(inc)/dpb.inc \
	$(inc)/error.INC $(inc)/fastxxxx.inc $(inc)/sf.inc $(inc)/version.inc

romfind.obj romfind.lst: romfind.asm $(inc)/version.inc $(inc)/dossym.inc \
	$(inc)/dosmac.inc $(inc)/dosseg.inc $(inc)/mi.inc $(inc)/error.inc \
	

search.obj search.lst: search.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/error.INC $(inc)/fastopen.inc $(inc)/fastxxxx.inc

segcheck.obj segcheck.lst: segcheck.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/error.INC $(inc)/dpb.inc $(inc)/version.inc $(inc)/sf.inc \
	$(inc)/int2a.inc

share.obj share.lst: share.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc $(inc)/dpb.inc \
	$(inc)/error.INC $(inc)/sf.inc

srvcall.obj srvcall.lst: srvcall.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc $(inc)/dpl.asm \
	$(inc)/error.INC $(inc)/mult.inc $(inc)/sf.inc

time.obj time.lst: time.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/dbcs.sw \
	$(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/error.INC $(inc)/version.inc

util.obj util.lst: util.asm $(inc)/DEVSYM.INC $(inc)/DOSSYM.INC \
	$(inc)/VERSIONA.INC $(inc)/buffer.INC $(inc)/bugtyp.inc \
	$(inc)/dbcs.sw $(inc)/dirent.INC $(inc)/dosmac.INC $(inc)/dosseg.inc \
	$(inc)/error.INC $(inc)/pdb.inc $(inc)/sf.inc $(inc)/version.inc

mstable.obj mstable.lst: mstable.asm mssw.asm \
	$(inc)\EA.inc $(inc)\VERSIONA.INC $(inc)\buffer.INC \
	$(inc)\copyrigh.inc $(inc)\dbcs.sw $(inc)\dirent.INC \
	$(inc)\dosmac.INC $(inc)\dosseg.inc $(inc)\dossym.inc \
	$(inc)\error.INC $(inc)\fastopen.inc $(inc)\syscall.inc \
	$(inc)\version.inc

msdos.bin: msdos.cl1 msdos.cl2 dosprint.obj exepatch.obj \
    msdata.obj mscode.obj msdosme.obj time.obj getset.obj \
    parse.obj misc.obj misc2.obj crit.obj cpmio.obj cpmio2.obj \
    mstable.obj msdisp.obj fcbio.obj fcbio2.obj segcheck.obj \
    search.obj path.obj ioctl.obj delete.obj rename.obj finfo.obj \
    dup.obj create.obj open.obj dinfo.obj isearch.obj buf.obj \
    abort.obj close.obj dircall.obj disk.obj disk2.obj disk3.obj dir.obj \
    dir2.obj dev.obj mknode.obj rom.obj fcb.obj msctrlc.obj fat.obj \
    msproc.obj alloc.obj srvcall.obj util.obj macro.obj macro2.obj \
    handle.obj file.obj lock.obj romfind.obj share.obj msinit.obj origin.obj \
    makefile msdos.lnk
	link $(link_opts) @msdos.lnk
	reloc msdos.exe msdos.bin


$(dest): msdos.bin
	stripz msdos.bin $(dest)
