#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1981-1991
#; *                      All Rights Reserved.
#; */
#************************** makefile for bios ***************************

!IF	"$(DEBUG)" == "soft_ice" || "$(debug)" == "soft_ice" || \
	"$(DEBUG)" == "SOFT_ICE" || "$(debug)" == "SOFT_ICE"

debug_flags = -Zd
link_opts = /MAP /LI

!ELSE

link_opts = /MAP

!ENDIF

!IFDEF ROMDOS
loc	=..\romimg
incloc  =-I$(loc)
!ENDIF

!IFNDEF		ROMDOS
dest    =io.sys
cpr     =io.sy_
!ELSE
dest    =rombio.sys
cpr     =rombio.sy_
!ENDIF

msg	=..\messages
inc     =..\inc
bins    =..\binaries
cprs    =..\compress

!IFDEF          IBM
ibmboot =-DIBM_VERSION
!ENDIF

!IFDEF BASE
extasw  =-I$(inc) $(debug_flags) $(ibmboot) $(incloc) -D$(COUNTRY) -DOEMBASE
bin2    = io.bse
cpr2    = io.bs_
!ELSE
extasw  =-I$(inc) $(debug_flags) $(ibmboot) $(incloc) -D$(COUNTRY) -DDBLSPACE_HOOKS
bin2    = io.sys
cpr2    = io.sy_
!ENDIF

#
#######################	dependencies begin here. #########################
#

!IFDEF        ROMDOS

all: $(dest)

!ELSE

!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(bin2) $(cprs)\$(bin2)

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


inc.lst: inc.asm

msbio.cl1 msbio.cl2 msbio.cl3 \
msbio.cl4 msbio.cl5 msbio.cl6 \
msbio.cl7 msbio.cl8 msbio.cl9 : msbio.skl \
	$(msg)\$(COUNTRY).msg


inc.obj inc.lst: inc.asm                \
                $(inc)\VERSIONA.inc     \
                $(inc)\arena.inc        \
	        $(inc)\buffer.inc       \
                $(inc)\copyrigh.inc     \
                $(inc)\cpmfcb.inc       \
	        $(inc)\cputype.inc      \
                $(inc)\curdir.inc       \
                $(inc)\dbcs.sw          \
                $(inc)\dirent.inc       \
	        $(inc)\doscntry.inc     \
                $(inc)\dosmac.inc       \
                $(inc)\dosseg.inc       \
	        $(inc)\dossym.inc       \
                $(inc)\dpb.inc          \
                $(inc)\ea.inc           \
                $(inc)\error.inc        \
	        $(inc)\exe.inc          \
                $(inc)\filemode.inc     \
                $(inc)\find.inc         \
                $(inc)\intnat.inc       \
	        $(inc)\mi.inc           \
                $(inc)\mult.inc         \
                $(inc)\pdb.inc          \
                $(inc)\sf.inc           \
	        $(inc)\smifssym.inc     \
                $(inc)\syscall.inc      \
                $(inc)\sysvar.inc       \
	        $(inc)\vector.inc       \
                $(inc)\version.inc      \
                biostruc.inc            \
                $(inc)\bootform.inc     \
                $(inc)\bpb.inc          \
                devmark.inc             \
                $(inc)\devsym.inc       \
                $(inc)\ioctl.inc        \
	        $(inc)\msbds.inc        \
                msdskpr.inc             \
                msequ.inc               \
                msgroup.inc

msbio1.obj msbio1.lst: msbio1.asm       \
                $(inc)\msbdata.inc      \
                $(inc)\bootform.inc     \
	        $(inc)\bpb.inc          \
                $(inc)\devsym.inc       \
                $(inc)\msbds.inc        \
                msequ.inc               \
	        $(inc)\version.inc

msbio2.obj msbio2.lst: msbio2.asm       \
                $(inc)\bootform.inc     \
                $(inc)\bpb.inc          \
	        $(inc)\msbds.inc        \
                $(inc)\version.inc      \
                msbio.cl2               \
                msequ.inc               \
                msgroup.inc

mschar.obj mschar.lst: mschar.asm       \
                $(inc)\bpb.inc          \
                $(inc)\devsym.inc       \
	        $(inc)\ioctl.inc        \
                $(inc)\version.inc      \
                msequ.inc               \
                msgroup.inc

msdioctl.obj msdioctl.lst: msdioctl.asm \
                $(inc)\bootform.inc     \
                $(inc)\bpb.inc          \
	        $(inc)\devsym.inc       \
                $(inc)\ioctl.inc        \
                $(inc)\version.inc      \
                $(inc)\msbds.inc        \
                msdskpr.inc             \
	        msequ.inc               \
                msgroup.inc

msdisk.obj msdisk.lst: msdisk.asm       \
                $(inc)\VERSIONA.inc     \
                $(inc)\buffer.inc       \
	        $(inc)\dbcs.sw          \
                $(inc)\dirent.inc       \
                $(inc)\dosmac.inc       \
                $(inc)\dossym.inc       \
	        $(inc)\error.inc        \
                $(inc)\bootform.inc     \
                $(inc)\version.inc      \
                $(inc)\bpb.inc          \
                $(inc)\msbds.inc        \
	        msdskpr.inc             \
                msequ.inc               \
                msgroup.inc

msend.obj msend.lst: msend.asm          \
                $(inc)\version.inc

msinit.obj msinit.lst: msinit.asm       \
                $(inc)\VERSIONA.inc     \
                $(inc)\buffer.inc       \
	        $(inc)\dbcs.sw          \
                $(inc)\dirent.inc       \
                $(inc)\dosmac.inc       \
                $(inc)\dosmac.inc       \
	        $(inc)\dossym.inc       \
                $(inc)\error.inc        \
                $(inc)\version.inc      \
                biostruc.inc            \
	        $(inc)\bootform.inc     \
                $(inc)\bpb.inc          \
                $(inc)\msbds.inc        \
                msbio.cl8               \
                msdskpr.inc             \
	        msequ.inc               \
                msgroup.inc             \
                $(inc)\romloc.inc

msint13.obj msint13.lst: msint13.asm    \
                biostruc.inc            \
                $(inc)\bpb.inc          \
                $(inc)\version.inc      \
	        $(inc)\msbds.inc        \
                msequ.inc               \
                msgroup.inc

msload.obj msload.lst: msload.asm       \
                $(inc)\versiona.inc     \
                $(inc)\version.inc      \
                $(inc)\bootform.inc     \
	        $(inc)\bpb.inc          \
                msbio.cl1

sysconf.obj sysconf.lst: sysconf.asm    \
                $(inc)\doscntry.inc     \
                $(inc)\dpb.inc          \
	        $(inc)\parse.asm        \
                $(inc)\psdata.inc       \
                $(inc)\smifssym.inc     \
	        $(inc)\syscall.inc      \
                $(inc)\sysvar.inc       \
                $(inc)\version.inc      \
                devmark.inc             \
                $(inc)\devsym.inc	\
		$(inc)\highvar.inc	\
		$(inc)\highload.inc	\
                $(inc)\highexit.inc     \
                $(inc)\magicdrv.inc

sysimes.obj sysimes.lst: sysimes.asm    \
                $(inc)\version.inc      \
                msbio.cl3

sysinit1.obj sysinit1.lst: sysinit1.asm \
                $(inc)\VERSIONA.inc     \
	        $(inc)\arena.inc        \
                $(inc)\buffer.inc       \
                $(inc)\cputype.inc      \
	        $(inc)\curdir.inc       \
                $(inc)\dbcs.sw          \
                $(inc)\dirent.inc       \
                $(inc)\dosmac.inc       \
	        $(inc)\dossym.inc       \
                $(inc)\dpb.inc          \
                $(inc)\error.inc        \
                $(inc)\exe.inc          \
	        $(inc)\pdb.inc          \
                $(inc)\sf.inc           \
                $(inc)\smifssym.inc     \
                $(inc)\syscall.inc      \
	        $(inc)\sysvar.inc       \
                $(inc)\version.inc      \
                biostruc.inc            \
                $(inc)\bpb.inc          \
                devmark.inc             \
                $(inc)\devsym.inc       \
                $(inc)\ioctl.inc        \
                msbio.cl4               \
                msbio.cl5               \
                msbio.cl6               \
                msbio.cl7

sysinit2.obj sysinit2.lst: sysinit2.asm \
                $(inc)\copyrigh.inc     \
	        $(inc)\doscntry.inc     \
                $(inc)\syscall.inc      \
                $(inc)\version.inc      \
	        $(inc)\bpb.inc          \
                devmark.inc             \
                $(inc)\devsym.inc       \
                $(inc)\ioctl.inc

syspre.obj syspre.lst: syspre.asm $(inc)\ds_vers.inc $(inc)\version.inc


power.obj power.lst: power.asm          \
                power.inc               \
                msequ.inc               \
                msgroup.inc             \
                biosseg.inc             \
	        $(inc)\version.inc      \
                $(inc)\devsym.inc       \
                $(inc)\bpb.inc          \
                $(inc)\ioctl.inc        \
	        $(inc)\error.inc        \
                apmequ.inc              \
                msbio.cl9

ptime.obj ptime.lst: ptime.asm          \
                power.inc               \
                msequ.inc               \
                msgroup.inc             \
                biosseg.inc             \
	        $(inc)\version.inc      \
                $(inc)\devsym.inc       \
                $(inc)\bpb.inc          \
                $(inc)\ioctl.inc        \
	        $(inc)\error.inc        \
                apmequ.inc

!IFDEF	ROMDRIVE
romdrive.obj:	romdrive.asm            \
                $(inc)\version.inc      \
		$(inc)\devsym.inc

romrd.obj:	romrd.asm               \
                $(inc)\version.inc

romdinit.obj:	romdinit.asm            \
                $(inc)\version.inc

!ENDIF

msbio.bin: msbio.cl1                    \
                msbio1.obj              \
                $(inc)\version.inc      \
                mschar.obj              \
                msdisk.obj              \
                msdioctl.obj            \
!IFDEF	ROMDRIVE
		romdrive.obj	        \
		romrd.obj	        \
		romdinit.obj	        \
!ENDIF
	        msint13.obj             \
                msbio2.obj              \
                msinit.obj              \
                sysinit1.obj            \
                sysconf.obj             \
		syspre.obj		\
	        sysinit2.obj            \
                sysimes.obj             \
                msend.obj               \
                power.obj               \
                ptime.obj               \
	        msbio.lnk               \
                msbior.lnk
!IFDEF	ROMDRIVE
	link $(link_opts) @msbior.lnk
!ELSE
	link $(link_opts) @msbio.lnk
!ENDIF
        reloc msbio.exe msbio.bin 70
!IF     "$(DEBUG)" == "soft_ice" || "$(debug)" == "soft_ice" || \
        "$(DEBUG)" == "SOFT_ICE" || "$(debug)" == "SOFT_ICE"
        msym msbio
!ELSE
        del msbio.exe
!ENDIF

msload.com: msload.obj
	link $(link_opts) msload.obj,msload,,;
	reloc msload.exe msload.com
	del msload.exe

!IFNDEF         ROMDOS

$(dest):      msload.com                \
                msbio.bin
        copy /b msload.com+msbio.bin $(dest)
!ELSE

$(dest): msbio.bin
        split msbio.bin msbio.map BIOS_CODE tmp.bin resbio.sys
        copy resbio.sys resbio.16
	compress tmp.bin $(dest)
#
# Verify whether uncompress brings back the same image
#
	copy $(dest) file.inp
	uncmprs
	compare file.out tmp.bin
	del tmp.bin
	del file.inp
	del file.out

!ENDIF
