#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1981-1991
#; *                      All Rights Reserved.
#; */
#************************** makefile for bios ***************************

# International Modifications:
MSG_DIR=$(LANG_SRC)\$(COUNTRY)\dev\power


!IF	"$(DEBUG)" != ""

debug_flags = -Zd -DDEBUG
link_opts = /MAP /LI 
dest    =dpower.exe
cpr     =dpower.ex_

!ELSE
dest    =power.exe
cpr     =power.ex_

link_opts = /MAP

!ENDIF


msg	=..\..\messages
inc     =..\..\inc
bins    =..\..\binaries
cprs    =..\..\compress
bios    =..\..\bios

!IFDEF          IBM
ibmboot =-DIBM_VERSION
!ENDIF

extasw	=-I$(inc) $(debug_flags) $(ibmboot) -DPOWERALONE -DPOWER -I$(bios) -D$(COUNTRY)

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
.\powermsg.inc:
	       del powermsg.inc
	       copy $(MSG_DIR)\powermsg.inc

.\power.obj .\power.lst: $(bios)\power.asm $(bios)\power.inc $(bios)\msequ.inc \
	$(inc)\version.inc $(inc)\devsym.inc $(inc)\bpb.inc $(inc)\ioctl.inc \
	$(inc)\error.inc $(bios)\msgroup.inc $(bios)\msbio.cl1
        $(asm) $(ainc) $(aflags) $(bios)\$*.asm;

.\ptime.obj .\ptime.lst: $(bios)\ptime.asm $(bios)\power.inc $(bios)\msequ.inc \
	$(inc)\version.inc $(inc)\devsym.inc $(inc)\bpb.inc $(inc)\ioctl.inc \
	$(inc)\error.inc $(bios)\msgroup.inc $(bios)\msbio.cl1
        $(asm) $(ainc) $(aflags) $(bios)\$*.asm;

lpc.obj lpc.lst: lpc.asm $(bios)\power.inc $(inc)\dosmac.inc $(inc)\pdb.inc \
		 powermsg.inc

lmul.obj lmul.lst: lmul.asm mm.inc

xtoa.obj xtoa.lst: xtoa.asm 

uldiv.obj uldiv.lst: uldiv.asm mm.inc

$(dest):        power.obj ptime.obj lpc.obj lmul.obj uldiv.obj xtoa.obj
        $(link) $(link_opts) power ptime lpc lmul uldiv xtoa,$(dest);

