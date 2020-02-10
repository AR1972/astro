#******************************************************************************
#
#   (C) Copyright MICROSOFT Corp., 1988
#
#   Title:      INCLUDE FILE FOR ALL WIN386 PROT MODE MAKE FILES
#
#   Version:    1.00
#
#   Date:       06-May-1988
#
#   Author:     RAL
#
#------------------------------------------------------------------------------
#
#   Change log:
#
#      DATE     REV                 DESCRIPTION
#   ----------- --- -----------------------------------------------------------
#   06-May-1988 RAL Original
#
#==============================================================================


#
#   Suffixes
#

.SUFFIXES:      .lst .def .lnk .com .386 .sym .hdr .map

!IFDEF SOURCES
LOCALINCS = $(LOCALINCS) -I$(SOURCES)
!ENDIF

!IFNDEF TOOLS
TOOLS   = ..\..\tools\bin
!ENDIF
!IFDEF LOCALINCS
INC     = $(LOCALINCS) -I$(ROOT)\include -I$(ROOT)\dosinc
!ELSE
INC     = -I$(ROOT)\include -I$(ROOT)\dosinc
!ENDIF

TOUCH   = $(TOOLS)\touch
MAKE    = $(TOOLS)\xmsmake

!IFNDEF WARNINGLEVEL
WARNINGLEVEL = 2
!ENDIF

AFLAGS  = -Mx -t $(MAKEFLAG) $(INC) -p

ASM     = $(TOOLS)\masm5


#       Definitions for 32 bit linker
LINK    = $(TOOLS)\link386
LFLAGS  = /NOI /NOD /MAP /NOPACKCODE

MAPSYM  = $(TOOLS)\MAPSYM32

!IFDEF REALMODE
CC      = $(TOOLS)\cl
CFLAGS  = /c /Asnd /G2 /W3 /Ot /Zp /X /I.\ $(INC)

.c.obj:
	$(CC) $(CFLAGS) $*.c
!ENDIF

{$(ROOT)\}.386.hdr:
	$(TOOLS)\exehdr -v $< > $*.hdr

{.\}.map.sym:
	$(MAPSYM) $(@B)
#	copy $(@B).sym $(ROOT)
#	del $(@B).sym

#******************************************************************************
#                    Rules for generating object files
#******************************************************************************

{$(SOURCES)}.asm.obj:
	$(ASM) -w$(WARNINGLEVEL) $(LISTFLAG) $(AFLAGS) $(LOCALFLAGS) $<;

{$(SOURCES)}.asm.lst:
	$(ASM) -w2 -l $(AFLAGS) $(LOCALFLAGS) $<;
#force warning level 2 when building LST files

LINK386 = TRUE

# international mode
!IFDEF LANG
!IF "$(BLDINTL)" != ""
INTL_BIN = $(ROOT)\INTL
!ELSE
INTL_BIN = $(ROOT)
!ENDIF
!ENDIF

!IFDEF DEVICE

!IFNDEF DDB_NAME
DDB_NAME = $(DEVICE)
!ENDIF

!IFDEF LINK386
!IFDEF MAKEDOC
all:    doc msg
!ELSE

!IFNDEF EXECUTABLE
!IFDEF VMM
EXECUTABLE = win386.exe
!ELSE
EXECUTABLE = $(DEVICE).386
!ENDIF
!ENDIF

!IF "$(BLDALL)" != ""

# international mode

!IFNDEF LANG
all:    dev
!ELSE
all:   intldev
!ENDIF

!ELSE

# international mode

!IFNDEF LANG
all:    dev $(ROOT)\win386.exe
!ELSE
all:   intldev $(INTL_BIN)\win386.exe
!ENDIF

!ENDIF

dev:    $(ROOT)\$(EXECUTABLE) $(ROOT)\win386.sym

intldev: $(INTL_BIN)\$(EXECUTABLE)

$(ROOT)\win386.sym: $(ROOT)\$(DEVICE).sym
	$(TOOLS)\symlib $(ROOT)\win386-+$(ROOT)\$(DEVICE).sym

$(ROOT)\win386.exe: $(ROOT)\VMM.386 $(ROOT)\BIOSXLAT.386 $(ROOT)\DOSMGR.386 \
		    $(ROOT)\DOSNET.386 $(ROOT)\EBIOS.386 $(ROOT)\VKD.386 \
		    $(ROOT)\VDDVGA.386 $(ROOT)\PAGESWAP.386 $(ROOT)\PARITY.386 \
		    $(ROOT)\REBOOT.386 $(ROOT)\WSHELL.386 $(ROOT)\VCD.386 \
		    $(ROOT)\COMBUFF.386 $(ROOT)\VDMAD.386 $(ROOT)\VFD.386 \
		    $(ROOT)\BLOCKDEV.386 $(ROOT)\VMCPD.386 $(ROOT)\VMD.386 \
		    $(ROOT)\VMPOLL.386 $(ROOT)\VNETBIOS.386 \
		    $(ROOT)\VPICD.386 $(ROOT)\VSD.386 $(ROOT)\VTD.386 \
		    $(ROOT)\V86MMGR.386 $(ROOT)\CDPSCSI.386 \
		    $(ROOT)\INT13.386 $(ROOT)\WDCTRL.386 $(ROOT)\PAGEFILE.386 \
		    $(ROOT)\QEMMFIX.386
       copy $(ROOT)\vmm.386 $(ROOT)\win386.exe
       $(TOOLS)\devlib -c $(ROOT)\win386.exe
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\INT13.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\WDCTRL.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\VMD.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\VNETBIOS.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\DOSNET.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\EBIOS.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\VDDVGA.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\VKD.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\VPICD.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\VTD.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\REBOOT.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\VDMAD.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\VSD.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\V86MMGR.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\PAGESWAP.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\DOSMGR.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\VMPOLL.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\WSHELL.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\BLOCKDEV.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\PAGEFILE.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\VFD.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\PARITY.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\BIOSXLAT.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\VCD.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\VMCPD.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\COMBUFF.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\CDPSCSI.386
       $(TOOLS)\devlib $(ROOT)\win386.exe $(ROOT)\QEMMFIX.386

$(INTL_BIN)\win386.exe: $(INTL_BIN)\VMM.386 $(ROOT)\BIOSXLAT.386 $(INTL_BIN)\DOSMGR.386 \
		    $(ROOT)\DOSNET.386 $(ROOT)\EBIOS.386 $(INTL_BIN)\VKD.386 \
		    $(INTL_BIN)\VDDVGA.386 $(ROOT)\PAGESWAP.386 $(INTL_BIN)\PARITY.386 \
		    $(INTL_BIN)\REBOOT.386 $(INTL_BIN)\WSHELL.386 $(ROOT)\VCD.386 \
		    $(ROOT)\COMBUFF.386 $(INTL_BIN)\VDMAD.386 $(ROOT)\VFD.386 \
		    $(ROOT)\BLOCKDEV.386 $(ROOT)\VMCPD.386 $(ROOT)\VMD.386 \
		    $(ROOT)\VMPOLL.386 $(INTL_BIN)\VNETBIOS.386 \
		    $(INTL_BIN)\VPICD.386 $(ROOT)\VSD.386 $(ROOT)\VTD.386 \
		    $(INTL_BIN)\V86MMGR.386 $(ROOT)\CDPSCSI.386 \
		    $(ROOT)\INT13.386 $(INTL_BIN)\WDCTRL.386 $(INTL_BIN)\PAGEFILE.386 \
		    $(ROOT)\QEMMFIX.386
       copy $(INTL_BIN)\vmm.386 $(INTL_BIN)\win386.exe
       $(TOOLS)\devlib -c $(INTL_BIN)\win386.exe
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(ROOT)\INT13.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(INTL_BIN)\WDCTRL.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(ROOT)\VMD.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(INTL_BIN)\VNETBIOS.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(ROOT)\DOSNET.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(ROOT)\EBIOS.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(INTL_BIN)\VDDVGA.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(INTL_BIN)\VKD.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(INTL_BIN)\VPICD.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(ROOT)\VTD.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(INTL_BIN)\REBOOT.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(INTL_BIN)\VDMAD.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(ROOT)\VSD.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(INTL_BIN)\V86MMGR.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(ROOT)\PAGESWAP.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(INTL_BIN)\DOSMGR.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(ROOT)\VMPOLL.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(INTL_BIN)\WSHELL.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(ROOT)\BLOCKDEV.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(INTL_BIN)\PAGEFILE.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(ROOT)\VFD.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(INTL_BIN)\PARITY.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(ROOT)\BIOSXLAT.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(ROOT)\VCD.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(ROOT)\VMCPD.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(ROOT)\COMBUFF.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(ROOT)\CDPSCSI.386
       $(TOOLS)\devlib $(INTL_BIN)\win386.exe $(ROOT)\QEMMFIX.386
       IF EXIST $(LANGFILE) ERASE $(LANGFILE)
       
!ENDIF

!ELSE
all:    allobjs
!ENDIF

depend: $(LANGFILE) $(LNGFILE2)
	del makefile.old
	ren makefile makefile.old
	$(TOOLS)\sed "/^# Dependencies follow/q" makefile.old > makefile
!IFDEF SOURCES
	$(TOOLS)\includes $(INC) $(SOURCES)\*.asm >> makefile
	$(TOOLS)\sed "s/$(SOURCES)\\\(.*\.obj\)/\1/" makefile > makefile.new
	$(TOOLS)\sed "s/$(SOURCES)\\\(.*\.lst\)/\1/" makefile.new > makefile
	$(TOOLS)\ls -vq $(SOURCES)\*.asm > makeobj
!ELSE
	$(TOOLS)\includes $(INC) *.asm >> makefile
	$(TOOLS)\ls -vq *.asm > makeobj
!ENDIF
	$(TOOLS)\sed "/^..\\dosinc/q" makefile > makefile.new
	$(TOOLS)\grep -v "^\.\." makefile.new > makefile
	$(TOOLS)\sed "s/.asm. */.obj /g" makeobj > makeobj2
	$(TOOLS)\sed "$$ !s/$$/\\/" makeobj2 > makeobj
	$(TOOLS)\sed "1 s/^/OBJS =  /" makeobj > makeobj2
	$(TOOLS)\sed "1 !s/^/        /" makeobj2 >> makefile
	echo XX> blankln.txt
	$(TOOLS)\sed "$$ s/XX//" blankln.txt >> makefile
	echo allobjs: version $$(OBJS) >> makefile
	$(TOOLS)\sed "$$ s/XX//" blankln.txt >> makefile
!IFDEF OTHEROBJS
	echo $$(ROOT)\$(EXECUTABLE): $(DEVICE).def version $(LOCALLINK) $(STUB) $$(OBJS) $$(OTHEROBJS)>> makefile
!ELSE
	echo $$(ROOT)\$(EXECUTABLE): $(DEVICE).def version $(LOCALLINK) $(STUB) $$(OBJS)>> makefile
!ENDIF
	$(TOOLS)\sed "s/  */+/g" makeobj > makeobj2
	$(TOOLS)\sed "s/.$$//g" makeobj2 > makeobj
!IFDEF LOCALLINK
	echo $$(LINK) $$(LFLAGS) @$(LOCALLINK)>> makefile
	$(TOOLS)\sed "$$ s/^/        /" makefile > makefile.new
	echo $$(TOOLS)\addhdr $$(ROOT)\$(EXECUTABLE)>> makefile.new
	$(TOOLS)\sed "$$ s/^/        /" makefile.new > makefile
!ELSE
	echo $$(LINK) @XX>> makefile
	$(TOOLS)\sed "$$ s/^/        /" makefile > makefile.new
	$(TOOLS)\sed "$$ s/X/</g" makefile.new > makefile
!IFDEF LINKEXCLUDES
	echo s/ /.obj /g> makeobj3
	echo s/^^/s\//>> makeobj3
	echo s/ / s\//g>> makeobj3
	echo s/ /\/\/ /g>> makeobj3
	echo s/^^/s\/+$$\/+\\\\\/ />> makeobj3
	echo s/$$/++*\/+\/g s\/^^+\/\/ s\/+$$\/\/ s\/+\\\\\/+\//>> makeobj3
	echo s/ /\>> makeobj3
	echo /g>> makeobj3
	$(TOOLS)\sed "s/\^\^/^/g" makeobj3 > makeobj4
	echo $(LINKEXCLUDES) > makeobj3
	$(TOOLS)\sed -f makeobj4 makeobj3 > makeobj2
	$(TOOLS)\sed -f makeobj2 makeobj >> makefile
!ELSE
	copy makefile + makeobj makefile /b
!ENDIF
	echo $$(ROOT)\$(EXECUTABLE) $$(LFLAGS)>> makefile
	echo $(DEVICE).map>> makefile
	$(TOOLS)\sed "$$ s/XX//" blankln.txt >> makefile
	echo $(DEVICE).def>> makefile
	$(TOOLS)\sed "$$ s/X/</g" blankln.txt >> makefile
	$(TOOLS)\sed "$$ s/XX/        $$(TOOLS)\\addhdr $$(ROOT)\\$(EXECUTABLE)/" blankln.txt >> makefile
!ENDIF
!IFDEF ADDEXEBLD
	echo $(ADDEXEBLD)> makefile.new
	$(TOOLS)\sed "$$ s/^/        /" makefile.new >> makefile
!ENDIF
	del makeobj?
	del makefile.new
	del blankln.txt

!IFNDEF DESCRIPTION
!IFDEF VERSION
DESCRIPTION = Win386 $(DEVICE) Device  (Version $(VERSION))
!ELSE
DESCRIPTION = Win386 $(DEVICE) Device
!ENDIF
!ENDIF

def:    $(ROOT)\win386.mk $(DEVICE).def

$(DEVICE).def:
	echo LIBRARY     $(DEVICE)> $(DEVICE).def
	echo XX> blankln.txt
	$(TOOLS)\sed "$$ s/XX//" blankln.txt >> $(DEVICE).def
	echo DESCRIPTION '$(DESCRIPTION)'>> $(DEVICE).def
	$(TOOLS)\sed "$$ s/XX//" blankln.txt >> $(DEVICE).def
!IFDEF STUB
	echo STUB        '$(STUB)'>> $(DEVICE).def
	$(TOOLS)\sed "$$ s/XX//" blankln.txt >> $(DEVICE).def
!ENDIF
	echo EXETYPE     DEV386>> $(DEVICE).def
	$(TOOLS)\sed "$$ s/XX//" blankln.txt >> $(DEVICE).def
	echo SEGMENTS>> $(DEVICE).def
	$(TOOLS)\sed "$$ s/XX/             _LTEXT PRELOAD NONDISCARDABLE/" blankln.txt >> $(DEVICE).def
	$(TOOLS)\sed "$$ s/XX/             _LDATA PRELOAD NONDISCARDABLE/" blankln.txt >> $(DEVICE).def
	$(TOOLS)\sed "$$ s/XX/             _ITEXT CLASS 'ICODE' DISCARDABLE/" blankln.txt >> $(DEVICE).def
	$(TOOLS)\sed "$$ s/XX/             _IDATA CLASS 'ICODE' DISCARDABLE/" blankln.txt >> $(DEVICE).def
	$(TOOLS)\sed "$$ s/XX/             _TEXT  CLASS 'PCODE' NONDISCARDABLE/" blankln.txt >> $(DEVICE).def
	$(TOOLS)\sed "$$ s/XX/             _DATA  CLASS 'PCODE' NONDISCARDABLE/" blankln.txt >> $(DEVICE).def
!IF "$(DDB_NAME)" != ""
	$(TOOLS)\sed "$$ s/XX//" blankln.txt >> $(DEVICE).def
	echo EXPORTS>> $(DEVICE).def
	$(TOOLS)\sed "$$ s/XX/             $(DDB_NAME)_DDB  @1/" blankln.txt >> $(DEVICE).def
!ENDIF
	del blankln.txt

!IFDEF VMM
hdr:
	$(TOOLS)\exehdr -v ..\win386.exe > vmm.hdr
!ELSE
hdr:    $(DEVICE).hdr
!ENDIF

sym:    $(ROOT)\win386.sym

!ELSE
depend:
	del makefile.old
	ren makefile makefile.old
	$(TOOLS)\sed "/^# Dependencies follow/q" makefile.old > makefile
!IFDEF SOURCES
	$(TOOLS)\includes $(INC) $(SOURCES)\*.c $(SOURCES)\*.asm >> makefile
!ELSE
	$(TOOLS)\includes $(INC) *.c *.asm >> makefile
!ENDIF
	$(TOOLS)\sed "/^..\\dosinc/q" makefile > makefile.new
	$(TOOLS)\grep -v "^\.\." makefile.new > makefile
	$(TOOLS)\ls -vq *.c *.asm > makeobj
	$(TOOLS)\sed "s/*//g" makeobj > makeobj3
	$(TOOLS)\sed "s/.asm/.obj/g" makeobj3 > makeobj2
	$(TOOLS)\sed "s/.c /.obj /g" makeobj2 > makeobj
	$(TOOLS)\sed "s/$$/\\/g" makeobj > makeobj2
	$(TOOLS)\sed "/^/q" makeobj2 > makeobj
	$(TOOLS)\sed "s/^/OBJS = /" makeobj > makeobj3
	command /c $(TOOLS)\fgrep -vf makeobj makeobj2 > makeobj4
	copy makefile + makeobj3 + makeobj4 makefile /b
	del makeobj?
	del makefile.new
	echo X > blankln.txt
	$(TOOLS)\sed "s/X/ /" blankln.txt >> makefile
	del blankln.txt
	echo allobjs: $$(OBJS) >> makefile
!ENDIF


version: $(ROOT)\makeflag.mk
	del *.obj
	copy $(ROOT)\makeflag.mk version

doc:
	$(TOOLS)\MakeDoc1 $(TOOLS) DOCFILE.TXT

msg:
	$(TOOLS)\MakeMsg1 $(TOOLS) MSGFILE.TXT

stripsym:
	$(MAPSYM) -s $(DEVICE)
	$(TOOLS)\symlib $(ROOT)\win386.ssm-+$(DEVICE).sym
	del $(DEVICE).sym
