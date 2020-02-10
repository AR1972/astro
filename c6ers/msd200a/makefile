PROJ = MSD
PROJFILE = MAKEFILE
DEBUG = 0

PWBRMAKE  = pwbrmake
NMAKEBSC1  = set
NMAKEBSC2  = nmake
CC  = cl
CFLAGS_G  = /AM /W3 /Gc /DCW_INCLUDED /BATCH /nologo /Zp
CFLAGS_D  = /Od /Zi /Gs
CFLAGS_R  = /Os /Ol /Og /Oe /Oi /Oa /Gs
MAPFILE_D  = MSD.MAP
MAPFILE_R  = MSD.MAP
LFLAGS_G  =  /NOE /STACK:6144  /BATCH
LFLAGS_D  =  /CO /FAR /PACKC
LFLAGS_R  =  /FAR /PACKC
LINKER  = link
ILINK  = ilink
LRF  = echo > NUL
LLIBS_R  = COW3A.USA
LLIBS_D  = DCOW3A.USA
LLIBS_G  = DOSLAN.LIB VERM.LIB LZEXPM.LIB
CVFLAGS  = /50 /F
BRFLAGS  = /o $(PROJ).bsc /Iu
BROWSE  = 0

OBJS_EXT  = DROP.OBJ CHIPS.OBJ IS386.OBJ VIDEOID.OBJ MD.OBJ PROCLANG.OBJ
OBJS  = MSD.obj GETINFO.obj SPRNINFO.obj RPTINFO.obj MSDSYS.obj CUSTINFO.obj\
        SHOWINFO.obj MENU.obj SUMMARY.obj DEVTAB.obj TSRLIST.obj COMPUTER.obj\
        VIDEO.obj OSINFO.obj LPTINFO.obj COMINFO.obj MOUSINFO.obj IRQINFO.obj\
        DISKINFO.obj NETINFO.obj MEMINFO.obj SUMINFO.obj DIALOGS.obj\
        INSERT.obj VIEWFILE.obj BROWSE.obj OTHRINFO.obj VERINFO.obj\
        $(OBJS_EXT)
SBRS  = MSD.sbr GETINFO.sbr SPRNINFO.sbr RPTINFO.sbr MSDSYS.sbr CUSTINFO.sbr\
        SHOWINFO.sbr MENU.sbr SUMMARY.sbr DEVTAB.sbr TSRLIST.sbr COMPUTER.sbr\
        VIDEO.sbr OSINFO.sbr LPTINFO.sbr COMINFO.sbr MOUSINFO.sbr IRQINFO.sbr\
        DISKINFO.sbr NETINFO.sbr MEMINFO.sbr SUMINFO.sbr DIALOGS.sbr\
        INSERT.sbr VIEWFILE.sbr BROWSE.sbr OTHRINFO.sbr VERINFO.sbr

all: $(PROJ).exe

.SUFFIXES:
.SUFFIXES: .sbr .obj .c

MSD.obj : MSD.C msd.h _msd.h cgraphic.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h SRMENU.H dialogs.adm\
        wndstruc.h

MSD.sbr : MSD.C msd.h _msd.h cgraphic.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h SRMENU.H dialogs.adm\
        wndstruc.h

GETINFO.obj : GETINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

GETINFO.sbr : GETINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

SPRNINFO.obj : SPRNINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

SPRNINFO.sbr : SPRNINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

RPTINFO.obj : RPTINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

RPTINFO.sbr : RPTINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

MSDSYS.obj : MSDSYS.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

MSDSYS.sbr : MSDSYS.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

CUSTINFO.obj : CUSTINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

CUSTINFO.sbr : CUSTINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

SHOWINFO.obj : SHOWINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

SHOWINFO.sbr : SHOWINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

MENU.obj : MENU.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

MENU.sbr : MENU.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

SUMMARY.obj : SUMMARY.C msd.h cgraphic.h scr.c auxcow.c NETBIOS2.H NETCONS.H\
        NETERR.H CONFIG.H REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

SUMMARY.sbr : SUMMARY.C msd.h cgraphic.h scr.c auxcow.c NETBIOS2.H NETCONS.H\
        NETERR.H CONFIG.H REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

DEVTAB.obj : DEVTAB.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

DEVTAB.sbr : DEVTAB.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

TSRLIST.obj : TSRLIST.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

TSRLIST.sbr : TSRLIST.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

COMPUTER.obj : COMPUTER.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

COMPUTER.sbr : COMPUTER.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

VIDEO.obj : VIDEO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

VIDEO.sbr : VIDEO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

OSINFO.obj : OSINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

OSINFO.sbr : OSINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

LPTINFO.obj : LPTINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

LPTINFO.sbr : LPTINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

COMINFO.obj : COMINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

COMINFO.sbr : COMINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

MOUSINFO.obj : MOUSINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

MOUSINFO.sbr : MOUSINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

IRQINFO.obj : IRQINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

IRQINFO.sbr : IRQINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

DISKINFO.obj : DISKINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

DISKINFO.sbr : DISKINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

NETINFO.obj : NETINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

NETINFO.sbr : NETINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

MEMINFO.obj : MEMINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

MEMINFO.sbr : MEMINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

SUMINFO.obj : SUMINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

SUMINFO.sbr : SUMINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

DIALOGS.obj : DIALOGS.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

DIALOGS.sbr : DIALOGS.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

INSERT.obj : INSERT.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

INSERT.sbr : INSERT.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

VIEWFILE.obj : VIEWFILE.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

VIEWFILE.sbr : VIEWFILE.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

BROWSE.obj : BROWSE.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

BROWSE.sbr : BROWSE.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

OTHRINFO.obj : OTHRINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

OTHRINFO.sbr : OTHRINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H\
        REMUTIL.H WKSTA.H SERVER.H SERVICE.H STRINGS.H\
        cwindows.h csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

VERINFO.obj : VERINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h

VERINFO.sbr : VERINFO.C msd.h NETBIOS2.H NETCONS.H NETERR.H CONFIG.H REMUTIL.H\
        WKSTA.H SERVER.H SERVICE.H STRINGS.H cwindows.h\
        csdm.h scr.h menu.h csdmtmpl.h wndstruc.h


$(PROJ).bsc : $(SBRS)
        $(PWBRMAKE) @<<
$(BRFLAGS) $(SBRS)
<<

$(PROJ).exe : $(OBJS)
!IF $(DEBUG)
        $(LRF) @<<$(PROJ).lrf
$(RT_OBJS: = +^
) $(OBJS: = +^
)
$@
$(MAPFILE_D)
$(LLIBS_G: = +^
) +
$(LLIBS_D: = +^
) +
$(LIBS: = +^
)
$(DEF_FILE) $(LFLAGS_G) $(LFLAGS_D);
<<
!ELSE
        $(LRF) @<<$(PROJ).lrf
$(RT_OBJS: = +^
) $(OBJS: = +^
)
$@
$(MAPFILE_R)
$(LLIBS_G: = +^
) +
$(LLIBS_R: = +^
) +
$(LIBS: = +^
)
$(DEF_FILE) $(LFLAGS_G) $(LFLAGS_R);
<<
!ENDIF
        $(LINKER) @$(PROJ).lrf
!IF $(DEBUG)

!ELSE
  HDROPT $(PROJ).EXE
  PKLITE -o -e $(PROJ).EXE
!ENDIF


.c.sbr :
!IF $(DEBUG)
        $(CC) /Zs $(CFLAGS_G) $(CFLAGS_D) /FR$@ $<
!ELSE
        $(CC) /Zs $(CFLAGS_G) $(CFLAGS_R) /FR$@ $<
!ENDIF

.c.obj :
!IF $(DEBUG)
        $(CC) /c $(CFLAGS_G) $(CFLAGS_D) /Fo$@ $<
!ELSE
        $(CC) /c $(CFLAGS_G) $(CFLAGS_R) /Fo$@ $<
!ENDIF


run: $(PROJ).exe
        $(PROJ).exe $(RUNFLAGS)

debug: $(PROJ).exe
        CV $(CVFLAGS) $(PROJ).exe $(RUNFLAGS)
