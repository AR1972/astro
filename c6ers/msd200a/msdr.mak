PROJ = MSDR
PROJFILE = MSDR.MAK
DEBUG = 0

PWBRMAKE  = pwbrmake
NMAKEBSC1  = set
NMAKEBSC2  = nmake
CC  = cl
CFLAGS_G  = /W4 /Gr /BATCH /FR$*.sbr /nologo /Zp
CFLAGS_D  = /Od /FPa /Zi
CFLAGS_R  = /Os /Ol /Og /Oe /Oi /Oa /FPa /Gs /Gr
MAPFILE_D  = NUL
MAPFILE_R  = NUL
LFLAGS_G  =  /STACK:5120  /BATCH
LFLAGS_D  =  /CO /FAR /PACKC
LFLAGS_R  =  /FAR /PACKC
LINKER  = link
ILINK  = ilink
LRF  = echo > NUL
CVFLAGS  = /50 /F
BRFLAGS  = /o $(PROJ).bsc
BROWSE  = 1

OBJS_EXT  = CHIPS.OBJ IS386.OBJ VIDEOID.OBJ MD.OBJ
OBJS  = MSD.obj GETINFO.obj SPRNINFO.obj RPTINFO.obj MSDSYS.obj CUSTINFO.obj\
        SHOWINFO.obj DEVTAB.obj TSRLIST.obj COMPUTER.obj VIDEO.obj OSINFO.obj\
        LPTINFO.obj COMINFO.obj MOUSINFO.obj IRQINFO.obj DISKINFO.obj\
        NETINFO.obj MEMINFO.obj SUMINFO.obj $(OBJS_EXT)
SBRS  = MSD.sbr GETINFO.sbr SPRNINFO.sbr RPTINFO.sbr MSDSYS.sbr CUSTINFO.sbr\
        SHOWINFO.sbr DEVTAB.sbr TSRLIST.sbr COMPUTER.sbr VIDEO.sbr OSINFO.sbr\
        LPTINFO.sbr COMINFO.sbr MOUSINFO.sbr IRQINFO.sbr DISKINFO.sbr\
        NETINFO.sbr MEMINFO.sbr SUMINFO.sbr

all: $(PROJ).exe

.SUFFIXES:
.SUFFIXES: .sbr .obj .c

MSD.obj : MSD.C msd.h _msd.h cgraphic.h csdmtmpl.h c:\c600\include\process.h\
        NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h menu.h SRMENU.H\
        wndstruc.h

MSD.sbr : MSD.C msd.h _msd.h cgraphic.h csdmtmpl.h c:\c600\include\process.h\
        NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h menu.h SRMENU.H\
        wndstruc.h

GETINFO.obj : GETINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

GETINFO.sbr : GETINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

SPRNINFO.obj : SPRNINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

SPRNINFO.sbr : SPRNINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

RPTINFO.obj : RPTINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

RPTINFO.sbr : RPTINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

MSDSYS.obj : MSDSYS.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

MSDSYS.sbr : MSDSYS.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

CUSTINFO.obj : CUSTINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

CUSTINFO.sbr : CUSTINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

SHOWINFO.obj : SHOWINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

SHOWINFO.sbr : SHOWINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

DEVTAB.obj : DEVTAB.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

DEVTAB.sbr : DEVTAB.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

TSRLIST.obj : TSRLIST.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

TSRLIST.sbr : TSRLIST.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

COMPUTER.obj : COMPUTER.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

COMPUTER.sbr : COMPUTER.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

VIDEO.obj : VIDEO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h menu.h\
        wndstruc.h

VIDEO.sbr : VIDEO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h menu.h\
        wndstruc.h

OSINFO.obj : OSINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

OSINFO.sbr : OSINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

LPTINFO.obj : LPTINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

LPTINFO.sbr : LPTINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

COMINFO.obj : COMINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

COMINFO.sbr : COMINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

MOUSINFO.obj : MOUSINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

MOUSINFO.sbr : MOUSINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

IRQINFO.obj : IRQINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

IRQINFO.sbr : IRQINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

DISKINFO.obj : DISKINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

DISKINFO.sbr : DISKINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

NETINFO.obj : NETINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

NETINFO.sbr : NETINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

MEMINFO.obj : MEMINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

MEMINFO.sbr : MEMINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

SUMINFO.obj : SUMINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h

SUMINFO.sbr : SUMINFO.C msd.h NETBIOS2.H STRINGS.H cwindows.h csdm.h scr.h\
        menu.h wndstruc.h


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
        $(NMAKEBSC1) MAKEFLAGS=
        $(NMAKEBSC2) $(NMFLAGS) -f $(PROJFILE) $(PROJ).bsc


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
