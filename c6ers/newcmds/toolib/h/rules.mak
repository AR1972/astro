PWBRMAKE   = pwbrmake
NMAKEBSC1  = set
NMAKEBSC2  = nmake

NMAKE   = nmake
MAKE    = nmake
CC	= cl
CC_NTI	= cl386
CC_NTM	= mcl
ASM     = masm
LINK    = link
BIND    = bind
RC      = rc
IMPLIB  = implib
CP      = cp
MAPSYM  = mapsym
MARKEXE = markexe
DEL	= del
ECHO	= echo
LIBEXE  = lib
NTLIB	= coff -lib
NT_INTEL_LIBS = \\toolsvr\build\lib\nt\intel
NT_MIPS_LIBS = \\toolsvr\build\lib\nt\mips
TOOLS_LIBS = \\toolsvr\build\toolslib
SETARGV = \\toolsvr\build\lib\setargv.obj

CFLAGS_DOS = /DMSDOS /DREALMODE /DDOS /Fo$(@B).obr
CFLAGS_OS2 = /DMSDOS /DOS2 /G2 /Fo$(@B).obp
CFLAGS_NT  = -DNT -DWIN32 -D_MT -I\\toolsvr\build\include\nt
CFLAGS_NTI = $(CFLAGS_NT) -Di386=1 -G3d -Fo$(@B).obi
CFLAGS_NTM = $(CFLAGS_NT) -DMIPS=1 -Fo$(@B).obm
#CFLAGS_NTM = $(CFLAGS_NT) -DMIPS=1 -Bd -Gt0 -Rqd4 -Fo$(@B).obm
LFLAGS	= /ST:4100

#set default values for command line switches
DEBUG =0
ROOT  =.

# set initial inference rules, including the special cases...

#Note: the tools library has basic differences between DOS and OS/2;
# you can't have just one .obj file, link with the correct libraries,
# and have it work.
.c.obj:
	$(CC) /c $(MODEL) /DMSDOS $(CFLAGS) $<

.c.obp:
	$(CC) /c $(MODEL) $(CFLAGS_OS2) $(CFLAGS) $<

.c.obr:
	$(CC) /c $(MODEL) $(CFLAGS_DOS) $(CFLAGS) $<

.c.obi:
	$(CC_NTI) -c $(CFLAGS_NTI) $(CFLAGS) $<
	cvtomf -g $(@B).obi

.c.obm:
	$(CC_NTM) -c $(CFLAGS_NTM) $(CFLAGS) $<
	mip2coff $(@B).obm

.asm.obj:
	$(ASM) $(AFLAGS) $<,$(@B).obj,$(@B).lst,$(@B).crf

.asm.obp:
	$(ASM) $(AFLAGS) /DOS2 $<,$(@B).obp,$(@B).lst,$(@B).crf

.asm.obr:
	$(ASM) $(AFLAGS) /DDOS $<,$(@B).obr,$(@B).lst,$(@B).crf

#NT shouldn't use assembler code -- non-portable
