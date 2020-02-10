PWBRMAKE   = pwbrmake
NMAKEBSC1  = set
NMAKEBSC2  = nmake

NMAKE   = nmake
MAKE    = nmake
CC      = cl
ASM     = masm
LINK    = link
ILINK   = ilink
BIND    = bind
RC      = rc
IMPLIB  = implib
CP      = cp
MAPSYM  = mapsym
MARKEXE = markexe
DEL	= del
ECHO	= echo
LIBEXE  = lib
SETARGV = \\toolsvr\build\lib\setargv.obj

CFLAGS  = /DMSDOS
LFLAGS  = /ST:4100

# set default values for command line switches
DEBUG =0
ROOT  =.

# set initial inference rules, including the special cases...

.c.obj:
	$(CC) /c $(CFLAGS) $<

.c.obp:
	$(CC) /c /DOS2 /Fo$(@B).obp $(CFLAGS) $<

.c.obr:
	$(CC) /c /DREALMODE /DDOS /Fo$(@B).obr $(CFLAGS) $<

.asm.obj:
	$(ASM) $(AFLAGS) $<,$(@B).obj,$(@B).lst,$(@B).crf

.asm.obp:
	$(ASM) $(AFLAGS) /DOS2 $<,$(@B).obp,$(@B).lst,$(@B).crf

.asm.obr:
	$(ASM) $(AFLAGS) /DDOS $<,$(@B).obr,$(@B).lst,$(@B).crf


