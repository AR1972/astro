PWBRMAKE   = pwbrmake
NMAKEBSC1  = set
NMAKEBSC2  = nmake

NMAKE   = nmake
MAKE    = nmake
CC      = cl
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
LIBEXE	= lib
NTLIB	= coff -lib

#CFLAGS  = /DMSDOS
CFLAGS_DOS = /DMSDOS /DREALMODE /DDOS /Oas /Gs
CFLAGS_OS2 = /DMSDOS /DOS2 /Oat /Gs2
CFLAGS_NT  = -DNT -DWIN32 -D_MT -I\\toolsvr\build\include\nt
CFLAGS_NTI = $(CFLAGS_NT) -Di386=1 -G3d
CFLAGS_NTM = $(CFLAGS_NT) -DMIPS=1
#CFLAGS_NTM = $(CFLAGS_NT) -DMIPS=1 -std -G0 -O -EL
AFLAGS	= -Mx -D?WIN -D?PLM

DEBFLAGS = -Zi

# Standard warning flag settings
CWARN = -W3
ASMWARN = -W1

# set default values for command line switches
ROOT  =.

# set initial inference rules, including the special cases...
#

{}.c{..\sobjr}.obj:
    $(CC) /c $(DEBFLAGS) $(CWARN) $(CFLAGS_DOS) /AS $(CFLAGS) -Fo..\sobjr\ $<

{}.c{..\sobjp}.obj:
    $(CC) /c $(DEBFLAGS) $(CWARN) $(CFLAGS_OS2) /AS $(CFLAGS) -Fo..\sobjp\ $<

{}.c{..\mobjr}.obj:
    $(CC) /c $(DEBFLAGS) $(CWARN) $(CFLAGS_DOS) /AM $(CFLAGS) -Fo..\mobjr\ $<

{}.c{..\mobjp}.obj:
    $(CC) /c $(DEBFLAGS) $(CWARN) $(CFLAGS_OS2) /AM $(CFLAGS) -Fo..\mobjp\ $<

{}.c{..\cobjr}.obj:
    $(CC) /c $(DEBFLAGS) $(CWARN) $(CFLAGS_DOS) /AC $(CFLAGS) -Fo..\cobjr\ $<

{}.c{..\cobjp}.obj:
    $(CC) /c $(DEBFLAGS) $(CWARN) $(CFLAGS_OS2) /AC $(CFLAGS) -Fo..\cobjp\ $<

{}.c{..\lobjr}.obj:
    $(CC) /c $(DEBFLAGS) $(CWARN) $(CFLAGS_DOS) /AL $(CFLAGS) -Fo..\lobjr\ $<

{}.c{..\lobjp}.obj:
    $(CC) /c $(DEBFLAGS) $(CWARN) $(CFLAGS_OS2) /AL $(CFLAGS) -Fo..\lobjp\ $<

{}.c{..\objnti}.obj:
    $(CC_NTI) /c $(DEBFLAGS) $(CWARN) $(CFLAGS_NTI) $(CFLAGS) -Fo..\objnti\ $<
    cvtomf -g ..\objnti\$(@B).obj

{}.c{..\objntm}.obj:
    $(CC_NTM) -c $(CFLAGS_NTM) $(CFLAGS) -Fo..\objntm\$(*B).obj $<
    mip2coff ..\objntm\$(@B).obj

{}.asm{..\sobjr}.obj:
    masm $(DEBFLAGS) $(ASMWARN) $(AFLAGS) $<,$@;

{}.asm{..\sobjp}.obj:
    masm $(DEBFLAGS) $(ASMWARN) $(AFLAGS) -DOS2 $<,$@;

{}.asm{..\mobjr}.obj:
    masm $(DEBFLAGS) $(ASMWARN) $(AFLAGS) -D?MEDIUM $<,$@;

{}.asm{..\mobjp}.obj:
    masm $(DEBFLAGS) $(ASMWARN) $(AFLAGS) -DOS2 -D?MEDIUM $<,$@;

{}.asm{..\cobjr}.obj:
    masm $(DEBFLAGS) $(ASMWARN) $(AFLAGS) -D?COMPACT $<,$@;

{}.asm{..\cobjp}.obj:
    masm $(DEBFLAGS) $(ASMWARN) $(AFLAGS) -DOS2 -D?COMPACT $<,$@;

{}.asm{..\lobjr}.obj:
    masm $(DEBFLAGS) $(ASMWARN) $(AFLAGS) -D?LARGE $<,$@;

{}.asm{..\lobjp}.obj:
    masm $(DEBFLAGS) $(ASMWARN) $(AFLAGS) -DOS2 -D?LARGE $<,$@;

{}.asm{..\apiobj}.obj:
    masm $(DEBFLAGS) $(ASMWARN) -Mx $<,$@;

{}.c{..\lobjmt}.obj:
    $(CC) /c $(DEBFLAGS) $(CWARN) /DOS2 /AL /MT /DMT $(CFLAGS) -Fo..\lobjmt\ $<

{}.asm{..\lobjmt}.obj:
    masm $(DEBFLAGS) $(ASMWARN) -Mx -D?WIN -D?PLM -DOS2 -D?LARGE $<,$@;
