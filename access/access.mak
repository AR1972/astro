ALL:  SETFLAGS ados.ovl ados.com 
FRONT: SETFLAGS ados.ovl
ACCESS: SETFLAGS ados.com

#relavent optimization codes:
# a - assumes no aliasing
# d - disables all optimizations
# e - enables global register allocation
# g - enables global optimizations and global common expressions
# i - enables generation of intrinsic routines
# l - enables loop optimization
# n - disables unsafe loop optimizations (default)
# r - disables in-line returns from functions
# s - favors small code size
# t - favors faster execution speed (default)
# w - assumes no aliases except across function calls
# x - maximizes optimizations ( /Oecilgt/Gs )
# z - enables maximum loop and global register allocation optimizations
#

SKCSRC = init.c comm.c gide.c kbd.c mou.c drv.c int9.c vars.c
SKASRC = serkeys.asm
ADOSASRC = handicap.asm param.asm filter.asm stickeys.asm mousekey.asm toggle.asm timeout.asm startup.asm equip.asm # hexout.asm #vars.asm 
FRNTCSRC = access.c datablk.c dialogs.c frontend.c menudata.c messages.c

SETFLAGS:
!IFDEF DEBUG
cflags = /AT /Gs /Od /Zi /DDEBUG=0 /nologo /c
masmflags = /ML /ZI /L /dDEBUG=0 /dBUG
linkflags = /CP:1 /NOI /CO
!ELSE
cflags = /ATw /Gs /Oils /DDEBUG=0 /nologo /c # /Fm /Fc 
masmflags = /ML /Z /dDEBUG=0  #/dBUG /ZI /L 
linkflags = /CP:1 /NOI #/CO 
!ENDIF

ados.ovl: $(FRNTCSRC:.c=.obj) front.lnk
   link $(linkflags) @front.lnk

ados.com: $(ADOSASRC:.asm=.obj) $(SKCSRC:.c=.obj) $(SKASRC:.asm=.obj) access.lnk
   link $(linkflags) @access.lnk

access.obj: access.c access.h dialogs.h datablk.h frontend.h menudata.h messages.h defines.h access.mak
   cl $(cflags:/ATw=/AS) /DFRONT $*.c

datablk.obj: datablk.c datablk.h defines.h globals.h access.mak
   cl $(cflags:/ATw=/AS) /DFRONT $*.c

dialogs.obj: dialogs.c dialogs.h access.h menudata.h messages.h frontend.h defines.h globals.h access.mak
   cl $(cflags:/ATw=/AS) /DFRONT $*.c

frontend.obj: frontend.c frontend.h menudata.h messages.h dialogs.h access.h defines.h globals.h access.mak
   cl $(cflags:/ATw=/AS) /DFRONT $*.c

menudata.obj: menudata.c menudata.h access.mak messages.h dialogs.h access.h defines.h globals.h
   cl $(cflags:/ATw=/AS) /DFRONT $*.c

messages.obj: messages.c messages.h access.mak defines.h
   cl $(cflags:/ATw=/AS) /DFRONT $*.c

equip.obj: equip.asm keyboard.inc access.mak
   masm $(masmflags) $*;

startup.obj: startup.asm access.mak
   masm $(masmflags) $*;

handicap.obj: handicap.asm keyboard.inc access.mak
   masm $(masmflags) $*;   

param.obj: param.asm keyboard.inc access.mak
   masm $(masmflags) $*;

filter.obj: filter.asm keyboard.inc access.mak #param.asm
   masm $(masmflags) $*;

stickeys.obj: stickeys.asm keyboard.inc access.mak #param.asm
   masm $(masmflags) $*;

mousekey.obj: mousekey.asm keyboard.inc access.mak #param.asm
   masm $(masmflags) $*;

toggle.obj: toggle.asm keyboard.inc access.mak #param.asm
   masm $(masmflags) $*;

timeout.obj: timeout.asm keyboard.inc access.mak #param.asm
   masm $(masmflags) $*;

hexout.obj: hexout.asm keyboard.inc access.mak
   masm $(masmflags) $*;

serkeys.obj: serkeys.asm access.mak
   masm $(masmflags) $*;

vars.obj: vars.c gideidef.h access.mak
   cl $(cflags) $*.c

#vars.obj: vars.asm access.mak
#   masm $(masmflags) $*;

comm.obj: comm.c comm.h vars.h  gideidef.h access.mak
   cl $(cflags) $*.c

gide.obj: gide.c gideidef.h vars.h gide.h kbd.h mou.h comm.h access.mak
   cl $(cflags) $*.c

kbd.obj: kbd.c drv.h vars.h kbd.h  gideidef.h access.mak
   cl $(cflags) $*.c

mou.obj: mou.c drv.h  gideidef.h vars.h mou.h access.mak
   cl $(cflags) $*.c

drv.obj: drv.c  gideidef.h vars.h drv.h access.mak
   cl $(cflags) $*.c

int9.obj: int9.c  gideidef.h vars.h int9.h drv.h access.mak
   cl $(cflags) $*.c

init.obj: init.c gideidef.h vars.h gide.h init.h access.mak
   cl $(cflags) $*.c



