#******************************************************************************
#
#   MAKEFILE FOR SDVXD
#
#==============================================================================

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\dev\smartdrv

DOCFILE = VXDDOC.TXT

DEVICE  = SDVXD
VERSION = 2.0
LINK    = link386 /NOI /NOD /MAP /NOPACKCODE
ASM     = masm5 -Mx -t -p

.asm.obj:
	$(ASM) $*.asm;

OBJS =  sdvxd.obj sdvxdtxt.obj 

#******************************************************************************
#                DO NOT MODIFY THE MAKEFILE BEYOND THIS LINE
#******************************************************************************

# Dependencies follow
sdvxd.386: $(OBJS) sdvxd.def

sdvxd.obj sdvxd.lst: sdvxd.asm shell.inc debug.inc vmm.inc shellfsc.inc

sdvxdtxt.obj sdvxdtxt.lst: sdvxdtxt.asm vmm.inc

sdvxdtxt.asm: $(MSG_DIR)\sdvxdtxt.asm
	copy    $(MSG_DIR)\sdvxdtxt.asm .

SDVXD.386: SDVXD.def $(OBJS)
	$(LINK) @<<
sdvxd.obj+sdvxdtxt.obj
SDVXD.386 
SDVXD.map
,
SDVXD.def
<<
	addhdr SDVXD.386
	copy sdvxd.386 ..\smartdrv.exe
