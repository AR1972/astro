#========================================================================
# Name: MSHERC 1.2 Hercules BIOS Support Driver
# Usage:
#      nmake
#========================================================================

#*************************************************************************
#	Aliases for components.
#*************************************************************************

#=================== MSHERC components ====================================

MSHERC112 =	cursor.obj   fcommon.obj  fmakeres.obj	\
		fstart.obj   fvars.obj	  gscroll.obj	\
		gwrite.obj   rwchar.obj   rwdot.obj	\
		setmode.obj  stanfnt.obj  tty.obj


#*************************************************************************
# Generate the MSHERC TSR com file.
#*************************************************************************
MSHERC: msherc12.com
	@echo "MSHERC built"

msherc12.com: $(MSHERC112)
	$(TL)\..\binb\link510 @msherc12.lnk
	@echo "MSHERC verion 1.12 build"

#*************************************************************************
# And now the object file dependencies!
#*************************************************************************
INCLUDE=-I..\inc -I..\crt 

cursor.obj:	..\herc\cursor.asm ..\inc\hgcdefs.inc
	$(TL)\masm $(INCLUDE) -MX -v -e ..\herc\cursor.asm, cursor.obj;

fcommon.obj:	..\herc\fcommon.asm ..\inc\hgcdefs.inc
	$(TL)\masm $(INCLUDE) -MX -v -e ..\herc\fcommon.asm, fcommon.obj;

fvars.obj:	..\herc\fvars.asm ..\inc\hgcdefs.inc
	$(TL)\masm $(INCLUDE) -MX -v -e ..\herc\fvars.asm, fvars.obj;

gscroll.obj:	..\herc\gscroll.asm ..\inc\hgcdefs.inc
	$(TL)\masm $(INCLUDE) -MX -v -e ..\herc\gscroll.asm, gscroll.obj;

gwrite.obj:	..\herc\gwrite.asm ..\inc\hgcdefs.inc
	$(TL)\masm $(INCLUDE) -MX -v -e ..\herc\gwrite.asm, gwrite.obj;

rwchar.obj:	..\herc\rwchar.asm ..\inc\hgcdefs.inc
	$(TL)\masm $(INCLUDE) -MX -v -e ..\herc\rwchar.asm, rwchar.obj;

rwdot.obj:	..\herc\rwdot.asm ..\inc\hgcdefs.inc
	$(TL)\masm $(INCLUDE) -MX -v -e ..\herc\rwdot.asm, rwdot.obj;

setmode.obj:	..\herc\setmode.asm ..\inc\hgcdefs.inc
	$(TL)\masm $(INCLUDE) -MX -v -e ..\herc\setmode.asm, setmode.obj;

stanfnt.obj:	..\herc\stanfnt.asm
	$(TL)\masm $(INCLUDE) -MX -v -e ..\herc\stanfnt.asm, stanfnt.obj;

tty.obj:	..\herc\tty.asm ..\inc\hgcdefs.inc
	$(TL)\masm $(INCLUDE) -MX -v -e ..\herc\tty.asm, tty.obj;

fmakeres.obj:	..\herc\fmakeres.asm ..\inc\hgcdefs.inc
	$(TL)\masm $(INCLUDE) -MX -v -e ..\herc\fmakeres.asm, fmakeres.obj;

fstart.obj:	..\herc\fstart.asm ..\inc\hgcdefs.inc
	$(TL)\masm $(INCLUDE) -MX -v -e ..\herc\fstart.asm, fstart.obj;
