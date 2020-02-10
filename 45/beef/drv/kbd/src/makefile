#############################################################################
#	Make file for KBD Drivers
#############################################################################

ALL =	 fx_kbd3.obj  fx_kbdw.obj  fx_kbd5.obj  fx_kbdk.obj \
	dfx_kbd3.obj dfx_kbdw.obj dfx_kbd5.obj dfx_kbdk.obj \
	                                                    \
	 fxdkbdw.obj  fxdkbd5.obj                           \
	dfxdkbdw.obj dfxdkbd5.obj                           \
	                                                    \
	ibmcvt.kbd toshiba.kbd tandy.kbd

INC = -X -I..\inc -I..\..\inc

goal: $(ALL)

##### Default rule for MASM #####

.SUFFIXES:
.SUFFIXES: .asm .lst .obj .o .kbd

.asm.lst:
	$(TL)\masm $(INC) $(DEFS) $*.asm,,$*.lst;

.asm.obj:
	$(TL)\masm $(INC) $(DEFS) $*.asm;

.asm.kbd:
	$(TL)\masm $(INC) $(DEFS) $*.asm;
	$(TL)\link $*.obj,$*.exe;
	$(TL)\exe2bin $*.exe $*.kbd
	rm $*.obj $*.exe

clean:
	if exist *.kbd del *.kbd
	if exist *.obj del *.obj
	if exist *.exe del *.exe
	if exist *.lst del *.lst

#############################################################################
# Standard DEBUG drivers (support NMI on SYSREQ)

dfx_kbd3.obj: fx_kbd3.obj
	$(TL)\masm $(INC) -DDEBUG fx_kbd3.asm,dfx_kbd3.obj;

dfx_kbd5.obj: fx_kbd5.obj
	$(TL)\masm $(INC) -DDEBUG fx_kbd5.asm,dfx_kbd5.obj;

#############################################################################
# Kanji driver

fx_kbdk.obj: fx_kbd3.obj
	$(TL)\masm $(INC) -DKANJI -DOAX fx_kbd3.asm,fx_kbdk.obj;

dfx_kbdk.obj: fx_kbd3.obj
	$(TL)\masm $(INC) -DDEBUG -DKANJI -DOAX fx_kbd3.asm,dfx_kbdk.obj;

#############################################################################
# WORDTSR support

fx_kbdw.obj: fx_kbd3.obj
	$(TL)\masm $(INC) -DWORDTSR fx_kbd3.asm,fx_kbdw.obj;

dfx_kbdw.obj: fx_kbd3.obj
	$(TL)\masm $(INC) -DDEBUG -DWORDTSR fx_kbd3.asm,dfx_kbdw.obj;

#############################################################################
# DUAL

fxdkbdw.obj: fx_kbd3.obj
	$(TL)\masm $(INC) -DDUAL -DDUALDOS3         -DWORDTSR fx_kbd3.asm, fxdkbdw.obj;

dfxdkbdw.obj: fx_kbd3.obj
	$(TL)\masm $(INC) -DDUAL -DDUALDOS3 -DDEBUG -DWORDTSR fx_kbd3.asm,dfxdkbdw.obj;

fxdkbd5.obj: fx_kbd5.obj
	$(TL)\masm $(INC) -DDUAL -DDUALOS2                    fx_kbd5.asm, fxdkbd5.obj;

dfxdkbd5.obj: fx_kbd5.obj
	$(TL)\masm $(INC) -DDUAL -DDUALOS2  -DDEBUG           fx_kbd5.asm,dfxdkbd5.obj;

#############################################################################

dep:
	$(TL)\sed -n -e "1,/D\ependencies (from dep)/p" makefile	>makefile.new
	..\..\..\cw\tools\mkdep -I ../inc -I ../../inc *.c *.asm	>>makefile.new
	$(TL)\mv makefile.new makefile

O=obj

###################################
##### Dependencies (from dep) #####
fx_kbd3.$O : \
	../inc/kbd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inkbd.inc  \
	../../inc/inmou.inc ../../inc/vkey.inc ../../inc/fxdrv.inc kbd3.inc  \
	../inc/scan3.inc ../inc/tsr3.inc ../inc/bios.inc ../inc/kbd_data.inc  \
	fx_data.asm keytable.asm ../inc/kbd_ibm.asm keyacc3.asm keyddl3.asm  \
	keyint3.asm keyxlat.asm ../inc/kbd_std.asm ../inc/kbd_tail.asm

fx_kbd5.$O : \
	../inc/kbd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inkbd.inc  \
	../../inc/inmou.inc ../../inc/vkey.inc ../../inc/fxdrv.inc  \
	../inc/scan3.inc kbd5.inc ../inc/kbd_data.inc fx_data.asm  \
	keytable.asm keymon5.asm keyxlat.asm ../inc/kbd_ibm.asm  \
	../inc/kbd_std.asm ../inc/kbd_tail.asm

ibmcvt.$O : \
	../inc/kbd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inkbd.inc  \
	../../inc/inmou.inc ../../inc/vkey.inc kbd3.inc ../inc/scan3.inc  \
	../inc/tsr3.inc ../inc/bios.inc ../inc/kbd_data.inc  \
	../inc/kbd_code.asm keytable.asm ../inc/kbd_ibm.asm keyacc3.asm  \
	keyddl3.asm keyint3.asm keyxlat.asm ../inc/kbd_std.asm  \
	../inc/kbd_tail.asm

tandy.$O : \
	../inc/kbd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inkbd.inc  \
	../../inc/inmou.inc ../../inc/vkey.inc kbd3.inc ../inc/scan3.inc  \
	../inc/tsr3.inc ../inc/bios.inc ../inc/kbd_data.inc  \
	../inc/kbd_code.asm tantable.asm ../inc/kbd_ibm.asm keyacc3.asm  \
	keyint3.asm tanxlat.asm ../inc/kbd_std.asm ../inc/kbd_tail.asm

toshiba.$O : \
	../inc/kbd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inkbd.inc  \
	../../inc/inmou.inc ../../inc/vkey.inc kbd3.inc ../inc/scan3.inc  \
	../inc/tsr3.inc ../inc/bios.inc ../inc/kbd_data.inc  \
	../inc/kbd_code.asm keytable.asm ../inc/kbd_ibm.asm keyacc3.asm  \
	keyddl3.asm keyint3.asm keyxlat.asm ../inc/kbd_std.asm  \
	../inc/kbd_tail.asm
