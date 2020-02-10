#############################################################################
#	Make file for CSD Drivers
#############################################################################

ALL = fx_csd3.obj fx_csd5.obj fxdcsd3.obj fxdcsd5.obj \
	biossafe.csd cgasnow.csd compaq3.csd \
	herc102.csd herc112.csd \
	ibmtext.csd ibm3270.csd ibmcvt.csd \
	mda.csd cga.csd ega.csd mcga.csd vga.csd \
	t3100.csd tandy.csd vectra.csd genius.csd \
	amstrad.csd ericsson.csd olivetti.csd \
	vio.csd fx_twin.obj fx_twink.obj

INC = -X -I..\inc -I..\..\inc

goal: $(ALL)

##### Default rule for MASM #####

.SUFFIXES:
.SUFFIXES: .asm .lst .obj .o .csd

.asm.lst:
	$(TL)\masm $(INC) $(DEFS) $*.asm,,$*.lst;

.asm.obj:
	$(TL)\masm $(INC) $(DEFS) $*.asm;

.asm.csd:
	$(TL)\masm $(INC) $(DEFS) $*.asm;
	$(TL)\link $*.obj,$*.exe;
	$(TL)\exe2bin $*.exe $*.csd
	rm $*.obj $*.exe
	..\..\..\cw\tools\mkdrv ..\..\test\$*.vid -Lfs $*.csd

clean:
	if exist *.csd del *.csd
	if exist *.obj del *.obj
	if exist *.exe del *.exe
	if exist *.lst del *.lst

#############################################################################
# DUAL

fxdcsd3.obj:
	$(TL)\masm $(INC) $(DEFS) -DDUAL -DDUALDOS3 fx_csd3.asm,fxdcsd3.obj;

fxdcsd5.obj:
	$(TL)\masm $(INC) $(DEFS) -DDUAL -DDUALOS2 fx_csd5.asm,fxdcsd5.obj;

#############################################################################
# Special .VIDs

egavio.vid: ega.csd vio.csd
	..\..\..\cw\tools\mkdrv ..\..\test\egavio.vid -Lfr ega.csd -Lfp vio.csd

#############################################################################

dep:
	$(TL)\sed -n -e "1,/D\ependencies (from dep)/p" makefile >makefile.new
	..\..\..\cw\tools\mkdep -I ../inc -I ../../inc *.c *.asm >>makefile.new
	$(TL)\mv makefile.new makefile

O=csd

###################################
##### Dependencies (from dep) #####
amstrad.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc  \
	../inc/csd_code.asm ../inc/csd_std.asm ../inc/csd_ibm.asm  \
	../inc/csd_vram.asm ../inc/csd_save.asm ../inc/csd_tail.asm

biossafe.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc  \
	../inc/csd_code.asm ../inc/csd_std.asm ../inc/csd_ibm.asm  \
	../inc/csd_bios.asm ../inc/csd_save.asm ../inc/csd_tail.asm

cga.$O : \
	ibmfont.asm ../inc/csd_head.inc ../../inc/drv.inc  \
	../../inc/cmacros.inc ../../inc/std.inc ../../inc/indrv.inc  \
	../../inc/inscr.inc ../inc/bios.inc ../inc/csd_data.inc  \
	../inc/csd_fvm.inc ../inc/csd_code.asm genmodes.asm geninit.asm  \
	cga8x8.inc cpfont.asm ../inc/csd_std.asm ../inc/csd_ibm.asm  \
	../inc/csd_vram.asm ../inc/csd_save.asm ../inc/csd_tail.asm

cgasnow.$O : \
	ibmfont.asm ../inc/csd_head.inc ../../inc/drv.inc  \
	../../inc/cmacros.inc ../../inc/std.inc ../../inc/indrv.inc  \
	../../inc/inscr.inc ../inc/bios.inc ../inc/csd_data.inc  \
	../inc/csd_fvm.inc ../inc/csd_code.asm genmodes.asm geninit.asm  \
	cga8x8.inc cpfont.asm ../inc/csd_std.asm ../inc/csd_ibm.asm  \
	../inc/csd_vram.asm ../inc/csd_save.asm ../inc/csd_tail.asm

compaq3.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc  \
	../inc/csd_code.asm update2.asm cga8x8.inc cpfont.asm vect8x16.inc  \
	../inc/csd_std.asm ../inc/csd_ibm.asm ../inc/csd_vram.asm  \
	../inc/csd_save.asm ../inc/csd_tail.asm

ega.$O : \
	ibmfont.asm ../inc/csd_head.inc ../../inc/drv.inc  \
	../../inc/cmacros.inc ../../inc/std.inc ../../inc/indrv.inc  \
	../../inc/inscr.inc ../inc/bios.inc ../inc/csd_data.inc  \
	../inc/csd_fvm.inc ../inc/csd_code.asm genmodes.asm geninit.asm  \
	cga8x8.inc cpfont.asm ../inc/csd_std.asm ../inc/csd_ibm.asm  \
	../inc/csd_vram.asm ../inc/csd_save.asm ../inc/csd_tail.asm

ericsson.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc  \
	../inc/csd_code.asm vect8x16.inc update2.asm cga8x8.inc cpfont.asm  \
	../inc/csd_std.asm ../inc/csd_ibm.asm ../inc/csd_vram.asm  \
	../inc/csd_save.asm ../inc/csd_tail.asm

fx_csd3.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../../inc/fxdrv.inc ../inc/csd_data.inc  \
	../inc/csd_fvm.inc fx_data.asm genmodes.asm geninit.asm  \
	../inc/csd_std.asm ../inc/csd_ibm.asm ../inc/csd_vram.asm  \
	../inc/csd_save.asm ../inc/csd_tail.asm

fx_csd5.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../../inc/fxdrv.inc ../inc/csd_data.inc  \
	../inc/csd_fvm.inc scr5.inc scr5data.inc fx_data.asm scr5.asm  \
	cga8x8.inc cpfont.asm ../inc/csd_std.asm ../inc/csd_vram.asm  \
	../inc/csd_save.asm ../inc/csd_tail.asm

fx_twin.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../../inc/fxdrv.inc ../inc/csd_data.inc  \
	../inc/csd_fvm.inc fx_data.asm saveega.inc genmodes.asm saveega.asm  \
	../inc/csd_std.asm ../inc/csd_ibm.asm ../inc/csd_vram.asm  \
	../inc/csd_save.asm ../inc/csd_tail.asm

fx_twink.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../../inc/fxdrv.inc ../inc/csd_data.inc  \
	../inc/csd_fvm.inc fx_data.asm saveega.inc genmodes.asm saveega.asm  \
	../inc/csd_std.asm ../inc/csd_ibm.asm ../../inc/kanji.inc  \
	../inc/csd_oax.asm ../inc/csd_save.asm ../inc/csd_tail.asm

genius.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc  \
	../inc/csd_code.asm update2.asm cga8x8.inc cpfont.asm vect8x16.inc  \
	../inc/csd_std.asm ../inc/csd_ibm.asm ../inc/csd_vram.asm  \
	../inc/csd_save.asm ../inc/csd_tail.asm

herc.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc  \
	../inc/csd_code.asm cpfont.asm herc9x14.inc herc8x10.inc herc8x14.inc \
	herc8x8.inc ../inc/csd_std.asm ../inc/csd_ibm.asm ../inc/csd_vram.asm \
	../inc/csd_save.asm ../inc/csd_tail.asm

herc102.$O : \
	herc.asm ../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc  \
	../inc/csd_code.asm cpfont.asm herc9x14.inc herc8x10.inc herc8x14.inc \
	herc8x8.inc ../inc/csd_std.asm ../inc/csd_ibm.asm ../inc/csd_vram.asm \
	../inc/csd_save.asm ../inc/csd_tail.asm

herc112.$O : \
	herc.asm ../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc  \
	../inc/csd_code.asm cpfont.asm herc9x14.inc herc8x10.inc herc8x14.inc \
	herc8x8.inc ../inc/csd_std.asm ../inc/csd_ibm.asm ../inc/csd_vram.asm \
	../inc/csd_save.asm ../inc/csd_tail.asm

hercules.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc  \
	../inc/csd_code.asm cpfont.asm herc9x14.inc herc8x10.inc herc8x14.inc \
	herc8x8.inc ../inc/csd_std.asm ../inc/csd_ibm.asm ../inc/csd_vram.asm \
	../inc/csd_save.asm ../inc/csd_tail.asm

ibm3270.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc  \
	../inc/csd_code.asm cpfont.asm cga8x8.inc herc8x14.inc  \
	../inc/csd_std.asm ../inc/csd_ibm.asm ../inc/csd_vram.asm  \
	../inc/csd_save.asm ../inc/csd_tail.asm

ibmcvt.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc  \
	../inc/csd_code.asm update1.asm cga8x8.inc cpfont.asm  \
	../inc/csd_std.asm ../inc/csd_ibm.asm ../inc/csd_vram.asm  \
	../inc/csd_save.asm ../inc/csd_tail.asm

ibmfont.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc  \
	../inc/csd_code.asm genmodes.asm geninit.asm cga8x8.inc cpfont.asm  \
	../inc/csd_std.asm ../inc/csd_ibm.asm ../inc/csd_vram.asm  \
	../inc/csd_save.asm ../inc/csd_tail.asm

ibmtext.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc  \
	../inc/csd_code.asm genmodes.asm geninit.asm ../inc/csd_std.asm  \
	../inc/csd_ibm.asm ../inc/csd_vram.asm ../inc/csd_save.asm  \
	../inc/csd_tail.asm

mcga.$O : \
	ibmfont.asm ../inc/csd_head.inc ../../inc/drv.inc  \
	../../inc/cmacros.inc ../../inc/std.inc ../../inc/indrv.inc  \
	../../inc/inscr.inc ../inc/bios.inc ../inc/csd_data.inc  \
	../inc/csd_fvm.inc ../inc/csd_code.asm genmodes.asm geninit.asm  \
	cga8x8.inc cpfont.asm ../inc/csd_std.asm ../inc/csd_ibm.asm  \
	../inc/csd_vram.asm ../inc/csd_save.asm ../inc/csd_tail.asm

oax.$O : \
	../../inc/kanji.inc

olivetti.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc  \
	../inc/csd_code.asm update2.asm cga8x8.inc cpfont.asm vect8x16.inc  \
	../inc/csd_std.asm ../inc/csd_ibm.asm ../inc/csd_vram.asm  \
	../inc/csd_save.asm ../inc/csd_tail.asm

scr5.$O : \
	cga8x8.inc cpfont.asm

t3100.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc  \
	../inc/csd_code.asm update2.asm cga8x8.inc cpfont.asm  \
	../inc/csd_std.asm ../inc/csd_ibm.asm ../inc/csd_vram.asm  \
	../inc/csd_save.asm ../inc/csd_tail.asm

tandy.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc  \
	../inc/csd_code.asm update1.asm cga8x8.inc cpfont.asm  \
	../inc/csd_std.asm ../inc/csd_ibm.asm ../inc/csd_vram.asm  \
	../inc/csd_save.asm ../inc/csd_tail.asm

update1.$O : \
	cga8x8.inc

update2.$O : \
	cga8x8.inc cpfont.asm

vectra.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc  \
	../inc/csd_code.asm update2.asm cga8x8.inc cpfont.asm vect8x16.inc  \
	../inc/csd_std.asm ../inc/csd_ibm.asm ../inc/csd_vram.asm  \
	../inc/csd_save.asm ../inc/csd_tail.asm

vga.$O : \
	ibmfont.asm ../inc/csd_head.inc ../../inc/drv.inc  \
	../../inc/cmacros.inc ../../inc/std.inc ../../inc/indrv.inc  \
	../../inc/inscr.inc ../inc/bios.inc ../inc/csd_data.inc  \
	../inc/csd_fvm.inc ../inc/csd_code.asm genmodes.asm geninit.asm  \
	cga8x8.inc cpfont.asm ../inc/csd_std.asm ../inc/csd_ibm.asm  \
	../inc/csd_vram.asm ../inc/csd_save.asm ../inc/csd_tail.asm

vio.$O : \
	../inc/csd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/inscr.inc  \
	../inc/bios.inc ../inc/csd_data.inc ../inc/csd_fvm.inc scr5.inc  \
	scr5data.inc ../inc/csd_code.asm scr5.asm cga8x8.inc cpfont.asm  \
	../inc/csd_std.asm ../inc/csd_vram.asm ../inc/csd_save.asm  \
	../inc/csd_tail.asm

