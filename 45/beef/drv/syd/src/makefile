#############################################################################
#	Make file for SYD Drivers
#############################################################################

ALL = template.syd fx_syd3.obj fx_syd5.obj fxdsyd3.obj fxdsyd5.obj
INC = -X -I..\inc -I..\..\inc

goal: $(ALL)

##### Default rule for MASM #####

.SUFFIXES:
.SUFFIXES: .asm .lst .obj .o .syd

.asm.lst:
	$(TL)\masm $(INC) $(DEFS) $*.asm,,$*.lst;

.asm.obj:
	$(TL)\masm $(INC) $(DEFS) $*.asm;

.asm.syd:
	$(TL)\masm $(INC) $(DEFS) $*.asm;
	$(TL)\link $*.obj,$*.exe;
	$(TL)\exe2bin $*.exe $*.syd
	rm $*.obj $*.exe

clean:
	if exist *.syd del *.syd
	if exist *.obj del *.obj
	if exist *.exe del *.exe
	if exist *.lst del *.lst

#############################################################################
# DUAL

fxdsyd3.obj:
	$(TL)\masm $(INC) $(DEFS) -DDUAL -DDUALDOS3 fx_syd3.asm,fxdsyd3.obj;

fxdsyd5.obj:
	$(TL)\masm $(INC) $(DEFS) -DDUAL -DDUALOS2 fx_syd5.asm,fxdsyd5.obj;

#############################################################################

dep:
	$(TL)\sed -n -e "1,/D\ependencies (from dep)/p" makefile >makefile.new
	..\..\..\cw\tools\mkdep -I ../inc -I ../../inc *.c *.asm >>makefile.new
	$(TL)\mv makefile.new makefile

O=csd

###################################
##### Dependencies (from dep) #####
fx_syd3.$O : \
	../inc/syd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/insyd.inc  \
	../../inc/fxdrv.inc ../inc/syd_data.inc fx_data.asm  \
	../inc/syd_std.asm ../inc/syd_tail.asm

fx_syd5.$O : \
	../inc/syd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/insyd.inc  \
	../../inc/fxdrv.inc ../inc/syd_data.inc glis.inc fx_data.asm  \
	../inc/syd_std.asm ../inc/syd_tail.asm

template.$O : \
	../inc/syd_head.inc ../../inc/drv.inc ../../inc/cmacros.inc  \
	../../inc/std.inc ../../inc/indrv.inc ../../inc/insyd.inc  \
	../inc/syd_data.inc ../inc/syd_code.asm ../inc/syd_std.asm  \
	../inc/syd_tail.asm

