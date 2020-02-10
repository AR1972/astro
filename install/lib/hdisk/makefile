.asm.obj:
	$(asm) -DMODEL_COMPACT $(ainc) $(aflags) $*.asm,$*.obj;
!IFDEF BUILD_L
	$(asm) -DMODEL_LARGE $(ainc) $(aflags) $*.asm,large\$*.obj;
!ENDIF
!IFDEF BUILD_M
	$(asm) -DMODEL_MEDIUM $(ainc) $(aflags) $*.asm,medium\$*.obj;
!ENDIF


.c.obj:
	$(cc) -AC $(cflags) -Fo$*.obj $*.c
!IFDEF BUILD_L
	$(cc) -AL $(cflags) -Folarge\$*.obj $*.c
!ENDIF
!IFDEF BUILD_M
	$(cc) -AM $(cflags) -Fomedium\$*.obj $*.c
!ENDIF


include	= ..\..\include
extasw	= -I.. -I..\..
extcsw	= -Os -Gs -I. -I$(include) -c



hdisk.lib:	findpart.obj	\
		hd_parms.obj	\
		partboot.obj	\
		getsec.obj	\
		fmtboot.obj	\
		hd_isfmt.obj	\
		isdosprt.obj	\
		is_dos4.obj	\
		map_ent.obj	\
		makepart.obj	\
		partmap.obj
	del hdisk.lib
	lib @libfile
!IFDEF BUILD_L
	cd  large
	del hdisk.lib
	lib @..\libfile
	cd  ..
!ENDIF
!IFDEF BUILD_M
	cd  medium
	del hdisk.lib
	lib @..\libfile
	cd  ..
!ENDIF

# ASM source files

hd_parms.obj:	hd_parms.asm ..\model.inc

partboot.obj:	partboot.asm ..\model.inc

hd_isfmt.obj:	hd_isfmt.asm ..\model.inc

# C source files

findpart.obj:	findpart.c $(include)\alias.h $(include)\hdisk.h

getsec.obj:	getsec.c $(include)\alias.h $(include)\hdisk.h

fmtboot.obj:	fmtboot.c $(include)\alias.h $(include)\hdisk.h \
		$(include)\disk_io.h

isdosprt.obj:	isdosprt.c $(include)\alias.h $(include)\hdisk.h

is_dos4.obj:	is_dos4.c $(include)\alias.h $(include)\hdisk.h \
		$(include)\disk_io.h

makepart.obj:	makepart.c $(include)\alias.h $(include)\hdisk.h

map_ent.obj:	map_ent.c $(include)\alias.h $(include)\hdisk.h

partmap.obj:	partmap.c $(include)\alias.h $(include)\hdisk.h
