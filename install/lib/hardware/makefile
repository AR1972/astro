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


hardware.lib:	cmos.obj     \
		getcpu.obj   \
		getemm.obj   \
		getext.obj   \
		getmodel.obj \
		getmem.obj   \
		himem.obj    \
                ms_mice.obj  \
		smartdrv.obj \
		vfeat.obj    \
		syquest.obj  \
		ramdrv.obj
	del hardware.lib
	lib @libfile
!IFDEF BUILD_L
	cd  large
	del hardware.lib
	lib @..\libfile
	cd  ..
!ENDIF
!IFDEF BUILD_M
	cd  medium
	del hardware.lib
	lib @..\libfile
	cd  ..
!ENDIF


cmos.obj:	cmos.asm

getcpu.obj:	getcpu.asm

getemm.obj:	getemm.asm

getext.obj:	getext.asm

getmem.obj:	getmem.asm

getmodel.obj:	getmodel.asm

himem.obj:	himem.asm

ms_mice.obj:    ms_mice.asm

smartdrv.obj:	smartdrv.asm

vfeat.obj:	vfeat.asm

ramdrv.obj:	ramdrv.c $(include)\alias.h $(include)\disk_io.h
