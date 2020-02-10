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



file_io.lib:	f_absdel.obj	\
		f_exists.obj	\
		f_isdir.obj	\
		f_rdwr.obj	\
		f_ren.obj	\
		f_replac.obj
	del file_io.lib
	lib @libfile
!IFDEF BUILD_L
	cd  large
	del file_io.lib
	lib @..\libfile
	cd  ..
!ENDIF
!IFDEF BUILD_M
	cd  medium
	del file_io.lib
	lib @..\libfile
	cd  ..
!ENDIF

f_absdel.obj:	f_absdel.c	$(include)\alias.h $(include)\disk_io.h

f_exists.obj:	f_exists.c	$(include)\disk_io.h

f_isdir.obj:	f_isdir.c	$(include)\disk_io.h

f_rdwr.obj:	f_rdwr.c 	$(include)\disk_io.h

f_ren.obj:	f_ren.c		$(include)\disk_io.h

f_replac.obj:	f_replac.c	$(include)\disk_io.h
