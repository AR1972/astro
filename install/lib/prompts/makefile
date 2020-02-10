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



prompts.lib:	error.obj	\
		header.obj	\
		message.obj	\
		mstring.obj	\
		dfltprmt.obj	\
		dskerror.obj	\
		dskprmpt.obj	\
		fmtstat.obj	\
		display.obj	\
		newscr.obj	\
		gage.obj
	del prompts.lib
	lib @libfile
!IFDEF BUILD_L
	cd  large
	del prompts.lib
	lib @..\libfile
	cd  ..
!ENDIF
!IFDEF BUILD_M
	cd  medium
	del prompts.lib
	lib @..\libfile
	cd  ..
!ENDIF

error.obj:	error.c $(include)\alias.h $(include)\bios_io.h \
		$(include)\window.h

header.obj:	header.c $(include)\alias.h $(include)\bios_io.h \
		$(include)\window.h

message.obj:	message.c $(include)\alias.h

mstring.obj:	mstring.c $(include)\alias.h

dfltprmt.obj:	dfltprmt.c $(include)\alias.h $(include)\window.h

dskerror.obj:	dskerror.c $(include)\alias.h $(include)\window.h

dskprmpt.obj:	dskprmpt.c $(include)\alias.h $(include)\window.h

fmtstat.obj:	fmtstat.c  $(include)\alias.h $(include)\window.h \
		$(include)\bios_io.h $(include)\strlib.h

display.obj:	display.c $(include)\window.h \
		$(include)\bios_io.h $(include)\alias.h

newscr.obj:	newscr.c $(include)\alias.h $(include)\strlib.h \
		$(include)\window.h

gage.obj:	gage.c $(include)\alias.h $(include)\bios_io.h \
		$(include)\window.h
