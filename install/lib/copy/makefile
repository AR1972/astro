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
extcsw	= -Os -Gs -I. -I$(include) -I..\..\..\h -c



filecopy.lib:	FILECOPY.OBJ	\
		UNPACK.OBJ
	del FILECOPY.lib
	lib @libfile
!IFDEF BUILD_L
	cd large
	del FILECOPY.lib
	lib @..\libfile
	cd ..
!ENDIF
!IFDEF BUILD_M
	cd medium
	del FILECOPY.lib
	lib @..\libfile
	cd ..
!ENDIF


FILECOPY.OBJ:	FILECOPY.C $(include)\ALIAS.H $(include)\DISK_IO.H \
		$(include)\STRLIB.H $(include)\COPY.H $(include)\WINDOW.H \
		..\..\..\h\decomp.h

#UNPACK.OBJ:	 UNPACK.ASM
