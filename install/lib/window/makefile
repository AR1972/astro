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



window.lib:	W_CENTER.OBJ	\
		W_CLS.OBJ	\
		W_CLEAN.OBJ	\
		W_COLOR.OBJ	\
		W_MOVE.OBJ	\
		W_PROMPT.OBJ	\
		W_PUTWIN.OBJ	\
		W_RESPON.OBJ	\
		W_RSTCUR.OBJ	\
		W_SAVCUR.OBJ	\
		W_SIZE.OBJ	\
		W_STRCEN.OBJ	\
		W_TWIN.OBJ	\
		W_MOVE.OBJ
	del window.lib
	lib @libfile
!IFDEF BUILD_L
	cd  large
	del window.lib
	lib @..\libfile
	cd  ..
!ENDIF
!IFDEF BUILD_M
	cd  medium
	del window.lib
	lib @..\libfile
	cd  ..
!ENDIF


W_CENTER.OBJ:	W_CENTER.C $(include)\BIOS_IO.H

W_CLS.OBJ:	W_CLS.C $(include)\ALIAS.H $(include)\BIOS_IO.H \
		$(include)\WINDOW.H

W_CLEAN.OBJ:	W_CLEAN.C $(include)\BIOS_IO.H

W_COLOR.OBJ:	W_COLOR.C $(include)\BIOS_IO.H

W_MOVE.OBJ:	W_MOVE.C  $(include)\ALIAS.H $(include)\BIOS_IO.H \
		$(include)\WINDOW.H

W_PROMPT.OBJ:	W_PROMPT.C $(include)\ALIAS.H $(include)\BIOS_IO.H \
		$(include)\WINDOW.H

W_PUTWIN.OBJ:	W_PUTWIN.C $(include)\ALIAS.H $(include)\BIOS_IO.H \
		$(include)\WINDOW.H

W_RESPON.OBJ:	W_RESPON.C $(include)\ALIAS.H $(include)\BIOS_IO.H \
		$(include)\WINDOW.H

W_RSTCUR.OBJ:	W_RSTCUR.C $(include)\BIOS_IO.H

W_SAVCUR.OBJ:	W_SAVCUR.C $(include)\BIOS_IO.H

W_SIZE.OBJ:	W_SIZE.C $(include)\ALIAS.H $(include)\BIOS_IO.H \
		$(include)\WINDOW.H $(include)\STRLIB.H

W_STRCEN.OBJ:	W_STRCEN.C $(include)\BIOS_IO.H

W_TWIN.OBJ:	W_TWIN.C $(include)\ALIAS.H $(include)\BIOS_IO.H \
		$(include)\WINDOW.H

W_MOVE.OBJ:	W_MOVE.C $(include)\ALIAS.H $(include)\BIOS_IO.H \
		$(include)\WINDOW.H
