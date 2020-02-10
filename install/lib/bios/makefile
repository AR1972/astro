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
	$(cc) -AM $(cflags) -Folarge\$*.obj $*.c
!ENDIF


include	= ..\..\include
extasw	= -I.. -I..\..
extcsw	= -Os -Gs -I. -I$(include) -c



bios_io.lib:	country.obj	\
		ctrlbrk.obj	\
		datavers.obj	\
		doint2f.obj	\
		findstr.obj	\
		getbrk.obj	\
		ibm_id.obj	\
		keyboard.obj	\
		memfree.obj	\
		reboot.obj	\
		setbrk.obj	\
		tsr_tog.obj	\
		video.obj 	\
		auxputs.obj	\
		win_chk.obj
	del bios_io.lib
	lib @libfile
!IFDEF BUILD_L
	cd  large
	del bios_io.lib
	lib @..\libfile
	cd  ..
!ENDIF

!IFDEF BUILD_M
	cd  medium
	del bios_io.lib
	lib @..\libfile
	cd  ..
!ENDIF


country.obj large\country.obj	: country.asm bios_io.inc

ctrlbrk.obj	: ctrlbrk.asm bios_io.inc

datavers.obj	: datavers.asm bios_io.inc

doint2f.obj	: doint2f.asm bios_io.inc

dummy.obj	: ctrlbrk.asm bios_io.inc

findstr.obj	: findstr.asm bios_io.inc

getbrk.obj	: getbrk.asm bios_io.inc

ibm_id.obj	: ibm_id.asm bios_io.inc

keyboard.obj	: keyboard.asm bios_io.inc

memfree.obj	: memfree.asm bios_io.inc

reboot.obj	: reboot.asm bios_io.inc

setbrk.obj	: setbrk.asm bios_io.inc

tsr_tog.obj	: tsr_tog.asm bios_io.inc

video.obj	: video.asm video.inc

auxputs.obj	: auxputs.asm bios_io.inc

win_chk.obj	: win_chk.asm bios_io.inc
