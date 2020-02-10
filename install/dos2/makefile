#===================================================================
# DOS2 makefile
#
# Copyright (c) 1991 Microsoft Corporation.  All rights reserved.
#
# History:
#
#===================================================================

#===================================================================
#	define debug flags
#

!IF	"$(DEBUG)" == "CODEVIEW" || "$(DEBUG)" == "codeview" || \
	"$(debug)" == "CODEVIEW" || "$(debug)" == "codeview"

c_debug 	= -Zi -Od -DDEBUG
asm_debug 	= -Zi -Zd -DDEBUG
link_debug 	= /CO 
rc_debug	= -k
O     		= debug


!ELSE

c_debug 	= -Oas
asm_debug 	= 
link_debug 	= 
rc_debug	=
O     		= retail

!ENDIF

!IF     "$(LOGGING)" == "1"
logging = -DLOGGING
!ELSE
logging =
!ENDIF

#===================================================================
# Macros
#

CC      = cl $(C_FLAGS) $(DBCS_SW) -Fo$@ 
ASM 	= masm $(ASM_FLAGS) 
RC	= rc $(rc_flags)
LINK    = link $(LINK_FLAGS)
LIBS 	= mlibcew libw
OURLIBS = lib\common\medium\sulib.lib lib\compress\medium\wlzcopy.lib ..\lib\mcommon.lib
com	= ..\common
ret	= ..\retail
include = ..\include


ASM_FLAGS 	= -W2 -DMODEL_MEDIUM -I. $(asm_debug)
C_FLAGS 	= -nologo -c -AM -W3 -Gsw -G2 -Zdp $(c_debug) $(logging) -I. -I$(include) -I..\retail
RC_FLAGS	= $(rc_debug)
LINK_FLAGS	= $(link_debug) /NOE/LI/MAP/NOD/ALIGN:16
DEFFILE 	= dos2.def
DBCS_SW 	=
LST		= -Fl$(*R).lst


#===================================================================
# Inference rules
#

.asm{$(O)}.obj:
    $(ASM) $(*B).asm, $@, $(*R).lst,,

.c{$(O)}.obj:
    $(CC) $(*B).c

.rc{$(O)}.res:
    $(RC) -r -FO $@ $(@B).rc


#===================================================================
# Dependencies
#

goal: checkdir all

checkdir:
   if not exist $(O)\nul md $(O)

all: $(O)\dossetup.exe

#
# Object files
#

OBJLST = $(O)\dos2.obj \
 $(O)\pro.obj \
 $(O)\upginit.obj \
 $(O)\auto.obj \
 $(O)\autoconf.obj \
 $(O)\config.obj \
 $(O)\extern.obj \
 $(O)\fake-ver.obj \
 $(O)\fixboot.obj \
 $(O)\fmt_io.obj \
 $(O)\harddisk.obj \
 $(O)\resolve.obj \
 $(O)\towin.obj \
 $(O)\saveboot.obj \
 $(O)\setup.obj \
 $(O)\upgrade.obj \
 $(O)\boot.obj \
 $(O)\wsdosdlg.obj \
 $(O)\sbutton.obj \
 $(O)\wsexit.obj \
 $(O)\data.obj \
 $(O)\b_disk.obj \
 $(O)\d_setvec.obj \
 $(O)\getinfo.obj \
 $(O)\dsk_rset.obj  
		

$(O)\dos2.obj:		dos2.c winenv.h pro.h wsrc.h ws.h doscode.h \
			sbutton.h lib\common\sulib.h $(include)\install.h

$(O)\auto.obj:		auto.c ..\retail\global.h $(include)\alias.h \
			$(include)\strlib.h $(include)\window.h prompts.h \
			errors.h $(include)\data.h

$(O)\autoconf.obj:	autoconf.c $(include)\disk_io.h prompts.h \
			..\retail\global.h  $(include)\window.h \
			$(include)\alias.h $(include)\strlib.h

$(O)\boot.obj:		boot.asm everex.inc boot.inc fdboot.inc

$(O)\config.obj:	config.c ..\retail\global.h $(include)\install.h \
			$(include)\alias.h $(include)\strlib.h errors.h \
			$(include)\data.h $(include)\window.h prompts.h

$(O)\extern.obj:	extern.c ..\retail\global.h

$(O)\fake-ver.obj:	fake-ver.c ..\retail\global.h \
			$(include)\alias.h $(include)\disk_io.h \
			$(include)\data.h

$(O)\fixboot.obj:	fixboot.c ..\retail\global.h upgrade.h \
			$(include)\alias.h $(include)\disk_io.h \
			$(include)\hdisk.h $(include)\install.h

$(O)\fmt_io.obj:	fmt_io.c  ..\retail\global.h $(include)\format.h \
			$(include)\disk_io.h $(include)\alias.h wsrc.h \
			$(include)\data.h $(include)\strlib.h prompts.h

$(O)\harddisk.obj:	harddisk.c upgrade.h  ..\retail\global.h \
			$(include)\alias.h wsrc.h $(include)\data.h \
			$(include)\disk_io.h $(include)\hdisk.h \
			$(include)\window.h 

$(O)\towin.obj:		towin.c errors.h winenv.h ws.h wsrc.h pro.h \
			lib\common\sulib.h

$(O)\saveboot.obj:	$(ret)\saveboot.c ..\retail\global.h upgrade.h \
			$(include)\alias.h $(include)\disk_io.h \
			$(include)\hdisk.h
		$(CC) $(ret)\saveboot.c

$(O)\setup.obj:		setup.c ..\retail\global.h upgrade.h \
			$(include)\copy.h $(include)\disk_io.h \
			$(include)\strlib.h $(include)\window.h \
			$(include)\alias.h prompts.h wsrc.h errors.h \
			$(include)\hdisk.h $(include)\data.h

$(O)\upgrade.obj:	upgrade.c upgrade.h ..\retail\global.h \
			$(include)\window.h $(include)\alias.h \
			$(include)\disk_io.h $(include)\data.h \
			$(include)\copy.h $(include)\strlib.h \
			$(include)\install.h prompts.h wsrc.h errors.h

$(O)\wsdosdlg.obj:	wsdosdlg.c winenv.h ws.h wsrc.h pro.h \
			lib\common\sulib.h

$(O)\pro.obj:		pro.c winenv.h wsrc.h pro.h lib\common\sulib.h

$(O)\resolve.obj:	resolve.c winenv.h ws.h wsrc.h pro.h \
			lib\common\sulib.h $(include)\install.h \
			..\retail\global.h $(include)\data.h

$(O)\upginit.obj:	upginit.c  errors.h ..\retail\global.h upgrade.h	\
			$(include)\alias.h $(include)\bios_io.h	\
			$(include)\data.h $(include)\disk_io.h prompts.h \
			$(include)\strlib.h $(include)\install.h

$(O)\sbutton.obj:	sbutton.c sbutton.h

$(O)\wsexit.obj:	wsexit.c winenv.h lib\common\sulib.h ws.h wsrc.h

$(O)\getinfo.obj:	getinfo.asm model.inc dpmi.inc

$(O)\d_setvec.obj:	d_setvec.asm model.inc dpmi.inc

$(O)\b_disk.obj:	b_disk.asm model.inc dpmi.inc

$(O)\dsk_rset.obj:	dsk_rset.asm model.inc

$(O)\dos2.res:		dos2.rc
	    $(RC) -r -FO $@ $(@B).rc


#### Common files and libs

#$(O)\data.obj:		data.c ..\retail\global.h $(include)\data.h \
#			$(include)\disk_io.h $(include)\alias.h

$(O)\data.obj:		$(com)\data.c ..\retail\global.h $(include)\data.h \
			$(include)\disk_io.h $(include)\alias.h
		cl $(C_FLAGS) $(DBCS_SW) -Fo$@ $(com)\data.c 

lib\common\medium\sulib.lib:
  cd lib\common
  xmsmake
  cd ..\..

lib\compress\medium\wlzcopy.lib:
  cd lib\compress
  xmsmake
  cd ..\..


#
# Make our EXE
#

$(O)\dossetup.exe: makefile $(OBJLST) $(O)\dos2.res $(DEFFILE) $(OURLIBS)
    cd lib\compress
    xmsmake
    cd ..\..
    cd lib\common
    xmsmake
    cd ..\..
    $(LINK) @<<$(@B).lnk
$(OBJLST:obj =obj +^
)
$(O)\$(@B).exe
$(O)\$(@B).map
$(LIBS) $(OURLIBS)
$(DEFFILE)
<<
 cd $(O)
 mapsym $(@B).map $(@B).sym
 cd ..
 $(RC) -k $(O)\dos2.res $@
