#*************************** Makefile for NCFIND ***************************

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\install\cleanup
MSG_INC=$(LANG_SRC)\$(COUNTRY)\install\include

!IF	"$(DEBUG)" == "CODEVIEW" || "$(debug)" == "codeview" || \
 	"$(DEBUG)" == "CODEVIEW" || "$(debug)" == "codeview"

debug_flags = -Zi -Od
link_opts = /CO	/MAP

!ELSE

debug_flags = -Os
link_opts = /E /MAP

!ENDIF

#************************************************************************


com = ..\common
include = ..\include
bins     =..\..\binaries
cprs     =..\..\compress



!IF	"$(test)" != ""
extcsw = /AC  /c /Gs /I$(include) $(debug_flags) /DNET_SET=1
!ELSE
extcsw = /AC /c /Gs  /I$(include) $(debug_flags) /I. /DNET_SET=1
!ENDIF

ainc =

dest1   =deloldos.exe
cpr1    =deloldos.ex_

#************************************************************************

!IFNDEF       BUILDER

all: iclean icopy $(dest1)

!ELSE

all: iclean icopy $(dest1)		\
     $(bins)\$(dest1)   \
     $(cprs)\$(cpr1)

$(cprs)\$(cpr1):    $(dest1)
              compress $(dest1) $(cpr1)
              copy $(cpr1) $(cprs)

$(bins)\$(dest1): $(dest1)
              copy $(dest1) $(bins)

!ENDIF

$(include)\window.h:	icopy

icopy:
	    copy $(MSG_INC)\window.h $(include)

$(dest1):    message.txt     \
		message.h	\
		message.obj	\
		cleanup.obj	\
		extern.obj	\
		getchar.obj	\
		initial.obj	\
		memory.obj	\
		menu.obj	\
		ncfind.obj  	\
		utility.obj	\
		window.obj	\
		..\lib\common.lib	
		link $(link_opts) /ST:4096 @linkfile


#### MESSAGE.X file must always be compiled first

message.asm message.h:      message.txt
     	index	message.txt message.h /S

message.obj: 	message.asm

message.txt :	$(MSG_DIR)\global.txt $(MSG_DIR)\ncfind.txt
	copy	$(MSG_DIR)\*.txt
	copy    global.txt+ncfind.txt message.txt
	
#### End messages


extern.obj:	extern.c 		\
		global.h		\
		message.h


cleanup.obj:	cleanup.c		\
		message.h		\
		global.h


fake-ver.obj:	fake-ver.c 		\
		message.h		\
		global.h


findstr.obj:	findstr.c		\
		global.h


initial.obj:	initial.c global.h $(include)\bios_io.h \
		$(include)\disk_io.h message.h \
		$(include)\data.h $(include)\window.h \
		$(include)\alias.h $(include)\hdisk.h


intrface.obj:	intrface.c		\
		global.h		\
		message.h		\
		intrface.h



menu.obj:	menu.c			\
		message.h		\
		global.h




ncfind.obj:   	ncfind.c 		\
		$(include)\strlib.h	\
		message.h		\
		global.h  		




netset.obj:	netset.c		\
		$(include)\alias.h	\
		message.h		\
		global.h


restore.obj:	restore.c		\
		$(include)\disk_io.h	\
		$(include)\window.h	\
		global.h

		
		
update.obj:	update.c		\
		$(include)\strlib.h	\
		$(include)\copy.h	\
		$(include)\disk_io.h	\
		global.h		\
		message.h		



utility.obj:	utility.c		\
		$(include)\disk_io.h	\
		message.h		\
		global.h


	
###Common Files



data.obj:	$(com)\data.c $(include)\data.h \
		$(include)\disk_io.h $(include)\alias.h
	cl $(cflags) -Fodata.obj $(com)\data.c

fmt_io.obj:	$(com)\fmt_io.c $(include)\format.h \
		$(include)\disk_io.h global.h $(include)\bios_io.h \
		message.h $(include)\window.h $(include)\alias.h \
		$(include)\data.h $(include)\strlib.h
	cl $(cflags) -Fofmt_io.obj $(com)\fmt_io.c

getchar.obj:	$(com)\getchar.c $(include)\bios_io.h \
		$(include)\strlib.h $(include)\alias.h \
		$(include)\window.h
	cl $(cflags) -Fogetchar.obj $(com)\getchar.c

memory.obj:	$(com)\memory.c message.h \
				$(include)\alias.h $(include)\window.h
	cl $(cflags) -Fomemory.obj $(com)\memory.c

window.obj:	window.c $(include)\bios_io.h global.h	\
		message.h $(include)\strlib.h 		\
		$(include)\window.h $(include)\alias.h
	cl $(cflags) -Fowindow.obj window.c

#International Modifications
iclean:
        del message.txt
        del $(include)\window.h
