#*************************** Makefile for DOS ***************************

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\install\backup
MSG_INC=$(LANG_SRC)\$(COUNTRY)\install\include

!IF	"$(DEBUG)" == "CODEVIEW" || "$(DEBUG)" == "codeview" || \
	"$(debug)" == "CODEVIEW" || "$(debug)" == "codeview"

debug_flags = -Zi -Od
link_opts = /CO	/ST:4096 

!ELSE

debug_flags = -Os
link_opts = $(exelink) /ST:4096

!ENDIF

dest      =hdbkup.exe

!IFDEF JANUS
JANUS =/DJANUS
!ELSE
JANUS =
!ENDIF

#************************************************************************

bins    =..\..\binaries
cprs    =..\..\compress

com	= ..\common
inc	= ..\include
here	= ..\backup
boot    = ..\..\boot
bootret = ..\install\backup

cwarn= /W3
extcsw = /AC /c /I$(inc) $(debug_flags) /DHD_BACKUP $(JANUS) /I.
ainc = -I$(boot)

!IFNDEF       BUILDER

all: iclean icopy $(dest)

!ELSE

all: iclean icopy $(dest) $(bins)\$(dest) $(cprs)\$(dest)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)

$(cprs)\$(dest): $(dest)
            copy $(dest) $(cprs)


!ENDIF

$(inc)\window.h:	icopy

icopy:
	    copy $(MSG_INC)\window.h $(inc)


$(dest):        message.txt \
		message.asm \
		message.obj \
		$(boot)\boot.inc \
		main.obj    \
		initial.obj \
		select.obj  \
		getchar.obj \
		fmt_io.obj  \
		buffer.obj  \
		work.obj    \
		boot.obj    \
		backup.obj  \
		memory.obj  \
		window.obj  \
		getchar.obj \
		extern.obj  \
      ..\lib\common.lib
	link $(link_opts) @linkfile;

message.txt: $(MSG_DIR)\backup.txt
	copy $(MSG_DIR)\backup.txt message.txt

message.asm message.h:    message.txt
	index	message.txt message.h

message.obj:	message.asm


#### boot record include file

$(boot)\boot.inc:    $(boot)\msboot.asm
              cd $(boot)
              $(MAKE)
              cd $(bootret)

#### end boot record

boot.obj:	boot.asm $(boot)\boot.inc

main.obj:	main.c global.h $(inc)\window.h $(inc)\bios_io.h message.h \
		$(inc)\install.h

initial.obj:	initial.c global.h $(inc)\window.h $(inc)\bios_io.h message.h

select.obj:	$(com)\select.c $(inc)\window.h $(inc)\bios_io.h
	cl $(extcsw) -Foselect.obj $(com)\select.c

getchar.obj:	$(com)\getchar.c $(inc)\bios_io.h \
		$(inc)\strlib.h $(inc)\alias.h \
		$(inc)\window.h

fmt_io.obj:	fmt_io.c message.h

backup.obj:	backup.c message.h

buffer.obj:	buffer.c message.h

work.obj:	work.c message.h $(inc)\install.h

window.obj:	$(com)\window.c global.h $(inc)\window.h \
		$(inc)\bios_io.h message.h
	cl $(extcsw) -Fowindow.obj $(com)\window.c

memory.obj:	$(com)\memory.c	$(inc)\alias.h \
		$(inc)\window.h
	cl $(extcsw) -Fomemory.obj $(com)\memory.c

getchar.obj:	$(com)\getchar.c $(inc)\bios_io.h $(inc)\window.h
	cl $(extcsw) -Fogetchar.obj $(com)\getchar.c

extern.obj:	extern.c message.h

#International Modifications
iclean:
        del message.txt
        del $(inc)\window.h

