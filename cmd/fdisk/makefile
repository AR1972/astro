#************************** makefile for cmd\fdisk ***************************
MSG_DIR=$(LANG_SRC)\$(COUNTRY)\cmd\fdisk

msg	=..\..\messages
inc	=..\..\inc
hinc    =..\..\h
doslib  =..\..\lib
here	=..\cmd\fdisk
extasw	=-I$(inc) -D$(COUNTRY)
extcsw	=-I$(hinc) -I$(MSG_DIR)
bins    =..\..\binaries
cprs    =..\..\compress

dest    =fdisk.exe
pkl     =fdisk.pkl

#
####################### dependencies begin here. #########################
#


!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(dest)

$(bins)\$(dest): $(pkl)
            copy $(pkl) $(bins)\$(dest)

$(cprs)\$(dest): $(pkl)
            copy $(pkl) $(cprs)\$(dest)

$(pkl): $(dest)
            pklite -o $(dest) $(pkl)


!ENDIF



# Handle the FDISK 'C' source compilations first

fdiskm.c  : $(msg)\$(COUNTRY).msg fdisk.msg
        menubld fdisk.msg  $(msg)\$(COUNTRY).MSG

fdisk5.cl1 : fdisk5.skl $(msg)\$(COUNTRY).msg

fdisk.ctl : fdisk.skl $(msg)\$(COUNTRY).msg

main.obj : main.c \
           $(MSG_DIR)\fdisk.h subtype.h extern.h fdiskmsg.h msgret.h doscall.h

fdisk.obj : fdisk.c \
            $(MSG_DIR)\fdisk.h subtype.h extern.h fdiskmsg.h doscall.h msgret.h

mainmenu.obj : mainmenu.c $(MSG_DIR)\fdisk.h subtype.h extern.h fdiskmsg.h

display.obj : display.c \
              $(MSG_DIR)\fdisk.h subtype.h extern.h fdiskmsg.h doscall.h

d_menus.obj : d_menus.c $(MSG_DIR)\fdisk.h subtype.h extern.h fdiskmsg.h

c_menus.obj : c_menus.c $(MSG_DIR)\fdisk.h subtype.h extern.h fdiskmsg.h

input.obj : input.c $(MSG_DIR)\fdisk.h subtype.h extern.h fdiskmsg.h doscall.h

tdisplay.obj : tdisplay.c $(MSG_DIR)\fdisk.h subtype.h extern.h fdiskmsg.h

vdisplay.obj : vdisplay.c $(MSG_DIR)\fdisk.h subtype.h extern.h fdiskmsg.h

space.obj : space.c $(MSG_DIR)\fdisk.h subtype.h extern.h

partinfo.obj : partinfo.c $(MSG_DIR)\fdisk.h subtype.h extern.h

makepart.obj : makepart.c $(MSG_DIR)\fdisk.h subtype.h extern.h

rempart.obj : rempart.c $(MSG_DIR)\fdisk.h subtype.h extern.h

video.obj : video.c $(MSG_DIR)\fdisk.h extern.h subtype.h fdiskmsg.h doscall.h

int13.obj : int13.c $(MSG_DIR)\fdisk.h subtype.h extern.h fdiskmsg.h

diskout.obj : diskout.c $(MSG_DIR)\fdisk.h subtype.h extern.h

fdparse.obj : fdparse.c $(MSG_DIR)\fdisk.h subtype.h extern.h parse.h msgret.h

convert.obj : convert.c $(MSG_DIR)\fdisk.h subtype.h extern.h

global.obj : global.c $(MSG_DIR)\fdisk.h makefile

fdiskm.obj : fdiskm.c

messages.obj : messages.c msgret.h extern.h subtype.h $(MSG_DIR)\fdisk.h


# Handle the FDISK MASM source


fdboot.obj : fdboot.asm fdisk5.cl1

fdboot.inc: fdboot.obj
        link fdboot;
        reloc fdboot fdboot.bin
        del fdboot.exe
        dbof fdboot.bin fdboot.inc 600 200

_parse.obj : _parse.asm \
             $(inc)\version.inc \
             $(inc)\psdata.inc \
             $(inc)\parse.asm \
             makefile

_msgret.obj : _msgret.asm $(inc)\psdata.inc \
             fdisk.ctl \
             makefile

bootrec.obj : bootrec.asm fdboot.inc \
              makefile

reboot.obj : reboot.asm \
             makefile

# Do the link of FDISK

fdisk.exe:    fdisk.obj reboot.obj bootrec.obj fdboot.obj \
              display.obj input.obj tdisplay.obj vdisplay.obj \
              space.obj partinfo.obj video.obj makepart.obj rempart.obj\
              int13.obj diskout.obj messages.obj fdparse.obj \
              convert.obj global.obj fdiskm.obj main.obj \
              c_menus.obj d_menus.obj mainmenu.obj _msgret.obj  \
              $(doslib)\mapper.lib makefile fdisk.lnk fdisk.ctl _parse.obj \
              $(doslib)\comsubs.lib
              link @fdisk.lnk
