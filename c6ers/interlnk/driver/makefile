# VERSION_NUM is the version number shown on the screen
# MAJOR_VERSION is the internal major version number
# MINOR_VERSION is the internal minor version number

VERSION_NUM = 1.00
MAJOR_VER   = 1
MINOR_VER   = 00

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\c6ers\interlnk\driver

extasw  =/DMAJOR_VER=$(MAJOR_VER) /DMINOR_VER=$(MINOR_VER)

bins    =..\..\..\binaries
cprs    =..\..\..\compress
dest    =interlnk.exe
cpr     =interlnk.ex_

#
####################### Dependencies Begin Here ############################
#

!IFNDEF         BUILDER

all: iclean $(dest)

!ELSE

sll: iclean $(dest) $(bins)\$(dest) $(cprs)\$(cpr)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)

$(cprs)\$(cpr):  $(cpr)
            copy $(cpr) $(cprs)

$(cpr):          $(dest)
                 $(compress) $(dest) $(cpr)
!ENDIF

.asm.obj:
                ml /c /Cp /nologo $(awarn) $(extasw) $*.asm

$(dest):        interdos.exe vild.obj
                link386 @vild.lnk
                addhdr $(dest)

interdos.exe:   core.obj                \
                block.obj               \
                serial.obj              \
                printer.obj             \
                init.obj                \
                drmain.obj              \
                msg.obj                 \
                parallel.obj
                link /nologo core+block+serial+printer+init+drmain+msg+parallel,interdos/noi;

vild.obj:       vild.asm                \
                VCD.INC                 \
                VMM.INC                 \
                VSD.INC                 \
                VTD.INC                 \
                makefile
                masm5 -t -p -Mx $(ainc) $*.ASM;

block.obj:      block.asm               \
                debug.mac               \
                drivers.mac             \
                packets.mac             \
                makefile

core.obj:       core.asm                \
                debug.mac               \
                drivers.mac             \
                packets.mac             \
                makefile

drmain.obj:     drmain.asm              \
                debug.mac               \
                drivers.mac             \
                packets.mac             \
                makefile

init.obj:       init.asm                \
                debug.mac               \
                drivers.mac             \
                packets.mac             \
                makefile

msg.asm:
		copy $(MSG_DIR)\msg.asm

msg.obj:        msg.asm                 \
                makefile
                ml /c /nologo $(awarn) $(extasw) /Cp  /DVERSION_NUM=\"$(VERSION_NUM)\" msg.asm

parallel.obj:   parallel.asm            \
                debug.mac               \
                drivers.mac             \
                makefile

printer.obj:    printer.asm             \
                debug.mac               \
                drivers.mac             \
                packets.mac             \
                makefile

serial.obj:     serial.asm              \
                debug.mac               \
                drivers.mac             \
                makefile


#International Modifications
iclean:
		del msg.obj
		del msg.asm
