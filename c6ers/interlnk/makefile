
# VERSION_NUM is the version number shown on the screen
# MAJOR_VERSION is the internal major version number
# MINOR_VERSION is the internal minor version number

VERSION_NUM = 1.00
MAJOR_VER   = 1
MINOR_VER   = 00

VERSION     =\"Version $(VERSION_NUM)\"

C_FLAGS    = -Osleazrg -Gs
LINK_FLAGS = /exepack/f/packcode/map

FLAGS      =-ML -Dlanguage=PASCAL -Dmemmodel=SMALL
FLAGS2     =-Gr -DMAJOR_VERSION=$(MAJOR_VER) -DMINOR_VERSION=$(MINOR_VER)

incs    =paragon\include
inct    =fxlib

################## International Changes ################################
# *.h handling is different for LANG builds because cl fails - too long 
# command line

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\c6ers\interlnk

##########################################################################

extcsw  =-I$(incs) -I$(inct) $(FLAGS2) $(C_FLAGS)
extasw  =-I$(incs) -I$(inct) $(FLAGS)




libs       =paragon\lib\plibs.lib fxlib\fastlynx.lib

LINKFLAGS = /stack:20000/noe$(LINK_FLAGS)

bins = ..\..\binaries
cprs = ..\..\compress
dest = intersvr.exe
cpr  = intersvr.ex_

#
########################## Dependencies Begin Here ##########################

!IFNDEF         BUILDER

all: $(dest) 

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(cpr) 

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)

$(cprs)\$(cpr):  $(cpr)
            copy $(cpr) $(cprs)

$(cpr):          $(dest)
                 $(compress) $(dest) $(cpr)
!ENDIF         

paragon\lib\plibs.lib:
                cd paragon\lib
                $(MAKE)
                cd ..\..

fxlib\fastlynx.lib:
                cd fxlib
                $(MAKE)
                cd ..

bell.obj:       bell.asm                \
                makefile

calldvr.obj:    calldvr.asm             \
                makefile

crctab.obj:     crctab.asm              \
                makefile

ctype.obj:      ctype.asm               \
                makefile

getprog.obj:    getprog.asm             \
                makefile

int2f.obj:      int2f.asm               \
                makefile

longtoa.obj:    longtoa.asm             \
                makefile

percent.obj:    percent.asm             \
                makefile

setcerr.obj:    setcerr.asm             \
                makefile

switcher.obj:   switcher.asm            \
                makefile

version.obj:    version.c               \
                makefile
                cl -c -DVERSION="$(VERSION)" version.c

handler.obj:    handler.c               \
                $(inct)\fastlynx.h      \
                $(inct)\server.h        \
                dc.h                    \
                $(incs)\osdep.h         \
                $(incs)\umfunc.h        \
                drivers.h               \
                packets.h               \
                boxfunc.h               \
                cwuser.h                \
                userform.h              \
                str.h                   \
                $(incs)\osdep.h         \
                $(incs)\bioskybd.h      \
                $(incs)\screen.h        \
                cwfunc.h                \
                makefile

clone.obj:      clone.c                 \
                $(inct)\fastlynx.h      \
                $(inct)\server.h        \
                dc.h                    \
                $(incs)\osdep.h         \
                $(incs)\umfunc.h        \
                drivers.h               \
                packets.h               \
                boxfunc.h               \
                cwuser.h                \
                userform.h              \
                str.h                   \
                $(incs)\osdep.h         \
                $(incs)\bioskybd.h      \
                $(incs)\screen.h        \
                cwfunc.h                \
                makefile

criterr.obj:    criterr.c               \
                dc.h                    \
                $(incs)\osdep.h         \
                $(incs)\umfunc.h        \
                drivers.h               \
                packets.h               \
                boxfunc.h               \
                cwuser.h                \
                userform.h              \
                str.h                   \
                $(incs)\osdep.h         \
                $(incs)\bioskybd.h      \
                $(incs)\screen.h        \
                cwfunc.h                \
                makefile

devinfo.obj:    devinfo.c               \
                dc.h                    \
                $(incs)\osdep.h         \
                $(incs)\umfunc.h        \
                drivers.h               \
                packets.h               \
                boxfunc.h               \
                cwuser.h                \
                userform.h              \
                str.h                   \
                $(incs)\osdep.h         \
                $(incs)\bioskybd.h      \
                $(incs)\screen.h        \
                cwfunc.h                \
                makefile

drivinfo.obj:   drivinfo.c              \
                $(inct)\fastlynx.h      \
                dc.h                    \
                $(incs)\osdep.h         \
                $(incs)\umfunc.h        \
                drivers.h               \
                packets.h               \
                boxfunc.h               \
                cwuser.h                \
                userform.h              \
                str.h                   \
                $(incs)\osdep.h         \
                $(incs)\bioskybd.h      \
                $(incs)\screen.h        \
                cwfunc.h                \
                makefile
                copy $(MSG_DIR)\ipg.h
                $(CC) -c $(cinc) $(cflags) $*.c
                del ipg.h
#### Have to copy ipg.h in, otherwise cl fails - too long command line

main.obj:       main.c                  \
                $(inct)\fastlynx.h      \
                dc.h                    \
                $(incs)\osdep.h         \
                $(incs)\umfunc.h        \
                drivers.h               \
                packets.h               \
                boxfunc.h               \
                cwuser.h                \
                userform.h              \
                str.h                   \
                $(incs)\osdep.h         \
                $(incs)\bioskybd.h      \
                $(incs)\screen.h        \
                cwfunc.h                \
                makefile

print.obj:      print.c                 \
                $(inct)\fastlynx.h      \
                dc.h                    \
                $(incs)\osdep.h         \
                $(incs)\umfunc.h        \
                drivers.h               \
                packets.h               \
                boxfunc.h               \
                cwuser.h                \
                userform.h              \
                str.h                   \
                $(incs)\osdep.h         \
                $(incs)\bioskybd.h      \
                $(incs)\screen.h        \
                cwfunc.h                \
                makefile

############################ International Changes #######################
# Have to copy str.c in, otherwise cl fails - too long command line

str.obj:        $(MSG_DIR)\str.c
                copy $(MSG_DIR)\str.c
                $(CC) -c $(cinc) $(cflags) str.c
                del str.c
##########################################################################

intersvr.exe:   bell.obj                \
                calldvr.obj             \
                crctab.obj              \
                ctype.obj               \
                getprog.obj             \
                int2f.obj               \
                longtoa.obj             \
                percent.obj             \
                setcerr.obj             \
                switcher.obj            \
                clone.obj               \
                criterr.obj             \
                devinfo.obj             \
                drivinfo.obj            \
                handler.obj             \
                main.obj                \
                print.obj               \
                str.obj                 \
                version.obj             \
                intersvr.lnk            \
                $(libs)
                link $(LINKFLAGS) @intersvr.lnk 
