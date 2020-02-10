# use path=dos50oak\cmd\interlnk\tools\bin
#     lib=dos50oak\cmd\interlnk\tools\lib
#     include=dos50oak\cmd\interlnk\tools\include
# C 6.00
# MASM 5.10

incs    =..\include
asw     =-Ml -Dmemmodel=SMALL -Dlanguage=PASCAL -I$(incs)
csw     =-c -nologo -AS -Zl -Zp -Osleazrg -Gsr -I$(incs)
dest    =plibs.lib

all:            $(dest)

$(dest):        bioskybd.obj            \
                drawbox.obj             \
                justify.obj             \
                loadherc.obj            \
                screen.obj              \
                scrsave.obj             \
                makefile                \
                plibs.lrf
                del plibs.lib
                lib @plibs.lrf


bioskybd.obj:   bioskybd.c              \
                $(incs)\bioskybd.h      \
	        $(incs)\osdep.h         \
                $(incs)\umfunc.h        \
                makefile
                cl $(csw) bioskybd.c

drawbox.obj:    drawbox.c               \
                $(incs)\osdep.h         \
                $(incs)\screen.h        \
	        $(incs)\umfunc.h        \
                makefile
                cl $(csw) drawbox.c
                

justify.obj:    justify.c               \
                $(incs)\osdep.h         \
                $(incs)\screen.h        \
	        $(incs)\umfunc.h        \
                makefile
                cl $(csw) justify.c

loadherc.obj:   loadherc.c              \
                $(incs)\osdep.h         \
                makefile
                cl $(csw) loadherc.c

screen.obj:     screen.c                \
                $(incs)\osdep.h         \
                $(incs)\screen.h        \
	        $(incs)\umfunc.h        \
                makefile
                cl $(csw) screen.c

scrsave.obj:    scrsave.c               \
                $(incs)\osdep.h         \
                $(incs)\screen.h        \
	        $(incs)\umfunc.h        \
                makefile
                cl $(csw) scrsave.c


biostick.obj:   biostick.asm           \
                makefile
                masm $(asw) biostick.asm;

b_keybrd.obj:   b_keybrd.asm           \
                makefile
                masm $(asw) b_keybrd.asm;

fmemcmpf.obj:   fmemcmpf.asm           \
                $(incs)\DOS.MAC        \
                $(incs)\osdep.mac      \
                makefile
                masm $(asw) fmemcmpf.asm;

fstrcpy.obj:    fstrcpy.asm            \
                $(incs)\DOS.MAC        \
                $(incs)\osdep.mac      \
                makefile
                masm $(asw) fstrcpy.asm;

htoi.obj:       htoi.asm               \
                $(incs)\DOS.MAC        \
                $(incs)\osdep.mac      \
                makefile
                masm $(asw) htoi.asm;

initscr.obj:    initscr.asm            \
                makefile
                masm $(asw) initscr.asm;

memcpyf.obj:    memcpyf.asm            \
                $(incs)\DOS.MAC        \
                $(incs)\osdep.mac      \
                makefile
                masm $(asw) memcpyf.asm;

nullproc.obj:   nullproc.asm           \
                makefile
                masm $(asw) nullproc.asm;

pstrlen.obj:    pstrlen.asm            \
                makefile
                masm $(asw) pstrlen.asm;

readtick.obj:   readtick.asm           \
                makefile
                masm $(asw) readtick.asm;

scrmem.obj:     scrmem.asm             \
                makefile
                masm $(asw) scrmem.asm;

setcbrk.obj:    setcbrk.asm            \
                $(incs)\DOS.MAC        \
                $(incs)\osdep.mac      \
                makefile
                masm $(asw) setcbrk.asm;

strlenf.obj:    strlenf.asm            \
                makefile
                masm $(asw) strlenf.asm;

strrepc.obj:    strrepc.asm            \
                $(incs)\DOS.MAC        \
                $(incs)\osdep.mac      \
                makefile
                masm $(asw) strrepc.asm;

strtcpy.obj:    strtcpy.asm            \
                $(incs)\DOS.MAC        \
                $(incs)\osdep.mac      \
                makefile
                masm $(asw) strtcpy.asm;

strtcpyf.obj:   strtcpyf.asm           \
                $(incs)\DOS.MAC        \
                $(incs)\osdep.mac      \
                makefile
                masm $(asw) strtcpyf.asm;

