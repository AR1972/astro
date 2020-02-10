#************************** makefile for cmd\graphics ***************************

msg	=..\..\messages
inc     =..\..\inc
extasw  =-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest1   =graphics.com
cpr1    =graphics.co_
dest2   =graphics.pro
cpr2    =graphics.pr_

#
#######################	dependencies begin here. #########################
#

!IFNDEF       BUILDER

all: $(dest1)

!ELSE

all: $(dest1) $(bins)\$(dest1) $(cprs)\$(cpr1)  \
              $(bins)\$(dest2) $(cprs)\$(cpr2)

$(bins)\$(dest1): $(dest1)
            copy $(dest1) $(bins)

$(cprs)\$(cpr1):  $(cpr1)
            copy $(cpr1) $(cprs)

$(cpr1):       $(dest1)
            $(compress) $(dest1) $(cpr1)

$(bins)\$(dest2): $(dest2)
            copy $(dest2) $(bins)

$(cprs)\$(cpr2):  $(cpr2)
            copy $(cpr2) $(cprs)

$(cpr2):       $(dest2)
            $(compress) $(dest2) $(cpr2)

!ENDIF

graphics.ctl: graphics.skl $(msg)\$(COUNTRY).msg

graphics.obj: graphics.asm  grinst.ext makefile

grinst.obj: grinst.asm	grload.ext grload2.ext grctrl.ext grprint.ext \
	    grcpsd.ext	grparms.ext  grparse.ext grbwprt.ext grcolprt.ext \
            grint2fh.ext grmsg.equ  graphics.ctl  \
	    $(inc)\sysmsg.inc $(inc)\struc.inc grshar.str  makefile

grcpsd.obj: grparse.asm makefile $(inc)\parse.asm

grparse.obj: grcpsd.asm makefile

grpattrn.obj: grpattrn.asm grpattrn.str makefile

grbwprt.obj: grbwprt.asm grcommon.ext grctrl.str grshar.str makefile \
	     grpattrn.str $(inc)\struc.inc makefile

grint2fh.obj: grint2fh.asm  grload.ext	grctrl.ext grprint.ext \
	      grcpsd.ext makefile

grctrl.obj: grctrl.asm grint2fh.ext grbwprt.ext grcolprt.ext grshar.str \
	    grpattrn.str grpattrn.ext grctrl.str $(inc)\struc.inc \
	    makefile

grcolprt.obj: grctrl.str grshar.str grpattrn.str grctrl.ext  \
	      $(inc)\struc.inc grcommon.asm makefile

grload.obj: grload.asm $(inc)\struc.inc grinst.ext grshar.str grparse.ext \
	    grload2.ext grload3.ext grmsg.equ  makefile

grload2.obj: grload2.asm $(inc)\struc.inc grinst.ext grshar.str grmsg.equ \
	     grinst.ext grload.ext grparse.ext grpattrn.str grpattrn.ext \
	     makefile

grload3.obj: grload3.asm $(inc)\struc.inc grinst.ext grshar.str grmsg.equ \
	     grinst.ext grload.ext grload2.ext grparse.ext grpattrn.str \
	     grpattrn.ext makefile

grparms.obj: grparms.asm grmsg.equ grshar.str grinst.ext grparse.ext \
	     $(inc)\struc.inc  makefile

graphics.exc: graphics.obj grint2fh.obj grpattrn.obj grctrl.obj grcpsd.obj \
	      grcolprt.obj grbwprt.obj grinst.obj grparse.obj grparms.obj \
	      grload.obj grload2.obj grload3.obj graphics.lnk
              link @graphics.lnk

$(dest1):     graphics.exc
