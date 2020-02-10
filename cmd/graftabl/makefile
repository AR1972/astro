#************************** makefile for cmd\graftabl *************************

msg     =..\..\messages
inc     =..\..\inc
extasw	=-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =graftabl.com
cpr     =graftabl.co_

#
####################### dependencies begin here. #########################
#

!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(cpr)

$(bins)\$(dest): $(dest)
	    copy $(dest) $(bins)

$(cprs)\$(cpr):  $(cpr)
	    copy $(cpr) $(cprs)

$(cpr):       $(dest)
	    $(compress) $(dest) $(cpr)

!ENDIF


graftabl.ctl: graftabl.skl              \
	      $(msg)\$(COUNTRY).msg     \
	      makefile

grtab.obj:    grtab.asm                 \
	      makefile

grtabp.obj:   grtabp.asm                \
	      makefile                  \
	      $(inc)\version.inc        \
	      $(inc)\psdata.inc         \
	      $(inc)\parse.asm

grtabsm.obj:  grtabsm.asm               \
	      makefile                  \
	      $(inc)\copyrigh.inc       \
	      $(inc)\versiona.inc       \
	      $(inc)\sysmsg.inc         \
	      $(inc)\msgserv.asm        \
	      graftabl.ctl              \
	      $(inc)\msghan.inc         \
	      grtabms.inc

grtabpar.obj: grtabpar.asm              \
	      makefile

grtabhan.obj: grtabhan.asm              \
	      makefile

grtabus.obj:  grtabus.asm               \
	      makefile

grtabno.obj:  grtabno.asm               \
	      makefile

grtabpo.obj:  grtabpo.asm               \
	      makefile

grtabcf.obj:  grtabcf.asm               \
	      makefile

grtabml.obj:  grtabml.asm               \
	      makefile

grtabee.obj:  grtabee.asm               \
	      makefile

graftabl.exc: grtab.obj                 \
	      makefile                  \
	      graftabl.lnk              \
	      grtabp.obj                \
	      grtabsm.obj               \
	      grtabpar.obj              \
	      grtabhan.obj              \
	      grtabus.obj               \
	      grtabno.obj               \
	      grtabpo.obj               \
	      grtabcf.obj               \
	      grtabml.obj               \
	      grtabee.obj
	      link @graftabl.lnk

$(dest):      graftabl.exc
