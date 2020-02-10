#************************** makefile for doskey ***************************

inc	=..\..\inc
msg	=..\..\messages

extasw	=-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =doskey.com
cpr     =doskey.co_

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



doskey.cl1: doskey.skl $(msg)\$(COUNTRY).msg

# no default rule exists for making .cl2, so for precision's sake, make
#  the rule explicit
doskey.cl2: doskey.skl $(msg)\$(COUNTRY).msg
        nosrvbld doskey.skl $(msg)\$(COUNTRY).msg

doskey.obj:   doskey.asm                \
              gendef.inc                \
	      makefile			\
	      doskey.cl1

buffer.obj:   buffer.asm                \
              gendef.inc                \
              makefile

display.obj:  display.asm               \
              gendef.inc                \
              makefile

template.obj: template.asm              \
              gendef.inc                \
              makefile

trans.obj:    trans.asm                 \
	      doskey.cl2		\
              gendef.inc                \
	      makefile			\
	      $(inc)\versiona.inc

doskey.exc:   doskey.obj                \
              buffer.obj                \
              display.obj               \
              template.obj              \
              trans.obj                 \
              makefile
              link @doskey.lnk

$(dest):      doskey.exc
