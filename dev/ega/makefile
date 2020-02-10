# International Modifications:
MSG_DIR=$(LANG_SRC)\$(COUNTRY)\dev\ega

bins    =..\..\binaries
cprs    =..\..\compress

!IFNDEF       BUILDER

all: ega.sys

!ELSE

all: ega.sys $(bins)\ega.sys $(cprs)\ega.sy_


$(bins)\ega.sys: ega.sys
                    copy ega.sys $(bins)

ega.sy_:         ega.sys
                    $(compress) ega.sys ega.sy_

$(cprs)\ega.sy_: ega.sy_
                    copy ega.sy_ $(cprs)

!ENDIF


ega.obj:      ega.asm                   \
              ega.inc                   \
              int10rtn.inc              \
              $(MSG_DIR)\load.inc
        masm  -w2 -z -t -b63 -I$(MSG_DIR) -Dsys ega;

ega.exs:      ega.obj
        link ega,ega.exs;

ega.sys:      ega.exs
