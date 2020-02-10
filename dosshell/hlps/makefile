bins    =..\..\binaries
cprs    =..\..\compress

########################### International Changes #######################

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\dosshell\hlps

#########################################################################

dest    =dosshell.hlp
cpr     =dosshell.hl_

!IFNDEF       BUILDER

all: ..\$(dest)

..\$(dest): $(MSG_DIR)\$(dest)
        copy $(MSG_DIR)\$(dest) ..

!ELSE

all: $(bins)\$(dest) $(cprs)\$(cpr)

$(bins)\$(dest): $(MSG_DIR)\$(dest)
            copy $(MSG_DIR)\$(dest) $(bins)

$(cprs)\$(cpr):  $(cpr)
            copy $(cpr) $(cprs)

$(cpr):       $(MSG_DIR)\$(dest)
            $(compress) $(MSG_DIR)\$(dest) $(cpr)

!ENDIF
