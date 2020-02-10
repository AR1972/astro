bins = ..\..\binaries
cprs = ..\..\compress
dest = choice.com
cpr  = choice.co_

########################### International Changes #######################

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\c6ers\choice

#########################################################################

all: $(dest) $(bins)/$(dest) $(cprs)\$(cpr)

$(bins)\$(dest): $(dest)
	copy $(dest) $(bins)

$(cprs)\$(cpr):  $(dest)
	copy $(dest) $(cprs)


$(dest):        choice.obj 
	ml /AT choice.obj


choice.obj:     choice.asm $(MSG_DIR)\choice.inc
	ml /c /AT /I$(MSG_DIR) choice.asm  
