#************************** makefile for cmd\assign ***************************

msg	=..\..\messages
inc	=..\..\inc
extasw	=-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =assign.com
cpr     =assign.co_

#
#######################	dependencies begin here. #########################
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

assign.ctl: assign.skl \
	$(msg)\$(COUNTRY).msg \
	makefile

assgparm.obj: assgparm.asm \
        $(inc)\version.inc \
	$(inc)\psdata.inc \
	$(inc)\parse.asm \
	makefile

assgmain.obj: assgmain.asm \
	$(inc)\dosmac.inc \
	$(inc)\sysvar.inc \
	$(inc)\mult.inc \
	$(inc)\pdb.inc \
	$(inc)\syscall.inc \
	$(inc)\msgserv.asm \
	$(inc)\sysmsg.inc \
	assgparm.inc \
	assgmsg.inc \
	assign.ctl \
	$(inc)\curdir.inc \
	makefile

assign.exc: assgmain.obj assgparm.obj
        link assgmain.obj+assgparm.obj,assign.exc;

$(dest):   assign.exc

