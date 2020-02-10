#**     makefile for fc

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\cmd\fc

MSG	=messages
bins    =..\..\binaries
cprs    =..\..\compress
dest    =fc.exe
cpr     =fc.ex_


inc	=..\..\INC
hinc    =..\..\h
extasw	=-I$(inc)
extcsw  =-I$(hinc) -I$(MSG_DIR)

#	List of object files required

OBJ	= fc.obj \
	  syserr.obj\
	  error.obj \
	  fgetl.obj \
	  getl.obj \
	  itoupper.obj \
          kstring.obj \
	  maxmin.obj \
	  move.obj \
	  ntoi.obj \
	  string.obj \
	  update.obj \
	  xtab.obj 

#       Rules and Dependencies follow

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


fc.obj:       fc.c $(MSG_DIR)\messages.h

syserr.obj:   syserr.c $(MSG_DIR)\syserr.msg

kstring.obj:  kstring.c $(hinc)\internat.h

getl.obj:     getl.asm $(inc)\cmacros.inc $(inc)\version.inc

itoupper.obj: itoupper.asm  $(inc)\cmacros.inc $(inc)\version.inc

maxmin.obj:   maxmin.asm $(inc)\cmacros.inc $(inc)\version.inc

move.obj:     move.asm $(inc)\cmacros.inc $(inc)\version.inc

string.obj:   string.asm $(inc)\cmacros.inc $(inc)\version.inc

xtab.obj:     xtab.asm $(inc)\cmacros.inc $(inc)\version.inc

error.obj:    error.c

fc.obj:       fc.c fc.h tools.h ttypes.h

fgetl.obj:    fgetl.c tools.h ttypes.h

ntoi.obj:     ntoi.c tools.h ttypes.h

update.obj:   update.c tools.h ttypes.h

$(dest): $(OBJ) fc.lnk
        $(link) $(exelink) @fc.lnk;

