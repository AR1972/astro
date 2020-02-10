#************************** makefile for cmd\... ***************************

msg	=..\..\messages
dos	=..\..\dos
inc	=..\..\inc
hinc	=..\..\h

#
#######################	dependencies begin here. #########################
#

map	=..\..\mapper
here	=..\cmd\adddrv

extasw = -I$(inc)
extcsw = -Gs -I$(hinc)

bins    =..\..\binaries
cprs    =..\..\compress
dest1   =adddrv.exe
dest2   =deldrv.exe
cpr1    =adddrv.ex_
cpr2    =deldrv.ex_

!IF "$(COUNTRY)" == "JAPAN" || "$(COUNTRY)" == "japan"

!IFNDEF       BUILDER

all: $(dest1) $(dest2)

!ELSE

all: $(dest1) $(bins)\$(dest1) $(cprs)\$(cpr1) \
     $(dest2) $(bins)\$(dest2) $(cprs)\$(cpr2)

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

adddrv.ctl:	adddrv.skl \
		$(msg)\$(COUNTRY).MSG

_msgret.obj: _msgret.asm \
	adddrv.ctl \
	$(inc)\msgserv.asm \
	$(inc)\sysmsg.inc  \
	$(inc)\copyrigh.inc \
	adddrv.skl

portliba.obj:	portliba.asm

adddrv.obj:	adddrv.c common.h

addsub.obj:	addsub.c

syncsigl.obj:	syncsigl.c

oem.obj:	oem.c

deldrv.obj:	deldrv.c common.h

keys.obj:	keys.asm

bio.obj:	bio.asm

hardint.obj:	hardint.asm

lpstring.obj:	lpstring.c


$(dest1):	exehigh.exe \
		adddrv.obj addsub.obj syncsigl.obj portliba.obj oem.obj \
		bio.obj hardint.obj lpstring.obj keys.obj \
		_msgret.obj
        link @adddrv.lnk
	exehigh low.exe adddrv.exe
	del low.exe

$(dest2): deldrv.obj oem.obj _msgret.obj lpstring.obj bio.obj hardint.obj \
		keys.obj
	link @deldrv.lnk

bin2txt.obj:	bin2txt.c
	cl -c bin2txt.c

bin2txt.exe:	bin2txt.obj
	link bin2txt;

init.i:	bin2txt.exe init.asm
	masm init;
	link init;
	exe2bin init.exe init.bin
	bin2txt init.bin > init.i
	del init.obj
	del init.exe
	del init.bin

exehigh.obj:	init.i exehigh.c
	cl -O -Ze -I. -c exehigh.c

exehigh.exe: exehigh.obj
	link /m/l/noi exehigh;


cleanup:
	del deldrv.exe   
	del deldrv.obj   
	del adddrv.exe   
	del _msgret.obj  
	del adddrv.cl1   
	del adddrv.cl2   
	del adddrv.cla   
	del adddrv.clb   
	del adddrv.ctl   
	del lpstring.obj 
	del hardint.obj  
	del bio.obj      
	del oem.obj      
	del portliba.obj 
	del syncsigl.obj 
	del addsub.obj   
	del adddrv.obj   
	del exehigh.map  
	del exehigh.exe  
	del exehigh.obj  
	del init.i       
	del bin2txt.exe  
	del bin2txt.obj  
	del keys.obj
!ENDIF
