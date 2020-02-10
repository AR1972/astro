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

extcsw = -Gs


all: adddrv.exe deldrv.exe

# adddrv.ctl:	adddrv.skl \
#		$(msg)\$(COUNTRY).MSG

# _msgret.obj: _msgret.asm \
# 	$(inc)\msgserv.asm \
# 	$(inc)\sysmsg.inc  \
# 	$(inc)\copyrigh.inc \
# 	adddrv.skl \
# 	adddrv.ctl \
# 	adddrv.cl1 \
# 	adddrv.cl2 \
# 	adddrv.cla \
# 	adddrv.clb

# portliba.obj:	portliba.asm

# adddrv.obj:	adddrv.c

# addsub.obj:	addsub.c

# syncsigl.obj:	syncsigl.c

# oem.obj:	oem.c

# deldrv.obj:	deldrv.c

keys.obj:	keys.asm

adddrv.exe : \
		adddrv.obj addsub.obj syncsigl.obj portliba.obj oem.obj \
		bio.obj hardint.obj lpstring.obj keys.obj \
		_msgret.obj
        link @adddrv.lnk
	exehigh low.exe adddrv.exe
	del low.exe

deldrv.exe : deldrv.obj oem.obj _msgret.obj lpstring.obj bio.obj hardint.obj \
		keys.obj
	link @deldrv.lnk

# exehigh.exe: exehigh.obj
# 	link /m/l/noi exehigh;

# init.i:	bin2txt.exe init.asm
# 	masm init;
# 	link init;
# 	exe2bin init.exe init.bin
# 	bin2txt init.bin > init.i
# 	del init.obj
# 	del init.exe
# 	del init.bin
# 
# exehigh.obj:	init.i exehigh.c
# 	cl -O -Ze -I. -c exehigh.c

# bin2txt.exe:	bin2txt.obj
# 	link bin2txt;

# bin2txt.obj:	bin2txt.c
# 	cl -c bin2txt.c


cleanup:
	del deldrv.exe   
	del adddrv.exe   
	del keys.obj
