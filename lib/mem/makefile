# Makefile for mem.lib
#
inc =..\..\inc
doslib  =..
#
#
#######################	dependencies begin here. #########################
#

all:	      $(doslib)\mem.lib

clean:
	if exist *.obj	     del *.obj
	if exist *.exe	     del *.exe
	if exist *.map	     del *.map
	if exist *.sym	     del *.sym
	if exist *.lib	     del *.lib

mem.obj:      mem.asm                   \
              makefile                  \
              $(inc)\cmacros.inc

mem.lib:      mem.obj
	      if exist mem.lib del mem.lib
              lib mem+mem;

$(doslib)\mem.lib: mem.lib
              copy mem.lib $(doslib)
