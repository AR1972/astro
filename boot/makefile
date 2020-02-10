#******************** makefile for boot *****************************

msg	=..\messages
inc	=..\inc
extasw  =-I$(inc)

#
#################### dependencies begin here ############################
#

all: msboot.bin

boot.cl1:     boot.skl                  \
              $(msg)\$(COUNTRY).MSG     \
              makefile

msboot.obj:   msboot.asm boot.cl1 $(inc)\version.inc $(inc)\dirent.inc

msboot.bin:   msboot.obj
              link msboot;
              reloc msboot.exe msboot.bin
              dbof msboot.bin boot.inc 7c00 200
