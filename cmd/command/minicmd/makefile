dest    =command.16 romhead.bin
loc     =..\..\..\romimg
inc	=..\..\..\inc
extasw  =-I$(loc) -I$(inc) -DROMDOS

all: $(dest)

romhead.bin:   romhead.asm $(loc)\command.loc
	       $(asm) $(aflags) romhead,,;
	       link /nologo /li romhead,,;
	       reloc romhead.exe romhead.bin

minicmd.obj: minicmd.asm minicmd.inc dataseg.asm dataseg.inc $(inc)\msbdata.inc

minicmd.exe minicmd.map: minicmd.obj
	link /M minicmd.obj,,minicmd.map;

command.16: minicmd.exe minicmd.map
	exe2bin minicmd.exe minicmd.com
	split minicmd.com minicmd.map UN_INITIALIZED_DATASEG mini1.tmp mini2.tmp
	del mini2.tmp
	if exist command.16 del command.16
	rename  mini1.tmp command.16
