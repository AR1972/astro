# nmake make file for cmd\command\romstub

inc=..\..\..\inc


command.com:	command.exe
                reloc command command.com

command.exe:	command.asm  $(inc)\oemconf.inc \
		$(inc)\version.inc $(inc)\versiona.inc $(inc)\syscall.inc
		masm /t /w2 /i$(inc) command,;
		link /co command,,;

