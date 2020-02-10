delay.exe: delay.bas
	bascom delay.bas /o/d;
	link delay;

#------------------------------------------------------------------------
#makemsgs is a tool which produces all Standard Message related files
# for Quick BASIC.  makemsgs takes basicmsg.doc and produces qbimsgs.h,
# qbimsgs.inc and qbimsgs.asm
#
makemsgs.obj: makemsgs.c
	cc -W3 -Zi -c -I..\hd -AS -FPc -Ze makemsgs.c

encode.obj: encode.c encode.h
	cc -W3 -Zi -c -I. -AS -FPc -Ze encode.c

makemsgs.exe: makemsgs.obj encode.obj
	cc -d -Fm makemsgs.obj encode.obj

#------------------------------------------------------------------------
#makeupld is a tool which produces a batch file full of mscom directives
# to upload QB3 sources from the network (under slm) to janice
makeupld.exe: makeupld.bas
	bascom makeupld.bas /o;
	link makeupld;

#------------------------------------------------------------------------
#upldftp is a tool which produces an ftp input file full of 'put' directives
# to upload QB3 sources from the network (under slm) to janice
upldftp.exe: upldftp.bas
	bascom upldftp.bas /o;
	link upldftp;

#mapmsgs is a tool which converts the files produced by the QBI user
# interface's 'record/playback' mechanism to and from ASCII/Binary.
#
mapmsgs.obj: mapmsgs.c
	cc -W3 -Zi -c -I..\hd -AS -FPc -Ze mapmsgs.c
mapmsgs.exe: mapmsgs.obj
	link mapmsgs /CODEVIEW;

#------------------------------------------------------------------------
#optabler is a tool which produces opcode related source files for
# QuickBASIC.  optabler reads peropcod.txt - a file that contains
# information about each opcode.

optabler.obj: optabler.c
	cc -c -W3 -AM -Zi -Gs optabler.c
optabler.exe: optabler.obj
	link optabler /CODEVIEW;

redirect.exe: redirect.c
	cc redirect.c /link  /ST:1024

szscrn.exe: szscrn.c
	cc szscrn.c /link

striphdr.exe: striphdr.c
	cc striphdr.c /link

stripmap.com: stripmap.asm
	masm stripmap;
	link stripmap;
	exe2bin stripmap.exe stripmap.com

#------------------------------------------------------------------------
#buildprs constructs the QuickBASIC parse tables from bnf.prs
#
buildprs.obj: buildprs.c
	cc -Zi -c -I..\hd -AL -FPc -Ze buildprs.c
buildyac.obj: buildyac.c buildlex.c
	cc -Zi -c -I..\hd -AL -FPc -Ze buildyac.c
buildprs.exe: buildprs.obj buildyac.obj
	link buildprs+buildyac /CODEVIEW;

#------------------------------------------------------------------------
#sizesum adds summation information to the output created while making the
#   sizesh.txt target in ..\qb\makefile
sizesum.exe:sizesum.bas
	qb sizesum.bas/o;
	link sizesum;

#------------------------------------------------------------------------
#qbdump dumps the symbol table of a QB4 user library built with qlink
# NOTE: lsetargv.obj is from the C distribution disk.  If you are not
#	using Version 4 of the C compiler, this file may have to be
#	updated.
#
qldump.obj: qldump.c
	cc -Ox -AL -c qldump.c
qldump.exe: qldump.obj lsetargv.obj
	link qldump.obj+lsetargv.obj;
#------------------------------------------------------------------------
# FixMap is a map file utility for profiling
#
fixmap.obj: fixmap.c
	cc /c fixmap.c
fixmap.exe: fixmap.obj
	link fixmap;
	del fixmap.obj

#------------------------------------------------------------------------
# GrepSym is a symbol grep-ing utility for profiling
#
grepsym.obj: grepsym.c
	cc /c /Zi /Od grepsym.c
grepsym.exe: grepsym.obj
	link /CODEVIEW grepsym.obj;

#------------------------------------------------------------------------
# MkHelp makes the help file.
#
# The source for mkhelp.exe is in the TWIN project (because it is
# common to QuickBASIC and QuickC).
#
mkhelp.exe:
	@echo Get mkhelp.exe from TWIN.
