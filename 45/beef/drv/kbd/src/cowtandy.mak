############################################################################
##
## Makefile for COWTANDY.LIB (used for TANDY 1000 keyboard driver and
## detection code installed in QBASIC
##
## Depends on the TL environment variable for tools
##
############################################################################

cowtandy.lib :  chktandy.obj fx_tandy.obj
        if exist cowtandy.lib del cowtandy.lib
	$(TL)\lib cowtandy +chktandy.obj +fx_tandy.obj;

chktandy.obj : chktandy.asm
        $(TL)\masm -MX -X -I..\inc -I..\..\inc chktandy.asm;

fx_tandy.obj : fx_tandy.asm
        $(TL)\masm -MX -I..\inc -I..\..\inc fx_tandy.asm, fx_tndy;
	..\..\..\cw\tools\modobj -e cowtandy.txt -o fx_tandy.obj fx_tndy.obj
