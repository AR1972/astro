#------------------------------------------------------------------------
#
# WinOldAp -- make file for WinOldAp
#
#-------------------------------------------------------------------------



########################### International Changes #######################

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\dosshell\swapper

#########################################################################



extasw          = -DREALMODE -I.. -I$(MSG_DIR)
bins    =..\..\binaries
cprs    =..\..\compress

dest    =dosswap.exe
cpr     =dosswap.ex_



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


woamgr.obj:     woamgr.asm  woaerr.inc woakeys.inc woagrab.inc \
		woasegs.inc woaswch.inc

woainit.obj:    woainit.asm woaswch.inc woasegs.inc woaerr.inc \
		woakeys.inc grabber.inc woakeys.inc $(MSG_DIR)\woahelp.inc

woafile.obj:    woafile.asm  woasegs.inc

woarlm.obj:     woarlm.asm  woakeys.inc woasegs.inc woaerr.inc \
		woaswch.inc  $(MSG_DIR)\woamsg2.inc woaswapi.inc

woaswapi.obj:   woaswapi.asm  woasegs.inc woaswapi.inc

woagrb.obj:     woagrb.asm woaerr.inc  woasegs.inc woagrab.inc \
		grabber.inc

woainstd.obj:   woainstd.asm woasegs.inc  woaswapi.inc

woados.obj:     woados.asm woasegs.inc woakeys.inc

woaerr.obj:     woaerr.asm woaerr.inc  woasegs.inc

woaswch.obj:    woaswch.asm woaerr.inc woakeys.inc woaswch.inc \
		woasegs.inc

woamouse.obj:   woamouse.asm  woasegs.inc woahpeqs.inc woahp.inc

woahp.obj:      woahp.asm woahp.inc woahpeqs.inc  woasegs.inc

woaxms.obj:     woaxms.asm  woasegs.inc

woatsr.obj:     woatsr.asm    woasegs.inc  $(MSG_DIR)\woamsg2.inc

woautils.obj:   woautils.asm  woaswch.inc woasegs.inc woaerr.inc \
		woaarena.inc

woapmrm.obj:    woapmrm.asm woasegs.inc woaerr.inc woaarena.inc

woastk.obj:     woastk.asm  woasegs.inc

woakkc.obj:     woakkc.asm woasegs.inc

#
# link'em together
#

$(dest):      woamgr.obj          \
	      woainit.obj         \
	      woafile.obj         \
	      woarlm.obj          \
	      woaswapi.obj        \
	      woakkc.obj          \
	      woagrb.obj          \
	      woainstd.obj        \
	      woados.obj          \
	      woaerr.obj          \
	      woaswch.obj         \
	      woamouse.obj        \
	      woahp.obj           \
	      woaxms.obj          \
	      woatsr.obj          \
	      woautils.obj        \
	      woapmrm.obj         \
	      woastk.obj          \
	      dosswap.lnk
	      link $(exelink) @dosswap.lnk
