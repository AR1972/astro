#--------------------------------------------------------------------
# File: $(QB5)\uq\makefile:
#
# This makefile contains make information for all cow user interface
# source, and should only be included if a cow ui is desired.
#
# The following macros are expected to be set up:
#   TL		path to tools\bin directory
#   QB		path to qb root
#   QB5 	path to qb5 root
#   PROD	if = "qbj" (case matters), build a kanji version
#   UIBLD	directory to build .obj files to
#   MKDIR	directory build is taking place on (ie: \qb5\test)
#   CC		cl command line switches
#   MASM	masm command line switches
#
# This file is used in conjunction with:
#   \qb5\test\makefile	root qb5 makefile (also \qb5\release\makefile)
#   \qb\ir\makefile	version independent interpreter source
#   ..\ir\makefile	QB5 specific interpreter source
#   ..\qb\makefile	QBI version dependent source
#--------------------------------------------------------------------

.SUFFIXES: .c .obj .asm .des .hs .sdm

#------------------------------------------------------------------
#   Define ROMBASIC to build IBM version of QBASIC
#
!IFDEF ROMBASIC
ROMBAS = /DROMBASIC
!ELSE
ROMBAS =
!ENDIF

#------------------------------------------------------------------
#   Macros for masm and cl command line switches specific to this
#	module, and include search paths.
#
CC_MOD = -Zp -Fo$(UIBLD)\ -NTUI
CC_INCL = -I$(DES) -I. -I$(QB5)\hdcw -I$(QB)\hd
MASM_MOD =
MASM_INCL = -E -I. -I$(QB5)\hdcw -I$(QB)\hd -I$(QB5)\hd

#------------------------------------------------------------------
#   UIOBJ are macros for the .obj's in the english ui (split up to
#	avoid writing lines which are too long for the lrf).
#
UI1 = $(UIBLD)\auxcow.obj $(UIBLD)\uimain.obj $(UIBLD)\uictl.obj
UI2 = $(UIBLD)\uiedit.obj $(UIBLD)\uiscrn.obj $(UIBLD)\uiwind.obj
UI3 = $(UIBLD)\uirsrcc.obj $(UIBLD)\uidebug.obj $(UIBLD)\uimsgbox.obj
UI4 = $(UIBLD)\uirsc.obj $(UIBLD)\uirs.obj $(UIBLD)\uifile.obj
UI5 = $(UIBLD)\uifileio.obj $(UIBLD)\uiview.obj $(UIBLD)\uioptns.obj
UI6 = $(UIBLD)\uimemory.obj $(UIBLD)\uihelp.obj
UI7 = $(UIBLD)\uisearch.obj $(UIBLD)\uiword.obj $(UIBLD)\scroll.obj
UI8 = $(UIBLD)\uiutil.obj $(UIBLD)\uimisc.obj	$(UIBLD)\uiinfhlp.obj
UI9 = $(UIBLD)\uinhelp.obj $(UIBLD)\uipaths.obj $(UIBLD)\text.obj
UI10 = $(UIBLD)\hengine.obj $(UIBLD)\defcolor.obj $(UIBLD)\textmgr.obj

#------------------------------------------------------------------
#   UIJOBJ is a macro for the .obj's in the kanji ui (split up to
#	avoid writing lines which are too long for the lrf).
#
UIJ1 = $(UIBLD)\auxcow.obj $(UIBLD)\uimain.obj $(UIBLD)\uictl.obj
UIJ2 = $(UIBLD)\uiedit.obj $(UIBLD)\uiscrn.obj $(UIBLD)\uiwind.obj
UIJ3 = $(UIBLD)\uirsrcc.obj $(UIBLD)\uidebug.obj $(UIBLD)\uimsgbox.obj
UIJ4 = $(UIBLD)\uirsc.obj $(UIBLD)\uirs.obj $(UIBLD)\uifile.obj
UIJ5 = $(UIBLD)\uifileio.obj $(UIBLD)\uiview.obj $(UIBLD)\uioptns.obj
UIJ6 = $(UIBLD)\uimemory.obj $(UIBLD)\uihelp.obj
UIJ7 = $(UIBLD)\uisearch.obj $(UIBLD)\uiword.obj $(UIBLD)\scroll.obj
UIJ8 = $(UIBLD)\uiutil.obj $(UIBLD)\uimisc.obj
UIJ9 = $(UIBLD)\defcolor.obj $(UIBLD)\uipaths.obj

#------------------------------------------------------------------
#   product specific macros
#
!if "$(PROD)" == "qbj"
DES = $(QB5)\uq\desj
UIOBJ = $(UIJ1) $(UIJ2) $(UIJ3) $(UIJ4) $(UIJ5) $(UIJ6) $(UIJ7) $(UIJ8) $(UIJ9)
!else
DES=$(QB5)\uq\des
UIOBJ = $(UI1) $(UI2) $(UI3) $(UI4) $(UI5) $(UI6) $(UI7) $(UI8) $(UI9) $(UI10)
!endif


#------------------------------------------------------------------
#   Default Rules:
#
#
{$(DES)}.des.hs:
	$(QB5)\tl\de -g $*.des
	copy *.hs $(DES)
	del *.hs
	copy *.sdm $(DES)
	del *.sdm

{$(DES)}.des.sdm:
	$(QB5)\tl\de -g $*.des
	copy *.hs $(DES)
	del *.hs
	copy *.sdm $(DES)
	del *.sdm


#------------------------------------------------------------------
#   English and Kanji COW User Interface Objects:
#
cowui: $(UIOBJ) $(UIBLD)\shell.lnk

!if "$(PROD)" != "qbj"
$(UIBLD)\shell.lnk: $(UIOBJ)
    echo $(UI1: \=+\)+> $(UIBLD)\shell.lnk
    echo $(UI2: \=+\)+>> $(UIBLD)\shell.lnk
    echo $(UI3: \=+\)+>> $(UIBLD)\shell.lnk
    echo $(UI4: \=+\)+>> $(UIBLD)\shell.lnk
    echo $(UI5: \=+\)+>> $(UIBLD)\shell.lnk
    echo $(UI6: \=+\)+>> $(UIBLD)\shell.lnk
    echo $(UI7: \=+\)+>> $(UIBLD)\shell.lnk
    echo $(UI8: \=+\)+>> $(UIBLD)\shell.lnk
    echo $(UI9: \=+\)+>> $(UIBLD)\shell.lnk
    echo $(UI10: \=+\)+>> $(UIBLD)\shell.lnk

!else
$(UIBLD)\shell.lnk: $(UIOBJ)
    echo $(UIJ1: \=+\)+> $(UIBLD)\shell.lnk
    echo $(UIJ2: \=+\)+>> $(UIBLD)\shell.lnk
    echo $(UIJ3: \=+\)+>> $(UIBLD)\shell.lnk
    echo $(UIJ4: \=+\)+>> $(UIBLD)\shell.lnk
    echo $(UIJ5: \=+\)+>> $(UIBLD)\shell.lnk
    echo $(UIJ6: \=+\)+>> $(UIBLD)\shell.lnk
    echo $(UIJ7: \=+\)+>> $(UIBLD)\shell.lnk
    echo $(UIJ8: \=+\)+>> $(UIBLD)\shell.lnk
    echo $(UIJ9: \=+\)+>> $(UIBLD)\shell.lnk
!endif

#------------------------------------------------------------------
#   C Object File Dependencies for User Interface:
#

$(UIBLD)\auxcow.obj: $(QB5)\uq\auxcow.c version.h qbimsgs.h		\
	    $(QB)\hd\switch.h						\
	    $(QB)\hd\interp.h						\
	    $(QB)\hd\debug.h						\
	    $(QB)\hd\heap.h						\
	    $(QB)\hd\ui.h						\
	    $(QB5)\hdcw\uiint.h 					\
	    $(QB5)\hdcw\uidec.h 					\
	    $(QB5)\hdcw\uiext.h 					\
	    $(QB5)\hdcw\uihelpid.h					\
	    $(QB5)\hdcw\uimenu.h					\
	    $(QB5)\hdcw\kkif.h						\
	    $(QB5)\hdcw\cw\version.h					\
	    $(QB5)\hdcw\cw\windows.h					\
	    $(QB5)\hdcw\cw\cwindows.h					\
	    $(QB5)\hdcw\cw\edityp.h					\
	    $(QB5)\hdcw\cw\dlg.h					\
	    $(QB5)\hdcw\cw\csdm.h					\
	    $(QB5)\hdcw\cw\csdmtmpl.h					\
	    $(QB5)\hdcw\cw\color.h
	    $(TL)\szscrn "Compiling  $(*B).c:"
	    $(TL)\cl $(CC) $(QB5)\uq\auxcow.c

$(UIBLD)\scroll.obj: $(QB5)\uq\scroll.c 				\
	    $(QB5)\hdcw\cw\cwindows.h
	    $(TL)\szscrn "Compiling  $(*B).c:"
	    $(TL)\cl $(CC) $(QB5)\uq\scroll.c

$(UIBLD)\uictl.obj: $(QB5)\uq\uictl.c version.h qbimsgs.h		\
	    $(QB)\hd\switch.h						\
	    $(QB)\hd\interp.h						\
	    $(QB)\hd\debug.h						\
	    $(QB)\hd\context.h						\
	    $(QB)\hd\heap.h						\
	    $(QB)\hd\txtmgr.h						\
	    $(QB)\hd\ui.h						\
	    $(QB5)\hdcw\uiint.h 					\
	    $(QB5)\hdcw\uidec.h 					\
	    $(QB5)\hdcw\uiext.h 					\
	    $(QB5)\hdcw\uihelpid.h					\
	    $(QB5)\hdcw\uimenu.h					\
	    $(QB5)\hdcw\uinhelp.h					\
	    $(QB5)\hdcw\cw\version.h					\
	    $(QB5)\hdcw\cw\windows.h					\
	    $(QB5)\hdcw\cw\cwindows.h					\
	    $(QB5)\hdcw\cw\edityp.h
	    $(TL)\szscrn "Compiling  $(*B).c:"
	    $(TL)\cl $(CC) $(QB5)\uq\uictl.c

$(UIBLD)\uifile.obj: $(QB5)\uq\uifile.c version.h qbimsgs.h		\
	    $(DES)\uifile.hs						\
	    $(DES)\uifile.sdm						\
	    $(QB)\hd\switch.h						\
	    $(QB)\hd\interp.h						\
	    $(QB)\hd\debug.h						\
	    $(QB)\hd\context.h						\
	    $(QB)\hd\heap.h						\
	    $(QB)\hd\names.h						\
	    $(QB)\hd\parser.h						\
	    $(QB)\hd\rtinterp.h 					\
	    $(QB)\hd\rtps.h						\
	    $(QB)\hd\txtmgr.h						\
	    $(QB)\hd\ui.h						\
	    $(QB)\hd\util.h						\
	    $(QB5)\hdcw\uiint.h 					\
	    $(QB5)\hdcw\uidec.h 					\
	    $(QB5)\hdcw\uihelpid.h					\
	    $(QB5)\hdcw\uimenu.h					\
	    $(QB5)\hdcw\uinhelp.h					\
	    $(QB5)\hdcw\uiext.h 					\
	    $(QB5)\hdcw\cw\version.h					\
	    $(QB5)\hdcw\cw\windows.h					\
	    $(QB5)\hdcw\cw\cwindows.h					\
	    $(QB5)\hdcw\cw\edityp.h					\
	    $(QB5)\hdcw\cw\dlg.h					\
	    $(QB5)\hdcw\cw\csdm.h					\
	    $(QB5)\hdcw\cw\csdmtmpl.h
	    $(TL)\szscrn "Compiling  $(*B).c:"
	    $(TL)\cl $(CC) $(QB5)\uq\uifile.c

$(UIBLD)\uihelp.obj: $(QB5)\uq\uihelp.c version.h qbimsgs.h prstab.h	\
	    $(DES)\uihelp.hs						\
	    $(DES)\uihelp.sdm						\
	    $(QB)\hd\switch.h						\
	    $(QB)\hd\interp.h						\
	    $(QB)\hd\debug.h						\
	    $(QB)\hd\heap.h						\
	    $(QB)\hd\parser.h						\
	    $(QB)\hd\ui.h						\
	    $(QB)\hd\rtps.h						\
	    $(QB5)\hdcw\uiext.h 					\
	    $(QB5)\hdcw\uiint.h 					\
	    $(QB5)\hdcw\uidec.h 					\
	    $(QB5)\hdcw\uihelpid.h					\
	    $(QB5)\hdcw\uimenu.h					\
	    $(QB5)\hdcw\uinhelp.h					\
	    $(QB5)\hdcw\cw\version.h					\
	    $(QB5)\hdcw\cw\windows.h					\
	    $(QB5)\hdcw\cw\cwindows.h					\
	    $(QB5)\hdcw\cw\edityp.h					\
	    $(QB5)\hdcw\cw\help.h					\
	    $(QB5)\hdcw\cw\dlg.h					\
	    $(QB5)\hdcw\cw\csdm.h					\
	    $(QB5)\hdcw\cw\csdmtmpl.h
	    $(TL)\szscrn "Compiling  $(*B).c:"
	    $(TL)\cl $(CC) $(QB5)\uq\uihelp.c

$(UIBLD)\uimsgbox.obj: $(QB5)\uq\uimsgbox.c version.h qbimsgs.h 	\
	    $(DES)\uimsgbox.hs						\
	    $(DES)\uimsgbox.sdm 					\
	    $(QB)\hd\switch.h						\
	    $(QB)\hd\interp.h						\
	    $(QB)\hd\debug.h						\
	    $(QB)\hd\ui.h						\
	    $(QB)\hd\heap.h						\
	    $(QB5)\hdcw\uiext.h 					\
	    $(QB5)\hdcw\uiint.h 					\
	    $(QB5)\hdcw\uidec.h 					\
	    $(QB5)\hdcw\uihelpid.h					\
	    $(QB5)\hdcw\uimenu.h					\
	    $(QB5)\hdcw\uinhelp.h					\
	    $(QB5)\hdcw\cw\version.h					\
	    $(QB5)\hdcw\cw\windows.h					\
	    $(QB5)\hdcw\cw\cwindows.h					\
	    $(QB5)\hdcw\cw\edityp.h					\
	    $(QB5)\hdcw\cw\dlg.h					\
	    $(QB5)\hdcw\cw\csdm.h					\
	    $(QB5)\hdcw\cw\csdmtmpl.h
	    $(TL)\szscrn "Compiling  $(*B).c:"
	    $(TL)\cl $(CC) $(QB5)\uq\uimsgbox.c

$(UIBLD)\uioptns.obj: $(QB5)\uq\uioptns.c version.h qbimsgs.h		\
	    $(DES)\uioptns.hs						\
	    $(DES)\uioptns.sdm						\
	    $(QB)\hd\switch.h						\
	    $(QB)\hd\interp.h						\
	    $(QB)\hd\debug.h						\
	    $(QB)\hd\context.h						\
	    $(QB)\hd\heap.h						\
	    $(QB)\hd\names.h						\
	    $(QB)\hd\parser.h						\
	    $(QB)\hd\rtps.h						\
	    $(QB)\hd\txtmgr.h						\
	    $(QB)\hd\ui.h						\
	    $(QB)\hd\util.h						\
	    $(QB5)\hdcw\uiint.h 					\
	    $(QB5)\hdcw\uidec.h 					\
	    $(QB5)\hdcw\uihelpid.h					\
	    $(QB5)\hdcw\uimenu.h					\
	    $(QB5)\hdcw\uiext.h 					\
	    $(QB5)\hdcw\cw\version.h					\
	    $(QB5)\hdcw\cw\windows.h					\
	    $(QB5)\hdcw\cw\cwindows.h					\
	    $(QB5)\hdcw\cw\edityp.h					\
	    $(QB5)\hdcw\cw\dlg.h					\
	    $(QB5)\hdcw\cw\csdm.h					\
	    $(QB5)\hdcw\cw\csdmtmpl.h
	    $(TL)\szscrn "Compiling  $(*B).c:"
	    $(TL)\cl $(CC) $(QB5)\uq\uioptns.c

$(UIBLD)\uirsc.obj: $(QB5)\uq\uirsc.c version.h qbimsgs.h		\
	    $(QB)\hd\switch.h						\
	    $(QB)\hd\interp.h						\
	    $(QB)\hd\debug.h						\
	    $(QB)\hd\context.h						\
	    $(QB)\hd\heap.h						\
	    $(QB)\hd\lister.h						\
	    $(QB)\hd\names.h						\
	    $(QB)\hd\parser.h						\
	    $(QB)\hd\txtmgr.h						\
	    $(QB)\hd\ui.h						\
	    $(QB)\hd\util.h						\
	    $(QB5)\hdcw\cw\version.h					\
	    $(QB5)\hdcw\cw\windows.h					\
	    $(QB5)\hdcw\cw\cwindows.h					\
	    $(QB5)\hdcw\cw\edityp.h					\
	    $(QB5)\hdcw\uiint.h 					\
	    $(QB5)\hdcw\uidec.h 					\
	    $(QB5)\hdcw\uimenu.h
	    $(TL)\szscrn "Compiling  $(*B).c:"
	    $(TL)\cl $(CC) $(QB5)\uq\uirsc.c

$(UIBLD)\uirsrcc.obj: $(QB5)\uq\uirsrcc.c version.h qbimsgs.h		\
	    $(QB)\hd\switch.h						\
	    $(QB)\hd\interp.h						\
	    $(QB)\hd\debug.h						\
	    $(QB)\hd\ui.h						\
	    $(QB)\hd\heap.h						\
	    $(QB5)\hdcw\uiint.h 					\
	    $(QB5)\hdcw\uidec.h 					\
	    $(QB5)\hdcw\uimenu.h					\
	    $(QB5)\hdcw\cw\version.h					\
	    $(QB5)\hdcw\cw\windows.h					\
	    $(QB5)\hdcw\cw\cwindows.h					\
	    $(QB5)\hdcw\cw\edityp.h
	    $(TL)\szscrn "Compiling  $(*B).c:"
	    $(TL)\cl $(CC) $(QB5)\uq\uirsrcc.c

$(UIBLD)\uiscrn.obj: $(QB5)\uq\uiscrn.c version.h qbimsgs.h		\
	    $(QB)\hd\switch.h						\
	    $(QB)\hd\interp.h						\
	    $(QB)\hd\debug.h						\
	    $(QB)\hd\ui.h						\
	    $(QB)\hd\heap.h						\
	    $(QB5)\hdcw\uiint.h 					\
	    $(QB5)\hdcw\uidec.h 					\
	    $(QB5)\hdcw\uimenu.h					\
	    $(QB5)\hdcw\cw\version.h					\
	    $(QB5)\hdcw\cw\windows.h					\
	    $(QB5)\hdcw\cw\cwindows.h					\
	    $(QB5)\hdcw\cw\edityp.h
	    $(TL)\szscrn "Compiling  $(*B).c:"
	    $(TL)\cl $(CC) $(QB5)\uq\uiscrn.c

$(UIBLD)\uisearch.obj: $(QB5)\uq\uisearch.c version.h qbimsgs.h 	\
	    $(DES)\uisearch.hs						\
	    $(DES)\uisearch.sdm 					\
	    $(QB)\hd\switch.h						\
	    $(QB)\hd\interp.h						\
	    $(QB)\hd\debug.h						\
	    $(QB)\hd\context.h						\
	    $(QB)\hd\heap.h						\
	    $(QB)\hd\ui.h						\
	    $(QB)\hd\util.h						\
	    $(QB5)\hdcw\uiext.h 					\
	    $(QB5)\hdcw\uiint.h 					\
	    $(QB5)\hdcw\uidec.h 					\
	    $(QB5)\hdcw\uihelpid.h					\
	    $(QB5)\hdcw\uimenu.h					\
	    $(QB5)\hdcw\uinhelp.h					\
	    $(QB5)\hdcw\cw\version.h					\
	    $(QB5)\hdcw\cw\windows.h					\
	    $(QB5)\hdcw\cw\cwindows.h					\
	    $(QB5)\hdcw\cw\edityp.h					\
	    $(QB5)\hdcw\cw\dlg.h					\
	    $(QB5)\hdcw\cw\csdm.h					\
	    $(QB5)\hdcw\cw\csdmtmpl.h
	    $(TL)\szscrn "Compiling  $(*B).c:"
	    $(TL)\cl $(CC) $(QB5)\uq\uisearch.c

$(UIBLD)\uiview.obj: $(QB5)\uq\uiview.c version.h qbimsgs.h		\
	    $(DES)\uiview.hs						\
	    $(DES)\uiview.sdm						\
	    $(QB)\hd\switch.h						\
	    $(QB)\hd\interp.h						\
	    $(QB)\hd\debug.h						\
	    $(QB)\hd\context.h						\
	    $(QB)\hd\heap.h						\
	    $(QB)\hd\names.h						\
	    $(QB)\hd\parser.h						\
	    $(QB)\hd\rtps.h						\
	    $(QB)\hd\txtmgr.h						\
	    $(QB)\hd\ui.h						\
	    $(QB)\hd\util.h						\
	    $(QB5)\hdcw\uiext.h 					\
	    $(QB5)\hdcw\uiint.h 					\
	    $(QB5)\hdcw\uidec.h 					\
	    $(QB5)\hdcw\uihelpid.h					\
	    $(QB5)\hdcw\uimenu.h					\
	    $(QB5)\hdcw\cw\version.h					\
	    $(QB5)\hdcw\cw\windows.h					\
	    $(QB5)\hdcw\cw\cwindows.h					\
	    $(QB5)\hdcw\cw\edityp.h					\
	    $(QB5)\hdcw\cw\dlg.h					\
	    $(QB5)\hdcw\cw\csdm.h					\
	    $(QB5)\hdcw\cw\csdmtmpl.h
	    $(TL)\szscrn "Compiling  $(*B).c:"
	    $(TL)\cl $(CC) $(QB5)\uq\uiview.c

$(UIBLD)\uiviewj.obj: $(QB5)\uq\uiviewj.c version.h qbimsgs.h		\
	    $(DES)\uiview.hs						\
	    $(DES)\uiview.sdm						\
	    $(QB)\hd\switch.h						\
	    $(QB)\hd\interp.h						\
	    $(QB)\hd\debug.h						\
	    $(QB)\hd\context.h						\
	    $(QB)\hd\heap.h						\
	    $(QB)\hd\names.h						\
	    $(QB)\hd\parser.h						\
	    $(QB)\hd\rtps.h						\
	    $(QB)\hd\txtmgr.h						\
	    $(QB)\hd\ui.h						\
	    $(QB)\hd\util.h						\
	    $(QB5)\hdcw\cw\version.h					\
	    $(QB5)\hdcw\cw\windows.h					\
	    $(QB5)\hdcw\cw\cwindows.h					\
	    $(QB5)\hdcw\cw\edityp.h					\
	    $(QB5)\hdcw\uiext.h 					\
	    $(QB5)\hdcw\cw\dlg.h					\
	    $(QB5)\hdcw\cw\csdm.h					\
	    $(QB5)\hdcw\cw\csdmtmpl.h					\
	    $(QB5)\hdcw\uiint.h 					\
	    $(QB5)\hdcw\uidec.h 					\
	    $(QB5)\hdcw\uihelpid.h					\
	    $(QB5)\hdcw\uimenu.h
	    $(TL)\szscrn "Compiling  $(*B).c:"
	    $(TL)\cl $(CC) $(QB5)\uq\uiviewj.c

$(UIBLD)\uiwind.obj: $(QB5)\uq\uiwind.c version.h qbimsgs.h		\
	    $(QB)\hd\switch.h						\
	    $(QB)\hd\interp.h						\
	    $(QB)\hd\debug.h						\
	    $(QB)\hd\context.h						\
	    $(QB)\hd\heap.h						\
	    $(QB)\hd\parser.h						\
	    $(QB)\hd\txtmgr.h						\
	    $(QB)\hd\ui.h						\
	    $(QB5)\hdcw\uiint.h 					\
	    $(QB5)\hdcw\uidec.h 					\
	    $(QB5)\hdcw\uimenu.h					\
	    $(QB5)\hdcw\uinhelp.h					\
	    $(QB5)\hdcw\uiext.h 					\
	    $(QB5)\hdcw\cw\version.h					\
	    $(QB5)\hdcw\cw\windows.h					\
	    $(QB5)\hdcw\cw\cwindows.h					\
	    $(QB5)\hdcw\cw\edityp.h
	    $(TL)\szscrn "Compiling  $(*B).c:"
	    $(TL)\cl $(CC) $(QB5)\uq\uiwind.c

$(UIBLD)\defcolor.obj: $(QB5)\uq\defcolor.c version.h			\
	    $(QB5)\hdcw\cw\color.h					\
	    $(QB5)\hdcw\uiext.h 					\
	    $(QB5)\hdcw\cw\version.h					\
	    $(QB5)\hdcw\cw\windows.h					\
	    $(QB5)\hdcw\cw\cwindows.h					\
	    $(QB5)\hdcw\cw\edityp.h					\
	    $(QB)\hd\ui.h
	    $(TL)\szscrn "Compiling  $(*B).c:"
	    $(TL)\cl $(CC) $(QB5)\uq\defcolor.c

$(UIBLD)\textmgr.obj: $(QB5)\uq\textmgr.c version.h			\
	    qbimsgs.h							\
	    $(QB5)\hd\edit.h						\
	    $(QB)\hd\heap.h
	    $(TL)\szscrn "Compiling  $(*B).c:"
	    $(TL)\cl $(CC) $(QB5)\uq\textmgr.c



#------------------------------------------------------------------
#   MASM Object File Dependencies for User Interface:
#
$(UIBLD)\uidebug.obj: $(QB5)\uq\uidebug.asm version.inc opmin.inc	\
	    opstmt.inc							\
	    qbimsgs.inc 						\
	    $(QB)\hd\switch.inc 					\
	    $(QB)\hd\architec.inc					\
	    $(QB)\hd\executor.inc					\
	    $(QB)\hd\filename.inc					\
	    $(QB)\hd\interp.inc 					\
	    $(QB)\hd\cmacros.inc					\
	    $(QB)\hd\debug.inc						\
	    $(QB)\hd\idmac.inc						\
	    $(QB)\hd\heap.inc						\
	    $(QB)\hd\parser.inc 					\
	    $(QB)\hd\scanner.inc					\
	    $(QB)\hd\txtmgr.inc 					\
	    $(QB)\hd\lister.inc 					\
	    $(QB)\hd\ui.inc						\
	    $(QB)\hd\util.inc						\
	    $(QB5)\hdcw\uiint.inc					\
	    $(QB5)\hdcw\uimenu.inc					\
	    $(QB5)\hdcw\cw\version.inc					\
	    $(QB5)\hdcw\cw\windows.inc					\
	    $(QB5)\hdcw\cw\cwindows.inc 				\
	    $(QB5)\hdcw\cw\cowdef.inc					\
	    $(QB5)\hdcw\cw\cowproc.inc					\
	    $(QB5)\hdcw\cw\edityp.inc
	    $(TL)\szscrn "Assembling $(*B).asm"
	    $(TL)\masm $(MASM) $(QB5)\uq\uidebug,$(@R);

$(UIBLD)\uiedit.obj: $(QB5)\uq\uiedit.asm version.inc qbimsgs.inc	\
	    $(QB)\hd\switch.inc 					\
	    $(QB)\hd\filename.inc					\
	    $(QB)\hd\interp.inc 					\
	    $(QB)\hd\cmacros.inc					\
	    $(QB)\hd\debug.inc						\
	    $(QB)\hd\idmac.inc						\
	    $(QB)\hd\parser.inc 					\
	    $(QB)\hd\architec.inc					\
	    $(QB)\hd\context.inc					\
	    $(QB)\hd\lister.inc 					\
	    $(QB)\hd\heap.inc						\
	    $(QB)\hd\pcode.inc						\
	    $(QB)\hd\rtps.inc						\
	    $(QB)\hd\txtmgr.inc 					\
	    $(QB)\hd\ui.inc						\
	    $(QB)\hd\util.inc						\
	    $(QB5)\hd\edit.inc						\
	    $(QB5)\hdcw\uinhelp.inc					\
	    $(QB5)\hdcw\uiint.inc					\
	    $(QB5)\hdcw\cw\version.inc					\
	    $(QB5)\hdcw\cw\windows.inc					\
	    $(QB5)\hdcw\cw\cwindows.inc 				\
	    $(QB5)\hdcw\cw\cowdef.inc					\
	    $(QB5)\hdcw\cw\cowproc.inc					\
	    $(QB5)\hdcw\cw\edityp.inc
	    $(TL)\szscrn "Assembling $(*B).asm"
	    $(TL)\masm $(MASM) $(QB5)\uq\uiedit,$(@R);

$(UIBLD)\uifileio.obj: $(QB5)\uq\uifileio.asm version.inc qbimsgs.inc	\
	    $(QB)\hd\switch.inc 					\
	    $(QB)\hd\filename.inc					\
	    $(QB)\hd\interp.inc 					\
	    $(QB)\hd\cmacros.inc					\
	    $(QB)\hd\debug.inc						\
	    $(QB)\hd\idmac.inc						\
	    $(QB)\hd\rtps.inc						\
	    $(QB5)\hdcw\uiint.inc					\
	    $(QB5)\hdcw\cw\version.inc					\
	    $(QB5)\hdcw\cw\windows.inc					\
	    $(QB5)\hdcw\cw\cwindows.inc 				\
	    $(QB5)\hdcw\cw\cowdef.inc					\
	    $(QB5)\hdcw\cw\cowproc.inc					\
	    $(QB5)\hdcw\cw\edityp.inc
	    $(TL)\szscrn "Assembling $(*B).asm"
	    $(TL)\masm $(MASM) $(QB5)\uq\uifileio,$(@R);

$(UIBLD)\uimain.obj: $(QB5)\uq\uimain.asm version.inc qbimsgs.inc	\
	    $(QB)\hd\switch.inc 					\
	    $(QB)\hd\filename.inc					\
	    $(QB)\hd\interp.inc 					\
	    $(QB)\hd\cmacros.inc					\
	    $(QB)\hd\debug.inc						\
	    $(QB)\hd\idmac.inc						\
	    $(QB)\hd\parser.inc 					\
	    $(QB)\hd\architec.inc					\
	    $(QB)\hd\context.inc					\
	    $(QB)\hd\heap.inc						\
	    $(QB)\hd\rtinterp.inc					\
	    $(QB)\hd\txtmgr.inc 					\
	    $(QB)\hd\ui.inc						\
	    $(QB)\hd\executor.inc					\
	    $(QB5)\hdcw\uiint.inc					\
	    $(QB5)\hdcw\cw\version.inc					\
	    $(QB5)\hdcw\cw\windows.inc					\
	    $(QB5)\hdcw\cw\cwindows.inc 				\
	    $(QB5)\hdcw\cw\cowdef.inc					\
	    $(QB5)\hdcw\cw\cowproc.inc					\
	    $(QB5)\hdcw\cw\edityp.inc
	    $(TL)\szscrn "Assembling $(*B).asm"
	    $(TL)\masm $(MASM) $(ROMBAS) $(QB5)\uq\uimain,$(@R);

$(UIBLD)\uimemory.obj: $(QB5)\uq\uimemory.asm version.inc qbimsgs.inc	\
	    $(QB)\hd\switch.inc 					\
	    $(QB)\hd\filename.inc					\
	    $(QB)\hd\interp.inc 					\
	    $(QB)\hd\cmacros.inc					\
	    $(QB)\hd\debug.inc						\
	    $(QB)\hd\idmac.inc						\
	    $(QB)\hd\ui.inc						\
	    $(QB)\hd\util.inc						\
	    $(QB)\hd\heap.inc						\
	    $(QB5)\hdcw\uiint.inc					\
	    $(QB5)\hdcw\cw\version.inc
	    $(TL)\szscrn "Assembling $(*B).asm"
	    $(TL)\masm $(MASM) $(QB5)\uq\uimemory,$(@R);

$(UIBLD)\uimisc.obj: $(QB5)\uq\uimisc.asm version.inc qbimsgs.inc	\
	    $(QB)\hd\switch.inc 					\
	    $(QB)\hd\filename.inc					\
	    $(QB)\hd\interp.inc 					\
	    $(QB)\hd\cmacros.inc					\
	    $(QB)\hd\debug.inc						\
	    $(QB)\hd\idmac.inc						\
	    $(QB)\hd\architec.inc					\
	    $(QB)\hd\context.inc					\
	    $(QB)\hd\ui.inc						\
	    $(QB5)\hdcw\uiint.inc					\
	    $(QB5)\hdcw\cw\version.inc					\
	    $(QB5)\hdcw\cw\windows.inc					\
	    $(QB5)\hdcw\cw\cwindows.inc 				\
	    $(QB5)\hdcw\cw\cowdef.inc					\
	    $(QB5)\hdcw\cw\cowproc.inc
	    $(TL)\szscrn "Assembling $(*B).asm"
	    $(TL)\masm $(MASM) $(QB5)\uq\uimisc,$(@R);


$(UIBLD)\uiinfhlp.obj: $(QB5)\uq\uiinfhlp.asm version.inc qbimsgs.inc	\
	    $(QB)\hd\switch.inc 					\
	    $(QB)\hd\interp.inc 					\
	    $(QB)\hd\cmacros.inc					\
	    $(QB)\hd\debug.inc						\
	    $(QB)\hd\idmac.inc						\
	    $(QB)\hd\ui.inc						\
	    $(QB)\hd\architec.inc					\
	    $(QB5)\hdcw\help.inc					\
	    $(QB5)\hdcw\uiint.inc					\
	    $(QB5)\hdcw\uinhelp.inc					\
	    $(QB5)\hdcw\cw\version.inc					\
	    $(QB5)\hdcw\cw\windows.inc					\
	    $(QB5)\hdcw\cw\cwindows.inc 				\
	    $(QB5)\hdcw\cw\cowdef.inc					\
	    $(QB5)\hdcw\cw\cowproc.inc					\
	    $(QB5)\hdcw\cw\edityp.inc
	    $(TL)\szscrn "Assembling $(*B).asm"
	    $(TL)\masm $(MASM) $(QB5)\uq\uiinfhlp,$(@R);


$(UIBLD)\uinhelp.obj: $(QB5)\uq\uinhelp.asm version.inc qbimsgs.inc	\
	    prstab.inc							\
	    $(QB)\hd\switch.inc 					\
	    $(QB)\hd\interp.inc 					\
	    $(QB)\hd\cmacros.inc					\
	    $(QB)\hd\debug.inc						\
	    $(QB)\hd\idmac.inc						\
	    $(QB)\hd\heap.inc						\
	    $(QB)\hd\parser.inc						\
	    $(QB)\hd\rtps.inc						\
	    $(QB)\hd\ui.inc						\
	    $(QB)\hd\architec.inc					\
	    $(QB5)\hdcw\help.inc					\
	    $(QB5)\hdcw\uiint.inc					\
	    $(QB5)\hdcw\uimenu.inc					\
	    $(QB5)\hdcw\uinhelp.inc					\
	    $(QB5)\hdcw\cw\version.inc					\
	    $(QB5)\hdcw\cw\windows.inc					\
	    $(QB5)\hdcw\cw\cwindows.inc 				\
	    $(QB5)\hdcw\cw\cowdef.inc					\
	    $(QB5)\hdcw\cw\cowproc.inc					\
	    $(QB5)\hdcw\cw\edityp.inc
	    $(TL)\szscrn "Assembling $(*B).asm"
	    $(TL)\masm $(MASM) $(QB5)\uq\uinhelp,$(@R);

$(UIBLD)\hengine.obj: $(QB5)\uq\hengine.asm version.inc qbimsgs.inc	\
	    prstab.inc							\
	    $(QB)\hd\switch.inc 					\
	    $(QB)\hd\interp.inc 					\
	    $(QB)\hd\cmacros.inc					\
	    $(QB)\hd\debug.inc						\
	    $(QB)\hd\idmac.inc						\
	    $(QB)\hd\heap.inc						\
	    $(QB)\hd\pointers.inc					\
	    $(QB)\hd\array.inc						\
	    $(QB5)\hdcw\uiint.inc
	    $(TL)\szscrn "Assembling $(*B).asm"
	    $(TL)\masm $(MASM) $(QB5)\uq\hengine,$(@R);

$(UIBLD)\uipaths.obj: $(QB5)\uq\uipaths.asm version.inc qbimsgs.inc	\
	    $(QB)\hd\switch.inc 					\
	    $(QB)\hd\interp.inc 					\
	    $(QB)\hd\cmacros.inc					\
	    $(QB)\hd\debug.inc						\
	    $(QB)\hd\idmac.inc						\
	    $(QB)\hd\heap.inc						\
	    $(QB)\hd\rtps.inc						\
	    $(QB)\hd\ui.inc						\
	    $(QB)\hd\architec.inc					\
	    $(QB5)\hdcw\uiint.inc					\
	    $(QB5)\hdcw\cw\version.inc					\
	    $(QB5)\hdcw\cw\windows.inc					\
	    $(QB5)\hdcw\cw\cwindows.inc 				\
	    $(QB5)\hdcw\cw\cowdef.inc					\
	    $(QB5)\hdcw\cw\cowproc.inc					\
	    $(QB5)\hdcw\cw\dlg.inc					\
	    $(QB5)\hdcw\cw\edityp.inc
	    $(TL)\szscrn "Assembling $(*B).asm"
	    $(TL)\masm $(MASM) $(QB5)\uq\uipaths,$(@R);

$(UIBLD)\uirs.obj: $(QB5)\uq\uirs.asm version.inc qbimsgs.inc		\
	    $(QB)\hd\switch.inc 					\
	    $(QB)\hd\filename.inc					\
	    $(QB)\hd\interp.inc 					\
	    $(QB)\hd\cmacros.inc					\
	    $(QB)\hd\debug.inc						\
	    $(QB)\hd\idmac.inc						\
	    $(QB)\hd\context.inc					\
	    $(QB)\hd\heap.inc						\
	    $(QB)\hd\names.inc						\
	    $(QB)\hd\txtmgr.inc 					\
	    $(QB)\hd\ui.inc						\
	    $(QB)\hd\architec.inc					\
	    $(QB5)\hdcw\uiint.inc
	    $(TL)\szscrn "Assembling $(*B).asm"
	    $(TL)\masm $(MASM) $(QB5)\uq\uirs,$(@R);

$(UIBLD)\uiword.obj: $(QB5)\uq\uiword.asm version.inc qbimsgs.inc	\
	    $(QB)\hd\switch.inc 					\
	    $(QB)\hd\filename.inc					\
	    $(QB)\hd\interp.inc 					\
	    $(QB)\hd\cmacros.inc					\
	    $(QB)\hd\debug.inc						\
	    $(QB)\hd\idmac.inc						\
	    $(QB5)\hdcw\uiint.inc					\
	    $(QB5)\hdcw\cw\version.inc
	    $(TL)\szscrn "Assembling $(*B).asm"
	    $(TL)\masm $(MASM) $(QB5)\uq\uiword,$(@R);

$(UIBLD)\uiutil.obj: $(QB5)\uq\uiutil.asm version.inc qbimsgs.inc	\
	    $(QB)\hd\switch.inc 					\
	    $(QB)\hd\filename.inc					\
	    $(QB)\hd\interp.inc 					\
	    $(QB)\hd\cmacros.inc					\
	    $(QB)\hd\debug.inc						\
	    $(QB)\hd\idmac.inc						\
	    $(QB5)\hdcw\cw\version.inc					\
	    $(QB5)\hdcw\cw\windows.inc					\
	    $(QB5)\hdcw\cw\cwindows.inc 				\
	    $(QB5)\hdcw\cw\cowdef.inc					\
	    $(QB5)\hdcw\cw\cowproc.inc					\
	    $(QB5)\hdcw\cw\edityp.inc
	    $(TL)\szscrn "Assembling $(*B).asm"
	    $(TL)\masm $(MASM) $(QB5)\uq\uiutil,$(@R);

$(UIBLD)\text.obj: $(QB5)\uq\text.asm version.inc			\
	    $(QB)\hd\switch.inc 					\
	    $(QB)\hd\filename.inc					\
	    $(QB)\hd\interp.inc 					\
	    $(QB)\hd\cmacros.inc					\
	    $(QB)\hd\debug.inc						\
	    $(QB)\hd\idmac.inc
	    $(TL)\szscrn "Assembling $(*B).asm"
	    $(TL)\masm $(MASM) $(QB5)\uq\text,$(@R);
