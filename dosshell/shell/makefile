#       Makefile for new DOS Shell
#       Set H = the directory containing header files
#       Define DEBUG if you want debugging code included
#       Define NOLOADER if you don't want the resident loader included.
#       You probably want to do this if you're debugging, so you can use
#       CodeView.
#       NOTE - setting DEBUG and NOLOADER to FALSE will not work as expected,
#       since the conditionals check only for definition, not TRUE or FALSE.


########################### International Changes #######################

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\dosshell

#########################################################################


H = ..\inc
INCL = ..\..\inc
INCL2 = ..\loader


# NDEBUG if defined causes 'assert()'s in source code to generate no code!!
# ASSERTSON = TRUE

# DEBUG = TRUE
# NOLOADER = TRUE
# DBGBEEP = TRUE
# CVW = TRUE
# ROMDOS = TRUE

#   Name is the name of the linker's output .exe file:
NAME = dosshell

#   Compiler options:
#   Always:
#        -c   compiler should not link
#        -DCC define CC(cc compiler) for the CW header files
#        -DNOMINMAX so cwindows header file won't make another min/max macro
#        -W3  warning level 3
#        -AM  medium model code..near data, far code
#        -Zp  pack structures on byte boundaries
#        -Gc  use Pascal calling conventions
#        -Ishellh default include file search directory
#   Debugging:
#        -DDEBUG define DEBUG for all code
#        -Zi codeview debugger information
#
#   Program Loader not included:
#       -DNOLOADER don't use loader code. Allows us to use CodeView.
#
#   Non-Debugging:
#        -Ox  total optimization
#             includes:
#             -Oa ingnore aliasing
#             -Oi intrinsic functions
#             -Ol loop optimizations
#             -Ot optimize for speed
#             -Gs disable stack checking
#
#  BEGIN IPG - Added -D$(COUNTRY)
!IFDEF ASSERTSON
CCOPTS2 = -D$(COUNTRY)
!ELSE
CCOPTS2 = -DNDEBUG -DNORESIZE -D$(COUNTRY)
!ENDIF
#  END IPG  - Added -D$(COUNTRY)

!IFDEF ROMDOS
ROMDOSOPTS = -DROMDOS
!ELSE
ROMDOSOPTS =
!ENDIF

!IFDEF OLDCW
!   IFDEF DBGBEEP
CCOPTS = -c -W3 -AM -Gc -DCC -DNOMINMAX -DBGBEEP -I$(H) 
!   ELSE
CCOPTS = -c -W3 -AM -Gc -DCC -DNOMINMAX -I$(H) 
!   ENDIF
!ELSE
!   IFDEF DBGBEEP
CCOPTS = -c -W3 -AM -Gc -DCC -DNOMINMAX -DSMM -DDBGBEEP -I$(H) 
!   ELSE
CCOPTS = -c -W3 -AM -Gc -DCC -DNOMINMAX -DSMM -I$(H)  
!   ENDIF
!ENDIF

!IFDEF DEBUG
!  IFDEF NOLOADER
CC = cl $(CCOPTS) $(CCOPTS2) $(ROMDOSOPTS) -DDEBUG -Zip -DNOLOADER
!  ELSE
CC = cl $(CCOPTS) $(CCOPTS2) $(ROMDOSOPTS) -DDEBUG -Zip
!  ENDIF
!ELSE
!  IFDEF NOLOADER
CC = cl $(CCOPTS) $(CCOPTS2) $(ROMDOSOPTS) -Zip -DNOLOADER -DNOCONSISTENCY
!  ELSE
CC      = cl $(CCOPTS) $(CCOPTS2) $(ROMDOSOPTS) -Zp -Oails -Gs -DNOCONSISTENCY
!  ENDIF
!ENDIF

!IFDEF CVW
CC = cl $(CCOPTS) $(CCOPTS2) $(ROMDOSOPTS) -Zip
!ENDIF

#   Assembler options:
#       always:
#       -MX preserve case of globals
#       -D?MEDIUM medium model
#       Debugging:
#       -DDEBUG define debug for all asm code
#

!IFDEF DEBUG
ASM = masm -MX -DmemM=1 -D?MEDIUM -DDEBUG $(ROMDOSOPTS) -I$(INCL) -I$(INCL2) 
!ELSE
ASM = masm -MX -DmemM=1 -D?MEDIUM $(ROMDOSOPTS) -I$(INCL) -I$(INCL2) 
!ENDIF

!IFDEF CVW
ASM = masm -MX -DmemM=1 -D?MEDIUM -I$(INCL) -I$(INCL2) 
!ENDIF

#   Linker options:
#       always:
#       /NOE no external dictionary for library search
#       /MAP generate a map file
#       Debugging:
#       /CO  keep codeview debugger information

STACKSIZE=4096

!IFDEF DEBUG
LINK= link /NOEXT /MAP /LINE /CODEVIEW /STACK:$(STACKSIZE)
!ELSE
LINK= link /NOEXT /MAP /FARCALL /PACKCODE /EXEPACK /STACK:$(STACKSIZE)
!ENDIF

!IFDEF CVW
LINK= link /NOEXT /MAP /LINE /CODEVIEW /STACK:$(STACKSIZE)
!ENDIF


!IFDEF NOLOADER
OBJSLD = 
!ELSE
OBJSLD = ..\loader\loader.obj
!ENDIF

!IFDEF DEBUG
OBJSDB = debug.obj
!ELSE
OBJSDB =
!ENDIF

!IFDEF CVW
OBJSDB = debug.obj
!ENDIF

#   OBJS and OBJS2 and OBJS3 contain a list of all the non-loader obj files.
#   OBJSLD contains the loader-specific code; set it to an empty string for
#   non-loader code.
#
OBJS1 =      add.obj asmutils.obj     attr.obj  busydlg.obj   cmdlan.obj 
OBJS2 =    color.obj  compare.obj  confirm.obj     copy.obj critical.obj 
OBJS3 =  delprog.obj      dir.obj     disp.obj  dosintf.obj  dotrees.obj
OBJS4 =    drive.obj   errbox.obj  filemgr.obj  fileops.obj flatleft.obj 
OBJS5 = graphics.obj   groups.obj   helper.obj     help.obj    icons.obj
OBJS6 =     init.obj   launch.obj     lbox.obj     list.obj  listbox.obj
OBJS7 =   locate.obj     main.obj    manip.obj   memmgr.obj    menus.obj
OBJS8 = mousebug.obj   pfiles.obj scancode.obj   screen.obj showinfo.obj
OBJS9 =     sort.obj    snext.obj swapdisk.obj  taskman.obj     text.obj 
OBJS10=    utils.obj     view.obj  winidle.obj  dts_api.obj
OBJS11= ..\iniparse\iniparse.obj ..\iniparse\lex.obj ..\iniparse\symbols.obj

!IF "$(COUNTRY)" == "JAPAN" || "$(COUNTRY)" == "japan"

!IFDEF DEBUG
LIBRARY = ..\lib\kow3m.lib
!ELSE
LIBRARY = ..\lib\kow3m.lib ..\lib\mlibce.lib
!ENDIF

!ELSE

!IFDEF DEBUG
LIBRARY = ..\lib\cow3m.lib
!ELSE
LIBRARY = ..\lib\cow3m.lib ..\lib\mlibce.lib
!ENDIF

!ENDIF

#
#   Compile all C files in makefile's directory:
.c.obj:
	$(CC) $*.c

#
#   Assemble all ASM files in makefile's directory, generate listing:
.asm.obj:
	$(ASM) $*.asm,$*.obj,$*.lst ;


#
#   Make will have succeeded if make finishes with this file existing:

bins    =..\..\binaries
cprs    =..\..\compress
dest    =dosshell.exe
cpr     =dosshell.ex_

!IFNDEF       BUILDER

all: $(dest) ..\$(dest)

..\$(dest): $(dest)
	    copy $(dest) ..



!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(cpr)

$(bins)\$(dest): $(dest)
	    copy $(dest) $(bins)

$(cprs)\$(cpr):  $(cpr)
	    copy $(cpr) $(cprs)

$(cpr):       $(dest)
	    $(compress) $(dest) $(cpr)

!ENDIF


*.obj:          $(H)\prot.h

#### decided to copy because $(ASM) here differs from $(ASM) in ..\loader
..\loader\loader.obj: 
		 copy $(MSG_DIR)\loader\*.inc ..\loader
		 $(ASM) $*.asm,$*.obj,$*.lst ;
		 del ..\loader\resmsg.inc
		 del ..\loader\transmsg.inc

#                cd ..\loader
#                $(MAKE) loader.obj /f makefile.itl
#                cd ..\shell

#### have to copy it in, otherwise cl fails - too long argument line
text.obj:       
		copy $(MSG_DIR)\shell\text.c
		$(CC) text.c
		del text.c
####

busydlg.obj:    $(H)\text.h

groups.obj:     $(H)\addgroup.hs $(H)\addgroup.sdm \
		$(H)\adddialo.hs $(H)\adddialo.sdm \
		$(H)\advanced.hs $(H)\advanced.sdm \
		$(H)\password.hs $(H)\password.sdm \
		$(H)\new.hs $(H)\new.sdm

utils.obj:      $(H)\text.h \
		$(H)\about.hs $(H)\about.sdm \
		$(H)\message.hs $(H)\message.sdm \
		$(H)\mswarn.hs $(H)\mswarn.sdm

add.obj:        $(H)\common.h $(H)\filemgr.h

sort.obj:       $(H)\common.h $(H)\filemgr.h

snext.obj:      $(H)\common.h $(H)\filemgr.h

flatleft.obj:      $(H)\common.h $(H)\filemgr.h \
		$(H)\dispopt.hs $(H)\dispopt.sdm \
		$(H)\help.h

manip.obj:         $(H)\common.h $(H)\filemgr.h

taskman.obj:    $(H)\common.h $(H)\filemgr.h \
		$(H)\prot.h \
		$(H)\warn.hs $(H)\warn.sdm

confirm.obj: $(H)\common.h $(H)\filemgr.h $(H)\text.h \
	     $(H)\dconfirm.hs $(H)\dconfirm.sdm \
	     $(H)\mconfirm.hs $(H)\mconfirm.sdm \
	     $(H)\rconfirm.hs $(H)\rconfirm.sdm 

dir.obj:           $(H)\common.h $(H)\filemgr.h \
	     $(H)\creatdir.hs $(H)\creatdir.sdm \
		$(H)\help.h

color.obj:      $(H)\common.h $(H)\color.hs $(H)\color.sdm

init.obj:       $(H)\common.h $(H)\text.h $(H)\screen.hs $(H)\screen.sdm

copy.obj:       $(H)\common.h $(H)\filemgr.h \
       $(H)\copy.hs  $(H)\copy.sdm $(H)\delfiles.hs $(H)\delfiles.sdm

copy.obj fileops.obj :  $(H)\common.h $(H)\filemgr.h $(H)\text.h \
       $(H)\fileopts.hs  $(H)\fileopts.sdm  $(H)\rename.hs $(H)\rename.sdm \
		$(H)\help.h

debug.obj:      $(H)\common.h $(H)\filemgr.h

errbox.obj:     $(H)\common.h $(H)\filemgr.h $(H)\err.hs $(H)\err.sdm \
			$(H)\attr0.hs $(H)\attr0.sdm \
			$(H)\critnew.hs $(H)\critnew.sdm \
			$(H)\delitem.hs $(H)\delitem.sdm

attr.obj:       $(H)\common.h $(H)\filemgr.h $(H)\text.h \
		$(H)\attr.hs $(H)\attr.sdm \
		$(H)\help.h

locate.obj:     $(H)\common.h $(H)\filemgr.h $(H)\locate.hs $(H)\locate.sdm

filemgr.obj:    $(H)\common.h $(H)\filemgr.h $(H)\menus.h

graphics.obj:   $(H)\common.h

helper.obj:     $(H)\common.h $(H)\filemgr.h \
		$(H)\help.h

icons.obj:      $(H)\common.h $(H)\icons.h

launch.obj:     $(H)\common.h $(H)\text.h \
		$(H)\run.hs $(H)\run.sdm \
		$(H)\associat.hs $(H)\associat.sdm \
		$(H)\assoc2.hs $(H)\assoc2.sdm \
		$(H)\help.h

lbox.obj:       $(H)\common.h $(H)\filemgr.h $(H)\icons.h

list.obj:       $(H)\common.h $(H)\filemgr.h

listbox.obj:    $(H)\common.h              $(H)\icons.h

memmgr.obj:     $(H)\common.h \
		$(H)\outofmem.hs $(H)\outofmem.sdm

menus.obj:      $(H)\common.h              $(H)\menus.h

screen.obj:     $(H)\common.h $(H)\filemgr.h $(H)\menus.h

disp.obj:       $(H)\common.h $(H)\filemgr.h $(H)\menus.h \
		$(H)\dispopt.hs $(H)\dispopt.sdm

showinfo.obj:   $(H)\common.h $(H)\filemgr.h $(H)\menus.h \
		$(H)\showinfo.hs $(H)\showinfo.sdm


groups.obj:     $(H)\common.h \
		$(H)\addgroup.hs $(H)\addgroup.sdm $(H)\addprog.hs $(H)\addprog.sdm \
		$(H)\adddialo.hs $(H)\adddialo.sdm $(H)\password.hs $(H)\password.sdm \
		$(H)\new.hs $(H)\new.sdm

help.obj:       $(H)\help.h $(H)\help.hs $(H)\help.sdm

cmdlan.obj:     $(H)\text.h $(H)\user.hs $(H)\user.sdm

view.obj:       $(H)\help.h $(H)\text.h

#
#   Link OBJS to create NAME.exe file
$(NAME).exe $(NAME).map: $(OBJSLD) $(OBJS1) $(OBJS2) $(OBJS3) $(OBJS4) $(OBJS5)\
	$(OBJS6) $(OBJS7) $(OBJS8) $(OBJS9) $(OBJS10) $(OBJS11) $(OBJSDB)
	$(LINK) @<<
$(OBJSLD)+
$(OBJS1) +
$(OBJS2) +
$(OBJS3) +
$(OBJS4) +
$(OBJS5) +
$(OBJS6) +
$(OBJS7) +
$(OBJS8) +
$(OBJS9) +
$(OBJS10) +
$(OBJS11) +
$(OBJSDB)
$(NAME)
$(NAME)
$(LIBRARY)
NUL;
<<
	exemod $(NAME).exe -min 0 -max 0
!IFDEF CVW
	msym dosshell
!ENDIF

