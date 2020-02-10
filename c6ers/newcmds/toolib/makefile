#
#		    MAKEFILE FOR TOOLS LIBRARY
#
#   Targets:
#	
#	all		- make all libraries
#	clean		- remove the obj files
#	clobber		- purge all libraries and obj files
#       stoolsr         - small model real mode
#       ctoolsr         - compact model real mode
#	mtoolsr		- medium model real mode
#       ltoolsr         - large model real mode
#
#  When issuing the make command, one command line switch is recognized.
#
#	ROOT = <path>	default: .
#
#  The ROOT command line parameter is used only for the "install" target.
#  When given, the installation of protect and real mode libraries will
#  be copied into $ROOT
#

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\c6ers\newcmds\toolib\src

!INCLUDE h\librules.mak

### Do not make Compact, Medium, or Large model libraries
# all:  stoolsr ctoolsr mtoolsr ltoolsr
all:  icopy stoolsr

icopy:
      copy $(MSG_DIR)\syserr.msg src\syserr.msg
      copy $(MSG_DIR)\messages.msg src\messages.msg
      copy $(MSG_DIR)\fcopytxt.h src\fcopytxt.h	

doc:
	cd doc
	$(MAKE) all
	cd ..

stoolsr: 
	cd src
	$(MAKE) stoolsr
	cd ..

### Do not make Compact, Medium, or Large model libraries
!IF 0
ctoolsr:
	cd src
	$(MAKE) ctoolsr
	cd ..

mtoolsr:
	cd src
	$(MAKE) mtoolsr
	cd ..

ltoolsr:
	cd src
	$(MAKE) ltoolsr
	cd ..
!ENDIF

clean:
	- $(DEL) sobjr\*.obj
	- $(DEL) cobjr\*.obj
	- $(DEL) mobjr\*.obj
	- $(DEL) lobjr\*.obj

clobber: clean
	- $(DEL) lib\*.lib
	- $(DEL) lib\*.map
