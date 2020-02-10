#************************** DEV root level Makefile ***********************


all:
	cd ansi
	$(MAKE)
	cd ..\country
	$(MAKE)
	cd ..\display
	$(MAKE)
	cd ..\driver
	$(MAKE)
	cd ..\ega
	$(MAKE)
	cd ..\emm386
	$(MAKE)
	cd ..\himem
	$(MAKE)
	cd ..\iceland
	$(MAKE)
	cd ..\keyboard
	$(MAKE)
!IF "$(COUNTRY)" == "JAPAN" || "$(COUNTRY)" == "japan"
	cd ..\kkcfunc
	$(MAKE)
!ENDIF
	cd ..\monoumb
	$(MAKE)
	cd ..\mouse
	$(MAKE)
	cd ..\power
	$(MAKE)
	cd ..\printer
#	$(MAKE)
	cd ..\ramdrive
	$(MAKE)
	cd ..\smartdrv
	$(MAKE)
!ifndef ROMDRIVE
!ifdef  ROMDOS
	cd ..\romdrive
	$(MAKE)
!endif
!endif
	cd ..
