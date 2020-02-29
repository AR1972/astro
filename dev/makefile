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
#	$(MAKE)
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
	$(MAKE)
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
	
clean:
	cd ansi
	nmake clean
	cd ..\country
	nmake clean
	cd ..\display
	nmake clean
	cd ..\driver
	nmake clean
	cd ..\ega
	nmake clean
	cd ..\emm386
	nmake clean
	cd ..\himem
	nmake clean
	cd ..\iceland
	nmake clean
	cd ..\keyboard
	nmake clean
	cd ..\kkcfunc
	nmake clean
	cd ..\monoumb
	nmake clean
	cd ..\mouse
	nmake clean
	cd ..\power
	nmake clean
	cd ..\printer
	nmake clean
	cd ..\ramdrive
	nmake clean
	cd ..\smartdrv
	nmake clean
	cd ..\romdrive
	nmake clean
	cd ..

