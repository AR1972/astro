#************************* Root level Makefile *************************


all:
	cd tools\bin\source\buildmsg
	$(MAKE)
	cd ..\..\..\..
	cd messages
        $(MAKE)
	cd ..\boot
        $(MAKE)
        cd ..\dos
        $(MAKE)
        makedos
        cd ..\cmd
        $(MAKE)
        cd ..\dev
        $(MAKE)
        cd ..\dosshell
        $(MAKE)
        cd ..\mkimage
        $(MAKE)
        cd ..
