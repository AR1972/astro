#************************* Root level Makefile *************************


all:
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
