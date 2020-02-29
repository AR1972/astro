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
#        $(MAKE)
    cd ..\mkimage
#        $(MAKE)
    cd ..
	
clean:
    cd messages
        nmake clean
    cd ..\boot
        nmake clean
    cd ..\dos
        nmake clean
	cd ..\cmd
        nmake clean
    cd ..\dev
        nmake clean
	cd ..\bios
        nmake clean
    cd ..\dosshell
        nmake clean
    cd ..\mkimage
        nmake clean
    cd ..