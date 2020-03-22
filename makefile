#************************* Root level Makefile *************************

all:
    cd c6ers
        $(MAKE)
    cd ..\messages
        $(MAKE)
    cd ..\lib
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
        $(MAKE) clean
    cd ..\boot
        $(MAKE) clean
    cd ..\dos
        $(MAKE) clean
    cd ..\cmd
        $(MAKE) clean
    cd ..\dev
        $(MAKE) clean
    cd ..\bios
        $(MAKE) clean
    cd ..\dosshell
        $(MAKE) clean
    cd ..\mkimage
        $(MAKE) clean
    cd ..\c6ers
        $(MAKE) clean
    cd ..\lib
        $(MAKE) clean
    cd ..