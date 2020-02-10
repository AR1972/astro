#************************** C6ers Root level Makefile ***********************

all:
	cd msd200a
	$(MAKE)
        copy msd.exe ..\..\binaries
        copy msd.exe ..\..\compress
	cd ..\smartmon
	$(MAKE)
        copy smartmon.exe ..\..\binaries
        copy smartmon.hlp ..\..\binaries
        compress -f smartmon.exe ..\..\compress\smartmon.ex_
        compress -f smartmon.hlp ..\..\compress\smartmon.hl_
        cd ..\choice
        $(MAKE)
        cd ..\newcmds
        $(MAKE)
        copy deltree.exe ..\..\binaries
        copy move.exe ..\..\binaries
        compress -f deltree.exe ..\..\compress\deltree.ex_
        compress -f move.exe ..\..\compress\move.ex_
	cd ..\interlnk\paragon\lib
	$(MAKE)
	cd ..\..\fxlib
	$(MAKE)
	cd ..\driver
	$(MAKE)
	cd ..
	$(MAKE)

	cd ..
