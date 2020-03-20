#************************** C6ers Root level Makefile ***********************

all:
    cd appcomp
	nmake /a MODEL=S /a OS=DOS /a DECOMP=COMPLEX /a OSLET=R /a DEBUG=NO -f mkfile2
	nmake /a MODEL=M /a OS=DOS /a DECOMP=COMPLEX /a OSLET=R /a DEBUG=NO -f mkfile2
	nmake /a MODEL=L /a OS=DOS /a DECOMP=COMPLEX /a OSLET=R /a DEBUG=NO -f mkfile2
    copy ldecompr.lib ..\..\lib
	copy mdecompr.lib ..\..\lib
	copy sdecompr.lib ..\..\lib	
	cd ..\msd200a
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
#	$(MAKE)
	cd ..\..\fxlib
#	$(MAKE)
	cd ..\driver
#	$(MAKE)
	cd ..
#	$(MAKE)
	cd ..

clean:
    cd msd200a
    nmake clean
    cd ..\smartmon
    nmake clean
    cd ..\choice
    nmake clean
    cd ..\newcmds
    nmake clean
    cd ..\appcomp
    nmake -f mkfile2 clean
    cd ..