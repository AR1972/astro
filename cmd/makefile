#************************** CMD Root level Makefile ***********************

all:
!IF "$(COUNTRY)" == "JAPAN" || "$(COUNTRY)" == "japan"
    cd adddrv
        $(MAKE)
    cd ..
!ENDIF
    cd append
        $(MAKE)
    cd ..\assign
        $(MAKE)
    cd ..\attrib
        $(MAKE)
    cd ..\backup
        $(MAKE)
    cd ..\chkdsk
        $(MAKE) /a BASE=
        $(MAKE) /a
    cd ..\command
        $(MAKE) /a BASE=
        $(MAKE) /a
    cd ..\comp
#        $(MAKE) #needs strings
    cd ..\cps
        $(MAKE)
        cd ..\debug
        $(MAKE)
    cd ..\diskcomp
        $(MAKE)
    cd ..\diskcopy
        $(MAKE)
    cd ..\doskey
        $(MAKE)
    cd ..\edlin
        $(MAKE)
    cd ..\exe2bin
        $(MAKE)
    cd ..\expand
        $(MAKE)
    cd ..\fastopen
        $(MAKE)
    cd ..\fc
        $(MAKE)
    cd ..\fdisk
#        $(MAKE) # broken probably a missing ;
    cd ..\find
        $(MAKE)
    cd ..\format
        $(MAKE) /a BASE=
        $(MAKE) /a
    cd ..\graftabl
        $(MAKE)
    cd ..\graphics
        $(MAKE)
    cd ..\help
        $(MAKE)
    cd ..\join
#        $(MAKE) # missing lib\master.lib
    cd ..\keyb
        $(MAKE)
    cd ..\label
        $(MAKE)
    cd ..\loadfix
        $(MAKE)
    cd ..\mem
#        $(MAKE) # needs strings
    cd ..\mode
        $(MAKE)
    cd ..\more
        $(MAKE)
    cd ..\nlsfunc
        $(MAKE)
    cd ..\print
        $(MAKE)
    cd ..\printfix
        $(MAKE)
!IF "$(COUNTRY)" != "JAPAN" && "$(COUNTRY)" != "japan"
    cd ..\qbasic
        $(MAKE)
!ENDIF
    cd ..\recover
        $(MAKE)
    cd ..\redir
        $(MAKE)
    cd ..\replace
        $(MAKE)
    cd ..\restore
        $(MAKE)
    cd ..\setver
        $(MAKE) /a BASE=
        $(MAKE) /a
    cd ..\share
#        $(MAKE)		#needs strings
    cd ..\sort
#        $(MAKE)     #needs strings
    cd ..\subst
#        $(MAKE)		#master.lib missing
    cd ..\sys
        $(MAKE) /a BASE=
        $(MAKE) /a
    cd ..\tree
        $(MAKE)
    cd ..\wina20
        $(MAKE)
    cd ..\xcopy
        $(MAKE)
    cd ..
	
clean:
    cd adddrv
        nmake clean
    cd ..\append
        nmake clean
    cd ..\assign
        nmake clean
    cd ..\attrib
        nmake clean
    cd ..\backup
        nmake clean
    cd ..\chkdsk
        nmake clean
    cd ..\command
        nmake clean
    cd ..\comp
        nmake clean
    cd ..\cps
        nmake clean
    cd ..\debug
        nmake clean
    cd..\diskcomp
        nmake clean
    cd..\diskcopy
        nmake clean
    cd..\doskey
        nmake clean
    cd..\edlin
        nmake clean
    cd..\exe2bin
        nmake clean
    cd..\expand
        nmake clean
    cd..\fastopen
        nmake clean
    cd..\fc
        nmake clean
    cd..\fdisk
        nmake clean
    cd..\find
        nmake clean
    cd ..\format
        nmake clean
    cd ..\graftabl
        nmake clean
    cd ..\graphics
        nmake clean
    cd ..\help
        nmake clean
    cd ..\join
        nmake clean
    cd ..\keyb
        nmake clean
    cd ..\label
        nmake clean
    cd ..\loadfix
        nmake clean
    cd ..\mem
        nmake clean
    cd ..\mode
        nmake clean
    cd ..\more
        nmake clean
    cd ..\nlsfunc
        nmake clean
    cd ..\print
        nmake clean
    cd ..\printfix
        nmake clean
    cd ..\qbasic
        nmake clean
    cd ..\recover
        nmake clean
    cd ..\redir
        nmake clean
    cd ..\replace
        nmake clean
    cd ..\restore
        nmake clean
    cd ..\setver
        nmake clean
    cd ..\share
        nmake clean
    cd ..\sort
        nmake clean
    cd ..\subst
        nmake clean
    cd ..\sys
        nmake clean
    cd ..\tree
        nmake clean
    cd ..\wina20
        nmake clean
    cd ..\xcopy
        nmake clean
    cd ..


