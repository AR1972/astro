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
#	$(MAKE)
	cd ..\attrib
	$(MAKE)
	cd ..\backup
#	$(MAKE)

	cd ..\chkdsk
	$(MAKE) /a BASE=
	$(MAKE) /a

	cd ..\command
	$(MAKE) /a BASE=
	$(MAKE) /a

	cd ..\comp
#	$(MAKE)
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
#	$(MAKE)
	cd ..\exe2bin
#	$(MAKE)
	cd ..\expand
	$(MAKE)
	cd ..\fastopen
	$(MAKE)
	cd ..\fc
	$(MAKE)
	cd ..\fdisk
	$(MAKE)
	cd ..\find
	$(MAKE)

	cd ..\format
	$(MAKE) /a BASE=
	$(MAKE) /a

	cd ..\graftabl
#	$(MAKE)
	cd ..\graphics
	$(MAKE)
	cd ..\help
	$(MAKE)
	cd ..\join
#	$(MAKE)
	cd ..\keyb
	$(MAKE)
	cd ..\label
	$(MAKE)
	cd ..\loadfix
	$(MAKE)
	cd ..\mem
	$(MAKE)
	cd ..\mode
	$(MAKE)
	cd ..\more
	$(MAKE)
	cd ..\nlsfunc
	$(MAKE)
	cd ..\print
	$(MAKE)
	cd ..\printfix
#	$(MAKE)
!IF "$(COUNTRY)" != "JAPAN" && "$(COUNTRY)" != "japan"
	cd ..\qbasic
	$(MAKE)
!ENDIF
	cd ..\recover
#	$(MAKE)
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
	$(MAKE)
	cd ..\sort
	$(MAKE)
	cd ..\subst
	$(MAKE)

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


