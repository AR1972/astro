# Runs through all of the install directories and creates
# and OEM and Retail installation programs.

ALL:
	cd LIB
	$(MAKE)
!ifndef OEMBASE
	cd ..\retail
	$(MAKE)
	cd ..\recover
	$(MAKE)
	cd ..\dosdata
	$(MAKE)
	cd ..\cleanup
	$(MAKE)
!endif   
        cd ..\oem
	$(MAKE)
	cd ..\oemdata
	$(MAKE)
	cd ..

