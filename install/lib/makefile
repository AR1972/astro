# Runs through all of the library subdirectories and
# builds each individual libraray and then uses LIBUILD
# to build a combined library which can be linked with
# the individual programs.

# INTERNATIONAL MODIFICATION- Since command lines become too large if
# we try to point to the LANG\directory for the correct language dependent
# window.h file, the correct window.h is copied from the lib directory makefile.

MSG_INC=$(LANG_SRC)\$(COUNTRY)\install\include

BUILD_L=TRUE
ModelType = BUILD_L=TRUE

BUILD_M=TRUE
#ModelType = BUILD_M=TRUE
ModelType = BUILD_M=TRUE BUILD_L=TRUE
inc     = ..\include


ALL:
	    del  $(inc)\window.h
	    copy $(MSG_INC)\window.h $(inc)
!IFDEF BUILD_L
	if not exist BIOS\large\nul     md BIOS\large
	if not exist COPY\large\nul     md COPY\large
	if not exist DISK\large\nul     md DISK\large
	if not exist HDISK\large\nul    md HDISK\large
	if not exist FILE\large\nul     md FILE\large
	if not exist HARDWARE\large\nul md HARDWARE\large
	if not exist PROMPTS\large\nul  md PROMPTS\large
	if not exist STRLIB\large\nul   md STRLIB\large
	if not exist WINDOW\large\nul   md WINDOW\large
!ENDIF
!IFDEF BUILD_M
	if not exist BIOS\medium\nul    md BIOS\medium
	if not exist COPY\medium\nul    md COPY\medium
	if not exist DISK\medium\nul    md DISK\medium
	if not exist HDISK\medium\nul   md HDISK\medium
	if not exist FILE\medium\nul    md FILE\medium
	if not exist HARDWARE\medium\nul        md HARDWARE\medium
	if not exist PROMPTS\medium\nul md PROMPTS\medium
	if not exist STRLIB\medium\nul  md STRLIB\medium
	if not exist WINDOW\medium\nul  md WINDOW\medium
!ENDIF
	cd BIOS
	$(MAKE) $(ModelType)
	cd ..\DISK
	$(MAKE) $(ModelType)
	cd ..\STRLIB
	$(MAKE) $(ModelType)
	cd ..\FILE
	$(MAKE) $(ModelType)
	cd ..\WINDOW
	$(MAKE) $(ModelType)
	cd ..\PROMPTS
	$(MAKE) $(ModelType)
	cd ..\COPY
	$(MAKE) $(ModelType)
	cd ..\HARDWARE
	$(MAKE) $(ModelType)
	cd ..\HDISK
	$(MAKE) $(ModelType)
	cd ..\pluscard
	$(MAKE)
	cd ..
	$(MAKE) $(ModelType) -f LIBUILD
	
