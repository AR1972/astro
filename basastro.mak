# Non-Upgrade (OEM BASE) Astro top level makefile

ROOT    = \msdos60
DOSMAKE = $(ROOT)\tools\bin\nmake
RESULTS = $(ROOT)\compress

all:
  del $(RESULTS)\*.*

  SET PATH=$(ROOT)\TOOLS\BIN
  SET LIB=$(ROOT)\TOOLS\LIB
  SET INCLUDE=.;$(ROOT)\TOOLS\INCLUDE
  SET PROJ=JANUS
  SET COUNTRY=USA
  SET LANG_SRC=$(ROOT)\LANG

  SET OEMBASE=TRUE
  
  cd messages
  $(DOSMAKE)
  cd ..

  cd lib
  $(DOSMAKE)
  cd ..

  cd boot
  $(DOSMAKE)
  cd ..

  cd cmd\fdisk
  $(DOSMAKE)
  cd ..\..

  cd install\lib
  $(DOSMAKE)
  cd ..\..

  cd install\oem
  $(DOSMAKE)
  copy BASESET.EXE $(RESULTS)\baseSET.EXE
        copy baseSET.MSG $(RESULTS)
  cd ..\..

  cd install\basedata\12
  $(DOSMAKE)
  copy SETUP12.OEM $(RESULTS)\DOSSETUP.INI
  
  cd ..\144
  $(DOSMAKE)
  copy SETUP144.OEM $(RESULTS)
  cd ..\..\..

  SET OEMBASE=
