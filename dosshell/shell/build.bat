@echo OFF
cd shellh
if NOT "%1" == "clean" goto dontdel
del *.hs
del *.sdm
:dontdel
nmk
if ERRORLEVEL 1 goto fail1
cd ..\iniparse
if "%1" == "clean" del *.obj
nmk
if ERRORLEVEL 1 goto fail1
cd ..\loader
if "%1" == "clean" del *.obj
if "%1" == "clean" del switcher\*.obj
nmk
if ERRORLEVEL 1 goto fail2
cd ..
if "%1" == "clean" del *.obj
nmk
if ERRORLEVEL 1 goto fail3
echo BUILD COMPLETED SUCCESSFULLY
goto outtahere
:fail1
:fail2
cd ..
:fail3
echo ERROR DURING BUILD
:outtahere
