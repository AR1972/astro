@echo off

REM Make sure that the ROOT is correct for your builds. Modify the
REM root environment variable to be sure the build works properly.
REM You must specify a country variable at the command line.

if "%1"=="" goto usage

SET ROOT=\MSDOS60
SET DOSMAKE=%ROOT%\TOOLS\BIN\NMAKE
SET CPRS=%ROOT%\compress
SET BIN=%ROOT%\binaries
SET TDSTAMP=/DATE:1993.03.09 /TIME:06.00.00
SET COUNTRY=%1
SET LANG_SRC=%ROOT%\LANG
SET PROJ=500
SET BUILDER=YES
SET INIT=%ROOT%\tools\bin

echo ********************** building c6ers **********************************
set savep=%path%
set PATH=%ROOT%\c6ers\tools6\bin
set lib=%ROOT%\c6ers\tools6\lib;%ROOT%\c6ers\toolsvr\lib
set include=.;%ROOT%\c6ers\tools6\include;..\toolsvr\inc

	cd c6ers\appcomp
	call makeit
	cd ..
	%DOSMAKE%
	cd %ROOT%

echo ********************** building magicdrve ###***************************
set PATH=%root%\C6ERS\TOOLS6\BIN;%root%\tools\bin;%PATH%
set INCLUDE=%root%\MAGICDRV\INC;%root%\C6ERS\TOOLS6\INCLUDE
set LIB=%root%\MAGICDRV\LIB;%root%\C6ERS\TOOLS6\LIB
	
	cd magicdrv
	call build.bat
	cd driver
	copy dblspace.bin %BIN%
	copy dblspace.bin %CPRS%
	copy dblspace.sys %BIN%
	copy dblspace.sys %CPRS%
	copy dsvxd.386 %BIN%
	compress -f dsvxd.386 %CPRS%\dsvxd.38_
	cd ..\setupman
	copy dblspace.inf %BIN%
	compress -f dblspace.inf %CPRS%\dblspace.in_
	copy dblspace.hlp %BIN%
	compress -f dblspace.hlp %CPRS%\dblspace.hl_
	copy dblspace.exe %BIN%
	compress -f dblspace.exe %CPRS%\dblspace.ex_
	cd %ROOT%

echo ********************** building msdos60 base ***************************
SET PATH=%ROOT%\TOOLS\BIN;%path%
SET INIT=%ROOT%\tools\bin
SET INCLUDE=%ROOT%\tools\include
SET LIB=%ROOT%\tools\lib

	%DOSMAKE%
	cd ..

echo ********************** building install ********************************
SET PROJ=JANUS
SET UJANUS=1
SET OEMBASE=

	cd install\lib
	%DOSMAKE%
	cd ..\..

	cd install\oem
	%DOSMAKE%
	cd ..\..

	cd install\retail
	%DOSMAKE%
	copy %LANG_SRC%\%COUNTRY%\install\common\FULLSET.MSG %BIN%\SETUP.MSG
	copy %LANG_SRC%\%COUNTRY%\install\common\FULLSET.MSG %CPRS%\SETUP.MSG
	cd ..\..

	cd install\recover
	%DOSMAKE%
	cd ..\..

	cd install\cleanup
	%DOSMAKE%
	cd ..\..

	cd install\dosdata\12
	%DOSMAKE%
	copy %CPRS%\SETUP12.UPG %CPRS%\DOSSETUP.INI
	cd ..\..\..
		
	cd install\dosdata\144
	%DOSMAKE%
	cd ..\..\..

	cd install\oemdata\12
	%DOSMAKE%
	cd ..\144
	%DOSMAKE%
	cd ..\..\..

	cd install\basedata\12
	%DOSMAKE%
	cd ..\144
	%DOSMAKE%

	
	cd %ROOT%


SET UJANUS=

	cd install\oem
	%DOSMAKE% /a
	pklite -o setup.exe
	copy SETUP.exe %CPRS%\setup.oem
	copy SETUP.exe %BIN%\setup.oem
	cd ..\..

SET OEMBASE=TRUE

	cd install\oem
	%DOSMAKE% /a
	copy %LANG_SRC%\%COUNTRY%\install\common\BASESET.MSG %BIN%
	copy %LANG_SRC%\%COUNTRY%\install\common\BASESET.MSG %CPRS%
	cd ..\..

	
SET OEMBASE=

goto end

:USAGE

echo Builds PC-DOS 6
echo.
echo buildall [language]
echo.
echo   [language] - Specifies language to build (USA, FRN, GER, SPA, etc).
echo.


:END

set path=%savep%
set savep=
SET ROOT=
SET DOSMAKE=
SET CPRS=
SET BIN=
