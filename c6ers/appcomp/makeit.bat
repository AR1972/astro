@echo off
rem set PATHSAV=%PATH%
rem set INCSAV=%INCLUDE%
rem set LIBSAV=%LIB%
set DEBUGSAV=%DEBUG%

set CMPROOT=

set DEBUG=NO
set DECOMP=COMPLEX
set OS=DOS
set OSLET=R
set MODEL=S

goto :got_env

rem
rem Default to building with C6
rem
rem goto :c51_env

rem C6 build

set CVER=C6
set PATH=c:\astro\c6ers\tools6\bin;c:\bin
set INCLUDE=c:\astro\c6ers\tools6\include;%CMPROOT%
set LIB=c:\astro\c6ers\tools6\lib;%CMPROOT%

goto :got_env

rem C5.1 build

:c51_env
set CVER=C51
set PATH=c:\astro\tools\bin;c:\bin
set INCLUDE=c:\astro\tools\include;%CMPROOT%
set LIB=c:\astro\tools\lib;%CMPROOT%

:got_env

nmake -f mkfile2 -nologo clean > NUL
nmake -f mkfile2 -nologo		 > MAKE.OUT

copy sdecompr.lib ..\..\lib
rem copy compress.exe %RELEASE%\compress.exe
rem copy decomp.exe %RELEASE%\decomp.exe

set DEBUG=NO
set MODEL=L

nmake -f mkfile2 -nologo clean > NUL
nmake -f mkfile2 -nologo		 >> MAKE.OUT

copy ldecompr.lib ..\..\lib

rem set PATH=%PATHSAV%
rem set INCLUDE=%INCSAV%
rem set LIB=%LIBSAV%
set DEBUG=%DEBUGSAV%

rem set PATHSAV=
rem set INCSAV=
rem set LIBSAV=
set DEBUGSAV=
set DECOMP=
set OS=
set OSLET=
set MODEL=
set CMPROOT=
set RELEASE=
