@echo off

if "%1"=="" goto syntax
if "%2"=="" goto syntax
if "%3"=="" goto syntax

set oaktype=FULL

set OAKDIR=%1
set LR=%2
set COUNTRY=%3

nmake -f build.oak %oaktype%

set OAKDIR=
set COUNTRY=
set LR=

goto done


:syntax
echo.
echo SYNTAX: BUILDOAK [oak-dir] [lang-root] [country]
echo   [oak-dir]   - Directory to build OAK in.
echo   [lang-root] - Root of language directory (ie. "LANG", "C:\INTL\LANG").
echo   [country]   - Language to build (USA, FRN, GER, SPA, etc).
echo.
echo This batch file builds the MS-DOS 6 OEMFULL OAK tree from the normal
echo DOS6 source tree.
echo.
echo WARNING: You must run this batch file from the root (MSDOS60) directory
echo   of the source tree.
echo You must also have built the project first, as the OAK uses built OBJ's
echo   and LIB's from the tree.
echo.

:done

