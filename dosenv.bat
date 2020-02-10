@echo off

rem SYNTAX: dosenv [drive:]
rem where "drive" is the drive the build tree resides in.

rem  This batch file sets up the environment for building DOS.
rem  Note that running this more than once per session will result in
rem  compounded growth of the path and probably related errors.
rem                     ****CAUTION*****
rem      Build problems will occur if you use more environment variables
rem      then are set below. The build will stop in the BOOT directory
rem      and nosrvbld.exe gives a "Null Pointer" error. If this happens
rem      reboot and use only this environment. Adding more environment
rem      space from command.com does not fix it.


prompt [dos 6 $p] $

if "%1"=="" set oakdrive=C:
if not "%1"=="" set oakdrive=%1

set oakpath=%oakdrive%\msdos60


set PATH=%oakpath%\TOOLS\BIN;%PATH%

rem  If you have a RAM drive, it will probably speed up your system to
rem  point these variables to a temporary directory there.
set TEMP=%oakpath%\tools\tmp
set TMP=%oakpath%\tools\tmp

set INIT=%oakpath%\tools\bin
set INCLUDE=%oakpath%\tools\include
set LIB=%oakpath%\tools\lib
set PROJ=500

REM COUNTRY needs to be in CAPS....

set COUNTRY=USA
set ROMDOS=
set BUILDER=YES
set TDSTAMP=/DATE:1992.04.02 /TIME:06.00.00
set DSK=360DSK0
set LANG_SRC=%oakpath%\LANG

rem  Clean up garbage which may remain from previous NMK use.
if exist %TMP%\pwb.shl del %TMP%\pwb.shl

rem  Remove any DEBUG environment variable definition ; M001
set DEBUG=

set oakdrive=
set oakpath=
