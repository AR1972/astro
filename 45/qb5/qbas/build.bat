@echo off

for %%a in (USA DUT FRN GER ITN POR SPA SWE XXX) do if %%a==%1 goto start
for %%a in (usa dut frn ger itn por spa swe xxx) do if %%a==%1 goto start


echo BUILD MS version of QBASIC.EXE and EDIT.COM (common for both)
echo Usage is:   BUILD [DUT  FRN  GER  ITN  POR  SPA  SWE  USA] [QB_ROOT]
goto end


:start


if "%2"=="" goto default
set QBROOT=%2
goto setup
:default
set QBROOT=..\..\


:setup
set lang=%1

set TLX=%TL%
set LIBx=%LIB%
set PATHX=%PATH%

set TL=..\..\TL\BIN
set LIB=..\..\TL\LIB
set PATH=..\..\tl\bin;..\..\tl\binb;
set INCLUDE=..\..\tl\inc

copy %QBROOT%qb%lang%\strings\messages.tok
tok -f messages.tok ..\..\QB\HD\messages.src            > ..\..\qb\hd\messages.inc
tok -f messages.tok ..\..\runtime\inc\messages.src > ..\..\runtime\inc\messages.inc
del messages.tok

copy %QBROOT%qb%lang%\strings\intl.inc
copy %QBROOT%qb%lang%\strings\intl.inc ..\uq

copy %QBROOT%qb%lang%\strings\tokens.tok
tok -f tokens.tok ..\uq\uifile.src              > ..\uq\uifile.c
tok -f tokens.tok ..\uq\uioptns.src             > ..\uq\uioptns.c
tok -f tokens.tok ..\uq\uirsrcc.src             > ..\uq\uirsrcc.c
del tokens.tok

copy %QBROOT%qb%lang%\strings\sizes.tok
tok -f sizes.tok ..\uq\uinhelp.src              > ..\uq\uinhelp.asm
del sizes.tok

copy %QBROOT%qb%lang%\strings\uiwind.tok
tok -f uiwind.tok ..\uq\uiwind.src              > ..\uq\uiwind.c
del uiwind.tok

copy %QBROOT%qb%lang%\strings\qedit.tok
tok -f qedit.tok ..\ir\qedit.src                > ..\ir\qedit.asm
REM  Needed for help.asm tokens.....
tok -f qedit.tok ..\ir\help.src                > ..\ir\help.asm
del qedit.tok


rem copy %QBROOT%qb%lang%\strings\herc.tok
rem tok -f herc.tok ..\..\runtime\herc\fmakeres.src         > ..\..\runtime\herc\fmakeres.asm
rem del herc.tok
rem cd  ..\..\runtime\tl
rem nmake -f hercmake
rem  msherc12.com ..\..\qb5\qbas\msherc.com

rem cd  ..\..\qb5\qbas


copy %QBROOT%qb%lang%\strings\itl.h ..\..\cow
cd ..\..\cow
call build
copy cow.lib ..\qb5\qbas
cd ..\qb5\qbas


cd ..\..\runtime\qbasic
call ..\tl\bldkit qbasic
nmake
copy bqb50.lib ..\..\qb5\qbas
cd ..\..\qb5\qbas

copy %QBROOT%qb%lang%\strings\qbasmsgs.txt ..\ir
copy %QBROOT%qb%lang%\strings\des\*.des ..\uq\des


%TL%\redirect %TL%\nmake -r localSources > qb.log
%TL%\nmake -nr qb.exe edit.com help.com HELP %1 | %TL%\sed -f notab.sed >makeit.bat
%TL%\redirect -e qb.log makeit >> qb.log
%TL%\results qb.log


set TL=%TLX%
set LIB=%LIBX%
set PATH=%PATHX%


set QBROOT=
set TLX=
set LIBX=
set pathx=
set lang=


:end
