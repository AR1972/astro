rem @echo off

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
set QBROOT=d:\qbasic\


:setup
set lang=%1

set TLX=%TL%
set LIBx=%LIB%

set TL=..\..\TL\BINB
set LIB=..\..\TL\LIB

copy %QBROOT%qb%lang%\strings\messages.tok
%tl%\tok -f messages.tok ..\..\QB\HD\messages.src       > ..\..\qb\hd\messages.inc
%tl%\tok -f messages.tok ..\..\runtime\inc\messages.src > ..\..\runtime\inc\messages.inc
del messages.tok

copy %QBROOT%qb%lang%\strings\intl.inc
copy %QBROOT%qb%lang%\strings\intl.inc ..\uq

copy %QBROOT%qb%lang%\strings\tokens.tok
%tl%\tok -f tokens.tok ..\uq\uifile.src		> ..\uq\uifile.c
%tl%\tok -f tokens.tok ..\uq\uioptns.src		> ..\uq\uioptns.c
%tl%\tok -f tokens.tok ..\uq\uirsrcc.src		> ..\uq\uirsrcc.c
del tokens.tok

copy %QBROOT%qb%lang%\strings\sizes.tok
%tl%\tok -f sizes.tok ..\uq\uinhelp.src	  > ..\uq\uinhelp.asm
del sizes.tok

copy %QBROOT%qb%lang%\strings\uiwind.tok
%tl%\tok -f uiwind.tok ..\uq\uiwind.src		> ..\uq\uiwind.c
del uiwind.tok


copy %QBROOT%qb%lang%\strings\herc.tok
%tl%\tok -f herc.tok ..\..\runtime\herc\fmakeres.src		> ..\..\runtime\herc\fmakeres.asm
del herc.tok
cd  ..\..\runtime\tl
%tl%\nmake -f hercmake
copy msherc12.com ..\..\qb5\qbas\msherc.com
cd  ..\..\qb5\qbas


copy %QBROOT%qb%lang%\strings\qedit.tok
%tl%\tok -f qedit.tok ..\ir\qedit.src		> ..\ir\qedit.asm
del qedit.tok


copy %QBROOT%qb%lang%\strings\itl.h ..\..\cow
cd ..\..\cow
call build
copy cow.lib ..\qb5\qbas
cd ..\qb5\qbas


cd ..\..\runtime\qbas
call ..\tl\bldkit qbas
%tl%\nmake
copy bqb50.lib ..\..\qb5\qbas
cd ..\..\qb5\qbas

copy %QBROOT%qb%lang%\strings\qbasmsgs.txt ..\ir

copy %QBROOT%qb%lang%\strings\des\*.des ..\uq\des


%TL%\nmake -r localSources -f makefile 1> qb.log 2>&1
%TL%\nmake -r qb.exe edit.com -f makefile 1>>qb.log 2>&1 
%TL%\results qb.log                                         

set TL=%TLX%
set LIB=%LIBX%


set QBROOT=
set TLX=
set LIBX=
set lang=


:end

