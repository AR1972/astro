prompt [qb $p] $

if "%1" == "DEBUG" goto doDebug
if "%1" == "debug" goto doDebug
REM Do retail build
goto doBuild

:doDebug
set CC_USR=-Zi
set MASM_USR=-Zi
set LINK_USR=/CO

:doBuild
set TL=\45\TL\BIN
set LIB=\45\TL\LIB
set INCLUDE=\45\TL\INC
%TL%\redirect %TL%\nmake -r localSources > qb.log
%TL%\nmake -nr qb.exe edit.com help.com HELP %1 | %TL%\sed -f notab.sed >makeit.bat
%TL%\redirect -e qb.log makeit >> qb.log
%TL%\results qb.log
