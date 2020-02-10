setlocal
set TL=\45\TL\BINB
set LIB=\45\TL\LIB
%TL%\nmake -r localSources -f makefile 1> qb.log 2>&1
%TL%\nmake -r qb.exe edit.com %1 -f makefile 1>>qb.log 2>&1
%TL%\results qb.log
