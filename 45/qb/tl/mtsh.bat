echo off
del log
del err
redirect -e log make localSources -f ..\ir\makefile > log
make -n %1 %2 %3 tshell.exe -f makefile -f ..\qb\makefile -f ..\ir\makefile >makeit.bat
redirect -e err makeit >> log
del makeit.bat
fgrep warning log err
fgrep error log err
