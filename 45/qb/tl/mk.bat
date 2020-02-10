del err
del log
redirect -e log make localSources -f ..\ir\makefile > log
make -n %1 %2 %3 %4 %5 %6 %7 %8 %9 -f makefile -f ..\qb\makefile -f ..\ir\makefile >makeit.bat
redirect -e err makeit >> log
del makeit.bat
fgrep warning err log
fgrep error err log
